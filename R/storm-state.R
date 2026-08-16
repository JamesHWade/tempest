# Fixed-schema state for the scripted STORM product flow.

tempest_storm_state_fields <- function() {
  c(
    "schema_version",
    "topic",
    "title",
    "perspectives",
    "experts",
    "draft_outline",
    "outline",
    "lead_section",
    "draft_md",
    "report_md",
    "references",
    "completed_stages"
  )
}

tempest_storm_state_abort <- function(
  message,
  ...,
  class = "tempest_storm_state_validation_error",
  parent = NULL
) {
  tempest_abort(
    message,
    ...,
    class = unique(c(class, "tempest_storm_state_error", "tempest_error")),
    parent = parent,
    .envir = rlang::caller_env()
  )
}

tempest_storm_state_string <- function(value, field, non_empty = FALSE) {
  if (!is.character(value) || length(value) != 1L || is.na(value)) {
    tempest_storm_state_abort(
      "{.field {field}} must be a single string."
    )
  }
  if (isTRUE(non_empty) && !nzchar(tempest_trim(value))) {
    tempest_storm_state_abort(
      "{.field {field}} must be a single non-empty string."
    )
  }
  if (isTRUE(non_empty)) tempest_trim(value) else value
}

tempest_storm_state_record_value <- function(value, path) {
  if (is.null(value)) {
    return(NULL)
  }
  if (
    is.function(value) ||
      is.environment(value) ||
      inherits(value, "connection") ||
      typeof(value) %in% c("externalptr", "weakref")
  ) {
    tempest_storm_state_abort(
      "STORM product state cannot contain runtime objects at {.field {path}}."
    )
  }
  if (inherits(value, "S7_object") || is.object(value)) {
    tempest_storm_state_abort(
      "{.field {path}} must contain only plain JSON-compatible values."
    )
  }
  if (is.list(value)) {
    value_names <- names(value)
    if (!is.null(value_names)) {
      if (anyNA(value_names) || anyDuplicated(value_names)) {
        tempest_storm_state_abort(
          "Lists in {.field {path}} cannot have missing or duplicate names."
        )
      }
      named <- nzchar(value_names)
      if (any(named) && any(!named)) {
        tempest_storm_state_abort(
          "Lists in {.field {path}} must be fully named or fully unnamed."
        )
      }
      if (!any(named)) names(value) <- NULL
    }
    return(stats::setNames(
      lapply(seq_along(value), function(index) {
        child <- if (is.null(names(value))) {
          paste0(path, "[[", index, "]]")
        } else {
          paste0(path, "$", names(value)[[index]])
        }
        tempest_storm_state_record_value(value[[index]], child)
      }),
      names(value)
    ))
  }
  if (
    !typeof(value) %in% c("logical", "integer", "double", "character") ||
      !is.null(names(value))
  ) {
    tempest_storm_state_abort(
      "{.field {path}} must contain only plain JSON-compatible values."
    )
  }
  if (anyNA(value)) {
    if (length(value) == 1L) {
      return(NULL)
    }
    return(lapply(seq_along(value), function(index) {
      tempest_storm_state_record_value(
        value[[index]],
        paste0(path, "[[", index, "]]")
      )
    }))
  }
  if (is.numeric(value) && any(!is.finite(value))) {
    tempest_storm_state_abort(
      "{.field {path}} must contain only plain JSON-compatible values."
    )
  }
  if (length(value) != 1L) {
    return(lapply(seq_along(value), function(index) {
      tempest_storm_state_record_value(
        value[[index]],
        paste0(path, "[[", index, "]]")
      )
    }))
  }
  value
}

tempest_storm_state_structured <- function(value, field, nullable = FALSE) {
  if (is.null(value) && isTRUE(nullable)) {
    return(NULL)
  }
  if (!is.list(value) || is.data.frame(value)) {
    tempest_storm_state_abort(
      "{.field {field}} must be {if (nullable) '`NULL` or ' else ''}a list."
    )
  }
  tempest_storm_state_record_value(value, field)
}

tempest_storm_state_completed_stages <- function(value, from_record = FALSE) {
  if (isTRUE(from_record) && is.list(value) && is.null(names(value))) {
    if (length(value) == 0L) {
      return(character())
    }
    valid <- vapply(
      value,
      \(stage) is.character(stage) && length(stage) == 1L && !is.na(stage),
      logical(1)
    )
    if (all(valid)) value <- unlist(value, use.names = FALSE)
  }
  allowed <- c("perspectives", "research", "outline", "write", "polish")
  if (
    !is.character(value) ||
      anyNA(value) ||
      any(!nzchar(value)) ||
      any(!value %in% allowed) ||
      anyDuplicated(value) ||
      is.unsorted(match(value, allowed), strictly = TRUE)
  ) {
    tempest_storm_state_abort(
      paste0(
        "{.field completed_stages} must contain unique STORM stages in ",
        "canonical relative order from: {.val {allowed}}."
      )
    )
  }
  unname(value)
}

tempest_storm_state_validate <- function(state) {
  fields <- tempest_storm_state_fields()
  if (
    !is.list(state) ||
      is.data.frame(state) ||
      !identical(names(state), fields)
  ) {
    tempest_storm_state_abort(
      "STORM product state must contain exactly the schema version 1 fields in schema order."
    )
  }
  if (
    !is.numeric(state$schema_version) ||
      length(state$schema_version) != 1L ||
      is.na(state$schema_version) ||
      !identical(as.integer(state$schema_version), 1L) ||
      state$schema_version != 1
  ) {
    tempest_storm_state_abort(
      "{.field schema_version} must be the supported version `1`."
    )
  }
  state$schema_version <- 1L
  state$topic <- tempest_storm_state_string(state$topic, "topic", TRUE)
  state$title <- tempest_storm_state_string(state$title, "title", TRUE)
  state$perspectives <- tempest_storm_state_structured(
    state$perspectives,
    "perspectives"
  )
  state$experts <- tryCatch(
    tempest_validate_experts(state$experts, active_only = FALSE),
    error = function(error) {
      tempest_storm_state_abort(
        "{.field experts} must contain only Tempest expert profiles.",
        parent = error
      )
    }
  )
  state["draft_outline"] <- list(tempest_storm_state_structured(
    state$draft_outline,
    "draft_outline",
    nullable = TRUE
  ))
  state["outline"] <- list(tempest_storm_state_structured(
    state$outline,
    "outline",
    nullable = TRUE
  ))
  for (field in c("lead_section", "draft_md", "report_md")) {
    value <- state[[field]]
    if (!is.null(value)) {
      state[[field]] <- tempest_storm_state_string(value, field)
    }
  }
  state$references <- tempest_storm_state_structured(
    state$references,
    "references"
  )
  state$completed_stages <- tempest_storm_state_completed_stages(
    state$completed_stages
  )
  required_outputs <- list(
    outline = c("draft_outline", "outline"),
    write = c("outline", "draft_md"),
    polish = c("draft_md", "report_md")
  )
  for (stage in intersect(names(required_outputs), state$completed_stages)) {
    missing <- required_outputs[[stage]][vapply(
      required_outputs[[stage]],
      function(field) is.null(state[[field]]),
      logical(1)
    )]
    if (length(missing) > 0L) {
      tempest_storm_state_abort(
        paste0(
          "Completed STORM stage {.val {stage}} requires fixed state field",
          "{?s}: {.field {missing}}."
        )
      )
    }
  }
  state
}

tempest_storm_state <- function(
  topic,
  title = topic,
  perspectives = list(),
  experts = list(),
  draft_outline = NULL,
  outline = NULL,
  lead_section = NULL,
  draft_md = NULL,
  report_md = NULL,
  references = list(),
  completed_stages = character(),
  schema_version = 1L
) {
  tempest_storm_state_validate(list(
    schema_version = schema_version,
    topic = topic,
    title = title,
    perspectives = perspectives,
    experts = experts,
    draft_outline = draft_outline,
    outline = outline,
    lead_section = lead_section,
    draft_md = draft_md,
    report_md = report_md,
    references = references,
    completed_stages = completed_stages
  ))
}

tempest_storm_state_is_complete <- function(state) {
  state <- tempest_storm_state_validate(state)
  "polish" %in%
    state$completed_stages &&
    rlang::is_string(state$report_md) &&
    nzchar(tempest_trim(state$report_md))
}

tempest_storm_state_record <- function(state) {
  state <- tempest_storm_state_validate(state)
  experts <- tryCatch(
    tempest_expert_records(state$experts),
    error = function(error) {
      tempest_storm_state_abort(
        "Could not encode {.field experts} in STORM product state.",
        class = "tempest_storm_state_record_error",
        parent = error
      )
    }
  )
  record <- state
  record$experts <- experts
  tempest_storm_state_record_value(record, "state")
}

tempest_storm_state_from_record <- function(record) {
  fields <- tempest_storm_state_fields()
  if (
    !is.list(record) ||
      is.data.frame(record) ||
      !identical(names(record), fields)
  ) {
    tempest_storm_state_abort(
      "STORM product-state records must contain exactly the schema version 1 fields in schema order.",
      class = "tempest_storm_state_restore_error"
    )
  }
  record$completed_stages <- tempest_storm_state_completed_stages(
    record$completed_stages,
    from_record = TRUE
  )
  record$experts <- tryCatch(
    tempest_experts_from_records(
      record$experts,
      what = "STORM product-state expert profiles",
      class = c(
        "tempest_storm_state_restore_error",
        "tempest_storm_state_error"
      )
    ),
    error = function(error) {
      tempest_storm_state_abort(
        "Could not restore {.field experts} from STORM product state.",
        class = "tempest_storm_state_restore_error",
        parent = error
      )
    }
  )
  tryCatch(
    tempest_storm_state_validate(record),
    error = function(error) {
      tempest_storm_state_abort(
        "Could not restore an invalid STORM product-state record.",
        class = "tempest_storm_state_restore_error",
        parent = error
      )
    }
  )
}
