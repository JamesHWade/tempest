# Fixed-schema state for the scripted STORM product flow.

tempest_storm_state_fields <- function() {
  c(
    "schema_version",
    "topic",
    "title",
    "requested_steps",
    "perspectives",
    "experts",
    "draft_outline",
    "outline",
    "lead_section",
    "draft_md",
    "report_md",
    "references",
    "stage_records",
    "completed_stages"
  )
}

tempest_storm_stage_order <- function() {
  c("perspectives", "research", "outline", "write", "polish")
}

tempest_storm_stage_array <- function(value, field, from_record = FALSE) {
  if (isTRUE(from_record) && is.list(value) && is.null(names(value))) {
    if (length(value) == 0L) {
      return(character())
    }
    valid <- vapply(
      value,
      \(stage) is.character(stage) && length(stage) == 1L && !is.na(stage),
      logical(1)
    )
    if (all(valid)) {
      value <- unlist(value, use.names = FALSE)
    }
  }
  if (!is.character(value) || is.object(value) || !is.null(names(value))) {
    tempest_storm_state_abort(
      "{.field {field}} must be a flat array of STORM stage names."
    )
  }
  unname(value)
}

tempest_storm_requested_steps <- function(value, from_record = FALSE) {
  value <- tempest_storm_stage_array(
    value,
    "requested_steps",
    from_record = from_record
  )
  allowed <- tempest_storm_stage_order()
  if (
    length(value) == 0L ||
      anyNA(value) ||
      any(!nzchar(value)) ||
      any(!value %in% allowed) ||
      anyDuplicated(value)
  ) {
    tempest_storm_state_abort(
      paste0(
        "{.field requested_steps} must contain unique STORM stages from: ",
        "{.val {allowed}}."
      )
    )
  }
  allowed[allowed %in% value]
}

tempest_stage_durable_output_contracts <- function() {
  list(
    perspectives = list(
      kind = "state_field",
      fields = c("title", "perspectives")
    ),
    personas = list(kind = "state_field", fields = "experts"),
    query_decomposition = list(kind = "content_digest", fields = NULL),
    extract_claims = list(kind = "workspace_claims", fields = NULL),
    verify_claim_support = list(kind = "claim_supports", fields = NULL),
    next_question = list(kind = "content_digest", fields = NULL),
    draft_outline = list(kind = "state_field", fields = "draft_outline"),
    refined_outline = list(kind = "state_field", fields = "outline"),
    section_writing = list(kind = "content_digest", fields = NULL),
    lead_section = list(kind = "content_digest", fields = NULL)
  )
}

tempest_stage_records_validate_product_outputs <- function(records, state) {
  records <- tempest_stage_records_validate(records)
  state_fields <- names(state)
  if (
    !is.list(state) ||
      is.data.frame(state) ||
      is.null(state_fields) ||
      anyNA(state_fields) ||
      anyDuplicated(state_fields)
  ) {
    tempest_stage_record_abort(
      "{.arg state} must be an exact named product-state record."
    )
  }
  contracts <- tempest_stage_durable_output_contracts()
  for (record in records) {
    if (!identical(record@status, "succeeded")) {
      next
    }
    contract <- contracts[[record@stage]] %||% NULL
    reference <- record@output_reference
    if (
      is.null(contract) ||
        !identical(reference$kind, contract$kind)
    ) {
      tempest_stage_record_abort(
        "Stage-record output kind does not match its durable stage contract."
      )
    }
    if (identical(contract$kind, "state_field")) {
      absent <- contract$fields[
        !contract$fields %in% state_fields |
          vapply(
            contract$fields,
            \(field) is.null(state[[field]]),
            logical(1)
          )
      ]
      if (
        !identical(reference$ids, as.list(contract$fields)) ||
          length(absent) > 0L
      ) {
        tempest_stage_record_abort(
          "Stage-record output does not identify present product state."
        )
      }
    }
  }
  invisible(records)
}

tempest_stage_records_validate_storm_content <- function(records, state) {
  records <- tempest_stage_records_validate(records)
  state_outputs <- list(
    perspectives = list(
      title = state$title,
      perspectives = state$perspectives
    ),
    personas = state$experts,
    draft_outline = state$draft_outline,
    refined_outline = state$outline
  )
  for (record in records) {
    if (!identical(record@status, "succeeded")) {
      next
    }
    if (record@stage %in% names(state_outputs)) {
      output <- state_outputs[[record@stage]]
      if (is.null(output)) {
        next
      }
      expected <- tempest_stage_state_output_digest(record@stage, output)
      if (!identical(record@output_reference$content_digest, expected)) {
        tempest_stage_record_abort(
          paste0(
            "Stage-record state-field digest does not match restored ",
            "STORM product content."
          )
        )
      }
    }
    if (
      identical(record@stage, "lead_section") &&
        !is.null(state$lead_section)
    ) {
      expected <- tempest_stage_content_digest_id(state$lead_section)
      actual_ids <- unlist(
        record@output_reference$ids,
        use.names = FALSE
      )
      if (
        !identical(actual_ids, expected) ||
          !identical(record@output_reference$content_digest, expected)
      ) {
        tempest_stage_record_abort(
          "Lead-section stage digest does not match restored STORM content."
        )
      }
    }
  }
  invisible(records)
}

tempest_storm_succeeded_stage_records <- function(records, stage) {
  Filter(
    \(record) {
      identical(record@stage, stage) && identical(record@status, "succeeded")
    },
    records
  )
}

tempest_storm_require_succeeded_stage <- function(records, stage, product) {
  matched <- tempest_storm_succeeded_stage_records(records, stage)
  if (length(matched) == 0L) {
    tempest_stage_record_abort(
      paste0(
        "Completed STORM ",
        product,
        " requires a succeeded {.val {stage}} stage record."
      )
    )
  }
  invisible(matched)
}

tempest_storm_draft_section_texts <- function(state) {
  lead <- state$lead_section
  draft <- state$draft_md
  if (!rlang::is_string(lead) || !rlang::is_string(draft)) {
    tempest_stage_record_abort(
      "Completed STORM writing requires durable lead and draft text."
    )
  }
  prefix <- paste0(lead, "\n\n")
  if (!startsWith(draft, prefix)) {
    tempest_stage_record_abort(
      "The durable STORM draft does not begin with its exact lead section."
    )
  }
  body <- substr(draft, nchar(prefix) + 1L, nchar(draft))
  sections <- tempest_sections_to_write(state$outline)
  if (length(sections) == 0L) {
    if (nzchar(body)) {
      tempest_stage_record_abort(
        "The durable STORM draft contains sections absent from its outline."
      )
    }
    return(character())
  }
  headers <- vapply(
    sections,
    \(section) paste0("## ", section$title %||% "Section", "\n\n"),
    character(1)
  )
  texts <- character(length(headers))
  remaining <- body
  for (index in seq_along(headers)) {
    header <- headers[[index]]
    if (!startsWith(remaining, header)) {
      tempest_stage_record_abort(
        "The durable STORM draft does not match its ordered outline sections."
      )
    }
    remaining <- substr(
      remaining,
      nchar(header) + 1L,
      nchar(remaining)
    )
    if (index < length(headers)) {
      boundary <- paste0("\n\n", headers[[index + 1L]])
      position <- regexpr(boundary, remaining, fixed = TRUE)[[1]]
      if (position < 1L) {
        tempest_stage_record_abort(
          "The durable STORM draft is missing an outlined section."
        )
      }
      texts[[index]] <- substr(remaining, 1L, position - 1L)
      remaining <- substr(
        remaining,
        position + 2L,
        nchar(remaining)
      )
    } else {
      texts[[index]] <- remaining
      remaining <- ""
    }
  }
  if (any(!nzchar(tempest_trim(texts))) || nzchar(remaining)) {
    tempest_stage_record_abort(
      "Every durable STORM outline section requires non-empty exact text."
    )
  }
  unname(texts)
}

tempest_stage_records_validate_storm_coverage <- function(records, state) {
  records <- tempest_stage_records_validate(records)
  completed <- state$completed_stages
  if ("perspectives" %in% completed) {
    tempest_storm_require_succeeded_stage(
      records,
      "perspectives",
      "perspectives"
    )
  }
  if ("research" %in% completed) {
    tempest_storm_require_succeeded_stage(
      records,
      "query_decomposition",
      "research"
    )
    tempest_storm_require_succeeded_stage(
      records,
      "extract_claims",
      "research"
    )
  }
  if ("outline" %in% completed) {
    tempest_storm_require_succeeded_stage(records, "draft_outline", "outline")
    tempest_storm_require_succeeded_stage(
      records,
      "refined_outline",
      "outline"
    )
  }
  if ("write" %in% completed) {
    tempest_storm_require_succeeded_stage(records, "lead_section", "writing")
    section_texts <- tempest_storm_draft_section_texts(state)
    expected <- unname(sort(vapply(
      section_texts,
      tempest_stage_content_digest_id,
      character(1)
    )))
    section_records <- tempest_storm_succeeded_stage_records(
      records,
      "section_writing"
    )
    actual <- unname(sort(vapply(
      section_records,
      \(record) record@output_reference$content_digest,
      character(1)
    )))
    if (!identical(actual, expected)) {
      tempest_stage_record_abort(
        paste0(
          "Succeeded section-writing records must exactly cover every ",
          "durable STORM draft section."
        )
      )
    }
  }
  invisible(records)
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
      if (length(value) > 0L && !any(named)) names(value) <- NULL
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
  value <- tempest_storm_stage_array(
    value,
    "completed_stages",
    from_record = from_record
  )
  allowed <- tempest_storm_stage_order()
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
      "STORM product state must contain exactly the schema version 5 fields in schema order."
    )
  }
  if (!tempest_exact_integer_scalar_valid(state$schema_version, 5L, 5L)) {
    tempest_storm_state_abort(
      "{.field schema_version} must be the supported version `5`."
    )
  }
  state$topic <- tempest_storm_state_string(state$topic, "topic", TRUE)
  state$title <- tempest_storm_state_string(state$title, "title", TRUE)
  state$requested_steps <- tempest_storm_requested_steps(
    state$requested_steps
  )
  state$perspectives <- tempest_storm_state_structured(
    state$perspectives,
    "perspectives"
  )
  state$experts <- tryCatch(
    tempest_validate_experts(state$experts),
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
  state$stage_records <- tryCatch(
    tempest_stage_records_validate(state$stage_records),
    error = function(error) {
      tempest_storm_state_abort(
        "{.field stage_records} must contain valid Tempest stage records.",
        parent = error
      )
    }
  )
  tryCatch(
    tempest_stage_records_validate_product_outputs(
      state$stage_records,
      state
    ),
    error = function(error) {
      tempest_storm_state_abort(
        "{.field stage_records} must reference present STORM product state.",
        parent = error
      )
    }
  )
  tryCatch(
    tempest_stage_records_validate_storm_content(
      state$stage_records,
      state
    ),
    error = function(error) {
      tempest_storm_state_abort(
        "{.field stage_records} must match exact STORM product content.",
        parent = error
      )
    }
  )
  tryCatch(
    tempest_stage_records_validate_execution_review(
      state$report_md,
      state$stage_records,
      trusted_title = state$title
    ),
    error = function(error) {
      tempest_storm_state_abort(
        "{.field report_md} must disclose exact STORM execution downgrades.",
        parent = error
      )
    }
  )
  state$completed_stages <- tempest_storm_state_completed_stages(
    state$completed_stages
  )
  unrequested <- setdiff(state$completed_stages, state$requested_steps)
  if (length(unrequested) > 0L) {
    tempest_storm_state_abort(
      paste0(
        "{.field completed_stages} cannot contain stages outside the ",
        "immutable {.field requested_steps}: {.val {unrequested}}."
      )
    )
  }
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
  requested_steps = tempest_storm_stage_order(),
  perspectives = list(),
  experts = list(),
  draft_outline = NULL,
  outline = NULL,
  lead_section = NULL,
  draft_md = NULL,
  report_md = NULL,
  references = list(),
  stage_records = list(),
  completed_stages = character(),
  schema_version = 5L
) {
  tempest_storm_state_validate(list(
    schema_version = schema_version,
    topic = topic,
    title = title,
    requested_steps = requested_steps,
    perspectives = perspectives,
    experts = experts,
    draft_outline = draft_outline,
    outline = outline,
    lead_section = lead_section,
    draft_md = draft_md,
    report_md = report_md,
    references = references,
    stage_records = stage_records,
    completed_stages = completed_stages
  ))
}

tempest_storm_state_is_complete <- function(state) {
  state <- tempest_storm_state_validate(state)
  identical(state$requested_steps, tempest_storm_stage_order()) &&
    identical(state$completed_stages, tempest_storm_stage_order()) &&
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
  record$stage_records <- tryCatch(
    tempest_stage_records_data(tempest_stage_records_interrupt(
      state$stage_records,
      completed_at = tempest_now_utc()
    )),
    error = function(error) {
      tempest_storm_state_abort(
        "Could not encode {.field stage_records} in STORM product state.",
        class = "tempest_storm_state_record_error",
        parent = error
      )
    }
  )
  tempest_storm_state_record_value(record, "state")
}

tempest_storm_state_from_record <- function(record) {
  fields <- tempest_storm_state_fields()
  if (
    !is.list(record) ||
      is.data.frame(record)
  ) {
    tempest_storm_state_abort(
      "STORM product-state records must be lists.",
      class = "tempest_storm_state_restore_error"
    )
  }
  schema_version <- record$schema_version
  if (!tempest_exact_integer_scalar_valid(schema_version, minimum = 0L)) {
    tempest_storm_state_abort(
      "STORM product-state schema version must be one exact integer.",
      class = "tempest_storm_state_restore_error"
    )
  }
  if (!identical(schema_version, 5L)) {
    tempest_product_unsupported_format_abort(
      "STORM product-state format",
      schema_version,
      c(
        "tempest_storm_state_restore_error",
        "tempest_storm_state_error",
        "tempest_error"
      )
    )
  }
  if (!identical(names(record), fields)) {
    tempest_storm_state_abort(
      "STORM product-state records must contain exactly the schema version 5 fields in schema order.",
      class = "tempest_storm_state_restore_error"
    )
  }
  record$requested_steps <- tempest_storm_requested_steps(
    record$requested_steps,
    from_record = TRUE
  )
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
  record$stage_records <- tryCatch(
    tempest_stage_records_from_data(
      record$stage_records,
      allow_running = FALSE
    ),
    error = function(error) {
      tempest_storm_state_abort(
        "Could not restore {.field stage_records} from STORM product state.",
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


#' @keywords internal
tempest_storm_stage_complete <- function(completed_stages, stage) {
  stage %in% completed_stages
}

#' @keywords internal
tempest_storm_mark_stage_complete <- function(completed_stages, stage) {
  allowed <- c("perspectives", "research", "outline", "write", "polish")
  completed_stages <- unique(c(completed_stages, stage))
  completed_stages[order(match(completed_stages, allowed))]
}
