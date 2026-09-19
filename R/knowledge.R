# Accepted organizational knowledge boundary

tempest_knowledge_abort <- function(
  message,
  ...,
  class = character(),
  parent = NULL
) {
  tempest_abort(
    message,
    ...,
    class = c("tempest_knowledge_error", class),
    parent = parent,
    .envir = rlang::caller_env()
  )
}

# Accepted evidence classes Tempest will read as data. Stored procedure and
# program records do not select executable modules.
tempest_knowledge_record_allowlist <- function() {
  c("Claim", "ClaimSupport", "EvidenceSpan", "Source")
}

tempest_is_accepted_knowledge_resource <- function(resource) {
  resource@resource_kind %in% "artifact.record"
}

tempest_knowledge_max_records <- function() {
  1000L
}

#' Retained research knowledge from an artifact selection
#'
#' @keywords internal
TempestKnowledge <- S7::new_class(
  "TempestKnowledge",
  package = "tempest",
  properties = list(
    admission = S7::new_property(S7::class_any),

    record_ids = S7::new_property(S7::class_character, default = character()),
    records = S7::new_property(S7::class_list, default = list()),
    artifact_selection = S7::new_property(S7::class_list, default = list())
  ),
  constructor = function(
    record_ids = character(),
    records = list(),
    artifact_selection = list(),
    admission = NULL
  ) {
    value <- S7::new_object(
      S7::S7_object(),
      admission = admission,

      record_ids = record_ids,
      records = records,
      artifact_selection = artifact_selection
    )
    # Carry the S3 class so the public print method dispatches.
    class(value) <- c("tempest_knowledge", class(value))
    value
  },
  validator = function(self) {
    if (anyDuplicated(self@record_ids)) {
      return("@record_ids must be unique.")
    }
    if (length(self@records) != length(self@record_ids)) {
      return("@records must materialize exactly one resource per record id.")
    }
    NULL
  }
)

tempest_is_knowledge <- function(x) {
  identical(S7::S7_class(x), TempestKnowledge)
}

tempest_knowledge_record_ids <- function(value) {
  if (is.null(value)) {
    return(character())
  }
  value <- tempest_product_character(value, "record_ids")
  if (anyDuplicated(value)) {
    tempest_knowledge_abort("{.arg record_ids} must be unique.")
  }
  if (length(value) > tempest_knowledge_max_records()) {
    tempest_knowledge_abort(paste0(
      "{.arg record_ids} cannot exceed ",
      tempest_knowledge_max_records(),
      " accepted records."
    ))
  }
  value
}


# Accepted Claims carry their exact statement text as metadata so the briefing
# can decide structurally whether a verified claim restates accepted knowledge.
tempest_knowledge_statement_metadata <- function(record_class, payload) {
  if (!identical(record_class, "Claim")) {
    return(list())
  }
  text <- payload[["statement_text"]]
  if (!rlang::is_string(text) || is.na(text)) {
    return(list())
  }
  status <- payload[["status"]]
  if (!rlang::is_string(status) || is.na(status)) {
    status <- "active"
  }
  list(graft_statement_text = text, graft_statement_status = status)
}

# Render an accepted record as canonical inert text. A record whose fields
# cannot be rendered exactly is rejected rather than truncated.
tempest_knowledge_record_text <- function(payload, record_id) {
  parts <- vapply(
    names(payload),
    function(field) {
      value <- payload[[field]]
      if (is.null(value)) {
        return(paste0(field, ": "))
      }
      normalized <- tryCatch(
        tempest_knowledge_value(value),
        error = function(error) {
          tempest_knowledge_abort(
            paste0(
              "Accepted record {.val {record_id}} field {.field ",
              field,
              "} is not exactly materializable."
            ),
            parent = error
          )
        }
      )
      if (
        is.atomic(normalized) &&
          length(normalized) == 1L &&
          !is.na(normalized) &&
          is.null(attributes(normalized))
      ) {
        return(paste0(field, ": ", as.character(normalized)))
      }
      encoded <- tryCatch(
        tempest_product_canonical_json(normalized),
        error = function(error) {
          tempest_knowledge_abort(
            paste0(
              "Accepted record {.val {record_id}} field {.field ",
              field,
              "} is not exactly materializable."
            ),
            parent = error
          )
        }
      )
      if (!rlang::is_string(encoded) || !nzchar(encoded)) {
        tempest_knowledge_abort(paste0(
          "Accepted record {.val {record_id}} field {.field ",
          field,
          "} is not exactly materializable."
        ))
      }
      paste0(field, ": ", encoded)
    },
    character(1)
  )
  paste(parts, collapse = "\n")
}


# Resolve the public `knowledge` argument into the validated artifact selection and the
# builtin ProgramSet.
tempest_knowledge_argument <- function(
  knowledge,
  arg = "knowledge",
  admit = TRUE
) {
  if (is.null(knowledge)) {
    return(list(
      value = NULL,

      records = list(),
      artifact_selection = list(),
      program_set = tempest_program_set()
    ))
  }
  if (!tempest_is_knowledge(knowledge)) {
    tempest_knowledge_abort(
      "{.arg {arg}} must be created by {.fn tempest_artifact_knowledge}.",
      class = "tempest_input_error"
    )
  }
  if (
    !is.null(knowledge@artifact_selection$provenance$graft_decision) &&
      !is.function(knowledge@admission)
  ) {
    tempest_knowledge_abort(
      "Graft decision knowledge requires current admission through tempest_reuse_artifact_research()."
    )
  }
  if (isTRUE(admit) && !is.null(knowledge@admission)) {
    current <- knowledge@admission()
    if (!identical(current@artifact_selection, knowledge@artifact_selection)) {
      tempest_knowledge_abort(
        "Research admission differs from its retained decision."
      )
    }
  }
  tempest_artifact_selection_validate(
    knowledge@artifact_selection,
    knowledge@records
  )
  list(
    artifact_selection = knowledge@artifact_selection,
    value = knowledge,

    records = knowledge@records,
    program_set = tempest_program_set()
  )
}

tempest_knowledge_workspace_preflight <- function(workspace, selection) {
  if (!inherits(workspace, "ResearchWorkspace")) {
    tempest_knowledge_abort(
      "Accepted knowledge records require a ResearchWorkspace."
    )
  }
  if (
    length(workspace$artifact_selection) &&
      !identical(workspace$artifact_selection, selection)
  ) {
    tempest_knowledge_abort(
      "A workspace cannot change its pinned artifact selection."
    )
  }
  if (
    !identical(tempest_research_workspace_mutation_state(workspace), "open")
  ) {
    tempest_knowledge_abort("New research requires an open workspace.")
  }
  invisible(workspace)
}

# Insert accepted evidence records into the product workspace as ordinary
# read-only resources.
tempest_knowledge_insert_records <- function(
  workspace,
  records,
  selection = list()
) {
  tempest_artifact_selection_validate(selection, records)
  tempest_knowledge_workspace_preflight(workspace, selection)
  if (length(records) == 0L) {
    return(invisible(workspace))
  }
  for (record in records) {
    workspace$upsert_retrieved_resource(record)
  }
  if (length(selection)) {
    workspace$bind_artifact_selection(selection)
  }
  invisible(workspace)
}

#' Print accepted organizational knowledge
#'
#' @param x A `TempestKnowledge` value from [tempest_artifact_knowledge()].
#' @param ... Ignored.
#' @return `x`, invisibly.
#' @export
print.tempest_knowledge <- function(x, ...) {
  cli::cli_text("{.cls tempest_knowledge}")
  cli::cli_bullets(c(
    "*" = "artifact selection: {.val {x@artifact_selection$selection_id}}",
    "*" = "accepted records: {length(x@record_ids)}"
  ))
  invisible(x)
}

tempest_knowledge_timestamp_value <- function(value) {
  if (
    !rlang::is_string(value) ||
      is.na(value) ||
      !grepl(
        paste0(
          "^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:",
          "[0-9]{2}:[0-9]{2}(?:\\.[0-9]{1,6})?Z$"
        ),
        value
      )
  ) {
    return(NULL)
  }
  parsed <- suppressWarnings(tempest_stage_time_parse(value))
  if (length(parsed) != 1L || is.na(parsed)) {
    return(NULL)
  }
  fraction <- if (nchar(value) == 20L) {
    ""
  } else {
    substr(value, 21L, nchar(value) - 1L)
  }
  paste0(
    substr(value, 1L, 19L),
    ".",
    fraction,
    strrep("0", 6L - nchar(fraction)),
    "Z"
  )
}

tempest_knowledge_posix_value <- function(value) {
  result <- rep(NA_character_, length(value))
  value <- tryCatch(
    suppressWarnings(as.POSIXct(value)),
    error = \(error) NULL
  )
  if (is.null(value) || length(value) != length(result)) {
    return(result)
  }
  numeric_value <- suppressWarnings(as.numeric(value))
  valid <- !is.na(numeric_value) & is.finite(numeric_value)
  indices <- which(valid)
  if (length(indices) == 0L) {
    return(result)
  }
  microseconds <- round(numeric_value[indices] * 1e6)
  seconds <- floor(microseconds / 1e6)
  fractions <- microseconds - seconds * 1e6
  finite <- is.finite(microseconds) &
    is.finite(seconds) &
    is.finite(fractions) &
    fractions >= 0 &
    fractions < 1e6
  indices <- indices[finite]
  seconds <- seconds[finite]
  fractions <- fractions[finite]
  if (length(indices) == 0L) {
    return(result)
  }
  rendered <- tryCatch(
    suppressWarnings(format(
      as.POSIXct(seconds, origin = "1970-01-01", tz = "UTC"),
      "%Y-%m-%dT%H:%M:%S",
      tz = "UTC"
    )),
    error = \(error) rep(NA_character_, length(seconds))
  )
  candidates <- paste0(
    rendered,
    ".",
    sprintf("%06d", as.integer(fractions)),
    "Z"
  )
  result[indices] <- vapply(
    candidates,
    function(candidate) {
      canonical <- tempest_knowledge_timestamp_value(candidate)
      if (is.null(canonical) || !identical(canonical, candidate)) {
        return(NA_character_)
      }
      canonical
    },
    character(1)
  )
  result
}

tempest_knowledge_value <- function(value) {
  if (
    is.null(value) ||
      length(value) == 0L ||
      (length(value) == 1L && is.atomic(value) && is.na(value))
  ) {
    return(NULL)
  }
  if (inherits(value, "POSIXt")) {
    return(tempest_knowledge_posix_value(value))
  }
  timestamp <- tempest_knowledge_timestamp_value(value)
  if (!is.null(timestamp)) {
    return(timestamp)
  }
  if (is.factor(value)) {
    value <- as.character(value)
  }
  if (is.list(value)) {
    return(lapply(value, tempest_knowledge_value))
  }
  unname(value)
}
