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

# Accepted evidence classes Tempest will read as data. GovernedProcedure and
# ProgramArtifact are executable authority and are reachable only through an
# explicit stage binding.
tempest_knowledge_record_allowlist <- function() {
  c("Claim", "ClaimSupport", "EvidenceSpan", "Source")
}

tempest_knowledge_max_records <- function() {
  100L
}

#' Accepted organizational knowledge pinned to one Graft view
#'
#' @keywords internal
TempestKnowledge <- S7::new_class(
  "TempestKnowledge",
  package = "tempest",
  properties = list(
    view = S7::new_property(S7::class_any),
    snapshot = S7::new_property(S7::class_any),
    reference = S7::new_property(S7::class_any),
    record_ids = S7::new_property(S7::class_character, default = character()),
    records = S7::new_property(S7::class_list, default = list()),
    governed_procedures = S7::new_property(S7::class_list, default = list())
  ),
  constructor = function(
    view = NULL,
    snapshot = NULL,
    reference = NULL,
    record_ids = character(),
    records = list(),
    governed_procedures = list()
  ) {
    value <- S7::new_object(
      S7::S7_object(),
      view = view,
      snapshot = snapshot,
      reference = reference,
      record_ids = record_ids,
      records = records,
      governed_procedures = governed_procedures
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

# Project one accepted immutable record into an exact evidence resource. The
# record text stays in a data field; it never becomes a prompt, a role, a tool,
# a procedure selection, or an executable artifact.
tempest_knowledge_record_resource <- function(knowledge_view, record_id) {
  history <- tryCatch(
    tempest_governed_procedure_history(knowledge_view, record_id),
    error = function(error) {
      tempest_knowledge_abort(
        "Could not resolve the accepted Graft record {.val {record_id}}.",
        parent = error
      )
    }
  )
  required <- c("revision_id", "record_id", "class", "record")
  if (
    !is.data.frame(history) ||
      nrow(history) != 1L ||
      !all(required %in% names(history)) ||
      !identical(history$record_id[[1L]], record_id) ||
      !is.list(history$record[[1L]])
  ) {
    tempest_knowledge_abort(
      "The pinned Graft view did not return one exact accepted record for {.val {record_id}}."
    )
  }
  record_class <- history$class[[1L]]
  if (!isTRUE(record_class %in% tempest_knowledge_record_allowlist())) {
    tempest_knowledge_abort(paste0(
      "Accepted record {.val {record_id}} has class {.val {record_class}}, ",
      "which is not readable accepted evidence."
    ))
  }
  payload <- history$record[[1L]]
  payload_names <- names(payload)
  if (
    is.null(payload_names) ||
      anyNA(payload_names) ||
      any(!nzchar(payload_names)) ||
      anyDuplicated(payload_names)
  ) {
    tempest_knowledge_abort(
      "Accepted record {.val {record_id}} is not materializable."
    )
  }
  content <- tempest_knowledge_record_text(payload, record_id)
  tempest_resource(
    resource_kind = "graft.record",
    locator = paste0("graft/", record_class, "/", record_id),
    title = paste(record_class, record_id),
    media_type = "text/plain",
    content = content,
    metadata = list(
      graft_record_id = record_id,
      graft_record_class = record_class,
      graft_revision_id = history$revision_id[[1L]]
    )
  )
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
        tempest_graft_plan_value(value),
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

#' Bring accepted organizational knowledge into a Tempest run
#'
#' `tempest_knowledge()` is the one strict constructor for accepted
#' organizational knowledge. It pins an immutable Graft view, materializes an
#' exact allowlist of accepted evidence records, and optionally binds accepted
#' governed procedures to Tempest stages.
#'
#' Accepted record text is evidence, not instruction. It is carried in a data
#' channel and can never change prompts, message roles, tools, governed
#' procedure selection, or executable artifacts. Executable authority comes
#' only from an explicit `governed_procedures` stage binding.
#'
#' @param graft_view A pinned `GraftView` from [graft::graft_at()].
#' @param record_ids Character vector of accepted record ids to read as
#'   evidence. Only `Claim`, `ClaimSupport`, `EvidenceSpan`, and `Source`
#'   records are readable.
#' @param governed_procedures Optional named list mapping an exact Tempest
#'   stage to an accepted `GovernedProcedure` record id.
#' @return A validated `TempestKnowledge` value for [tempest_run()] and
#'   [tempest_session()].
#' @examples
#' \dontrun{
#' view <- graft::graft_at(store, graft::graft_snapshot(store))
#' records <- graft::graft_find(view, "battery recycling", limit = 25)
#' knowledge <- tempest_knowledge(view, record_ids = records$id)
#' result <- tempest_run("Battery recycling", knowledge = knowledge)
#' }
#' @export
tempest_knowledge <- function(
  graft_view,
  record_ids = character(),
  governed_procedures = list()
) {
  snapshot <- tryCatch(
    tempest_governed_procedure_view_snapshot(graft_view),
    error = function(error) {
      tempest_knowledge_abort(
        "{.arg graft_view} must be a valid pinned Graft view.",
        class = "tempest_input_error",
        parent = error
      )
    }
  )
  reference <- tryCatch(
    tempest_snapshot_reference(snapshot),
    error = function(error) {
      tempest_knowledge_abort(
        "The pinned Graft view does not expose a valid immutable snapshot.",
        parent = error
      )
    }
  )
  record_ids <- tempest_knowledge_record_ids(record_ids)
  records <- lapply(
    record_ids,
    function(record_id) {
      tempest_knowledge_record_resource(graft_view, record_id)
    }
  )
  refs <- tempest_knowledge_governed_procedures(
    graft_view,
    governed_procedures
  )
  TempestKnowledge(
    view = graft_view,
    snapshot = tempest_research_workspace_graft_snapshot(snapshot),
    reference = reference,
    record_ids = record_ids,
    records = records,
    governed_procedures = refs
  )
}

tempest_knowledge_governed_procedures <- function(graft_view, value) {
  if (is.null(value) || length(value) == 0L) {
    return(list())
  }
  stages <- tempest_program_set_stages()
  names_value <- names(value)
  if (
    !is.list(value) ||
      is.data.frame(value) ||
      is.null(names_value) ||
      anyNA(names_value) ||
      any(!nzchar(names_value)) ||
      anyDuplicated(names_value) ||
      any(!names_value %in% stages)
  ) {
    tempest_knowledge_abort(paste0(
      "{.arg governed_procedures} must be a uniquely named list using only ",
      "exact Tempest stages."
    ))
  }
  stats::setNames(
    lapply(
      names_value,
      function(stage) {
        record_id <- value[[stage]]
        if (!rlang::is_string(record_id) || is.na(record_id)) {
          tempest_knowledge_abort(paste0(
            "{.arg governed_procedures$",
            stage,
            "} must be one accepted GovernedProcedure record id."
          ))
        }
        tempest_governed_procedure_ref(graft_view, record_id)
      }
    ),
    names_value
  )
}

# Resolve the public `knowledge` argument into the internal pinned view and the
# ProgramSet carrying any accepted governed-procedure stage bindings.
tempest_knowledge_argument <- function(knowledge, arg = "knowledge") {
  if (is.null(knowledge)) {
    return(list(
      value = NULL,
      view = NULL,
      records = list(),
      program_set = tempest_program_set()
    ))
  }
  if (!tempest_is_knowledge(knowledge)) {
    tempest_knowledge_abort(
      "{.arg {arg}} must be created by {.fn tempest_knowledge}.",
      class = "tempest_input_error"
    )
  }
  list(
    value = knowledge,
    view = knowledge@view,
    records = knowledge@records,
    program_set = tempest_program_set(
      governed_procedure_refs = knowledge@governed_procedures
    )
  )
}

# Insert accepted evidence records into the product workspace as ordinary
# read-only resources.
tempest_knowledge_insert_records <- function(workspace, records) {
  if (length(records) == 0L) {
    return(invisible(workspace))
  }
  if (!inherits(workspace, "ResearchWorkspace")) {
    tempest_knowledge_abort(
      "Accepted knowledge records require a ResearchWorkspace."
    )
  }
  for (record in records) {
    workspace$upsert_retrieved_resource(record)
  }
  invisible(workspace)
}

#' Print accepted organizational knowledge
#'
#' @param x A `TempestKnowledge` value from [tempest_knowledge()].
#' @param ... Ignored.
#' @return `x`, invisibly.
#' @export
print.tempest_knowledge <- function(x, ...) {
  cli::cli_text("{.cls tempest_knowledge}")
  cli::cli_bullets(c(
    "*" = "snapshot: {.val {x@reference$snapshot_id %||% NA_character_}}",
    "*" = "accepted records: {length(x@record_ids)}",
    "*" = "governed stages: {.val {names(x@governed_procedures)}}"
  ))
  invisible(x)
}
