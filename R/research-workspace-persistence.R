#' @keywords internal
tempest_research_workspace_snapshot_abort <- function(message, parent = NULL) {
  tempest_abort(
    c("Cannot snapshot ResearchWorkspace.", x = message),
    class = tempest_persistence_error_class(
      "tempest_research_workspace_snapshot_error"
    ),
    parent = parent
  )
}

#' @keywords internal
tempest_research_workspace_max_sources_data <- function(max_sources) {
  if (is.infinite(max_sources)) {
    return("unbounded")
  }
  as.integer(max_sources)
}

#' @keywords internal
tempest_research_workspace_array_record <- function(record, fields) {
  for (field in fields) {
    value <- record[[field]] %||% character()
    record[[field]] <- unname(as.list(value))
  }
  record
}

#' @keywords internal
tempest_research_workspace_claim_record <- function(claim) {
  tempest_research_workspace_array_record(
    tempest_claim_to_list(claim),
    c(
      "source_ids",
      "evidence_span_ids",
      "supporting_quotes",
      "contradicting_source_ids"
    )
  )
}

#' @keywords internal
tempest_research_workspace_dispute_record <- function(dispute) {
  tempest_research_workspace_array_record(
    tempest_dispute_to_list(dispute),
    c("claim_ids", "unresolved_questions")
  )
}

#' @keywords internal
tempest_research_workspace_validate_noncontent <- function(
  workspace,
  action = c("snapshot", "restore")
) {
  action <- match.arg(action)
  abort <- if (identical(action, "snapshot")) {
    tempest_research_workspace_snapshot_abort
  } else {
    tempest_research_workspace_restore_abort
  }
  validate_ids <- function(values, field) {
    values <- unname(unlist(values, use.names = FALSE))
    values <- values[!is.na(values)]
    if (
      any(!nzchar(values)) ||
        any(
          !vapply(
            values,
            tempest_research_workspace_reference_id_valid,
            logical(1)
          )
        )
    ) {
      abort(
        paste0(
          "Workspace ",
          field,
          " must contain bounded credential-free identifiers."
        )
      )
    }
  }

  claims <- workspace$list_proposed_claims()
  for (claim in claims) {
    validate_ids(
      c(
        claim@claim_id,
        claim@source_ids,
        claim@evidence_span_ids,
        claim@contradicting_source_ids,
        claim@retrieval_step_id,
        claim@perspective_id,
        claim@expert_id,
        claim@session_id,
        claim@section_id,
        claim@verifier_model
      ),
      "claim provenance"
    )
    if (tempest_contract_sensitive_scalar(claim@verifier_model)) {
      abort("Workspace claim verifier metadata contains a credential value.")
    }
  }
  for (span in workspace$list_evidence_spans()) {
    validate_ids(
      c(span@evidence_span_id, span@source_id, span@extracted_by),
      "evidence-span provenance"
    )
  }
  for (support in workspace$list_claim_supports()) {
    validate_ids(
      c(
        support@claim_support_id,
        support@claim_id,
        support@evidence_span_id,
        support@source_id
      ),
      "claim-support provenance"
    )
    if (tempest_contract_sensitive_scalar(support@rationale)) {
      abort("Workspace claim-support rationale contains a credential value.")
    }
  }
  for (dispute in workspace$list_disputes()) {
    validate_ids(
      c(dispute@dispute_id, dispute@claim_ids),
      "dispute provenance"
    )
  }
  invisible(workspace)
}

#' @keywords internal
tempest_research_workspace_snapshot_fields <- function() {
  c(
    "schema_version",
    "base_snapshot_id",
    "max_sources",
    "accepted_graft_references",
    "retrieved_resources",
    "proposed_claims",
    "evidence_spans",
    "claim_supports",
    "disputes"
  )
}

#' @keywords internal
tempest_research_workspace_snapshot <- function(workspace) {
  if (!inherits(workspace, "ResearchWorkspace")) {
    tempest_research_workspace_snapshot_abort(
      "{.arg workspace} must be a ResearchWorkspace."
    )
  }
  tryCatch(
    workspace$validate_integrity(),
    error = function(error) {
      tempest_research_workspace_snapshot_abort(
        "Workspace cross-record integrity validation failed.",
        parent = error
      )
    }
  )
  tempest_research_workspace_validate_noncontent(workspace, "snapshot")
  entries <- workspace$list_retrieved_resources()
  retrieved_resources <- unname(lapply(
    entries,
    tempest_resource_record,
    include_content = TRUE
  ))

  snapshot <- list(
    schema_version = 5L,
    base_snapshot_id = workspace$base_snapshot_id,
    max_sources = tempest_research_workspace_max_sources_data(
      workspace$max_sources
    ),
    accepted_graft_references = workspace$list_accepted_graft_references(),
    retrieved_resources = retrieved_resources,
    proposed_claims = lapply(
      workspace$list_proposed_claims(),
      tempest_research_workspace_claim_record
    ),
    evidence_spans = lapply(
      workspace$list_evidence_spans(),
      tempest_evidence_span_to_list
    ),
    claim_supports = lapply(
      workspace$list_claim_supports(),
      tempest_claim_support_to_list
    ),
    disputes = lapply(
      workspace$list_disputes(),
      tempest_research_workspace_dispute_record
    )
  )
  snapshot <- tryCatch(
    tempest_storm_state_record_value(snapshot, "workspace"),
    error = function(error) {
      tempest_research_workspace_snapshot_abort(
        "Workspace records must contain only canonical portable values."
      )
    }
  )
  snapshot <- snapshot[tempest_research_workspace_snapshot_fields()]
  tempest_persistence_credential_audit(
    list(workspace = snapshot),
    "ResearchWorkspace snapshot",
    tempest_persistence_error_class(
      "tempest_research_workspace_snapshot_error"
    )
  )
  snapshot
}

#' @keywords internal
tempest_research_workspace_restore_abort <- function(message, parent = NULL) {
  tempest_abort(
    c("Cannot restore ResearchWorkspace snapshot.", x = message),
    class = tempest_persistence_error_class(
      "tempest_research_workspace_restore_error"
    ),
    parent = parent,
    .envir = rlang::caller_env()
  )
}

#' @keywords internal
tempest_research_workspace_restore_schema <- function(snapshot) {
  value <- snapshot$schema_version %||% NA_integer_
  value <- tempest_exact_integer_scalar(
    value,
    "The workspace schema version",
    class = tempest_persistence_error_class(
      "tempest_research_workspace_restore_error"
    ),
    minimum = 0L
  )
  if (!identical(value, 5L)) {
    tempest_product_unsupported_format_abort(
      "ResearchWorkspace snapshot format",
      value,
      tempest_persistence_error_class(
        "tempest_research_workspace_restore_error"
      )
    )
  }
  value
}

#' @keywords internal
tempest_research_workspace_require_current_schema <- function(
  snapshot,
  what,
  class
) {
  if (!is.list(snapshot) || is.data.frame(snapshot)) {
    tempest_abort(
      "{what} must be a ResearchWorkspace snapshot record.",
      class = class
    )
  }
  schema_version <- tempest_persistence_schema_version(
    snapshot$schema_version %||% NA_integer_,
    paste0(what, " schema version"),
    class
  )
  if (!identical(schema_version, 5L)) {
    tempest_product_unsupported_format_abort(
      paste0(what, " format"),
      schema_version,
      class
    )
  }
  invisible(schema_version)
}

#' @keywords internal
tempest_research_workspace_restore_max_sources <- function(value) {
  if (identical(value, "unbounded")) {
    return(Inf)
  }
  tempest_exact_integer_scalar(
    value,
    "Workspace field max_sources",
    class = tempest_persistence_error_class(
      "tempest_research_workspace_restore_error"
    ),
    minimum = 1L
  )
}

#' @keywords internal
tempest_research_workspace_restore_records <- function(
  snapshot,
  field
) {
  if (!field %in% names(snapshot)) {
    tempest_research_workspace_restore_abort(
      "Workspace snapshot is missing required field {.field {field}}."
    )
  }
  records <- snapshot[[field]]
  if (is.null(records) || !is.list(records) || is.data.frame(records)) {
    tempest_research_workspace_restore_abort(
      "Workspace field {.field {field}} must be a list of records."
    )
  }
  records
}

#' @keywords internal
tempest_research_workspace_record_fields <- function(field) {
  switch(
    field,
    retrieved_resources = tempest_resource_record_fields(),
    proposed_claims = tempest_claim_record_fields(),
    evidence_spans = tempest_evidence_span_record_fields(),
    claim_supports = c(
      "claim_support_id",
      "claim_id",
      "evidence_span_id",
      "source_id",
      "verification_status",
      "support_score",
      "rationale"
    ),
    disputes = tempest_dispute_record_fields(),
    tempest_research_workspace_restore_abort(
      "Unknown workspace record field {.field {field}}."
    )
  )
}

#' @keywords internal
tempest_research_workspace_exact_records <- function(records, field) {
  fields <- tempest_research_workspace_record_fields(field)
  ordered <- is.list(records) &&
    !is.data.frame(records) &&
    is.null(names(records)) &&
    all(vapply(
      records,
      \(record) identical(names(record), fields),
      logical(1)
    ))
  if (!isTRUE(ordered)) {
    tempest_research_workspace_restore_abort(paste0(
      "Workspace ",
      gsub("_", "-", field),
      " records must use the exact current field order."
    ))
  }
  tempest_persistence_exact_records(
    records,
    fields,
    paste0("workspace ", gsub("_", "-", field), " records"),
    tempest_persistence_error_class(
      "tempest_research_workspace_restore_error"
    )
  )
}

#' @keywords internal
tempest_research_workspace_record_string <- function(
  value,
  field,
  nullable = FALSE,
  non_empty = FALSE
) {
  if (is.null(value) && isTRUE(nullable)) {
    return(NULL)
  }
  if (
    !rlang::is_string(value) ||
      is.na(value) ||
      (isTRUE(non_empty) && !nzchar(tempest_trim(value)))
  ) {
    tempest_research_workspace_restore_abort(paste0(
      "Workspace record field ",
      field,
      " must be ",
      if (nullable) "NULL or " else "",
      "one ",
      if (non_empty) "non-empty " else "",
      "string."
    ))
  }
  value
}

#' @keywords internal
tempest_research_workspace_record_string_array <- function(
  value,
  field,
  ids = FALSE
) {
  values <- if (
    is.list(value) && !is.data.frame(value) && is.null(names(value))
  ) {
    if (length(value) == 0L) {
      character()
    } else {
      valid <- vapply(
        value,
        \(item) rlang::is_string(item) && !is.na(item),
        logical(1)
      )
      if (!all(valid)) {
        tempest_research_workspace_restore_abort(paste0(
          "Workspace record field ",
          field,
          " must be a flat string array."
        ))
      }
      unlist(value, use.names = FALSE)
    }
  } else {
    tempest_research_workspace_restore_abort(paste0(
      "Workspace record field ",
      field,
      " must be a flat string array."
    ))
  }
  if (
    anyNA(values) ||
      (isTRUE(ids) &&
        (any(!nzchar(values)) ||
          !identical(values, tempest_trim(values)) ||
          anyDuplicated(values)))
  ) {
    tempest_research_workspace_restore_abort(paste0(
      "Workspace record field ",
      field,
      " contains invalid or duplicate identifiers."
    ))
  }
  unname(values)
}

#' @keywords internal
tempest_research_workspace_record_number <- function(
  value,
  field,
  nullable = FALSE,
  integer = FALSE,
  minimum = -Inf,
  maximum = Inf
) {
  if (is.null(value) && isTRUE(nullable)) {
    return(NULL)
  }
  valid <- is.numeric(value) &&
    length(value) == 1L &&
    !is.na(value) &&
    is.finite(value) &&
    value >= minimum &&
    value <= maximum &&
    (!isTRUE(integer) ||
      tempest_exact_integer_scalar_valid(
        value,
        minimum = max(minimum, -.Machine$integer.max),
        maximum = min(maximum, .Machine$integer.max)
      ))
  if (!isTRUE(valid)) {
    tempest_research_workspace_restore_abort(paste0(
      "Workspace record field ",
      field,
      " must be ",
      if (nullable) "NULL or " else "",
      if (integer) "one exact integer." else "one finite number."
    ))
  }
  value
}

#' @keywords internal
tempest_research_workspace_record_timestamp <- function(
  value,
  field,
  nullable = FALSE
) {
  value <- tempest_research_workspace_record_string(
    value,
    field,
    nullable = nullable,
    non_empty = TRUE
  )
  if (is.null(value)) {
    return(NULL)
  }
  parsed <- suppressWarnings(as.POSIXct(value, tz = "UTC"))
  if (is.na(parsed)) {
    tempest_research_workspace_restore_abort(paste0(
      "Workspace record field ",
      field,
      " must be a valid timestamp."
    ))
  }
  value
}

#' @keywords internal
tempest_research_workspace_validate_claim_record <- function(record) {
  for (field in c(
    "claim_id",
    "claim_text",
    "claim_type",
    "confidence",
    "verification_status"
  )) {
    record[[field]] <- tempest_research_workspace_record_string(
      record[[field]],
      field,
      non_empty = TRUE
    )
  }
  for (field in c(
    "contradiction_note",
    "retrieval_query",
    "retrieval_step_id",
    "perspective_id",
    "expert_id",
    "session_id",
    "section_id",
    "verifier_model"
  )) {
    record[field] <- list(tempest_research_workspace_record_string(
      record[[field]],
      field,
      nullable = TRUE
    ))
  }
  record$created_at <- tempest_research_workspace_record_timestamp(
    record$created_at,
    "created_at"
  )
  record["verified_at"] <- list(tempest_research_workspace_record_timestamp(
    record$verified_at,
    "verified_at",
    nullable = TRUE
  ))
  for (field in c(
    "source_ids",
    "evidence_span_ids",
    "contradicting_source_ids"
  )) {
    record[[field]] <- tempest_research_workspace_record_string_array(
      record[[field]],
      field,
      ids = TRUE
    )
  }
  quotes <- record$supporting_quotes
  if (!is.list(quotes) || is.data.frame(quotes) || !is.null(names(quotes))) {
    tempest_research_workspace_restore_abort(
      "Workspace record field supporting_quotes must be a flat string array."
    )
  }
  tempest_research_workspace_record_string_array(quotes, "supporting_quotes")
  for (field in c(
    "support_score",
    "contradiction_score",
    "source_quality_score"
  )) {
    record[field] <- list(tempest_research_workspace_record_number(
      record[[field]],
      field,
      nullable = TRUE,
      minimum = 0,
      maximum = 1
    ))
  }
  record
}

#' @keywords internal
tempest_research_workspace_validate_span_record <- function(record) {
  for (field in c("evidence_span_id", "source_id", "extracted_by")) {
    record[[field]] <- tempest_research_workspace_record_string(
      record[[field]],
      field,
      non_empty = TRUE
    )
  }
  for (field in c("chunk_id", "quote", "section_heading")) {
    record[field] <- list(tempest_research_workspace_record_string(
      record[[field]],
      field,
      nullable = TRUE
    ))
  }
  record$created_at <- tempest_research_workspace_record_timestamp(
    record$created_at,
    "created_at"
  )
  for (field in c("start_offset", "end_offset", "page")) {
    record[field] <- list(tempest_research_workspace_record_number(
      record[[field]],
      field,
      nullable = TRUE,
      integer = TRUE
    ))
  }
  record["relevance_score"] <- list(tempest_research_workspace_record_number(
    record$relevance_score,
    "relevance_score",
    nullable = TRUE,
    minimum = 0,
    maximum = 1
  ))
  record
}

#' @keywords internal
tempest_research_workspace_validate_dispute_record <- function(record) {
  for (field in c("dispute_id", "topic", "evidence_balance")) {
    record[[field]] <- tempest_research_workspace_record_string(
      record[[field]],
      field,
      non_empty = TRUE
    )
  }
  for (field in c("axis_of_disagreement", "likely_explanation")) {
    record[field] <- list(tempest_research_workspace_record_string(
      record[[field]],
      field,
      nullable = TRUE
    ))
  }
  record$created_at <- tempest_research_workspace_record_timestamp(
    record$created_at,
    "created_at"
  )
  record$claim_ids <- tempest_research_workspace_record_string_array(
    record$claim_ids,
    "claim_ids",
    ids = TRUE
  )
  record$unresolved_questions <- tempest_research_workspace_record_string_array(
    record$unresolved_questions,
    "unresolved_questions"
  )
  record
}

#' @keywords internal
tempest_research_workspace_unique_record_ids <- function(
  records,
  id_field,
  what
) {
  ids <- vapply(
    records,
    function(record) {
      id <- record[[id_field]]
      if (!rlang::is_string(id) || is.na(id) || !nzchar(tempest_trim(id))) {
        tempest_research_workspace_restore_abort(paste0(
          what,
          " contains an invalid ",
          id_field,
          "."
        ))
      }
      id
    },
    character(1)
  )
  if (anyDuplicated(ids)) {
    tempest_research_workspace_restore_abort(paste0(
      what,
      " contains duplicate ",
      id_field,
      " values."
    ))
  }
  invisible(ids)
}

#' @keywords internal
tempest_research_workspace_restore_metadata <- function(snapshot) {
  if ("artifacts" %in% names(snapshot)) {
    tempest_research_workspace_restore_abort(
      "Current workspaces cannot contain an arbitrary artifacts field."
    )
  }
  base_snapshot_id <- tryCatch(
    tempest_research_workspace_snapshot_id(snapshot$base_snapshot_id),
    error = function(error) {
      tempest_research_workspace_restore_abort(
        "{.field base_snapshot_id} is invalid.",
        parent = error
      )
    }
  )
  if (is.null(snapshot$accepted_graft_references)) {
    tempest_research_workspace_restore_abort(
      "{.field accepted_graft_references} cannot be literal null."
    )
  }
  references <- tryCatch(
    tempest_research_workspace_references(
      snapshot$accepted_graft_references
    ),
    error = function(error) {
      tempest_research_workspace_restore_abort(
        "{.field accepted_graft_references} is invalid.",
        parent = error
      )
    }
  )
  max_sources <- tempest_research_workspace_restore_max_sources(
    snapshot$max_sources
  )

  list(
    base_snapshot_id = base_snapshot_id,
    max_sources = max_sources,
    accepted_graft_references = references
  )
}

#' @keywords internal
tempest_research_workspace_restore <- function(
  snapshot,
  workspace = NULL,
  graft_snapshot = NULL
) {
  graft_snapshot_missing <- missing(graft_snapshot)
  if (!is.list(snapshot)) {
    tempest_abort(
      "{.arg snapshot} must be a list.",
      class = tempest_persistence_error_class(
        "tempest_research_workspace_restore_error"
      )
    )
  }
  if (!is.null(workspace) && !inherits(workspace, "ResearchWorkspace")) {
    tempest_abort(
      "{.arg workspace} must be a ResearchWorkspace or `NULL`.",
      class = tempest_persistence_error_class(
        "tempest_research_workspace_restore_error"
      )
    )
  }
  if (graft_snapshot_missing && !is.null(workspace)) {
    graft_snapshot <- workspace$graft_snapshot
  }
  schema_version <- tempest_research_workspace_restore_schema(snapshot)
  expected_fields <- tempest_research_workspace_snapshot_fields()
  snapshot_fields <- names(snapshot)
  if (!identical(snapshot_fields, expected_fields)) {
    tempest_product_unsupported_format_abort(
      "ResearchWorkspace snapshot format",
      schema_version,
      tempest_persistence_error_class(
        "tempest_research_workspace_restore_error"
      )
    )
  }
  tempest_persistence_credential_audit(
    list(workspace = snapshot),
    "ResearchWorkspace snapshot",
    tempest_persistence_error_class(
      "tempest_research_workspace_restore_error"
    )
  )
  metadata <- tempest_research_workspace_restore_metadata(snapshot)
  graft_snapshot <- tryCatch(
    tempest_research_workspace_graft_snapshot(
      graft_snapshot,
      metadata$base_snapshot_id
    ),
    error = function(error) {
      tempest_research_workspace_restore_abort(
        "The retained Graft snapshot is invalid.",
        parent = error
      )
    }
  )
  candidate <- tempest_research_workspace(
    base_snapshot_id = metadata$base_snapshot_id,
    graft_snapshot = graft_snapshot,
    max_sources = metadata$max_sources,
    accepted_graft_references = metadata$accepted_graft_references
  )
  if (!is.null(workspace)) {
    if (!identical(workspace$base_snapshot_id, metadata$base_snapshot_id)) {
      tempest_research_workspace_restore_abort(
        "{.field base_snapshot_id} does not match the pinned workspace."
      )
    }
    workspace_references <- workspace$list_accepted_graft_references()
    workspace_reference_keys <- vapply(
      workspace_references,
      tempest_research_workspace_reference_json,
      character(1)
    )
    metadata_reference_keys <- vapply(
      metadata$accepted_graft_references,
      tempest_research_workspace_reference_json,
      character(1)
    )
    if (
      length(
        setdiff(workspace_reference_keys, metadata_reference_keys)
      ) >
        0L
    ) {
      tempest_research_workspace_restore_abort(
        paste0(
          "{.field accepted_graft_references} contain identities outside ",
          "the pinned workspace snapshot."
        )
      )
    }
    workspace_graft <- tryCatch(
      tempest_research_workspace_graft_snapshot(
        workspace$graft_snapshot,
        metadata$base_snapshot_id
      ),
      error = function(error) {
        tempest_research_workspace_restore_abort(
          "The supplied workspace Graft snapshot is invalid.",
          parent = error
        )
      }
    )
    if (
      !identical(
        lapply(
          tempest_graft_snapshot_field_names(),
          \(field) {
            if (is.null(workspace_graft)) {
              NULL
            } else {
              S7::prop(workspace_graft, field)
            }
          }
        ),
        lapply(
          tempest_graft_snapshot_field_names(),
          \(field) {
            if (is.null(graft_snapshot)) {
              NULL
            } else {
              S7::prop(graft_snapshot, field)
            }
          }
        )
      )
    ) {
      tempest_research_workspace_restore_abort(
        "The retained Graft snapshot does not match the supplied workspace."
      )
    }
  }

  records <- tempest_research_workspace_restore_records(
    snapshot,
    "retrieved_resources"
  )
  records <- tempest_research_workspace_exact_records(
    records,
    "retrieved_resources"
  )
  tempest_research_workspace_unique_record_ids(
    records,
    "resource_id",
    "Retrieved-resource records"
  )
  for (i in seq_along(records)) {
    resource <- tryCatch(
      tempest_resource_from_data(records[[i]]),
      error = function(error) {
        tempest_research_workspace_restore_abort(
          paste0("Retrieved-resource entry ", i, " is invalid."),
          parent = error
        )
      }
    )
    tryCatch(
      candidate$upsert_retrieved_resource(resource),
      error = function(error) {
        tempest_research_workspace_restore_abort(
          paste0("Retrieved-resource entry ", i, " cannot be restored."),
          parent = error
        )
      }
    )
  }

  source_ids <- purrr::map_chr(
    candidate$list_retrieved_resources(),
    tempest_resource_identity
  )
  span_records <- tempest_research_workspace_restore_records(
    snapshot,
    "evidence_spans"
  )
  span_records <- tempest_research_workspace_exact_records(
    span_records,
    "evidence_spans"
  )
  span_records <- lapply(
    span_records,
    tempest_research_workspace_validate_span_record
  )
  tempest_research_workspace_unique_record_ids(
    span_records,
    "evidence_span_id",
    "Evidence-span records"
  )
  for (i in seq_along(span_records)) {
    span <- tryCatch(
      tempest_evidence_span_from_list(span_records[[i]]),
      error = function(error) {
        tempest_research_workspace_restore_abort(
          paste0("Evidence-span entry ", i, " is invalid."),
          parent = error
        )
      }
    )
    if (!(span@source_id %in% source_ids)) {
      tempest_research_workspace_restore_abort(
        paste0(
          "Evidence span ",
          span@evidence_span_id,
          " cites unknown source id: ",
          span@source_id,
          "."
        )
      )
    }
    candidate$add_evidence_span(span)
  }

  claim_records <- tempest_research_workspace_restore_records(
    snapshot,
    "proposed_claims"
  )
  claim_records <- tempest_research_workspace_exact_records(
    claim_records,
    "proposed_claims"
  )
  claim_records <- lapply(
    claim_records,
    tempest_research_workspace_validate_claim_record
  )
  tempest_research_workspace_unique_record_ids(
    claim_records,
    "claim_id",
    "Proposed-claim records"
  )
  restored_claims <- list()
  for (i in seq_along(claim_records)) {
    claim <- tryCatch(
      tempest_claim_from_list(claim_records[[i]]),
      error = function(error) {
        tempest_research_workspace_restore_abort(
          paste0("Proposed-claim entry ", i, " is invalid."),
          parent = error
        )
      }
    )
    restored_claims[[length(restored_claims) + 1L]] <- claim
    missing_source_ids <- setdiff(claim@source_ids, source_ids)
    if (length(missing_source_ids) > 0) {
      tempest_research_workspace_restore_abort(
        paste0(
          "Claim ",
          claim@claim_id,
          " cites unknown source id(s): ",
          paste(missing_source_ids, collapse = ", "),
          "."
        )
      )
    }
    tryCatch(
      candidate$add_proposed_claim(claim),
      error = function(error) {
        tempest_research_workspace_restore_abort(
          paste0("Proposed claim ", claim@claim_id, " cannot be restored."),
          parent = error
        )
      }
    )
  }

  claim_ids <- purrr::map_chr(
    candidate$list_proposed_claims(),
    ~ .x@claim_id
  )
  dispute_records <- tempest_research_workspace_restore_records(
    snapshot,
    "disputes"
  )
  dispute_records <- tempest_research_workspace_exact_records(
    dispute_records,
    "disputes"
  )
  dispute_records <- lapply(
    dispute_records,
    tempest_research_workspace_validate_dispute_record
  )
  tempest_research_workspace_unique_record_ids(
    dispute_records,
    "dispute_id",
    "Dispute records"
  )
  for (i in seq_along(dispute_records)) {
    dispute <- tryCatch(
      tempest_dispute_from_list(dispute_records[[i]]),
      error = function(error) {
        tempest_research_workspace_restore_abort(
          paste0("Dispute entry ", i, " is invalid."),
          parent = error
        )
      }
    )
    missing_claim_ids <- setdiff(dispute@claim_ids, claim_ids)
    if (length(missing_claim_ids) > 0) {
      tempest_research_workspace_restore_abort(
        paste0(
          "Dispute ",
          dispute@dispute_id,
          " cites unknown claim id(s): ",
          paste(missing_claim_ids, collapse = ", "),
          "."
        )
      )
    }
    candidate$add_dispute(dispute)
  }

  support_records <- tempest_research_workspace_restore_records(
    snapshot,
    "claim_supports"
  )
  support_records <- tempest_research_workspace_exact_records(
    support_records,
    "claim_supports"
  )
  tempest_research_workspace_unique_record_ids(
    support_records,
    "claim_support_id",
    "Claim-support records"
  )
  supports <- lapply(seq_along(support_records), function(i) {
    tryCatch(
      tempest_claim_support_from_list(support_records[[i]]),
      error = function(error) {
        tempest_research_workspace_restore_abort(
          paste0("Claim-support entry ", i, " is invalid."),
          parent = error
        )
      }
    )
  })
  if (length(supports) == 0L) {
    exact_unverified <- all(vapply(
      restored_claims,
      function(claim) {
        identical(candidate$get_proposed_claim(claim@claim_id), claim)
      },
      logical(1)
    ))
    if (!isTRUE(exact_unverified)) {
      tempest_research_workspace_restore_abort(
        paste0(
          "Verified claim summaries require the complete authoritative ",
          "claim-support set."
        )
      )
    }
  } else {
    verifier_models <- unique(vapply(
      restored_claims,
      \(claim) claim@verifier_model,
      character(1)
    ))
    verified_times <- unique(vapply(
      restored_claims,
      \(claim) claim@verified_at,
      character(1)
    ))
    if (
      length(verifier_models) != 1L ||
        !tempest_ledger_identifier_valid(
          verifier_models[[1]],
          optional = TRUE
        ) ||
        length(verified_times) != 1L ||
        !tempest_ledger_timestamp_valid(verified_times[[1]])
    ) {
      tempest_research_workspace_restore_abort(
        paste0(
          "A complete claim-support set requires one exact verifier and ",
          "verification timestamp across all claim summaries."
        )
      )
    }
    tryCatch(
      candidate$verify_proposed_claims_batch(
        supports,
        verified_at = verified_times[[1]],
        min_support_score = 0,
        verifier = verifier_models[[1]]
      ),
      error = function(error) {
        tempest_research_workspace_restore_abort(
          "{.field claim_supports} cannot be restored.",
          parent = error
        )
      }
    )
    restored_claims_value <- new.env(parent = emptyenv())
    for (claim in restored_claims) {
      derived <- candidate$get_proposed_claim(claim@claim_id)
      if (!identical(derived, claim)) {
        tempest_research_workspace_restore_abort(
          paste0(
            "Claim summary {.val {claim@claim_id}} is not exactly derived ",
            "from the authoritative claim-support set."
          )
        )
      }
      restored_claims_value[[claim@claim_id]] <- claim
    }
    candidate$.__enclos_env__$private$claims_value <- restored_claims_value
  }

  tryCatch(
    candidate$validate_integrity(),
    error = function(error) {
      tempest_research_workspace_restore_abort(
        "Restored claim-support integrity validation failed.",
        parent = error
      )
    }
  )
  tempest_research_workspace_validate_noncontent(candidate, "restore")

  if (is.null(workspace)) {
    return(candidate)
  }
  target_private <- workspace$.__enclos_env__$private
  source_private <- candidate$.__enclos_env__$private
  fields <- c(
    "resources_value",
    "claims_value",
    "evidence_spans_value",
    "claim_supports_value",
    "disputes_value",
    "max_sources_value",
    "claims_by_source",
    "base_snapshot_id_value",
    "graft_snapshot_value",
    "accepted_graft_references_value",
    "citation_audit_value"
  )
  for (field in fields) {
    target_private[[field]] <- source_private[[field]]
  }
  workspace
}
