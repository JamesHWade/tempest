# Explicit product-owned stage execution records

tempest_stage_abort <- function(
  message,
  ...,
  class = NULL,
  parent = NULL,
  .envir = rlang::caller_env()
) {
  tempest_abort(
    message,
    ...,
    class = c(class, "tempest_stage_error", "tempest_error"),
    parent = parent,
    .envir = .envir
  )
}

tempest_stage_record_abort <- function(message, ..., parent = NULL) {
  tempest_stage_abort(
    message,
    ...,
    class = "tempest_stage_record_error",
    parent = parent,
    .envir = rlang::caller_env()
  )
}

tempest_stage_lifecycle_abort <- function(message, ..., parent = NULL) {
  tempest_stage_abort(
    message,
    ...,
    class = "tempest_stage_lifecycle_error",
    parent = parent,
    .envir = rlang::caller_env()
  )
}

tempest_stage_statuses <- function() {
  c("running", "succeeded", "failed", "cancelled")
}

tempest_stage_fallback_policies <- function() {
  c("fail_closed", "exploratory_allowed", "grounded_only")
}

tempest_stage_output_kinds <- function() {
  c("state_field", "workspace_claims", "claim_supports", "content_digest")
}

tempest_stage_record_fields <- function() {
  c(
    "stage",
    "attempt_id",
    "status",
    "output_reference",
    "program_artifact_id",
    "governed_procedure_revision_id",
    "trace_references",
    "started_at",
    "completed_at",
    "failure_class",
    "failure_message",
    "fallback_policy",
    "fallback_implementation",
    "fallback_taken",
    "execution_path",
    "support_status",
    "publication_allowed"
  )
}

tempest_stage_policy_table <- function() {
  list(
    perspectives = list(
      fallback_policy = "exploratory_allowed",
      fallback_implementation = "tempest::fallback/perspectives/ellmer-structured@1",
      execution_path = "exploratory",
      requires_verified_evidence = FALSE
    ),
    personas = list(
      fallback_policy = "exploratory_allowed",
      fallback_implementation = "tempest::fallback/personas/ellmer-structured@1",
      execution_path = "exploratory",
      requires_verified_evidence = FALSE
    ),
    query_decomposition = list(
      fallback_policy = "exploratory_allowed",
      fallback_implementation = paste0(
        "tempest::fallback/query-decomposition/",
        "original-question@1"
      ),
      execution_path = "exploratory",
      requires_verified_evidence = FALSE
    ),
    extract_claims = list(
      fallback_policy = "fail_closed",
      fallback_implementation = NA_character_,
      execution_path = "grounded",
      requires_verified_evidence = FALSE
    ),
    verify_claim_support = list(
      fallback_policy = "fail_closed",
      fallback_implementation = NA_character_,
      execution_path = "grounded",
      requires_verified_evidence = FALSE
    ),
    next_question = list(
      fallback_policy = "exploratory_allowed",
      fallback_implementation = "tempest::fallback/next-question/ellmer-structured@1",
      execution_path = "exploratory",
      requires_verified_evidence = FALSE
    ),
    draft_outline = list(
      fallback_policy = "exploratory_allowed",
      fallback_implementation = "tempest::fallback/draft-outline/ellmer-structured@1",
      execution_path = "exploratory",
      requires_verified_evidence = FALSE
    ),
    refined_outline = list(
      fallback_policy = "grounded_only",
      fallback_implementation = "tempest::fallback/refined-outline/ellmer-structured@1",
      execution_path = "grounded",
      requires_verified_evidence = TRUE
    ),
    section_writing = list(
      fallback_policy = "grounded_only",
      fallback_implementation = "tempest::fallback/section-writing/briefing-items@1",
      execution_path = "grounded",
      requires_verified_evidence = TRUE
    ),
    lead_section = list(
      fallback_policy = "grounded_only",
      fallback_implementation = "tempest::fallback/lead-section/briefing-items@1",
      execution_path = "grounded",
      requires_verified_evidence = TRUE
    )
  )
}

tempest_stage_policy <- function(stage) {
  if (!rlang::is_string(stage) || !stage %in% tempest_program_set_stages()) {
    tempest_stage_record_abort(
      "{.arg stage} must identify an exact Tempest ProgramSet stage."
    )
  }
  policy <- tempest_stage_policy_table()[[stage]]
  c(list(stage = stage), policy)
}

tempest_stage_failure_message <- function(
  error = NULL,
  kind = c("execution", "validation", "fallback", "commit", "cancelled")
) {
  kind <- match.arg(kind)
  unname(tempest_stage_failure_messages()[[kind]])
}

tempest_stage_failure_classes <- function() {
  c(
    "tempest_stage_execution_error",
    "tempest_stage_output_validation_error",
    "tempest_stage_fallback_error",
    "tempest_stage_commit_error",
    "tempest_stage_cancelled",
    "tempest_stage_evaluator_contract_error",
    "tempest_stage_governance_error",
    "tempest_program_set_verification_error",
    "tempest_ecosystem_contract_error",
    "dsprrr_trace_contract_error",
    "dsprrr_program_trace_contract_error",
    "dsprrr_artifact_integrity_error"
  )
}

tempest_stage_failure_class <- function(error = NULL, kind = "execution") {
  defaults <- c(
    execution = "tempest_stage_execution_error",
    validation = "tempest_stage_output_validation_error",
    fallback = "tempest_stage_fallback_error",
    commit = "tempest_stage_commit_error",
    cancelled = "tempest_stage_cancelled"
  )
  if (!kind %in% names(defaults)) {
    tempest_stage_record_abort("Unknown stage failure kind {.val {kind}}.")
  }
  if (identical(kind, "execution") && !is.null(error)) {
    known <- intersect(class(error), tempest_stage_failure_classes())
    if (length(known) > 0L) {
      return(known[[1]])
    }
  }
  unname(defaults[[kind]])
}

tempest_stage_optional_string <- function(value, arg) {
  if (is.null(value) || length(value) == 0L) {
    return(NA_character_)
  }
  if (is.character(value) && length(value) == 1L && is.na(value)) {
    return(NA_character_)
  }
  if (!rlang::is_string(value) || !nzchar(tempest_trim(value))) {
    tempest_stage_record_abort(
      "{.arg {arg}} must be `NULL`, `NA`, or a single non-empty string."
    )
  }
  tempest_trim(value)
}

tempest_stage_required_string <- function(value, arg) {
  if (
    !rlang::is_string(value) ||
      is.na(value) ||
      !nzchar(tempest_trim(value))
  ) {
    tempest_stage_record_abort(
      "{.arg {arg}} must be a single non-empty string."
    )
  }
  tempest_trim(value)
}

tempest_stage_output_reference <- function(
  kind,
  ids = character(),
  content_digest = NULL
) {
  kind <- tempest_stage_required_string(kind, "kind")
  if (!kind %in% tempest_stage_output_kinds()) {
    tempest_stage_record_abort(
      "{.arg kind} must be one of {.val {tempest_stage_output_kinds()}}."
    )
  }
  if (is.list(ids)) {
    if (
      !is.null(names(ids)) ||
        !all(vapply(ids, rlang::is_string, logical(1)))
    ) {
      tempest_stage_record_abort("{.arg ids} must be an unnamed collection.")
    }
    ids <- unlist(ids, use.names = FALSE)
  }
  if (
    !is.character(ids) ||
      is.object(ids) ||
      !is.null(names(ids)) ||
      anyNA(ids) ||
      any(!nzchar(tempest_trim(ids)))
  ) {
    if (length(ids) > 0L) {
      tempest_stage_record_abort(
        "{.arg ids} must contain only non-empty strings."
      )
    }
  }
  ids <- tempest_trim(ids)
  if (anyDuplicated(ids)) {
    tempest_stage_record_abort("{.arg ids} must not contain duplicates.")
  }
  invalid_ids <- !vapply(
    ids,
    tempest_opaque_identifier_valid,
    logical(1)
  )
  if (any(invalid_ids)) {
    tempest_stage_record_abort(
      paste0(
        "{.arg ids} must contain only bounded opaque identifiers, not ",
        "prose or credentials."
      )
    )
  }
  if (
    identical(kind, "content_digest") &&
      (length(ids) != 1L || !grepl("^sha256:[a-f0-9]{64}$", ids))
  ) {
    tempest_stage_record_abort(
      "A content-digest output reference must contain one SHA-256 ID."
    )
  }
  if (identical(kind, "content_digest") && is.null(content_digest)) {
    content_digest <- ids[[1]]
  }
  content_digest <- tempest_stage_optional_string(
    content_digest,
    "content_digest"
  )
  if (
    is.na(content_digest) ||
      !grepl("^sha256:[a-f0-9]{64}$", content_digest)
  ) {
    tempest_stage_record_abort(
      "Every output reference requires an exact SHA-256 content digest."
    )
  }
  if (
    identical(kind, "content_digest") &&
      !identical(content_digest, ids[[1]])
  ) {
    tempest_stage_record_abort(
      "A content-digest reference must repeat its exact SHA-256 ID."
    )
  }
  list(
    kind = kind,
    ids = unname(as.list(ids)),
    content_digest = if (is.na(content_digest)) NULL else content_digest
  )
}

tempest_stage_output_reference_validate <- function(
  value,
  allow_empty = FALSE
) {
  if (is.null(value) || length(value) == 0L) {
    if (allow_empty) {
      return(list())
    }
    tempest_stage_record_abort(
      "A succeeded stage requires an output reference."
    )
  }
  fields <- names(value)
  if (
    !is.list(value) ||
      is.data.frame(value) ||
      is.null(fields) ||
      anyNA(fields) ||
      anyDuplicated(fields) ||
      !setequal(fields, c("kind", "ids", "content_digest"))
  ) {
    tempest_stage_record_abort(
      paste0(
        "{.arg output_reference} must contain exactly {.field kind}, ",
        "{.field ids}, and {.field content_digest}."
      )
    )
  }
  canonical <- tempest_stage_output_reference(
    value$kind,
    value$ids,
    content_digest = value$content_digest
  )
  if (!identical(value, canonical)) {
    tempest_stage_record_abort(
      paste0(
        "{.arg output_reference} must use the exact canonical record shape; ",
        "{.field ids} must be an unnamed list of scalar strings."
      )
    )
  }
  canonical
}

tempest_stage_trace_reference_fields <- function() {
  c(
    "research_run_id",
    "stage_attempt_id",
    "deputy_run_id",
    "deputy_session_id",
    "parent_run_id",
    "delegation_id",
    "tool_call_id",
    "trace_id",
    "expert_id",
    "correlation_id",
    "mode",
    "role",
    "min_support_score",
    "verified_at",
    "verifier_model",
    "governed_procedure",
    "evidence_claim_ids",
    "verified_evidence_claim_ids"
  )
}

tempest_stage_trace_reference_collection_fields <- function() {
  c("evidence_claim_ids", "verified_evidence_claim_ids")
}

tempest_stage_trace_identifier <- function(value, field) {
  if (!tempest_opaque_identifier_valid(value)) {
    tempest_stage_record_abort(
      paste0(
        "{.field trace_references$",
        field,
        "} must be a bounded opaque identifier, not prose or credentials."
      )
    )
  }
  value
}

tempest_stage_support_threshold_string <- function(value) {
  score <- tempest_normalize_min_support_score(value)
  if (identical(score, 0) || score == 0) {
    return("0")
  }
  for (digits in seq_len(17L)) {
    candidate <- sprintf("%.*g", digits, score)
    candidate <- sub("e\\+", "e", candidate, fixed = FALSE)
    if (identical(suppressWarnings(as.double(candidate)), score)) {
      return(candidate)
    }
  }
  tempest_stage_record_abort(
    "{.arg min_support_score} cannot be represented canonically."
  )
}

tempest_stage_support_threshold_value <- function(value) {
  if (
    !rlang::is_string(value) ||
      is.na(value) ||
      !nzchar(value)
  ) {
    tempest_stage_record_abort(
      paste0(
        "{.field trace_references$min_support_score} must be one canonical ",
        "decimal string."
      )
    )
  }
  score <- suppressWarnings(as.double(value))
  score <- tryCatch(
    tempest_normalize_min_support_score(score),
    error = function(error) {
      tempest_stage_record_abort(
        paste0(
          "{.field trace_references$min_support_score} must be in [0, 1]."
        ),
        parent = error
      )
    }
  )
  if (!identical(value, tempest_stage_support_threshold_string(score))) {
    tempest_stage_record_abort(
      paste0(
        "{.field trace_references$min_support_score} must retain its exact ",
        "canonical decimal form."
      )
    )
  }
  score
}

tempest_stage_trace_references <- function(value, attempt_id) {
  if (is.null(value)) {
    value <- list()
  }
  if (!is.list(value) || is.data.frame(value)) {
    tempest_stage_record_abort("{.arg trace_references} must be a list.")
  }
  fields <- names(value)
  if (
    length(value) > 0L &&
      (is.null(fields) ||
        anyNA(fields) ||
        any(!nzchar(fields)) ||
        anyDuplicated(fields))
  ) {
    tempest_stage_record_abort(
      "{.arg trace_references} must be an exact named record."
    )
  }
  unknown <- setdiff(
    fields %||% character(),
    tempest_stage_trace_reference_fields()
  )
  if (length(unknown) > 0L) {
    tempest_stage_record_abort(
      "{.arg trace_references} contains unsupported fields: {.field {unknown}}."
    )
  }
  if (is.null(value$stage_attempt_id)) {
    value$stage_attempt_id <- attempt_id
  }
  if (is.null(value$trace_id)) {
    value$trace_id <- attempt_id
  }
  collections <- tempest_stage_trace_reference_collection_fields()
  for (field in names(value)) {
    item <- value[[field]]
    if (identical(field, "min_support_score")) {
      tempest_stage_support_threshold_value(item)
      value[[field]] <- item
    } else if (identical(field, "verified_at")) {
      if (!tempest_ledger_timestamp_valid(item)) {
        tempest_stage_record_abort(
          paste0(
            "{.field trace_references$verified_at} must be one exact ",
            "canonical UTC timestamp."
          )
        )
      }
      value[[field]] <- item
    } else if (identical(field, "governed_procedure")) {
      binding <- tryCatch(
        {
          binding_fields <- c(
            "kind",
            tempest_governed_procedure_fields()
          )
          if (
            !is.list(item) ||
              is.data.frame(item) ||
              is.null(names(item)) ||
              anyNA(names(item)) ||
              anyDuplicated(names(item)) ||
              !setequal(names(item), binding_fields) ||
              !identical(item$kind, "governed_procedure")
          ) {
            stop("invalid governed-procedure trace shape")
          }
          reference <- item[tempest_governed_procedure_fields()]
          tempest_governed_procedure_trace_binding(reference)
        },
        error = function(error) {
          tempest_stage_record_abort(
            paste0(
              "{.field trace_references$governed_procedure} must be the ",
              "exact governed-procedure proof."
            ),
            parent = error
          )
        }
      )
      value[[field]] <- binding
    } else if (field %in% collections) {
      if (is.character(item) && !is.object(item) && is.null(names(item))) {
        item <- as.list(item)
      }
      valid <- is.list(item) &&
        !is.data.frame(item) &&
        is.null(names(item)) &&
        all(vapply(item, rlang::is_string, logical(1)))
      if (!valid) {
        tempest_stage_record_abort(
          "{.field trace_references${field}} must be a flat string array."
        )
      }
      ids <- if (length(item) == 0L) {
        character()
      } else {
        unlist(item, use.names = FALSE)
      }
      ids <- tempest_trim(ids)
      if (anyNA(ids) || any(!nzchar(ids)) || anyDuplicated(ids)) {
        tempest_stage_record_abort(
          paste0(
            "{.field trace_references$",
            field,
            "} must contain unique non-empty strings."
          )
        )
      }
      ids <- vapply(
        ids,
        tempest_stage_trace_identifier,
        character(1),
        field = field
      )
      value[[field]] <- unname(as.list(ids))
    } else {
      if (
        !rlang::is_string(item) || is.na(item) || !nzchar(tempest_trim(item))
      ) {
        tempest_stage_record_abort(
          "{.field trace_references${field}} must be a non-empty string."
        )
      }
      value[[field]] <- tempest_stage_trace_identifier(item, field)
    }
  }
  allowed <- tempest_stage_trace_reference_fields()
  value <- value[intersect(allowed, names(value))]
  value <- tryCatch(
    tempest_research_manifest_canonical_value(value, "trace_references"),
    error = function(error) {
      tempest_stage_record_abort(
        "{.arg trace_references} must be canonical and credential-free."
      )
    }
  )
  if (!identical(value$stage_attempt_id, attempt_id)) {
    tempest_stage_record_abort(
      "{.field trace_references$stage_attempt_id} must match {.arg attempt_id}."
    )
  }
  if (!identical(value$trace_id, attempt_id)) {
    tempest_stage_record_abort(
      "{.field trace_references$trace_id} must match {.arg attempt_id}."
    )
  }
  value
}

tempest_stage_time_parse <- function(value) {
  if (
    !rlang::is_string(value) ||
      is.na(value) ||
      !grepl(
        paste0(
          "^[0-9]{4}-[0-9]{2}-[0-9]{2}T",
          "[0-9]{2}:[0-9]{2}:[0-9]{2}(\\.[0-9]{1,6})?Z$"
        ),
        value
      )
  ) {
    return(as.POSIXct(NA, tz = "UTC"))
  }
  parsed <- tryCatch(
    suppressWarnings(as.POSIXct(
      value,
      format = "%Y-%m-%dT%H:%M:%OSZ",
      tz = "UTC"
    )),
    error = \(error) as.POSIXct(NA, tz = "UTC")
  )
  if (
    length(parsed) != 1L ||
      is.na(parsed) ||
      !identical(
        format(parsed, "%Y-%m-%dT%H:%M:%S", tz = "UTC"),
        substr(value, 1L, 19L)
      )
  ) {
    return(as.POSIXct(NA, tz = "UTC"))
  }
  parsed
}

tempest_stage_time <- function(value, arg, optional = FALSE) {
  if (optional && (is.null(value) || length(value) == 0L)) {
    return(NA_character_)
  }
  if (
    optional &&
      is.character(value) &&
      !is.object(value) &&
      is.null(names(value)) &&
      length(value) == 1L &&
      is.na(value)
  ) {
    return(NA_character_)
  }
  if (!rlang::is_string(value) || is.na(value)) {
    tempest_stage_record_abort(
      "{.arg {arg}} must be one canonical UTC timestamp."
    )
  }
  parsed <- tempest_stage_time_parse(value)
  if (length(parsed) != 1L || is.na(parsed)) {
    tempest_stage_record_abort(
      paste0(
        "{.arg {arg}} must use canonical UTC form ",
        "{.code YYYY-MM-DDTHH:MM:SS[.fraction]Z}."
      )
    )
  }
  value
}

tempest_stage_publication_allowed <- function(
  status,
  execution_path,
  support_status
) {
  identical(status, "succeeded") &&
    execution_path %in% c("governed", "grounded") &&
    identical(support_status, "verified")
}

tempest_stage_execution_path_derive <- function(
  status,
  policy,
  governed_procedure_revision_id,
  trace_references,
  fallback_taken
) {
  governed <- identical(status, "succeeded") &&
    !isTRUE(fallback_taken) &&
    !is.na(governed_procedure_revision_id) &&
    !is.null(trace_references$governed_procedure %||% NULL)
  if (isTRUE(governed)) "governed" else policy$execution_path
}

tempest_stage_record_validation_message <- function(self) {
  tryCatch(
    {
      tempest_stage_required_string(self@stage, "stage")
      tempest_stage_required_string(self@attempt_id, "attempt_id")
      tempest_stage_required_string(self@status, "status")
      if (!self@status %in% tempest_stage_statuses()) {
        stop("status must identify one exact lifecycle state")
      }
      tempest_research_manifest_program_artifact_id(
        self@program_artifact_id,
        "program_artifact_id"
      )
      tempest_stage_optional_string(
        self@governed_procedure_revision_id,
        "governed_procedure_revision_id"
      )
      tempest_stage_time(self@started_at, "started_at")
      tempest_stage_time(self@completed_at, "completed_at", optional = TRUE)
      tempest_stage_optional_string(self@failure_class, "failure_class")
      tempest_stage_optional_string(self@failure_message, "failure_message")
      tempest_stage_optional_string(
        self@fallback_implementation,
        "fallback_implementation"
      )
      tempest_stage_required_string(self@fallback_policy, "fallback_policy")
      tempest_stage_required_string(self@execution_path, "execution_path")
      tempest_stage_required_string(self@support_status, "support_status")
      if (!rlang::is_bool(self@fallback_taken)) {
        stop("fallback_taken must be one non-missing logical")
      }
      if (!rlang::is_bool(self@publication_allowed)) {
        stop("publication_allowed must be one non-missing logical")
      }
      policy <- tempest_stage_policy(self@stage)
      if (!identical(self@fallback_policy, policy$fallback_policy)) {
        stop("fallback_policy must match the exact stage policy")
      }
      expected_path <- tempest_stage_execution_path_derive(
        self@status,
        policy,
        self@governed_procedure_revision_id,
        self@trace_references,
        self@fallback_taken
      )
      if (!identical(self@execution_path, expected_path)) {
        stop("execution_path must be derived from exact terminal authority")
      }
      canonical_output_reference <- tempest_stage_output_reference_validate(
        self@output_reference,
        allow_empty = !identical(self@status, "succeeded")
      )
      if (!identical(self@output_reference, canonical_output_reference)) {
        stop("output_reference must retain its exact canonical stored shape")
      }
      if (identical(self@status, "succeeded")) {
        tempest_stage_output_reference_validate_stage(
          self@stage,
          self@output_reference
        )
      }
      canonical_trace_references <- tempest_stage_trace_references(
        self@trace_references,
        self@attempt_id
      )
      if (!identical(self@trace_references, canonical_trace_references)) {
        stop("trace_references must retain their exact canonical stored shape")
      }
      threshold <- self@trace_references$min_support_score %||% NULL
      verification_time <- self@trace_references$verified_at %||% NULL
      verifier_model <- self@trace_references$verifier_model %||% NULL
      if (
        identical(self@stage, "verify_claim_support") &&
          is.null(threshold)
      ) {
        stop("verification stages require the exact min_support_score trace")
      }
      if (
        !identical(self@stage, "verify_claim_support") &&
          !is.null(threshold)
      ) {
        stop("only verification stages can carry min_support_score traces")
      }
      if (
        identical(self@stage, "verify_claim_support") &&
          is.null(verification_time)
      ) {
        stop("verification stages require the exact verified_at trace")
      }
      if (
        !identical(self@stage, "verify_claim_support") &&
          (!is.null(verification_time) || !is.null(verifier_model))
      ) {
        stop("only verification stages can carry verifier provenance traces")
      }
      started <- tempest_stage_time_parse(self@started_at)
      if (is.na(started)) {
        stop("started_at must be a valid UTC timestamp")
      }
      terminal <- !identical(self@status, "running")
      if (terminal) {
        completed <- tempest_stage_time_parse(self@completed_at)
        if (is.na(completed) || completed < started) {
          stop("terminal completion must be at or after stage start")
        }
      } else if (!is.na(self@completed_at)) {
        stop("a running stage cannot have completed_at")
      }
      has_failure <- !is.na(self@failure_class) && !is.na(self@failure_message)
      if (xor(is.na(self@failure_class), is.na(self@failure_message))) {
        stop("failure_class and failure_message must be present together")
      }
      if (
        has_failure &&
          (!self@failure_class %in% tempest_stage_failure_classes() ||
            !self@failure_message %in% tempest_stage_failure_messages())
      ) {
        stop("failure fields must use controlled stage values")
      }
      if (identical(self@status, "running")) {
        if (
          length(self@output_reference) > 0L ||
            has_failure ||
            isTRUE(self@fallback_taken) ||
            !is.na(self@fallback_implementation) ||
            !identical(self@support_status, "unknown") ||
            isTRUE(self@publication_allowed)
        ) {
          stop("a running stage cannot contain terminal result state")
        }
      }
      if (identical(self@status, "succeeded")) {
        if (length(self@output_reference) == 0L) {
          stop("a succeeded stage requires an output reference")
        }
        if (isTRUE(self@fallback_taken) && !has_failure) {
          stop("fallback success must retain the primary safe failure")
        }
        if (!isTRUE(self@fallback_taken) && has_failure) {
          stop("direct success cannot retain a failure")
        }
        unknown_support_stages <- c(
          "perspectives",
          "personas",
          "query_decomposition",
          "extract_claims",
          "next_question",
          "draft_outline",
          "refined_outline"
        )
        if (
          self@stage %in%
            unknown_support_stages &&
            !identical(self@support_status, "unknown")
        ) {
          stop(
            "this stage can only record unknown support in the current schema"
          )
        }
        if (
          self@stage %in%
            c("section_writing", "lead_section") &&
            !identical(self@support_status, "verified")
        ) {
          stop(
            "grounded writing success requires verified assertion binding"
          )
        }
      }
      if (self@status %in% c("failed", "cancelled")) {
        if (length(self@output_reference) > 0L || !has_failure) {
          stop(
            "failed and cancelled stages require failure state and no output"
          )
        }
        if (!identical(self@support_status, "unknown")) {
          stop("failed and cancelled stages must have unknown support")
        }
      }
      if (
        identical(self@status, "cancelled") &&
          !identical(self@failure_class, "tempest_stage_cancelled")
      ) {
        stop("cancelled stages must use the controlled cancellation failure")
      }
      if (
        identical(self@fallback_policy, "fail_closed") &&
          isTRUE(self@fallback_taken)
      ) {
        stop("fail-closed stages cannot take a fallback")
      }
      expected_implementation <- policy$fallback_implementation
      if (isTRUE(self@fallback_taken)) {
        if (
          is.na(expected_implementation) ||
            !identical(self@fallback_implementation, expected_implementation)
        ) {
          stop("fallback implementation must be derived from stage policy")
        }
      } else if (!is.na(self@fallback_implementation)) {
        stop("fallback implementation is recorded only when fallback is taken")
      }
      expected_publication <- tempest_stage_publication_allowed(
        self@status,
        self@execution_path,
        self@support_status
      )
      if (!identical(self@publication_allowed, expected_publication)) {
        stop("publication_allowed must be derived from status and trust")
      }
      NULL
    },
    error = conditionMessage
  )
}

tempest_stage_prop_string <- function(default = NA_character_) {
  S7::new_property(S7::class_character, default = default)
}

TempestStageRecord <- S7::new_class(
  "TempestStageRecord",
  properties = list(
    stage = tempest_stage_prop_string(),
    attempt_id = tempest_stage_prop_string(),
    status = prop_enum(tempest_stage_statuses()),
    output_reference = S7::new_property(S7::class_list, default = list()),
    program_artifact_id = tempest_stage_prop_string(),
    governed_procedure_revision_id = tempest_stage_prop_string(),
    trace_references = S7::new_property(S7::class_list, default = list()),
    started_at = tempest_stage_prop_string(),
    completed_at = tempest_stage_prop_string(),
    failure_class = tempest_stage_prop_string(),
    failure_message = tempest_stage_prop_string(),
    fallback_policy = prop_enum(tempest_stage_fallback_policies()),
    fallback_implementation = tempest_stage_prop_string(),
    fallback_taken = S7::new_property(S7::class_logical, default = FALSE),
    execution_path = prop_enum(tempest_execution_paths()),
    support_status = prop_enum(tempest_support_statuses(), "unknown"),
    publication_allowed = S7::new_property(
      S7::class_logical,
      default = FALSE
    )
  ),
  validator = tempest_stage_record_validation_message
)

tempest_stage_record <- function(
  stage,
  attempt_id,
  status,
  program_artifact_id,
  output_reference = list(),
  governed_procedure_revision_id = NA_character_,
  trace_references = list(),
  started_at = tempest_now_utc(),
  completed_at = NA_character_,
  failure_class = NA_character_,
  failure_message = NA_character_,
  fallback_policy = NULL,
  fallback_implementation = NA_character_,
  fallback_taken = FALSE,
  execution_path = NULL,
  support_status = "unknown",
  publication_allowed = NULL
) {
  policy <- tempest_stage_policy(stage)
  attempt_id <- tempest_stage_required_string(attempt_id, "attempt_id")
  status <- tempest_stage_required_string(status, "status")
  if (!status %in% tempest_stage_statuses()) {
    tempest_stage_record_abort(
      "{.arg status} must be one of {.val {tempest_stage_statuses()}}."
    )
  }
  program_artifact_id <-
    tempest_research_manifest_program_artifact_id(
      program_artifact_id,
      "program_artifact_id"
    )
  governed_procedure_revision_id <- tempest_stage_optional_string(
    governed_procedure_revision_id,
    "governed_procedure_revision_id"
  )
  started_at <- tempest_stage_time(started_at, "started_at")
  completed_at <- tempest_stage_time(
    completed_at,
    "completed_at",
    optional = TRUE
  )
  failure_class <- tempest_stage_optional_string(failure_class, "failure_class")
  failure_message <- tempest_stage_optional_string(
    failure_message,
    "failure_message"
  )
  fallback_policy <- fallback_policy %||% policy$fallback_policy
  fallback_implementation <- tempest_stage_optional_string(
    fallback_implementation,
    "fallback_implementation"
  )
  if (!rlang::is_bool(fallback_taken)) {
    tempest_stage_record_abort(
      "{.arg fallback_taken} must be `TRUE` or `FALSE`."
    )
  }
  if (!support_status %in% tempest_support_statuses()) {
    tempest_stage_record_abort(
      "{.arg support_status} must be one of {.val {tempest_support_statuses()}}."
    )
  }
  output_reference <- tempest_stage_output_reference_validate(
    output_reference,
    allow_empty = !identical(status, "succeeded")
  )
  trace_references <- tempest_stage_trace_references(
    trace_references,
    attempt_id
  )
  derived_execution_path <- tempest_stage_execution_path_derive(
    status,
    policy,
    governed_procedure_revision_id,
    trace_references,
    fallback_taken
  )
  if (
    !is.null(execution_path) &&
      !identical(execution_path, derived_execution_path)
  ) {
    tempest_stage_record_abort(
      "{.arg execution_path} must equal the derived terminal authority path."
    )
  }
  execution_path <- derived_execution_path
  derived_publication <- tempest_stage_publication_allowed(
    status,
    execution_path,
    support_status
  )
  if (
    !is.null(publication_allowed) &&
      !identical(publication_allowed, derived_publication)
  ) {
    tempest_stage_record_abort(
      "{.arg publication_allowed} must equal the derived trust decision."
    )
  }
  tryCatch(
    TempestStageRecord(
      stage = stage,
      attempt_id = attempt_id,
      status = status,
      output_reference = output_reference,
      program_artifact_id = program_artifact_id,
      governed_procedure_revision_id = governed_procedure_revision_id,
      trace_references = trace_references,
      started_at = started_at,
      completed_at = completed_at,
      failure_class = failure_class,
      failure_message = failure_message,
      fallback_policy = fallback_policy,
      fallback_implementation = fallback_implementation,
      fallback_taken = fallback_taken,
      execution_path = execution_path,
      support_status = support_status,
      publication_allowed = derived_publication
    ),
    error = function(error) {
      tempest_stage_record_abort(
        "Could not construct a valid Tempest stage record."
      )
    }
  )
}

tempest_attempt_id <- function() {
  tempest_uuid("stage-attempt")
}

tempest_stage_record_start <- function(
  stage,
  program_artifact_id,
  governed_procedure_revision_id = NULL,
  trace_references = list(),
  attempt_id = tempest_attempt_id(),
  started_at = tempest_now_utc()
) {
  tempest_stage_record(
    stage = stage,
    attempt_id = attempt_id,
    status = "running",
    program_artifact_id = program_artifact_id,
    governed_procedure_revision_id = governed_procedure_revision_id,
    trace_references = trace_references,
    started_at = started_at
  )
}

tempest_stage_record_assert_running <- function(record) {
  if (!S7::S7_inherits(record, TempestStageRecord)) {
    tempest_stage_lifecycle_abort(
      "{.arg record} must be a TempestStageRecord."
    )
  }
  if (!identical(record@status, "running")) {
    tempest_stage_lifecycle_abort("A terminal stage record is immutable.")
  }
  record
}

tempest_stage_record_succeed <- function(
  record,
  output_reference,
  support_status,
  fallback_taken = FALSE,
  primary_error = NULL,
  completed_at = tempest_now_utc()
) {
  record <- tempest_stage_record_assert_running(record)
  policy <- tempest_stage_policy(record@stage)
  failure_class <- NA_character_
  failure_message <- NA_character_
  fallback_implementation <- NA_character_
  if (isTRUE(fallback_taken)) {
    failure_kind <- if (
      inherits(primary_error, "tempest_stage_output_validation_error")
    ) {
      "validation"
    } else {
      "execution"
    }
    failure_class <- tempest_stage_failure_class(primary_error, failure_kind)
    failure_message <- tempest_stage_failure_message(
      primary_error,
      failure_kind
    )
    fallback_implementation <- policy$fallback_implementation
  }
  tempest_stage_record(
    stage = record@stage,
    attempt_id = record@attempt_id,
    status = "succeeded",
    program_artifact_id = record@program_artifact_id,
    output_reference = output_reference,
    governed_procedure_revision_id = record@governed_procedure_revision_id,
    trace_references = record@trace_references,
    started_at = record@started_at,
    completed_at = completed_at,
    failure_class = failure_class,
    failure_message = failure_message,
    fallback_policy = record@fallback_policy,
    fallback_implementation = fallback_implementation,
    fallback_taken = fallback_taken,
    support_status = support_status
  )
}

tempest_stage_record_fail <- function(
  record,
  error = NULL,
  kind = c("execution", "validation", "fallback", "commit"),
  fallback_taken = FALSE,
  completed_at = tempest_now_utc()
) {
  record <- tempest_stage_record_assert_running(record)
  kind <- match.arg(kind)
  policy <- tempest_stage_policy(record@stage)
  tempest_stage_record(
    stage = record@stage,
    attempt_id = record@attempt_id,
    status = "failed",
    program_artifact_id = record@program_artifact_id,
    governed_procedure_revision_id = record@governed_procedure_revision_id,
    trace_references = record@trace_references,
    started_at = record@started_at,
    completed_at = completed_at,
    failure_class = tempest_stage_failure_class(error, kind),
    failure_message = tempest_stage_failure_message(error, kind),
    fallback_policy = record@fallback_policy,
    fallback_implementation = if (isTRUE(fallback_taken)) {
      policy$fallback_implementation
    } else {
      NA_character_
    },
    fallback_taken = fallback_taken,
    support_status = "unknown"
  )
}

tempest_stage_record_cancel <- function(
  record,
  completed_at = tempest_now_utc()
) {
  record <- tempest_stage_record_assert_running(record)
  tempest_stage_record(
    stage = record@stage,
    attempt_id = record@attempt_id,
    status = "cancelled",
    program_artifact_id = record@program_artifact_id,
    governed_procedure_revision_id = record@governed_procedure_revision_id,
    trace_references = record@trace_references,
    started_at = record@started_at,
    completed_at = completed_at,
    failure_class = "tempest_stage_cancelled",
    failure_message = tempest_stage_failure_message(kind = "cancelled"),
    fallback_policy = record@fallback_policy,
    support_status = "unknown"
  )
}

tempest_stage_nullable_data <- function(value) {
  if (length(value) == 1L && is.atomic(value) && is.na(value)) NULL else value
}

tempest_stage_record_data <- function(record) {
  if (!S7::S7_inherits(record, TempestStageRecord)) {
    tempest_stage_record_abort(
      "{.arg record} must be a TempestStageRecord."
    )
  }
  tryCatch(
    S7::validate(record),
    error = function(error) {
      tempest_stage_record_abort(
        "The TempestStageRecord failed validation."
      )
    }
  )
  fields <- tempest_stage_record_fields()
  data <- stats::setNames(
    lapply(fields, \(field) S7::prop(record, field)),
    fields
  )
  data["output_reference"] <- list(
    if (length(data$output_reference) == 0L) {
      NULL
    } else {
      data$output_reference
    }
  )
  for (field in c(
    "governed_procedure_revision_id",
    "completed_at",
    "failure_class",
    "failure_message",
    "fallback_implementation"
  )) {
    data[field] <- list(tempest_stage_nullable_data(data[[field]]))
  }
  data
}

tempest_stage_record_from_data <- function(data) {
  fields <- names(data)
  expected <- tempest_stage_record_fields()
  if (
    !is.list(data) ||
      is.data.frame(data) ||
      !identical(fields, expected)
  ) {
    tempest_stage_record_abort(
      "Stage record data must contain the exact durable fields."
    )
  }
  nullable <- function(value, field) {
    if (is.null(value)) {
      return(NA_character_)
    }
    if (length(value) == 0L) {
      tempest_stage_record_abort(
        "Stage record nullable field {.field {field}} must use exact null."
      )
    }
    if (is.atomic(value) && anyNA(value)) {
      tempest_stage_record_abort(
        "Stage record nullable field {.field {field}} must use exact null."
      )
    }
    value
  }
  output_reference <- data$output_reference
  if (is.null(output_reference)) {
    output_reference <- list()
  } else if (length(output_reference) == 0L) {
    tempest_stage_record_abort(
      "Stage record empty output references must use exact null."
    )
  }
  if (length(output_reference) > 0L) {
    tempest_stage_output_reference_validate(output_reference)
  }
  canonical_trace_references <- tempest_stage_trace_references(
    data$trace_references,
    data$attempt_id
  )
  if (!identical(data$trace_references, canonical_trace_references)) {
    tempest_stage_record_abort(
      "Stage record trace references must use the exact canonical array shape."
    )
  }
  tempest_stage_record(
    stage = data$stage,
    attempt_id = data$attempt_id,
    status = data$status,
    output_reference = output_reference,
    program_artifact_id = data$program_artifact_id,
    governed_procedure_revision_id = nullable(
      data$governed_procedure_revision_id,
      "governed_procedure_revision_id"
    ),
    trace_references = data$trace_references,
    started_at = data$started_at,
    completed_at = nullable(data$completed_at, "completed_at"),
    failure_class = nullable(data$failure_class, "failure_class"),
    failure_message = nullable(data$failure_message, "failure_message"),
    fallback_policy = data$fallback_policy,
    fallback_implementation = nullable(
      data$fallback_implementation,
      "fallback_implementation"
    ),
    fallback_taken = data$fallback_taken,
    execution_path = data$execution_path,
    support_status = data$support_status,
    publication_allowed = data$publication_allowed
  )
}

tempest_stage_records_order_fields <- function(started_at, attempt_ids) {
  if (length(started_at) == 0L) {
    return(integer())
  }
  started <- vapply(
    started_at,
    \(value) as.numeric(tempest_stage_time_parse(value)),
    numeric(1)
  )
  order(started, attempt_ids, method = "radix")
}

tempest_stage_records_order <- function(records) {
  if (length(records) == 0L) {
    return(integer())
  }
  tempest_stage_records_order_fields(
    vapply(records, \(record) record@started_at, character(1)),
    vapply(records, \(record) record@attempt_id, character(1))
  )
}

tempest_stage_records_validate <- function(records, allow_running = TRUE) {
  if (!rlang::is_bool(allow_running)) {
    tempest_stage_record_abort(
      "{.arg allow_running} must be `TRUE` or `FALSE`."
    )
  }
  if (!is.list(records) || is.data.frame(records)) {
    tempest_stage_record_abort("{.arg records} must be an unnamed list.")
  }
  if (!is.null(names(records))) {
    tempest_stage_record_abort("{.arg records} must be an unnamed list.")
  }
  if (length(records) == 0L) {
    return(list())
  }
  for (record in records) {
    if (!S7::S7_inherits(record, TempestStageRecord)) {
      tempest_stage_record_abort(
        "{.arg records} must contain only TempestStageRecord objects."
      )
    }
    tryCatch(
      S7::validate(record),
      error = function(error) {
        tempest_stage_record_abort(
          "A stage record failed live validation."
        )
      }
    )
  }
  attempt_ids <- vapply(records, \(record) record@attempt_id, character(1))
  if (anyDuplicated(attempt_ids)) {
    tempest_stage_record_abort("Stage attempt IDs must be unique.")
  }
  canonical_order <- tempest_stage_records_order(records)
  if (!identical(canonical_order, seq_along(records))) {
    tempest_stage_record_abort(
      paste0(
        "Stage records must be ordered canonically by {.field started_at}, ",
        "then {.field attempt_id}."
      )
    )
  }
  if (
    !allow_running &&
      any(vapply(records, \(record) record@status == "running", logical(1)))
  ) {
    tempest_stage_record_abort(
      "Durable restored stage records cannot contain running attempts."
    )
  }
  unname(records)
}

tempest_stage_records_data <- function(records) {
  records <- tempest_stage_records_validate(records)
  unname(lapply(records, tempest_stage_record_data))
}

tempest_stage_records_from_data <- function(data, allow_running = TRUE) {
  if (!is.list(data) || is.data.frame(data)) {
    tempest_stage_record_abort("Stage record data must be an unnamed list.")
  }
  if (!is.null(names(data))) {
    tempest_stage_record_abort("Stage record data must be an unnamed list.")
  }
  records <- unname(lapply(data, tempest_stage_record_from_data))
  tempest_stage_records_validate(records, allow_running = allow_running)
}

tempest_stage_record_same_attempt <- function(x, y) {
  fields <- c(
    "stage",
    "attempt_id",
    "program_artifact_id",
    "governed_procedure_revision_id",
    "trace_references",
    "started_at",
    "fallback_policy"
  )
  all(vapply(
    fields,
    \(field) identical(S7::prop(x, field), S7::prop(y, field)),
    logical(1)
  ))
}

tempest_stage_records_upsert <- function(records, record) {
  records <- tempest_stage_records_validate(records)
  if (!S7::S7_inherits(record, TempestStageRecord)) {
    tempest_stage_record_abort("{.arg record} must be a TempestStageRecord.")
  }
  ids <- vapply(records, \(item) item@attempt_id, character(1))
  index <- match(record@attempt_id, ids)
  if (is.na(index)) {
    records <- c(records, list(record))
    records <- records[tempest_stage_records_order(records)]
    return(tempest_stage_records_validate(unname(records)))
  }
  existing <- records[[index]]
  if (!tempest_stage_record_same_attempt(existing, record)) {
    tempest_stage_lifecycle_abort(
      "A stage attempt cannot change its immutable identity fields."
    )
  }
  if (!identical(existing@status, "running")) {
    if (identical(existing, record)) {
      return(records)
    }
    tempest_stage_lifecycle_abort("A terminal stage record is immutable.")
  }
  if (identical(record@status, "running") && !identical(existing, record)) {
    tempest_stage_lifecycle_abort(
      "A running stage record can only transition to one terminal state."
    )
  }
  records[[index]] <- record
  tempest_stage_records_validate(records)
}

tempest_stage_records_upsert_many <- function(records, updates) {
  records <- tempest_stage_records_validate(records, allow_running = TRUE)
  if (!is.list(updates) || is.data.frame(updates)) {
    tempest_stage_record_abort(
      "{.arg updates} must be a list of TempestStageRecord objects."
    )
  }
  candidate <- records
  for (record in updates) {
    candidate <- tempest_stage_records_upsert(candidate, record)
  }
  tempest_stage_records_validate(candidate, allow_running = TRUE)
}

tempest_stage_records_interrupt <- function(
  records,
  completed_at = tempest_now_utc()
) {
  records <- tempest_stage_records_validate(records)
  unname(lapply(records, function(record) {
    if (identical(record@status, "running")) {
      tempest_stage_record_cancel(record, completed_at = completed_at)
    } else {
      record
    }
  }))
}

tempest_stage_records_validate_manifest <- function(records, manifest) {
  records <- tempest_stage_records_validate(records)
  if (!S7::S7_inherits(manifest, TempestResearchManifest)) {
    tempest_stage_record_abort(
      "{.arg manifest} must be a TempestResearchManifest."
    )
  }
  programs <- tempest_research_manifest_programs(manifest@programs)
  trace_values <- function(value, field) {
    if (!is.list(value) || length(value) == 0L) {
      return(character())
    }
    values <- character()
    if (!is.null(names(value)) && field %in% names(value)) {
      values <- c(values, value[[field]])
    }
    children <- value[vapply(value, is.list, logical(1))]
    for (child in children) {
      values <- c(values, trace_values(child, field))
    }
    unique(values)
  }
  for (record in records) {
    reference <- programs[[record@stage]] %||% NULL
    if (is.null(reference)) {
      tempest_stage_record_abort(
        "Stage record references a program absent from the manifest."
      )
    }
    if (!identical(record@program_artifact_id, reference$program_artifact_id)) {
      tempest_stage_record_abort(
        "Stage record program identity does not match the manifest."
      )
    }
    governed_reference <- reference$governed_procedure_ref %||% NULL
    revision <- if (is.null(governed_reference)) {
      NA_character_
    } else {
      governed_reference$revision_id
    }
    if (!identical(record@governed_procedure_revision_id, revision)) {
      tempest_stage_record_abort(
        "Stage record governed-procedure revision does not match the manifest."
      )
    }
    expected_governed_trace <- if (is.null(governed_reference)) {
      NULL
    } else {
      c(list(kind = "governed_procedure"), governed_reference)
    }
    actual_governed_trace <-
      record@trace_references$governed_procedure %||% NULL
    if (!identical(actual_governed_trace, expected_governed_trace)) {
      tempest_stage_record_abort(
        paste0(
          "Stage record governed-procedure trace does not exactly match ",
          "the manifest."
        )
      )
    }
    run_id <- record@trace_references$research_run_id %||% NULL
    if (!identical(run_id, manifest@research_run_id)) {
      tempest_stage_record_abort(
        "Stage record research-run trace does not match the manifest."
      )
    }
    mode <- record@trace_references$mode %||% NULL
    if (!identical(mode, manifest@mode)) {
      tempest_stage_record_abort(
        "Stage record mode trace does not match the manifest."
      )
    }
    role <- record@trace_references$role %||% NULL
    if (!identical(role, "program")) {
      tempest_stage_record_abort(
        "Stage record role trace must identify ProgramSet execution."
      )
    }
    runtime_bindings <- c(
      deputy_run_id = "deputy_run_ids",
      deputy_session_id = "deputy_session_ids"
    )
    for (field in names(runtime_bindings)) {
      value <- record@trace_references[[field]] %||% NULL
      allowed <- unlist(
        manifest@runtime[[runtime_bindings[[field]]]] %||% list(),
        use.names = FALSE
      )
      if (!is.null(value) && !value %in% allowed) {
        tempest_stage_record_abort(
          "Stage record {.field {field}} trace is absent from the manifest."
        )
      }
    }
    for (field in c(
      "parent_run_id",
      "delegation_id",
      "tool_call_id",
      "trace_id",
      "expert_id",
      "correlation_id"
    )) {
      value <- record@trace_references[[field]] %||% NULL
      if (
        !is.null(value) &&
          !value %in% trace_values(manifest@traces, field)
      ) {
        tempest_stage_record_abort(
          "Stage record {.field {field}} trace is absent from the manifest."
        )
      }
    }
  }
  invisible(records)
}

#' @keywords internal
tempest_stage_records_verification_projection <- function(records, workspace) {
  records <- tempest_stage_records_validate(records)
  if (!inherits(workspace, "ResearchWorkspace")) {
    tempest_stage_record_abort(
      "{.arg workspace} must be a ResearchWorkspace."
    )
  }
  supports <- workspace$list_claim_supports()
  verification <- Filter(
    function(record) {
      identical(record@stage, "verify_claim_support") &&
        identical(record@status, "succeeded")
    },
    records
  )
  if (length(supports) == 0L) {
    if (length(verification) > 0L) {
      tempest_stage_record_abort(
        paste0(
          "Succeeded verification records cannot exist without the exact ",
          "durable claim-support set."
        )
      )
    }
    return(invisible(list(
      verified_at = NA_character_,
      verifier_model = NA_character_
    )))
  }

  support_ids <- vapply(
    supports,
    \(support) support@claim_support_id,
    character(1)
  )
  record_ids <- vapply(
    verification,
    function(record) {
      ids <- unlist(record@output_reference$ids, use.names = FALSE)
      if (
        !identical(record@output_reference$kind, "claim_supports") ||
          length(ids) != 1L
      ) {
        tempest_stage_record_abort(
          paste0(
            "Each succeeded verification record must bind exactly one ",
            "claim-support assessment."
          )
        )
      }
      ids[[1]]
    },
    character(1)
  )
  if (
    length(record_ids) != length(support_ids) ||
      anyDuplicated(record_ids) ||
      !setequal(record_ids, support_ids)
  ) {
    tempest_stage_record_abort(
      paste0(
        "Succeeded verification records must bind every durable ",
        "claim-support assessment exactly once."
      )
    )
  }
  verification <- verification[match(support_ids, record_ids)]
  verified_at <- vapply(
    verification,
    \(record) record@trace_references$verified_at,
    character(1)
  )
  verifier_model <- vapply(
    verification,
    function(record) {
      record@trace_references$verifier_model %||% NA_character_
    },
    character(1)
  )
  if (
    !all(vapply(
      verified_at,
      tempest_ledger_timestamp_valid,
      logical(1)
    )) ||
      !all(vapply(
        verifier_model,
        tempest_ledger_identifier_valid,
        logical(1),
        optional = TRUE
      )) ||
      !all(vapply(verified_at, identical, logical(1), verified_at[[1]])) ||
      !all(vapply(
        verifier_model,
        identical,
        logical(1),
        verifier_model[[1]]
      ))
  ) {
    tempest_stage_record_abort(
      paste0(
        "A verification batch must bind one exact canonical timestamp and ",
        "optional verifier identity across every claim-support record."
      )
    )
  }
  batch_time <- tempest_stage_time_parse(verified_at[[1]])
  starts <- vapply(
    verification,
    function(record) as.numeric(tempest_stage_time_parse(record@started_at)),
    numeric(1)
  )
  if (is.na(batch_time) || any(as.numeric(batch_time) > starts)) {
    tempest_stage_record_abort(
      paste0(
        "Verification batch time must be at or before every bound stage ",
        "attempt start."
      )
    )
  }
  for (index in seq_along(supports)) {
    support <- supports[[index]]
    claim <- workspace$get_proposed_claim(support@claim_id)
    if (
      is.null(claim) ||
        !identical(claim@verified_at, verified_at[[index]]) ||
        !identical(claim@verifier_model, verifier_model[[index]])
    ) {
      tempest_stage_record_abort(
        paste0(
          "Claim verifier metadata must be the exact projection of its ",
          "authoritative verification-stage proof."
        )
      )
    }
  }
  invisible(list(
    verified_at = verified_at[[1]],
    verifier_model = verifier_model[[1]]
  ))
}

#' @keywords internal
tempest_stage_records_validate_workspace <- function(
  records,
  workspace,
  min_support_score = 0.7
) {
  records <- tempest_stage_records_validate(records)
  if (!inherits(workspace, "ResearchWorkspace")) {
    tempest_stage_record_abort(
      "{.arg workspace} must be a ResearchWorkspace."
    )
  }
  min_support_score <- tempest_normalize_min_support_score(min_support_score)
  claim_ids <- vapply(
    workspace$list_proposed_claims(),
    \(claim) claim@claim_id,
    character(1)
  )
  claim_supports <- workspace$list_claim_supports()
  support_ids <- vapply(
    claim_supports,
    \(support) support@claim_support_id,
    character(1)
  )
  verification_reference_ids <- list()
  for (record in records) {
    if (identical(record@stage, "verify_claim_support")) {
      expected_threshold <- tempest_stage_support_threshold_string(
        min_support_score
      )
      actual_threshold <-
        record@trace_references$min_support_score %||% NULL
      if (!identical(actual_threshold, expected_threshold)) {
        tempest_stage_record_abort(
          paste0(
            "Verification-stage threshold trace does not match the exact ",
            "configured min_support_score."
          )
        )
      }
    }
    reference <- record@output_reference
    if (length(reference) == 0L) {
      next
    }
    ids <- unlist(reference$ids, use.names = FALSE)
    mismatched <- switch(
      reference$kind,
      workspace_claims = length(setdiff(ids, claim_ids)) > 0L,
      claim_supports = {
        verification_reference_ids <- c(
          verification_reference_ids,
          list(ids)
        )
        length(ids) == 0L || length(setdiff(ids, support_ids)) > 0L
      },
      FALSE
    )
    if (isTRUE(mismatched)) {
      tempest_stage_record_abort(
        paste0(
          "Stage-record output references do not match the durable ",
          "ResearchWorkspace."
        )
      )
    }
    if (identical(reference$kind, "workspace_claims")) {
      referenced_claims <- lapply(ids, workspace$get_proposed_claim)
      referenced_span_ids <- unname(unlist(
        lapply(referenced_claims, \(claim) claim@evidence_span_ids),
        use.names = FALSE
      ))
      referenced_spans <- lapply(
        referenced_span_ids,
        workspace$get_evidence_span
      )
      expected_digest <- tempest_stage_claims_output_digest(
        referenced_claims,
        record,
        referenced_spans
      )
      if (!identical(reference$content_digest, expected_digest)) {
        tempest_stage_record_abort(
          paste0(
            "Extraction-stage output digest does not match the exact ",
            "durable claim records."
          )
        )
      }
    }
    if (identical(reference$kind, "claim_supports")) {
      support <- if (length(ids) == 1L) {
        workspace$get_claim_support(ids[[1]])
      } else {
        NULL
      }
      claim <- if (is.null(support)) {
        NULL
      } else {
        workspace$get_proposed_claim(support@claim_id)
      }
      evidence_span <- if (is.null(support)) {
        NULL
      } else {
        workspace$get_evidence_span(support@evidence_span_id)
      }
      expected_digest <- tempest_stage_verification_output_digest(
        support,
        record,
        claim,
        evidence_span,
        workspace
      )
      if (!identical(reference$content_digest, expected_digest)) {
        tempest_stage_record_abort(
          paste0(
            "Verification-stage output digest does not match the exact ",
            "durable claim-span support and authoritative source evidence."
          )
        )
      }
    }
    if (identical(record@stage, "verify_claim_support")) {
      if (length(ids) != 1L) {
        tempest_stage_record_abort(
          paste0(
            "Each verification-stage record must reference exactly one ",
            "claim-support assessment."
          )
        )
      }
      support <- workspace$get_claim_support(ids[[1]])
      normalized_status <- tempest_apply_min_support_score(
        support@verification_status,
        support@support_score,
        min_support_score = min_support_score
      )
      if (!identical(support@verification_status, normalized_status)) {
        tempest_stage_record_abort(
          paste0(
            "Persisted claim-span status is not the exact normalized ",
            "verification result at the configured threshold."
          )
        )
      }
      expected_support <- tempest_stage_verification_support_status(
        support@verification_status,
        support@support_score,
        min_support_score
      )
      if (!identical(record@support_status, expected_support)) {
        tempest_stage_record_abort(
          paste0(
            "Verification-stage trust does not match the durable claim-span ",
            "support at the configured threshold."
          )
        )
      }
    }
    if (
      record@stage %in%
        c(
          "refined_outline",
          "section_writing",
          "lead_section"
        )
    ) {
      evidence_field <- if (isTRUE(record@fallback_taken)) {
        "verified_evidence_claim_ids"
      } else {
        "evidence_claim_ids"
      }
      evidence_ids <- unlist(
        record@trace_references[[evidence_field]] %||% list(),
        use.names = FALSE
      )
      unknown_evidence <- setdiff(evidence_ids, claim_ids)
      if (length(unknown_evidence) > 0L) {
        tempest_stage_record_abort(
          "Grounded stage trace references unknown evidence claims."
        )
      }
      evidence <- lapply(
        evidence_ids,
        workspace$get_proposed_claim
      )
      expected_support <- tempest_stage_evidence_support(
        evidence,
        list(min_support_score = min_support_score)
      )
      if (
        isTRUE(record@fallback_taken) &&
          (length(evidence) == 0L ||
            !identical(expected_support, "verified"))
      ) {
        tempest_stage_record_abort(
          "Grounded fallback trace must bind exact threshold-supported claims."
        )
      }
      persisted_support <- if (identical(record@stage, "refined_outline")) {
        "unknown"
      } else {
        expected_support
      }
      if (
        !identical(record@support_status, persisted_support) ||
          (identical(record@stage, "refined_outline") &&
            isTRUE(record@publication_allowed))
      ) {
        tempest_stage_record_abort(
          paste0(
            "Grounded stage support does not match its durable evidence ",
            "claims at the configured threshold."
          )
        )
      }
    }
  }
  if (length(verification_reference_ids) > 0L) {
    covered_ids <- unique(unlist(
      verification_reference_ids,
      use.names = FALSE
    ))
    if (!setequal(covered_ids, support_ids)) {
      tempest_stage_record_abort(
        paste0(
          "Successful verification-stage references must cover the durable ",
          "claim-support ledger."
        )
      )
    }
  }
  tempest_stage_records_verification_projection(records, workspace)
  invisible(records)
}

#' @keywords internal
tempest_stage_records_validate_workspace_coverage <- function(
  records,
  workspace,
  require_extraction = FALSE,
  require_verification = FALSE
) {
  records <- tempest_stage_records_validate(records)
  claims <- workspace$list_proposed_claims()
  claim_ids <- vapply(claims, \(claim) claim@claim_id, character(1))
  succeeded <- Filter(\(record) identical(record@status, "succeeded"), records)
  extraction <- Filter(
    \(record) identical(record@stage, "extract_claims"),
    succeeded
  )
  verification <- Filter(
    \(record) identical(record@stage, "verify_claim_support"),
    succeeded
  )
  extracted_ids <- unname(unlist(lapply(
    extraction,
    \(record) record@output_reference$ids
  )))
  if (
    isTRUE(require_extraction) &&
      (length(extraction) == 0L || !setequal(extracted_ids, claim_ids))
  ) {
    tempest_stage_record_abort(
      paste0(
        "Succeeded extraction-stage records must exactly cover the ",
        "durable workspace claim ledger."
      )
    )
  }

  support_ids <- vapply(
    workspace$list_claim_supports(),
    \(support) support@claim_support_id,
    character(1)
  )
  verified_ids <- unname(unlist(lapply(
    verification,
    \(record) record@output_reference$ids
  )))
  if (
    isTRUE(require_verification) &&
      length(claim_ids) > 0L &&
      (length(support_ids) == 0L ||
        length(verification) == 0L ||
        !setequal(verified_ids, support_ids))
  ) {
    tempest_stage_record_abort(
      paste0(
        "Succeeded verification-stage records must exactly cover the ",
        "durable claim-support ledger."
      )
    )
  }
  invisible(records)
}

#' @keywords internal
tempest_stage_records_validate_generated_experts <- function(records, experts) {
  records <- tempest_stage_records_validate(records)
  experts <- tempest_validate_experts(experts)
  persona_records <- tempest_storm_succeeded_stage_records(records, "personas")
  if (length(persona_records) == 0L) {
    return(invisible(records))
  }
  if (length(experts) == 0L) {
    tempest_stage_record_abort(
      "Succeeded persona-stage records require a durable expert roster."
    )
  }
  record_digests <- vapply(
    persona_records,
    \(record) record@output_reference$content_digest,
    character(1)
  )
  if (anyDuplicated(record_digests)) {
    tempest_stage_record_abort(
      "Succeeded persona-stage records must not duplicate roster bindings."
    )
  }
  singleton_digests <- vapply(
    seq_along(experts),
    \(index) tempest_stage_state_output_digest("personas", experts[index]),
    character(1)
  )
  prefix_digests <- vapply(
    seq_along(experts),
    function(index) {
      tempest_stage_state_output_digest(
        "personas",
        experts[seq_len(index)]
      )
    },
    character(1)
  )
  bindings <- vector("list", length(record_digests))
  prefix_seen <- FALSE
  for (index in seq_along(record_digests)) {
    digest <- record_digests[[index]]
    singleton <- which(singleton_digests == digest)
    prefix <- which(prefix_digests == digest)
    if (
      length(singleton) > 1L ||
        length(prefix) > 1L ||
        (length(singleton) == 1L &&
          length(prefix) == 1L &&
          !identical(singleton, prefix))
    ) {
      tempest_stage_record_abort(
        "A persona-stage record ambiguously binds the durable expert roster."
      )
    }
    if (length(singleton) == 0L && length(prefix) == 0L) {
      tempest_stage_record_abort(
        paste0(
          "Every succeeded persona-stage record must bind an exact canonical ",
          "durable expert-profile prefix or singleton."
        )
      )
    }
    if (length(prefix) == 1L && !isTRUE(prefix_seen)) {
      bindings[[index]] <- list(kind = "prefix", index = prefix[[1]])
      prefix_seen <- TRUE
    } else if (length(singleton) == 1L) {
      bindings[[index]] <- list(kind = "singleton", index = singleton[[1]])
    } else {
      tempest_stage_record_abort(
        "A persona-stage roster prefix can only be recorded once and first."
      )
    }
  }
  prefix_bindings <- which(vapply(
    bindings,
    \(binding) identical(binding$kind, "prefix"),
    logical(1)
  ))
  if (length(prefix_bindings) > 1L || any(prefix_bindings != 1L)) {
    tempest_stage_record_abort(
      "A persona-stage roster prefix can only be recorded once and first."
    )
  }
  singleton_indices <- vapply(
    bindings[vapply(
      bindings,
      \(binding) identical(binding$kind, "singleton"),
      logical(1)
    )],
    `[[`,
    integer(1),
    "index"
  )
  prefix_end <- if (length(prefix_bindings) == 0L) {
    0L
  } else {
    bindings[[prefix_bindings[[1]]]]$index
  }
  if (
    any(singleton_indices <= prefix_end) ||
      is.unsorted(singleton_indices, strictly = TRUE)
  ) {
    tempest_stage_record_abort(
      paste0(
        "Succeeded persona-stage records must bind one initial roster prefix ",
        "followed only by distinct appended expert singletons."
      )
    )
  }
  invisible(records)
}

#' @keywords internal
tempest_stage_records_validate_claim_provenance <- function(
  records,
  workspace,
  research_run_id,
  experts = list(),
  builtin_expert_ids = "moderator"
) {
  records <- tempest_stage_records_validate(records)
  if (
    !rlang::is_string(research_run_id) ||
      is.na(research_run_id) ||
      !tempest_research_workspace_reference_id_valid(research_run_id)
  ) {
    tempest_stage_record_abort(
      "Authoritative claim provenance requires a credential-free run id."
    )
  }
  experts <- tempest_validate_experts(experts)
  expert_ids <- c(
    vapply(experts, \(expert) expert@expert_id, character(1)),
    builtin_expert_ids
  )
  succeeded <- Filter(\(record) identical(record@status, "succeeded"), records)
  extraction <- Filter(
    \(record) identical(record@stage, "extract_claims"),
    succeeded
  )
  present <- function(value) {
    is.character(value) &&
      length(value) == 1L &&
      !is.na(value) &&
      nzchar(value)
  }

  claims <- workspace$list_proposed_claims()
  for (claim in claims) {
    if (
      present(claim@session_id) &&
        !identical(claim@session_id, research_run_id)
    ) {
      tempest_stage_record_abort(
        "Claim provenance session id does not match the authoritative run."
      )
    }
    if (
      present(claim@expert_id) &&
        !claim@expert_id %in% expert_ids
    ) {
      tempest_stage_record_abort(
        "Claim provenance references an expert outside the durable roster."
      )
    }
    matching <- Filter(
      function(record) {
        claim@claim_id %in%
          unlist(
            record@output_reference$ids,
            use.names = FALSE
          )
      },
      extraction
    )
    spans <- lapply(claim@evidence_span_ids, workspace$get_evidence_span)
    program_spans <- Filter(
      \(span) startsWith(span@extracted_by, "sha256:"),
      spans
    )
    requires_extraction <- present(claim@session_id) ||
      present(claim@expert_id) ||
      present(claim@retrieval_step_id) ||
      length(program_spans) > 0L
    if (requires_extraction && length(matching) != 1L) {
      tempest_stage_record_abort(
        paste0(
          "Generated claim provenance requires one exact succeeded ",
          "extraction-stage record."
        )
      )
    }
    if (length(matching) == 1L) {
      record <- matching[[1]]
      if (
        length(program_spans) > 0L &&
          any(vapply(
            program_spans,
            \(span) {
              !identical(
                span@extracted_by,
                record@program_artifact_id
              )
            },
            logical(1)
          ))
      ) {
        tempest_stage_record_abort(
          "Evidence-span extraction provenance does not match its stage."
        )
      }
      trace_expert <- record@trace_references$expert_id %||% NULL
      if (
        present(claim@expert_id) &&
          !identical(claim@expert_id, trace_expert)
      ) {
        tempest_stage_record_abort(
          "Claim expert provenance does not match its extraction trace."
        )
      }
      correlation <- record@trace_references$correlation_id %||% NULL
      if (
        present(claim@retrieval_step_id) &&
          !identical(claim@retrieval_step_id, correlation)
      ) {
        tempest_stage_record_abort(
          "Claim retrieval provenance does not match its extraction trace."
        )
      }
    }
  }

  tempest_stage_records_verification_projection(records, workspace)
  invisible(records)
}

#' @keywords internal
tempest_stage_records_validate_persisted_trust <- function(
  records,
  workspace,
  min_support_score = 0.7
) {
  records <- tempest_stage_records_validate(records)
  min_support_score <- tempest_normalize_min_support_score(min_support_score)
  succeeded <- Filter(\(record) identical(record@status, "succeeded"), records)
  fixed_unknown <- c(
    "perspectives",
    "personas",
    "query_decomposition",
    "extract_claims",
    "next_question",
    "draft_outline",
    "refined_outline"
  )
  for (record in succeeded) {
    if (
      record@stage %in%
        fixed_unknown &&
        (!identical(record@support_status, "unknown") ||
          isTRUE(record@publication_allowed))
    ) {
      tempest_stage_record_abort(
        paste0(
          "Persisted exploratory, extraction, and planning stages must ",
          "remain unknown and non-publishable."
        )
      )
    }
  }

  verification <- Filter(
    \(record) identical(record@stage, "verify_claim_support"),
    succeeded
  )
  proof_ids <- character()
  grounded <- Filter(
    \(record) record@stage %in% c("section_writing", "lead_section"),
    succeeded
  )
  for (record in grounded) {
    verified_ids <- unlist(
      record@trace_references$verified_evidence_claim_ids %||% list(),
      use.names = FALSE
    )
    proof_ids <- c(proof_ids, verified_ids)
    if (
      identical(record@support_status, "verified") ||
        isTRUE(record@publication_allowed)
    ) {
      proof_ids <- c(
        proof_ids,
        unlist(
          record@trace_references$evidence_claim_ids %||% list(),
          use.names = FALSE
        )
      )
    }
  }
  proof_ids <- unique(proof_ids)
  for (claim_id in proof_ids) {
    supports <- Filter(
      \(support) identical(support@claim_id, claim_id),
      workspace$list_claim_supports()
    )
    if (length(supports) == 0L) {
      tempest_stage_record_abort(
        paste0(
          "Verified publication evidence requires the complete bound ",
          "claim-by-span support set."
        )
      )
    }
    for (support in supports) {
      matching <- Filter(
        \(record) {
          identical(
            unlist(record@output_reference$ids, use.names = FALSE),
            support@claim_support_id
          )
        },
        verification
      )
      expected_pair <- tempest_stage_verification_support_status(
        support@verification_status,
        support@support_score,
        min_support_score
      )
      if (length(matching) != 1L || !identical(expected_pair, "verified")) {
        tempest_stage_record_abort(
          paste0(
            "Verified publication evidence requires one exact succeeded ",
            "verification record for every threshold-supported span."
          )
        )
      }
    }
    claim <- workspace$get_proposed_claim(claim_id)
    expected <- tempest_stage_verification_support_status(
      claim@verification_status,
      claim@support_score,
      min_support_score
    )
    if (!identical(expected, "verified")) {
      tempest_stage_record_abort(
        "Publication proof is below the persisted support threshold."
      )
    }
  }
  invisible(records)
}
