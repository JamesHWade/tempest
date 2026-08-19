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
      fallback_implementation = "tempest::fallback/section-writing/ellmer-text@1",
      execution_path = "grounded",
      requires_verified_evidence = TRUE
    ),
    lead_section = list(
      fallback_policy = "grounded_only",
      fallback_implementation = "tempest::fallback/lead-section/ellmer-text@1",
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

tempest_stage_fallback_perspectives <- function(chat, inputs, context) {
  prompt <- paste0(
    "You are planning a comprehensive research report.\n",
    "Topic: ",
    inputs$topic,
    "\n\n",
    inputs$seed_context,
    "\n\n",
    "Propose exactly ",
    inputs$n_experts,
    paste0(
      " distinct perspectives to cover the topic. Each perspective ",
      "should have 3-6 research questions.\n"
    ),
    "Return structured data."
  )
  chat$chat_structured(
    prompt,
    type = tempest_type_perspectives(),
    echo = "none",
    convert = FALSE
  )
}

tempest_stage_fallback_personas <- function(chat, inputs, context) {
  tempest_personas_ellmer_fallback(chat, inputs, context)
}

tempest_stage_fallback_query_decomposition <- function(chat, inputs, context) {
  list(queries = inputs$question)
}

tempest_stage_fallback_next_question <- function(chat, inputs, context) {
  prompt <- paste0(
    "You are interviewing an expert to build a factual knowledge base.\n\n",
    "Topic: ",
    inputs$topic,
    "\nPerspective: ",
    inputs$perspective,
    "\n\nAnswered Q&A so far:\n",
    inputs$answered,
    "\n\nCurrent fact notes summary:\n",
    inputs$facts,
    "\n\nPropose the single most useful next question to ask.\n",
    "If this perspective is sufficiently covered, set done = true.\n",
    "Return structured data."
  )
  chat$chat_structured(
    prompt,
    type = tempest_type_next_question(),
    echo = "none",
    convert = FALSE
  )
}

tempest_stage_fallback_draft_outline <- function(chat, inputs, context) {
  prompt <- paste0(
    "Create a draft outline for a report based on your own knowledge.\n\n",
    "Topic: ",
    inputs$topic,
    "\nDesired report title: ",
    inputs$report_title,
    "\n\nRequirements:\n",
    "- Organize into 4-6 sections based on what you know about the topic.\n",
    "- Include subsections with bullet points.\n",
    "- This is a preliminary outline; it will be refined with research findings.\n",
    "- Return structured data.\n"
  )
  chat$chat_structured(
    prompt,
    type = tempest_type_outline(),
    echo = "none",
    convert = FALSE
  )
}

tempest_stage_fallback_refined_outline <- function(chat, inputs, context) {
  prompt <- paste0(
    "Refine this draft outline using verified research findings.\n\n",
    "Topic: ",
    inputs$topic,
    "\nDesired report title: ",
    inputs$report_title,
    "\n\nDraft outline sections:\n",
    inputs$draft_outline,
    "\n\nAvailable verified fact notes (each includes citations):\n",
    context$verified_facts,
    "\n\nRequirements:\n",
    "- Adjust sections based on available evidence.\n",
    "- Add, merge, or remove sections as needed.\n",
    "- Ensure each section has supporting facts.\n",
    "- Return structured data.\n"
  )
  chat$chat_structured(
    prompt,
    type = tempest_type_outline(),
    echo = "none",
    convert = FALSE
  )
}

tempest_stage_fallback_section_writing <- function(chat, inputs, context) {
  prompt <- paste0(
    "Write a report section in Markdown.\n\n",
    "Section title: ",
    inputs$section_title,
    "\nSection intent: ",
    inputs$section_summary,
    "\n\nPlanned subsections:\n",
    inputs$subsections,
    "\n\nVerified fact notes you MUST use (do not invent facts or fetch additional sources):\n",
    context$verified_facts,
    "\n\nRules:\n",
    "- Every factual claim must end with one or more citations like [Sxxxxxxxxxxxx].\n",
    "- Use ONLY the facts provided above. Do NOT call tools or fetch new sources.\n",
    "- Keep the writing concise, technical, and well-structured.\n\n",
    "Write the section now:"
  )
  list(section_text = chat$chat(prompt, echo = "none"))
}

tempest_stage_fallback_lead_section <- function(chat, inputs, context) {
  prompt <- paste0(
    "Write a Wikipedia-style lead section (2-3 paragraphs) for this report.\n\n",
    "Topic: ",
    inputs$topic,
    "\nTitle: ",
    inputs$title,
    "\n\nArticle body (for context):\n",
    inputs$article_body,
    "\n\nKey facts:\n",
    context$verified_facts,
    "\n\nRules:\n",
    "- Summarize the most important points from the article.\n",
    "- Include citations like [Sxxxxxxxxxxxx] for key claims.\n",
    "- The lead should be self-contained.\n",
    "- Write 2-3 paragraphs.\n"
  )
  list(lead_section = chat$chat(prompt, echo = "none"))
}

tempest_stage_fallback_registry <- function() {
  list(
    perspectives = tempest_stage_fallback_perspectives,
    personas = tempest_stage_fallback_personas,
    query_decomposition = tempest_stage_fallback_query_decomposition,
    next_question = tempest_stage_fallback_next_question,
    draft_outline = tempest_stage_fallback_draft_outline,
    refined_outline = tempest_stage_fallback_refined_outline,
    section_writing = tempest_stage_fallback_section_writing,
    lead_section = tempest_stage_fallback_lead_section
  )
}

tempest_stage_fallback_resolve <- function(stage) {
  policy <- tempest_stage_policy(stage)
  fallback <- tempest_stage_fallback_registry()[[stage]] %||% NULL
  if (identical(policy$fallback_policy, "fail_closed")) {
    if (!is.null(fallback)) {
      tempest_stage_evaluator_abort(
        "Fail-closed stage unexpectedly has a fallback implementation."
      )
    }
    return(NULL)
  }
  if (!is.function(fallback)) {
    tempest_stage_evaluator_abort(
      "Fallback-eligible stage lacks its package-owned implementation."
    )
  }
  fallback
}

tempest_stage_failure_messages <- function() {
  c(
    execution = "Primary stage execution failed.",
    validation = "Primary stage output failed validation.",
    fallback = "Primary execution failed and configured fallback also failed.",
    commit = "Validated stage output could not be committed.",
    cancelled = "Stage execution was cancelled."
  )
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
    "knowledge_snapshot_id",
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

tempest_stage_records_order <- function(records) {
  if (length(records) == 0L) {
    return(integer())
  }
  started <- vapply(
    records,
    \(record) as.numeric(tempest_stage_time_parse(record@started_at)),
    numeric(1)
  )
  attempt_ids <- vapply(records, \(record) record@attempt_id, character(1))
  order(started, attempt_ids, method = "radix")
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
    snapshot_id <- record@trace_references$knowledge_snapshot_id %||% NULL
    manifest_snapshot_id <- manifest@knowledge_snapshot$snapshot_id %||% NULL
    if (!identical(snapshot_id, manifest_snapshot_id)) {
      tempest_stage_record_abort(
        "Stage record knowledge-snapshot trace does not match the manifest."
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

tempest_stage_output_abort <- function(message, ..., parent = NULL) {
  tempest_stage_abort(
    message,
    ...,
    class = "tempest_stage_output_validation_error",
    parent = parent,
    .envir = rlang::caller_env()
  )
}

tempest_stage_evaluator_abort <- function(message, ..., parent = NULL) {
  tempest_stage_abort(
    message,
    ...,
    class = "tempest_stage_evaluator_contract_error",
    parent = parent,
    .envir = rlang::caller_env()
  )
}

tempest_stage_governance_abort <- function(message, ..., parent = NULL) {
  tempest_stage_abort(
    message,
    ...,
    class = "tempest_stage_governance_error",
    parent = parent,
    .envir = rlang::caller_env()
  )
}

tempest_stage_scalar_character <- function(value, field) {
  if (
    !rlang::is_string(value) ||
      is.na(value) ||
      !nzchar(tempest_trim(value))
  ) {
    tempest_stage_output_abort(
      "Stage output field {.field {field}} must be a non-empty string."
    )
  }
  tempest_trim(value)
}

tempest_stage_plain_record <- function(value, field) {
  if (
    !is.list(value) ||
      is.data.frame(value) ||
      !is.null(attr(value, "class", exact = TRUE)) ||
      is.null(names(value)) ||
      anyNA(names(value)) ||
      any(!nzchar(names(value))) ||
      anyDuplicated(names(value))
  ) {
    tempest_stage_output_abort(
      "Stage output field {.field {field}} must be a plain named record."
    )
  }
  value
}

tempest_stage_exact_record <- function(
  value,
  field,
  required,
  optional = character()
) {
  value <- tempest_stage_plain_record(value, field)
  fields <- names(value)
  missing <- setdiff(required, fields)
  unexpected <- setdiff(fields, c(required, optional))
  if (length(missing) > 0L || length(unexpected) > 0L) {
    tempest_stage_output_abort(
      paste0(
        "Stage output field {.field {field}} has missing or unsupported ",
        "fields."
      )
    )
  }
  value
}

tempest_stage_character_vector <- function(value, field, allow_empty = FALSE) {
  if (is.list(value)) {
    if (is.data.frame(value) || !is.null(names(value))) {
      tempest_stage_output_abort(
        "Stage output field {.field {field}} must be an unnamed string array."
      )
    }
    if (!all(vapply(value, rlang::is_string, logical(1)))) {
      tempest_stage_output_abort(
        "Stage output field {.field {field}} must contain only scalar strings."
      )
    }
    value <- unlist(value, use.names = FALSE)
  }
  if (
    !is.character(value) ||
      is.object(value) ||
      !is.null(names(value))
  ) {
    tempest_stage_output_abort(
      "Stage output field {.field {field}} must be an unnamed string array."
    )
  }
  value <- tempest_trim(value)
  if (
    anyNA(value) ||
      any(!nzchar(value)) ||
      (!allow_empty && length(value) == 0L)
  ) {
    tempest_stage_output_abort(
      "Stage output field {.field {field}} must contain non-empty strings."
    )
  }
  if (anyDuplicated(value)) {
    tempest_stage_output_abort(
      "Stage output field {.field {field}} must not contain duplicates."
    )
  }
  value
}

tempest_stage_normalize_output <- function(callback) {
  tryCatch(
    callback(),
    tempest_stage_output_validation_error = function(error) stop(error),
    error = function(error) {
      tempest_stage_output_abort(
        "Stage output failed its exact normalization contract."
      )
    }
  )
}

tempest_stage_evaluate_perspectives <- function(output, context) {
  output <- tempest_stage_exact_record(
    output,
    "output",
    c("title", "perspectives")
  )
  perspectives <- output$perspectives
  if (
    !is.list(perspectives) ||
      is.data.frame(perspectives) ||
      !is.null(names(perspectives)) ||
      length(perspectives) == 0L
  ) {
    tempest_stage_output_abort(
      "Perspective output must contain at least one perspective."
    )
  }
  for (perspective in perspectives) {
    perspective <- tempest_stage_exact_record(
      perspective,
      "perspectives",
      c("name", "description", "key_questions")
    )
    tempest_stage_scalar_character(perspective$name, "perspectives$name")
    tempest_stage_scalar_character(
      perspective$description,
      "perspectives$description"
    )
    tempest_stage_character_vector(
      perspective$key_questions,
      "perspectives$key_questions"
    )
  }
  topic <- context$topic %||% output$title %||% "Research report"
  normalized <- tempest_stage_normalize_output(\() {
    tempest_normalize_perspectives(
      output,
      topic = tempest_stage_scalar_character(topic, "topic"),
      n_experts = context$n_experts %||% NULL
    )
  })
  list(output = normalized, support_status = "unknown")
}

tempest_stage_evaluate_personas <- function(output, context) {
  output <- tempest_stage_exact_record(output, "output", "personas")
  values <- output$personas
  if (
    !is.list(values) ||
      is.data.frame(values) ||
      !is.null(names(values)) ||
      length(values) == 0L
  ) {
    tempest_stage_output_abort(
      "Persona output must contain at least one persona."
    )
  }
  for (persona in values) {
    persona <- tempest_stage_exact_record(
      persona,
      "personas",
      c(
        "name",
        "title",
        "affiliation",
        "background",
        "focus_areas",
        "perspective",
        "initial_questions"
      )
    )
    tempest_stage_scalar_character(persona$name, "personas$name")
    tempest_stage_scalar_character(persona$title, "personas$title")
    tempest_stage_scalar_character(persona$affiliation, "personas$affiliation")
    tempest_stage_scalar_character(persona$background, "personas$background")
    tempest_stage_character_vector(persona$focus_areas, "personas$focus_areas")
    tempest_stage_scalar_character(persona$perspective, "personas$perspective")
    tempest_stage_character_vector(
      persona$initial_questions,
      "personas$initial_questions"
    )
  }
  normalized <- tempest_stage_normalize_output(\() {
    tempest_normalize_experts(
      output,
      n = context$n_experts %||% NULL
    )
  })
  if (length(normalized) == 0L) {
    tempest_stage_output_abort("Persona output did not produce expert records.")
  }
  list(output = normalized, support_status = "unknown")
}

tempest_stage_evaluate_queries <- function(output, context) {
  output <- tempest_stage_exact_record(output, "output", "queries")
  queries <- output$queries
  queries <- tempest_stage_character_vector(queries, "queries")
  max_queries <- context$max_queries %||% length(queries)
  if (
    !is.numeric(max_queries) ||
      length(max_queries) != 1L ||
      is.na(max_queries) ||
      max_queries < 1L
  ) {
    tempest_stage_evaluator_abort("Query evaluator context is malformed.")
  }
  list(
    output = list(
      queries = queries[seq_len(min(length(queries), max_queries))]
    ),
    support_status = "unknown"
  )
}

tempest_stage_known_source_ids <- function(context) {
  if (!inherits(context$workspace, "ResearchWorkspace")) {
    tempest_stage_evaluator_abort(
      "Claim evaluator context requires the exact ResearchWorkspace."
    )
  }
  resources <- context$workspace$list_retrieved_resources()
  vapply(resources, \(resource) resource@resource_id, character(1))
}

tempest_stage_fact_sources <- function(value, workspace) {
  value <- tempest_stage_plain_record(value, "facts")
  sources <- value$sources
  if (
    !is.list(sources) ||
      is.data.frame(sources) ||
      !is.null(names(sources)) ||
      length(sources) == 0L
  ) {
    tempest_stage_output_abort("Every extracted claim must cite a source.")
  }
  sources <- lapply(
    sources,
    function(source) {
      source <- tempest_stage_plain_record(source, "facts$sources")
      allowed <- c("source_id", "url", "quote")
      unexpected <- setdiff(names(source), allowed)
      if (!"source_id" %in% names(source) || length(unexpected) > 0L) {
        tempest_stage_output_abort(
          paste0(
            "Claim source records require {.field source_id} and may only ",
            "contain {.field url} and {.field quote}."
          )
        )
      }
      source_id <- tempest_stage_scalar_character(
        source$source_id,
        "facts$sources$source_id"
      )
      resource <- workspace$get_retrieved_source(source_id)
      if (is.null(resource)) {
        tempest_stage_output_abort(
          "Extracted claims cite unknown source ID {.val {source_id}}."
        )
      }
      url <- if ("url" %in% names(source)) {
        tempest_stage_scalar_character(source$url, "facts$sources$url")
      } else {
        NULL
      }
      if (!is.null(url)) {
        canonical_url <- resource$url %||% NA_character_
        normalized <- tryCatch(
          list(
            output = tempest_normalize_url(url),
            canonical = tempest_normalize_url(canonical_url)
          ),
          error = identity
        )
        if (
          inherits(normalized, "condition") ||
            is.na(normalized$canonical) ||
            !identical(normalized$output, normalized$canonical)
        ) {
          tempest_stage_output_abort(
            paste0(
              "Claim source URL does not match canonical workspace source ",
              "{.val {source_id}}."
            ),
            parent = if (inherits(normalized, "condition")) normalized else NULL
          )
        }
      }
      quote <- if ("quote" %in% names(source)) {
        tempest_stage_scalar_character(source$quote, "facts$sources$quote")
      } else {
        NULL
      }
      if (!is.null(quote)) {
        captured_text <- c(
          resource$content_text %||% NA_character_,
          resource$snippet %||% NA_character_,
          resource$context_text %||% NA_character_
        )
        captured_text <- unique(captured_text[
          !is.na(captured_text) & nzchar(captured_text)
        ])
        if (length(captured_text) == 0L) {
          tempest_stage_output_abort(
            paste0(
              "Claim quote cannot be validated because workspace source ",
              "{.val {source_id}} has no captured text."
            )
          )
        }
        matched <- any(vapply(
          captured_text,
          \(text) grepl(quote, text, fixed = TRUE),
          logical(1)
        ))
        if (!matched) {
          tempest_stage_output_abort(
            "Claim quote is not an exact substring of source {.val {source_id}}."
          )
        }
      }
      list(source_id = source_id, url = url, quote = quote)
    }
  )
  keys <- vapply(
    sources,
    function(source) {
      as.character(jsonlite::toJSON(
        list(source_id = source$source_id, quote = source$quote),
        auto_unbox = TRUE,
        null = "null"
      ))
    },
    character(1)
  )
  if (anyDuplicated(keys)) {
    tempest_stage_output_abort(
      "Extracted claims cannot repeat an identical source record."
    )
  }
  sources
}

tempest_stage_fact_source_ids <- function(value, workspace) {
  sources <- tempest_stage_fact_sources(value, workspace)
  unique(vapply(sources, `[[`, character(1), "source_id"))
}

tempest_stage_claim_context_trace_validate <- function(
  claim_context,
  trace_references
) {
  if (
    !is.list(trace_references) ||
      (length(trace_references) > 0L && is.null(names(trace_references)))
  ) {
    tempest_stage_evaluator_abort(
      "Claim evaluator context requires exact execution trace references."
    )
  }
  identifier_fields <- intersect(
    c(
      "claim_id",
      "retrieval_step_id",
      "perspective_id",
      "expert_id",
      "session_id",
      "section_id"
    ),
    names(claim_context)
  )
  for (field in identifier_fields) {
    value <- claim_context[[field]]
    if (
      !is.character(value) ||
        length(value) != 1L ||
        (!is.na(value) && !tempest_opaque_identifier_valid(value))
    ) {
      tempest_stage_governance_abort(
        "Claim context field {.field {field}} is not a safe opaque identifier."
      )
    }
  }
  bindings <- c(
    session_id = "research_run_id",
    expert_id = "expert_id",
    retrieval_step_id = "correlation_id"
  )
  for (claim_field in names(bindings)) {
    value <- claim_context[[claim_field]] %||% NA_character_
    if (is.na(value)) {
      next
    }
    trace_field <- bindings[[claim_field]]
    if (!identical(trace_references[[trace_field]] %||% NULL, value)) {
      tempest_stage_governance_abort(
        paste0(
          "Claim context field {.field {claim_field}} must exactly match ",
          "the executing trace reference {.field {trace_field}}."
        )
      )
    }
  }
  invisible(claim_context)
}

tempest_stage_evaluate_claims <- function(output, context) {
  output <- tryCatch(
    tempest_stage_exact_record(output, "output", "facts"),
    tempest_stage_output_validation_error = function(error) stop(error)
  )
  if (is.null(output$facts)) {
    tempest_stage_output_abort(
      "Claim extraction output must contain the exact facts field."
    )
  }
  facts <- output$facts
  if (!is.list(facts) || is.data.frame(facts) || !is.null(names(facts))) {
    tempest_stage_output_abort(
      "Claim extraction output must be a list of facts."
    )
  }
  workspace <- context$workspace
  known_source_ids <- tempest_stage_known_source_ids(context)
  claim_context <- context$claim_context %||% NULL
  if (!is.list(claim_context) || is.null(names(claim_context))) {
    tempest_stage_evaluator_abort("Claim evaluator context is malformed.")
  }
  claim_arguments <- c(
    "claim_type",
    "contradicting_source_ids",
    "contradiction_note",
    "contradiction_score",
    "source_quality_score",
    "retrieval_query",
    "retrieval_step_id",
    "perspective_id",
    "expert_id",
    "session_id",
    "section_id",
    "claim_id",
    "created_at"
  )
  reserved <- c(
    "claim_text",
    "source_ids",
    "confidence",
    "support_score",
    "evidence_span_ids",
    "supporting_quotes"
  )
  if (
    anyDuplicated(names(claim_context)) ||
      any(names(claim_context) %in% reserved) ||
      any(!names(claim_context) %in% claim_arguments)
  ) {
    tempest_stage_evaluator_abort(
      "Claim evaluator context contains unsupported constructor fields."
    )
  }
  tempest_stage_claim_context_trace_validate(
    claim_context,
    context$execution_trace_references %||% list()
  )
  evidence_spans <- list()
  claims <- lapply(facts, function(fact) {
    fact <- tempest_stage_plain_record(fact, "facts")
    allowed_fact_fields <- c(
      "claim",
      "sources",
      "confidence",
      "support_score",
      "note"
    )
    unexpected_fact_fields <- setdiff(names(fact), allowed_fact_fields)
    if (
      !all(c("claim", "sources") %in% names(fact)) ||
        length(unexpected_fact_fields) > 0L
    ) {
      tempest_stage_output_abort(
        "Claim facts contain missing or unsupported fields."
      )
    }
    claim_text <- tempest_stage_scalar_character(fact$claim, "facts$claim")
    sources <- tempest_stage_fact_sources(fact, workspace)
    source_ids <- unique(vapply(
      sources,
      `[[`,
      character(1),
      "source_id"
    ))
    unknown <- setdiff(source_ids, known_source_ids)
    if (length(unknown) > 0L) {
      tempest_stage_output_abort(
        "Extracted claims cite unknown source IDs: {.val {unknown}}."
      )
    }
    confidence <- fact$confidence %||% "medium"
    if (
      !rlang::is_string(confidence) ||
        !confidence %in% c("low", "medium", "high")
    ) {
      tempest_stage_output_abort(
        "Claim confidence must be low, medium, or high."
      )
    }
    score <- fact$support_score %||% NA_real_
    if (
      !is.numeric(score) ||
        length(score) != 1L ||
        (!is.na(score) && (!is.finite(score) || score < 0 || score > 1))
    ) {
      tempest_stage_output_abort("Claim support score must be in [0, 1].")
    }
    if (!is.null(fact$note)) {
      tempest_stage_scalar_character(fact$note, "facts$note")
    }
    quoted_sources <- Filter(\(source) !is.null(source$quote), sources)
    fact_spans <- lapply(quoted_sources, function(source) {
      tempest_evidence_span(
        source_id = source$source_id,
        quote = source$quote,
        extracted_by = context$program_artifact_id
      )
    })
    evidence_spans <<- c(evidence_spans, fact_spans)
    do.call(
      tempest_claim,
      c(
        list(
          claim_text = claim_text,
          source_ids = source_ids,
          confidence = confidence,
          support_score = as.numeric(score),
          evidence_span_ids = vapply(
            fact_spans,
            \(span) span@evidence_span_id,
            character(1)
          ),
          supporting_quotes = unname(lapply(
            quoted_sources,
            `[[`,
            "quote"
          ))
        ),
        claim_context
      )
    )
  })
  attr(claims, "tempest_evidence_spans") <- unname(evidence_spans)
  list(output = claims, support_status = "unknown")
}

tempest_stage_verification_support_status <- function(
  status,
  score,
  min_support_score
) {
  status <- tempest_apply_min_support_score(
    status,
    score,
    min_support_score = min_support_score
  )
  switch(
    status,
    supported = "verified",
    partially_supported = "partially_supported",
    unsupported = "unsupported",
    contradicted = "conflicted",
    unverifiable = "unknown",
    tempest_stage_evaluator_abort(
      "Verification status cannot be mapped to support trust."
    )
  )
}

tempest_stage_evaluate_verification <- function(output, context) {
  output <- tempest_stage_exact_record(
    output,
    "output",
    c("status", "rationale"),
    "score"
  )
  claim <- context$claim %||% NULL
  span <- context$evidence_span %||% NULL
  if (!S7::S7_inherits(claim, tempest_claim)) {
    tempest_stage_evaluator_abort(
      "Verification evaluator context requires one exact claim record."
    )
  }
  if (!S7::S7_inherits(span, tempest_evidence_span)) {
    tempest_stage_evaluator_abort(
      "Verification evaluator context requires one exact evidence span."
    )
  }
  if (
    !span@evidence_span_id %in% claim@evidence_span_ids ||
      !span@source_id %in% claim@source_ids
  ) {
    tempest_stage_governance_abort(
      "Verification evaluator span must be bound to its exact claim."
    )
  }
  statuses <- c(
    "supported",
    "partially_supported",
    "unsupported",
    "contradicted",
    "unverifiable"
  )
  status <- output$status %||% NULL
  if (!rlang::is_string(status) || !status %in% statuses) {
    tempest_stage_output_abort("Verification output has an invalid status.")
  }
  score <- output$score %||% NA_real_
  if (
    !is.numeric(score) ||
      is.object(score) ||
      !is.null(names(score)) ||
      length(score) != 1L ||
      (!is.na(score) && (!is.finite(score) || score < 0 || score > 1))
  ) {
    tempest_stage_output_abort("Verification score must be in [0, 1].")
  }
  if (identical(status, "unverifiable") && !is.na(score)) {
    tempest_stage_output_abort(
      "Unverifiable claim-span support must omit its score."
    )
  }
  if (!identical(status, "unverifiable") && is.na(score)) {
    tempest_stage_output_abort(
      "A verifiable claim-span support assessment requires a score."
    )
  }
  rationale <- output$rationale
  if (
    !rlang::is_string(rationale) ||
      is.na(rationale) ||
      !nzchar(tempest_trim(rationale)) ||
      !identical(rationale, tempest_trim(rationale)) ||
      nchar(rationale, type = "bytes") > 2000L ||
      tempest_contract_sensitive_scalar(rationale)
  ) {
    tempest_stage_output_abort(
      paste0(
        "Verification rationale must be canonical, non-empty, bounded, ",
        "and credential-free."
      )
    )
  }
  min_support_score <- tempest_normalize_min_support_score(
    context$min_support_score %||% 0.7
  )
  status <- tempest_apply_min_support_score(
    status,
    score,
    min_support_score = min_support_score
  )
  support_status <- tempest_stage_verification_support_status(
    status,
    score,
    min_support_score
  )
  support <- tempest_claim_support(
    claim_id = claim@claim_id,
    evidence_span_id = span@evidence_span_id,
    source_id = span@source_id,
    verification_status = status,
    support_score = as.double(score),
    rationale = rationale
  )
  list(
    output = support,
    support_status = support_status
  )
}

tempest_stage_evaluate_next_question <- function(output, context) {
  output <- tempest_stage_exact_record(
    output,
    "output",
    "question",
    "done"
  )
  done <- if ("done" %in% names(output)) output$done else FALSE
  if (!rlang::is_bool(done)) {
    tempest_stage_output_abort(
      "Next-question output must contain a question and one logical done flag."
    )
  }
  question <- tempest_stage_scalar_character(output$question, "question")
  list(
    output = list(question = question, done = done),
    support_status = "unknown"
  )
}

tempest_stage_evaluate_outline <- function(output, context) {
  output <- tempest_stage_exact_record(
    output,
    "output",
    c("title", "sections")
  )
  sections <- output$sections
  if (
    !is.list(sections) ||
      is.data.frame(sections) ||
      !is.null(names(sections)) ||
      length(sections) == 0L
  ) {
    tempest_stage_output_abort(
      "Outline output must contain a non-empty unnamed sections array."
    )
  }
  for (section in sections) {
    section <- tempest_stage_exact_record(
      section,
      "sections",
      c("title", "summary", "subsections")
    )
    tempest_stage_scalar_character(section$title, "sections$title")
    tempest_stage_scalar_character(section$summary, "sections$summary")
    subsections <- section$subsections
    if (
      !is.list(subsections) ||
        is.data.frame(subsections) ||
        !is.null(names(subsections)) ||
        length(subsections) == 0L
    ) {
      tempest_stage_output_abort(
        "Outline sections require a non-empty unnamed subsections array."
      )
    }
    for (subsection in subsections) {
      subsection <- tempest_stage_exact_record(
        subsection,
        "subsections",
        c("title", "bullets"),
        "needed"
      )
      tempest_stage_scalar_character(subsection$title, "subsections$title")
      tempest_stage_character_vector(subsection$bullets, "subsections$bullets")
      if ("needed" %in% names(subsection)) {
        tempest_stage_character_vector(
          subsection$needed,
          "subsections$needed",
          allow_empty = TRUE
        )
      }
    }
  }
  normalized <- tempest_stage_normalize_output(\() {
    tempest_normalize_outline(output)
  })
  if (length(normalized$sections) == 0L) {
    tempest_stage_output_abort(
      "Outline output must contain at least one section."
    )
  }
  list(output = normalized, support_status = "unknown")
}

tempest_stage_verified_evidence <- function(context) {
  evidence <- tempest_stage_authoritative_claims(
    context,
    "verified_evidence"
  )
  if (!is.list(evidence) || length(evidence) == 0L) {
    tempest_stage_governance_abort(
      "Grounded fallback requires explicit verified evidence."
    )
  }
  min_support_score <- tempest_normalize_min_support_score(
    context$min_support_score %||% 0.7
  )
  workspace <- tempest_stage_workspace(context)
  valid <- vapply(
    evidence,
    function(claim) {
      tempest_stage_claim_verified(claim, workspace, min_support_score)
    },
    logical(1)
  )
  if (!all(valid)) {
    tempest_stage_governance_abort(
      "Grounded fallback evidence must contain only supported claims."
    )
  }
  evidence
}

tempest_stage_workspace <- function(context) {
  workspace <- context$workspace %||% NULL
  if (!inherits(workspace, "ResearchWorkspace")) {
    tempest_stage_evaluator_abort(
      "Grounded stage context requires the authoritative ResearchWorkspace."
    )
  }
  tryCatch(
    workspace$validate_integrity(),
    error = function(error) {
      tempest_stage_evaluator_abort(
        "Grounded stage workspace failed authoritative integrity validation."
      )
    }
  )
  workspace
}

tempest_stage_authoritative_claims <- function(context, field) {
  workspace <- tempest_stage_workspace(context)
  claims <- context[[field]] %||% list()
  if (
    !is.list(claims) ||
      is.data.frame(claims) ||
      !is.null(names(claims)) ||
      !all(vapply(
        claims,
        \(claim) S7::S7_inherits(claim, tempest_claim),
        logical(1)
      ))
  ) {
    tempest_stage_evaluator_abort(
      "Stage context {.field {field}} must be an unnamed list of exact claims."
    )
  }
  ids <- vapply(claims, \(claim) claim@claim_id, character(1))
  if (anyDuplicated(ids)) {
    tempest_stage_evaluator_abort(
      "Stage context {.field {field}} cannot repeat claim IDs."
    )
  }
  for (index in seq_along(claims)) {
    authoritative <- workspace$get_proposed_claim(ids[[index]])
    if (is.null(authoritative) || !identical(authoritative, claims[[index]])) {
      tempest_stage_governance_abort(
        paste0(
          "Stage context {.field {field}} contains a missing, replaced, or ",
          "forged claim."
        )
      )
    }
  }
  claims
}

tempest_stage_claim_support_records <- function(
  workspace,
  claim,
  required = TRUE
) {
  supports <- Filter(
    \(support) identical(support@claim_id, claim@claim_id),
    workspace$list_claim_supports()
  )
  support_span_ids <- vapply(
    supports,
    \(support) support@evidence_span_id,
    character(1)
  )
  exact <- length(supports) > 0L &&
    setequal(support_span_ids, claim@evidence_span_ids)
  if (!exact) {
    if (isTRUE(required)) {
      tempest_stage_governance_abort(
        paste0(
          "Grounded evidence claim {.val {claim@claim_id}} requires the ",
          "complete authoritative claim-by-span support set."
        )
      )
    }
    return(NULL)
  }
  supports
}

tempest_stage_source_evidence_projection <- function(
  workspace,
  source_id,
  required = TRUE
) {
  resource <- workspace$get_retrieved_resource(source_id)
  if (is.null(resource)) {
    if (isTRUE(required)) {
      tempest_stage_governance_abort(
        "Verification source {.val {source_id}} is not in the workspace."
      )
    }
    return(NULL)
  }
  source <- tempest_resource_as_source(resource)
  candidates <- list(
    source$content_text %||% NA_character_,
    source$snippet %||% NA_character_,
    source$context_text %||% NA_character_
  )
  captured_text <- NULL
  for (candidate in candidates) {
    if (
      rlang::is_string(candidate) &&
        !is.na(candidate) &&
        nzchar(tempest_trim(candidate))
    ) {
      captured_text <- candidate
      break
    }
  }
  if (is.null(captured_text)) {
    if (isTRUE(required)) {
      tempest_stage_governance_abort(
        paste0(
          "Verification source {.val {source_id}} has no exact captured ",
          "content, snippet, or context."
        )
      )
    }
    return(NULL)
  }
  title <- source$title %||% ""
  if (!rlang::is_string(title) || is.na(title)) {
    title <- ""
  }
  excerpt <- paste0(
    "[",
    source_id,
    "] ",
    title,
    ": ",
    substr(captured_text, 1L, 1500L)
  )
  list(
    source_id = source_id,
    resource_fingerprint = tempest_resource_fingerprint(resource),
    content_hash = if (is.na(resource@content_hash)) {
      NULL
    } else {
      resource@content_hash
    },
    captured_text_digest = tempest_stage_content_digest_id(captured_text),
    excerpt = excerpt,
    excerpt_digest = tempest_stage_content_digest_id(excerpt)
  )
}

tempest_stage_claim_source_evidence <- function(
  claim,
  workspace,
  required = TRUE
) {
  if (!S7::S7_inherits(claim, tempest_claim)) {
    tempest_stage_evaluator_abort(
      "Verification evidence requires one exact claim."
    )
  }
  if (!inherits(workspace, "ResearchWorkspace")) {
    tempest_stage_evaluator_abort(
      "Verification evidence requires one authoritative ResearchWorkspace."
    )
  }
  lapply(
    claim@source_ids,
    \(source_id) {
      tempest_stage_source_evidence_projection(
        workspace,
        source_id,
        required = required
      )
    }
  )
}

tempest_stage_claim_has_captured_evidence <- function(claim, workspace) {
  if (length(claim@source_ids) == 0L) {
    return(FALSE)
  }
  evidence <- tempest_stage_claim_source_evidence(
    claim,
    workspace,
    required = FALSE
  )
  length(evidence) == length(claim@source_ids) &&
    all(!vapply(evidence, is.null, logical(1)))
}

tempest_stage_claim_verified <- function(claim, workspace, min_support_score) {
  if (!identical(claim@verification_status, "supported")) {
    return(FALSE)
  }
  supports <- tempest_stage_claim_support_records(
    workspace,
    claim,
    required = TRUE
  )
  support_scores <- vapply(
    supports,
    \(support) support@support_score,
    numeric(1)
  )
  all(vapply(
    supports,
    \(support) identical(support@verification_status, "supported"),
    logical(1)
  )) &&
    all(!is.na(support_scores) & support_scores >= min_support_score) &&
    !is.na(claim@support_score) &&
    is.finite(claim@support_score) &&
    claim@support_score >= min_support_score &&
    tempest_stage_claim_has_captured_evidence(claim, workspace)
}

tempest_stage_evidence <- function(context) {
  tempest_stage_authoritative_claims(context, "evidence")
}

tempest_stage_evidence_support <- function(evidence, context) {
  if (length(evidence) == 0L) {
    return("unknown")
  }
  statuses <- vapply(
    evidence,
    \(claim) claim@verification_status,
    character(1)
  )
  if (any(statuses == "contradicted")) {
    return("conflicted")
  }
  min_support_score <- tempest_normalize_min_support_score(
    context$min_support_score %||% 0.7
  )
  verified <- vapply(
    evidence,
    function(claim) {
      identical(claim@verification_status, "supported") &&
        !is.na(claim@support_score) &&
        claim@support_score >= min_support_score
    },
    logical(1)
  )
  low_score_supported <- vapply(
    evidence,
    function(claim) {
      identical(claim@verification_status, "supported") &&
        !is.na(claim@support_score) &&
        claim@support_score < min_support_score
    },
    logical(1)
  )
  if (any(statuses == "unsupported") || any(low_score_supported)) {
    return("unsupported")
  }
  unknown <- statuses %in%
    c("unverified", "unverifiable") |
    (statuses == "supported" & !verified)
  if (any(unknown)) {
    return("unknown")
  }
  if (any(statuses == "partially_supported")) {
    return("partially_supported")
  }
  if (all(verified)) {
    return("verified")
  }
  "unknown"
}

tempest_stage_evaluate_evidence_outline <- function(output, context) {
  evidence <- tempest_stage_evidence(context)
  evaluated <- tempest_stage_evaluate_outline(output, context)
  # An outline is planning metadata rather than a publishable factual product.
  # Its evidence trace remains durable, but it cannot itself assert verification.
  evaluated$support_status <- "unknown"
  evaluated
}

tempest_stage_assertion_normalize <- function(value) {
  value <- gsub("\\[S[0-9a-f]{12}\\]", "", value, perl = TRUE)
  value <- gsub("^[[:space:]]*#{1,6}[[:space:]]+", "", value)
  value <- gsub("^[[:space:]]*[-*+][[:space:]]+", "", value)
  value <- gsub("[[:space:]]+", " ", tempest_trim(value))
  value <- sub("[.!?]+$", "", value)
  tolower(tempest_trim(value))
}

tempest_stage_text_assertions <- function(text) {
  lines <- strsplit(text, "\\n", fixed = FALSE)[[1]]
  lines <- lines[nzchar(tempest_trim(lines))]
  lines <- lines[!tempest_markdown_structural_heading(lines)]
  if (length(lines) == 0L) {
    return(character())
  }
  assertions <- unlist(
    lapply(lines, \(line) strsplit(line, "(?<=[.!?])\\s+", perl = TRUE)[[1]]),
    use.names = FALSE
  )
  assertions[nzchar(tempest_stage_assertion_normalize(assertions))]
}

tempest_stage_bind_text_assertions <- function(text, evidence, context) {
  if (!tempest_citation_tokens_valid(text)) {
    tempest_stage_output_abort(
      "Grounded writing contains a malformed source-citation token."
    )
  }
  min_support_score <- tempest_normalize_min_support_score(
    context$min_support_score %||% 0.7
  )
  workspace <- tempest_stage_workspace(context)
  verified <- Filter(
    function(claim) {
      tempest_stage_claim_verified(claim, workspace, min_support_score)
    },
    evidence
  )
  assertions <- tempest_stage_text_assertions(
    text
  )
  if (length(assertions) == 0L) {
    tempest_stage_output_abort(
      "Grounded writing must contain at least one cited factual assertion."
    )
  }
  for (assertion in assertions) {
    citations <- tempest_extract_citation_ids(assertion)
    if (length(citations) == 0L) {
      tempest_stage_output_abort(
        "Every grounded writing assertion must carry a source citation."
      )
    }
    normalized <- tempest_stage_assertion_normalize(assertion)
    matches <- Filter(
      \(claim) {
        identical(
          tempest_stage_assertion_normalize(claim@claim_text),
          normalized
        )
      },
      verified
    )
    bound <- vapply(
      matches,
      function(claim) {
        length(citations) > 0L && setequal(citations, claim@source_ids)
      },
      logical(1)
    )
    if (!any(bound)) {
      tempest_stage_output_abort(
        paste0(
          "Every grounded writing assertion must exactly match a ",
          "threshold-verified workspace claim and cite only its sources."
        )
      )
    }
  }
  invisible(assertions)
}

tempest_stage_evaluate_evidence_text <- function(output, context, field) {
  evidence <- tempest_stage_evidence(context)
  output <- tempest_stage_exact_record(output, "output", field)
  text <- output[[field]]
  text <- tempest_stage_scalar_character(text, field)
  tempest_stage_bind_text_assertions(text, evidence, context)
  list(
    output = text,
    support_status = "verified"
  )
}

tempest_stage_evaluator_registry <- function() {
  list(
    perspectives = tempest_stage_evaluate_perspectives,
    personas = tempest_stage_evaluate_personas,
    query_decomposition = tempest_stage_evaluate_queries,
    extract_claims = tempest_stage_evaluate_claims,
    verify_claim_support = tempest_stage_evaluate_verification,
    next_question = tempest_stage_evaluate_next_question,
    draft_outline = tempest_stage_evaluate_outline,
    refined_outline = tempest_stage_evaluate_evidence_outline,
    section_writing = \(output, context) {
      tempest_stage_evaluate_evidence_text(output, context, "section_text")
    },
    lead_section = \(output, context) {
      tempest_stage_evaluate_evidence_text(output, context, "lead_section")
    }
  )
}

tempest_stage_evaluator_resolve <- function(execution) {
  if (!inherits(execution, "tempest_dsprrr_execution")) {
    tempest_stage_evaluator_abort(
      "Stage execution must be resolved from a TempestProgramSet."
    )
  }
  stage <- execution$stage %||% NULL
  if (!rlang::is_string(stage) || !stage %in% tempest_program_set_stages()) {
    tempest_stage_evaluator_abort("Stage execution metadata is incomplete.")
  }
  expected <- tempest_program_set_default_evaluators()[[stage]]
  if (
    !identical(execution$evaluator_id, expected$evaluator_id) ||
      !identical(execution$evaluator_version, expected$evaluator_version)
  ) {
    tempest_stage_evaluator_abort(
      "Stage execution references an unknown builtin evaluator pair."
    )
  }
  evaluator <- tempest_stage_evaluator_registry()[[stage]] %||% NULL
  if (!is.function(evaluator)) {
    tempest_stage_evaluator_abort("Builtin stage evaluator is unavailable.")
  }
  list(
    stage = stage,
    evaluator_id = expected$evaluator_id,
    evaluator_version = expected$evaluator_version,
    evaluate = evaluator
  )
}

tempest_stage_evaluate <- function(execution, output, context = list()) {
  if (!is.list(context)) {
    tempest_stage_evaluator_abort("{.arg context} must be a list.")
  }
  evaluator <- tempest_stage_evaluator_resolve(execution)
  context$program_artifact_id <- execution$program_artifact_id
  context$execution_trace_references <- execution$trace_context
  result <- tryCatch(
    evaluator$evaluate(output, context),
    error = identity
  )
  if (!inherits(result, "condition")) {
    return(result)
  }
  if (
    inherits(result, "tempest_stage_output_validation_error") ||
      inherits(result, "tempest_stage_governance_error")
  ) {
    stop(result)
  }
  tempest_stage_evaluator_abort(
    "Builtin stage evaluator failed its static contract."
  )
}

tempest_stage_content_digest_id <- function(output) {
  canonical <- tryCatch(
    tempest_research_manifest_canonical_value(output, "stage_output"),
    error = function(error) {
      tempest_stage_evaluator_abort(
        "Stage output cannot be represented by a canonical content digest."
      )
    }
  )
  json <- jsonlite::toJSON(
    canonical,
    auto_unbox = TRUE,
    null = "null",
    digits = NA
  )
  paste0(
    "sha256:",
    digest::digest(json, algo = "sha256", serialize = FALSE)
  )
}

tempest_stage_content_reference <- function(output) {
  digest <- tempest_stage_content_digest_id(output)
  tempest_stage_output_reference(
    "content_digest",
    digest,
    content_digest = digest
  )
}

tempest_stage_record_output_digest <- function(records, record, what) {
  if (!S7::S7_inherits(record, TempestStageRecord)) {
    tempest_stage_evaluator_abort(
      "Stage output provenance requires an exact TempestStageRecord."
    )
  }
  nullable <- function(value) {
    if (is.list(value)) {
      return(lapply(value, nullable))
    }
    if (length(value) == 1L && is.atomic(value) && is.na(value)) {
      return(NULL)
    }
    value
  }
  tempest_stage_content_digest_id(nullable(list(
    schema_version = 3L,
    stage = record@stage,
    attempt_id = record@attempt_id,
    program_artifact_id = record@program_artifact_id,
    trace_references = record@trace_references,
    record_kind = what,
    records = records
  )))
}

tempest_stage_claims_output_digest <- function(
  claims,
  record,
  evidence_spans = attr(claims, "tempest_evidence_spans", exact = TRUE)
) {
  if (
    !is.list(claims) ||
      is.data.frame(claims) ||
      !is.null(names(claims)) ||
      !all(vapply(
        claims,
        \(claim) S7::S7_inherits(claim, tempest_claim),
        logical(1)
      ))
  ) {
    tempest_stage_evaluator_abort(
      "Claim output digest requires an unnamed list of exact claims."
    )
  }
  ids <- vapply(claims, \(claim) claim@claim_id, character(1))
  if (anyDuplicated(ids)) {
    tempest_stage_evaluator_abort(
      "Claim output digest cannot contain duplicate claim IDs."
    )
  }
  extraction_fields <- c(
    "claim_id",
    "claim_text",
    "claim_type",
    "source_ids",
    "evidence_span_ids",
    "supporting_quotes",
    "contradicting_source_ids",
    "contradiction_note",
    "confidence",
    "contradiction_score",
    "source_quality_score",
    "retrieval_query",
    "retrieval_step_id",
    "perspective_id",
    "expert_id",
    "session_id",
    "section_id",
    "created_at"
  )
  claim_records <- lapply(claims, function(claim) {
    tryCatch(
      S7::validate(claim),
      error = function(error) {
        tempest_stage_evaluator_abort(
          "Claim output digest received an invalid claim."
        )
      }
    )
    tempest_claim_to_list(claim)[extraction_fields]
  })
  referenced_span_ids <- as.character(unname(unlist(
    lapply(claims, \(claim) claim@evidence_span_ids),
    use.names = FALSE
  )))
  if (anyDuplicated(referenced_span_ids)) {
    tempest_stage_evaluator_abort(
      "Claim output digest cannot repeat referenced evidence-span IDs."
    )
  }
  if (is.null(evidence_spans)) {
    evidence_spans <- list()
  }
  if (
    !is.list(evidence_spans) ||
      is.data.frame(evidence_spans) ||
      !is.null(names(evidence_spans)) ||
      !all(vapply(
        evidence_spans,
        \(span) S7::S7_inherits(span, tempest_evidence_span),
        logical(1)
      ))
  ) {
    tempest_stage_evaluator_abort(
      "Claim output digest requires an unnamed list of exact evidence spans."
    )
  }
  span_ids <- vapply(
    evidence_spans,
    \(span) span@evidence_span_id,
    character(1)
  )
  if (!identical(span_ids, referenced_span_ids)) {
    tempest_stage_evaluator_abort(
      paste0(
        "Claim output digest requires the exact referenced evidence spans ",
        "in claim order."
      )
    )
  }
  span_records <- lapply(evidence_spans, function(span) {
    tryCatch(
      S7::validate(span),
      error = function(error) {
        tempest_stage_evaluator_abort(
          "Claim output digest received an invalid evidence span."
        )
      }
    )
    if (!identical(span@extracted_by, record@program_artifact_id)) {
      tempest_stage_evaluator_abort(
        paste0(
          "Extracted evidence-span provenance must match the executing ",
          "program artifact."
        )
      )
    }
    tempest_evidence_span_to_list(span)
  })
  tempest_stage_record_output_digest(
    list(
      claims = claim_records,
      evidence_spans = span_records
    ),
    record,
    "workspace_claims"
  )
}

tempest_stage_verification_output_digest <- function(
  support,
  record,
  claim,
  evidence_span,
  workspace
) {
  if (!S7::S7_inherits(support, TempestClaimSupport)) {
    tempest_stage_evaluator_abort(
      "Verification output digest requires one exact claim-support record."
    )
  }
  tryCatch(
    S7::validate(support),
    error = function(error) {
      tempest_stage_evaluator_abort(
        "Verification output digest received invalid claim support."
      )
    }
  )
  if (
    !S7::S7_inherits(claim, tempest_claim) ||
      !S7::S7_inherits(evidence_span, tempest_evidence_span) ||
      !identical(support@claim_id, claim@claim_id) ||
      !identical(
        support@evidence_span_id,
        evidence_span@evidence_span_id
      ) ||
      !identical(support@source_id, evidence_span@source_id) ||
      !support@evidence_span_id %in% claim@evidence_span_ids ||
      !support@source_id %in% claim@source_ids
  ) {
    tempest_stage_evaluator_abort(
      paste0(
        "Verification output digest must bind the exact claim, evidence ",
        "span, and source."
      )
    )
  }
  authoritative_claim <- workspace$get_proposed_claim(claim@claim_id)
  authoritative_span <- workspace$get_evidence_span(
    evidence_span@evidence_span_id
  )
  if (
    is.null(authoritative_claim) ||
      is.null(authoritative_span) ||
      !identical(authoritative_claim, claim) ||
      !identical(authoritative_span, evidence_span)
  ) {
    tempest_stage_governance_abort(
      "Verification output digest requires authoritative workspace inputs."
    )
  }
  claim_record <- tempest_claim_to_list(claim)
  claim_record[c(
    "support_score",
    "verification_status",
    "verified_at",
    "verifier_model"
  )] <- NULL
  source_evidence <- tempest_stage_source_evidence_projection(
    workspace,
    evidence_span@source_id,
    required = TRUE
  )
  tempest_stage_record_output_digest(
    list(
      claim_support = tempest_claim_support_to_list(support),
      claim = claim_record,
      evidence_span = tempest_evidence_span_to_list(evidence_span),
      source_evidence = source_evidence
    ),
    record,
    "claim_supports"
  )
}

tempest_stage_state_output_value <- function(stage, output) {
  switch(
    stage,
    perspectives = list(
      title = output$title,
      perspectives = output$perspectives
    ),
    personas = tempest_expert_records(output),
    draft_outline = output,
    refined_outline = output,
    tempest_stage_evaluator_abort(
      "Stage {.val {stage}} has no state-field output digest contract."
    )
  )
}

tempest_stage_state_output_digest <- function(stage, output) {
  tempest_stage_content_digest_id(
    tempest_stage_state_output_value(stage, output)
  )
}

tempest_stage_output_reference_validate_stage <- function(stage, reference) {
  reference <- tempest_stage_output_reference_validate(reference)
  expected <- switch(
    stage,
    perspectives = list(kind = "state_field", ids = c("title", "perspectives")),
    personas = list(kind = "state_field", ids = "experts"),
    extract_claims = list(kind = "workspace_claims", ids = NULL),
    verify_claim_support = list(kind = "claim_supports", ids = NULL),
    draft_outline = list(kind = "state_field", ids = "draft_outline"),
    refined_outline = list(kind = "state_field", ids = "outline"),
    query_decomposition = list(kind = "content_digest", ids = NULL),
    next_question = list(kind = "content_digest", ids = NULL),
    section_writing = list(kind = "content_digest", ids = NULL),
    lead_section = list(kind = "content_digest", ids = NULL)
  )
  ids <- unlist(reference$ids, use.names = FALSE)
  if (!identical(reference$kind, expected$kind)) {
    tempest_stage_evaluator_abort(
      "Stage output reference kind does not match the closed stage mapping."
    )
  }
  if (!is.null(expected$ids) && !identical(ids, expected$ids)) {
    tempest_stage_evaluator_abort(
      "Stage output reference fields do not match the closed stage mapping."
    )
  }
  reference
}

tempest_stage_output_reference_derive <- function(
  output,
  record,
  context,
  output_reference = NULL
) {
  stage <- record@stage
  reference <- switch(
    stage,
    perspectives = tempest_stage_output_reference(
      "state_field",
      c("title", "perspectives"),
      content_digest = tempest_stage_state_output_digest(stage, output)
    ),
    personas = tempest_stage_output_reference(
      "state_field",
      "experts",
      content_digest = tempest_stage_state_output_digest(stage, output)
    ),
    draft_outline = tempest_stage_output_reference(
      "state_field",
      "draft_outline",
      content_digest = tempest_stage_state_output_digest(stage, output)
    ),
    refined_outline = tempest_stage_output_reference(
      "state_field",
      "outline",
      content_digest = tempest_stage_state_output_digest(stage, output)
    ),
    query_decomposition = tempest_stage_content_reference(output),
    next_question = tempest_stage_content_reference(output),
    section_writing = tempest_stage_content_reference(output),
    lead_section = tempest_stage_content_reference(output),
    extract_claims = NULL,
    verify_claim_support = NULL
  )
  if (stage %in% c("extract_claims", "verify_claim_support")) {
    if (!is.function(output_reference)) {
      tempest_stage_evaluator_abort(
        "Claim and verification stages require an exact output-reference callback."
      )
    }
    reference <- output_reference(output, record, context)
    reference <- tempest_stage_output_reference_validate_stage(stage, reference)
    expected_ids <- if (identical(stage, "extract_claims")) {
      vapply(output, \(claim) claim@claim_id, character(1))
    } else {
      output@claim_support_id
    }
    actual_ids <- unlist(reference$ids, use.names = FALSE)
    if (length(actual_ids) == 0L) {
      actual_ids <- character()
    }
    if (!identical(actual_ids, unname(expected_ids))) {
      tempest_stage_evaluator_abort(
        "Stage output reference IDs do not match the evaluated output."
      )
    }
    expected_digest <- if (identical(stage, "extract_claims")) {
      tempest_stage_claims_output_digest(output, record)
    } else {
      tempest_stage_verification_output_digest(
        output,
        record,
        context$claim,
        context$evidence_span,
        context$workspace
      )
    }
    if (!identical(reference$content_digest, expected_digest)) {
      tempest_stage_evaluator_abort(
        "Stage output reference digest does not match the evaluated output."
      )
    }
  } else if (!is.null(output_reference)) {
    tempest_stage_evaluator_abort(
      "Only claim and verification stages accept output-reference callbacks."
    )
  }
  tempest_stage_output_reference_validate_stage(stage, reference)
}

tempest_stage_preflight_record <- function(value, required, label) {
  if (
    is.list(value) &&
      !is.data.frame(value) &&
      length(value) == 0L &&
      length(required) == 0L
  ) {
    return(list())
  }
  fields <- names(value)
  if (
    !is.list(value) ||
      is.data.frame(value) ||
      is.null(fields) ||
      anyNA(fields) ||
      any(!nzchar(fields)) ||
      anyDuplicated(fields) ||
      !setequal(fields, required)
  ) {
    tempest_stage_evaluator_abort(
      "Stage {.field {label}} must contain exactly {.field {required}}."
    )
  }
  value[required]
}

tempest_stage_preflight_string <- function(value, field, allow_empty = FALSE) {
  valid <- rlang::is_string(value) &&
    !is.na(value) &&
    (isTRUE(allow_empty) || nzchar(tempest_trim(value)))
  if (!valid) {
    tempest_stage_evaluator_abort(
      "Stage input {.field {field}} must be one valid string."
    )
  }
  value
}

tempest_stage_preflight_count <- function(value, field) {
  if (
    !is.numeric(value) ||
      is.object(value) ||
      !is.null(names(value)) ||
      length(value) != 1L ||
      is.na(value) ||
      !is.finite(value) ||
      value < 1L ||
      value != as.integer(value)
  ) {
    tempest_stage_evaluator_abort(
      "Stage context {.field {field}} must be one positive whole number."
    )
  }
  as.integer(value)
}

tempest_stage_preflight_score <- function(value, field) {
  tryCatch(
    tempest_normalize_min_support_score(value),
    error = function(error) {
      tempest_stage_evaluator_abort(
        "Stage context {.field {field}} must be one finite score in [0, 1]."
      )
    }
  )
}

tempest_stage_input_fields <- function(stage) {
  switch(
    stage,
    perspectives = c("topic", "seed_context", "n_experts"),
    personas = c("topic", "n_experts", "requirements"),
    query_decomposition = c("question", "topic"),
    extract_claims = c(
      "answer_text",
      "source_context",
      "source_ids",
      "citation_mode"
    ),
    verify_claim_support = c("claim_text", "source_excerpts"),
    next_question = c("topic", "perspective", "answered", "facts"),
    draft_outline = c("topic", "report_title"),
    refined_outline = c("topic", "report_title", "draft_outline", "facts"),
    section_writing = c(
      "section_title",
      "section_summary",
      "subsections",
      "facts"
    ),
    lead_section = c("topic", "title", "article_body", "facts")
  )
}

tempest_stage_context_fields <- function(stage) {
  switch(
    stage,
    perspectives = c("topic", "n_experts"),
    personas = "n_experts",
    query_decomposition = "max_queries",
    extract_claims = c("workspace", "known_source_ids", "claim_context"),
    verify_claim_support = c(
      "workspace",
      "claim",
      "evidence_span",
      "min_support_score",
      "verified_at",
      "verifier_model"
    ),
    next_question = character(),
    draft_outline = character(),
    refined_outline = c(
      "workspace",
      "title",
      "evidence",
      "verified_evidence",
      "verified_facts",
      "min_support_score"
    ),
    section_writing = c(
      "workspace",
      "evidence",
      "verified_evidence",
      "verified_facts",
      "min_support_score"
    ),
    lead_section = c(
      "workspace",
      "evidence",
      "verified_evidence",
      "verified_facts",
      "min_support_score"
    )
  )
}

tempest_stage_execution_contract_preflight <- function(
  execution,
  inputs,
  context
) {
  stage <- execution$stage
  inputs <- tempest_stage_preflight_record(
    inputs,
    tempest_stage_input_fields(stage),
    "inputs"
  )
  common_context <- intersect(
    c(
      "attempt_id",
      "now",
      "stage_records",
      "knowledge_view",
      "deputy_execution"
    ),
    names(context) %||% character()
  )
  required_context <- tempest_stage_context_fields(stage)
  context <- tempest_stage_preflight_record(
    context,
    c(required_context, common_context),
    "context"
  )
  tempest_stage_deputy_execution_references(context)
  if (!is.null(context$attempt_id)) {
    tempest_stage_trace_identifier(context$attempt_id, "attempt_id")
  }
  if (!is.null(context$now) && !is.function(context$now)) {
    tempest_stage_evaluator_abort("Stage clock context must be a function.")
  }
  if (!is.null(context$stage_records)) {
    tempest_stage_records_validate(context$stage_records)
  }

  count_fields <- intersect(c("n_experts"), names(inputs))
  for (field in count_fields) {
    inputs[[field]] <- tempest_stage_preflight_count(inputs[[field]], field)
  }
  allow_empty <- c(
    "seed_context",
    "source_context",
    "source_ids",
    "answered",
    "facts",
    "section_summary",
    "subsections",
    "article_body"
  )
  string_fields <- setdiff(names(inputs), c("n_experts"))
  for (field in string_fields) {
    tempest_stage_preflight_string(
      inputs[[field]],
      field,
      allow_empty = field %in% allow_empty
    )
  }
  if (
    identical(stage, "extract_claims") &&
      !inputs$citation_mode %in%
        c(
          "provider_native",
          "url",
          "tempest_inline",
          "mixed"
        )
  ) {
    tempest_stage_evaluator_abort(
      "Claim extraction citation mode is outside the closed contract."
    )
  }

  if (stage %in% c("perspectives", "personas")) {
    n_experts <- tempest_stage_preflight_count(
      context$n_experts,
      "n_experts"
    )
    if (!identical(n_experts, inputs$n_experts)) {
      tempest_stage_governance_abort(
        "Stage expert budget must match its exact provider input."
      )
    }
  }
  if (identical(stage, "perspectives")) {
    topic <- tempest_stage_preflight_string(context$topic, "topic")
    if (!identical(topic, inputs$topic)) {
      tempest_stage_governance_abort(
        "Perspective topic context must match its exact provider input."
      )
    }
  }
  if (identical(stage, "query_decomposition")) {
    tempest_stage_preflight_count(context$max_queries, "max_queries")
  }
  if (identical(stage, "extract_claims")) {
    workspace <- tempest_stage_workspace(context)
    known_source_ids <- context$known_source_ids
    if (
      !is.character(known_source_ids) ||
        is.object(known_source_ids) ||
        !is.null(names(known_source_ids)) ||
        anyNA(known_source_ids) ||
        anyDuplicated(known_source_ids) ||
        !all(vapply(
          known_source_ids,
          tempest_opaque_identifier_valid,
          logical(1)
        ))
    ) {
      tempest_stage_evaluator_abort(
        "Known source context must be a unique safe identifier array."
      )
    }
    authoritative_ids <- vapply(
      workspace$list_retrieved_resources(),
      \(resource) resource@resource_id,
      character(1)
    )
    if (!identical(known_source_ids, authoritative_ids)) {
      tempest_stage_governance_abort(
        "Known source context must exactly match the authoritative workspace."
      )
    }
    claim_fields <- c(
      "claim_type",
      "session_id",
      "expert_id",
      "retrieval_step_id",
      "perspective_id",
      "section_id"
    )
    claim_context <- tempest_stage_preflight_record(
      context$claim_context,
      claim_fields,
      "claim_context"
    )
    if (!identical(claim_context$claim_type, "finding")) {
      tempest_stage_governance_abort(
        "Claim extraction context must use the fixed finding claim type."
      )
    }
    tempest_stage_claim_context_trace_validate(
      claim_context,
      execution$trace_context
    )
  }
  if (identical(stage, "verify_claim_support")) {
    if (!S7::S7_inherits(context$claim, tempest_claim)) {
      tempest_stage_evaluator_abort(
        "Verification stage context requires one exact claim."
      )
    }
    if (!S7::S7_inherits(context$evidence_span, tempest_evidence_span)) {
      tempest_stage_evaluator_abort(
        "Verification stage context requires one exact evidence span."
      )
    }
    tempest_stage_preflight_score(
      context$min_support_score,
      "min_support_score"
    )
    if (!tempest_ledger_timestamp_valid(context$verified_at)) {
      tempest_stage_evaluator_abort(
        paste0(
          "Verification stage context requires one exact canonical UTC ",
          "verified_at timestamp."
        )
      )
    }
    if (
      !tempest_ledger_identifier_valid(
        context$verifier_model,
        optional = TRUE
      )
    ) {
      tempest_stage_evaluator_abort(
        paste0(
          "Verification stage context verifier_model must be `NA` or one ",
          "bounded credential-free identifier."
        )
      )
    }
    if (!identical(inputs$claim_text, context$claim@claim_text)) {
      tempest_stage_governance_abort(
        "Verification input text must match the authoritative claim."
      )
    }
  }
  if (stage %in% c("refined_outline", "section_writing", "lead_section")) {
    tempest_stage_preflight_score(
      context$min_support_score,
      "min_support_score"
    )
    tempest_stage_preflight_string(
      context$verified_facts,
      "verified_facts",
      allow_empty = TRUE
    )
  }
  if (identical(stage, "refined_outline")) {
    title <- tempest_stage_preflight_string(context$title, "title")
    if (!identical(title, inputs$report_title)) {
      tempest_stage_governance_abort(
        "Refined-outline title must match its exact provider input."
      )
    }
  }
  invisible(TRUE)
}

tempest_stage_executor_preflight <- function(
  execution,
  inputs,
  context,
  record_stage,
  output_reference,
  is_current = NULL
) {
  stage <- execution$stage
  if (!is.list(inputs) || !is.list(context)) {
    tempest_stage_evaluator_abort("Stage inputs and context must be lists.")
  }
  if (!is.function(record_stage)) {
    tempest_stage_evaluator_abort("{.arg record_stage} must be a function.")
  }
  claim_stage <- stage %in% c("extract_claims", "verify_claim_support")
  if (claim_stage && !is.function(output_reference)) {
    tempest_stage_evaluator_abort(
      "Claim and verification stages require an output-reference callback."
    )
  }
  if (!claim_stage && !is.null(output_reference)) {
    tempest_stage_evaluator_abort(
      "Only claim and verification stages accept output-reference callbacks."
    )
  }
  policy <- tempest_stage_policy(stage)
  tempest_stage_fallback_resolve(stage)
  if (!is.null(is_current) && !is.function(is_current)) {
    tempest_stage_evaluator_abort("{.arg is_current} must be a function.")
  }
  tempest_stage_execution_contract_preflight(execution, inputs, context)
  if (
    stage %in%
      c(
        "extract_claims",
        "verify_claim_support",
        "refined_outline",
        "section_writing",
        "lead_section"
      )
  ) {
    workspace <- tempest_stage_workspace(context)
    if (identical(stage, "verify_claim_support")) {
      claim <- context$claim %||% NULL
      evidence_span <- context$evidence_span %||% NULL
      if (!S7::S7_inherits(claim, tempest_claim)) {
        tempest_stage_evaluator_abort(
          "Verification stage context requires one exact claim."
        )
      }
      if (!S7::S7_inherits(evidence_span, tempest_evidence_span)) {
        tempest_stage_evaluator_abort(
          "Verification stage context requires one exact evidence span."
        )
      }
      authoritative <- workspace$get_proposed_claim(claim@claim_id)
      if (is.null(authoritative) || !identical(authoritative, claim)) {
        tempest_stage_governance_abort(
          "Verification stage context claim is missing, replaced, or forged."
        )
      }
      authoritative_span <- workspace$get_evidence_span(
        evidence_span@evidence_span_id
      )
      if (
        is.null(authoritative_span) ||
          !identical(authoritative_span, evidence_span) ||
          !evidence_span@evidence_span_id %in% claim@evidence_span_ids ||
          !evidence_span@source_id %in% claim@source_ids
      ) {
        tempest_stage_governance_abort(
          paste0(
            "Verification stage context evidence span is missing, ",
            "replaced, forged, or not bound to the claim."
          )
        )
      }
      expected_span_input <- tempest_verification_span_input(
        claim,
        evidence_span,
        workspace
      )
      if (!identical(inputs$source_excerpts, expected_span_input)) {
        tempest_stage_governance_abort(
          paste0(
            "Verification span input must match the exact captured workspace ",
            "evidence."
          )
        )
      }
    }
    if (
      stage %in%
        c(
          "refined_outline",
          "section_writing",
          "lead_section"
        )
    ) {
      evidence <- tempest_stage_authoritative_claims(context, "evidence")
      verified_evidence <- tempest_stage_authoritative_claims(
        context,
        "verified_evidence"
      )
      min_support_score <- tempest_normalize_min_support_score(
        context$min_support_score %||% 0.7
      )
      workspace <- tempest_stage_workspace(context)
      supported <- Filter(
        \(claim) identical(claim@verification_status, "supported"),
        evidence
      )
      for (claim in supported) {
        tempest_stage_claim_support_records(
          workspace,
          claim,
          required = TRUE
        )
      }
      valid_verified <- vapply(
        verified_evidence,
        tempest_stage_claim_verified,
        logical(1),
        workspace = workspace,
        min_support_score = min_support_score
      )
      if (!all(valid_verified)) {
        tempest_stage_governance_abort(
          paste0(
            "Verified-evidence context must contain only threshold-supported ",
            "claims with exact citation-audit rows."
          )
        )
      }
    }
  }
  invisible(policy)
}

tempest_stage_record_discard <- function(record, output = NULL) {
  invisible(record)
}

tempest_stage_result <- function(output, record) {
  structure(
    list(output = output, record = record),
    class = c("tempest_stage_result", "list")
  )
}

tempest_stage_result_output <- function(result) {
  if (!inherits(result, "tempest_stage_result")) {
    tempest_stage_evaluator_abort("{.arg result} must be a stage result.")
  }
  result$output
}

tempest_stage_error_static <- function(error) {
  classes <- c(
    "tempest_ecosystem_contract_error",
    "tempest_governed_procedure_error",
    "tempest_program_set_error",
    "tempest_stage_evaluator_contract_error",
    "tempest_stage_governance_error",
    "tempest_stage_commit_error",
    "tempest_stage_cancelled",
    "dsprrr_trace_context_error",
    "dsprrr_trace_contract_error",
    "dsprrr_program_trace_contract_error"
  )
  any(vapply(classes, \(class) inherits(error, class), logical(1))) ||
    any(grepl("^dsprrr_(artifact_|program_artifact_)", class(error)))
}

tempest_stage_error_retryable <- function(error) {
  !tempest_stage_error_static(error) &&
    !inherits(error, "tempest_stage_output_validation_error")
}

tempest_stage_error_allows_fallback <- function(error) {
  !tempest_stage_error_static(error)
}

tempest_stage_execution_attempt <- function(execution, attempt_id) {
  if (!is.null(execution$trace_context$stage_attempt_id)) {
    tempest_stage_evaluator_abort(
      "A ProgramSet execution cannot be reused across stage attempts."
    )
  }
  execution$trace_context$stage_attempt_id <- attempt_id
  execution$trace_context <- tempest_research_manifest_canonical_value(
    execution$trace_context,
    "trace_context"
  )
  execution
}

tempest_stage_context_claim_ids <- function(context, field) {
  claims <- context[[field]] %||% list()
  if (
    !is.list(claims) ||
      is.data.frame(claims) ||
      !is.null(names(claims)) ||
      !all(vapply(
        claims,
        \(claim) S7::S7_inherits(claim, tempest_claim),
        logical(1)
      ))
  ) {
    tempest_stage_evaluator_abort(
      "Stage context {.field {field}} must be an unnamed list of exact claims."
    )
  }
  ids <- vapply(claims, \(claim) claim@claim_id, character(1))
  if (anyDuplicated(ids)) {
    tempest_stage_evaluator_abort(
      "Stage context {.field {field}} cannot repeat claim IDs."
    )
  }
  unname(ids)
}

tempest_stage_deputy_execution_references <- function(context) {
  deputy_execution <- context$deputy_execution %||% NULL
  if (is.null(deputy_execution)) {
    return(list())
  }
  base_fields <- c("deputy_run_id", "deputy_session_id")
  delegation_fields <- c("parent_run_id", "delegation_id", "tool_call_id")
  fields <- names(deputy_execution)
  valid_shape <- identical(fields, base_fields) ||
    identical(fields, c(base_fields, delegation_fields))
  if (
    !is.list(deputy_execution) ||
      is.data.frame(deputy_execution) ||
      !valid_shape
  ) {
    tempest_stage_governance_abort(
      paste0(
        "Stage Deputy execution context must contain the exact run/session ",
        "pair and, when delegated, the complete parent/delegation/tool tuple."
      )
    )
  }
  for (field in fields) {
    value <- deputy_execution[[field]]
    if (
      !is.character(value) ||
        length(value) != 1L ||
        is.object(value) ||
        !is.null(names(value)) ||
        !tempest_opaque_identifier_valid(value)
    ) {
      tempest_stage_governance_abort(
        paste0(
          "Stage Deputy execution field {.field {field}} must be a bounded ",
          "opaque identifier, not prose or credentials."
        )
      )
    }
  }
  if (
    "parent_run_id" %in%
      fields &&
      identical(
        deputy_execution$parent_run_id,
        deputy_execution$deputy_run_id
      )
  ) {
    tempest_stage_governance_abort(
      "A delegated Deputy execution cannot be its own parent run."
    )
  }
  deputy_execution
}

tempest_stage_execution_trace_references <- function(execution, context) {
  execution_fields <- c(
    "research_run_id",
    "stage_attempt_id",
    "deputy_run_id",
    "deputy_session_id",
    "parent_run_id",
    "delegation_id",
    "tool_call_id",
    "trace_id",
    "knowledge_snapshot_id",
    "expert_id",
    "correlation_id",
    "mode",
    "role"
  )
  references <- execution$trace_context[
    intersect(execution_fields, names(execution$trace_context))
  ]
  deputy_execution <- tempest_stage_deputy_execution_references(context)
  for (field in names(deputy_execution)) {
    existing <- references[[field]] %||% NULL
    if (!is.null(existing) && !identical(existing, deputy_execution[[field]])) {
      tempest_stage_governance_abort(
        "Stage Deputy execution identity conflicts with the ProgramSet trace."
      )
    }
    references[[field]] <- deputy_execution[[field]]
  }
  if (identical(execution$stage, "verify_claim_support")) {
    references$min_support_score <-
      tempest_stage_support_threshold_string(context$min_support_score)
    references$verified_at <- context$verified_at
    if (!is.na(context$verifier_model)) {
      references$verifier_model <- context$verifier_model
    }
  }
  if (
    execution$stage %in%
      c(
        "refined_outline",
        "section_writing",
        "lead_section"
      )
  ) {
    evidence_ids <- tempest_stage_context_claim_ids(context, "evidence")
    verified_ids <- tempest_stage_context_claim_ids(
      context,
      "verified_evidence"
    )
    if (length(evidence_ids) > 0L) {
      references$evidence_claim_ids <- unname(as.list(evidence_ids))
    }
    if (length(verified_ids) > 0L) {
      references$verified_evidence_claim_ids <- unname(as.list(verified_ids))
    }
  }
  governed_procedure <- tempest_dsprrr_execution_governance_trace(execution)
  if (!is.null(governed_procedure)) {
    references$governed_procedure <- governed_procedure
  }
  references
}

tempest_stage_record_call <- function(record_stage, record, output = NULL) {
  if (!is.function(record_stage)) {
    tempest_stage_evaluator_abort("{.arg record_stage} must be a function.")
  }
  record_stage(record, output)
  invisible(record)
}

tempest_stage_signal <- function(error, record, kind) {
  failure_class <- tempest_stage_failure_class(error, kind)
  rlang::abort(
    tempest_stage_failure_message(error, kind),
    class = c(failure_class, "tempest_stage_execution_error"),
    stage_record = record
  )
}

tempest_stage_error_record <- function(error) {
  error$stage_record %||% NULL
}

tempest_storm_stage_complete_success <- function(
  evaluated,
  running,
  context,
  output_reference,
  record_stage,
  fallback_taken,
  primary_error,
  now
) {
  reference <- tryCatch(
    tempest_stage_output_reference_derive(
      evaluated$output,
      running,
      context,
      output_reference
    ),
    error = identity
  )
  if (inherits(reference, "condition")) {
    tempest_storm_stage_complete_failure(
      reference,
      running,
      record_stage,
      "commit",
      fallback_taken,
      now
    )
  }
  terminal <- tempest_stage_record_succeed(
    running,
    reference,
    evaluated$support_status,
    fallback_taken = fallback_taken,
    primary_error = primary_error,
    completed_at = now()
  )
  tryCatch(
    tempest_stage_record_call(record_stage, terminal, evaluated$output),
    error = function(error) {
      failed <- tempest_stage_record_fail(
        running,
        error = error,
        kind = "commit",
        fallback_taken = fallback_taken,
        completed_at = now()
      )
      try(tempest_stage_record_call(record_stage, failed), silent = TRUE)
      tempest_stage_signal(error, failed, "commit")
    }
  )
  tempest_stage_result(evaluated$output, terminal)
}

tempest_storm_stage_complete_failure <- function(
  error,
  running,
  record_stage,
  kind,
  fallback_taken,
  now
) {
  terminal <- tempest_stage_record_fail(
    running,
    error = error,
    kind = kind,
    fallback_taken = fallback_taken,
    completed_at = now()
  )
  commit_error <- tryCatch(
    {
      tempest_stage_record_call(record_stage, terminal)
      NULL
    },
    error = identity
  )
  if (inherits(commit_error, "condition")) {
    failed <- tempest_stage_record_fail(
      running,
      error = commit_error,
      kind = "commit",
      fallback_taken = fallback_taken,
      completed_at = now()
    )
    try(tempest_stage_record_call(record_stage, failed), silent = TRUE)
    tempest_stage_signal(commit_error, failed, "commit")
  }
  tempest_stage_signal(error, terminal, kind)
}

tempest_stage_run_fallback <- function(
  primary_error,
  execution,
  chat,
  inputs,
  context,
  fallback,
  output_reference,
  record_stage,
  running,
  now
) {
  policy <- tempest_stage_policy(execution$stage)
  if (
    identical(policy$fallback_policy, "fail_closed") ||
      !tempest_stage_error_allows_fallback(primary_error) ||
      !is.function(fallback)
  ) {
    kind <- if (
      inherits(primary_error, "tempest_stage_output_validation_error")
    ) {
      "validation"
    } else {
      "execution"
    }
    tempest_storm_stage_complete_failure(
      primary_error,
      running,
      record_stage,
      kind,
      FALSE,
      now
    )
  }
  if (identical(policy$fallback_policy, "grounded_only")) {
    grounding <- tryCatch(
      tempest_stage_verified_evidence(context),
      error = identity
    )
    if (inherits(grounding, "condition")) {
      tempest_storm_stage_complete_failure(
        grounding,
        running,
        record_stage,
        "execution",
        FALSE,
        now
      )
    }
    context$evidence <- grounding
  }
  fallback_output <- tryCatch(
    fallback(chat, inputs, context),
    error = function(error) {
      tempest_storm_stage_complete_failure(
        error,
        running,
        record_stage,
        "fallback",
        TRUE,
        now
      )
    }
  )
  evaluated <- tryCatch(
    tempest_stage_evaluate(execution, fallback_output, context),
    error = function(error) {
      tempest_storm_stage_complete_failure(
        error,
        running,
        record_stage,
        "fallback",
        TRUE,
        now
      )
    }
  )
  tempest_storm_stage_complete_success(
    evaluated,
    running,
    context,
    output_reference,
    record_stage,
    TRUE,
    primary_error,
    now
  )
}

tempest_execute_stage <- function(
  module,
  chat,
  inputs,
  context = list(),
  record_stage = tempest_stage_record_discard,
  output_reference = NULL
) {
  evaluator <- tempest_stage_evaluator_resolve(module)
  module <- tempest_dsprrr_execution_verify(module, module$stage)
  tempest_stage_executor_preflight(
    module,
    inputs,
    context,
    record_stage,
    output_reference
  )
  fallback <- tempest_stage_fallback_resolve(evaluator$stage)
  attempt_id <- context$attempt_id %||% tempest_attempt_id()
  execution <- tempest_stage_execution_attempt(module, attempt_id)
  governed_procedure <- tempest_dsprrr_execution_governance_preflight(
    execution,
    context$knowledge_view %||% execution$knowledge_view %||% NULL
  )
  execution$governed_procedure_revision_id <-
    governed_procedure$revision_id %||% NA_character_
  now <- context$now %||% tempest_now_utc
  if (!is.function(now)) {
    tempest_stage_evaluator_abort("Stage clock context must be a function.")
  }
  running <- tempest_stage_record_start(
    execution$stage,
    execution$program_artifact_id,
    governed_procedure_revision_id = execution$governed_procedure_revision_id,
    trace_references = tempest_stage_execution_trace_references(
      execution,
      context
    ),
    attempt_id = attempt_id,
    started_at = now()
  )
  tempest_stage_record_call(record_stage, running)
  primary <- tryCatch(
    tempest_run_dsprrr_module_structured(
      execution,
      chat,
      inputs,
      step = execution$stage
    ),
    error = identity
  )
  if (inherits(primary, "condition")) {
    return(tempest_stage_run_fallback(
      primary,
      execution,
      chat,
      inputs,
      context,
      fallback,
      output_reference,
      record_stage,
      running,
      now
    ))
  }
  evaluated <- tryCatch(
    tempest_stage_evaluate(execution, primary$output, context),
    error = identity
  )
  if (inherits(evaluated, "condition")) {
    return(tempest_stage_run_fallback(
      evaluated,
      execution,
      chat,
      inputs,
      context,
      fallback,
      output_reference,
      record_stage,
      running,
      now
    ))
  }
  tempest_storm_stage_complete_success(
    evaluated,
    running,
    context,
    output_reference,
    record_stage,
    FALSE,
    NULL,
    now
  )
}

tempest_stage_cancel_attempt <- function(running, record_stage, now) {
  terminal <- tempest_stage_record_cancel(running, completed_at = now())
  tryCatch(
    tempest_stage_record_call(record_stage, terminal),
    error = function(error) {
      tempest_storm_stage_complete_failure(
        error,
        running,
        record_stage,
        "commit",
        FALSE,
        now
      )
    }
  )
  error <- rlang::error_cnd(
    "tempest_stage_cancelled",
    message = tempest_stage_failure_message(kind = "cancelled")
  )
  tempest_stage_signal(error, terminal, "cancelled")
}

tempest_stage_async_fallback <- function(
  primary_error,
  execution,
  chat,
  inputs,
  context,
  fallback,
  output_reference,
  record_stage,
  running,
  is_current,
  now
) {
  if (!isTRUE(is_current())) {
    tempest_stage_cancel_attempt(running, record_stage, now)
  }
  policy <- tempest_stage_policy(execution$stage)
  if (
    identical(policy$fallback_policy, "fail_closed") ||
      !tempest_stage_error_allows_fallback(primary_error) ||
      !is.function(fallback)
  ) {
    kind <- if (
      inherits(primary_error, "tempest_stage_output_validation_error")
    ) {
      "validation"
    } else {
      "execution"
    }
    tempest_storm_stage_complete_failure(
      primary_error,
      running,
      record_stage,
      kind,
      FALSE,
      now
    )
  }
  if (identical(policy$fallback_policy, "grounded_only")) {
    grounding <- tryCatch(
      tempest_stage_verified_evidence(context),
      error = identity
    )
    if (inherits(grounding, "condition")) {
      tempest_storm_stage_complete_failure(
        grounding,
        running,
        record_stage,
        "execution",
        FALSE,
        now
      )
    }
    context$evidence <- grounding
  }
  request <- tryCatch(
    fallback(chat, inputs, context),
    error = promises::promise_reject
  )
  promises::then(
    promises::promise_resolve(request),
    function(output) {
      if (!isTRUE(is_current())) {
        tempest_stage_cancel_attempt(running, record_stage, now)
      }
      evaluated <- tryCatch(
        tempest_stage_evaluate(execution, output, context),
        error = identity
      )
      if (inherits(evaluated, "condition")) {
        tempest_storm_stage_complete_failure(
          evaluated,
          running,
          record_stage,
          "fallback",
          TRUE,
          now
        )
      }
      tempest_storm_stage_complete_success(
        evaluated,
        running,
        context,
        output_reference,
        record_stage,
        TRUE,
        primary_error,
        now
      )
    },
    function(error) {
      if (!isTRUE(is_current())) {
        tempest_stage_cancel_attempt(running, record_stage, now)
      }
      tempest_storm_stage_complete_failure(
        error,
        running,
        record_stage,
        "fallback",
        TRUE,
        now
      )
    }
  )
}

tempest_execute_stage_async <- function(
  module,
  chat,
  inputs,
  context = list(),
  record_stage = tempest_stage_record_discard,
  output_reference = NULL,
  is_current = function() TRUE
) {
  tempest_require("promises", "Asynchronous stage execution requires promises.")
  evaluator <- tempest_stage_evaluator_resolve(module)
  module <- tempest_dsprrr_execution_verify(module, module$stage)
  tempest_stage_executor_preflight(
    module,
    inputs,
    context,
    record_stage,
    output_reference,
    is_current = is_current
  )
  fallback <- tempest_stage_fallback_resolve(evaluator$stage)
  attempt_id <- context$attempt_id %||% tempest_attempt_id()
  execution <- tempest_stage_execution_attempt(module, attempt_id)
  governed_procedure <- tempest_dsprrr_execution_governance_preflight(
    execution,
    context$knowledge_view %||% execution$knowledge_view %||% NULL
  )
  execution$governed_procedure_revision_id <-
    governed_procedure$revision_id %||% NA_character_
  now <- context$now %||% tempest_now_utc
  if (!is.function(now)) {
    tempest_stage_evaluator_abort("Stage clock context must be a function.")
  }
  running <- tempest_stage_record_start(
    execution$stage,
    execution$program_artifact_id,
    governed_procedure_revision_id = execution$governed_procedure_revision_id,
    trace_references = tempest_stage_execution_trace_references(
      execution,
      context
    ),
    attempt_id = attempt_id,
    started_at = now()
  )
  tempest_stage_record_call(record_stage, running)
  if (!isTRUE(is_current())) {
    cancellation <- tryCatch(
      tempest_stage_cancel_attempt(running, record_stage, now),
      error = identity
    )
    return(promises::promise_reject(cancellation))
  }
  request <- tryCatch(
    tempest_run_dsprrr_module_async(
      execution,
      chat,
      inputs,
      step = execution$stage
    ),
    error = promises::promise_reject
  )
  promises::then(
    promises::promise_resolve(request),
    function(output) {
      if (!isTRUE(is_current())) {
        tempest_stage_cancel_attempt(running, record_stage, now)
      }
      evaluated <- tryCatch(
        tempest_stage_evaluate(execution, output, context),
        error = identity
      )
      if (inherits(evaluated, "condition")) {
        return(tempest_stage_async_fallback(
          evaluated,
          execution,
          chat,
          inputs,
          context,
          fallback,
          output_reference,
          record_stage,
          running,
          is_current,
          now
        ))
      }
      tempest_storm_stage_complete_success(
        evaluated,
        running,
        context,
        output_reference,
        record_stage,
        FALSE,
        NULL,
        now
      )
    },
    function(error) {
      tempest_stage_async_fallback(
        error,
        execution,
        chat,
        inputs,
        context,
        fallback,
        output_reference,
        record_stage,
        running,
        is_current,
        now
      )
    }
  )
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
  experts <- tempest_validate_experts(experts, active_only = FALSE)
  expert_ids <- vapply(experts, \(expert) expert@expert_id, character(1))
  generated <- which(startsWith(expert_ids, "expert.generated-"))
  persona_records <- tempest_storm_succeeded_stage_records(records, "personas")
  record_digests <- vapply(
    persona_records,
    \(record) record@output_reference$content_digest,
    character(1)
  )
  candidate_digests <- character()
  candidate_coverage <- list()
  covered <- integer()
  for (index in seq_along(experts)) {
    singleton <- tempest_stage_state_output_digest(
      "personas",
      experts[index]
    )
    candidate_digests <- c(candidate_digests, singleton)
    candidate_coverage <- c(candidate_coverage, list(index))
    prefix <- tempest_stage_state_output_digest(
      "personas",
      experts[seq_len(index)]
    )
    candidate_digests <- c(candidate_digests, prefix)
    candidate_coverage <- c(candidate_coverage, list(seq_len(index)))
  }
  matches <- match(record_digests, candidate_digests)
  if (anyNA(matches)) {
    tempest_stage_record_abort(
      paste0(
        "Every succeeded persona-stage record must bind an exact canonical ",
        "durable expert-profile set."
      )
    )
  }
  if (length(matches) > 0L) {
    covered <- unique(unlist(candidate_coverage[matches], use.names = FALSE))
  }
  if (length(setdiff(generated, unique(covered))) > 0L) {
    tempest_stage_record_abort(
      paste0(
        "Every automatically generated expert requires an exact succeeded ",
        "persona-stage content binding."
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
  experts <- tempest_validate_experts(experts, active_only = FALSE)
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
