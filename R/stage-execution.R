# Stage output digests, preflight checks, and sync/async execution

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

tempest_stage_output_reference_contract <- function(stage) {
  switch(
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
}

tempest_stage_output_reference_validate_stage <- function(stage, reference) {
  reference <- tempest_stage_output_reference_validate(reference)
  expected <- tempest_stage_output_reference_contract(stage)
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

tempest_stage_error_static <- function(error) {
  classes <- c(
    "tempest_ecosystem_contract_error",
    "tempest_governed_procedure_error",
    "tempest_program_set_error",
    "tempest_stage_evaluator_contract_error",
    "tempest_stage_governance_error",
    "tempest_stage_commit_error",
    "tempest_stage_cancelled"
  )
  any(vapply(classes, \(class) inherits(error, class), logical(1))) ||
    tempest_dsprrr_contract_condition(error)
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
  references
}

tempest_stage_record_call <- function(record_stage, record, output = NULL) {
  if (!is.function(record_stage)) {
    tempest_stage_evaluator_abort("{.arg record_stage} must be a function.")
  }
  record_stage(record, output)
  invisible(record)
}

tempest_stage_safe_error_parent <- function(error) {
  if (tempest_condition_chain_sensitive(error)) {
    tempest_condition_safe_summary(error)
  } else {
    error
  }
}

tempest_stage_signal <- function(error, record, kind) {
  failure_class <- tempest_stage_failure_class(error, kind)
  rlang::abort(
    tempest_stage_failure_message(error, kind),
    class = tempest_condition_class(c(
      failure_class,
      "tempest_stage_execution_error"
    )),
    stage_record = record,
    parent = tempest_stage_safe_error_parent(error)
  )
}

tempest_stage_error_record <- function(error) {
  error$stage_record %||% NULL
}

tempest_stage_complete_success <- function(
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
    tempest_stage_complete_failure(
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

tempest_stage_complete_failure <- function(
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

tempest_stage_chat_checkpoint <- function(chat) {
  if (
    is.null(chat) ||
      !is.function(chat$get_turns) ||
      !is.function(chat$set_turns)
  ) {
    return(NULL)
  }
  turns <- tryCatch(
    chat$get_turns(),
    error = function(error) {
      tempest_stage_evaluator_abort(
        "Stage execution could not capture provider conversation state.",
        parent = error
      )
    }
  )
  list(turns = turns)
}

tempest_stage_chat_restore <- function(chat, checkpoint) {
  if (is.null(checkpoint)) {
    return(invisible(NULL))
  }
  tryCatch(
    chat$set_turns(checkpoint$turns),
    error = function(error) {
      tempest_stage_evaluator_abort(
        "Stage fallback could not restore provider conversation state.",
        parent = error
      )
    }
  )
  invisible(NULL)
}

tempest_stage_prepare_fallback <- function(
  primary_error,
  execution,
  chat,
  context,
  fallback,
  record_stage,
  running,
  chat_checkpoint,
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
    tempest_stage_complete_failure(
      primary_error,
      running,
      record_stage,
      kind,
      FALSE,
      now
    )
  }
  restore_error <- tryCatch(
    {
      tempest_stage_chat_restore(chat, chat_checkpoint)
      NULL
    },
    error = identity
  )
  if (inherits(restore_error, "condition")) {
    tempest_stage_complete_failure(
      restore_error,
      running,
      record_stage,
      "execution",
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
      tempest_stage_complete_failure(
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
  context
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
  chat_checkpoint,
  now
) {
  context <- tempest_stage_prepare_fallback(
    primary_error = primary_error,
    execution = execution,
    chat = chat,
    context = context,
    fallback = fallback,
    record_stage = record_stage,
    running = running,
    chat_checkpoint = chat_checkpoint,
    now = now
  )
  fallback_output <- tryCatch(
    fallback(chat, inputs, context),
    error = function(error) {
      tempest_stage_complete_failure(
        error,
        running,
        record_stage,
        "fallback",
        TRUE,
        now
      )
    }
  )
  fallback_output <- tempest_dsprrr_structured_output(fallback_output)
  evaluated <- tryCatch(
    tempest_stage_evaluate(execution, fallback_output, context),
    error = function(error) {
      tempest_stage_complete_failure(
        error,
        running,
        record_stage,
        "fallback",
        TRUE,
        now
      )
    }
  )
  tempest_stage_complete_success(
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
  execute <- function() {
    fallback <- tempest_stage_fallback_resolve(evaluator$stage)
    attempt_id <- context$attempt_id %||% tempest_attempt_id()
    execution <- tempest_stage_execution_attempt(module, attempt_id)
    now <- context$now %||% tempest_now_utc
    if (!is.function(now)) {
      tempest_stage_evaluator_abort("Stage clock context must be a function.")
    }
    running <- tempest_stage_record_start(
      execution$stage,
      execution$program_artifact_id,
      trace_references = tempest_stage_execution_trace_references(
        execution,
        context
      ),
      attempt_id = attempt_id,
      started_at = now()
    )
    tempest_stage_record_call(record_stage, running)
    chat_checkpoint <- tempest_stage_chat_checkpoint(chat)
    on.exit(
      tempest_stage_chat_restore(chat, chat_checkpoint),
      add = TRUE
    )
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
        chat_checkpoint,
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
        chat_checkpoint,
        now
      ))
    }
    tempest_stage_complete_success(
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
  tempest_otel_trace(
    "stage.execute",
    execute(),
    stage = evaluator$stage
  )
}

tempest_stage_cancel_attempt <- function(running, record_stage, now) {
  terminal <- tempest_stage_record_cancel(running, completed_at = now())
  tryCatch(
    tempest_stage_record_call(record_stage, terminal),
    error = function(error) {
      tempest_stage_complete_failure(
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
  chat_checkpoint,
  now
) {
  if (!isTRUE(is_current())) {
    tempest_stage_cancel_attempt(running, record_stage, now)
  }
  context <- tempest_stage_prepare_fallback(
    primary_error = primary_error,
    execution = execution,
    chat = chat,
    context = context,
    fallback = fallback,
    record_stage = record_stage,
    running = running,
    chat_checkpoint = chat_checkpoint,
    now = now
  )
  request <- tryCatch(
    fallback(chat, inputs, context),
    error = promises::promise_reject
  )
  tempest_otel_then(
    promises::promise_resolve(request),
    function(output) {
      if (!isTRUE(is_current())) {
        tempest_stage_cancel_attempt(running, record_stage, now)
      }
      output <- tempest_dsprrr_structured_output(output)
      evaluated <- tryCatch(
        tempest_stage_evaluate(execution, output, context),
        error = identity
      )
      if (inherits(evaluated, "condition")) {
        tempest_stage_complete_failure(
          evaluated,
          running,
          record_stage,
          "fallback",
          TRUE,
          now
        )
      }
      tempest_stage_complete_success(
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
      tempest_stage_complete_failure(
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
  otel_context <- tempest_otel_provider_call(
    tempest_otel_context_start(
      "stage.execute",
      stage = evaluator$stage
    )
  )
  otel_callback_context <- otel_context %||%
    tempest_otel_no_context_sentinel
  execute <- function() {
    .tempest_otel_context <- otel_callback_context
    fallback <- tempest_stage_fallback_resolve(evaluator$stage)
    attempt_id <- context$attempt_id %||% tempest_attempt_id()
    execution <- tempest_stage_execution_attempt(module, attempt_id)
    now <- context$now %||% tempest_now_utc
    if (!is.function(now)) {
      tempest_stage_evaluator_abort("Stage clock context must be a function.")
    }
    running <- tempest_stage_record_start(
      execution$stage,
      execution$program_artifact_id,
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
    chat_checkpoint <- tempest_stage_chat_checkpoint(chat)
    request <- tryCatch(
      tempest_run_dsprrr_module_async(
        execution,
        chat,
        inputs,
        step = execution$stage
      ),
      error = promises::promise_reject
    )
    completed <- tempest_otel_then(
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
            chat_checkpoint,
            now
          ))
        }
        tempest_stage_complete_success(
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
          chat_checkpoint,
          now
        )
      }
    )
    promises::finally(
      completed,
      function() tempest_stage_chat_restore(chat, chat_checkpoint)
    )
  }
  tempest_otel_trace_promise(
    "stage.execute",
    execute(),
    stage = evaluator$stage,
    context = otel_context
  )
}
