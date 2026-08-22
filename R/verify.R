# R/verify.R
# Explicit claim-by-evidence-span verification against captured source evidence

#' @keywords internal
tempest_type_verification <- function() {
  tempest_require("ellmer")
  ellmer::type_object(
    status = ellmer::type_enum(
      c(
        "supported",
        "partially_supported",
        "unsupported",
        "contradicted",
        "unverifiable"
      ),
      "Whether this exact evidence span supports the claim."
    ),
    score = ellmer::type_number(
      "Support strength in [0, 1]; omit only when status is unverifiable.",
      required = FALSE
    ),
    rationale = ellmer::type_string(
      "Brief non-empty justification for this exact span."
    )
  )
}

#' @keywords internal
tempest_verification_span_input <- function(claim, span, workspace) {
  if (
    !S7::S7_inherits(claim, tempest_claim) ||
      !S7::S7_inherits(span, tempest_evidence_span) ||
      !inherits(workspace, "ResearchWorkspace")
  ) {
    tempest_stage_evaluator_abort(
      "Verification-span input requires one exact claim, span, and workspace."
    )
  }
  resource <- workspace$get_retrieved_resource(span@source_id)
  if (is.null(resource) || is.na(span@quote) || !nzchar(span@quote)) {
    tempest_stage_governance_abort(
      "Verification requires a captured non-empty evidence-span quote."
    )
  }
  projection <- list(
    evidence_span_id = span@evidence_span_id,
    source_id = span@source_id,
    quote = span@quote,
    chunk_id = if (is.na(span@chunk_id)) NULL else span@chunk_id,
    start_offset = if (is.na(span@start_offset)) NULL else span@start_offset,
    end_offset = if (is.na(span@end_offset)) NULL else span@end_offset,
    page = if (is.na(span@page)) NULL else span@page,
    section_heading = if (is.na(span@section_heading)) {
      NULL
    } else {
      span@section_heading
    },
    source_content_hash = resource@content_hash
  )
  jsonlite::toJSON(
    tempest_research_manifest_canonical_value(
      projection,
      "verification_span"
    ),
    auto_unbox = TRUE,
    null = "null",
    digits = NA
  )
}

#' @keywords internal
tempest_verification_work_items <- function(workspace) {
  workspace$validate_integrity()
  claims <- workspace$list_proposed_claims()
  items <- list()
  for (claim in claims) {
    if (length(claim@evidence_span_ids) == 0L) {
      tempest_stage_governance_abort(
        "Every verified claim requires at least one exact evidence span."
      )
    }
    spans <- lapply(claim@evidence_span_ids, workspace$get_evidence_span)
    if (any(vapply(spans, is.null, logical(1)))) {
      tempest_stage_governance_abort(
        "Verification claim context cites a missing evidence span."
      )
    }
    span_sources <- vapply(spans, \(span) span@source_id, character(1))
    if (!setequal(unique(span_sources), claim@source_ids)) {
      tempest_stage_governance_abort(
        "Every cited claim source requires at least one exact evidence span."
      )
    }
    for (span in spans) {
      if (is.na(span@quote) || !nzchar(span@quote)) {
        tempest_stage_governance_abort(
          "Verification requires non-empty captured evidence-span quotes."
        )
      }
      items[[length(items) + 1L]] <- list(claim = claim, span = span)
    }
  }
  items
}

#' @keywords internal
tempest_verify_one_claim_span <- function(
  claim,
  span,
  store,
  verifier,
  module,
  verified_at,
  verifier_model,
  min_support_score = 0.7,
  record_stage = function(record, output = NULL) invisible(record)
) {
  span_input <- tempest_verification_span_input(claim, span, store)
  stage_result <- tempest_execute_stage(
    module,
    verifier,
    inputs = list(
      claim_text = claim@claim_text,
      source_excerpts = span_input
    ),
    context = tempest_stage_context_knowledge_view(
      list(
        workspace = store,
        claim = claim,
        evidence_span = span,
        min_support_score = min_support_score,
        verified_at = verified_at,
        verifier_model = verifier_model
      ),
      module
    ),
    output_reference = function(output, running_record, context) {
      tempest_stage_output_reference(
        "claim_supports",
        output@claim_support_id,
        content_digest = tempest_stage_verification_output_digest(
          output,
          running_record,
          context$claim,
          context$evidence_span,
          context$workspace
        )
      )
    },
    record_stage = record_stage
  )
  stage_result$output
}

#' @keywords internal
tempest_empty_claim_supports <- function() {
  tempest_claim_supports_tibble(list())
}

#' Verify claim citations against their sources
#'
#' @param workspace A [ResearchWorkspace] holding proposed claims and retrieved
#'   sources, or a `TempestSession` whose authoritative workspace and bound
#'   verification program should be used.
#' @param verifier A chat object (e.g. from `tempest_make_chat(config, "judge")`).
#' @param policy Citation policy for standalone workspaces; verification runs
#'   only for "claim_verified" or "strict". A `TempestSession` always runs
#'   product-required claim verification and rejects non-verifying policies.
#' @param verifier_model Optional model id bound into every verification-stage
#'   proof and projected onto each verified claim.
#' @param program_set A [TempestProgramSet] containing the exact
#'   `verify_claim_support` program. If `NULL`, [tempest_program_set()] creates
#'   the builtin set. When `workspace` is a `TempestSession`, its immutable
#'   ProgramSet, citation policy, and support threshold are authoritative;
#'   supplied values must match.
#' @param knowledge_view Optional immutable Graft view required when a
#'   standalone `program_set` binds verification to a governed procedure.
#' @param min_support_score Minimum support score in `[0, 1]` for a claim to be
#'   considered supported.
#' @return A claim-support audit tibble with one row per verified
#'   claim-by-evidence-span pair.
#'
#' Passing a standalone [ResearchWorkspace] returns an audit but does not own a
#' durable product stage ledger. Pass the `TempestSession` for Co-STORM bundles
#' so pair assessments, claim summaries, and verification records commit
#' together.
#' @keywords internal
tempest_verify_claims <- function(
  workspace,
  verifier,
  policy = "claim_verified",
  verifier_model = NA_character_,
  program_set = NULL,
  knowledge_view = NULL,
  min_support_score = 0.7
) {
  policy_supplied <- !missing(policy)
  threshold_supplied <- !missing(min_support_score)
  model_supplied <- !missing(verifier_model)
  session <- if (inherits(workspace, "TempestSession")) workspace else NULL
  if (!is.null(session)) {
    tempest_session_assert_mutable(session, "verify claims")
    if (!is.null(program_set)) {
      tempest_stage_governance_abort(
        paste0(
          "A TempestSession must use its immutable ProgramSet; ",
          "{.arg program_set} must be {.code NULL}."
        )
      )
    }
    if (!is.null(knowledge_view)) {
      tempest_stage_governance_abort(
        paste0(
          "A TempestSession must use its immutable knowledge view; ",
          "{.arg knowledge_view} must be {.code NULL}."
        )
      )
    }
    config <- tempest_session_config(session)
    if (!model_supplied) {
      verifier_model <- config@models[["judge"]] %||% NA_character_
    }
    if (
      policy_supplied &&
        !policy %in% c("claim_verified", "strict")
    ) {
      tempest_stage_governance_abort(
        paste0(
          "Session product verification requires claim-verified semantics; ",
          "citation rendering policy cannot disable execution."
        )
      )
    }
    policy <- "claim_verified"
    if (!threshold_supplied) {
      min_support_score <- config@min_support_score
    } else if (
      !identical(
        tempest_normalize_min_support_score(min_support_score),
        config@min_support_score
      )
    ) {
      tempest_stage_governance_abort(
        paste0(
          "Session verification threshold must match the session ",
          "configuration."
        )
      )
    }
    existing_records <- tempest_session_stage_records(session)
    already_verified <- length(tempest_session_workspace(
      session
    )$list_claim_supports()) >
      0L ||
      any(vapply(
        existing_records,
        function(record) {
          identical(record@stage, "verify_claim_support") &&
            identical(record@status, "succeeded")
        },
        logical(1)
      ))
    if (already_verified) {
      tempest_stage_governance_abort(
        paste0(
          "Session-owned claim verification is one-shot; existing ",
          "claim-support proof must not be superseded."
        )
      )
    }
    program <- tempest_session_programs(session)$verify_claim_support
    return(tempest_verify_claims_internal(
      workspace = tempest_session_workspace(session),
      verifier = verifier,
      policy = policy,
      verifier_model = verifier_model,
      program = program,
      min_support_score = min_support_score,
      verification_owner_token = tempest_session_verification_owner_token(
        session
      ),
      record_stage = tempest_session_stage_recorder(session),
      record_stages = tempest_session_stage_batch_recorder(session)
    ))
  }
  if (!inherits(workspace, "ResearchWorkspace")) {
    tempest_research_workspace_abort(
      "{.arg workspace} must be a ResearchWorkspace or TempestSession."
    )
  }
  tempest_research_workspace_assert_standalone_verification(workspace)
  program <- NULL
  if (policy %in% c("claim_verified", "strict")) {
    program_set <- program_set %||% tempest_program_set()
    knowledge <- tempest_product_knowledge_view(
      program_set,
      knowledge_view
    )
    workspace <- tempest_product_workspace_validate(
      workspace,
      knowledge,
      arg = "workspace"
    )
    snapshot_id <- workspace$base_snapshot_id %||% NULL
    program <- tempest_program_set_execution(
      program_set,
      "verify_claim_support",
      trace_context = tempest_standalone_dsprrr_trace_context(
        "verify_claim_support",
        knowledge_snapshot_id = snapshot_id
      )
    )
    program$knowledge_view <- knowledge$view
  }
  tempest_verify_claims_internal(
    workspace = workspace,
    verifier = verifier,
    policy = policy,
    verifier_model = verifier_model,
    program = program,
    min_support_score = min_support_score
  )
}

tempest_verify_claims_internal <- function(
  workspace,
  verifier,
  policy,
  verifier_model,
  program,
  min_support_score,
  verification_owner_token = NULL,
  record_stage = function(record, output = NULL) invisible(record),
  record_stages = function(records, outputs = NULL) invisible(records)
) {
  if (!inherits(workspace, "ResearchWorkspace")) {
    tempest_research_workspace_abort(
      "{.arg workspace} must be a ResearchWorkspace."
    )
  }
  if (!is.function(record_stage) || !is.function(record_stages)) {
    tempest_stage_evaluator_abort(
      "Verification record callbacks must be functions."
    )
  }
  if (!tempest_ledger_identifier_valid(verifier_model, optional = TRUE)) {
    tempest_stage_governance_abort(
      paste0(
        "Verification model must be `NA` or a bounded credential-free ",
        "identifier."
      )
    )
  }
  if (!policy %in% c("claim_verified", "strict")) {
    return(tempest_empty_claim_supports())
  }
  min_support_score <- tempest_normalize_min_support_score(min_support_score)
  verification_batch_at <- tempest_now_utc()
  work_items <- tempest_verification_work_items(workspace)
  pending_records <- list()
  running_records <- new.env(parent = emptyenv())
  committed <- FALSE
  collect_record <- function(record, output = NULL) {
    if (identical(record@status, "running")) {
      running_records[[record@attempt_id]] <- record
    }
    pending_records <<- tempest_stage_records_upsert(pending_records, record)
    invisible(record)
  }
  flush_records <- function(records = pending_records, outputs = NULL) {
    record_stages(records, outputs)
    invisible(NULL)
  }
  on.exit(
    {
      if (!committed) {
        failed_records <- lapply(pending_records, function(record) {
          if (record@status %in% c("failed", "cancelled")) {
            return(record)
          }
          running <- running_records[[record@attempt_id]] %||% NULL
          if (is.null(running)) {
            tempest_stage_evaluator_abort(
              "Verification batch lost its running attempt record."
            )
          }
          tempest_stage_record_fail(
            running,
            kind = if (identical(record@status, "succeeded")) {
              "commit"
            } else {
              "execution"
            },
            completed_at = if (identical(record@status, "succeeded")) {
              record@completed_at
            } else {
              tempest_now_utc()
            }
          )
        })
        for (record in failed_records) {
          try(record_stage(record), silent = TRUE)
        }
      }
    },
    add = TRUE
  )
  results <- lapply(work_items, function(item) {
    tempest_verify_one_claim_span(
      item$claim,
      item$span,
      workspace,
      verifier,
      module = program,
      verified_at = verification_batch_at,
      verifier_model = verifier_model,
      min_support_score = min_support_score,
      record_stage = collect_record
    )
  })
  workspace$verify_proposed_claims_batch(
    results,
    verified_at = verification_batch_at,
    min_support_score = min_support_score,
    verifier = verifier_model,
    .verification_owner_token = verification_owner_token,
    commit = function() {
      flush_records(
        pending_records,
        outputs = results
      )
    }
  )
  committed <- TRUE
  tempest_claim_supports_tibble(workspace$list_claim_supports())
}
