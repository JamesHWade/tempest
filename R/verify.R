# R/verify.R
# Claim-level citation verification: does each cited source support the claim?

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
      "Whether the cited sources support the claim."
    ),
    score = ellmer::type_number(
      "Support strength in [0, 1].",
      required = FALSE
    ),
    rationale = ellmer::type_string("Brief justification.", required = FALSE)
  )
}

#' @keywords internal
tempest_verify_one_claim <- function(
  claim,
  store,
  verifier,
  module,
  min_support_score = 0.7,
  record_stage = function(record, output = NULL) invisible(record)
) {
  excerpt_text <- tempest_stage_verification_source_excerpts(claim, store)
  stage_result <- tempest_execute_stage(
    module,
    verifier,
    inputs = list(
      claim_text = claim@claim_text,
      source_excerpts = excerpt_text
    ),
    context = list(
      workspace = store,
      claim = claim,
      min_support_score = min_support_score
    ),
    output_reference = function(output, running_record, context) {
      tempest_stage_output_reference(
        "citation_audit",
        context$claim@claim_id,
        content_digest = tempest_stage_verification_output_digest(
          output,
          running_record,
          context$claim,
          context$workspace
        )
      )
    },
    record_stage = record_stage
  )
  stage_result$output
}

#' @keywords internal
tempest_normalize_verification_output <- function(output) {
  if (!is.list(output) || is.data.frame(output)) {
    tempest_abort(
      "Claim-verification stage output must be a record.",
      class = "tempest_stage_output_error"
    )
  }
  status <- tempest_stage_string(output$status, "status")
  valid_statuses <- setdiff(tempest_verification_statuses(), "unverified")
  if (!status %in% valid_statuses) {
    tempest_abort(
      "Claim-verification status must be one of: {.val {valid_statuses}}.",
      class = "tempest_stage_output_error"
    )
  }
  score <- tempest_normalize_optional_score(output$score)
  rationale <- output$rationale %||% NA_character_
  if (
    !is.character(rationale) ||
      is.object(rationale) ||
      !is.null(names(rationale)) ||
      length(rationale) != 1L
  ) {
    tempest_abort(
      "Claim-verification rationale must be a single string or `NA`.",
      class = "tempest_stage_output_error"
    )
  }
  if (
    !is.na(rationale) &&
      (nchar(rationale, type = "bytes") > 2000L ||
        tempest_contract_sensitive_scalar(rationale))
  ) {
    tempest_abort(
      "Claim-verification rationale must be bounded and credential-free.",
      class = "tempest_stage_output_error"
    )
  }
  list(status = status, score = score, rationale = rationale)
}

#' @keywords internal
tempest_empty_citation_audit <- function() {
  tibble::tibble(
    claim_id = character(),
    claim_text = character(),
    verification_status = character(),
    support_score = numeric(),
    rationale = character()
  )
}

#' Verify claim citations against their sources
#'
#' @param workspace A [ResearchWorkspace] holding proposed claims and retrieved
#'   sources, or a `TempestSession` whose authoritative workspace and bound
#'   verification program should be used.
#' @param verifier A chat object (e.g. from `tempest_make_chat(config, "judge")`).
#' @param policy Citation policy; verification runs only for "claim_verified" or
#'   "strict". Defaults to "claim_verified".
#' @param verifier_model Optional model id recorded on each verified claim.
#' @param program_set A [TempestProgramSet] containing the exact
#'   `verify_claim_support` program. If `NULL`, [tempest_program_set()] creates
#'   the builtin set. When `workspace` is a `TempestSession`, its immutable
#'   ProgramSet, citation policy, and support threshold are authoritative;
#'   supplied values must match.
#' @param min_support_score Minimum support score in `[0, 1]` for a claim to be
#'   considered supported.
#' @return A `citation_audit` tibble (one row per verified claim).
#'
#' Passing a standalone [ResearchWorkspace] returns an audit but does not own a
#' durable product stage ledger. Pass the `TempestSession` for Co-STORM bundles
#' so claim updates, audit rows, and verification records commit together.
#' @export
tempest_verify_claims <- function(
  workspace,
  verifier,
  policy = "claim_verified",
  verifier_model = NA_character_,
  program_set = NULL,
  min_support_score = 0.7
) {
  policy_supplied <- !missing(policy)
  threshold_supplied <- !missing(min_support_score)
  model_supplied <- !missing(verifier_model)
  session <- if (inherits(workspace, "TempestSession")) workspace else NULL
  if (!is.null(session)) {
    if (!is.null(program_set)) {
      tempest_stage_governance_abort(
        paste0(
          "A TempestSession must use its immutable ProgramSet; ",
          "{.arg program_set} must be {.code NULL}."
        )
      )
    }
    config <- session$config
    if (!model_supplied) {
      verifier_model <- config@models[["judge"]] %||% NA_character_
    }
    if (!policy_supplied) {
      policy <- config@citation_policy
    } else if (!identical(policy, config@citation_policy)) {
      tempest_stage_governance_abort(
        "Session verification policy must match the session configuration."
      )
    }
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
    program <- NULL
    if (policy %in% c("claim_verified", "strict")) {
      program <- tempest_session_programs(session)$verify_claim_support
    }
    return(tempest_verify_claims_internal(
      workspace = session$workspace,
      verifier = verifier,
      policy = policy,
      verifier_model = verifier_model,
      program = program,
      min_support_score = min_support_score,
      record_stage = tempest_session_stage_recorder(session),
      record_stages = tempest_session_stage_batch_recorder(session)
    ))
  }
  if (!inherits(workspace, "ResearchWorkspace")) {
    tempest_research_workspace_abort(
      "{.arg workspace} must be a ResearchWorkspace or TempestSession."
    )
  }
  program <- NULL
  if (policy %in% c("claim_verified", "strict")) {
    program_set <- program_set %||% tempest_program_set()
    snapshot_id <- workspace$base_snapshot_id %||% NULL
    program <- tempest_program_set_execution(
      program_set,
      "verify_claim_support",
      trace_context = tempest_standalone_dsprrr_trace_context(
        "verify_claim_support",
        knowledge_snapshot_id = snapshot_id
      )
    )
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
    audit <- tempest_empty_citation_audit()
    workspace$set_citation_audit(audit)
    return(workspace$citation_audit)
  }
  min_support_score <- tempest_normalize_min_support_score(min_support_score)
  claims <- workspace$list_proposed_claims()
  pending_records <- list()
  committed <- FALSE
  collect_record <- function(record, output = NULL) {
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
        failed_records <- Filter(
          \(record) record@status %in% c("failed", "cancelled"),
          pending_records
        )
        for (record in failed_records) {
          record_stage(record)
        }
      }
    },
    add = TRUE
  )
  results <- lapply(claims, function(claim) {
    tempest_verify_one_claim(
      claim,
      workspace,
      verifier,
      module = program,
      min_support_score = min_support_score,
      record_stage = collect_record
    )
  })
  rows <- purrr::map2(claims, results, function(claim, result) {
    if (!identical(result$claim_id, claim@claim_id)) {
      tempest_abort(
        "Claim-verification output does not match its input claim.",
        class = "tempest_stage_output_error"
      )
    }
    status <- result$verification_status
    score <- result$support_score
    status <- tempest_apply_min_support_score(
      status,
      score,
      min_support_score = min_support_score
    )
    tibble::tibble(
      claim_id = claim@claim_id,
      claim_text = claim@claim_text,
      verification_status = status,
      support_score = as.numeric(score),
      rationale = result$rationale
    )
  })
  audit <- if (length(rows) == 0) {
    tempest_empty_citation_audit()
  } else {
    do.call(rbind, rows)
  }
  updates <- purrr::map2(claims, rows, function(claim, row) {
    list(
      claim_id = claim@claim_id,
      status = row$verification_status[[1]],
      score = row$support_score[[1]],
      verifier = verifier_model
    )
  })
  workspace$verify_proposed_claims_batch(
    updates,
    audit,
    commit = function() {
      flush_records(
        pending_records,
        outputs = results
      )
    }
  )
  committed <- TRUE
  workspace$citation_audit
}
