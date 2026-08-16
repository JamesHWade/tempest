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
tempest_verify_one_claim <- function(claim, store, verifier, module = NULL) {
  excerpts <- purrr::map_chr(claim@source_ids, function(sid) {
    src <- store$get_retrieved_source(sid)
    if (is.null(src)) {
      return(paste0("[", sid, "]: (source missing)"))
    }
    body <- src$content_text %||% src$snippet %||% ""
    paste0("[", sid, "] ", src$title %||% "", ": ", substr(body, 1, 1500))
  })
  excerpt_text <- paste(excerpts, collapse = "\n\n")
  out <- tempest_run_dsprrr_module(
    module,
    verifier,
    inputs = list(
      claim_text = claim@claim_text,
      source_excerpts = excerpt_text
    ),
    step = "claim verification"
  )
  if (!is.null(out)) {
    return(out)
  }
  prompt <- paste0(
    "Claim:\n",
    claim@claim_text,
    "\n\n",
    "Cited sources:\n",
    excerpt_text,
    "\n\n",
    "Does the cited evidence support the claim? Reply with a status, a score in [0,1], ",
    "and a short rationale."
  )
  verifier$chat_structured(prompt, type = tempest_type_verification())
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
#'   sources.
#' @param verifier A chat object (e.g. from `tempest_make_chat(config, "judge")`).
#' @param policy Citation policy; verification runs only for "claim_verified" or
#'   "strict". Defaults to "claim_verified".
#' @param verifier_model Optional model id recorded on each verified claim.
#' @param program_set A [TempestProgramSet] containing the exact
#'   `verify_claim_support` program. If `NULL`, [tempest_program_set()] creates
#'   the builtin set.
#' @param min_support_score Minimum support score in `[0, 1]` for a claim to be
#'   considered supported.
#' @return A `citation_audit` tibble (one row per verified claim).
#' @export
tempest_verify_claims <- function(
  workspace,
  verifier,
  policy = "claim_verified",
  verifier_model = NA_character_,
  program_set = NULL,
  min_support_score = 0.7
) {
  if (!inherits(workspace, "ResearchWorkspace")) {
    tempest_research_workspace_abort(
      "{.arg workspace} must be a ResearchWorkspace."
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
  min_support_score
) {
  if (!inherits(workspace, "ResearchWorkspace")) {
    tempest_research_workspace_abort(
      "{.arg workspace} must be a ResearchWorkspace."
    )
  }
  if (!policy %in% c("claim_verified", "strict")) {
    audit <- tempest_empty_citation_audit()
    workspace$set_citation_audit(audit)
    return(workspace$citation_audit)
  }
  min_support_score <- tempest_normalize_min_support_score(min_support_score)
  claims <- workspace$list_proposed_claims()
  rows <- purrr::map(claims, function(claim) {
    v <- tryCatch(
      tempest_verify_one_claim(
        claim,
        workspace,
        verifier,
        module = program
      ),
      error = function(e) {
        tempest_rethrow_dsprrr_contract(e)
        list(
          status = "unverifiable",
          score = NA_real_,
          rationale = conditionMessage(e)
        )
      }
    )
    # The judge's status/score are soft hints, not validator-enforced. Sanitize
    # them before they reach the S7 claim validators so a misbehaving judge
    # cannot abort the run.
    valid_status <- c(
      "supported",
      "partially_supported",
      "unsupported",
      "contradicted",
      "unverifiable"
    )
    status <- v$status %||% "unverifiable"
    if (length(status) != 1 || is.na(status) || !status %in% valid_status) {
      status <- "unverifiable"
    }
    score <- suppressWarnings(as.numeric(v$score %||% NA_real_))
    if (length(score) != 1 || is.na(score)) {
      score <- NA_real_
    } else {
      score <- max(0, min(1, score))
    }
    status <- tempest_apply_min_support_score(
      status,
      score,
      min_support_score = min_support_score
    )
    workspace$verify_proposed_claim(
      claim@claim_id,
      status = status,
      score = score,
      verifier = verifier_model
    )
    tibble::tibble(
      claim_id = claim@claim_id,
      claim_text = claim@claim_text,
      verification_status = status,
      support_score = as.numeric(score),
      rationale = v$rationale %||% NA_character_
    )
  })
  audit <- if (length(rows) == 0) {
    tempest_empty_citation_audit()
  } else {
    do.call(rbind, rows)
  }
  workspace$set_citation_audit(audit)
  workspace$citation_audit
}
