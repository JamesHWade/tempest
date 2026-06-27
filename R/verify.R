# R/verify.R
# Claim-level citation verification: does each cited source support the claim?

#' @keywords internal
tempest_type_verification <- function() {
  tempest_require("ellmer")
  ellmer::type_object(
    status = ellmer::type_enum(
      c("supported", "partially_supported", "unsupported", "contradicted", "unverifiable"),
      "Whether the cited sources support the claim."
    ),
    score = ellmer::type_number("Support strength in [0, 1].", required = FALSE),
    rationale = ellmer::type_string("Brief justification.", required = FALSE)
  )
}

#' @keywords internal
tempest_verify_one_claim <- function(claim, store, verifier) {
  excerpts <- purrr::map_chr(claim@source_ids, function(sid) {
    src <- store$get_source(sid)
    if (is.null(src)) return(paste0("[", sid, "]: (source missing)"))
    body <- src$content_text %||% src$snippet %||% ""
    paste0("[", sid, "] ", src$title %||% "", ": ", substr(body, 1, 1500))
  })
  prompt <- paste0(
    "Claim:\n", claim@claim_text, "\n\n",
    "Cited sources:\n", paste(excerpts, collapse = "\n\n"), "\n\n",
    "Does the cited evidence support the claim? Reply with a status, a score in [0,1], ",
    "and a short rationale."
  )
  verifier$chat_structured(prompt, type = tempest_type_verification())
}

#' Verify claim citations against their sources
#'
#' @param store A `SourceStore` holding claims and sources.
#' @param verifier A chat object (e.g. from `tempest_make_chat(config, "judge")`).
#' @param policy Citation policy; verification runs only for "claim_verified" or
#'   "strict". Defaults to "claim_verified".
#' @param verifier_model Optional model id recorded on each verified claim.
#' @return A `citation_audit` tibble (one row per verified claim).
#' @export
tempest_verify_claims <- function(store, verifier, policy = "claim_verified",
                                  verifier_model = NA_character_) {
  stopifnot(inherits(store, "SourceStore"))
  if (!policy %in% c("claim_verified", "strict")) {
    return(tibble::tibble(
      claim_id = character(), claim_text = character(),
      verification_status = character(), support_score = numeric(),
      rationale = character()
    ))
  }
  claims <- store$list_claims()
  rows <- purrr::map(claims, function(claim) {
    v <- tryCatch(
      tempest_verify_one_claim(claim, store, verifier),
      error = function(e) list(status = "unverifiable", score = NA_real_,
                               rationale = conditionMessage(e))
    )
    status <- v$status %||% "unverifiable"
    score <- v$score %||% NA_real_
    store$verify_claim(claim@claim_id, status = status, score = score,
                       verifier = verifier_model)
    tibble::tibble(
      claim_id = claim@claim_id,
      claim_text = claim@claim_text,
      verification_status = status,
      support_score = as.numeric(score),
      rationale = v$rationale %||% NA_character_
    )
  })
  audit <- if (length(rows) == 0) {
    tibble::tibble(
      claim_id = character(), claim_text = character(),
      verification_status = character(), support_score = numeric(),
      rationale = character()
    )
  } else {
    do.call(rbind, rows)
  }
  store$set_artifact("citation_audit", audit)
  audit
}
