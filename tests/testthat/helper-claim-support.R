test_add_verifiable_claim <- function(
  workspace,
  key = "1",
  claim_text = paste("Claim", key),
  quote = paste("Evidence for claim", key),
  source_id = paste0("source.verify.", key),
  span_id = paste0("span.verify.", key),
  claim_id = paste0("claim.verify.", key),
  extracted_by = "expert.verification-fixture"
) {
  if (is.null(workspace$get_retrieved_resource(source_id))) {
    workspace$upsert_retrieved_resource(tempest_resource(
      resource_kind = "web.page",
      locator = paste0("https://example.org/verify/", key),
      title = paste("Verification source", key),
      media_type = "text/plain",
      content = quote,
      resource_id = source_id,
      retrieved_at = "2026-08-16T12:00:00Z"
    ))
  }
  span <- tempest_evidence_span(
    source_id = source_id,
    quote = quote,
    extracted_by = extracted_by,
    evidence_span_id = span_id,
    created_at = "2026-08-16T12:01:00Z"
  )
  claim <- tempest_claim(
    claim_text = claim_text,
    source_ids = source_id,
    evidence_span_ids = span_id,
    supporting_quotes = list(quote),
    claim_id = claim_id,
    created_at = "2026-08-16T12:02:00Z"
  )
  workspace$add_extracted_claim_batch(list(claim), list(span))
  list(
    claim = workspace$get_proposed_claim(claim_id),
    span = workspace$get_evidence_span(span_id),
    source = workspace$get_retrieved_resource(source_id)
  )
}

test_claim_support <- function(
  claim,
  span,
  status = "supported",
  score = 0.9,
  rationale = "The exact captured span supports the claim."
) {
  tempest_claim_support(
    claim_id = claim@claim_id,
    evidence_span_id = span@evidence_span_id,
    source_id = span@source_id,
    verification_status = status,
    support_score = score,
    rationale = rationale
  )
}

test_verified_workspace <- function(
  statuses = "supported",
  scores = rep(0.9, length(statuses)),
  min_support_score = 0.7,
  verifier = "judge.fixture",
  verified_at = "2026-08-16T12:03:00Z"
) {
  workspace <- tempest_research_workspace()
  fixtures <- lapply(seq_along(statuses), function(index) {
    test_add_verifiable_claim(workspace, as.character(index))
  })
  supports <- lapply(seq_along(fixtures), function(index) {
    test_claim_support(
      fixtures[[index]]$claim,
      fixtures[[index]]$span,
      status = statuses[[index]],
      score = scores[[index]],
      rationale = paste("Assessment", index)
    )
  })
  workspace$verify_proposed_claims_batch(
    supports,
    verified_at = verified_at,
    min_support_score = min_support_score,
    verifier = verifier
  )
  list(workspace = workspace, fixtures = fixtures, supports = supports)
}
