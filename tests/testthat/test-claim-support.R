test_that("claim-support identity is derived from exactly one claim-span pair", {
  support <- tempest_claim_support(
    claim_id = "claim.1",
    evidence_span_id = "span.1",
    source_id = "source.1",
    verification_status = "supported",
    support_score = 0.9,
    rationale = "Direct support."
  )
  same_pair <- tempest_claim_support(
    claim_id = "claim.1",
    evidence_span_id = "span.1",
    source_id = "source.2",
    verification_status = "unsupported",
    support_score = 0.1,
    rationale = "No direct support."
  )

  expect_s7_class(support, tempest:::TempestClaimSupport)
  expect_identical(support@claim_support_id, same_pair@claim_support_id)
  expect_identical(
    names(formals(tempest_claim_support)),
    c(
      "claim_id",
      "evidence_span_id",
      "source_id",
      "verification_status",
      "support_score",
      "rationale"
    )
  )
})

test_that("claim-support restoration recomputes and checks derived identity", {
  support <- tempest_claim_support(
    "claim.1",
    "span.1",
    "source.1",
    "unverifiable",
    NA_real_,
    "The span cannot answer the claim."
  )
  record <- tempest:::tempest_claim_support_to_list(support)
  expect_null(record$support_score)

  expect_identical(
    tempest:::tempest_claim_support_from_list(record),
    support
  )
  record$claim_support_id <- paste0("sha256:", strrep("0", 64L))
  expect_error(
    tempest:::tempest_claim_support_from_list(record),
    class = "tempest_claim_support_error"
  )
})

test_that("claim-support decoding requires exact JSON score types", {
  supported <- tempest:::tempest_claim_support_to_list(tempest_claim_support(
    "claim.1",
    "span.1",
    "source.1",
    "supported",
    0.9,
    "Direct support."
  ))
  for (malformed in list(
    "0.9",
    list(0.9),
    NULL,
    NA_real_,
    structure(0.9, unit = "score")
  )) {
    record <- supported
    record["support_score"] <- list(malformed)
    expect_error(
      tempest:::tempest_claim_support_from_list(record),
      class = "tempest_claim_support_error"
    )
  }

  unverifiable <- tempest:::tempest_claim_support_to_list(
    tempest_claim_support(
      "claim.1",
      "span.1",
      "source.1",
      "unverifiable",
      NA_real_,
      "The span cannot answer the claim."
    )
  )
  for (malformed in list("not-a-number", NA_real_, 0.9, list(NULL))) {
    record <- unverifiable
    record["support_score"] <- list(malformed)
    expect_error(
      tempest:::tempest_claim_support_from_list(record),
      class = "tempest_claim_support_error"
    )
  }
})

test_that("claim-support status, score, and rationale are exact", {
  malformed_scores <- list(
    "0.9",
    list(0.9),
    c(0.8, 0.9),
    structure(0.9, names = "score"),
    structure(0.9, unit = "score")
  )
  for (malformed in malformed_scores) {
    expect_error(
      tempest_claim_support(
        "claim.1",
        "span.1",
        "source.1",
        "supported",
        malformed,
        "Direct support."
      ),
      class = "tempest_claim_support_error"
    )
  }
  expect_error(
    tempest_claim_support(
      "claim.1",
      "span.1",
      "source.1",
      "unverifiable",
      0.2,
      "Cannot verify."
    ),
    class = "tempest_claim_support_error"
  )
  expect_error(
    tempest_claim_support(
      "claim.1",
      "span.1",
      "source.1",
      "unverifiable",
      NaN,
      "Cannot verify."
    ),
    class = "tempest_claim_support_error"
  )
  expect_error(
    tempest_claim_support(
      "claim.1",
      "span.1",
      "source.1",
      "supported",
      NA_real_,
      "Direct support."
    ),
    class = "tempest_claim_support_error"
  )
  expect_error(
    tempest_claim_support(
      "claim.1",
      "span.1",
      "source.1",
      "supported",
      0.9,
      " padded rationale "
    ),
    class = "tempest_claim_support_error"
  )
  expect_error(
    tempest_claim_support(
      "claim.1",
      "span.1",
      "source.1",
      "unverified",
      0.9,
      "Invalid status."
    ),
    class = "tempest_claim_support_error"
  )
})

test_that("workspace replacement requires the exact complete pair set", {
  verified <- test_verified_workspace(
    statuses = c("supported", "unsupported"),
    scores = c(0.9, 0.2)
  )
  workspace <- verified$workspace
  before <- tempest_claim_supports(workspace)

  expect_error(
    workspace$verify_proposed_claims_batch(
      verified$supports[1],
      verified_at = "2026-08-16T12:03:00Z"
    ),
    class = "tempest_research_workspace_integrity_error"
  )
  expect_identical(tempest_claim_supports(workspace), before)
  expect_error(
    workspace$verify_proposed_claims_batch(
      c(
        verified$supports,
        verified$supports[1]
      ),
      verified_at = "2026-08-16T12:03:00Z"
    ),
    class = "tempest_research_workspace_integrity_error"
  )
  expect_identical(tempest_claim_supports(workspace), before)
})

test_that("claim summaries are deterministic projections of pair support", {
  workspace <- tempest_research_workspace()
  source <- tempest_resource(
    resource_kind = "web.page",
    locator = "https://example.org/aggregate",
    title = "Aggregate evidence",
    media_type = "text/plain",
    content = "Supporting span. Contradicting span.",
    resource_id = "source.aggregate"
  )
  workspace$upsert_retrieved_resource(source)
  spans <- list(
    tempest_evidence_span(
      source_id = source@resource_id,
      quote = "Supporting span.",
      evidence_span_id = "span.aggregate.1"
    ),
    tempest_evidence_span(
      source_id = source@resource_id,
      quote = "Contradicting span.",
      evidence_span_id = "span.aggregate.2"
    )
  )
  claim <- tempest_claim(
    "Aggregate claim",
    source_ids = source@resource_id,
    evidence_span_ids = vapply(spans, \(span) span@evidence_span_id, ""),
    supporting_quotes = lapply(spans, \(span) span@quote),
    claim_id = "claim.aggregate"
  )
  workspace$add_extracted_claim_batch(list(claim), spans)
  supports <- list(
    test_claim_support(claim, spans[[1]], score = 0.9),
    test_claim_support(
      claim,
      spans[[2]],
      status = "contradicted",
      score = 0.8,
      rationale = "The exact span contradicts the claim."
    )
  )

  workspace$verify_proposed_claims_batch(
    supports,
    verified_at = "2026-08-16T12:03:00Z",
    verifier = "judge.1"
  )
  derived <- workspace$get_proposed_claim(claim@claim_id)

  expect_identical(derived@verification_status, "contradicted")
  expect_identical(derived@support_score, 0.8)
  expect_identical(derived@verifier_model, "judge.1")
  expect_identical(workspace$citation_audit, tempest_claim_supports(workspace))
})

test_that("verified workspaces seal every verification-relevant mutation", {
  workspace <- tempest_research_workspace()
  fixture <- test_add_verifiable_claim(workspace)
  extra_span <- tempest_evidence_span(
    source_id = fixture$source@resource_id,
    quote = fixture$span@quote,
    evidence_span_id = "span.verify.extra",
    created_at = "2026-08-16T12:02:30Z"
  )
  workspace$add_evidence_span(extra_span)
  support <- test_claim_support(fixture$claim, fixture$span)
  workspace$verify_proposed_claims_batch(
    list(support),
    verified_at = "2026-08-16T12:03:00Z",
    verifier = "judge.fixture"
  )
  before <- tempest:::tempest_research_workspace_snapshot(workspace)

  replacement <- S7::set_props(
    fixture$source,
    content = paste(fixture$span@quote, "Additional content.")
  )
  new_span <- S7::set_props(
    extra_span,
    evidence_span_id = "span.verify.new"
  )
  new_claim <- S7::set_props(
    fixture$claim,
    claim_id = "claim.verify.new",
    evidence_span_ids = extra_span@evidence_span_id,
    supporting_quotes = list(extra_span@quote)
  )
  dispute <- tempest_dispute(
    dispute_id = "dispute.verify.new",
    topic = "Late dispute",
    claim_ids = fixture$claim@claim_id
  )
  mutations <- list(
    \() workspace$upsert_retrieved_resource(replacement),
    \() {
      workspace$upsert_retrieved_resource(tempest_resource(
        resource_kind = "web.page",
        locator = "https://example.org/verify/new",
        title = "New source",
        media_type = "text/plain",
        content = "New evidence.",
        resource_id = "source.verify.new"
      ))
    },
    \() workspace$add_proposed_claim(new_claim),
    \() workspace$add_evidence_span(new_span),
    \() {
      workspace$link_evidence_to_proposed_claim(
        fixture$claim@claim_id,
        extra_span@evidence_span_id
      )
    },
    \() workspace$add_extracted_claim_batch(list(new_claim)),
    \() workspace$add_dispute(dispute)
  )
  for (mutate in mutations) {
    expect_error(
      mutate(),
      class = "tempest_research_workspace_integrity_error"
    )
    expect_identical(
      tempest:::tempest_research_workspace_snapshot(workspace),
      before
    )
  }

  expect_no_error(workspace$upsert_retrieved_resource(fixture$source))
  expect_no_error(workspace$add_evidence_span(extra_span))
  expect_no_error(workspace$add_proposed_claim(
    workspace$get_proposed_claim(fixture$claim@claim_id)
  ))
  expect_no_error(workspace$link_evidence_to_proposed_claim(
    fixture$claim@claim_id,
    fixture$span@evidence_span_id
  ))
  expect_identical(
    tempest:::tempest_research_workspace_snapshot(workspace),
    before
  )
})
