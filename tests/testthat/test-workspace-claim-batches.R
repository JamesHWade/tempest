test_that("ledger identity fields reject credential-shaped values", {
  claim_fields <- c(
    "claim_id",
    "retrieval_step_id",
    "perspective_id",
    "expert_id",
    "session_id",
    "section_id",
    "verifier_model"
  )
  for (field in claim_fields) {
    arguments <- c(
      list(claim_text = "Scientific evidence", source_ids = "source-1"),
      stats::setNames(list("sk-live-secret"), field)
    )
    expect_error(
      do.call(tempest:::tempest_claim, arguments),
      class = "simpleError",
      info = field
    )
  }
  for (field in c(
    "evidence_span_id",
    "source_id",
    "chunk_id",
    "extracted_by"
  )) {
    arguments <- list(source_id = "source-1")
    arguments[[field]] <- "Authorization:Bearer-sk-live-secret"
    expect_error(
      do.call(tempest:::tempest_evidence_span, arguments),
      class = "simpleError",
      info = field
    )
  }
  expect_error(
    tempest:::tempest_dispute(
      "Disputed result",
      claim_ids = "sk-live-secret"
    ),
    class = "simpleError"
  )
})

test_that("ledger timestamps are canonical and revalidated on serialization", {
  expect_error(
    tempest:::tempest_claim(
      "Scientific evidence",
      created_at = "2026-08-16 01:00:00"
    ),
    class = "simpleError"
  )
  expect_error(
    tempest:::tempest_evidence_span(
      "source-1",
      created_at = "2026-08-16T01:00:00PST"
    ),
    class = "simpleError"
  )
  expect_error(
    tempest:::tempest_dispute(
      "Disputed result",
      created_at = "sk-live-secret"
    ),
    class = "simpleError"
  )

  claim <- tempest:::tempest_claim("Scientific evidence")
  expect_error(
    claim@verified_at <- "sk-live-secret",
    class = "simpleError"
  )
  span <- tempest:::tempest_evidence_span("source-1")
  expect_error(
    span@created_at <- "2026-08-16T01:00:00Zjunk",
    class = "simpleError"
  )
  dispute <- tempest:::tempest_dispute("Disputed result")
  expect_error(
    dispute@created_at <- "2026-02-30T01:00:00Z",
    class = "simpleError"
  )
})

test_that("claim insertion validates the complete batch before mutation", {
  verified <- test_verified_workspace()
  workspace <- verified$workspace
  source_id <- verified$fixtures[[1]]$source@resource_id
  existing <- verified$fixtures[[1]]$claim
  prior_supports <- tempest:::tempest_claim_supports_resolved(workspace)
  valid <- tempest_claim(
    claim_id = "claim-valid",
    claim_text = "Valid claim",
    source_ids = source_id
  )
  invalid <- tempest_claim(
    claim_id = "claim-invalid",
    claim_text = "Invalid claim",
    source_ids = "Sunknown00000"
  )
  committed <- FALSE

  expect_error(
    workspace$add_proposed_claims(
      list(valid, invalid),
      commit = function() committed <<- TRUE
    ),
    class = "tempest_research_workspace_integrity_error"
  )
  expect_identical(committed, FALSE)
  expect_equal(
    vapply(
      workspace$list_proposed_claims(),
      \(claim) claim@claim_id,
      character(1)
    ),
    existing@claim_id
  )
  expect_identical(
    tempest:::tempest_claim_supports_resolved(workspace),
    prior_supports
  )
})

test_that("claim insertion rolls back when its commit callback fails", {
  workspace <- fake_store_with_sources(1)
  source_id <- workspace$list_retrieved_sources()[[1]]$id
  claim <- tempest_claim(
    claim_id = "claim-rollback",
    claim_text = "Rollback claim",
    source_ids = source_id
  )

  expect_error(
    workspace$add_proposed_claims(
      list(claim),
      commit = function() {
        rlang::abort("commit failed", class = "test_commit_error")
      }
    ),
    class = "test_commit_error"
  )
  expect_length(workspace$list_proposed_claims(), 0L)
  expect_length(workspace$proposed_claims_for_resource(source_id), 0L)
})

test_that("claim verification validates and commits one complete batch", {
  workspace <- tempest_research_workspace()
  fixtures <- list(
    test_add_verifiable_claim(workspace, "a"),
    test_add_verifiable_claim(workspace, "b")
  )
  prior_claims <- workspace$list_proposed_claims()
  supports <- list(
    test_claim_support(fixtures[[1]]$claim, fixtures[[1]]$span),
    test_claim_support(
      fixtures[[2]]$claim,
      fixtures[[2]]$span,
      status = "unsupported",
      score = 0.2,
      rationale = "The span does not support the claim."
    )
  )

  expect_error(
    workspace$verify_proposed_claims_batch(
      supports[1],
      verified_at = "2026-08-16T12:03:00Z"
    ),
    class = "tempest_research_workspace_integrity_error"
  )
  expect_identical(workspace$list_proposed_claims(), prior_claims)
  expect_null(workspace$citation_audit)

  workspace$verify_proposed_claims_batch(
    supports,
    verified_at = "2026-08-16T12:03:00Z",
    verifier = "judge"
  )

  verified <- workspace$list_proposed_claims()
  expect_equal(
    vapply(verified, \(claim) claim@verification_status, character(1)),
    c("supported", "unsupported")
  )
  expect_identical(
    workspace$citation_audit,
    tempest:::tempest_claim_supports_tibble(workspace$list_claim_supports())
  )
})

test_that("claim verification rolls back when its commit callback fails", {
  workspace <- tempest_research_workspace()
  fixture <- test_add_verifiable_claim(workspace, "rollback")
  support <- test_claim_support(fixture$claim, fixture$span)

  expect_error(
    workspace$verify_proposed_claims_batch(
      list(support),
      verified_at = "2026-08-16T12:03:00Z",
      verifier = "judge",
      commit = function() {
        rlang::abort("commit failed", class = "test_commit_error")
      }
    ),
    class = "test_commit_error"
  )
  restored <- workspace$list_proposed_claims()[[1]]
  expect_equal(restored@verification_status, "unverified")
  expect_equal(restored@support_score, NA_real_)
  expect_null(workspace$citation_audit)
})

test_that("extracted spans and claims commit as one linked batch", {
  workspace <- fake_store_with_sources(1)
  source_id <- workspace$list_retrieved_sources()[[1]]$id
  span <- tempest_evidence_span(
    evidence_span_id = "span-linked",
    source_id = source_id,
    quote = "Body text"
  )
  claim <- tempest_claim(
    claim_id = "claim-linked",
    claim_text = "Linked claim",
    source_ids = source_id,
    evidence_span_ids = span@evidence_span_id,
    supporting_quotes = list(span@quote)
  )

  workspace$add_extracted_claim_batch(list(claim), list(span))

  expect_identical(
    workspace$get_evidence_span(span@evidence_span_id),
    span
  )
  expect_identical(
    workspace$get_proposed_claim(claim@claim_id),
    claim
  )
  expect_identical(
    workspace$get_evidence_for_proposed_claim(claim@claim_id),
    list(span)
  )
})

test_that("extracted batches require authoritative quote lineage", {
  workspace <- fake_store_with_sources(1)
  source_id <- workspace$list_retrieved_sources()[[1]]$id
  span <- tempest_evidence_span(
    evidence_span_id = "span-quote-lineage",
    source_id = source_id,
    quote = "Body text"
  )
  claim <- tempest_claim(
    claim_id = "claim-quote-lineage",
    claim_text = "Quoted claim",
    source_ids = source_id,
    evidence_span_ids = span@evidence_span_id,
    supporting_quotes = list("fabricated quote")
  )

  expect_error(
    workspace$add_extracted_claim_batch(list(claim), list(span)),
    class = "tempest_research_workspace_integrity_error"
  )
  missing_projection <- tempest_claim(
    claim_id = "claim-missing-quote-lineage",
    claim_text = "Quoted claim",
    source_ids = source_id,
    evidence_span_ids = span@evidence_span_id,
    supporting_quotes = list()
  )
  expect_error(
    workspace$add_extracted_claim_batch(
      list(missing_projection),
      list(span)
    ),
    class = "tempest_research_workspace_integrity_error"
  )
  expect_length(workspace$list_proposed_claims(), 0L)
  expect_length(workspace$list_evidence_spans(), 0L)
})

test_that("quoted spans cannot be laundered through an empty quote projection", {
  workspace <- fake_store_with_sources(1)
  source_id <- workspace$list_retrieved_sources()[[1]]$id
  span <- tempest_evidence_span(
    evidence_span_id = "span-empty-quote-projection",
    source_id = source_id,
    quote = "Body text"
  )
  claim <- tempest_claim(
    claim_id = "claim-empty-quote-projection",
    claim_text = "Quoted claim",
    source_ids = source_id,
    evidence_span_ids = span@evidence_span_id,
    supporting_quotes = list()
  )

  expect_error(
    workspace$add_extracted_claim_batch(list(claim), list(span)),
    class = "tempest_research_workspace_integrity_error"
  )
  expect_length(workspace$list_proposed_claims(), 0L)
  expect_length(workspace$list_evidence_spans(), 0L)
})

test_that("evidence offsets are zero-based half-open character offsets", {
  workspace <- tempest_research_workspace()
  source <- fake_source(
    "https://example.org/unicode-offsets",
    content_text = "αβγ evidence"
  )
  workspace$upsert_retrieved_resource(source)
  span <- tempest_evidence_span(
    source_id = source@resource_id,
    quote = "βγ",
    start_offset = 1L,
    end_offset = 3L
  )
  expect_no_error(workspace$add_evidence_span(span))

  for (offsets in list(c(-1L, 1L), c(2L, 1L))) {
    expect_error(
      tempest_evidence_span(
        source_id = source@resource_id,
        quote = "βγ",
        start_offset = offsets[[1]],
        end_offset = offsets[[2]]
      ),
      class = "simpleError"
    )
  }
  expect_error(
    workspace$add_evidence_span(tempest_evidence_span(
      source_id = source@resource_id,
      quote = "βγ",
      start_offset = 0L,
      end_offset = 2L
    )),
    class = "tempest_research_workspace_integrity_error"
  )
  expect_error(
    workspace$add_evidence_span(tempest_evidence_span(
      source_id = source@resource_id,
      quote = "evidence",
      start_offset = 4L,
      end_offset = 99L
    )),
    class = "tempest_research_workspace_integrity_error"
  )
  expect_error(
    tempest_evidence_span(
      source_id = source@resource_id,
      start_offset = 0L,
      end_offset = 1L
    ),
    class = "simpleError"
  )
})

test_that("source replacement rolls back when it invalidates quote lineage", {
  workspace <- tempest_research_workspace()
  source <- fake_source(
    "https://example.org/replacement",
    content_text = "Authoritative captured quote."
  )
  workspace$upsert_retrieved_resource(source)
  span <- tempest_evidence_span(
    evidence_span_id = "span-replacement",
    source_id = source@resource_id,
    quote = "captured quote"
  )
  claim <- tempest_claim(
    claim_id = "claim-replacement",
    claim_text = "Replacement claim",
    source_ids = source@resource_id,
    evidence_span_ids = span@evidence_span_id,
    supporting_quotes = list(span@quote)
  )
  workspace$add_extracted_claim_batch(list(claim), list(span))
  replacement <- fake_source(
    "https://example.org/replacement",
    content_text = "Content no longer contains the evidence."
  )

  expect_error(
    workspace$upsert_retrieved_resource(replacement),
    class = "tempest_research_workspace_integrity_error"
  )
  expect_identical(
    workspace$get_retrieved_source(source@resource_id)$content_text,
    source@content
  )
  expect_no_error(workspace$validate_integrity())
})

test_that("claims require resolvable unique contradicting sources", {
  workspace <- fake_store_with_sources(1)
  source_id <- workspace$list_retrieved_sources()[[1]]$id
  expect_error(
    workspace$add_proposed_claim(tempest_claim(
      "Contradicted claim",
      source_ids = source_id,
      contradicting_source_ids = "Sunknownsource"
    )),
    class = "tempest_research_workspace_integrity_error"
  )
  expect_error(
    tempest_claim(
      "Duplicate contradiction",
      source_ids = source_id,
      contradicting_source_ids = rep(source_id, 2L)
    ),
    class = "simpleError"
  )
  expect_no_error(workspace$add_proposed_claim(tempest_claim(
    "Mixed evidence",
    source_ids = source_id,
    contradicting_source_ids = source_id
  )))
})

test_that("extracted batches reject duplicate spans without mutation", {
  workspace <- fake_store_with_sources(1)
  source_id <- workspace$list_retrieved_sources()[[1]]$id
  span <- tempest_evidence_span(
    evidence_span_id = "span-duplicate",
    source_id = source_id,
    quote = "Body text"
  )
  claim <- tempest_claim(
    claim_id = "claim-duplicate",
    claim_text = "Duplicate span claim",
    source_ids = source_id,
    evidence_span_ids = span@evidence_span_id
  )

  expect_error(
    workspace$add_extracted_claim_batch(
      list(claim),
      list(span, span)
    ),
    class = "tempest_research_workspace_integrity_error"
  )
  expect_length(workspace$list_proposed_claims(), 0L)
  expect_length(workspace$list_evidence_spans(), 0L)
})

test_that("extracted batches reject source-mismatched spans atomically", {
  workspace <- fake_store_with_sources(2)
  source_ids <- vapply(
    workspace$list_retrieved_sources(),
    `[[`,
    character(1),
    "id"
  )
  span <- tempest_evidence_span(
    evidence_span_id = "span-mismatched",
    source_id = source_ids[[1]],
    quote = "Body text"
  )
  claim <- tempest_claim(
    claim_id = "claim-mismatched",
    claim_text = "Mismatched span claim",
    source_ids = source_ids[[2]],
    evidence_span_ids = span@evidence_span_id
  )

  expect_error(
    workspace$add_extracted_claim_batch(list(claim), list(span)),
    class = "tempest_research_workspace_integrity_error"
  )
  expect_length(workspace$list_proposed_claims(), 0L)
  expect_length(workspace$list_evidence_spans(), 0L)
})

test_that("extracted batches roll back spans and claims on commit tampering", {
  workspace <- fake_store_with_sources(1)
  source_id <- workspace$list_retrieved_sources()[[1]]$id
  span <- tempest_evidence_span(
    evidence_span_id = "span-rollback",
    source_id = source_id,
    quote = "Body text"
  )
  claim <- tempest_claim(
    claim_id = "claim-span-rollback",
    claim_text = "Rollback linked claim",
    source_ids = source_id,
    evidence_span_ids = span@evidence_span_id,
    supporting_quotes = list(span@quote)
  )

  expect_error(
    workspace$add_extracted_claim_batch(
      list(claim),
      list(span),
      commit = function() {
        rlang::abort("terminal record tampered", class = "test_commit_error")
      }
    ),
    class = "test_commit_error"
  )
  expect_length(workspace$list_proposed_claims(), 0L)
  expect_length(workspace$list_evidence_spans(), 0L)
  expect_length(workspace$proposed_claims_for_resource(source_id), 0L)
})
