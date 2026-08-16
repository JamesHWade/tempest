test_that("ResearchWorkspace stores and indexes proposed claims", {
  store <- fake_store_with_sources(2)
  source_ids <- vapply(store$list_retrieved_sources(), `[[`, character(1), "id")
  id1 <- store$add_proposed_claim(tempest_claim(
    claim_text = "a",
    source_ids = source_ids[[1]]
  ))
  id2 <- store$add_proposed_claim(tempest_claim(
    claim_text = "b",
    source_ids = source_ids
  ))
  expect_length(store$list_proposed_claims(), 2)
  expect_equal(store$get_proposed_claim(id1)@claim_text, "a")
  by_src <- store$proposed_claims_for_resource(source_ids[[1]])
  expect_length(by_src, 2)
})

test_that("ResearchWorkspace links evidence spans and verifies claims", {
  store <- fake_store_with_sources(1)
  source_id <- store$list_retrieved_sources()[[1]]$id
  cid <- store$add_proposed_claim(tempest_claim(
    claim_text = "a",
    source_ids = source_id
  ))
  sid <- store$add_evidence_span(tempest_evidence_span(
    source_id = source_id,
    quote = "Body text"
  ))
  store$link_evidence_to_proposed_claim(cid, sid)
  expect_equal(store$get_proposed_claim(cid)@evidence_span_ids, sid)
  expect_length(store$get_evidence_for_proposed_claim(cid), 1)

  store$verify_proposed_claim(
    cid,
    status = "supported",
    score = 0.8,
    verifier = "judge/x"
  )
  cl <- store$get_proposed_claim(cid)
  expect_equal(cl@verification_status, "supported")
  expect_equal(cl@support_score, 0.8)
  expect_type(cl@verified_at, "character")
  expect_match(cl@verified_at, "^\\d{4}-\\d{2}-\\d{2}")
})

test_that("to_tibbles exposes canonical research collections", {
  store <- fake_store_with_sources(1)
  source_id <- store$list_retrieved_sources()[[1]]$id
  store$add_proposed_claim(tempest_claim(
    claim_text = "a",
    source_ids = source_id
  ))
  tb <- store$to_tibbles()
  expect_named(tb, c("retrieved_resources", "proposed_claims"))
  expect_equal(nrow(tb$proposed_claims), 1)
  expect_contains(
    names(tb$retrieved_resources),
    c("content_text", "meta")
  )
})

test_that("ResearchWorkspace rejects orphans and keeps indexes valid", {
  store <- fake_store_with_sources(2)
  source_ids <- vapply(store$list_retrieved_sources(), `[[`, character(1), "id")
  claim <- tempest_claim(
    claim_text = "Indexed claim",
    source_ids = source_ids[[1]]
  )
  store$add_proposed_claim(claim)
  replacement <- S7::set_props(claim, source_ids = source_ids[[2]])
  store$add_proposed_claim(replacement)

  expect_length(store$proposed_claims_for_resource(source_ids[[1]]), 0)
  expect_equal(
    store$proposed_claims_for_resource(source_ids[[2]])[[1]]@claim_id,
    claim@claim_id
  )
  expect_error(
    store$add_proposed_claim(tempest_claim(
      claim_text = "Orphan",
      source_ids = "Smissing"
    )),
    class = "tempest_research_workspace_integrity_error"
  )
  expect_error(
    store$add_evidence_span(tempest_evidence_span(
      source_id = "Smissing",
      quote = "Orphan evidence"
    )),
    class = "tempest_research_workspace_integrity_error"
  )
  expect_error(
    store$link_evidence_to_proposed_claim(claim@claim_id, "Eunknown"),
    class = "tempest_research_workspace_integrity_error"
  )
})

test_that("ResearchWorkspace enforces its resource budget", {
  store <- test_research_workspace(max_sources = 1L)
  store$upsert_retrieved_resource(fake_source("https://example.org/first"))

  expect_no_error(store$upsert_retrieved_resource(fake_source(
    "https://example.org/first"
  )))
  expect_error(
    store$upsert_retrieved_resource(fake_source("https://example.org/second")),
    class = "tempest_research_workspace_integrity_error"
  )
})

test_that("ResearchWorkspace validates complete source records", {
  store <- test_research_workspace()
  malformed <- fake_source("https://example.org/malformed")
  malformed$meta <- "not a list"

  expect_error(
    store$upsert_retrieved_resource(malformed),
    class = "tempest_research_workspace_integrity_error"
  )
  malformed <- fake_source("https://example.org/missing-field")
  malformed$title <- NULL
  expect_error(
    store$upsert_retrieved_resource(malformed),
    class = "tempest_research_workspace_integrity_error"
  )
})
