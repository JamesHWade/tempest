test_that("SourceStore stores and indexes claims", {
  store <- fake_store_with_sources(2)
  id1 <- store$add_claim(tempest_claim(claim_text = "a", source_ids = "S1"))
  id2 <- store$add_claim(tempest_claim(claim_text = "b", source_ids = c("S1", "S2")))
  expect_length(store$list_claims(), 2)
  expect_equal(store$get_claim(id1)@claim_text, "a")
  by_src <- store$claims_for_source("S1")
  expect_length(by_src, 2)
})

test_that("SourceStore links evidence spans and verifies claims", {
  store <- SourceStore$new()
  cid <- store$add_claim(tempest_claim(claim_text = "a", source_ids = "S1"))
  sid <- store$add_evidence_span(tempest_evidence_span(source_id = "S1", quote = "q"))
  store$link_evidence(cid, sid)
  expect_equal(store$get_claim(cid)@evidence_span_ids, sid)
  expect_length(store$get_evidence_for_claim(cid), 1)

  store$verify_claim(cid, status = "supported", score = 0.8, verifier = "judge/x")
  cl <- store$get_claim(cid)
  expect_equal(cl@verification_status, "supported")
  expect_equal(cl@support_score, 0.8)
  expect_false(is.na(cl@verified_at))
})

test_that("to_tibbles includes claims and keeps content_text/meta on sources", {
  store <- fake_store_with_sources(1)
  store$add_claim(tempest_claim(claim_text = "a", source_ids = "S1"))
  tb <- store$to_tibbles()
  expect_true("claims" %in% names(tb))
  expect_equal(nrow(tb$claims), 1)
  expect_true(all(c("content_text", "meta") %in% names(tb$sources)))
})
