test_that("tempest_claim validates enums and scores", {
  cl <- tempest_claim(
    claim_text = "Water boils at 100C",
    source_ids = "S0123456789ab"
  )
  expect_equal(cl@claim_type, "finding")
  expect_equal(cl@verification_status, "unverified")
  expect_true(startsWith(cl@claim_id, "C"))

  expect_error(
    tempest_claim(claim_text = "x", claim_type = "nonsense"),
    "claim_type"
  )
  expect_error(
    tempest_claim(claim_text = "x", claim_type = NA_character_),
    "claim_type"
  )
  expect_error(
    tempest_claim(claim_text = "x", verification_status = "bogus"),
    "verification_status"
  )
  expect_error(
    tempest_claim(claim_text = "x", support_score = 1.5),
    "\\[0, 1\\]"
  )
  expect_error(tempest_claim(claim_text = ""), "claim_text")
})

test_that("tempest_evidence_span and tempest_dispute construct", {
  sp <- tempest_evidence_span(
    source_id = "S0123456789ab",
    quote = "boils at 100C"
  )
  expect_true(startsWith(sp@evidence_span_id, "E"))

  d <- tempest_dispute(
    topic = "boiling point",
    claim_ids = c("C1", "C2"),
    evidence_balance = "mixed"
  )
  expect_true(startsWith(d@dispute_id, "D"))
  expect_error(
    tempest_dispute(topic = "x", evidence_balance = "weird"),
    "evidence_balance"
  )
})

test_that("tempest_source_record carries fields and meta", {
  s <- tempest_source_record(url = "https://example.org", title = "Ex")
  expect_true(startsWith(s@id, "S"))
  expect_type(s@meta, "list")
})

test_that("claim round-trips through list", {
  cl <- tempest_claim(
    claim_text = "x",
    source_ids = c("S1", "S2"),
    claim_type = "method"
  )
  lst <- tempest_claim_to_list(cl)
  expect_equal(lst$claim_text, "x")
  expect_equal(lst$claim_type, "method")
  cl2 <- tempest_claim_from_list(lst)
  expect_equal(cl2@claim_id, cl@claim_id)
  expect_equal(cl2@source_ids, c("S1", "S2"))
})

test_that("claims convert to a tibble", {
  cls <- list(
    tempest_claim(claim_text = "a", source_ids = "S1"),
    tempest_claim(
      claim_text = "b",
      source_ids = "S2",
      verification_status = "supported"
    )
  )
  tb <- tempest_claims_tibble(cls)
  expect_s3_class(tb, "tbl_df")
  expect_equal(nrow(tb), 2)
  expect_true(all(
    c("claim_id", "claim_text", "verification_status", "support_score") %in%
      names(tb)
  ))
})
