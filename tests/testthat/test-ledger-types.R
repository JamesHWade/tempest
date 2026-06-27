test_that("tempest_claim validates enums and scores", {
  cl <- tempest_claim(claim_text = "Water boils at 100C", source_ids = "S0123456789ab")
  expect_equal(cl@claim_type, "finding")
  expect_equal(cl@verification_status, "unverified")
  expect_true(startsWith(cl@claim_id, "C"))

  expect_error(tempest_claim(claim_text = "x", claim_type = "nonsense"), "claim_type")
  expect_error(tempest_claim(claim_text = "x", verification_status = "bogus"), "verification_status")
  expect_error(tempest_claim(claim_text = "x", support_score = 1.5), "\\[0, 1\\]")
  expect_error(tempest_claim(claim_text = ""), "claim_text")
})

test_that("tempest_evidence_span and tempest_dispute construct", {
  sp <- tempest_evidence_span(source_id = "S0123456789ab", quote = "boils at 100C")
  expect_true(startsWith(sp@evidence_span_id, "E"))

  d <- tempest_dispute(topic = "boiling point", claim_ids = c("C1", "C2"),
                       evidence_balance = "mixed")
  expect_true(startsWith(d@dispute_id, "D"))
  expect_error(tempest_dispute(topic = "x", evidence_balance = "weird"), "evidence_balance")
})

test_that("tempest_source_record carries fields and meta", {
  s <- tempest_source_record(url = "https://example.org", title = "Ex")
  expect_true(startsWith(s@id, "S"))
  expect_type(s@meta, "list")
})
