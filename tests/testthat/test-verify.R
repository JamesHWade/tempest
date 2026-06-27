test_that("verify_claims labels each claim and returns an audit tibble", {
  store <- fake_store_with_sources(1)
  s1 <- store$list_sources()[[1]]$id
  store$add_claim(tempest_claim(claim_text = "supported claim", source_ids = s1))
  store$add_claim(tempest_claim(claim_text = "weak claim", source_ids = s1))

  judge <- fake_chat(structured = list(
    list(status = "supported", score = 0.92, rationale = "stated verbatim"),
    list(status = "unsupported", score = 0.10, rationale = "not in source")
  ))

  audit <- tempest_verify_claims(store, verifier = judge)
  expect_s3_class(audit, "tbl_df")
  expect_equal(nrow(audit), 2)
  expect_setequal(audit$verification_status, c("supported", "unsupported"))

  statuses <- vapply(store$list_claims(), function(c) c@verification_status, character(1))
  expect_true("supported" %in% statuses)
  expect_true("unsupported" %in% statuses)
})

test_that("verify_claims skips when policy is none/source_attributed", {
  store <- fake_store_with_sources(1)
  store$add_claim(tempest_claim(claim_text = "c", source_ids = store$list_sources()[[1]]$id))
  audit <- tempest_verify_claims(store, verifier = fake_chat(), policy = "source_attributed")
  expect_equal(nrow(audit), 0)
})
