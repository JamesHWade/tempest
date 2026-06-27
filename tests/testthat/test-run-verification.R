test_that("tempest_run verifies claims before polishing when policy requires it", {
  store <- fake_store_with_sources(1)
  s1 <- store$list_sources()[[1]]$id
  store$add_claim(tempest_claim(claim_text = "c", source_ids = s1))
  judge <- fake_chat(structured = list(list(status = "supported", score = 0.9, rationale = "ok")))
  cfg <- tempest_config(citation_policy = "claim_verified")

  tempest_run_verification(store, cfg, verifier = judge)

  expect_equal(store$list_claims()[[1]]@verification_status, "supported")
  expect_false(is.null(store$get_artifact("citation_audit")))
})

test_that("verification is skipped under the default policy", {
  store <- fake_store_with_sources(1)
  store$add_claim(tempest_claim(claim_text = "c", source_ids = store$list_sources()[[1]]$id))
  cfg <- tempest_config()  # source_attributed
  tempest_run_verification(store, cfg, verifier = fake_chat())
  expect_equal(store$list_claims()[[1]]@verification_status, "unverified")
})
