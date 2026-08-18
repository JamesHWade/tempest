test_that("tempest_run verifies claims before polishing when policy requires it", {
  store <- fake_store_with_sources(1)
  test_add_verifiable_claim(
    store,
    key = "run-verification-supported",
    claim_text = "c",
    quote = "c"
  )
  judge <- fake_chat(
    structured = list(list(status = "supported", score = 0.9, rationale = "ok"))
  )
  cfg <- tempest_config(citation_policy = "claim_verified")
  program <- test_program_executions(cfg)$verify_claim_support

  tempest_run_verification(store, cfg, verifier = judge, program = program)

  expect_equal(
    store$list_proposed_claims()[[1]]@verification_status,
    "supported"
  )
  expect_s3_class(store$citation_audit, "tbl_df")
})

test_that("tempest_run_verification passes configured min_support_score", {
  store <- fake_store_with_sources(1)
  test_add_verifiable_claim(
    store,
    key = "run-verification-threshold",
    claim_text = "c",
    quote = "c"
  )
  judge <- fake_chat(
    structured = list(list(status = "supported", score = 0.8, rationale = "ok"))
  )
  cfg <- tempest_config(
    citation_policy = "claim_verified",
    min_support_score = 0.9
  )
  program <- test_program_executions(cfg)$verify_claim_support

  tempest_run_verification(store, cfg, verifier = judge, program = program)

  expect_equal(
    store$list_proposed_claims()[[1]]@verification_status,
    "unsupported"
  )
})

test_that("verification runs under the default rendering policy", {
  store <- fake_store_with_sources(1)
  test_add_verifiable_claim(
    store,
    key = "run-verification-default",
    claim_text = "c",
    quote = "c"
  )
  judge <- fake_chat(
    structured = list(list(status = "supported", score = 0.9, rationale = "ok"))
  )
  cfg <- tempest_config()
  program <- test_program_executions(cfg)$verify_claim_support

  tempest_run_verification(store, cfg, verifier = judge, program = program)

  expect_equal(
    store$list_proposed_claims()[[1]]@verification_status,
    "supported"
  )
})
