test_that("module factory includes extract_claims and verify_claim_support", {
  skip_if_not_installed("dsprrr")
  cfg <- tempest_config()
  modules <- tempest_make_dsprrr_modules(cfg)
  expect_true(all(c("extract_claims", "verify_claim_support") %in% names(modules)))
  # `fact_extraction` is retained as a transitional alias of `extract_claims`.
  expect_identical(modules$fact_extraction, modules$extract_claims)
})
