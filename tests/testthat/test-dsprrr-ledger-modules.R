test_that("module factory includes extract_claims and verify_claim_support", {
  skip_if_not_installed("dsprrr")
  cfg <- tempest_config()
  modules <- tempest_make_dsprrr_modules(cfg)
  expect_true(all(c("extract_claims", "verify_claim_support") %in% names(modules)))
  expect_false("fact_extraction" %in% names(modules))
})
