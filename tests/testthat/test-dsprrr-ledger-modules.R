test_that("module factory includes extract_claims and verify_claim_support", {
  skip_if_not_installed("dsprrr")
  cfg <- tempest_config()
  modules <- tempest_make_dsprrr_modules(cfg)
  expect_contains(names(modules), c("extract_claims", "verify_claim_support"))
  # `fact_extraction` is retained as a transitional alias of `extract_claims`.
  expect_identical(modules$fact_extraction, modules$extract_claims)
})

test_that("extract_claims module accepts source context inputs", {
  skip_if_not_installed("dsprrr")
  cfg <- tempest_config()
  modules <- tempest_make_dsprrr_modules(cfg)
  inputs <- S7::prop(modules$extract_claims$signature, "inputs")
  input_names <- vapply(inputs, `[[`, character(1), "name")
  expect_equal(
    input_names,
    c("answer_text", "source_context", "source_ids", "citation_mode")
  )
})
