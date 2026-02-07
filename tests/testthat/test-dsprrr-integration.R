test_that("tempest_has_dsprrr returns logical", {
  result <- tempest:::tempest_has_dsprrr()
  expect_type(result, "logical")
  expect_length(result, 1)
})

test_that("tempest_make_dsprrr_modules returns NULL without dsprrr", {
  skip_if(tempest:::tempest_has_dsprrr(), "dsprrr is installed, skipping absence test")

  cfg <- tempest_config()
  result <- tempest:::tempest_make_dsprrr_modules(cfg)
  expect_null(result)
})

test_that("tempest_make_dsprrr_modules creates modules when available", {
  skip_if_not_installed("dsprrr")

  cfg <- tempest_config()
  result <- tempest:::tempest_make_dsprrr_modules(cfg)

  if (!is.null(result)) {
    expect_type(result, "list")
    expect_true("query_decomposition" %in% names(result))
    expect_true("fact_extraction" %in% names(result))
    expect_true("draft_outline" %in% names(result))
    expect_true("refined_outline" %in% names(result))
    expect_true("section_writing" %in% names(result))
    expect_true("lead_section" %in% names(result))
  }
})
