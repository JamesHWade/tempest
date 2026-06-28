test_that("tempest_run_dsprrr_module signals fallback on error", {
  skip_if_not_installed("dsprrr")
  bad_module <- structure(list(), class = "dsprrr_module")
  local_mocked_bindings(
    .package = "dsprrr",
    run = function(...) stop("boom")
  )
  expect_warning(
    out <- tempest:::tempest_run_dsprrr_module(
      bad_module,
      fake_chat(),
      list(x = 1),
      "test step"
    ),
    "falling back"
  )
  expect_null(out)
})
