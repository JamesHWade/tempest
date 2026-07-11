test_that("run_app validates dependencies and delegates to the app runner", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("shinychat")
  skip_if_not_installed("ellmer")
  skip_if_not_installed("promises")
  skip_if_not_installed("later")
  skip_if_not_installed("mirai")
  skip_if_not_installed("zip")
  called <- NULL
  withr::local_options(tempest.app_runner = function(app_dir, ...) {
    called <<- list(app_dir = app_dir, args = list(...))
    invisible("launched")
  })

  result <- run_app(port = 9999L, launch.browser = FALSE)

  expect_equal(result, "launched")
  expect_equal(dir.exists(called$app_dir), TRUE)
  expect_match(called$app_dir, "shiny$")
  expect_equal(called$args$port, 9999L)
  expect_equal(called$args$launch.browser, FALSE)
})

test_that("run_app rejects an invalid configured runner", {
  withr::local_options(tempest.app_runner = "not a function")

  expect_error(run_app(), class = "tempest_shiny_error")
})
