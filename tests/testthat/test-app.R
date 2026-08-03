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
    called <<- list(
      app_dir = app_dir,
      args = list(...),
      provider_timeout_s = getOption("ellmer_timeout_s")
    )
    invisible("launched")
  })

  result <- run_app(port = 9999L, launch.browser = FALSE)

  expect_equal(result, "launched")
  expect_equal(dir.exists(called$app_dir), TRUE)
  expect_match(called$app_dir, "shiny$")
  expect_equal(called$args$port, 9999L)
  expect_equal(called$args$launch.browser, FALSE)
  expect_equal(called$provider_timeout_s, 120)
})

test_that("run_app rejects an invalid configured runner", {
  withr::local_options(tempest.app_runner = "not a function")

  expect_error(run_app(), class = "tempest_shiny_error")
})

test_that("Shiny requirements reject an incompatible shinychat backend", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  local_mocked_bindings(
    tempest_shinychat_backend = \() list(version = "0.4.0.9000")
  )

  ui_error <- tryCatch(
    tempest_shiny_require_ui("chat"),
    error = identity
  )
  server_error <- tryCatch(
    tempest_shiny_require_server("chat"),
    error = identity
  )

  expect_s3_class(ui_error, "tempest_shinychat_error")
  expect_match(conditionMessage(ui_error), "chat_server")
  expect_s3_class(server_error, "tempest_shinychat_error")
  expect_match(conditionMessage(server_error), "chat_server")
})

test_that("run_app validates and preserves shorter provider timeouts", {
  withr::local_options(
    tempest.shiny.provider_timeout_s = 0,
    tempest.app_runner = function(...) invisible(NULL)
  )
  expect_error(run_app(), class = "tempest_shiny_error")

  withr::local_options(
    tempest.shiny.provider_timeout_s = 120,
    ellmer_timeout_s = 30,
    tempest.app_runner = function(...) getOption("ellmer_timeout_s")
  )
  expect_equal(run_app(), 30)
})
