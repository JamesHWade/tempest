test_that("tempest_run_async returns before background work completes", {
  skip_if_not_installed("promises")
  skip_if_not_installed("mirai")
  skip_if_not_installed("later")
  withr::local_options(tempest.async_runner = function(topic) {
    Sys.sleep(0.4)
    list(topic = topic, worker_pid = Sys.getpid())
  })
  started <- Sys.time()

  run <- tempest_run_async("Background topic")
  elapsed <- as.numeric(difftime(Sys.time(), started, units = "secs"))
  settled <- await_tempest_promise(run)

  expect_s3_class(run, "tempest_async_run")
  expect_lt(elapsed, 0.2)
  expect_null(settled$error)
  expect_equal(settled$value$topic, "Background topic")
  expect_false(identical(settled$value$worker_pid, Sys.getpid()))
  expect_equal(tempest_run_cancel(run), FALSE)
})

test_that("tempest_run_async preserves classed worker errors", {
  skip_if_not_installed("promises")
  skip_if_not_installed("mirai")
  skip_if_not_installed("later")
  withr::local_options(tempest.async_runner = function(topic) {
    rlang::abort("worker failed", class = "tempest_test_worker_error")
  })

  settled <- await_tempest_promise(tempest_run_async("Failure topic"))

  expect_s3_class(settled$error, "tempest_test_worker_error")
  expect_match(conditionMessage(settled$error), "worker failed", fixed = TRUE)
})

test_that("tempest_run_cancel stops unresolved workers", {
  skip_if_not_installed("promises")
  skip_if_not_installed("mirai")
  skip_if_not_installed("later")
  withr::local_options(tempest.async_runner = function(topic) {
    Sys.sleep(5)
    topic
  })
  run <- tempest_run_async("Cancelled topic")

  expect_equal(tempest_run_cancel(run), TRUE)
  settled <- await_tempest_promise(run)

  expect_s3_class(settled$error, "tempest_async_cancelled")
  expect_equal(tempest_run_cancel(run), FALSE)
})
