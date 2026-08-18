test_that("execution events query only product session histories", {
  event <- tempest_progress_event(
    run_id = "session-events",
    workflow = "costorm",
    event_type = "workflow",
    status = "started",
    event_id = "event-1",
    timestamp = "2026-08-17 UTC"
  ) |>
    tempest_progress_event_data()
  event$sequence <- 1L
  session <- structure(list(events = list(event)), class = "TempestSession")

  expect_identical(tempest_execution_events(session), list(event))
  expect_identical(tempest_execution_events(session, 1L), list())
  expect_error(
    tempest_execution_events(list()),
    class = "tempest_execution_events_error"
  )
})
