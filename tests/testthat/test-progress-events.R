test_that("tempest_progress_event records STORM stage progress", {
  event <- tempest_progress_event(
    run_id = "run-1",
    workflow = "storm",
    event_type = "stage",
    status = "started",
    stage = "research",
    payload = list(query_count = 3L)
  )

  expect_identical(S7::S7_inherits(event, tempest_progress_event), TRUE)
  expect_equal(event@workflow, "storm")
  expect_equal(event@event_type, "stage")
  expect_equal(event@stage, "research")
  expect_equal(event@payload$query_count, 3L)

  data <- tempest_progress_event_data(event)
  expect_named(
    data,
    c(
      "event_id",
      "run_id",
      "workflow",
      "event_type",
      "stage",
      "step",
      "status",
      "timestamp",
      "message",
      "payload",
      "parent_event_id",
      "correlation_id"
    )
  )
})

test_that("tempest_progress_event records Co-STORM expert and tool progress", {
  expert_event <- tempest_progress_event(
    run_id = "session-1",
    workflow = "costorm",
    event_type = "expert",
    status = "started",
    stage = "warmup",
    step = "expert_fanout",
    payload = list(expert_id = "history", expert_name = "Historian"),
    correlation_id = "turn-1"
  )
  tool_event <- tempest_progress_event(
    run_id = "session-1",
    workflow = "costorm",
    event_type = "tool",
    status = "running",
    stage = "dialogue",
    step = "ask_expert",
    payload = list(tool_name = "ask_history", expert_id = "history"),
    parent_event_id = expert_event@event_id,
    correlation_id = expert_event@correlation_id
  )

  expect_equal(expert_event@payload$expert_id, "history")
  expect_equal(tool_event@payload$tool_name, "ask_history")
  expect_equal(tool_event@parent_event_id, expert_event@event_id)
  expect_equal(tool_event@correlation_id, "turn-1")
})

test_that("tempest_progress_event records cancellation and failure progress", {
  cancelled <- tempest_progress_event(
    run_id = "session-1",
    workflow = "costorm",
    event_type = "cancellation",
    status = "cancelled",
    message = "Superseded by a newer user turn."
  )
  failed <- tempest_progress_event(
    run_id = "run-1",
    workflow = "storm",
    event_type = "stage",
    status = "failed",
    stage = "verification",
    payload = list(error_class = "tempest_error")
  )

  expect_equal(cancelled@status, "cancelled")
  expect_equal(failed@status, "failed")
  expect_equal(failed@payload$error_class, "tempest_error")
})

test_that("tempest_progress_event rejects malformed core fields", {
  expect_error(
    tempest_progress_event(
      run_id = "",
      workflow = "storm",
      event_type = "stage",
      status = "started",
      stage = "research"
    ),
    "run_id"
  )
  expect_error(
    tempest_progress_event(
      run_id = "run-1",
      workflow = "scripted",
      event_type = "stage",
      status = "started",
      stage = "research"
    ),
    "workflow"
  )
  expect_error(
    tempest_progress_event(
      run_id = "run-1",
      workflow = "storm",
      event_type = "stage",
      status = "started"
    ),
    "stage"
  )
  expect_error(
    tempest_progress_event(
      run_id = "run-1",
      workflow = "storm",
      event_type = "workflow",
      status = "done"
    ),
    "status"
  )
  expect_error(
    tempest_progress_event_data(list()),
    "tempest_progress_event"
  )
})
