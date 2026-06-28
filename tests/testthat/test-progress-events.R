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

test_that("tempest_progress_collector records and filters events", {
  collector <- tempest_progress_collector(include_payload = TRUE)
  events <- list(
    tempest_progress_event(
      run_id = "run-1",
      workflow = "storm",
      event_type = "stage",
      status = "started",
      stage = "research",
      correlation_id = "stage-1",
      payload = list(position = 1L)
    ),
    tempest_progress_event(
      run_id = "run-1",
      workflow = "storm",
      event_type = "stage",
      status = "succeeded",
      stage = "research",
      correlation_id = "stage-1",
      payload = list(position = 2L)
    ),
    tempest_progress_event(
      run_id = "session-1",
      workflow = "costorm",
      event_type = "tool",
      status = "running",
      stage = "dialogue",
      correlation_id = "turn-1",
      payload = list(tool_name = "ask_history")
    )
  )
  lapply(events, collector$record)

  expect_length(collector$events(run_id = "run-1"), 2L)
  expect_length(collector$events(workflow = "costorm"), 1L)
  expect_length(collector$events(stage = "research", status = "succeeded"), 1L)
  expect_length(collector$events(correlation_id = "stage-1"), 2L)
  expect_equal(
    collector$data(stage = "research", status = "succeeded")[[
      1
    ]]$payload$position,
    2L
  )
})

test_that("tempest_progress_collector strips payloads unless requested", {
  event <- tempest_progress_event(
    run_id = "run-1",
    workflow = "storm",
    event_type = "stage",
    status = "started",
    stage = "research",
    payload = list(secret = "keep out")
  )
  default_collector <- tempest_progress_collector()
  rich_collector <- tempest_progress_collector(include_payload = TRUE)

  default_collector$record(event)
  rich_collector$record(event)

  expect_equal(default_collector$data()[[1]]$payload, list())
  expect_equal(rich_collector$data()[[1]]$payload$secret, "keep out")
})

test_that("tempest_progress_replay sends filtered events to a callback", {
  collector <- tempest_progress_collector(include_payload = TRUE)
  collector$record(tempest_progress_event(
    run_id = "run-1",
    workflow = "storm",
    event_type = "stage",
    status = "started",
    stage = "research"
  ))
  collector$record(tempest_progress_event(
    run_id = "run-1",
    workflow = "storm",
    event_type = "stage",
    status = "succeeded",
    stage = "research"
  ))
  replayed <- character()

  collector$replay(
    function(event) {
      replayed <<- c(replayed, event@status)
    },
    status = "succeeded"
  )

  expect_equal(replayed, "succeeded")
})

test_that("tempest_progress_state reduces completed STORM events", {
  events <- list(
    tempest_progress_event(
      event_id = "event-workflow-start",
      run_id = "run-1",
      workflow = "storm",
      event_type = "workflow",
      status = "started",
      timestamp = "2026-06-28T00:00:00Z"
    ),
    tempest_progress_event(
      event_id = "event-research-start",
      run_id = "run-1",
      workflow = "storm",
      event_type = "stage",
      status = "started",
      stage = "research",
      timestamp = "2026-06-28T00:00:01Z"
    ),
    tempest_progress_event(
      event_id = "event-research-done",
      run_id = "run-1",
      workflow = "storm",
      event_type = "stage",
      status = "succeeded",
      stage = "research",
      timestamp = "2026-06-28T00:00:02Z"
    ),
    tempest_progress_event(
      event_id = "event-writing-start",
      run_id = "run-1",
      workflow = "storm",
      event_type = "stage",
      status = "started",
      stage = "writing",
      timestamp = "2026-06-28T00:00:03Z"
    ),
    tempest_progress_event(
      event_id = "event-writing-done",
      run_id = "run-1",
      workflow = "storm",
      event_type = "stage",
      status = "succeeded",
      stage = "writing",
      timestamp = "2026-06-28T00:00:04Z"
    ),
    tempest_progress_event(
      event_id = "event-report",
      run_id = "run-1",
      workflow = "storm",
      event_type = "artifact",
      status = "available",
      stage = "writing",
      step = "report_md",
      payload = list(artifact = "report_md", persisted = TRUE),
      timestamp = "2026-06-28T00:00:05Z"
    ),
    tempest_progress_event(
      event_id = "event-workflow-done",
      run_id = "run-1",
      workflow = "storm",
      event_type = "workflow",
      status = "succeeded",
      timestamp = "2026-06-28T00:00:06Z"
    )
  )

  state <- tempest_progress_state(events)

  expect_equal(state$run_id, "run-1")
  expect_equal(state$workflow, "storm")
  expect_equal(state$status, "succeeded")
  expect_equal(state$terminal, TRUE)
  expect_equal(state$completed_stages, c("research", "writing"))
  expect_equal(state$current_stage, NA_character_)
  expect_equal(state$current_step, NA_character_)
  expect_length(state$active$stages, 0L)
  expect_equal(state$artifacts$report_md$persisted, TRUE)
  expect_equal(state$event_count, 7L)
})

test_that("tempest_progress_state tracks Co-STORM active expert and tool work", {
  events <- list(
    tempest_progress_event(
      event_id = "workflow-start",
      run_id = "session-1",
      workflow = "costorm",
      event_type = "workflow",
      status = "started",
      stage = "session",
      step = "created"
    ),
    tempest_progress_event(
      event_id = "warmup-start",
      run_id = "session-1",
      workflow = "costorm",
      event_type = "stage",
      status = "started",
      stage = "warmup",
      step = "expert_fanout"
    ),
    tempest_progress_event(
      event_id = "expert-start",
      run_id = "session-1",
      workflow = "costorm",
      event_type = "expert",
      status = "started",
      stage = "warmup",
      step = "expert_fanout",
      parent_event_id = "warmup-start",
      payload = list(expert_id = "2", expert_name = "Dr. Flow")
    ),
    tempest_progress_event(
      event_id = "tool-start",
      run_id = "session-1",
      workflow = "costorm",
      event_type = "tool",
      status = "started",
      stage = "warmup",
      step = "expert_question",
      parent_event_id = "expert-start",
      payload = list(
        expert_id = "2",
        expert_name = "Dr. Flow",
        question_index = 1L
      )
    )
  )

  active <- tempest_progress_state(events)

  expect_equal(active$status, "running")
  expect_equal(active$current_stage, "warmup")
  expect_equal(active$current_step, "expert_question")
  expect_length(active$active$experts, 1L)
  expect_length(active$active$tools, 1L)
  expect_equal(active$active$experts[[1]]$expert_name, "Dr. Flow")

  done <- tempest_progress_state(c(
    events,
    list(
      tempest_progress_event(
        event_id = "tool-done",
        run_id = "session-1",
        workflow = "costorm",
        event_type = "tool",
        status = "succeeded",
        stage = "warmup",
        step = "expert_question",
        parent_event_id = "tool-start",
        payload = list(
          expert_id = "2",
          expert_name = "Dr. Flow",
          question_index = 1L
        )
      ),
      tempest_progress_event(
        event_id = "expert-done",
        run_id = "session-1",
        workflow = "costorm",
        event_type = "expert",
        status = "succeeded",
        stage = "warmup",
        step = "expert_fanout",
        parent_event_id = "expert-start",
        payload = list(expert_id = "2", expert_name = "Dr. Flow")
      ),
      tempest_progress_event(
        event_id = "warmup-done",
        run_id = "session-1",
        workflow = "costorm",
        event_type = "stage",
        status = "succeeded",
        stage = "warmup",
        step = "expert_fanout",
        parent_event_id = "warmup-start"
      )
    )
  ))

  expect_equal(done$completed_stages, "warmup")
  expect_equal(done$current_stage, NA_character_)
  expect_length(done$active$experts, 0L)
  expect_length(done$active$tools, 0L)
})

test_that("tempest_progress_state summarizes failures and cancellations", {
  failed <- tempest_progress_state(list(
    tempest_progress_event(
      event_id = "workflow-start",
      run_id = "run-1",
      workflow = "storm",
      event_type = "workflow",
      status = "started"
    ),
    tempest_progress_event(
      event_id = "research-start",
      run_id = "run-1",
      workflow = "storm",
      event_type = "stage",
      status = "started",
      stage = "research"
    ),
    tempest_progress_event(
      event_id = "research-failed",
      run_id = "run-1",
      workflow = "storm",
      event_type = "stage",
      status = "failed",
      stage = "research",
      payload = list(
        error_class = "tempest_error",
        error_message = "Research failed."
      )
    ),
    tempest_progress_event(
      event_id = "workflow-failed",
      run_id = "run-1",
      workflow = "storm",
      event_type = "workflow",
      status = "failed",
      payload = list(
        error_class = "tempest_error",
        error_message = "Workflow failed."
      )
    )
  ))

  expect_equal(failed$status, "failed")
  expect_equal(failed$terminal, TRUE)
  expect_length(failed$failures, 2L)
  expect_equal(failed$failures[[1]]$stage, "research")
  expect_equal(failed$failures[[2]]$error_message, "Workflow failed.")
  expect_length(failed$active$stages, 0L)

  cancelled <- tempest_progress_state(list(
    tempest_progress_event(
      event_id = "workflow-start",
      run_id = "session-1",
      workflow = "costorm",
      event_type = "workflow",
      status = "started"
    ),
    tempest_progress_event(
      event_id = "turn-start",
      run_id = "session-1",
      workflow = "costorm",
      event_type = "stage",
      status = "started",
      stage = "dialogue",
      step = "turn"
    ),
    tempest_progress_event(
      event_id = "cancel",
      run_id = "session-1",
      workflow = "costorm",
      event_type = "cancellation",
      status = "cancelled",
      stage = "dialogue",
      message = "Superseded."
    )
  ))

  expect_equal(cancelled$status, "cancelled")
  expect_equal(cancelled$terminal, TRUE)
  expect_equal(cancelled$cancellation$message, "Superseded.")
  expect_equal(cancelled$current_stage, NA_character_)
  expect_length(cancelled$active$stages, 0L)
})

test_that("tempest_progress_state composes with collectors and event data", {
  collector <- tempest_progress_collector(include_payload = TRUE)
  collector$record(tempest_progress_event(
    event_id = "workflow-start",
    run_id = "run-1",
    workflow = "storm",
    event_type = "workflow",
    status = "started"
  ))
  collector$record(tempest_progress_event(
    event_id = "verification-skipped",
    run_id = "run-1",
    workflow = "storm",
    event_type = "stage",
    status = "skipped",
    stage = "verification"
  ))

  from_collector <- tempest_progress_state(collector)
  from_data <- tempest_progress_state(collector$data())

  expect_equal(from_collector$status, "running")
  expect_equal(from_collector$skipped_stages, "verification")
  expect_equal(from_data$skipped_stages, "verification")
})
