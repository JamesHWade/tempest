test_that("run status and event accessors preserve ordered cursors", {
  objective <- tempest_objective(
    "Complete one step",
    objective_id = "objective-accessors",
    created_at = "2026-07-18 UTC"
  )
  registry <- tempest_operation_registry(list(
    complete = list(
      kind = "step",
      implementation = function() "done"
    )
  ))
  workflow <- tempest_workflow_spec(
    "accessor-workflow",
    title = "Accessor workflow",
    purpose = "Exercise accessors",
    steps = list(tempest_workflow_step(
      "complete",
      title = "Complete",
      purpose = "Complete the work",
      operation_id = "complete"
    ))
  )
  run <- tempest_run_workflow(objective, workflow, registry)
  events <- tempest_run_events(run)
  execution_events <- tempest_execution_events(run)
  cursor <- events[[2]]$sequence
  remaining <- tempest_run_events(run, after_sequence = cursor)

  expect_identical(tempest_run_status(run), "succeeded")
  expect_identical(execution_events, events)
  expect_identical(
    vapply(events, \(event) event$sequence, integer(1)),
    seq_along(events)
  )
  expect_true(all(vapply(
    remaining,
    \(event) event$sequence > cursor,
    logical(1)
  )))
  expect_error(
    tempest_run_events(run, after_sequence = -1L),
    class = "tempest_run_accessor_error"
  )
  expect_error(
    tempest_run_status(list()),
    class = "tempest_run_accessor_error"
  )
  expect_error(
    tempest_execution_events(list()),
    class = "tempest_execution_events_error"
  )
})

test_that("run accessor operations discard external condition details", {
  secret <- "Authorization: Bearer accessor-secret-token"
  condition <- rlang::catch_cnd(
    tempest:::tempest_run_accessor_call(
      "resume",
      \() stop(secret)
    )
  )

  expect_s3_class(condition, "tempest_run_accessor_error")
  expect_identical(
    conditionMessage(condition),
    "Tempest run operation `resume` failed."
  )
  expect_no_match(
    paste(capture.output(print(condition)), collapse = "\n"),
    secret,
    fixed = TRUE
  )
  expect_null(condition$parent)
})

test_that("approval accessors resume only until the next checkpoint", {
  calls <- character()
  registry <- tempest_operation_registry(list(
    first = list(
      kind = "step",
      implementation = function() {
        calls <<- c(calls, "first")
      }
    ),
    second = list(
      kind = "step",
      implementation = function() {
        calls <<- c(calls, "second")
      }
    )
  ))
  workflow <- tempest_workflow_spec(
    "approval-workflow",
    title = "Approval workflow",
    purpose = "Exercise nonblocking approvals",
    steps = list(
      tempest_workflow_step(
        "first",
        title = "First",
        purpose = "First action",
        operation_id = "first",
        approval_checkpoint = TRUE
      ),
      tempest_workflow_step(
        "second",
        title = "Second",
        purpose = "Second action",
        operation_id = "second",
        dependency_ids = "first",
        approval_checkpoint = TRUE
      )
    )
  )
  run <- tempest_run_workflow(
    tempest_objective(
      "Complete approved actions",
      created_at = "2026-07-18 UTC"
    ),
    workflow,
    registry
  )
  first_id <- names(tempest_run_approvals(run, "pending"))[[1]]

  returned <- tempest_run_record_approval(
    run,
    first_id,
    decision = "approved",
    metadata = list(actor = "host"),
    resume = FALSE
  )

  expect_identical(returned, run)
  expect_identical(tempest_run_status(run), "pending")
  expect_length(calls, 0L)
  run$resume()
  expect_identical(tempest_run_status(run), "awaiting_approval")
  expect_identical(calls, "first")
  expect_length(tempest_run_approvals(run, "approved"), 1L)
  expect_length(tempest_run_approvals(run, "pending"), 1L)
  expect_error(
    tempest_run_approvals(run, "unknown"),
    class = "tempest_run_accessor_error"
  )
})

test_that("artifact and cancellation accessors delegate to run controls", {
  deliverable <- tempest_deliverable_spec(
    "answer",
    title = "Answer",
    purpose = "Store an answer",
    instructions = "Preserve content.",
    generator_id = "generator",
    renderer_ids = "renderer",
    media_types = "text/plain"
  )
  artifact <- tempest_artifact(
    deliverable,
    content = "Answer body",
    artifact_id = "answer-1",
    media_type = "text/plain"
  )
  catalog <- tempest_artifact_catalog(
    artifacts = list(artifact),
    deliverables = list(deliverable)
  )
  registry <- tempest_operation_registry(list(
    generator = list(
      kind = "generator",
      implementation = function(content) content
    ),
    renderer = list(
      kind = "renderer",
      implementation = function(content) content
    ),
    wait = list(
      kind = "step",
      implementation = function() "done"
    )
  ))
  workflow <- tempest_workflow_spec(
    "cancel-workflow",
    title = "Cancel workflow",
    purpose = "Wait for approval",
    steps = list(tempest_workflow_step(
      "wait",
      title = "Wait",
      purpose = "Wait",
      operation_id = "wait",
      approval_checkpoint = TRUE
    ))
  )
  run <- tempest_run_workflow(
    tempest_objective(
      "Wait",
      created_at = "2026-07-18 UTC"
    ),
    workflow,
    registry,
    deliverables = list(deliverable),
    artifact_catalog = catalog
  )

  records <- tempest_run_artifacts(run, include_content = TRUE)
  restored_artifact <- tempest_run_artifact(run, "answer-1")
  returned <- tempest_run_request_cancel(run, "Host stopped the run.")

  expect_equal(records[["answer-1"]]$content, "Answer body")
  expect_equal(restored_artifact@content, "Answer body")
  expect_error(
    tempest_run_artifact(run, "missing-artifact"),
    class = "tempest_run_accessor_error"
  )
  expect_identical(returned, run)
  expect_identical(tempest_run_status(run), "cancel_requested")
  expect_equal(
    tail(tempest_run_events(run), 1L)[[1]]$event_type,
    "cancellation.requested"
  )
  run$resume()
  expect_identical(tempest_run_status(run), "cancelled")
  expect_length(tempest_run_approvals(run, "pending"), 0L)
  expect_length(tempest_run_approvals(run, "cancelled"), 1L)
  expect_error(
    tempest_run_request_cancel(run, ""),
    class = "tempest_run_accessor_error"
  )
  expect_error(
    tempest_run_record_approval(
      run,
      names(run$approvals)[[1]],
      resume = NA
    ),
    class = "tempest_run_accessor_error"
  )
})
