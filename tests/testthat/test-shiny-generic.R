test_that("Shiny adapter exposes and controls generic run state", {
  skip_if_not_installed("shiny")

  objective <- tempest_objective(
    "Publish an approved action",
    objective_id = "objective-shiny",
    created_at = "2026-07-18 UTC"
  )
  registry <- tempest_operation_registry(list(
    publish = list(
      kind = "step",
      implementation = function() "published"
    )
  ))
  workflow <- tempest_workflow_spec(
    "host-action",
    title = "Host action",
    purpose = "Exercise generic host controls",
    steps = list(tempest_workflow_step(
      "publish",
      title = "Publish",
      purpose = "Publish the action",
      operation_id = "publish",
      approval_checkpoint = TRUE
    ))
  )
  run <- tempest_run_workflow(objective, workflow, registry)
  expect_identical(run$status, "awaiting_approval")

  shiny::testServer(
    tempest_shiny_server,
    args = list(
      panels = "sources",
      run = run
    ),
    {
      session$flushReact()
      expect_identical(shiny::isolate(run_status()), "awaiting_approval")
      pending <- shiny::isolate(run_approvals())
      expect_length(pending, 1L)
      expect_identical(shiny::isolate(run_assignments())$publish, character())
      expect_identical(shiny::isolate(run_events())[[1]]$sequence, 1L)
      expect_identical(
        shiny::isolate(run_events()),
        tempest_execution_events(run)
      )
      expect_identical(shiny::isolate(run_grants()), list())

      approve(names(pending)[[1]], "approved")
      session$flushReact()
      expect_identical(run$status, "succeeded")
      expect_identical(shiny::isolate(run_status()), "succeeded")
      expect_length(shiny::isolate(run_approvals()), 0L)
      grant <- shiny::isolate(run_grants())$publish
      expect_identical(grant$experts, list())
      expect_identical(grant$step, list())
      expect_identical(grant$attempt, 1L)
      expect_named(grant$attempts, "1")
    }
  )
})

test_that("Shiny shared store publishes custom run evidence and cancellation", {
  skip_if_not_installed("shiny")

  objective <- tempest_objective(
    "Review private evidence",
    objective_id = "objective-evidence",
    created_at = "2026-07-18 UTC"
  )
  registry <- tempest_operation_registry(list(
    review = list(kind = "step", implementation = function() "reviewed")
  ))
  workflow <- tempest_workflow_spec(
    "host-review",
    title = "Host review",
    purpose = "Review evidence",
    steps = list(tempest_workflow_step(
      "review",
      title = "Review",
      purpose = "Review evidence",
      operation_id = "review"
    ))
  )
  evidence <- quiet_source_store()
  resource <- tempest_resource(
    resource_kind = "host.document",
    locator = "documents/private-1",
    title = "Private brief",
    media_type = "text/plain",
    content = "Approved evidence."
  )
  evidence$upsert_resource(resource)
  run <- TempestRun$new(
    objective = objective,
    workflow = workflow,
    runtime = registry,
    source_store = evidence
  )
  store <- tempest_shiny_store()
  store$set(list(
    store = quiet_source_store(),
    workflow_run = NULL
  ))

  shiny::testServer(
    tempest_shiny_server,
    args = list(
      panels = "sources",
      store = store,
      run = run
    ),
    {
      session$flushReact()
      expect_identical(store$peek_run(), run)
      expect_identical(
        shiny::isolate(store$evidence_store()),
        evidence
      )
      records <- shiny::isolate(run_evidence())$resources
      expect_identical(records[[1]]$resource_kind, "host.document")
      expect_false("content" %in% names(records[[1]]))

      cancel("Host stopped the run.")
      session$flushReact()
      active <- shiny::isolate(run_reactive())
      expect_identical(active$status, "cancel_requested")
      expect_identical(shiny::isolate(run_status()), "cancel_requested")
    }
  )
})
