test_that("example host uses action and output approval lifecycle", {
  skip_if_not_installed("bslib")
  skip_if_not_installed("ellmer")
  skip_if_not_installed("shiny")
  skip_if_not_installed("shinychat")
  skip_if_not_installed("zip")

  example <- new.env(parent = globalenv())
  app_path <- system.file(
    "examples",
    "shiny-host",
    "app.R",
    package = "tempest",
    mustWork = TRUE
  )
  expect_match(app_path, "app[.]R$")
  sys.source(
    app_path,
    envir = example
  )
  expect_no_error(shiny::testServer(example$server, {
    session$flushReact()
    expect_match(output$run_status$html, "awaiting_approval")
  }))
  run <- example$new_host_run()
  action_approvals <- tempest_run_approvals(run, "pending")

  expect_length(action_approvals, 1L)
  expect_identical(action_approvals[[1]]$approval_kind, "step")
  tempest_run_record_approval(run, names(action_approvals)[[1]])

  artifact <- tempest_run_artifact(run, "action-register-json")
  output_approvals <- tempest_run_approvals(run, "pending")
  step_result <- run$step_states[["build-action-register"]]$result

  expect_identical(tempest_run_status(run), "awaiting_approval")
  expect_identical(artifact@status, "awaiting_approval")
  expect_length(output_approvals, 1L)
  expect_identical(output_approvals[[1]]$approval_kind, "artifact")
  expect_identical(
    output_approvals[[1]]$artifact_ids,
    "action-register-json"
  )
  expect_length(
    run$step_states[["build-action-register"]]$attempts,
    1L
  )
  expect_s3_class(step_result, "tempest_deliverable_result")
  expect_equal(
    unname(vapply(
      step_result$resolved_operations,
      \(operation) operation$id,
      character(1)
    )),
    c(
      "tempest.generator.provided_content",
      "tempest.validator.required_fields",
      "host.renderer.json"
    )
  )
  expect_identical(
    artifact@validation_results[[1]]@validator_id,
    "tempest.validator.required_fields"
  )
  expect_identical(artifact@validation_results[[1]]@status, "passed")
  expect_identical(artifact@producer_operation_id, "host.renderer.json")
  expect_identical(artifact@metadata$rendered_by, "host.renderer.json")

  tempest_run_record_approval(run, names(output_approvals)[[1]])
  artifact <- tempest_run_artifact(run, "action-register-json")

  expect_identical(tempest_run_status(run), "succeeded")
  expect_identical(artifact@status, "approved")
  expect_length(
    run$step_states[["build-action-register"]]$attempts,
    1L
  )
  expect_identical(artifact@media_type, "application/json")
  expect_identical(artifact@artifact_kind, "action-register")
  expect_identical(
    artifact@content$connection_scope,
    "project-documents"
  )
  expect_length(artifact@content$actions, 2L)
})
