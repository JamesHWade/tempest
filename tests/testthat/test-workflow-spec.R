test_that("workflow specifications are deterministic validated DAGs", {
  finish <- tempest_workflow_step(
    "finish",
    title = "Finish",
    purpose = "Use the draft",
    operation_id = "step.finish",
    dependency_ids = "draft",
    required_input_artifact_ids = "draft-artifact",
    assignment_rule = c("expert.two", "expert.one")
  )
  draft <- tempest_workflow_step(
    "draft",
    title = "Draft",
    purpose = "Create a draft",
    operation_id = "step.draft",
    produced_artifact_ids = "draft-artifact"
  )
  workflow <- tempest_workflow_spec(
    "customer-response",
    title = "Customer response",
    purpose = "Produce a reviewed response",
    steps = list(finish, draft),
    supported_deliverable_types = "text"
  )

  expect_identical(
    unname(vapply(
      workflow@steps,
      \(step) step@step_id,
      character(1)
    )),
    c("draft", "finish")
  )
  expect_identical(
    workflow@steps[[2]]@assignment_rule$expert_ids,
    c("expert.one", "expert.two")
  )
  expect_match(tempest_workflow_fingerprint(workflow), "^[a-f0-9]{64}$")

  restored <- tempest_workflow_spec_from_data(
    tempest_workflow_spec_record(workflow)
  )
  expect_identical(
    tempest_workflow_fingerprint(restored),
    tempest_workflow_fingerprint(workflow)
  )
})

test_that("workflow specifications reject invalid dependencies and artifacts", {
  make_step <- function(
    id,
    dependencies = character(),
    required = character(),
    produced = character()
  ) {
    tempest_workflow_step(
      id,
      title = id,
      purpose = id,
      operation_id = paste0("operation.", id),
      dependency_ids = dependencies,
      required_input_artifact_ids = required,
      produced_artifact_ids = produced
    )
  }

  expect_error(
    tempest_workflow_spec(
      "unknown-dependency",
      title = "Bad",
      purpose = "Bad",
      steps = list(make_step("one", "missing"))
    ),
    class = "tempest_workflow_definition_error"
  )
  expect_error(
    tempest_workflow_spec(
      "cycle",
      title = "Bad",
      purpose = "Bad",
      steps = list(
        make_step("one", "two"),
        make_step("two", "one")
      )
    ),
    class = "tempest_workflow_definition_error"
  )
  expect_error(
    tempest_workflow_spec(
      "unknown-artifact",
      title = "Bad",
      purpose = "Bad",
      steps = list(make_step("one", required = "missing"))
    ),
    class = "tempest_workflow_definition_error"
  )
})

test_that("workflow records detect specification drift", {
  workflow <- tempest_workflow_spec(
    "one-step",
    title = "One step",
    purpose = "Test",
    steps = list(tempest_workflow_step(
      "run",
      title = "Run",
      purpose = "Run",
      operation_id = "operation.run"
    ))
  )
  record <- tempest_workflow_spec_record(workflow)
  record$purpose <- "Changed"

  expect_error(
    tempest_workflow_spec_from_data(record),
    class = "tempest_workflow_definition_error"
  )
})
