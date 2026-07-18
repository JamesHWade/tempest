test_that("objectives capture application-neutral outcome requirements", {
  objective <- tempest_objective(
    "Prepare a response",
    title = "Customer response",
    objective_id = "objective-1",
    context = list(account = "Example"),
    constraints = "Do not speculate",
    acceptance_criteria = "Cite every recommendation",
    input_resource_ids = "resource-1",
    deliverable_ids = "response",
    created_at = "2026-07-18 UTC"
  )

  expect_identical(S7::S7_inherits(objective, TempestObjective), TRUE)
  expect_equal(objective@objective_id, "objective-1")
  expect_equal(objective@context$account, "Example")
  expect_equal(objective@acceptance_criteria, "Cite every recommendation")
  expect_identical(objective@schema_version, 1L)
})

test_that("objective validation rejects malformed values", {
  expect_error(
    tempest_objective(""),
    class = "tempest_workflow_spec_error"
  )
  expect_error(
    tempest_objective("Outcome", context = "not a list"),
    class = "tempest_workflow_spec_error"
  )
  expect_error(
    tempest_objective("Outcome", schema_version = 0),
    class = "tempest_workflow_spec_error"
  )
})

test_that("deliverable specifications are serializable and fingerprinted", {
  spec <- tempest_deliverable_spec(
    "response",
    title = "Response",
    purpose = "Answer the request",
    instructions = "Use evidence.",
    version = "2026-07-18",
    content_schema = list(response = "character"),
    required_fields = "response",
    generator_id = "generator.response",
    validator_ids = "validator.required",
    renderer_ids = c("renderer.markdown", "renderer.json"),
    operation_versions = c(
      "generator.response" = "2",
      "renderer.markdown" = "2026.1"
    ),
    media_types = c("text/markdown", "application/json"),
    filename_policy = list(stem = "response")
  )
  copy <- tempest_deliverable_spec(
    "response",
    title = "Response",
    purpose = "Answer the request",
    instructions = "Use evidence.",
    version = "2026-07-18",
    content_schema = list(response = "character"),
    required_fields = "response",
    generator_id = "generator.response",
    validator_ids = "validator.required",
    renderer_ids = c("renderer.markdown", "renderer.json"),
    operation_versions = c(
      "generator.response" = "2",
      "renderer.markdown" = "2026.1"
    ),
    media_types = c("text/markdown", "application/json"),
    filename_policy = list(stem = "response")
  )
  changed <- tempest_deliverable_spec(
    "response",
    title = "Response",
    purpose = "Answer the request",
    instructions = "Use verified evidence.",
    version = "2026-07-18",
    content_schema = list(response = "character"),
    required_fields = "response",
    generator_id = "generator.response",
    validator_ids = "validator.required",
    renderer_ids = c("renderer.markdown", "renderer.json"),
    operation_versions = c(
      "generator.response" = "2",
      "renderer.markdown" = "2026.1"
    ),
    media_types = c("text/markdown", "application/json"),
    filename_policy = list(stem = "response")
  )

  expect_identical(
    S7::S7_inherits(spec, TempestDeliverableSpec),
    TRUE
  )
  expect_equal(spec@media_types, c("text/markdown", "application/json"))
  expect_equal(spec@operation_versions[["generator.response"]], "2")
  expect_equal(
    tempest_deliverable_fingerprint(spec),
    tempest_deliverable_fingerprint(copy)
  )
  expect_false(
    identical(
      tempest_deliverable_fingerprint(spec),
      tempest_deliverable_fingerprint(changed)
    )
  )
})

test_that("deliverable specifications reject invalid operations and policies", {
  make_spec <- function(
    renderer_ids = "renderer.markdown",
    evidence_policy = "source_attributed",
    version = "1"
  ) {
    tempest_deliverable_spec(
      "response",
      title = "Response",
      purpose = "Answer the request",
      instructions = "Use evidence.",
      version = version,
      evidence_policy = evidence_policy,
      generator_id = "generator.response",
      renderer_ids = renderer_ids
    )
  }

  expect_error(
    make_spec(renderer_ids = character()),
    class = "tempest_workflow_spec_error"
  )
  expect_error(
    make_spec(evidence_policy = "anything"),
    class = "tempest_workflow_spec_error"
  )
  expect_error(
    make_spec(version = "bad version"),
    class = "tempest_workflow_spec_error"
  )
  expect_error(
    tempest_deliverable_spec(
      "response",
      title = "Response",
      purpose = "Answer the request",
      instructions = "Use evidence.",
      generator_id = "generator.response",
      renderer_ids = "renderer.markdown",
      operation_versions = c("unknown" = "1")
    ),
    class = "tempest_workflow_spec_error"
  )
})

test_that("validation results and typed artifacts preserve lineage", {
  spec <- tempest_deliverable_spec(
    "response",
    title = "Response",
    purpose = "Answer the request",
    instructions = "Use evidence.",
    generator_id = "generator.response",
    renderer_ids = "renderer.markdown"
  )
  validation <- tempest_validation_result(
    "validator.required",
    status = "passed",
    message = "All fields are present.",
    created_at = "2026-07-18 UTC"
  )
  artifact <- tempest_artifact(
    spec,
    content = "# Response",
    artifact_id = "artifact-1",
    producer_operation_id = "renderer.markdown",
    run_id = "run-1",
    resource_ids = "resource-1",
    claim_ids = "claim-1",
    evidence_span_ids = "span-1",
    validation_results = list(validation),
    status = "valid",
    created_at = "2026-07-18 UTC"
  )

  expect_identical(S7::S7_inherits(artifact, TempestArtifact), TRUE)
  expect_equal(artifact@deliverable_id, "response")
  expect_equal(artifact@resource_ids, "resource-1")
  expect_equal(artifact@validation_results[[1]]@status, "passed")
  expect_equal(artifact@status, "valid")
  expect_match(artifact@checksum, "^[a-f0-9]{64}$")
})

test_that("typed artifacts enforce content and validation invariants", {
  spec <- tempest_deliverable_spec(
    "response",
    title = "Response",
    purpose = "Answer the request",
    instructions = "Use evidence.",
    generator_id = "generator.response",
    renderer_ids = "renderer.markdown"
  )

  expect_error(
    tempest_artifact(spec),
    class = "tempest_workflow_spec_error"
  )
  expect_error(
    tempest_artifact(
      spec,
      content = "Body",
      validation_results = list("not a result")
    ),
    class = "tempest_workflow_spec_error"
  )
  expect_error(
    tempest_artifact(spec, content = "Body", status = "ready"),
    regexp = "arg"
  )
})
