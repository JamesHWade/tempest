test_that("run preflights every operation before executing any step", {
  objective <- tempest_objective(
    "Produce an outcome",
    objective_id = "objective-1",
    created_at = "2026-07-18 UTC"
  )
  calls <- 0L
  registry <- tempest_operation_registry(list(
    first = list(
      kind = "step",
      implementation = function() calls <<- calls + 1L
    )
  ))
  workflow <- tempest_workflow_spec(
    "test-workflow",
    title = "Test workflow",
    purpose = "Exercise generic execution",
    steps = list(
      tempest_workflow_step(
        "first",
        title = "First",
        purpose = "First",
        operation_id = "first"
      ),
      tempest_workflow_step(
        "second",
        title = "Second",
        purpose = "Second",
        operation_id = "missing",
        dependency_ids = "first"
      )
    )
  )

  expect_error(
    tempest_run_workflow(objective, workflow, registry),
    class = "tempest_run_preflight_error"
  )
  expect_identical(calls, 0L)
})

test_that("execution errors retain the inspectable failed run", {
  registry <- tempest_operation_registry(list(
    fail = list(
      kind = "step",
      implementation = function() stop("operation failed")
    )
  ))
  workflow <- tempest_workflow_spec(
    "failing-workflow",
    title = "Failing workflow",
    purpose = "Exercise failed-run inspection",
    steps = list(tempest_workflow_step(
      "fail",
      title = "Fail",
      purpose = "Fail during execution",
      operation_id = "fail"
    ))
  )

  condition <- rlang::catch_cnd(tempest_run_workflow(
    tempest_objective("Exercise failure inspection"),
    workflow,
    registry,
    run_id = "failed-run"
  ))

  expect_s3_class(condition, "tempest_step_execution_error")
  expect_r6_class(condition$run, "TempestRun")
  expect_identical(condition$run_id, "failed-run")
  expect_identical(condition$run$status, "failed")
  expect_identical(
    tail(condition$run$events, 1L)[[1]]$event_type,
    "workflow.failed"
  )
})

test_that("run preflights requested deliverables and input resources", {
  deliverable <- tempest_deliverable_spec(
    "available-output",
    title = "Available output",
    purpose = "Represent the outcome",
    instructions = "Return the outcome.",
    generator_id = "generate",
    renderer_ids = "render"
  )
  registry <- tempest_operation_registry(list(
    complete = list(
      kind = "step",
      implementation = function(run_id, step) {
        tempest_artifact(
          deliverable,
          content = "complete",
          artifact_id = "available-output-1",
          run_id = run_id,
          step_id = step@step_id,
          status = "valid"
        )
      }
    ),
    generate = list(
      kind = "generator",
      implementation = function() "content"
    ),
    render = list(
      kind = "renderer",
      implementation = function(content) content
    )
  ))
  workflow <- tempest_workflow_spec(
    "input-contract-workflow",
    title = "Input contract workflow",
    purpose = "Exercise objective references",
    steps = list(tempest_workflow_step(
      "complete",
      title = "Complete",
      purpose = "Complete the work",
      operation_id = "complete",
      produced_artifact_ids = "available-output-1"
    ))
  )

  expect_error(
    tempest_run_workflow(
      tempest_objective(
        "Produce an outcome",
        deliverable_ids = "missing-output"
      ),
      workflow,
      registry,
      deliverables = list(deliverable)
    ),
    class = "tempest_run_preflight_error"
  )
  expect_error(
    tempest_run_workflow(
      tempest_objective(
        "Use an input",
        input_resource_ids = "resource.missing"
      ),
      workflow,
      registry
    ),
    class = "tempest_run_preflight_error"
  )

  store <- SourceStore$new()
  resource <- tempest_resource(
    resource_kind = "host.document",
    locator = "documents/approved",
    title = "Approved input",
    media_type = "text/plain",
    content = "Approved context",
    resource_id = "resource.approved"
  )
  store$upsert_resource(resource)
  run <- tempest_run_workflow(
    tempest_objective(
      "Use an input",
      input_resource_ids = resource@resource_id,
      deliverable_ids = deliverable@deliverable_id
    ),
    workflow,
    registry,
    deliverables = list(deliverable),
    source_store = store
  )

  expect_equal(run$status, "succeeded")
})

test_that("run execution is deterministic with attempts and ordered events", {
  objective <- tempest_objective(
    "Produce an outcome",
    objective_id = "objective-1",
    created_at = "2026-07-18 UTC"
  )
  order <- character()
  attempts <- 0L
  registry <- tempest_operation_registry(list(
    alpha = list(
      kind = "step",
      implementation = function(step) {
        order <<- c(order, step@step_id)
        "alpha"
      }
    ),
    beta = list(
      kind = "step",
      implementation = function(step) {
        attempts <<- attempts + 1L
        if (attempts == 1L) {
          stop("retry")
        }
        order <<- c(order, step@step_id)
        "beta"
      }
    )
  ))
  workflow <- tempest_workflow_spec(
    "test-workflow",
    title = "Test workflow",
    purpose = "Exercise generic execution",
    steps = list(
      tempest_workflow_step(
        "beta",
        title = "Beta",
        purpose = "Beta",
        operation_id = "beta",
        dependency_ids = "alpha",
        retry_policy = list(max_attempts = 2L)
      ),
      tempest_workflow_step(
        "alpha",
        title = "Alpha",
        purpose = "Alpha",
        operation_id = "alpha"
      )
    )
  )

  run <- tempest_run_workflow(
    objective,
    workflow,
    registry,
    run_id = "run-1"
  )

  expect_identical(run$status, "succeeded")
  expect_identical(order, c("alpha", "beta"))
  expect_length(run$step_states$beta$attempts, 2L)
  expect_identical(
    vapply(run$events, \(event) event$sequence, integer(1)),
    seq_along(run$events)
  )
})

test_that("exact experts and approvals are nonblocking and resumable", {
  objective <- tempest_objective(
    "Produce an outcome",
    objective_id = "objective-1",
    created_at = "2026-07-18 UTC"
  )
  calls <- 0L
  registry <- tempest_operation_registry(list(
    act = list(
      kind = "step",
      implementation = function(expert_ids) {
        calls <<- calls + 1L
        expect_identical(expert_ids, "expert.one")
      }
    )
  ))
  expert <- tempest_expert(
    "expert.one",
    name = "One",
    title = "Expert",
    description = "An expert",
    instructions = "Work carefully."
  )
  workflow <- tempest_workflow_spec(
    "test-workflow",
    title = "Test workflow",
    purpose = "Exercise generic execution",
    steps = list(tempest_workflow_step(
      "act",
      title = "Act",
      purpose = "Act",
      operation_id = "act",
      assignment_rule = "expert.one",
      side_effecting = TRUE
    ))
  )

  run <- tempest_run_workflow(
    objective,
    workflow,
    registry,
    experts = list(expert)
  )

  expect_identical(run$status, "awaiting_approval")
  expect_identical(calls, 0L)
  approval_id <- names(run$approvals)[[1]]
  run$record_approval(approval_id, "approved")
  run$resume()
  expect_identical(run$status, "succeeded")
  expect_identical(calls, 1L)
})

test_that("cooperative cancellation reaches operations and run state", {
  objective <- tempest_objective(
    "Produce an outcome",
    objective_id = "objective-1",
    created_at = "2026-07-18 UTC"
  )
  registry <- tempest_operation_registry(list(
    cancel = list(
      kind = "step",
      implementation = function(cancel_token) {
        cancel_token$request("Stop after this operation.")
      }
    ),
    never = list(
      kind = "step",
      implementation = function() stop("must not execute")
    )
  ))
  workflow <- tempest_workflow_spec(
    "test-workflow",
    title = "Test workflow",
    purpose = "Exercise generic execution",
    steps = list(
      tempest_workflow_step(
        "cancel",
        title = "Cancel",
        purpose = "Cancel",
        operation_id = "cancel"
      ),
      tempest_workflow_step(
        "never",
        title = "Never",
        purpose = "Never",
        operation_id = "never",
        dependency_ids = "cancel"
      )
    )
  )

  run <- tempest_run_workflow(objective, workflow, registry)
  expect_identical(run$status, "cancelled")
  expect_identical(run$step_states$never$status, "cancelled")
})

test_that("run snapshots exclude runtime and restore with explicit bindings", {
  objective <- tempest_objective(
    "Produce an outcome",
    objective_id = "objective-1",
    created_at = "2026-07-18 UTC"
  )
  registry <- tempest_operation_registry(list(
    wait = list(kind = "step", implementation = function() "done")
  ))
  workflow <- tempest_workflow_spec(
    "test-workflow",
    title = "Test workflow",
    purpose = "Exercise generic execution",
    steps = list(tempest_workflow_step(
      "wait",
      title = "Wait",
      purpose = "Wait",
      operation_id = "wait",
      approval_checkpoint = TRUE
    ))
  )
  runtime_client <- new.env(parent = emptyenv())
  run <- tempest_run_workflow(
    objective,
    workflow,
    registry,
    runtime_context = list(client = runtime_client)
  )
  snapshot <- tempest_run_snapshot(run)

  expect_null(snapshot$runtime)
  expect_null(snapshot$runtime_context)
  restored <- tempest_run_restore(
    snapshot,
    runtime = registry,
    runtime_context = list(client = runtime_client)
  )
  expect_identical(restored$runtime_context$client, runtime_client)
  expect_identical(restored$status, "awaiting_approval")
  expect_identical(restored$events, snapshot$events)
  expect_identical(restored$approvals, snapshot$approvals)
  expect_identical(
    tempest_workflow_fingerprint(restored$workflow),
    tempest_workflow_fingerprint(workflow)
  )

  snapshot$workflow$purpose <- "tampered"
  expect_error(
    tempest_run_restore(snapshot, runtime = registry),
    class = "tempest_workflow_definition_error"
  )

  unsafe <- tempest_run_snapshot(run)
  unsafe$capability_grants <- list(access_token = "must-not-restore")
  expect_error(
    tempest_run_restore(unsafe, runtime = registry),
    class = "tempest_run_restore_error"
  )
})

test_that("run connection permissions are durable opaque allow-lists", {
  capability_calls <- 0L
  host_service <- new.env(parent = emptyenv())
  operations <- tempest_operation_registry(list(
    act = list(
      kind = "step",
      implementation = function(
        expert_resolutions,
        capability_resolution,
        service
      ) {
        expect_identical(service, host_service)
        expect_length(expert_resolutions, 1L)
        expect_equal(
          expert_resolutions[[1]]$grants$documents.search$status,
          "granted"
        )
        expect_equal(
          capability_resolution$grants$documents.search$status,
          "granted"
        )
        "complete"
      }
    )
  ))
  connection <- tempest_connection_ref(
    "connection.customer-documents",
    provider_id = "test.host",
    connection_type = "document-search",
    title = "Customer documents",
    description = "Approved customer documents"
  )
  capability <- tempest_capability_spec(
    "documents.search",
    purpose = "Search approved documents",
    instructions = "Use only the approved connection.",
    operation_id = "capability.documents.search",
    connection_ref_ids = "connection.customer-documents",
    model_roles = "expert"
  )
  runtime <- tempest_runtime(
    operations = operations,
    capability_specs = list(capability),
    capability_implementations = list(
      "documents.search" = function(
        capability_spec,
        connections,
        context
      ) {
        capability_calls <<- capability_calls + 1L
        expect_named(connections, "connection.customer-documents")
        expect_identical(context$service, host_service)
        list(
          tools = list(),
          metadata = list(provider = "test", access = "read")
        )
      }
    ),
    connection_refs = list(connection),
    connection_bindings = list(
      "connection.customer-documents" = list(
        access_token = "must-never-be-persisted"
      )
    ),
    include_builtins = FALSE
  )
  expert <- tempest_expert(
    "expert.one",
    name = "One",
    title = "Expert",
    description = "Customer document specialist",
    instructions = "Use approved documents.",
    required_capability_ids = "documents.search"
  )
  objective <- tempest_objective(
    "Produce an outcome",
    objective_id = "objective-connection",
    created_at = "2026-07-18 UTC"
  )
  workflow <- tempest_workflow_spec(
    "connection-workflow",
    title = "Connection workflow",
    purpose = "Exercise scoped connections",
    steps = list(tempest_workflow_step(
      "act",
      title = "Act",
      purpose = "Use the approved connection",
      operation_id = "act",
      assignment_rule = "expert.one",
      required_capability_ids = "documents.search"
    ))
  )

  run <- tempest_run_workflow(
    objective,
    workflow,
    runtime,
    experts = list(expert),
    runtime_context = list(service = host_service),
    connection_permissions = list(
      "expert.one" = "connection.customer-documents",
      expert = "connection.customer-documents"
    )
  )

  expect_equal(run$status, "succeeded")
  expect_equal(capability_calls, 2L)
  expect_equal(
    run$connection_permissions$expert.one,
    "connection.customer-documents"
  )
  grants <- tempest_run_capability_grants(run)
  expect_equal(
    grants$act$experts$expert.one$documents.search$status,
    "granted"
  )
  expect_equal(grants$act$step$documents.search$status, "granted")
  snapshot <- tempest_run_snapshot(run)
  expect_null(snapshot$runtime_context)
  expect_identical(snapshot$capability_grants, grants)
  expect_equal(
    snapshot$capability_grants$act$step$documents.search$status,
    "granted"
  )
  encoded <- tempest:::tempest_canonical_json(snapshot)
  expect_match(encoded, "connection.customer-documents", fixed = TRUE)
  expect_no_match(encoded, "must-never-be-persisted", fixed = TRUE)
  expect_no_match(encoded, "access_token", fixed = TRUE)

  restored <- tempest_run_restore(snapshot, runtime = runtime)
  expect_identical(restored$runtime, runtime)
  expect_equal(
    restored$connection_permissions,
    run$connection_permissions
  )
  bundle <- file.path(withr::local_tempdir(), "connection-run")
  tempest_run_save(run, bundle)
  persisted <- paste(
    readLines(file.path(bundle, "snapshot.json"), warn = FALSE),
    collapse = "\n"
  )
  expect_match(persisted, "connection.customer-documents", fixed = TRUE)
  expect_no_match(persisted, "must-never-be-persisted", fixed = TRUE)
  expect_no_match(persisted, "access_token", fixed = TRUE)
  resumed <- tempest_run_resume(bundle, runtime = runtime)
  expect_equal(
    resumed$connection_permissions,
    run$connection_permissions
  )
  expect_identical(tempest_run_capability_grants(resumed), grants)
  expect_error(
    tempest_run_workflow(
      objective,
      workflow,
      runtime,
      experts = list(expert),
      connection_permissions = list(
        "expert.one" = "connection.unknown"
      )
    ),
    class = "tempest_run_preflight_error"
  )
})

test_that("step capabilities require a common assigned model role", {
  factory_calls <- 0L
  operation_calls <- 0L
  operations <- tempest_operation_registry(list(
    act = list(
      kind = "step",
      implementation = function(capability_resolution) {
        operation_calls <<- operation_calls + 1L
        expect_equal(
          capability_resolution$grants$documents.search$status,
          "granted"
        )
        "complete"
      }
    )
  ))
  connection <- tempest_connection_ref(
    "connection.shared-documents",
    provider_id = "test.host",
    connection_type = "document-search",
    title = "Shared documents",
    description = "Approved shared documents"
  )
  capability <- tempest_capability_spec(
    "documents.search",
    purpose = "Search approved documents",
    instructions = "Use only the shared connection.",
    operation_id = "capability.documents.search",
    connection_ref_ids = connection@connection_id,
    model_roles = "expert"
  )
  runtime <- tempest_runtime(
    operations = operations,
    capability_specs = list(capability),
    capability_implementations = list(
      "documents.search" = function(connections, ...) {
        factory_calls <<- factory_calls + 1L
        expect_named(connections, connection@connection_id)
        list(tools = list())
      }
    ),
    connection_refs = list(connection),
    connection_bindings = list(
      "connection.shared-documents" = list(scope = "read")
    ),
    include_builtins = FALSE
  )
  make_expert <- function(expert_id, model_role) {
    tempest_expert(
      expert_id,
      name = expert_id,
      title = "Expert",
      description = "Exercises collective capability scope.",
      instructions = "Use only granted capabilities.",
      model_role = model_role
    )
  }
  make_workflow <- function(
    required_capability_ids = character(),
    optional_capability_ids = character()
  ) {
    tempest_workflow_spec(
      "collective-capability-workflow",
      title = "Collective capability workflow",
      purpose = "Exercise shared step capability scope",
      steps = list(tempest_workflow_step(
        "act",
        title = "Act",
        purpose = "Use a shared capability",
        operation_id = "act",
        assignment_rule = list(type = "all"),
        required_capability_ids = required_capability_ids,
        optional_capability_ids = optional_capability_ids
      ))
    )
  }
  objective <- tempest_objective("Exercise shared capability scope")
  mixed_roles <- list(
    make_expert("expert.writer", "writer"),
    make_expert("expert.researcher", "expert")
  )

  expect_error(
    tempest_run_workflow(
      objective,
      make_workflow(required_capability_ids = "documents.search"),
      runtime,
      experts = mixed_roles,
      connection_permissions = list(
        expert = connection@connection_id,
        writer = connection@connection_id
      )
    ),
    class = "tempest_run_preflight_error"
  )
  expect_error(
    tempest_run_workflow(
      objective,
      make_workflow(optional_capability_ids = "documents.search"),
      runtime,
      experts = rev(mixed_roles),
      connection_permissions = list(
        expert = connection@connection_id,
        writer = connection@connection_id
      )
    ),
    class = "tempest_run_preflight_error"
  )
  policy_expert <- tempest_expert(
    "expert.policy",
    name = "Policy expert",
    title = "Expert",
    description = "Exercises a host-managed model policy.",
    instructions = "Use only granted capabilities.",
    model_role = NA_character_,
    model_policy_ref = "policy.customer-research"
  )
  expect_error(
    tempest_run_workflow(
      objective,
      make_workflow(required_capability_ids = "documents.search"),
      runtime,
      experts = list(policy_expert),
      connection_permissions = list(
        "expert.policy" = connection@connection_id
      )
    ),
    class = "tempest_run_preflight_error"
  )
  expect_identical(factory_calls, 0L)
  expect_identical(operation_calls, 0L)

  common_role <- list(
    make_expert("expert.two", "expert"),
    make_expert("expert.one", "expert")
  )
  run <- tempest_run_workflow(
    objective,
    make_workflow(required_capability_ids = "documents.search"),
    runtime,
    experts = common_role,
    connection_permissions = list(
      expert = connection@connection_id
    )
  )

  expect_identical(run$status, "succeeded")
  expect_identical(factory_calls, 1L)
  expect_identical(operation_calls, 1L)
})

test_that("side-effecting capabilities require approval before resolution", {
  factory_calls <- 0L
  operation_calls <- 0L
  factory_cancel_token <- NULL
  operations <- tempest_operation_registry(list(
    act = list(
      kind = "step",
      implementation = function(capability_resolution) {
        operation_calls <<- operation_calls + 1L
        expect_equal(
          capability_resolution$grants$customer.write$status,
          "granted"
        )
        "complete"
      }
    )
  ))
  capability <- tempest_capability_spec(
    "customer.write",
    purpose = "Update a customer record",
    instructions = "Only update the approved record.",
    operation_id = "capability.customer.write",
    side_effecting = TRUE
  )
  runtime <- tempest_runtime(
    operations = operations,
    capability_specs = list(capability),
    capability_implementations = list(
      "customer.write" = function(
        capability_spec,
        connections,
        context
      ) {
        factory_calls <<- factory_calls + 1L
        factory_cancel_token <<- context$cancel_token
        list(tools = list(), metadata = list())
      }
    ),
    include_builtins = FALSE
  )
  objective <- tempest_objective(
    "Update the customer record",
    objective_id = "objective-side-effect",
    created_at = "2026-07-18 UTC"
  )
  workflow <- tempest_workflow_spec(
    "side-effect-workflow",
    title = "Side-effect workflow",
    purpose = "Exercise capability policy gates",
    steps = list(tempest_workflow_step(
      "act",
      title = "Act",
      purpose = "Update the record",
      operation_id = "act",
      required_capability_ids = "customer.write"
    ))
  )

  run <- tempest_run_workflow(objective, workflow, runtime)

  expect_equal(run$status, "awaiting_approval")
  expect_identical(factory_calls, 0L)
  expect_identical(operation_calls, 0L)
  expect_equal(
    run$policy_decisions[[1]]$side_effecting_capability_ids,
    "customer.write"
  )
  approval_id <- names(run$approvals)[[1]]
  run$record_approval(approval_id, "approved")
  run$resume()

  expect_equal(run$status, "succeeded")
  expect_identical(factory_calls, 1L)
  expect_identical(operation_calls, 1L)
  expect_identical(factory_cancel_token, run$cancel_token)
})

test_that("a denied capability policy prevents all execution", {
  factory_calls <- 0L
  operation_calls <- 0L
  policy_calls <- 0L
  operations <- tempest_operation_registry(list(
    act = list(
      kind = "step",
      implementation = function() {
        operation_calls <<- operation_calls + 1L
      }
    )
  ))
  capability <- tempest_capability_spec(
    "customer.write",
    purpose = "Update a customer record",
    instructions = "Only update the approved record.",
    operation_id = "capability.customer.write",
    side_effecting = TRUE
  )
  runtime <- tempest_runtime(
    operations = operations,
    capability_specs = list(capability),
    capability_implementations = list(
      "customer.write" = function(
        capability_spec,
        connections,
        context
      ) {
        factory_calls <<- factory_calls + 1L
        list(tools = list(), metadata = list())
      }
    ),
    include_builtins = FALSE
  )
  objective <- tempest_objective(
    "Update the customer record",
    objective_id = "objective-policy-denied",
    created_at = "2026-07-18 UTC"
  )
  workflow <- tempest_workflow_spec(
    "policy-denied-workflow",
    title = "Policy-denied workflow",
    purpose = "Exercise policy denial",
    steps = list(tempest_workflow_step(
      "act",
      title = "Act",
      purpose = "Update the record",
      operation_id = "act",
      required_capability_ids = "customer.write"
    ))
  )
  policy <- function(side_effecting_capability_ids) {
    policy_calls <<- policy_calls + 1L
    expect_equal(side_effecting_capability_ids, "customer.write")
    list(decision = "deny", reason = "The host denied this write.")
  }
  run <- TempestRun$new(
    objective,
    workflow,
    runtime,
    policy_adapter = policy
  )

  expect_error(
    run$resume(),
    class = "tempest_policy_denied_error"
  )
  expect_equal(run$status, "failed")
  expect_identical(policy_calls, 1L)
  expect_identical(factory_calls, 0L)
  expect_identical(operation_calls, 0L)
})

test_that("artifact approval resumes without rerunning its producer", {
  producer_calls <- 0L
  exporter_calls <- 0L
  deliverable <- tempest_deliverable_spec(
    "customer-response",
    title = "Customer response",
    purpose = "Resolve the customer request",
    instructions = "Provide a concise response.",
    generator_id = "generate",
    renderer_ids = "render",
    exporter_ids = "export",
    requires_approval = TRUE
  )
  registry <- tempest_operation_registry(list(
    generate = list(
      kind = "generator",
      implementation = function() {
        producer_calls <<- producer_calls + 1L
        "The requested outcome"
      }
    ),
    render = list(
      kind = "renderer",
      implementation = function(content) content
    ),
    export = list(
      kind = "exporter",
      implementation = function(artifact, runtime) {
        exporter_calls <<- exporter_calls + 1L
        expect_equal(runtime$destination, "approved-output")
        artifact@metadata <- utils::modifyList(
          artifact@metadata,
          list(exported_to = runtime$destination)
        )
        artifact
      }
    ),
    produce = list(
      kind = "step",
      implementation = function(artifact_catalog, run_id, step) {
        tempest_generate_deliverable(
          deliverable,
          registry = registry,
          catalog = artifact_catalog,
          provenance = list(
            artifact_id = "customer-response-1",
            run_id = run_id,
            step_id = step@step_id
          )
        )
      }
    )
  ))
  objective <- tempest_objective(
    "Resolve the request",
    objective_id = "objective-artifact-approval",
    created_at = "2026-07-18 UTC"
  )
  workflow <- tempest_workflow_spec(
    "artifact-approval-workflow",
    title = "Artifact approval workflow",
    purpose = "Exercise post-generation approval",
    steps = list(tempest_workflow_step(
      "produce",
      title = "Produce",
      purpose = "Create the customer response",
      operation_id = "produce",
      produced_artifact_ids = "customer-response-1"
    ))
  )

  run <- tempest_run_workflow(
    objective,
    workflow,
    registry,
    deliverables = list(deliverable),
    runtime_context = list(destination = "approved-output")
  )

  expect_equal(run$status, "awaiting_approval")
  expect_identical(producer_calls, 1L)
  expect_identical(exporter_calls, 0L)
  expect_length(run$step_states$produce$attempts, 1L)
  expect_equal(
    run$artifact("customer-response-1")@status,
    "awaiting_approval"
  )
  approval_id <- names(Filter(
    \(approval) identical(approval$approval_kind, "artifact"),
    run$approvals
  ))[[1]]
  run$record_approval(approval_id, "approved")
  run$resume()

  expect_equal(run$status, "succeeded")
  expect_identical(producer_calls, 1L)
  expect_identical(exporter_calls, 1L)
  expect_length(run$step_states$produce$attempts, 1L)
  expect_equal(run$artifact("customer-response-1")@status, "approved")
  expect_equal(
    run$artifact("customer-response-1")@metadata$exported_to,
    "approved-output"
  )
})

test_that("approved exporters cannot rewrite reviewed artifact records", {
  deliverable <- tempest_deliverable_spec(
    "reviewed-response",
    title = "Reviewed response",
    purpose = "Preserve the approved response",
    instructions = "Return the reviewed response unchanged.",
    generator_id = "generate",
    validator_ids = "validate",
    renderer_ids = "render",
    exporter_ids = "export",
    requires_approval = TRUE
  )
  registry <- tempest_operation_registry(list(
    generate = list(
      kind = "generator",
      implementation = function() "Reviewed content"
    ),
    validate = list(
      kind = "validator",
      implementation = function() {
        tempest_validation_result(
          "validate",
          message = "The response is ready for review."
        )
      }
    ),
    render = list(
      kind = "renderer",
      implementation = function(content) content
    ),
    export = list(
      kind = "exporter",
      implementation = function(artifact) {
        artifact@content <- "Changed after approval"
        artifact
      }
    ),
    produce = list(
      kind = "step",
      implementation = function(artifact_catalog, run_id, step) {
        tempest_generate_deliverable(
          deliverable,
          registry = registry,
          catalog = artifact_catalog,
          provenance = list(
            artifact_id = "reviewed-response-1",
            run_id = run_id,
            step_id = step@step_id
          )
        )
      }
    )
  ))
  workflow <- tempest_workflow_spec(
    "reviewed-response-workflow",
    title = "Reviewed response workflow",
    purpose = "Exercise immutable approved exports",
    steps = list(tempest_workflow_step(
      "produce",
      title = "Produce",
      purpose = "Produce the reviewed response",
      operation_id = "produce",
      produced_artifact_ids = "reviewed-response-1"
    ))
  )
  run <- tempest_run_workflow(
    tempest_objective(
      "Produce a reviewed response",
      deliverable_ids = deliverable@deliverable_id
    ),
    workflow,
    registry,
    deliverables = list(deliverable)
  )
  approval_id <- names(tempest_run_approvals(run, "pending"))[[1]]
  before <- tempest:::tempest_artifact_data(
    run$artifact("reviewed-response-1")
  )

  expect_error(
    run$record_approval(approval_id, "approved"),
    class = "tempest_approval_error"
  )
  expect_identical(
    tempest:::tempest_artifact_data(run$artifact("reviewed-response-1")),
    before
  )
  expect_equal(run$approvals[[approval_id]]$status, "pending")
  expect_equal(run$status, "awaiting_approval")
})

test_that("catalog-resident declared artifacts enter the approval lifecycle", {
  deliverable <- tempest_deliverable_spec(
    "catalog-response",
    title = "Catalog response",
    purpose = "Approve catalog-resident output",
    instructions = "Return the approved response.",
    generator_id = "generate",
    renderer_ids = "render",
    requires_approval = TRUE
  )
  registry <- tempest_operation_registry(list(
    generate = list(
      kind = "generator",
      implementation = function() "unused"
    ),
    render = list(
      kind = "renderer",
      implementation = function(content) content
    ),
    produce = list(
      kind = "step",
      implementation = function(artifact_catalog, run_id, step) {
        artifact_catalog$add(tempest_artifact(
          deliverable,
          content = "Catalog supplied output",
          artifact_id = "catalog-response-1",
          run_id = run_id,
          step_id = step@step_id,
          status = "valid"
        ))
        NULL
      }
    )
  ))
  workflow <- tempest_workflow_spec(
    "catalog-response-workflow",
    title = "Catalog response workflow",
    purpose = "Exercise approval over declared catalog output",
    steps = list(tempest_workflow_step(
      "produce",
      title = "Produce",
      purpose = "Publish the catalog response",
      operation_id = "produce",
      produced_artifact_ids = "catalog-response-1"
    ))
  )
  run <- tempest_run_workflow(
    tempest_objective(
      "Produce a catalog response",
      deliverable_ids = deliverable@deliverable_id
    ),
    workflow,
    registry,
    deliverables = list(deliverable)
  )

  expect_equal(run$status, "awaiting_approval")
  expect_equal(
    run$artifact("catalog-response-1")@status,
    "awaiting_approval"
  )
  expect_length(tempest_run_approvals(run, "pending"), 1L)
  publication_events <- Filter(
    \(event) identical(event$event_type, "artifact.published"),
    run$events
  )
  expect_length(publication_events, 1L)

  approval_id <- names(tempest_run_approvals(run, "pending"))[[1]]
  run$record_approval(approval_id, "approved")
  run$resume()

  expect_equal(run$artifact("catalog-response-1")@status, "approved")
  expect_equal(run$status, "succeeded")
})

test_that("run enforces deliverable approval on custom artifacts", {
  deliverable <- tempest_deliverable_spec(
    "host-output",
    title = "Host output",
    purpose = "Exercise approval enforcement",
    instructions = "Return the requested output.",
    generator_id = "generate",
    renderer_ids = "render",
    requires_approval = TRUE
  )
  registry <- tempest_operation_registry(list(
    generate = list(
      kind = "generator",
      implementation = function() "unused"
    ),
    render = list(
      kind = "renderer",
      implementation = function(content) content
    ),
    produce = list(
      kind = "step",
      implementation = function(run_id, step) {
        tempest_artifact(
          deliverable,
          content = "Host supplied output",
          artifact_id = "host-output-1",
          run_id = run_id,
          step_id = step@step_id,
          status = "valid"
        )
      }
    )
  ))
  workflow <- tempest_workflow_spec(
    "host-output-workflow",
    title = "Host output workflow",
    purpose = "Exercise approval enforcement",
    steps = list(tempest_workflow_step(
      "produce",
      title = "Produce",
      purpose = "Produce the output",
      operation_id = "produce",
      produced_artifact_ids = "host-output-1"
    ))
  )
  run <- tempest_run_workflow(
    tempest_objective(
      "Produce an output",
      deliverable_ids = deliverable@deliverable_id
    ),
    workflow,
    registry,
    deliverables = list(deliverable)
  )

  expect_equal(run$status, "awaiting_approval")
  expect_equal(run$artifact("host-output-1")@status, "awaiting_approval")
  approval_id <- names(tempest_run_approvals(run, "pending"))[[1]]
  run$record_approval(
    approval_id,
    "rejected",
    note = "The output needs material changes."
  )

  expect_equal(run$status, "failed")
  expect_equal(run$artifact("host-output-1")@status, "rejected")
  expect_equal(
    tail(
      vapply(run$events, \(event) event$event_type, character(1)),
      3L
    ),
    c("approval.resolved", "step.failed", "workflow.failed")
  )
})

test_that("requested deliverables are a completion contract", {
  deliverable <- tempest_deliverable_spec(
    "required-output",
    title = "Required output",
    purpose = "Exercise workflow completion",
    instructions = "Return the requested output.",
    generator_id = "generate",
    renderer_ids = "render"
  )
  registry <- tempest_operation_registry(list(
    generate = list(
      kind = "generator",
      implementation = function() "unused"
    ),
    render = list(
      kind = "renderer",
      implementation = function(content) content
    ),
    omit = list(
      kind = "step",
      implementation = function() "No artifact"
    )
  ))
  workflow <- tempest_workflow_spec(
    "missing-output-workflow",
    title = "Missing output workflow",
    purpose = "Exercise workflow completion",
    steps = list(tempest_workflow_step(
      "omit",
      title = "Omit",
      purpose = "Omit the output",
      operation_id = "omit"
    ))
  )
  run <- TempestRun$new(
    tempest_objective(
      "Produce an output",
      deliverable_ids = deliverable@deliverable_id
    ),
    workflow,
    registry,
    deliverables = list(deliverable)
  )

  expect_error(
    run$resume(),
    class = "tempest_run_completion_error"
  )
  expect_equal(run$status, "failed")
  expect_equal(tail(run$events, 1L)[[1]]$event_type, "workflow.failed")
})

test_that("requested artifacts must be owned and published by the run", {
  deliverable <- tempest_deliverable_spec(
    "required-output",
    title = "Required output",
    purpose = "Exercise run-owned publication",
    instructions = "Return the requested output.",
    generator_id = "generate",
    renderer_ids = "render"
  )
  stale <- tempest_artifact(
    deliverable,
    content = "Output from an earlier run",
    artifact_id = "required-output-1",
    run_id = "earlier-run",
    step_id = "produce",
    status = "valid"
  )
  catalog <- tempest_artifact_catalog(
    artifacts = list(stale),
    deliverables = list(deliverable)
  )
  registry <- tempest_operation_registry(list(
    generate = list(
      kind = "generator",
      implementation = function() "unused"
    ),
    render = list(
      kind = "renderer",
      implementation = function(content) content
    ),
    omit = list(
      kind = "step",
      implementation = function() NULL
    )
  ))
  objective <- tempest_objective(
    "Produce an output",
    deliverable_ids = deliverable@deliverable_id
  )
  unpublished_workflow <- tempest_workflow_spec(
    "unpublished-output-workflow",
    title = "Unpublished output workflow",
    purpose = "Exercise publication ownership",
    steps = list(tempest_workflow_step(
      "produce",
      title = "Produce",
      purpose = "Publish no output",
      operation_id = "omit"
    ))
  )
  unpublished_run <- TempestRun$new(
    objective,
    unpublished_workflow,
    registry,
    deliverables = list(deliverable),
    artifact_catalog = catalog,
    run_id = "current-run-unpublished"
  )

  expect_error(
    unpublished_run$resume(),
    class = "tempest_run_completion_error"
  )
  expect_equal(unpublished_run$status, "failed")

  foreign_workflow <- tempest_workflow_spec(
    "foreign-output-workflow",
    title = "Foreign output workflow",
    purpose = "Exercise run ownership",
    steps = list(tempest_workflow_step(
      "produce",
      title = "Produce",
      purpose = "Declare a foreign output",
      operation_id = "omit",
      produced_artifact_ids = "required-output-1"
    ))
  )
  foreign_run <- TempestRun$new(
    objective,
    foreign_workflow,
    registry,
    deliverables = list(deliverable),
    artifact_catalog = catalog,
    run_id = "current-run-foreign"
  )

  expect_error(
    foreign_run$resume(),
    class = "tempest_step_execution_error"
  )
  expect_equal(foreign_run$status, "failed")
  expect_equal(
    foreign_run$step_states$produce$error$class,
    "tempest_step_output_error"
  )
})

test_that("artifact approval remains retryable after a store failure", {
  fail_once <- TRUE
  store <- tempest_artifact_store(
    write = function(artifact) {
      if (
        identical(artifact@artifact_id, "approval-output-2") &&
          identical(artifact@status, "approved") &&
          fail_once
      ) {
        fail_once <<- FALSE
        stop("temporary store failure")
      }
      invisible(artifact@artifact_id)
    }
  )
  deliverable <- tempest_deliverable_spec(
    "approval-output",
    title = "Approval output",
    purpose = "Exercise retryable approval",
    instructions = "Return both representations.",
    generator_id = "generate",
    renderer_ids = "render",
    requires_approval = TRUE
  )
  catalog <- tempest_artifact_catalog(
    store = store,
    deliverables = list(deliverable)
  )
  registry <- tempest_operation_registry(list(
    generate = list(
      kind = "generator",
      implementation = function() "unused"
    ),
    render = list(
      kind = "renderer",
      implementation = function(content) content
    ),
    produce = list(
      kind = "step",
      implementation = function(run_id, step) {
        list(
          artifacts = list(
            tempest_artifact(
              deliverable,
              content = "First",
              artifact_id = "approval-output-1",
              run_id = run_id,
              step_id = step@step_id,
              status = "awaiting_approval"
            ),
            tempest_artifact(
              deliverable,
              content = "Second",
              artifact_id = "approval-output-2",
              run_id = run_id,
              step_id = step@step_id,
              status = "awaiting_approval"
            )
          )
        )
      }
    )
  ))
  workflow <- tempest_workflow_spec(
    "retryable-approval-workflow",
    title = "Retryable approval workflow",
    purpose = "Exercise retryable approval",
    steps = list(tempest_workflow_step(
      "produce",
      title = "Produce",
      purpose = "Produce both representations",
      operation_id = "produce",
      produced_artifact_ids = c(
        "approval-output-1",
        "approval-output-2"
      )
    ))
  )
  run <- tempest_run_workflow(
    tempest_objective(
      "Produce both outputs",
      deliverable_ids = deliverable@deliverable_id
    ),
    workflow,
    registry,
    deliverables = list(deliverable),
    artifact_catalog = catalog
  )
  approval_id <- names(tempest_run_approvals(run, "pending"))[[1]]

  expect_error(
    run$record_approval(approval_id, "approved"),
    class = "tempest_artifact_catalog_error"
  )
  expect_equal(run$approvals[[approval_id]]$status, "pending")
  expect_equal(run$artifact("approval-output-1")@status, "approved")
  expect_equal(
    run$artifact("approval-output-2")@status,
    "awaiting_approval"
  )

  restored <- tempest_run_restore(
    tempest_run_snapshot(run),
    runtime = registry
  )
  expect_equal(restored$approvals[[approval_id]]$status, "pending")
  expect_equal(
    restored$artifact("approval-output-1")@status,
    "approved"
  )
  expect_equal(
    restored$artifact("approval-output-2")@status,
    "awaiting_approval"
  )

  restored$record_approval(approval_id, "approved")
  restored$resume()
  expect_equal(restored$approvals[[approval_id]]$status, "approved")
  expect_equal(
    restored$artifact("approval-output-2")@status,
    "approved"
  )
  expect_equal(restored$status, "succeeded")
})

test_that("validator failure preserves an invalid artifact on a failed run", {
  producer_calls <- 0L
  deliverable <- tempest_deliverable_spec(
    "customer-response",
    title = "Customer response",
    purpose = "Resolve the customer request",
    instructions = "Provide a complete response.",
    generator_id = "generate",
    validator_ids = "validate",
    renderer_ids = "render"
  )
  registry <- tempest_operation_registry(list(
    generate = list(
      kind = "generator",
      implementation = function() {
        producer_calls <<- producer_calls + 1L
        "Incomplete response"
      }
    ),
    validate = list(
      kind = "validator",
      implementation = function() {
        tempest_validation_result(
          "validate",
          status = "failed",
          message = "The required outcome is missing."
        )
      }
    ),
    render = list(
      kind = "renderer",
      implementation = function(content) content
    ),
    produce = list(
      kind = "step",
      implementation = function(artifact_catalog, run_id, step) {
        tempest_generate_deliverable(
          deliverable,
          registry = registry,
          catalog = artifact_catalog,
          provenance = list(
            artifact_id = "invalid-response-1",
            run_id = run_id,
            step_id = step@step_id
          )
        )
      }
    )
  ))
  objective <- tempest_objective(
    "Resolve the request",
    objective_id = "objective-invalid-artifact",
    created_at = "2026-07-18 UTC"
  )
  workflow <- tempest_workflow_spec(
    "invalid-artifact-workflow",
    title = "Invalid artifact workflow",
    purpose = "Exercise output validation failure",
    steps = list(tempest_workflow_step(
      "produce",
      title = "Produce",
      purpose = "Create the customer response",
      operation_id = "produce",
      produced_artifact_ids = "invalid-response-1"
    ))
  )
  run <- TempestRun$new(
    objective,
    workflow,
    registry,
    deliverables = list(deliverable)
  )

  expect_error(
    run$resume(),
    class = "tempest_step_execution_error"
  )
  expect_equal(run$status, "failed")
  expect_identical(producer_calls, 1L)
  expect_equal(run$artifact("invalid-response-1")@status, "invalid")
  expect_equal(
    run$artifact("invalid-response-1")@validation_results[[1]]@status,
    "failed"
  )
  expect_equal(run$step_states$produce$status, "failed")
})

test_that("restore rejects state substitution and gates partial recovery", {
  registry <- tempest_operation_registry(list(
    complete = list(
      kind = "step",
      implementation = function() "complete"
    ),
    generate = list(
      kind = "generator",
      implementation = function() "unused"
    ),
    render = list(
      kind = "renderer",
      implementation = function(content) content
    )
  ))
  deliverable <- tempest_deliverable_spec(
    "registered-output",
    title = "Registered output",
    purpose = "Exercise catalog restoration",
    instructions = "Remain registered.",
    generator_id = "generate",
    renderer_ids = "render"
  )
  store <- SourceStore$new()
  store$upsert_source(tempest:::tempest_source(
    "https://example.com/restored-source",
    title = "Restored source"
  ))
  objective <- tempest_objective(
    "Complete the workflow",
    objective_id = "objective-restore-state",
    created_at = "2026-07-18 UTC"
  )
  workflow <- tempest_workflow_spec(
    "restore-state-workflow",
    title = "Restore-state workflow",
    purpose = "Exercise strict restoration",
    steps = list(tempest_workflow_step(
      "complete",
      title = "Complete",
      purpose = "Complete the workflow",
      operation_id = "complete"
    ))
  )
  run <- tempest_run_workflow(
    objective,
    workflow,
    registry,
    deliverables = list(deliverable),
    source_store = store
  )
  snapshot <- tempest_run_snapshot(run)

  expect_error(
    tempest_run_restore(
      snapshot,
      runtime = registry,
      artifact_catalog = tempest_artifact_catalog()
    ),
    class = "tempest_run_restore_error"
  )
  expect_error(
    tempest_run_restore(
      snapshot,
      runtime = registry,
      source_store = SourceStore$new()
    ),
    class = "tempest_run_restore_error"
  )

  in_flight <- snapshot
  in_flight$status <- "running"
  in_flight$step_states$complete$status <- "running"
  expect_error(
    tempest_run_restore(in_flight, runtime = registry),
    class = "tempest_run_restore_error"
  )
  recovered <- tempest_run_restore(
    in_flight,
    runtime = registry,
    partial_recovery = TRUE
  )
  expect_equal(recovered$status, "partially_recovered")
  expect_equal(recovered$step_states$complete$status, "pending")
})

test_that("partial recovery preserves attempt numbers and retry budgets", {
  captured <- NULL
  registry <- tempest_operation_registry(list(
    work = list(
      kind = "step",
      implementation = function(run) {
        if (is.null(captured)) {
          captured <<- tempest_run_snapshot(run)
        }
        "complete"
      }
    )
  ))
  workflow <- tempest_workflow_spec(
    "partial-retry-workflow",
    title = "Partial retry workflow",
    purpose = "Exercise interrupted attempt recovery",
    steps = list(tempest_workflow_step(
      "work",
      title = "Work",
      purpose = "Complete the work",
      operation_id = "work",
      retry_policy = list(max_attempts = 2L)
    ))
  )
  tempest_run_workflow(
    tempest_objective("Complete the work"),
    workflow,
    registry
  )

  recovered <- tempest_run_restore(
    captured,
    runtime = registry,
    partial_recovery = TRUE
  )
  expect_equal(
    vapply(
      recovered$step_states$work$attempts,
      \(attempt) attempt$attempt,
      integer(1)
    ),
    1L
  )
  expect_equal(
    recovered$step_states$work$attempts[[1]]$status,
    "interrupted"
  )

  recovered$resume()
  expect_equal(recovered$status, "succeeded")
  expect_equal(
    vapply(
      recovered$step_states$work$attempts,
      \(attempt) attempt$attempt,
      integer(1)
    ),
    c(1L, 2L)
  )
  expect_equal(
    names(tempest_run_capability_grants(recovered)$work$attempts),
    c("1", "2")
  )
})

test_that("restored result snapshots remain stable across snapshots", {
  registry <- tempest_operation_registry(list(
    work = list(
      kind = "step",
      implementation = function() {
        list(z_result = "complete", a_result = "stable")
      }
    )
  ))
  workflow <- tempest_workflow_spec(
    "stable-result-workflow",
    title = "Stable result workflow",
    purpose = "Exercise stable result snapshots",
    steps = list(tempest_workflow_step(
      "work",
      title = "Work",
      purpose = "Complete the work",
      operation_id = "work"
    ))
  )
  run <- tempest_run_workflow(
    tempest_objective("Complete the work"),
    workflow,
    registry
  )
  first <- tempest_run_snapshot(run)
  restored <- tempest_run_restore(first, runtime = registry)
  second <- tempest_run_snapshot(restored)

  expect_identical(
    second$step_states$work$result,
    first$step_states$work$result
  )

  bundle <- file.path(withr::local_tempdir(), "stable-result")
  tempest_run_save(run, bundle)
  resumed <- tempest_run_resume(bundle, runtime = registry)
  third <- tempest_run_snapshot(resumed)
  expect_identical(
    third$step_states$work$result,
    first$step_states$work$result
  )
})

test_that("restore rejects malformed and forged approval state", {
  registry <- tempest_operation_registry(list(
    act = list(kind = "step", implementation = function() "complete")
  ))
  workflow <- tempest_workflow_spec(
    "restore-approval-workflow",
    title = "Restore approval workflow",
    purpose = "Exercise approval integrity",
    steps = list(tempest_workflow_step(
      "act",
      title = "Act",
      purpose = "Perform the action",
      operation_id = "act",
      side_effecting = TRUE
    ))
  )
  run <- tempest_run_workflow(
    tempest_objective("Perform the action"),
    workflow,
    registry
  )
  snapshot <- tempest_run_snapshot(run)

  malformed_status <- snapshot
  malformed_status$status <- NULL
  expect_error(
    tempest_run_restore(malformed_status, runtime = registry),
    class = "tempest_run_restore_error"
  )

  malformed_approval <- snapshot
  malformed_approval$approvals[[1]] <- "approved"
  expect_error(
    tempest_run_restore(malformed_approval, runtime = registry),
    class = "tempest_run_restore_error"
  )

  forged <- snapshot
  forged$approvals[[1]]$status <- "approved"
  forged$step_states$act$status <- "pending"
  forged$status <- "pending"
  expect_error(
    tempest_run_restore(forged, runtime = registry),
    class = "tempest_run_restore_error"
  )

  incomplete <- TempestRun$new(
    tempest_objective("Perform the action"),
    tempest_workflow_spec(
      "incomplete-workflow",
      title = "Incomplete workflow",
      purpose = "Exercise status consistency",
      steps = list(tempest_workflow_step(
        "act",
        title = "Act",
        purpose = "Perform the action",
        operation_id = "act"
      ))
    ),
    registry
  ) |>
    tempest_run_snapshot()
  incomplete$status <- "succeeded"
  expect_error(
    tempest_run_restore(incomplete, runtime = registry),
    class = "tempest_run_restore_error"
  )
})

test_that("restore binds approvals to their policy and event step", {
  registry <- tempest_operation_registry(list(
    act = list(kind = "step", implementation = function() "complete")
  ))
  workflow <- tempest_workflow_spec(
    "approval-binding-workflow",
    title = "Approval binding workflow",
    purpose = "Exercise approval provenance",
    steps = list(
      tempest_workflow_step(
        "first",
        title = "First",
        purpose = "Perform the first action",
        operation_id = "act",
        side_effecting = TRUE
      ),
      tempest_workflow_step(
        "second",
        title = "Second",
        purpose = "Perform the second action",
        operation_id = "act",
        dependency_ids = "first",
        side_effecting = TRUE
      )
    )
  )
  run <- tempest_run_workflow(
    tempest_objective("Perform both actions"),
    workflow,
    registry
  )
  snapshot <- tempest_run_snapshot(run)
  approval_id <- names(snapshot$approvals)[[1]]

  retargeted <- snapshot
  retargeted$approvals[[approval_id]]$step_id <- "second"
  policy_id <- retargeted$approvals[[approval_id]]$policy_decision_id
  policy_index <- which(vapply(
    retargeted$policy_decisions,
    \(decision) identical(decision$decision_id, policy_id),
    logical(1)
  ))
  retargeted$policy_decisions[[policy_index]]$step_id <- "second"
  retargeted$step_states$first$status <- "pending"
  retargeted$step_states$second$status <- "awaiting_approval"

  expect_error(
    tempest_run_restore(retargeted, runtime = registry),
    class = "tempest_run_restore_error"
  )

  mismatched_policy <- snapshot
  mismatched_policy$policy_decisions[[policy_index]]$step_id <- "second"
  expect_error(
    tempest_run_restore(mismatched_policy, runtime = registry),
    class = "tempest_run_restore_error"
  )
})

test_that("restore classes malformed scalar and cancellation records", {
  registry <- tempest_operation_registry(list(
    work = list(kind = "step", implementation = function() "complete")
  ))
  workflow <- tempest_workflow_spec(
    "malformed-restore-workflow",
    title = "Malformed restore workflow",
    purpose = "Exercise defensive restoration",
    steps = list(tempest_workflow_step(
      "work",
      title = "Work",
      purpose = "Complete the work",
      operation_id = "work"
    ))
  )
  snapshot <- TempestRun$new(
    tempest_objective("Complete the work"),
    workflow,
    registry
  ) |>
    tempest_run_snapshot()

  malformed <- list(
    function(value) {
      value$schema_version <- function() 2L
      value
    },
    function(value) {
      value$sequence <- function() 1L
      value
    },
    function(value) {
      value$events[[1]]$sequence <- function() 1L
      value
    },
    function(value) {
      value$cancel_token <- "requested"
      value
    }
  )
  for (mutate in malformed) {
    expect_error(
      tempest_run_restore(mutate(snapshot), runtime = registry),
      class = "tempest_run_restore_error"
    )
  }
})

test_that("restore rejects succeeded state without durable execution", {
  registry <- tempest_operation_registry(list(
    work = list(kind = "step", implementation = function() "complete")
  ))
  workflow <- tempest_workflow_spec(
    "succeeded-invariant-workflow",
    title = "Succeeded invariant workflow",
    purpose = "Exercise succeeded-state invariants",
    steps = list(tempest_workflow_step(
      "work",
      title = "Work",
      purpose = "Complete the work",
      operation_id = "work"
    ))
  )
  completed <- tempest_run_workflow(
    tempest_objective("Complete the work"),
    workflow,
    registry
  ) |>
    tempest_run_snapshot()
  completed$step_states$work$attempts <- list()

  expect_error(
    tempest_run_restore(completed, runtime = registry),
    class = "tempest_run_restore_error"
  )
})

test_that("restore validates artifacts referenced by result snapshots", {
  deliverable <- tempest_deliverable_spec(
    "dynamic-output",
    title = "Dynamic output",
    purpose = "Exercise restored result artifacts",
    instructions = "Return one dynamic output.",
    generator_id = "generate",
    renderer_ids = "render"
  )
  registry <- tempest_operation_registry(list(
    generate = list(
      kind = "generator",
      implementation = function() "unused"
    ),
    render = list(
      kind = "renderer",
      implementation = function(content) content
    ),
    produce = list(
      kind = "step",
      implementation = function(run_id, step) {
        tempest_artifact(
          deliverable,
          content = "Complete",
          artifact_id = "dynamic-output-1",
          run_id = run_id,
          step_id = step@step_id,
          status = "valid"
        )
      }
    )
  ))
  workflow <- tempest_workflow_spec(
    "dynamic-result-workflow",
    title = "Dynamic result workflow",
    purpose = "Exercise dynamic result restoration",
    steps = list(tempest_workflow_step(
      "produce",
      title = "Produce",
      purpose = "Produce a dynamic artifact",
      operation_id = "produce"
    ))
  )
  snapshot <- tempest_run_workflow(
    tempest_objective("Produce the output"),
    workflow,
    registry,
    deliverables = list(deliverable)
  ) |>
    tempest_run_snapshot()

  missing <- snapshot
  missing$artifact_catalog$artifacts[["dynamic-output-1"]] <- NULL
  expect_error(
    tempest_run_restore(missing, runtime = registry),
    class = "tempest_run_restore_error"
  )

  invalid <- snapshot
  invalid$artifact_catalog$artifacts[["dynamic-output-1"]]$status <-
    "invalid"
  expect_error(
    tempest_run_restore(invalid, runtime = registry),
    class = "tempest_run_restore_error"
  )
})
