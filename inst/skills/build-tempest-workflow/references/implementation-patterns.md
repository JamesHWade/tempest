# Tempest workflow implementation patterns

Use current package documentation as the source of truth. The patterns below
show the intended construction order and boundaries.

## Construction order

1. `tempest_objective()` and approved `tempest_resource()` inputs.
2. `tempest_skill()`, `tempest_capability_spec()`, and
   `tempest_connection_ref()` when the expert needs permissioned tools.
3. `tempest_expert()` profiles and exact run-level expert selection.
4. `tempest_deliverable_spec()` contracts.
5. `tempest_operation_registry()` or `tempest_builtin_operation_registry()`.
6. Registered step, generator, validator, renderer, and exporter functions.
7. `tempest_workflow_step()` objects and `tempest_workflow_spec()`.
8. `tempest_runtime()` when capabilities or connections are needed.
9. `tempest_run_workflow()` and public run accessors.

## Small executable pattern

```r
objective <- tempest_objective(
  "Create a reviewable action register.",
  acceptance_criteria = "Every action has an owner.",
  deliverable_ids = "action-register"
)

expert <- tempest_expert(
  expert_id = "expert.delivery",
  name = "Delivery Analyst",
  title = "Implementation specialist",
  description = "Turns requests into executable plans.",
  instructions = "Preserve dependencies and unresolved decisions."
)

deliverable <- tempest_deliverable_spec(
  "action-register",
  title = "Action register",
  purpose = "Record concrete next actions.",
  instructions = "Return a summary and actions.",
  required_fields = c("summary", "actions"),
  evidence_policy = "none",
  generator_id = "tempest.generator.provided_content",
  validator_ids = "tempest.validator.required_fields",
  renderer_ids = "example.renderer.action-register",
  content_type = "action-register",
  media_types = "application/json",
  requires_approval = TRUE
)

operations <- tempest_builtin_operation_registry()
operations$register(
  "example.renderer.action-register",
  kind = "renderer",
  version = "1",
  implementation = function(content) {
    tempest_artifact_representation(
      content = content,
      artifact_kind = "action-register",
      media_type = "application/json"
    )
  }
)
operations$register(
  "example.step.prepare",
  kind = "step",
  version = "1",
  implementation = function(
    objective,
    expert_id,
    artifact_catalog,
    run_id,
    step
  ) {
    content <- list(
      summary = objective@description,
      actions = list(list(action = "Confirm scope", owner = expert_id))
    )
    tempest_generate_deliverable(
      deliverable,
      context = list(content = content),
      registry = operations,
      catalog = artifact_catalog,
      provenance = list(
        artifact_id = "action-register-json",
        run_id = run_id,
        step_id = step@step_id,
        expert_id = expert_id
      )
    )
  }
)

workflow <- tempest_workflow_spec(
  "example.action-register",
  title = "Action-register workflow",
  purpose = "Create a reviewed action register.",
  supported_deliverable_types = "action-register",
  steps = list(tempest_workflow_step(
    "prepare",
    title = "Prepare",
    purpose = "Prepare the action register.",
    operation_id = "example.step.prepare",
    produced_artifact_ids = "action-register-json",
    assignment_rule = "expert.delivery"
  ))
)

run <- tempest_run_workflow(
  objective,
  workflow,
  runtime = operations,
  experts = list(expert),
  deliverables = list(deliverable)
)
```

Expect `awaiting_approval` because the deliverable requires review. Query
`tempest_run_approvals(run, status = "pending")`, record a decision with
`tempest_run_record_approval()`, then inspect the final artifact with
`tempest_run_artifact()`.

## Capability and connection pattern

Use a capability when behavior needs explicit authorization:

```r
connection <- tempest_connection_ref(
  "approved-context",
  provider_id = "host.connections",
  connection_type = "project-context",
  title = "Approved project context",
  description = "Read-only context selected for this run.",
  scopes = "read"
)

capability <- tempest_capability_spec(
  "project-context.read",
  purpose = "Read approved project context.",
  instructions = "Use only the granted connection.",
  operation_id = "host.capability.project-context-read",
  connection_ref_ids = connection@connection_id,
  model_roles = "expert"
)

runtime <- tempest_runtime(
  operations = operations,
  capability_specs = list(capability),
  capability_implementations = list(
    "project-context.read" = function(
      capability_spec,
      connections,
      context
    ) {
      client <- connections[["approved-context"]]
      list(
        tools = list(make_project_context_tool(client)),
        registrars = list(),
        metadata = list(connection_id = "approved-context")
      )
    }
  ),
  connection_refs = list(connection),
  connection_bindings = list(
    "approved-context" = function(connection_ref, context) host_client
  )
)
```

`make_project_context_tool()` and `host_client` above are host-owned runtime
values. Match the exact capability-factory arguments to the current runtime
contract.
Pass `connection_permissions` to `tempest_run_workflow()` as a named allow-list
for the selected expert or model role.

## Operation rules

- Register stable IDs, versions, and kinds.
- Let functions declare only the named context arguments they consume.
- Resolve all operations during preflight; missing or mismatched operations
  should fail before side effects begin.
- Rebuild closures from a factory so the same runtime can be reattached after
  restore.
- Use `tempest_generate_deliverable()` for canonical generation, validation,
  rendering, exporting, checksums, and provenance.

## Test pattern

Test at least:

- graph construction and operation preflight;
- success or expected `awaiting_approval` status;
- approval decision and terminal status;
- artifact content, status, checksum, and provenance;
- strictly increasing event sequences;
- capability allow and deny paths;
- retries without losing prior grant or validation diagnostics;
- cancellation before another side effect;
- save, checksum validation, explicit runtime reattachment, and restore;
- refusal to broaden saved permissions.

Use `tempfile()`, in-memory catalogs/stores, fake chats, and local fixtures.
