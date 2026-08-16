# Retirement of Tempest's experimental generic kernel

**\[experimental\]**

## Details

Tempest 0.2 becomes a focused scientific-research product. The
experimental application-neutral workflow, runtime, capability,
connection, skill, deliverable, and artifact APIs are frozen and
scheduled for removal in Tempest 0.2.0. They remain available in the 0.1
development line only to preserve the product baseline while replacement
seams are proven.

No compatibility framework or generic-kernel migration layer will be
added. STORM and Co-STORM product bundles will receive direct,
product-specific replacements before the generic implementation is
deleted.

## Retirement

This experimental API is frozen and scheduled for removal in Tempest
0.2.0. No compatibility shim is planned.

## Scheduled API families

- Generic workflows:
  [`tempest_workflow_spec()`](https://jameshwade.github.io/tempest/reference/tempest_workflow_spec.md),
  [`tempest_workflow_step()`](https://jameshwade.github.io/tempest/reference/tempest_workflow_step.md),
  [`tempest_run_workflow()`](https://jameshwade.github.io/tempest/reference/tempest_run_workflow.md),
  and the `tempest_storm_workflow_*()` and
  `tempest_costorm_workflow_*()` families.

- Generic runs:
  [`tempest_run_status()`](https://jameshwade.github.io/tempest/reference/tempest_run_accessors.md),
  [`tempest_run_events()`](https://jameshwade.github.io/tempest/reference/tempest_run_accessors.md),
  [`tempest_run_approvals()`](https://jameshwade.github.io/tempest/reference/tempest_run_accessors.md),
  [`tempest_run_capability_grants()`](https://jameshwade.github.io/tempest/reference/tempest_run_accessors.md),
  [`tempest_run_artifact()`](https://jameshwade.github.io/tempest/reference/tempest_run_accessors.md),
  [`tempest_run_artifacts()`](https://jameshwade.github.io/tempest/reference/tempest_run_accessors.md),
  [`tempest_run_record_approval()`](https://jameshwade.github.io/tempest/reference/tempest_run_accessors.md),
  [`tempest_run_request_cancel()`](https://jameshwade.github.io/tempest/reference/tempest_run_accessors.md),
  [`tempest_run_snapshot()`](https://jameshwade.github.io/tempest/reference/tempest_run_snapshot.md),
  [`tempest_run_restore()`](https://jameshwade.github.io/tempest/reference/tempest_run_restore.md),
  [`tempest_run_save()`](https://jameshwade.github.io/tempest/reference/tempest_run_save.md),
  and
  [`tempest_run_resume()`](https://jameshwade.github.io/tempest/reference/tempest_run_resume.md).
  The shared
  [`tempest_execution_events()`](https://jameshwade.github.io/tempest/reference/tempest_execution_events.md)
  query remains a product seam and will be narrowed to STORM and
  Co-STORM state.

- Runtime contracts:
  [`tempest_operation_registry()`](https://jameshwade.github.io/tempest/reference/tempest_operation_registry.md),
  [`tempest_builtin_operation_registry()`](https://jameshwade.github.io/tempest/reference/tempest_builtin_operation_registry.md),
  [`tempest_builtin_workflow_operation_registry()`](https://jameshwade.github.io/tempest/reference/tempest_builtin_workflow_operation_registry.md),
  [`tempest_runtime()`](https://jameshwade.github.io/tempest/reference/tempest_runtime.md),
  [`tempest_skill()`](https://jameshwade.github.io/tempest/reference/tempest_skill.md),
  [`tempest_skill_registry()`](https://jameshwade.github.io/tempest/reference/tempest_skill_registry.md),
  [`tempest_capability_spec()`](https://jameshwade.github.io/tempest/reference/tempest_capability_spec.md),
  [`tempest_capability_resolver()`](https://jameshwade.github.io/tempest/reference/tempest_capability_resolver.md),
  [`tempest_connection_ref()`](https://jameshwade.github.io/tempest/reference/tempest_connection_ref.md),
  and
  [`tempest_connection_provider()`](https://jameshwade.github.io/tempest/reference/tempest_connection_provider.md).

- Generic outputs:
  [`tempest_objective()`](https://jameshwade.github.io/tempest/reference/tempest_objective.md),
  [`tempest_deliverable_spec()`](https://jameshwade.github.io/tempest/reference/tempest_deliverable_spec.md),
  [`tempest_generate_deliverable()`](https://jameshwade.github.io/tempest/reference/tempest_generate_deliverable.md),
  and the `tempest_artifact*()` and `tempest_*artifact_store()`
  families. Product validation results remain available for report
  bundles.

## Replacement direction

Fixed scientific transformations become dsprrr program references;
open-ended agent work uses Deputy; accepted knowledge uses graft
snapshots and commits; provisional scientific evidence remains in
Tempest; and reports and promotion plans become product-specific
bundles.

## See also

[`tempest_run()`](https://jameshwade.github.io/tempest/reference/tempest_run.md)
and
[`tempest_session()`](https://jameshwade.github.io/tempest/reference/tempest_session.md)
for the supported research product APIs.
