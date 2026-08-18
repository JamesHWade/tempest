# Tempest 0.2 generic-kernel cutover

Tempest 0.2 supports the STORM and Co-STORM scientific-research products
through
[`tempest_run()`](https://jameshwade.github.io/tempest/reference/tempest_run.md)
and
[`tempest_session()`](https://jameshwade.github.io/tempest/reference/tempest_session.md).
The former experimental application-neutral workflow, runtime,
capability, connection, skill, deliverable, and artifact APIs are
unavailable. Every retained symbol fails immediately with class
`tempest_generic_kernel_cutover_error` and identifies the symbol that
was called.

## Details

This is an immediate cutover. There is no compatibility framework,
fallback, or generic-kernel migration layer. Retained names exist only
as cutover sentinels until T8 removes them physically.

## Unavailable API families

- Generic workflows: `tempest_workflow_spec()`,
  `tempest_workflow_step()`, `tempest_run_workflow()`, and the
  `tempest_storm_workflow_*()` and `tempest_costorm_workflow_*()`
  families.

- Generic runs: `tempest_run_status()`, `tempest_run_events()`,
  `tempest_run_approvals()`, `tempest_run_capability_grants()`,
  `tempest_run_artifact()`, `tempest_run_artifacts()`,
  `tempest_run_record_approval()`, `tempest_run_request_cancel()`,
  `tempest_run_snapshot()`, and `tempest_run_save()`. The shared
  [`tempest_execution_events()`](https://jameshwade.github.io/tempest/reference/tempest_execution_events.md)
  query remains a Co-STORM-only product seam for `TempestSession` event
  history.

- Runtime contracts: `tempest_operation_registry()`,
  `tempest_builtin_operation_registry()`,
  `tempest_builtin_workflow_operation_registry()`, `tempest_runtime()`,
  `tempest_skill()`, `tempest_skill_registry()`,
  `tempest_capability_spec()`, `tempest_capability_resolver()`,
  `tempest_connection_ref()`, and `tempest_connection_provider()`.

- Generic outputs: `tempest_objective()`, `tempest_deliverable_spec()`,
  `tempest_generate_deliverable()`, and the `tempest_artifact*()` and
  `tempest_*artifact_store()` families.

## Supported product boundary

Use
[`tempest_run()`](https://jameshwade.github.io/tempest/reference/tempest_run.md)
for scripted STORM research and
[`tempest_session()`](https://jameshwade.github.io/tempest/reference/tempest_session.md)
for interactive Co-STORM research. Fixed scientific transformations
carry dsprrr program identity, open-ended agent work carries Deputy
execution identity, and accepted knowledge uses graft snapshots and
commits. Execution identity supports correlation and audit joins only;
it does not claim that an agent execution caused, authored, or validated
report content.

## See also

[`tempest_run()`](https://jameshwade.github.io/tempest/reference/tempest_run.md)
and
[`tempest_session()`](https://jameshwade.github.io/tempest/reference/tempest_session.md)
for the supported research product APIs.
