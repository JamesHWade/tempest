# Execute an application-neutral Tempest workflow

**\[experimental\]**

## Usage

``` r
tempest_run_workflow(
  objective,
  workflow,
  runtime,
  experts = list(),
  connection_permissions = list(),
  deliverables = list(),
  artifact_catalog = NULL,
  source_store = NULL,
  runtime_context = list(),
  policy_adapter = NULL,
  run_id = NULL,
  progress = NULL
)
```

## Arguments

- objective:

  A
  [`tempest_objective()`](https://jameshwade.github.io/tempest/reference/tempest_objective.md).

- workflow:

  A
  [`tempest_workflow_spec()`](https://jameshwade.github.io/tempest/reference/tempest_workflow_spec.md).

- runtime:

  A
  [`tempest_runtime()`](https://jameshwade.github.io/tempest/reference/tempest_runtime.md),
  operation registry, or runtime list containing an operation registry.

- experts:

  Exact pool of selected
  [`tempest_expert()`](https://jameshwade.github.io/tempest/reference/tempest_expert.md)
  profiles.

- connection_permissions:

  Named list mapping expert or model-role ids to opaque connection ids
  allowed for this run. Step-scoped capabilities use the union of
  assigned expert-id permissions and their one shared model-role
  permission.

- deliverables:

  Deliverable specifications available to the run.

- artifact_catalog:

  Optional typed artifact catalog.

- source_store:

  Optional `SourceStore` evidence ledger.

- runtime_context:

  Process-local named services, such as a retriever or expert-session
  manager, made available to operations and capability factories.
  Approved-output exporters also receive these services as their
  runtime. Runtime context is never serialized.

- policy_adapter:

  Optional policy function or object with `evaluate()`.

- run_id:

  Optional stable run identifier.

- progress:

  Optional generic event callback.

## Value

A mutable `TempestRun`.

## Details

This experimental API is frozen and scheduled for removal in Tempest
0.2.0. No compatibility shim is planned; see
[tempest-generic-kernel-retirement](https://jameshwade.github.io/tempest/reference/tempest-generic-kernel-retirement.md).

Construction preflights every step operation before execution. The
function returns a mutable run in a terminal state or in nonblocking
`awaiting_approval` state. If execution fails after construction, the
classed error contains the inspectable failed run in `condition$run` and
its identity in `condition$run_id`. Construction and preflight errors
occur before a run exists.
