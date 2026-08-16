# Run STORM through the generic Tempest workflow kernel

**\[experimental\]**

## Usage

``` r
tempest_storm_workflow_run(
  topic,
  config = tempest_config(),
  retriever = NULL,
  n_experts = 3,
  experts = NULL,
  runtime = tempest_runtime(),
  runtime_factory = function() tempest_runtime(),
  connection_permissions = list(),
  artifact_catalog = NULL,
  run_id = NULL,
  progress = NULL,
  stage_progress = NULL,
  verbose = TRUE,
  ...
)
```

## Arguments

- topic:

  Research objective.

- config:

  A `TempestConfig`.

- retriever:

  Optional retriever whose workspace must be the run's `source_store`.

- n_experts:

  Number of experts to generate when the run starts without an explicit
  expert pool.

- experts:

  Optional exact expert pool.

- runtime:

  A `TempestRuntime` containing process-local adapters.

- runtime_factory:

  Existing STORM runtime factory.

- connection_permissions:

  Named list mapping expert or model-role ids to opaque connection ids
  allowed for the run.

- artifact_catalog:

  Optional shared typed artifact catalog.

- run_id:

  Optional stable generic run id.

- progress:

  Optional generic run event callback.

- stage_progress:

  Optional legacy STORM progress callback.

- verbose:

  Whether existing STORM stages print progress.

- ...:

  Additional named
  [`tempest_run()`](https://jameshwade.github.io/tempest/reference/tempest_run.md)
  arguments such as `research_strategy` or `max_rounds`.

## Value

A `TempestRun`.

## Details

This experimental API is frozen and scheduled for removal in Tempest
0.2.0. No compatibility shim is planned; see
[tempest-generic-kernel-retirement](https://jameshwade.github.io/tempest/reference/tempest-generic-kernel-retirement.md).

This is the generic-run counterpart to
[`tempest_run()`](https://jameshwade.github.io/tempest/reference/tempest_run.md).
It returns the `TempestRun`; each step result retains the corresponding
STORM result under `$value`.
