# Create an adapter for the built-in STORM workflow

**\[experimental\]**

## Usage

``` r
tempest_storm_workflow_adapter(
  config = tempest_config(),
  retriever = NULL,
  n_experts = 3,
  runtime_factory = function() tempest_runtime(),
  stage_progress = NULL,
  verbose = TRUE,
  ...
)
```

## Arguments

- config:

  A `TempestConfig`.

- retriever:

  Optional retriever whose workspace must be the run's `source_store`.

- n_experts:

  Number of experts to generate when the run starts without an explicit
  expert pool.

- runtime_factory:

  Existing STORM runtime factory.

- stage_progress:

  Optional legacy STORM progress callback.

- verbose:

  Whether existing STORM stages print progress.

- ...:

  Additional named
  [`tempest_run()`](https://jameshwade.github.io/tempest/reference/tempest_run.md)
  arguments such as `research_strategy` or `max_rounds`.

## Value

A process-local workflow adapter function.

## Details

This experimental API is frozen and scheduled for removal in Tempest
0.2.0. No compatibility shim is planned; see
[tempest-generic-kernel-retirement](https://jameshwade.github.io/tempest/reference/tempest-generic-kernel-retirement.md).

The adapter executes one existing
[`tempest_run()`](https://jameshwade.github.io/tempest/reference/tempest_run.md)
stage at a time against the generic run's shared evidence store and
artifact catalog. Runtime and connection permissions always come from
the owning `TempestRun`.
