# Create an adapter for the built-in Co-STORM workflow

**\[experimental\]**

## Usage

``` r
tempest_costorm_workflow_adapter(
  session,
  style = c("technical", "executive"),
  include_references = TRUE,
  reorganize = TRUE,
  verbose = TRUE
)
```

## Arguments

- session:

  A `TempestSession`.

- style:

  Report style.

- include_references:

  Whether the report includes references.

- reorganize:

  Whether to reorganize the mind map before reporting.

- verbose:

  Whether warmup prints progress.

## Value

A process-local workflow adapter function.

## Details

Warmup and report operations call the existing `TempestSession` methods.
The dialogue operation records the current session boundary; individual
interactive turns remain host-driven while the run awaits approval.
