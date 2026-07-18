# Create a Tempest workflow specification

**\[experimental\]**

## Usage

``` r
tempest_workflow_spec(
  workflow_id,
  title,
  purpose,
  steps,
  version = "1",
  supported_objective_types = "tempest_objective",
  supported_deliverable_types = character(),
  metadata = list(),
  schema_version = 1L
)
```

## Arguments

- workflow_id:

  Stable workflow identifier.

- title:

  Display title.

- purpose:

  What the workflow accomplishes.

- steps:

  Non-empty list of
  [`tempest_workflow_step()`](https://jameshwade.github.io/tempest/reference/tempest_workflow_step.md)
  objects.

- version:

  Stable workflow version.

- supported_objective_types:

  Objective type ids accepted by the workflow.

- supported_deliverable_types:

  Deliverable content types accepted by the workflow.

- metadata:

  Canonical JSON-compatible metadata without credentials.

- schema_version:

  Serializable record schema version.

## Value

A validated `tempest_workflow_spec` S7 object.
