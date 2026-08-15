# Create a Tempest workflow step

**\[experimental\]**

## Usage

``` r
tempest_workflow_step(
  step_id,
  title,
  purpose,
  operation_id,
  version = "1",
  operation_version = "1",
  dependency_ids = character(),
  required_input_artifact_ids = character(),
  produced_artifact_ids = character(),
  assignment_rule = NULL,
  required_capability_ids = character(),
  optional_capability_ids = character(),
  retry_policy = list(max_attempts = 1L),
  failure_policy = c("stop", "continue"),
  approval_checkpoint = FALSE,
  side_effecting = FALSE,
  metadata = list(),
  schema_version = 1L
)
```

## Arguments

- step_id:

  Stable step identifier.

- title:

  Display title.

- purpose:

  What the step accomplishes.

- operation_id:

  Runtime step-operation identifier.

- version, operation_version:

  Stable definition and operation versions.

- dependency_ids:

  Step identifiers that must succeed first.

- required_input_artifact_ids:

  Artifact ids required by the step.

- produced_artifact_ids:

  Artifact ids the step promises to publish.

- assignment_rule:

  `"none"`, `"all"`, an exact character vector of expert ids, or a list
  with `type` and `expert_ids`.

- required_capability_ids, optional_capability_ids:

  Capabilities resolved once for the shared step context. Experts
  assigned to a step with scoped capabilities must share the same
  `model_role`.

- retry_policy:

  A serializable list with positive `max_attempts`.

- failure_policy:

  Either `"stop"` or `"continue"`.

- approval_checkpoint:

  Whether approval is required before execution.

- side_effecting:

  Whether the operation may change external state.

- metadata:

  Canonical JSON-compatible metadata without credentials.

- schema_version:

  Serializable record schema version.

## Value

A `tempest_workflow_step` S7 object.

## Details

This experimental API is frozen and scheduled for removal in Tempest
0.2.0. No compatibility shim is planned; see
[tempest-generic-kernel-retirement](https://jameshwade.github.io/tempest/reference/tempest-generic-kernel-retirement.md).

A workflow step is a serializable declaration. Its executable operation
is resolved from the run's runtime registry before any step begins.
