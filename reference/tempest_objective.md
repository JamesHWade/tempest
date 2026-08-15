# Create a Tempest objective

**\[experimental\]**

## Usage

``` r
tempest_objective(
  description,
  title = description,
  objective_id = NULL,
  context = list(),
  constraints = character(),
  acceptance_criteria = character(),
  input_resource_ids = character(),
  deliverable_ids = character(),
  metadata = list(),
  created_at = NULL,
  schema_version = 1L
)
```

## Arguments

- description:

  Requested outcome.

- title:

  Short display title. Defaults to `description`.

- objective_id:

  Optional stable identifier.

- context:

  Serializable host-provided context.

- constraints:

  Character vector of requirements and exclusions.

- acceptance_criteria:

  Character vector of observable completion conditions.

- input_resource_ids:

  Approved input resource identifiers.

- deliverable_ids:

  Requested deliverable specification identifiers.

- metadata:

  Serializable, namespaced host metadata.

- created_at:

  Optional creation timestamp.

- schema_version:

  Positive objective schema version.

## Value

A `tempest_objective` S7 object.

## Details

This experimental API is frozen and scheduled for removal in Tempest
0.2.0. No compatibility shim is planned; see
[tempest-generic-kernel-retirement](https://jameshwade.github.io/tempest/reference/tempest-generic-kernel-retirement.md).

An objective describes an application-neutral requested outcome, its
constraints, approved inputs, completion criteria, and requested
deliverables.

## Examples

``` r
objective <- tempest_objective(
  "Prepare an evidence-backed response",
  acceptance_criteria = "Every recommendation cites supporting evidence",
  deliverable_ids = "customer-response"
)
```
