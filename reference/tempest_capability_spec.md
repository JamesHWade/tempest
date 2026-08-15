# Create a Tempest capability specification

**\[experimental\]**

## Usage

``` r
tempest_capability_spec(
  capability_id,
  purpose,
  instructions,
  operation_id,
  version = "1",
  title = capability_id,
  operation_version = "1",
  connection_ref_ids = character(),
  model_roles = character(),
  input_schema = list(),
  output_schema = list(),
  side_effecting = FALSE,
  state = "active",
  metadata = list(),
  schema_version = 1L
)
```

## Arguments

- capability_id:

  Stable capability identifier.

- purpose:

  Outcome the capability supports.

- instructions:

  Usage and safety instructions.

- operation_id:

  Runtime capability operation identifier.

- version:

  Stable capability version.

- title:

  Display title. Defaults to `capability_id`.

- operation_version:

  Required runtime operation version.

- connection_ref_ids:

  Opaque connection reference identifiers required at runtime.

- model_roles:

  Model roles allowed to receive this capability. An empty vector does
  not restrict roles.

- input_schema, output_schema:

  Canonical JSON-compatible contracts.

- side_effecting:

  Whether the capability can change external state.

- state:

  Definition state, either `"active"` or `"retired"`.

- metadata:

  Canonical JSON-compatible host metadata. Metadata cannot contain
  credentials or executable values.

- schema_version:

  Serializable record schema version.

## Value

A `tempest_capability_spec` S7 object.

## Details

This experimental API is frozen and scheduled for removal in Tempest
0.2.0. No compatibility shim is planned; see
[tempest-generic-kernel-retirement](https://jameshwade.github.io/tempest/reference/tempest-generic-kernel-retirement.md).

A capability specification declares permissioned callable behavior.
Runtime implementations and authenticated connections are resolved
separately.

## Examples

``` r
capability <- tempest_capability_spec(
  "evidence.search",
  purpose = "Find approved evidence",
  instructions = "Use only the granted connection.",
  operation_id = "tempest.capability.search",
  connection_ref_ids = "knowledge-base"
)
```
