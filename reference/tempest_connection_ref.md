# Create an opaque Tempest connection reference

**\[experimental\]**

## Usage

``` r
tempest_connection_ref(
  connection_id,
  provider_id,
  connection_type,
  title,
  description,
  version = "1",
  scopes = character(),
  state = "active",
  metadata = list(),
  schema_version = 1L
)
```

## Arguments

- connection_id:

  Stable, opaque connection identifier.

- provider_id:

  Host connection-provider identifier.

- connection_type:

  Host-defined connection type.

- title:

  Display title.

- description:

  Non-secret description of the connection's purpose.

- version:

  Stable reference version.

- scopes:

  Non-secret scope labels used for capability resolution.

- state:

  Reference state, either `"active"` or `"retired"`.

- metadata:

  Canonical JSON-compatible non-secret metadata.

- schema_version:

  Serializable record schema version.

## Value

A `tempest_connection_ref` S7 object.

## Details

This experimental API is frozen and scheduled for removal in Tempest
0.2.0. No compatibility shim is planned; see
[tempest-generic-kernel-retirement](https://jameshwade.github.io/tempest/reference/tempest-generic-kernel-retirement.md).

A connection reference identifies a host-owned authenticated binding
without storing credentials or a live client in a durable workflow
definition.

## Examples

``` r
connection <- tempest_connection_ref(
  "knowledge-base",
  provider_id = "host.connections",
  connection_type = "document-search",
  title = "Approved knowledge base",
  description = "Read-only customer documentation"
)
```
