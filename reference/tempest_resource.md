# Create a typed evidence resource

**\[experimental\]**

## Usage

``` r
tempest_resource(
  resource_kind,
  locator,
  title,
  media_type,
  resource_id = NULL,
  content = NULL,
  storage_ref = NULL,
  origin_connection_id = NULL,
  scope_metadata = list(),
  content_hash = NULL,
  retrieved_at = NULL,
  redaction = list(),
  retention = list(),
  metadata = list(),
  schema_version = 1L
)
```

## Arguments

- resource_kind:

  Stable resource-kind identifier such as `"web"`, `"file"`, or
  `"host.document"`.

- locator:

  Opaque locator or URI. Tempest records it but does not resolve it
  outside a resource-kind adapter.

- title:

  Display title.

- media_type:

  IANA media type.

- resource_id:

  Optional stable resource identifier. By default it is derived from
  `resource_kind` and `locator`.

- content:

  Optional inline canonical content.

- storage_ref:

  Optional opaque reference to externally stored content.

- origin_connection_id:

  Optional host connection reference identifier.

- scope_metadata:

  Serializable tenant or project scope metadata.

- content_hash:

  Optional content checksum. Tempest computes one for inline content
  when omitted.

- retrieved_at:

  Retrieval timestamp.

- redaction:

  Serializable redaction metadata.

- retention:

  Serializable retention metadata.

- metadata:

  Serializable namespaced host metadata.

- schema_version:

  Positive resource schema version.

## Value

A `tempest_resource` S7 object.

## Details

Resources identify evidence without requiring a public URL. The durable
value may describe a web page, file, email, database result, host
document, or another application-defined resource kind. Authenticated
clients and credentials remain in host runtime adapters and are never
stored here.

## Examples

``` r
resource <- tempest_resource(
  resource_kind = "host.document",
  locator = "documents/brief-42",
  title = "Approved project brief",
  media_type = "text/plain",
  content = "The requested outcome is a rollout plan."
)
```
