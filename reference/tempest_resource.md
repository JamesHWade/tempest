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
  metadata = list()
)
```

## Arguments

- resource_kind:

  Stable resource-kind identifier such as `"web"`, `"file"`, or
  `"scientific.document"`.

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

  Serializable tenant or project scope metadata. Credential-like field
  names and values are rejected recursively.

- content_hash:

  Optional content checksum. Tempest computes one for inline content
  when omitted.

- retrieved_at:

  Retrieval timestamp.

- redaction:

  Serializable redaction metadata. Credential-like field names and
  values are rejected recursively.

- retention:

  Serializable retention metadata. Credential-like field names and
  values are rejected recursively.

- metadata:

  Serializable namespaced host metadata. Credential-like field names and
  values are rejected recursively.

## Value

A `tempest_resource` S7 object.

## Details

Resources identify provisional scientific evidence without requiring a
public URL. The durable value may describe a web page, file, lab record,
or database result used during research. Authenticated clients and
credentials remain host-owned and are never stored here. This record is
not a generic connection-management contract; its 0.2 role narrows to
scientific source and context evidence in a research workspace.
