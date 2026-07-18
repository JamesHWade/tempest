# Define a typed artifact codec

**\[experimental\]**

## Usage

``` r
tempest_artifact_codec_definition(
  codec_id,
  encode = NULL,
  decode = NULL,
  version = "1",
  media_types = "*/*",
  extension = "bin",
  supports = NULL,
  external = FALSE,
  priority = 0,
  metadata = list()
)

tempest_artifact_codec(...)
```

## Arguments

- codec_id:

  Stable codec identifier.

- encode, decode:

  Runtime functions for inline content. `encode` returns raw bytes or a
  list containing `bytes` and an optional `extension`; `decode` returns
  reconstructed inline content.

- version:

  Stable codec version.

- media_types:

  Supported media types or patterns such as `"text/*"`.

- extension:

  Default filename extension without a leading dot.

- supports:

  Optional runtime predicate for content selection.

- external:

  Whether the codec represents an external storage reference rather than
  inline bytes.

- priority:

  Numeric automatic-selection priority.

- metadata:

  Canonical JSON-compatible descriptive metadata.

- ...:

  Arguments forwarded to `tempest_artifact_codec_definition()`.

## Value

A runtime artifact codec definition.

## Details

Codec definitions keep executable encode/decode functions in a runtime
registry while exposing only serializable identity and media metadata in
durable listings.
