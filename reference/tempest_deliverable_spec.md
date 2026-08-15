# Create a Tempest deliverable specification

**\[experimental\]**

## Usage

``` r
tempest_deliverable_spec(
  deliverable_id,
  title,
  purpose,
  instructions,
  version = "1",
  content_schema = list(),
  required_fields = character(),
  evidence_policy = "source_attributed",
  generator_id,
  validator_ids = character(),
  renderer_ids,
  exporter_ids = character(),
  operation_versions = character(),
  content_type = "text",
  media_types = "text/markdown",
  filename_policy = list(),
  requires_approval = FALSE,
  metadata = list()
)
```

## Arguments

- deliverable_id:

  Stable specification identifier.

- title:

  Display title.

- purpose:

  What the deliverable is intended to accomplish.

- instructions:

  Generation instructions.

- version:

  Stable specification version.

- content_schema:

  Serializable canonical JSON content schema. Tempest records this
  contract but enforces it only through validators named in
  `validator_ids`.

- required_fields:

  Required content fields or sections.

- evidence_policy:

  Evidence policy.

- generator_id:

  Runtime generator operation identifier.

- validator_ids:

  Runtime validator operation identifiers.

- renderer_ids:

  Runtime renderer operation identifiers.

- exporter_ids:

  Runtime exporter operation identifiers.

- operation_versions:

  Optional named character vector mapping operation identifiers to
  required versions.

- content_type:

  Canonical content type.

- media_types:

  Artifact media types this specification may produce.

- filename_policy:

  Serializable filename policy.

- requires_approval:

  Whether output requires approval.

- metadata:

  Serializable host metadata.

## Value

A `tempest_deliverable_spec` S7 object.

## Details

This experimental API is frozen and scheduled for removal in Tempest
0.2.0. No compatibility shim is planned; see
[tempest-generic-kernel-retirement](https://jameshwade.github.io/tempest/reference/tempest-generic-kernel-retirement.md).

A deliverable specification separates serializable output requirements
from runtime generator, validator, renderer, and exporter
implementations.

## Examples

``` r
spec <- tempest_deliverable_spec(
  "customer-response",
  title = "Customer response",
  purpose = "Answer the customer's request with evidence",
  instructions = "Be concise and preserve uncertainty.",
  required_fields = c("response", "risks"),
  generator_id = "tempest.generator.provided_content",
  renderer_ids = "tempest.renderer.markdown"
)
```
