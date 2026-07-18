# Create a typed Tempest artifact

**\[experimental\]**

## Usage

``` r
tempest_artifact(
  deliverable,
  content = NULL,
  storage_ref = NA_character_,
  artifact_id = NULL,
  artifact_kind = "primary",
  media_type = NULL,
  schema_version = 1L,
  producer_operation_id = NA_character_,
  run_id = NA_character_,
  step_id = NA_character_,
  expert_id = NA_character_,
  resource_ids = character(),
  claim_ids = character(),
  evidence_span_ids = character(),
  parent_artifact_ids = character(),
  validation_results = list(),
  status = c("draft", "valid", "invalid", "awaiting_approval", "approved", "rejected"),
  checksum = NULL,
  created_at = NULL,
  updated_at = created_at,
  metadata = list()
)
```

## Arguments

- deliverable:

  A `tempest_deliverable_spec` object.

- content:

  Inline artifact content: a single UTF-8 string or canonical
  JSON-compatible lists and atomic values. JSON content restores with
  JSON object and array semantics; use `storage_ref` for other
  representations.

- storage_ref:

  Optional external storage reference.

- artifact_id:

  Optional stable artifact identifier.

- artifact_kind:

  Artifact role within the deliverable.

- media_type:

  Artifact media type.

- schema_version:

  Positive artifact schema version.

- producer_operation_id:

  Producing operation identifier.

- run_id, step_id, expert_id:

  Optional provenance identifiers.

- resource_ids, claim_ids, evidence_span_ids:

  Evidence identifiers.

- parent_artifact_ids:

  Parent artifact identifiers.

- validation_results:

  Validation result objects.

- status:

  Artifact lifecycle status.

- checksum:

  Optional content checksum.

- created_at, updated_at:

  Optional timestamps.

- metadata:

  Serializable metadata.

## Value

A `tempest_artifact` S7 object.

## Examples

``` r
spec <- tempest_deliverable_spec(
  "brief",
  title = "Brief",
  purpose = "Summarize findings",
  instructions = "Use verified evidence.",
  generator_id = "tempest.generator.provided_content",
  renderer_ids = "tempest.renderer.markdown"
)
artifact <- tempest_artifact(spec, content = "# Brief")
```
