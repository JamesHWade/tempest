# Describe one renderer-produced artifact representation

**\[experimental\]**

## Usage

``` r
tempest_artifact_representation(
  content = NULL,
  storage_ref = NA_character_,
  artifact_kind = "primary",
  media_type = NULL,
  resource_ids = character(),
  claim_ids = character(),
  evidence_span_ids = character(),
  parent_artifact_ids = character(),
  metadata = list()
)
```

## Arguments

- content:

  Inline representation content: a single UTF-8 string or canonical
  JSON-compatible lists and atomic values.

- storage_ref:

  Optional external storage reference.

- artifact_kind:

  Role within the deliverable.

- media_type:

  Optional media type. Defaults to renderer metadata or the deliverable
  specification.

- resource_ids, claim_ids, evidence_span_ids:

  Evidence identifiers.

- parent_artifact_ids:

  Parent artifact identifiers.

- metadata:

  Serializable representation metadata.

## Value

A runtime `tempest_artifact_representation`.

## Details

Custom renderer operations return this lightweight runtime value. The
deliverable lifecycle adds specification identity, validation results,
provenance, checksums, and lifecycle status when it creates the final
typed artifact.
