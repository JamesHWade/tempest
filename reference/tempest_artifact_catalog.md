# Create a typed Tempest artifact catalog

**\[experimental\]**

## Usage

``` r
tempest_artifact_catalog(
  store = NULL,
  artifacts = list(),
  deliverables = list()
)
```

## Arguments

- store:

  Optional host artifact-store adapter. Catalog writes are persisted
  through the adapter before becoming visible in memory.

- artifacts:

  Optional initial list of typed artifacts.

- deliverables:

  Deliverable specifications referenced by `artifacts`.

## Value

A `TempestArtifactCatalog` with `add()`,
[`get()`](https://rdrr.io/r/base/get.html), `has()`,
[`version()`](https://rdrr.io/r/base/Version.html),
[`list()`](https://rdrr.io/r/base/list.html), deliverable-registration,
and `snapshot()` methods.

## Details

The catalog owns typed artifacts for one workflow run. It supports
metadata-only listing so host applications do not need to load large
artifact content to render an output index.

## Examples

``` r
spec <- tempest_deliverable_spec(
  "brief",
  title = "Brief",
  purpose = "Summarize the result",
  instructions = "Be concise.",
  generator_id = "tempest.generator.markdown_report",
  renderer_ids = "tempest.renderer.markdown"
)
catalog <- tempest_artifact_catalog(deliverables = list(spec))
artifact <- tempest_artifact(spec, content = "# Brief")
catalog$add(artifact)
catalog$list()
#> $artifact_415841db91611f5a
#> $artifact_415841db91611f5a$artifact_id
#> [1] "artifact_415841db91611f5a"
#> 
#> $artifact_415841db91611f5a$deliverable_id
#> [1] "brief"
#> 
#> $artifact_415841db91611f5a$deliverable_version
#> [1] "1"
#> 
#> $artifact_415841db91611f5a$spec_fingerprint
#> [1] "7163d0f76e102677ea68a30af5b96c74655df967c758356e45fe39defa69bb71"
#> 
#> $artifact_415841db91611f5a$artifact_kind
#> [1] "primary"
#> 
#> $artifact_415841db91611f5a$media_type
#> [1] "text/markdown"
#> 
#> $artifact_415841db91611f5a$schema_version
#> [1] 1
#> 
#> $artifact_415841db91611f5a$storage_ref
#> [1] NA
#> 
#> $artifact_415841db91611f5a$producer_operation_id
#> [1] NA
#> 
#> $artifact_415841db91611f5a$run_id
#> [1] NA
#> 
#> $artifact_415841db91611f5a$step_id
#> [1] NA
#> 
#> $artifact_415841db91611f5a$expert_id
#> [1] NA
#> 
#> $artifact_415841db91611f5a$resource_ids
#> character(0)
#> 
#> $artifact_415841db91611f5a$claim_ids
#> character(0)
#> 
#> $artifact_415841db91611f5a$evidence_span_ids
#> character(0)
#> 
#> $artifact_415841db91611f5a$parent_artifact_ids
#> character(0)
#> 
#> $artifact_415841db91611f5a$validation_results
#> list()
#> 
#> $artifact_415841db91611f5a$status
#> [1] "draft"
#> 
#> $artifact_415841db91611f5a$checksum
#> [1] "fd55350669a978d5a8cde0218d92baa5d6f8e1c9102f40cc42301a56543cc99d"
#> 
#> $artifact_415841db91611f5a$created_at
#> [1] "2026-07-18 22:41:51 UTC"
#> 
#> $artifact_415841db91611f5a$updated_at
#> [1] "2026-07-18 22:41:51 UTC"
#> 
#> $artifact_415841db91611f5a$metadata
#> list()
#> 
#> 
```
