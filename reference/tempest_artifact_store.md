# Create a Tempest artifact store adapter

**\[experimental\]**

## Usage

``` r
tempest_artifact_store(
  write = NULL,
  read = NULL,
  list_metadata = NULL,
  exists = NULL,
  version = NULL
)
```

## Arguments

- write:

  Function with signature `function(artifact)` used to persist a typed
  artifact.

- read:

  Function with signature `function(artifact_id, default)` that returns
  a typed artifact.

- list_metadata:

  Function with no arguments that returns a named list of artifact
  metadata records without inline content.

- exists:

  Function with signature `function(artifact_id, version)` used to test
  artifact identity and optional deliverable version.

- version:

  Function with signature `function(artifact_id, default)` that returns
  the persisted deliverable version.

## Value

A typed artifact-store adapter.

## Details

Artifact stores let host applications observe or persist typed Tempest
outputs without replacing the live in-memory artifact catalog. The
default store is a no-op adapter.

## Examples

``` r
store <- tempest_memory_artifact_store()
spec <- tempest_deliverable_spec(
  "report",
  title = "Report",
  purpose = "Explain the result",
  instructions = "Be concise.",
  generator_id = "tempest.generator.provided_content",
  renderer_ids = "tempest.renderer.markdown"
)
artifact <- tempest_artifact(spec, content = "# Report")
store$write(artifact)
store$read(artifact@artifact_id)
#> <tempest::tempest_artifact>
#>  @ artifact_id          : chr "artifact_b7d5db6086a4166a"
#>  @ deliverable_id       : chr "report"
#>  @ deliverable_version  : chr "1"
#>  @ spec_fingerprint     : chr "c34c9d934526738c62ad2db702339f587bd7cd48ea38ffcb6547751480e6e060"
#>  @ artifact_kind        : chr "primary"
#>  @ media_type           : chr "text/markdown"
#>  @ schema_version       : int 1
#>  @ content              : chr "# Report"
#>  @ storage_ref          : chr NA
#>  @ producer_operation_id: chr NA
#>  @ run_id               : chr NA
#>  @ step_id              : chr NA
#>  @ expert_id            : chr NA
#>  @ resource_ids         : chr(0) 
#>  @ claim_ids            : chr(0) 
#>  @ evidence_span_ids    : chr(0) 
#>  @ parent_artifact_ids  : chr(0) 
#>  @ validation_results   : list()
#>  @ status               : chr "draft"
#>  @ checksum             : chr "04e1d1467e73933d8841c0c22eca9710ee72d020f5d494b091d68d4d2efea89d"
#>  @ created_at           : chr "2026-07-19 02:12:42 UTC"
#>  @ updated_at           : chr "2026-07-19 02:12:42 UTC"
#>  @ metadata             : list()
```
