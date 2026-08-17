# Create a Tempest research manifest

`tempest_research_manifest()` records only durable identities and
references for one STORM or Co-STORM run. Supply a
[`tempest_config()`](https://jameshwade.github.io/tempest/reference/tempest_config.md)
object to compute its behavior-relevant digest. `config_digest` is
intended for restoration of an existing record.

## Usage

``` r
tempest_research_manifest(
  research_run_id,
  mode = c("storm", "costorm"),
  config = NULL,
  config_digest = NULL,
  programs = list(),
  knowledge_snapshot = list(),
  runtime = list(deputy_session_ids = character(), deputy_run_ids = character()),
  traces = list(),
  deliverables = list(),
  status = "running",
  schema_version = 2L
)
```

## Arguments

- research_run_id:

  Stable research-run identifier.

- mode:

  Product mode, either `"storm"` or `"costorm"`.

- config:

  A `TempestConfig` used to compute `config_digest`, or `NULL` when
  restoring an existing digest.

- config_digest:

  Existing SHA-256 configuration identity. Supply this only when
  `config` is unavailable during restoration.

- programs:

  Named references to exact scientific programs.

- knowledge_snapshot:

  Reference to a pinned accepted-knowledge snapshot.

- runtime:

  Opaque Deputy session and run references.

- traces:

  References to Deputy or dsprrr traces.

- deliverables:

  References to product deliverables.

- status:

  Run status: `"running"`, `"succeeded"`, `"failed"`, or `"cancelled"`.

- schema_version:

  Manifest record schema. Only version 2 is supported.

## Value

A `TempestResearchManifest` S7 object.

## Details

Reference fields accept only canonical JSON-compatible plain values.
They cannot contain credentials, chats, functions, environments,
connections, S7 or R6 objects, external pointers, or missing and
non-finite values.

## Examples

``` r
manifest <- tempest_research_manifest(
  research_run_id = "research-123",
  mode = "storm",
  config = tempest_config()
)
manifest@status
#> [1] "running"
```
