# TempestResearchManifest (S7)

A small immutable value contract describing the identities and
references used by one STORM or Co-STORM research run. Runtime clients,
credentials, stores, tools, and executable objects are deliberately
excluded.

## Usage

``` r
TempestResearchManifest(
  schema_version = 3L,
  research_run_id = character(0),
  mode = character(0),
  config_digest = character(0),
  programs = list(),
  knowledge_snapshot = list(),
  runtime = list(),
  traces = list(),
  deliverables = list(),
  status = character(0)
)
```
