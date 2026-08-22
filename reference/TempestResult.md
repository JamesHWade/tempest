# Completed Tempest research product

The validated value returned by
[`tempest_run()`](https://jameshwade.github.io/tempest/reference/tempest_run.md).
Read it with
[`tempest_report()`](https://jameshwade.github.io/tempest/reference/tempest_report.md),
[`tempest_sources()`](https://jameshwade.github.io/tempest/reference/tempest_sources.md),
[`tempest_claims()`](https://jameshwade.github.io/tempest/reference/tempest_claims.md),
[`tempest_claim_supports()`](https://jameshwade.github.io/tempest/reference/tempest_claim_supports.md),
[`tempest_trajectory_review()`](https://jameshwade.github.io/tempest/reference/tempest_trajectory_review.md),
and the promotion functions. Retrievers, mutable workspaces, manifests,
and stage state stay internal to validation, persistence, telemetry, and
promotion.

## Usage

``` r
TempestResult(
  title = NA_character_,
  topic = NA_character_,
  run_id = NA_character_,
  status = NA_character_,
  report_md = NULL,
  output_dir = NULL,
  perspectives = list(),
  experts = list(),
  outline = NULL,
  draft_md = NULL,
  manifest = NULL,
  state = list(),
  workspace = NULL,
  retriever = NULL
)
```
