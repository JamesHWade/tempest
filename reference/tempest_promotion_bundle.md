# Build a deterministic proposal for reviewed Graft promotion

`tempest_promotion_bundle()` validates a completed research product and
packages only promotable claims and their exact pair-level evidence. It
does not write accepted knowledge. Review
[`tempest_graft_plan()`](https://jameshwade.github.io/tempest/reference/tempest_graft_plan.md)
and call
[`graft::graft_commit()`](https://jameshwade.github.io/graft/reference/graft_commit.html)
explicitly to exercise acceptance authority.

## Usage

``` r
tempest_promotion_bundle(workspace, manifest, stage_records, claim_ids = NULL)
```

## Arguments

- workspace:

  A completed
  [ResearchWorkspace](https://jameshwade.github.io/tempest/reference/ResearchWorkspace.md).

- manifest:

  Its succeeded
  [TempestResearchManifest](https://jameshwade.github.io/tempest/reference/TempestResearchManifest.md).

- stage_records:

  Canonically ordered terminal `TempestStageRecord` values for the
  research product.

- claim_ids:

  Optional exact claim selection. By default all supported or partially
  supported claims are selected. Selection must be closed over every
  output bound by each retained extraction and verification record;
  Tempest rejects a partial stage-output selection.

## Value

A deterministic `TempestPromotionBundle` proposal.
