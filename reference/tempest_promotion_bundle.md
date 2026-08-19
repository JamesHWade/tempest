# Build a deterministic proposal for reviewed Graft promotion

`tempest_promotion_bundle()` validates a completed research product and
packages only promotable claims and their exact pair-level evidence. It
does not write accepted knowledge. Review
[`tempest_graft_plan()`](https://jameshwade.github.io/tempest/reference/tempest_graft_plan.md)
and call
[`graft::graft_commit()`](https://jameshwade.github.io/graft/reference/graft_commit.html)
explicitly to exercise acceptance authority. Loose Workspace, Manifest,
or StageRecord tuples are not accepted. The promotion payload remains
the exact schema-1 evidence-only product shape; the terminal report is
an eligibility gate and is not copied into it.

## Usage

``` r
tempest_promotion_bundle(research, claim_ids = NULL)
```

## Arguments

- research:

  A completed result returned by
  [`tempest_run()`](https://jameshwade.github.io/tempest/reference/tempest_run.md)
  or a succeeded `TempestSession` returned by
  [`tempest_session()`](https://jameshwade.github.io/tempest/reference/tempest_session.md).
  Tempest requires the product's exact sealed Workspace, terminal
  execution records, committed report, configuration, and publication
  authority.

- claim_ids:

  Optional exact claim selection. By default all supported or partially
  supported claims are selected. Selection must be closed over every
  output bound by each retained extraction and verification record;
  Tempest rejects a partial stage-output selection.

## Value

A deterministic `TempestPromotionBundle` proposal.
