# Record exact accepted revisions for a committed promotion plan

`tempest_promotion_receipt()` verifies the commit summary, captures the
immediate immutable Graft snapshot, reopens it through `graft_at()`, and
checks every planned record and current revision before returning a
receipt.

## Usage

``` r
tempest_promotion_receipt(store, bundle, plan, commit_result)
```

## Arguments

- store:

  The open Graft store used for the commit.

- bundle:

  The exact Tempest promotion bundle.

- plan:

  The reviewed plan returned by
  [`tempest_graft_plan()`](https://jameshwade.github.io/tempest/reference/tempest_graft_plan.md).

- commit_result:

  The ordinary list returned by
  [`graft::graft_commit()`](https://jameshwade.github.io/graft/reference/graft_commit.html).

## Value

A validated `TempestPromotionReceipt`.
