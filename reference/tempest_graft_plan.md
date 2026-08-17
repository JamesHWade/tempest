# Plan a Tempest research promotion without accepting it

This adapter performs a deterministic read-only seed plan for Source and
Claim identities, rewrites typed references, and returns only the final
reviewable `graft::GraftCommitPlan`. Neither plan is committed.

## Usage

``` r
tempest_graft_plan(store, bundle)
```

## Arguments

- store:

  A writable or read-only `graft::GraftStore` opened with
  [`tempest_graft_schema()`](https://jameshwade.github.io/tempest/reference/tempest_graft_schema.md).

- bundle:

  A
  [`tempest_promotion_bundle()`](https://jameshwade.github.io/tempest/reference/tempest_promotion_bundle.md)
  proposal.

## Value

The final valid `graft::GraftCommitPlan` for explicit host review.
