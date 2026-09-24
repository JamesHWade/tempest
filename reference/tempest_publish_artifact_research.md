# Preserve completed research in a Graft artifact store

Publish the validated proposal, exact source bodies, evidence, program
provenance and readable report as one immutable selection. Publication
is not acceptance. The host reviews the selection and explicitly calls
[`graft::graft_accept()`](https://jameshwade.github.io/graft/reference/graft_accept.html)
to accept it or
[`graft::graft_withdraw()`](https://jameshwade.github.io/graft/reference/graft_withdraw.html)
to withdraw it.

## Usage

``` r
tempest_publish_artifact_research(
  research,
  store,
  claim_ids = NULL,
  report = NULL
)
```

## Arguments

- research:

  A completed research product accepted by
  [`tempest_promotion_bundle()`](https://jameshwade.github.io/tempest/reference/tempest_promotion_bundle.md),
  or a validated `TempestPromotionBundle`.

- store:

  A
  [`graft::graft_store()`](https://jameshwade.github.io/graft/reference/graft_store.html)
  handle. Applications enforce access to this trusted local,
  single-writer store.

- claim_ids:

  Optional exact claim selection, passed to
  [`tempest_promotion_bundle()`](https://jameshwade.github.io/tempest/reference/tempest_promotion_bundle.md).

- report:

  Exact report text when `research` is a promotion bundle. Its digest
  must match the retained completed-product manifest. Omit it for a live
  completed product.

## Value

An immutable Graft selection digest. Identical publication reuses the
same artifacts; corrected contents retain the earlier selection.

## See also

[`tempest_read_artifact_research()`](https://jameshwade.github.io/tempest/reference/tempest_read_artifact_research.md),
[`tempest_reuse_artifact_research()`](https://jameshwade.github.io/tempest/reference/tempest_reuse_artifact_research.md)
