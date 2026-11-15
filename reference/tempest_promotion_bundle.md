# Build a deterministic proposal for reviewed Graft promotion

`tempest_promotion_bundle()` validates a completed research product and
packages promotable claims and their exact pair-level evidence, with
advisory claim reviews and their retained source dossiers. Reviews
remain separate from scientific claims and claim support; they do not
change promotion eligibility. It does not write accepted knowledge.
Publish with
[`tempest_publish_artifact_research()`](https://jameshwade.github.io/tempest/reference/tempest_publish_artifact_research.md)
and call
[`graft::graft_accept()`](https://jameshwade.github.io/graft/reference/graft_accept.html)
after host review. Loose Workspace, Manifest, or StageRecord tuples are
not accepted. The schema-2 payload retains complete review context while
keeping the terminal report as an eligibility gate rather than copying
it.

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
  Tempest rejects a partial stage-output selection. Advisory reviews for
  claims in the selected stage-bound proof are retained; reviews of
  other claims remain with the complete research product.

## Value

A deterministic `TempestPromotionBundle` proposal.
