# A bounded review of one completed Tempest product

**\[experimental\]**

Builds a deterministic, read-only projection of the exact execution,
program, knowledge, and evidence identities retained by a completed
STORM or Co-STORM product. Correlation identifiers are grouping evidence
only; they never establish causation. The returned projection is
reconstructable in memory and is not a persistence or acceptance
authority.

The value contains exactly `schema_version`, `review_id`, `product`,
`stages`, `agent_runs`, `programs`, `knowledge`, `evidence`, `joins`,
and `findings`. The `stages` lane retains authoritative StageRecord
order. The `agent_runs`, `evidence`, `joins`, and `findings` lanes are
canonical sets; accepted promotion revisions use the same canonical
envelope beneath `knowledge$acceptance$record_revisions`. Every variable
lane contains exactly `total`, `retained`, `omitted`, `digest`, and
`items`, retains at most 250 items, and binds the complete lane digest.
Mutable progress events, prompts, responses, source content, paths,
credentials, capabilities, and live objects are excluded.

Joins distinguish authority-validated bindings, exact identity, and
correlation-only grouping. A `correlation_id` can support only a
`correlated_with` relation and never claims causation or authorship. An
exact promotion bundle adds proposed state; its exact matching receipt
adds accepted state. A receipt alone or a cross-product combination is
rejected.

## Usage

``` r
tempest_trajectory_review(
  research,
  promotion_bundle = NULL,
  promotion_receipt = NULL
)
```

## Arguments

- research:

  One exact completed result returned by
  [`tempest_run()`](https://jameshwade.github.io/tempest/reference/tempest_run.md)
  or a succeeded, quiescent `TempestSession` returned by
  [`tempest_session()`](https://jameshwade.github.io/tempest/reference/tempest_session.md).

- promotion_bundle:

  Optional exact bundle returned by
  [`tempest_promotion_bundle()`](https://jameshwade.github.io/tempest/reference/tempest_promotion_bundle.md).

- promotion_receipt:

  Optional exact receipt returned by
  [`tempest_promotion_receipt()`](https://jameshwade.github.io/tempest/reference/tempest_promotion_receipt.md).
  Requires `promotion_bundle`.

## Value

A validated `TempestTrajectoryReview` S7 value containing the closed
ten-field bounded projection. The class is internal and the review is
not persisted.

## Examples

``` r
if (FALSE) { # \dontrun{
result <- tempest_run("Grid-scale battery recycling")
review <- tempest_trajectory_review(result)
review@product
review@stages
} # }
```
