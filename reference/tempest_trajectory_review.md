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
canonical sets; input artifact identities use the same canonical
envelope beneath `knowledge$input_selection$records`. Every variable
lane contains exactly `total`, `retained`, `omitted`, `digest`, and
`items`, retains at most 250 items, and binds the complete lane digest.
Mutable progress events, prompts, responses, source content, paths,
credentials, capabilities, and live objects are excluded.

Joins distinguish authority-validated bindings, exact identity, and
correlation-only grouping. A `correlation_id` can support only a
`correlated_with` relation and never claims causation or authorship. An
exact promotion bundle or verified publication adds proposed state. An
exact recorded accept decision adds historical accepted state. This
remains inspectable after withdrawal and never grants current reuse
permission. Input selection identities and their full metadata digest
remain distinct from output publication. Input `reported_decision` is
unverified host provenance, never authenticated acceptance; malformed
metadata is omitted. Decision actor, reason, key and other arbitrary
input provenance are excluded. Schema 3 retains artifact inputs and
directly replaces native snapshot and receipt-based reviews.

## Usage

``` r
tempest_trajectory_review(
  research,
  promotion_bundle = NULL,
  store = NULL,
  selection = NULL,
  stream = NULL,
  decision = NULL
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

- store:

  Artifact store used to verify an output publication. Required with
  `selection`. The store is never retained in the review.

- selection:

  Exact output selection returned by
  [`tempest_publish_artifact_research()`](https://jameshwade.github.io/tempest/reference/tempest_publish_artifact_research.md).
  The retained bundle and report must match this completed research
  product.

- stream:

  Decision stream containing `decision`. Supply both together.

- decision:

  Exact historical accept decision digest. Omit to inspect a publication
  without asserting acceptance. No latest-decision lookup or current
  admission check is performed.

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
