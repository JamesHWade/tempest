# Bring accepted organizational knowledge into a Tempest run

`tempest_knowledge()` is the Graft constructor for accepted
organizational knowledge. It pins an immutable Graft view, materializes
an exact allowlist of accepted evidence records, and keeps those records
separate from executable research programs.

## Usage

``` r
tempest_knowledge(graft_view, record_ids = character())
```

## Arguments

- graft_view:

  A pinned `GraftView` from
  [`graft::graft_at()`](https://jameshwade.github.io/graft/reference/graft_at.html).

- record_ids:

  Character vector of accepted record ids to read as evidence. Only
  `Claim`, `ClaimSupport`, `EvidenceSpan`, and `Source` records are
  readable.

## Value

A validated `TempestKnowledge` value for
[`tempest_run()`](https://jameshwade.github.io/tempest/reference/tempest_run.md)
and
[`tempest_session()`](https://jameshwade.github.io/tempest/reference/tempest_session.md).

## Details

Accepted record text is evidence, not instruction. It is carried in a
data channel and can never change prompts, message roles, tools,
governed procedure selection, or executable artifacts. The host selects
research programs independently of stored knowledge.

## Examples

``` r
if (FALSE) { # \dontrun{
# Retain complete selected claim, support, span and source IDs from
# host-accepted promotion receipts, not only matching claim IDs.
source(system.file("examples", "briefing-basis.R", package = "tempest"))
basis <- capture_briefing_basis(store, list(briefing_selection(receipt)))
saveRDS(basis, "accepted-basis.rds")
knowledge <- read_briefing_basis(store, basis)
result <- tempest_run("Battery recycling", knowledge = knowledge)
} # }
```
