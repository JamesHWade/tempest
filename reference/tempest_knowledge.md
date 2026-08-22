# Bring accepted organizational knowledge into a Tempest run

`tempest_knowledge()` is the one strict constructor for accepted
organizational knowledge. It pins an immutable Graft view, materializes
an exact allowlist of accepted evidence records, and optionally binds
accepted governed procedures to Tempest stages.

## Usage

``` r
tempest_knowledge(
  graft_view,
  record_ids = character(),
  governed_procedures = list()
)
```

## Arguments

- graft_view:

  A pinned `GraftView` from
  [`graft::graft_at()`](https://jameshwade.github.io/graft/reference/graft_at.html).

- record_ids:

  Character vector of accepted record ids to read as evidence. Only
  `Claim`, `ClaimSupport`, `EvidenceSpan`, and `Source` records are
  readable.

- governed_procedures:

  Optional named list mapping an exact Tempest stage to an accepted
  `GovernedProcedure` record id.

## Value

A validated `TempestKnowledge` value for
[`tempest_run()`](https://jameshwade.github.io/tempest/reference/tempest_run.md)
and
[`tempest_session()`](https://jameshwade.github.io/tempest/reference/tempest_session.md).

## Details

Accepted record text is evidence, not instruction. It is carried in a
data channel and can never change prompts, message roles, tools,
governed procedure selection, or executable artifacts. Executable
authority comes only from an explicit `governed_procedures` stage
binding.

## Examples

``` r
if (FALSE) { # \dontrun{
view <- graft::graft_at(store, graft::graft_snapshot(store))
records <- graft::graft_find(view, "battery recycling", limit = 25)
knowledge <- tempest_knowledge(view, record_ids = records$id)
result <- tempest_run("Battery recycling", knowledge = knowledge)
} # }
```
