# Create a provisional research workspace

`tempest_research_workspace()` creates the run-scoped ledger for
material gathered or proposed during scientific research. Accepted
knowledge remains in graft; this workspace stores only opaque references
to accepted records.

## Usage

``` r
tempest_research_workspace(
  base_snapshot_id = NULL,
  max_sources = Inf,
  accepted_graft_references = list()
)
```

## Arguments

- base_snapshot_id:

  Optional opaque identifier for the pinned accepted knowledge snapshot.

- max_sources:

  Maximum number of unique resources admitted.

- accepted_graft_references:

  Unnamed list of canonical JSON-compatible references to accepted graft
  records.

## Value

A
[ResearchWorkspace](https://jameshwade.github.io/tempest/reference/ResearchWorkspace.md)
object.
