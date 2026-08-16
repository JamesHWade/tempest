# Create a provisional research workspace

`tempest_research_workspace()` creates the run-scoped ledger for
material gathered or proposed during scientific research. Accepted
knowledge remains in graft; this workspace retains opaque record
references and, when pinned, the path-free immutable Graft snapshot
needed to reopen that boundary.

## Usage

``` r
tempest_research_workspace(
  base_snapshot_id = NULL,
  graft_snapshot = NULL,
  max_sources = Inf,
  accepted_graft_references = list()
)
```

## Arguments

- base_snapshot_id:

  Optional opaque identifier for the pinned accepted knowledge snapshot.

- graft_snapshot:

  Optional real, path-free `graft::GraftSnapshot`.

- max_sources:

  Maximum number of unique resources admitted.

- accepted_graft_references:

  Unnamed list of canonical JSON-compatible references to accepted graft
  records.

## Value

A
[ResearchWorkspace](https://jameshwade.github.io/tempest/reference/ResearchWorkspace.md)
object.
