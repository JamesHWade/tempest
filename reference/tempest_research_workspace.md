# Create a provisional research workspace

`tempest_research_workspace()` creates the run-scoped ledger for
material gathered or proposed during scientific research. Accepted
knowledge remains in Graft; this workspace retains exact artifact
selection references and materialized evidence for inspection. Reuse
requires fresh host admission.

## Usage

``` r
tempest_research_workspace(max_sources = Inf, artifact_selection = list())
```

## Arguments

- max_sources:

  Maximum number of unique resources admitted.

- artifact_selection:

  Exact artifact input retained by the workspace.

## Value

A
[ResearchWorkspace](https://jameshwade.github.io/tempest/reference/ResearchWorkspace.md)
object.
