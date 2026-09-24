# Create a provisional research workspace

`tempest_research_workspace()` creates the run-scoped ledger for
material gathered or proposed during scientific research. Accepted
knowledge remains in its authoritative store; this workspace retains
exact selection references and materialized evidence for inspection.
Reuse requires fresh host admission.

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
object. Supply it as a custom retriever's `$workspace` so Tempest and
the host write to the same research ledger.

## Examples

``` r
workspace <- tempest_research_workspace(max_sources = 10L)
inherits(workspace, "ResearchWorkspace")
#> [1] TRUE
```
