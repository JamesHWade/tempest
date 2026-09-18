# Inspect retained research without admitting it to execution

Verify the complete selected contents and Tempest evidence proof.
Historical inspection does not imply current acceptance or grant reuse
permission. Reports remain model-generated synthesis, separate from
source evidence.

## Usage

``` r
tempest_read_artifact_research(store, selection)
```

## Arguments

- store:

  A
  [`graft::graft_artifact_store()`](https://jameshwade.github.io/graft/reference/graft_artifact_store.html)
  handle. Applications enforce access to this trusted local,
  single-writer store.

- selection:

  Exact digest returned by
  [`tempest_publish_artifact_research()`](https://jameshwade.github.io/tempest/reference/tempest_publish_artifact_research.md).

## Value

A list with `selection`, validated `bundle`, `report_md`, and the exact
evidence `records` and text `contents` used for research admission. The
selection retains program identities as inert provenance, never tools or
execution authority. Evidence is limited to 998 records and 1 MiB of
projected text; Graft also enforces its store and selection bounds.
