# Admit a current accepted research decision

Resolve an exact accepted decision through Graft and validate its
research evidence. The host callback is called again when this value
enters
[`tempest_run()`](https://jameshwade.github.io/tempest/reference/tempest_run.md)
or
[`tempest_session()`](https://jameshwade.github.io/tempest/reference/tempest_session.md),
or is supplied to
[`tempest_session_resume()`](https://jameshwade.github.io/tempest/reference/tempest_session_resume.md).
It must return exactly `TRUE` for the current purpose. The callback and
store handle are transient and are never saved in a session. Resume
requires a newly supplied knowledge value.

## Usage

``` r
tempest_reuse_artifact_research(store, stream, decision, purpose, eligible)
```

## Arguments

- store:

  A
  [`graft::graft_artifact_store()`](https://jameshwade.github.io/graft/reference/graft_artifact_store.html)
  handle. Applications enforce access to this trusted local,
  single-writer store.

- stream:

  Graft decision stream.

- decision:

  Exact acceptance event digest, distinct from the selection.

- purpose:

  Requested consultation purpose.

- eligible:

  Host function receiving the recorded decision and returning a single
  logical eligibility decision. Do not cache an earlier permission.

## Value

A `TempestKnowledge` value retaining decision provenance and exact
selected evidence, with no executable procedures.

## Details

This checks admission at a point in time. Hosts own active-task
cancellation, authorization on all read paths and any subsequent policy
changes.
