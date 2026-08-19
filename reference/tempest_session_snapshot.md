# Snapshot a Co-STORM session

**\[experimental\]**

## Usage

``` r
tempest_session_snapshot(session)
```

## Arguments

- session:

  A
  [TempestSession](https://jameshwade.github.io/tempest/reference/TempestSession.md)
  object.

## Value

A list containing an exact schema-9 session snapshot.

## Details

`tempest_session_snapshot()` returns a structured, in-memory
representation of the durable state in a
[TempestSession](https://jameshwade.github.io/tempest/reference/TempestSession.md).
The only supported snapshot is the exact current schema-9 product shape;
no legacy or migration reader is provided. It includes the research
manifest; fixed session and configuration identity; the authoritative
[ResearchWorkspace](https://jameshwade.github.io/tempest/reference/ResearchWorkspace.md);
expert profiles; transcript and mind map; the latest report Markdown;
stage-record, progress-event, and expert-session metadata; and the
optional immutable Graft snapshot. Live chat handles, runtime clients,
tools, closures, generic workflows, generic artifact catalogs, Shiny
reactive state, credentials, and provider request bodies are not
included.

Use
[`tempest_session_restore()`](https://jameshwade.github.io/tempest/reference/tempest_session_restore.md)
to rebuild a session from the returned list, or
[`tempest_session_save()`](https://jameshwade.github.io/tempest/reference/tempest_session_save.md)
to write the same durable state to a directory bundle. A stage attempt
that is still running is projected to a cancelled durable record without
changing the live session.
