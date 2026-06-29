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

A list containing a schema-versioned session snapshot.

## Details

`tempest_session_snapshot()` returns a structured, in-memory
representation of the durable state in a
[TempestSession](https://jameshwade.github.io/tempest/reference/TempestSession.md).
It includes the session identity, personas, transcript, mind map, report
artifacts, progress-event history, expert-session metadata, and the
underlying `SourceStore` ledger. Live chat handles, tools, Shiny
reactive state, credentials, and provider request bodies are not
included.

Use
[`tempest_session_restore()`](https://jameshwade.github.io/tempest/reference/tempest_session_restore.md)
to rebuild a session from the returned list, or
[`tempest_session_save()`](https://jameshwade.github.io/tempest/reference/tempest_session_save.md)
to write the same durable state to a directory bundle.
