# Save a Co-STORM session bundle

**\[experimental\]**

## Usage

``` r
tempest_session_save(session, path, overwrite = FALSE)
```

## Arguments

- session:

  A
  [TempestSession](https://jameshwade.github.io/tempest/reference/TempestSession.md)
  object.

- path:

  Directory where the session bundle should be written.

- overwrite:

  If `TRUE`, replace an existing bundle directory.

## Value

Invisibly returns the normalized bundle directory.

## Details

`tempest_session_save()` writes a schema-versioned directory bundle for
a
[TempestSession](https://jameshwade.github.io/tempest/reference/TempestSession.md).
The bundle stores the research manifest, authoritative workspace,
explicit stage-record history, optional immutable Graft snapshot, and
narrow report product. Every declared file is checksummed, and the
`session.json` manifest is written last. Generic workflow and
artifact-catalog state, live chat handles, registered tool closures,
Shiny reactive state, credentials, and raw provider request bodies are
not serialized. A stage attempt that is still running is written as
cancelled without changing the live session.

Use
[`tempest_session_resume()`](https://jameshwade.github.io/tempest/reference/tempest_session_resume.md)
to load the bundle with a fresh runtime
[TempestConfig](https://jameshwade.github.io/tempest/reference/TempestConfig.md).
