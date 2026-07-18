# Save a Co-STORM session bundle

**\[experimental\]**

## Usage

``` r
tempest_session_save(session, path, overwrite = FALSE, codec_registry = NULL)
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

- codec_registry:

  Optional
  [`tempest_artifact_codec_registry()`](https://jameshwade.github.io/tempest/reference/tempest_artifact_codec_registry.md)
  containing host-defined codecs needed to encode typed artifact
  content.

## Value

Invisibly returns the normalized bundle directory.

## Details

`tempest_session_save()` writes a schema-versioned directory bundle for
a
[TempestSession](https://jameshwade.github.io/tempest/reference/TempestSession.md).
The bundle stores durable research state plus a typed artifact catalog.
Inline artifacts use explicit UTF-8 or canonical JSON codecs, every
declared file is checksummed, and the `session.json` manifest is written
last. Live chat handles, registered tool closures, Shiny reactive state,
credentials, and raw provider request bodies are not serialized.

Use
[`tempest_session_resume()`](https://jameshwade.github.io/tempest/reference/tempest_session_resume.md)
to load the bundle with a fresh runtime
[TempestConfig](https://jameshwade.github.io/tempest/reference/TempestConfig.md)
and
[`tempest_runtime()`](https://jameshwade.github.io/tempest/reference/tempest_runtime.md).
