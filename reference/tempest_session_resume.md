# Resume a saved Co-STORM session bundle

`tempest_session_resume()` reads a directory bundle written by
[`tempest_session_save()`](https://jameshwade.github.io/tempest/reference/tempest_session_save.md)
and rebuilds a
[TempestSession](https://jameshwade.github.io/tempest/reference/TempestSession.md)
with a fresh runtime
[TempestConfig](https://jameshwade.github.io/tempest/reference/TempestConfig.md)
and
[`tempest_runtime()`](https://jameshwade.github.io/tempest/reference/tempest_runtime.md).
Historical progress events are loaded for display and reduction, but
they are not replayed into `progress`.

## Usage

``` r
tempest_session_resume(
  path,
  config = tempest_config(),
  runtime = tempest_runtime(),
  connection_permissions = NULL,
  progress = NULL,
  partial_recovery = FALSE,
  codec_registry = NULL
)
```

## Arguments

- path:

  Directory containing a session bundle.

- config:

  Runtime
  [TempestConfig](https://jameshwade.github.io/tempest/reference/TempestConfig.md)
  used to recreate chats, retrievers, and tools.

- runtime:

  A fresh
  [`tempest_runtime()`](https://jameshwade.github.io/tempest/reference/tempest_runtime.md)
  supplying process-local skill, capability, and connection
  implementations.

- connection_permissions:

  Optional named connection-permission override that may only remove
  saved contexts or connection ids. When `NULL`, the saved opaque
  connection ids are reused.

- progress:

  Optional callback for future `tempest_progress_event` objects.

- partial_recovery:

  Whether to allow explicitly requested recovery when allowlisted
  presentation files are missing or fail integrity checks. All other
  declared files, including workflow, permission, grant, expert,
  runtime, workspace, and typed-artifact state, must pass integrity
  checks.

- codec_registry:

  Optional
  [`tempest_artifact_codec_registry()`](https://jameshwade.github.io/tempest/reference/tempest_artifact_codec_registry.md)
  containing host-defined codecs needed to decode typed artifact
  content.

## Value

A restored
[TempestSession](https://jameshwade.github.io/tempest/reference/TempestSession.md).
