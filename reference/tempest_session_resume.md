# Resume a saved Co-STORM session bundle

`tempest_session_resume()` reads a directory bundle written by
[`tempest_session_save()`](https://jameshwade.github.io/tempest/reference/tempest_session_save.md)
and rebuilds a
[TempestSession](https://jameshwade.github.io/tempest/reference/TempestSession.md)
with a fresh runtime
[TempestConfig](https://jameshwade.github.io/tempest/reference/TempestConfig.md).
Only the exact current schema-9 bundle is accepted; no compatibility or
migration reader is provided. Historical progress events are loaded for
display and reduction, but they are not replayed into `progress`.
Stage-record history is restored for audit, but running attempts are
rejected rather than resumed.

## Usage

``` r
tempest_session_resume(
  path,
  config = tempest_config(),
  progress = NULL,
  program_set = NULL,
  knowledge_view = NULL
)
```

## Arguments

- path:

  Directory containing a session bundle.

- config:

  Runtime
  [TempestConfig](https://jameshwade.github.io/tempest/reference/TempestConfig.md)
  used to recreate chats, retrievers, and tools.

- progress:

  Optional callback for future `tempest_progress_event` objects.

- program_set:

  A
  [TempestProgramSet](https://jameshwade.github.io/tempest/reference/TempestProgramSet.md)
  carrying the same program identities recorded in the bundle. If
  `NULL`, the builtin set is used.

- knowledge_view:

  Optional transient immutable Graft view required by future execution
  when `program_set` contains governed procedures. It is never
  reconstructed from or written to persistence.

## Value

A restored
[TempestSession](https://jameshwade.github.io/tempest/reference/TempestSession.md).
