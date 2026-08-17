# Resume a saved Co-STORM session bundle

`tempest_session_resume()` reads a directory bundle written by
[`tempest_session_save()`](https://jameshwade.github.io/tempest/reference/tempest_session_save.md)
and rebuilds a
[TempestSession](https://jameshwade.github.io/tempest/reference/TempestSession.md)
with a fresh runtime
[TempestConfig](https://jameshwade.github.io/tempest/reference/TempestConfig.md).
Historical progress events are loaded for display and reduction, but
they are not replayed into `progress`. Stage-record history is restored
for audit, but running attempts are rejected rather than resumed.

## Usage

``` r
tempest_session_resume(
  path,
  config = tempest_config(),
  progress = NULL,
  partial_recovery = FALSE,
  program_set = NULL
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

- partial_recovery:

  Whether to allow explicitly requested recovery when allowlisted
  presentation files are missing or fail integrity checks. All other
  declared files, including stage-record, expert, workspace, report, and
  Graft snapshot state, must pass integrity checks.

- program_set:

  A
  [TempestProgramSet](https://jameshwade.github.io/tempest/reference/TempestProgramSet.md)
  carrying the same program identities recorded in the bundle. If
  `NULL`, the builtin set is used.

## Value

A restored
[TempestSession](https://jameshwade.github.io/tempest/reference/TempestSession.md).
