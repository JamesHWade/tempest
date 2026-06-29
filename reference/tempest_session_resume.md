# Resume a saved Co-STORM session bundle

`tempest_session_resume()` reads a directory bundle written by
[`tempest_session_save()`](https://jameshwade.github.io/tempest/reference/tempest_session_save.md)
and rebuilds a
[TempestSession](https://jameshwade.github.io/tempest/reference/TempestSession.md)
with a fresh runtime
[TempestConfig](https://jameshwade.github.io/tempest/reference/TempestConfig.md).
Historical progress events are loaded for display and reduction, but
they are not replayed into `progress`.

## Usage

``` r
tempest_session_resume(path, config = tempest_config(), progress = NULL)
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

## Value

A restored
[TempestSession](https://jameshwade.github.io/tempest/reference/TempestSession.md).
