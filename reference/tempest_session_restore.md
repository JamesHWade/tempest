# Restore a Co-STORM session from a snapshot

**\[experimental\]**

## Usage

``` r
tempest_session_restore(
  snapshot,
  config = tempest_config(),
  runtime = tempest_runtime(),
  connection_permissions = NULL,
  progress = NULL
)
```

## Arguments

- snapshot:

  A list from
  [`tempest_session_snapshot()`](https://jameshwade.github.io/tempest/reference/tempest_session_snapshot.md).

- config:

  Runtime
  [TempestConfig](https://jameshwade.github.io/tempest/reference/TempestConfig.md)
  used to recreate chats, retrievers, and tools. Functions, credentials,
  and host-specific stores should be supplied here rather than read from
  the snapshot.

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

## Value

A restored
[TempestSession](https://jameshwade.github.io/tempest/reference/TempestSession.md).

## Details

`tempest_session_restore()` rebuilds a
[TempestSession](https://jameshwade.github.io/tempest/reference/TempestSession.md)
from a structured snapshot created by
[`tempest_session_snapshot()`](https://jameshwade.github.io/tempest/reference/tempest_session_snapshot.md)
or read by
[`tempest_session_resume()`](https://jameshwade.github.io/tempest/reference/tempest_session_resume.md).
It restores durable research state and creates fresh chat/tool handles
using the supplied runtime and `config`.

Historical progress events are restored as session artifact data and can
be reduced with
[`tempest_progress_state()`](https://jameshwade.github.io/tempest/reference/tempest_progress_state.md).
They are not replayed into the new `progress` callback; future calls on
the restored session use that callback.
