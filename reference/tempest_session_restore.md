# Restore a Co-STORM session from a snapshot

**\[experimental\]**

## Usage

``` r
tempest_session_restore(
  snapshot,
  config = tempest_config(),
  progress = NULL,
  program_set = NULL,
  knowledge_view = NULL
)
```

## Arguments

- snapshot:

  A list from
  [`tempest_session_snapshot()`](https://jameshwade.github.io/tempest/reference/tempest_session_snapshot.md).

- config:

  Runtime
  [TempestConfig](https://jameshwade.github.io/tempest/reference/TempestConfig.md)
  used to recreate chats, retrievers, and tools.

- progress:

  Optional callback for future `tempest_progress_event` objects.

- program_set:

  A
  [TempestProgramSet](https://jameshwade.github.io/tempest/reference/TempestProgramSet.md)
  carrying the same program identities recorded in the snapshot. If
  `NULL`, the builtin set is used.

- knowledge_view:

  Optional transient immutable Graft view required by future execution
  when `program_set` contains governed procedures. It is never
  reconstructed from or written to persistence.

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
It restores the research manifest and authoritative workspace, and
creates fresh chat/tool handles using `config`. Only the exact current
schema-9 snapshot is accepted; older, future, missing, extra, coerced,
or mismatched shapes are rejected rather than migrated.

Historical progress events are restored as session artifact data and can
be reduced with
[`tempest_progress_state()`](https://jameshwade.github.io/tempest/reference/tempest_progress_state.md).
They are not replayed into the new `progress` callback; future calls on
the restored session use that callback. Stage-record history is restored
for audit, but running attempts are rejected rather than resumed.
