# Restore generic Tempest run state

**\[experimental\]**

## Usage

``` r
tempest_run_restore(
  snapshot,
  runtime,
  artifact_catalog = NULL,
  source_store = NULL,
  runtime_context = list(),
  connection_permissions = NULL,
  partial_recovery = FALSE,
  policy_adapter = NULL,
  progress = NULL
)
```

## Arguments

- snapshot:

  A record from
  [`tempest_run_snapshot()`](https://jameshwade.github.io/tempest/reference/tempest_run_snapshot.md).

- runtime:

  Explicit process-local runtime.

- artifact_catalog:

  Optional restored catalog override.

- source_store:

  Optional restored evidence-store override.

- runtime_context:

  Process-local services to reattach after restore.

- connection_permissions:

  Optional restored connection allow-lists. Defaults to the saved
  permissions. Explicit overrides may only narrow saved grants.

- partial_recovery:

  Whether to recover an explicitly in-flight snapshot by resetting
  running steps to pending. Defaults to `FALSE`.

- policy_adapter:

  Optional process-local policy adapter.

- progress:

  Optional generic event callback.

## Value

A rehydrated `TempestRun`. Call `$resume()` explicitly to continue.

## Details

This experimental API is frozen and scheduled for removal in Tempest
0.2.0. No compatibility shim is planned; see
[tempest-generic-kernel-retirement](https://jameshwade.github.io/tempest/reference/tempest-generic-kernel-retirement.md).
