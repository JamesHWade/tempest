# Resume a generic Tempest run bundle

**\[experimental\]**

## Usage

``` r
tempest_run_resume(
  path,
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

- path:

  Tempest run bundle directory.

- runtime:

  Explicit process-local runtime or operation registry.

- artifact_catalog:

  Optional process-local catalog override.

- source_store:

  Optional process-local evidence-store override.

- runtime_context:

  Process-local services to reattach to operations and capability
  factories. These values are never read from disk.

- connection_permissions:

  Optional connection allow-lists that only narrow the saved grants.

- partial_recovery:

  Whether to recover explicitly in-flight step state.

- policy_adapter:

  Optional process-local policy adapter.

- progress:

  Optional process-local event callback.

## Value

A rehydrated `TempestRun`.

## Details

Resume validates the complete file inventory and checksums before
parsing the snapshot. The supplied runtime and adapters are attached
explicitly; they are never loaded from disk. The returned run is
rehydrated but is not executed automatically. Call `$resume()` to
continue eligible steps.
