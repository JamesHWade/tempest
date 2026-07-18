# Create a registry for Tempest's built-in research workflows

**\[experimental\]**

## Usage

``` r
tempest_builtin_workflow_operation_registry(
  storm_adapter = NULL,
  costorm_adapter = NULL,
  registry = tempest_builtin_operation_registry()
)
```

## Arguments

- storm_adapter:

  Optional function created by
  [`tempest_storm_workflow_adapter()`](https://jameshwade.github.io/tempest/reference/tempest_storm_workflow_adapter.md)
  or a compatible function accepting `stage`, `context`, and `run`.

- costorm_adapter:

  Optional function created by
  [`tempest_costorm_workflow_adapter()`](https://jameshwade.github.io/tempest/reference/tempest_costorm_workflow_adapter.md)
  or a compatible function accepting `stage`, `context`, and `run`.

- registry:

  Optional operation registry to extend. By default the built-in
  deliverable operations are included.

## Value

A `TempestOperationRegistry`.

## Details

The returned registry resolves every operation used by
[`tempest_storm_workflow_spec()`](https://jameshwade.github.io/tempest/reference/tempest_storm_workflow_spec.md)
and
[`tempest_costorm_workflow_spec()`](https://jameshwade.github.io/tempest/reference/tempest_costorm_workflow_spec.md).
Adapters bind the serializable definitions to process-local STORM or
Co-STORM execution state.
