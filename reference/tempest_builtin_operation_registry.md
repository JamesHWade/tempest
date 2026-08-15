# Create a registry containing Tempest's built-in deliverable operations

**\[experimental\]**

## Usage

``` r
tempest_builtin_operation_registry()
```

## Value

A `TempestOperationRegistry`.

## Details

This experimental API is frozen and scheduled for removal in Tempest
0.2.0. No compatibility shim is planned; see
[tempest-generic-kernel-retirement](https://jameshwade.github.io/tempest/reference/tempest-generic-kernel-retirement.md).

Hosts can register additional operations or explicitly replace a
built-in operation on the returned registry.

The built-in ids are `tempest.generator.markdown_report`,
`tempest.generator.provided_content`,
`tempest.validator.required_fields`, `tempest.renderer.markdown`,
`tempest.renderer.markdown_report`, and `tempest.exporter.markdown`.

## Examples

``` r
registry <- tempest_builtin_operation_registry()
names(registry$list())
#> [1] "tempest.exporter.markdown"          "tempest.generator.markdown_report" 
#> [3] "tempest.generator.provided_content" "tempest.renderer.markdown"         
#> [5] "tempest.renderer.markdown_report"   "tempest.validator.required_fields" 
```
