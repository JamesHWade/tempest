# Create the built-in STORM workflow specification

**\[experimental\]**

## Usage

``` r
tempest_storm_workflow_spec()
```

## Value

A `tempest_workflow_spec`.

## Details

The specification declares the five durable STORM stages. Executable
operations are supplied by
[`tempest_builtin_workflow_operation_registry()`](https://jameshwade.github.io/tempest/reference/tempest_builtin_workflow_operation_registry.md).

## Examples

``` r
workflow <- tempest_storm_workflow_spec()
names(workflow@steps)
#> [1] "perspectives" "research"     "outline"      "write"        "polish"      
```
