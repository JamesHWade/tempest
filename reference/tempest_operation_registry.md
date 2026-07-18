# Create a Tempest runtime operation registry

**\[experimental\]**

## Usage

``` r
tempest_operation_registry(operations = list())
```

## Arguments

- operations:

  Optional list of named function shorthand entries or descriptor lists.
  A descriptor contains `id`, `implementation`, `version`, `kind`, and
  optional serializable `metadata`.

## Value

A mutable runtime registry with `register()`, `resolve()`, `describe()`,
`has()`, and [`list()`](https://rdrr.io/r/base/list.html) methods.

## Details

The registry keeps executable functions outside serializable workflow
and deliverable specifications. Operations are resolved by stable id,
version, and kind when a run is rehydrated.

## Examples

``` r
registry <- tempest_operation_registry(list(
  summarize = list(
    kind = "generator",
    implementation = function(context) context
  )
))
registry$has("summarize", kind = "generator")
#> [1] TRUE
```
