# Create a STORM configuration

Create a STORM configuration

## Usage

``` r
tempest_config(...)
```

## Arguments

- ...:

  Passed to `TempestConfig$new()`.

## Value

A `TempestConfig` R6 object.

## Examples

``` r
cfg <- tempest_config()
cfg <- tempest_config(search_provider = "wikipedia")
```
