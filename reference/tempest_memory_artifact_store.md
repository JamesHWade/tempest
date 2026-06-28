# Create an in-memory Tempest artifact store

This is useful for tests and host apps that want to capture artifacts
before deciding where to persist them.

## Usage

``` r
tempest_memory_artifact_store()
```

## Value

A `tempest_artifact_store`.

## Examples

``` r
store <- tempest_memory_artifact_store()
store$write("summary", "Done")
store$list()
#> [1] "summary"
```
