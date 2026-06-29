# Create an in-memory Tempest artifact store

**\[experimental\]**

## Usage

``` r
tempest_memory_artifact_store()
```

## Value

A `tempest_artifact_store`.

## Details

This is useful for tests and host apps that want to capture artifacts
before deciding where to persist them.

## Examples

``` r
store <- tempest_memory_artifact_store()
store$write("summary", "Done")
store$list()
#> [1] "summary"
```
