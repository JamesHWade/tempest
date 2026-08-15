# Create an in-memory Tempest artifact store

**\[experimental\]**

## Usage

``` r
tempest_memory_artifact_store()
```

## Value

A `tempest_artifact_store`.

## Details

This experimental API is frozen and scheduled for removal in Tempest
0.2.0. No compatibility shim is planned; see
[tempest-generic-kernel-retirement](https://jameshwade.github.io/tempest/reference/tempest-generic-kernel-retirement.md).

This is useful for tests and host apps that want to capture artifacts
before deciding where to persist them.

## Examples

``` r
store <- tempest_memory_artifact_store()
# Stores accept typed artifacts produced by a deliverable lifecycle.
store$list()
#> named list()
```
