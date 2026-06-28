# Create a Tempest artifact store adapter

Artifact stores let host applications observe or persist Tempest outputs
without replacing the live in-memory `SourceStore`. The default store is
a no-op adapter.

## Usage

``` r
tempest_artifact_store(write = NULL, read = NULL, list_names = NULL)
```

## Arguments

- write:

  Function with signature `function(name, value, metadata)` used to
  persist an artifact.

- read:

  Function with signature `function(name, default)` used to read an
  artifact.

- list_names:

  Function with no arguments that returns available artifact names.

## Value

A list with `write`, `read`, and `list` functions.

## Examples

``` r
store <- tempest_memory_artifact_store()
store$write("report_md", "# Report", metadata = list(topic = "Demo"))
store$read("report_md")
#> [1] "# Report"
```
