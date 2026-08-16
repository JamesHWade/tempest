# Return evidence resources as a tibble

Return evidence resources as a tibble

## Usage

``` r
tempest_sources(store)

tempest_resources(store)
```

## Arguments

- store:

  A
  [ResearchWorkspace](https://jameshwade.github.io/tempest/reference/ResearchWorkspace.md)
  or
  [TempestRetriever](https://jameshwade.github.io/tempest/reference/TempestRetriever.md).

## Value

A tibble with resource identity, kind, opaque locator, optional web URL,
title, media type, content context, retrieval time, and metadata.

## Examples

``` r
if (FALSE) { # \dontrun{
result <- tempest_run("History of jazz", config = tempest_config())
tempest_sources(result$store)
} # }
```
