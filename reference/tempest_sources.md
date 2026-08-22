# Return evidence resources as a tibble

Reports every evidence resource consumed by a product, including
accepted organizational knowledge records supplied through
[`tempest_knowledge()`](https://jameshwade.github.io/tempest/reference/tempest_knowledge.md).

## Usage

``` r
tempest_sources(x)
```

## Arguments

- x:

  A completed
  [`tempest_run()`](https://jameshwade.github.io/tempest/reference/tempest_run.md)
  product or a `TempestSession`.

## Value

A tibble with resource identity, kind, opaque locator, optional web URL,
title, media type, content context, retrieval time, and metadata.

## Examples

``` r
if (FALSE) { # \dontrun{
result <- tempest_run("History of jazz", config = tempest_config())
tempest_sources(result)
} # }
```
