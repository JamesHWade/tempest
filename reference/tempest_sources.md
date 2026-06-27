# Return sources as a tibble

Return sources as a tibble

## Usage

``` r
tempest_sources(store)
```

## Arguments

- store:

  A `SourceStore` or `TempestRetriever`.

## Value

A tibble of sources with columns: id, url, title, snippet, content_text,
fetched_at.

## Examples

``` r
if (FALSE) { # \dontrun{
result <- tempest_run("History of jazz", config = tempest_config())
tempest_sources(result$store)
} # }
```
