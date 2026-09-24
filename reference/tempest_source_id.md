# Create a deterministic source ID from a URL

Use this when a host retriever constructs search results or fetched
sources for
[`tempest_run()`](https://jameshwade.github.io/tempest/reference/tempest_run.md)
or
[`tempest_session()`](https://jameshwade.github.io/tempest/reference/tempest_session.md).
The same trimmed URL always produces the same ID.

## Usage

``` r
tempest_source_id(url)
```

## Arguments

- url:

  A single non-empty HTTP or HTTPS URL string.

## Value

A source ID such as `"Sxxxxxxxxxxxx"`.

## Examples

``` r
tempest_source_id("https://example.org/study")
#> [1] "Sab611bb640d8"
```
