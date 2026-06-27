# Run STORM asynchronously (Shiny-friendly)

This is a thin wrapper around
[`tempest_run()`](https://jameshwade.github.io/tempest/reference/tempest_run.md)
that returns a promise. It is useful when calling STORM from a Shiny app
without blocking the session.

## Usage

``` r
tempest_run_async(...)
```

## Arguments

- ...:

  Arguments passed to
  [`tempest_run()`](https://jameshwade.github.io/tempest/reference/tempest_run.md).
  See
  [`tempest_run()`](https://jameshwade.github.io/tempest/reference/tempest_run.md)
  for details on available parameters including `topic`, `config`,
  `retriever`, `n_experts`, `research_strategy`, `max_rounds`, `steps`,
  and `verbose`.

## Value

A
[`promises::promise`](https://rstudio.github.io/promises/reference/promise.html)
that resolves with the tempest_run result.

## See also

[`tempest_run()`](https://jameshwade.github.io/tempest/reference/tempest_run.md)
for the synchronous version.

## Examples

``` r
if (FALSE) { # \dontrun{
tempest_run_async("History of jazz", config = tempest_config()) |>
  promises::then(function(result) cat(result$report_md))
} # }
```
