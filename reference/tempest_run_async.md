# Run STORM asynchronously (Shiny-friendly)

This runs
[`tempest_run()`](https://jameshwade.github.io/tempest/reference/tempest_run.md)
in a Mirai worker and returns a promise immediately. Use
[`tempest_run_cancel()`](https://jameshwade.github.io/tempest/reference/tempest_run_cancel.md)
to stop a run that is no longer needed.

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

A `tempest_async_run` promise that resolves with the
[`tempest_run()`](https://jameshwade.github.io/tempest/reference/tempest_run.md)
result.

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
