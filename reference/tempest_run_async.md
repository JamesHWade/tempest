# Run STORM asynchronously (Shiny-friendly)

This runs
[`tempest_run()`](https://jameshwade.github.io/tempest/reference/tempest_run.md)
in a Mirai worker and returns a promise immediately. Use
[`tempest_run_cancel()`](https://jameshwade.github.io/tempest/reference/tempest_run_cancel.md)
to stop a run that is no longer needed.

## Usage

``` r
tempest_run_async(..., knowledge_view = NULL)
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

- knowledge_view:

  Accepted knowledge and a live pinned Graft view cannot cross the
  asynchronous worker boundary. Governed runs must use
  [`tempest_run()`](https://jameshwade.github.io/tempest/reference/tempest_run.md)
  in the process that owns the view.

## Value

A `tempest_async_run` promise that resolves with the
[`tempest_run()`](https://jameshwade.github.io/tempest/reference/tempest_run.md)
result.

## See also

[`tempest_run()`](https://jameshwade.github.io/tempest/reference/tempest_run.md)
for the synchronous version.
