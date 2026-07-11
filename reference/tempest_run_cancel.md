# Cancel an asynchronous STORM run

Stops the Mirai worker owned by a promise returned from
[`tempest_run_async()`](https://jameshwade.github.io/tempest/reference/tempest_run_async.md).
Cancellation is idempotent after a run has settled.

## Usage

``` r
tempest_run_cancel(run)
```

## Arguments

- run:

  A `tempest_async_run` returned by
  [`tempest_run_async()`](https://jameshwade.github.io/tempest/reference/tempest_run_async.md).

## Value

Invisibly returns `TRUE` when cancellation was requested and `FALSE`
when the run had already settled.
