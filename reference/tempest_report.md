# Read the committed Markdown report from a Tempest product

`tempest_report()` returns the exact authoritative Markdown already
committed by a completed
[`tempest_run()`](https://jameshwade.github.io/tempest/reference/tempest_run.md)
product or a finalized
[`tempest_session()`](https://jameshwade.github.io/tempest/reference/tempest_session.md).
It never generates, repairs, or republishes a report, and it fails when
the product is not published.

## Usage

``` r
tempest_report(x)
```

## Arguments

- x:

  A completed
  [`tempest_run()`](https://jameshwade.github.io/tempest/reference/tempest_run.md)
  product or a finalized `TempestSession`.

## Value

The exact committed Markdown report.

## Examples

``` r
if (FALSE) { # \dontrun{
result <- tempest_run("History of jazz", config = tempest_config())
tempest_report(result)

session <- tempest_session("History of jazz", config = tempest_config())
session$step("Tell me about bebop.")
session$finalize()
tempest_report(session)
} # }
```
