# Read the committed Markdown report from a Co-STORM session

This accessor returns the exact bytes already committed during Co-STORM
publication. It never generates, repairs, or republishes a report, and
it fails unless the session is succeeded, quiescent, and bound to the
same report reference as its research Manifest.

## Usage

``` r
tempest_session_report_md(session)
```

## Arguments

- session:

  A `TempestSession`.

## Value

The exact committed Markdown report.

## Examples

``` r
if (FALSE) { # \dontrun{
session <- tempest_session("History of jazz", config = tempest_config())
session$step("Tell me about bebop.")
session$report()
md <- tempest_session_report_md(session)
} # }
```
