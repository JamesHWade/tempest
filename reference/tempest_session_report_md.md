# Assemble a Markdown report from a Co-STORM session

Assemble a Markdown report from a Co-STORM session

## Usage

``` r
tempest_session_report_md(session)
```

## Arguments

- session:

  A `TempestSession`.

## Value

Markdown with footnotes.

## Examples

``` r
if (FALSE) { # \dontrun{
session <- tempest_session("History of jazz", config = tempest_config())
session$step("Tell me about bebop.")
md <- tempest_session_report_md(session)
} # }
```
