# Create a Deputy-backed R execution callback

The callback runs each analysis in a fresh Deputy `RSession`, captures
text, warnings, plots, and files written under the task directory, and
closes the worker before returning. Code runs with the current user's
local R access; use this callback only for code execution that has been
explicitly enabled.

## Usage

``` r
tempest_r_executor(timeout = 60)
```

## Arguments

- timeout:

  Maximum time in seconds for each R evaluation.

## Value

A callback with the `function(task, directory)` interface used by
[`tempest_enrich_report()`](https://jameshwade.github.io/tempest/reference/tempest_enrich_report.md).
It returns `output`, `files`, and `metadata`.
