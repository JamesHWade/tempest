# Create the built-in Co-STORM workflow specification

**\[experimental\]**

## Usage

``` r
tempest_costorm_workflow_spec()
```

## Value

A `tempest_workflow_spec`.

## Details

The interactive dialogue is an approval checkpoint. Starting the
workflow runs warmup and then returns in `awaiting_approval`. A host
conducts as many
[TempestSession](https://jameshwade.github.io/tempest/reference/TempestSession.md)
turns as needed before approving the dialogue checkpoint and resuming
the run to produce the report.

## Examples

``` r
workflow <- tempest_costorm_workflow_spec()
workflow@steps$dialogue@approval_checkpoint
#> [1] TRUE
```
