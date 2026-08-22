# Progress labels for Tempest workflows

`tempest_progress_labels()` returns compact, host-neutral labels for
progress event stages and steps. Host apps can use these labels with
[`tempest_progress_state()`](https://jameshwade.github.io/tempest/reference/tempest_progress_state.md)
instead of maintaining their own workflow-specific remapping tables.

## Usage

``` r
tempest_progress_labels(
  workflow = c("storm", "costorm"),
  kind = c("stage", "step")
)
```

## Arguments

- workflow:

  Workflow kind, one of `"storm"` or `"costorm"`.

- kind:

  Label kind, either `"stage"` or `"step"`.

## Value

A named character vector.
