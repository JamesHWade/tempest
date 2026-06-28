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

## Examples

``` r
tempest_progress_labels("costorm")
#>     session      warmup    dialogue    evidence     mindmap suggestions 
#>     "Setup"    "Warmup"    "Answer"  "Evidence"       "Map"      "Next" 
#>      report 
#>    "Report" 
tempest_progress_labels("costorm", kind = "step")
#>             created       expert_fanout     expert_question                turn 
#>             "Ready"           "Experts"          "Question"              "Turn" 
#>           user_turn  moderator_response     fact_extraction              update 
#>              "User"         "Moderator"     "Capture facts"        "Update map" 
#> question_generation            generate           report_md 
#>         "Questions"          "Generate"            "Report" 
```
