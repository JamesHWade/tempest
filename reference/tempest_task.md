# Create a vitals Task for tempest

The built-in solver runs a real
[`tempest_run()`](https://jameshwade.github.io/tempest/reference/tempest_run.md)
product for each input and returns its authoritative report.
`solver_metadata` contains only credential-safe Manifest, Workspace, and
StageRecord summaries; live chats, clients, tools, and credentials are
not returned as metadata.

## Usage

``` r
tempest_task(
  dataset = c("qa"),
  solver = NULL,
  scorer = NULL,
  scorer_chat = NULL,
  config = tempest_config(),
  ...
)
```

## Arguments

- dataset:

  Which built-in dataset to use. Currently "qa".

- solver:

  Optional vitals-compatible solver. When `NULL`, evaluates the
  authoritative
  [`tempest_run()`](https://jameshwade.github.io/tempest/reference/tempest_run.md)
  product.

- scorer:

  A vitals scorer. If `NULL`, defaults to
  [`vitals::model_graded_qa()`](https://vitals.tidyverse.org/reference/scorer_model.html).

- scorer_chat:

  Chat used by the scorer. Required for model-graded scoring.

- config:

  A `TempestConfig` passed to the solver.

- ...:

  Passed to `vitals::Task$new()`.

## Value

A [`vitals::Task`](https://vitals.tidyverse.org/reference/Task.html).

## Examples

``` r
if (FALSE) { # \dontrun{
scorer_chat <- ellmer::chat("openai/gpt-5.6-luna")
task <- tempest_task(scorer_chat = scorer_chat, config = tempest_config())
task$eval()
} # }
```
