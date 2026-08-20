# Create a vitals Task for tempest

The built-in solver runs a real
[`tempest_run()`](https://jameshwade.github.io/tempest/reference/tempest_run.md)
product for each input and returns its authoritative report.
`solver_metadata` contains only a versioned, credential-safe trajectory
summary; live chats, clients, tools, evidence identifiers, source
content, and credentials are excluded. Caller datasets are validated and
bound to the Task name and metadata by a canonical digest. Custom
solvers cannot claim `program_set` or `knowledge_view` inputs on
Tempest's behalf.

## Usage

``` r
tempest_task(
  dataset = "qa",
  solver = NULL,
  scorer = NULL,
  scorer_chat = NULL,
  config = tempest_config(),
  program_set = NULL,
  knowledge_view = NULL,
  ...
)
```

## Arguments

- dataset:

  The built-in `"qa"` smoke dataset or an exact data frame with `input`,
  `target`, and optional unique `id` columns.

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

- program_set:

  Optional
  [TempestProgramSet](https://jameshwade.github.io/tempest/reference/TempestProgramSet.md)
  evaluated by the built-in solver.

- knowledge_view:

  Optional immutable Graft view used by governed programs.

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
