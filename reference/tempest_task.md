# Create a vitals Task for tempest

Create a vitals Task for tempest

## Usage

``` r
tempest_task(
  dataset = c("qa"),
  solver = tempest_solver_cited_answer,
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

  A vitals-compatible solver. Defaults to `tempest_solver_cited_answer`.

- scorer:

  A vitals scorer. If `NULL`, defaults to
  [`vitals::model_graded_qa()`](https://vitals.tidyverse.org/reference/scorer_model.html).

- scorer_chat:

  Chat used by the scorer (required for model-graded scoring).

- config:

  A `TempestConfig` passed to the solver.

- ...:

  Passed to `vitals::Task$new()`.

## Value

A [`vitals::Task`](https://vitals.tidyverse.org/reference/Task.html).
