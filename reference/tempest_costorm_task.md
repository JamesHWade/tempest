# Create a Co-STORM evaluation task using SimulatedUser

Runs automated Co-STORM sessions with a simulated user for evaluation.
Moderator and expert turns use the same persistent Deputy agents as
normal Co-STORM sessions. The built-in solver includes credential-safe
terminal Deputy traces in `solver_metadata`; it never returns Deputy
Agent objects.

## Usage

``` r
tempest_costorm_task(
  dataset = c("qa"),
  config = tempest_config(),
  max_turns = 5L,
  solver = NULL,
  scorer = NULL,
  scorer_chat = NULL,
  ...
)
```

## Arguments

- dataset:

  Which built-in dataset to use. Currently "qa".

- config:

  A `TempestConfig`.

- max_turns:

  Maximum turns per simulated session.

- solver:

  Optional vitals-compatible solver. When `NULL`, uses the built-in
  simulated Co-STORM session solver.

- scorer:

  Optional vitals-compatible scorer. When `NULL`, uses
  [`vitals::model_graded_qa()`](https://vitals.tidyverse.org/reference/scorer_model.html).

- scorer_chat:

  Optional chat for the default model-graded scorer. When `NULL`, a
  judge chat is created from `config`.

- ...:

  Passed to `vitals::Task$new()`.

## Value

A [`vitals::Task`](https://vitals.tidyverse.org/reference/Task.html).

## Examples

``` r
if (FALSE) { # \dontrun{
task <- tempest_costorm_task(config = tempest_config(), max_turns = 5)
task$eval()
} # }
```
