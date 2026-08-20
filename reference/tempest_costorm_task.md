# Create a Co-STORM evaluation task using SimulatedUser

Runs automated Co-STORM sessions with a simulated user for evaluation.
Moderator and expert turns use the same persistent Deputy agents as
normal Co-STORM sessions. The built-in solver completes a real
[`tempest_session()`](https://jameshwade.github.io/tempest/reference/tempest_session.md),
reads its exact committed report, and includes a versioned,
credential-safe trajectory summary in `solver_metadata`; it never
returns complete review lanes, Deputy Agent objects, chats, clients,
tools, or credentials as metadata. Caller datasets are validated and
bound to the Task name and metadata by a canonical digest. Custom
solvers cannot claim `program_set` or `knowledge_view` inputs on
Tempest's behalf.

## Usage

``` r
tempest_costorm_task(
  dataset = "qa",
  config = tempest_config(),
  max_turns = 5L,
  solver = NULL,
  scorer = NULL,
  scorer_chat = NULL,
  program_set = NULL,
  knowledge_view = NULL,
  ...
)
```

## Arguments

- dataset:

  The built-in `"qa"` smoke dataset or an exact data frame with `input`,
  `target`, and optional unique `id` columns.

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
task <- tempest_costorm_task(config = tempest_config(), max_turns = 5)
task$eval()
} # }
```
