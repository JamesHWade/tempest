# Create a Co-STORM evaluation task using SimulatedUser

Runs automated Co-STORM sessions with a simulated user for evaluation.

## Usage

``` r
tempest_costorm_task(
  dataset = c("qa"),
  config = tempest_config(),
  max_turns = 5L,
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
