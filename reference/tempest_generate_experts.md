# Generate expert profiles for a topic

Uses an LLM to propose diverse experts who would naturally approach the
topic from different angles, then normalizes the provider response into
validated, versioned
[`tempest_expert()`](https://jameshwade.github.io/tempest/reference/tempest_expert.md)
profiles.

## Usage

``` r
tempest_generate_experts(
  topic,
  n = 3,
  config = tempest_config(),
  verbose = FALSE,
  program_set = NULL
)
```

## Arguments

- topic:

  The research topic.

- n:

  Number of experts to generate.

- config:

  A `TempestConfig` object.

- verbose:

  Print progress.

- program_set:

  A
  [TempestProgramSet](https://jameshwade.github.io/tempest/reference/TempestProgramSet.md)
  containing the exact `personas` program. If `NULL`,
  [`tempest_program_set()`](https://jameshwade.github.io/tempest/reference/tempest_program_set.md)
  creates the builtin set.

## Value

A list of `tempest_expert` profiles.

## Examples

``` r
if (FALSE) { # \dontrun{
experts <- tempest_generate_experts(
  topic = "Climate change adaptation",
  n = 3,
  config = tempest_config()
)
} # }
```
