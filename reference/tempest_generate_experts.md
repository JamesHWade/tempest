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
  module = NULL
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

- module:

  Optional dsprrr module used internally.

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
