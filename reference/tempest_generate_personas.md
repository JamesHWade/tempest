# Generate Expert Personas for a Topic

Uses an LLM to generate diverse expert personas who would naturally
approach the topic from different angles, following the STORM
methodology.

## Usage

``` r
tempest_generate_personas(
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

  Number of personas to generate.

- config:

  A `TempestConfig` object.

- verbose:

  Print progress.

- module:

  Optional dsprrr module used internally.

## Value

A list of persona objects.

## Examples

``` r
if (FALSE) { # \dontrun{
personas <- tempest_generate_personas(
  topic = "Climate change adaptation",
  n = 3,
  config = tempest_config()
)
} # }
```
