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
  verbose = FALSE
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

## Value

A list of persona objects.
