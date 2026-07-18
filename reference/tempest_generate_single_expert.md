# Generate a single expert profile for a specific area

Generate a single expert profile for a specific area

## Usage

``` r
tempest_generate_single_expert(topic, area, existing_experts, config)
```

## Arguments

- topic:

  The research topic.

- area:

  The area of expertise needed.

- existing_experts:

  Existing expert profiles to avoid duplicating.

- config:

  A `TempestConfig` object.

## Value

A validated `tempest_expert` profile.
