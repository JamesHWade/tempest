# Generate a single expert profile for a specific area

Generate a single expert profile for a specific area

## Usage

``` r
tempest_generate_single_expert(
  topic,
  area,
  existing_experts,
  config,
  module,
  record_stage = function(record, output = NULL) invisible(record)
)
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

- module:

  ProgramSet-bound persona execution.

- record_stage:

  Product-owned stage-record callback.

## Value

A validated `tempest_expert` profile.
