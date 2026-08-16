# Research a single perspective (search + expert synthesis)

Shared by the parallel and sequential research fallbacks so both paths
behave identically. Returns the retrieved resources and proposed claims
gathered for one perspective in an isolated workspace.

## Usage

``` r
tempest_research_one_perspective(
  i,
  perspectives,
  experts,
  config,
  runtime = tempest_runtime(),
  connection_permissions = list(),
  topic,
  research_strategy,
  max_questions_per_perspective,
  programs,
  run_id = NA_character_
)
```
