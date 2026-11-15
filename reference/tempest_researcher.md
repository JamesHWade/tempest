# Research a topic and produce an enriched report

Runs the normal STORM research and publication gates, then adds the
optional computations and illustrations configured in `enrichment`.

## Usage

``` r
tempest_researcher(topic, output_dir, ..., enrichment = list())
```

## Arguments

- topic:

  Research topic or question.

- output_dir:

  New directory for the enriched report.

- ...:

  Arguments passed to
  [`tempest_run()`](https://jameshwade.github.io/tempest/reference/tempest_run.md).

- enrichment:

  Named list of arguments to
  [`tempest_enrich_report()`](https://jameshwade.github.io/tempest/reference/tempest_enrich_report.md),
  excluding `x` and `output_dir`.

## Value

A list with `research` (the canonical Tempest product) and `report` (the
derived `TempestEnrichedReport`).
