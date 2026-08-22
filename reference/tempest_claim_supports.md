# List explicit claim-support assessments

Returns the complete joined proof table rather than foreign keys alone.
Each row carries the support identity and judgment, the claim identity
and text, the exact evidence-span identity with its quote, offsets,
page, and section, and the source identity. Pair it with
[`tempest_sources()`](https://jameshwade.github.io/tempest/reference/tempest_sources.md)
for the corresponding source metadata and locator.

## Usage

``` r
tempest_claim_supports(x)
```

## Arguments

- x:

  A completed
  [`tempest_run()`](https://jameshwade.github.io/tempest/reference/tempest_run.md)
  product or a `TempestSession`.

## Value

A tibble with one row per exact claim-by-evidence-span judgment.
