# Inspect concepts in an Open Knowledge Format bundle

Returns a compact, deterministic catalog with the trust tier and
freshness derived according to OKF v0.2. These signals are advisory
metadata, not authorization decisions.

## Usage

``` r
tempest_okf_concepts(
  bundle,
  concept_ids = NULL,
  types = NULL,
  today = Sys.Date()
)
```

## Arguments

- bundle:

  A bundle returned by
  [`tempest_read_okf()`](https://jameshwade.github.io/tempest/reference/tempest_read_okf.md).

- concept_ids:

  Optional exact concept IDs.

- types:

  Optional exact OKF type values.

- today:

  Date used to derive staleness.

## Value

A tibble with one row per selected concept.
