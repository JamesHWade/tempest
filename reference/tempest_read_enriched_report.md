# Read and verify a portable enriched report

Checks every declared file against its SHA-256 digest and rejects
missing, additional, linked, or changed files. Checksums detect
accidental changes; they are not a signature or proof of the report
author's identity.

## Usage

``` r
tempest_read_enriched_report(path)
```

## Arguments

- path:

  Directory returned by
  [`tempest_enrich_report()`](https://jameshwade.github.io/tempest/reference/tempest_enrich_report.md).

## Value

A validated `TempestEnrichedReport`.
