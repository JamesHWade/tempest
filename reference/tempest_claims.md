# Return claims as a tibble

Return claims as a tibble

## Usage

``` r
tempest_claims(x)
```

## Arguments

- x:

  A completed
  [`tempest_run()`](https://jameshwade.github.io/tempest/reference/tempest_run.md)
  product or a `TempestSession`.

## Value

A tibble of claims with columns: claim_id, claim_text, claim_type,
source_ids, confidence, verification_status, support_score, created_at.

## Examples

``` r
if (FALSE) { # \dontrun{
result <- tempest_run("History of jazz", config = tempest_config())
tempest_claims(result)
} # }
```
