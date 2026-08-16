# Return claims as a tibble

Return claims as a tibble

## Usage

``` r
tempest_claims(workspace)
```

## Arguments

- workspace:

  A
  [ResearchWorkspace](https://jameshwade.github.io/tempest/reference/ResearchWorkspace.md)
  or
  [TempestRetriever](https://jameshwade.github.io/tempest/reference/TempestRetriever.md).

## Value

A tibble of claims with columns: claim_id, claim_text, claim_type,
source_ids, confidence, verification_status, support_score, created_at.

## Examples

``` r
if (FALSE) { # \dontrun{
result <- tempest_run("History of jazz", config = tempest_config())
tempest_claims(result$workspace)
} # }
```
