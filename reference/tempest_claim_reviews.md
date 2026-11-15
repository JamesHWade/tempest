# Read scientific claim reviews

Returns the exact advisory review records retained by a completed
research product. Each record binds a claim to captured source excerpts,
an initial reviewer, an independent adjudication, and a bounded
evidence-search outcome. `no_issue` means that the reviewers found no
issue; it does not certify the claim. Reviews do not replace
[`tempest_claim_supports()`](https://jameshwade.github.io/tempest/reference/tempest_claim_supports.md).

## Usage

``` r
tempest_claim_reviews(x)
```

## Arguments

- x:

  A completed
  [`tempest_run()`](https://jameshwade.github.io/tempest/reference/tempest_run.md)
  product or published
  [`tempest_session()`](https://jameshwade.github.io/tempest/reference/tempest_session.md).

## Value

A list of versioned plain records, in deterministic claim order. An
empty list means no claims were reviewed.
