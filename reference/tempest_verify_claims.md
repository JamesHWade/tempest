# Verify claim citations against their sources

Verify claim citations against their sources

## Usage

``` r
tempest_verify_claims(
  store,
  verifier,
  policy = "claim_verified",
  verifier_model = NA_character_,
  modules = NULL,
  min_support_score = 0.7
)
```

## Arguments

- store:

  A `SourceStore` holding claims and sources.

- verifier:

  A chat object (e.g. from `tempest_make_chat(config, "judge")`).

- policy:

  Citation policy; verification runs only for "claim_verified" or
  "strict". Defaults to "claim_verified".

- verifier_model:

  Optional model id recorded on each verified claim.

- modules:

  Optional named list of dsprrr modules; when it contains
  `verify_claim_support`, that module performs the judgement (with an
  ellmer fallback).

- min_support_score:

  Minimum support score in `[0, 1]` for a claim to be considered
  supported.

## Value

A `citation_audit` tibble (one row per verified claim).
