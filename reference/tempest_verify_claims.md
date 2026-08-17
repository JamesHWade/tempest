# Verify claim citations against their sources

Verify claim citations against their sources

## Usage

``` r
tempest_verify_claims(
  workspace,
  verifier,
  policy = "claim_verified",
  verifier_model = NA_character_,
  program_set = NULL,
  min_support_score = 0.7
)
```

## Arguments

- workspace:

  A
  [ResearchWorkspace](https://jameshwade.github.io/tempest/reference/ResearchWorkspace.md)
  holding proposed claims and retrieved sources, or a `TempestSession`
  whose authoritative workspace and bound verification program should be
  used.

- verifier:

  A chat object (e.g. from `tempest_make_chat(config, "judge")`).

- policy:

  Citation policy; verification runs only for "claim_verified" or
  "strict". Defaults to "claim_verified".

- verifier_model:

  Optional model id recorded on each verified claim.

- program_set:

  A
  [TempestProgramSet](https://jameshwade.github.io/tempest/reference/TempestProgramSet.md)
  containing the exact `verify_claim_support` program. If `NULL`,
  [`tempest_program_set()`](https://jameshwade.github.io/tempest/reference/tempest_program_set.md)
  creates the builtin set. When `workspace` is a `TempestSession`, its
  immutable ProgramSet, citation policy, and support threshold are
  authoritative; supplied values must match.

- min_support_score:

  Minimum support score in `[0, 1]` for a claim to be considered
  supported.

## Value

A `citation_audit` tibble (one row per verified claim).

Passing a standalone
[ResearchWorkspace](https://jameshwade.github.io/tempest/reference/ResearchWorkspace.md)
returns an audit but does not own a durable product stage ledger. Pass
the `TempestSession` for Co-STORM bundles so claim updates, audit rows,
and verification records commit together.
