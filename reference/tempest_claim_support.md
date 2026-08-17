# Create an explicit claim-support assessment

`tempest_claim_support()` records one verifier judgment for one exact
claim-by-evidence-span pair. The source binding is explicit, while
claim, span, and source existence are validated by the owning
[ResearchWorkspace](https://jameshwade.github.io/tempest/reference/ResearchWorkspace.md).

## Usage

``` r
tempest_claim_support(
  claim_id,
  evidence_span_id,
  source_id,
  verification_status,
  support_score,
  rationale
)
```

## Arguments

- claim_id:

  Exact provisional claim identifier.

- evidence_span_id:

  Exact evidence-span identifier.

- source_id:

  Exact source identifier owned by the evidence span.

- verification_status:

  One of `"supported"`, `"partially_supported"`, `"unsupported"`,
  `"contradicted"`, or `"unverifiable"`.

- support_score:

  Finite support strength in `[0, 1]`, or `NA` only for an
  `"unverifiable"` assessment.

- rationale:

  Required bounded credential-free rationale.

## Value

A `tempest_claim_support` S7 value.
