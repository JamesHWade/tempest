# Bring a retained artifact selection into a research run

The host resolves immutable content and checks current permission and
consultation eligibility before calling this constructor. Tempest
verifies exact selection coverage, content digests and declared
dependencies. It does not open storage, infer approval, or grant
execution authority.

## Usage

``` r
tempest_artifact_knowledge(selection, contents)
```

## Arguments

- selection:

  A JSON-compatible list with `selection_id` (host-owned identity),
  `purpose`, `records`, and `provenance` (a list of source references).
  Each record has `record_id`, `revision_id`, `class`, `sha256`
  (lowercase SHA-256 of UTF-8 text bytes), and `dependencies` (a list of
  exact `record_id`/`revision_id` pairs). Supported classes are `Claim`,
  `ClaimSupport`, `EvidenceSpan`, and `Source`. The selection contains 1
  to 1000 unique records; its metadata is limited to 1 MiB. Record and
  revision identifiers must not contain surrounding whitespace.
  Dependencies describe the host's evidence selection, not an inferred
  graph or authorization.

- contents:

  Named list of retained text strings, keyed by `record_id`, with
  exactly the selection's records and at most 1 MiB of UTF-8 text total.
  Claims use the same inert `statement_text: ...` and `status: ...`
  fields as the accepted research projection; active statements support
  no-change briefing detection. Content is never evaluated as code or
  instructions.

## Value

A `TempestKnowledge` value for
[`tempest_run()`](https://jameshwade.github.io/tempest/reference/tempest_run.md)
or
[`tempest_session()`](https://jameshwade.github.io/tempest/reference/tempest_session.md).
The run retains the selection separately from any Graft snapshot. Its
computed input digest binds the input description; it does not
authenticate a host, prove factual truth, or represent a native
acceptance event.

## See also

[`tempest_knowledge()`](https://jameshwade.github.io/tempest/reference/tempest_knowledge.md)
for the existing Graft producer.
