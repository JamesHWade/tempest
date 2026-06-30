# data-dict evaluation for Tempest schemas

**Date:** 2026-06-29
**Status:** Recommendation recorded for v6kb
**Decision:** Defer adding `tidyverse/data-dict` as a Tempest dependency.

## Recommendation

Do not add `data-dict` to `DESCRIPTION`, runtime validation, tests, or the
bundled Shiny app yet.

`data-dict.yaml` is a good vocabulary for documenting related tables, column
semantics, constraints, relationships, and glossary terms. It is not yet a good
fit as a Tempest dependency because Tempest's schema boundaries are currently
in-memory S7/R6 records, tibbles, JSON session bundles, and optional ragnar
metadata. The current `data-dict` validation tooling is a Rust CLI centered on
`data-dict.yaml` files and Parquet-backed datasets.

The practical path is to keep using Tempest's existing validation and docs now,
and revisit `data-dict` when either:

- Tempest starts emitting stable Parquet table artifacts for sources, claims,
  evidence spans, disputes, citation audits, run manifests, or session bundles.
- `data-dict` adds first-class R, JSON, SQL, or database-table source
  validation that can check Tempest's existing artifacts without materializing
  Parquet.
- The `data-dict.yaml` spec and CLI stabilize beyond the current pre-1.0 state.

## Upstream findings

Primary sources checked on 2026-06-29:

- `tidyverse/data-dict` README:
  <https://github.com/tidyverse/data-dict#readme>
- `data-dict.yaml` overview:
  <https://data-dict.tidyverse.org/>
- `data-dict.yaml` specification:
  <https://data-dict.tidyverse.org/spec.html>
- `data-dict.yaml` validation:
  <https://data-dict.tidyverse.org/validation.html>

The useful parts:

- A dictionary describes a collection of tables with column names, types,
  constraints, relationships, and a glossary.
- Table descriptions explicitly ask for grain and population, which maps well
  to Tempest's exported evidence tables.
- Column descriptors include type, constraints, description, details, examples,
  ranges, values, units, and time zone metadata.
- Relationships can describe safe joins between tables and cardinality.
- Validation has three levels: spec structure, metadata against a data source,
  and data values against a data source.
- The CLI can print embedded agent skills for reading or writing dictionaries,
  which is aligned with Tempest's agent-facing schema needs.

The blocking parts:

- The current spec version is `0.1.0` and explicitly pre-1.0, so breaking
  changes are expected.
- The repository currently exposes a Rust CLI, not an R package API.
- GitHub releases were empty when checked.
- Parquet is the only currently supported source type for validation.
- Tempest's canonical exports are live tibbles from `tempest_sources()` and
  `tempest_claims()`, JSON snapshots from session/run persistence, and optional
  ragnar/DuckDB metadata, not Parquet datasets.

## Tempest comparison

Tempest already has the validation that matters for correctness:

- `tempest_claim` S7 records validate enums and bounded scores at construction.
- `TempestConfig` validates provider and score settings at construction.
- `SourceStore` is the mutable R6 accumulator threaded through STORM,
  Co-STORM, retrieval, session persistence, and the Shiny app.
- `tempest_sources()` and `tempest_claims()` expose stable tibbles for users and
  app tables.
- Session/run persistence uses schema-versioned JSON with restore-time checks.
- Focused tests cover ledger construction, source/fact tibbles, persistence,
  Shiny table formatting, and provider-native citation extraction.

Adding `data-dict` now would duplicate roxygen return-value docs and tests
without strengthening these runtime boundaries. It would also introduce a
non-R build/tooling surface that package checks and users do not otherwise
need.

## What a future adoption could look like

If Tempest later publishes stable table artifacts, use `data-dict` as a
documentation and external validation layer, not as the source of truth for
runtime objects.

Candidate tables:

- `sources`: `id`, `url`, `title`, `snippet`, `content_text`, `context_text`,
  `fetched_at`, `meta`
- `claims`: `claim_id`, `claim_text`, `claim_type`, `source_ids`,
  `confidence`, `verification_status`, `support_score`, `session_id`,
  `created_at`
- `evidence_spans`: `evidence_span_id`, `source_id`, `quote`, offsets,
  page/section metadata, relevance score
- `disputes`: `dispute_id`, `topic`, `claim_ids`, evidence balance,
  unresolved questions
- `citation_audit`: claim id/text, verification status, support score,
  rationale

Implementation shape if revisited:

1. Add an internal generator that derives `data-dict.yaml` from the S7 record
   classes and tibble exporters rather than hand-maintaining duplicate schemas.
2. Write generated dictionaries to `dev/schema/` or `inst/schema/` only after
   the exported table contract is stable enough to support.
3. Validate dictionaries in optional developer checks, not required package
   checks, until the CLI is released and easy to install in R CI.
4. Keep S7 constructors, restore checks, and testthat expectations as the
   authoritative runtime validation.

## No follow-up issues created

No implementation issues are needed right now. Existing package metadata,
roxygen return docs, S7 validators, restore checks, and tests are sufficient
for Tempest's current source/fact schema needs.

Create a new issue later only if Tempest starts writing stable Parquet table
artifacts or `data-dict` gains a source type that can validate the current
R/JSON artifacts directly.
