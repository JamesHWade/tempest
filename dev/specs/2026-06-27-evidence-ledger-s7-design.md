# Evidence Ledger + S7 Records: Phase 0 and Phase 1

**Date:** 2026-06-27
**Status:** Approved design, ready for implementation planning
**Scope:** Foundations (Phase 0) + evidence ledger and claim-level citation verification (Phase 1)

## Why

tempest can already run STORM and Co-STORM end to end. The gap is trust: a generated report cites sources with `[Sxxxx]` markers, but nothing checks that a cited source actually supports the sentence it sits next to. Facts are plain lists with no schema, so a hallucinated citation, an illegal confidence value, or a malformed mind-map node all flow through unchecked.

This work makes the general web-research pipeline auditable. Every factual claim becomes a typed, validated record linked to source spans, and a verification pass labels whether each cited source supports its claim before the report is written.

The `tempest_recommendations.md` document lays out six phases. This spec covers Phase 0 and Phase 1, which it names the highest priority. Scientific literature retrieval, contradiction agents, claim-aware mind maps, dedup, replay, and data analysis are out of scope here.

## Decisions

Four scoping decisions, settled with the package author:

1. **S7 adoption is additive.** New value records (claim, evidence span, dispute, source) and `TempestConfig` move to S7. The four stateful R6 classes stay R6. No blanket migration.
2. **This cycle = Phase 0 + Phase 1.** Foundations first, then the ledger and verification.
3. **General research, not scientific.** Make the existing web pipeline auditable. Literature providers and citation-graph traversal wait.
4. **Clean break.** Nothing is published, so the claim API replaces the fact API outright. No deprecation shims.

## R6 vs S7: the rationale

R6 gives mutable reference semantics. S7 gives functional generics with validated properties. They solve different problems, and tempest already splits along that line. The deciding evidence: `ellmer` and `ragnar`, tempest's own dependencies, already use S7 for typed value records and R6 for stateful objects (ellmer's `Chat` is R6; its content, types, and turns are S7). tempest follows the same pattern.

Per-class verdict:

| Class | Semantics | Verdict |
|---|---|---|
| `SourceStore` | mutable accumulator threaded through every stage | stay R6 |
| `TempestSession` | `step()` loop mutates transcript/mindmap/store in place | stay R6 |
| `ExpertSessionManager` | live registry of per-expert chat objects | stay R6 |
| `TempestRetriever` | mutates the shared store, writes the cache | stay R6 |
| `TempestConfig` | created once, never mutated — a value object | **move to S7** |
| `DiscourseManager` | near-stateless | leave for now |
| `SimulatedUser` | only mutable state is a turn counter | leave for now |

The real S7 win is the data records, not the classes. Today a source, fact, outline, and mind-map node are plain lists with zero validation. The new records (claim, evidence span, dispute) carry enums and bounded scores that must be enforced at construction. Building verification on unvalidated lists would lock in silent corruption and make a later S7 retrofit expensive, so the records are S7 from day one.

## Phase 0 — Foundations

Do this first. It de-risks every edit in Phase 1.

### 0.1 Split `storm.R`

`storm.R` is 2,764 lines mixing type definitions, normalization, five pipeline stages, orchestration, dsprrr wiring, fact extraction, and citation glue. Split it by pipeline stage:

- `storm.R` — keep only `tempest_run()` orchestration and stage dispatch
- `storm-perspectives.R`, `storm-research.R`, `storm-outline.R`, `storm-write.R`, `storm-polish.R` — one file per stage
- `storm-types.R` — ellmer type constructors shared across stages
- `dsprrr.R` — the dsprrr module factory and `tempest_run_dsprrr_module`

This is a pure code move with no behavior change. Existing tests plus a few smoke tests cover it.

**Choice:** by-stage split over by-concern. It matches how the pipeline is reasoned about and keeps each stage's logic in one place.

### 0.2 Mock-LLM test harness

Today there are about 4 mocked-binding sites across 18 test files, ~54 `skip_if_not_installed` calls, and ~35 API-key skips. The verification and ledger logic would be untestable in CI without network access.

Add `tests/testthat/helper-mocks.R`:

- `local_fake_chat()` — a fake ellmer chat whose `$chat_structured()` returns scripted output, so dsprrr and ellmer calls are deterministic
- `local_fake_judge()` — scripted verification verdicts for the verifier pass
- fixture builders — `fake_source()`, `fake_claim()`, and a prepopulated in-memory `SourceStore`

Swap `make_chat` and the dsprrr module runners via `local_mocked_bindings()`. Every Phase-1 function gets a CI-runnable test with no key and no network.

### 0.3 Structured-output validation

Validate the shape of LLM and dsprrr structured output before using it. Validate extracted `[Sxxxx]` IDs against the store and flag unknown IDs instead of degrading to silent placeholder text. Once S7 records land in Phase 1, this validation moves to construction time.

## Phase 1 — Evidence ledger and citation verification

### 1.1 S7 record layer (`R/ledger-types.R`)

The heart of the S7 adoption. Each record is an S7 class with property validators that reject illegal enum values and out-of-range scores at construction.

`tempest_claim`:

- `claim_id`, `claim_text`
- `claim_type` — enum: finding, definition, method, limitation, contradiction, open_question
- `source_ids`, `evidence_span_ids` — character
- `supporting_quotes`, `contradicting_source_ids`, `contradiction_note`
- `confidence` — enum: low, medium, high
- `support_score`, `contradiction_score`, `source_quality_score` — numeric in [0,1] or NA
- provenance — `retrieval_query`, `retrieval_step_id`, `perspective_id`, `persona_id`, `section_id`
- `created_at`, `verified_at`, `verifier_model`
- `verification_status` — enum: unverified (initial state), supported, partially_supported, unsupported, contradicted, unverifiable. The verifier in 1.4 sets one of the five non-initial labels.

`tempest_evidence_span`:

- `evidence_span_id`, `source_id`, `chunk_id`
- `quote`, `start_offset`, `end_offset` (integer), `page`, `section_heading`
- `relevance_score`, `extracted_by`, `created_at`

`tempest_dispute`:

- `dispute_id`, `topic`, `claim_ids`
- `axis_of_disagreement`, `likely_explanation`
- `evidence_balance` — enum: agreement, mixed, conflict
- `unresolved_questions`

`tempest_source`:

- promote today's source list to an S7 record: `id`, `url`, `title`, `snippet`, `content_text`, `fetched_at`, `content_hash`, `meta`
- `meta` stays an open list so paper metadata (DOI, year, venue) can attach later without a schema change

IDs: keep deterministic source IDs (xxhash64 of URL). Claim, evidence-span, and dispute IDs get `C`/`E`/`D` prefixes. Generics: `as_tibble()` and `format()` methods for export and printing, and a `tempest_verify()` generic over claims.

### 1.2 Extend `SourceStore` into the ledger container (`R/models.R`)

`SourceStore` already holds sources, facts, and artifacts and is threaded through every stage. Extend it in place rather than wrapping it.

Add containers: `claims`, `evidence_spans`, `disputes`, each keyed by ID, plus secondary indexes by `source_id` and by tag so lookups are not linear scans.

Add methods: `add_claim`, `get_claim`, `list_claims`, `add_evidence_span`, `link_evidence`, `add_dispute`, `get_evidence_for_claim`, `find_contradictions`, `verify_claim`. Extend `to_tibbles()` to emit claims, evidence spans, and disputes, and fix the current bug where it drops `content_text` and `meta`.

Replace the fact API: `add_fact` becomes `add_claim`, `tempest_fact` becomes `tempest_claim`, `tempest_facts` becomes `tempest_claims`. Add `tempest_evidence()` and `tempest_disputes()` accessors. `SourceStore` stays R6 — it is the mutable accumulator.

**Choice:** extend `SourceStore` in place, over a separate `EvidenceLedger` that composes it (needless indirection, re-threading through every stage) or replacing `SourceStore` (loses the artifacts container that run-persistence depends on).

### 1.3 Claim extraction into the ledger

Today fact extraction parses `[Sxxxx]` markers out of answer text. Reroute it through a dsprrr `extract_claims` module that emits typed claim candidates with source IDs and optional supporting quotes. Construct each as a validated S7 claim. An unknown source ID is flagged, not silently dropped. Evidence-span quotes are best-effort: locate the quote in the source `content_text` to fill offsets, leave offsets NA when not found.

This is the Phase-1 subset of the larger dsprrr module set; the rest waits.

### 1.4 Citation verification (`R/verify.R`)

```r
tempest_verify_claims(claims, sources, verifier = config$make_chat("judge"))
```

For each claim, gather the cited source excerpts (or ragnar chunks when configured), ask the judge for a support label, a score, and a rationale, then update the claim's `verification_status`, `support_score`, `verified_at`, and `verifier_model`. Return a `citation_audit` tibble.

Implement as a dsprrr `verify_claim_support` module with an ellmer fallback.

The canonical path verifies the claim ledger before the report is written, so the writer can select only verified claims. A post-hoc `tempest_audit_report()` covers reports produced outside that path: split into sentences, extract citation IDs, verify each.

Five labels: supported, partially_supported, unsupported, contradicted, unverifiable.

### 1.5 `citation_policy` in config and report assembly (`R/config.R`, `R/citations.R`)

New config fields:

- `citation_policy` — enum: none, source_attributed (default), claim_verified, strict
- `min_support_score` — default 0.70
- `on_unsupported_claim` — enum: flag, drop, revise, keep_with_warning

The default `source_attributed` reproduces today's behavior exactly, so verification is opt-in. The reason is cost: verification adds judge-model calls per claim, and a research run can hold many claims. Docs recommend `claim_verified` for users who want the audit. (The package author can flip the default to `claim_verified` later if auditability should be on by default; the machinery supports either.)

Under `claim_verified` and `strict`, `tempest_report_md()` renders verification badges in footnotes (supported / partially_supported / unsupported). Under `strict`, unsupported sentences are dropped or flagged per `on_unsupported_claim`. The `citation_audit` tibble is saved as a run artifact.

### 1.6 `TempestConfig` to S7 (`R/config.R`)

Convert `TempestConfig` from R6 to S7 with validated properties: the 17 existing fields plus the new enum fields from 1.5. Function and external-object fields (`chat_fn`, `embed_fn`, `ragnar_store`, `tools`) are held as function or `any` properties. The config is passed by value and never mutated, so the switch is safe. `make_chat` becomes a method or a plain function over the config.

The new enum fields are exactly where construction-time validation pays off — an illegal `citation_policy` fails immediately rather than surfacing deep in report assembly.

### 1.7 Persistence (`R/run-persistence.R`)

Save the new ledger tables (claims, evidence spans, disputes, citation audit) as JSON alongside the existing run artifacts.

**Choice:** JSON this cycle, not parquet. Parquet would pull in `arrow`, a heavy dependency, for no Phase-1 benefit. The full parquet manifest with replay and compare (recommendation R9) is Phase 5 and stays deferred.

### 1.8 Tests and docs

Cover, via the 0.2 harness:

- S7 record validation — illegal enum and out-of-range score are rejected
- ledger operations — add, link, query, find contradictions
- claim extraction — a hallucinated source ID is flagged
- `tempest_verify_claims` — using the fake judge
- `citation_policy` rendering — badges under each policy
- config S7 validation

Update README, NEWS, and pkgdown.

## Out of scope this cycle

Scientific mode and literature providers, citation-graph traversal, contradiction-finder agents and new discourse actions, claim-aware mind map, semantic dedup, manifest/replay/compare, the stateful report editor, data analysis, and vitals scorers beyond what the tests need. These are Phases 2 through 6.

## Sequencing

Phase 0 in order, because each step makes the next safer:

1. 0.1 split `storm.R`
2. 0.2 mock harness
3. 0.3 structured-output validation

Then Phase 1:

4. 1.1 S7 records
5. 1.2 ledger container
6. 1.6 config to S7
7. 1.3 claim extraction
8. 1.4 verification
9. 1.5 policy and report rendering
10. 1.7 persistence
11. 1.8 tests and docs

## Risks

- **Building on unvalidated lists.** The mitigation is the whole design: define claims, spans, and disputes as S7 records before any verification logic touches them.
- **Test infrastructure can't support the work.** Phase 0.2 (the mock harness) is a hard prerequisite for testing verification and ledger logic in CI; it comes before any Phase-1 code.
- **`storm.R` is hard to edit safely at 2,764 lines.** Phase 0.1 splits it before Phase 1 adds hooks.
- **Silent dsprrr-to-ellmer fallback hides when modules fail.** Add logging when a module falls back, so eval-driven optimization later isn't measuring the wrong path.
- **`SourceStore` mutation has no rollback.** A stage failing mid-write can orphan partial data. Out of full scope here, but new ledger writes should be ordered so a crash leaves a consistent store.

## Success criteria

1. Every claim in the ledger is a typed, validated S7 record.
2. A claim links to source spans, not just source IDs.
3. `tempest_verify_claims()` labels each cited source's support for its claim.
4. `citation_policy = "strict"` keeps unsupported claims out of the final report.
5. A `citation_audit` table is produced and saved per run.
6. The full Phase-1 path runs in CI with no API key, against the mock harness.
7. `TempestConfig` and the four ledger records are S7; the four stateful classes remain R6.
