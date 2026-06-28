# Recommendations for Tempest: From R STORM/Co-STORM Port to Auditable Scientific Research System

**Audience:** Tempest development team  
**Repository context:** `JamesHWade/tempest`  
**Date:** 2026-06-27  
**Status:** Draft for team review

## Executive summary

Tempest already has the core STORM and Co-STORM workflow primitives in
place: multi-perspective research, citation/source tracking, query
decomposition, semantic retrieval through `ragnar`, dsprrr-backed
structured extraction, Co-STORM discourse management, a mind map,
persistent runs, parallel execution, and simulated-user evaluation. The
next major opportunity is therefore not to add “more Co-STORM,” but to
make Tempest a rigorous, auditable, R-native research substrate.

The recommended direction is to evolve Tempest from a report-generating
agent into a **claim-centered evidence system**. The central object
should not be the generated report, chat transcript, or mind map. It
should be a verified, replayable evidence graph that links claims to
sources, source spans, retrieval paths, uncertainty, contradictions, and
downstream report sections.

The highest-priority additions are:

1.  Promote `SourceStore` and `tempest_fact()` into a richer evidence
    ledger.
2.  Add claim-level citation verification before report assembly.
3.  Add a scientific retrieval mode inspired by Asta, PaperQA2, and
    OpenScholar.
4.  Extend retrieval from `search -> fetch -> retrieve` to
    `plan -> search -> fetch -> rerank -> traverse -> contradict -> verify`.
5.  Add contradiction, uncertainty, and method-critique agents to
    Co-STORM.
6.  Make the mind map claim-aware, not just topic/source-aware.
7.  Expand `vitals` evaluations around citation support, source recall,
    contradiction handling, and user steering.
8.  Formalize reproducible run manifests and replay.
9.  Keep dsprrr central by turning each research-quality step into an
    optimizable module.

Asta is worth incorporating, but best treated as an optional scientific
back end and design benchmark rather than as a replacement architecture.
The broader goal should be: **Tempest = STORM/Co-STORM interface +
scientific evidence operating system.**

## Current Tempest baseline

Tempest is already well positioned. The current README describes it as
an R-native implementation of Stanford STORM and Co-STORM, with
multi-perspective research, evidence tracking, two-step outline
refinement, query decomposition, semantic fact retrieval, Co-STORM
moderation/mind map support, and automated evaluation. The
implementation is built on the modern R AI stack: `ellmer`, `ragnar`,
`dsprrr`, `shinychat`, and `vitals`.

The scripted pipeline currently runs five stages: `perspectives`,
`research`, `outline`, `write`, and `polish`. It also supports
persistent staged runs, seed URL table-of-contents extraction, query
decomposition, semantic retrieval, two-step outline generation,
lead-section generation, parallel section writing, and dsprrr modules
for structured output.

The current `SourceStore` abstraction is a strong foundation. It stores
sources, fact notes, and arbitrary artifacts. A fact currently contains
an atomic claim, source IDs, confidence, notes, tags, and a timestamp.
That is exactly the right starting point for a richer evidence ledger.

The current citation system converts inline source IDs such as
`[Sxxxxxxxxxxxx]` into Markdown footnotes. This provides source
attribution, but it does not yet prove that the cited source supports
the exact sentence or claim. That gap should become the next core
engineering target.

The current Co-STORM discourse manager already has useful turn actions
such as `expert_speaks`, `moderator_probes`, `add_expert`,
`retire_expert`, `surface_unseen`, and `end_round`. This is a good hook
for adding research-quality actions such as citation verification,
contrary-evidence search, and method critique.

The existing roadmap already captures several important prerequisites:
improved tests, structured-output validation, unbounded fact
accumulation, Shiny timeout protection, race conditions, vignettes,
export formats, UI enhancements, and streaming. Those should remain
important, but the recommendations below focus on product and research
capability beyond the current roadmap.

## External direction of travel

The field has moved beyond simple RAG and multi-agent discussion toward
systems that explicitly manage evidence, citations, reproducibility,
scientific retrieval, and evaluation.

Stanford STORM introduced a pre-writing workflow based on perspective
discovery, grounded simulated expert conversations, and outline
construction. Co-STORM extended this into collaborative exploration
where a user can observe and steer a discourse among language-model
agents while the system maintains a dynamic mind map and produces a
report. These are valuable interaction patterns, and Tempest has already
ported much of the relevant structure.

Ai2’s Asta ecosystem points toward the next layer: scientific agents
that help users find papers, summarize literature, and analyze data
while keeping outputs cited, traceable, and reproducible. Asta resources
include scientific-agent tools, an MCP-based Scientific Corpus Tool,
baseline agents, and evaluation infrastructure. AstaBench emphasizes
controlled, reproducible evaluation across literature understanding,
code execution, data analysis, and end-to-end discovery.

PaperQA2 and OpenScholar point to the retrieval and synthesis direction:
iterative query refinement, full-text retrieval, reranking, citation
traversal, contradiction detection, and citation-backed long-form
answers. The important lesson is not to copy a particular Python
codebase. It is to make retrieval, claim extraction, citation support,
contradiction handling, and self-feedback explicit stages.

## Design principle

Tempest should be organized around this invariant:

> Every factual sentence in a generated report should be traceable to a
> claim object, every claim object should be traceable to source spans,
> every source span should be traceable to retrieval actions, and every
> retrieval action should be replayable.

That single invariant implies most of the recommendations below.

## Recommendation 1: Promote `SourceStore` into an evidence ledger

### Current state

`SourceStore` currently stores sources, facts, and artifacts.
`tempest_fact()` stores an atomic claim with source IDs, confidence,
notes, tags, and creation time.

### Recommendation

Keep `SourceStore`, but add a first-class evidence ledger abstraction.
This could be implemented as a richer `SourceStore` or as a separate
`EvidenceLedger`/`ClaimStore` object that composes with `SourceStore`.

Suggested claim schema:

``` r

claim <- list(
  claim_id = "C...",
  claim_text = "...",
  claim_type = "finding",          # finding, definition, method, limitation, contradiction, open_question
  source_ids = c("S..."),
  evidence_span_ids = c("E..."),
  supporting_quotes = list(),
  contradicting_source_ids = c(),
  contradiction_note = NA_character_,
  confidence = "medium",
  support_score = NA_real_,
  contradiction_score = NA_real_,
  source_quality_score = NA_real_,
  retrieval_query = "...",
  retrieval_step_id = "R...",
  perspective_id = "...",
  persona_id = "...",
  section_id = NA_character_,
  created_at = tempest_now_utc(),
  verified_at = NA_character_,
  verifier_model = NA_character_,
  verification_status = "unverified" # supported, partial, unsupported, contradicted, unverifiable
)
```

Suggested evidence span schema:

``` r

evidence_span <- list(
  evidence_span_id = "E...",
  source_id = "S...",
  chunk_id = "...",
  quote = "...",
  start_offset = NA_integer_,
  end_offset = NA_integer_,
  page = NA_integer_,
  section_heading = NA_character_,
  relevance_score = NA_real_,
  extracted_by = "expert",
  created_at = tempest_now_utc()
)
```

### API sketch

``` r

ledger <- tempest_evidence_ledger()

ledger$add_source(source)
ledger$add_evidence_span(source_id, quote, chunk_id = NULL)
ledger$add_claim(
  claim_text,
  source_ids,
  evidence_span_ids = NULL,
  claim_type = "finding",
  confidence = "medium"
)

claims <- tempest_claims(result)
evidence <- tempest_evidence(result)
disputes <- tempest_disputes(result)
```

### Why this matters

A richer ledger unlocks citation verification, contradiction handling,
claim deduplication, report auditing, reproducibility, and better UI. It
also gives dsprrr concrete structured targets to optimize.

## Recommendation 2: Add claim-level citation verification

### Current state

Tempest currently converts inline `[S...]` citations to Markdown
footnotes. This is useful attribution, but not source-support
verification.

### Recommendation

Add a verifier pass that checks whether each cited source actually
supports the exact claim or sentence.

Suggested verification labels:

- `supported`: cited evidence directly supports the claim.
- `partially_supported`: evidence supports part of the claim, but the
  claim is too broad or too specific.
- `unsupported`: citation is related but does not support the claim.
- `contradicted`: citation conflicts with the claim.
- `unverifiable`: source text is unavailable or insufficient.

### API sketch

``` r

verified <- tempest_verify_claims(
  claims = ledger$claims,
  sources = ledger$sources,
  verifier = cfg$make_chat("judge")
)

report <- tempest_report_md(
  result,
  citation_policy = "verified_only"
)
```

For report generation:

``` r

cfg <- tempest_config(
  citation_policy = "claim_verified",  # none, source_attributed, claim_verified, strict
  min_support_score = 0.70,
  on_unsupported_claim = "revise"       # flag, drop, revise, keep_with_warning
)
```

### Implementation notes

The first implementation can be simple:

1.  Split report into sentences.
2.  Extract source IDs from each sentence.
3.  Retrieve cited source excerpts or relevant `ragnar` chunks.
4.  Ask the judge model for a support label and rationale.
5.  Rewrite, flag, or drop unsupported sentences.

A later implementation can verify the claim ledger before report
writing, which is preferable because the writer can select only verified
claims.

### Done when

- Every factual report sentence has a citation-support status.
- Unsupported citations can be listed in an audit table.
- Strict mode can prevent unsupported claims from appearing in final
  reports.

## Recommendation 3: Add `mode = "scientific"` as a retrieval and reasoning policy

### Current state

Tempest already supports native provider web search, multiple external
search providers, `ragnar` ingestion, and hybrid retrieval.

### Recommendation

Add a scientific mode that changes retrieval, source scoring, report
structure, and verification behavior.

``` r

cfg <- tempest_config(
  mode = "scientific",
  literature_provider = "semantic_scholar", # later: openalex, pubmed, crossref, asta_mcp
  retrieval_policy = "citation_graph",
  citation_policy = "claim_verified",
  contradiction_policy = "required"
)
```

The first version does not need deep Asta integration. It should define
a provider interface that can support Semantic Scholar, OpenAlex,
PubMed, Crossref, local corpora, and eventually Asta’s MCP tools.

Suggested provider interface:

``` r

tempest_lit_search(query, provider, limit = 20)
tempest_lit_fetch_metadata(paper_id, provider)
tempest_lit_fetch_abstract(paper_id, provider)
tempest_lit_fetch_fulltext(paper_id, provider)
tempest_lit_references(paper_id, provider)
tempest_lit_citations(paper_id, provider)
tempest_lit_related(paper_id, provider)
```

### Scientific source metadata

Add source fields for papers:

``` r

source$meta$paper_id
source$meta$doi
source$meta$authors
source$meta$year
source$meta$venue
source$meta$citation_count
source$meta$reference_ids
source$meta$cited_by_ids
source$meta$is_open_access
source$meta$source_type # paper, preprint, report, standard, webpage, patent, dataset
```

### Why this matters

General web search is not enough for literature synthesis. Scientific
research workflows need paper metadata, citation graph traversal,
related-paper discovery, author/venue signals, and full-text retrieval
when possible.

## Recommendation 4: Extend retrieval from search/fetch to plan/rerank/traverse/contradict

### Current state

The current retriever can search, fetch, ingest content into `ragnar`,
and retrieve chunks using hybrid/vector/BM25 methods.

### Recommendation

Add a research planner and evidence gatherer on top of the current
retriever.

Target retrieval flow:

``` text
research objective
  -> query decomposition
  -> broad search
  -> source fetch
  -> source quality scoring
  -> claim extraction
  -> relevance reranking
  -> citation graph expansion
  -> contrary evidence search
  -> evidence sufficiency check
  -> verified claim set
```

### API sketch

``` r

plan <- tempest_research_plan(
  topic = "Life cycle assessment of lithium-ion batteries",
  mode = "scientific",
  depth = "standard"
)

evidence <- tempest_gather_evidence(
  plan,
  retriever,
  policy = "search_fetch_rerank_traverse"
)

evidence <- tempest_find_contrary_evidence(evidence, retriever)
evidence <- tempest_score_evidence_sufficiency(evidence, objective = plan$objective)
```

### Evidence sufficiency rubric

Use a simple scorecard first:

- Relevance to research objective.
- Source quality.
- Source diversity.
- Recency, when relevant.
- Coverage across subquestions.
- Presence of contrary evidence.
- Full-text support rather than title/abstract-only support.
- Citation support strength.

### Why this matters

This makes Tempest less dependent on whichever sources appear in the
first search results. It also enables scientific workflows where
important sources are found through citation neighborhoods rather than
keyword match.

## Recommendation 5: Add contradiction and uncertainty handling

### Current state

Tempest fact notes have a `confidence` field and optional notes, but
contradiction handling is not yet a central workflow.

### Recommendation

Add explicit support for disputes and uncertainty.

Suggested new claim types:

- `established_finding`
- `limited_evidence`
- `conflicting_finding`
- `methodological_caveat`
- `negative_result`
- `open_question`

Suggested dispute object:

``` r

dispute <- list(
  dispute_id = "D...",
  topic = "...",
  claim_ids = c("C1", "C2"),
  axis_of_disagreement = "measurement method",
  likely_explanation = "different sample class and endpoint definition",
  evidence_balance = "mixed",
  unresolved_questions = c("...")
)
```

### Co-STORM agent roles

Add non-topical agents that appear when the discourse needs quality
control:

- `Contradiction Finder`: searches for contrary evidence and boundary
  conditions.
- `Methods Critic`: examines experimental design, measurement
  assumptions, and causal claims.
- `Citation Auditor`: checks whether citations support claims.
- `Novelty Scout`: looks for adjacent-field analogies and determines
  whether an idea is truly new or just a local recombination.
- `Practical Implementer`: translates findings into implementation
  constraints, APIs, tests, or operational concerns.

### Discourse manager actions

Extend the action enum with:

``` r

c(
  "challenge_claim",
  "seek_contrary_evidence",
  "verify_citation",
  "request_method_details",
  "ask_scope_boundary",
  "promote_open_question"
)
```

### Report default

For scientific mode, reports should include:

``` markdown
## What is established
## What is disputed
## Important caveats
## Open questions
## Recommended next searches
```

## Recommendation 6: Make the mind map claim-aware

### Current state

The mind map tracks nodes, notes, source IDs, and can split oversized
nodes into child topics.

### Recommendation

Extend mind map nodes from topic containers into evidence-status
summaries.

Suggested node fields:

``` r

node <- list(
  id = "...",
  label = "...",
  parent = NULL,
  notes = "...",
  source_ids = c("S..."),
  established_claim_ids = c("C..."),
  disputed_claim_ids = c("C..."),
  open_question_ids = c("C..."),
  weak_claim_ids = c("C..."),
  next_searches = c("..."),
  status = "mixed" # sparse, emerging, established, disputed, weakly_supported
)
```

### UI implications

The Shiny/Co-STORM interface should make epistemic status visible:

- Green/check: well-supported claims.
- Yellow/warning: weak or partial support.
- Red/conflict: disputed or contradicted claims.
- Blue/question: open questions.

Color choices can be handled later. The important product decision is
that the mind map should show what the system knows, what it doubts, and
what remains unresolved.

## Recommendation 7: Expand evaluations around research quality

### Current state

Tempest already includes `vitals` helpers for QA and Co-STORM
simulated-user evaluation.

### Recommendation

Add custom scorers and datasets that measure research behavior rather
than generic answer quality.

Suggested scorers:

``` r

tempest_score_citation_support()
tempest_score_source_recall()
tempest_score_source_precision()
tempest_score_claim_precision()
tempest_score_contradiction_handling()
tempest_score_abstention()
tempest_score_outline_coverage()
tempest_score_report_coherence()
tempest_score_session_steering()
tempest_score_claim_deduplication()
```

Suggested eval datasets:

1.  **Citation support mini-benchmark**: claims with known supporting
    and non-supporting sources.
2.  **Contradiction mini-benchmark**: topics where literature is known
    to disagree.
3.  **Gold-source recall benchmark**: questions with expected canonical
    papers or standards.
4.  **Co-STORM steering benchmark**: simulated users request pivots,
    exclusions, or deeper evidence.
5.  **Report audit benchmark**: generated reports are scored for
    unsupported factual claims.

### Example vitals task

``` r

task <- tempest_citation_support_task(
  dataset = "citation_support",
  config = tempest_config(mode = "scientific")
)

task$eval()
```

### Why this matters

Tempest’s dsprrr integration makes it unusually well suited to
optimization. Evals should drive dsprrr module improvement and
provider/model selection.

## Recommendation 8: Deduplicate facts semantically, while preserving disputes

### Current state

The roadmap already identifies unbounded fact accumulation and suggests
`max_facts` plus deduplication by claim similarity.

### Recommendation

Implement semantic claim clustering rather than simple truncation.

``` r

tempest_claim_dedupe(
  claims,
  method = "semantic_cluster",
  preserve_contradictions = TRUE,
  prefer = c("verified", "source_diverse", "specific", "recent")
)
```

Deduplication should preserve:

- The clearest claim wording.
- The strongest source set.
- Contradictory claims as dispute clusters.
- Important caveats.
- Source diversity.

Do not dedupe away contradictions. If two claims are similar but
conflict, they should become a dispute cluster rather than one being
discarded.

## Recommendation 9: Formalize run manifests and replay

### Current state

Tempest already supports persistent runs with saved JSON artifacts for
intermediate stages and final outputs.

### Recommendation

Make persistence a reproducibility contract.

Suggested run directory:

``` text
run/
  manifest.json
  config.json
  prompts/
  retrievals.parquet
  sources.parquet
  chunks.parquet
  evidence_spans.parquet
  claims.parquet
  disputes.parquet
  mindmap.json
  turns.parquet
  outlines.json
  drafts/
  reports/
  citation_audit.parquet
  evals.json
```

Suggested manifest fields:

``` json
{
  "tempest_version": "...",
  "r_version": "...",
  "started_at": "...",
  "topic": "...",
  "mode": "scientific",
  "models": {},
  "model_params": {},
  "search_provider": "...",
  "literature_provider": "...",
  "retrieval_policy": "...",
  "citation_policy": "...",
  "as_of_date": "...",
  "cache_keys": [],
  "prompt_hashes": {},
  "source_hashes": {},
  "random_seed": null
}
```

Suggested API:

``` r

tempest_manifest(result)
tempest_replay(path)
tempest_audit(path)
tempest_compare_runs(path1, path2)
```

### Why this matters

Scientific users need to know what the system saw, when it saw it, what
model was used, and how evidence moved into claims and report text. This
also creates a foundation for regression testing.

## Recommendation 10: Keep dsprrr central

### Current state

Tempest already uses dsprrr modules for structured extraction and
generation when available, and it supports module optimization from
labeled examples.

### Recommendation

Turn each high-value research operation into a dsprrr module with
explicit signatures and evals.

Suggested modules:

| Module | Purpose | Output |
|----|----|----|
| `decompose_research_question` | Break topic into searchable subquestions | queries, subquestions, intent |
| `classify_query_intent` | Distinguish fact lookup, literature review, method question, contradiction search | intent label |
| `extract_claims` | Convert source chunks into atomic claims | claim objects |
| `extract_evidence_spans` | Identify exact supporting excerpts | evidence spans |
| `verify_claim_support` | Check whether evidence supports claim | support label + score |
| `detect_contradiction` | Identify conflicting claims | dispute object |
| `classify_source_quality` | Score source type and reliability | quality score |
| `rerank_evidence` | Select best evidence for objective | ranked claims/sources |
| `generate_open_questions` | Identify gaps and unresolved questions | open-question claims |
| `write_claim_grounded_section` | Write only from verified claims | section markdown |
| `audit_report` | Validate final report | audit table |

Each module should have a tiny labeled dataset from the beginning. This
will let Tempest improve systematically instead of accumulating prompt
heuristics.

## Recommendation 11: Add stateful writing/editing tools

Asta’s WritingEditors idea is especially relevant. Tempest currently
writes sections and assembles reports, but scientific users often want
iterative editing of a persistent artifact.

Recommended addition:

``` r

editor <- tempest_report_editor(result)
editor$insert_section("What is disputed", after = "Background")
editor$revise_section("Methods", instruction = "Make caveats more explicit")
editor$attach_claims(section_id, claim_ids)
editor$audit_section(section_id)
editor$export("report.md")
```

The key constraint: edits should preserve claim IDs and citation support
wherever possible. If an edit introduces new factual claims, those
claims should return to the ledger for verification.

## Recommendation 12: Add R-native data analysis as a later scientific-mode pillar

Asta’s three-part decomposition—find papers, summarize literature,
analyze data—is a useful product model. Tempest should eventually
support all three, but data analysis should come after evidence/citation
rigor.

Possible long-term API:

``` r

res <- tempest_analyze_data(
  question = "Which variables explain yield loss?",
  data = experiment_df,
  config = tempest_config(mode = "scientific")
)
```

The data-analysis agent should produce:

- Executed R code.
- Tables and plots.
- Statistical assumptions.
- Diagnostics.
- Narrative interpretation.
- Links between data-derived claims and generated report sections.

The same evidence ledger pattern can support data claims by adding
`evidence_type = "data_analysis"`, `code_hash`, `data_hash`, and
`output_id`.

## Proposed phased roadmap

### Phase 0: Stabilize package foundations

Continue with existing roadmap items that affect correctness and
reliability:

- Structured output validation.
- Improved unit tests with mocked LLM responses.
- Better test coverage for `citations.R`, `models.R`, `storm.R`, and
  `retriever.R`.
- Shiny timeout/cancellation behavior.
- Race-condition hardening.
- Security/privacy documentation.

### Phase 1: Evidence ledger and citation verifier

Deliverables:

- Rich claim schema.
- Evidence span schema.
- Claim export/import as tibble/parquet.
- Citation verification pass.
- `citation_policy = "claim_verified"`.
- Report audit table.

This is the most important phase.

### Phase 2: Scientific retrieval mode

Deliverables:

- `mode = "scientific"`.
- Literature provider interface.
- Semantic Scholar or OpenAlex adapter.
- Paper metadata fields in source objects.
- Source quality scoring.
- Citation/reference graph traversal.

### Phase 3: Contradiction-aware Co-STORM

Deliverables:

- Contradiction Finder, Methods Critic, Citation Auditor personas.
- New discourse-manager actions.
- Dispute objects.
- Default “established/disputed/open questions” report sections.
- Claim-aware mind map state.

### Phase 4: Evaluation suite

Deliverables:

- Citation-support scorer.
- Source recall/precision scorer.
- Contradiction-handling scorer.
- Session-steering scorer.
- Claim-deduplication scorer.
- Small gold datasets.
- CI-friendly mocked model tests.

### Phase 5: Reproducibility, replay, and scientific editing

Deliverables:

- Manifest schema.
- Replay/audit/compare APIs.
- Stateful report editor.
- Section-level claim attachment.
- Strict final audit mode.

### Phase 6: Asta/MCP and data-analysis integration

Deliverables:

- Optional Asta MCP adapter if available and stable.
- Scientific Corpus Tool adapter.
- Computational analysis mode for R data frames.
- Code/data provenance in evidence ledger.

## Recommended near-term issues

If turning this into GitHub issues, start with these:

1.  **Design evidence ledger schema**  
    Define claim, evidence span, dispute, retrieval log, and citation
    audit objects.

2.  **Implement claim-level citation verification**  
    Add
    [`tempest_verify_claims()`](https://jameshwade.github.io/tempest/reference/tempest_verify_claims.md)
    and `tempest_audit_report()` using the `judge` model.

3.  **Add citation policy to config**  
    Support `source_attributed`, `claim_verified`, and `strict`
    policies.

4.  **Add claim deduplication**  
    Cluster semantically similar claims while preserving contradictions.

5.  **Add scientific source metadata**  
    Extend source objects with DOI, paper ID, year, venue, authors,
    citation count, and source type.

6.  **Create literature provider interface**  
    Add generic functions before implementing specific providers.

7.  **Implement OpenAlex or Semantic Scholar provider**  
    Start with metadata/search and add graph traversal next.

8.  **Add contradiction finder module**  
    Implement as a dsprrr module and Co-STORM specialist role.

9.  **Make mind map nodes claim-aware**  
    Add established/disputed/open-question claim IDs to node state.

10. **Add citation-support eval task**  
    Build a small local dataset and a `vitals` scorer.

## What not to prioritize yet

Do not prioritize these before evidence rigor is in place:

- Large UI redesigns.
- Many more generic personas.
- Autonomous “AI scientist” claims.
- Report export formats beyond Markdown/HTML.
- Broad MCP integration without a stable provider abstraction.
- More search providers that return generic web results.

These are useful later, but they do not address the core quality
problem: whether Tempest can make, support, audit, and revise scientific
claims reliably.

## Suggested north-star API

``` r

library(tempest)

cfg <- tempest_config(
  mode = "scientific",
  search_provider = "native",
  literature_provider = "openalex",
  retrieval_policy = "search_fetch_rerank_traverse",
  citation_policy = "claim_verified",
  contradiction_policy = "required",
  embed_fn = ragnar::embed_openai()
)

res <- tempest_run(
  "Life cycle assessment of lithium-ion batteries",
  config = cfg,
  output_dir = "storm-runs",
  resume = TRUE
)

claims <- tempest_claims(res)
disputes <- tempest_disputes(res)
audit <- tempest_audit_report(res)

cat(res$report_md)
```

For Co-STORM:

``` r

session <- tempest_session(
  "Life cycle assessment of lithium-ion batteries",
  config = cfg,
  n_experts = 4,
  specialists = c("contradiction_finder", "methods_critic", "citation_auditor")
)

session$step("Focus on recycling allocation assumptions and conflicting findings.")
session$verify_claims()
session$mindmap(status = "claim_aware")
session$report(policy = "verified_only")
```

## Success criteria

Tempest should be considered successful in this next stage when:

1.  A user can inspect every factual claim in a report.
2.  Every claim has source spans, not just source IDs.
3.  Unsupported citations are detected automatically.
4.  Contradictions are preserved and summarized rather than smoothed
    away.
5.  Scientific literature workflows can traverse citation graphs.
6.  Co-STORM can steer toward weak evidence, contrary evidence, and
    unresolved questions.
7.  Runs are replayable and auditable.
8.  Evals can detect regressions in citation quality and research
    behavior.
9.  dsprrr modules can be trained or optimized for each structured
    research step.
10. The system can produce reports that are not merely plausible, but
    inspectably grounded.

## References

- Stanford STORM: “Assisting in Writing Wikipedia-like Articles From
  Scratch with Large Language Models,” arXiv, 2024.
  <https://arxiv.org/abs/2402.14207>
- Stanford Co-STORM: “Into the Unknown Unknowns: Engaged Human Learning
  through Participation in Language Model Agent Conversations,”
  arXiv, 2024. <https://arxiv.org/abs/2408.15232>
- Ai2 Asta announcement: “Asta: Accelerating science through trustworthy
  agentic AI,” 2025. <https://allenai.org/blog/asta>
- Ai2 Asta resources: “Asta Resources: Tools for Building Scientific AI
  Agents,” 2025. <https://allenai.org/asta/resources>
- AstaBench: “Rigorous Benchmarking of AI Agents with a Scientific
  Research Suite,” arXiv, 2025. <https://arxiv.org/abs/2510.21652>
- PaperQA: “Retrieval-Augmented Generative Agent for Scientific
  Research,” arXiv, 2023. <https://arxiv.org/abs/2312.07559>
- PaperQA2: “Language agents achieve superhuman synthesis of scientific
  knowledge,” arXiv, 2024. <https://arxiv.org/abs/2409.13740>
- OpenScholar: “Synthesizing Scientific Literature with
  Retrieval-augmented LMs,” arXiv, 2024.
  <https://arxiv.org/abs/2411.14199>
