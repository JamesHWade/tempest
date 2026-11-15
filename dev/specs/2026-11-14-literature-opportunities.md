# Literature opportunities for Tempest

Date: 2026-11-14
Status: Research recommendations, not an implementation commitment
Scope: Scientific synthesis, evidence review, and evaluation

## Naming and evidence boundary

Google **Deep Review** is a technical-document verification product, publicly
announced on **2026-10-14**. It evolved from the Paper Assistant Tool (PAT),
whose preprint appeared on **2026-06-26**. It checks manuscripts and proposes
corrections; this is a different objective from discovering literature and
synthesizing an answer. Its conference pilots supplied author feedback outside
formal acceptance decisions. The public launch describes later deployments
than the June paper, so their deployment counts should not be mixed.
[Google announcement](https://research.google/blog/analyzing-technical-documents-with-deep-review/)
[PAT paper](https://arxiv.org/abs/2606.28277v1)

**DeepReview** by Zhu, Weng, Yang, and Zhang is a separate **ACL July 2025**
paper. It combines structured paper analysis, retrieval, and evidence-based
review argumentation. Its review-preference scores do not establish accurate
error detection or comprehensive literature coverage.
[ACL paper](https://aclanthology.org/2025.acl-long.1420/)

The proposals below are design inferences from these sources, not claims that
their results transfer to Tempest. They build on Tempest's existing claims,
evidence spans, support judgments, disputes, research workspace, and session
state. Fixed structured stages belong in dsprrr; bounded investigations can use
Deputy; accepted knowledge remains in Graft. The current ownership rationale is
recorded in [Commons ecosystem alignment](2026-08-27-commons-ecosystem-alignment.md).

## Recommended opportunities

### 1. Review a claim dossier before strengthening its support

PAT allocates review work by logical manuscript segment, permits overlapping
segments, gives specialists full-document context, and grounds/deduplicates the
combined findings. Its reported math/proof evaluation contains only 26 papers
and 29 errors, with a different grader from original SPOT; pilot satisfaction
is not independently measured correctness.
[PAT, 2026-06-26, sections 2–3](https://arxiv.org/html/2606.28277v1)

**Proposed slice:** add an optional structured review of one important claim:
relevant methods/results spans, assumptions, conflicting evidence, proposed
counterexample, and resolution status. Pin the evidence snapshot; preserve the
original claim and review findings. Require concrete reproducible defects and
independent adjudication before changing a scientific-validity judgment. Exact
claim-span verification already supplies a useful foundation, but finding a
matching quote does not establish that the underlying science is valid.

**Acceptance:** curated fixtures include a valid claim, genuine flaw, apparent
contradiction resolved by context, and insufficient evidence. Measure issue
precision, missed flaws, and human adjudication time, with a fixed call budget.

### 2. Spend follow-up retrieval on explicit evidence gaps

OpenScholar combines scientific retrieval, reranking, iterative feedback,
additional retrieval, and citation verification. Its ablations favor combined
retrieval over web-only search. The Nature publication is **2026-02-04**;
benchmark sources and judgments have earlier time cutoffs. The authors caution
that representative papers can still be missed and synthesis is not fully
automated.
[OpenScholar, methods and limitations](https://pmc.ncbi.nlm.nih.gov/articles/PMC12935541/)

**Proposed slice:** turn report review into a bounded list of missing evidence
questions tied to claims or outline sections. Retrieve and rerank specifically
for those questions; record whether each gap was resolved, remained unresolved,
or exhausted its budget. Keep the source set and report revision reproducible.

**Acceptance:** compare one-pass synthesis with one targeted repair round on
frozen local corpora. Evaluate additional supported coverage, citation errors,
latency, and cost. A longer report or more citations alone is not improvement.

### 3. Evaluate literature coverage separately from organization

TaxoBench separates topic-to-literature discovery from a condition that supplies
the expert paper set, isolating organization. Its domain is LLM surveys, and
expert-cited sets are useful references rather than exhaustive truth. Initial
preprint: **2026-01-18**; inspected version: **v6, 2026-10-22**.
[TaxoBench](https://arxiv.org/abs/2601.12369v6)

**Proposed slice:** make the Co-STORM mind map expose which sources and claims
justify each branch, which important comparisons are missing, and which papers
remain unassigned. An unexplained branch or an unassigned source becomes a
reviewable gap rather than disappearing during reorganization.

**Acceptance:** evaluate the same research questions twice, first with discovery
enabled and then with a fixed expert-selected corpus. Score essential-source
coverage separately from useful grouping and cross-paper comparison. This tells
maintainers whether to change retrieval or synthesis.

### 4. Treat the support verifier as a measured instrument

A **2026-07-10 preprint** reports that verifier choice changes measured citation
failure rates on identical outputs, and proposes human-gold calibration. Its
statistical guard bounds a specified citation flagging behavior under calibration
conditions; it does not guarantee a scientific conclusion is true.
[Evaluating and guarding citation faithfulness](https://arxiv.org/abs/2607.20527v1)

**Proposed slice:** retain verifier/model/prompt versions and the evidence
presented for each support judgment. Distinguish a quote's presence from whether
it supports the claim, and retain disagreement or abstention. Build a small
human-adjudicated calibration set before adding numerical confidence claims.

**Acceptance:** include scope inflation, reversed direction, omitted caveats,
partial support, and correct support. Report unsupported-claim detection and
false alarms separately; preserve results when swapping verifiers. Offline
tests validate record handling; an explicit evaluation run measures models.

### 5. Test whether plausible false evidence survives synthesis

MisKnow-Agent injects task-specific misleading documents with controlled
authority cues into research workflows. Its controlled experiments support
testing intermediate evidence handling, not an estimate of everyday failure
prevalence. Initial preprint: **2026-07-23**; inspected version: **v2,
2026-07-30**.
[Is deep research reliable?](https://arxiv.org/abs/2607.20891v2)

**Proposed slice:** add a local adversarial corpus with one plausible but false
source, an authoritative-looking restatement, and independent contradicting
evidence. Require conflicting claims to retain their source lineage through
mind-map updates, report synthesis, persistence, and promotion proposals.

**Acceptance:** assess false-conclusion adoption at intermediate and final
stages, not merely whether the suspicious document was detected once. Check
that disagreement survives resume and that a repeated assertion does not become
independent corroboration.

## Suggested sequence

Start with the evaluation split and verifier fixtures (3–4); they create the
evidence needed to accept changes. Then implement one evidence-gap repair pass
(2), followed by the claim-review experiment (1). Run the misleading-evidence
fixtures (5) across both. These are additions to research behavior and visible
evidence handling, not a new orchestration or persistence framework.

Use deterministic local fixtures for package tests. Keep live-model evaluation
opt-in and record corpus date/version, model identifiers, prompts, budget,
rubric, and human adjudications. Never substitute a judge-model preference,
author satisfaction survey, or another system's published score for measured
Tempest behavior.
