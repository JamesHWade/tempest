# Dependency and research improvements

Date: 2026-11-14
Status: Implemented and qualified offline; live-model effectiveness evaluation remains separate

## Recommendation

Improve Tempest through a bounded research, challenge, and repair cycle. Start
with current dependency contracts and a scientific evaluation corpus, then add
targeted evidence repair and independent review. Keep execution in Deputy,
structured programs and optimization in dsprrr, scientific decisions in
Tempest, and accepted knowledge in Graft.

This proposal builds on the accompanying
[literature review](../specs/2026-11-14-literature-opportunities.md). It does
not require a new generic workflow layer or restoration of the removed public
compilation/evaluation API.

## Verified baseline

The implementation is isolated on `feat/research-challenge-repair`, based on
Tempest main `568f51d73d5cf58646396f69986157ee564e6747`. The original configured
checkout at `bfc32f6cd387ac64aa628301e110396b668d69a7` and its uncommitted work
remain untouched. Qualification uses isolated installations of current Deputy,
dsprrr, Graft, and shinymcp instead of changing the user's installed libraries.

| Package | Qualified source revision |
|---|---|
| Deputy | `f464cb660e7a8d49cb0af5ffb69bf1fd9fce1f7d` |
| dsprrr | `a0a372d515cc32ab9296fad8f91440bba84d187e` |
| Graft | `ad0ab921a02df602bb3687649e48b2e470cf23d6` |
| shinymcp | `0476fcf54a66bf82b0d8a1c0ab3bc516629b0a6b` |

Sources: [Deputy main](https://github.com/JamesHWade/deputy/commit/f464cb660e7a8d49cb0af5ffb69bf1fd9fce1f7d),
[dsprrr main](https://github.com/JamesHWade/dsprrr/commit/a0a372d515cc32ab9296fad8f91440bba84d187e),
[Tempest main](https://github.com/JamesHWade/tempest/commit/568f51d73d5cf58646396f69986157ee564e6747).

Tempest already has exact claim-by-span verification, immutable program
identities, staged commits, retained expert conversations, report publication
gates, and accepted-knowledge integration. Relevant implementation anchors are
`R/verify.R`, `R/dsprrr.R`, `R/program-set.R`, `R/deputy-adapter.R`,
`R/deputy-experts.R`, and `R/costorm-report.R`.

Current main uses GitHub Issues. Implementation and verification are tracked in
[#188](https://github.com/JamesHWade/tempest/issues/188). The original research
pass used the older checkout's Kata instructions; the implementation follows
the current main contract.

## Delivered design

- Reject Deputy cut-off endings before completion issuance, including async
  and streaming paths. Consume dsprrr's resolved structured async metadata.
- Verify citation support, then select a bounded set of claims for independent
  scientific review and adjudication using separate Chats. An optional single
  evidence search per claim expands the adjudicator's captured-source dossier.
- Keep findings advisory. Repair does not rewrite source-supported claims,
  evidence spans, citation-support decisions, or accepted Graft knowledge.
  Revising those scientific judgments requires the ordinary evidence workflow.
- Retain immutable review records, exact source excerpts and hashes, fixed
  program identities, model labels, repair outcomes, and a canonical report
  appendix. Save/load and promotion proofs reject missing or altered records.
- Provide a clearly synthetic, topic-disjoint developer evaluation corpus and
  offline metric checks, plus opt-in dsprrr evaluation and GEPA experiments.
  Human adjudication and live-model quality measurements remain prerequisites
  for claiming scientific improvements.
- Use host-controlled bounded concurrency. Model-controlled background
  specialists and executable scientific checks remain later experiments, for
  the reasons documented below.

The sections below preserve the research rationale and distinguish future
qualification from the implemented advisory-review path.

## 1. Qualify execution endings and structured async results

**Execution contract:** consume Deputy's typed run ending before
issuing a Tempest completion. The current adapter maps the stop-reason string
and accepts `trace$status == "complete"`. Current Deputy can return that same
stop reason with `RunEndingCutOff` for `max_tokens`, `context_window`, or
`content_filter`. Deterministic adapter regressions now reproduce and reject this truncated
answer path across synchronous, asynchronous, and streaming execution.
[Deputy ending contract](https://github.com/JamesHWade/deputy/blob/f464cb660e7a8d49cb0af5ffb69bf1fd9fce1f7d/R/run-ending.R)

Preserve the original ending reason and make non-answered runs ineligible for
completion issuance. Keep `SessionEnd` as the terminal event; `Stop` can now
continue the same run. Cover synchronous, asynchronous, and streaming paths,
and verify one terminal record with no authoritative commit for cut-off output.

The async adapter now consumes resolved structured Predict results instead of
relying on simple output and launch-time promise attributes alone. Use one Chat per overlapping call
and validate the final program/agent correlation. Current program artifacts
are version 6 and ellmer must be at least 0.5.0; test save/load/resume against
that producer rather than relying on the installed older package.
[dsprrr current contracts](https://github.com/JamesHWade/dsprrr/blob/a0a372d515cc32ab9296fad8f91440bba84d187e/NEWS.md)

Run focused adapter, dsprrr, persistence, and ecosystem contract tests against
isolated installations of the exact inspected heads before broader gates.

## 2. Establish a scientific evaluation corpus, then tune programs

The checked-in seed corpus is synthetic. A future human-adjudicated corpus should span correct support, omitted
caveats, scope inflation, opposite effect direction, conflicting studies,
missing evidence, and plausible false sources. Separate topic-disjoint train,
validation, and test sets. Test discovery with a topic alone and synthesis
with a fixed corpus so retrieval failures do not masquerade as writing failures.

Use dsprrr `metric_with_feedback()` and `metric_with_trace()` to give explicit
feedback about citation validity, claim coverage, false support, and budget
use. Start with instruction-only `GEPA()` tuning of extraction and verification,
then inspect `optimization_result()` and retain only held-out improvements.
Use dsprrr's normal artifact identity and Tempest's existing internal ProgramSet
binding. A developer evaluation script can do this without a public optimizer
API in Tempest.
[dsprrr GEPA implementation](https://github.com/JamesHWade/dsprrr/blob/a0a372d515cc32ab9296fad8f91440bba84d187e/R/teleprompter-gepa.R)

A directly relevant October 2026 preprint studies GEPA on a deep-research agent
and motivates topic-disjoint evaluation, multiple seeds, decomposed feedback,
and independent verifier checks. Its measured gains are not a forecast for
Tempest. Inspect optimized prompts for embedded benchmark answers.
[Prompt optimization study](https://arxiv.org/abs/2610.12601v1)

`with_decisions()` and experimental `ReAnchor()` are candidates for calibrating
a future support/abstention decision, not justification for treating the current
0.7 support threshold as a calibrated probability. Preserve the existing
scientific support statuses and measure false acceptance separately from recall.
[Calibrated decisions](https://github.com/JamesHWade/dsprrr/blob/a0a372d515cc32ab9296fad8f91440bba84d187e/vignettes/calibrated-decisions.Rmd)

GEPA checkpoint resume is limited to supported sequential Predict graphs and
plain data. The current general optimization vignette still says GEPA cannot
resume; the current implementation documents narrower support. Do not promise
resume for Deputy-backed custom Chats, tools, or Flex graphs.

## 3. Repair reviewer evidence gaps

After initial research and citation verification, select claims with weak or
conflicting support first. The reviewer identifies a concrete gap and may
suggest one search. New sources expand the independent adjudicator dossier;
retained source hashes and exact quotes bind the resulting finding. Preserve
unresolved gaps and an explicit budget or retrieval outcome. This first
implementation does not revise citation-support judgments from advisory
findings; that requires evidence extraction and ordinary claim verification.

The implementation uses one repair round and frozen local source fixtures.
A future scientific effectiveness study must show better evidence coverage
without more false-supported claims, with latency and call cost reported separately. Count independent source origins rather
than treating copied assertions as independent corroboration. This adapts
OpenScholar's retrieval/feedback cycle; see the literature note for evidence.

## 4. Pilot Deep Review-style scientific review

Build immutable claim dossiers containing the claim, methods/results context,
assumptions, supporting and conflicting spans, and source versions. Allocate
more review budget to consequential, disputed, or technically dense claims.
Reviewers need cross-section context, not just the quoted sentence.

Ask for concrete findings with evidence, a proposed check or counterexample,
severity, and an explicit unresolved state. Independently adjudicate findings
before revising scientific judgments. Keep source support distinct from
scientific validity: a paper can faithfully state an invalid inference.
An absence of detected flaws must not become proof of correctness.

This adapts Deep Review's segmentation, adaptive budgets, parallel review,
and final grounding. Add review findings at Tempest's validated value boundary;
the product owns their commit and report behavior. Begin with one dossier
and no autonomous publication into accepted Graft knowledge.
[Google's design](https://research.google/blog/analyzing-technical-documents-with-deep-review/)

## Later experiment: background specialists where overlap helps

Deputy's `background_tools()` now supports retained specialists, follow-ups,
progress, cancellation, and reserved request/tool budgets. Use it after the
single-dossier flow works, when the moderator has useful concurrent work.
Fixed fan-out verification can remain host-controlled; it need not become a
model-controlled task queue.
[Background task contract](https://github.com/JamesHWade/deputy/blob/f464cb660e7a8d49cb0af5ffb69bf1fd9fce1f7d/vignettes/background-tasks.Rmd)

These tasks belong to one run, cannot survive process restart, and cannot be
combined with a delegation graph or `TrustedResults()` policy on the lead.
Cancellation does not interrupt an active tool. Thus they do not by themselves
solve Shiny hangs or durable resume. Keep immutable worker inputs and serialize
validated workspace commits; reject late results from a cancelled session.
Test shared budgets, same-specialist queuing, cancellation, retirement,
uncollected outcomes, and actual Shiny session teardown.

## Later experiment: executable scientific checks

Once textual review improves measured outcomes, try dsprrr's factory-backed
RLM on bounded table consistency, units, algebra, or reproducible numerical
checks. Capture inputs, code, outputs, environment identity, and failures as
evidence. A successful execution is not a scientific-validity judgment.
Use an invocation-owned managed sandbox; do not expose arbitrary host execution.
This is an optional experiment, not the first migration.

## Delivery order

1. Exact-head compatibility and non-answered completion rejection.
2. Offline corpus and opt-in model evaluation; baseline before optimization.
3. One evidence-gap repair pass and instruction-only optimization experiment.
4. One independently adjudicated claim-review dossier.
5. Background execution and executable checks only when the measured workload
   benefits from them.

For runtime changes, run focused deterministic tests, the package suite,
formatting/lint checks, pkgdown validation, and R CMD check as appropriate.
Public behavior changes need NEWS and generated documentation; new topics need
the reference index. Live-model experiments remain explicitly separate from
key-free, network-free package tests.


## Offline qualification

The complete suite passed 6,439 assertions with no failures, warnings, or skips
against the isolated dependency revisions above. The final `R CMD check`
(`--no-manual --no-tests`, with the full suite run separately) reported zero
errors, warnings, or notes. `air format .`, `git diff --check`, generated
roxygen documentation, `pkgdown::check_pkgdown()`, and a full pkgdown site build
also completed successfully. An installed-package smoke check verified that
the pipeline, exported reader, and configuration are present after installation.

The bundled Shiny application was opened in Chrome: scientific review is off
by default, and enabling it reveals the evidence-search checkbox and claim
budget. Deterministic `testServer` and product-path tests cover the resulting
configuration, publication, cancellation, and persistence behavior.

Independent review reproduced and verified fixes for promise aggregation,
late sibling/timeout results, required source and quote coverage, atomic
advisory-source admission, subset promotion, failed-prediction scoring, and
namespace reuse during evaluation. A serial combined regression gate passed
277 assertions while retaining valid S7 objects across real fake-chat GEPA
execution and subsequent product operations. No live provider evaluation or
scientific accuracy improvement is claimed.

PR review then identified two additional boundary defects. Optional accepted
Graft artifacts are now excluded from review dossiers; required accepted
sources fail before model calls. Missing trace-event telemetry stays unknown,
while an explicitly retained empty event collection counts as zero. Independent
review accepted both corrections. The combined review/evaluation regression
gate passed 312 assertions with no failures, warnings, or skips, followed by a
fresh package check with zero errors, warnings, or notes and a successful
pkgdown reference check.
