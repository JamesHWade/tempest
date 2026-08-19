# Scripted STORM with Tempest

Use scripted STORM when the user wants one bounded research run that produces
an evidence-backed report.

## Workflow model

Preserve Tempest's five STORM stages:

1. `perspectives`: survey seed material, discover complementary perspectives,
   generate or accept the expert pool, and record research questions.
2. `research`: decompose questions, retrieve sources, conduct expert turns,
   extract claims, and preserve source relationships.
3. `outline`: draft an outline and refine it against the collected evidence.
4. `write`: select relevant facts and write cited sections plus the lead.
5. `polish`: deterministically assemble the report, enforce citation policy,
   validate product authority, and publish the product report. Duplicate
   removal is unavailable on the authoritative path.

Do not collapse these stages when progress, resume, provenance, or failure
recovery matters.

## Choose the API

Use `tempest_run()` for the direct STORM result:

```r
config <- tempest_config(
  models = "openai/gpt-5.6-luna",
  search_provider = "wikipedia",
  citation_policy = "source_attributed",
  max_search_results = 4,
  max_sources = 12
)

result <- tempest_run(
  "What constrains grid-scale battery recycling?",
  config = config,
  n_experts = 2,
  max_questions_per_perspective = 2,
  output_dir = "tempest-runs",
  run_id = "battery-recycling",
  verbose = TRUE
)
```

Use `tempest_run_async()` when a Shiny or other event-driven host must keep its
main session responsive. Read the installed help for its result and
cancellation contract before integrating it.

## Configure deliberately

- Use a single model string for a small first run or role-specific models for
  coordinator, expert, writer, judge, and mind-map work.
- Use `"native"` search only when the selected provider and model support the
  intended web behavior. Use an explicit provider when reproducibility or
  credential requirements matter.
- Configure `embed_fn` or a prebuilt ragnar store only when semantic retrieval
  is needed.
- Choose `citation_policy` deliberately:
  - every publication verifies exact claim-by-evidence-span pairs regardless of
    rendering policy;
  - `"source_attributed"` preserves attribution while verification still runs;
  - `"claim_verified"` renders threshold-verified support;
  - `"strict"` also applies the unsupported-claim policy.
- Bound experts, questions, searches, sources, and retrieved facts before
  increasing quality or coverage budgets.
- Supply exact `tempest_expert()` profiles when the host owns expert selection.
  Keep executable tools and authenticated clients out of serialized profiles.

## Inspect the result

Treat the report as one representation of a larger research result. Inspect:

```r
cat(result$report_md)
result$perspectives
result$experts
result$outline

sources <- tempest_sources(result$workspace)
claims <- tempest_claims(result$workspace)
```

Verify the report, source relationships, claim support, citation references,
and evidence lineage required by the configured policy. Inspect warnings and
unsupported claims instead of hiding them in final prose.

`result$report_md` is the authoritative report committed by a completed STORM
product. `tempest_report_md()` only renders caller-supplied Markdown against an
explicit ResearchWorkspace; that value alone does not finalize a Manifest or
grant promotion authority. Construct a promotion proposal only from the
completed product with `tempest_promotion_bundle(result)`.

Use `tempest_trajectory_review(result)` to inspect the completed product's
ordered StageRecords, exact program and knowledge identities, safe Deputy
references, evidence identities, joins, and structural findings. The review is
bounded and reconstructable. It excludes mutable progress, raw content,
credentials, and capabilities; correlation proves grouping only, not
causation.

## Persist and resume

Pass `output_dir` and a stable `run_id` to persist direct STORM stage
artifacts. Resume with the same topic, compatible configuration, output
directory, and run identifier:

```r
result <- tempest_run(
  "What constrains grid-scale battery recycling?",
  config = config,
  output_dir = "tempest-runs",
  run_id = "battery-recycling",
  resume = TRUE
)
```

Keep durable research state and checksums. Recreate process-local functions,
clients, credentials, callbacks, and host dependencies after process restart.
Do not silently continue a run whose topic, evidence policy, or runtime
assumptions no longer match.

Current STORM persistence accepts only a schema-7 bundle with schema-4 state.
Older, future, missing, extra, coerced, or mismatched shapes are rejected rather
than migrated. `parallel_research` must remain `FALSE`; use
`parallel_writing = TRUE` only for already-grounded section writing when
appropriate.

The default `tempest_task()` solver evaluates this real product path and returns
the authoritative report with a bounded credential-safe trajectory summary.
It can evaluate an exact caller dataset and optional ProgramSet/knowledge view.
Those controls are unavailable to a custom solver because Tempest could not
verify that it used them. Keep baseline and candidate vitals Tasks separate;
compile selected stages explicitly and never imply automatic adoption.

## Verify

Use deterministic fake chats and retrieval fixtures for package tests. Verify:

- stage order and terminal failure events;
- resume from each completed stage;
- source, claim, citation, and report provenance;
- selected-expert identity boundaries;
- cancellation before further work begins;
- asynchronous cleanup and stale-result rejection in hosts;
- the rendered report and downloads when the UI is part of the behavior.
