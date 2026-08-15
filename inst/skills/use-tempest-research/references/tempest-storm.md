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
5. `polish`: assemble the report, remove duplication when requested, enforce
   citation policy, and publish the product report.

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
  - `"source_attributed"` preserves attribution;
  - `"claim_verified"` verifies extracted claims;
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

sources <- tempest_sources(result$store)
claims <- tempest_claims(result$store)
```

Verify the report, source relationships, claim support, citation references,
and evidence lineage required by the configured policy. Inspect warnings and
unsupported claims instead of hiding them in final prose.

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

## Verify

Use deterministic fake chats and retrieval fixtures for package tests. Verify:

- stage order and terminal failure events;
- resume from each completed stage;
- source, claim, citation, and report provenance;
- selected-expert identity boundaries;
- cancellation before further work begins;
- asynchronous cleanup and stale-result rejection in hosts;
- the rendered report and downloads when the UI is part of the behavior.
