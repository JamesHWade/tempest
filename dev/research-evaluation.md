# Developer research evaluation harness

This developer-only harness evaluates Tempest's claim-by-evidence-span status
labels. It is not a public package API and adds no public functions.

## Corpus limits

`inst/extdata/research-eval/corpus.json` is a frozen synthetic fixture with 30
claim–citation records. All study names, interventions, results, and source
records are fictional. The expected statuses are designed examples, not human
adjudication or scientific gold. The fixture validates that the harness can
distinguish supported, partially supported, unsupported, contradicted, and
unverifiable records; it does not establish model quality.

The train, validation, and test splits are stratified across eight challenge
types and disjoint by `topic_id`. Conflicting-study records share a topic but
retain separate citations and expected statuses. Duplicate-source records
share an origin ID and challenge a claim that overstates independent
replication. The corpus records its fixed split seed (`20261114`). The loader
checks the synthetic labels, required challenge coverage, unique case and
citation IDs, and topic separation before scoring.

## Metrics

Every row is one claim–citation support judgment. The deterministic summary
reports these measures separately:

- **Citation support correctness**: exact five-status match rate.
- **False support**: predicted `supported` or `partially_supported` when the
  expected status is `unsupported`, `contradicted`, or `unverifiable`.
- **Full-support overcall**: predicted `supported` when the expected status is
  anything other than `supported`, including evidence that is only partial.
- **False abstention**: predicted `unverifiable` among records with any
  expected judgment other than `unverifiable`.
- **Coverage**: non-`unverifiable` predictions over all records, plus coverage
  over answerable records. Correct abstention on an unverifiable record does
  not improve answerable coverage.

The summary also groups metrics by challenge, retains per-citation decisions,
and reports available input/output/total tokens, provider-reported cost,
duration, latency, and trace-event counts. Unknown telemetry remains unknown;
the harness does not substitute zero. Provider failures and missing or malformed
structured outputs are execution errors: they reduce exact accuracy and
coverage, remain visible overall and by challenge, and never count as correct
unverifiable judgments or valid abstentions. The GEPA token-efficiency objective
uses trace-reported total tokens against a configurable per-citation budget;
provider-reported cost and latency remain separate measurements.

GEPA receives textual feedback for exact status errors, false support,
full-support overcalls, false abstention, and missing or malformed status
outputs. Invalid outputs receive an execution-error penalty rather than being
treated as abstentions. Its tuning data consists only of train and validation rows.
The test split is evaluated after compilation and is never passed to `compile()`.
The report preserves `dsprrr::optimization_result()` baseline and best scores,
plus final baseline/best metrics on each split. No improvement is claimed until
an explicitly opted-in model run produces measured results.

## Offline selfcheck

Prepare a CSV with one prediction for every `case_id` in one selected split.
Required columns are `case_id` and `predicted_status`; accepted status values
are the five Tempest verification statuses. `abstain` is normalized to
`unverifiable`. Optional columns are `rationale`, `input_tokens`,
`output_tokens`, `total_tokens`, `provider_reported_cost`, `duration_s`,
`latency_ms`, and `trace_event_count`.

From the repository root, source the script and save a report to a new path:

```r
source("tools/research-eval.R")
report <- research_eval_selfcheck(
  predictions = "predictions.csv",
  split = "test",
  report_path = "dev/reports/research-eval-selfcheck.json"
)
```

The CLI accepts the same CSV input:

```sh
Rscript tools/research-eval.R selfcheck predictions.csv dev/reports/research-eval-selfcheck.json
```

The output includes the corpus SHA-256, model label for the supplied
predictions, null executable program IDs, the selected split and seed, metrics,
and `optimization_result: null`. The selfcheck calls no model or provider.
Reports are not overwritten unless `overwrite = TRUE` is supplied.

## Opt-in `dsprrr` evaluation and GEPA

The optimizer function builds `verify_claim_support` through Tempest's current
internal producer, evaluates that baseline, calls `dsprrr::compile()` with
`dsprrr::GEPA()`, then evaluates the resulting module. It requires the caller
to supply ellmer task and reflection Chats and their model identifiers:

```r
source("tools/research-eval.R")
task_chat <- ellmer::chat_openai(model = "your/provider-model")
reflection_chat <- ellmer::chat_openai(model = "your/reflection-model")

report <- research_eval_optimize(
  chat = task_chat,
  model_id = "your/provider-model",
  reflection_chat = reflection_chat,
  reflection_model_id = "your/reflection-model",
  report_path = "dev/reports/research-eval-gepa.json"
)
```

Calling `research_eval_optimize()` is the explicit opt-in to provider work. Its
small defaults are four GEPA candidates and one generation; pass larger budgets
only when intended. The report records task/reflection model IDs, baseline and
best `dsprrr` program artifact IDs, package Git head, corpus hash, split and
optimizer seeds, per-split metrics and trace costs, optimizer settings, and
`optimization_result()` baseline-versus-best scores. The held-out test set is
not used by the optimizer.

The harness loads the requested checkout only if Tempest is not already loaded.
If another Tempest installation or checkout is loaded, it stops with an
instruction to start a fresh R session; it never reloads a live namespace.
