# Get started with Tempest

Tempest brings STORM and Co-STORM research workflows to R and exposes
their shared execution machinery for other applications. It combines
multi-perspective research, source and claim tracking, cited writing,
selected or generated experts, typed artifacts, and resumable execution.

This article helps you choose a workflow and produce a first result.
Provider-backed examples are not executed while the vignette is built,
so rendering the article makes no model or search calls and requires no
provider credentials.

## Choose a workflow

Start with the interface that matches the outcome you need:

| Goal | Interface | Result |
|----|----|----|
| Create an evidence-backed report | [`tempest_run()`](https://jameshwade.github.io/tempest/reference/tempest_run.md) | A report, sources, claims, outline, and artifacts |
| Explore a topic interactively | [`run_app()`](https://jameshwade.github.io/tempest/reference/run_app.md) or [`tempest_session()`](https://jameshwade.github.io/tempest/reference/tempest_session.md) | A continuing Co-STORM conversation, mind map, evidence, and report |
| Build an application-specific outcome | [`tempest_run_workflow()`](https://jameshwade.github.io/tempest/reference/tempest_run_workflow.md) | A generic run with selected experts, approvals, events, and typed artifacts |

Most package users should begin with scripted STORM. Co-STORM is useful
when the research question should evolve through dialogue. The generic
workflow kernel is for host applications that need their own output
templates and operations.

## Install and load Tempest

Install the development package from GitHub:

``` r

# install.packages("pak")
pak::pak("JamesHWade/tempest")
```

Then load it:

``` r

library(tempest)
```

## Configure models and search

Tempest uses `ellmer` for model access. Configure the credential
required by your chosen provider outside your R source files. For the
default OpenAI models, set `OPENAI_API_KEY` in your user `.Renviron` and
restart R. Do not put credentials in a Tempest configuration, expert
profile, workflow specification, or saved run.

[`tempest_config()`](https://jameshwade.github.io/tempest/reference/tempest_config.md)
controls model roles, retrieval, evidence policy, caching, and Co-STORM
behavior. A single model string can be used for every role, or `models`
can be a named list for finer control.

This small first-run configuration uses Wikipedia search, which does not
need a separate search-provider key:

``` r

cfg <- tempest_config(
  models = "openai/gpt-5.4-mini",
  search_provider = "wikipedia",
  max_search_results = 4,
  max_sources = 12,
  citation_policy = "source_attributed"
)
```

The default `"native"` search provider uses provider-native web search
when available and falls back to Wikipedia. Other providers may require
their own environment variables.

## Create a first STORM report

[`tempest_run()`](https://jameshwade.github.io/tempest/reference/tempest_run.md)
performs five stages:

1.  discover perspectives and expert profiles;
2.  research questions from each perspective;
3.  draft and refine an outline;
4.  write cited sections; and
5.  polish and publish the report artifact.

Start with two experts and a small question budget while learning the
package:

``` r

result <- tempest_run(
  "What are the main technical and policy barriers to recycling grid-scale batteries?",
  config = cfg,
  n_experts = 2,
  max_questions_per_perspective = 2,
  output_dir = "tempest-runs",
  run_id = "grid-battery-recycling",
  verbose = TRUE
)
```

This call uses the configured model and search provider, so it incurs
provider usage and requires network access. `output_dir` creates a run
directory and persists each stage after it completes.

The returned list contains the main report and the state used to create
it:

``` r

cat(result$report_md)

result$title
result$perspectives
result$experts
result$outline

sources <- tempest_sources(result$store)
claims <- tempest_claims(result$store)

utils::head(sources[c("id", "title", "url")])
utils::head(claims[
  c(
    "claim_id",
    "claim_text",
    "source_ids",
    "verification_status",
    "support_score"
  )
])

report_artifact <- result$artifact_catalog$get("report_md")
report_artifact@status
report_artifact@media_type
report_artifact@checksum
cat(report_artifact@content)

result$artifact_catalog$list()
```

The source store is the evidence ledger. Sources, claims, support
scores, and citations remain inspectable instead of being flattened into
report text. The artifact catalog contains the typed final
representations and their validation and provenance metadata.

Choose a stronger `citation_policy` when a workflow requires verified
claims:

- `"source_attributed"` converts known inline source IDs to footnotes
  without verifying the claims;
- `"claim_verified"` runs claim verification and adds verification
  status to the final references; and
- `"strict"` also applies the configured unsupported-claim behavior to
  inline citations.

Use `min_support_score` in
[`tempest_config()`](https://jameshwade.github.io/tempest/reference/tempest_config.md)
to set the verification threshold. Under the `"strict"` policy,
`on_unsupported_claim` controls whether weak claims are flagged,
dropped, revised, or retained with a warning.

## Resume a staged run

When `output_dir` is supplied, Tempest saves completed stages, source
and claim records, report artifacts, and checksummed metadata beneath
the run directory. Resume with the same topic, configuration, output
directory, and run ID:

``` r

result <- tempest_run(
  "What are the main technical and policy barriers to recycling grid-scale batteries?",
  config = cfg,
  n_experts = 2,
  max_questions_per_perspective = 2,
  output_dir = "tempest-runs",
  run_id = "grid-battery-recycling",
  resume = TRUE,
  verbose = TRUE
)
```

Completed stages are loaded rather than rerun. Keep model, retrieval,
and workflow settings stable when continuing an existing run.

## Supply your own expert team

By default, STORM generates a topic-specific expert pool and pairs it
with the discovered perspectives. A host can instead choose exact
profiles from its own expert pool:

``` r

experts <- list(
  tempest_expert(
    expert_id = "expert.recycling-engineering",
    name = "Recycling Engineer",
    title = "Battery recovery specialist",
    description = "Focuses on process yield, safety, and scale-up.",
    instructions = "Separate demonstrated performance from projections.",
    required_capability_ids = c(
      "tempest.research.web",
      "tempest.evidence.read",
      "tempest.evidence.write"
    ),
    optional_capability_ids = "tempest.retrieval.semantic"
  ),
  tempest_expert(
    expert_id = "expert.recycling-policy",
    name = "Policy Analyst",
    title = "Circular-economy policy specialist",
    description = "Focuses on incentives, standards, and accountability.",
    instructions = "Compare jurisdictions and preserve policy uncertainty.",
    required_capability_ids = c(
      "tempest.research.web",
      "tempest.evidence.read",
      "tempest.evidence.write"
    ),
    optional_capability_ids = "tempest.retrieval.semantic"
  )
)

result <- tempest_run(
  "What are the main technical and policy barriers to recycling grid-scale batteries?",
  config = cfg,
  experts = experts,
  output_dir = "tempest-runs",
  run_id = "selected-expert-recycling",
  verbose = TRUE
)
```

Profiles are serializable definitions that declare skills and
capabilities by ID. Capability specifications may name opaque connection
reference IDs, while live tools, clients, and credentials are resolved
separately through
[`tempest_runtime()`](https://jameshwade.github.io/tempest/reference/tempest_runtime.md).

## Explore interactively with Co-STORM

Launch the bundled Shiny application for a multi-expert conversation,
live mind map, evidence tables, and report generation. The app uses
optional packages from `Suggests`; install them if they are not already
available:

``` r

pak::pak(
  c(
    "shiny",
    "bslib",
    "shinychat",
    "DT",
    "visNetwork",
    "promises",
    "later",
    "mirai",
    "zip"
  )
)

run_app()
```

For console control, create a session directly:

``` r

session <- tempest_session(
  "Grid-scale battery recycling",
  config = cfg,
  n_experts = 3
)

session$warmup(verbose = TRUE)
answer <- session$step(
  "Which barriers are most likely to constrain deployment this decade?"
)
cat(answer$answer)

report <- session$report(
  style = "executive",
  include_references = TRUE
)
cat(report)

tempest_session_save(
  session,
  "tempest-runs/grid-battery-session",
  overwrite = TRUE
)

restored <- tempest_session_resume(
  "tempest-runs/grid-battery-session",
  config = cfg
)
```

Each expert keeps its own conversation continuity and receives only the
capabilities and connections granted for that session. The moderator
delegates by stable expert ID, and evidence collected during dialogue
remains available to the final report. Session bundles preserve durable
research state, but not credentials, live chat handles, or executable
runtime bindings. Supply a fresh configuration when resuming and
reattach a custom `runtime` when the session uses host-defined
capabilities or connections.

## Build application-specific workflows

STORM and Co-STORM are built-in specializations. The same kernel can
also turn a customer request or internal objective into a host-defined
action register, implementation plan, review packet, or other typed
outcome.

The `reusable-workflows` vignette builds a complete offline example
with:

- an application-neutral objective;
- a host-selected expert;
- a versioned output template;
- runtime generator, validator, renderer, and step operations;
- artifact approval and ordered events;
- typed output inspection; and
- save/restore with explicit runtime reattachment.

Open it from an installed package:

``` r

vignette("reusable-workflows", package = "tempest")
```

On the pkgdown website, see [Build reusable workflows with
Tempest](https://jameshwade.github.io/tempest/articles/reusable-workflows.md).

## Where to go next

- Use
  [`tempest_storm_workflow_run()`](https://jameshwade.github.io/tempest/reference/tempest_storm_workflow_run.md)
  when a host wants STORM behind the generic `TempestRun` status, event,
  artifact, and persistence interface.
- Use
  [`tempest_costorm_workflow_run()`](https://jameshwade.github.io/tempest/reference/tempest_costorm_workflow_run.md)
  to wrap an interactive Co-STORM session in the same generic run model.
- Add semantic retrieval with `ragnar` through `embed_fn` or a pre-built
  `ragnar_store`.
- Optimize structured STORM modules with
  [`tempest_optimize_dsprrr_modules()`](https://jameshwade.github.io/tempest/reference/tempest_optimize_dsprrr_modules.md).
- Capture progress with
  [`tempest_progress_collector()`](https://jameshwade.github.io/tempest/reference/tempest_progress_collector.md)
  and inspect events with
  [`tempest_progress_event_data()`](https://jameshwade.github.io/tempest/reference/tempest_progress_event_data.md).
- Evaluate scripted and interactive workflows with
  [`tempest_task()`](https://jameshwade.github.io/tempest/reference/tempest_task.md)
  and
  [`tempest_costorm_task()`](https://jameshwade.github.io/tempest/reference/tempest_costorm_task.md).

Start with a narrow topic, a small expert and question budget, and
durable output. Expand model roles, retrieval, parallelism, expert
capabilities, and evaluation only after the first workflow behaves as
intended.
