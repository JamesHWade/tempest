# Get started with Tempest

Tempest brings STORM and Co-STORM research workflows to R. It combines
multi-perspective research, source and claim tracking, cited writing,
selected or generated experts, scientific reports, and resumable
research state.

This article helps you choose a workflow and produce a first result.
Provider-backed examples are not executed while the vignette is built,
so rendering the article makes no model or search calls and requires no
provider credentials.

## Choose a workflow

Start with the interface that matches the outcome you need:

| Goal | Interface | Result |
|----|----|----|
| Create an evidence-backed report | [`tempest_run()`](https://jameshwade.github.io/tempest/reference/tempest_run.md) | A report, sources, claims, outline, and run state |
| Explore a topic interactively | [`run_app()`](https://jameshwade.github.io/tempest/reference/run_app.md) or [`tempest_session()`](https://jameshwade.github.io/tempest/reference/tempest_session.md) | A continuing Co-STORM conversation, mind map, evidence, and report |
| Inspect the frozen generic kernel | [`tempest_run_workflow()`](https://jameshwade.github.io/tempest/reference/tempest_run_workflow.md) | Frozen experimental 0.1 behavior scheduled for removal in 0.2.0 |

Most package users should begin with scripted STORM. Co-STORM is useful
when the research question should evolve through dialogue. The
experimental generic workflow kernel is frozen; do not use it for new
integrations.

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
default OpenAI models, Tempest uses
`ellmer::chat_openai(auth = "codex")` and reuses the file-backed ChatGPT
subscription authentication managed by Codex CLI. If Codex has not
stored file-backed credentials, run
`codex login -c 'cli_auth_credentials_store="file"'`. Do not put
credentials in a Tempest configuration, expert profile, workflow
specification, or saved run.

[`tempest_config()`](https://jameshwade.github.io/tempest/reference/tempest_config.md)
controls model roles, retrieval, evidence policy, caching, and Co-STORM
behavior. A single model string can be used for every role, or `models`
can be a named list for finer control.

This small first-run configuration uses Wikipedia search, which does not
need a separate search-provider key:

``` r

cfg <- tempest_config(
  models = "openai/gpt-5.6-luna",
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
5.  polish and publish the Markdown report.

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

sources <- tempest_sources(result$workspace)
claims <- tempest_claims(result$workspace)
supports <- tempest_claim_supports(result$workspace)

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
utils::head(supports)
```

The research workspace holds run-scoped provisional evidence. Retrieved
resources, proposed claims, exact claim-by-evidence-span support
records, and citations remain inspectable instead of being flattened
into report text. The pair records are authoritative; claim summaries
and citation-audit tables are derived projections. The product report is
available directly in `result$report_md`; new callers should not depend
on the frozen 0.1 artifact catalog.

Choose a stronger `citation_policy` when a workflow requires verified
claims:

- `"source_attributed"` converts known inline source IDs to footnotes
  without verifying the claims;
- `"claim_verified"` runs the exact verifier ProgramSet stage,
  atomically replaces the complete claim-by-evidence-span support set,
  and exposes the derived claim status in the final references; and
- `"strict"` fails publication unless each publishable assertion is
  bound to a completed, provenance-bound verification result at the
  configured threshold and cites that claim’s exact source set.

Use `min_support_score` in
[`tempest_config()`](https://jameshwade.github.io/tempest/reference/tempest_config.md)
to set the verification threshold. Under the `"strict"` policy,
`on_unsupported_claim` controls whether weak claims are flagged,
dropped, revised, or retained with an explicit warning. They are never
presented as unmarked verified facts.

## Review promotion to Graft

A succeeded product can become a deterministic proposal for Graft review
without granting Tempest acceptance authority:

``` r

bundle <- tempest_promotion_bundle(
  workspace = result$workspace,
  manifest = result$manifest,
  stage_records = result$state$stage_records
)
trusted_bundle_id <- bundle@bundle_id
tempest_save_promotion_bundle(bundle, "promotion/grid-battery-recycling")
bundle <- tempest_read_promotion_bundle(
  "promotion/grid-battery-recycling",
  expected_bundle_id = trusted_bundle_id
)

schema <- tempest_graft_schema()
# Open `store` with this schema using the host's chosen Graft location.
plan <- tempest_graft_plan(store, bundle)

# Review the plan before the host explicitly accepts it.
commit_result <- graft::graft_commit(store, plan)
receipt <- tempest_promotion_receipt(store, bundle, plan, commit_result)
```

Tempest does not commit during bundle construction or planning. The
current closed promotion format contains exact Sources, Claims,
EvidenceSpans, ClaimSupports, and extraction and verification
ProgramArtifacts. A closed proof projection retains the exact resources,
claims, spans, and supports needed to recompute every retained
StageRecord digest. A selection must include every output bound by each
retained extraction or verification record; Tempest rejects partial
stage-output selection instead of packaging nonselected evidence.
Reading requires the original bundle id as an out-of-band trust pin;
bundle-local checksums establish internal consistency, not authenticity.
Older bundle shapes are rejected. Research promotion never mints a
`GovernedProcedure`; that record requires its own reviewed Graft
acceptance flow.

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

Current readers accept only `ResearchWorkspace` snapshot schema 5,
Co-STORM snapshot and bundle schema 9, STORM bundle schema 7 with state
schema 4, ProgramSet and research-manifest schema 2, StageRecord
output-digest payload schema 3, and promotion-bundle schema 1. Every
other version is rejected, as is any missing or extra field or value
that becomes valid only after coercion.

Every typed attempt is saved with its exact stage, program, evaluator,
trace, support decision, and fallback path. Structured output is
validated as a whole before product state changes. A resumed running
attempt is recorded as cancelled, and the final report adds a
deterministic `Execution review` for failed, cancelled, policy-fallback,
or nonverified grounded outcomes.

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
    instructions = "Separate demonstrated performance from projections."
  ),
  tempest_expert(
    expert_id = "expert.recycling-policy",
    name = "Policy Analyst",
    title = "Circular-economy policy specialist",
    description = "Focuses on incentives, standards, and accountability.",
    instructions = "Compare jurisdictions and preserve policy uncertainty."
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

Profiles describe a scientific perspective and stable expert identity.
Keep live tools, clients, and credentials outside the serialized
profile. The 0.1 skill, capability, and connection fields are frozen and
should not be used for new integrations.

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
role-specific tools needed for that session. The moderator delegates by
stable expert ID, and evidence collected during dialogue remains
available to the final report. Session bundles preserve durable research
state, but not credentials, live chat handles, tools, authenticated
clients, generic workflow state, or runtime registries. Configure fresh
supported chats and retrieval dependencies through `config` before
resuming.

The moderator and experts use persistent Deputy agents as the required
Co-STORM runtime. Tempest disables ambient file, shell, R, web, and
package-install capabilities, then allowlists only the tools already
attached for that role. Snapshots persist canonical opaque terminal
traces for each run and never serialize the Deputy Agent or provider
credentials.

## Frozen generic-kernel deletion inventory

> **Lifecycle notice:** The experimental generic workflow kernel is
> frozen and retained only for the section-10 deletion PR. It is not a
> supported Tempest 0.2 product surface.

The `reusable-workflows` vignette is an offline deletion-owned fixture
that records:

- an application-neutral objective;
- a host-selected expert;
- a versioned output template;
- runtime generator, validator, renderer, and step operations;
- artifact approval and ordered events;
- typed output inspection; and
- the former generic runtime boundary, which supported product bundles
  do not restore.

Open it from an installed package:

``` r

vignette("reusable-workflows", package = "tempest")
```

On the pkgdown website, see the [frozen generic workflow deletion
inventory](https://jameshwade.github.io/tempest/articles/reusable-workflows.md).

## Where to go next

- Use
  [`tempest_run()`](https://jameshwade.github.io/tempest/reference/tempest_run.md)
  for a scripted STORM report.
- Use
  [`tempest_session()`](https://jameshwade.github.io/tempest/reference/tempest_session.md)
  or
  [`run_app()`](https://jameshwade.github.io/tempest/reference/run_app.md)
  for interactive Co-STORM research.
- Add semantic retrieval with `ragnar` through `embed_fn` or a pre-built
  `ragnar_store`.
- Compile selected structured programs with
  [`tempest_compile_programs()`](https://jameshwade.github.io/tempest/reference/tempest_compile_programs.md),
  then pass the complete verified `TempestProgramSet` to STORM or
  Co-STORM.
- Resolve an accepted procedure with
  [`tempest_governed_procedure_ref()`](https://jameshwade.github.io/tempest/reference/tempest_governed_procedure_ref.md),
  bind it by stage in a ProgramSet, and pass its matching pinned
  `knowledge_view` to every governed run or session.
- Review provisional evidence with
  [`tempest_promotion_bundle()`](https://jameshwade.github.io/tempest/reference/tempest_promotion_bundle.md)
  and
  [`tempest_graft_plan()`](https://jameshwade.github.io/tempest/reference/tempest_graft_plan.md)
  before exercising Graft acceptance authority.
- Capture progress with
  [`tempest_progress_collector()`](https://jameshwade.github.io/tempest/reference/tempest_progress_collector.md)
  and inspect events with
  [`tempest_progress_event_data()`](https://jameshwade.github.io/tempest/reference/tempest_progress_event_data.md).
- Evaluate scripted and interactive workflows with
  [`tempest_task()`](https://jameshwade.github.io/tempest/reference/tempest_task.md)
  and
  [`tempest_costorm_task()`](https://jameshwade.github.io/tempest/reference/tempest_costorm_task.md).

Start with a narrow topic, a small expert and question budget, and
durable output. Expand model roles, retrieval, parallelism, and
evaluation only after the first workflow behaves as intended.
