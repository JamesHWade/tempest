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
| Explore a topic interactively | [`tempest_app()`](https://jameshwade.github.io/tempest/reference/tempest_app.md) or [`tempest_session()`](https://jameshwade.github.io/tempest/reference/tempest_session.md) | A continuing Co-STORM conversation, mind map, evidence, and report |

Most package users should begin with scripted STORM. Co-STORM is useful
when the research question should evolve through dialogue. Tempest 0.2
has no application-neutral workflow or runtime API.

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

cat(tempest_report(result))

result$title
result$perspectives
result$experts
result$outline

sources <- tempest_sources(result)
claims <- tempest_claims(result)
supports <- tempest_claim_supports(result)

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
and citation-audit tables are derived projections. The authoritative
product report is read with `tempest_report(result)`.

[`tempest_report()`](https://jameshwade.github.io/tempest/reference/tempest_report.md)
is the one read accessor for both product shapes. It returns the exact
committed bytes and never generates, repairs, or republishes a report.
For Co-STORM, generate and commit with `session$publish()`, then read
those exact bytes with `tempest_report(session)`.

Every STORM and Co-STORM publication runs the exact verifier ProgramSet
stage and atomically binds claim-by-evidence-span support.
`citation_policy` controls only report rendering and unsupported-claim
handling:

- `"source_attributed"` converts known inline source IDs to footnotes
  without disabling product verification;
- `"claim_verified"` exposes threshold-verified support in the rendered
  report and final references; and
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

bundle <- tempest_promotion_bundle(result)
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

The constructor accepts only a completed
[`tempest_run()`](https://jameshwade.github.io/tempest/reference/tempest_run.md)
result or a succeeded, quiescent `TempestSession`. Independently
supplied Workspace, Manifest, and StageRecord values cannot prove the
exact completed product or its committed report.

## Review the completed trajectory

Build a bounded, reconstructable review without creating new product or
Graft authority:

``` r

review <- tempest_trajectory_review(result)
proposed_review <- tempest_trajectory_review(
  result,
  promotion_bundle = bundle
)
accepted_review <- tempest_trajectory_review(
  result,
  promotion_bundle = bundle,
  promotion_receipt = receipt
)
```

The closed ten-field projection contains the product identity, ordered
StageRecord summaries, safe terminal Deputy identities, fixed ProgramSet
references, input and optional promotion knowledge, evidence identities,
explicit joins, and structural findings. Variable lanes retain at most
250 records while binding the complete lane count and digest. It
contains no prompts, responses, source content, local paths,
credentials, live objects, or capabilities, and Tempest does not persist
it.

Join proofs distinguish authority-validated bindings and exact identity
from mere correlation. A `correlation_id` appears only in
`correlated_with` joins with `correlation_only` proof and never
establishes causation or authorship. Mutable progress events remain
outside the review identity. Proposed and accepted promotion states
require the exact completed product, and receipt-only or cross-run
combinations fail closed.

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
Co-STORM snapshot and bundle schema 10, STORM bundle schema 8 with state
schema 5, ProgramSet schema 2, research-manifest schema 3, StageRecord
output-digest payload schema 3, and promotion-bundle schema 1. Every
other version is rejected, as is any missing or extra field or value
that becomes valid only after coercion. Shared envelope primitives,
ResearchWorkspace snapshots, Co-STORM bundles, and STORM bundles have
separate product owners; no generic run-persistence reader or
compatibility layer remains.

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
    name = "Recycling Engineer",
    title = "Battery recovery specialist",
    description = "Focuses on process yield, safety, and scale-up.",
    instructions = "Separate demonstrated performance from projections.",
    focus_areas = c("hydrometallurgy", "process safety"),
    initial_questions = "Which recovery steps constrain full-scale yield?"
  ),
  tempest_expert(
    name = "Policy Analyst",
    title = "Circular-economy policy specialist",
    description = "Focuses on incentives, standards, and accountability.",
    instructions = "Compare jurisdictions and preserve policy uncertainty.",
    focus_areas = c("producer responsibility", "recycling standards"),
    initial_questions = "Which policies have changed recovery outcomes?"
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

Generated and supplied experts use the same canonical constructor.
Profiles are immutable descriptions of scientific perspectives, and
Tempest derives each profile’s `expert_id` and `version` from its six
authored fields. Keep live tools, clients, credentials, and roster state
outside the serialized profile. Product code attaches only the tools
required for its STORM or Co-STORM role, and Co-STORM retirement remains
manager-owned session-roster state rather than profile data.

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

tempest_app()
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

session$publish(
  style = "executive",
  include_references = TRUE
)
committed_report <- tempest_report(session)
cat(committed_report)

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
state, but not credentials, live chat handles, tools, or authenticated
clients. Configure fresh supported chats and retrieval dependencies
through `config` before resuming.

Snapshot, save, restore, and resume accept only the exact schema-10
Co-STORM product. Expert, transcript, mind-map, StageRecord, Workspace,
report, suggested-question, and Graft snapshot state must pass integrity
checks.

The moderator and experts use persistent Deputy agents as the required
Co-STORM runtime. Tempest disables ambient file, shell, R, web, and
package-install capabilities, then allowlists only the tools already
attached for that role. Snapshots persist canonical opaque terminal
traces for each run and never serialize the Deputy Agent or provider
credentials. Those execution identities support correlation and audit
joins only; they do not claim that an execution caused, authored, or
validated report content.

The bundled app includes Chat, STORM, Mind Map, Sources, Facts,
Transcript, Report, and Run review panels. The Run review shows the
bounded authoritative StageRecord trajectory beside separately labeled,
untrusted live progress. Co-STORM persistence is explicit bounded
archive download and upload; there is no browser-temporary autosave. The
STORM panel has no parallel perspective control and runs through the
maintained asynchronous worker path. Progress, persistence, and
successful publication use polite live status; validation, cancellation,
and publication failures use alerts.

A host app can embed that same asynchronous STORM adapter without
calling The Shiny modules are Tempest implementation details. The
bundled application is reachable only through
[`tempest_app()`](https://jameshwade.github.io/tempest/reference/tempest_app.md);
there is no supported contract for embedding its panels in a host app.

## Product boundary

Tempest supports only
[`tempest_run()`](https://jameshwade.github.io/tempest/reference/tempest_run.md)
and
[`tempest_session()`](https://jameshwade.github.io/tempest/reference/tempest_session.md)
as research product entry points, with
[`tempest_app()`](https://jameshwade.github.io/tempest/reference/tempest_app.md)
for the bundled application. A completed run is read through
[`tempest_report()`](https://jameshwade.github.io/tempest/reference/tempest_report.md),
[`tempest_sources()`](https://jameshwade.github.io/tempest/reference/tempest_sources.md),
[`tempest_claims()`](https://jameshwade.github.io/tempest/reference/tempest_claims.md),
[`tempest_claim_supports()`](https://jameshwade.github.io/tempest/reference/tempest_claim_supports.md),
and
[`tempest_trajectory_review()`](https://jameshwade.github.io/tempest/reference/tempest_trajectory_review.md);
the retriever, mutable workspace, manifest, and stage state are not part
of that surface. No compatibility layer is provided.

## Where to go next

- Use
  [`tempest_run()`](https://jameshwade.github.io/tempest/reference/tempest_run.md)
  for a scripted STORM report.
- Use
  [`tempest_session()`](https://jameshwade.github.io/tempest/reference/tempest_session.md)
  or
  [`tempest_app()`](https://jameshwade.github.io/tempest/reference/tempest_app.md)
  for interactive Co-STORM research.
- Add semantic retrieval with `ragnar` through `embed_fn` or a pre-built
  `ragnar_store`.
- Bring accepted organizational knowledge into a run with
  [`tempest_knowledge()`](https://jameshwade.github.io/tempest/reference/tempest_knowledge.md),
  which pins the immutable Graft view, names the exact accepted evidence
  records, and binds any accepted governed procedure to its stage.
- Review provisional evidence with
  [`tempest_promotion_bundle()`](https://jameshwade.github.io/tempest/reference/tempest_promotion_bundle.md)
  and
  [`tempest_graft_plan()`](https://jameshwade.github.io/tempest/reference/tempest_graft_plan.md)
  before exercising Graft acceptance authority.
- Reconstruct a non-authoritative review with
  [`tempest_trajectory_review()`](https://jameshwade.github.io/tempest/reference/tempest_trajectory_review.md).
- Capture progress by passing a `progress` callback, which receives one
  canonical plain record per event.

Start with a narrow topic, a small expert and question budget, and
durable output. Expand model roles, retrieval, parallelism, and
evaluation only after the first workflow behaves as intended.
