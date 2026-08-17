# tempest

<!-- badges: start -->
[![R-CMD-check](https://github.com/JamesHWade/tempest/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/JamesHWade/tempest/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/JamesHWade/tempest/graph/badge.svg)](https://app.codecov.io/gh/JamesHWade/tempest)
<!-- badges: end -->

An R-native implementation of [STORM](https://storm.genie.stanford.edu/) (Synthesis of Topic Outlines through Retrieval and Multi-perspective Question Asking) and [Co-STORM](https://co-storm.genie.stanford.edu/) from Stanford's STORM project.

This package reproduces the core workflow primitives:

- **Multi-perspective research** with selected or automatically generated
  expert profiles
- **Provisional evidence workspaces** with citations and source attribution
- **Pair-level claim support** with exact claim, span, and source bindings
- **Research manifests** that identify configuration, programs, knowledge
  snapshots, traces, and deliverables
- **Review-only Graft promotion** with immutable acceptance receipts
- **Resumable research state** for scripted STORM and interactive Co-STORM
- **Two-step outline refinement** and **lead section generation**
- **Query decomposition** and **semantic fact retrieval**
- **Parallel research** across perspectives (optional)
- **Interactive multi-agent moderation** with mind map (Co-STORM)
- **LLM-driven discourse management** with dynamic expert roster (Co-STORM)
- **Automated evaluation** with simulated users

Built on the R AI ecosystem:

- [ellmer](https://github.com/tidyverse/ellmer) — LLM orchestration: tool calling, structured output, streaming
- [ragnar](https://github.com/tidyverse/ragnar) — RAG: chunking, embedding, semantic retrieval
- [dsprrr](https://github.com/JamesHWade/dsprrr) — structured extraction/generation modules
- [shinychat](https://github.com/posit-dev/shinychat) — interactive chat UI
- [vitals](https://github.com/tidyverse/vitals) — evaluation tasks

## Installation

```r
# install.packages("pak")
pak::pak("JamesHWade/tempest")
```

After installation, run `vignette("tempest", package = "tempest")` for the
package tour and a first STORM workflow.

## Setup

### LLM provider credentials

By default, Tempest creates OpenAI clients with
`ellmer::chat_openai(auth = "codex")`. This reuses file-backed ChatGPT
subscription authentication managed by an installed Codex CLI; no
`OPENAI_API_KEY` is required. If Codex has not stored file-backed credentials,
run `codex login -c 'cli_auth_credentials_store="file"'`.

Alternative providers and custom chat factories use the credential mechanism
configured for that provider. For example, an OpenAI API-key factory can read:

```r
Sys.setenv(OPENAI_API_KEY = "<your key>")
```

### Search provider

By default, `tempest` uses **native provider web search** when available (OpenAI, Anthropic, Google). This leverages each provider's built-in web search capabilities for better results.

To use alternative search providers:

```r
cfg <- tempest_config(
  search_provider = "wikipedia"  # or "you", "bing", "serper", "brave", "duckduckgo", "tavily", "searxng", "google", "azure_ai_search"
)
```

Provider-specific API keys for alternative search:

- Wikipedia: no API key required (fallback when native not available)
- You.com: set `YDC_API_KEY`
- Bing: set `BING_SEARCH_API_KEY`
- Serper: set `SERPER_API_KEY`
- Brave: set `BRAVE_API_KEY`
- DuckDuckGo: no API key required
- Tavily: set `TAVILY_API_KEY`
- SearXNG: set `SEARXNG_API_URL`; optionally set `SEARXNG_API_KEY`
- Google Custom Search: set `GOOGLE_SEARCH_API_KEY` and `GOOGLE_CSE_ID`
- Azure AI Search: set `AZURE_AI_SEARCH_API_KEY`,
  `AZURE_AI_SEARCH_ENDPOINT`, and `AZURE_AI_SEARCH_INDEX_NAME`

## Open knowledge as evidence

Tempest reads [Open Knowledge
Format](https://github.com/GoogleCloudPlatform/knowledge-catalog/tree/main/okf)
(OKF) directories as portable, bounded evidence:

```r
knowledge <- tempest_read_okf("knowledge/okf")
tempest_okf_concepts(knowledge)

resources <- tempest_okf_resources(
  knowledge,
  include_stale = FALSE
)

workspace <- tempest_research_workspace()
invisible(lapply(resources, workspace$upsert_retrieved_resource))

context <- tempest_okf_context(
  knowledge,
  types = c("Assessment", "Business"),
  include_stale = FALSE,
  max_concepts = 25,
  max_chars = 50000
)
```

The reader preserves each Markdown document and its metadata, derives advisory
trust and freshness signals, and never follows links or executes referenced
code. Reading, converting, and adding resources are separate operations so the
host retains the write boundary. An OKF document cannot grant capabilities,
change policy, approve output, or authorize an action.

STORM results expose a `manifest`, validated `state`, and authoritative
`workspace`; Co-STORM sessions expose the same manifest/workspace identity.
Session correlation fields and retriever configuration are read-only, while
the pinned workspace remains mutable through its explicit methods. The
workspace contains only provisional run material and opaque references to
accepted knowledge; acceptance still requires an explicit graft review and
commit.

Graft can export current or historical accepted revisions directly into this
format. Read [Use Open Knowledge Format with
Tempest](https://jameshwade.github.io/tempest/articles/open-knowledge-format.html)
for the complete handoff and safety model.

## Verify and promote research evidence

Claim verification is authoritative at the exact claim-by-evidence-span pair.
`tempest_verify_claims()` replaces the complete support set atomically, and
`tempest_claim_supports()` exposes each deterministic pair identity, source
binding, status, score, and rationale. Claim-level status and citation-audit
tables are derived views of those records; they are not separate evidence
authority.

A succeeded STORM product can be packaged for explicit Graft review:

```r
supports <- tempest_claim_supports(result$workspace)

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
# Open `store` with this exact schema using the host's chosen Graft location.
plan <- tempest_graft_plan(store, bundle)

# Acceptance authority remains an explicit Graft operation after review.
commit_result <- graft::graft_commit(store, plan)
receipt <- tempest_promotion_receipt(store, bundle, plan, commit_result)
```

The packaged schema is compiled against Graft accessor commit
`81bd3f83a3c8ee2bee22b61ff09b475f58b4f0e5`; runtime loading checks its exact
immutable build digest and never recompiles LinkML.

The bundle contains the selected Sources, Claims, EvidenceSpans,
ClaimSupports, and exact extraction and verification ProgramArtifacts. Its
closed proof projection retains the exact resources, claims, spans, and
supports needed to recompute each retained StageRecord digest. A selection
must include every output bound by each retained extraction or verification
record; Tempest rejects partial stage-output selection instead of packaging
unselected evidence. Planning is read-only: Tempest does not call
`graft::graft_commit()` on the host's behalf. The promotion directory is a
closed current-format bundle, its destination must not already exist, and any
older or extra shape is rejected. Reading requires the original bundle id as an
out-of-band trust pin; checksums stored inside the directory establish internal
consistency but are not a signature. A `GovernedProcedure` is accepted through
a separate reviewed Graft flow; research promotion never mints one.

The current persistence line accepts only `ResearchWorkspace` snapshot schema 5,
Co-STORM snapshot and bundle schema 8, STORM bundle schema 7 with state schema
4, ProgramSet and research-manifest schema 2, StageRecord output-digest payload
schema 3, and promotion-bundle schema 1. Readers reject every other version;
missing fields, extra fields, and values that only become valid after coercion
are errors.

## Frozen generic-kernel deletion inventory

> **Warning:** The experimental generic deliverable and workflow kernel remains
> only as section-10 deletion inventory in the Tempest 0.2 migration train. It
> is not a compatibility path, and no migration shim is planned. Use the STORM
> and Co-STORM product APIs for research workflows.

The frozen 0.1 output kernel previously let host applications define an
objective, versioned output contract, and runtime operations. This example is
retained only to make the deletion boundary reviewable:

```r
objective <- tempest_objective(
  "Turn the supplied request into a concise action brief",
  constraints = "Do not invent unsupported commitments",
  acceptance_criteria = c(
    "The brief includes a summary",
    "The brief includes explicit actions"
  ),
  deliverable_ids = "action-brief"
)

registry <- tempest_builtin_operation_registry()
registry$register(
  "example.generator.action_brief",
  kind = "generator",
  version = "1",
  implementation = function(context) {
    list(
      summary = context$objective@description,
      actions = context$actions
    )
  }
)
registry$register(
  "example.renderer.action_brief",
  kind = "renderer",
  version = "1",
  implementation = function(content) {
    list(
      tempest_artifact_representation(
        content = paste0(
          "# Action brief\n\n",
          content$summary,
          "\n\n## Actions\n\n- ",
          paste(content$actions, collapse = "\n- ")
        ),
        artifact_kind = "brief",
        media_type = "text/markdown"
      ),
      tempest_artifact_representation(
        content = content,
        artifact_kind = "structured",
        media_type = "application/json"
      )
    )
  }
)

spec <- tempest_deliverable_spec(
  "action-brief",
  version = "1",
  title = "Action brief",
  purpose = "Make the requested outcome actionable",
  instructions = "Preserve constraints and uncertainty.",
  required_fields = c("summary", "actions"),
  generator_id = "example.generator.action_brief",
  validator_ids = "tempest.validator.required_fields",
  renderer_ids = "example.renderer.action_brief",
  operation_versions = c(
    "example.generator.action_brief" = "1",
    "tempest.validator.required_fields" = "1",
    "example.renderer.action_brief" = "1"
  ),
  media_types = c("text/markdown", "application/json")
)

catalog <- tempest_artifact_catalog(
  store = tempest_memory_artifact_store()
)
result <- tempest_generate_deliverable(
  spec,
  context = list(
    objective = objective,
    actions = c("Confirm scope", "Prepare evidence", "Review outcome")
  ),
  registry = registry,
  catalog = catalog
)

catalog$list()
```

Specifications contain only serializable values and stable operation ids.
Runtime functions stay in the operation registry, while artifacts retain their
specification fingerprint, validation state, provenance, and content checksum.
When `requires_approval = TRUE`, a `TempestRun` publishes the validated
artifact as `awaiting_approval`, pauses without rerunning its producer, and
invokes exporters only after the host approves the output. Invalid output stays
inspectable and is never exported. A retry can explicitly replace a stable
invalid artifact from the same run, step, and specification while retaining
the prior validation diagnostics.
Inline structured content uses canonical JSON semantics: objects and arrays
restore as lists, while classed R objects, missing values, and binary content
should use an external `storage_ref`.
Supported STORM and Co-STORM bundles persist only their explicit research
manifest, workspace, product state, report, and optional immutable Graft
snapshot. They do not persist or restore generic artifact catalogs, workflow
runs, codec registries, or process-local runtime objects.

## Frozen generic-kernel deletion inventory

> **Warning:** The experimental generic workflow kernel is retained only so the
> section-10 deletion PR can remove it as a coherent unit. It is not a supported
> Tempest 0.2 product surface and must not be used for new work.

The following offline example is a deletion-owned fixture. It records the old
run model without promising bundle restoration or compatibility:

```r
expert <- tempest_expert(
  expert_id = "expert.delivery",
  name = "Delivery Analyst",
  title = "Implementation specialist",
  description = "Turns evidence into executable plans.",
  instructions = "Surface dependencies, risks, and unresolved decisions."
)

objective <- tempest_objective(
  "Prepare a reviewable implementation plan",
  acceptance_criteria = "Every action has an owner and completion signal"
)

operations <- tempest_operation_registry()
operations$register(
  "host.step.plan",
  kind = "step",
  implementation = function(objective, expert_ids) {
    list(
      objective = objective@description,
      assigned_experts = expert_ids,
      actions = c("Confirm scope", "Collect evidence", "Review outcome")
    )
  }
)

workflow <- tempest_workflow_spec(
  "host.action-plan",
  title = "Action plan",
  purpose = "Turn an objective into an actionable outcome",
  steps = list(tempest_workflow_step(
    "plan",
    title = "Plan",
    purpose = "Create the action plan",
    operation_id = "host.step.plan",
    assignment_rule = "expert.delivery"
  ))
)

run <- tempest_run_workflow(
  objective,
  workflow,
  runtime = operations,
  experts = list(expert)
)
tempest_run_status(run)
tempest_run_events(run)
```

In the frozen kernel, `tempest_runtime()` adds least-privilege skills,
capabilities, and opaque connection references. An expert profile declares what
it needs; the host grants connection IDs for that run; a connection provider
rehydrates authenticated clients only after authorization. Profiles, snapshots,
and events never contain tool closures, clients, or credentials.

The deletion-owned kernel passes process-local services through
`runtime_context` and records the resulting per-step and per-expert
authorization decisions with
`tempest_run_capability_grants()`, including per-attempt grant history for
retries, while leaving the live services themselves out of snapshots and run
bundles. Side-effecting capabilities pass through policy and approval before
their factories or step operation can execute.

`tempest_storm_workflow_run()` and `tempest_costorm_workflow_run()` are frozen
for the section-10 deletion PR; they are not supported 0.2 product entry
points. Research integrations should call `tempest_run()` or
`tempest_session()` directly.
Classed execution errors retain the failed run in `condition$run` for the
offline deletion fixture. No supported 0.2 product bundle restores that generic
state.

Run `vignette("reusable-workflows", package = "tempest")` to inspect the
offline deletion-owned baseline.

## Agent skills

Tempest ships two supported research skills and three retiring Tempest 0.1
workflow skills:

The custom-workflow design, build, and verification skills document the frozen
generic kernel and will be removed with it in Tempest 0.2.0. The two research
skills remain the supported product direction.

- `use-tempest-research` chooses, configures, runs, resumes, inspects, and
  embeds Tempest's scripted STORM and interactive Co-STORM workflows.
- `conduct-storm-research` carries the provider- and framework-neutral STORM
  and Co-STORM protocols. It can guide another implementation or be loaded by
  a tool-capable chat host without calling Tempest APIs.
- `design-tempest-workflow`, `build-tempest-workflow`, and
  `verify-tempest-workflow` are frozen deletion-owned 0.1 artifacts and are not
  part of the 0.2 product contract.

List the bundled skill directories or install them into the directory used by
your agent:

```r
tempest_agent_skills()

# Install the supported research skills for user-level Codex sessions.
tempest_install_agent_skills(
  "~/.codex/skills",
  skills = c("use-tempest-research", "conduct-storm-research")
)
```

Existing copies are preserved unless `overwrite = TRUE`. A compatible skill
loader can also discover the bundled skills from an attached Tempest package
and expose `conduct-storm-research` inside a tool-capable chat application.
The host must still provide retrieval, evidence, state, and output tools.

These Agent Skills are distinct from the frozen 0.1 `tempest_skill()` runtime
contract, which is also scheduled for removal in Tempest 0.2.0.

Read `vignette("agent-skills", package = "tempest")` to install a supported
research skill or expose portable STORM research through an ellmer client and
shinychat application.

## Scripted STORM

```r
library(tempest)

cfg <- tempest_config(
  # search_provider = "native" is the default (uses OpenAI/Anthropic/Google native search)
  models = list(
    coordinator = "openai/gpt-5.6-sol",
    expert = "openai/gpt-5.6-luna",
    writer = "openai/gpt-5.6-sol",
    judge = "openai/gpt-5.6-luna",
    mindmap = "openai/gpt-5.6-luna"
  )
)

res <- tempest_run("Life cycle assessment of lithium-ion batteries", config = cfg, verbose = TRUE)

cat(res$report_md)
```

Use `remove_duplicate = TRUE` to ask the polishing step to collapse repeated
content while preserving unique cited claims.

You can also use a single model for all roles:

```r
cfg <- tempest_config(models = "anthropic/claude-sonnet-4-20250514")
```

### Parallel Research

Run perspective research in parallel using [mirai](https://github.com/shikokuchuo/mirai):

```r
res <- tempest_run(
  "Quantum computing applications",
  config = cfg,
  parallel_research = TRUE,  # requires mirai
  parallel_writing = TRUE,   # also requires mirai
  verbose = TRUE
)
```

### Persistent Runs

Save intermediate STORM artifacts to disk and resume interrupted runs:

```r
res <- tempest_run(
  "Quantum computing applications",
  config = cfg,
  output_dir = "storm-runs"
)

# Later: loads completed stages from storm-runs/quantum-computing-applications
# and continues from the first incomplete stage.
res <- tempest_run(
  "Quantum computing applications",
  config = cfg,
  output_dir = "storm-runs",
  resume = TRUE
)
```

Each run directory includes checksummed JSON state for perspectives, experts,
sources, claims, outlines, and references; Markdown drafts; and the final
Markdown report.

Every typed ProgramSet attempt also has a durable record of its stage, program,
evaluator, trace, support decision, and execution or fallback path. Tempest
evaluates a complete structured output before changing product state, so
malformed output cannot leave a partial update. On resume, persisted running
attempts become cancelled records. Final reports append a deterministic
`Execution review` when an attempt failed, was cancelled, used a policy
fallback, or produced a grounded result that was not verified.

### Pipeline Details

`tempest_run()` executes five steps: `perspectives`, `research`, `outline`, `write`, `polish`. Key features:

- **Persistent staged runs** -- pass `output_dir` to save run artifacts after
  each completed stage; pass `resume = TRUE` to load saved artifacts and skip
  completed stages.
- **ToC-enriched perspective discovery** -- seed URLs are fetched and their table-of-contents headings are extracted to give the coordinator richer context for generating expert perspectives.
- **Query decomposition** -- each expert research question is decomposed into 2-3 targeted search queries before retrieval.
- **Semantic fact retrieval** -- when ragnar is configured, section writing uses semantic similarity to select the most relevant facts (falls back to keyword matching otherwise).
- **Two-step outline** -- a draft outline is generated from the LLM's parametric knowledge, then refined with research findings.
- **Lead section** -- a Wikipedia-style lead (2-3 paragraphs) is generated and prepended to the article.
- **Parallel section writing** -- pass `parallel_writing = TRUE` to write
  report sections concurrently with mirai, then assemble the already-grounded
  sections and evidence in deterministic outline order.
- **Governed dsprrr programs** -- every structured stage resolves an exact
  addressable program from a `TempestProgramSet`, including its verified dsprrr
  artifact ID, stage contract, and evaluator identity.

### Compiling dsprrr programs

Compile selected programs with your own labeled examples, then reuse the
complete verified set in a run:

```r
query_train <- data.frame(
  question = c("What are battery recycling bottlenecks?"),
  topic = c("Life cycle assessment of lithium-ion batteries")
)
query_train$queries <- I(list(c(
  "lithium battery recycling bottlenecks",
  "EV battery recycling capacity constraints"
)))

program_set <- tempest_program_set()
compiled_program_set <- tempest_compile_programs(
  program_set,
  trainsets = list(query_decomposition = query_train),
  teleprompters = dsprrr::LabeledFewShot(k = 1L, seed = 123L),
  path = "compiled-storm-programs"
)

res <- tempest_run(
  "Life cycle assessment of lithium-ion batteries",
  config = cfg,
  program_set = compiled_program_set
)
```

`compiled-storm-programs` contains a closed manifest and one dsprrr artifact per
stage. Loading recomputes every artifact ID before any program can execute.
Runtime registries, model clients, credentials, and executable objects never
enter ProgramSet metadata.

When resuming a custom-program run, supply a ProgramSet with the same verified
identity; moving an intact bundle is allowed. If `program_set` is omitted,
Tempest resolves its current builtins and resumes only when their artifact IDs
match the persisted run.

Co-STORM uses the same boundary: pass `program_set` to `tempest_session()` or
`tempest_shiny_server()`, and pass the matching `knowledge_view` whenever that
set contains a governed procedure. Session snapshots record the complete
ProgramSet identity but never the live view. A governed session can be restored
or resumed with its matching verified set and no view for read-only inspection.
Its next governed stage fails before provider execution unless the exact pinned
view is supplied; if a view is supplied during restore, it must match. An
ungoverned custom-program session still requires its matching verified set.

### Run an accepted governed procedure

An accepted `GovernedProcedure` binds one stage to an exact dsprrr
`ProgramArtifact`, contract, and evaluator. Resolve it through an immutable
Graft view, then place the typed reference in the ProgramSet by stage:

```r
snapshot <- graft::graft_snapshot(store)
knowledge_view <- graft::graft_at(store, snapshot)

verify_procedure <- tempest_governed_procedure_ref(
  knowledge_view,
  record_id = "governed-procedure-record-id"
)
program_set <- tempest_program_set(
  governed_procedure_refs = list(
    verify_claim_support = verify_procedure
  )
)

result <- tempest_run(
  "Life cycle assessment of lithium-ion batteries",
  config = cfg,
  program_set = program_set,
  knowledge_view = knowledge_view
)
```

Tempest verifies the accepted procedure, program artifact, revision, schema,
store, snapshot, and commit boundary again immediately before the provider
runs. The live view is transient and is never serialized. A restored or
resumed governed workflow therefore needs the matching pinned view before its
next governed stage; a stored typed reference alone cannot authorize
execution.

### Configuration options

`tempest_config()` accepts the following parameters:

| Parameter | Default | Description |
|-----------|---------|-------------|
| `models` | role-specific OpenAI defaults | Single model string or named list with `coordinator`, `expert`, `writer`, `mindmap`, `judge` |
| `search_provider` | `"native"` | Search backend: `"native"`, `"wikipedia"`, `"you"`, `"bing"`, `"serper"`, `"brave"`, `"duckduckgo"`, `"tavily"`, `"searxng"`, `"google"`, `"azure_ai_search"` |
| `embed_fn` | `NULL` | Embedding function for RAG (e.g., `ragnar::embed_openai()`) |
| `ragnar_store` | `NULL` | Pre-built ragnar store; auto-created if `embed_fn` provided |
| `artifact_store` | `NULL` | Frozen Tempest 0.1 host adapter; do not use for new integrations |
| `chat_fn` | `NULL` | Custom chat factory: `function(role, model, system_prompt, echo)` |
| `cache_dir` | `NULL` | Cache directory; defaults to `tempdir()/tempest-cache` or `TEMPEST_CACHE_DIR` env var |
| `cache_enabled` | `TRUE` | Whether search and fetch calls read from and write to the cache |
| `cache_ttl` | `Inf` | Maximum cache age in seconds before search/fetch entries are refreshed |
| `max_search_results` | `8` | Maximum results per search query |
| `max_search_queries_per_turn` | `3` | Maximum decomposed search queries per research turn |
| `retrieve_top_k` | `25` | Maximum facts/chunks retrieved for each section |
| `max_sources` | `24` | Maximum sources to track per session |
| `node_expansion_trigger_count` | `NULL` | Co-STORM: auto-split oversized mind map nodes when note count exceeds this (NULL = disabled) |
| `enable_discourse_manager` | `FALSE` | Co-STORM: use LLM-driven turn management |
| `max_active_experts` | `5` | Co-STORM: maximum concurrent active experts |
| `enable_unseen_surfacing` | `FALSE` | Co-STORM: surface undiscussed sources as moderator questions |

By default, coordinator and writer roles use `openai/gpt-5.6-sol`; expert,
mind map, and judge roles use `openai/gpt-5.6-luna`. Tempest constructs
these clients with `ellmer::chat_openai(auth = "codex")`, reusing the ChatGPT
subscription authenticated by Codex CLI. Built-in subscription clients use
lower reasoning effort for mind-map and judge calls; explicit `params` values
override these defaults. `run_app()` limits individual provider requests to 120
seconds by default, configurable with `tempest.shiny.provider_timeout_s`.
The bundled app's warmup asks each active expert for one brief,
bounded, evidence-backed orientation. Each expert reuses session evidence or
runs one targeted search, inspects at most two results, and preserves at least
one citation. Tempest then commits source-backed claims in stable expert order
and updates the shared mind map once for the complete panel. The summary says
explicitly when no citable evidence was collected. Warmup concurrency and its
120-second safety timeout are configurable with `tempest.shiny.warmup_*`
options.

To set a personal default model, add the `tempest.chat` option to your
`.Rprofile`, using the same provider/model format as `ellmer::chat()`:

```r
options(tempest.chat = "anthropic/claude-sonnet-4-20250514")
```

You can also supply a configured ellmer Chat object:

```r
options(
  tempest.chat = ellmer::chat_ollama(model = "qwen3.5:9b")
)
```

Tempest clones a configured Chat for each role, prepends its role-specific
system prompt, and retains the Chat's provider settings and existing system
instructions. Explicit `models` and `chat_fn` arguments take precedence over
the option. Use either override when API-key authentication or another provider
is preferred.

## RAG with ragnar

Enable semantic search over fetched sources by providing an embedding function:

```r
library(tempest)

cfg <- tempest_config(
  embed_fn = ragnar::embed_openai(),  # or embed_ollama(), custom function
  search_provider = "wikipedia"
)

res <- tempest_run("Quantum computing applications", config = cfg)
```

When `embed_fn` is provided, tempest automatically:

- Chunks fetched web content using `ragnar::markdown_chunk()`
- Stores chunks with metadata (source_id, url, title, perspective)
- Registers a semantic retrieve tool with chat agents

You can also provide a pre-built ragnar store:

```r
store <- tempest_create_ragnar_store(
  embed_fn = ragnar::embed_openai(),
  cache_dir = "~/.tempest_cache"
)
cfg <- tempest_config(ragnar_store = store)
```

## Custom chat functions

Use custom LLM providers (e.g., internal APIs) with the `chat_fn` parameter:

```r
cfg <- tempest_config(
  chat_fn = function(role, model, system_prompt, echo) {
    my_company_chat(
      model = model,
      system_prompt = system_prompt,
      echo = echo
    )
  }
)
```

## Frozen 0.1 scoped runtime

This experimental runtime layer remains solely as section-10 deletion
inventory in the Tempest 0.2 migration train. It is not a compatibility path.
Co-STORM moves to Deputy-managed agents, permissions, and tools; hosts inject
connection implementations directly. Do not adopt or extend
`tempest_skill()`, `tempest_capability_spec()`, `tempest_connection_ref()`, or
`tempest_runtime()`.

## Interactive Co-STORM

Co-STORM provides an interactive multi-expert research experience with selected
or automatically generated expert profiles.

### Console Usage

```r
library(tempest)

# Create a session - expert profiles are generated automatically
session <- tempest_session("AI safety and alignment", n_experts = 3)

# See who's on the panel
session$get_expert_names()
#> [1] "Dr. Sarah Chen" "Prof. Marcus Webb" "Dr. Aisha Patel"

# Optional: Run warmup phase (experts research their initial questions)
session$warmup(verbose = TRUE)
#> ℹ Warmup: Dr. Sarah Chen (3 questions)
#>   Q: What are the current technical approaches to AI alignment?
#> ℹ Warmup: Prof. Marcus Webb (3 questions)
#>   Q: What governance frameworks exist for AI safety?
#> ℹ Warmup complete: 18 facts, 12 sources

# Interactive Q&A - moderator delegates to experts via tools
result <- session$step("What are the main risks from advanced AI systems?")
cat(result$answer)

# Generate report (styles: "technical" or "executive")
report <- session$report(style = "technical", include_references = TRUE)
cat(report)
```

### Expert delegation

The moderator receives one scoped
`delegate_to_expert(expert_id, question)` tool. It resolves the active roster by
stable expert id, so display-name changes cannot redirect work. Each expert:

- Has their own chat session with conversation continuity
- Receives only the role-specific tools needed for that run
- Can use host-provided web or evidence tools to find and cite sources
- Extracts claims automatically after each response
- Keeps an opaque, host-inaccessible chat session binding for continuity

This pattern provides clean separation of concerns and leverages ellmer's native tool calling.

### Warmup Phase

The optional warmup phase has each expert research their initial questions before interactive Q&A begins. This primes the knowledge base with foundational research:

```r
# Skip warmup - jump straight into Q&A
session <- tempest_session("Quantum computing")
session$step("What is quantum supremacy?")

# With warmup - experts research first
session <- tempest_session("Quantum computing")
session$warmup()  # Each expert answers 2-4 initial questions
session$step("What is quantum supremacy?")  # Benefits from prior research
```

### Discourse Manager

Enable LLM-driven turn management for autonomous multi-agent conversations:

```r
cfg <- tempest_config(enable_discourse_manager = TRUE)
session <- tempest_session("CRISPR gene editing", config = cfg)
session$warmup()

# Autonomous mode: the discourse manager decides who speaks next
result <- session$step(auto = TRUE)
```

The discourse manager decides each turn's action: which expert speaks, whether to add or retire experts, whether to probe for deeper questions, or when to end the round.

### Dynamic Expert Roster

Experts can be added or retired during a session:

```r
session$add_expert("quantum error correction")
session$retire_expert("expert.sarah-chen")
session$get_active_experts()
```

The roster respects `max_active_experts` (default 5). Retired experts' tools return a notice that the expert is no longer available.

### Mind Map Node Expansion

When `node_expansion_trigger_count` is set, oversized mind map nodes are automatically split into subtopics:
```r
cfg <- tempest_config(node_expansion_trigger_count = 8)
session <- tempest_session("AI safety", config = cfg)
```

### Report Generation

```r
# Reorganize the mind map before generating the report
report <- session$report(style = "technical", reorganize = TRUE)
```

### Shiny App

```r
library(tempest)
run_app()
```

Untouched model fields inherit `options(tempest.chat = )`; otherwise the app
uses Tempest's GPT-5.6 Sol/Luna defaults with ChatGPT subscription
authentication. Editing any model field switches the app to those explicit
per-role model selections.

The app provides:

- **Chat tab**: Multi-expert conversation with selected or generated experts,
  shinychat `chat_server()` streaming, cancellation, and greeting support
- **Mind Map tab**: Real-time knowledge graph visualization
- **Sources tab**: Table of all retrieved sources with metadata
- **Facts tab**: Extracted facts with citations and confidence
- **Report tab**: Rendered markdown report with footnotes

Features:

- Configurable number of experts and optional warmup phase
- Report style selection (technical or executive)
- Dark mode toggle

The app currently depends on the shinychat development version that provides
`chat_server()`, pinned in `DESCRIPTION`.

## Evaluation

### STORM evaluation with vitals

```r
library(tempest)
library(vitals)
library(ellmer)

judge <- ellmer::chat("openai/gpt-5.6-luna")
tsk <- tempest_task(dataset = "qa", scorer_chat = judge)
tsk$get_samples()
```

### Co-STORM evaluation with SimulatedUser

Run automated Co-STORM sessions with a simulated curious researcher:

```r
# Standalone SimulatedUser
session <- tempest_session("AI safety")
sim <- SimulatedUser$new("AI safety", max_turns = 5)
sim$run_session(session, warmup = TRUE, verbose = TRUE)
report <- session$report()

# As a vitals task
tsk <- tempest_costorm_task(dataset = "qa", max_turns = 5, scorer_chat = judge)
```

## Notes

- This package is designed as a starting point and reference implementation.
- For production use, you may want to:
  - use ragnar with domain-specific content (internal docs, databases)
  - add guardrails (policy, bias checks) and more systematic evaluations
  - customize prompts in `inst/prompts/`

## References

Papers:

- **STORM**: Shao, Y., et al. (2024). [Assisting in Writing Wikipedia-like Articles From Scratch with Large Language Models](https://arxiv.org/abs/2402.14207). NAACL 2024.
- **Co-STORM**: Jiang, Y., et al. (2024). [Into the Unknown Unknowns: Engaged Human Learning through Participation in Language Model Agent Conversations](https://arxiv.org/abs/2408.15232). EMNLP 2024.
- **DSPy**: Khattab, O., et al. (2024). [DSPy: Compiling Declarative Language Model Calls into Self-Improving Pipelines](https://arxiv.org/abs/2310.03714). ICLR 2024.

Code that inspired tempest:

- [stanford-oval/storm](https://github.com/stanford-oval/storm) — the reference STORM and Co-STORM implementation this package ports to R.
- [stanfordnlp/dspy](https://github.com/stanfordnlp/dspy) — the DSPy framework that [dsprrr](https://github.com/JamesHWade/dsprrr) brings to R for tempest's optimizable modules.
- [Stanford STORM project](https://storm.genie.stanford.edu/) — live demo and project overview.
