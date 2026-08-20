# tempest

<!-- badges: start -->
[![R-CMD-check](https://github.com/JamesHWade/tempest/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/JamesHWade/tempest/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/JamesHWade/tempest/graph/badge.svg)](https://app.codecov.io/gh/JamesHWade/tempest)
<!-- badges: end -->

An R-native implementation of [STORM](https://storm.genie.stanford.edu/) (Synthesis of Topic Outlines through Retrieval and Multi-perspective Question Asking) and [Co-STORM](https://github.com/stanford-oval/storm) from Stanford's STORM project.

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
- **Parallel section writing** from already-grounded evidence (optional)
- **Interactive multi-agent moderation** with mind map (Co-STORM)
- **Deterministic dialogue projections** with an explicit expert roster
- **Automated evaluation** with simulated users

Built on the R AI ecosystem:

- [ellmer](https://github.com/tidyverse/ellmer) — LLM orchestration: tool calling, structured output, streaming
- [ragnar](https://github.com/tidyverse/ragnar) — RAG: chunking, embedding, semantic retrieval
- [dsprrr](https://github.com/JamesHWade/dsprrr) — structured extraction/generation modules
- [Deputy](https://github.com/JamesHWade/deputy) — persistent, permission-bounded Co-STORM agent execution
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

Co-STORM moderator and expert chats run through required persistent Deputy
agents. Tempest disables ambient file, shell, R, web, and package-install
authority at that boundary and allowlists only the Tempest tools already
attached to each chat. Session persistence stores credential-safe opaque run,
session, agent, stage, role, expert, and correlation references; it never
serializes a Deputy Agent or provider credentials.

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

bundle <- tempest_promotion_bundle(result)
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

Promotion accepts only a completed `tempest_run()` result or a succeeded,
quiescent `TempestSession`. A loose Workspace, Manifest, or StageRecord tuple
cannot reconstruct publication authority. The bundle contains the selected
Sources, Claims, EvidenceSpans,
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

## Review a completed run

`tempest_trajectory_review()` reconstructs a bounded, read-only review from one
exact completed STORM result or succeeded, quiescent Co-STORM session:

```r
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

The ten-field value identifies the product and contains ordered StageRecord
summaries, safe terminal Deputy identities, the fixed ProgramSet references,
input and optional promotion knowledge, evidence identities, explicit joins,
and structural findings. Variable lanes retain at most 250 records and report
the complete count and digest when rows are omitted. The review contains no
prompts, responses, source text, paths, credentials, live objects, or
capabilities, and it is reconstructable rather than persisted.

Every join names its relation and proof. Exact run, stage, program, snapshot,
bundle, and receipt identities can establish a validated binding;
`correlation_id` can establish only `correlated_with` with
`correlation_only` proof. It never establishes authorship or causation.
Mutable progress events are intentionally outside the review identity. A
promotion bundle is shown as proposed, and an exact matching bundle plus
receipt is shown as accepted; a receipt alone or a cross-run product fails
closed.

The current persistence line accepts only `ResearchWorkspace` snapshot schema 5,
Co-STORM snapshot and bundle schema 9, STORM bundle schema 7 with state schema
4, ProgramSet schema 2, research-manifest schema 3, StageRecord output-digest payload
schema 3, and promotion-bundle schema 1. Readers reject every other version;
missing fields, extra fields, and values that only become valid after coercion
are errors.

## Product boundary

Tempest 0.2 supports only the STORM and Co-STORM product APIs. Use
`tempest_run()` for scripted research and `tempest_session()` for interactive
research. The former application-neutral workflow, runtime, capability,
connection, skill, deliverable, and artifact kernel and its symbols have been
removed. There is no compatibility or generic-kernel migration layer.

Product bundles contain the exact research manifest, provisional workspace,
product state, report, and optional immutable Graft snapshot. Fixed scientific
transformations carry dsprrr program identity, while open-ended agent calls
carry Deputy execution identity. Those identities support correlation and
audit joins only; they do not claim that an execution caused, authored, or
validated report content.

## Agent skills

Tempest ships two supported research skills.

- `use-tempest-research` chooses, configures, runs, resumes, inspects, and
  embeds Tempest's scripted STORM and interactive Co-STORM workflows.
- `conduct-storm-research` carries the provider- and framework-neutral STORM
  and Co-STORM protocols. It can guide another implementation or be loaded by
  a tool-capable chat host without calling Tempest APIs.

List the supported skill directories or install them into the directory used
by your agent:

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

These Agent Skills are external operating guidance. They do not grant runtime
authority; the host still owns tools, authorization, state, and lifecycle.

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

### Experimental OpenTelemetry traces

Tempest can project bounded STORM, stage-execution, and Co-STORM
moderator-completion, turn-commit, warmup, and report traces through a provider
configured by the host. Start R with the exact Tempest-only scope and generative
AI content capture disabled:

```sh
OTEL_R_EMIT_SCOPES=io.github.jameshwade.tempest \
OTEL_INSTRUMENTATION_GENAI_CAPTURE_MESSAGE_CONTENT=false \
R
```

Then opt in for that R process before starting a run:

```r
options(tempest.otel.enabled = TRUE)
```

The integration is experimental and off by default. Tempest does not configure
the OpenTelemetry provider, exporter, endpoint, credentials, or shutdown; the
host owns those settings. The Tempest projection excludes prompts, responses,
queries, evidence, source content, URLs, paths, identifiers, and progress
payloads.

Set the privacy environment before starting R and any Mirai workers. The exact
Tempest-only scope suppresses automatic Shiny, Mirai, ellmer, and httr2 spans,
which are separate telemetry contracts that Tempest has not audited. In
particular, httr2 records the complete request URL as `url.full`. Tempest's
Google Custom Search request places both `GOOGLE_SEARCH_API_KEY` and the
research query in URL query parameters, so enabling the httr2 scope could
expose a credential and research content. Do not broaden the supported scope
without a separate privacy review.

#### Local OTLP/HTTP Collector

Install [`otelsdk`](https://otelsdk.r-lib.org/reference/collecting.html) in the
host environment, then save this traces-only Collector configuration as
`otel-collector.yaml`:

```yaml
receivers:
  otlp:
    protocols:
      http:
        endpoint: 0.0.0.0:4318

exporters:
  debug:
    verbosity: detailed

service:
  pipelines:
    traces:
      receivers: [otlp]
      exporters: [debug]
```

Run the Collector with its OTLP/HTTP port bound only to the local interface:

```sh
docker run --rm --name tempest-otel-collector \
  -p 127.0.0.1:4318:4318 \
  -v "$PWD/otel-collector.yaml:/etc/otelcol/config.yaml:ro" \
  otel/opentelemetry-collector:0.158.0
```

Start a fresh R process configured to send only Tempest traces to that local
Collector:

```sh
OTEL_TRACES_EXPORTER=http \
OTEL_EXPORTER_OTLP_ENDPOINT=http://127.0.0.1:4318 \
OTEL_EXPORTER_OTLP_PROTOCOL=http/protobuf \
OTEL_R_EMIT_SCOPES=io.github.jameshwade.tempest \
OTEL_INSTRUMENTATION_GENAI_CAPTURE_MESSAGE_CONTENT=false \
OTEL_SERVICE_NAME=tempest-local \
R
```

The local example needs no exporter header or credential. For a short-lived
smoke test, the host may also set `OTEL_BSP_SCHEDULE_DELAY=100` so the SDK's
batch processor exports promptly. Tempest does not flush or shut down the
provider.

[Logfire](https://pydantic.dev/docs/logfire/guides/otel-collector/otel-collector-overview/)
is one optional operator-selected destination downstream of the Collector. It
is not a Tempest dependency, API, default, endpoint, SDK, or schema. Keep any
backend token and exporter configuration in operator-owned Collector secrets,
never in Tempest configuration, source code, examples, snapshots, or bundles.

Report polishing is deterministic. `remove_duplicate = TRUE` is unsupported
and fails before report publication.

You can also use a single model for all roles:

```r
cfg <- tempest_config(models = "anthropic/claude-sonnet-4-20250514")
```

### Parallel section writing

Perspective research is deliberately sequential so each open-ended expert turn
can be synchronously bound to one terminal Deputy trace. Setting
`parallel_research = TRUE` is an error. Already-grounded report sections can be
written in parallel using [mirai](https://github.com/r-lib/mirai):

```r
res <- tempest_run(
  "Quantum computing applications",
  config = cfg,
  parallel_writing = TRUE,  # requires mirai
  verbose = TRUE
)
```

### STORM persistence

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

Each run directory is an exact current schema-7 STORM product bundle with
schema-4 state. It includes checksummed JSON state for perspectives, experts,
sources, claims, outlines, and references; Markdown drafts; and the final
Markdown report. Resume rejects older, future, missing, extra, coerced, or
mismatched shapes rather than migrating them.

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
| `chat_fn` | `NULL` | Custom chat factory: `function(role, model, system_prompt, echo)` |
| `cache_dir` | `NULL` | Cache directory; defaults to `tempdir()/tempest-cache` or `TEMPEST_CACHE_DIR` env var |
| `cache_enabled` | `TRUE` | Whether search and fetch calls read from and write to the cache |
| `cache_ttl` | `Inf` | Maximum cache age in seconds before search/fetch entries are refreshed |
| `max_search_results` | `8` | Maximum results per search query |
| `max_search_queries_per_turn` | `3` | Maximum decomposed search queries per research turn |
| `retrieve_top_k` | `25` | Maximum facts/chunks retrieved for each section |
| `max_sources` | `24` | Maximum sources to track per session |
| `max_active_experts` | `5` | Co-STORM: maximum concurrent active experts |

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

## Deputy execution identity

Co-STORM moderator and expert calls run through Deputy with product-owned tools.
Tempest persists only credential-safe terminal execution identities for
correlation and audit joins. These references do not grant authority and do not
assert causal content provenance.

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

# Generate and commit a report (styles: "technical" or "executive")
session$report(style = "technical", include_references = TRUE)
committed_report <- tempest_session_report_md(session)
cat(committed_report)
```

`session$report()` validates and commits the canonical Co-STORM report.
`tempest_session_report_md()` only reads those exact committed bytes from a
succeeded, quiescent session; it does not generate or repair a report.
`tempest_report_md()` is a separate deterministic renderer over caller-supplied
Markdown and an explicit `ResearchWorkspace`. Rendering alone does not finalize
a Manifest, publish a product, or grant promotion authority.

### Expert delegation

The moderator receives one scoped
`delegate_to_expert(expert_id, question)` tool. It resolves the active roster by
stable expert id, so display-name changes cannot redirect work. Each expert:

- Has one persistent Deputy-backed chat session with conversation continuity
- Receives only the role-specific tools needed for that run
- Can use host-provided web or evidence tools to find and cite sources
- Extracts claims automatically after each response
- Keeps an opaque, host-inaccessible chat session binding for continuity

The moderator uses the same bounded Deputy runtime. Each completed moderator or
expert run records an opaque terminal trace that is carried through Co-STORM
snapshot and bundle persistence.

Co-STORM save, snapshot, restore, and resume accept only the exact current
schema-9 product. `partial_recovery = TRUE` is an explicit narrow exception for
the optional `artifacts/suggested_questions.json` presentation file; expert,
transcript, mind-map, StageRecord, Workspace, report, and Graft snapshot state
must always pass integrity checks. Live chats, tools, credentials, clients,
callbacks, and Shiny reactives are recreated rather than serialized.

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

### Dynamic Expert Roster

Experts can be added or retired during a session:

```r
session$add_expert("quantum error correction")
session$retire_expert("expert.sarah-chen")
session$get_active_experts()
```

The roster respects `max_active_experts` (default 5). Retired experts' tools return a notice that the expert is no longer available.

### Report Generation

```r
session$reorganize_mindmap()
report <- session$report(style = "technical", include_references = TRUE)
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
- **STORM tab**: Scripted research through a responsive asynchronous worker
- **Mind Map tab**: Real-time knowledge graph visualization
- **Sources tab**: Table of all retrieved sources with metadata
- **Facts tab**: Extracted facts with citations and confidence
- **Transcript tab**: Ordered public Co-STORM turns
- **Report tab**: Rendered markdown report with footnotes
- **Run review tab**: Bounded authoritative StageRecord review beside clearly
  labeled, untrusted live progress observations

Features:

- Configurable number of experts and optional warmup phase
- Report style selection (technical or executive)
- Bounded Co-STORM session download and upload with archive validation
- Polite live status for progress, persistence, and publication, with alerts
  for validation, cancellation, and publication failures
- Dark mode toggle

Session archives are explicit downloads and uploads. The app does not claim
browser-temporary autosave, and its STORM panel does not expose unsupported
parallel perspective research. STORM runs in a `shiny::ExtendedTask` backed by
Mirai so the Shiny session remains responsive.

Host apps can embed the same maintained asynchronous STORM path:

```r
ui <- bslib::page_fillable(
  tempest_shiny_ui("research", panels = "storm")
)

server <- function(input, output, session) {
  tempest_shiny_server(
    "research",
    config = tempest_config(),
    panels = "storm"
  )
}

shiny::shinyApp(ui, server)
```

The installed minimal example is available at
`system.file("examples/shiny-host/app.R", package = "tempest")`. The public
store has 13 product-named members: `peek_costorm_session`, `costorm_session`,
`costorm_workspace`, `set_costorm_session`, `touch_costorm_session`,
`save_costorm_session`, `resume_costorm_session`,
`costorm_persistence_status`, `report_md`, `report_workspace`, `report_topic`,
`publish_costorm_report`, and `publish_storm_report`. The server returns exactly
10 members: `store`, `costorm_session`, `costorm_events`, `costorm_evidence`,
`storm_events`, `report_md`, `report_workspace`, `report_topic`,
`report_navigation_event`, and `touch_costorm_session`.

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

Unless a solver is supplied explicitly, each sample runs a real
`tempest_run()` product and returns its authoritative report. Solver metadata
contains only a versioned review reference, exact product identity, fixed
program artifact and evaluator identities, and a ten-stage structural summary.
It never logs the full trajectory review, live chats, clients, tools, prompts,
responses, paths, or credentials. Pass a caller data frame with exact `input`,
`target`, and optional unique `id` columns to evaluate a content-addressed local
benchmark.

### Co-STORM evaluation with SimulatedUser

Run automated Co-STORM sessions with a simulated curious researcher:

```r
# Standalone SimulatedUser
session <- tempest_session("AI safety")
sim <- SimulatedUser$new("AI safety", max_turns = 5)
sim$run_session(session, warmup = TRUE, verbose = TRUE)
session$report()
report <- tempest_session_report_md(session)

# As a vitals task
tsk <- tempest_costorm_task(dataset = "qa", max_turns = 5, scorer_chat = judge)
```

The default Co-STORM solver likewise completes a real `TempestSession`, reads
its committed report through `tempest_session_report_md()`, and records only
the same bounded credential-safe product, program, and stage summary.

### Explicit program improvement loop

Tempest keeps evaluation, compilation, and adoption separate. Use distinct
vitals tasks for an exact baseline and candidate, compile only explicitly
selected dsprrr stages, and adopt the candidate only after reviewing its
scores and trajectory summaries:

```r
baseline_task <- tempest_task(
  dataset = held_out,
  config = cfg,
  scorer_chat = judge,
  program_set = program_set,
  knowledge_view = knowledge_view
)
baseline_task$eval()

candidate_program_set <- tempest_compile_programs(
  program_set,
  trainsets = stage_trainsets,
  valsets = stage_validation_sets,
  teleprompters = teleprompters,
  path = "candidate-programs"
)

candidate_task <- tempest_task(
  dataset = held_out,
  config = cfg,
  scorer_chat = judge,
  program_set = candidate_program_set,
  knowledge_view = knowledge_view
)
candidate_task$eval()

# Inspect baseline and candidate samples/metrics, then make an explicit choice.
baseline_samples <- baseline_task$get_samples()
candidate_samples <- candidate_task$get_samples()
chosen_program_set <- candidate_program_set

next_result <- tempest_run(
  "Grid-scale battery recycling",
  config = cfg,
  program_set = chosen_program_set,
  knowledge_view = knowledge_view
)
```

Keep compilation training and validation examples separate from the held-out
evaluation set to avoid leakage, overfitting, and optimistic scores. Compare
the same sample IDs, inputs, targets, and scorer before selecting a candidate.
Tempest does not invent training data, choose a teleprompter, optimize
automatically, accept a changed governed artifact, or replace the baseline
ProgramSet. Supply an explicit scorer or let the task use
`vitals::model_graded_qa()` with the chosen judge chat. Compilation uses
isolated Module copies; a changed artifact loses the baseline stage's governed
reference until the new artifact is reviewed and accepted separately.

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
