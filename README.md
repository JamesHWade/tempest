# tempest

<!-- badges: start -->
[![R-CMD-check](https://github.com/JamesHWade/tempest/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/JamesHWade/tempest/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/JamesHWade/tempest/graph/badge.svg)](https://app.codecov.io/gh/JamesHWade/tempest)
<!-- badges: end -->

An R-native implementation of [STORM](https://storm.genie.stanford.edu/) (Synthesis of Topic Outlines through Retrieval and Multi-perspective Question Asking) and [Co-STORM](https://co-storm.genie.stanford.edu/) from Stanford's STORM project.

This package reproduces the core workflow primitives:
- **Multi-perspective research** with automatically generated expert personas
- **Evidence tracking** with citations and source attribution
- **Two-step outline refinement** and **lead section generation**
- **Query decomposition** and **semantic fact retrieval**
- **Parallel research** across perspectives (optional)
- **Interactive multi-agent moderation** with mind map (Co-STORM)
- **LLM-driven discourse management** with dynamic expert roster (Co-STORM)
- **Automated evaluation** with simulated users

Built on the R AI ecosystem:
- [ellmer](https://github.com/tidyverse/ellmer) — LLM orchestration: tool calling, structured output, streaming
- [ragnar](https://github.com/tidyverse/ragnar) — RAG: chunking, embedding, semantic retrieval
- [dsprrr](https://github.com/JamesHWade/dsprrr) — structured extraction/generation modules (optional)
- [shinychat](https://github.com/posit-dev/shinychat) — interactive chat UI
- [vitals](https://github.com/tidyverse/vitals) — evaluation tasks

## Installation

```r
# install.packages("pak")
pak::pak("JamesHWade/tempest")
```

## Setup

### LLM provider credentials

`ellmer` reads provider credentials from environment variables. For OpenAI, for example:

```r
Sys.setenv(OPENAI_API_KEY = "<your key>")
```

### Search provider

By default, `tempest` uses **native provider web search** when available (OpenAI, Anthropic, Google). This leverages each provider's built-in web search capabilities for better results.

To use alternative search providers:

```r
cfg <- tempest_config(
  search_provider = "wikipedia"  # or "serper", "brave", "tavily"
)
```

Provider-specific API keys for alternative search:
- Wikipedia: no API key required (fallback when native not available)
- Serper: set `SERPER_API_KEY`
- Brave: set `BRAVE_API_KEY`
- Tavily: set `TAVILY_API_KEY`

## Scripted STORM

```r
library(tempest)

cfg <- tempest_config(
  # search_provider = "native" is the default (uses OpenAI/Anthropic/Google native search)
  models = list(
    coordinator = "openai/gpt-4o-mini",
    expert = "openai/gpt-4o-mini",
    writer = "openai/gpt-4o",
    judge = "openai/gpt-4o-mini",
    mindmap = "openai/gpt-4o-mini"
  )
)

res <- tempest_run("Life cycle assessment of lithium-ion batteries", config = cfg, verbose = TRUE)

cat(res$report_md)
```

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
  verbose = TRUE
)
```

### Pipeline Details

`tempest_run()` executes five steps: `perspectives`, `research`, `outline`, `write`, `polish`. Key features:

- **ToC-enriched perspective discovery** -- seed URLs are fetched and their table-of-contents headings are extracted to give the coordinator richer context for generating expert perspectives.
- **Query decomposition** -- each expert research question is decomposed into 2-3 targeted search queries before retrieval.
- **Semantic fact retrieval** -- when ragnar is configured, section writing uses semantic similarity to select the most relevant facts (falls back to keyword matching otherwise).
- **Two-step outline** -- a draft outline is generated from the LLM's parametric knowledge, then refined with research findings.
- **Lead section** -- a Wikipedia-style lead (2-3 paragraphs) is generated and prepended to the article.
- **dsprrr modules** -- when [dsprrr](https://github.com/JamesHWade/dsprrr) is installed, structured extraction steps (query decomposition, fact extraction, outline drafting, section writing) use dsprrr modules for more reliable structured output. Falls back to `chat$chat_structured()` otherwise.

### Configuration Options

`tempest_config()` accepts the following parameters:

| Parameter | Default | Description |
|-----------|---------|-------------|
| `models` | `"openai/gpt-5-mini"` | Single model string or named list with `coordinator`, `expert`, `writer`, `mindmap`, `judge` |
| `search_provider` | `"native"` | Search backend: `"native"`, `"wikipedia"`, `"serper"`, `"brave"`, `"tavily"` |
| `embed_fn` | `NULL` | Embedding function for RAG (e.g., `ragnar::embed_openai()`) |
| `ragnar_store` | `NULL` | Pre-built ragnar store; auto-created if `embed_fn` provided |
| `chat_fn` | `NULL` | Custom chat factory: `function(role, model, system_prompt, echo)` |
| `tools` | `NULL` | Additional ellmer tools (e.g., `btw::btw_tools()`) |
| `cache_dir` | `NULL` | Cache directory; defaults to `tempdir()/tempest-cache` or `TEMPEST_CACHE_DIR` env var |
| `max_search_results` | `8` | Maximum results per search query |
| `max_sources` | `24` | Maximum sources to track per session |
| `node_expansion_trigger_count` | `NULL` | Co-STORM: auto-split oversized mind map nodes when note count exceeds this (NULL = disabled) |
| `enable_discourse_manager` | `FALSE` | Co-STORM: use LLM-driven turn management |
| `max_active_experts` | `5` | Co-STORM: maximum concurrent active experts |
| `enable_unseen_surfacing` | `FALSE` | Co-STORM: surface undiscussed sources as moderator questions |

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

## Custom Chat Functions

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

## Additional Tools

Register additional tools (e.g., from btw) with chat agents:

```r
cfg <- tempest_config(
  tools = btw::btw_tools()
  # or: tools = list(my_custom_tool1, my_custom_tool2)
)
```

## Interactive Co-STORM

Co-STORM provides an interactive multi-expert research experience with automatically generated expert personas.

### Console Usage

```r
library(tempest)

# Create a session - personas are generated automatically
session <- tempest_session("AI safety and alignment", n_experts = 3)

# See who's on the panel
session$get_persona_names()
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

### Expert Tools (Subagent Pattern)

The moderator has access to expert tools (`ask_dr_sarah_chen`, `ask_prof_marcus_webb`, etc.) that delegate research questions to specialized experts. Each expert:

- Has their own chat session with conversation continuity
- Can use `web_search` and `fetch_url` to find and cite sources
- Extracts facts automatically after each response
- Returns session IDs for follow-up questions

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
session$retire_expert("Dr. Sarah Chen")
session$get_active_personas()
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

The app provides:

- **Chat tab**: Multi-expert conversation with auto-generated personas
- **Mind Map tab**: Real-time knowledge graph visualization
- **Sources tab**: Table of all retrieved sources with metadata
- **Facts tab**: Extracted facts with citations and confidence
- **Report tab**: Rendered markdown report with footnotes

Features:
- Configurable number of experts and optional warmup phase
- Report style selection (technical or executive)
- Dark mode toggle

## Evaluation

### STORM evaluation with vitals

```r
library(tempest)
library(vitals)
library(ellmer)

judge <- ellmer::chat("openai/gpt-4o-mini")
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

- **STORM**: Shao, Y., et al. (2024). [Assisting in Writing Wikipedia-like Articles From Scratch with Large Language Models](https://arxiv.org/abs/2402.14207). NAACL 2024.
- **Co-STORM**: Jiang, Y., et al. (2024). [Into the Unknown Unknowns: Engaged Human Learning through Participation in Language Model Agent Conversations](https://arxiv.org/abs/2408.15232). arXiv preprint.
- **Stanford STORM Project**: <https://storm.genie.stanford.edu/>
