# stormr

An R-native implementation of [STORM](https://storm.genie.stanford.edu/) (Synthesis of Topic Outlines through Retrieval and Multi-perspective Question Asking) and [Co-STORM](https://co-storm.genie.stanford.edu/) from Stanford's STORM project.

This package reproduces the core workflow primitives:
- **Multi-perspective research** with automatically generated expert personas
- **Evidence tracking** with citations and source attribution
- **Outline-to-draft writing** for structured report generation
- **Interactive multi-agent moderation** with mind map (Co-STORM)

Built on the R AI ecosystem:
- [ellmer](https://github.com/tidyverse/ellmer) — LLM orchestration: tool calling, structured output, streaming
- [ragnar](https://github.com/tidyverse/ragnar) — RAG: chunking, embedding, semantic retrieval
- [shinychat](https://github.com/posit-dev/shinychat) — interactive chat UI
- [vitals](https://github.com/tidyverse/vitals) — evaluation tasks

## Installation

```r
# install.packages("pak")
pak::pak("JamesHWade/stormr")
```

## Setup

### LLM provider credentials

`ellmer` reads provider credentials from environment variables. For OpenAI, for example:

```r
Sys.setenv(OPENAI_API_KEY = "<your key>")
```

### Search provider

By default, `stormr` uses **native provider web search** when available (OpenAI, Anthropic, Google). This leverages each provider's built-in web search capabilities for better results.

To use alternative search providers:

```r
cfg <- storm_config(
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
library(stormr)

cfg <- storm_config(
  # search_provider = "native" is the default (uses OpenAI/Anthropic/Google native search)
  models = list(
    coordinator = "openai/gpt-4o-mini",
    expert = "openai/gpt-4o-mini",
    writer = "openai/gpt-4o",
    judge = "openai/gpt-4o-mini",
    mindmap = "openai/gpt-4o-mini"
  )
)

res <- storm_run("Life cycle assessment of lithium-ion batteries", config = cfg, verbose = TRUE)

cat(res$report_md)
```

You can also use a single model for all roles:

```r
cfg <- storm_config(models = "anthropic/claude-sonnet-4-20250514")
```

### Configuration Options

`storm_config()` accepts the following parameters:

| Parameter | Default | Description |
|-----------|---------|-------------|
| `models` | `"openai/gpt-4o-mini"` | Single model string or named list with `coordinator`, `expert`, `writer`, `mindmap`, `judge` |
| `search_provider` | `"native"` | Search backend: `"native"`, `"wikipedia"`, `"serper"`, `"brave"`, `"tavily"` |
| `embed_fn` | `NULL` | Embedding function for RAG (e.g., `ragnar::embed_openai()`) |
| `ragnar_store` | `NULL` | Pre-built ragnar store; auto-created if `embed_fn` provided |
| `chat_fn` | `NULL` | Custom chat factory: `function(role, model, system_prompt, echo)` |
| `tools` | `NULL` | Additional ellmer tools (e.g., `btw::btw_tools()`) |
| `cache_dir` | `"~/.stormr"` | Cache directory for searches and fetched content |
| `max_search_results` | `8` | Maximum results per search query |
| `max_sources` | `24` | Maximum sources to track per session |

## RAG with ragnar

Enable semantic search over fetched sources by providing an embedding function:
```r
library(stormr)

cfg <- storm_config(
  embed_fn = ragnar::embed_openai(),  # or embed_ollama(), custom function
  search_provider = "wikipedia"
)

res <- storm_run("Quantum computing applications", config = cfg)
```

When `embed_fn` is provided, stormr automatically:
- Chunks fetched web content using `ragnar::markdown_chunk()`
- Stores chunks with metadata (source_id, url, title, perspective)
- Registers a semantic retrieve tool with chat agents

You can also provide a pre-built ragnar store:
```r
store <- storm_create_ragnar_store(
  embed_fn = ragnar::embed_openai(),
  cache_dir = "~/.stormr_cache"
)
cfg <- storm_config(ragnar_store = store)
```

## Custom Chat Functions

Use custom LLM providers (e.g., internal APIs) with the `chat_fn` parameter:
```r
cfg <- storm_config(
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
cfg <- storm_config(
  tools = btw::btw_tools()
  # or: tools = list(my_custom_tool1, my_custom_tool2)
)
```

## Interactive Co-STORM

Co-STORM provides an interactive multi-expert research experience with automatically generated expert personas.

### Console Usage

```r
library(stormr)

# Create a session - personas are generated automatically
session <- costorm_session("AI safety and alignment", n_experts = 3)

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
session <- costorm_session("Quantum computing")
session$step("What is quantum supremacy?")

# With warmup - experts research first
session <- costorm_session("Quantum computing")
session$warmup()  # Each expert answers 2-4 initial questions
session$step("What is quantum supremacy?")  # Benefits from prior research
```

### Shiny App

```r
library(stormr)
run_stormchat()
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

## vitals evals

```r
library(stormr)
library(vitals)
library(ellmer)

# scorer chat must be provided for model-graded scoring:
judge <- ellmer::chat("openai/gpt-4o-mini")

tsk <- stormr_task(dataset = "qa", scorer_chat = judge)
tsk$eval(view = interactive())

tsk$get_samples()
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
- **Stanford STORM Project**: https://storm.genie.stanford.edu/
