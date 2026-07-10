# tempest upstream comparison

Analysis date: 2026-06-27

Compared snapshots:

- `stanford-oval/storm`: `fb951af`
  <https://github.com/stanford-oval/storm>
- `JamesHWade/dsprrr`: `e0a91ed`
  <https://github.com/JamesHWade/dsprrr>
- `tidyverse/ellmer`: `2370f31`
  <https://github.com/tidyverse/ellmer>
- `posit-dev/shinychat`: `1f7b89c`
  <https://github.com/posit-dev/shinychat>

## Summary

`tempest` already implements the main STORM and Co-STORM shape in R:
multi-perspective research, expert personas, source/fact tracking, two-step
outline generation, section writing, polishing, Co-STORM sessions, mind maps,
discourse management, optional `ragnar`, optional provider-native web search
through `ellmer`, and a Shiny UI backed by `shinychat`.

The largest implementation gap was not missing prompts; it was that dsprrr was
advertised as a structured-program layer but the scripted STORM pipeline still
called `ellmer::chat_structured()` directly for most structured steps. This pass
adds real dsprrr modules with explicit output schemas and safe ellmer fallbacks.

## Upstream STORM capabilities

The current Python runner has these core pipeline pieces:

- Separate model slots for conversation simulation, question asking, outline
  generation, article generation, and polishing.
- Perspective-guided question asking and simulated expert conversation.
- Query decomposition with a configurable maximum number of search queries per
  turn.
- Retriever abstraction with a broad catalog, including web search engines and
  vector/document retrieval.
- Two-step outline generation: direct draft followed by research-grounded
  refinement.
- Section writing from retrieved references with a configurable retrieve-top-k.
- Polishing with optional duplicate removal.
- Per-run artifact persistence: conversation logs, raw search results, outlines,
  articles, references, run config, and LLM call history.
- Co-STORM warm start, turn management, moderator questions from unused
  retrieved information, dynamic shared mind map, and report generation.

## tempest coverage after this pass

Implemented or already present:

- Role-specific model configuration through `TempestConfig`.
- Provider-native web search registration for supported `ellmer` providers,
  with local retrieval fallbacks.
- Source and fact stores with citation ids.
- Perspective discovery enriched by seed source table-of-contents data.
- Expert personas and persona-specific expert prompts.
- Query decomposition, fact extraction, next-question selection, outline
  drafting/refinement, section writing, and lead generation.
- dsprrr-backed modules for the structured STORM steps above, all guarded by
  fallback to direct ellmer calls.
- Explicit dsprrr optimization through `tempest_optimize_dsprrr_modules()`,
  with save/load helpers for compiled module reuse in `tempest_run()`.
- Configurable `max_search_queries_per_turn` and `retrieve_top_k`, matching the
  upstream runner controls.
- Upstream-style HTTP retrievers for You.com, Bing, Serper, Brave,
  DuckDuckGo, Tavily, SearXNG, Google Custom Search, and Azure AI Search, plus
  Wikipedia and provider-native `ellmer` search.
- R-native VectorRM analogue through optional `ragnar` stores, semantic chunk
  retrieval, and section-level fact filtering.
- Persistent staged run artifacts through `tempest_run(output_dir = )`, with
  `resume = TRUE` loading saved stages and skipping completed work.
- Optional upstream-style duplicate removal during polishing through
  `tempest_run(remove_duplicate = TRUE)`.
- Optional upstream-style concurrent section writing through
  `tempest_run(parallel_writing = TRUE)`.
- Sequential research and optional mirai parallel research for key-question
  strategy.
- Co-STORM session, warmup, dynamic roster, discourse manager, unseen-source
  surfacing, mind-map reorganization, and report generation.
- Shiny app work already in the local tree for pipeline controls, progress UI,
  source/fact tables, mind-map visualization, transcript, downloads, and the
  shinychat development `chat_server()` API for automatic streaming,
  cancellation, greetings, and client state management. The required upstream
  shinychat PR #264 has merged, so `DESCRIPTION` pins the development package
  without a feature-branch suffix.

## Implementation note

The dsprrr integration is intentionally conservative: every module is optional,
all module runtime failures warn and fall back to the existing ellmer path, and
normalizers enforce the same downstream shape regardless of which backend
produced the structured output.
