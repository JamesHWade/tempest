# Create a STORM configuration

Create a STORM configuration

## Usage

``` r
tempest_config(
  models = NULL,
  params = NULL,
  chat_fn = NULL,
  tools = NULL,
  embed_fn = NULL,
  ragnar_store = NULL,
  artifact_store = NULL,
  search_provider = "native",
  cache_dir = NULL,
  cache_enabled = TRUE,
  cache_ttl = Inf,
  max_search_results = 8,
  max_search_queries_per_turn = 3,
  retrieve_top_k = 25,
  max_sources = 24,
  user_agent = "tempest (R; +https://github.com/JamesHWade/tempest)",
  node_expansion_trigger_count = NULL,
  enable_discourse_manager = FALSE,
  max_active_experts = 5L,
  enable_unseen_surfacing = FALSE,
  citation_policy = "source_attributed",
  min_support_score = 0.7,
  on_unsupported_claim = "flag"
)
```

## Arguments

- models:

  Named list of model identifiers for each role, or a single string to
  use for all roles.

- params:

  Additional parameters passed to chat creation.

- chat_fn:

  Custom chat factory function. Should accept `role`, `model`,
  `system_prompt`, and `echo` arguments and return an ellmer-compatible
  Chat object. Use this for custom providers like `chat_company()`.

- tools:

  Additional tools to register with each chat. Can be a list of ellmer
  tools, or a function that returns tools (e.g., `btw::btw_tools`).

- embed_fn:

  Embedding function for RAG. Should accept a character vector and
  return a matrix of embeddings. Use
  [`ragnar::embed_openai()`](https://ragnar.tidyverse.org/reference/embed_ollama.html),
  [`ragnar::embed_ollama()`](https://ragnar.tidyverse.org/reference/embed_ollama.html),
  or a custom function. If provided, a ragnar store is automatically
  created.

- ragnar_store:

  A pre-built ragnar store. If NULL and `embed_fn` is provided, a store
  is created automatically with the tempest metadata schema.

- artifact_store:

  Optional artifact-store adapter from
  [`tempest_artifact_store()`](https://jameshwade.github.io/tempest/reference/tempest_artifact_store.md)
  or
  [`tempest_memory_artifact_store()`](https://jameshwade.github.io/tempest/reference/tempest_memory_artifact_store.md).

- search_provider:

  Search provider: "native" (use provider's built-in web search when
  available), "wikipedia", "you", "bing", "serper", "brave",
  "duckduckgo", "tavily", "searxng", "google", or "azure_ai_search".
  Default is "native" which uses OpenAI, Anthropic, or Google's native
  web search capabilities when available, falling back to "wikipedia".

- cache_dir:

  Path to cache directory.

- cache_enabled:

  Whether retriever search/fetch calls should read from and write to the
  on-disk cache.

- cache_ttl:

  Maximum cache age in seconds. Defaults to `Inf`, which keeps cached
  search/fetch entries valid until explicitly cleared.

- max_search_results:

  Maximum search results to return.

- max_search_queries_per_turn:

  Maximum decomposed queries per research turn.

- retrieve_top_k:

  Maximum facts/chunks retrieved for each section.

- max_sources:

  Maximum sources to keep.

- user_agent:

  User agent string for HTTP requests.

- node_expansion_trigger_count:

  Number of notes/sources per node that triggers expansion (NULL =
  disabled).

- enable_discourse_manager:

  Whether to enable LLM-driven discourse management in Co-STORM.

- max_active_experts:

  Maximum number of active expert agents in Co-STORM.

- enable_unseen_surfacing:

  Whether to surface undiscussed sources in Co-STORM.

- citation_policy:

  Citation enforcement policy: one of "none", "source_attributed",
  "claim_verified", or "strict".

- min_support_score:

  Minimum support score in `[0, 1]` for a claim to be considered
  supported.

- on_unsupported_claim:

  Action for unsupported claims: one of "flag", "drop", "revise", or
  "keep_with_warning".

## Value

A `TempestConfig` S7 object.

## Examples

``` r
cfg <- tempest_config()
cfg <- tempest_config(search_provider = "wikipedia")
```
