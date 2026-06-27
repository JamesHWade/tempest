# TempestConfig

Holds configuration for STORM / Co-STORM sessions: LLM models, prompts,
retrieval settings, and cache paths.

## Public fields

- `models`:

  Named list of model identifiers for each role.

- `params`:

  Additional parameters passed to chat creation.

- `chat_fn`:

  Custom chat factory function (optional).

- `tools`:

  Additional tools to register with chats (optional).

- `embed_fn`:

  Embedding function for RAG (optional).

- `ragnar_store`:

  A ragnar store for semantic retrieval (optional).

- `search_provider`:

  Search provider name.

- `cache_dir`:

  Path to cache directory.

- `max_search_results`:

  Maximum number of search results to return.

- `max_search_queries_per_turn`:

  Maximum decomposed queries per research turn.

- `retrieve_top_k`:

  Maximum facts/chunks retrieved for each section.

- `max_sources`:

  Maximum number of sources to keep.

- `user_agent`:

  User agent string for HTTP requests.

- `node_expansion_trigger_count`:

  Number of notes/sources per node that triggers expansion (NULL =
  disabled).

- `enable_discourse_manager`:

  Whether to enable LLM-driven discourse management in Co-STORM.

- `max_active_experts`:

  Maximum number of active expert agents in Co-STORM.

- `enable_unseen_surfacing`:

  Whether to surface undiscussed sources in Co-STORM.

## Methods

### Public methods

- [`TempestConfig$new()`](#method-TempestConfig-initialize)

- [`TempestConfig$make_chat()`](#method-TempestConfig-make_chat)

- [`TempestConfig$clone()`](#method-TempestConfig-clone)

------------------------------------------------------------------------

### `TempestConfig$new()`

Create a new TempestConfig.

#### Usage

    TempestConfig$new(
      models = NULL,
      params = NULL,
      chat_fn = NULL,
      tools = NULL,
      embed_fn = NULL,
      ragnar_store = NULL,
      search_provider = "native",
      cache_dir = NULL,
      max_search_results = 8,
      max_search_queries_per_turn = 3,
      retrieve_top_k = 25,
      max_sources = 24,
      user_agent = "tempest (R; +https://github.com/JamesHWade/tempest)",
      node_expansion_trigger_count = NULL,
      enable_discourse_manager = FALSE,
      max_active_experts = 5L,
      enable_unseen_surfacing = FALSE
    )

#### Arguments

- `models`:

  Named list of model identifiers for each role, or a single string to
  use for all roles.

- `params`:

  Additional parameters passed to chat creation.

- `chat_fn`:

  Custom chat factory function. Should accept `role`, `model`,
  `system_prompt`, and `echo` arguments and return an ellmer-compatible
  Chat object. Use this for custom providers like `chat_company()`.

- `tools`:

  Additional tools to register with each chat. Can be a list of ellmer
  tools, or a function that returns tools (e.g., `btw::btw_tools`).

- `embed_fn`:

  Embedding function for RAG. Should accept a character vector and
  return a matrix of embeddings. Use
  [`ragnar::embed_openai()`](https://ragnar.tidyverse.org/reference/embed_ollama.html),
  [`ragnar::embed_ollama()`](https://ragnar.tidyverse.org/reference/embed_ollama.html),
  or a custom function. If provided, a ragnar store is automatically
  created.

- `ragnar_store`:

  A pre-built ragnar store. If NULL and `embed_fn` is provided, a store
  is created automatically with the tempest metadata schema.

- `search_provider`:

  Search provider: "native" (use provider's built-in web search when
  available), "wikipedia", "you", "bing", "serper", "brave",
  "duckduckgo", "tavily", "searxng", "google", or "azure_ai_search".
  Default is "native" which uses OpenAI, Anthropic, or Google's native
  web search capabilities when available, falling back to "wikipedia".

- `cache_dir`:

  Path to cache directory.

- `max_search_results`:

  Maximum search results to return.

- `max_search_queries_per_turn`:

  Maximum decomposed queries per research turn.

- `retrieve_top_k`:

  Maximum facts/chunks retrieved for each section.

- `max_sources`:

  Maximum sources to keep.

- `user_agent`:

  User agent string for HTTP requests.

- `node_expansion_trigger_count`:

  Number of notes/sources per node that triggers expansion (NULL =
  disabled).

- `enable_discourse_manager`:

  Whether to enable LLM-driven discourse management in Co-STORM.

- `max_active_experts`:

  Maximum number of active expert agents in Co-STORM.

- `enable_unseen_surfacing`:

  Whether to surface undiscussed sources in Co-STORM.

------------------------------------------------------------------------

### `TempestConfig$make_chat()`

Create a chat object for a given role.

#### Usage

    TempestConfig$make_chat(role, system_prompt = NULL, echo = "none")

#### Arguments

- `role`:

  Role name (e.g., "coordinator", "writer", "expert").

- `system_prompt`:

  Optional system prompt override.

- `echo`:

  Echo setting for the chat.

#### Returns

An ellmer-compatible Chat object.

------------------------------------------------------------------------

### `TempestConfig$clone()`

The objects of this class are cloneable with this method.

#### Usage

    TempestConfig$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
