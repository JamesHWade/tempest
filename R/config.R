# Configuration

#' TempestConfig
#'
#' Holds configuration for STORM / Co-STORM sessions: LLM models, prompts,
#' retrieval settings, and cache paths.
#'
#' @field models Named list of model identifiers for each role.
#' @field params Additional parameters passed to chat creation.
#' @field chat_fn Custom chat factory function (optional).
#' @field tools Additional tools to register with chats (optional).
#' @field embed_fn Embedding function for RAG (optional).
#' @field ragnar_store A ragnar store for semantic retrieval (optional).
#' @field search_provider Search provider name.
#' @field cache_dir Path to cache directory.
#' @field max_search_results Maximum number of search results to return.
#' @field max_search_queries_per_turn Maximum decomposed queries per research
#'   turn.
#' @field retrieve_top_k Maximum facts/chunks retrieved for each section.
#' @field max_sources Maximum number of sources to keep.
#' @field user_agent User agent string for HTTP requests.
#' @field node_expansion_trigger_count Number of notes/sources per node that triggers expansion (NULL = disabled).
#' @field enable_discourse_manager Whether to enable LLM-driven discourse management in Co-STORM.
#' @field max_active_experts Maximum number of active expert agents in Co-STORM.
#' @field enable_unseen_surfacing Whether to surface undiscussed sources in Co-STORM.
#'
#' @export
TempestConfig <- R6::R6Class(
  "TempestConfig",
  public = list(
    models = NULL,
    params = NULL,
    chat_fn = NULL,
    tools = NULL,
    embed_fn = NULL,
    ragnar_store = NULL,
    search_provider = NULL,
    cache_dir = NULL,
    max_search_results = NULL,
    max_search_queries_per_turn = NULL,
    retrieve_top_k = NULL,
    max_sources = NULL,
    user_agent = NULL,
    node_expansion_trigger_count = NULL,
    enable_discourse_manager = NULL,
    max_active_experts = NULL,
    enable_unseen_surfacing = NULL,

    #' @description
    #' Create a new TempestConfig.
    #' @param models Named list of model identifiers for each role, or a single
    #'   string to use for all roles.
    #' @param params Additional parameters passed to chat creation.
    #' @param chat_fn Custom chat factory function. Should accept `role`,
    #'   `model`, `system_prompt`, and `echo` arguments and return an
    #'   ellmer-compatible Chat object. Use this for custom providers like
    #'   `chat_company()`.
    #' @param tools Additional tools to register with each chat. Can be a list
    #'   of ellmer tools, or a function that returns tools (e.g., `btw::btw_tools`).
    #' @param embed_fn Embedding function for RAG. Should accept a character
    #'   vector and return a matrix of embeddings. Use `ragnar::embed_openai()`,
    #'   `ragnar::embed_ollama()`, or a custom function. If provided, a ragnar
    #'   store is automatically created.
    #' @param ragnar_store A pre-built ragnar store. If NULL and `embed_fn` is
    #'   provided, a store is created automatically with the tempest metadata schema.
    #' @param search_provider Search provider: "native" (use provider's built-in web
    #'   search when available), "wikipedia", "you", "bing", "serper",
    #'   "brave", "duckduckgo", "tavily", "searxng", "google", or
    #'   "azure_ai_search". Default is "native" which uses OpenAI, Anthropic,
    #'   or Google's native web search capabilities when available, falling
    #'   back to "wikipedia".
    #' @param cache_dir Path to cache directory.
    #' @param max_search_results Maximum search results to return.
    #' @param max_search_queries_per_turn Maximum decomposed queries per
    #'   research turn.
    #' @param retrieve_top_k Maximum facts/chunks retrieved for each section.
    #' @param max_sources Maximum sources to keep.
    #' @param user_agent User agent string for HTTP requests.
    #' @param node_expansion_trigger_count Number of notes/sources per node that triggers expansion (NULL = disabled).
    #' @param enable_discourse_manager Whether to enable LLM-driven discourse management in Co-STORM.
    #' @param max_active_experts Maximum number of active expert agents in Co-STORM.
    #' @param enable_unseen_surfacing Whether to surface undiscussed sources in Co-STORM.
    initialize = function(
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
    ) {
      default_models <- list(
        coordinator = "openai/gpt-5-mini",
        writer = "openai/gpt-5-mini",
        expert = "openai/gpt-5-mini",
        mindmap = "openai/gpt-5-mini",
        judge = "openai/gpt-5-mini"
      )

      if (is.null(models)) {
        self$models <- default_models
      } else if (is.character(models) && length(models) == 1) {
        self$models <- lapply(default_models, function(x) models)
      } else {
        self$models <- models
      }

      self$params <- params %||% list()
      self$chat_fn <- chat_fn
      self$tools <- tools
      self$embed_fn <- embed_fn
      self$search_provider <- tempest_normalize_search_provider(search_provider)
      self$cache_dir <- tempest_cache_dir(cache_dir)
      self$max_search_results <- max_search_results
      self$max_search_queries_per_turn <- as.integer(
        max_search_queries_per_turn
      )
      self$retrieve_top_k <- as.integer(retrieve_top_k)
      if (
        is.na(self$max_search_queries_per_turn) ||
          self$max_search_queries_per_turn < 1
      ) {
        self$max_search_queries_per_turn <- 3L
      }
      if (is.na(self$retrieve_top_k) || self$retrieve_top_k < 1) {
        self$retrieve_top_k <- 25L
      }
      self$max_sources <- max_sources
      self$user_agent <- user_agent
      self$node_expansion_trigger_count <- node_expansion_trigger_count
      self$enable_discourse_manager <- enable_discourse_manager
      self$max_active_experts <- as.integer(max_active_experts)
      self$enable_unseen_surfacing <- enable_unseen_surfacing

      # Create or use ragnar store
      if (!is.null(ragnar_store)) {
        self$ragnar_store <- ragnar_store
      } else if (!is.null(embed_fn)) {
        self$ragnar_store <- tempest_create_ragnar_store(
          embed_fn,
          self$cache_dir
        )
      }

      invisible(self)
    },

    #' @description
    #' Create a chat object for a given role.
    #' @param role Role name (e.g., "coordinator", "writer", "expert").
    #' @param system_prompt Optional system prompt override.
    #' @param echo Echo setting for the chat.
    #' @return An ellmer-compatible Chat object.
    make_chat = function(role, system_prompt = NULL, echo = "none") {
      model <- self$models[[role]] %||%
        self$models[["coordinator"]] %||%
        "openai/gpt-5-mini"

      if (is.null(system_prompt)) {
        prompt_name <- paste0(role, "_system")
        system_prompt <- tryCatch(
          tempest_prompt(prompt_name),
          error = function(e) NULL
        )
        if (is.null(system_prompt)) {
          system_prompt <- "You are a helpful assistant."
        }
      }

      # Use custom chat function if provided
      if (!is.null(self$chat_fn)) {
        chat <- self$chat_fn(
          role = role,
          model = model,
          system_prompt = system_prompt,
          echo = echo
        )
      } else {
        tempest_require("ellmer", "LLM orchestration for STORM/Co-STORM.")
        chat <- ellmer::chat(
          name = model,
          system_prompt = system_prompt,
          params = self$params,
          echo = echo
        )
      }

      # Register additional tools if provided
      if (!is.null(self$tools)) {
        tools_to_register <- if (is.function(self$tools)) {
          self$tools()
        } else {
          self$tools
        }
        chat$register_tools(tools_to_register)
      }

      chat
    }
  )
)

#' Create a STORM configuration
#'
#' @param ... Passed to `TempestConfig$new()`.
#' @return A `TempestConfig` R6 object.
#' @examples
#' cfg <- tempest_config()
#' cfg <- tempest_config(search_provider = "wikipedia")
#' @export
tempest_config <- function(...) {
  TempestConfig$new(...)
}

#' @keywords internal
tempest_search_provider_choices <- function() {
  c(
    "native",
    "wikipedia",
    "you",
    "bing",
    "serper",
    "brave",
    "duckduckgo",
    "tavily",
    "searxng",
    "google",
    "azure_ai_search"
  )
}

#' @keywords internal
tempest_normalize_search_provider <- function(search_provider) {
  if (!rlang::is_string(search_provider)) {
    tempest_abort(
      "{.arg search_provider} must be a single string, not {.obj_type_friendly {search_provider}}."
    )
  }

  provider <- tolower(tempest_trim(search_provider))
  provider <- gsub("[.-]+", "_", provider)
  provider <- switch(
    provider,
    "you_com" = "you",
    "you_search" = "you",
    "bing_search" = "bing",
    "bingsearch" = "bing",
    "ddg" = "duckduckgo",
    "duck_duck_go" = "duckduckgo",
    "duckduckgosearch" = "duckduckgo",
    "searx" = "searxng",
    "sear_xng" = "searxng",
    "searx_ng" = "searxng",
    "google_search" = "google",
    "google_custom_search" = "google",
    "googlesearch" = "google",
    "azure" = "azure_ai_search",
    "azure_ai" = "azure_ai_search",
    "azure_search" = "azure_ai_search",
    "azureaisearch" = "azure_ai_search",
    provider
  )

  choices <- tempest_search_provider_choices()
  if (!provider %in% choices) {
    tempest_abort(c(
      "Unknown search provider: {.val {search_provider}}",
      i = "Available providers: {.val {choices}}"
    ))
  }

  provider
}

#' Create a ragnar store with tempest metadata schema
#'
#' Creates a ragnar store configured with metadata columns useful for
#' STORM/Co-STORM workflows: source tracking, citation linking, and
#' content organization.
#'
#' @param embed_fn Embedding function (e.g., `ragnar::embed_openai()`).
#' @param cache_dir Optional directory for persistent storage. If NULL,
#'   creates an in-memory store.
#' @param name Store name for tool registration.
#' @param title Human-readable store title.
#' @return A ragnar store object.
#'
#' @details
#' The store includes these metadata columns:
#' \describe{
#'   \item{source_id}{tempest source ID (Sxxxxxxxxxxxx) for citation linking}
#'   \item{url}{Original source URL}
#'   \item{title}{Document title}
#'   \item{fetched_at}{ISO timestamp when content was fetched}
#'   \item{content_type}{Content type (html, pdf, etc.)}
#'   \item{perspective}{STORM perspective this content relates to (optional)}
#' }
#'
#' @examples
#' \dontrun{
#' store <- tempest_create_ragnar_store(
#'   embed_fn = ragnar::embed_openai(),
#'   cache_dir = tempfile()
#' )
#' }
#' @export
tempest_create_ragnar_store <- function(
  embed_fn,
  cache_dir = NULL,
  name = "tempest_knowledge",
  title = "STORM Knowledge Base"
) {
  tempest_require("ragnar", "RAG capabilities require the ragnar package.")

  # Determine storage location
  location <- if (!is.null(cache_dir)) {
    fs::path(cache_dir, "tempest_ragnar.duckdb")
  } else {
    ":memory:"
  }

  # Define metadata schema for tempest
  extra_cols <- data.frame(
    source_id = character(),
    url = character(),
    title = character(),
    fetched_at = character(),
    content_type = character(),
    perspective = character(),
    stringsAsFactors = FALSE
  )

  ragnar::ragnar_store_create(
    location = location,
    embed = embed_fn,
    extra_cols = extra_cols,
    name = name,
    title = title,
    overwrite = TRUE
  )
}
