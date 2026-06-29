# Configuration

#' Create a Tempest artifact store adapter
#'
#' `r lifecycle::badge("experimental")`
#'
#' Artifact stores let host applications observe or persist Tempest outputs
#' without replacing the live in-memory `SourceStore`. The default store is a
#' no-op adapter.
#'
#' @param write Function with signature `function(name, value, metadata)` used
#'   to persist an artifact.
#' @param read Function with signature `function(name, default)` used to read an
#'   artifact.
#' @param list_names Function with no arguments that returns available artifact
#'   names.
#' @return A list with `write`, `read`, and `list` functions.
#' @examples
#' store <- tempest_memory_artifact_store()
#' store$write("report_md", "# Report", metadata = list(topic = "Demo"))
#' store$read("report_md")
#' @export
tempest_artifact_store <- function(
  write = NULL,
  read = NULL,
  list_names = NULL
) {
  write <- write %||%
    function(name, value, metadata = list()) {
      invisible(name)
    }
  read <- read %||%
    function(name, default = NULL) {
      default
    }
  list_fn <- list_names %||%
    function() {
      character()
    }
  for (fn in list(write = write, read = read, list = list_fn)) {
    if (!is.function(fn)) {
      tempest_abort(
        c(
          "Artifact store entries must be functions.",
          i = "Use {.fn tempest_artifact_store} with function values for {.arg write}, {.arg read}, and {.arg list_names}."
        ),
        class = c(
          "tempest_artifact_store_error",
          "tempest_config_error",
          "tempest_error"
        )
      )
    }
  }
  structure(
    list(write = write, read = read, list = list_fn),
    class = "tempest_artifact_store"
  )
}

#' Create an in-memory Tempest artifact store
#'
#' `r lifecycle::badge("experimental")`
#'
#' This is useful for tests and host apps that want to capture artifacts before
#' deciding where to persist them.
#'
#' @return A `tempest_artifact_store`.
#' @examples
#' store <- tempest_memory_artifact_store()
#' store$write("summary", "Done")
#' store$list()
#' @export
tempest_memory_artifact_store <- function() {
  artifacts <- new.env(parent = emptyenv())
  tempest_artifact_store(
    write = function(name, value, metadata = list()) {
      artifacts[[name]] <- list(value = value, metadata = metadata)
      invisible(name)
    },
    read = function(name, default = NULL) {
      item <- artifacts[[name]]
      if (is.null(item)) default else item$value
    },
    list_names = function() {
      sort(ls(artifacts, all.names = TRUE))
    }
  )
}

#' @keywords internal
tempest_artifact_write <- function(store, name, value, metadata = list()) {
  if (is.null(store)) {
    return(invisible(name))
  }
  if (!inherits(store, "tempest_artifact_store")) {
    tempest_abort(
      c(
        "{.arg artifact_store} must be created by {.fn tempest_artifact_store}.",
        i = "Use {.fn tempest_memory_artifact_store} for a simple in-memory adapter."
      ),
      class = c(
        "tempest_artifact_store_error",
        "tempest_config_error",
        "tempest_error"
      )
    )
  }
  store$write(name, value, metadata)
  invisible(name)
}

#' TempestConfig (S7)
#'
#' Holds configuration for STORM / Co-STORM sessions: LLM models, prompts,
#' retrieval settings, cache paths, and citation policy.
#'
#' @include ledger-types.R
#' @keywords internal
TempestConfig <- S7::new_class(
  "TempestConfig",
  properties = list(
    models = S7::new_property(S7::class_list, default = list()),
    params = S7::new_property(S7::class_list, default = list()),
    chat_fn = S7::new_property(S7::class_function | NULL, default = NULL),
    tools = S7::new_property(S7::class_any, default = NULL),
    embed_fn = S7::new_property(S7::class_function | NULL, default = NULL),
    ragnar_store = S7::new_property(S7::class_any, default = NULL),
    artifact_store = S7::new_property(S7::class_any, default = NULL),
    search_provider = prop_chr("native"),
    cache_dir = prop_chr(),
    cache_enabled = S7::new_property(S7::class_logical, default = TRUE),
    cache_ttl = S7::new_property(S7::class_numeric, default = Inf),
    max_search_results = S7::new_property(S7::class_numeric, default = 8),
    max_search_queries_per_turn = S7::new_property(
      S7::class_integer,
      default = 3L
    ),
    retrieve_top_k = S7::new_property(S7::class_integer, default = 25L),
    max_sources = S7::new_property(S7::class_numeric, default = 24),
    user_agent = prop_chr(
      "tempest (R; +https://github.com/JamesHWade/tempest)"
    ),
    node_expansion_trigger_count = S7::new_property(
      S7::class_any,
      default = NULL
    ),
    enable_discourse_manager = S7::new_property(
      S7::class_logical,
      default = FALSE
    ),
    max_active_experts = S7::new_property(S7::class_integer, default = 5L),
    enable_unseen_surfacing = S7::new_property(
      S7::class_logical,
      default = FALSE
    ),
    citation_policy = prop_enum(
      c("none", "source_attributed", "claim_verified", "strict"),
      "source_attributed"
    ),
    min_support_score = prop_score_default(0.7),
    on_unsupported_claim = prop_enum(
      c("flag", "drop", "revise", "keep_with_warning"),
      "flag"
    )
  )
)

#' Create a STORM configuration
#'
#' @param models Named list of model identifiers for each role, or a single
#'   string to use for all roles.
#' @param params Additional parameters passed to chat creation.
#' @param chat_fn Custom chat factory function. Should accept `role`, `model`,
#'   `system_prompt`, and `echo` arguments and return an ellmer-compatible Chat
#'   object. Use this for custom providers like `chat_company()`.
#' @param tools Additional tools to register with each chat. Can be a list of
#'   ellmer tools, or a function that returns tools (e.g., `btw::btw_tools`).
#' @param embed_fn Embedding function for RAG. Should accept a character vector
#'   and return a matrix of embeddings. Use `ragnar::embed_openai()`,
#'   `ragnar::embed_ollama()`, or a custom function. If provided, a ragnar store
#'   is automatically created.
#' @param ragnar_store A pre-built ragnar store. If NULL and `embed_fn` is
#'   provided, a store is created automatically with the tempest metadata schema.
#' @param artifact_store Optional artifact-store adapter from
#'   `tempest_artifact_store()` or `tempest_memory_artifact_store()`.
#' @param search_provider Search provider: "native" (use provider's built-in web
#'   search when available), "wikipedia", "you", "bing", "serper", "brave",
#'   "duckduckgo", "tavily", "searxng", "google", or "azure_ai_search". Default
#'   is "native" which uses OpenAI, Anthropic, or Google's native web search
#'   capabilities when available, falling back to "wikipedia".
#' @param cache_dir Path to cache directory.
#' @param cache_enabled Whether retriever search/fetch calls should read from
#'   and write to the on-disk cache.
#' @param cache_ttl Maximum cache age in seconds. Defaults to `Inf`, which keeps
#'   cached search/fetch entries valid until explicitly cleared.
#' @param max_search_results Maximum search results to return.
#' @param max_search_queries_per_turn Maximum decomposed queries per research
#'   turn.
#' @param retrieve_top_k Maximum facts/chunks retrieved for each section.
#' @param max_sources Maximum sources to keep.
#' @param user_agent User agent string for HTTP requests.
#' @param node_expansion_trigger_count Number of notes/sources per node that
#'   triggers expansion (NULL = disabled).
#' @param enable_discourse_manager Whether to enable LLM-driven discourse
#'   management in Co-STORM.
#' @param max_active_experts Maximum number of active expert agents in Co-STORM.
#' @param enable_unseen_surfacing Whether to surface undiscussed sources in
#'   Co-STORM.
#' @param citation_policy Citation enforcement policy: one of "none",
#'   "source_attributed", "claim_verified", or "strict".
#' @param min_support_score Minimum support score in `[0, 1]` for a claim to be
#'   considered supported.
#' @param on_unsupported_claim Action for unsupported claims: one of "flag",
#'   "drop", "revise", or "keep_with_warning".
#' @return A `TempestConfig` S7 object.
#' @examples
#' cfg <- tempest_config()
#' cfg <- tempest_config(search_provider = "wikipedia")
#' @export
tempest_config <- function(
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
) {
  default_models <- list(
    coordinator = "openai/gpt-5.4",
    writer = "openai/gpt-5.4",
    expert = "openai/gpt-5.4-mini",
    mindmap = "openai/gpt-5.4-mini",
    judge = "openai/gpt-5.4-mini"
  )
  models <- if (is.null(models)) {
    default_models
  } else if (is.character(models) && length(models) == 1) {
    lapply(default_models, function(x) models)
  } else {
    models
  }

  mq <- as.integer(max_search_queries_per_turn)
  if (is.na(mq) || mq < 1) {
    mq <- 3L
  }
  rk <- as.integer(retrieve_top_k)
  if (is.na(rk) || rk < 1) {
    rk <- 25L
  }

  cache_dir <- tempest_cache_dir(cache_dir)
  cache_ttl <- tempest_cache_max_age(cache_ttl)
  if (is.null(ragnar_store) && !is.null(embed_fn)) {
    ragnar_store <- tempest_create_ragnar_store(embed_fn, cache_dir)
  }
  if (
    !is.null(artifact_store) &&
      !inherits(artifact_store, "tempest_artifact_store")
  ) {
    tempest_abort(
      c(
        "{.arg artifact_store} must be created by {.fn tempest_artifact_store}.",
        i = "Use {.fn tempest_memory_artifact_store} for a simple in-memory adapter."
      ),
      class = c(
        "tempest_artifact_store_error",
        "tempest_config_error",
        "tempest_error"
      )
    )
  }

  TempestConfig(
    models = models,
    params = params %||% list(),
    chat_fn = chat_fn,
    tools = tools,
    embed_fn = embed_fn,
    ragnar_store = ragnar_store,
    artifact_store = artifact_store,
    search_provider = tempest_normalize_search_provider(search_provider),
    cache_dir = cache_dir %||% NA_character_,
    cache_enabled = isTRUE(cache_enabled),
    cache_ttl = cache_ttl,
    max_search_results = max_search_results,
    max_search_queries_per_turn = mq,
    retrieve_top_k = rk,
    max_sources = max_sources,
    user_agent = user_agent,
    node_expansion_trigger_count = node_expansion_trigger_count,
    enable_discourse_manager = enable_discourse_manager,
    max_active_experts = as.integer(max_active_experts),
    enable_unseen_surfacing = enable_unseen_surfacing,
    citation_policy = citation_policy,
    min_support_score = min_support_score,
    on_unsupported_claim = on_unsupported_claim
  )
}

#' Create a chat object for a given role.
#'
#' @param config A `TempestConfig` object.
#' @param role Role name (e.g., "coordinator", "writer", "expert").
#' @param system_prompt Optional system prompt override.
#' @param echo Echo setting for the chat.
#' @return An ellmer-compatible Chat object.
#' @keywords internal
tempest_make_chat <- function(
  config,
  role,
  system_prompt = NULL,
  echo = "none"
) {
  model <- config@models[[role]] %||%
    config@models[["coordinator"]] %||%
    "openai/gpt-5.4"
  if (is.null(system_prompt)) {
    prompt_name <- paste0(role, "_system")
    system_prompt <- tryCatch(tempest_prompt(prompt_name), error = function(e) {
      NULL
    }) %||%
      "You are a helpful assistant."
  }
  if (!is.null(config@chat_fn)) {
    chat <- tryCatch(
      config@chat_fn(
        role = role,
        model = model,
        system_prompt = system_prompt,
        echo = echo
      ),
      error = function(e) {
        tempest_abort(
          c(
            "Failed to create a Tempest chat client.",
            i = "Role: {.val {role}}.",
            i = "Model: {.val {model}}."
          ),
          class = c(
            "tempest_chat_error",
            "tempest_config_error",
            "tempest_error"
          ),
          parent = e,
          role = role,
          model = model
        )
      }
    )
  } else {
    tempest_require("ellmer", "LLM orchestration for STORM/Co-STORM.")
    chat <- tryCatch(
      ellmer::chat(
        name = model,
        system_prompt = system_prompt,
        params = config@params,
        echo = echo
      ),
      error = function(e) {
        tempest_abort(
          c(
            "Failed to create a Tempest chat client.",
            i = "Role: {.val {role}}.",
            i = "Model: {.val {model}}."
          ),
          class = c(
            "tempest_chat_error",
            "tempest_config_error",
            "tempest_error"
          ),
          parent = e,
          role = role,
          model = model
        )
      }
    )
  }
  if (!is.null(config@tools)) {
    tryCatch(
      {
        tools_to_register <- if (is.function(config@tools)) {
          config@tools()
        } else {
          config@tools
        }
        chat$register_tools(tools_to_register)
      },
      error = function(e) {
        tempest_abort(
          c(
            "Failed to register tools on a Tempest chat client.",
            i = "Role: {.val {role}}."
          ),
          class = c(
            "tempest_chat_error",
            "tempest_config_error",
            "tempest_error"
          ),
          parent = e,
          role = role,
          model = model
        )
      }
    )
  }
  chat
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
      "{.arg search_provider} must be a single string, not {.obj_type_friendly {search_provider}}.",
      class = c(
        "tempest_search_provider_error",
        "tempest_config_error",
        "tempest_error"
      ),
      search_provider = search_provider
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
    tempest_abort(
      c(
        "Unknown search provider: {.val {search_provider}}",
        i = "Available providers: {.val {choices}}"
      ),
      class = c(
        "tempest_search_provider_error",
        "tempest_config_error",
        "tempest_error"
      ),
      search_provider = search_provider,
      choices = choices
    )
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
