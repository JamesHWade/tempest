# Configuration

#' @keywords internal
tempest_config_abort <- function(message, ..., parent = NULL) {
  tempest_abort(
    message,
    ...,
    class = c("tempest_config_error", "tempest_error"),
    parent = parent,
    .envir = rlang::caller_env()
  )
}

#' @keywords internal
tempest_config_count <- function(value, arg, allow_zero = FALSE) {
  if (
    !is.numeric(value) ||
      length(value) != 1L ||
      is.na(value) ||
      !is.finite(value) ||
      value != as.integer(value) ||
      value < if (allow_zero) 0L else 1L
  ) {
    qualifier <- if (allow_zero) "non-negative" else "positive"
    tempest_config_abort(
      "{.arg {arg}} must be a {qualifier} whole number."
    )
  }
  as.integer(value)
}

#' @keywords internal
tempest_config_flag <- function(value, arg) {
  if (!is.logical(value) || length(value) != 1L || is.na(value)) {
    tempest_config_abort("{.arg {arg}} must be `TRUE` or `FALSE`.")
  }
  value
}

#' @keywords internal
tempest_config_string <- function(value, arg) {
  if (
    !is.character(value) ||
      length(value) != 1L ||
      is.na(value) ||
      !nzchar(value)
  ) {
    tempest_config_abort("{.arg {arg}} must be a non-empty string.")
  }
  value
}

#' @keywords internal
tempest_config_models <- function(models) {
  valid <- is.list(models) &&
    length(models) > 0L &&
    !is.null(names(models)) &&
    all(nzchar(names(models))) &&
    all(vapply(
      models,
      function(model) {
        is.character(model) &&
          length(model) == 1L &&
          !is.na(model) &&
          nzchar(model)
      },
      logical(1)
    ))
  if (!valid) {
    tempest_config_abort(
      "{.arg models} must be a named list of non-empty model strings."
    )
  }
  canonical <- vapply(
    models,
    function(model) {
      if (
        !identical(model, trimws(model)) ||
          grepl("\\s", model, perl = TRUE)
      ) {
        return(FALSE)
      }
      if (!grepl("/", model, fixed = TRUE)) {
        return(TRUE)
      }
      provider <- sub("/.*$", "", model)
      model_name <- sub("^[^/]*/", "", model)
      grepl("^[a-z][a-z0-9_.-]*$", provider) &&
        nzchar(model_name) &&
        !provider %in% c("claude", "gemini")
    },
    logical(1)
  )
  if (!all(canonical)) {
    tempest_config_abort(
      paste0(
        "{.arg models} must use canonical provider prefixes and exact model ",
        "identifiers; use ",
        "{.val anthropic/...} or {.val google/...}."
      )
    )
  }
  models
}

#' @keywords internal
tempest_config_chat_option <- function(chat) {
  if (is.null(chat)) {
    return(list(chat = NULL, model = NULL))
  }
  if (
    is.character(chat) &&
      length(chat) == 1L &&
      !is.na(chat) &&
      nzchar(chat)
  ) {
    return(list(chat = NULL, model = chat))
  }
  if (!inherits(chat, "Chat")) {
    tempest_config_abort(c(
      "The {.option tempest.chat} option must be an ellmer Chat or a provider/model string.",
      i = paste0(
        "For example, use ",
        "{.code options(tempest.chat = ",
        "\"anthropic/claude-sonnet-4-20250514\")}."
      )
    ))
  }

  model <- tryCatch(
    chat$get_model(),
    error = function(error) {
      tempest_config_abort(
        "Failed to read the model from the {.option tempest.chat} Chat.",
        parent = error
      )
    }
  )
  if (
    !is.character(model) ||
      length(model) != 1L ||
      is.na(model) ||
      !nzchar(model)
  ) {
    tempest_config_abort(
      "The {.option tempest.chat} Chat must return a non-empty model string."
    )
  }

  list(chat = chat, model = model)
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
    chat = S7::new_property(S7::class_any, default = NULL),
    chat_fn = S7::new_property(S7::class_function | NULL, default = NULL),
    embed_fn = S7::new_property(S7::class_function | NULL, default = NULL),
    ragnar_store = S7::new_property(S7::class_any, default = NULL),
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
    max_active_experts = S7::new_property(S7::class_integer, default = 5L),
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
#' @param params Additional parameters passed to chat creation. Explicit values
#'   override the role defaults used by built-in ChatGPT-subscription clients.
#' @param chat_fn Custom chat factory function. Should accept `role`, `model`,
#'   `system_prompt`, and `echo` arguments and return an ellmer-compatible Chat
#'   object. Use this for custom providers like `chat_company()`.
#' @param embed_fn Embedding function for RAG. Should accept a character vector
#'   and return a matrix of embeddings. Use `ragnar::embed_openai()`,
#'   `ragnar::embed_ollama()`, or a custom function. If provided, a ragnar store
#'   is automatically created.
#' @param ragnar_store A pre-built ragnar store. If NULL and `embed_fn` is
#'   provided, a store is created automatically with the tempest metadata schema.
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
#' @param max_active_experts Maximum number of active expert agents in Co-STORM.
#' @param citation_policy Report citation-rendering and unsupported-claim
#'   handling policy: one of "none", "source_attributed", "claim_verified", or
#'   "strict". Product publication always runs exact claim verification.
#' @param min_support_score Minimum support score in `[0, 1]` for a claim to be
#'   considered supported.
#' @param on_unsupported_claim Action for unsupported claims: one of "flag",
#'   "drop", "revise", or "keep_with_warning". `drop` removes the complete
#'   unsupported assertion; `revise` replaces it with a revision notice.
#' @section Default chat:
#' When both `models` and `chat_fn` are `NULL`, `tempest_config()` consults the
#' `tempest.chat` R option. Set it to a provider/model string accepted by
#' [ellmer::chat()] or an ellmer `Chat` object. String values are used for every
#' role. Chat objects are cloned for every role, retain their provider settings
#' and system instructions, and receive the appropriate Tempest role prompt.
#' When the option is unset, Tempest creates its built-in OpenAI clients with
#' [ellmer::chat_openai()] and `auth = "codex"`, which uses file-backed ChatGPT
#' subscription authentication managed by Codex CLI. Explicit `models` and
#' `chat_fn` arguments take precedence over the option.
#' @return A `TempestConfig` S7 object.
#' @examples
#' cfg <- tempest_config()
#' cfg <- tempest_config(search_provider = "wikipedia")
#' @export
tempest_config <- function(
  models = NULL,
  params = NULL,
  chat_fn = NULL,
  embed_fn = NULL,
  ragnar_store = NULL,
  search_provider = "native",
  cache_dir = NULL,
  cache_enabled = TRUE,
  cache_ttl = Inf,
  max_search_results = 8,
  max_search_queries_per_turn = 3,
  retrieve_top_k = 25,
  max_sources = 24,
  user_agent = "tempest (R; +https://github.com/JamesHWade/tempest)",
  max_active_experts = 5L,
  citation_policy = "source_attributed",
  min_support_score = 0.7,
  on_unsupported_claim = "flag"
) {
  default_models <- list(
    coordinator = "openai/gpt-5.6-sol",
    writer = "openai/gpt-5.6-sol",
    expert = "openai/gpt-5.6-luna",
    mindmap = "openai/gpt-5.6-luna",
    judge = "openai/gpt-5.6-luna"
  )
  configured_chat <- if (is.null(models) && is.null(chat_fn)) {
    tempest_config_chat_option(getOption("tempest.chat"))
  } else {
    list(chat = NULL, model = NULL)
  }
  models <- if (!is.null(configured_chat$model)) {
    lapply(default_models, \(model) configured_chat$model)
  } else if (is.null(models)) {
    default_models
  } else if (is.character(models) && length(models) == 1) {
    lapply(default_models, function(x) models)
  } else {
    models
  }
  models <- tempest_config_models(models)
  if (!is.list(params %||% list())) {
    tempest_config_abort("{.arg params} must be a list.")
  }
  for (arg in c("chat_fn", "embed_fn")) {
    value <- get(arg)
    if (!is.null(value) && !is.function(value)) {
      tempest_config_abort("{.arg {arg}} must be a function or `NULL`.")
    }
  }
  cache_enabled <- tempest_config_flag(cache_enabled, "cache_enabled")
  max_search_results <- tempest_config_count(
    max_search_results,
    "max_search_results"
  )
  mq <- tempest_config_count(
    max_search_queries_per_turn,
    "max_search_queries_per_turn"
  )
  rk <- tempest_config_count(retrieve_top_k, "retrieve_top_k")
  max_sources <- tempest_config_count(max_sources, "max_sources")
  user_agent <- tempest_config_string(user_agent, "user_agent")
  max_active_experts <- tempest_config_count(
    max_active_experts,
    "max_active_experts"
  )
  if (
    !is.character(citation_policy) ||
      length(citation_policy) != 1L ||
      is.na(citation_policy) ||
      !citation_policy %in%
        c(
          "none",
          "source_attributed",
          "claim_verified",
          "strict"
        )
  ) {
    tempest_config_abort(
      "Invalid {.arg citation_policy}: {.val {citation_policy}}."
    )
  }
  if (
    !is.character(on_unsupported_claim) ||
      length(on_unsupported_claim) != 1L ||
      is.na(on_unsupported_claim) ||
      !on_unsupported_claim %in%
        c(
          "flag",
          "drop",
          "revise",
          "keep_with_warning"
        )
  ) {
    tempest_config_abort(
      "Invalid {.arg on_unsupported_claim}: {.val {on_unsupported_claim}}."
    )
  }
  min_support_score <- tempest_normalize_min_support_score(min_support_score)

  ragnar_cache_dir <- cache_dir
  cache_dir <- tempest_cache_dir(cache_dir)
  cache_ttl <- tempest_cache_max_age(cache_ttl)
  if (is.null(ragnar_store) && !is.null(embed_fn)) {
    ragnar_store <- tempest_create_ragnar_store(embed_fn, ragnar_cache_dir)
  }
  TempestConfig(
    models = models,
    params = params %||% list(),
    chat = configured_chat$chat,
    chat_fn = chat_fn,
    embed_fn = embed_fn,
    ragnar_store = ragnar_store,
    search_provider = tempest_normalize_search_provider(search_provider),
    cache_dir = cache_dir %||% NA_character_,
    cache_enabled = cache_enabled,
    cache_ttl = cache_ttl,
    max_search_results = max_search_results,
    max_search_queries_per_turn = mq,
    retrieve_top_k = rk,
    max_sources = max_sources,
    user_agent = user_agent,
    max_active_experts = max_active_experts,
    citation_policy = citation_policy,
    min_support_score = min_support_score,
    on_unsupported_claim = on_unsupported_claim
  )
}

# Call ellmer through a package-local seam so the provider boundary can be
# tested without credentials or network access.
tempest_chat_openai <- function(...) {
  if (!"auth" %in% names(formals(ellmer::chat_openai))) {
    tempest_abort(
      c(
        "The installed ellmer does not support ChatGPT subscription authentication.",
        i = "Reinstall Tempest dependencies to obtain tidyverse/ellmer PR #1067."
      ),
      class = c(
        "tempest_chat_error",
        "tempest_config_error",
        "tempest_error"
      )
    )
  }
  ellmer::chat_openai(...)
}

tempest_subscription_chat_params <- function(role, params) {
  defaults <- switch(
    role,
    mindmap = list(reasoning_effort = "low"),
    judge = list(reasoning_effort = "low"),
    list()
  )
  utils::modifyList(defaults, params)
}

tempest_default_chat <- function(model, system_prompt, params, echo, role) {
  tempest_require("ellmer", "LLM orchestration for STORM/Co-STORM.")
  if (startsWith(model, "openai/")) {
    return(tempest_chat_openai(
      system_prompt = system_prompt,
      model = sub("^openai/", "", model),
      params = tempest_subscription_chat_params(role, params),
      echo = echo,
      auth = "codex"
    ))
  }

  ellmer::chat(
    name = model,
    system_prompt = system_prompt,
    params = params,
    echo = echo
  )
}

#' Create a chat object for a given role
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
    "openai/gpt-5.6-sol"
  if (is.null(system_prompt)) {
    prompt_name <- paste0(role, "_system")
    system_prompt <- tryCatch(tempest_prompt(prompt_name), error = function(e) {
      NULL
    }) %||%
      "You are a helpful assistant."
  }
  chat <- tryCatch(
    if (!is.null(config@chat_fn)) {
      config@chat_fn(
        role = role,
        model = model,
        system_prompt = system_prompt,
        echo = echo
      )
    } else if (!is.null(config@chat)) {
      client_prompt <- config@chat$get_system_prompt()
      chat <- config@chat$clone(deep = TRUE)
      combined_prompt <- c(
        system_prompt,
        if (!is.null(client_prompt)) c("---", client_prompt)
      ) |>
        paste(collapse = "\n\n")
      chat$set_system_prompt(combined_prompt)
      chat
    } else {
      tempest_default_chat(
        model = model,
        system_prompt = system_prompt,
        params = config@params,
        echo = echo,
        role = role
      )
    },
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

  provider <- search_provider
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
#' @param reset Whether to replace an existing persistent store. Destructive
#'   replacement is never performed unless this is `TRUE`.
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
#' @keywords internal
tempest_create_ragnar_store <- function(
  embed_fn,
  cache_dir = NULL,
  name = "tempest_knowledge",
  title = "STORM Knowledge Base",
  reset = FALSE
) {
  tempest_require("ragnar", "RAG capabilities require the ragnar package.")
  if (!is.function(embed_fn)) {
    tempest_config_abort("{.arg embed_fn} must be a function.")
  }
  reset <- tempest_config_flag(reset, "reset")

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

  if (!identical(location, ":memory:") && file.exists(location) && !reset) {
    store <- tryCatch(
      tempest_ragnar_store_connect(location),
      error = function(error) {
        tempest_config_abort(
          c(
            "Existing Ragnar store is not compatible.",
            i = "Use {.code reset = TRUE} only when replacement is intended."
          )
        )
      }
    )
    required <- names(extra_cols)
    store_fields <- unique(c(
      tryCatch(
        DBI::dbListFields(store@con, "embeddings"),
        error = function(error) character()
      ),
      tryCatch(DBI::dbListFields(store@con, "chunks"), error = function(error) {
        character()
      })
    ))
    missing <- setdiff(required, store_fields)
    embedding_size <- tryCatch(
      ncol(embed_fn("tempest schema check")),
      error = function(error) NA_integer_
    )
    stored_size <- tryCatch(
      DBI::dbGetQuery(
        store@con,
        "SELECT embedding_size FROM metadata"
      )$embedding_size[[1]],
      error = function(error) NA_integer_
    )
    if (
      length(missing) > 0L ||
        is.na(embedding_size) ||
        !identical(as.integer(embedding_size), as.integer(stored_size))
    ) {
      try(DBI::dbDisconnect(store@con, shutdown = TRUE), silent = TRUE)
      tempest_config_abort(
        c(
          "Existing Ragnar store schema is incompatible.",
          i = "Use a compatible embedding function or opt in to {.code reset = TRUE}."
        )
      )
    }
    return(store)
  }

  tryCatch(
    tempest_ragnar_store_create(
      location = location,
      embed = embed_fn,
      extra_cols = extra_cols,
      name = name,
      title = title,
      overwrite = reset
    ),
    error = function(error) {
      tempest_rethrow_operation(error, class = "tempest_config_error")
    }
  )
}

tempest_ragnar_store_connect <- function(location) {
  ragnar::ragnar_store_connect(location, read_only = FALSE)
}

tempest_ragnar_store_create <- function(...) {
  ragnar::ragnar_store_create(...)
}
