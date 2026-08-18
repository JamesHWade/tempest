# Configuration

tempest_artifact_store_abort <- function(message, ..., parent = NULL) {
  tempest_abort(
    message,
    ...,
    class = c("tempest_artifact_store_error", "tempest_error"),
    parent = parent,
    .envir = rlang::caller_env()
  )
}

tempest_artifact_store_call <- function(operation, callback) {
  tryCatch(
    callback(),
    error = function(error) {
      if (inherits(error, "tempest_artifact_store_error")) {
        stop(error)
      }
      tempest_artifact_store_abort(
        "Artifact store operation {.val {operation}} failed.",
        parent = error
      )
    }
  )
}

tempest_artifact_store_runtime_value <- function(value) {
  if (
    is.function(value) ||
      is.environment(value) ||
      typeof(value) %in% c("externalptr", "weakref") ||
      inherits(value, "S7_object")
  ) {
    return(TRUE)
  }
  is.list(value) &&
    any(vapply(value, tempest_artifact_store_runtime_value, logical(1)))
}

tempest_artifact_store_validate_listing <- function(value) {
  if (!is.list(value) || is.data.frame(value)) {
    tempest_artifact_store_abort(
      "Artifact store metadata listings must be a named list."
    )
  }
  ids <- names(value)
  if (
    length(value) > 0L &&
      (is.null(ids) ||
        anyNA(ids) ||
        any(!nzchar(ids)) ||
        anyDuplicated(ids))
  ) {
    tempest_artifact_store_abort(
      "Artifact store metadata listings require unique artifact-id names."
    )
  }
  for (artifact_id in ids %||% character()) {
    record <- value[[artifact_id]]
    if (
      !is.list(record) ||
        is.data.frame(record) ||
        !is.null(record$content) ||
        tempest_artifact_store_runtime_value(record)
    ) {
      tempest_artifact_store_abort(
        "Artifact metadata for {.val {artifact_id}} is not a durable content-free record."
      )
    }
    if (
      !is.null(record$artifact_id) &&
        !identical(record$artifact_id, artifact_id)
    ) {
      tempest_artifact_store_abort(
        "Artifact metadata key does not match its artifact id."
      )
    }
  }
  value
}

tempest_artifact_store_validate_read <- function(
  value,
  artifact_id,
  default
) {
  if (is.null(value) || identical(value, default)) {
    return(value)
  }
  if (!S7::S7_inherits(value, TempestArtifact)) {
    tempest_artifact_store_abort(
      "Artifact store reads must return a typed artifact or the supplied default."
    )
  }
  if (!identical(value@artifact_id, artifact_id)) {
    tempest_artifact_store_abort(
      "Artifact store read returned an artifact with a mismatched id."
    )
  }
  value
}

#' Create a Tempest artifact store adapter
#'
#' `r lifecycle::badge("experimental")`
#'
#' This experimental API is frozen and scheduled for removal in Tempest 0.2.0.
#' No compatibility shim is planned; see
#' [tempest-generic-kernel-retirement].
#'
#' Artifact stores let host applications observe or persist typed Tempest
#' outputs without replacing the live in-memory artifact catalog. The default
#' store is a no-op adapter.
#'
#' @param write Function with signature `function(artifact)` used to persist a
#'   typed artifact.
#' @param read Function with signature `function(artifact_id, default)` that
#'   returns a typed artifact.
#' @param list_metadata Function with no arguments that returns a named list of
#'   artifact metadata records without inline content.
#' @param exists Function with signature `function(artifact_id, version)` used
#'   to test artifact identity and optional deliverable version.
#' @param version Function with signature `function(artifact_id, default)` that
#'   returns the persisted deliverable version.
#' @return A typed artifact-store adapter.
#' @examples
#' store <- tempest_memory_artifact_store()
#' spec <- tempest_deliverable_spec(
#'   "report",
#'   title = "Report",
#'   purpose = "Explain the result",
#'   instructions = "Be concise.",
#'   generator_id = "tempest.generator.provided_content",
#'   renderer_ids = "tempest.renderer.markdown"
#' )
#' artifact <- tempest_artifact(spec, content = "# Report")
#' store$write(artifact)
#' store$read(artifact@artifact_id)
#' @export
tempest_artifact_store <- function(
  write = NULL,
  read = NULL,
  list_metadata = NULL,
  exists = NULL,
  version = NULL
) {
  write_impl <- write %||%
    function(artifact) {
      invisible(artifact@artifact_id)
    }
  read_impl <- read %||%
    function(artifact_id, default = NULL) {
      default
    }
  list_impl <- list_metadata %||%
    function() {
      list()
    }
  exists_impl <- exists %||%
    function(artifact_id, version = NULL) {
      FALSE
    }
  version_impl <- version %||%
    function(artifact_id, default = NULL) {
      artifact <- tempest_artifact_store_validate_read(
        read_impl(artifact_id, default = NULL),
        artifact_id,
        NULL
      )
      if (S7::S7_inherits(artifact, TempestArtifact)) {
        artifact@deliverable_version
      } else {
        default
      }
    }
  for (fn in list(
    write = write_impl,
    read = read_impl,
    list = list_impl,
    exists = exists_impl,
    version = version_impl
  )) {
    if (!is.function(fn)) {
      tempest_abort(
        c(
          "Artifact store entries must be functions.",
          i = "Use {.fn tempest_artifact_store} with function values for its adapter arguments."
        ),
        class = c(
          "tempest_artifact_store_error",
          "tempest_config_error",
          "tempest_error"
        )
      )
    }
  }
  write_fn <- function(artifact) {
    if (!S7::S7_inherits(artifact, TempestArtifact)) {
      tempest_artifact_store_abort(
        "{.arg artifact} must be created by {.fn tempest_artifact}."
      )
    }
    tempest_artifact_store_call("write", function() {
      write_impl(artifact)
    })
    invisible(artifact@artifact_id)
  }
  read_fn <- function(artifact_id, default = NULL) {
    artifact_id <- tempest_workflow_scalar(artifact_id, "artifact_id")
    value <- tempest_artifact_store_call("read", function() {
      read_impl(artifact_id, default = default)
    })
    tempest_artifact_store_validate_read(value, artifact_id, default)
  }
  list_fn <- function() {
    value <- tempest_artifact_store_call("list", list_impl)
    tempest_artifact_store_validate_listing(value)
  }
  exists_fn <- function(artifact_id, version = NULL) {
    artifact_id <- tempest_workflow_scalar(artifact_id, "artifact_id")
    if (!is.null(version)) {
      version <- tempest_workflow_version(version)
    }
    value <- tempest_artifact_store_call("exists", function() {
      exists_impl(artifact_id, version = version)
    })
    if (!is.logical(value) || length(value) != 1L || is.na(value)) {
      tempest_artifact_store_abort(
        "Artifact store exists checks must return `TRUE` or `FALSE`."
      )
    }
    value
  }
  version_fn <- function(artifact_id, default = NULL) {
    artifact_id <- tempest_workflow_scalar(artifact_id, "artifact_id")
    value <- tempest_artifact_store_call("version", function() {
      version_impl(artifact_id, default = default)
    })
    if (is.null(value) || identical(value, default)) {
      return(default)
    }
    tryCatch(
      tempest_workflow_version(value),
      error = function(error) {
        tempest_artifact_store_abort(
          "Artifact store versions must be stable non-empty version strings.",
          parent = error
        )
      }
    )
  }
  structure(
    list(
      write = write_fn,
      read = read_fn,
      list = list_fn,
      exists = exists_fn,
      version = version_fn
    ),
    class = "tempest_artifact_store"
  )
}

#' Create an in-memory Tempest artifact store
#'
#' `r lifecycle::badge("experimental")`
#'
#' This experimental API is frozen and scheduled for removal in Tempest 0.2.0.
#' No compatibility shim is planned; see
#' [tempest-generic-kernel-retirement].
#'
#' This is useful for tests and host apps that want to capture artifacts before
#' deciding where to persist them.
#'
#' @return A `tempest_artifact_store`.
#' @examples
#' store <- tempest_memory_artifact_store()
#' # Stores accept typed artifacts produced by a deliverable lifecycle.
#' store$list()
#' @export
tempest_memory_artifact_store <- function() {
  artifacts <- new.env(parent = emptyenv())
  tempest_artifact_store(
    write = function(artifact) {
      if (!S7::S7_inherits(artifact, TempestArtifact)) {
        tempest_abort(
          "{.arg artifact} must be created by {.fn tempest_artifact}.",
          class = c("tempest_artifact_store_error", "tempest_error")
        )
      }
      artifacts[[artifact@artifact_id]] <- artifact
      invisible(artifact@artifact_id)
    },
    read = function(artifact_id, default = NULL) {
      artifact <- artifacts[[artifact_id]]
      if (is.null(artifact)) default else artifact
    },
    list_metadata = function() {
      ids <- sort(ls(artifacts, all.names = TRUE))
      stats::setNames(
        lapply(
          ids,
          function(id) {
            tempest_artifact_data(
              artifacts[[id]],
              include_content = FALSE
            )
          }
        ),
        ids
      )
    },
    exists = function(artifact_id, version = NULL) {
      artifact <- artifacts[[artifact_id]]
      if (is.null(artifact)) {
        return(FALSE)
      }
      is.null(version) || identical(artifact@deliverable_version, version)
    },
    version = function(artifact_id, default = NULL) {
      artifact <- artifacts[[artifact_id]]
      if (is.null(artifact)) {
        default
      } else {
        artifact@deliverable_version
      }
    }
  )
}

#' @keywords internal
tempest_artifact_store_write <- function(store, artifact) {
  if (is.null(store)) {
    return(invisible(artifact@artifact_id))
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
  if (!S7::S7_inherits(artifact, TempestArtifact)) {
    tempest_abort(
      "{.arg artifact} must be created by {.fn tempest_artifact}.",
      class = c("tempest_artifact_store_error", "tempest_error")
    )
  }
  tryCatch(
    store$write(artifact),
    error = function(error) {
      tempest_abort(
        "Could not persist artifact {.val {artifact@artifact_id}}.",
        class = c("tempest_artifact_store_error", "tempest_error"),
        parent = error
      )
    }
  )
  invisible(artifact@artifact_id)
}

#' @keywords internal
tempest_artifact_store_read <- function(
  store,
  artifact_id,
  default = NULL
) {
  if (!inherits(store, "tempest_artifact_store")) {
    tempest_artifact_store_abort(
      "{.arg store} must be a Tempest artifact store."
    )
  }
  store$read(artifact_id, default = default)
}

#' @keywords internal
tempest_artifact_store_list <- function(store) {
  if (!inherits(store, "tempest_artifact_store")) {
    tempest_artifact_store_abort(
      "{.arg store} must be a Tempest artifact store."
    )
  }
  store$list()
}

#' @keywords internal
tempest_artifact_store_exists <- function(
  store,
  artifact_id,
  version = NULL
) {
  if (!inherits(store, "tempest_artifact_store")) {
    tempest_artifact_store_abort(
      "{.arg store} must be a Tempest artifact store."
    )
  }
  store$exists(artifact_id, version = version)
}

#' @keywords internal
tempest_artifact_store_version <- function(
  store,
  artifact_id,
  default = NULL
) {
  if (!inherits(store, "tempest_artifact_store")) {
    tempest_artifact_store_abort(
      "{.arg store} must be a Tempest artifact store."
    )
  }
  store$version(artifact_id, default = default)
}

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
  node_expansion_trigger_count = NULL,
  enable_discourse_manager = FALSE,
  max_active_experts = 5L,
  enable_unseen_surfacing = FALSE,
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
  if (!is.null(node_expansion_trigger_count)) {
    node_expansion_trigger_count <- tempest_config_count(
      node_expansion_trigger_count,
      "node_expansion_trigger_count"
    )
  }
  enable_discourse_manager <- tempest_config_flag(
    enable_discourse_manager,
    "enable_discourse_manager"
  )
  max_active_experts <- tempest_config_count(
    max_active_experts,
    "max_active_experts"
  )
  enable_unseen_surfacing <- tempest_config_flag(
    enable_unseen_surfacing,
    "enable_unseen_surfacing"
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
    node_expansion_trigger_count = node_expansion_trigger_count,
    enable_discourse_manager = enable_discourse_manager,
    max_active_experts = max_active_experts,
    enable_unseen_surfacing = enable_unseen_surfacing,
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
