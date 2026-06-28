# Tools for ellmer tool calling

#' Detect provider from model name
#'
#' Parses model names like "openai/gpt-5.4-mini" or
#' "anthropic/claude-sonnet"
#' to extract the provider.
#'
#' @param model Model name string
#' @return Provider name (e.g., "openai", "anthropic", "google") or NULL if unknown
#' @keywords internal
tempest_detect_provider <- function(model) {
  if (is.null(model) || !is.character(model) || length(model) == 0) {
    return(NULL)
  }
  # Parse "provider/model-name" format
  if (grepl("/", model)) {
    provider <- sub("/.*$", "", model)
    # Normalize provider names
    provider <- tolower(provider)
    # Map common aliases
    provider <- switch(
      provider,
      "claude" = "anthropic",
      "gemini" = "google",
      provider
    )
    return(provider)
  }
  NULL
}

#' Get provider-native web tools
#'
#' Returns the appropriate web search/fetch tools for a given provider.
#' These are ellmer's built-in tools that use provider-native APIs.
#'
#' @param provider Provider name ("openai", "anthropic", "google")
#' @return List of ellmer tools, or NULL if provider doesn
#' @keywords internal
tempest_get_native_web_tools <- function(provider) {
  tempest_require("ellmer", "Provider-native web tools require ellmer.")

  tools <- switch(
    provider,
    "openai" = list(
      ellmer::openai_tool_web_search()
    ),
    "anthropic" = list(
      ellmer::claude_tool_web_search()
    ),
    "google" = list(
      ellmer::google_tool_web_search(),
      ellmer::google_tool_web_fetch()
    ),
    NULL
  )

  tools
}

#' Check if provider has native web search
#'
#' @param provider Provider name
#' @return TRUE if provider supports native web search
#' @keywords internal
tempest_provider_has_native_search <- function(provider) {
  provider %in% c("openai", "anthropic", "google")
}

#' @keywords internal
tempest_native_scalar <- function(...) {
  values <- list(...)
  for (value in values) {
    if (is.null(value) || length(value) == 0) {
      next
    }
    value <- as.character(value[[1]])
    if (!is.na(value) && nzchar(value)) {
      return(value)
    }
  }
  NA_character_
}

#' @keywords internal
tempest_native_source_candidates <- function(x) {
  candidates <- list()

  visit <- function(value) {
    if (is.data.frame(value)) {
      rows <- split(value, seq_len(nrow(value)))
      for (row in rows) {
        visit(as.list(row))
      }
      return(invisible(NULL))
    }
    if (!is.list(value)) {
      return(invisible(NULL))
    }

    url <- value$url %||% value$link %||% value$uri
    if (is.character(url) && length(url) > 0 && !is.na(url[[1]])) {
      candidates[[length(candidates) + 1L]] <<- list(
        url = url[[1]],
        title = tempest_native_scalar(value$title, value$name),
        snippet = tempest_native_scalar(
          value$snippet,
          value$description,
          value$summary
        ),
        content_text = tempest_native_scalar(value$content, value$text)
      )
    }

    for (child in value) {
      visit(child)
    }
    invisible(NULL)
  }

  visit(x)
  candidates
}

#' @keywords internal
tempest_native_source_from_url <- function(
  url,
  title = NA_character_,
  snippet = NA_character_,
  content_text = NA_character_,
  kind = "native_search"
) {
  url <- tryCatch(tempest_normalize_url(url), error = function(e) NA_character_)
  if (is.na(url) || !nzchar(url)) {
    return(NULL)
  }
  tempest_source(
    url = url,
    title = title,
    snippet = snippet,
    content_text = content_text,
    fetched_at = tempest_now_utc(),
    meta = list(kind = kind, provider_tool = "native")
  )
}

#' @keywords internal
tempest_merge_source_record <- function(old, new) {
  if (is.null(old)) {
    return(new)
  }
  for (field in c("title", "snippet", "content_text", "fetched_at")) {
    value <- new[[field]]
    if (length(value) != 1L || is.na(value) || !nzchar(value)) {
      new[[field]] <- old[[field]] %||% new[[field]]
    }
  }
  new$meta <- utils::modifyList(old$meta %||% list(), new$meta %||% list())
  new
}

#' @keywords internal
tempest_upsert_native_source <- function(store, source) {
  if (is.null(source)) {
    return(NA_character_)
  }
  source <- tempest_merge_source_record(store$get_source(source$id), source)
  store$upsert_source(source)
  source$id
}

#' @keywords internal
tempest_harvest_native_source_candidates <- function(
  x,
  store,
  kind = "native_search"
) {
  ids <- character()
  for (candidate in tempest_native_source_candidates(x)) {
    ids <- c(
      ids,
      tempest_upsert_native_source(
        store,
        tempest_native_source_from_url(
          candidate$url,
          title = candidate$title,
          snippet = candidate$snippet,
          content_text = candidate$content_text,
          kind = kind
        )
      )
    )
  }
  unique(ids[!is.na(ids) & nzchar(ids)])
}

#' @keywords internal
tempest_harvest_native_sources_from_content <- function(content, store) {
  ids <- character()

  if (inherits(content, "ellmer::ContentToolResponseSearch")) {
    ids <- c(
      ids,
      tempest_harvest_native_source_candidates(
        content@json,
        store,
        kind = "native_search"
      )
    )
    for (url in content@urls %||% character()) {
      ids <- c(
        ids,
        tempest_upsert_native_source(
          store,
          tempest_native_source_from_url(url, kind = "native_search")
        )
      )
    }
  } else if (inherits(content, "ellmer::ContentToolResponseFetch")) {
    json <- content@json %||% list()
    ids <- c(
      ids,
      tempest_upsert_native_source(
        store,
        tempest_native_source_from_url(
          content@url,
          title = tempest_native_scalar(json$title, json$name),
          snippet = tempest_native_scalar(
            json$snippet,
            json$description,
            json$summary
          ),
          content_text = tempest_native_scalar(json$content, json$text),
          kind = "native_fetch"
        )
      )
    )
  }

  if (S7::S7_inherits(content)) {
    for (property in S7::prop_names(content)) {
      value <- S7::prop(content, property)
      if (is.list(value)) {
        for (child in value) {
          ids <- c(
            ids,
            tempest_harvest_native_sources_from_content(
              child,
              store
            )
          )
        }
      }
    }
  }

  unique(ids[!is.na(ids) & nzchar(ids)])
}

#' @keywords internal
tempest_harvest_native_sources_from_json <- function(json, store) {
  tempest_harvest_native_source_candidates(
    json,
    store,
    kind = "native_search"
  )
}

#' @keywords internal
tempest_harvest_native_sources_from_turn <- function(turn, store) {
  if (is.null(turn) || is.null(store)) {
    return(character())
  }
  if (!inherits(store, "SourceStore")) {
    return(character())
  }
  contents <- tryCatch(turn@contents, error = function(e) list())
  ids <- unlist(
    purrr::map(contents, tempest_harvest_native_sources_from_content, store),
    use.names = FALSE
  )
  json <- tryCatch(turn@json, error = function(e) list())
  ids <- c(ids, tempest_harvest_native_sources_from_json(json, store))
  unique(ids[!is.na(ids) & nzchar(ids)])
}

#' @keywords internal
tempest_harvest_native_sources_from_chat <- function(chat, store) {
  turn <- tryCatch(chat$last_turn(), error = function(e) NULL)
  tempest_harvest_native_sources_from_turn(turn, store)
}

#' @keywords internal
tempest_tool_source_payload <- function(source, excerpt_chars = 1200L) {
  excerpt_chars <- as.integer(excerpt_chars %||% 1200L)
  if (is.na(excerpt_chars) || excerpt_chars < 0L) {
    excerpt_chars <- 1200L
  }
  excerpt <- source$content_text %||% NA_character_
  if (!is.na(excerpt) && nzchar(excerpt)) {
    excerpt <- substr(excerpt, 1, excerpt_chars)
  } else {
    excerpt <- source$snippet %||% NA_character_
  }
  list(
    source_id = source$id,
    url = source$url,
    title = source$title,
    fetched_at = source$fetched_at,
    kind = source$meta$kind %||% NA_character_,
    error = source$meta$error %||% NA_character_,
    excerpt = excerpt
  )
}

#' @keywords internal
tempest_tool_source_summary_payload <- function(source) {
  list(
    source_id = source$id,
    url = source$url,
    title = source$title,
    snippet = source$snippet %||% NA_character_,
    fetched_at = source$fetched_at,
    kind = source$meta$kind %||% NA_character_,
    error = source$meta$error %||% NA_character_
  )
}

#' @keywords internal
tempest_tool_evidence_span_payload <- function(span) {
  list(
    evidence_span_id = span@evidence_span_id,
    source_id = span@source_id,
    quote = span@quote,
    start_offset = span@start_offset,
    end_offset = span@end_offset,
    page = span@page,
    section_heading = span@section_heading,
    relevance_score = span@relevance_score,
    extracted_by = span@extracted_by,
    created_at = span@created_at
  )
}

#' @keywords internal
tempest_tool_claim_payload <- function(
  claim,
  store = NULL,
  include_sources = FALSE
) {
  sources <- list()
  if (isTRUE(include_sources) && !is.null(store)) {
    sources <- purrr::map(
      claim@source_ids,
      function(source_id) {
        source <- store$get_source(source_id)
        if (is.null(source)) {
          return(list(source_id = source_id, missing = TRUE))
        }
        tempest_tool_source_payload(source, excerpt_chars = 500L)
      }
    )
  }
  list(
    claim_id = claim@claim_id,
    claim_text = claim@claim_text,
    source_ids = claim@source_ids,
    evidence_span_ids = claim@evidence_span_ids,
    confidence = claim@confidence,
    verification_status = claim@verification_status,
    support_score = claim@support_score,
    session_id = claim@session_id,
    persona_id = claim@persona_id,
    retrieval_step_id = claim@retrieval_step_id,
    created_at = claim@created_at,
    sources = sources
  )
}

#' @keywords internal
tempest_tool_fact_payload <- function(claim) {
  list(
    fact_id = claim@claim_id,
    claim = claim@claim_text,
    claim_id = claim@claim_id,
    claim_text = claim@claim_text,
    source_ids = claim@source_ids,
    confidence = claim@confidence,
    verification_status = claim@verification_status,
    support_score = claim@support_score,
    session_id = claim@session_id,
    persona_id = claim@persona_id,
    retrieval_step_id = claim@retrieval_step_id,
    created_at = claim@created_at
  )
}

#' @keywords internal
tempest_add_tool_claim <- function(
  store,
  claim_text,
  source_ids,
  confidence = "medium",
  tool_name = "add_claim",
  provenance = list()
) {
  provenance <- tempest_resolve_claim_provenance(provenance)
  source_ids <- unlist(
    source_ids %||% character(),
    recursive = TRUE,
    use.names = FALSE
  )
  source_ids <- unique(as.character(source_ids))
  source_ids <- source_ids[!is.na(source_ids) & nzchar(source_ids)]
  if (length(source_ids) == 0) {
    tempest_abort(
      "{tool_name} requires at least one source_id in {.arg source_ids}."
    )
  }

  unknown <- source_ids[purrr::map_lgl(
    source_ids,
    ~ is.null(store$get_source(.x))
  )]
  if (length(unknown) > 0) {
    tempest_abort(c(
      "{tool_name} requires source_ids already in the SourceStore.",
      x = "Unknown source_id(s): {.val {unknown}}",
      i = "Use web_search and fetch_url, or provider-native search, before recording the claim."
    ))
  }

  claim <- tempest_claim(
    claim_text = claim_text,
    source_ids = source_ids,
    confidence = confidence,
    session_id = provenance$session_id %||% NA_character_,
    persona_id = provenance$persona_id %||% NA_character_,
    retrieval_step_id = provenance$retrieval_step_id %||% NA_character_
  )
  store$add_claim(claim)
  claim
}

#' @keywords internal
tempest_resolve_claim_provenance <- function(provenance) {
  if (is.null(provenance)) {
    return(list())
  }
  if (is.function(provenance)) {
    return(provenance() %||% list())
  }
  provenance
}

#' @keywords internal
tempest_tool_review_functions <- function(store) {
  get_source <- function(source_id, excerpt_chars = 1200L) {
    src <- store$get_source(source_id)
    if (is.null(src)) {
      return(NULL)
    }
    tempest_tool_source_payload(src, excerpt_chars = excerpt_chars)
  }

  list_sources <- function() {
    purrr::map(store$list_sources(), tempest_tool_source_summary_payload)
  }

  list_claims <- function() {
    purrr::map(store$list_claims(), tempest_tool_claim_payload)
  }

  get_claim <- function(claim_id) {
    claim <- store$get_claim(claim_id)
    if (is.null(claim)) {
      return(NULL)
    }
    tempest_tool_claim_payload(claim, store = store, include_sources = TRUE)
  }

  get_evidence_for_claim <- function(claim_id) {
    claim <- store$get_claim(claim_id)
    if (is.null(claim)) {
      return(NULL)
    }
    spans <- store$get_evidence_for_claim(claim_id)
    list(
      claim = tempest_tool_claim_payload(
        claim,
        store = store,
        include_sources = FALSE
      ),
      evidence_spans = purrr::map(spans, tempest_tool_evidence_span_payload),
      cited_sources = purrr::map(claim@source_ids, function(source_id) {
        source <- store$get_source(source_id)
        if (is.null(source)) {
          return(list(source_id = source_id, missing = TRUE))
        }
        tempest_tool_source_payload(source, excerpt_chars = 500L)
      })
    )
  }

  list_unsupported_claims <- function(limit = 20L) {
    limit <- as.integer(limit %||% 20L)
    if (is.na(limit) || limit < 1L) {
      limit <- 20L
    }
    unsupported_statuses <- c(
      "partially_supported",
      "unsupported",
      "contradicted",
      "unverifiable"
    )
    claims <- purrr::keep(
      store$list_claims(),
      ~ .x@verification_status %in% unsupported_statuses
    )
    claims <- utils::head(claims, limit)
    purrr::map(claims, tempest_tool_claim_payload)
  }

  list(
    get_source = get_source,
    list_sources = list_sources,
    list_claims = list_claims,
    get_claim = get_claim,
    get_evidence_for_claim = get_evidence_for_claim,
    list_unsupported_claims = list_unsupported_claims
  )
}

#' @keywords internal
tempest_tool_review_ellmer_tools <- function(review) {
  list(
    ellmer::tool(
      review$get_source,
      name = "get_source",
      description = "Get a previously fetched source by source_id. Returns metadata and a compact excerpt.",
      arguments = list(
        source_id = ellmer::type_string("Source id, e.g. S123abc..."),
        excerpt_chars = ellmer::type_integer(
          "Maximum characters to include in the excerpt.",
          required = FALSE
        )
      )
    ),
    ellmer::tool(
      review$list_sources,
      name = "list_sources",
      description = "List compact metadata for sources currently stored in memory for this session.",
      arguments = list()
    ),
    ellmer::tool(
      review$list_claims,
      name = "list_claims",
      description = "List compact source-backed claim records currently stored in memory for this session.",
      arguments = list()
    ),
    ellmer::tool(
      review$get_claim,
      name = "get_claim",
      description = "Get a claim by claim_id, including verification status, support score, and cited source context.",
      arguments = list(
        claim_id = ellmer::type_string("Claim id, e.g. C123abc...")
      )
    ),
    ellmer::tool(
      review$get_evidence_for_claim,
      name = "get_evidence_for_claim",
      description = "Get evidence spans and compact cited-source excerpts for a claim.",
      arguments = list(
        claim_id = ellmer::type_string("Claim id, e.g. C123abc...")
      )
    ),
    ellmer::tool(
      review$list_unsupported_claims,
      name = "list_unsupported_claims",
      description = "List claims whose verification status indicates weak, missing, or contradictory support.",
      arguments = list(
        limit = ellmer::type_integer(
          "Maximum number of claims to return.",
          required = FALSE
        )
      )
    )
  )
}

#' @keywords internal
tempest_tool_write_functions <- function(store, provenance = list()) {
  add_claim <- function(claim_text, source_ids, confidence = "medium") {
    claim <- tempest_add_tool_claim(
      store,
      claim_text = claim_text,
      source_ids = source_ids,
      confidence = confidence,
      provenance = tempest_resolve_claim_provenance(provenance)
    )
    tempest_tool_claim_payload(claim)
  }

  # Transitional aliases keep older prompts/tool loops working while new
  # agent-facing instructions move to claim terminology.
  list_facts <- function() {
    purrr::map(store$list_claims(), tempest_tool_fact_payload)
  }

  add_fact <- function(claim, source_ids, confidence = "medium") {
    cl <- tempest_add_tool_claim(
      store,
      claim_text = claim,
      source_ids = source_ids,
      confidence = confidence,
      tool_name = "add_fact",
      provenance = tempest_resolve_claim_provenance(provenance)
    )
    tempest_tool_fact_payload(cl)
  }

  list(add_claim = add_claim, list_facts = list_facts, add_fact = add_fact)
}

#' @keywords internal
tempest_tool_write_ellmer_tools <- function(write) {
  list(
    ellmer::tool(
      write$add_claim,
      name = "add_claim",
      description = paste(
        "Add an atomic claim to the session memory with supporting source_ids.",
        "Use this after inspecting sources so later writing can cite the claim."
      ),
      arguments = list(
        claim_text = ellmer::type_string("Atomic factual claim."),
        source_ids = ellmer::type_array(ellmer::type_string(
          "One or more source ids that support the claim."
        )),
        confidence = ellmer::type_enum(
          c("low", "medium", "high"),
          "Confidence level.",
          required = FALSE
        )
      )
    ),
    ellmer::tool(
      write$list_facts,
      name = "list_facts",
      description = "Transitional alias for list_claims.",
      arguments = list()
    ),
    ellmer::tool(
      write$add_fact,
      name = "add_fact",
      description = "Transitional alias for add_claim.",
      arguments = list(
        claim = ellmer::type_string("Atomic factual claim."),
        source_ids = ellmer::type_array(ellmer::type_string(
          "One or more source ids that support the claim."
        )),
        confidence = ellmer::type_enum(
          c("low", "medium", "high"),
          "Confidence level.",
          required = FALSE
        )
      )
    )
  )
}

#' @keywords internal
tempest_tools_retrieval <- function(
  retriever,
  allow_claim_writes = TRUE,
  claim_provenance = list()
) {
  tempest_require("ellmer", "Tool calling for retrieval.")
  stopifnot(inherits(retriever, "TempestRetriever"))

  web_search <- function(query, k = retriever$config@max_search_results) {
    retriever$search(query = query, k = k)
  }

  fetch_url <- function(url) {
    src <- retriever$fetch(url)
    # return a compact payload to the model
    list(
      source_id = src$id,
      url = src$url,
      title = src$title,
      fetched_at = src$fetched_at,
      kind = src$meta$kind %||% NA_character_,
      error = src$meta$error %||% NA_character_,
      excerpt = if (!is.na(src$content_text)) {
        substr(src$content_text, 1, 1200)
      } else {
        NA_character_
      }
    )
  }

  review <- tempest_tool_review_functions(retriever$store)
  write <- tempest_tool_write_functions(
    retriever$store,
    provenance = claim_provenance
  )

  tools <- c(
    list(
      ellmer::tool(
        web_search,
        name = "web_search",
        description = paste(
          "Search the web (or Wikipedia) for relevant sources. Use this to discover sources to cite.",
          "Return values include source_id, title, url, and snippet."
        ),
        arguments = list(
          query = ellmer::type_string("Search query."),
          k = ellmer::type_integer(
            "Maximum number of results to return.",
            required = FALSE
          )
        )
      ),
      ellmer::tool(
        fetch_url,
        name = "fetch_url",
        description = paste(
          "Fetch a URL and extract readable plain text. Use this after web_search to read a source.",
          "Returns source_id, title, and a text excerpt."
        ),
        arguments = list(
          url = ellmer::type_string("The URL to fetch.")
        )
      )
    ),
    tempest_tool_review_ellmer_tools(review)
  )
  if (isTRUE(allow_claim_writes)) {
    tools <- c(tools, tempest_tool_write_ellmer_tools(write))
  }

  tools
}

#' Get source/claim management tools (without web_search/fetch_url)
#'
#' Returns tools for managing sources and claims, but not the web search tools.
#' Used when provider-native web search is enabled.
#'
#' @param retriever A TempestRetriever object
#' @param allow_claim_writes Whether to include `add_claim` and transitional
#'   fact-writing aliases.
#' @param claim_provenance Optional provenance recorded on claims written via
#'   `add_claim`.
#' @return List of ellmer tools for source/claim management
#' @keywords internal
tempest_tools_source_management <- function(
  retriever,
  allow_claim_writes = TRUE,
  claim_provenance = list()
) {
  tempest_require("ellmer", "Tool calling for retrieval.")
  stopifnot(inherits(retriever, "TempestRetriever"))

  review <- tempest_tool_review_functions(retriever$store)
  write <- tempest_tool_write_functions(
    retriever$store,
    provenance = claim_provenance
  )

  tools <- tempest_tool_review_ellmer_tools(review)
  if (isTRUE(allow_claim_writes)) {
    tools <- c(tools, tempest_tool_write_ellmer_tools(write))
  }
  tools
}

#' Register default tools on a chat
#'
#' Registers web search and source/claim management tools on a chat object.
#' When search_provider is "native" and the provider supports it, uses the
#' provider's built-in web search. Otherwise uses custom tempest tools.
#'
#' @param chat An ellmer chat object
#' @param retriever A TempestRetriever object
#' @param model Model name (used to detect provider for native tools)
#' @param search_provider Search provider setting from config
#' @param allow_claim_writes Whether to register claim-writing tools. Read-heavy
#'   roles can set this to `FALSE` and still inspect sources, claims, evidence,
#'   and unsupported claims.
#' @param claim_provenance Optional provenance recorded on claims written via
#'   `add_claim`.
#' @return The chat object (invisibly)
#' @keywords internal
tempest_register_default_tools <- function(
  chat,
  retriever,
  model = NULL,
  search_provider = "native",
  allow_claim_writes = TRUE,
  claim_provenance = list()
) {
  tempest_require("ellmer", "Tool calling for retrieval.")

  # Determine if we should use native provider tools
  use_native <- FALSE
  if (identical(search_provider, "native") && !is.null(model)) {
    provider <- tempest_detect_provider(model)
    if (!is.null(provider) && tempest_provider_has_native_search(provider)) {
      native_tools <- tempest_get_native_web_tools(provider)
      if (!is.null(native_tools)) {
        chat$register_tools(native_tools)
        use_native <- TRUE
      }
    }
  }

  # If not using native tools, register custom web search/fetch tools
  if (!use_native) {
    tools <- tempest_tools_retrieval(
      retriever,
      allow_claim_writes = allow_claim_writes,
      claim_provenance = claim_provenance
    )
    chat$register_tools(tools)
  } else {
    # Still register source/claim management tools even with native search
    mgmt_tools <- tempest_tools_source_management(
      retriever,
      allow_claim_writes = allow_claim_writes,
      claim_provenance = claim_provenance
    )
    chat$register_tools(mgmt_tools)
  }

  # Register ragnar retrieve tool if store is available
  if (!is.null(retriever$ragnar_store) && tempest_has("ragnar")) {
    ragnar::ragnar_register_tool_retrieve(
      chat,
      retriever$ragnar_store,
      store_description = "the STORM knowledge base containing fetched web sources"
    )
  }

  invisible(chat)
}

# Expert Subagent Pattern (inspired by btw)

#' Expert Session Manager
#'
#' Manages expert chat sessions with session IDs for conversation continuity.
#' Each expert persona gets their own chat session that can be resumed using
#' the session ID returned from previous interactions.
#'
#' @field sessions Environment storing active chat sessions keyed by session ID.
#' @field config A `TempestConfig` object for creating chats.
#' @field retriever A `TempestRetriever` for registering tools.
#' @field extractor Chat object for fact extraction (optional).
#' @field store A `SourceStore` for storing extracted facts (optional).
#' @field progress Optional progress callback.
#' @field run_id Shared Co-STORM session id for progress events.
#' @field session_provenance Environments keyed by expert session id for
#'   claim-write provenance.
#'
#' @keywords internal
ExpertSessionManager <- R6::R6Class(
  "ExpertSessionManager",
  public = list(
    sessions = NULL,
    config = NULL,
    retriever = NULL,
    extractor = NULL,
    store = NULL,
    progress = NULL,
    run_id = NULL,
    session_provenance = NULL,

    #' @description
    #' Create a new ExpertSessionManager.
    #' @param config A `TempestConfig` object.
    #' @param retriever A `TempestRetriever` object.
    #' @param extractor Optional chat object for fact extraction.
    #' @param store Optional `SourceStore` for storing extracted facts.
    #' @param progress Optional progress callback.
    #' @param run_id Shared Co-STORM session id for progress events.
    initialize = function(
      config,
      retriever,
      extractor = NULL,
      store = NULL,
      progress = NULL,
      run_id = NULL
    ) {
      self$sessions <- new.env(parent = emptyenv())
      self$session_provenance <- new.env(parent = emptyenv())
      self$config <- config
      self$retriever <- retriever
      self$extractor <- extractor
      self$store <- store
      self$progress <- tempest_progress_callback(progress)
      self$run_id <- run_id %||% tempest_uuid("session")
      invisible(self)
    },

    #' @description
    #' Emit a Co-STORM expert progress event.
    #' @param event_type Progress event type.
    #' @param status Progress event status.
    #' @param stage Optional workflow stage.
    #' @param step Optional workflow step.
    #' @param message Optional progress message.
    #' @param payload Optional progress metadata.
    #' @param parent_event_id Optional parent event id.
    #' @param correlation_id Optional correlation id.
    emit_progress = function(
      event_type,
      status,
      stage = NA_character_,
      step = NA_character_,
      message = NA_character_,
      payload = list(),
      parent_event_id = NA_character_,
      correlation_id = NA_character_
    ) {
      tempest_emit_progress(
        self$progress,
        run_id = self$run_id,
        workflow = "costorm",
        event_type = event_type,
        status = status,
        stage = stage,
        step = step,
        message = message,
        payload = payload,
        parent_event_id = parent_event_id,
        correlation_id = correlation_id
      )
    },

    #' @description
    #' Extract facts from an expert response.
    #' @param response Character string response from expert.
    #' @param turn Optional ellmer turn to inspect for provider-native sources.
    #' @param source_ids Optional source ids already harvested for the turn.
    #' @param session_id Optional expert session id.
    #' @param persona_id Optional persona id.
    #' @param correlation_id Optional progress correlation id for the turn.
    #' @return Invisibly returns NULL.
    extract_facts = function(
      response,
      turn = NULL,
      source_ids = NULL,
      session_id = NA_character_,
      persona_id = NA_character_,
      correlation_id = NA_character_
    ) {
      if (!is.null(self$extractor) && !is.null(self$store)) {
        event <- self$emit_progress(
          "step",
          "started",
          stage = "evidence",
          step = "fact_extraction",
          correlation_id = correlation_id
        )
        tryCatch(
          {
            # Only re-harvest when the caller did not already do so; callers that
            # pass source_ids have harvested the turn into the store already.
            harvested <- if (is.null(source_ids)) {
              tempest_harvest_native_sources_from_turn(turn, self$store)
            } else {
              character()
            }
            tempest_extract_facts_from_answer(
              self$extractor,
              response,
              self$store,
              source_ids = unique(c(source_ids, harvested)),
              session_id = session_id,
              persona_id = persona_id,
              retrieval_step_id = correlation_id
            )
            self$emit_progress(
              "step",
              "succeeded",
              stage = "evidence",
              step = "fact_extraction",
              parent_event_id = event@event_id,
              correlation_id = event@correlation_id,
              payload = list(claim_count = length(self$store$list_claims()))
            )
          },
          error = function(e) {
            self$emit_progress(
              "step",
              "failed",
              stage = "evidence",
              step = "fact_extraction",
              parent_event_id = event@event_id,
              correlation_id = event@correlation_id,
              payload = tempest_progress_error_payload(e)
            )
            stop(e)
          }
        )
      }
      invisible(NULL)
    },

    #' @description
    #' Generate a human-readable session ID from a persona name.
    #' @param persona_name Name of the persona.
    #' @return Character string session ID like "dr-sarah-chen-abc123".
    generate_session_id = function(persona_name) {
      base <- tolower(gsub("[^a-zA-Z0-9]+", "-", persona_name))
      base <- gsub("^-|-$", "", base)
      suffix <- substr(digest::digest(runif(1), algo = "xxhash64"), 1, 7)
      paste0(base, "-", suffix)
    },

    #' @description
    #' Get an existing session or create a new one.
    #' @param persona A persona object from `tempest_generate_personas()`.
    #' @param session_id Optional session ID to resume.
    #' @return List with `chat`, `session_id`, and `is_new` fields.
    get_or_create = function(persona, session_id = NULL) {
      # If session_id provided and exists, return it
      if (!is.null(session_id) && exists(session_id, envir = self$sessions)) {
        return(list(
          chat = get(session_id, envir = self$sessions),
          session_id = session_id,
          is_new = FALSE,
          provenance = get(
            session_id,
            envir = self$session_provenance,
            inherits = FALSE
          )
        ))
      }

      # Create new session
      sp <- tempest_render_expert_prompt(
        persona = persona,
        expert_id = persona$id %||% 1L
      )
      new_id <- session_id %||%
        self$generate_session_id(persona$name %||% "expert")
      persona_id <- as.character(persona$id %||% NA_integer_)
      provenance <- new.env(parent = emptyenv())
      provenance$base <- list(
        session_id = new_id,
        persona_id = persona_id
      )
      provenance$current <- list()
      chat <- tempest_make_chat(
        self$config,
        "expert",
        system_prompt = sp,
        echo = "none"
      )
      tempest_register_default_tools(
        chat,
        self$retriever,
        model = self$config@models[["expert"]],
        search_provider = self$config@search_provider,
        claim_provenance = function() {
          utils::modifyList(provenance$base, provenance$current %||% list())
        }
      )

      assign(new_id, chat, envir = self$sessions)
      assign(new_id, provenance, envir = self$session_provenance)

      list(
        chat = chat,
        session_id = new_id,
        is_new = TRUE,
        provenance = provenance
      )
    },

    #' @description
    #' List all active session IDs.
    #' @return Character vector of session IDs.
    list_sessions = function() {
      ls(envir = self$sessions)
    }
  )
)

#' Create an Expert Subagent Tool
#'
#' Creates an ellmer tool for a specific expert persona that can be called
#' by the moderator to delegate questions.
#'
#' @param persona A persona object from `tempest_generate_personas()`.
#' @param session_manager An `ExpertSessionManager` instance.
#' @param topic The research topic (for context).
#' @return An ellmer tool.
#' @keywords internal
tempest_create_expert_tool <- function(persona, session_manager, topic) {
  tempest_require("ellmer", "Expert tools require ellmer.")

  # Sanitize name for tool name (e.g., "Dr. Sarah Chen" -> "ask_dr_sarah_chen")
  safe_name <- tolower(gsub("[^a-zA-Z0-9]+", "_", persona$name %||% "expert"))
  safe_name <- gsub("^_|_$", "", safe_name)
  tool_name <- paste0("ask_", safe_name)

  # Build description from persona
  desc <- paste0(
    "Ask ",
    persona$name %||% "Expert",
    ", ",
    persona$title %||% "Research Specialist",
    ". ",
    persona$perspective %||% "General research perspective."
  )

  # Capture persona in closure
  p <- persona
  topic_str <- topic
  mgr <- session_manager

  ask_expert <- function(question, session_id = NULL) {
    result <- mgr$get_or_create(p, session_id)
    chat <- result$chat
    sid <- result$session_id
    provenance <- result$provenance
    persona_id <- as.character(p$id %||% NA_integer_)
    expert_name <- p$name %||% "Expert"
    correlation_id <- tempest_uuid("tool")
    tool_event <- mgr$emit_progress(
      "tool",
      "started",
      stage = "dialogue",
      step = tool_name,
      correlation_id = correlation_id,
      payload = list(
        expert_id = persona_id,
        expert_name = expert_name,
        session_id = sid
      )
    )

    # Build prompt with context
    prompt <- paste0(
      "Topic: ",
      topic_str,
      "\n\n",
      "Question: ",
      question,
      "\n\n",
      "Instructions:\n",
      "- Use the available web/source tools to find and cite sources.\n",
      "- If web_search and fetch_url are available, search first and then fetch sources.\n",
      "- Only state factual claims supported by sources you inspected.\n",
      "- If add_claim is available, record key source-backed claims with it.\n",
      "- For each factual sentence, add source IDs like [Sxxxxxxxxxxxx] when available.\n",
      "- If evidence is weak or unclear, say so.\n\n",
      "Respond now:"
    )

    old_provenance <- provenance$current %||% list()
    provenance$current <- list(
      session_id = sid,
      persona_id = persona_id,
      retrieval_step_id = correlation_id
    )
    response <- tryCatch(
      chat$chat(prompt, echo = "none"),
      error = function(e) {
        mgr$emit_progress(
          "tool",
          "failed",
          stage = "dialogue",
          step = tool_name,
          parent_event_id = tool_event@event_id,
          correlation_id = correlation_id,
          payload = c(
            list(
              expert_id = persona_id,
              expert_name = expert_name,
              session_id = sid
            ),
            tempest_progress_error_payload(e)
          )
        )
        stop(e)
      },
      finally = {
        provenance$current <- old_provenance
      }
    )

    # Extract plain text from the response using ellmer's contents_markdown
    # This properly handles ellmer_output objects and tool call results
    last_turn <- tryCatch(chat$last_turn(), error = function(e) NULL)
    response_text <- if (
      is.null(last_turn) || length(last_turn@contents) == 0
    ) {
      if (is.character(response) && length(response) > 0) {
        paste(response, collapse = "\n")
      } else {
        "(Expert completed but returned no message.)"
      }
    } else {
      ellmer::contents_markdown(last_turn)
    }

    # Extract facts from expert response
    mgr$extract_facts(
      response_text,
      turn = last_turn,
      session_id = sid,
      persona_id = persona_id,
      correlation_id = correlation_id
    )
    mgr$emit_progress(
      "tool",
      "succeeded",
      stage = "dialogue",
      step = tool_name,
      parent_event_id = tool_event@event_id,
      correlation_id = correlation_id,
      payload = list(
        expert_id = persona_id,
        expert_name = expert_name,
        session_id = sid
      )
    )

    list(
      expert = expert_name,
      response = response_text,
      session_id = sid
    )
  }

  ellmer::tool(
    ask_expert,
    name = tool_name,
    description = desc,
    arguments = list(
      question = ellmer::type_string(
        "The question or topic to research. Be specific about what you need."
      ),
      session_id = ellmer::type_string(
        "Optional: Resume a previous conversation with this expert. Use the session_id from a previous response.",
        required = FALSE
      )
    )
  )
}

#' Create Expert Tools for All Personas
#'
#' Creates ellmer tools for each expert persona and returns them as a list.
#'
#' @param personas List of persona objects.
#' @param session_manager An `ExpertSessionManager` instance.
#' @param topic The research topic.
#' @return A list of ellmer tools.
#' @keywords internal
tempest_create_expert_tools <- function(personas, session_manager, topic) {
  purrr::map(personas, ~ tempest_create_expert_tool(.x, session_manager, topic))
}

#' Register Expert Tools on a Chat
#'
#' Registers expert subagent tools on a moderator chat, allowing it to
#' delegate questions to specialized experts.
#'
#' @param chat An ellmer chat object (the moderator).
#' @param personas List of persona objects.
#' @param session_manager An `ExpertSessionManager` instance.
#' @param topic The research topic.
#' @return The chat object (invisibly).
#' @keywords internal
tempest_register_expert_tools <- function(
  chat,
  personas,
  session_manager,
  topic
) {
  tools <- tempest_create_expert_tools(personas, session_manager, topic)
  chat$register_tools(tools)
  invisible(chat)
}

#' Register a single expert tool on a chat
#'
#' @param chat An ellmer chat object.
#' @param persona A persona object.
#' @param session_manager An `ExpertSessionManager` instance.
#' @param topic The research topic.
#' @return The chat object (invisibly).
#' @keywords internal
tempest_register_single_expert_tool <- function(
  chat,
  persona,
  session_manager,
  topic
) {
  tool <- tempest_create_expert_tool(persona, session_manager, topic)
  chat$register_tools(list(tool))
  invisible(chat)
}

#' Generate a single expert persona for a specific area
#'
#' @param topic The research topic.
#' @param area The area of expertise needed.
#' @param existing_personas List of existing personas to avoid duplication.
#' @param config A `TempestConfig` object.
#' @return A persona list.
#' @keywords internal
tempest_generate_single_persona <- function(
  topic,
  area,
  existing_personas,
  config
) {
  tempest_require("ellmer", "Generating expert personas requires ellmer.")

  chat <- tempest_make_chat(
    config,
    "coordinator",
    system_prompt = tempest_prompt("expert_generator_system"),
    echo = "none"
  )

  existing_desc <- if (length(existing_personas) > 0) {
    paste(
      purrr::map_chr(existing_personas, function(p) {
        paste0(
          "- ",
          p$name %||% "Expert",
          " (",
          p$title %||% "",
          "): ",
          p$perspective %||% ""
        )
      }),
      collapse = "\n"
    )
  } else {
    "(none)"
  }

  type <- tempest_type_personas()
  prompt <- paste0(
    "Topic: ",
    topic,
    "\n\n",
    "Needed expertise: ",
    area,
    "\n\n",
    "Existing experts (do not duplicate):\n",
    existing_desc,
    "\n\n",
    "Generate exactly 1 expert persona to fill this knowledge gap.\n",
    "Return structured data."
  )

  result <- tryCatch(
    chat$chat_structured(prompt, type = type, echo = "none", convert = FALSE),
    error = function(e) {
      tempest_warn(
        "Failed to generate expert persona for {.val {area}}: {conditionMessage(e)}"
      )
      list(personas = list())
    }
  )
  personas <- result$personas %||% list()
  if (length(personas) == 0) {
    tempest_warn("Using generic fallback persona for {.val {area}}.")
    return(list(
      name = paste("Expert in", area),
      title = area,
      affiliation = "Independent",
      background = paste("Expert in", area),
      focus_areas = list(area),
      perspective = paste("Specializes in", area),
      initial_questions = list(paste(
        "What are the key aspects of",
        area,
        "related to",
        topic,
        "?"
      ))
    ))
  }
  personas[[1]]
}
