# Tools for ellmer tool calling

#' Detect provider from model name
#'
#' Parses model names like "openai/gpt-5.6-luna" or
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

  add_candidate <- function(
    url,
    title = NA_character_,
    snippet = NA_character_,
    content_text = NA_character_,
    context_text = NA_character_
  ) {
    if (is.character(url) && length(url) > 0 && !is.na(url[[1]])) {
      candidates[[length(candidates) + 1L]] <<- list(
        url = url[[1]],
        title = title,
        snippet = snippet,
        content_text = content_text,
        context_text = context_text
      )
    }
    invisible(NULL)
  }

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

    context_text <- tempest_native_scalar(
      value$context_text,
      value$citation_context,
      value$context
    )
    annotations <- value$annotations %||% value$citations
    if (is.data.frame(annotations)) {
      annotations <- split(annotations, seq_len(nrow(annotations)))
    }
    if (is.list(annotations) && length(annotations) > 0L) {
      parent_text <- tempest_native_scalar(value$text)
      annotation_context <- tempest_native_scalar(context_text, parent_text)
      for (annotation in annotations) {
        if (!is.list(annotation)) {
          next
        }
        url <- annotation$url %||% annotation$link %||% annotation$uri
        add_candidate(
          url,
          title = tempest_native_scalar(annotation$title, annotation$name),
          snippet = tempest_native_scalar(
            annotation$snippet,
            annotation$description,
            annotation$summary
          ),
          content_text = tempest_native_scalar(
            annotation$content,
            annotation$text
          ),
          context_text = annotation_context
        )
      }
    }

    url <- value$url %||% value$link %||% value$uri
    add_candidate(
      url,
      title = tempest_native_scalar(value$title, value$name),
      snippet = tempest_native_scalar(
        value$snippet,
        value$description,
        value$summary
      ),
      content_text = tempest_native_scalar(value$content, value$text),
      context_text = context_text
    )

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
  context_text = NA_character_,
  kind = "native_search"
) {
  url <- tryCatch(tempest_normalize_url(url), error = function(e) NA_character_)
  if (is.na(url) || !nzchar(url)) {
    return(NULL)
  }
  meta <- list(kind = kind, provider_tool = "native")
  context_text <- tempest_source_scalar(context_text)
  if (!is.na(context_text) && nzchar(context_text)) {
    meta$context_text <- context_text
  }
  tempest_source(
    url = url,
    title = title,
    snippet = snippet,
    content_text = content_text,
    fetched_at = tempest_now_utc(),
    meta = meta
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
  source <- tempest_merge_source_record(
    store$get_retrieved_source(source$id),
    source
  )
  store$upsert_retrieved_resource(source)
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
          context_text = candidate$context_text,
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
          context_text = tempest_native_scalar(
            json$context_text,
            json$citation_context
          ),
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
  if (!inherits(store, "ResearchWorkspace")) {
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
        source <- store$get_retrieved_source(source_id)
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
    expert_id = claim@expert_id,
    retrieval_step_id = claim@retrieval_step_id,
    created_at = claim@created_at,
    retrieved_resources = sources
  )
}

#' @keywords internal
tempest_add_tool_claim <- function(
  store,
  claim_text,
  source_ids,
  confidence = "medium",
  tool_name = "add_proposed_claim",
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
    ~ is.null(store$get_retrieved_source(.x))
  )]
  if (length(unknown) > 0) {
    tempest_abort(c(
      "{tool_name} requires source_ids already in the ResearchWorkspace.",
      x = "Unknown source_id(s): {.val {unknown}}",
      i = "Use web_search and fetch_url, or provider-native search, before recording the claim."
    ))
  }

  claim <- tempest_claim(
    claim_text = claim_text,
    source_ids = source_ids,
    confidence = confidence,
    session_id = provenance$session_id %||% NA_character_,
    expert_id = provenance$expert_id %||% NA_character_,
    retrieval_step_id = provenance$retrieval_step_id %||% NA_character_
  )
  store$add_proposed_claim(claim)
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
tempest_tool_review_functions <- function(workspace) {
  get_retrieved_source <- function(source_id, excerpt_chars = 1200L) {
    src <- workspace$get_retrieved_source(source_id)
    if (is.null(src)) {
      return(NULL)
    }
    tempest_tool_source_payload(src, excerpt_chars = excerpt_chars)
  }

  list_retrieved_sources <- function() {
    purrr::map(
      workspace$list_retrieved_sources(),
      tempest_tool_source_summary_payload
    )
  }

  list_proposed_claims <- function() {
    purrr::map(
      workspace$list_proposed_claims(),
      tempest_tool_claim_payload
    )
  }

  get_proposed_claim <- function(claim_id) {
    claim <- workspace$get_proposed_claim(claim_id)
    if (is.null(claim)) {
      return(NULL)
    }
    tempest_tool_claim_payload(
      claim,
      store = workspace,
      include_sources = TRUE
    )
  }

  get_evidence_for_proposed_claim <- function(claim_id) {
    claim <- workspace$get_proposed_claim(claim_id)
    if (is.null(claim)) {
      return(NULL)
    }
    spans <- workspace$get_evidence_for_proposed_claim(claim_id)
    list(
      claim = tempest_tool_claim_payload(
        claim,
        store = workspace,
        include_sources = FALSE
      ),
      evidence_spans = purrr::map(spans, tempest_tool_evidence_span_payload),
      cited_sources = purrr::map(claim@source_ids, function(source_id) {
        source <- workspace$get_retrieved_source(source_id)
        if (is.null(source)) {
          return(list(source_id = source_id, missing = TRUE))
        }
        tempest_tool_source_payload(source, excerpt_chars = 500L)
      })
    )
  }

  list_unsupported_proposed_claims <- function(limit = 20L) {
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
      workspace$list_proposed_claims(),
      ~ .x@verification_status %in% unsupported_statuses
    )
    claims <- utils::head(claims, limit)
    purrr::map(claims, tempest_tool_claim_payload)
  }

  list(
    get_retrieved_source = get_retrieved_source,
    list_retrieved_sources = list_retrieved_sources,
    list_proposed_claims = list_proposed_claims,
    get_proposed_claim = get_proposed_claim,
    get_evidence_for_proposed_claim = get_evidence_for_proposed_claim,
    list_unsupported_proposed_claims = list_unsupported_proposed_claims
  )
}

#' @keywords internal
tempest_tool_review_ellmer_tools <- function(review) {
  list(
    ellmer::tool(
      review$get_retrieved_source,
      name = "get_retrieved_source",
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
      review$list_retrieved_sources,
      name = "list_retrieved_sources",
      description = "List compact metadata for sources currently stored in memory for this session.",
      arguments = list()
    ),
    ellmer::tool(
      review$list_proposed_claims,
      name = "list_proposed_claims",
      description = "List compact source-backed claim records currently stored in memory for this session.",
      arguments = list()
    ),
    ellmer::tool(
      review$get_proposed_claim,
      name = "get_proposed_claim",
      description = "Get a claim by claim_id, including verification status, support score, and cited source context.",
      arguments = list(
        claim_id = ellmer::type_string("Claim id, e.g. C123abc...")
      )
    ),
    ellmer::tool(
      review$get_evidence_for_proposed_claim,
      name = "get_evidence_for_proposed_claim",
      description = "Get evidence spans and compact cited-source excerpts for a claim.",
      arguments = list(
        claim_id = ellmer::type_string("Claim id, e.g. C123abc...")
      )
    ),
    ellmer::tool(
      review$list_unsupported_proposed_claims,
      name = "list_unsupported_proposed_claims",
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
tempest_tool_write_functions <- function(workspace, provenance = list()) {
  add_proposed_claim <- function(
    claim_text,
    source_ids,
    confidence = "medium"
  ) {
    claim <- tempest_add_tool_claim(
      workspace,
      claim_text = claim_text,
      source_ids = source_ids,
      confidence = confidence,
      provenance = tempest_resolve_claim_provenance(provenance)
    )
    tempest_tool_claim_payload(claim)
  }

  list(add_proposed_claim = add_proposed_claim)
}

#' @keywords internal
tempest_tool_write_ellmer_tools <- function(write) {
  list(
    ellmer::tool(
      write$add_proposed_claim,
      name = "add_proposed_claim",
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
    )
  )
}

#' @keywords internal
tempest_tools_web <- function(
  retriever,
  model = NULL,
  search_provider = "native"
) {
  tempest_require("ellmer", "Tool calling for web research.")
  stopifnot(inherits(retriever, "TempestRetriever"))

  if (identical(search_provider, "native") && !is.null(model)) {
    provider <- tempest_detect_provider(model)
    if (!is.null(provider) && tempest_provider_has_native_search(provider)) {
      native_tools <- tempest_get_native_web_tools(provider)
      if (!is.null(native_tools)) {
        return(native_tools)
      }
    }
  }

  web_search <- function(query, k = retriever$config@max_search_results) {
    k <- tempest_config_count(k, "k")
    if (k > retriever$config@max_search_results) {
      tempest_config_abort(
        c(
          "Search result request exceeds the configured budget.",
          x = "Requested {k}; maximum is {retriever$config@max_search_results}."
        )
      )
    }
    retriever$search(query = query, k = k)
  }

  fetch_url <- function(url) {
    source <- retriever$fetch(url)
    list(
      source_id = source$id,
      url = source$url,
      title = source$title,
      fetched_at = source$fetched_at,
      kind = source$meta$kind %||% NA_character_,
      error = source$meta$error %||% NA_character_,
      excerpt = if (!is.na(source$content_text)) {
        substr(source$content_text, 1L, 1200L)
      } else {
        NA_character_
      }
    )
  }

  list(
    ellmer::tool(
      web_search,
      name = "web_search",
      description = paste(
        "Search the web for relevant sources.",
        "Results include source ids, titles, URLs, and snippets."
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
        "Fetch a URL and extract readable plain text.",
        "Use this after web_search to inspect a source."
      ),
      arguments = list(
        url = ellmer::type_string("The URL to fetch.")
      )
    )
  )
}

#' @keywords internal
tempest_tools_evidence_read <- function(retriever) {
  tempest_require("ellmer", "Tool calling for evidence review.")
  stopifnot(inherits(retriever, "TempestRetriever"))
  retriever$workspace |>
    tempest_tool_review_functions() |>
    tempest_tool_review_ellmer_tools()
}

#' @keywords internal
tempest_tools_evidence_write <- function(
  retriever,
  claim_provenance = list()
) {
  tempest_require("ellmer", "Tool calling for evidence writing.")
  stopifnot(inherits(retriever, "TempestRetriever"))
  tempest_tool_write_functions(
    retriever$workspace,
    provenance = claim_provenance
  ) |>
    tempest_tool_write_ellmer_tools()
}

#' @keywords internal
tempest_semantic_retrieval_registrar <- function(retriever) {
  stopifnot(inherits(retriever, "TempestRetriever"))
  if (is.null(retriever$ragnar_store) || !tempest_has("ragnar")) {
    return(NULL)
  }
  function(chat, context = list()) {
    ragnar::ragnar_register_tool_retrieve(
      chat,
      retriever$ragnar_store,
      store_description = paste(
        "the Tempest knowledge base containing approved evidence resources"
      )
    )
    invisible(chat)
  }
}

#' @keywords internal
tempest_tools_retrieval <- function(
  retriever,
  allow_claim_writes = TRUE,
  claim_provenance = list()
) {
  tools <- c(
    tempest_tools_web(retriever, search_provider = "custom"),
    tempest_tools_evidence_read(retriever)
  )
  if (isTRUE(allow_claim_writes)) {
    tools <- c(
      tools,
      tempest_tools_evidence_write(retriever, claim_provenance)
    )
  }

  tools
}

#' Get source/claim management tools (without web_search/fetch_url)
#'
#' Returns tools for managing sources and claims, but not the web search tools.
#' Used when provider-native web search is enabled.
#'
#' @param retriever A TempestRetriever object
#' @param allow_claim_writes Whether to include `add_proposed_claim`.
#' @param claim_provenance Optional provenance recorded on claims written via
#'   `add_proposed_claim`.
#' @return List of ellmer tools for source/claim management
#' @keywords internal
tempest_tools_source_management <- function(
  retriever,
  allow_claim_writes = TRUE,
  claim_provenance = list()
) {
  tempest_require("ellmer", "Tool calling for retrieval.")
  stopifnot(inherits(retriever, "TempestRetriever"))

  tools <- tempest_tools_evidence_read(retriever)
  if (isTRUE(allow_claim_writes)) {
    tools <- c(
      tools,
      tempest_tools_evidence_write(retriever, claim_provenance)
    )
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
#'   `add_proposed_claim`.
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

tempest_expert_session_abort <- function(message, ..., parent = NULL) {
  tempest_abort(
    message,
    ...,
    class = c("tempest_expert_session_error", "tempest_error"),
    parent = parent,
    .envir = rlang::caller_env()
  )
}

tempest_expert_session_connection_ids <- function(value, field) {
  ids <- if (is.character(value) && is.null(names(value))) {
    value
  } else if (
    is.list(value) &&
      !is.data.frame(value) &&
      is.null(names(value))
  ) {
    if (length(value) == 0L) {
      character()
    } else {
      valid <- vapply(
        value,
        \(item) rlang::is_string(item) && !is.na(item),
        logical(1)
      )
      if (!all(valid)) {
        tempest_expert_session_abort(
          "Saved {.field {field}} must be a flat string array."
        )
      }
      unlist(value, use.names = FALSE)
    }
  } else {
    tempest_expert_session_abort(
      "Saved {.field {field}} must be a flat string array."
    )
  }
  if (
    anyNA(ids) ||
      any(!nzchar(ids)) ||
      !identical(ids, tempest_trim(ids)) ||
      anyDuplicated(ids)
  ) {
    tempest_expert_session_abort(
      "Saved {.field {field}} contains invalid or duplicate identifiers."
    )
  }
  unname(ids)
}

tempest_expert_session_grants <- function(grants) {
  grants <- tryCatch(
    tempest_contract_serializable_list(grants %||% list(), "binding$grants"),
    error = function(error) {
      tempest_expert_session_abort(
        "Saved capability grants must be canonical non-secret audit records.",
        parent = error
      )
    }
  )
  if (length(grants) == 0L) {
    if (!is.null(names(grants))) {
      names(grants) <- NULL
    }
    return(grants)
  }
  grant_ids <- names(grants)
  if (
    is.null(grant_ids) ||
      anyNA(grant_ids) ||
      any(!nzchar(grant_ids)) ||
      anyDuplicated(grant_ids)
  ) {
    tempest_expert_session_abort(
      "Saved capability grants must be uniquely named by capability id."
    )
  }
  fields <- c(
    "capability_id",
    "capability_version",
    "operation_id",
    "operation_version",
    "required",
    "status",
    "connection_ref_ids",
    "reason_code",
    "reason",
    "metadata"
  )
  for (index in seq_along(grants)) {
    grant <- grants[[index]]
    grant_fields <- names(grant)
    if (
      !is.list(grant) ||
        is.data.frame(grant) ||
        is.null(grant_fields) ||
        anyNA(grant_fields) ||
        anyDuplicated(grant_fields) ||
        !setequal(grant_fields, fields)
    ) {
      tempest_expert_session_abort(
        "Saved capability-grant records do not match the current schema."
      )
    }
    capability_id <- tryCatch(
      tempest_contract_id(grant$capability_id, "capability_id"),
      error = function(error) {
        tempest_expert_session_abort(
          "Saved capability grant has an invalid capability id.",
          parent = error
        )
      }
    )
    if (!identical(grant_ids[[index]], capability_id)) {
      tempest_expert_session_abort(
        "Saved capability-grant names must match their capability ids."
      )
    }
    for (field in c(
      "capability_version",
      "operation_id",
      "operation_version",
      "reason_code",
      "reason"
    )) {
      value <- grant[[field]]
      if (!is.null(value) && (!rlang::is_string(value) || is.na(value))) {
        tempest_expert_session_abort(
          "Saved capability grant has an invalid {.field {field}}."
        )
      }
    }
    if (
      !is.logical(grant$required) ||
        length(grant$required) != 1L ||
        is.na(grant$required) ||
        !rlang::is_string(grant$status) ||
        !grant$status %in% c("granted", "denied")
    ) {
      tempest_expert_session_abort(
        "Saved capability grant has an invalid requirement or status."
      )
    }
    tryCatch(
      tempest_contract_ids(
        tempest_expert_session_connection_ids(
          grant$connection_ref_ids,
          "connection_ref_ids"
        ),
        "connection_ref_ids"
      ),
      error = function(error) {
        tempest_expert_session_abort(
          "Saved capability grant has invalid connection ids.",
          parent = error
        )
      }
    )
    tryCatch(
      tempest_contract_serializable_list(grant$metadata, "grant$metadata"),
      error = function(error) {
        tempest_expert_session_abort(
          "Saved capability-grant metadata is invalid.",
          parent = error
        )
      }
    )
  }
  grants
}

tempest_expert_connection_grants <- function(
  experts,
  allowed_connection_ref_ids,
  runtime
) {
  allowed_connection_ref_ids <- allowed_connection_ref_ids %||% list()
  if (
    !is.list(allowed_connection_ref_ids) ||
      is.data.frame(allowed_connection_ref_ids)
  ) {
    tempest_expert_session_abort(
      "{.arg allowed_connection_ref_ids} must be a named list."
    )
  }
  expert_ids <- purrr::map_chr(experts, \(expert) expert@expert_id)
  if (length(allowed_connection_ref_ids) > 0L) {
    grant_ids <- names(allowed_connection_ref_ids)
    if (
      is.null(grant_ids) ||
        anyNA(grant_ids) ||
        any(!nzchar(grant_ids)) ||
        anyDuplicated(grant_ids)
    ) {
      tempest_expert_session_abort(
        "{.arg allowed_connection_ref_ids} must be named by expert id."
      )
    }
    unknown <- setdiff(grant_ids, expert_ids)
    if (length(unknown) > 0L) {
      tempest_expert_session_abort(
        "Connection grants identify unknown expert {.val {unknown[[1]]}}."
      )
    }
  }
  grants <- stats::setNames(
    lapply(expert_ids, function(expert_id) {
      connection_ids <- tempest_contract_ids(
        allowed_connection_ref_ids[[expert_id]] %||% character(),
        paste0("allowed_connection_ref_ids$", expert_id)
      )
      unavailable <- connection_ids[
        !vapply(
          connection_ids,
          runtime$connections$has,
          logical(1)
        )
      ]
      if (length(unavailable) > 0L) {
        tempest_expert_session_abort(c(
          "Expert {.val {expert_id}} has an unavailable connection grant.",
          x = "Connection {.val {unavailable[[1]]}} is not registered."
        ))
      }
      connection_ids
    }),
    expert_ids
  )
  grants
}

tempest_expert_system_prompt <- function(expert, resolution) {
  parts <- c(
    tempest_render_expert_prompt(expert, expert_id = expert@expert_id),
    paste0("Expert instructions:\n", expert@instructions)
  )
  if (nzchar(resolution$instructions %||% "")) {
    parts <- c(
      parts,
      paste0("Assigned skill instructions:\n", resolution$instructions)
    )
  }
  paste(parts, collapse = "\n\n")
}

#' Expert Session Manager
#'
#' Manages capability-scoped chats for validated expert profiles.
#'
#' @field sessions Environment storing active chat sessions keyed by session ID.
#' @field session_profiles Environment storing serializable session bindings.
#' @field config A `TempestConfig` object for creating chats.
#' @field retriever A `TempestRetriever` for registering tools.
#' @field runtime A `TempestRuntime` used to resolve skills and capabilities.
#' @field experts Environment of expert profiles keyed by stable expert id.
#' @field expert_connection_ref_ids Environment of allowed connection ids by
#'   expert.
#' @field extractor Chat object for fact extraction (optional).
#' @field extract_claims_program ProgramSet-bound claim-extraction execution.
#' @field workspace A [ResearchWorkspace] for extracted facts (optional).
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
    session_profiles = NULL,
    config = NULL,
    retriever = NULL,
    runtime = NULL,
    experts = NULL,
    expert_connection_ref_ids = NULL,
    extractor = NULL,
    extract_claims_program = NULL,
    workspace = NULL,
    progress = NULL,
    run_id = NULL,
    session_provenance = NULL,

    #' @description
    #' Create a new ExpertSessionManager.
    #' @param experts Validated `tempest_expert` profiles.
    #' @param runtime A `TempestRuntime`.
    #' @param config A `TempestConfig` object.
    #' @param retriever A `TempestRetriever` object.
    #' @param allowed_connection_ref_ids Named list of allowed connection ids by
    #'   expert id.
    #' @param extractor Optional chat object for fact extraction.
    #' @param extract_claims_program ProgramSet-bound claim-extraction
    #'   execution. Required when `extractor` is supplied.
    #' @param workspace Optional [ResearchWorkspace] for extracted facts.
    #' @param progress Optional progress callback.
    #' @param run_id Shared Co-STORM session id for progress events.
    #' @param stage_recorder Optional callback accepting a stage record and its
    #'   evaluated output.
    #' @param manifest Research manifest that owns Deputy execution identity.
    #' @param on_start Callback accepting one pending Deputy run record.
    #' @param on_run Callback accepting one terminal Deputy run trace.
    initialize = function(
      experts,
      runtime,
      config,
      retriever,
      allowed_connection_ref_ids = list(),
      extractor = NULL,
      extract_claims_program = NULL,
      workspace = NULL,
      progress = NULL,
      run_id = NULL,
      stage_recorder = NULL,
      manifest = NULL,
      on_start = function(pending_run) invisible(pending_run),
      on_run = function(trace) invisible(trace)
    ) {
      experts <- tryCatch(
        tempest_validate_experts(experts, active_only = FALSE),
        error = function(error) {
          tempest_expert_session_abort(
            "{.arg experts} must contain validated expert profiles.",
            parent = error
          )
        }
      )
      if (!inherits(runtime, "TempestRuntime")) {
        tempest_expert_session_abort(
          "{.arg runtime} must be created by {.fn tempest_runtime}."
        )
      }
      if (!S7::S7_inherits(config, TempestConfig)) {
        tempest_expert_session_abort(
          "{.arg config} must be created by {.fn tempest_config}."
        )
      }
      if (!inherits(retriever, "TempestRetriever")) {
        tempest_expert_session_abort(
          "{.arg retriever} must be a {.cls TempestRetriever}."
        )
      }
      self$sessions <- new.env(parent = emptyenv())
      self$session_profiles <- new.env(parent = emptyenv())
      self$session_provenance <- new.env(parent = emptyenv())
      self$experts <- new.env(parent = emptyenv())
      self$expert_connection_ref_ids <- new.env(parent = emptyenv())
      grants <- tempest_expert_connection_grants(
        experts,
        allowed_connection_ref_ids,
        runtime
      )
      for (expert in experts) {
        expert_id <- expert@expert_id
        assign(expert_id, expert, envir = self$experts)
        assign(
          expert_id,
          grants[[expert_id]],
          envir = self$expert_connection_ref_ids
        )
      }
      self$runtime <- runtime
      self$config <- config
      self$retriever <- retriever
      self$extractor <- extractor
      if (!is.null(extractor) || !is.null(extract_claims_program)) {
        extract_claims_program <- tempest_dsprrr_execution_require(
          extract_claims_program,
          "fact extraction"
        )
      }
      self$extract_claims_program <- extract_claims_program
      workspace <- workspace %||% retriever$workspace
      if (!inherits(workspace, "ResearchWorkspace")) {
        tempest_expert_session_abort(
          "{.arg workspace} must be a ResearchWorkspace or `NULL`."
        )
      }
      self$workspace <- workspace
      self$progress <- tempest_progress_callback(progress)
      self$run_id <- run_id %||%
        if (S7::S7_inherits(manifest, TempestResearchManifest)) {
          manifest@research_run_id
        } else {
          tempest_uuid("session")
        }
      if (!is.null(stage_recorder) && !is.function(stage_recorder)) {
        tempest_expert_session_abort(
          "{.arg stage_recorder} must be a function or {.code NULL}."
        )
      }
      private$stage_recorder_value <- stage_recorder
      private$manifest_value <- if (is.null(manifest)) {
        tempest_research_manifest(
          research_run_id = self$run_id,
          mode = "costorm",
          config = self$config,
          knowledge_snapshot = tempest_costorm_manifest_snapshot_reference(
            self$workspace
          ),
          runtime = list(),
          traces = list(),
          deliverables = list(),
          status = "running"
        )
      } else {
        tempest_costorm_manifest_validate(
          manifest,
          self$run_id,
          self$config,
          self$workspace
        )
      }
      if (!is.function(on_start) || !is.function(on_run)) {
        tempest_expert_session_abort(
          "{.arg on_start} and {.arg on_run} must be functions."
        )
      }
      private$on_start_value <- on_start
      private$on_run_value <- on_run
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
    #' @param session_id Optional manager-owned expert session id. This is
    #'   delegation metadata only; extracted claims use the manager's exact
    #'   research run id.
    #' @param expert_id Optional stable expert id.
    #' @param correlation_id Optional progress correlation id for the turn.
    #' @param deputy_execution Optional terminal Deputy trace for the answer.
    #' @return Invisibly returns NULL.
    extract_facts = function(
      response,
      turn = NULL,
      source_ids = NULL,
      session_id = NA_character_,
      expert_id = NA_character_,
      correlation_id = NA_character_,
      deputy_execution = NULL
    ) {
      if (!is.null(deputy_execution)) {
        deputy_execution <- tempest_costorm_deputy_trace(deputy_execution)
      }
      if (!is.null(self$extractor) && !is.null(self$workspace)) {
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
              tempest_harvest_native_sources_from_turn(turn, self$workspace)
            } else {
              character()
            }
            source_ids <- tempest_session_answer_source_ids(
              list(workspace = self$workspace),
              response,
              unique(c(source_ids, harvested))
            )
            if (length(source_ids) == 0L) {
              self$emit_progress(
                "step",
                "skipped",
                stage = "evidence",
                step = "fact_extraction",
                parent_event_id = event@event_id,
                correlation_id = event@correlation_id,
                payload = list(reason = "no_cited_sources")
              )
              return(invisible(FALSE))
            }
            tempest_extract_facts_from_answer(
              self$extractor,
              response,
              self$workspace,
              module = self$extract_claims_program,
              source_ids = source_ids,
              session_id = self$run_id,
              expert_id = expert_id,
              retrieval_step_id = correlation_id,
              deputy_run_id = deputy_execution$deputy_run_id %||%
                NA_character_,
              deputy_session_id = deputy_execution$deputy_session_id %||%
                NA_character_,
              record_stage = private$stage_recorder_value
            )
            self$emit_progress(
              "step",
              "succeeded",
              stage = "evidence",
              step = "fact_extraction",
              parent_event_id = event@event_id,
              correlation_id = event@correlation_id,
              payload = list(
                claim_count = length(self$workspace$list_proposed_claims())
              )
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
            tempest_rethrow_operation(
              e,
              class = "tempest_expert_session_error"
            )
          }
        )
      }
      invisible(NULL)
    },

    #' @description
    #' Add an active expert profile to the live roster.
    #' @param expert A validated `tempest_expert`.
    #' @param allowed_connection_ref_ids Connection ids granted to this expert.
    #' @param replace Whether to replace an existing profile with the same id.
    #' @return The stable expert id, invisibly.
    add_expert = function(
      expert,
      allowed_connection_ref_ids = character(),
      replace = FALSE
    ) {
      tryCatch(
        tempest_validate_experts(list(expert), active_only = TRUE),
        error = function(error) {
          tempest_expert_session_abort(
            "{.arg expert} must be an active validated expert profile.",
            parent = error
          )
        }
      )
      replace <- tempest_workflow_flag(replace, "replace")
      expert_id <- expert@expert_id
      present <- exists(expert_id, envir = self$experts, inherits = FALSE)
      if (present && !replace) {
        tempest_expert_session_abort(c(
          "Expert {.val {expert_id}} is already in the live roster.",
          i = "Set {.arg replace} to `TRUE` to replace it explicitly."
        ))
      }
      grant <- tempest_expert_connection_grants(
        list(expert),
        stats::setNames(
          list(allowed_connection_ref_ids),
          expert_id
        ),
        self$runtime
      )[[expert_id]]
      if (present) {
        private$retire_expert_sessions(expert_id)
      }
      assign(expert_id, expert, envir = self$experts)
      assign(
        expert_id,
        grant,
        envir = self$expert_connection_ref_ids
      )
      invisible(expert_id)
    },

    #' @description
    #' Retire an expert and all chats bound to that profile.
    #' @param expert_id Stable expert id.
    #' @return Whether the expert was present.
    retire_expert = function(expert_id) {
      expert_id <- private$expert_id(expert_id)
      if (!exists(expert_id, envir = self$experts, inherits = FALSE)) {
        return(FALSE)
      }
      expert <- get(expert_id, envir = self$experts, inherits = FALSE)
      if (!identical(expert@state, "retired")) {
        expert <- tempest_expert_update(expert, state = "retired")
        assign(expert_id, expert, envir = self$experts)
      }
      private$retire_expert_sessions(expert_id)
      TRUE
    },

    #' @description
    #' Look up an expert by exact stable id.
    #' @param expert_id Stable expert id.
    #' @param active_only Whether retired profiles should be rejected.
    #' @return A validated expert profile.
    profile = function(expert_id, active_only = TRUE) {
      expert_id <- private$expert_id(expert_id)
      active_only <- tempest_workflow_flag(active_only, "active_only")
      if (!exists(expert_id, envir = self$experts, inherits = FALSE)) {
        tempest_expert_session_abort(
          "Expert {.val {expert_id}} is not in the live roster."
        )
      }
      expert <- get(expert_id, envir = self$experts, inherits = FALSE)
      if (active_only && !identical(expert@state, "active")) {
        tempest_expert_session_abort(
          "Expert {.val {expert_id}} is retired and cannot run."
        )
      }
      expert
    },

    #' @description
    #' List expert profiles in stable-id order.
    #' @param active_only Whether to omit retired profiles.
    #' @return A list of validated expert profiles.
    list_experts = function(active_only = TRUE) {
      active_only <- tempest_workflow_flag(active_only, "active_only")
      expert_ids <- sort(ls(self$experts, all.names = TRUE))
      experts <- lapply(
        expert_ids,
        \(expert_id) get(expert_id, envir = self$experts, inherits = FALSE)
      )
      if (active_only) {
        experts <- Filter(
          \(expert) identical(expert@state, "active"),
          experts
        )
      }
      unname(experts)
    },

    #' @description
    #' Get an expert's existing session or create a scoped chat.
    #' @param expert_id Stable expert id or matching expert profile.
    #' @param session_id Optional existing, manager-owned session id to resume.
    #' @return Chat, session binding, grants, provenance, and creation status.
    get_or_create = function(expert_id, session_id = NULL) {
      expert <- private$resolve_expert(expert_id)
      if (is.null(session_id)) {
        session_id <- private$session_for_expert(expert@expert_id)
        if (!is.null(session_id)) {
          return(private$resume(expert, session_id))
        }
        return(private$create(expert))
      }
      session_id <- private$session_id(session_id)
      if (!exists(session_id, envir = self$sessions, inherits = FALSE)) {
        tempest_expert_session_abort(c(
          "Expert session {.val {session_id}} is not active.",
          i = "Only manager-owned session ids can be resumed."
        ))
      }
      private$resume(expert, session_id)
    },

    #' @description
    #' Restore a saved session binding through fresh runtime authorization.
    #' @param binding Serializable session profile containing the opaque
    #'   session id and exact expert identity fields.
    #' @return The same result shape as `get_or_create()`.
    restore_session = function(binding) {
      binding <- tryCatch(
        tempest_contract_serializable_list(binding, "binding"),
        error = function(error) {
          tempest_expert_session_abort(
            "{.arg binding} must be a serializable session profile.",
            parent = error
          )
        }
      )
      required_fields <- c(
        "session_id",
        "expert_id",
        "expert_version",
        "expert_fingerprint",
        "model_role",
        "allowed_connection_ref_ids",
        "grants",
        "created_at"
      )
      binding_fields <- names(binding)
      if (
        is.null(binding_fields) ||
          anyNA(binding_fields) ||
          anyDuplicated(binding_fields) ||
          !setequal(binding_fields, required_fields)
      ) {
        tempest_expert_session_abort(
          "Session binding does not match the current eight-field schema."
        )
      }
      session_id <- private$session_id(binding$session_id)
      if (!grepl("^expert-session_[a-f0-9]{16}$", session_id)) {
        tempest_expert_session_abort(
          "Restored session ids must be opaque Tempest expert-session ids."
        )
      }
      if (
        exists(session_id, envir = self$sessions, inherits = FALSE) ||
          exists(
            session_id,
            envir = self$session_profiles,
            inherits = FALSE
          )
      ) {
        tempest_expert_session_abort(
          "Expert session {.val {session_id}} is already active."
        )
      }
      expert <- self$profile(binding$expert_id)
      expert_version <- tryCatch(
        tempest_workflow_version(
          binding$expert_version,
          "expert_version"
        ),
        error = function(error) {
          tempest_expert_session_abort(
            "Session binding has an invalid expert version.",
            parent = error
          )
        }
      )
      fingerprint <- binding$expert_fingerprint
      if (
        !rlang::is_string(fingerprint) ||
          !grepl("^[a-f0-9]{64}$", fingerprint)
      ) {
        tempest_expert_session_abort(
          "Session binding has an invalid expert fingerprint."
        )
      }
      current_fingerprint <- tempest_expert_profile_fingerprint(expert)
      if (
        !identical(expert_version, expert@version) ||
          !identical(fingerprint, current_fingerprint)
      ) {
        tempest_expert_session_abort(c(
          "Expert session {.val {session_id}} cannot be restored.",
          x = paste0(
            "The saved expert version or fingerprint does not match the ",
            "live profile."
          )
        ))
      }
      model_role <- tryCatch(
        tempest_contract_id(binding$model_role, "binding$model_role"),
        error = function(error) {
          tempest_expert_session_abort(
            "Session binding has an invalid model role.",
            parent = error
          )
        }
      )
      allowed_connection_ref_ids <- tryCatch(
        tempest_contract_ids(
          tempest_expert_session_connection_ids(
            binding$allowed_connection_ref_ids,
            "allowed_connection_ref_ids"
          ),
          "binding$allowed_connection_ref_ids"
        ),
        error = function(error) {
          tempest_expert_session_abort(
            "Session binding has invalid allowed connection ids.",
            parent = error
          )
        }
      )
      created_at <- binding$created_at
      if (
        !rlang::is_string(created_at) ||
          is.na(created_at) ||
          !nzchar(tempest_trim(created_at))
      ) {
        tempest_expert_session_abort(
          "Session binding has an invalid creation timestamp."
        )
      }
      parsed_created_at <- suppressWarnings(as.POSIXct(created_at, tz = "UTC"))
      if (is.na(parsed_created_at)) {
        tempest_expert_session_abort(
          "Session binding has an invalid creation timestamp."
        )
      }
      prior_grants <- tempest_expert_session_grants(binding$grants)
      private$create(
        expert,
        session_id = session_id,
        prior_grants = prior_grants
      )
      restored <- self$session_profile(session_id)
      if (
        !identical(restored$model_role, model_role) ||
          !identical(
            unname(restored$allowed_connection_ref_ids),
            unname(allowed_connection_ref_ids)
          )
      ) {
        self$retire_session(session_id)
        tempest_expert_session_abort(
          paste0(
            "Expert session {.val {session_id}} cannot be restored because ",
            "its live authorization differs from the saved binding."
          )
        )
      }
      restored$created_at <- created_at
      assign(session_id, restored, envir = self$session_profiles)
      private$result(session_id, is_new = TRUE)
    },

    #' @description
    #' Return the serializable binding for an active session.
    #' @param session_id Manager-owned expert session id.
    #' @return Session binding including expert fingerprint and grants.
    session_profile = function(session_id) {
      session_id <- private$session_id(session_id)
      if (
        !exists(
          session_id,
          envir = self$session_profiles,
          inherits = FALSE
        )
      ) {
        tempest_expert_session_abort(
          "Expert session {.val {session_id}} is not active."
        )
      }
      get(
        session_id,
        envir = self$session_profiles,
        inherits = FALSE
      )
    },

    #' @description
    #' List all active session IDs.
    #' @return Character vector of session IDs.
    list_sessions = function() {
      sort(ls(envir = self$sessions, all.names = TRUE))
    },

    #' @description
    #' Retire a stateful expert chat so it cannot be reused after timeout or
    #' cancellation.
    #' @param session_id Session id returned by `get_or_create()`.
    #' @return A list describing whether the session existed and whether a
    #'   provider cancellation method was available.
    retire_session = function(session_id) {
      if (is.null(session_id)) {
        return(list(retired = FALSE, cancellation_supported = FALSE))
      }
      session_id <- private$session_id(session_id)
      if (!exists(session_id, envir = self$sessions, inherits = FALSE)) {
        return(list(retired = FALSE, cancellation_supported = FALSE))
      }
      chat <- get(session_id, envir = self$sessions, inherits = FALSE)
      cancel <- tryCatch(
        chat$cancel %||% chat$stop %||% NULL,
        error = \(error) NULL
      )
      cancellation_supported <- is.function(cancel)
      if (cancellation_supported) {
        try(cancel(), silent = TRUE)
      }
      rm(list = session_id, envir = self$sessions)
      for (records in list(
        self$session_profiles,
        self$session_provenance
      )) {
        if (exists(session_id, envir = records, inherits = FALSE)) {
          rm(list = session_id, envir = records)
        }
      }
      list(
        retired = TRUE,
        cancellation_supported = cancellation_supported
      )
    }
  ),
  private = list(
    stage_recorder_value = NULL,
    manifest_value = NULL,
    on_start_value = NULL,
    on_run_value = NULL,
    expert_id = function(expert_id) {
      tryCatch(
        tempest_contract_id(expert_id, "expert_id"),
        error = function(error) {
          tempest_expert_session_abort(
            "{.arg expert_id} must be a valid stable expert id.",
            parent = error
          )
        }
      )
    },

    session_id = function(session_id) {
      tryCatch(
        tempest_workflow_scalar(session_id, "session_id"),
        error = function(error) {
          tempest_expert_session_abort(
            "{.arg session_id} must be a manager-owned session id.",
            parent = error
          )
        }
      )
    },

    resolve_expert = function(expert_or_id) {
      if (S7::S7_inherits(expert_or_id, TempestExpertProfile)) {
        tryCatch(
          tempest_validate_experts(list(expert_or_id), active_only = TRUE),
          error = function(error) {
            tempest_expert_session_abort(
              "{.arg expert_id} identifies an invalid expert profile.",
              parent = error
            )
          }
        )
        current <- self$profile(expert_or_id@expert_id)
        matches <- identical(current@version, expert_or_id@version) &&
          identical(
            tempest_expert_profile_fingerprint(current),
            tempest_expert_profile_fingerprint(expert_or_id)
          )
        if (!matches) {
          tempest_expert_session_abort(
            paste0(
              "Expert {.val {expert_or_id@expert_id}} does not match the ",
              "profile in the live roster."
            )
          )
        }
        return(current)
      }
      self$profile(expert_or_id)
    },

    session_for_expert = function(expert_id) {
      session_ids <- sort(ls(self$session_profiles, all.names = TRUE))
      for (session_id in session_ids) {
        binding <- get(
          session_id,
          envir = self$session_profiles,
          inherits = FALSE
        )
        if (identical(binding$expert_id, expert_id)) {
          return(session_id)
        }
      }
      NULL
    },

    result = function(session_id, is_new) {
      list(
        chat = get(session_id, envir = self$sessions, inherits = FALSE),
        session_id = session_id,
        is_new = is_new,
        provenance = get(
          session_id,
          envir = self$session_provenance,
          inherits = FALSE
        ),
        profile = get(
          session_id,
          envir = self$session_profiles,
          inherits = FALSE
        ),
        grants = get(
          session_id,
          envir = self$session_profiles,
          inherits = FALSE
        )$grants
      )
    },

    create = function(
      expert,
      session_id = NULL,
      prior_grants = list()
    ) {
      supplied_session_id <- !is.null(session_id)
      session_id <- session_id %||% tempest_uuid("expert-session")
      while (
        exists(session_id, envir = self$sessions, inherits = FALSE) ||
          exists(
            session_id,
            envir = self$session_profiles,
            inherits = FALSE
          )
      ) {
        if (supplied_session_id) {
          tempest_expert_session_abort(
            "Expert session {.val {session_id}} is already active."
          )
        }
        session_id <- tempest_uuid("expert-session")
      }
      provenance <- new.env(parent = emptyenv())
      provenance$base <- list(
        session_id = self$run_id,
        expert_id = expert@expert_id
      )
      provenance$current <- list()
      model_role <- expert@model_role
      if (is.na(model_role)) {
        tempest_expert_session_abort(
          paste0(
            "Expert {.val {expert@expert_id}} requires a host model-policy ",
            "resolver before chat creation."
          )
        )
      }
      model <- tempest_runtime_model(self$config, model_role)
      context <- list(
        retriever = self$retriever,
        expert = expert,
        expert_id = expert@expert_id,
        model = model,
        search_provider = self$config@search_provider,
        claim_provenance = function() {
          utils::modifyList(
            provenance$base,
            provenance$current %||% list()
          )
        }
      )
      allowed_connection_ref_ids <- get(
        expert@expert_id,
        envir = self$expert_connection_ref_ids,
        inherits = FALSE
      )
      resolution <- tryCatch(
        self$runtime$resolve_expert(
          expert,
          allowed_connection_ref_ids = allowed_connection_ref_ids,
          context = context
        ),
        error = function(error) {
          tempest_expert_session_abort(
            "Expert {.val {expert@expert_id}} could not be resolved."
          )
        }
      )
      system_prompt <- tempest_expert_system_prompt(expert, resolution)
      chat <- tempest_make_chat(
        self$config,
        model_role,
        system_prompt = system_prompt,
        echo = "none"
      )
      tryCatch(
        self$runtime$attach(chat, resolution, context = context),
        error = function(error) {
          tempest_expert_session_abort(
            "Expert {.val {expert@expert_id}} tools could not be attached."
          )
        }
      )
      chat <- tryCatch(
        tempest_deputy_chat_adapter(
          chat,
          manifest = private$manifest_value,
          deputy_session_id = session_id,
          agent_name = expert@name,
          stage = "dialogue",
          role = "expert",
          expert_id = expert@expert_id,
          on_start = private$on_start_value,
          on_run = private$on_run_value
        ),
        error = function(error) {
          tempest_expert_session_abort(
            "Expert {.val {expert@expert_id}} execution session could not be created."
          )
        }
      )
      binding <- list(
        session_id = session_id,
        expert_id = expert@expert_id,
        expert_version = expert@version,
        expert_fingerprint = resolution$expert_fingerprint,
        model_role = model_role,
        allowed_connection_ref_ids = allowed_connection_ref_ids,
        grants = resolution$grants,
        prior_grants = prior_grants,
        created_at = tempest_now_utc()
      )
      assign(session_id, chat, envir = self$sessions)
      assign(
        session_id,
        binding,
        envir = self$session_profiles
      )
      assign(
        session_id,
        provenance,
        envir = self$session_provenance
      )
      private$result(session_id, is_new = TRUE)
    },

    resume = function(expert, session_id) {
      binding <- self$session_profile(session_id)
      current_fingerprint <- tempest_expert_profile_fingerprint(expert)
      if (
        !identical(binding$expert_id, expert@expert_id) ||
          !identical(binding$expert_version, expert@version) ||
          !identical(binding$expert_fingerprint, current_fingerprint)
      ) {
        tempest_expert_session_abort(c(
          "Expert session {.val {session_id}} cannot be resumed.",
          x = paste0(
            "The session is bound to a different expert id, version, ",
            "or profile fingerprint."
          )
        ))
      }
      private$result(session_id, is_new = FALSE)
    },

    retire_expert_sessions = function(expert_id) {
      session_ids <- ls(self$session_profiles, all.names = TRUE)
      for (session_id in session_ids) {
        binding <- get(
          session_id,
          envir = self$session_profiles,
          inherits = FALSE
        )
        if (identical(binding$expert_id, expert_id)) {
          self$retire_session(session_id)
        }
      }
      invisible(NULL)
    }
  )
)

#' Create a scoped Tempest expert-session manager
#'
#' `r lifecycle::badge("experimental")`
#'
#' The manager owns a validated live expert roster and creates one
#' capability-scoped chat per expert. Runtime tools and authenticated
#' connections are resolved before chat creation and are never inferred from
#' display names.
#'
#' @param experts List of [tempest_expert()] profiles.
#' @param runtime A [tempest_runtime()].
#' @param config A [tempest_config()].
#' @param retriever A `TempestRetriever`.
#' @param allowed_connection_ref_ids Named list of allowed connection ids by
#'   stable expert id.
#' @param extractor Optional fact-extraction chat.
#' @param extract_claims_program ProgramSet-bound claim-extraction execution.
#'   Required when `extractor` is supplied.
#' @param workspace Optional [ResearchWorkspace]; defaults to the retriever
#'   workspace.
#' @param progress Optional progress callback.
#' @param run_id Optional shared workflow run id.
#' @param stage_recorder Optional callback accepting a stage record and its
#'   evaluated output.
#' @param manifest Research manifest that owns Deputy execution identity.
#' @param on_run Callback accepting one terminal Deputy run trace.
#' @return An `ExpertSessionManager`.
#' @keywords internal
tempest_expert_session_manager <- function(
  experts,
  runtime,
  config,
  retriever,
  allowed_connection_ref_ids = list(),
  extractor = NULL,
  extract_claims_program = NULL,
  workspace = NULL,
  progress = NULL,
  run_id = NULL,
  stage_recorder = NULL,
  manifest = NULL,
  on_run = function(trace) invisible(trace)
) {
  ExpertSessionManager$new(
    experts = experts,
    runtime = runtime,
    config = config,
    retriever = retriever,
    allowed_connection_ref_ids = allowed_connection_ref_ids,
    extractor = extractor,
    extract_claims_program = extract_claims_program,
    workspace = workspace,
    progress = progress,
    run_id = run_id,
    stage_recorder = stage_recorder,
    manifest = manifest,
    on_run = on_run
  )
}

#' Create the expert delegation tool
#'
#' Creates one generic tool that resolves the manager's live roster by exact
#' stable expert id.
#'
#' @param session_manager An `ExpertSessionManager` instance.
#' @param topic The research topic (for context).
#' @param experts Optional selected expert profiles. These are validated for
#'   runtime composition, while calls resolve the manager's live roster.
#' @return An ellmer tool.
#' @keywords internal
tempest_create_expert_delegation_tool <- function(
  session_manager,
  topic,
  experts = NULL
) {
  tempest_require("ellmer", "Expert tools require ellmer.")
  if (!inherits(session_manager, "ExpertSessionManager")) {
    tempest_expert_session_abort(
      "{.arg session_manager} must be an {.cls ExpertSessionManager}."
    )
  }
  topic <- tryCatch(
    tempest_workflow_scalar(topic, "topic"),
    error = function(error) {
      tempest_expert_session_abort(
        "{.arg topic} must be a single non-empty string.",
        parent = error
      )
    }
  )
  if (!is.null(experts)) {
    tryCatch(
      tempest_validate_experts(experts, active_only = FALSE),
      error = function(error) {
        tempest_expert_session_abort(
          "{.arg experts} must contain validated expert profiles.",
          parent = error
        )
      }
    )
  }
  mgr <- session_manager
  roster <- mgr$list_experts()
  roster_text <- paste(
    vapply(
      roster,
      function(expert) {
        paste0(
          expert@expert_id,
          " (",
          expert@name,
          ", ",
          expert@title,
          ")"
        )
      },
      character(1)
    ),
    collapse = "; "
  )
  source_ids_in_store <- function() {
    if (!inherits(mgr$workspace, "ResearchWorkspace")) {
      return(character())
    }
    vapply(
      mgr$workspace$list_retrieved_sources(),
      \(source) source$id,
      character(1)
    )
  }
  claim_ids_in_store <- function() {
    if (!inherits(mgr$workspace, "ResearchWorkspace")) {
      return(character())
    }
    vapply(
      mgr$workspace$list_proposed_claims(),
      \(claim) claim@claim_id,
      character(1)
    )
  }

  delegate_to_expert <- function(expert_id, question) {
    expert <- mgr$profile(expert_id)
    result <- mgr$get_or_create(expert@expert_id)
    chat <- result$chat
    sid <- result$session_id
    provenance <- result$provenance
    expert_name <- expert@name
    correlation_id <- tempest_uuid("tool")
    prior_source_ids <- source_ids_in_store()
    prior_claim_ids <- claim_ids_in_store()
    tool_event <- mgr$emit_progress(
      "tool",
      "started",
      stage = "dialogue",
      step = "delegate_to_expert",
      correlation_id = correlation_id,
      payload = list(
        expert_id = expert@expert_id,
        expert_name = expert_name,
        session_id = sid
      )
    )

    prompt <- paste0(
      "Topic: ",
      topic,
      "\n\n",
      "Question: ",
      question,
      "\n\n",
      "Instructions:\n",
      "- Follow your expert profile and assigned skill instructions.\n",
      "- Use only the capabilities and connections granted to this session.\n",
      "- Start with evidence already in the shared session by using ",
      paste0(
        "list_retrieved_sources, get_retrieved_source, or retrieve when ",
        "available.\n"
      ),
      "- If shared evidence cannot answer the question, make exactly one web ",
      "search and set k = 2 when the search tool accepts k.\n",
      "- Inspect no more than two search results and make no more than two ",
      "retrieval or fetch calls in total.\n",
      "- Stop when those bounds are reached. Do not expand into an exhaustive ",
      "survey; state the remaining evidence gap instead.\n",
      "- Only state factual claims supported by sources you inspected.\n",
      "- Cite source IDs like [Sxxxxxxxxxxxx] when evidence is available.\n",
      "- Do not call add_proposed_claim; the host commits evidence after ",
      "your response.\n",
      "- If evidence is weak or unclear, say so.\n",
      "- Respond in no more than 250 words.\n\n",
      "Respond now:"
    )

    old_provenance <- provenance$current %||% list()
    provenance$current <- list(
      expert_id = expert@expert_id,
      retrieval_step_id = correlation_id
    )
    response <- tryCatch(
      chat$chat(
        prompt,
        echo = "none",
        run_context = list(
          correlation_id = correlation_id,
          role = "expert",
          stage = "dialogue"
        )
      ),
      error = function(e) {
        mgr$emit_progress(
          "tool",
          "failed",
          stage = "dialogue",
          step = "delegate_to_expert",
          parent_event_id = tool_event@event_id,
          correlation_id = correlation_id,
          payload = c(
            list(
              expert_id = expert@expert_id,
              expert_name = expert_name,
              session_id = sid
            ),
            tempest_progress_error_payload(e)
          )
        )
        tempest_rethrow_operation(
          e,
          class = "tempest_expert_session_error"
        )
      },
      finally = {
        provenance$current <- old_provenance
      }
    )
    deputy_execution <- tempest_costorm_last_deputy_execution(
      chat,
      stage = "dialogue",
      role = "expert"
    )

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

    native_source_ids <- if (inherits(mgr$workspace, "ResearchWorkspace")) {
      tempest_harvest_native_sources_from_turn(last_turn, mgr$workspace)
    } else {
      character()
    }
    mgr$extract_facts(
      response_text,
      turn = last_turn,
      source_ids = native_source_ids,
      session_id = sid,
      expert_id = expert@expert_id,
      correlation_id = correlation_id,
      deputy_execution = deputy_execution
    )
    current_source_ids <- source_ids_in_store()
    cited_source_ids <- intersect(
      tempest_extract_citation_ids(response_text),
      current_source_ids
    )
    evidence_source_ids <- unique(c(
      native_source_ids,
      cited_source_ids,
      setdiff(current_source_ids, prior_source_ids)
    ))
    evidence_claim_ids <- setdiff(claim_ids_in_store(), prior_claim_ids)
    mgr$emit_progress(
      "tool",
      "succeeded",
      stage = "dialogue",
      step = "delegate_to_expert",
      parent_event_id = tool_event@event_id,
      correlation_id = correlation_id,
      payload = list(
        expert_id = expert@expert_id,
        expert_name = expert_name,
        session_id = sid,
        deputy_run_id = deputy_execution$deputy_run_id,
        deputy_session_id = deputy_execution$deputy_session_id
      )
    )

    list(
      expert_id = expert@expert_id,
      expert = expert_name,
      response = response_text,
      session_id = sid,
      deputy_run_id = deputy_execution$deputy_run_id,
      deputy_session_id = deputy_execution$deputy_session_id,
      source_ids = evidence_source_ids,
      claim_ids = evidence_claim_ids
    )
  }

  ellmer::tool(
    delegate_to_expert,
    name = "delegate_to_expert",
    description = paste(
      "Delegate a question to one active expert from the live roster.",
      "Use the expert's exact stable expert_id.",
      "Active experts:",
      roster_text
    ),
    arguments = list(
      expert_id = ellmer::type_string(
        "Exact stable id of an active expert in the live roster."
      ),
      question = ellmer::type_string(
        paste(
          "One narrow, answerable evidence question.",
          "Do not request an exhaustive survey or multiple deliverables."
        )
      )
    )
  )
}

#' Generate a single expert profile for a specific area
#'
#' @param topic The research topic.
#' @param area The area of expertise needed.
#' @param existing_experts Existing expert profiles to avoid duplicating.
#' @param config A `TempestConfig` object.
#' @param module ProgramSet-bound persona execution.
#' @param record_stage Product-owned stage-record callback.
#' @return A validated `tempest_expert` profile.
#' @keywords internal
tempest_generate_single_expert <- function(
  topic,
  area,
  existing_experts,
  config,
  module,
  record_stage = function(record, output = NULL) invisible(record)
) {
  topic <- tempest_workflow_scalar(topic, "topic")
  area <- tempest_workflow_scalar(area, "area")
  existing_experts <- tempest_validate_experts(
    existing_experts,
    "existing_experts",
    active_only = FALSE
  )

  existing_desc <- if (length(existing_experts) > 0) {
    paste(
      purrr::map_chr(existing_experts, function(expert) {
        paste0(
          "- ",
          expert@name,
          " [",
          expert@expert_id,
          "]",
          " (",
          expert@title,
          "): ",
          expert@description
        )
      }),
      collapse = "\n"
    )
  } else {
    "(none)"
  }
  requirements <- paste0(
    "Needed expertise: ",
    area,
    "\n\nExisting experts (do not duplicate):\n",
    existing_desc,
    paste0(
      "\n\nGenerate exactly one complementary expert persona for this ",
      "knowledge gap."
    )
  )
  expert <- tempest_generate_experts_with_program(
    topic = topic,
    n = 1L,
    config = config,
    verbose = FALSE,
    module = module,
    requirements = requirements,
    record_stage = record_stage
  )[[1]]
  tempest_expert_update(
    expert,
    expert_id = tempest_generated_expert_id(list(
      kind = "dynamic",
      topic = topic,
      area = area
    )),
    metadata = utils::modifyList(
      expert@metadata,
      list(generated_for = list(topic = topic, area = area))
    )
  )
}
