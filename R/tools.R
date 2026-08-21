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
tempest_native_resource_from_url <- function(
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
  title <- tempest_native_scalar(title, url)
  snippet <- tempest_native_scalar(snippet)
  content_text <- tempest_native_scalar(content_text)
  context_text <- tempest_native_scalar(context_text)
  content <- tempest_native_scalar(context_text, content_text, snippet)
  if (is.na(content)) {
    content <- NULL
  }
  metadata <- list(kind = kind, provider_tool = "native")
  for (field in c("snippet", "content_text", "context_text")) {
    value <- get(field, inherits = FALSE)
    if (!is.na(value) && nzchar(value)) {
      metadata[[field]] <- value
    }
  }
  tempest_resource(
    resource_kind = "web",
    locator = url,
    title = title,
    media_type = "text/html",
    resource_id = tempest_source_id(url),
    content = content,
    retrieved_at = tempest_now_utc(),
    metadata = metadata
  )
}

#' @keywords internal
tempest_merge_resource_record <- function(old, new) {
  if (!tempest_is_exact_resource(new)) {
    tempest_research_workspace_abort(
      "Native evidence merging requires typed evidence resources."
    )
  }
  new_data <- tempest_resource_data(new)
  if (is.null(old)) {
    return(new)
  }
  if (!tempest_is_exact_resource(old)) {
    tempest_research_workspace_abort(
      "Native evidence merging requires typed evidence resources."
    )
  }
  old_data <- tempest_resource_data(old)
  if (
    !identical(old_data$resource_id, new_data$resource_id) ||
      !identical(old_data$resource_kind, new_data$resource_kind) ||
      !identical(old_data$locator, new_data$locator)
  ) {
    tempest_research_workspace_abort(
      "Native evidence merging requires identical resource identity."
    )
  }
  metadata <- utils::modifyList(old_data$metadata, new_data$metadata)
  content <- NULL
  for (field in c("context_text", "content_text", "snippet")) {
    value <- metadata[[field]] %||% NA_character_
    if (rlang::is_string(value) && !is.na(value) && nzchar(value)) {
      content <- value
      break
    }
  }
  if (is.null(content)) {
    content <- new_data$content %||% old_data$content
  }
  content_hash <- if (is.null(content)) {
    NULL
  } else {
    tempest_product_content_hash(content, new_data$media_type)
  }
  title <- if (
    identical(new_data$title, new_data$locator) &&
      !identical(old_data$title, old_data$locator)
  ) {
    old_data$title
  } else {
    new_data$title
  }
  tempest_resource(
    resource_kind = new_data$resource_kind,
    locator = new_data$locator,
    title = title,
    media_type = new_data$media_type,
    resource_id = new_data$resource_id,
    content = content,
    storage_ref = new_data$storage_ref,
    origin_connection_id = new_data$origin_connection_id,
    scope_metadata = new_data$scope_metadata,
    content_hash = content_hash,
    retrieved_at = new_data$retrieved_at,
    redaction = new_data$redaction,
    retention = new_data$retention,
    metadata = metadata
  )
}

#' @keywords internal
tempest_upsert_native_resource <- function(store, resource) {
  if (is.null(resource)) {
    return(NA_character_)
  }
  resource <- tempest_merge_resource_record(
    store$get_retrieved_resource(resource@resource_id),
    resource
  )
  store$upsert_retrieved_resource(resource)
  resource@resource_id
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
      tempest_upsert_native_resource(
        store,
        tempest_native_resource_from_url(
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
        tempest_upsert_native_resource(
          store,
          tempest_native_resource_from_url(url, kind = "native_search")
        )
      )
    }
  } else if (inherits(content, "ellmer::ContentToolResponseFetch")) {
    json <- content@json %||% list()
    ids <- c(
      ids,
      tempest_upsert_native_resource(
        store,
        tempest_native_resource_from_url(
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
    resource <- retriever$fetch(url)
    tempest_tool_source_payload(tempest_resource_as_source(resource))
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
  topic <- tempest_product_scalar(topic, "topic")
  area <- tempest_product_scalar(area, "area")
  existing_experts <- tempest_validate_experts(
    existing_experts,
    "existing_experts"
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
  tempest_generate_experts_with_program(
    topic = topic,
    n = 1L,
    config = config,
    verbose = FALSE,
    module = module,
    requirements = requirements,
    record_stage = record_stage
  )[[1]]
}
