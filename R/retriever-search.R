# Search provider adapters, cache keys, and ToC extraction

#' @keywords internal
tempest_wikipedia_api <- function(params) {
  base <- "https://en.wikipedia.org/w/api.php"
  req <- httr2::request(base) |>
    httr2::req_url_query(!!!params) |>
    httr2::req_user_agent("tempest (R; Wikipedia API)")
  resp <- tempest_search_perform(req)
  httr2::resp_body_json(resp, simplifyVector = TRUE)
}

#' @keywords internal
tempest_wiki_search <- function(query, limit = 8) {
  res <- tempest_wikipedia_api(list(
    action = "query",
    list = "search",
    srsearch = query,
    srlimit = limit,
    format = "json",
    utf8 = 1
  ))
  hits <- res$query$search
  if (is.null(hits) || length(hits) == 0) {
    return(tibble::tibble(
      title = character(),
      url = character(),
      snippet = character()
    ))
  }
  tibble::tibble(
    title = hits$title,
    url = paste0("https://en.wikipedia.org/wiki/", gsub(" ", "_", hits$title)),
    snippet = gsub("<[^>]+>", "", hits$snippet)
  )
}

#' @keywords internal
tempest_empty_search_results <- function() {
  tibble::tibble(
    title = character(),
    url = character(),
    snippet = character()
  )
}

#' @keywords internal
tempest_search_results <- function(title = NULL, url = NULL, snippet = NULL) {
  title <- as.character(title %||% character())
  url <- as.character(url %||% character())
  snippet <- as.character(snippet %||% character())

  n <- max(length(title), length(url), length(snippet), 0L)
  if (n == 0L) {
    return(tempest_empty_search_results())
  }

  pad <- function(x) {
    if (length(x) == n) {
      return(x)
    }
    c(x, rep(NA_character_, n - length(x)))
  }

  tibble::tibble(
    title = pad(title),
    url = pad(url),
    snippet = pad(snippet)
  )
}

#' @keywords internal
tempest_required_env <- function(var, provider) {
  value <- Sys.getenv(var, unset = "")
  if (identical(value, "")) {
    tempest_abort(
      c(
        "The {provider} search provider requires the {.envvar {var}} environment variable.",
        i = "Set it with: {.code Sys.setenv({var} = \"your-key\")}",
        i = "Or use a different provider: {.code tempest_config(search_provider = \"wikipedia\")}"
      ),
      class = c(
        "tempest_missing_envvar_error",
        "tempest_retriever_error",
        "tempest_error"
      ),
      envvar = var,
      provider = provider
    )
  }
  value
}

#' @keywords internal
tempest_first_field <- function(x, fields, default = NA_character_) {
  for (field in fields) {
    value <- x[[field]]
    if (!is.null(value) && length(value) > 0L && !is.na(value[[1]])) {
      return(as.character(value[[1]]))
    }
  }
  default
}

#' @keywords internal
tempest_duckduckgo_result_url <- function(url) {
  url <- as.character(url %||% NA_character_)
  if (is.na(url) || identical(url, "")) {
    return(NA_character_)
  }
  if (!grepl("[?&]uddg=", url)) {
    return(url)
  }
  parsed <- utils::URLdecode(sub("^.*[?&]uddg=([^&]+).*$", "\\1", url))
  if (identical(parsed, url)) {
    return(url)
  }
  parsed
}

#' @keywords internal
tempest_search_cache_key <- function(provider, query, k) {
  tempest_cache_key(
    "search-v2",
    provider,
    tempest_trim(query),
    as.integer(k),
    tempest_search_cache_options(provider)
  )
}

#' @keywords internal
tempest_search_cache_options <- function(provider) {
  switch(
    provider,
    bing = list(
      market = Sys.getenv("BING_SEARCH_MKT", unset = "en-US"),
      language = Sys.getenv("BING_SEARCH_LANGUAGE", unset = "en")
    ),
    duckduckgo = list(
      region = Sys.getenv("DUCKDUCKGO_REGION", unset = "us-en"),
      safe_search = Sys.getenv("DUCKDUCKGO_SAFE_SEARCH", unset = "1")
    ),
    searxng = list(
      api_url = Sys.getenv("SEARXNG_API_URL", unset = "")
    ),
    google = list(
      cse_id = Sys.getenv("GOOGLE_CSE_ID", unset = "")
    ),
    azure_ai_search = list(
      endpoint = Sys.getenv("AZURE_AI_SEARCH_ENDPOINT", unset = ""),
      index = Sys.getenv("AZURE_AI_SEARCH_INDEX_NAME", unset = "")
    ),
    list()
  )
}

#' @keywords internal
tempest_fetch_cache_key <- function(url, user_agent = NULL) {
  tempest_cache_key("fetch-v2", url, user_agent %||% "")
}

tempest_retriever_config_digest <- function(retriever) {
  # Custom retrievers have no Tempest-owned configuration identity. Their
  # workspace contract remains the only package-level seam.
  if (!inherits(retriever, "TempestRetriever")) {
    return(NULL)
  }
  tempest_research_config_digest(retriever$config)
}

tempest_retriever_compatible <- function(retriever) {
  if (inherits(retriever, "TempestRetriever")) {
    return(TRUE)
  }
  if (!is.list(retriever) && !is.environment(retriever)) {
    return(FALSE)
  }
  inherits(retriever[["workspace"]], "ResearchWorkspace") &&
    is.function(retriever[["search"]]) &&
    is.function(retriever[["fetch"]])
}

tempest_retriever_bind_workspace_budget <- function(
  retriever,
  workspace,
  config
) {
  if (
    !inherits(retriever, "TempestRetriever") &&
      !isTRUE(workspace$max_sources == config@max_sources)
  ) {
    workspace$set_max_sources(config@max_sources)
  }
  invisible(workspace)
}

tempest_host_search_results <- function(results, k) {
  invalid <- function() {
    tempest_abort(
      paste0(
        "A host retriever's search() must return a data frame with ",
        "non-empty character title and HTTP/HTTPS url columns."
      ),
      class = c("tempest_retriever_search_error", "tempest_retriever_error")
    )
  }
  if (
    !is.data.frame(results) ||
      !all(c("title", "url") %in% names(results)) ||
      !is.character(results$title) ||
      !is.character(results$url) ||
      anyNA(results$title) ||
      anyNA(results$url) ||
      any(!nzchar(tempest_trim(results$title))) ||
      any(!nzchar(tempest_trim(results$url))) ||
      nrow(results) > k
  ) {
    invalid()
  }
  if (!"snippet" %in% names(results)) {
    results$snippet <- rep(NA_character_, nrow(results))
  }
  if (!is.character(results$snippet)) {
    invalid()
  }
  results$title <- tempest_trim(results$title)
  results$url <- tempest_trim(results$url)
  source_ids <- tryCatch(
    vapply(results$url, tempest_source_id, character(1)),
    error = function(error) invalid()
  )
  if (
    "source_id" %in%
      names(results) &&
      !identical(results$source_id, unname(source_ids))
  ) {
    tempest_abort(
      "Host search() source IDs must match their URLs.",
      class = c("tempest_retriever_search_error", "tempest_retriever_error")
    )
  }
  results$source_id <- unname(source_ids)
  tibble::as_tibble(results)
}

tempest_retriever_search <- function(retriever, query, k) {
  results <- retriever$search(query = query, k = k)
  if (inherits(retriever, "TempestRetriever")) {
    return(results)
  }
  tempest_host_search_results(results, k)
}

# --- Provider-specific search helpers ----------------------------------------
# API keys are retrieved from environment variables inside functions,
# never as parameters (security: prevents exposure in stack traces)

#' Perform a search-provider request with a timeout and transient-error retry
#'
#' Search providers are the hottest network path in the pipeline. Without a
#' timeout a hung connection stalls the whole run, and without retries a
#' transient 429/5xx aborts it. This wraps [httr2::req_perform()] with both.
#' @keywords internal
tempest_search_request_policy <- function(req, timeout_s = 30L) {
  req <- httr2::req_timeout(req, timeout_s)
  httr2::req_retry(
    req,
    max_tries = 3L,
    is_transient = function(resp) {
      httr2::resp_status(resp) %in% c(429L, 500L, 502L, 503L, 504L)
    }
  )
}

#' @keywords internal
tempest_search_perform <- function(req, timeout_s = 30L) {
  req <- tempest_search_request_policy(req, timeout_s = timeout_s)
  httr2::req_perform(req)
}

#' @keywords internal
tempest_search_items <- function(items, provider) {
  if (is.null(items) || length(items) == 0L) {
    return(NULL)
  }
  if (!is.list(items) && !is.data.frame(items)) {
    tempest_abort(
      "{provider} returned a malformed search response.",
      class = c(
        "tempest_search_response_error",
        "tempest_retriever_error",
        "tempest_error"
      )
    )
  }
  items
}

#' Search using You.com API
#'
#' @param query Search query string.
#' @param k Maximum number of results.
#' @return Tibble with title, url, snippet columns.
#' @keywords internal
tempest_search_you <- function(query, k = 8L) {
  api_key <- tempest_required_env("YDC_API_KEY", "You.com")

  req <- httr2::request("https://api.ydc-index.io/search") |>
    httr2::req_headers(`X-API-Key` = api_key) |>
    httr2::req_url_query(query = query)

  resp <- tempest_search_perform(req)
  body <- httr2::resp_body_json(resp, simplifyVector = FALSE)
  items <- tempest_search_items(body$hits %||% list(), "You.com")

  if (length(items) == 0L) {
    return(tempest_empty_search_results())
  }

  items <- utils::head(items, k)
  tempest_search_results(
    title = purrr::map_chr(
      items,
      ~ tempest_first_field(.x, c("title", "url"), default = "Untitled")
    ),
    url = purrr::map_chr(items, ~ tempest_first_field(.x, "url")),
    snippet = purrr::map_chr(items, function(.x) {
      snippets <- .x$snippets %||% .x$snippet %||% .x$description %||% ""
      if (is.list(snippets)) {
        snippets <- unlist(snippets, use.names = FALSE)
      }
      paste(as.character(snippets), collapse = "\n")
    })
  )
}

#' Search using Bing Web Search API
#'
#' @param query Search query string.
#' @param k Maximum number of results.
#' @return Tibble with title, url, snippet columns.
#' @keywords internal
tempest_search_bing <- function(query, k = 8L) {
  api_key <- tempest_required_env("BING_SEARCH_API_KEY", "Bing Search")

  req <- httr2::request("https://api.bing.microsoft.com/v7.0/search") |>
    httr2::req_headers(`Ocp-Apim-Subscription-Key` = api_key) |>
    httr2::req_url_query(
      q = query,
      count = k,
      mkt = Sys.getenv("BING_SEARCH_MKT", unset = "en-US"),
      setLang = Sys.getenv("BING_SEARCH_LANGUAGE", unset = "en")
    )

  resp <- tempest_search_perform(req)
  body <- httr2::resp_body_json(resp, simplifyVector = TRUE)
  items <- tempest_search_items(body$webPages$value, "Bing Search")

  if (is.null(items) || length(items) == 0L) {
    return(tempest_empty_search_results())
  }

  tempest_search_results(
    title = items$name %||% character(),
    url = items$url %||% character(),
    snippet = items$snippet %||% character()
  )
}

#' Search using Serper API
#'
#' @param query Search query string.
#' @param k Maximum number of results.
#' @return Tibble with title, url, snippet columns.
#' @keywords internal
tempest_search_serper <- function(query, k = 8L) {
  api_key <- tempest_required_env("SERPER_API_KEY", "Serper")

  req <- httr2::request("https://google.serper.dev/search") |>
    httr2::req_headers(
      `X-API-KEY` = api_key,
      `Content-Type` = "application/json"
    ) |>
    httr2::req_body_json(list(q = query, num = k))

  resp <- tempest_search_perform(req)
  body <- httr2::resp_body_json(resp, simplifyVector = TRUE)
  items <- tempest_search_items(body$organic, "Serper")

  if (is.null(items) || length(items) == 0L) {
    return(tempest_empty_search_results())
  }

  tempest_search_results(
    title = items$title %||% character(),
    url = items$link %||% character(),
    snippet = items$snippet %||% character()
  )
}

#' Search using Brave Search API
#'
#' @param query Search query string.
#' @param k Maximum number of results.
#' @return Tibble with title, url, snippet columns.
#' @keywords internal
tempest_search_brave <- function(query, k = 8L) {
  api_key <- tempest_required_env("BRAVE_API_KEY", "Brave Search")

  req <- httr2::request("https://api.search.brave.com/res/v1/web/search") |>
    httr2::req_headers(
      Accept = "application/json",
      `X-Subscription-Token` = api_key
    ) |>
    httr2::req_url_query(q = query, count = k)

  resp <- tempest_search_perform(req)
  body <- httr2::resp_body_json(resp, simplifyVector = TRUE)
  items <- tempest_search_items(body$web$results, "Brave Search")

  if (is.null(items) || length(items) == 0L) {
    return(tempest_empty_search_results())
  }

  tempest_search_results(
    title = items$title %||% character(),
    url = items$url %||% character(),
    snippet = items$description %||% character()
  )
}

#' Search using DuckDuckGo HTML results
#'
#' @param query Search query string.
#' @param k Maximum number of results.
#' @return Tibble with title, url, snippet columns.
#' @keywords internal
tempest_search_duckduckgo <- function(query, k = 8L) {
  tempest_require("rvest", "DuckDuckGo search requires HTML parsing.")

  req <- httr2::request("https://html.duckduckgo.com/html/") |>
    httr2::req_url_query(
      q = query,
      kl = Sys.getenv("DUCKDUCKGO_REGION", unset = "us-en"),
      kp = Sys.getenv("DUCKDUCKGO_SAFE_SEARCH", unset = "1")
    ) |>
    httr2::req_user_agent("tempest (R; DuckDuckGo HTML search)")

  resp <- tempest_search_perform(req)
  html <- httr2::resp_body_string(resp)
  doc <- xml2::read_html(html)
  nodes <- rvest::html_elements(doc, ".result")

  if (length(nodes) == 0L) {
    return(tempest_empty_search_results())
  }

  nodes <- nodes[seq_len(min(length(nodes), k))]
  title <- rvest::html_text2(rvest::html_element(nodes, ".result__a"))
  url <- rvest::html_attr(rvest::html_element(nodes, ".result__a"), "href")
  snippet <- rvest::html_text2(rvest::html_element(nodes, ".result__snippet"))

  tempest_search_results(
    title = title,
    url = purrr::map_chr(url, tempest_duckduckgo_result_url),
    snippet = snippet
  )
}

#' Search using Tavily API
#'
#' @param query Search query string.
#' @param k Maximum number of results.
#' @return Tibble with title, url, snippet columns.
#' @keywords internal
tempest_search_tavily <- function(query, k = 8L) {
  api_key <- tempest_required_env("TAVILY_API_KEY", "Tavily")

  req <- httr2::request("https://api.tavily.com/search") |>
    httr2::req_headers(`Content-Type` = "application/json") |>
    httr2::req_body_json(list(
      api_key = api_key,
      query = query,
      max_results = k
    ))

  resp <- tempest_search_perform(req)
  body <- httr2::resp_body_json(resp, simplifyVector = TRUE)
  items <- tempest_search_items(body$results, "Tavily")

  if (is.null(items) || length(items) == 0L) {
    return(tempest_empty_search_results())
  }

  tempest_search_results(
    title = items$title %||% character(),
    url = items$url %||% character(),
    snippet = items$content %||% character()
  )
}

#' Search using SearXNG API
#'
#' @param query Search query string.
#' @param k Maximum number of results.
#' @return Tibble with title, url, snippet columns.
#' @keywords internal
tempest_search_searxng <- function(query, k = 8L) {
  api_url <- Sys.getenv("SEARXNG_API_URL", unset = "")
  if (identical(api_url, "")) {
    tempest_abort(c(
      "{.envvar SEARXNG_API_URL} environment variable is not set.",
      i = "Set it to your SearXNG search endpoint, e.g. {.url https://search.example.com/search}",
      i = "Optionally set {.envvar SEARXNG_API_KEY} for bearer-token protected instances."
    ))
  }

  req <- httr2::request(api_url) |>
    httr2::req_url_query(q = query, format = "json")

  api_key <- Sys.getenv("SEARXNG_API_KEY", unset = "")
  if (!identical(api_key, "")) {
    req <- httr2::req_headers(req, Authorization = paste("Bearer", api_key))
  }

  resp <- tempest_search_perform(req)
  body <- httr2::resp_body_json(resp, simplifyVector = TRUE)
  items <- tempest_search_items(body$results, "SearXNG")

  if (is.null(items) || length(items) == 0L) {
    return(tempest_empty_search_results())
  }

  items <- utils::head(items, k)
  tempest_search_results(
    title = items$title %||% character(),
    url = items$url %||% character(),
    snippet = items$content %||% items$snippet %||% character()
  )
}

#' Search using Google Custom Search API
#'
#' @param query Search query string.
#' @param k Maximum number of results.
#' @return Tibble with title, url, snippet columns.
#' @keywords internal
tempest_search_google <- function(query, k = 8L) {
  api_key <- tempest_required_env("GOOGLE_SEARCH_API_KEY", "Google Search")
  cse_id <- tempest_required_env("GOOGLE_CSE_ID", "Google Search")

  req <- httr2::request("https://www.googleapis.com/customsearch/v1") |>
    httr2::req_url_query(
      key = api_key,
      cx = cse_id,
      q = query,
      num = min(k, 10L)
    )

  resp <- tempest_search_perform(req)
  body <- httr2::resp_body_json(resp, simplifyVector = TRUE)
  items <- tempest_search_items(body$items, "Google Search")

  if (is.null(items) || length(items) == 0L) {
    return(tempest_empty_search_results())
  }

  tempest_search_results(
    title = items$title %||% character(),
    url = items$link %||% character(),
    snippet = items$snippet %||% character()
  )
}

#' Search using Azure AI Search
#'
#' @param query Search query string.
#' @param k Maximum number of results.
#' @return Tibble with title, url, snippet columns.
#' @keywords internal
tempest_search_azure_ai_search <- function(query, k = 8L) {
  api_key <- tempest_required_env(
    "AZURE_AI_SEARCH_API_KEY",
    "Azure AI Search"
  )
  endpoint <- Sys.getenv("AZURE_AI_SEARCH_ENDPOINT", unset = "")
  if (identical(endpoint, "")) {
    tempest_abort(c(
      "{.envvar AZURE_AI_SEARCH_ENDPOINT} environment variable is not set.",
      i = "Set it to your Azure AI Search endpoint."
    ))
  }
  index_name <- tempest_required_env(
    "AZURE_AI_SEARCH_INDEX_NAME",
    "Azure AI Search"
  )
  api_version <- Sys.getenv(
    "AZURE_AI_SEARCH_API_VERSION",
    unset = "2023-11-01"
  )

  endpoint <- sub("/+$", "", endpoint)
  url <- paste0(endpoint, "/indexes/", index_name, "/docs/search")
  req <- httr2::request(url) |>
    httr2::req_headers(
      `api-key` = api_key,
      `Content-Type` = "application/json"
    ) |>
    httr2::req_url_query(`api-version` = api_version) |>
    httr2::req_body_json(list(search = query, top = k))

  resp <- tempest_search_perform(req)
  body <- httr2::resp_body_json(resp, simplifyVector = FALSE)
  items <- tempest_search_items(body$value %||% list(), "Azure AI Search")

  if (length(items) == 0L) {
    return(tempest_empty_search_results())
  }

  items <- utils::head(items, k)
  tempest_search_results(
    title = purrr::map_chr(
      items,
      ~ tempest_first_field(.x, c("title", "name", "metadata_storage_name"))
    ),
    url = purrr::map_chr(
      items,
      ~ tempest_first_field(.x, c("url", "metadata_storage_path", "source"))
    ),
    snippet = purrr::map_chr(
      items,
      ~ tempest_first_field(.x, c("chunk", "content", "text", "description"))
    )
  )
}

#' Extract table of contents from a URL
#'
#' Fetches a page and extracts h1-h4 headings as a structured ToC.
#'
#' @param url URL to extract ToC from.
#' @return Character vector of indented headings, or `character()` on error.
#' @keywords internal
tempest_extract_toc_from_url <- function(url) {
  tryCatch(
    {
      url <- tempest_normalize_url(url)
      if (is.na(url)) {
        return(character())
      }
      resp <- tempest_http_get(url, timeout_s = 10)
      html <- httr2::resp_body_string(resp)
      doc <- xml2::read_html(html)
      headings <- xml2::xml_find_all(doc, ".//h1|.//h2|.//h3|.//h4")
      if (length(headings) == 0) {
        return(character())
      }
      levels <- as.integer(sub("h", "", xml2::xml_name(headings)))
      texts <- tempest_trim(xml2::xml_text(headings))
      keep <- nzchar(texts)
      levels <- levels[keep]
      texts <- texts[keep]
      if (length(texts) == 0) {
        return(character())
      }
      min_level <- min(levels)
      indents <- strrep("  ", levels - min_level)
      paste0(indents, "- ", texts)
    },
    error = function(e) {
      tempest_warn("Failed to extract the table of contents from {.url {url}}.")
      character()
    }
  )
}

#' Get Wikipedia page sections via Parse API
#'
#' Uses the Wikipedia Parse API to get structured section headings.
#'
#' @param title Wikipedia page title.
#' @return Character vector of indented section headings, or `character()` on error.
#' @keywords internal
tempest_wiki_page_sections <- function(title) {
  tryCatch(
    {
      res <- tempest_wikipedia_api(list(
        action = "parse",
        page = title,
        prop = "sections",
        format = "json",
        utf8 = 1
      ))
      sections <- res$parse$sections
      if (is.null(sections) || length(sections) == 0) {
        return(character())
      }
      # sections is a data.frame with toclevel, line, number columns
      levels <- as.integer(sections$toclevel)
      texts <- sections$line
      if (length(texts) == 0) {
        return(character())
      }
      indents <- strrep("  ", levels - 1L)
      paste0(indents, "- ", texts)
    },
    error = function(e) {
      tempest_warn("Failed to get Wikipedia sections for {.val {title}}.")
      character()
    }
  )
}
