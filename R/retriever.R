# Retrieval and web access
# Security-hardened URL handling with tidyverse style

#' Validate and normalize a URL
#'
#' Normalizes URLs to HTTPS and validates against security risks including
#' SSRF attacks (Server-Side Request Forgery).
#'
#' @param url URL string to normalize.
#' @return Normalized URL or `NA_character_` if invalid.
#' @keywords internal
tempest_normalize_url <- function(url) {
  if (!is.character(url) || length(url) != 1L) {
    tempest_abort(
      "{.arg url} must be a single string, not {.obj_type_friendly {url}}.",
      class = c(
        "tempest_retriever_url_error",
        "tempest_retriever_error",
        "tempest_error"
      ),
      url = url
    )
  }
  url <- tempest_trim(url)

  if (is.na(url) || identical(url, "")) {
    return(NA_character_)
  }
  parsed <- tryCatch(
    curl::curl_parse_url(url),
    error = function(e) NULL
  )
  if (
    is.null(parsed) ||
      !tolower(parsed$scheme %||% "") %in% c("http", "https") ||
      !nzchar(parsed$host %||% "")
  ) {
    tempest_url_abort(
      "Only valid HTTP/HTTPS URLs are permitted.",
      url
    )
  }
  host <- tolower(parsed$host)
  if (tempest_host_is_local_name(host) || !tempest_ip_is_public(host)) {
    tempest_url_abort("Only public network URLs are permitted.", url)
  }

  sub("^http:", "https:", parsed$url, ignore.case = TRUE)
}

tempest_url_abort <- function(message, url, parent = NULL) {
  safe_url <- if (is.character(url) && tempest_contract_sensitive_scalar(url)) {
    "<redacted>"
  } else {
    url
  }
  tempest_abort(
    c(message, x = "Blocked URL: {.url {safe_url}}"),
    class = c(
      "tempest_retriever_url_error",
      "tempest_retriever_error",
      "tempest_error"
    ),
    url = safe_url,
    parent = parent
  )
}

tempest_host_is_local_name <- function(host) {
  host <- sub("^\\[|\\]$", "", tolower(host))
  identical(host, "localhost") ||
    grepl("[.](localhost|local|internal|home|lan)$", host)
}

tempest_ipv4_octets <- function(host) {
  pieces <- strsplit(host, ".", fixed = TRUE)[[1]]
  if (length(pieces) != 4L || any(!grepl("^[0-9]{1,3}$", pieces))) {
    return(NULL)
  }
  octets <- suppressWarnings(as.integer(pieces))
  if (any(is.na(octets)) || any(octets < 0L | octets > 255L)) {
    return(NULL)
  }
  octets
}

tempest_ipv4_is_public <- function(octets) {
  first <- octets[[1]]
  second <- octets[[2]]
  if (first == 0L || first == 10L || first == 127L || first >= 224L) {
    return(FALSE)
  }
  if (first == 100L && second >= 64L && second <= 127L) {
    return(FALSE)
  }
  if (first == 169L && second == 254L) {
    return(FALSE)
  }
  if (first == 172L && second >= 16L && second <= 31L) {
    return(FALSE)
  }
  if (first == 192L && second == 168L) {
    return(FALSE)
  }
  if (first == 192L && second == 0L && octets[[3]] %in% c(0L, 2L)) {
    return(FALSE)
  }
  if (first == 192L && second == 88L && octets[[3]] == 99L) {
    return(FALSE)
  }
  if (first == 198L && second %in% c(18L, 19L)) {
    return(FALSE)
  }
  if (first == 198L && second == 51L && octets[[3]] == 100L) {
    return(FALSE)
  }
  if (first == 203L && second == 0L && octets[[3]] == 113L) {
    return(FALSE)
  }
  TRUE
}

tempest_ipv6_is_public <- function(host) {
  host <- sub("^\\[|\\]$", "", tolower(host))
  mapped <- sub("^.*::ffff:", "", host)
  if (!identical(mapped, host)) {
    octets <- tempest_ipv4_octets(mapped)
    return(!is.null(octets) && tempest_ipv4_is_public(octets))
  }
  if (host %in% c("::", "::1")) {
    return(FALSE)
  }
  !grepl("^(f[cd]|fe[89ab]|fe[c-f]|ff|2001:db8)", host)
}

tempest_ip_is_public <- function(host) {
  host <- sub("^\\[|\\]$", "", tolower(host))
  octets <- tempest_ipv4_octets(host)
  if (!is.null(octets)) {
    return(tempest_ipv4_is_public(octets))
  }
  if (grepl(":", host, fixed = TRUE)) {
    return(tempest_ipv6_is_public(host))
  }
  TRUE
}

tempest_resolve_host <- function(host) {
  host <- sub("^\\[|\\]$", "", host)
  addresses <- unique(c(
    tryCatch(curl::nslookup(host, ipv4_only = TRUE), error = function(e) NULL),
    tryCatch(curl::nslookup(host, ipv4_only = FALSE), error = function(e) NULL)
  ))
  addresses[!is.na(addresses) & nzchar(addresses)]
}

tempest_validate_fetch_url <- function(url, resolver = tempest_resolve_host) {
  url <- tempest_normalize_url(url)
  if (is.na(url)) {
    tempest_url_abort("Provide a non-empty HTTP or HTTPS URL.", url)
  }
  host <- curl::curl_parse_url(url)$host
  if (
    is.null(tempest_ipv4_octets(sub("^\\[|\\]$", "", host))) &&
      !grepl(":", host, fixed = TRUE)
  ) {
    addresses <- resolver(host)
    if (length(addresses) == 0L) {
      tempest_url_abort("The URL host could not be resolved.", url)
    }
    if (any(!vapply(addresses, tempest_ip_is_public, logical(1)))) {
      tempest_url_abort(
        "The URL resolves to a non-public network address.",
        url
      )
    }
  }
  url
}

#' Check if URL is safe (non-throwing version)
#'
#' @param url URL to check.
#' @return Logical indicating if URL passes security checks.
#' @keywords internal
tempest_url_is_safe <- function(url) {
  tryCatch(
    {
      normalized <- tempest_normalize_url(url)
      !is.na(normalized) && nzchar(normalized)
    },
    error = function(e) FALSE
  )
}

tempest_fetch_positive <- function(value, arg) {
  if (
    !is.numeric(value) ||
      length(value) != 1L ||
      is.na(value) ||
      !is.finite(value) ||
      value <= 0
  ) {
    tempest_abort(
      "{.arg {arg}} must be a positive finite number.",
      class = c(
        "tempest_retriever_config_error",
        "tempest_retriever_error",
        "tempest_error"
      )
    )
  }
  as.numeric(value)
}

tempest_response_content_type_supported <- function(content_type, url) {
  content_type <- tolower(strsplit(content_type %||% "", ";", fixed = TRUE)[[
    1
  ]][[1]])
  if (!nzchar(content_type)) {
    return(TRUE)
  }
  content_type %in%
    c(
      "text/html",
      "application/xhtml+xml",
      "text/plain",
      "text/markdown",
      "application/pdf"
    ) ||
    (identical(content_type, "application/octet-stream") &&
      grepl("[.]pdf($|[?])", url, ignore.case = TRUE))
}

#' Perform a bounded HTTP fetch
#' @keywords internal
#' @param url URL to fetch.
#' @param user_agent Optional HTTP user agent.
#' @param timeout_s Connect and total request timeout in seconds.
#' @param max_bytes Maximum response size in bytes.
#' @param max_redirects Maximum redirects to follow.
#' @param perform Request executor used by deterministic tests.
#' @param validate URL validator used before every request.
#' @return An `httr2_response`.
tempest_http_get <- function(
  url,
  user_agent = NULL,
  timeout_s = getOption("tempest.fetch_timeout_s", 20),
  max_bytes = getOption("tempest.fetch_max_bytes", 10 * 1024^2),
  max_redirects = getOption("tempest.fetch_max_redirects", 5L),
  perform = httr2::req_perform,
  validate = tempest_validate_fetch_url
) {
  timeout_s <- tempest_fetch_positive(timeout_s, "timeout_s")
  max_bytes <- tempest_fetch_positive(max_bytes, "max_bytes")
  max_redirects <- tempest_config_count(
    max_redirects,
    "max_redirects",
    allow_zero = TRUE
  )
  current <- url
  for (redirect in 0:max_redirects) {
    current <- validate(current)
    req <- httr2::request(current) |>
      httr2::req_timeout(timeout_s) |>
      httr2::req_options(
        connecttimeout = timeout_s,
        followlocation = FALSE,
        maxfilesize = max_bytes
      )
    if (!is.null(user_agent)) {
      req <- httr2::req_user_agent(req, user_agent)
    }
    resp <- perform(req)
    status <- httr2::resp_status(resp)
    if (!status %in% 300:399) {
      if (length(httr2::resp_body_raw(resp)) > max_bytes) {
        tempest_url_abort(
          "The response exceeds the configured size limit.",
          current
        )
      }
      content_type <- httr2::resp_header(resp, "content-type") %||% ""
      if (!tempest_response_content_type_supported(content_type, current)) {
        tempest_url_abort(
          "The response content type is not supported.",
          current
        )
      }
      return(resp)
    }
    location <- httr2::resp_header(resp, "location") %||% ""
    if (!nzchar(location) || redirect >= max_redirects) {
      tempest_url_abort("The URL exceeded the redirect limit.", current)
    }
    current <- httr2::url_modify_relative(current, location)
  }
  tempest_url_abort("The URL exceeded the redirect limit.", current)
}

#' @keywords internal
tempest_html_to_text <- function(html) {
  # Prefer rvest/xml2 when available.
  if (tempest_has("xml2") && tempest_has("rvest")) {
    doc <- xml2::read_html(html)
    # Remove script/style/noscript nodes.
    for (tag in c("script", "style", "noscript")) {
      nodes <- xml2::xml_find_all(doc, paste0(".//", tag))
      if (length(nodes) > 0) xml2::xml_remove(nodes)
    }
    txt <- rvest::html_text2(doc)
    txt <- gsub("[ \t]+", " ", txt)
    txt <- gsub("\n{3,}", "\n\n", txt)
    return(tempest_trim(txt))
  }

  # Fallback: extremely naive tag stripping.
  txt <- gsub(
    "(?is)<(script|style|noscript).*?>.*?</\\1>",
    " ",
    html,
    perl = TRUE
  )
  txt <- gsub("(?s)<[^>]+>", " ", txt, perl = TRUE)
  txt <- gsub("&nbsp;", " ", txt, fixed = TRUE)
  txt <- gsub("&amp;", "&", txt, fixed = TRUE)
  txt <- gsub("&lt;", "<", txt, fixed = TRUE)
  txt <- gsub("&gt;", ">", txt, fixed = TRUE)
  txt <- gsub("[ \t]+", " ", txt)
  txt <- gsub("\n{3,}", "\n\n", txt)
  tempest_trim(txt)
}

#' Fetch URL as markdown using ragnar
#'
#' Uses ragnar's read_as_markdown() for high-quality content extraction.
#' Handles HTML, PDFs, and many other document types.
#'
#' @param url The URL to fetch.
#' @return A list with `kind`, `text`, `title`, and `error`.
#' @keywords internal
tempest_fetch_url_markdown <- function(url, user_agent = NULL) {
  url <- tempest_normalize_url(url)
  if (is.na(url)) {
    tempest_abort(
      c(
        "Invalid URL.",
        i = "Provide a non-empty HTTP or HTTPS URL."
      ),
      class = c(
        "tempest_retriever_url_error",
        "tempest_retriever_error",
        "tempest_error"
      )
    )
  }

  # Determine content type from URL extension
  is_pdf <- grepl("\\.pdf($|\\?)", url, ignore.case = TRUE)
  kind <- if (is_pdf) "pdf" else "html"

  result <- tryCatch(
    {
      resp <- tempest_http_get(url, user_agent = user_agent)
      ctype <- httr2::resp_header(resp, "content-type") %||% ""
      kind <- if (
        grepl("application/pdf", ctype, fixed = TRUE) ||
          grepl("\\.pdf($|\\?)", url, ignore.case = TRUE)
      ) {
        "pdf"
      } else {
        "html"
      }
      tmp <- tempfile(fileext = if (identical(kind, "pdf")) ".pdf" else ".html")
      on.exit(unlink(tmp), add = TRUE)
      writeBin(httr2::resp_body_raw(resp), tmp)
      md <- ragnar::read_as_markdown(
        tmp,
        html_extract_selectors = c("main", "article", "#content", ".content"),
        html_zap_selectors = c(
          "nav",
          "header",
          "footer",
          "aside",
          ".sidebar",
          ".navigation"
        )
      )

      # Extract title from first heading if present
      title <- NA_character_
      lines <- strsplit(as.character(md), "\n")[[1]]
      for (line in lines) {
        if (grepl("^#+ ", line)) {
          title <- sub("^#+ ", "", tempest_trim(line))
          if (nchar(title) > 120) {
            title <- paste0(substr(title, 1, 117), "...")
          }
          break
        }
      }

      list(
        kind = kind,
        text = tempest_trim(as.character(md)),
        title = title,
        error = NULL
      )
    },
    error = function(e) {
      list(
        kind = kind,
        text = NA_character_,
        title = NA_character_,
        error = "The operation failed."
      )
    }
  )

  result
}

tempest_ragnar_retrieve <- function(store, query, top_k, method) {
  switch(
    method,
    hybrid = ragnar::ragnar_retrieve(
      store,
      text = query,
      top_k = top_k
    ),
    vss = ragnar::ragnar_retrieve_vss(
      store,
      query = query,
      top_k = top_k
    ),
    bm25 = ragnar::ragnar_retrieve_bm25(
      store,
      text = query,
      top_k = top_k
    )
  )
}

tempest_ragnar_store_insert <- function(store, chunks) {
  ragnar::ragnar_store_insert(store, chunks)
}

tempest_ragnar_store_build_index <- function(store) {
  ragnar::ragnar_store_build_index(store)
}

#' @keywords internal
tempest_fetch_url_text <- function(url, user_agent = NULL) {
  url <- tempest_normalize_url(url)
  if (is.na(url)) {
    tempest_abort(
      c(
        "Invalid URL.",
        i = "Provide a non-empty HTTP or HTTPS URL."
      ),
      class = c(
        "tempest_retriever_url_error",
        "tempest_retriever_error",
        "tempest_error"
      )
    )
  }

  # Use ragnar's read_as_markdown when available (preferred)
  if (tempest_has("ragnar")) {
    result <- tempest_fetch_url_markdown(url, user_agent = user_agent)
    # Return in same format as legacy, but include title
    return(list(
      kind = result$kind,
      text = result$text,
      title = result$title,
      error = result$error
    ))
  }

  # Legacy fallback when ragnar is not available
  resp <- tempest_http_get(url, user_agent = user_agent)
  ctype <- httr2::resp_header(resp, "content-type") %||% ""

  if (
    grepl("application/pdf", ctype, fixed = TRUE) ||
      grepl("\\.pdf($|\\?)", url, ignore.case = TRUE)
  ) {
    if (!tempest_has("pdftools")) {
      return(list(
        kind = "pdf",
        text = NA_character_,
        title = NA_character_,
        error = "pdftools not installed; cannot parse PDF."
      ))
    }
    bin <- httr2::resp_body_raw(resp)
    tmp <- tempfile(fileext = ".pdf")
    on.exit(unlink(tmp), add = TRUE)
    writeBin(bin, tmp)
    txt <- paste(pdftools::pdf_text(tmp), collapse = "\n\n")
    return(list(
      kind = "pdf",
      text = tempest_trim(txt),
      title = NA_character_,
      error = NULL
    ))
  }

  html <- httr2::resp_body_string(resp)
  list(
    kind = "html",
    text = tempest_html_to_text(html),
    title = NA_character_,
    error = NULL
  )
}

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
tempest_wiki_page_plaintext <- function(title) {
  res <- tempest_wikipedia_api(list(
    action = "query",
    prop = "extracts",
    explaintext = 1,
    exintro = 0,
    exsectionformat = "plain",
    redirects = 1,
    titles = title,
    format = "json",
    utf8 = 1
  ))
  pages <- res$query$pages
  if (is.null(pages)) {
    return(NA_character_)
  }
  # pages is a named list keyed by pageid
  page <- pages[[1]]
  page$extract %||% NA_character_
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
      endpoint = Sys.getenv(
        "AZURE_AI_SEARCH_ENDPOINT",
        unset = Sys.getenv("AZURE_AI_SEARCH_URL", unset = "")
      ),
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

#' TempestRetriever
#'
#' Provides web and Wikipedia retrieval with caching, plus helper methods to
#' register retrieval tools with ellmer chats. Optionally integrates with
#' ragnar for semantic search capabilities.
#'
#' @field config Read-only `TempestConfig` fixed at construction.
#' @field workspace Read-only reference to the authoritative
#'   [ResearchWorkspace] containing provisional research evidence. Workspace
#'   mutation methods remain available.
#' @field ragnar_store Read-only ragnar store reference derived from `config`.
#' @field cache_dir Read-only cache path derived from `config`.
#' @field cache_enabled Read-only cache policy derived from `config`.
#' @field cache_ttl Read-only maximum cache age derived from `config`.
#'
#' @export
TempestRetriever <- R6::R6Class(
  "TempestRetriever",
  public = list(
    #' @description
    #' Create a new TempestRetriever.
    #' @param config A `TempestConfig` object.
    #' @param workspace A [ResearchWorkspace].
    initialize = function(
      config = tempest_config(),
      workspace = tempest_research_workspace()
    ) {
      if (!inherits(workspace, "ResearchWorkspace")) {
        tempest_research_workspace_abort(
          "{.arg workspace} must be a ResearchWorkspace."
        )
      }
      private$config_value <- config
      private$workspace_value <- workspace
      private$workspace_value$set_max_sources(config@max_sources)
      private$cache_counts <- new.env(parent = emptyenv())
      invisible(self)
    },

    #' @description
    #' Search for sources using the configured provider.
    #' @param query Search query string.
    #' @param k Maximum number of results.
    #' @param provider Search provider override.
    #' @param force If TRUE, bypass any cached result and refresh the cache.
    #' @return A tibble of search results.
    search = function(query, k = NULL, provider = NULL, force = FALSE) {
      k <- k %||% self$config@max_search_results
      k <- tempest_config_count(k, "k")
      if (k > self$config@max_search_results) {
        tempest_config_abort(
          c(
            "Search result request exceeds the configured budget.",
            x = "Requested {k}; maximum is {self$config@max_search_results}."
          )
        )
      }
      provider <- provider %||% self$config@search_provider
      provider <- tempest_normalize_search_provider(provider)

      key <- tempest_search_cache_key(provider, query, k)
      if (self$cache_enabled && !isTRUE(force)) {
        cached <- tempest_cache_lookup(
          self$cache_dir,
          key,
          max_age = self$cache_ttl
        )
        private$record_cache("search", cached$status)
        if (!is.null(cached$value)) {
          return(cached$value)
        }
      } else {
        private$record_cache("search", "bypass")
      }

      # Handle "native" provider - falls back to Wikipedia for direct search() calls
      # since native provider web search is handled via tool registration
      effective_provider <- if (identical(provider, "native")) {
        "wikipedia"
      } else {
        provider
      }

      out <- tryCatch(
        switch(
          effective_provider,
          wikipedia = tempest_wiki_search(query, limit = k),
          you = tempest_search_you(query, k = k),
          bing = tempest_search_bing(query, k = k),
          serper = tempest_search_serper(query, k = k),
          brave = tempest_search_brave(query, k = k),
          duckduckgo = tempest_search_duckduckgo(query, k = k),
          tavily = tempest_search_tavily(query, k = k),
          searxng = tempest_search_searxng(query, k = k),
          google = tempest_search_google(query, k = k),
          azure_ai_search = tempest_search_azure_ai_search(query, k = k),
          tempest_abort(c(
            "Unknown search provider: {.val {provider}}",
            i = "Available providers: {.val {tempest_search_provider_choices()}}"
          ))
        ),
        error = function(error) {
          tempest_rethrow_operation(error, class = "tempest_retriever_error")
        }
      )

      # Normalize URLs without aborting on missing/unsafe values, then drop
      # any rows we cannot use. A single bad or blocked URL must not discard
      # every other (valid) result for the query.
      out$url <- purrr::map_chr(out$url, function(u) {
        tryCatch(tempest_normalize_url(u), error = function(e) NA_character_)
      })
      out <- out[!is.na(out$url) & nzchar(out$url), , drop = FALSE]
      out$source_id <- purrr::map_chr(out$url, tempest_source_id)

      if (self$cache_enabled && tempest_cache_set(self$cache_dir, key, out)) {
        private$record_cache("search", "write")
      }
      out
    },

    #' @description
    #' Fetch and cache content from a URL.
    #' @param url The URL to fetch.
    #' @param force If TRUE, bypass cache.
    #' @param perspective Optional perspective name for ragnar metadata.
    #' @return A source object.
    fetch = function(url, force = FALSE, perspective = NA_character_) {
      url <- tempest_normalize_url(url)
      if (is.na(url)) {
        tempest_abort(
          c(
            "Invalid URL.",
            i = "Provide a non-empty HTTP or HTTPS URL."
          ),
          class = c(
            "tempest_retriever_url_error",
            "tempest_retriever_error",
            "tempest_error"
          )
        )
      }

      key <- tempest_fetch_cache_key(url, self$config@user_agent)
      if (self$cache_enabled && !isTRUE(force)) {
        cached <- tempest_cache_lookup(
          self$cache_dir,
          key,
          max_age = self$cache_ttl
        )
        private$record_cache("fetch", cached$status)
        if (!is.null(cached$value)) {
          self$workspace$upsert_retrieved_resource(cached$value)
          return(cached$value)
        }
      } else {
        private$record_cache("fetch", "bypass")
      }

      res <- tryCatch(
        tempest_fetch_url_text(url, user_agent = self$config@user_agent),
        error = function(error) {
          tempest_rethrow_operation(error, class = "tempest_retriever_error")
        }
      )
      fetched_at <- tempest_now_utc()

      if (!is.null(res$error)) {
        src <- tempest_source(
          url = url,
          title = NA_character_,
          snippet = NA_character_,
          content_text = NA_character_,
          fetched_at = fetched_at,
          content_hash = NA_character_,
          meta = list(kind = res$kind, error = res$error)
        )
        self$workspace$upsert_retrieved_resource(src)
        return(src)
      }

      txt <- res$text %||% NA_character_
      txt_hash <- if (!is.na(txt) && nzchar(txt)) {
        tempest_product_content_hash(txt, "text/html")
      } else {
        NA_character_
      }

      # Use title from result if available (ragnar extracts from headings)
      # Otherwise fall back to first line
      title <- res$title %||% NA_character_
      if (is.na(title) && !is.na(txt) && nzchar(txt)) {
        first <- unlist(strsplit(txt, "\n", fixed = TRUE))[1]
        title <- tempest_trim(first)
        if (nchar(title) > 120) title <- paste0(substr(title, 1, 117), "...")
      }

      snippet <- if (!is.na(txt) && nzchar(txt)) {
        substr(txt, 1, 300)
      } else {
        NA_character_
      }

      src <- tempest_source(
        url = url,
        title = title,
        snippet = snippet,
        content_text = txt,
        fetched_at = fetched_at,
        content_hash = txt_hash,
        meta = list(kind = res$kind, error = NULL)
      )
      self$workspace$upsert_retrieved_resource(src)
      if (self$cache_enabled && tempest_cache_set(self$cache_dir, key, src)) {
        private$record_cache("fetch", "write")
      }

      # Ingest into ragnar store if available
      if (!is.null(self$ragnar_store) && !is.na(txt) && nzchar(txt)) {
        self$ingest_to_ragnar(
          source_id = src$id,
          url = url,
          title = title,
          text = txt,
          fetched_at = fetched_at,
          content_type = res$kind,
          perspective = perspective
        )
      }

      src
    },

    #' @description
    #' Ingest content into the ragnar store.
    #' @param source_id The tempest source ID.
    #' @param url The source URL.
    #' @param title The document title.
    #' @param text The full text content.
    #' @param fetched_at Timestamp when fetched.
    #' @param content_type Content type (html, pdf, etc.).
    #' @param perspective Optional perspective name.
    ingest_to_ragnar = function(
      source_id,
      url,
      title,
      text,
      fetched_at,
      content_type,
      perspective = NA_character_
    ) {
      if (is.null(self$ragnar_store)) {
        return(invisible(NULL))
      }
      tempest_require("ragnar", "RAG capabilities require the ragnar package.")

      tryCatch(
        {
          doc <- ragnar::MarkdownDocument(text, origin = url)
          chunks <- ragnar::markdown_chunk(
            doc,
            target_size = 512,
            target_overlap = 0.25
          )

          n_chunks <- nrow(chunks)
          chunks$source_id <- rep(source_id, n_chunks)
          chunks$url <- rep(url, n_chunks)
          chunks$title <- rep(title %||% NA_character_, n_chunks)
          chunks$fetched_at <- rep(fetched_at, n_chunks)
          chunks$content_type <- rep(content_type %||% NA_character_, n_chunks)
          chunks$perspective <- rep(perspective %||% NA_character_, n_chunks)
          chunks$context <- paste0(
            "[Source: ",
            source_id,
            "] ",
            ifelse(!is.na(title), paste0(title, " - "), "")
          )

          tempest_ragnar_store_insert(self$ragnar_store, chunks)
          invisible(n_chunks)
        },
        error = function(error) {
          tempest_rethrow_operation(error, class = "tempest_retriever_error")
        }
      )
    },

    #' @description
    #' Build the ragnar store index for faster retrieval.
    build_ragnar_index = function() {
      if (is.null(self$ragnar_store)) {
        return(invisible(NULL))
      }
      tempest_require("ragnar", "RAG capabilities require the ragnar package.")
      tryCatch(
        tempest_ragnar_store_build_index(self$ragnar_store),
        error = function(error) {
          tempest_rethrow_operation(error, class = "tempest_retriever_error")
        }
      )
      invisible(TRUE)
    },

    #' @description
    #' Retrieve relevant chunks from the ragnar store.
    #' @param query The search query.
    #' @param k Maximum number of chunks to return.
    #' @param method Retrieval method: "hybrid", "vss", or "bm25".
    #' @return A data frame of relevant chunks with metadata.
    retrieve = function(query, k = 10, method = c("hybrid", "vss", "bm25")) {
      if (is.null(self$ragnar_store)) {
        tempest_abort(
          "No ragnar store configured. Use embed_fn in tempest_config()."
        )
      }
      tempest_require("ragnar", "RAG capabilities require the ragnar package.")
      method <- match.arg(method)
      k <- tempest_config_count(k, "k")

      tryCatch(
        tempest_ragnar_retrieve(
          self$ragnar_store,
          query,
          top_k = k,
          method = method
        ),
        error = function(error) {
          tempest_rethrow_operation(error, class = "tempest_retriever_error")
        }
      )
    },

    #' @description
    #' Get the text content for a source by id.
    #' @param source_id The source id.
    #' @return The content text or NA.
    get_source_text = function(source_id) {
      src <- self$workspace$get_retrieved_source(source_id)
      if (is.null(src)) {
        return(NA_character_)
      }
      src$content_text %||% NA_character_
    },

    #' @description
    #' List all sources in the store.
    #' @return A list of source objects.
    list_retrieved_sources = function() {
      self$workspace$list_retrieved_sources()
    },

    #' @description
    #' Return cache hit/miss counters for this retriever.
    #' @param reset Whether to reset counters after reading them.
    #' @return A tibble with one row each for search and fetch cache counters.
    cache_stats = function(reset = FALSE) {
      out <- private$cache_stats_table()
      if (isTRUE(reset)) {
        private$cache_counts <- new.env(parent = emptyenv())
      }
      out
    }
  ),
  active = list(
    config = function(value) {
      if (!missing(value)) {
        tempest_abort(
          "{.field config} is fixed when the retriever is created.",
          class = c("tempest_retriever_identity_error", "tempest_error")
        )
      }
      private$config_value
    },
    workspace = function(value) {
      if (!missing(value)) {
        tempest_abort(
          "{.field workspace} is fixed when the retriever is created.",
          class = c("tempest_retriever_identity_error", "tempest_error")
        )
      }
      private$workspace_value
    },
    ragnar_store = function(value) {
      if (!missing(value)) {
        tempest_abort(
          "{.field ragnar_store} is fixed when the retriever is created.",
          class = c("tempest_retriever_identity_error", "tempest_error")
        )
      }
      private$config_value@ragnar_store
    },
    cache_dir = function(value) {
      if (!missing(value)) {
        tempest_abort(
          "{.field cache_dir} is fixed when the retriever is created.",
          class = c("tempest_retriever_identity_error", "tempest_error")
        )
      }
      private$config_value@cache_dir
    },
    cache_enabled = function(value) {
      if (!missing(value)) {
        tempest_abort(
          "{.field cache_enabled} is fixed when the retriever is created.",
          class = c("tempest_retriever_identity_error", "tempest_error")
        )
      }
      isTRUE(private$config_value@cache_enabled)
    },
    cache_ttl = function(value) {
      if (!missing(value)) {
        tempest_abort(
          "{.field cache_ttl} is fixed when the retriever is created.",
          class = c("tempest_retriever_identity_error", "tempest_error")
        )
      }
      private$config_value@cache_ttl
    }
  ),
  private = list(
    config_value = NULL,
    workspace_value = NULL,
    cache_counts = NULL,

    record_cache = function(kind, status) {
      key <- paste(kind, status, sep = "_")
      private$cache_counts[[key]] <- (private$cache_counts[[key]] %||% 0L) + 1L
      invisible(key)
    },

    cache_stats_table = function() {
      kinds <- c("search", "fetch")
      count <- function(kind, status) {
        private$cache_counts[[paste(kind, status, sep = "_")]] %||% 0L
      }
      tibble::tibble(
        kind = kinds,
        hits = unname(vapply(kinds, count, integer(1), status = "hit")),
        misses = unname(vapply(kinds, count, integer(1), status = "miss")),
        expired = unname(vapply(kinds, count, integer(1), status = "expired")),
        read_errors = unname(vapply(
          kinds,
          count,
          integer(1),
          status = "read_error"
        )),
        bypasses = unname(vapply(kinds, count, integer(1), status = "bypass")),
        writes = unname(vapply(kinds, count, integer(1), status = "write"))
      )
    }
  )
)

#' Create a TempestRetriever
#'
#' @param config A `TempestConfig`.
#' @param workspace A [ResearchWorkspace].
#' @return A `TempestRetriever`.
#' @examples
#' retriever <- tempest_retriever(config = tempest_config())
#' \dontrun{
#' results <- retriever$search("history of jazz", provider = "wikipedia")
#' }
#' @export
tempest_retriever <- function(
  config = tempest_config(),
  workspace = tempest_research_workspace()
) {
  TempestRetriever$new(config = config, workspace = workspace)
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
  tempest_require("xml2", "DuckDuckGo search requires HTML parsing.")
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
  endpoint <- Sys.getenv(
    "AZURE_AI_SEARCH_ENDPOINT",
    unset = Sys.getenv("AZURE_AI_SEARCH_URL", unset = "")
  )
  if (identical(endpoint, "")) {
    tempest_abort(c(
      "{.envvar AZURE_AI_SEARCH_ENDPOINT} environment variable is not set.",
      i = "Set it to your Azure AI Search endpoint.",
      i = "{.envvar AZURE_AI_SEARCH_URL} is also accepted for upstream STORM compatibility."
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
      if (!tempest_has("xml2")) {
        return(character())
      }
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
