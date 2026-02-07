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
  url <- tempest_trim(url)

  if (is.na(url) || identical(url, "")) {
    return(NA_character_)
  }

  # Block dangerous protocols (SSRF prevention)
  dangerous_protocols <- "^(file|ftp|gopher|data|javascript|vbscript):"
  if (
    stringi::stri_detect_regex(
      url,
      dangerous_protocols,
      case_insensitive = TRUE
    )
  ) {
    tempest_abort(c(
      "URL protocol not allowed for security reasons.",
      x = "Blocked URL: {.url {url}}",
      i = "Only HTTP/HTTPS URLs are permitted."
    ))
  }

  # Block local/internal addresses (SSRF prevention)
  local_patterns <- paste0(
    "^https?://(",
    "localhost|",
    "127\\.0\\.0\\.1|",
    "0\\.0\\.0\\.0|",
    "\\[::1?\\]|",
    "192\\.168\\.[0-9]+\\.[0-9]+|",
    "10\\.[0-9]+\\.[0-9]+\\.[0-9]+|",
    "172\\.(1[6-9]|2[0-9]|3[01])\\.[0-9]+\\.[0-9]+",
    ")(/|:|$)"
  )
  if (
    stringi::stri_detect_regex(url, local_patterns, case_insensitive = TRUE)
  ) {
    tempest_abort(c(
      "Local network URLs not allowed for security reasons.",
      x = "Blocked URL: {.url {url}}",
      i = "Only public URLs are permitted."
    ))
  }

  # Upgrade HTTP to HTTPS
  url <- sub("^http://", "https://", url)
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
      tempest_normalize_url(url)
      TRUE
    },
    error = function(e) FALSE
  )
}

#' @keywords internal
tempest_http_get <- function(url, user_agent = NULL, timeout_s = 20) {
  req <- httr2::request(url)
  if (!is.null(user_agent)) {
    req <- httr2::req_user_agent(req, user_agent)
  }
  req <- httr2::req_timeout(req, timeout_s)
  httr2::req_perform(req)
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
tempest_fetch_url_markdown <- function(url) {
  url <- tempest_normalize_url(url)
  if (is.na(url)) {
    tempest_abort("Invalid URL.")
  }

  # Determine content type from URL extension
  is_pdf <- grepl("\\.pdf($|\\?)", url, ignore.case = TRUE)
  kind <- if (is_pdf) "pdf" else "html"

  result <- tryCatch(
    {
      # Use ragnar's read_as_markdown with good defaults for web content
      md <- ragnar::read_as_markdown(
        url,
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
        error = conditionMessage(e)
      )
    }
  )

  result
}

#' @keywords internal
tempest_fetch_url_text <- function(url, user_agent = NULL) {
  url <- tempest_normalize_url(url)
  if (is.na(url)) {
    tempest_abort("Invalid URL.")
  }

  # Use ragnar's read_as_markdown when available (preferred)
  if (tempest_has("ragnar")) {
    result <- tempest_fetch_url_markdown(url)
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
    writeBin(bin, tmp)
    txt <- paste(pdftools::pdf_text(tmp), collapse = "\n\n")
    unlink(tmp)
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
  resp <- httr2::req_perform(req)
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

#' TempestRetriever
#'
#' Provides web and Wikipedia retrieval with caching, plus helper methods to
#' register retrieval tools with ellmer chats. Optionally integrates with
#' ragnar for semantic search capabilities.
#'
#' @field config A `TempestConfig` object.
#' @field store A `SourceStore` object.
#' @field ragnar_store A ragnar store for semantic retrieval (optional).
#' @field cache_dir Path to cache directory.
#'
#' @export
TempestRetriever <- R6::R6Class(
  "TempestRetriever",
  public = list(
    config = NULL,
    store = NULL,
    ragnar_store = NULL,
    cache_dir = NULL,

    #' @description
    #' Create a new TempestRetriever.
    #' @param config A `TempestConfig` object.
    #' @param store A `SourceStore` object.
    initialize = function(config = tempest_config(), store = SourceStore$new()) {
      self$config <- config
      self$store <- store
      self$ragnar_store <- config$ragnar_store
      self$cache_dir <- config$cache_dir
      invisible(self)
    },

    #' @description
    #' Search for sources using the configured provider.
    #' @param query Search query string.
    #' @param k Maximum number of results.
    #' @param provider Search provider override.
    #' @return A tibble of search results.
    search = function(query, k = NULL, provider = NULL) {
      k <- k %||% self$config$max_search_results
      provider <- provider %||% self$config$search_provider

      key <- tempest_cache_key("search", provider, query, k)
      cached <- tempest_cache_get(self$cache_dir, key)
      if (!is.null(cached)) {
        return(cached)
      }

      # Handle "native" provider - falls back to Wikipedia for direct search() calls
      # since native provider web search is handled via tool registration
      effective_provider <- if (identical(provider, "native")) {
        "wikipedia"
      } else {
        provider
      }

      out <- switch(
        effective_provider,
        wikipedia = tempest_wiki_search(query, limit = k),
        serper = tempest_search_serper(query, k = k),
        brave = tempest_search_brave(query, k = k),
        tavily = tempest_search_tavily(query, k = k),
        tempest_abort(c(
          "Unknown search provider: {.val {provider}}",
          i = "Available providers: native, wikipedia, serper, brave, tavily"
        ))
      )

      # Normalize and add deterministic source ids
      out$url <- purrr::map_chr(out$url, tempest_normalize_url)
      out$source_id <- purrr::map_chr(out$url, tempest_source_id)

      tempest_cache_set(self$cache_dir, key, out)
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
        tempest_abort("Invalid URL.")
      }

      key <- tempest_cache_key("fetch", url)
      cached <- if (!force) tempest_cache_get(self$cache_dir, key) else NULL
      if (!is.null(cached)) {
        self$store$upsert_source(cached)
        return(cached)
      }

      res <- tempest_fetch_url_text(url, user_agent = self$config$user_agent)
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
        self$store$upsert_source(src)
        tempest_cache_set(self$cache_dir, key, src)
        return(src)
      }

      txt <- res$text %||% NA_character_
      txt_hash <- if (!is.na(txt) && nzchar(txt)) {
        digest::digest(txt, algo = "xxhash64")
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
      self$store$upsert_source(src)
      tempest_cache_set(self$cache_dir, key, src)

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

      # Create markdown document and chunk it
      doc <- ragnar::MarkdownDocument(text, origin = url)
      chunks <- ragnar::markdown_chunk(
        doc,
        target_size = 512,
        target_overlap = 0.25
      )

      # Add tempest metadata to chunks
      n_chunks <- nrow(chunks)
      chunks$source_id <- rep(source_id, n_chunks)
      chunks$url <- rep(url, n_chunks)
      chunks$title <- rep(title %||% NA_character_, n_chunks)
      chunks$fetched_at <- rep(fetched_at, n_chunks)
      chunks$content_type <- rep(content_type %||% NA_character_, n_chunks)
      chunks$perspective <- rep(perspective %||% NA_character_, n_chunks)

      # Add context for better retrieval (source info prepended)
      chunks$context <- paste0(
        "[Source: ",
        source_id,
        "] ",
        ifelse(!is.na(title), paste0(title, " - "), "")
      )

      # Insert into ragnar store
      ragnar::ragnar_store_insert(self$ragnar_store, chunks)

      invisible(n_chunks)
    },

    #' @description
    #' Build the ragnar store index for faster retrieval.
    build_ragnar_index = function() {
      if (is.null(self$ragnar_store)) {
        return(invisible(NULL))
      }
      tempest_require("ragnar", "RAG capabilities require the ragnar package.")
      ragnar::ragnar_store_build_index(self$ragnar_store)
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

      retrieve_fn <- switch(
        method,
        hybrid = ragnar::ragnar_retrieve_vss_and_bm25,
        vss = ragnar::ragnar_retrieve_vss,
        bm25 = ragnar::ragnar_retrieve_bm25
      )

      retrieve_fn(self$ragnar_store, query, k = k)
    },

    #' @description
    #' Get the text content for a source by id.
    #' @param source_id The source id.
    #' @return The content text or NA.
    get_source_text = function(source_id) {
      src <- self$store$get_source(source_id)
      if (is.null(src)) {
        return(NA_character_)
      }
      src$content_text %||% NA_character_
    },

    #' @description
    #' List all sources in the store.
    #' @return A list of source objects.
    list_sources = function() {
      self$store$list_sources()
    }
  )
)

#' Create a TempestRetriever
#'
#' @param config A `TempestConfig`.
#' @param store A `SourceStore`.
#' @return A `TempestRetriever`.
#' @export
tempest_retriever <- function(
  config = tempest_config(),
  store = SourceStore$new()
) {
  TempestRetriever$new(config = config, store = store)
}

# --- Provider-specific search helpers ----------------------------------------
# API keys are retrieved from environment variables inside functions,
# never as parameters (security: prevents exposure in stack traces)

#' Search using Serper API
#'
#' @param query Search query string.
#' @param k Maximum number of results.
#' @return Tibble with title, url, snippet columns.
#' @keywords internal
tempest_search_serper <- function(query, k = 8L) {
  api_key <- Sys.getenv("SERPER_API_KEY", unset = "")

  if (identical(api_key, "")) {
    tempest_abort(c(
      "SERPER_API_KEY environment variable is not set.",
      i = "Set it with: {.code Sys.setenv(SERPER_API_KEY = \"your-key\")}",
      i = "Or use a different provider: {.code tempest_config(search_provider = \"wikipedia\")}"
    ))
  }

  req <- httr2::request("https://google.serper.dev/search") |>
    httr2::req_headers(
      `X-API-KEY` = api_key,
      `Content-Type` = "application/json"
    ) |>
    httr2::req_body_json(list(q = query, num = k))

  resp <- httr2::req_perform(req)
  body <- httr2::resp_body_json(resp, simplifyVector = TRUE)
  items <- body$organic

  if (is.null(items) || length(items) == 0L) {
    return(tibble::tibble(
      title = character(),
      url = character(),
      snippet = character()
    ))
  }

  tibble::tibble(
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
  api_key <- Sys.getenv("BRAVE_API_KEY", unset = "")

  if (identical(api_key, "")) {
    tempest_abort(c(
      "BRAVE_API_KEY environment variable is not set.",
      i = "Set it with: {.code Sys.setenv(BRAVE_API_KEY = \"your-key\")}",
      i = "Or use a different provider: {.code tempest_config(search_provider = \"wikipedia\")}"
    ))
  }

  req <- httr2::request("https://api.search.brave.com/res/v1/web/search") |>
    httr2::req_headers(
      Accept = "application/json",
      `X-Subscription-Token` = api_key
    ) |>
    httr2::req_url_query(q = query, count = k)

  resp <- httr2::req_perform(req)
  body <- httr2::resp_body_json(resp, simplifyVector = TRUE)
  items <- body$web$results

  if (is.null(items) || length(items) == 0L) {
    return(tibble::tibble(
      title = character(),
      url = character(),
      snippet = character()
    ))
  }

  tibble::tibble(
    title = items$title %||% character(),
    url = items$url %||% character(),
    snippet = items$description %||% character()
  )
}

#' Search using Tavily API
#'
#' @param query Search query string.
#' @param k Maximum number of results.
#' @return Tibble with title, url, snippet columns.
#' @keywords internal
tempest_search_tavily <- function(query, k = 8L) {
  api_key <- Sys.getenv("TAVILY_API_KEY", unset = "")

  if (identical(api_key, "")) {
    tempest_abort(c(
      "TAVILY_API_KEY environment variable is not set.",
      i = "Set it with: {.code Sys.setenv(TAVILY_API_KEY = \"your-key\")}",
      i = "Or use a different provider: {.code tempest_config(search_provider = \"wikipedia\")}"
    ))
  }

  req <- httr2::request("https://api.tavily.com/search") |>
    httr2::req_headers(`Content-Type` = "application/json") |>
    httr2::req_body_json(list(
      api_key = api_key,
      query = query,
      max_results = k
    ))

  resp <- httr2::req_perform(req)
  body <- httr2::resp_body_json(resp, simplifyVector = TRUE)
  items <- body$results

  if (is.null(items) || length(items) == 0L) {
    return(tibble::tibble(
      title = character(),
      url = character(),
      snippet = character()
    ))
  }

  tibble::tibble(
    title = items$title %||% character(),
    url = items$url %||% character(),
    snippet = items$content %||% character()
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
  tryCatch({
    if (!tempest_has("xml2")) return(character())
    url <- tempest_normalize_url(url)
    if (is.na(url)) return(character())
    resp <- tempest_http_get(url, timeout_s = 10)
    html <- httr2::resp_body_string(resp)
    doc <- xml2::read_html(html)
    headings <- xml2::xml_find_all(doc, ".//h1|.//h2|.//h3|.//h4")
    if (length(headings) == 0) return(character())
    levels <- as.integer(sub("h", "", xml2::xml_name(headings)))
    texts <- tempest_trim(xml2::xml_text(headings))
    keep <- nzchar(texts)
    levels <- levels[keep]
    texts <- texts[keep]
    if (length(texts) == 0) return(character())
    min_level <- min(levels)
    indents <- strrep("  ", levels - min_level)
    paste0(indents, "- ", texts)
  }, error = function(e) {
    tempest_warn("Failed to extract ToC from {.url {url}}: {conditionMessage(e)}")
    character()
  })
}

#' Get Wikipedia page sections via Parse API
#'
#' Uses the Wikipedia Parse API to get structured section headings.
#'
#' @param title Wikipedia page title.
#' @return Character vector of indented section headings, or `character()` on error.
#' @keywords internal
tempest_wiki_page_sections <- function(title) {
  tryCatch({
    res <- tempest_wikipedia_api(list(
      action = "parse",
      page = title,
      prop = "sections",
      format = "json",
      utf8 = 1
    ))
    sections <- res$parse$sections
    if (is.null(sections) || length(sections) == 0) return(character())
    # sections is a data.frame with toclevel, line, number columns
    levels <- as.integer(sections$toclevel)
    texts <- sections$line
    if (length(texts) == 0) return(character())
    indents <- strrep("  ", levels - 1L)
    paste0(indents, "- ", texts)
  }, error = function(e) {
    tempest_warn("Failed to get Wikipedia sections for {.val {title}}: {conditionMessage(e)}")
    character()
  })
}
