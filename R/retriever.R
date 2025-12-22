# Retrieval and web access

#' @keywords internal
storm_normalize_url <- function(url) {
  url <- stormr_trim(url)
  if (is.na(url) || url == "") return(NA_character_)
  # Minimal normalization; avoid introducing dependencies.
  url <- sub("^http://", "https://", url)
  url
}

#' @keywords internal
storm_http_get <- function(url, user_agent = NULL, timeout_s = 20) {
  req <- httr2::request(url)
  if (!is.null(user_agent)) req <- httr2::req_user_agent(req, user_agent)
  req <- httr2::req_timeout(req, timeout_s)
  httr2::req_perform(req)
}

#' @keywords internal
storm_html_to_text <- function(html) {

  # Prefer rvest/xml2 when available.
  if (stormr_has("xml2") && stormr_has("rvest")) {
    doc <- xml2::read_html(html)
    # Remove script/style/noscript nodes.
    for (tag in c("script", "style", "noscript")) {
      nodes <- xml2::xml_find_all(doc, paste0(".//", tag))
      if (length(nodes) > 0) xml2::xml_remove(nodes)
    }
    txt <- rvest::html_text2(doc)
    txt <- gsub("[ \t]+", " ", txt)
    txt <- gsub("\n{3,}", "\n\n", txt)
    return(stormr_trim(txt))
  }

  # Fallback: extremely naive tag stripping.
  txt <- gsub("(?is)<(script|style|noscript).*?>.*?</\\1>", " ", html, perl = TRUE)
  txt <- gsub("(?s)<[^>]+>", " ", txt, perl = TRUE)
  txt <- gsub("&nbsp;", " ", txt, fixed = TRUE)
  txt <- gsub("&amp;", "&", txt, fixed = TRUE)
  txt <- gsub("&lt;", "<", txt, fixed = TRUE)
  txt <- gsub("&gt;", ">", txt, fixed = TRUE)
  txt <- gsub("[ \t]+", " ", txt)
  txt <- gsub("\n{3,}", "\n\n", txt)
  stormr_trim(txt)
}

#' Fetch URL as markdown using ragnar
#'
#' Uses ragnar's read_as_markdown() for high-quality content extraction.
#' Handles HTML, PDFs, and many other document types.
#'
#' @param url The URL to fetch.
#' @return A list with `kind`, `text`, `title`, and `error`.
#' @keywords internal
storm_fetch_url_markdown <- function(url) {
  url <- storm_normalize_url(url)
  if (is.na(url)) stormr_abort("Invalid URL.")

  # Determine content type from URL extension
  is_pdf <- grepl("\\.pdf($|\\?)", url, ignore.case = TRUE)
  kind <- if (is_pdf) "pdf" else "html"

  result <- tryCatch(
    {
      # Use ragnar's read_as_markdown with good defaults for web content
      md <- ragnar::read_as_markdown(
        url,
        html_extract_selectors = c("main", "article", "#content", ".content"),
        html_zap_selectors = c("nav", "header", "footer", "aside", ".sidebar", ".navigation")
      )

      # Extract title from first heading if present
      title <- NA_character_
      lines <- strsplit(as.character(md), "\n")[[1]]
      for (line in lines) {
        if (grepl("^#+ ", line)) {
          title <- sub("^#+ ", "", stormr_trim(line))
          if (nchar(title) > 120) title <- paste0(substr(title, 1, 117), "...")
          break
        }
      }

      list(
        kind = kind,
        text = stormr_trim(as.character(md)),
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
storm_fetch_url_text <- function(url, user_agent = NULL) {
  url <- storm_normalize_url(url)
  if (is.na(url)) stormr_abort("Invalid URL.")

  # Use ragnar's read_as_markdown when available (preferred)
  if (stormr_has("ragnar")) {
    result <- storm_fetch_url_markdown(url)
    # Return in same format as legacy, but include title
    return(list(
      kind = result$kind,
      text = result$text,
      title = result$title,
      error = result$error
    ))
  }

  # Legacy fallback when ragnar is not available
  resp <- storm_http_get(url, user_agent = user_agent)
  ctype <- httr2::resp_header(resp, "content-type") %||% ""

  if (grepl("application/pdf", ctype, fixed = TRUE) || grepl("\\.pdf($|\\?)", url, ignore.case = TRUE)) {
    if (!stormr_has("pdftools")) {
      return(list(kind = "pdf", text = NA_character_, title = NA_character_, error = "pdftools not installed; cannot parse PDF."))
    }
    bin <- httr2::resp_body_raw(resp)
    tmp <- tempfile(fileext = ".pdf")
    writeBin(bin, tmp)
    txt <- paste(pdftools::pdf_text(tmp), collapse = "\n\n")
    unlink(tmp)
    return(list(kind = "pdf", text = stormr_trim(txt), title = NA_character_, error = NULL))
  }

  html <- httr2::resp_body_string(resp)
  list(kind = "html", text = storm_html_to_text(html), title = NA_character_, error = NULL)
}

#' @keywords internal
storm_wikipedia_api <- function(params) {
  base <- "https://en.wikipedia.org/w/api.php"
  req <- httr2::request(base) |>
    httr2::req_url_query(!!!params) |>
    httr2::req_user_agent("stormr (R; Wikipedia API)")
  resp <- httr2::req_perform(req)
  httr2::resp_body_json(resp, simplifyVector = TRUE)
}

#' @keywords internal
storm_wiki_search <- function(query, limit = 8) {
  res <- storm_wikipedia_api(list(
    action = "query",
    list = "search",
    srsearch = query,
    srlimit = limit,
    format = "json",
    utf8 = 1
  ))
  hits <- res$query$search
  if (is.null(hits) || length(hits) == 0) {
    return(tibble::tibble(title = character(), url = character(), snippet = character()))
  }
  tibble::tibble(
    title = hits$title,
    url = paste0("https://en.wikipedia.org/wiki/", gsub(" ", "_", hits$title)),
    snippet = gsub("<[^>]+>", "", hits$snippet)
  )
}

#' @keywords internal
storm_wiki_page_plaintext <- function(title) {
  res <- storm_wikipedia_api(list(
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
  if (is.null(pages)) return(NA_character_)
  # pages is a named list keyed by pageid
  page <- pages[[1]]
  page$extract %||% NA_character_
}

#' StormRetriever
#'
#' Provides web and Wikipedia retrieval with caching, plus helper methods to
#' register retrieval tools with ellmer chats. Optionally integrates with
#' ragnar for semantic search capabilities.
#'
#' @field config A `StormConfig` object.
#' @field store A `SourceStore` object.
#' @field ragnar_store A ragnar store for semantic retrieval (optional).
#' @field cache_dir Path to cache directory.
#'
#' @export
StormRetriever <- R6::R6Class(
  "StormRetriever",
  public = list(
    config = NULL,
    store = NULL,
    ragnar_store = NULL,
    cache_dir = NULL,

    #' @description
    #' Create a new StormRetriever.
    #' @param config A `StormConfig` object.
    #' @param store A `SourceStore` object.
    initialize = function(config = storm_config(), store = SourceStore$new()) {
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

      key <- stormr_cache_key("search", provider, query, k)
      cached <- stormr_cache_get(self$cache_dir, key)
      if (!is.null(cached)) return(cached)

      out <- switch(
        provider,
        wikipedia = storm_wiki_search(query, limit = k),
        serper = storm_search_serper(query, k = k),
        brave = storm_search_brave(query, k = k),
        tavily = storm_search_tavily(query, k = k),
        stormr_abort("Unknown search provider {.val {provider}}.")
      )

      # Normalize and add deterministic source ids
      out$url <- purrr::map_chr(out$url, storm_normalize_url)
      out$source_id <- purrr::map_chr(out$url, storm_source_id)

      stormr_cache_set(self$cache_dir, key, out)
      out
    },

    #' @description
    #' Fetch and cache content from a URL.
    #' @param url The URL to fetch.
    #' @param force If TRUE, bypass cache.
    #' @param perspective Optional perspective name for ragnar metadata.
    #' @return A source object.
    fetch = function(url, force = FALSE, perspective = NA_character_) {
      url <- storm_normalize_url(url)
      if (is.na(url)) stormr_abort("Invalid URL.")

      key <- stormr_cache_key("fetch", url)
      cached <- if (!force) stormr_cache_get(self$cache_dir, key) else NULL
      if (!is.null(cached)) {
        self$store$upsert_source(cached)
        return(cached)
      }

      res <- storm_fetch_url_text(url, user_agent = self$config$user_agent)
      fetched_at <- stormr_now_utc()

      if (!is.null(res$error)) {
        src <- storm_source(
          url = url,
          title = NA_character_,
          snippet = NA_character_,
          content_text = NA_character_,
          fetched_at = fetched_at,
          content_hash = NA_character_,
          meta = list(kind = res$kind, error = res$error)
        )
        self$store$upsert_source(src)
        stormr_cache_set(self$cache_dir, key, src)
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
        title <- stormr_trim(first)
        if (nchar(title) > 120) title <- paste0(substr(title, 1, 117), "...")
      }

      snippet <- if (!is.na(txt) && nzchar(txt)) substr(txt, 1, 300) else NA_character_

      src <- storm_source(
        url = url,
        title = title,
        snippet = snippet,
        content_text = txt,
        fetched_at = fetched_at,
        content_hash = txt_hash,
        meta = list(kind = res$kind, error = NULL)
      )
      self$store$upsert_source(src)
      stormr_cache_set(self$cache_dir, key, src)

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
    #' @param source_id The stormr source ID.
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
      if (is.null(self$ragnar_store)) return(invisible(NULL))
      stormr_require("ragnar", "RAG capabilities require the ragnar package.")

      # Create markdown document and chunk it
      doc <- ragnar::MarkdownDocument(text, origin = url)
      chunks <- ragnar::markdown_chunk(
        doc,
        target_size = 512,
        target_overlap = 0.25
      )

      # Add stormr metadata to chunks
      n_chunks <- nrow(chunks)
      chunks$source_id <- rep(source_id, n_chunks)
      chunks$url <- rep(url, n_chunks)
      chunks$title <- rep(title %||% NA_character_, n_chunks)
      chunks$fetched_at <- rep(fetched_at, n_chunks)
      chunks$content_type <- rep(content_type %||% NA_character_, n_chunks)
      chunks$perspective <- rep(perspective %||% NA_character_, n_chunks)

      # Add context for better retrieval (source info prepended)
      chunks$context <- paste0(
        "[Source: ", source_id, "] ",
        ifelse(!is.na(title), paste0(title, " - "), "")
      )

      # Insert into ragnar store
      ragnar::ragnar_store_insert(self$ragnar_store, chunks)

      invisible(n_chunks)
    },

    #' @description
    #' Build the ragnar store index for faster retrieval.
    build_ragnar_index = function() {
      if (is.null(self$ragnar_store)) return(invisible(NULL))
      stormr_require("ragnar", "RAG capabilities require the ragnar package.")
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
        stormr_abort("No ragnar store configured. Use embed_fn in storm_config().")
      }
      stormr_require("ragnar", "RAG capabilities require the ragnar package.")
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
      if (is.null(src)) return(NA_character_)
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

#' Create a StormRetriever
#'
#' @param config A `StormConfig`.
#' @param store A `SourceStore`.
#' @return A `StormRetriever`.
#' @export
storm_retriever <- function(config = storm_config(), store = SourceStore$new()) {
  StormRetriever$new(config = config, store = store)
}

# --- Provider-specific search helpers (API-key based) -----------------------

#' @keywords internal
storm_search_serper <- function(query, k = 8, api_key = Sys.getenv("SERPER_API_KEY", "")) {
  if (identical(api_key, "")) {
    stormr_abort("SERPER_API_KEY is not set. Set it to use provider = 'serper'.")
  }
  req <- httr2::request("https://google.serper.dev/search") |>
    httr2::req_headers(`X-API-KEY` = api_key, `Content-Type` = "application/json") |>
    httr2::req_body_json(list(q = query, num = k))
  resp <- httr2::req_perform(req)
  body <- httr2::resp_body_json(resp, simplifyVector = TRUE)
  items <- body$organic
  if (is.null(items) || length(items) == 0) {
    return(tibble::tibble(title = character(), url = character(), snippet = character()))
  }
  tibble::tibble(
    title = items$title %||% character(),
    url = items$link %||% character(),
    snippet = items$snippet %||% character()
  )
}

#' @keywords internal
storm_search_brave <- function(query, k = 8, api_key = Sys.getenv("BRAVE_API_KEY", "")) {
  if (identical(api_key, "")) {
    stormr_abort("BRAVE_API_KEY is not set. Set it to use provider = 'brave'.")
  }
  req <- httr2::request("https://api.search.brave.com/res/v1/web/search") |>
    httr2::req_headers(`Accept` = "application/json", `X-Subscription-Token` = api_key) |>
    httr2::req_url_query(q = query, count = k)
  resp <- httr2::req_perform(req)
  body <- httr2::resp_body_json(resp, simplifyVector = TRUE)
  items <- body$web$results
  if (is.null(items) || length(items) == 0) {
    return(tibble::tibble(title = character(), url = character(), snippet = character()))
  }
  tibble::tibble(
    title = items$title %||% character(),
    url = items$url %||% character(),
    snippet = items$description %||% character()
  )
}

#' @keywords internal
storm_search_tavily <- function(query, k = 8, api_key = Sys.getenv("TAVILY_API_KEY", "")) {
  if (identical(api_key, "")) {
    stormr_abort("TAVILY_API_KEY is not set. Set it to use provider = 'tavily'.")
  }
  req <- httr2::request("https://api.tavily.com/search") |>
    httr2::req_headers(`Content-Type` = "application/json") |>
    httr2::req_body_json(list(api_key = api_key, query = query, max_results = k))
  resp <- httr2::req_perform(req)
  body <- httr2::resp_body_json(resp, simplifyVector = TRUE)
  items <- body$results
  if (is.null(items) || length(items) == 0) {
    return(tibble::tibble(title = character(), url = character(), snippet = character()))
  }
  tibble::tibble(
    title = items$title %||% character(),
    url = items$url %||% character(),
    snippet = items$content %||% character()
  )
}
