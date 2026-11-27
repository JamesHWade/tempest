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
  # Prefer rvest when available (xml2 is in Imports).
  if (tempest_has("rvest")) {
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

tempest_retriever_require_url <- function(url) {
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
  url
}

tempest_retriever_is_pdf <- function(url, content_type = "") {
  grepl("application/pdf", content_type, fixed = TRUE) ||
    grepl("\\.pdf($|\\?)", url, ignore.case = TRUE)
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
  url <- tempest_retriever_require_url(url)

  # Determine content type from URL extension
  kind <- if (tempest_retriever_is_pdf(url)) "pdf" else "html"

  result <- tryCatch(
    {
      resp <- tempest_http_get(url, user_agent = user_agent)
      ctype <- httr2::resp_header(resp, "content-type") %||% ""
      kind <- if (tempest_retriever_is_pdf(url, ctype)) "pdf" else "html"
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
  url <- tempest_retriever_require_url(url)

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

  if (tempest_retriever_is_pdf(url, ctype)) {
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
