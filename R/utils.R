# tempest utility helpers
# Uses explicit namespacing for clarity and traceability

tempest_has <- function(pkg) {
  if (!rlang::is_string(pkg)) {
    tempest_abort(
      "{.arg pkg} must be a single string, not {.obj_type_friendly {pkg}}."
    )
  }
  rlang::is_installed(pkg)
}

tempest_abort <- function(
  message,
  ...,
  class = character(),
  .envir = rlang::caller_env()
) {
  cli::cli_abort(
    message,
    ...,
    class = tempest_condition_class(class),
    .envir = .envir,
    call = rlang::caller_env()
  )
}

tempest_rethrow_operation <- function(error, class = character()) {
  package_error <- inherits(error, "tempest_error")
  sensitive <- tempest_condition_chain_sensitive(error)
  if (package_error && !sensitive) {
    class(error) <- tempest_condition_class(c(class, class(error)))
    stop(error)
  }
  tempest_abort(
    "The operation failed.",
    class = unique(c(class, "tempest_operation_error", "tempest_error"))
  )
}

tempest_inform <- function(message, ..., .envir = rlang::caller_env()) {
  cli::cli_inform(message, ..., .envir = .envir)
}

tempest_warn <- function(message, ..., .envir = rlang::caller_env()) {
  cli::cli_warn(message, ..., .envir = .envir)
}

tempest_now_utc <- function() {
  format(Sys.time(), "%Y-%m-%dT%H:%M:%OS6Z", tz = "UTC")
}

tempest_trim <- function(x) {
  if (length(x) == 0L) {
    return(x)
  }
  stringi::stri_trim_both(x)
}

tempest_uuid <- function(prefix = "id") {
  raw <- paste0(
    prefix,
    "-",
    sprintf("%.0f", runif(1, 1e8, 1e9)),
    "-",
    sprintf("%.0f", Sys.time())
  )
  paste0(prefix, "_", digest::digest(raw, algo = "xxhash64"))
}

tempest_require <- function(pkg, why = NULL) {
  if (!rlang::is_string(pkg)) {
    tempest_abort(
      "{.arg pkg} must be a single string, not {.obj_type_friendly {pkg}}."
    )
  }

  if (!tempest_has(pkg)) {
    msg <- if (is.null(why)) {
      c(
        "Package {.pkg {pkg}} is required but not installed.",
        i = "Install it with: {.run install.packages(\"{pkg}\")}"
      )
    } else {
      c(
        "Package {.pkg {pkg}} is required: {why}",
        i = "Install it with: {.run install.packages(\"{pkg}\")}"
      )
    }
    tempest_abort(
      msg,
      class = c("tempest_missing_package_error", "tempest_error"),
      package = pkg,
      reason = why
    )
  }
  invisible(TRUE)
}

tempest_pkg_file <- function(...) {
  system.file(..., package = "tempest")
}

tempest_read_text <- function(path) {
  if (!rlang::is_string(path)) {
    tempest_abort(
      "{.arg path} must be a single string, not {.obj_type_friendly {path}}."
    )
  }

  if (!file.exists(path)) {
    tempest_abort("File does not exist: {.path {path}}")
  }
  paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

tempest_atomic_write_lines <- function(lines, path) {
  dir <- dirname(path)
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }
  tmp <- tempfile(tmpdir = dir, fileext = ".tmp")
  done <- FALSE
  on.exit(if (!done && file.exists(tmp)) unlink(tmp), add = TRUE)
  writeLines(lines, con = tmp, useBytes = TRUE)
  if (!file.rename(tmp, path)) {
    # Rename can fail across devices or on locked files; fall back to copy.
    if (!file.copy(tmp, path, overwrite = TRUE)) {
      tempest_abort("Could not write file: {.path {path}}")
    }
    unlink(tmp)
  }
  done <- TRUE
  invisible(path)
}

tempest_write_text <- function(path, text) {
  if (!rlang::is_string(path)) {
    tempest_abort(
      "{.arg path} must be a single string, not {.obj_type_friendly {path}}."
    )
  }
  if (!rlang::is_string(text)) {
    tempest_abort(
      "{.arg text} must be a single string, not {.obj_type_friendly {text}}."
    )
  }

  tempest_atomic_write_lines(text, path)
  invisible(path)
}

tempest_await_promise <- function(
  promise,
  timeout_s,
  on_timeout,
  on_error = stop,
  poll_s = 0.02,
  sleep_s = 0.01
) {
  state <- new.env(parent = emptyenv())
  state$resolved <- FALSE
  state$value <- NULL
  state$error <- NULL
  promises::then(
    promise,
    onFulfilled = function(result) {
      state$value <- result
      state$resolved <- TRUE
      invisible(result)
    },
    onRejected = function(condition) {
      state$error <- condition
      state$resolved <- TRUE
      invisible(NULL)
    }
  )
  deadline <- unname(proc.time()[["elapsed"]]) + timeout_s
  while (!state$resolved) {
    remaining <- deadline - unname(proc.time()[["elapsed"]])
    if (remaining <= 0) {
      return(on_timeout())
    }
    later::run_now(timeoutSecs = min(remaining, poll_s))
    if (!state$resolved && sleep_s > 0) {
      Sys.sleep(min(remaining, sleep_s))
    }
  }
  if (!is.null(state$error)) {
    return(on_error(state$error))
  }
  state$value
}

# Prompt loading

tempest_prompt <- function(name) {
  path <- tempest_pkg_file("prompts", paste0(name, ".md"))
  if (identical(path, "")) {
    tempest_abort(
      "Unknown prompt {.val {name}}. Prompt file not found in inst/prompts."
    )
  }
  tempest_read_text(path)
}

tempest_prompt_render <- function(name, ...) {
  tmpl <- tempest_prompt(name)
  glue::glue_data(list(...), tmpl, .open = "{{", .close = "}}")
}

# Shared safe URL validation and Markdown-to-HTML sanitization

tempest_safe_markdown_elements <- function() {
  c(
    "a",
    "blockquote",
    "br",
    "code",
    "del",
    "em",
    "h1",
    "h2",
    "h3",
    "h4",
    "h5",
    "h6",
    "hr",
    "li",
    "ol",
    "p",
    "pre",
    "section",
    "strong",
    "sup",
    "table",
    "tbody",
    "td",
    "th",
    "thead",
    "tr",
    "ul"
  )
}

tempest_safe_http_url <- function(value) {
  if (is.null(value) || length(value) != 1L || is.na(value)) {
    return(NULL)
  }
  if (is.factor(value)) {
    value <- as.character(value)
  }
  if (!is.atomic(value)) {
    return(NULL)
  }
  value <- as.character(value)
  if (
    !nzchar(value) ||
      grepl("[[:cntrl:][:space:]<>\\\\]", value, perl = TRUE) ||
      !grepl("^https?://", value, ignore.case = TRUE, perl = TRUE)
  ) {
    return(NULL)
  }
  parsed <- tryCatch(curl::curl_parse_url(value), error = function(error) NULL)
  if (
    is.null(parsed) ||
      !tolower(parsed$scheme %||% "") %in% c("http", "https") ||
      !nzchar(parsed$host %||% "") ||
      nzchar(parsed$user %||% "") ||
      nzchar(parsed$password %||% "")
  ) {
    return(NULL)
  }
  value
}

tempest_render_markdown_sanitized_html <- function(
  markdown,
  mode = c("strict", "unwrap"),
  footnote_prefix = "research-"
) {
  mode <- match.arg(mode)
  allowed <- tempest_safe_markdown_elements()
  rendered <- commonmark::markdown_html(
    markdown,
    footnotes = TRUE,
    extensions = TRUE
  )
  wrapper <- paste0(
    "<!doctype html><html><body><main id=\"tempest-markdown\">",
    rendered,
    "</main></body></html>"
  )
  document <- xml2::read_html(
    wrapper,
    options = c("RECOVER", "NOERROR", "NOWARNING", "NONET")
  )
  root <- xml2::xml_find_first(document, "//main[@id='tempest-markdown']")
  if (inherits(root, "xml_missing")) {
    return("")
  }

  if (identical(mode, "unwrap")) {
    xml2::xml_remove(xml2::xml_find_all(
      root,
      paste0(
        ".//script|.//style|.//iframe|.//object|.//embed|.//svg|.//math|",
        ".//template|.//form|.//input|.//button|.//textarea|.//select|",
        ".//img|.//audio|.//video|.//source|.//link|.//meta|.//base|",
        ".//comment()"
      )
    ))
    elements <- as.list(xml2::xml_find_all(root, ".//*"))
    for (element in rev(elements)) {
      if (!tolower(xml2::xml_name(element)) %in% allowed) {
        for (child in xml2::xml_contents(element)) {
          xml2::xml_add_sibling(element, child, .where = "before")
        }
        xml2::xml_remove(element)
      }
    }
    for (element in xml2::xml_find_all(root, ".//*")) {
      allowed_attributes <- switch(
        tolower(xml2::xml_name(element)),
        a = c("id", "href", "title"),
        ol = c("id", "start"),
        "id"
      )
      attributes <- xml2::xml_attrs(element)
      xml2::xml_attrs(element) <- attributes[
        names(attributes) %in% allowed_attributes
      ]
    }
    links <- xml2::xml_find_all(root, ".//a[@href]")
    href <- xml2::xml_attr(links, "href")
    safe <- grepl("^(https?://|#)", href, ignore.case = TRUE) &
      !grepl("[[:cntrl:]]", href)
    for (link in links[!safe]) {
      attributes <- xml2::xml_attrs(link)
      xml2::xml_attrs(link) <- attributes[names(attributes) != "href"]
    }
    return(paste(as.character(xml2::xml_contents(root)), collapse = "\n"))
  }

  nodes <- as.list(xml2::xml_find_all(root, ".//*"))
  for (node in rev(nodes)) {
    if (inherits(node, "xml_missing")) {
      next
    }
    name <- tolower(xml2::xml_name(node))
    if (!name %in% allowed) {
      xml2::xml_remove(node)
      next
    }
    original_href <- xml2::xml_attr(node, "href")
    footnote_href <- !is.na(original_href) &&
      grepl("^#fn(ref)?-[A-Za-z0-9_-]+$", original_href)
    identifier <- xml2::xml_attr(node, "id")
    footnote_id <- !is.na(identifier) &&
      grepl("^fn(ref)?-[A-Za-z0-9_-]+$", identifier)
    href <- if (identical(name, "a") && footnote_href) {
      sub("^#", paste0("#", footnote_prefix), original_href)
    } else if (identical(name, "a")) {
      tempest_safe_http_url(original_href)
    } else {
      NULL
    }
    attributes <- names(xml2::xml_attrs(node))
    for (attribute in attributes) {
      xml2::xml_attr(node, attribute) <- NULL
    }
    if (footnote_id) {
      xml2::xml_set_attr(node, "id", paste0(footnote_prefix, identifier))
    }
    if (!is.null(href)) {
      xml2::xml_set_attr(node, "href", href)
      if (footnote_href) {
        xml2::xml_set_attr(node, "aria-label", "Follow research citation")
      } else {
        xml2::xml_set_attr(node, "target", "_blank")
        xml2::xml_set_attr(node, "rel", "noopener noreferrer")
      }
    }
  }

  children <- xml2::xml_children(root)
  if (length(children) == 0L) {
    return("")
  }
  paste(as.character(children), collapse = "")
}
