# Markdown and interactive citation rendering for the Shiny app.

# Render markdown to HTML when commonmark is available, otherwise show the
# source text in a <pre> block.
markdown_ui <- function(md, workspace = NULL, include_references = FALSE) {
  if (!nzchar(md %||% "")) {
    return(NULL)
  }
  md <- citation_markdown(
    md,
    workspace = workspace,
    include_references = include_references
  )
  if (has_pkg("commonmark")) {
    shiny::HTML(commonmark::markdown_html(md))
  } else {
    shiny::pre(md)
  }
}

markdown_escape_raw_html <- function(md) {
  if (is.null(md)) {
    return("")
  }
  htmltools::htmlEscape(as.character(md), attribute = FALSE)
}

citation_markdown <- function(
  md,
  workspace = NULL,
  include_references = FALSE
) {
  if (!nzchar(md %||% "")) {
    return("")
  }
  md <- sanitize_external_citation_markers(md)
  md <- markdown_escape_raw_html(md)
  model <- citation_reference_model(md, workspace = workspace)
  if (nrow(model$matches) == 0) {
    return(model$markdown)
  }
  rendered <- citation_render_markers(model)
  if (isTRUE(include_references)) {
    rendered <- paste0(
      rendered,
      "\n\n",
      citation_reference_panel_markdown(model$references)
    )
  }
  rendered
}

sanitize_external_citation_markers <- function(text) {
  tempest:::tempest_shinychat_sanitize_text(text)
}

citation_reference_model <- function(md, workspace = NULL) {
  markdown <- citation_strip_tempest_footnotes(md)
  matches <- citation_marker_matches(markdown)
  ids <- unique(matches$id)
  workspace <- citation_workspace(workspace)
  references <- lapply(seq_along(ids), function(i) {
    citation_reference(ids[[i]], i, workspace)
  })
  names(references) <- ids
  list(markdown = markdown, matches = matches, references = references)
}

citation_strip_tempest_footnotes <- function(md) {
  lines <- strsplit(md, "\n", fixed = TRUE)[[1]]
  footnotes <- grepl("^\\[\\^S[0-9a-f]{12}\\]:", lines, perl = TRUE)
  if (!any(footnotes)) {
    return(md)
  }
  fenced <- logical(length(lines))
  fence_char <- NULL
  fence_length <- 0L
  for (index in seq_along(lines)) {
    trimmed <- trimws(lines[[index]])
    marker <- regmatches(
      trimmed,
      regexpr("^(`{3,}|~{3,})", trimmed, perl = TRUE)
    )
    if (is.null(fence_char)) {
      if (length(marker) == 1L && nzchar(marker)) {
        fence_char <- substr(marker, 1L, 1L)
        fence_length <- nchar(marker)
        fenced[[index]] <- TRUE
      }
      next
    }
    fenced[[index]] <- TRUE
    if (
      length(marker) == 1L &&
        nzchar(marker) &&
        identical(substr(marker, 1L, 1L), fence_char) &&
        nchar(marker) >= fence_length &&
        identical(marker, trimmed)
    ) {
      fence_char <- NULL
      fence_length <- 0L
    }
  }

  footnotes <- footnotes & !fenced
  is_heading <- !fenced & grepl("^\\s*#{1,6}\\s+", lines, perl = TRUE)
  heading_levels <- integer(length(lines))
  heading_levels[is_heading] <- nchar(
    sub("^\\s*(#+).*$", "\\1", lines[is_heading], perl = TRUE)
  )
  references <- which(
    is_heading &
      grepl(
        "^\\s*#{2,6}\\s+References\\s*#*\\s*$",
        lines,
        perl = TRUE,
        ignore.case = TRUE
      )
  )
  remove <- which(footnotes)
  for (heading in references) {
    level <- heading_levels[[heading]]
    next_heading <- which(
      is_heading &
        seq_along(lines) > heading &
        heading_levels <= level
    )
    section_end <- if (length(next_heading) == 0L) {
      length(lines)
    } else {
      min(next_heading) - 1L
    }
    section <- if (heading < section_end) {
      seq.int(heading + 1L, section_end)
    } else {
      integer()
    }
    if (
      any(footnotes[section]) &&
        all(footnotes[section] | !nzchar(trimws(lines[section])))
    ) {
      remove <- c(remove, heading)
    }
  }

  paste(lines[!seq_along(lines) %in% remove], collapse = "\n")
}

citation_marker_matches <- function(text) {
  rx <- gregexpr("\\[\\^?(S[0-9a-f]{12})\\]", text, perl = TRUE)
  starts <- as.integer(rx[[1]])
  if (length(starts) == 1 && starts[[1]] == -1L) {
    return(data.frame(
      id = character(),
      start = integer(),
      end = integer()
    ))
  }
  tokens <- regmatches(text, rx)[[1]]
  lens <- attr(rx[[1]], "match.length")
  data.frame(
    id = sub("^\\[\\^?(S[0-9a-f]{12})\\]$", "\\1", tokens),
    start = starts,
    end = starts + lens - 1L,
    stringsAsFactors = FALSE
  )
}

citation_workspace <- function(workspace = NULL) {
  if (is.null(workspace)) {
    return(NULL)
  }
  if (inherits(workspace, "TempestRetriever")) {
    workspace <- tryCatch(
      workspace[["workspace"]],
      error = function(e) NULL
    )
  }
  if (inherits(workspace, "ResearchWorkspace")) {
    return(workspace)
  }
  NULL
}

citation_reference <- function(id, number, workspace = NULL) {
  source <- if (is.null(workspace)) {
    NULL
  } else {
    tryCatch(workspace$get_retrieved_source(id), error = function(e) NULL)
  }
  known <- !is.null(source)
  url <- citation_safe_url(source$url %||% "")
  title <- citation_text(source$title %||% "")
  snippet <- citation_snippet(source$snippet %||% source$content_text %||% "")
  if (!nzchar(title)) {
    title <- if (known) paste("Source", id) else paste("Unknown source", id)
  }
  list(
    id = id,
    number = number,
    known = known,
    title = title,
    url = url,
    domain = citation_domain(url),
    snippet = snippet,
    provenance = if (known) "Tempest source" else "Missing source metadata"
  )
}

citation_safe_url <- function(url) {
  url <- citation_text(url)
  if (!nzchar(url) || grepl("[[:cntrl:]]", url)) {
    return("")
  }
  if (!grepl("^https?://", url, ignore.case = TRUE)) {
    return("")
  }
  url
}

citation_text <- function(x) {
  x <- x %||% ""
  if (length(x) == 0 || is.na(x[[1]])) {
    return("")
  }
  trimws(as.character(x[[1]]))
}

citation_snippet <- function(x, max_chars = 220) {
  x <- citation_text(x)
  x <- gsub("\\s+", " ", x, perl = TRUE)
  if (nchar(x) <= max_chars) {
    return(x)
  }
  paste0(substr(x, 1L, max_chars - 1L), "...")
}

citation_domain <- function(url) {
  if (!nzchar(url)) {
    return("")
  }
  domain <- sub("^https?://([^/?#]+).*$", "\\1", url)
  if (identical(domain, url)) "" else domain
}

citation_render_markers <- function(model) {
  matches <- model$matches
  pieces <- character()
  cursor <- 1L
  seen <- list()
  for (i in seq_len(nrow(matches))) {
    id <- matches$id[[i]]
    start <- matches$start[[i]]
    end <- matches$end[[i]]
    ref <- model$references[[id]]
    seen[[id]] <- (seen[[id]] %||% 0L) + 1L
    pieces <- c(
      pieces,
      substr(model$markdown, cursor, start - 1L),
      citation_marker_html(ref, seen[[id]])
    )
    cursor <- end + 1L
  }
  pieces <- c(pieces, substr(model$markdown, cursor, nchar(model$markdown)))
  paste0(pieces, collapse = "")
}

citation_marker_html <- function(ref, occurrence) {
  classes <- paste(
    c(
      "tempest-citation",
      if (!isTRUE(ref$known)) "tempest-citation-missing"
    ),
    collapse = " "
  )
  label <- if (isTRUE(ref$known)) {
    paste0("[", ref$number, "]")
  } else {
    paste0("[", ref$number, "?]")
  }
  preview <- citation_preview(ref)
  paste0(
    '<a id="tempest-cite-',
    citation_attr(ref$id),
    "-",
    occurrence,
    '" class="',
    citation_attr(classes),
    '" href="#tempest-ref-',
    citation_attr(ref$id),
    '" title="',
    citation_attr(preview),
    '" aria-label="Reference ',
    citation_attr(ref$number),
    ": ",
    citation_attr(preview),
    '">',
    citation_html(label),
    "</a>"
  )
}

citation_preview <- function(ref) {
  parts <- c(ref$title, ref$domain, ref$snippet, ref$provenance)
  parts <- parts[nzchar(parts)]
  paste(parts, collapse = " | ")
}

citation_reference_panel_markdown <- function(references) {
  if (length(references) == 0) {
    return("")
  }
  items <- vapply(
    references,
    citation_reference_item_html,
    character(1),
    USE.NAMES = FALSE
  )
  paste0(
    '<section class="tempest-reference-panel" aria-label="Cited references">',
    '<h2 class="tempest-reference-heading">References</h2>',
    paste(items, collapse = "\n"),
    "</section>"
  )
}

citation_reference_item_html <- function(ref) {
  cite_link <- paste0(
    '<a class="tempest-reference-backlink" href="#tempest-cite-',
    citation_attr(ref$id),
    '-1">Cited as [',
    citation_html(ref$number),
    "]</a>"
  )
  source_link <- if (nzchar(ref$url)) {
    paste0(
      '<a class="tempest-reference-open" href="',
      citation_attr(ref$url),
      '" target="_blank" rel="noopener noreferrer">Open source</a>'
    )
  } else {
    '<span class="tempest-reference-open text-muted">No URL</span>'
  }
  paste0(
    '<article id="tempest-ref-',
    citation_attr(ref$id),
    '" class="tempest-reference-item',
    if (!isTRUE(ref$known)) " tempest-reference-missing" else "",
    '" tabindex="-1">',
    '<div class="tempest-reference-topline">',
    '<span class="tempest-reference-number">[',
    citation_html(ref$number),
    "]</span>",
    '<strong class="tempest-reference-title">',
    citation_html(ref$title),
    "</strong>",
    "</div>",
    '<div class="tempest-reference-meta">',
    citation_html(citation_reference_meta(ref)),
    "</div>",
    if (nzchar(ref$snippet)) {
      paste0(
        '<p class="tempest-reference-snippet">',
        citation_html(ref$snippet),
        "</p>"
      )
    } else {
      ""
    },
    '<div class="tempest-reference-actions">',
    source_link,
    cite_link,
    "</div>",
    "</article>"
  )
}

citation_reference_meta <- function(ref) {
  parts <- c(ref$domain, ref$provenance)
  parts <- parts[nzchar(parts)]
  paste(parts, collapse = " | ")
}

citation_html <- function(x) {
  htmltools::htmlEscape(as.character(x), attribute = FALSE)
}

citation_attr <- function(x) {
  htmltools::htmlEscape(as.character(x), attribute = TRUE)
}
