# Shared helpers for the tempest Shiny app.

`%||%` <- rlang::`%||%`

# Is an optional package available?
has_pkg <- function(pkg) {
  requireNamespace(pkg, quietly = TRUE)
}

# The search providers offered in the config panel.
search_provider_choices <- function() {
  c(
    "native",
    "wikipedia",
    "you",
    "bing",
    "serper",
    "brave",
    "duckduckgo",
    "tavily",
    "searxng",
    "google",
    "azure_ai_search"
  )
}

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
  if (is.null(text)) {
    return("")
  }
  text <- as.character(text)
  has_text <- !is.na(text) & nzchar(text)
  if (!any(has_text)) {
    return(text)
  }
  out <- text
  text <- text[has_text]
  pua_open <- intToUtf8(0xE200)
  pua_sep <- intToUtf8(0xE202)
  pua_close <- intToUtf8(0xE201)
  pua_range <- paste0("[", intToUtf8(0xE000), "-", intToUtf8(0xF8FF), "]*")
  text <- gsub(
    paste0(
      pua_open,
      "cite",
      pua_sep,
      "[^",
      pua_close,
      "\n]*(",
      pua_close,
      ")?"
    ),
    "",
    text,
    perl = TRUE
  )
  text <- gsub(
    paste0(
      pua_range,
      "cite",
      pua_range,
      "turn[0-9]+(search|view|fetch|image|news|source)[0-9]+",
      "(",
      pua_range,
      "turn[0-9]+",
      "(search|view|fetch|image|news|source)[0-9]+)*",
      pua_range
    ),
    "",
    text,
    perl = TRUE
  )
  text <- gsub("[ \t]+([.,;:!?])", "\\1", text, perl = TRUE)
  text <- gsub("([^\n])[ \t]{2,}([^\n])", "\\1 \\2", text, perl = TRUE)
  text <- gsub("[ \t]+\n", "\n", text, perl = TRUE)
  out[has_text] <- gsub("\n{3,}", "\n\n", text, perl = TRUE)
  out
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

  headings <- which(grepl("^##+\\s+References\\s*$", lines, perl = TRUE))
  if (length(headings) > 0) {
    heading <- headings[[length(headings)]]
    meaningful <- integer()
    if (heading < length(lines)) {
      after <- seq.int(heading + 1L, length(lines))
      meaningful <- after[nzchar(trimws(lines[after]))]
    }
    if (length(meaningful) > 0 && all(footnotes[meaningful])) {
      keep <- seq_len(heading - 1L)
      return(paste(lines[keep], collapse = "\n"))
    }
  }

  paste(lines[!footnotes], collapse = "\n")
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

# Credits shown in the navbar "About" popover: the papers and upstream repos
# tempest is based on.
about_content <- function() {
  link <- function(text, href) shiny::a(text, href = href, target = "_blank")
  shiny::div(
    style = "max-width: 320px;",
    shiny::p(
      shiny::strong("tempest"),
      " ports Stanford's STORM and Co-STORM research workflows to R."
    ),
    shiny::p(class = "mb-1 fw-semibold", "Papers"),
    shiny::tags$ul(
      class = "mb-2 ps-3",
      shiny::tags$li(link(
        "STORM (NAACL 2024)",
        "https://arxiv.org/abs/2402.14207"
      )),
      shiny::tags$li(link(
        "Co-STORM (EMNLP 2024)",
        "https://arxiv.org/abs/2408.15232"
      )),
      shiny::tags$li(link(
        "DSPy (ICLR 2024)",
        "https://arxiv.org/abs/2310.03714"
      ))
    ),
    shiny::p(class = "mb-1 fw-semibold", "Code"),
    shiny::tags$ul(
      class = "mb-0 ps-3",
      shiny::tags$li(link(
        "stanford-oval/storm",
        "https://github.com/stanford-oval/storm"
      )),
      shiny::tags$li(link(
        "stanfordnlp/dspy",
        "https://github.com/stanfordnlp/dspy"
      )),
      shiny::tags$li(link(
        "JamesHWade/dsprrr",
        "https://github.com/JamesHWade/dsprrr"
      )),
      shiny::tags$li(link(
        "JamesHWade/tempest",
        "https://github.com/JamesHWade/tempest"
      ))
    )
  )
}

# Navbar item: an info icon that opens the About popover.
about_nav_item <- function() {
  trigger <- shiny::tags$a(
    href = "#",
    class = "nav-link",
    role = "button",
    shiny::icon("circle-info"),
    shiny::span("About", class = "visually-hidden")
  )
  bslib::nav_item(
    bslib::popover(trigger, title = "About tempest", about_content())
  )
}

# Centered placeholder for empty tab content.
empty_state <- function(icon_name, message) {
  shiny::div(
    class = "text-center text-muted py-5",
    shiny::icon(icon_name, class = "fa-3x mb-3"),
    shiny::p(message)
  )
}

evidence_table_header <- function(
  ns,
  title,
  description,
  icon_name,
  count_id
) {
  shiny::div(
    class = "tempest-evidence-heading",
    shiny::div(
      class = "tempest-evidence-heading-copy",
      shiny::span(
        class = "tempest-evidence-heading-icon",
        `aria-hidden` = "true",
        shiny::icon(icon_name)
      ),
      shiny::div(
        shiny::h2(class = "h5 mb-1", title),
        shiny::p(class = "mb-0 text-muted small", description)
      )
    ),
    shiny::span(
      class = "badge rounded-pill tempest-evidence-count",
      shiny::textOutput(ns(count_id), inline = TRUE)
    )
  )
}

tempest_app_styles <- function() {
  shiny::tags$style(shiny::HTML(
    "
.tempest-chat-icon {
  display: block;
  width: 100%;
  height: 100%;
}

.tempest-inline-icon {
  width: 1rem;
  height: 1rem;
  vertical-align: -0.125em;
}

.tempest-chat-layout {
  min-width: 0;
}

.tempest-chat-card {
  min-width: 0;
  overflow: hidden;
}

.tempest-chat-card shiny-chat-container {
  --shiny-chat-greeting-max-width: 680px;
}

.tempest-chat-welcome {
  display: grid;
  gap: 1.25rem;
}

.tempest-chat-welcome-copy {
  max-width: 38rem;
}

.tempest-chat-welcome-copy p {
  line-height: 1.55;
}

.tempest-chat-welcome-form {
  display: grid;
  gap: .75rem;
  min-width: 0;
}

.tempest-chat-welcome-actions {
  --tempest-chat-welcome-control-height: 2.375rem;
  display: grid;
  grid-template-columns: 8rem max-content minmax(9rem, 1fr);
  align-items: flex-end;
  gap: .65rem;
  min-width: 0;
}

.tempest-chat-welcome-form .form-group,
.tempest-chat-research-options .form-group,
.tempest-chat-footer-actions .form-group {
  margin-bottom: 0;
}

.tempest-chat-welcome-form .control-label {
  margin-bottom: .2rem;
  color: var(--bs-secondary-color, #596771);
  font-size: .76rem;
  font-weight: 650;
}

.tempest-chat-welcome-experts {
  min-width: 8rem;
}

.tempest-chat-welcome-topic .shiny-input-container,
.tempest-chat-welcome-experts .shiny-input-container {
  width: 100%;
}

.tempest-chat-welcome-experts .form-select,
.tempest-chat-expert-setup,
.tempest-chat-welcome-tools .bslib-toolbar-input-button,
.tempest-chat-start {
  height: var(--tempest-chat-welcome-control-height);
  min-height: var(--tempest-chat-welcome-control-height);
}

.tempest-chat-expert-setup {
  width: 100%;
  overflow: hidden;
  justify-content: flex-start;
  text-overflow: ellipsis;
  white-space: nowrap;
}

.tempest-chat-start {
  white-space: nowrap;
}

.tempest-chat-welcome-tools {
  min-height: var(--tempest-chat-welcome-control-height);
  padding: 0;
}

.tempest-chat-welcome-tools .bslib-toolbar-input-button[data-type='icon'] {
  width: var(--tempest-chat-welcome-control-height);
}

.tempest-chat-research-options {
  display: grid;
  gap: .65rem;
  min-width: 17rem;
}

.tempest-custom-expert-builder {
  display: grid;
  gap: .85rem;
}

.tempest-custom-expert-grid {
  display: grid;
  grid-template-columns: repeat(2, minmax(0, 1fr));
  gap: .85rem;
}

.tempest-custom-expert-card {
  display: grid;
  gap: .7rem;
  min-width: 0;
  padding: .85rem;
  border: 1px solid var(--bs-border-color, #dee2e6);
  border-radius: var(--bs-border-radius-lg, .5rem);
  background: var(--bs-tertiary-bg, #f7f9fa);
}

.tempest-custom-expert-heading {
  display: flex;
  align-items: center;
  gap: .55rem;
}

.tempest-custom-expert-identity {
  display: grid;
  grid-template-columns: repeat(2, minmax(0, 1fr));
  gap: .65rem;
}

.tempest-custom-expert-card .form-group {
  margin-bottom: 0;
}

.tempest-chat-footer-idle {
  display: none;
}

.tempest-chat-footer {
  width: 100%;
  max-width: 680px;
  margin-inline: auto;
}

.tempest-chat-settings {
  border-left-color: var(--bs-border-color, #dee2e6);
  background: var(--bs-tertiary-bg, #f7f9fa);
}

.tempest-chat-config {
  margin-top: .75rem;
  padding-top: .75rem;
  border-top: 1px solid var(--bs-border-color, #dee2e6);
}

.tempest-chat-footer-actions {
  flex: 0 0 auto;
  flex-wrap: wrap;
}

.tempest-persona-icon {
  --tempest-persona-bg: #4a90a4;
  --tempest-persona-fg: #fff;
  display: inline-flex;
  align-items: center;
  justify-content: center;
  flex: 0 0 auto;
  width: 2rem;
  height: 2rem;
  border-radius: 50%;
  background: var(--tempest-persona-bg);
  color: var(--tempest-persona-fg);
  font-size: .72rem;
  font-weight: 700;
  line-height: 1;
  text-transform: uppercase;
}

.tempest-persona-icon-sm {
  width: 1.35rem;
  height: 1.35rem;
  font-size: .55rem;
}

.tempest-persona-icon-1 { --tempest-persona-bg: #4a90a4; }
.tempest-persona-icon-2 { --tempest-persona-bg: #6c63a8; }
.tempest-persona-icon-3 { --tempest-persona-bg: #2f855a; }
.tempest-persona-icon-4 { --tempest-persona-bg: #b7791f; }
.tempest-persona-icon-5 { --tempest-persona-bg: #b83280; }
.tempest-persona-icon-6 { --tempest-persona-bg: #2b6cb0; }

.tempest-activity-item {
  display: inline-flex;
  align-items: center;
  gap: .35rem;
  min-width: 0;
}

.tempest-activity-label {
  overflow-wrap: anywhere;
}

.tempest-citation {
  display: inline-flex;
  align-items: center;
  justify-content: center;
  min-width: 1.45em;
  margin: 0 .08rem;
  padding: .02rem .26rem;
  border-radius: 999px;
  border: 1px solid rgba(74, 144, 164, .35);
  background: rgba(74, 144, 164, .10);
  color: #285f70;
  font-size: .72em;
  font-weight: 700;
  line-height: 1.35;
  text-decoration: none;
  vertical-align: super;
}

.tempest-citation:hover,
.tempest-citation:focus {
  background: rgba(74, 144, 164, .18);
  color: #1f4d5a;
  outline: 2px solid rgba(74, 144, 164, .30);
  outline-offset: 1px;
}

.tempest-citation-missing {
  border-color: rgba(176, 42, 55, .35);
  background: rgba(176, 42, 55, .10);
  color: #842029;
}

.tempest-reference-panel {
  display: grid;
  gap: .75rem;
  margin-top: 2rem;
  padding-top: 1rem;
  border-top: 1px solid var(--bs-border-color, #dee2e6);
}

.tempest-reference-heading {
  margin-bottom: .25rem;
}

.tempest-reference-item {
  display: grid;
  gap: .35rem;
  padding: .75rem;
  border: 1px solid var(--bs-border-color, #dee2e6);
  border-radius: 8px;
  background: var(--bs-body-bg, #fff);
}

.tempest-reference-item:target {
  border-color: #4a90a4;
  box-shadow: 0 0 0 .2rem rgba(74, 144, 164, .18);
}

.tempest-reference-missing {
  border-color: rgba(176, 42, 55, .30);
  background: rgba(176, 42, 55, .04);
}

.tempest-reference-topline {
  display: flex;
  gap: .5rem;
  align-items: baseline;
  min-width: 0;
}

.tempest-reference-number {
  flex: 0 0 auto;
  color: #285f70;
  font-weight: 700;
}

.tempest-reference-title {
  overflow-wrap: anywhere;
}

.tempest-reference-meta,
.tempest-reference-snippet,
.tempest-reference-actions {
  font-size: .9rem;
}

.tempest-reference-meta {
  color: var(--bs-secondary-color, #6c757d);
}

.tempest-reference-snippet {
  margin-bottom: 0;
}

.tempest-reference-actions {
  display: flex;
  flex-wrap: wrap;
  gap: .75rem;
}

.tempest-mindmap-kpis {
  margin-bottom: .75rem;
}

.tempest-mindmap-kpi {
  min-height: 150px;
}

.tempest-mindmap-card {
  min-height: 620px;
  overflow: hidden;
}

.tempest-mindmap-heading {
  display: grid;
  gap: .2rem;
  padding: .85rem 1rem;
}

.tempest-mindmap-heading > .shiny-html-output {
  min-height: 1.5rem;
  font-weight: 600;
}

.tempest-mindmap-canvas {
  position: relative;
  min-height: 0;
  background: var(--bs-body-bg, #fff);
}

.tempest-mindmap-visualization,
.tempest-mindmap-visualization .vis-network {
  min-height: 520px;
}

.tempest-mindmap-outline {
  overflow: hidden;
  border: 1px solid var(--bs-border-color, #dee2e6);
  border-radius: var(--bs-border-radius-lg, .5rem);
  background: var(--bs-body-bg, #fff);
}

.tempest-mindmap-outline > summary {
  display: flex;
  align-items: center;
  justify-content: space-between;
  gap: 1rem;
  padding: .9rem 1rem;
  cursor: pointer;
  color: var(--bs-body-color, #15293a);
  font-weight: 600;
  list-style: none;
}

.tempest-mindmap-outline > summary::-webkit-details-marker {
  display: none;
}

.tempest-mindmap-outline > summary::after {
  content: '+';
  flex: 0 0 auto;
  color: var(--bs-primary, #4a90a4);
  font-size: 1.25rem;
  line-height: 1;
}

.tempest-mindmap-outline[open] > summary::after {
  content: '−';
}

.tempest-mindmap-outline > summary:hover,
.tempest-mindmap-outline > summary:focus-visible {
  background: var(--bs-tertiary-bg, #eef3f5);
}

.tempest-mindmap-outline-title {
  display: inline-flex;
  align-items: center;
  gap: .5rem;
}

.tempest-mindmap-outline-hint {
  margin-left: auto;
  color: var(--bs-secondary-color, #6c757d);
  font-size: .8rem;
  font-weight: 500;
}

.tempest-mindmap-outline-body {
  padding: 1rem;
  border-top: 1px solid var(--bs-border-color, #dee2e6);
}

.tempest-evidence-card {
  overflow: hidden;
}

.tempest-evidence-heading {
  display: flex;
  align-items: center;
  justify-content: space-between;
  gap: 1rem;
  padding: .15rem 0;
}

.tempest-evidence-heading-copy {
  display: flex;
  align-items: center;
  gap: .8rem;
  min-width: 0;
}

.tempest-evidence-heading-icon {
  display: inline-flex;
  align-items: center;
  justify-content: center;
  flex: 0 0 auto;
  width: 2.25rem;
  height: 2.25rem;
  border-radius: 50%;
  background: rgba(74, 144, 164, .12);
  color: var(--bs-primary, #4a90a4);
}

.tempest-evidence-count {
  flex: 0 0 auto;
  border: 1px solid rgba(74, 144, 164, .3);
  background: rgba(74, 144, 164, .1);
  color: var(--bs-body-color, #15293a);
  font-weight: 600;
}

.tempest-evidence-table .dataTables_wrapper {
  padding: 1rem;
}

.tempest-table-toolbar,
.tempest-table-footer {
  display: flex;
  align-items: center;
  justify-content: space-between;
  gap: .75rem;
}

.tempest-table-toolbar {
  flex-wrap: wrap;
  margin-bottom: .85rem;
}

.tempest-table-footer {
  flex-wrap: wrap;
  margin-top: .85rem;
}

.tempest-table-search .dataTables_filter,
.tempest-table-actions .dt-buttons,
.tempest-table-footer .dataTables_info,
.tempest-table-footer .dataTables_paginate {
  float: none;
  margin: 0;
}

.tempest-table-search label {
  display: flex;
  align-items: center;
  margin: 0;
}

.tempest-table-search input[type='search'] {
  min-width: min(22rem, 62vw);
  margin: 0;
  padding: .45rem .7rem;
  border: 1px solid var(--bs-border-color, #ced4da);
  border-radius: var(--bs-border-radius, .5rem);
  background: var(--bs-body-bg, #fff);
  color: var(--bs-body-color, #15293a);
}

.tempest-table-search input[type='search']:focus {
  border-color: var(--bs-primary, #4a90a4);
  outline: 0;
  box-shadow: 0 0 0 .2rem rgba(74, 144, 164, .18);
}

.tempest-table-actions .dt-buttons {
  display: flex;
  gap: .4rem;
}

.tempest-evidence-table .tempest-table-actions .dt-buttons .btn,
.tempest-table-actions .dt-button {
  flex: 0 0 auto;
  width: auto;
  min-width: 0;
  margin: 0;
  padding: .4rem .7rem;
  border: 1px solid var(--bs-border-color, #ced4da);
  border-radius: var(--bs-border-radius, .5rem);
  background: var(--bs-body-bg, #fff);
  color: var(--bs-body-color, #15293a);
  font-size: .82rem;
  font-weight: 600;
  line-height: 1.3;
}

.tempest-evidence-table .tempest-table-actions .dt-buttons .btn:hover,
.tempest-evidence-table .tempest-table-actions .dt-buttons .btn:focus,
.tempest-table-actions .dt-button:hover,
.tempest-table-actions .dt-button:focus {
  border-color: var(--bs-primary, #4a90a4);
  background: rgba(74, 144, 164, .1);
  color: var(--bs-body-color, #15293a);
}

.tempest-evidence-table table.dataTable {
  width: 100% !important;
  margin: 0 !important;
  border-collapse: collapse !important;
}

.tempest-evidence-table table.dataTable thead th {
  padding: .65rem .75rem;
  border-top: 1px solid var(--bs-border-color, #dee2e6);
  border-bottom: 1px solid var(--bs-border-color, #dee2e6);
  background: var(--bs-tertiary-bg, #eef3f5);
  color: var(--bs-secondary-color, #596771);
  font-size: .72rem;
  font-weight: 700;
  letter-spacing: .055em;
  text-transform: uppercase;
  white-space: nowrap;
}

.tempest-evidence-table table.dataTable tbody td {
  padding: .8rem .75rem;
  border-color: var(--bs-border-color, #dee2e6);
  vertical-align: top;
}

.tempest-evidence-table table.dataTable tbody tr:hover > * {
  background: rgba(74, 144, 164, .055);
}

.tempest-col-primary,
.tempest-col-wrap {
  white-space: normal !important;
  overflow-wrap: anywhere;
}

.tempest-col-primary {
  min-width: 18rem;
  font-weight: 500;
}

.tempest-col-wrap {
  min-width: 14rem;
  line-height: 1.45;
}

.tempest-col-secondary {
  white-space: nowrap;
  color: var(--bs-secondary-color, #596771);
}

.tempest-col-mono {
  color: var(--bs-secondary-color, #596771);
  font-family: var(--bs-font-monospace, monospace);
  font-size: .76rem;
  white-space: nowrap;
}

.tempest-source-cell {
  display: grid;
  gap: .2rem;
}

.tempest-source-title {
  color: var(--bs-body-color, #15293a);
  font-weight: 600;
  text-decoration: none;
}

a.tempest-source-title:hover,
a.tempest-source-title:focus {
  color: var(--bs-primary, #4a90a4);
  text-decoration: underline;
}

.tempest-source-location {
  color: var(--bs-secondary-color, #596771);
  font-size: .78rem;
  font-weight: 400;
}

.tempest-evidence-badge {
  display: inline-flex;
  align-items: center;
  padding: .28rem .5rem;
  border: 1px solid transparent;
  border-radius: 999px;
  font-size: .75rem;
  font-weight: 700;
  line-height: 1;
  white-space: nowrap;
}

.tempest-evidence-badge-success {
  border-color: var(--bs-success-border-subtle, rgba(47, 133, 90, .28));
  background: var(--bs-success-bg-subtle, rgba(47, 133, 90, .12));
  color: var(--bs-success-text-emphasis, #246b48);
}

.tempest-evidence-badge-warning {
  border-color: var(--bs-warning-border-subtle, rgba(183, 121, 31, .32));
  background: var(--bs-warning-bg-subtle, rgba(244, 183, 64, .18));
  color: var(--bs-warning-text-emphasis, #7a5318);
}

.tempest-evidence-badge-danger {
  border-color: var(--bs-danger-border-subtle, rgba(184, 50, 80, .28));
  background: var(--bs-danger-bg-subtle, rgba(184, 50, 80, .1));
  color: var(--bs-danger-text-emphasis, #943049);
}

.tempest-evidence-badge-neutral {
  border-color: var(--bs-border-color, #ced4da);
  background: var(--bs-tertiary-bg, #eef3f5);
  color: var(--bs-secondary-color, #596771);
}

@media (max-width: 575.98px) {
  .tempest-chat-welcome-actions {
    grid-template-areas:
      'experts'
      'tools'
      'start';
    grid-template-columns: minmax(0, 1fr);
  }

  .tempest-chat-welcome-experts {
    grid-area: experts;
    min-width: 0;
  }

  .tempest-chat-start {
    grid-area: start;
    width: 100%;
  }

  .tempest-chat-welcome-tools {
    grid-area: tools;
  }

  .tempest-custom-expert-grid,
  .tempest-custom-expert-identity {
    grid-template-columns: minmax(0, 1fr);
  }

  .tempest-chat-footer-active {
    align-items: stretch !important;
  }

  .tempest-chat-footer-actions {
    width: 100%;
    justify-content: flex-end;
    padding-right: 2.25rem;
  }

  .tempest-mindmap-card {
    height: 560px !important;
    min-height: 560px;
  }

  .tempest-mindmap-visualization,
  .tempest-mindmap-visualization .vis-network {
    min-height: 460px;
  }

  .tempest-mindmap-outline-hint {
    display: none;
  }

  .tempest-evidence-heading {
    align-items: flex-start;
  }

  .tempest-evidence-table .dataTables_wrapper {
    padding: .75rem;
  }

  .tempest-table-search,
  .tempest-table-actions,
  .tempest-table-search input[type='search'] {
    width: 100%;
  }
}
"
  ))
}

tempest_chat_icon <- function() {
  shiny::tags$img(
    src = tempest_logo_src(),
    alt = "tempest assistant",
    class = "tempest-chat-icon"
  )
}

tempest_inline_icon <- function(class = NULL) {
  shiny::tags$img(
    src = tempest_logo_src(),
    alt = "",
    class = paste(c("tempest-inline-icon", class), collapse = " ")
  )
}

tempest_logo_src <- function() {
  paste0(tempest_logo_resource_path(), "/tempest.svg")
}

tempest_logo_resource_path <- function(prefix = "tempest-logos") {
  logo_dir <- system.file("shiny", "logos", package = "tempest")
  if (!nzchar(logo_dir) || !dir.exists(logo_dir)) {
    return("logos")
  }

  target <- normalizePath(logo_dir, winslash = "/", mustWork = TRUE)
  paths <- shiny::resourcePaths()
  current <- if (prefix %in% names(paths)) {
    unname(paths[[prefix]])
  } else {
    NULL
  }
  if (!is.null(current)) {
    current <- normalizePath(current, winslash = "/", mustWork = FALSE)
  }
  if (!is.null(current) && !identical(current, target)) {
    suffix <- sum(utf8ToInt(target)) %% 100000L
    prefix <- paste0(prefix, "-", suffix)
    current <- if (prefix %in% names(paths)) {
      unname(paths[[prefix]])
    } else {
      NULL
    }
    if (!is.null(current)) {
      current <- normalizePath(current, winslash = "/", mustWork = FALSE)
    }
  }
  if (is.null(current)) {
    shiny::addResourcePath(prefix, target)
  }
  prefix
}

persona_icon <- function(name = NULL, id = NULL, size = c("md", "sm")) {
  size <- match.arg(size)
  label <- persona_icon_label(name, id)
  shiny::span(
    class = paste(
      c(
        "tempest-persona-icon",
        paste0("tempest-persona-icon-", persona_icon_variant(name, id)),
        if (identical(size, "sm")) "tempest-persona-icon-sm"
      ),
      collapse = " "
    ),
    role = "img",
    `aria-label` = label,
    title = label,
    persona_initials(name, id)
  )
}

persona_icon_label <- function(name = NULL, id = NULL) {
  name <- workflow_first_text(name)
  if (nzchar(name)) {
    return(paste("Expert", name))
  }
  id <- workflow_first_text(id)
  if (nzchar(id)) {
    return(paste("Expert", id))
  }
  "Expert"
}

persona_initials <- function(name = NULL, id = NULL) {
  text <- workflow_first_text(name, id, "Expert")
  parts <- unlist(strsplit(text, "[^[:alnum:]]+", perl = TRUE))
  parts <- parts[nzchar(parts)]
  if (length(parts) == 0L) {
    return("E")
  }
  initials <- paste0(substr(parts, 1, 1), collapse = "")
  toupper(substr(initials, 1, 2))
}

persona_icon_variant <- function(name = NULL, id = NULL) {
  key <- workflow_first_text(id, name, "expert")
  codepoints <- utf8ToInt(key)
  if (length(codepoints) == 0L) {
    return(1L)
  }
  (sum(codepoints) %% 6L) + 1L
}

workflow_progress_ui <- function(state, stage_labels = NULL) {
  if (is.null(state) || is.na(state$workflow)) {
    return(NULL)
  }
  status <- workflow_display_status(state)
  shiny::div(
    class = "tempest-progress py-2",
    shiny::div(
      class = "d-flex flex-wrap align-items-center gap-2 mb-2",
      shiny::span(
        class = paste(
          "badge rounded-pill",
          workflow_status_class(status)
        ),
        workflow_status_label(status)
      ),
      shiny::strong(workflow_title(state$workflow)),
      workflow_current_label(state)
    ),
    workflow_stage_list(state, stage_labels),
    workflow_activity_list(state),
    workflow_failure_list(state),
    workflow_artifact_list(state)
  )
}

workflow_display_status <- function(state) {
  if (
    identical(state$workflow, "costorm") &&
      identical(state$status, "running") &&
      !workflow_state_has_active(state)
  ) {
    return("ready")
  }
  state$status
}

workflow_state_has_active <- function(state) {
  active <- c(
    state$active$stages,
    state$active$steps,
    state$active$experts,
    state$active$tools
  )
  length(active) > 0L
}

workflow_status_label <- function(status) {
  switch(
    status %||% "pending",
    pending = "Queued",
    ready = "Ready",
    running = "Running",
    succeeded = "Done",
    failed = "Failed",
    cancelled = "Cancelled",
    stringi::stri_trans_totitle(status %||% "Pending")
  )
}

workflow_status_class <- function(status) {
  switch(
    status %||% "pending",
    pending = "text-bg-secondary",
    ready = "text-bg-secondary",
    running = "text-bg-primary",
    succeeded = "text-bg-success",
    failed = "text-bg-danger",
    cancelled = "text-bg-warning",
    "text-bg-secondary"
  )
}

workflow_title <- function(workflow) {
  switch(
    workflow %||% "",
    storm = "STORM",
    costorm = "Co-STORM",
    workflow %||% "Workflow"
  )
}

workflow_current_label <- function(state) {
  stage_labels <- workflow_labels(state$workflow, "stage")
  step_labels <- workflow_labels(state$workflow, "step")
  current <- workflow_stage_label(state$current_stage, stage_labels)
  step <- workflow_stage_label(state$current_step, step_labels)
  if (!is.na(step) && nzchar(step)) {
    current <- if (!is.na(current) && nzchar(current)) {
      paste(current, step, sep = " / ")
    } else {
      step
    }
  }
  if (is.na(current) || !nzchar(current)) {
    return(NULL)
  }
  shiny::span(class = "small text-muted", paste("Current:", current))
}

workflow_stage_list <- function(state, stage_labels = NULL) {
  if (is.null(stage_labels) || length(stage_labels) == 0L) {
    return(NULL)
  }
  items <- lapply(names(stage_labels), function(stage) {
    status <- workflow_stage_status(state, stage)
    shiny::span(
      class = paste("tempest-progress-stage", workflow_stage_class(status)),
      shiny::span(class = "me-1", workflow_stage_icon(status)),
      stage_labels[[stage]]
    )
  })
  shiny::div(class = "d-flex flex-wrap gap-2 small", items)
}

workflow_stage_status <- function(state, stage) {
  if (stage %in% state$completed_stages) {
    return("succeeded")
  }
  if (stage %in% state$skipped_stages) {
    return("skipped")
  }
  if (identical(state$current_stage, stage)) {
    return("running")
  }
  if (length(state$failures) > 0L) {
    failed <- vapply(
      state$failures,
      function(failure) identical(failure$stage, stage),
      logical(1)
    )
    if (any(failed)) {
      return("failed")
    }
  }
  "pending"
}

workflow_stage_class <- function(status) {
  switch(
    status,
    succeeded = "text-success",
    skipped = "text-secondary",
    running = "text-primary fw-semibold",
    failed = "text-danger fw-semibold",
    "text-muted"
  )
}

workflow_stage_icon <- function(status) {
  switch(
    status,
    succeeded = shiny::icon("circle-check"),
    skipped = shiny::icon("circle-minus"),
    running = shiny::icon("spinner", class = "fa-spin"),
    failed = shiny::icon("triangle-exclamation"),
    shiny::icon("circle")
  )
}

workflow_activity_list <- function(state) {
  active <- c(state$active$experts, state$active$tools, state$active$steps)
  if (length(active) == 0L) {
    return(NULL)
  }
  items <- lapply(
    active,
    function(item) workflow_activity_item(item, state$workflow)
  )
  items <- Filter(Negate(is.null), items)
  if (length(items) == 0L) {
    return(NULL)
  }
  shiny::div(
    class = "small text-muted mt-2 tempest-progress-activity",
    shiny::icon("arrows-rotate", class = "me-1"),
    shiny::span(class = "visually-hidden", "Active work:"),
    shiny::div(
      class = "d-inline-flex flex-wrap align-items-center gap-2",
      items
    )
  )
}

workflow_activity_item <- function(item, workflow = NULL) {
  label <- workflow_activity_label(item, workflow)
  if (is.na(label) || !nzchar(label)) {
    return(NULL)
  }
  expert_name <- item$expert_name %||% NA_character_
  expert_id <- item$expert_id %||% NA_character_
  has_expert <- workflow_has_text(expert_name) || workflow_has_text(expert_id)
  shiny::span(
    class = "tempest-activity-item",
    if (has_expert) {
      persona_icon(expert_name, expert_id, size = "sm")
    },
    shiny::span(class = "tempest-activity-label", label)
  )
}

workflow_activity_label <- function(item, workflow = NULL) {
  step_labels <- workflow_labels(workflow, "step")
  expert_name <- item$expert_name %||% NA_character_
  label <- if (!is.na(expert_name) && nzchar(expert_name)) {
    expert_name
  } else {
    workflow_first_text(item$step, item$stage)
  }
  label <- workflow_stage_label(label, step_labels)
  if (is.na(label) || !nzchar(label)) {
    return("")
  }
  if (identical(item$event_type, "tool") && !is.na(item$step)) {
    paste0(label, " running")
  } else {
    label
  }
}

workflow_has_text <- function(value) {
  !is.na(workflow_first_text(value)) && nzchar(workflow_first_text(value))
}

workflow_failure_list <- function(state) {
  if (length(state$failures) == 0L) {
    return(NULL)
  }
  # Don't surface recorded transient failures once the workflow has no active
  # work; the reducer keeps tool/expert failures in `state$failures`, which
  # would otherwise make a ready session look failed or stuck.
  if (
    identical(state$status, "succeeded") ||
      (!identical(state$status, "failed") && !workflow_state_has_active(state))
  ) {
    return(NULL)
  }
  latest <- state$failures[[length(state$failures)]]
  msg <- workflow_first_text(
    latest$error_message,
    latest$message,
    "Workflow failed."
  )
  shiny::div(
    class = "small text-danger mt-2",
    shiny::icon("triangle-exclamation", class = "me-1"),
    msg
  )
}

workflow_artifact_list <- function(state) {
  if (length(state$artifacts) == 0L) {
    return(NULL)
  }
  labels <- vapply(
    state$artifacts,
    function(artifact) workflow_stage_label(artifact$artifact),
    character(1)
  )
  shiny::div(
    class = "small text-success mt-2",
    shiny::icon("file-lines", class = "me-1"),
    paste("Available:", paste(labels, collapse = ", "))
  )
}

workflow_stage_label <- function(value, labels = NULL) {
  if (is.null(value) || length(value) == 0L || is.na(value)) {
    return(NA_character_)
  }
  value <- as.character(value[[1]])
  if (!is.null(labels) && value %in% names(labels)) {
    return(labels[[value]])
  }
  value <- gsub("_", " ", value)
  stringi::stri_trans_totitle(value)
}

workflow_labels <- function(workflow, kind = c("stage", "step")) {
  kind <- match.arg(kind)
  if (is.null(workflow) || length(workflow) == 0L) {
    return(NULL)
  }
  workflow <- workflow[[1]]
  if (is.na(workflow) || !workflow %in% c("storm", "costorm")) {
    return(NULL)
  }
  tryCatch(
    tempest::tempest_progress_labels(workflow, kind = kind),
    error = function(e) NULL
  )
}

workflow_first_text <- function(...) {
  values <- list(...)
  for (value in values) {
    if (is.null(value) || length(value) == 0L) {
      next
    }
    value <- as.character(value[[1]])
    if (!is.na(value) && nzchar(value)) {
      return(value)
    }
  }
  ""
}

table_missing_text <- function(x) {
  is.na(x) | !nzchar(trimws(as.character(x)))
}

table_compact_text <- function(x, max_chars = 300L, missing = "Not available") {
  vapply(
    as.character(x),
    function(value) {
      if (length(value) == 0L || is.na(value) || !nzchar(trimws(value))) {
        return(missing)
      }
      value <- gsub("\\s+", " ", trimws(value), perl = TRUE)
      if (nchar(value) <= max_chars) {
        return(value)
      }
      paste0(substr(value, 1L, max_chars - 3L), "...")
    },
    character(1)
  )
}

sources_table_data <- function(df) {
  if (is.null(df)) {
    return(NULL)
  }
  n <- nrow(df)
  if (!"context_text" %in% names(df)) {
    df$context_text <- rep(NA_character_, n)
  }
  if (!"snippet" %in% names(df)) {
    df$snippet <- rep(NA_character_, n)
  }

  context <- as.character(df$context_text)
  snippet <- as.character(df$snippet)
  content <- if ("content_text" %in% names(df)) {
    as.character(df$content_text)
  } else {
    rep(NA_character_, n)
  }

  missing_context <- table_missing_text(context)
  context[missing_context] <- content[missing_context]
  missing_context <- table_missing_text(context)
  context[missing_context] <- snippet[missing_context]

  missing_snippet <- table_missing_text(snippet)
  snippet[missing_snippet] <- table_compact_text(
    context[missing_snippet],
    missing = NA_character_
  )

  ids <- if ("id" %in% names(df)) {
    table_compact_text(df$id, max_chars = 80L)
  } else {
    rep("Not available", n)
  }
  urls <- if ("url" %in% names(df)) {
    as.character(df$url)
  } else {
    rep(NA_character_, n)
  }
  locators <- urls
  fallback_locators <- if ("locator" %in% names(df)) {
    as.character(df$locator)
  } else {
    rep(NA_character_, n)
  }
  missing_locator <- table_missing_text(locators)
  locators[missing_locator] <- fallback_locators[missing_locator]
  locations <- table_compact_text(locators, max_chars = 180L)

  titles <- if ("title" %in% names(df)) {
    as.character(df$title)
  } else {
    rep(NA_character_, n)
  }
  missing_title <- table_missing_text(titles)
  titles[missing_title] <- vapply(
    locations[missing_title],
    source_location_label,
    character(1)
  )
  missing_title <- table_missing_text(titles)
  titles[missing_title] <- ids[missing_title]

  kinds <- if ("resource_kind" %in% names(df)) {
    table_display_label(df$resource_kind)
  } else {
    rep("Not available", n)
  }
  fetched <- if ("fetched_at" %in% names(df)) {
    table_timestamp_text(df$fetched_at)
  } else {
    rep("Not available", n)
  }

  data.frame(
    Source = table_compact_text(titles, max_chars = 140L),
    Location = locations,
    `Evidence excerpt` = table_compact_text(
      table_plain_text(snippet),
      max_chars = 260L
    ),
    Type = kinds,
    Retrieved = fetched,
    `Source ID` = ids,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

facts_table_data <- function(df) {
  if (is.null(df)) {
    return(NULL)
  }
  n <- nrow(df)
  source_ids <- if ("source_ids" %in% names(df)) {
    vapply(
      df$source_ids,
      function(ids) {
        ids <- as.character(unlist(ids, use.names = FALSE))
        ids <- ids[!table_missing_text(ids)]
        if (length(ids) == 0L) {
          return("Not linked")
        }
        paste0(
          length(ids),
          " linked · ",
          paste(ids, collapse = ", ")
        )
      },
      character(1)
    )
  } else {
    rep("Not linked", n)
  }
  score <- if ("support_score" %in% names(df)) {
    suppressWarnings(as.numeric(df$support_score))
  } else {
    rep(NA_real_, n)
  }
  support <- ifelse(
    is.na(score),
    "Not scored",
    paste0(round(score * 100), "%")
  )
  claim_text <- if ("claim_text" %in% names(df)) {
    table_compact_text(table_plain_text(df$claim_text), max_chars = 500L)
  } else {
    rep("Not available", n)
  }
  claim_type <- if ("claim_type" %in% names(df)) {
    table_display_label(df$claim_type)
  } else {
    rep("Not available", n)
  }
  confidence <- if ("confidence" %in% names(df)) {
    table_display_label(df$confidence)
  } else {
    rep("Not available", n)
  }
  status <- if ("verification_status" %in% names(df)) {
    table_display_label(df$verification_status)
  } else {
    rep("Not available", n)
  }

  data.frame(
    Fact = claim_text,
    Sources = source_ids,
    Confidence = confidence,
    Status = status,
    Support = support,
    Type = claim_type,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

table_display_label <- function(x) {
  vapply(
    as.character(x),
    function(value) {
      if (is.na(value) || !nzchar(trimws(value))) {
        return("Not available")
      }
      value <- gsub("[._-]+", " ", trimws(value), perl = TRUE)
      tools::toTitleCase(tolower(value))
    },
    character(1)
  )
}

table_timestamp_text <- function(x) {
  values <- table_compact_text(x, max_chars = 40L)
  values <- sub(
    "^([0-9]{4}-[0-9]{2}-[0-9]{2})T",
    "\\1 ",
    values,
    perl = TRUE
  )
  sub("Z$", " UTC", values)
}

table_plain_text <- function(x) {
  values <- as.character(x)
  values <- gsub("\\*{1,3}|_{1,3}|`+", "", values, perl = TRUE)
  gsub("\\[([^]]+)\\]\\([^)]*\\)", "\\1", values, perl = TRUE)
}

source_location_label <- function(location) {
  if (is.na(location) || !nzchar(trimws(location))) {
    return("Not available")
  }
  safe <- citation_safe_url(location)
  if (nzchar(safe)) {
    return(sub("^https?://([^/]+).*$", "\\1", safe, perl = TRUE))
  }
  table_compact_text(location, max_chars = 64L)
}

source_table_links <- function(labels, locations, ids) {
  mapply(
    function(label, location, id) {
      label <- table_compact_text(label, max_chars = 140L)
      location_label <- source_location_label(location)
      escaped_label <- htmltools::htmlEscape(label)
      escaped_location <- htmltools::htmlEscape(location_label)
      safe <- citation_safe_url(location)
      title <- if (nzchar(safe)) {
        paste0(
          '<a class="tempest-source-title" href="',
          htmltools::htmlEscape(safe, attribute = TRUE),
          '" target="_blank" rel="noopener noreferrer">',
          escaped_label,
          '<span class="visually-hidden"> (opens in a new tab)</span></a>'
        )
      } else {
        paste0('<span class="tempest-source-title">', escaped_label, "</span>")
      }
      meta <- if (identical(location_label, "Not available")) {
        htmltools::htmlEscape(id)
      } else {
        escaped_location
      }
      paste0(
        '<div class="tempest-source-cell">',
        title,
        '<span class="tempest-source-location">',
        meta,
        "</span></div>"
      )
    },
    labels,
    locations,
    ids,
    USE.NAMES = FALSE
  )
}

evidence_status_badges <- function(values, palette) {
  palette <- match.arg(palette, c("confidence", "verification"))
  vapply(
    as.character(values),
    function(value) {
      key <- tolower(trimws(value %||% ""))
      tone <- if (identical(palette, "confidence")) {
        switch(
          key,
          high = "success",
          medium = "warning",
          low = "danger",
          "neutral"
        )
      } else {
        switch(
          key,
          supported = "success",
          verified = "success",
          disputed = "warning",
          pending = "warning",
          unsupported = "danger",
          rejected = "danger",
          "neutral"
        )
      }
      paste0(
        '<span class="tempest-evidence-badge tempest-evidence-badge-',
        tone,
        '">',
        htmltools::htmlEscape(value %||% "Not available"),
        "</span>"
      )
    },
    character(1)
  )
}

# A responsive DT datatable with the app's standard search and export options.
styled_datatable <- function(
  df,
  html_columns = character(),
  search_placeholder = "Search evidence",
  column_defs = list()
) {
  html_columns <- intersect(html_columns, names(df))
  escape <- if (length(html_columns) == 0L) {
    TRUE
  } else {
    setdiff(names(df), html_columns)
  }
  DT::datatable(
    df,
    escape = escape,
    extensions = c("Buttons", "Responsive"),
    options = list(
      dom = paste0(
        "<'tempest-table-toolbar'",
        "<'tempest-table-search'f>",
        "<'tempest-table-actions'B>>",
        "rt",
        "<'tempest-table-footer'ip>"
      ),
      buttons = list(
        list(extend = "csv", text = "Export CSV"),
        list(extend = "excel", text = "Export Excel")
      ),
      pageLength = 10,
      lengthChange = FALSE,
      responsive = TRUE,
      autoWidth = FALSE,
      orderClasses = FALSE,
      columnDefs = column_defs,
      language = list(
        search = "",
        searchPlaceholder = search_placeholder,
        info = "_START_–_END_ of _TOTAL_",
        infoEmpty = "No records",
        paginate = list(previous = "Previous", `next` = "Next")
      )
    ),
    rownames = FALSE,
    class = "compact hover row-border"
  )
}

# Slugify a topic for use in download filenames.
topic_slug <- function(topic) {
  if (is.null(topic) || is.na(topic) || !nzchar(topic)) {
    return("untitled")
  }
  slug <- tolower(topic)
  slug <- gsub("[^a-z0-9]+", "-", slug)
  slug <- gsub("^-+|-+$", "", slug)
  if (nchar(slug) > 40) {
    slug <- substr(slug, 1, 40)
  }
  slug
}

# Wrap report HTML in a standalone, styled document for download.
report_html_document <- function(body_html, title = "tempest Report") {
  paste0(
    '<!DOCTYPE html>\n<html lang="en">\n<head>\n',
    '<meta charset="UTF-8">\n',
    '<meta name="viewport" content="width=device-width, initial-scale=1.0">\n',
    '<meta http-equiv="Content-Security-Policy" content="default-src \'none\'; ',
    "style-src 'unsafe-inline'; img-src https: data:; base-uri 'none'; ",
    "form-action 'none'\">\n",
    "<title>",
    htmltools::htmlEscape(title),
    "</title>\n",
    "<style>\n",
    "body { max-width: 800px; margin: 2rem auto; padding: 0 1rem; ",
    'font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif; ',
    "line-height: 1.6; color: #333; }\n",
    "h1, h2, h3 { color: #4A90A4; }\n",
    "a { color: #4A90A4; }\n",
    "pre { background: #f5f5f5; padding: 1rem; border-radius: 4px; overflow-x: auto; }\n",
    "code { background: #f5f5f5; padding: 0.15em 0.3em; border-radius: 3px; }\n",
    "blockquote { border-left: 3px solid #4A90A4; margin-left: 0; padding-left: 1rem; color: #666; }\n",
    "table { border-collapse: collapse; width: 100%; margin: 1rem 0; }\n",
    "th, td { border: 1px solid #ddd; padding: 0.5rem; text-align: left; }\n",
    "th { background: #f5f5f5; }\n",
    "</style>\n</head>\n<body>\n",
    body_html,
    "\n</body>\n</html>"
  )
}

# Convert a mind map (list of nodes/edges) into visNetwork data frames.
mindmap_to_visnetwork <- function(mindmap) {
  nodes <- mindmap$nodes %||% list()
  edges <- mindmap$edges %||% list()
  if (length(nodes) == 0) {
    return(NULL)
  }

  parent_map <- list()
  for (nd in nodes) {
    if (!is.null(nd$parent)) {
      parent_map[[nd$id]] <- nd$parent
    }
  }
  node_level <- function(id) {
    level <- 1L
    cur <- id
    visited <- character()
    while (!is.null(parent_map[[cur]]) && !cur %in% visited) {
      visited <- c(visited, cur)
      level <- level + 1L
      cur <- parent_map[[cur]]
    }
    level
  }

  # Shade nodes from light blue toward the brand primary as evidence grows.
  color_for_count <- function(n) {
    frac <- min(n / 5, 1)
    r <- as.integer(200 - frac * (200 - 74))
    g <- as.integer(220 - frac * (220 - 144))
    b <- as.integer(240 - frac * (240 - 164))
    sprintf("#%02X%02X%02X", r, g, b)
  }

  node_tooltip <- function(nd, id) {
    notes_html <- if (!is.null(nd$notes) && nzchar(nd$notes)) {
      paste0("<p>", htmltools::htmlEscape(nd$notes), "</p>")
    } else {
      ""
    }
    src_count <- length(nd$source_ids %||% character())
    src_html <- if (src_count > 0) {
      paste0(
        "<p><em>Sources: ",
        htmltools::htmlEscape(paste(nd$source_ids, collapse = ", ")),
        "</em></p>"
      )
    } else {
      ""
    }
    paste0(
      "<b>",
      htmltools::htmlEscape(nd$label %||% id),
      "</b>",
      notes_html,
      src_html
    )
  }

  ids <- vapply(
    seq_along(nodes),
    function(i) nodes[[i]]$id %||% paste0("node_", i),
    character(1)
  )
  nodes_df <- data.frame(
    id = ids,
    label = vapply(
      seq_along(nodes),
      function(i) nodes[[i]]$label %||% ids[i],
      character(1)
    ),
    level = vapply(ids, node_level, integer(1)),
    color = vapply(
      nodes,
      function(nd) color_for_count(length(nd$source_ids %||% character())),
      character(1)
    ),
    title = vapply(
      seq_along(nodes),
      function(i) node_tooltip(nodes[[i]], ids[i]),
      character(1)
    ),
    stringsAsFactors = FALSE
  )

  edge_pairs <- vapply(
    edges,
    function(edge) paste(edge$from %||% "", edge$to %||% "", sep = "\r"),
    character(1)
  )
  hierarchy_edges <- Filter(
    Negate(is.null),
    lapply(nodes, function(node) {
      parent <- node$parent %||% NULL
      if (is.null(parent)) {
        return(NULL)
      }
      pair <- paste(parent, node$id, sep = "\r")
      if (pair %in% edge_pairs) {
        return(NULL)
      }
      list(
        from = parent,
        to = node$id,
        relation = "Contains",
        structural = TRUE
      )
    })
  )
  edges <- c(edges, hierarchy_edges)

  edges_df <- if (length(edges) > 0) {
    data.frame(
      from = vapply(edges, function(e) e$from %||% "", character(1)),
      to = vapply(edges, function(e) e$to %||% "", character(1)),
      label = "",
      title = vapply(
        edges,
        function(e) htmltools::htmlEscape(e$relation %||% "Related"),
        character(1)
      ),
      dashes = vapply(edges, function(e) isTRUE(e$structural), logical(1)),
      color = vapply(
        edges,
        function(e) if (isTRUE(e$structural)) "#B7C3CA" else "#8A9AA4",
        character(1)
      ),
      arrows = "to",
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(
      from = character(),
      to = character(),
      label = character(),
      title = character(),
      dashes = logical(),
      color = character(),
      arrows = character(),
      stringsAsFactors = FALSE
    )
  }

  list(nodes = nodes_df, edges = edges_df)
}
