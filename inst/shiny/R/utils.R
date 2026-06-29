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
markdown_ui <- function(md, store = NULL, include_references = FALSE) {
  if (!nzchar(md %||% "")) {
    return(NULL)
  }
  md <- citation_markdown(
    md,
    store = store,
    include_references = include_references
  )
  if (has_pkg("commonmark")) {
    shiny::HTML(commonmark::markdown_html(md))
  } else {
    shiny::pre(md)
  }
}

citation_markdown <- function(md, store = NULL, include_references = FALSE) {
  if (!nzchar(md %||% "")) {
    return("")
  }
  model <- citation_reference_model(md, store = store)
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

citation_reference_model <- function(md, store = NULL) {
  markdown <- citation_strip_tempest_footnotes(md)
  matches <- citation_marker_matches(markdown)
  ids <- unique(matches$id)
  source_store <- citation_source_store(store)
  references <- lapply(seq_along(ids), function(i) {
    citation_reference(ids[[i]], i, source_store)
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

citation_source_store <- function(store = NULL) {
  if (is.null(store)) {
    return(NULL)
  }
  if (inherits(store, "TempestRetriever")) {
    store <- store$store
  }
  if (inherits(store, "SourceStore")) {
    return(store)
  }
  NULL
}

citation_reference <- function(id, number, store = NULL) {
  source <- if (is.null(store)) {
    NULL
  } else {
    tryCatch(store$get_source(id), error = function(e) NULL)
  }
  known <- !is.null(source)
  url <- citation_text(source$url %||% "")
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

.tempest-expert-card {
  border-radius: 8px;
}

.tempest-activity-item {
  display: inline-flex;
  align-items: center;
  gap: .35rem;
  min-width: 0;
}

.tempest-activity-label {
  overflow-wrap: anywhere;
}

.tempest-chat-footer {
  display: flex;
  flex-wrap: wrap;
  align-items: center;
  justify-content: space-between;
  gap: .5rem;
  width: 100%;
}

.tempest-chat-footer-status {
  min-width: 0;
  flex: 1 1 18rem;
}

.tempest-chat-runtime,
.tempest-chat-footer-actions {
  display: flex;
  flex-wrap: wrap;
  align-items: center;
  gap: .35rem;
}

.tempest-chat-runtime {
  color: var(--bs-secondary-color, #6c757d);
}

.tempest-chat-runtime > span {
  display: inline-flex;
  align-items: center;
  gap: .25rem;
  white-space: nowrap;
}

.tempest-runtime-pill {
  padding: .12rem .45rem;
  border: 1px solid var(--bs-border-color, #dee2e6);
  border-radius: 999px;
  color: var(--bs-body-color, #212529);
  background: var(--bs-body-bg, #fff);
}

.tempest-footer-button {
  display: inline-flex;
  align-items: center;
  justify-content: center;
  width: 2rem;
  height: 2rem;
  padding: 0;
}

.tempest-footer-button > .fa,
.tempest-footer-button > .fas,
.tempest-footer-button > .far,
.tempest-footer-button > .fab {
  margin-right: 0;
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
  shiny::div(
    class = "tempest-progress py-2",
    shiny::div(
      class = "d-flex flex-wrap align-items-center gap-2 mb-2",
      shiny::span(
        class = paste(
          "badge rounded-pill",
          workflow_status_class(state$status)
        ),
        workflow_status_label(state$status)
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

workflow_status_label <- function(status) {
  switch(
    status %||% "pending",
    pending = "Queued",
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
  # Don't surface recorded failures once the workflow has succeeded; the
  # reducer keeps transient tool/expert failures in `state$failures`, which
  # would otherwise contradict the "Done" status badge.
  if (identical(state$status, "succeeded")) {
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

# A DT datatable with the app's standard export options.
styled_datatable <- function(df) {
  DT::datatable(
    df,
    escape = FALSE,
    extensions = "Buttons",
    options = list(
      dom = "Bfrtip",
      buttons = c("csv", "excel"),
      pageLength = 25,
      scrollX = TRUE
    ),
    rownames = FALSE
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

  edges_df <- if (length(edges) > 0) {
    data.frame(
      from = vapply(edges, function(e) e$from %||% "", character(1)),
      to = vapply(edges, function(e) e$to %||% "", character(1)),
      label = vapply(edges, function(e) e$relation %||% "", character(1)),
      arrows = "to",
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(
      from = character(),
      to = character(),
      label = character(),
      arrows = character(),
      stringsAsFactors = FALSE
    )
  }

  list(nodes = nodes_df, edges = edges_df)
}
