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
markdown_ui <- function(md) {
  if (!nzchar(md %||% "")) {
    return(NULL)
  }
  if (has_pkg("commonmark")) {
    shiny::HTML(commonmark::markdown_html(md))
  } else {
    shiny::pre(md)
  }
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
  current <- workflow_stage_label(state$current_stage)
  step <- workflow_stage_label(state$current_step)
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
  labels <- vapply(active, workflow_activity_label, character(1))
  labels <- labels[nzchar(labels)]
  if (length(labels) == 0L) {
    return(NULL)
  }
  shiny::div(
    class = "small text-muted mt-2",
    shiny::icon("arrows-rotate", class = "me-1"),
    paste(labels, collapse = " · ")
  )
}

workflow_activity_label <- function(item) {
  label <- workflow_first_text(item$expert_name, item$step, item$stage)
  label <- workflow_stage_label(label)
  if (is.na(label) || !nzchar(label)) {
    return("")
  }
  if (identical(item$event_type, "tool") && !is.na(item$step)) {
    paste0(label, " running")
  } else {
    label
  }
}

workflow_failure_list <- function(state) {
  if (length(state$failures) == 0L) {
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

workflow_stage_label <- function(value) {
  if (is.null(value) || length(value) == 0L || is.na(value)) {
    return(NA_character_)
  }
  value <- as.character(value[[1]])
  value <- gsub("_", " ", value)
  stringi::stri_trans_totitle(value)
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
