# Shared helpers for the tempest Shiny app.

`%||%` <- rlang::`%||%`

# Is an optional package available?
has_pkg <- function(pkg) {
  requireNamespace(pkg, quietly = TRUE)
}

# The search providers offered in the config panel.
search_provider_choices <- function() {
  tempest:::tempest_search_provider_choices()
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

evidence_panel_empty_state <- function(icon_name, title, message) {
  shiny::div(
    class = "tempest-evidence-empty",
    role = "status",
    `aria-live` = "polite",
    shiny::div(
      class = "tempest-evidence-empty-icon",
      `aria-hidden` = "true",
      shiny::icon(icon_name)
    ),
    shiny::div(
      class = "tempest-evidence-empty-copy",
      shiny::h3(class = "h5 tempest-evidence-empty-title", title),
      shiny::p(class = "mb-0 text-muted", message)
    )
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

chat_settings_drawer_controller_ui <- function(
  drawer_id,
  title_id,
  trigger_ids,
  footer_id
) {
  config <- jsonlite::toJSON(
    list(
      drawerId = drawer_id,
      titleId = title_id,
      triggerIds = unname(as.character(trigger_ids)),
      footerId = footer_id
    ),
    auto_unbox = TRUE
  )
  config <- gsub("</", "<\\/", config, fixed = TRUE)

  shiny::tags$script(
    `data-tempest-settings-drawer-controller` = drawer_id,
    shiny::HTML(paste0(
      "\n",
      tempest:::tempest_read_text(
        tempest:::tempest_pkg_file("shiny", "www", "settings-drawer.js")
      ),
      "(",
      config,
      ");\n"
    ))
  )
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
