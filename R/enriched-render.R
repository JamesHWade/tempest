# Portable, evidence-preserving rendering for derived research enrichment.

tempest_enriched_render_text <- function(value, default = "") {
  if (is.null(value) || length(value) != 1L || is.na(value)) {
    return(default)
  }
  if (is.factor(value)) {
    value <- as.character(value)
  }
  if (!is.atomic(value)) {
    return(default)
  }
  as.character(value)
}

tempest_enriched_render_items <- function(value) {
  if (is.null(value)) {
    return(list())
  }
  if (is.data.frame(value)) {
    return(lapply(seq_len(nrow(value)), function(i) {
      as.list(value[i, , drop = FALSE])
    }))
  }
  if (is.list(value)) {
    return(value)
  }
  list()
}

tempest_enriched_render_safe_url <- function(value) {
  value <- tempest_enriched_render_text(value)
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

tempest_enriched_render_local_file <- function(directory, relative_path) {
  relative_path <- tempest_enriched_render_text(relative_path)
  if (
    !nzchar(relative_path) ||
      grepl("[[:cntrl:]\\\\]", relative_path, perl = TRUE) ||
      grepl("^[A-Za-z][A-Za-z0-9+.-]*:", relative_path, perl = TRUE) ||
      startsWith(relative_path, "/")
  ) {
    return(NULL)
  }
  parts <- strsplit(relative_path, "/", fixed = TRUE)[[1L]]
  if (any(parts %in% c("", ".", ".."))) {
    return(NULL)
  }
  root <- tryCatch(
    normalizePath(directory, winslash = "/", mustWork = TRUE),
    error = function(error) NULL
  )
  if (is.null(root)) {
    return(NULL)
  }
  path <- tryCatch(
    normalizePath(
      file.path(root, relative_path),
      winslash = "/",
      mustWork = TRUE
    ),
    error = function(error) NULL
  )
  if (is.null(path)) {
    return(NULL)
  }
  prefix <- if (identical(root, "/")) root else paste0(root, "/")
  if (!startsWith(path, prefix)) {
    return(NULL)
  }
  info <- file.info(path)
  if (is.na(info$isdir) || info$isdir) {
    return(NULL)
  }
  encoded <- vapply(
    parts,
    utils::URLencode,
    character(1),
    reserved = TRUE,
    USE.NAMES = FALSE
  )
  list(
    path = path,
    relative = relative_path,
    href = paste(encoded, collapse = "/")
  )
}

tempest_enriched_render_image_file <- function(directory, relative_path) {
  file <- tempest_enriched_render_local_file(directory, relative_path)
  if (is.null(file)) {
    return(NULL)
  }
  extension <- tolower(tools::file_ext(file$relative))
  if (!extension %in% c("png", "jpg", "jpeg", "webp")) {
    return(NULL)
  }
  file
}

tempest_enriched_render_markdown <- function(markdown) {
  markdown <- tempest_enriched_render_text(markdown)
  if (!nzchar(markdown)) {
    return(htmltools::tagList())
  }
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
    return(htmltools::tagList())
  }

  allowed <- c(
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
      sub("^#", "#research-", original_href)
    } else if (identical(name, "a")) {
      tempest_enriched_render_safe_url(original_href)
    } else {
      NULL
    }
    attributes <- names(xml2::xml_attrs(node))
    for (attribute in attributes) {
      xml2::xml_attr(node, attribute) <- NULL
    }
    if (footnote_id) {
      xml2::xml_set_attr(node, "id", paste0("research-", identifier))
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
    return(htmltools::tagList())
  }
  htmltools::HTML(paste(as.character(children), collapse = ""))
}

tempest_enriched_render_status_class <- function(status) {
  status <- tolower(tempest_enriched_render_text(status))
  if (status %in% c("succeeded", "failed", "skipped")) {
    return(status)
  }
  "recorded"
}

tempest_enriched_render_illustration_notice <- function() {
  "AI-generated illustration \u00b7 Not scientific evidence"
}

tempest_enriched_render_task_paths <- function(task) {
  paths <- task$files %||% character()
  if (is.list(paths)) {
    paths <- unlist(paths, use.names = FALSE)
  }
  if (!is.atomic(paths)) {
    return(character())
  }
  paths <- as.character(paths)
  paths[!is.na(paths) & nzchar(paths)]
}

tempest_enriched_render_file_link <- function(directory, relative_path) {
  file <- tempest_enriched_render_local_file(directory, relative_path)
  if (is.null(file)) {
    return(NULL)
  }
  htmltools::tags$a(
    class = "file-link",
    href = file$href,
    download = basename(file$relative),
    basename(file$relative)
  )
}

tempest_enriched_render_source_link <- function(source_id, sources) {
  source_id <- tempest_enriched_render_text(source_id)
  if (!nzchar(source_id) || nrow(sources) == 0L || !"id" %in% names(sources)) {
    return(htmltools::tags$span(class = "source-chip", source_id))
  }
  ids <- vapply(
    sources$id,
    tempest_enriched_render_text,
    character(1)
  )
  position <- match(source_id, ids)
  if (is.na(position)) {
    return(htmltools::tags$span(class = "source-chip", source_id))
  }
  htmltools::tags$a(
    class = "source-chip",
    href = paste0("#source-item-", position),
    source_id
  )
}

tempest_enriched_render_sources <- function(sources) {
  if (!is.data.frame(sources) || nrow(sources) == 0L) {
    return(htmltools::tags$p(
      class = "quiet-note",
      "No source records were included in this bundle."
    ))
  }
  id_column <- if ("id" %in% names(sources)) "id" else "source_id"
  if (!id_column %in% names(sources)) {
    return(htmltools::tags$p(
      class = "quiet-note",
      "Source records did not include displayable identifiers."
    ))
  }
  rows <- lapply(seq_len(nrow(sources)), function(i) {
    value <- function(column) {
      if (!column %in% names(sources)) {
        return("")
      }
      item <- sources[[column]][i]
      if (is.list(item)) {
        item <- item[[1L]]
      }
      tempest_enriched_render_text(item)
    }
    source_id <- value(id_column)
    title <- value("title")
    if (!nzchar(title)) {
      title <- source_id
    }
    url <- tempest_enriched_render_safe_url(value("url"))
    if (is.null(url)) {
      url <- tempest_enriched_render_safe_url(value("locator"))
    }
    snippet <- value("snippet")
    resource_kind <- value("resource_kind")
    fetched_at <- value("fetched_at")
    source_title <- if (is.null(url)) {
      htmltools::tags$span(title)
    } else {
      htmltools::tags$a(
        href = url,
        target = "_blank",
        rel = "noopener noreferrer",
        title
      )
    }
    htmltools::tags$article(
      id = paste0("source-item-", i),
      class = "source-card",
      htmltools::tags$div(
        class = "source-card-top",
        htmltools::tags$span(class = "source-id", source_id),
        if (nzchar(resource_kind)) {
          htmltools::tags$span(class = "source-kind", resource_kind)
        }
      ),
      htmltools::tags$h3(source_title),
      if (nzchar(snippet)) htmltools::tags$p(class = "source-snippet", snippet),
      if (nzchar(fetched_at)) {
        htmltools::tags$p(
          class = "source-date",
          htmltools::tags$span("Retrieved "),
          htmltools::tags$time(fetched_at)
        )
      }
    )
  })
  htmltools::tagList(rows)
}

tempest_enriched_render_analysis <- function(task, directory, sources) {
  title <- tempest_enriched_render_text(task$title, "Untitled analysis")
  status <- tempest_enriched_render_text(task$status, "unknown")
  status_class <- tempest_enriched_render_status_class(status)
  question <- tempest_enriched_render_text(task$question)
  code <- tempest_enriched_render_text(task$code)
  error <- tempest_enriched_render_text(task$error)
  source_ids <- task$source_ids %||% character()
  if (is.list(source_ids)) {
    source_ids <- unlist(source_ids, use.names = FALSE)
  }
  source_ids <- as.character(source_ids)
  source_ids <- source_ids[!is.na(source_ids) & nzchar(source_ids)]
  assumptions <- task$assumptions %||% character()
  if (is.list(assumptions)) {
    assumptions <- unlist(assumptions, use.names = FALSE)
  }
  assumptions <- as.character(assumptions)
  assumptions <- assumptions[!is.na(assumptions) & nzchar(assumptions)]
  source_chips <- lapply(
    source_ids,
    tempest_enriched_render_source_link,
    sources = sources
  )
  methods <- list()
  if (nzchar(question)) {
    methods <- c(methods, list(htmltools::tags$p(question)))
  }
  if (length(source_chips) > 0L) {
    methods <- c(
      methods,
      list(
        htmltools::tags$div(
          class = "source-chip-row",
          htmltools::tags$span(class = "method-label", "Sources"),
          htmltools::tagList(source_chips)
        )
      )
    )
  }
  if (length(assumptions) > 0L) {
    methods <- c(
      methods,
      list(
        htmltools::tags$div(
          class = "method-assumptions",
          htmltools::tags$span(class = "method-label", "Assumptions"),
          htmltools::tags$ul(lapply(assumptions, htmltools::tags$li))
        )
      )
    )
  }
  if (nzchar(code)) {
    methods <- c(
      methods,
      list(htmltools::tags$div(
        class = "code-wrap",
        htmltools::tags$div(
          class = "code-label",
          htmltools::tags$span("Analysis code"),
          htmltools::tags$span(class = "code-language", "R")
        ),
        htmltools::tags$pre(htmltools::tags$code(code))
      ))
    )
  }
  task_files <- tempest_enriched_render_task_paths(task)
  file_links <- Filter(
    Negate(is.null),
    lapply(task_files, tempest_enriched_render_file_link, directory = directory)
  )
  code_files <- task_files[grepl("\\.(r|py)$", task_files, ignore.case = TRUE)]
  code_download <- if (length(code_files) > 0L) {
    tempest_enriched_render_file_link(directory, code_files[[1L]])
  } else {
    NULL
  }
  figures <- Filter(
    Negate(is.null),
    lapply(
      task_files,
      tempest_enriched_render_image_file,
      directory = directory
    )
  )
  figure_cards <- lapply(figures, function(file) {
    alt_parts <- c("Chart produced for", title)
    if (nzchar(question)) {
      alt_parts <- c(alt_parts, question)
    }
    htmltools::tags$figure(
      class = "analysis-figure",
      htmltools::tags$img(
        src = file$href,
        alt = paste(alt_parts, collapse = ": "),
        loading = "lazy"
      ),
      htmltools::tags$figcaption(paste(title, "\u2014 Computed figure"))
    )
  })
  body <- list()
  if (identical(status, "failed")) {
    body <- c(
      body,
      list(htmltools::tags$div(
        class = "task-alert",
        role = "note",
        htmltools::tags$strong("This analysis did not complete."),
        if (nzchar(error)) htmltools::tags$p(error)
      ))
    )
  } else if (identical(status, "skipped")) {
    body <- c(
      body,
      list(htmltools::tags$p(
        class = "quiet-note",
        if (nzchar(error)) error else "This analysis was skipped."
      ))
    )
  } else {
    output <- tempest_enriched_render_text(task$output)
    if (length(figure_cards) > 0L) {
      body <- c(
        body,
        list(htmltools::tags$div(
          class = "analysis-figures",
          htmltools::tagList(figure_cards)
        ))
      )
    }
    if (nzchar(output)) {
      body <- c(
        body,
        list(htmltools::tags$details(
          class = "method-details analysis-output",
          htmltools::tags$summary("Observed console output"),
          htmltools::tags$pre(htmltools::tags$code(output))
        ))
      )
    }
  }
  details <- if (length(methods) > 0L) {
    htmltools::tags$details(
      class = "method-details",
      htmltools::tags$summary("Methods and code"),
      htmltools::tagList(methods),
      if (!is.null(code_download)) {
        htmltools::tags$p(
          class = "download-row",
          htmltools::tags$span(class = "method-label", "Download code"),
          code_download
        )
      }
    )
  } else {
    NULL
  }
  htmltools::tags$article(
    class = "analysis-card",
    htmltools::tags$div(
      class = "task-heading",
      htmltools::tags$div(
        htmltools::tags$p(class = "eyebrow", "Computational analysis"),
        htmltools::tags$h3(title)
      ),
      htmltools::tags$span(
        class = paste("status-pill", status_class),
        status
      )
    ),
    htmltools::tagList(body),
    details,
    if (length(file_links) > 0L) {
      htmltools::tags$div(
        class = "download-row task-downloads",
        htmltools::tags$span(class = "method-label", "Task files"),
        htmltools::tagList(file_links)
      )
    }
  )
}

tempest_enriched_render_outcome <- function(task, kind) {
  title <- tempest_enriched_render_text(task$title, paste("Untitled", kind))
  status <- tempest_enriched_render_text(task$status, "unknown")
  status_class <- tempest_enriched_render_status_class(status)
  error <- tempest_enriched_render_text(task$error)
  htmltools::tags$div(
    class = "outcome-row",
    htmltools::tags$div(
      htmltools::tags$p(class = "eyebrow", kind),
      htmltools::tags$h3(title),
      if (nzchar(error)) htmltools::tags$p(class = "outcome-note", error)
    ),
    htmltools::tags$span(
      class = paste("status-pill", status_class),
      status
    )
  )
}

tempest_enriched_render_illustration <- function(task, directory) {
  title <- tempest_enriched_render_text(task$title, "Untitled illustration")
  status <- tempest_enriched_render_text(task$status, "unknown")
  status_class <- tempest_enriched_render_status_class(status)
  error <- tempest_enriched_render_text(task$error)
  path <- tempest_enriched_render_text(task$file)
  file <- if (identical(status, "succeeded")) {
    tempest_enriched_render_image_file(directory, path)
  } else {
    NULL
  }
  content <- if (!is.null(file)) {
    htmltools::tags$figure(
      class = "illustration-figure",
      htmltools::tags$img(
        src = file$href,
        alt = tempest_enriched_render_text(task$alt),
        loading = "lazy"
      ),
      htmltools::tags$figcaption(
        htmltools::tags$span(
          class = "generated-label",
          tempest_enriched_render_illustration_notice()
        ),
        htmltools::tags$strong(title),
        if (nzchar(tempest_enriched_render_text(task$caption))) {
          htmltools::tags$p(tempest_enriched_render_text(task$caption))
        }
      )
    )
  } else if (identical(status, "failed")) {
    htmltools::tags$p(
      class = "task-alert",
      if (nzchar(error)) error else "Image generation failed."
    )
  } else if (identical(status, "skipped")) {
    htmltools::tags$p(
      class = "quiet-note",
      if (nzchar(error)) error else "This illustration was skipped."
    )
  } else {
    htmltools::tags$p(
      class = "quiet-note",
      "No bundled image file was available for this task."
    )
  }
  prompt <- tempest_enriched_render_text(task$prompt)
  metadata <- as.character(jsonlite::toJSON(
    task$metadata %||% list(),
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null"
  ))
  htmltools::tags$article(
    class = "illustration-card",
    htmltools::tags$div(
      class = "task-heading",
      htmltools::tags$h3(title),
      htmltools::tags$span(
        class = paste("status-pill", status_class),
        status
      )
    ),
    content,
    htmltools::tags$details(
      class = "method-details illustration-provenance",
      htmltools::tags$summary("Illustration prompt and provenance"),
      if (nzchar(prompt)) htmltools::tags$p(prompt),
      htmltools::tags$pre(htmltools::tags$code(metadata))
    )
  )
}

tempest_enriched_render_file_inventory <- function(directory) {
  paths <- list.files(
    directory,
    recursive = TRUE,
    all.files = FALSE,
    no.. = TRUE,
    full.names = FALSE,
    include.dirs = FALSE
  )
  paths <- gsub("\\\\", "/", paths)
  paths <- paths[!grepl("^index\\.html$", paths, ignore.case = TRUE)]
  files <- lapply(
    paths,
    tempest_enriched_render_local_file,
    directory = directory
  )
  files <- Filter(Negate(is.null), files)
  files[
    !vapply(
      files,
      function(file) identical(tolower(file$relative), "index.html"),
      logical(1)
    )
  ]
}

tempest_enriched_render_download_section <- function(directory, files) {
  manifest <- Filter(
    function(file) {
      tolower(basename(file$relative)) %in%
        c("manifest.json", "bundle-manifest.json")
    },
    files
  )
  if (length(manifest) == 0L) {
    manifest <- list(list(relative = "manifest.json", href = "manifest.json"))
  }
  code <- Filter(
    function(file) grepl("\\.(r|py)$", file$relative, ignore.case = TRUE),
    files
  )
  research <- Filter(
    function(file) {
      tolower(basename(file$relative)) %in%
        c("research.md", "original-research.md", "report.md")
    },
    files
  )
  categories <- list(
    list(title = "Manifest", files = manifest),
    list(title = "Original research", files = research),
    list(title = "Analysis code", files = code),
    list(
      title = "Other bundle files",
      files = Filter(
        function(file) {
          !file$relative %in%
            c(
              vapply(manifest, `[[`, character(1), "relative"),
              vapply(research, `[[`, character(1), "relative"),
              vapply(code, `[[`, character(1), "relative")
            )
        },
        files
      )
    )
  )
  cards <- lapply(categories, function(category) {
    if (length(category$files) == 0L) {
      return(NULL)
    }
    htmltools::tags$div(
      class = "download-group",
      htmltools::tags$h3(category$title),
      htmltools::tags$ul(lapply(category$files, function(file) {
        htmltools::tags$li(
          htmltools::tags$a(
            href = file$href,
            download = basename(file$relative),
            file$relative
          )
        )
      }))
    )
  })
  htmltools::tagList(cards)
}

tempest_enriched_render_styles <- function() {
  tempest_read_text(tempest_pkg_file("enriched", "report.css"))
}

tempest_enriched_render <- function(bundle, directory) {
  if (!is.list(bundle)) {
    tempest_enriched_abort(
      "`bundle` must be a plain list.",
      class = "tempest_input_error"
    )
  }
  if (
    !rlang::is_string(directory) ||
      is.na(directory) ||
      !dir.exists(directory)
  ) {
    tempest_enriched_abort(
      "`directory` must exist before rendering.",
      class = "tempest_input_error"
    )
  }
  title <- tempest_enriched_render_text(
    bundle$title,
    "Enriched research report"
  )
  summary <- tempest_enriched_render_text(bundle$summary)
  research_md <- tempest_enriched_render_text(bundle$research_md)
  status <- tempest_enriched_render_text(bundle$status, "recorded")
  sources <- bundle$sources
  if (!is.data.frame(sources)) {
    sources <- data.frame()
  }
  analyses <- tempest_enriched_render_items(bundle$analyses)
  illustrations <- tempest_enriched_render_items(bundle$illustrations)

  analysis_cards <- lapply(
    analyses,
    tempest_enriched_render_analysis,
    directory = directory,
    sources = sources
  )
  illustration_cards <- lapply(
    illustrations,
    tempest_enriched_render_illustration,
    directory = directory
  )
  inventory <- tempest_enriched_render_file_inventory(directory)
  source_count <- if (is.data.frame(sources)) nrow(sources) else 0L
  completed_analysis_count <- sum(vapply(
    analyses,
    function(task) {
      identical(tempest_enriched_render_text(task$status), "succeeded")
    },
    logical(1)
  ))
  task_outcomes <- c(
    lapply(analyses, tempest_enriched_render_outcome, kind = "Analysis"),
    lapply(
      illustrations,
      tempest_enriched_render_outcome,
      kind = "Illustration"
    )
  )

  nav_items <- list(
    c("Original research", "original-research"),
    c("Computational analysis", "analyses"),
    c("Illustrations", "illustrations"),
    c("Source library", "sources"),
    c("Limitations", "limitations"),
    c("Downloads", "downloads")
  )
  section_nav <- lapply(nav_items, function(item) {
    htmltools::tags$li(htmltools::tags$a(
      href = paste0("#", item[[2L]]),
      item[[1L]]
    ))
  })
  analysis_content <- if (length(analysis_cards) > 0L) {
    htmltools::tags$div(
      class = "analysis-list",
      htmltools::tagList(analysis_cards)
    )
  } else {
    htmltools::tags$p(
      class = "quiet-note",
      "No computational analyses were planned."
    )
  }
  illustration_content <- if (length(illustration_cards) > 0L) {
    htmltools::tags$div(
      class = "illustration-grid",
      htmltools::tagList(illustration_cards)
    )
  } else {
    htmltools::tags$p(
      class = "quiet-note",
      "No illustrations were planned."
    )
  }
  summary_content <- if (nzchar(summary)) {
    htmltools::tags$p(summary)
  } else {
    htmltools::tags$p(
      class = "quiet-note",
      "No proposed synthesis was supplied for this report."
    )
  }
  research_content <- tempest_enriched_render_markdown(research_md)
  if (length(research_content) == 0L) {
    research_content <- htmltools::tags$p(
      class = "quiet-note",
      "No original research Markdown was supplied for this report."
    )
  }
  source_content <- tempest_enriched_render_sources(sources)
  download_content <- tempest_enriched_render_download_section(
    directory,
    inventory
  )

  page <- htmltools::tags$html(
    lang = "en",
    htmltools::tags$head(
      htmltools::tags$meta(charset = "utf-8"),
      htmltools::tags$meta(
        name = "viewport",
        content = "width=device-width, initial-scale=1"
      ),
      htmltools::tags$meta(name = "color-scheme", content = "light"),
      htmltools::tags$title(title),
      htmltools::tags$style(htmltools::HTML(tempest_enriched_render_styles()))
    ),
    htmltools::tags$body(
      htmltools::tags$a(
        class = "skip-link",
        href = "#report-content",
        "Skip to report content"
      ),
      htmltools::tags$header(
        class = "masthead",
        htmltools::tags$span(class = "brand", "Tempest / Research dossier"),
        htmltools::tags$span(
          class = "edition",
          "Derived enrichment \u00b7 portable edition"
        )
      ),
      htmltools::tags$div(
        class = "page",
        htmltools::tags$header(
          class = "hero",
          htmltools::tags$div(
            htmltools::tags$p(class = "eyebrow", "Enriched research report"),
            htmltools::tags$h1(title),
            htmltools::tags$div(
              class = "hero-meta",
              htmltools::tags$span(
                class = paste(
                  "status-pill",
                  tempest_enriched_render_status_class(status)
                ),
                paste("Enrichment status", status)
              ),
              htmltools::tags$span(
                class = "meta-pill",
                paste(
                  source_count,
                  if (source_count == 1L) "source" else "sources"
                )
              ),
              htmltools::tags$span(
                class = "meta-pill",
                paste(completed_analysis_count, "completed analyses")
              )
            )
          )
        ),
        htmltools::tags$aside(
          class = "summary-band",
          htmltools::tags$div(
            htmltools::tags$span(class = "summary-label", "Interpretive layer"),
            htmltools::tags$h2("Proposed synthesis")
          ),
          summary_content
        ),
        htmltools::tags$div(
          class = "report-layout",
          htmltools::tags$nav(
            class = "rail",
            `aria-label` = "Report sections",
            htmltools::tags$p(class = "rail-label", "In this report"),
            htmltools::tags$ol(htmltools::tagList(section_nav))
          ),
          htmltools::tags$main(
            id = "report-content",
            tabindex = "-1",
            class = "content",
            htmltools::tags$section(
              id = "original-research",
              htmltools::tags$p(
                class = "section-kicker",
                "01 / Evidence record"
              ),
              htmltools::tags$h2("Original research"),
              htmltools::tags$div(class = "research-body", research_content)
            ),
            htmltools::tags$section(
              id = "analyses",
              htmltools::tags$p(class = "section-kicker", "02 / Derived work"),
              htmltools::tags$h2("Computational analyses"),
              analysis_content
            ),
            htmltools::tags$section(
              id = "illustrations",
              htmltools::tags$p(
                class = "section-kicker",
                "03 / Visual context"
              ),
              htmltools::tags$h2("Generated illustrations"),
              illustration_content
            ),
            htmltools::tags$section(
              id = "sources",
              htmltools::tags$p(class = "section-kicker", "04 / Source record"),
              htmltools::tags$h2("Source library"),
              htmltools::tags$div(class = "source-library", source_content)
            ),
            htmltools::tags$section(
              id = "limitations",
              htmltools::tags$p(class = "section-kicker", "05 / Reading guide"),
              htmltools::tags$h2("Limitations and task record"),
              htmltools::tags$div(
                class = "limitations",
                htmltools::tags$h3("How to read this report"),
                htmltools::tags$p(
                  "This report is a derived research aid. Its proposed synthesis, ",
                  "analyses, and illustrations do not replace or revise the original ",
                  "cited research record."
                ),
                htmltools::tags$p(
                  "A completed computation records that its supplied task ran; ",
                  "scientific interpretation remains conditional on its code, ",
                  "assumptions, and cited sources. Generated illustrations provide ",
                  "visual context and are not evidence."
                )
              ),
              if (length(task_outcomes) > 0L) {
                htmltools::tags$div(
                  class = "task-ledger",
                  htmltools::tags$h3("Task outcomes"),
                  htmltools::tagList(task_outcomes)
                )
              }
            ),
            htmltools::tags$section(
              id = "downloads",
              htmltools::tags$p(
                class = "section-kicker",
                "06 / Reproducibility"
              ),
              htmltools::tags$h2("Files and methods"),
              htmltools::tags$p(
                class = "quiet-note",
                "Keep this folder intact to preserve local figures, source files, ",
                "and the integrity manifest alongside this HTML report."
              ),
              htmltools::tags$div(
                class = "download-grid",
                download_content
              )
            )
          )
        ),
        htmltools::tags$footer(
          class = "report-footer",
          htmltools::tags$p(
            htmltools::tags$strong("Tempest"),
            " \u00b7 portable research enrichment. The source record and its citations ",
            "remain the publication authority."
          )
        )
      )
    )
  )
  output <- file.path(directory, "index.html")
  rendered <- htmltools::renderTags(page)
  html_body <- as.character(rendered$html)
  html_head <- as.character(rendered$head)
  root_match <- regexpr("<html\\b[^>]*>", html_body, perl = TRUE)
  if (as.integer(root_match) < 1L) {
    tempest_enriched_abort(
      "The HTML renderer did not return a document root.",
      class = "tempest_render_error"
    )
  }
  root_end <- as.integer(root_match) + attr(root_match, "match.length") - 1L
  html_body <- paste0(
    substr(html_body, 1L, root_end),
    "\n<head>\n",
    html_head,
    "\n</head>\n",
    substr(html_body, root_end + 1L, nchar(html_body))
  )
  html <- paste0("<!doctype html>\n", html_body)
  writeLines(enc2utf8(html), output, useBytes = TRUE)
  invisible(output)
}
