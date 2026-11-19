tempest_researcher_library_child <- function(directory, id) {
  if (!rlang::is_string(id) || !grepl("^research-run-[a-fA-F0-9]+$", id)) {
    stop("Invalid report identifier.")
  }
  root <- normalizePath(directory, mustWork = TRUE)
  path <- file.path(root, id)
  if (!dir.exists(path) || fs::is_link(path)) {
    stop("Unavailable report.")
  }
  path <- normalizePath(path, mustWork = TRUE)
  if (!identical(dirname(path), root)) {
    stop("Unavailable report.")
  }
  path
}

tempest_researcher_library_metadata <- function(directory, id) {
  path <- tempest_researcher_library_child(directory, id)
  read_metadata <- function(name, limit) {
    file <- file.path(path, name)
    info <- fs::file_info(file, follow = FALSE)
    if (!isTRUE(info$type == "file" && info$size <= limit)) {
      stop("Unavailable report metadata.")
    }
    jsonlite::read_json(file)
  }
  manifest <- read_metadata("manifest.json", 1024^2)
  article <- read_metadata("report.json", 10 * 1024^2)
  if (
    !identical(manifest$schema_version, 1L) ||
      !manifest$status %in% c("complete", "partial") ||
      !identical(article$status, manifest$status) ||
      !rlang::is_string(article$title)
  ) {
    stop("Unrecognized report metadata.")
  }
  data.frame(
    id = id,
    title = article$title,
    summary = tempest_enriched_render_text(article$summary),
    status = manifest$status,
    created_at = tempest_enriched_render_text(manifest$created_at)
  )
}

tempest_researcher_library_catalog <- function(directory) {
  root <- normalizePath(directory, mustWork = TRUE)
  if (!dir.exists(root) || file.access(root, 4L) != 0L) {
    stop("Directory unavailable.")
  }
  ids <- list.files(root, pattern = "^research-run-[a-fA-F0-9]+$")
  records <- lapply(ids, function(id) {
    tryCatch(
      tempest_researcher_library_metadata(root, id),
      error = function(e) NULL
    )
  })
  valid <- Filter(Negate(is.null), records)
  rows <- if (length(valid)) {
    do.call(rbind, valid)
  } else {
    data.frame(
      id = character(),
      title = character(),
      summary = character(),
      status = character(),
      created_at = character()
    )
  }
  list(
    status = "ok",
    reports = rows[order(rows$created_at, decreasing = TRUE), , drop = FALSE],
    skipped = length(ids) - length(valid)
  )
}

tempest_researcher_library_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tags$details(
    class = "tempest-researcher-library",
    `aria-labelledby` = ns("title"),
    shiny::tags$summary(id = ns("title"), "Saved research reports"),
    shiny::textInput(
      ns("search"),
      "Search saved reports",
      placeholder = "Title, summary, or date"
    ),
    shiny::actionButton(ns("refresh"), "Refresh saved reports"),
    shiny::uiOutput(ns("status")),
    shiny::uiOutput(ns("reports"))
  )
}

tempest_researcher_library_server <- function(
  id,
  directory,
  on_open,
  refresh_token
) {
  shiny::moduleServer(id, function(input, output, session) {
    notice <- shiny::reactiveVal(NULL)
    catalog <- shiny::reactive({
      refresh_token()
      input$refresh
      tryCatch(
        tempest_researcher_library_catalog(directory),
        error = function(e) NULL
      )
    })
    filtered <- shiny::reactive({
      rows <- catalog()$reports
      query <- trimws(input$search %||% "")
      if (!length(rows$id) || !nzchar(query)) {
        return(rows)
      }
      matched <- grepl(
        tolower(query),
        tolower(paste(rows$title, rows$summary, rows$created_at)),
        fixed = TRUE
      )
      rows[matched, , drop = FALSE]
    })
    shiny::observeEvent(
      list(input$selected, input$refresh, input$search),
      notice(NULL)
    )
    output$status <- shiny::renderUI({
      value <- catalog()
      if (!is.null(notice())) {
        return(shiny::div(role = "alert", notice()))
      }
      if (is.null(value)) {
        return(shiny::div(role = "alert", "Saved reports are unavailable."))
      }
      count <- length(value$reports$id)
      shiny::div(
        role = "status",
        if (!count) {
          "No saved reports yet."
        } else {
          sprintf("%d saved report%s", count, if (count == 1L) "" else "s")
        },
        if (value$skipped) sprintf("(%d unavailable)", value$skipped)
      )
    })
    output$reports <- shiny::renderUI({
      rows <- filtered()
      if (!length(rows$id)) {
        if (length(catalog()$reports$id)) {
          return(shiny::p("No reports match this search."))
        }
        return(NULL)
      }
      labels <- sprintf(
        "%s (%s, %s)",
        rows$title,
        substr(rows$created_at, 1L, 10L),
        rows$status
      )
      shiny::tagList(
        shiny::selectInput(
          session$ns("selected"),
          "Choose a saved report",
          choices = stats::setNames(rows$id, labels),
          selectize = FALSE
        ),
        shiny::actionButton(session$ns("open"), "Open selected report")
      )
    })
    shiny::observeEvent(input$open, {
      id <- input$selected
      if (!rlang::is_string(id) || !id %in% filtered()$id) {
        notice("Choose a saved report before opening it.")
        return()
      }
      tryCatch(
        {
          path <- tempest_researcher_library_child(directory, id)
          on_open(tempest_read_enriched_report(path))
          notice(NULL)
        },
        error = function(e) {
          notice("This report could not be verified or opened.")
        }
      )
    })
  })
}
