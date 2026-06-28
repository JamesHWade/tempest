# Report tab: render the generated report and offer Markdown/HTML downloads.

mod_report_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::nav_panel(
    title = shiny::tagList(shiny::icon("file-lines"), "Report"),
    value = "Report",
    bslib::card(
      full_screen = TRUE,
      bslib::card_header(
        class = "d-flex justify-content-between align-items-center",
        shiny::span("Generated Report"),
        shiny::div(
          shiny::downloadButton(
            ns("download_md"),
            "Markdown",
            class = "btn-sm btn-outline-secondary me-1"
          ),
          shiny::downloadButton(
            ns("download_html"),
            "HTML",
            class = "btn-sm btn-outline-secondary"
          )
        )
      ),
      bslib::card_body(shiny::uiOutput(ns("body")))
    )
  )
}

mod_report_server <- function(id, store) {
  shiny::moduleServer(id, function(input, output, session) {
    report_md <- shiny::reactive(store$report() %||% "")
    report_store <- shiny::reactive({
      source_store <- store$report_store()
      if (!is.null(source_store)) {
        return(source_store)
      }
      ses <- store$get()
      if (is.null(ses)) {
        return(NULL)
      }
      citation_source_store(ses$store %||% NULL)
    })

    filename <- function(ext) {
      paste0("tempest-", topic_slug(store$report_topic()), ext)
    }

    output$body <- shiny::renderUI({
      md <- report_md()
      if (!nzchar(md)) {
        return(empty_state("file-lines", "Generate a report to view it here."))
      }
      markdown_ui(md, store = report_store(), include_references = TRUE)
    })

    output$download_md <- shiny::downloadHandler(
      filename = function() filename(".md"),
      content = function(file) {
        md <- report_md()
        shiny::req(nzchar(md))
        writeLines(md, file)
      },
      contentType = "text/markdown"
    )

    output$download_html <- shiny::downloadHandler(
      filename = function() filename(".html"),
      content = function(file) {
        md <- report_md()
        shiny::req(nzchar(md))
        rendered_md <- citation_markdown(
          md,
          store = report_store(),
          include_references = TRUE
        )
        body_html <- if (has_pkg("commonmark")) {
          commonmark::markdown_html(rendered_md)
        } else {
          paste0("<pre>", htmltools::htmlEscape(rendered_md), "</pre>")
        }
        writeLines(report_html_document(body_html), file)
      },
      contentType = "text/html"
    )
  })
}
