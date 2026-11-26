# Report tab: render the generated report and offer Markdown/HTML downloads.

mod_report_ui <- function(id, memory_ui = NULL) {
  ns <- shiny::NS(id)
  bslib::nav_panel(
    title = shiny::tagList(shiny::icon("file-lines"), "Report"),
    value = "Report",
    bslib::card(
      full_screen = TRUE,
      class = "tempest-evidence-card tempest-report-card",
      bslib::card_header(
        class = "tempest-report-header",
        shiny::div(
          class = "tempest-panel-heading",
          shiny::h2(
            id = ns("report_title"),
            class = "h4 mb-1",
            "Generated report"
          ),
          shiny::p(
            class = "mb-0 small text-muted",
            "A readable, citation-linked account of the evidence in this session."
          )
        ),
        shiny::div(
          class = "tempest-report-downloads",
          shiny::conditionalPanel(
            condition = "output.report_available",
            ns = ns,
            shiny::downloadButton(
              ns("download_md"),
              "Markdown",
              class = "btn-sm btn-outline-secondary",
              `aria-label` = "Download report as Markdown"
            ),
            shiny::downloadButton(
              ns("download_html"),
              "HTML",
              class = "btn-sm btn-outline-secondary",
              `aria-label` = "Download report as HTML"
            )
          )
        )
      ),
      bslib::card_body(
        class = "tempest-report-body",
        memory_ui,
        shiny::uiOutput(ns("body"))
      )
    )
  )
}

mod_report_server <- function(id, store) {
  shiny::moduleServer(id, function(input, output, session) {
    report_md <- shiny::reactive(store$report_md() %||% "")
    report_workspace <- shiny::reactive(store$report_workspace())
    report_topic <- shiny::reactive(store$report_topic() %||% "")

    evidence_counts <- function(workspace) {
      if (is.null(workspace)) {
        return(c(sources = 0L, facts = 0L))
      }
      count_rows <- function(reader) {
        n <- tryCatch(nrow(reader()), error = function(error) NA_integer_)
        if (is.null(n) || length(n) != 1L || is.na(n)) {
          return(NA_integer_)
        }
        as.integer(n)
      }
      c(
        sources = count_rows(function() {
          tempest:::tempest_workspace_sources(workspace)
        }),
        facts = count_rows(function() {
          tempest:::tempest_workspace_claims(workspace)
        })
      )
    }

    count_label <- function(value, singular, plural) {
      if (is.na(value)) {
        return(paste(plural, "unavailable"))
      }
      paste(value, if (identical(value, 1L)) singular else plural)
    }

    report_available <- shiny::reactive(nzchar(report_md()))
    output$report_available <- report_available
    shiny::outputOptions(
      output,
      "report_available",
      suspendWhenHidden = FALSE
    )

    filename <- function(ext) {
      paste0("tempest-", topic_slug(store$report_topic()), ext)
    }

    output$body <- shiny::renderUI({
      md <- report_md()
      if (!nzchar(md)) {
        return(evidence_panel_empty_state(
          "file-lines",
          "Your report will appear here",
          "Generate a report to view it here."
        ))
      }
      counts <- evidence_counts(report_workspace())
      shiny::tags$article(
        class = "tempest-report-reader",
        `aria-labelledby` = session$ns("report_title"),
        shiny::div(
          class = "tempest-report-reader-meta",
          if (nzchar(report_topic())) {
            shiny::p(
              class = "tempest-report-topic mb-1",
              shiny::strong("Topic: "),
              report_topic()
            )
          },
          shiny::p(
            class = "tempest-report-evidence-count mb-0 small text-muted",
            paste0(
              count_label(counts[["sources"]], "source", "sources"),
              " · ",
              count_label(counts[["facts"]], "fact", "facts"),
              " retained"
            )
          )
        ),
        shiny::div(
          class = "tempest-report-prose",
          markdown_ui(
            md,
            workspace = report_workspace(),
            include_references = TRUE
          )
        )
      )
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
          workspace = report_workspace(),
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

    list(report_available = report_available)
  })
}
