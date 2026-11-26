# Sources tab: the collected sources as an interactive (DT) or basic table.

mod_sources_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::nav_panel(
    title = shiny::tagList(shiny::icon("link"), "Sources"),
    value = "Sources",
    bslib::card(
      full_screen = TRUE,
      class = "tempest-evidence-card tempest-source-browser",
      bslib::card_header(evidence_table_header(
        ns = ns,
        title = "Collected sources",
        description = "Review the evidence gathered during this session.",
        icon_name = "link",
        count_id = "source_count"
      )),
      bslib::card_body(
        class = "p-0",
        shiny::uiOutput(ns("body"))
      )
    )
  )
}

mod_sources_server <- function(id, store) {
  shiny::moduleServer(id, function(input, output, session) {
    sources <- shiny::reactive({
      evidence <- store$costorm_workspace()
      if (is.null(evidence)) {
        return(NULL)
      }
      tempest:::tempest_workspace_sources(evidence)
    })

    output$body <- shiny::renderUI({
      df <- sources()
      if (is.null(df)) {
        return(evidence_panel_empty_state(
          "link",
          "No active session",
          "Start a session to collect sources."
        ))
      }
      if (nrow(df) == 0) {
        return(evidence_panel_empty_state(
          "link",
          "No sources collected yet",
          "Ask a research question to gather source material."
        ))
      }

      table <- if (has_pkg("DT")) {
        shiny::div(
          class = "tempest-evidence-table",
          role = "region",
          `aria-label` = "Collected sources",
          DT::DTOutput(session$ns("table"))
        )
      } else {
        shiny::div(
          class = "tempest-evidence-table p-3",
          role = "region",
          `aria-label` = "Collected sources",
          shiny::tableOutput(session$ns("table_basic"))
        )
      }

      if (has_pkg("DT")) {
        shiny::div(
          class = "tempest-evidence-panel",
          shiny::div(
            class = "tempest-evidence-toolbar tempest-evidence-toolbar-copy",
            shiny::p(
              class = "mb-0 small text-muted",
              "Search the table to filter by title, location, excerpt, or source ID."
            )
          ),
          table
        )
      } else {
        shiny::div(
          class = "tempest-evidence-panel",
          shiny::div(
            class = "tempest-evidence-toolbar tempest-evidence-toolbar-copy",
            shiny::p(
              class = "mb-0 small text-muted",
              "The table lists the title, location, excerpt, type, and source ID."
            )
          ),
          table
        )
      }
    })

    output$source_count <- shiny::renderText({
      df <- sources()
      n <- if (is.null(df)) 0L else nrow(df)
      paste(n, if (identical(n, 1L)) "source" else "sources")
    })

    if (has_pkg("DT")) {
      output$table <- DT::renderDT({
        df <- sources()
        shiny::req(df, nrow(df) > 0)
        df <- sources_table_data(df)
        df$Source <- source_table_links(
          df$Source,
          df$Location,
          df[["Source ID"]]
        )
        df$Location <- NULL
        widget <- styled_datatable(
          df,
          html_columns = "Source",
          search_placeholder = "Search sources",
          column_defs = list(
            list(
              targets = 0,
              className = "tempest-col-primary",
              responsivePriority = 1
            ),
            list(
              targets = 1,
              className = "tempest-col-wrap",
              responsivePriority = 2
            ),
            list(
              targets = c(2, 3),
              className = "tempest-col-secondary",
              responsivePriority = 3
            ),
            list(
              targets = 4,
              className = "tempest-col-mono",
              responsivePriority = 4
            )
          )
        )
        widget$x$options$language$search <- "Search sources:"
        widget
      })
    } else {
      output$table_basic <- shiny::renderTable(
        sources_table_data(sources()),
        striped = TRUE,
        hover = TRUE,
        bordered = TRUE,
        spacing = "s"
      )
    }
  })
}
