# Sources tab: the collected sources as an interactive (DT) or basic table.

mod_sources_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::nav_panel(
    title = shiny::tagList(shiny::icon("link"), "Sources"),
    value = "Sources",
    bslib::card(
      full_screen = TRUE,
      class = "tempest-evidence-card",
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
      evidence <- store$evidence_store()
      if (is.null(evidence)) {
        return(NULL)
      }
      tempest::tempest_sources(evidence)
    })

    output$body <- shiny::renderUI({
      df <- sources()
      if (is.null(df)) {
        return(empty_state("link", "Start a session to collect sources."))
      }
      if (nrow(df) == 0) {
        return(empty_state("link", "No sources collected yet."))
      }
      if (has_pkg("DT")) {
        shiny::div(
          class = "tempest-evidence-table",
          DT::DTOutput(session$ns("table"))
        )
      } else {
        shiny::div(
          class = "tempest-evidence-table p-3",
          shiny::tableOutput(session$ns("table_basic"))
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
        styled_datatable(
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
