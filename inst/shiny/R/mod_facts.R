# Facts tab: extracted facts as an interactive (DT) or basic table, with
# confidence badges and collapsed source ids.

mod_facts_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::nav_panel(
    title = shiny::tagList(shiny::icon("check-circle"), "Facts"),
    value = "Facts",
    bslib::card(
      full_screen = TRUE,
      class = "tempest-evidence-card",
      bslib::card_header(evidence_table_header(
        ns = ns,
        title = "Extracted facts",
        description = "Inspect claim status, support, and linked evidence.",
        icon_name = "check-circle",
        count_id = "fact_count"
      )),
      bslib::card_body(
        class = "p-0",
        shiny::uiOutput(ns("body"))
      )
    )
  )
}

mod_facts_server <- function(id, store) {
  shiny::moduleServer(id, function(input, output, session) {
    facts <- shiny::reactive({
      evidence <- store$costorm_workspace()
      if (is.null(evidence)) {
        return(NULL)
      }
      tempest:::tempest_workspace_claims(evidence)
    })

    output$body <- shiny::renderUI({
      df <- facts()
      if (is.null(df)) {
        return(empty_state("check-circle", "Start a session to collect facts."))
      }
      if (nrow(df) == 0) {
        return(empty_state("check-circle", "No facts extracted yet."))
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

    output$fact_count <- shiny::renderText({
      df <- facts()
      n <- if (is.null(df)) 0L else nrow(df)
      paste(n, if (identical(n, 1L)) "fact" else "facts")
    })

    if (has_pkg("DT")) {
      output$table <- DT::renderDT({
        df <- facts()
        shiny::req(df, nrow(df) > 0)
        df <- facts_table_data(df)
        df$Confidence <- evidence_status_badges(
          df$Confidence,
          palette = "confidence"
        )
        df$Status <- evidence_status_badges(
          df$Status,
          palette = "verification"
        )
        styled_datatable(
          df,
          html_columns = c("Confidence", "Status"),
          search_placeholder = "Search facts",
          column_defs = list(
            list(
              targets = 0,
              className = "tempest-col-primary",
              responsivePriority = 1
            ),
            list(
              targets = c(2, 3, 4),
              className = "tempest-col-secondary",
              responsivePriority = 2
            ),
            list(
              targets = c(1, 5),
              className = "tempest-col-wrap",
              responsivePriority = 3
            )
          )
        )
      })
    } else {
      output$table_basic <- shiny::renderTable(
        facts_table_data(facts()),
        striped = TRUE,
        hover = TRUE,
        bordered = TRUE,
        spacing = "s"
      )
    }
  })
}
