# Sources tab: the collected sources as an interactive (DT) or basic table.

mod_sources_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::nav_panel(
    title = shiny::tagList(shiny::icon("link"), "Sources"),
    value = "Sources",
    bslib::card(
      full_screen = TRUE,
      bslib::card_header("Collected Sources"),
      bslib::card_body(
        class = "p-2",
        shiny::uiOutput(ns("body"))
      )
    )
  )
}

mod_sources_server <- function(id, store) {
  shiny::moduleServer(id, function(input, output, session) {
    sources <- shiny::reactive({
      ses <- store$get()
      if (is.null(ses)) {
        return(NULL)
      }
      tempest::tempest_sources(ses$store)
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
        DT::DTOutput(session$ns("table"))
      } else {
        shiny::tableOutput(session$ns("table_basic"))
      }
    })

    linkify <- function(urls) {
      vapply(
        urls,
        function(u) {
          if (is.na(u) || !nzchar(u)) {
            return("")
          }
          escaped <- htmltools::htmlEscape(u)
          paste0('<a href="', escaped, '" target="_blank">', escaped, "</a>")
        },
        character(1)
      )
    }

    if (has_pkg("DT")) {
      output$table <- DT::renderDT({
        df <- sources()
        shiny::req(df, nrow(df) > 0)
        df <- sources_table_data(df)
        if ("url" %in% names(df)) {
          df$url <- linkify(df$url)
        }
        styled_datatable(df)
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
