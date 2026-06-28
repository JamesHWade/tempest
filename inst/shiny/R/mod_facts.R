# Facts tab: extracted facts as an interactive (DT) or basic table, with
# confidence badges and collapsed source ids.

mod_facts_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::nav_panel(
    title = shiny::tagList(shiny::icon("check-circle"), "Facts"),
    value = "Facts",
    bslib::card(
      full_screen = TRUE,
      bslib::card_header("Extracted Facts"),
      bslib::card_body(
        class = "p-2",
        shiny::uiOutput(ns("body"))
      )
    )
  )
}

mod_facts_server <- function(id, store) {
  shiny::moduleServer(id, function(input, output, session) {
    facts <- shiny::reactive({
      ses <- store$get()
      if (is.null(ses)) {
        return(NULL)
      }
      tempest::tempest_claims(ses$store)
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
        DT::DTOutput(session$ns("table"))
      } else {
        shiny::tableOutput(session$ns("table_basic"))
      }
    })

    confidence_badge <- function(values) {
      vapply(
        values,
        function(value) {
          color <- switch(
            tolower(value %||% ""),
            high = "bg-success",
            medium = "bg-warning text-dark",
            low = "bg-danger",
            "bg-secondary"
          )
          paste0(
            '<span class="badge ',
            color,
            '">',
            htmltools::htmlEscape(value %||% "N/A"),
            "</span>"
          )
        },
        character(1)
      )
    }

    collapse_ids <- function(ids_list) {
      vapply(
        ids_list,
        function(ids) paste(unlist(ids), collapse = ", "),
        character(1)
      )
    }

    if (has_pkg("DT")) {
      output$table <- DT::renderDT({
        df <- facts()
        shiny::req(df, nrow(df) > 0)
        if ("confidence" %in% names(df)) {
          df$confidence <- confidence_badge(df$confidence)
        }
        if ("source_ids" %in% names(df)) {
          df$source_ids <- collapse_ids(df$source_ids)
        }
        styled_datatable(df)
      })
    } else {
      output$table_basic <- shiny::renderTable(
        facts(),
        striped = TRUE,
        hover = TRUE,
        bordered = TRUE,
        spacing = "s"
      )
    }
  })
}
