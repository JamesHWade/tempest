# Transcript tab: the conversation as a stack of speaker cards.

mod_transcript_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::nav_panel(
    title = shiny::tagList(shiny::icon("clock"), "Transcript"),
    value = "Transcript",
    bslib::card(
      full_screen = TRUE,
      bslib::card_header(shiny::uiOutput(ns("header"))),
      bslib::card_body(
        class = "p-2",
        style = "overflow-y: auto;",
        shiny::uiOutput(ns("body"))
      )
    )
  )
}

mod_transcript_server <- function(id, store) {
  shiny::moduleServer(id, function(input, output, session) {
    turns <- shiny::reactive({
      ses <- store$get()
      if (is.null(ses)) list() else ses$transcript %||% list()
    })
    source_store <- shiny::reactive({
      ses <- store$get()
      if (is.null(ses)) {
        return(NULL)
      }
      citation_source_store(ses$store %||% NULL)
    })

    output$header <- shiny::renderUI({
      n_turns <- length(turns())
      shiny::tagList(
        shiny::span("Conversation Transcript"),
        if (n_turns > 0) {
          shiny::span(
            class = "badge bg-secondary ms-2",
            paste0(n_turns, " turns")
          )
        }
      )
    })

    turn_card <- function(turn) {
      role <- turn$role %||% "assistant"
      speaker <- turn$speaker %||% role
      text <- turn$text %||% ""
      at <- turn$at %||% ""
      is_user <- identical(role, "user")

      text_html <- if (nzchar(text)) {
        markdown_ui(text, store = source_store())
      } else {
        shiny::p(text)
      }

      shiny::div(
        class = paste("card mb-2", if (is_user) "bg-light" else ""),
        shiny::div(
          class = "card-body py-2 px-3",
          shiny::div(
            class = "d-flex justify-content-between align-items-center mb-1",
            shiny::span(
              if (is_user) {
                shiny::icon("user", class = "me-1")
              } else {
                tempest_inline_icon("me-1")
              },
              shiny::strong(speaker)
            ),
            if (nzchar(at)) shiny::tags$small(class = "text-muted", at)
          ),
          text_html
        )
      )
    }

    output$body <- shiny::renderUI({
      all_turns <- turns()
      if (length(all_turns) == 0) {
        return(empty_state(
          "clock",
          "Start a conversation to see the transcript."
        ))
      }
      recent <- utils::tail(all_turns, 100)
      shiny::tagList(lapply(recent, turn_card))
    })
  })
}
