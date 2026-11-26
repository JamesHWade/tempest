# Transcript tab: the conversation as a stack of speaker cards.

mod_transcript_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::nav_panel(
    title = shiny::tagList(shiny::icon("clock"), "Transcript"),
    value = "Transcript",
    bslib::card(
      full_screen = TRUE,
      class = "tempest-evidence-card tempest-transcript-card",
      bslib::card_header(
        class = "tempest-transcript-header",
        shiny::uiOutput(ns("header"))
      ),
      bslib::card_body(
        class = "p-2 tempest-transcript-body",
        style = "overflow-y: auto;",
        shiny::div(
          role = "log",
          `aria-live` = "polite",
          `aria-label` = "Conversation transcript",
          shiny::uiOutput(ns("body"))
        )
      )
    )
  )
}

mod_transcript_server <- function(id, store) {
  shiny::moduleServer(id, function(input, output, session) {
    turns <- shiny::reactive({
      ses <- store$costorm_session()
      if (is.null(ses)) list() else ses$transcript %||% list()
    })
    workspace <- shiny::reactive(store$costorm_workspace())

    output$header <- shiny::renderUI({
      n_turns <- length(turns())
      shiny::tagList(
        shiny::div(
          class = "tempest-panel-heading",
          shiny::h2(class = "h5 mb-1", "Conversation transcript"),
          shiny::p(
            class = "mb-0 small text-muted",
            "Questions and expert answers from the active session."
          )
        ),
        if (n_turns > 0) {
          shiny::span(
            class = "badge rounded-pill tempest-evidence-count",
            paste0(n_turns, " ", if (n_turns == 1L) "turn" else "turns")
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
        markdown_ui(text, workspace = workspace())
      } else {
        shiny::p(
          class = "text-muted mb-0",
          "No response text was recorded for this turn."
        )
      }

      shiny::tags$article(
        class = paste(
          "tempest-transcript-turn",
          if (is_user) {
            "tempest-transcript-turn-user"
          } else {
            "tempest-transcript-turn-assistant"
          }
        ),
        `aria-label` = paste(speaker, if (nzchar(at)) at else ""),
        shiny::div(
          class = "tempest-transcript-turn-body",
          shiny::tags$header(
            class = "tempest-transcript-turn-header",
            shiny::div(
              class = "tempest-transcript-speaker",
              if (is_user) {
                shiny::icon("user", class = "me-1", `aria-hidden` = "true")
              } else {
                tempest_inline_icon("me-1")
              },
              shiny::strong(speaker)
            ),
            if (nzchar(at)) {
              shiny::tags$time(class = "text-muted", datetime = at, at)
            }
          ),
          shiny::div(class = "tempest-transcript-turn-text", text_html)
        )
      )
    }

    output$body <- shiny::renderUI({
      all_turns <- turns()
      if (length(all_turns) == 0) {
        return(evidence_panel_empty_state(
          "clock",
          "Transcript is waiting",
          "Start a conversation to see the transcript."
        ))
      }
      recent <- utils::tail(all_turns, 100)
      shiny::tagList(
        if (length(all_turns) > length(recent)) {
          shiny::div(
            class = "tempest-transcript-limit small text-muted",
            paste0(
              "Showing the latest ",
              length(recent),
              " turns of ",
              length(all_turns),
              "."
            )
          )
        },
        lapply(recent, turn_card)
      )
    })
  })
}
