library(shiny)
library(bslib)
library(tempest)

fake_host_chat <- function(role) {
  list(
    chat = function(prompt, ...) {
      paste(
        "Mock",
        role,
        "response from the host app. Replace the chat_fn with a real provider",
        "for production use."
      )
    },
    chat_structured = function(prompt, type = NULL, ...) {
      if (identical(role, "mindmap")) {
        return(list(
          nodes = list(list(
            id = "root",
            label = "Host research",
            parent = NULL,
            notes = "",
            source_ids = character()
          )),
          edges = list()
        ))
      }
      if (identical(role, "judge")) {
        return(list(facts = list()))
      }
      list()
    },
    register_tools = function(...) invisible(NULL)
  )
}

host_config <- tempest_config(
  chat_fn = function(role, model, system_prompt, echo) {
    fake_host_chat(role)
  }
)

host_experts <- list(tempest_expert(
  name = "Host Analyst",
  title = "Project researcher",
  perspective = "Uses host-provided context and project constraints.",
  initial_questions = "What should this project investigate first?"
))

ui <- page_fillable(
  title = "Embedded tempest host",
  theme = bs_theme(version = 5),
  tempest_shiny_ui("research")
)

server <- function(input, output, session) {
  tempest_shiny_server(
    "research",
    config = host_config,
    personas = host_experts,
    session_id = "example-host-session"
  )
}

shinyApp(ui, server)
