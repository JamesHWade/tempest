# Config module: model/search/advanced settings shared by the Chat and STORM
# tabs. The UI lives in the Chat settings drawer; the server returns a reactive
# `TempestConfig` that both tabs consume.

shiny_default_models <- function() {
  tempest::tempest_config()@models
}

shiny_config_models <- function(input, default_models) {
  models <- list(
    coordinator = input$coordinator %||% default_models$coordinator,
    writer = input$writer %||% default_models$writer,
    expert = input$expert %||% default_models$expert,
    mindmap = input$mindmap %||% default_models$mindmap,
    judge = input$judge %||% default_models$judge
  )
  if (identical(models, default_models)) NULL else models
}

mod_config_ui <- function(id) {
  ns <- shiny::NS(id)
  default_models <- shiny_default_models()
  model_input <- function(input_id, label) {
    shiny::textInput(ns(input_id), label, value = default_models[[input_id]])
  }

  bslib::accordion(
    id = ns("accordion"),
    open = FALSE,
    bslib::accordion_panel(
      title = "Models",
      icon = shiny::icon("microchip"),
      model_input("coordinator", "Coordinator"),
      model_input("expert", "Expert"),
      model_input("writer", "Writer"),
      model_input("mindmap", "Mind Map"),
      model_input("judge", "Judge")
    ),
    bslib::accordion_panel(
      title = "Search",
      icon = shiny::icon("magnifying-glass"),
      shiny::selectInput(
        ns("search_provider"),
        "Search Provider",
        choices = search_provider_choices(),
        selected = "native"
      )
    )
  )
}

mod_config_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::reactive({
      default_models <- shiny_default_models()
      tempest::tempest_config(
        models = shiny_config_models(input, default_models),
        search_provider = input$search_provider %||% "native"
      )
    })
  })
}
