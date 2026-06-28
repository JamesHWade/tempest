# Config module: model/search/advanced settings shared by the Chat and STORM
# tabs. The UI lives in the Chat sidebar; the server returns a reactive
# `TempestConfig` that both tabs consume.

shiny_default_models <- function() {
  list(
    coordinator = "openai/gpt-5.4",
    expert = "openai/gpt-5.4-mini",
    writer = "openai/gpt-5.4",
    mindmap = "openai/gpt-5.4-mini",
    judge = "openai/gpt-5.4-mini"
  )
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
    ),
    bslib::accordion_panel(
      title = "Advanced",
      icon = shiny::icon("sliders"),
      shiny::checkboxInput(ns("discourse"), "Enable discourse manager", FALSE),
      shiny::checkboxInput(ns("unseen"), "Surface unseen sources", FALSE),
      shiny::numericInput(
        ns("node_trigger"),
        "Node expansion trigger",
        value = NA,
        min = 1,
        max = 50,
        step = 1
      )
    )
  )
}

mod_config_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::reactive({
      default_models <- shiny_default_models()
      trigger <- input$node_trigger
      if (isTRUE(is.na(trigger))) {
        trigger <- NULL
      }
      tempest::tempest_config(
        models = list(
          coordinator = input$coordinator %||% default_models$coordinator,
          expert = input$expert %||% default_models$expert,
          writer = input$writer %||% default_models$writer,
          mindmap = input$mindmap %||% default_models$mindmap,
          judge = input$judge %||% default_models$judge
        ),
        search_provider = input$search_provider %||% "native",
        enable_discourse_manager = input$discourse %||% FALSE,
        enable_unseen_surfacing = input$unseen %||% FALSE,
        node_expansion_trigger_count = trigger
      )
    })
  })
}
