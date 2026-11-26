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

shiny_config_review_settings <- function(input, default_config) {
  research_review <- input$research_review %||% default_config@research_review
  list(
    research_review = research_review,
    repair_evidence = if (isTRUE(research_review)) {
      input$repair_evidence %||% default_config@repair_evidence
    } else {
      FALSE
    },
    max_review_claims = input$max_review_claims %||%
      default_config@max_review_claims
  )
}

mod_config_ui <- function(id) {
  ns <- shiny::NS(id)
  default_config <- tempest::tempest_config()
  default_models <- default_config@models
  model_input <- function(input_id, label) {
    shiny::div(
      class = "tempest-config-field",
      shiny::textInput(
        ns(input_id),
        label,
        value = default_models[[input_id]],
        width = "100%"
      )
    )
  }

  bslib::accordion(
    id = ns("accordion"),
    open = FALSE,
    multiple = TRUE,
    class = "tempest-config-accordion",
    bslib::accordion_panel(
      title = "Models",
      icon = shiny::icon("microchip"),
      shiny::p(
        class = "small text-body-secondary tempest-config-intro",
        "Route each research role to the model best suited to it."
      ),
      model_input("coordinator", "Coordinator"),
      model_input("expert", "Expert"),
      model_input("writer", "Writer"),
      model_input("mindmap", "Mind Map"),
      model_input("judge", "Judge")
    ),
    bslib::accordion_panel(
      title = "Search",
      icon = shiny::icon("magnifying-glass"),
      shiny::p(
        class = "small text-body-secondary tempest-config-intro",
        "Choose where the expert panel looks for sources."
      ),
      shiny::selectInput(
        ns("search_provider"),
        "Search Provider",
        choices = search_provider_choices(),
        selected = "native",
        width = "100%"
      )
    ),
    bslib::accordion_panel(
      title = "Scientific review",
      icon = shiny::icon("microscope"),
      shiny::checkboxInput(
        ns("research_review"),
        "Enable scientific review",
        value = default_config@research_review
      ),
      shiny::helpText(
        paste(
          "Review selected claims for scientific issues.",
          "Findings are advisory."
        )
      ),
      shiny::conditionalPanel(
        condition = sprintf("input['%s']", ns("research_review")),
        shiny::div(
          class = "tempest-config-review-options",
          shiny::checkboxInput(
            ns("repair_evidence"),
            "Search for missing evidence",
            value = default_config@repair_evidence
          ),
          shiny::numericInput(
            ns("max_review_claims"),
            "Maximum claims to review",
            value = default_config@max_review_claims,
            min = 1,
            max = 20,
            step = 1,
            width = "100%"
          )
        )
      )
    )
  )
}

mod_config_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    default_config <- tempest::tempest_config()
    shiny::reactive({
      default_models <- shiny_default_models()
      review_settings <- shiny_config_review_settings(input, default_config)
      tempest::tempest_config(
        models = shiny_config_models(input, default_models),
        search_provider = input$search_provider %||% "native",
        research_review = review_settings$research_review,
        repair_evidence = review_settings$repair_evidence,
        max_review_claims = review_settings$max_review_claims
      )
    })
  })
}
