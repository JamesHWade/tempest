# STORM tab: run the scripted pipeline in the background via ExtendedTask, so
# the button state, progress, and result are all driven by the task's status
# rather than hand-managed reactive values.

mod_storm_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::nav_panel(
    title = shiny::tagList(shiny::icon("bolt"), "STORM"),
    value = "STORM",
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        title = "STORM Settings",
        width = 300,
        shiny::textInput(
          ns("topic"),
          "Research Topic",
          placeholder = "Enter a research topic..."
        ),
        shiny::numericInput(
          ns("n_experts"),
          "Expert Count",
          3,
          min = 1,
          max = 10
        ),
        shiny::selectInput(
          ns("strategy"),
          "Research Strategy",
          choices = c("key_questions", "conversation"),
          selected = "key_questions"
        ),
        shiny::numericInput(
          ns("max_rounds"),
          "Max Rounds",
          3,
          min = 1,
          max = 10
        ),
        shiny::checkboxInput(ns("parallel"), "Parallel research", FALSE),
        bslib::input_task_button(
          ns("run"),
          "Run STORM Pipeline",
          icon = shiny::icon("bolt"),
          label_busy = "Running...",
          class = "w-100"
        )
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("STORM Pipeline Progress"),
        bslib::card_body(
          shiny::uiOutput(ns("progress")),
          shiny::uiOutput(ns("result"))
        )
      )
    )
  )
}

mod_storm_server <- function(id, config, store) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    storm_task <- shiny::ExtendedTask$new(
      function(topic, cfg, n_experts, strategy, max_rounds, parallel) {
        mirai::mirai(
          tempest::tempest_run(
            topic = topic,
            config = cfg,
            n_experts = n_experts,
            research_strategy = strategy,
            max_rounds = max_rounds,
            parallel_research = parallel,
            verbose = FALSE
          ),
          topic = topic,
          cfg = cfg,
          n_experts = n_experts,
          strategy = strategy,
          max_rounds = max_rounds,
          parallel = parallel
        )
      }
    ) |>
      bslib::bind_task_button("run")

    shiny::observeEvent(input$run, {
      topic <- stringi::stri_trim_both(input$topic %||% "")
      shiny::req(nzchar(topic))
      storm_task$invoke(
        topic = topic,
        cfg = config(),
        n_experts = input$n_experts %||% 3,
        strategy = input$strategy %||% "key_questions",
        max_rounds = input$max_rounds %||% 3,
        parallel = input$parallel %||% FALSE
      )
    })

    # Share the report once the pipeline succeeds.
    shiny::observeEvent(storm_task$status(), {
      if (identical(storm_task$status(), "success")) {
        result <- storm_task$result()
        store$set_report(
          result$report_md %||% "",
          shiny::isolate(input$topic) %||% "STORM Report"
        )
      }
    })

    output$progress <- shiny::renderUI({
      switch(
        storm_task$status(),
        initial = empty_state(
          "bolt",
          "Configure settings and click 'Run STORM Pipeline' to begin."
        ),
        running = shiny::div(
          shiny::div(
            class = "d-flex align-items-center mb-3 text-primary",
            shiny::icon("spinner", class = "fa-spin fa-lg me-2"),
            shiny::strong("Running STORM pipeline...")
          ),
          shiny::p(
            class = "text-muted small",
            "Runs in the background — the app stays responsive."
          ),
          storm_stage_list(complete = FALSE)
        ),
        success = storm_stage_list(complete = TRUE),
        error = empty_state(
          "triangle-exclamation",
          "The pipeline failed — see details below."
        )
      )
    })

    output$result <- shiny::renderUI({
      status <- storm_task$status()
      if (identical(status, "error")) {
        msg <- tryCatch(
          {
            storm_task$result()
            "Unknown error"
          },
          error = function(e) conditionMessage(e)
        )
        return(shiny::div(
          class = "alert alert-danger mt-3",
          shiny::icon("triangle-exclamation"),
          " Pipeline error: ",
          msg
        ))
      }
      if (!identical(status, "success")) {
        return(NULL)
      }
      result <- storm_task$result()
      chars <- nchar(result$report_md %||% "")
      shiny::div(
        class = "alert alert-success mt-3",
        shiny::icon("circle-check"),
        sprintf(" Pipeline complete! Report: %d characters. ", chars),
        shiny::actionLink(
          ns("view_report"),
          "View Report",
          class = "alert-link"
        )
      )
    })

    report_ready <- shiny::reactiveVal(0L)
    shiny::observeEvent(input$view_report, {
      report_ready(report_ready() + 1L)
    })

    shiny::reactive(report_ready())
  })
}

# The pipeline's stages, shown as a checklist (pending while running, all
# complete on success).
storm_stage_list <- function(complete) {
  stages <- list(
    c("Perspectives", "users"),
    c("Research", "magnifying-glass"),
    c("Outline", "list-ol"),
    c("Write", "pen-nib"),
    c("Polish", "wand-magic-sparkles")
  )
  items <- lapply(stages, function(stage) {
    mark <- if (complete) {
      shiny::icon("circle-check", class = "text-success")
    } else {
      shiny::icon("circle", class = "text-muted")
    }
    shiny::div(
      class = "d-flex align-items-center mb-2",
      shiny::span(class = "me-2", mark),
      shiny::icon(stage[2], class = "me-2"),
      shiny::strong(stage[1])
    )
  })
  shiny::div(class = "py-3 px-2", items)
}
