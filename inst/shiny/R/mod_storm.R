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
    progress_events <- shiny::reactiveVal(list())

    storm_task <- shiny::ExtendedTask$new(
      function(topic, cfg, n_experts, strategy, max_rounds, parallel) {
        mirai::mirai(
          {
            collector <- tempest::tempest_progress_collector(
              include_payload = TRUE
            )
            result <- tempest::tempest_run(
              topic = topic,
              config = cfg,
              n_experts = n_experts,
              research_strategy = strategy,
              max_rounds = max_rounds,
              parallel_research = parallel,
              progress = collector$record,
              verbose = FALSE
            )
            list(result = result, progress = collector$data())
          },
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
      progress_events(list(storm_running_event(topic)))
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
        value <- storm_task$result()
        progress_events(storm_task_progress(value))
        result <- storm_task_result(value)
        if (is.null(result)) {
          return()
        }
        store$set_report(
          result$report_md %||% "",
          shiny::isolate(input$topic) %||% "STORM Report"
        )
      }
    })

    output$progress <- shiny::renderUI({
      status <- storm_task$status()
      state <- storm_progress_state(progress_events(), status)
      switch(
        status,
        initial = empty_state(
          "bolt",
          "Configure settings and click 'Run STORM Pipeline' to begin."
        ),
        running = workflow_progress_ui(state, storm_stage_labels()),
        success = workflow_progress_ui(state, storm_stage_labels()),
        error = workflow_progress_ui(state, storm_stage_labels())
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
      result <- storm_task_result(storm_task$result())
      if (is.null(result)) {
        return(NULL)
      }
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

storm_running_event <- function(topic) {
  tempest::tempest_progress_event(
    run_id = paste0(
      "shiny-storm-",
      format(Sys.time(), "%Y%m%d%H%M%OS3")
    ),
    workflow = "storm",
    event_type = "workflow",
    status = "started",
    message = paste("Running STORM pipeline for", topic)
  )
}

storm_progress_state <- function(events, task_status = "initial") {
  if (length(events) > 0L) {
    state <- tryCatch(
      tempest::tempest_progress_state(events),
      error = function(e) NULL
    )
    if (!is.null(state) && !identical(task_status, "error")) {
      return(state)
    }
  }
  status <- switch(
    task_status,
    error = "failed",
    success = "succeeded",
    "started"
  )
  tempest::tempest_progress_state(list(
    tempest::tempest_progress_event(
      run_id = "shiny-storm",
      workflow = "storm",
      event_type = "workflow",
      status = status,
      message = if (identical(status, "failed")) {
        "The STORM pipeline failed."
      } else {
        "Running STORM pipeline."
      },
      payload = if (identical(status, "failed")) {
        list(error_message = "The STORM pipeline failed.")
      } else {
        list()
      }
    )
  ))
}

storm_task_result <- function(value) {
  if (is.list(value) && "result" %in% names(value)) {
    value$result
  } else {
    value
  }
}

storm_task_progress <- function(value) {
  if (is.list(value) && "progress" %in% names(value)) {
    value$progress %||% list()
  } else {
    list()
  }
}

storm_stage_labels <- function() {
  c(
    perspectives = "Perspectives",
    research = "Research",
    outline = "Outline",
    write = "Write",
    polish = "Polish",
    verification = "Verify"
  )
}
