#' Launch the enriched research application
#'
#' Opens a dedicated Shiny application for research briefs, optional R analyses
#' and illustrations, and portable reports. Each research or MCP discovery job
#' runs in a separate R process. Cancelling a job or closing its browser session
#' stops its process tree. The existing [tempest_app()] remains available for
#' interactive Co-STORM sessions.
#'
#' @param ... Arguments passed to [shiny::runApp()].
#' @param config Research configuration from [tempest_config()]. The model
#'   selected in the app applies to all research roles. Custom chat factories
#'   and other configuration settings are retained.
#' @param tools Named list of trusted, zero-argument R functions. Each selected
#'   function creates a list of ellmer tool definitions inside the research
#'   worker. Factories must work in a fresh R process; use explicit package
#'   namespaces and do not capture open connections. Tools are attached only
#'   to research experts, under Tempest's existing Deputy research permissions.
#' @param mcp_config Optional path to an MCP JSON configuration with a
#'   `mcpServers` object. The app also accepts a session-local JSON upload.
#'   Only explicitly selected servers are contacted during discovery, and only
#'   selected tools are attached during research. MCP transport and
#'   authentication are supplied by Deputy and its supported mcptools versions.
#' @param output_dir Parent directory for newly generated report directories.
#'   Existing reports are never overwritten. The default lasts for the current
#'   R session; choose a persistent directory to retain reports after R exits.
#' @param model Initial provider/model selection, editable in the application.
#'   When omitted with a configured Chat object, starts with that Chat's model.
#'   A supplied Chat can change models within its provider; use a custom
#'   `config` chat factory to support switching providers.
#'
#' @details
#' Run the application for trusted users. MCP configurations can launch local
#' programs, and host-supplied tool factories execute R code. Discovery does
#' not enable tools. Tool selections do not override Deputy's research
#' permissions: tools requiring filesystem writes, shell execution, or other
#' denied effects remain unavailable. External tool output is research context,
#' not automatically a verified source or claim.
#'
#' Generated R analysis and image generation are disabled initially and enabled
#' separately for each run. R execution uses the local account's access; it is
#' not an OS sandbox. Image generation uses [tempest_image_generator()] and
#' reads `OPENAI_API_KEY` in the worker. Credentials and uploaded MCP
#' configuration contents are not copied into report downloads. Host tools
#' remain responsible for the contents of their own returned data.
#'
#' @return Invisibly, the result of [shiny::runApp()].
#' @examples
#' \dontrun{
#' tempest_researcher_app(output_dir = "research-reports")
#'
#' tempest_researcher_app(
#'   tools = list(plant_data = function() {
#'     list(ellmer::tool(
#'       function() as.data.frame(datasets::CO2),
#'       name = "read_co2_data",
#'       description = "Read the built-in CO2 plant experiment data",
#'       arguments = list(),
#'       annotations = ellmer::tool_annotations(read_only_hint = TRUE)
#'     ))
#'   }),
#'   mcp_config = "mcp.json"
#' )
#' }
#' @export
tempest_researcher_app <- function(
  ...,
  config = tempest_config(),
  tools = list(),
  mcp_config = NULL,
  output_dir = file.path(tempdir(), "tempest-researcher"),
  model = "openai/gpt-6.1-sol"
) {
  for (package in c("shiny", "bslib", "callr", "zip")) {
    tempest_require(package, "The dedicated researcher application needs it.")
  }
  if (!S7::S7_inherits(config, TempestConfig)) {
    tempest_abort("{.arg config} must be created by {.fn tempest_config}.")
  }
  if (missing(model) && !is.null(config@chat)) {
    model <- config@chat$get_model()
  }
  tempest_enriched_string(output_dir, "output_dir")
  tempest_enriched_string(model, "model")
  catalog <- tempest_researcher_tool_catalog(tools)
  servers <- tempest_researcher_mcp_servers(mcp_config)
  runner <- getOption("tempest.app_runner", shiny::runApp)
  if (!is.function(runner)) {
    tempest_abort("The configured Shiny app runner must be a function.")
  }
  app <- shiny::shinyApp(
    ui = tempest_researcher_ui(catalog, servers, model),
    server = tempest_researcher_app_server(
      config,
      tools,
      mcp_config,
      output_dir
    )
  )
  old <- options(ellmer_timeout_s = tempest_shiny_provider_timeout())
  on.exit(options(old), add = TRUE)
  invisible(runner(app, ...))
}

tempest_researcher_app_controller <- function(...) {
  TempestResearcherAppSession$new(...)
}

tempest_researcher_app_server <- function(
  config,
  tools,
  mcp_config,
  output_dir
) {
  force(config)
  force(tools)
  force(mcp_config)
  force(output_dir)
  function(input, output, session) {
    controller <- tempest_researcher_app_controller(
      config = config,
      tools = tools,
      mcp_config = mcp_config,
      output_dir = output_dir
    )
    state <- shiny::reactiveVal(controller$snapshot())
    notice <- shiny::reactiveVal(NULL)
    catalog <- shiny::reactiveVal(NULL)
    discovery_servers <- shiny::reactiveVal(character())
    published <- shiny::reactiveVal(NULL)
    resource <- NULL

    refresh <- function() {
      value <- controller$snapshot()
      state(value)
      invisible(value)
    }
    attempt <- function(action, message) {
      notice(NULL)
      tryCatch(action(), error = function(error) notice(message))
      refresh()
    }

    shiny::observeEvent(
      input$mcp_file,
      {
        attempt(
          function() {
            servers <- controller$set_mcp_config(input$mcp_file$datapath)
            catalog(NULL)
            shiny::updateSelectizeInput(
              session,
              "mcp_servers",
              choices = servers,
              selected = character()
            )
          },
          "The MCP configuration could not be loaded. Check its JSON format and wait for any active job to finish."
        )
      },
      ignoreNULL = TRUE
    )

    shiny::observeEvent(
      input$mcp_servers,
      {
        catalog(NULL)
      },
      ignoreNULL = FALSE
    )

    shiny::observeEvent(input$discover, {
      attempt(
        function() {
          servers <- input$mcp_servers %||% character()
          controller$discover(servers)
          discovery_servers(servers)
          catalog(NULL)
        },
        "Tool discovery could not start. Select a configured server and check that Deputy's supported mcptools version is installed."
      )
    })

    shiny::observeEvent(input$run, {
      attempt(
        function() {
          available <- catalog()
          selected_mcp <- input$selected_mcp %||% character()
          enabled_ids <- if (is.null(available)) {
            character()
          } else {
            available$id[available$available %||% rep(TRUE, nrow(available))]
          }
          if (!all(selected_mcp %in% enabled_ids)) {
            tempest_abort("Discover and select MCP tools again before running.")
          }
          selected_servers <- if (length(selected_mcp)) {
            unique(available$server[available$id %in% selected_mcp])
          } else {
            character()
          }
          controller$start(list(
            topic = input$topic,
            model = input$model,
            research_strategy = input$research_strategy %||% "key_questions",
            n_experts = input$n_experts,
            max_rounds = input$max_rounds,
            max_questions = input$max_questions,
            allow_code = isTRUE(input$allow_code),
            allow_images = isTRUE(input$allow_images),
            max_analyses = input$max_analyses,
            max_illustrations = input$max_illustrations,
            selected_tools = input$local_tools %||% character(),
            mcp_servers = selected_servers,
            selected_mcp = selected_mcp
          ))
        },
        "Research could not start. Check the topic, model, and run limits; rediscover changed MCP tools, and wait for any active job to finish."
      )
    })

    shiny::observeEvent(input$cancel, {
      attempt(
        function() controller$cancel(),
        "The job could not be cancelled. Close this app session to stop its worker."
      )
    })

    shiny::observe({
      current <- state()
      if (!identical(current$status, "running")) {
        return(invisible(NULL))
      }
      shiny::invalidateLater(200, session)
      value <- tryCatch(controller$poll(), error = function(error) {
        try(controller$cancel(), silent = TRUE)
        list(
          status = "error",
          kind = current$kind,
          result = NULL,
          error = "The worker could not be read. The job was stopped.",
          progress = current$progress
        )
      })
      if (identical(value$status, "complete")) {
        if (identical(value$kind, "discover")) {
          if (
            identical(discovery_servers(), input$mcp_servers %||% character())
          ) {
            catalog(value$result)
          }
        } else if (identical(value$kind, "research")) {
          tryCatch(
            {
              report <- tempest_read_enriched_report(value$result$path)
              next_resource <- paste0("tempest-report-", tempest_uuid("view"))
              shiny::addResourcePath(next_resource, report@path)
              previous_resource <- resource
              resource <<- next_resource
              published(list(
                path = report@path,
                status = report@status,
                url = paste0(resource, "/index.html")
              ))
              if (!is.null(previous_resource)) {
                try(shiny::removeResourcePath(previous_resource), silent = TRUE)
              }
            },
            error = function(error) {
              value$status <<- "error"
              value$error <<- paste(
                "Research finished, but its report could not be verified or opened.",
                "Previously completed reports are retained."
              )
              value$result <<- NULL
            }
          )
        }
      }
      if (!identical(value, current)) {
        state(value)
      }
    })

    session$onSessionEnded(function() {
      on.exit(try(controller$close(), silent = TRUE), add = TRUE)
      if (!is.null(resource)) {
        shiny::removeResourcePath(resource)
      }
    })

    output$status <- shiny::renderUI({
      current <- state()
      if (!is.null(notice())) {
        return(shiny::div(role = "alert", notice()))
      }
      label <- switch(
        current$status,
        idle = "Ready for a research question.",
        running = if (identical(current$kind, "discover")) {
          "Discovering tools from the selected servers\u2026"
        } else {
          "Research is running. You can keep reading or cancel this run."
        },
        complete = if (identical(current$kind, "discover")) {
          "Tool discovery complete. Select tools to enable them for research."
        } else {
          "Report ready. Review its evidence, methods, and task outcomes below."
        },
        cancelled = "Job cancelled. Previously completed reports are retained.",
        error = current$error %||% "The job did not complete.",
        "Ready."
      )
      shiny::div(
        role = if (identical(current$status, "error")) "alert" else "status",
        label
      )
    })

    output$progress <- shiny::renderUI({
      events <- utils::tail(state()$progress, 8L)
      if (!length(events)) {
        return(NULL)
      }
      shiny::tags$ol(lapply(events, function(event) {
        shiny::tags$li(paste(
          event$stage %||% "Research",
          event$status %||% event$event_type %||% "working",
          sep = " \u00b7 "
        ))
      }))
    })

    output$tools_status <- shiny::renderUI({
      if (is.null(catalog())) {
        return(shiny::p("Select servers and discover their tools to begin."))
      }
      shiny::p(
        nrow(catalog()),
        " tools discovered. Selection applies to the next run."
      )
    })
    output$mcp_tools <- shiny::renderUI({
      value <- catalog()
      if (is.null(value) || !nrow(value)) {
        return(NULL)
      }
      enabled <- value$available %||% rep(TRUE, nrow(value))
      choices <- stats::setNames(
        value$id[enabled],
        paste(value$server[enabled], value$name[enabled], sep = " / ")
      )
      shiny::tagList(
        shiny::checkboxGroupInput("selected_mcp", "Enabled MCP tools", choices),
        shiny::tags$details(
          shiny::tags$summary("Tool descriptions"),
          lapply(seq_len(nrow(value)), function(i) {
            shiny::tags$p(
              shiny::tags$strong(paste(
                value$server[[i]],
                value$name[[i]],
                sep = " / "
              )),
              " \u2014 ",
              value$description[[i]],
              if (!enabled[[i]]) {
                shiny::tags$span(
                  " Unavailable: ",
                  value$reason[[i]] %||% "Not permitted by research policy."
                )
              }
            )
          })
        )
      )
    })

    output$report <- shiny::renderUI({
      report <- published()
      if (is.null(report)) {
        return(shiny::div(
          class = "researcher-empty",
          shiny::h2("A question becomes a research report."),
          shiny::p(
            "Follow the evidence, inspect computed figures, and keep the sources and methods with your report."
          )
        ))
      }
      shiny::tagList(
        if (identical(report$status, "partial")) {
          shiny::p(
            role = "status",
            "Some enrichment tasks failed or were skipped. Their outcomes are included in the report."
          )
        },
        shiny::tags$a(
          href = report$url,
          target = "_blank",
          rel = "noopener",
          "Open report in a new tab"
        ),
        shiny::tags$iframe(
          src = report$url,
          title = "Enriched research report",
          sandbox = "allow-same-origin allow-downloads allow-popups",
          style = "width:100%;height:78vh;min-height:500px;border:0;"
        )
      )
    })

    output$download <- shiny::downloadHandler(
      filename = function() "tempest-research-report.zip",
      content = function(file) {
        report <- shiny::req(published())
        tempest_read_enriched_report(report$path)
        zip::zipr(
          file,
          files = list.files(report$path, all.files = TRUE, no.. = TRUE),
          root = report$path
        )
      },
      contentType = "application/zip"
    )
    output$report_ready <- function(...) !is.null(published())
    shiny::outputOptions(output, "report_ready", suspendWhenHidden = FALSE)
  }
}
