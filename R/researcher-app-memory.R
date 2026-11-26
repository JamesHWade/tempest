# Graft project-memory sidecar and UI modules for tempest_researcher_app().

tempest_researcher_memory_dir <- function(output_dir) {
  file.path(normalizePath(output_dir, mustWork = TRUE), ".tempest-graft")
}

tempest_researcher_memory_record_path <- function(output_dir, report_id) {
  if (
    !rlang::is_string(report_id) ||
      !grepl("^research-run-[a-fA-F0-9]+$", report_id)
  ) {
    tempest_graft_memory_abort(
      "Invalid report identifier for project memory record.",
      class = "tempest_input_error"
    )
  }
  file.path(
    tempest_researcher_memory_dir(output_dir),
    paste0(report_id, ".json")
  )
}

tempest_researcher_memory_write_record <- function(
  output_dir,
  report_id,
  selection_id,
  research_run_id,
  bundle_id
) {
  dir_path <- tempest_researcher_memory_dir(output_dir)
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE, mode = "0700")
  }
  target <- tempest_researcher_memory_record_path(output_dir, report_id)
  payload <- list(
    schema_version = 1L,
    report_id = report_id,
    selection_id = selection_id,
    research_run_id = research_run_id,
    bundle_id = bundle_id,
    published_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )
  tmp <- tempfile("graft-record-", tmpdir = dir_path, fileext = ".json")
  on.exit(unlink(tmp, force = TRUE), add = TRUE)
  writeLines(
    as.character(jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null")),
    tmp,
    useBytes = TRUE
  )
  file.rename(tmp, target)
  target
}

tempest_researcher_memory_read_record <- function(output_dir, report_id) {
  target <- tryCatch(
    tempest_researcher_memory_record_path(output_dir, report_id),
    error = function(error) NULL
  )
  if (is.null(target) || !file.exists(target) || fs::is_link(target)) {
    return(NULL)
  }
  info <- fs::file_info(target, follow = FALSE)
  if (!isTRUE(info$type == "file" && info$size <= 64 * 1024)) {
    return(NULL)
  }
  parsed <- tryCatch(
    jsonlite::read_json(target, simplifyVector = FALSE),
    error = function(error) NULL
  )
  if (
    !is.list(parsed) ||
      !identical(parsed$schema_version, 1L) ||
      !identical(parsed$report_id, report_id) ||
      !rlang::is_string(parsed$selection_id) ||
      !nzchar(parsed$selection_id)
  ) {
    return(NULL)
  }
  parsed
}

tempest_researcher_memory_status_badge <- function(status, stream = NULL) {
  label <- switch(
    status %||% "provisional",
    accepted = if (nzchar(stream %||% "")) {
      paste("Accepted \u00b7", stream)
    } else {
      "Accepted"
    },
    superseded = if (nzchar(stream %||% "")) {
      paste("Superseded \u00b7", stream)
    } else {
      "Superseded"
    },
    withdrawn = if (nzchar(stream %||% "")) {
      paste("Withdrawn \u00b7", stream)
    } else {
      "Withdrawn"
    },
    provisional = "Provisional draft",
    "Not published to Graft"
  )
  tone <- switch(
    status %||% "provisional",
    accepted = "bg-success-subtle text-success-emphasis",
    superseded = "bg-warning-subtle text-warning-emphasis",
    withdrawn = "bg-secondary-subtle text-secondary-emphasis",
    provisional = "bg-info-subtle text-info-emphasis",
    "bg-light text-muted"
  )
  shiny::tags$span(
    class = paste("badge rounded-pill", tone),
    label
  )
}

tempest_researcher_memory_sidebar_ui <- function(streams = character()) {
  stream_choices <- c(
    "None (fresh research)" = "",
    stats::setNames(streams, streams)
  )
  bslib::accordion_panel(
    "Project memory (Graft)",
    value = "memory",
    shiny::p(
      class = "researcher-tools-note",
      paste(
        "Let research experts consult team-reviewed findings from Graft,",
        "or pin one accepted stream to track what changed."
      )
    ),
    shiny::checkboxInput(
      "use_memory_tools",
      "Let experts consult reviewed memory",
      value = TRUE
    ),
    shiny::selectInput(
      "memory_basis_stream",
      "Compare against accepted stream",
      choices = stream_choices,
      selected = ""
    ),
    shiny::div(
      class = "researcher-help",
      role = "status",
      `aria-live` = "polite",
      shiny::uiOutput("memory_sidebar_status")
    )
  )
}

tempest_researcher_memory_review_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("panel"))
}

tempest_researcher_memory_review_server <- function(
  id,
  store,
  output_dir,
  current_report,
  purpose = "research",
  default_actor = "local-reviewer",
  eligible = NULL,
  memory_revision = shiny::reactiveVal(0L)
) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    feedback <- shiny::reactiveVal(NULL)
    feedback_is_error <- shiny::reactiveVal(FALSE)

    record <- shiny::reactive({
      memory_revision()
      report <- current_report()
      if (is.null(report) || !rlang::is_string(report$path)) {
        return(NULL)
      }
      tempest_researcher_memory_read_record(output_dir, basename(report$path))
    })

    report_title <- shiny::reactive({
      report <- current_report()
      if (is.null(report) || !rlang::is_string(report$path)) {
        return("")
      }
      article <- tryCatch(
        jsonlite::read_json(
          file.path(report$path, "report.json"),
          simplifyVector = FALSE
        ),
        error = function(error) NULL
      )
      tempest_enriched_render_text(article$title, "")
    })

    selection_state <- shiny::reactive({
      memory_revision()
      rec <- record()
      if (is.null(rec)) {
        return(NULL)
      }
      tempest_graft_selection_status(
        store = store,
        selection_id = rec$selection_id,
        purpose = purpose,
        eligible = eligible
      )
    })

    shiny::observeEvent(current_report(), {
      feedback(NULL)
      feedback_is_error(FALSE)
    })

    shiny::observeEvent(input$accept, {
      rec <- record()
      if (is.null(rec)) {
        feedback("This report does not have a Graft research candidate.")
        feedback_is_error(TRUE)
        return()
      }
      if (!isTRUE(input$reviewed)) {
        feedback("Confirm that you reviewed the report and its cited evidence.")
        feedback_is_error(TRUE)
        return()
      }
      existing <- trimws(input$existing_stream %||% "")
      stream <- if (nzchar(existing)) {
        existing
      } else {
        trimws(input$stream %||% "")
      }
      tryCatch(
        {
          decision <- tempest_graft_accept_selection(
            store = store,
            selection_id = rec$selection_id,
            stream = stream,
            actor = input$actor %||% default_actor,
            reason = input$reason %||% "",
            purpose = purpose
          )
          feedback(sprintf(
            "Accepted into stream '%s' (decision %s).",
            decision@stream,
            substr(decision@id, 1L, 12L)
          ))
          feedback_is_error(FALSE)
          memory_revision(shiny::isolate(memory_revision()) + 1L)
        },
        error = function(error) {
          feedback(conditionMessage(error))
          feedback_is_error(TRUE)
        }
      )
    })

    shiny::observeEvent(input$withdraw, {
      state <- selection_state()
      if (is.null(state) || !identical(state$status, "accepted")) {
        feedback("Only a currently accepted stream can be withdrawn.")
        feedback_is_error(TRUE)
        return()
      }
      tryCatch(
        {
          decision <- tempest_graft_withdraw_stream(
            store = store,
            stream = state$stream,
            actor = input$actor %||% default_actor,
            reason = input$reason %||% "Withdrawn after review",
            expected = state$decision_id
          )
          feedback(sprintf(
            "Withdrew stream '%s' from active memory (decision %s). Historical reports remain readable.",
            decision@stream,
            substr(decision@id, 1L, 12L)
          ))
          feedback_is_error(FALSE)
          memory_revision(shiny::isolate(memory_revision()) + 1L)
        },
        error = function(error) {
          feedback(conditionMessage(error))
          feedback_is_error(TRUE)
        }
      )
    })

    output$panel <- shiny::renderUI({
      rec <- record()
      if (is.null(rec)) {
        return(NULL)
      }
      state <- selection_state()
      existing_streams <- tempest_graft_memory_streams(
        store,
        purpose = purpose,
        eligible = eligible
      )$stream
      default_stream <- state$stream %||%
        tempest_graft_stream_slug(report_title())
      existing_choices <- c(
        "Create new stream below" = "",
        stats::setNames(existing_streams, existing_streams)
      )
      selected_existing <- if (default_stream %in% existing_streams) {
        default_stream
      } else {
        ""
      }

      history_ui <- if (!is.null(state) && nrow(state$history) > 0L) {
        shiny::tags$details(
          class = "mt-2",
          shiny::tags$summary(sprintf(
            "Decision history (%d)",
            nrow(state$history)
          )),
          shiny::tags$table(
            class = "table table-sm mt-2 mb-0",
            shiny::tags$thead(shiny::tags$tr(
              shiny::tags$th("#"),
              shiny::tags$th("Action"),
              shiny::tags$th("Reviewer"),
              shiny::tags$th("Reason"),
              shiny::tags$th("Decision")
            )),
            shiny::tags$tbody(lapply(
              seq_len(nrow(state$history)),
              function(i) {
                row <- state$history[i, , drop = FALSE]
                shiny::tags$tr(
                  shiny::tags$td(row$sequence),
                  shiny::tags$td(row$action),
                  shiny::tags$td(row$actor),
                  shiny::tags$td(row$reason),
                  shiny::tags$td(shiny::tags$code(substr(
                    row$decision_id,
                    1L,
                    12L
                  )))
                )
              }
            ))
          )
        )
      } else {
        NULL
      }

      notice_ui <- if (!is.null(feedback())) {
        shiny::div(
          class = if (isTRUE(feedback_is_error())) {
            "alert alert-warning py-2 px-3 mt-2 mb-0"
          } else {
            "alert alert-success py-2 px-3 mt-2 mb-0"
          },
          role = if (isTRUE(feedback_is_error())) "alert" else "status",
          feedback()
        )
      } else {
        NULL
      }

      shiny::tags$section(
        class = "researcher-memory-review card mb-3",
        `aria-label` = "Review and project memory",
        shiny::div(
          class = "card-body",
          shiny::div(
            class = "d-flex flex-wrap justify-content-between align-items-center gap-2 mb-2",
            shiny::tags$strong("Project memory (Graft)"),
            tempest_researcher_memory_status_badge(state$status, state$stream)
          ),
          shiny::p(
            class = "small text-body-secondary mb-2",
            paste(
              "Accepting records your explicit review in Graft so future",
              "research runs and chats can consult these verified claims and sources."
            )
          ),
          shiny::tags$details(
            open = if (!identical(state$status, "accepted")) "open" else NULL,
            shiny::tags$summary(
              if (identical(state$status, "accepted")) {
                "Update or withdraw accepted memory"
              } else {
                "Review and accept into project memory"
              }
            ),
            shiny::div(
              class = "mt-2 d-grid gap-2",
              if (length(existing_streams) > 0L) {
                shiny::selectInput(
                  ns("existing_stream"),
                  "Update an existing stream (optional)",
                  choices = existing_choices,
                  selected = selected_existing
                )
              },
              shiny::textInput(
                ns("stream"),
                "New stream name",
                value = default_stream
              ),
              shiny::textInput(
                ns("actor"),
                "Reviewer",
                value = default_actor
              ),
              shiny::textAreaInput(
                ns("reason"),
                "Review reason",
                placeholder = "Why is this finding ready to remember (or withdraw)?",
                rows = 2
              ),
              shiny::checkboxInput(
                ns("reviewed"),
                "I reviewed this report and its cited evidence",
                value = FALSE
              ),
              shiny::div(
                class = "d-flex flex-wrap gap-2",
                shiny::actionButton(
                  ns("accept"),
                  if (identical(state$status, "accepted")) {
                    "Record new acceptance"
                  } else {
                    "Accept to project memory"
                  },
                  class = "btn-primary btn-sm"
                ),
                if (identical(state$status, "accepted")) {
                  shiny::actionButton(
                    ns("withdraw"),
                    "Withdraw from active memory",
                    class = "btn-outline-secondary btn-sm"
                  )
                }
              )
            )
          ),
          notice_ui,
          history_ui
        )
      )
    })

    list(
      record = record,
      status = selection_state
    )
  })
}
