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
  selection_id = NULL,
  research_run_id = NULL,
  bundle_id = NULL,
  publish_error = NULL,
  stream = NULL
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
    stream = stream,
    publish_error = publish_error,
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
  has_selection <- rlang::is_string(parsed$selection_id) &&
    nzchar(parsed$selection_id)
  has_error <- rlang::is_string(parsed$publish_error) &&
    nzchar(parsed$publish_error)
  if (
    !is.list(parsed) ||
      !identical(parsed$schema_version, 1L) ||
      !identical(parsed$report_id, report_id) ||
      (!has_selection && !has_error)
  ) {
    return(NULL)
  }
  parsed
}

tempest_researcher_memory_status_badge <- function(status, stream = NULL) {
  tempest_graft_memory_status_badge(status = status, stream = stream)
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

    displayed_streams <- shiny::reactive({
      memory_revision()
      tempest_graft_memory_streams(
        store,
        purpose = purpose,
        eligible = eligible
      )
    })

    selection_state <- shiny::reactive({
      memory_revision()
      rec <- record()
      if (
        is.null(rec) ||
          !rlang::is_string(rec$selection_id) ||
          !nzchar(rec$selection_id)
      ) {
        return(NULL)
      }
      tempest_graft_selection_status(
        store = store,
        selection_id = rec$selection_id,
        purpose = purpose,
        eligible = eligible,
        stream = rec$stream
      )
    })

    shiny::observeEvent(current_report(), {
      feedback(NULL)
      feedback_is_error(FALSE)
    })

    shiny::observeEvent(input$refresh, {
      feedback(NULL)
      feedback_is_error(FALSE)
      memory_revision(shiny::isolate(memory_revision()) + 1L)
    })

    shiny::observeEvent(input$accept, {
      rec <- record()
      if (
        is.null(rec) ||
          !rlang::is_string(rec$selection_id) ||
          !nzchar(rec$selection_id)
      ) {
        feedback("This report does not have a Graft research candidate.")
        feedback_is_error(TRUE)
        return()
      }
      tempest_graft_memory_handle_accept(
        input = input,
        store = store,
        purpose = purpose,
        default_actor = default_actor,
        streams_df = displayed_streams(),
        state = selection_state(),
        resolve_selection = function() rec$selection_id,
        feedback = feedback,
        feedback_is_error = feedback_is_error,
        memory_revision = memory_revision,
        on_accepted = function(decision) {
          tryCatch(
            tempest_researcher_memory_write_record(
              output_dir = output_dir,
              report_id = rec$report_id,
              selection_id = rec$selection_id,
              research_run_id = rec$research_run_id,
              bundle_id = rec$bundle_id,
              stream = decision@stream
            ),
            error = function(error) NULL
          )
        }
      )
    })

    shiny::observeEvent(input$withdraw, {
      tempest_graft_memory_handle_withdraw(
        input = input,
        store = store,
        default_actor = default_actor,
        state = selection_state(),
        feedback = feedback,
        feedback_is_error = feedback_is_error,
        memory_revision = memory_revision,
        success_format = paste0(
          "Withdrew stream '%s' from active memory (decision %s). ",
          "Historical reports remain readable."
        )
      )
    })

    output$panel <- shiny::renderUI({
      rec <- record()
      if (is.null(rec)) {
        return(NULL)
      }
      if (!rlang::is_string(rec$selection_id) || !nzchar(rec$selection_id)) {
        return(shiny::tags$section(
          class = "researcher-memory-review card mb-3",
          `aria-label` = "Review and project memory",
          shiny::div(
            class = "card-body",
            shiny::div(
              class = "d-flex flex-wrap justify-content-between align-items-center gap-2 mb-2",
              shiny::tags$strong("Project memory (Graft)"),
              tempest_graft_memory_status_badge("unlinked")
            ),
            shiny::p(
              class = "small text-body-secondary mb-0",
              paste(
                "This report completed normally, but no Graft memory candidate",
                "was published:",
                rec$publish_error %||% "promotion requirements were not met."
              )
            )
          )
        ))
      }
      state <- selection_state()
      default_stream <- state$stream %||%
        tempest_graft_stream_slug(report_title())
      tempest_graft_memory_review_card_ui(
        ns = ns,
        heading = "Project memory (Graft)",
        status_code = state$status,
        stream = state$stream,
        existing_streams = displayed_streams()$stream,
        default_stream = default_stream,
        default_actor = default_actor,
        feedback_text = feedback(),
        feedback_is_error = feedback_is_error(),
        state = state,
        card_class = "researcher-memory-review card mb-3",
        card_tag = "section",
        aria_label = "Review and project memory",
        intro_text = paste(
          "Accepting records your explicit review in Graft so future",
          "research runs and chats can consult these verified claims and sources."
        ),
        summary_new = "Review and accept into project memory",
        accept_label = if (identical(state$status, "accepted")) {
          "Record new acceptance"
        } else {
          "Accept to project memory"
        },
        open_when_unaccepted = TRUE
      )
    })

    list(
      record = record,
      status = selection_state
    )
  })
}
