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
        bslib::input_task_button(
          ns("run"),
          "Run STORM Pipeline",
          icon = shiny::icon("bolt"),
          label_busy = "Running...",
          class = "w-100"
        ),
        shiny::uiOutput(ns("cancel_control"))
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("STORM Pipeline Progress"),
        bslib::card_body(
          shiny::div(
            role = "status",
            `aria-live` = "polite",
            `aria-atomic` = "true",
            shiny::uiOutput(ns("progress"))
          ),
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
    validation_error <- shiny::reactiveVal(NULL)
    publication_error <- shiny::reactiveVal(NULL)
    published_result <- shiny::reactiveVal(NULL)
    last_successful_product <- shiny::reactiveVal(NULL)
    progress_stream <- new.env(parent = emptyenv())
    progress_stream$path <- NULL
    progress_stream$token <- 0L
    progress_stream$active <- FALSE
    worker_state <- new.env(parent = emptyenv())
    worker_state$job <- NULL
    worker_state$cancelled <- FALSE
    worker_state$topic <- NULL
    worker_state$run_id <- NULL
    worker_state$config <- NULL

    finish_storm_worker <- function() {
      stream_path <- progress_stream$path
      progress_stream$active <- FALSE
      progress_stream$path <- NULL
      worker_state$job <- NULL
      worker_state$topic <- NULL
      worker_state$run_id <- NULL
      worker_state$config <- NULL
      storm_cleanup_progress_stream(stream_path)
      invisible(stream_path)
    }

    fail_storm_publication <- function(message) {
      publication_error(message)
      published_result(NULL)
      finish_storm_worker()
      invisible(FALSE)
    }

    storm_task <- shiny::ExtendedTask$new(
      function(
        topic,
        cfg,
        n_experts,
        strategy,
        max_rounds,
        progress_stream_path,
        progress_run_id,
        otel_enabled
      ) {
        job <- mirai::mirai(
          {
            storm_runner(
              topic = topic,
              cfg = cfg,
              n_experts = n_experts,
              strategy = strategy,
              max_rounds = max_rounds,
              package_root = package_root,
              progress_stream_path = progress_stream_path,
              progress_run_id = progress_run_id,
              progress_collector = progress_collector,
              tempest_run_factory = tempest_run_factory,
              otel_enabled = otel_enabled
            )
          },
          topic = topic,
          cfg = cfg,
          n_experts = n_experts,
          strategy = strategy,
          max_rounds = max_rounds,
          progress_stream_path = progress_stream_path,
          progress_run_id = progress_run_id,
          otel_enabled = otel_enabled,
          package_root = storm_package_root(),
          progress_collector = storm_worker_progress_collector,
          storm_runner = storm_run_with_progress,
          tempest_run_factory = storm_worker_tempest_run
        )
        worker_state$job <- job
        promises::catch(job, function(error) {
          if (storm_task_was_cancelled(job, error)) {
            return(NULL)
          }
          stop(error)
        })
      }
    ) |>
      bslib::bind_task_button("run")

    shiny::observeEvent(input$run, {
      if (
        identical(shiny::isolate(storm_task$status()), "running") ||
          isTRUE(progress_stream$active) ||
          !is.null(worker_state$job)
      ) {
        return()
      }
      topic <- stringi::stri_trim_both(input$topic %||% "")
      if (!nzchar(topic)) {
        validation_error(
          "Enter a research topic before running the STORM pipeline."
        )
        bslib::update_task_button("run", state = "ready", session = session)
        return()
      }
      validation_error(NULL)
      publication_error(NULL)
      published_result(NULL)
      progress_stream$active <- FALSE
      worker_state$cancelled <- FALSE
      worker_state$job <- NULL
      worker_state$topic <- topic
      progress_stream$token <- progress_stream$token + 1L
      progress_stream$path <- storm_progress_stream_path()
      run_token <- progress_stream$token
      progress_stream$active <- TRUE
      progress_run_id <- storm_progress_run_id()
      worker_state$run_id <- progress_run_id
      worker_state$config <- shiny::isolate(config())
      progress_events(list(storm_running_event(topic, progress_run_id)))
      storm_poll_progress_stream(
        path = progress_stream$path,
        progress_events = progress_events,
        session = session,
        is_current = function() {
          isTRUE(progress_stream$active) &&
            identical(run_token, progress_stream$token)
        }
      )
      otel_enabled <- tempest:::tempest_otel_worker_intent()
      storm_task$invoke(
        topic = topic,
        cfg = worker_state$config,
        n_experts = input$n_experts %||% 3,
        strategy = input$strategy %||% "key_questions",
        max_rounds = input$max_rounds %||% 3,
        progress_stream_path = progress_stream$path,
        progress_run_id = progress_run_id,
        otel_enabled = otel_enabled
      )
    })

    shiny::observeEvent(input$cancel, {
      if (!storm_cancel_worker(worker_state$job)) {
        return()
      }
      worker_state$cancelled <- TRUE
      progress_stream$active <- FALSE
      progress_events(storm_merge_progress_events(
        shiny::isolate(progress_events()),
        list(storm_cancelled_event(
          worker_state$topic %||% "STORM run",
          worker_state$run_id %||% storm_progress_run_id()
        ))
      ))
    })

    # Share the report once the pipeline succeeds.
    shiny::observeEvent(storm_task$status(), {
      if (identical(storm_task$status(), "success")) {
        if (isTRUE(worker_state$cancelled)) {
          finish_storm_worker()
          return()
        }
        value <- storm_task$result()
        envelope <- tryCatch(
          storm_task_envelope(value),
          error = function(error) NULL
        )
        result_run_id <- if (is.null(envelope)) {
          worker_state$run_id
        } else {
          storm_result_run_id(envelope$result)
        }
        if (!identical(result_run_id, worker_state$run_id)) {
          fail_storm_publication(
            "The STORM result could not be published."
          )
          return()
        }
        if (is.null(envelope)) {
          fail_storm_publication(
            "The STORM result could not be published."
          )
          return()
        }
        progress_events(storm_merge_progress_events(
          shiny::isolate(progress_events()),
          envelope$progress
        ))
        published <- tryCatch(
          {
            store$publish_storm_report(
              envelope$result,
              config = worker_state$config
            )
            TRUE
          },
          error = function(error) {
            fail_storm_publication(
              "The STORM report failed product integrity validation."
            )
            FALSE
          }
        )
        if (!isTRUE(published)) {
          return()
        }
        published_result(envelope$result)
        last_successful_product(envelope$result)
        finish_storm_worker()
      } else if (identical(storm_task$status(), "error")) {
        progress_events(storm_merge_progress_events(
          shiny::isolate(progress_events()),
          storm_read_progress_stream(progress_stream$path)
        ))
        finish_storm_worker()
      }
    })

    session$onSessionEnded(function() {
      storm_cancel_worker(worker_state$job)
      finish_storm_worker()
    })

    output$cancel_control <- shiny::renderUI({
      if (!identical(storm_task$status(), "running")) {
        return(NULL)
      }
      shiny::actionButton(
        ns("cancel"),
        "Cancel STORM run",
        icon = shiny::icon("stop"),
        class = "btn-outline-danger w-100 mt-2"
      )
    })

    output$progress <- shiny::renderUI({
      status <- storm_task$status()
      if (identical(status, "success") && !is.null(publication_error())) {
        status <- "error"
      }
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
      if (!is.null(validation_error())) {
        return(shiny::div(
          class = "alert alert-danger mt-3",
          role = "alert",
          shiny::icon("triangle-exclamation"),
          " ",
          validation_error()
        ))
      }
      if (isTRUE(worker_state$cancelled)) {
        return(shiny::div(
          class = "alert alert-warning mt-3",
          role = "alert",
          shiny::icon("ban"),
          " STORM run cancelled."
        ))
      }
      status <- storm_task$status()
      if (identical(status, "success") && !is.null(publication_error())) {
        return(shiny::div(
          class = "alert alert-danger mt-3",
          role = "alert",
          shiny::icon("triangle-exclamation"),
          " ",
          publication_error()
        ))
      }
      if (identical(status, "error")) {
        msg <- "The pipeline failed."
        return(shiny::div(
          class = "alert alert-danger mt-3",
          role = "alert",
          shiny::icon("triangle-exclamation"),
          " Pipeline error: ",
          msg
        ))
      }
      if (!identical(status, "success")) {
        return(NULL)
      }
      result <- published_result()
      if (is.null(result)) {
        return(NULL)
      }
      chars <- nchar(result@report_md %||% "")
      shiny::div(
        class = "alert alert-success mt-3",
        role = "status",
        `aria-live` = "polite",
        `aria-atomic` = "true",
        shiny::icon("circle-check"),
        sprintf(" Pipeline complete! Report: %d characters. ", chars),
        shiny::actionLink(
          ns("view_report"),
          "View Report",
          class = "alert-link"
        )
      )
    })

    report_navigation_event <- shiny::reactiveVal(0L)
    shiny::observeEvent(input$view_report, {
      if (is.null(published_result())) {
        return()
      }
      report_navigation_event(report_navigation_event() + 1L)
    })

    list(
      storm_events = shiny::reactive(progress_events()),
      last_successful_product = shiny::reactive(
        last_successful_product()
      ),
      report_navigation_event = shiny::reactive(
        report_navigation_event()
      )
    )
  })
}

storm_progress_run_id <- function() {
  tempest:::tempest_uuid("shiny-storm")
}

storm_running_event <- function(topic, run_id = storm_progress_run_id()) {
  tempest:::tempest_progress_event(
    run_id = run_id,
    workflow = "storm",
    event_type = "workflow",
    status = "started",
    message = paste("Running STORM pipeline for", topic)
  )
}

storm_worker_progress_collector <- function(
  include_payload = FALSE,
  stream_path = NULL
) {
  include_payload <- isTRUE(include_payload)
  events <- list()

  event_data <- function(event) {
    data <- tempest:::tempest_progress_event_data(event)
    if (!include_payload) {
      data$payload <- list()
    }
    data
  }

  list(
    record = function(event) {
      data <- event_data(event)
      events[[length(events) + 1L]] <<- data
      storm_append_progress_stream(stream_path, data)
      invisible(event)
    },
    data = function() events
  )
}

storm_progress_stream_path <- function() {
  path <- tempfile("tempest-storm-progress-", fileext = ".ndjson")
  file.create(path)
  path
}

storm_append_progress_stream <- function(path, event) {
  if (is.null(path) || !nzchar(path)) {
    return(invisible(FALSE))
  }
  ok <- tryCatch(
    {
      json <- jsonlite::toJSON(
        event,
        auto_unbox = TRUE,
        null = "null",
        na = "null"
      )
      cat(json, "\n", file = path, append = TRUE, sep = "")
      TRUE
    },
    error = function(e) FALSE
  )
  invisible(ok)
}

storm_read_progress_stream <- function(path) {
  if (is.null(path) || !nzchar(path) || !file.exists(path)) {
    return(list())
  }
  storm_parse_progress_lines(readLines(path, warn = FALSE))
}

# Parse NDJSON progress lines, skipping blank or unparsable lines so a
# partially-written or malformed line never breaks progress rendering.
storm_parse_progress_lines <- function(lines) {
  lines <- lines[nzchar(lines)]
  events <- lapply(lines, function(line) {
    tryCatch(
      storm_normalize_progress_stream_event(
        jsonlite::fromJSON(line, simplifyVector = FALSE)
      ),
      error = function(e) NULL
    )
  })
  events[!vapply(events, is.null, logical(1))]
}

# A mutable cursor tracking the byte offset already consumed from a stream,
# so each poll only reads and parses newly appended bytes.
storm_progress_stream_cursor <- function() {
  cursor <- new.env(parent = emptyenv())
  cursor$offset <- 0
  cursor
}

# Read only the bytes appended since the cursor's last offset. A trailing
# partial line (no terminating newline) is left unconsumed for the next poll.
storm_read_progress_stream_incremental <- function(path, cursor) {
  if (is.null(path) || !nzchar(path) || !file.exists(path)) {
    return(list())
  }
  size <- file.info(path)$size
  if (is.na(size) || size <= cursor$offset) {
    return(list())
  }
  con <- file(path, open = "rb")
  on.exit(close(con))
  if (cursor$offset > 0) {
    seek(con, where = cursor$offset, origin = "start")
  }
  bytes <- readBin(con, what = "raw", n = size - cursor$offset)
  newlines <- which(bytes == as.raw(0x0a))
  if (length(newlines) == 0L) {
    return(list())
  }
  last <- newlines[length(newlines)]
  complete <- rawToChar(bytes[seq_len(last)])
  Encoding(complete) <- "UTF-8"
  cursor$offset <- cursor$offset + last
  storm_parse_progress_lines(strsplit(complete, "\n", fixed = TRUE)[[1]])
}

storm_normalize_progress_stream_event <- function(event) {
  for (field in c(
    "stage",
    "step",
    "message",
    "parent_event_id",
    "correlation_id"
  )) {
    if (is.null(event[[field]])) {
      event[[field]] <- NA_character_
    }
  }
  if (is.null(event$payload)) {
    event$payload <- list()
  }
  event
}

storm_poll_progress_stream <- function(
  path,
  progress_events,
  session,
  is_current,
  interval = 0.2
) {
  cursor <- storm_progress_stream_cursor()
  poll <- function() {
    if (!isTRUE(is_current())) {
      return(invisible(FALSE))
    }
    events <- storm_read_progress_stream_incremental(path, cursor)
    if (length(events) > 0L) {
      shiny::withReactiveDomain(session, {
        progress_events(storm_merge_progress_events(
          shiny::isolate(progress_events()),
          events
        ))
      })
    }
    later::later(poll, delay = interval)
    invisible(TRUE)
  }

  later::later(poll, delay = 0)
  invisible(path)
}

storm_merge_progress_events <- function(current, incoming) {
  merged <- c(current %||% list(), incoming %||% list())
  if (length(merged) == 0L) {
    return(list())
  }
  ids <- vapply(
    merged,
    storm_progress_event_id,
    character(1)
  )
  keep <- !duplicated(ids) | is.na(ids)
  merged[keep]
}

storm_progress_event_id <- function(event) {
  if (S7::S7_inherits(event, tempest:::tempest_progress_event)) {
    return(S7::prop(event, "event_id"))
  }
  if (is.list(event)) {
    return(event$event_id %||% NA_character_)
  }
  NA_character_
}

storm_cleanup_progress_stream <- function(path) {
  if (!is.null(path) && nzchar(path) && file.exists(path)) {
    unlink(path)
  }
  invisible(path)
}

storm_run_with_progress <- function(
  topic,
  cfg,
  n_experts,
  strategy,
  max_rounds,
  package_root = NULL,
  progress_stream_path = NULL,
  progress_run_id = NULL,
  progress_collector = storm_worker_progress_collector,
  tempest_run_factory = storm_worker_tempest_run,
  otel_enabled = FALSE
) {
  collector <- progress_collector(
    include_payload = TRUE,
    stream_path = progress_stream_path
  )
  tempest_run <- tempest_run_factory(package_root)
  result <- tempest:::tempest_otel_worker_call(
    tempest_run,
    list(
      topic = topic,
      config = cfg,
      n_experts = n_experts,
      research_strategy = strategy,
      max_rounds = max_rounds,
      run_id = progress_run_id,
      progress = collector$record,
      verbose = FALSE
    ),
    otel_enabled
  )
  list(
    result = result,
    progress = collector$data()
  )
}

storm_worker_tempest_run <- function(package_root = NULL) {
  storm_worker_load_checkout(package_root)
  getExportedValue("tempest", "tempest_run")
}

storm_is_source_checkout <- function(package_root = NULL) {
  if (
    !is.character(package_root) ||
      length(package_root) != 1L ||
      is.na(package_root) ||
      !nzchar(package_root)
  ) {
    return(FALSE)
  }
  desc <- file.path(package_root, "DESCRIPTION")
  if (
    !file.exists(desc) ||
      !file.exists(file.path(package_root, "R", "storm.R"))
  ) {
    return(FALSE)
  }
  package <- tryCatch(
    read.dcf(desc, fields = "Package")[[1]],
    error = function(e) NA_character_
  )
  identical(package, "tempest")
}

storm_worker_load_checkout <- function(package_root = NULL) {
  if (
    !storm_is_source_checkout(package_root) ||
      !requireNamespace("pkgload", quietly = TRUE)
  ) {
    return(FALSE)
  }
  isTRUE(tryCatch(
    {
      pkgload::load_all(
        package_root,
        attach = FALSE,
        export_all = FALSE,
        helpers = FALSE,
        attach_testthat = FALSE,
        quiet = TRUE
      )
      TRUE
    },
    error = function(e) FALSE
  ))
}

storm_package_root <- function() {
  package_dir <- system.file(package = "tempest")
  if (!nzchar(package_dir)) {
    return(NULL)
  }
  candidates <- c(package_dir, file.path(package_dir, ".."))
  for (candidate in candidates) {
    if (!storm_is_source_checkout(candidate)) {
      next
    }
    return(normalizePath(candidate, mustWork = FALSE))
  }
  NULL
}

storm_progress_state <- function(events, task_status = "initial") {
  if (length(events) > 0L) {
    state <- tryCatch(
      tempest:::tempest_progress_state(events),
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
  tempest:::tempest_progress_state(list(
    tempest:::tempest_progress_event(
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

storm_task_envelope <- function(value) {
  valid <- is.list(value) &&
    !is.data.frame(value) &&
    identical(names(value), c("result", "progress")) &&
    is.list(value$progress) &&
    !is.data.frame(value$progress)
  if (!isTRUE(valid)) {
    stop(
      "STORM worker output must contain exact result and progress fields.",
      call. = FALSE
    )
  }
  value
}

storm_result_run_id <- function(result) {
  run_id <- tryCatch(
    result@manifest@research_run_id,
    error = function(error) NULL
  )
  if (!is.character(run_id) || length(run_id) != 1L || is.na(run_id)) {
    return(NULL)
  }
  run_id
}

storm_cancel_worker <- function(job) {
  if (is.null(job) || !mirai::unresolved(job)) {
    return(FALSE)
  }
  mirai::stop_mirai(job)
  TRUE
}

storm_task_was_cancelled <- function(job, error) {
  value <- tryCatch(job$data, error = function(...) NULL)
  inherits(value, "errorValue") &&
    identical(unclass(value), 20L) &&
    identical(conditionMessage(error), "20 | Operation canceled")
}

storm_cancelled_event <- function(topic, run_id) {
  tempest:::tempest_progress_event(
    run_id = run_id,
    workflow = "storm",
    event_type = "cancellation",
    status = "cancelled",
    message = paste("Cancelled STORM research for", topic),
    payload = list(topic = topic)
  )
}

storm_stage_labels <- function() {
  tempest:::tempest_progress_labels("storm", kind = "stage")
}
