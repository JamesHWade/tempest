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
    progress_stream <- new.env(parent = emptyenv())
    progress_stream$path <- NULL
    progress_stream$token <- 0L
    progress_stream$active <- FALSE

    storm_task <- shiny::ExtendedTask$new(
      function(
        topic,
        cfg,
        n_experts,
        strategy,
        max_rounds,
        parallel,
        progress_stream_path,
        progress_run_id
      ) {
        mirai::mirai(
          {
            storm_runner(
              topic = topic,
              cfg = cfg,
              n_experts = n_experts,
              strategy = strategy,
              max_rounds = max_rounds,
              parallel = parallel,
              package_root = package_root,
              progress_stream_path = progress_stream_path,
              progress_run_id = progress_run_id,
              progress_collector = progress_collector,
              tempest_run_factory = tempest_run_factory
            )
          },
          topic = topic,
          cfg = cfg,
          n_experts = n_experts,
          strategy = strategy,
          max_rounds = max_rounds,
          parallel = parallel,
          progress_stream_path = progress_stream_path,
          progress_run_id = progress_run_id,
          package_root = storm_package_root(),
          progress_collector = storm_worker_progress_collector,
          storm_runner = storm_run_with_progress,
          tempest_run_factory = storm_worker_tempest_run
        )
      }
    ) |>
      bslib::bind_task_button("run")

    shiny::observeEvent(input$run, {
      topic <- stringi::stri_trim_both(input$topic %||% "")
      shiny::req(nzchar(topic))
      progress_stream$active <- FALSE
      progress_stream$token <- progress_stream$token + 1L
      progress_stream$path <- storm_progress_stream_path()
      run_token <- progress_stream$token
      progress_stream$active <- TRUE
      progress_run_id <- storm_progress_run_id()
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
      storm_task$invoke(
        topic = topic,
        cfg = config(),
        n_experts = input$n_experts %||% 3,
        strategy = input$strategy %||% "key_questions",
        max_rounds = input$max_rounds %||% 3,
        parallel = input$parallel %||% FALSE,
        progress_stream_path = progress_stream$path,
        progress_run_id = progress_run_id
      )
    })

    # Share the report once the pipeline succeeds.
    shiny::observeEvent(storm_task$status(), {
      if (identical(storm_task$status(), "success")) {
        progress_stream$active <- FALSE
        value <- storm_task$result()
        progress_events(storm_merge_progress_events(
          shiny::isolate(progress_events()),
          storm_task_progress(value)
        ))
        result <- storm_task_result(value)
        if (is.null(result)) {
          return()
        }
        store$set_report(
          result$report_md %||% "",
          shiny::isolate(input$topic) %||% "STORM Report",
          source_store = result$store %||% NULL
        )
        storm_cleanup_progress_stream(progress_stream$path)
      } else if (identical(storm_task$status(), "error")) {
        progress_stream$active <- FALSE
        progress_events(storm_merge_progress_events(
          shiny::isolate(progress_events()),
          storm_read_progress_stream(progress_stream$path)
        ))
        storm_cleanup_progress_stream(progress_stream$path)
      }
    })

    session$onSessionEnded(function() {
      progress_stream$active <- FALSE
      storm_cleanup_progress_stream(progress_stream$path)
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

storm_progress_run_id <- function() {
  paste0(
    "shiny-storm-",
    format(Sys.time(), "%Y%m%d%H%M%OS3")
  )
}

storm_running_event <- function(topic, run_id = storm_progress_run_id()) {
  tempest::tempest_progress_event(
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
    data <- tempest::tempest_progress_event_data(event)
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
  if (S7::S7_inherits(event, tempest::tempest_progress_event)) {
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
  parallel,
  package_root = NULL,
  progress_stream_path = NULL,
  progress_run_id = NULL,
  progress_collector = storm_worker_progress_collector,
  tempest_run_factory = storm_worker_tempest_run,
  tempest_run = NULL
) {
  collector <- storm_create_worker_progress_collector(
    progress_collector,
    progress_stream_path
  )
  if (is.null(tempest_run)) {
    tempest_run <- tempest_run_factory(package_root)
  }
  args <- list(
    topic = topic,
    config = cfg,
    n_experts = n_experts,
    research_strategy = strategy,
    max_rounds = max_rounds,
    parallel_research = parallel,
    run_id = progress_run_id,
    progress = collector$record,
    verbose = FALSE
  )
  fn_args <- names(formals(tempest_run))
  if (!"..." %in% fn_args) {
    args <- args[names(args) %in% fn_args]
  }
  list(
    result = do.call(tempest_run, args),
    progress = collector$data()
  )
}

storm_create_worker_progress_collector <- function(
  progress_collector,
  progress_stream_path = NULL
) {
  args <- list(include_payload = TRUE)
  fn_args <- names(formals(progress_collector))
  if ("..." %in% fn_args || "stream_path" %in% fn_args) {
    args$stream_path <- progress_stream_path
  }
  do.call(progress_collector, args)
}

storm_worker_tempest_run <- function(package_root = NULL) {
  storm_worker_load_checkout(package_root)
  getExportedValue("tempest", "tempest_run")
}

storm_worker_load_checkout <- function(package_root = NULL) {
  if (
    is.null(package_root) ||
      !nzchar(package_root) ||
      !file.exists(file.path(package_root, "DESCRIPTION")) ||
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
    desc <- file.path(candidate, "DESCRIPTION")
    if (!file.exists(desc)) {
      next
    }
    package <- tryCatch(
      read.dcf(desc, fields = "Package")[[1]],
      error = function(e) NA_character_
    )
    if (identical(package, "tempest")) {
      return(normalizePath(candidate, mustWork = FALSE))
    }
  }
  NULL
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
  tempest::tempest_progress_labels("storm", kind = "stage")
}
