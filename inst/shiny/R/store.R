# Shared reactive state for the tempest app.
#
# The Co-STORM `TempestSession` is an R6 object that is mutated in place (chat
# tools add sources and facts, the mind map updates, etc.). Rather than spread
# a manual invalidation trigger across every output, this store centralises it:
# `get()` is a version-aware read, `set()` swaps the session, and `touch()`
# signals "the session changed in place, re-render". The generated report is
# kept here too so the Report tab is decoupled from any particular session.

new_session_store <- function() {
  rv <- shiny::reactiveValues(
    session = NULL,
    run = NULL,
    prefer_run_evidence = FALSE,
    version = 0L,
    run_version = 0L,
    # Separate counter for autosave. It tracks in-place content changes (`set()`
    # and `touch()`) but not `restore()`, so loading a bundle re-renders outputs
    # without immediately writing the just-loaded session back to disk and
    # clobbering the "restored" status.
    autosave_version = 0L,
    report_md = NULL,
    report_topic = NULL,
    report_source_store = NULL,
    persistence_status = "idle",
    persistence_message = NULL,
    persistence_path = NULL
  )

  bump_version <- function(autosave = TRUE) {
    version <- shiny::isolate(rv$version)
    if (is.null(version) || is.na(version)) {
      version <- 0L
    }
    version <- version + 1L
    rv$version <- version
    if (isTRUE(autosave)) {
      autosave_version <- shiny::isolate(rv$autosave_version)
      if (is.null(autosave_version) || is.na(autosave_version)) {
        autosave_version <- 0L
      }
      rv$autosave_version <- autosave_version + 1L
    }
    invisible(version)
  }

  set_persistence <- function(status, path = NULL, message = NULL) {
    rv$persistence_status <- status
    rv$persistence_path <- path
    rv$persistence_message <- message
    invisible(NULL)
  }

  set_report_from_session <- function(session) {
    report_md <- if (
      !is.null(session$artifact_catalog) &&
        session$artifact_catalog$has("report_md")
    ) {
      session$artifact_catalog$get("report_md")@content
    } else {
      session$artifacts[["report_md"]] %||% NULL
    }
    rv$report_md <- report_md
    rv$report_topic <- if (is.null(report_md)) NULL else session$topic
    rv$report_source_store <- if (is.null(report_md)) NULL else session$store
    invisible(report_md)
  }

  list(
    # Non-reactive read for observers that should not take a dependency.
    peek = function() {
      shiny::isolate(rv$session)
    },

    # Change counter for debounced autosave observers. Excludes `restore()` so
    # loading a bundle does not trigger an immediate write-back.
    autosave_trigger = shiny::reactive(rv$autosave_version),

    # Version-aware read of the current session. Re-fires on set()/touch().
    get = shiny::reactive({
      rv$version
      rv$session
    }),

    # Version-aware read of a generic TempestRun supplied by the host or owned
    # by a built-in workflow.
    get_run = shiny::reactive({
      rv$run_version
      rv$run
    }),

    # Prefer Co-STORM session evidence when a session is active. Otherwise,
    # expose the evidence ledger owned by a generic workflow run.
    evidence_store = shiny::reactive({
      rv$run_version
      current_run <- rv$run
      if (
        isTRUE(rv$prefer_run_evidence) &&
          !is.null(current_run)
      ) {
        return(current_run$source_store)
      }

      rv$version
      current_session <- rv$session
      if (!is.null(current_session)) {
        return(current_session$store)
      }

      if (is.null(current_run)) {
        return(NULL)
      }
      current_run$source_store
    }),

    peek_run = function() {
      shiny::isolate(rv$run)
    },

    # Replace the session (e.g. on session start).
    set = function(session) {
      rv$session <- session
      workflow_run <- tryCatch(
        session$workflow_run,
        error = function(error) NULL
      )
      if (inherits(workflow_run, "TempestRun")) {
        rv$run <- workflow_run
        rv$prefer_run_evidence <- FALSE
        rv$run_version <- shiny::isolate(rv$run_version) + 1L
      }
      bump_version()
      invisible(session)
    },

    set_run = function(run, prefer_evidence = FALSE) {
      if (!is.null(run) && !inherits(run, "TempestRun")) {
        stop("run must be NULL or a TempestRun.", call. = FALSE)
      }
      if (
        !is.logical(prefer_evidence) ||
          length(prefer_evidence) != 1L ||
          is.na(prefer_evidence)
      ) {
        stop("prefer_evidence must be TRUE or FALSE.", call. = FALSE)
      }
      rv$run <- run
      rv$prefer_run_evidence <- prefer_evidence
      rv$run_version <- shiny::isolate(rv$run_version) + 1L
      invisible(run)
    },

    touch_run = function() {
      rv$run_version <- shiny::isolate(rv$run_version) + 1L
      invisible()
    },

    # Signal that the current session was mutated in place.
    touch = function() {
      bump_version()
      invisible()
    },

    save = function(path, overwrite = TRUE, status = "saved") {
      session <- shiny::isolate(rv$session)
      if (is.null(session)) {
        stop("No Co-STORM session is active.", call. = FALSE)
      }
      saved <- tempest::tempest_session_save(
        session,
        path = path,
        overwrite = overwrite
      )
      set_persistence(
        status,
        path = saved,
        message = paste0(
          if (identical(status, "autosaved")) "Autosaved" else "Saved",
          " session bundle."
        )
      )
      saved
    },

    restore = function(
      path,
      config,
      runtime = tempest::tempest_runtime(),
      connection_permissions = list(),
      progress = NULL
    ) {
      session <- tempest::tempest_session_resume(
        path,
        config = config,
        runtime = runtime,
        connection_permissions = connection_permissions,
        progress = progress
      )
      rv$session <- session
      workflow_run <- tryCatch(
        session$workflow_run,
        error = function(error) NULL
      )
      if (inherits(workflow_run, "TempestRun")) {
        rv$run <- workflow_run
        rv$prefer_run_evidence <- FALSE
        rv$run_version <- shiny::isolate(rv$run_version) + 1L
      }
      set_report_from_session(session)
      set_persistence(
        "restored",
        path = normalizePath(
          path.expand(path),
          winslash = "/",
          mustWork = FALSE
        ),
        message = "Loaded session bundle."
      )
      bump_version(autosave = FALSE)
      session
    },

    persistence = shiny::reactive({
      list(
        status = rv$persistence_status,
        message = rv$persistence_message,
        path = rv$persistence_path
      )
    }),
    set_persistence = set_persistence,

    # Generated report, shared across the Chat and STORM tabs.
    report = shiny::reactive(rv$report_md),
    report_store = shiny::reactive(rv$report_source_store),
    report_topic = shiny::reactive({
      rv$report_topic %||%
        {
          ses <- rv$session
          if (is.null(ses)) NULL else ses$topic
        }
    }),
    set_report = function(md, topic = NULL, source_store = NULL) {
      rv$report_md <- md
      if (is.null(md) && is.null(topic)) {
        rv$report_topic <- NULL
      } else if (!is.null(topic)) {
        rv$report_topic <- topic
      }
      rv$report_source_store <- source_store
      invisible()
    }
  )
}
