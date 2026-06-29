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
    version = 0L,
    report_md = NULL,
    report_topic = NULL,
    report_source_store = NULL,
    persistence_status = "idle",
    persistence_message = NULL,
    persistence_path = NULL
  )

  bump_version <- function() {
    version <- shiny::isolate(rv$version)
    if (is.null(version) || is.na(version)) {
      version <- 0L
    }
    version <- version + 1L
    rv$version <- version
    invisible(version)
  }

  set_persistence <- function(status, path = NULL, message = NULL) {
    rv$persistence_status <- status
    rv$persistence_path <- path
    rv$persistence_message <- message
    invisible(NULL)
  }

  set_report_from_session <- function(session) {
    report_md <- session$artifacts[["report_md"]] %||% NULL
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

    # Version counter for debounced autosave observers.
    version = shiny::reactive(rv$version),

    # Version-aware read of the current session. Re-fires on set()/touch().
    get = shiny::reactive({
      rv$version
      rv$session
    }),

    # Replace the session (e.g. on session start).
    set = function(session) {
      rv$session <- session
      bump_version()
      invisible(session)
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

    restore = function(path, config, progress = NULL) {
      session <- tempest::tempest_session_resume(
        path,
        config = config,
        progress = progress
      )
      rv$session <- session
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
      bump_version()
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
