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
    report_source_store = NULL
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

  list(
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
      if (!is.null(topic)) {
        rv$report_topic <- topic
      }
      rv$report_source_store <- source_store
      invisible()
    }
  )
}
