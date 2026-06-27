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
    report_topic = NULL
  )

  list(
    # Version-aware read of the current session. Re-fires on set()/touch().
    get = shiny::reactive({
      rv$version
      rv$session
    }),

    # Replace the session (e.g. on session start).
    set = function(session) {
      rv$session <- session
      rv$version <- rv$version + 1L
      invisible(session)
    },

    # Signal that the current session was mutated in place.
    touch = function() {
      rv$version <- rv$version + 1L
      invisible()
    },

    # Generated report, shared across the Chat and STORM tabs.
    report = shiny::reactive(rv$report_md),
    report_topic = shiny::reactive({
      rv$report_topic %||%
        {
          ses <- rv$session
          if (is.null(ses)) NULL else ses$topic
        }
    }),
    set_report = function(md, topic = NULL) {
      rv$report_md <- md
      if (!is.null(topic)) {
        rv$report_topic <- topic
      }
      invisible()
    }
  )
}
