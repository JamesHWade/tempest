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
    if (is.null(session)) {
      rv$report_md <- NULL
      rv$report_topic <- NULL
      rv$report_source_store <- NULL
      return(invisible(NULL))
    }
    if (!inherits(session, "TempestSession")) {
      stop("session must be a TempestSession.", call. = FALSE)
    }
    report_md <- tryCatch(
      tempest:::tempest_session_report_value(session),
      error = function(error) NULL
    )
    if (
      !is.character(report_md) ||
        length(report_md) != 1L ||
        is.na(report_md) ||
        !nzchar(report_md)
    ) {
      report_md <- NULL
    }
    rv$report_md <- report_md
    rv$report_topic <- if (is.null(report_md)) NULL else session$topic
    rv$report_source_store <- if (is.null(report_md)) {
      NULL
    } else {
      session$workspace
    }
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

    # Expose only the authoritative Co-STORM ResearchWorkspace.
    evidence_store = shiny::reactive({
      rv$version
      current_session <- rv$session
      if (is.null(current_session)) NULL else current_session$workspace
    }),

    # Replace the session (e.g. on session start).
    set = function(session) {
      if (!is.null(session) && !inherits(session, "TempestSession")) {
        stop("session must be NULL or a TempestSession.", call. = FALSE)
      }
      rv$session <- session
      set_report_from_session(session)
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

    restore = function(
      path,
      config,
      progress = NULL,
      program_set = NULL,
      knowledge_view = NULL
    ) {
      session <- tempest::tempest_session_resume(
        path,
        config = config,
        progress = progress,
        program_set = program_set,
        knowledge_view = knowledge_view
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

    # Published product report, shared across the Chat and STORM tabs.
    report = shiny::reactive(rv$report_md),
    report_store = shiny::reactive(rv$report_source_store),
    report_topic = shiny::reactive({
      rv$report_topic %||%
        {
          ses <- rv$session
          if (is.null(ses)) NULL else ses$topic
        }
    }),
    set_session_report = function(session) {
      if (
        !inherits(session, "TempestSession") ||
          !identical(session$manifest@status, "succeeded")
      ) {
        stop("session must be a succeeded TempestSession.", call. = FALSE)
      }
      set_report_from_session(session)
    },
    set_storm_result = function(result, config) {
      valid <- is.list(result) &&
        inherits(result$workspace %||% NULL, "ResearchWorkspace") &&
        S7::S7_inherits(
          result$manifest %||% NULL,
          tempest:::TempestResearchManifest
        ) &&
        identical(result$manifest@mode, "storm") &&
        identical(result$manifest@status, "succeeded") &&
        is.list(result$state) &&
        is.list(result$state$stage_records) &&
        is.character(result$report_md) &&
        length(result$report_md) == 1L &&
        !is.na(result$report_md)
      if (!isTRUE(valid)) {
        stop("result must be a succeeded STORM product.", call. = FALSE)
      }
      tempest:::tempest_product_report_reference_validate(
        result$manifest@deliverables$report_md[c("report_id", "sha256")],
        result$report_md
      )
      tempest:::tempest_product_authority_validate(
        manifest = result$manifest,
        stage_records = result$state$stage_records,
        workspace = result$workspace,
        report_md = result$report_md,
        report_reference = result$manifest@deliverables$report_md[
          c("report_id", "sha256")
        ],
        config = config,
        experts = result$experts %||% list(),
        product_state = result$state,
        require_publishable = TRUE
      )
      rv$report_md <- result$report_md
      rv$report_topic <- result$title
      rv$report_source_store <- result$workspace
      invisible(result$report_md)
    }
  )
}
