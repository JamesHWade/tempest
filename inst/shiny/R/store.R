# Shared reactive state for the Tempest research products.

new_session_store <- function() {
  rv <- shiny::reactiveValues(
    costorm_session = NULL,
    costorm_version = 0L,
    report_md = NULL,
    report_topic = NULL,
    report_workspace = NULL,
    costorm_persistence_status = "idle",
    costorm_persistence_message = NULL,
    costorm_persistence_path = NULL
  )

  bump_costorm_version <- function() {
    version <- shiny::isolate(rv$costorm_version)
    if (is.null(version) || is.na(version)) {
      version <- 0L
    }
    version <- version + 1L
    rv$costorm_version <- version
    invisible(version)
  }

  set_costorm_persistence_status <- function(
    status,
    path = NULL,
    message = NULL
  ) {
    rv$costorm_persistence_status <- status
    rv$costorm_persistence_path <- path
    rv$costorm_persistence_message <- message
    invisible(NULL)
  }

  clear_report <- function() {
    rv$report_md <- NULL
    rv$report_topic <- NULL
    rv$report_workspace <- NULL
    invisible(NULL)
  }

  costorm_report_candidate <- function(session) {
    if (
      !inherits(session, "TempestSession") ||
        !identical(session$manifest@status, "succeeded")
    ) {
      stop("session must be a succeeded TempestSession.", call. = FALSE)
    }
    list(
      report_md = tempest::tempest_session_report_md(session),
      report_topic = session$topic,
      report_workspace = session$workspace
    )
  }

  commit_report <- function(candidate) {
    rv$report_md <- candidate$report_md
    rv$report_topic <- candidate$report_topic
    rv$report_workspace <- candidate$report_workspace
    invisible(candidate$report_md)
  }

  set_costorm_session <- function(session) {
    if (!is.null(session) && !inherits(session, "TempestSession")) {
      stop("session must be NULL or a TempestSession.", call. = FALSE)
    }
    candidate <- if (
      !is.null(session) && identical(session$manifest@status, "succeeded")
    ) {
      costorm_report_candidate(session)
    } else {
      NULL
    }
    rv$costorm_session <- session
    if (is.null(candidate)) {
      clear_report()
    } else {
      commit_report(candidate)
    }
    bump_costorm_version()
    invisible(session)
  }

  save_costorm_session <- function(path, overwrite = TRUE) {
    session <- shiny::isolate(rv$costorm_session)
    if (is.null(session)) {
      stop("No Co-STORM session is active.", call. = FALSE)
    }
    tryCatch(
      {
        saved <- tempest::tempest_session_save(
          session,
          path = path,
          overwrite = overwrite
        )
        set_costorm_persistence_status(
          "saved",
          path = saved,
          message = "Saved session bundle."
        )
        saved
      },
      error = function(error) {
        set_costorm_persistence_status(
          "error",
          message = "Could not save the session bundle."
        )
        stop(error)
      }
    )
  }

  resume_costorm_session <- function(
    path,
    config,
    progress = NULL,
    program_set = NULL,
    knowledge_view = NULL
  ) {
    tryCatch(
      {
        session <- tempest::tempest_session_resume(
          path,
          config = config,
          progress = progress,
          program_set = program_set,
          knowledge_view = knowledge_view
        )
        set_costorm_session(session)
        set_costorm_persistence_status(
          "restored",
          path = normalizePath(
            path.expand(path),
            winslash = "/",
            mustWork = FALSE
          ),
          message = "Loaded session bundle."
        )
        session
      },
      error = function(error) {
        set_costorm_persistence_status(
          "error",
          message = "Could not load the session bundle."
        )
        stop(error)
      }
    )
  }

  publish_costorm_report <- function(session) {
    if (!identical(session, shiny::isolate(rv$costorm_session))) {
      stop("session must be the active Co-STORM session.", call. = FALSE)
    }
    commit_report(costorm_report_candidate(session))
  }

  publish_storm_report <- function(result, config) {
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
    reference <- result$manifest@deliverables$report_md[
      c("report_id", "sha256")
    ]
    tempest:::tempest_product_report_reference_validate(
      reference,
      result$report_md
    )
    tempest:::tempest_product_authority_validate(
      manifest = result$manifest,
      stage_records = result$state$stage_records,
      workspace = result$workspace,
      report_md = result$report_md,
      report_reference = reference,
      config = config,
      experts = result$experts %||% list(),
      product_state = result$state,
      require_publishable = TRUE
    )
    commit_report(list(
      report_md = result$report_md,
      report_topic = result$title,
      report_workspace = result$workspace
    ))
  }

  list(
    peek_costorm_session = function() {
      shiny::isolate(rv$costorm_session)
    },
    costorm_session = shiny::reactive({
      rv$costorm_version
      rv$costorm_session
    }),
    costorm_workspace = shiny::reactive({
      rv$costorm_version
      session <- rv$costorm_session
      if (is.null(session)) NULL else session$workspace
    }),
    set_costorm_session = set_costorm_session,
    touch_costorm_session = function() {
      bump_costorm_version()
      invisible(NULL)
    },
    save_costorm_session = save_costorm_session,
    resume_costorm_session = resume_costorm_session,
    costorm_persistence_status = shiny::reactive({
      list(
        status = rv$costorm_persistence_status,
        message = rv$costorm_persistence_message,
        path = rv$costorm_persistence_path
      )
    }),
    report_md = shiny::reactive(rv$report_md),
    report_workspace = shiny::reactive(rv$report_workspace),
    report_topic = shiny::reactive(rv$report_topic),
    publish_costorm_report = publish_costorm_report,
    publish_storm_report = publish_storm_report
  )
}
