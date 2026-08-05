# Embeddable Shiny adapter

tempest_shiny_module_env <- local({
  env <- NULL

  function() {
    if (!is.null(env)) {
      return(env)
    }

    module_dir <- tempest_pkg_file("shiny", "R")
    if (identical(module_dir, "") || !dir.exists(module_dir)) {
      tempest_abort("The bundled Shiny module files were not found.")
    }

    env <<- new.env(parent = asNamespace("tempest"))
    files <- sort(list.files(
      module_dir,
      pattern = "[.][Rr]$",
      full.names = TRUE
    ))
    for (file in files) {
      sys.source(file, envir = env)
    }
    env
  }
})

tempest_shiny_panel_choices <- function() {
  c("chat", "sources", "facts", "mindmap", "transcript", "report", "storm")
}

tempest_shiny_panels <- function(panels) {
  choices <- tempest_shiny_panel_choices()
  if (identical(panels, "all")) {
    return(choices)
  }
  if (!is.character(panels) || length(panels) == 0L) {
    tempest_abort("{.arg panels} must be a non-empty character vector.")
  }
  unknown <- setdiff(panels, choices)
  if (length(unknown) > 0L) {
    tempest_abort(c(
      "{.arg panels} contains unknown panel name{?s}.",
      x = "Unknown: {.val {unknown}}.",
      i = "Use one or more of {.val {choices}}."
    ))
  }
  unique(panels)
}

tempest_shiny_require_ui <- function(panels) {
  tempest_require("shiny", "tempest_shiny_ui() builds Shiny UI.")
  tempest_require("bslib", "tempest_shiny_ui() builds a bslib tabset.")
  if ("chat" %in% panels) {
    tempest_shinychat_require()
    tempest_require("zip", "Tempest session downloads require zip.")
  }
  invisible(NULL)
}

tempest_shiny_require_server <- function(panels) {
  tempest_require("shiny", "tempest_shiny_server() runs Shiny modules.")
  if (any(c("chat", "storm") %in% panels)) {
    tempest_require("ellmer", "Tempest Shiny sessions require ellmer.")
    tempest_require(
      "promises",
      "Tempest Shiny background work requires promises."
    )
    tempest_require("later", "Tempest Shiny async callbacks require later.")
  }
  if ("chat" %in% panels) {
    tempest_shinychat_require()
  }
  if ("storm" %in% panels) {
    tempest_require("mirai", "the Tempest STORM panel requires mirai.")
  }
  invisible(NULL)
}

tempest_shiny_as_reactive <- function(value) {
  if (shiny::is.reactive(value)) {
    value
  } else if (is.function(value)) {
    shiny::reactive(value())
  } else {
    shiny::reactive(value)
  }
}

#' Create a shared Tempest Shiny store
#'
#' `r lifecycle::badge("experimental")`
#'
#' `tempest_shiny_store()` creates the small reactive store used by the
#' embeddable Tempest Shiny adapter. Host apps can pass the returned store to
#' [tempest_shiny_server()] when they want to share state across adapter
#' instances or inspect the current `TempestSession`.
#'
#' The returned object should be treated as an adapter handle; prefer its
#' public methods over relying on its internal representation.
#'
#' @return A Tempest Shiny store handle.
#' @export
tempest_shiny_store <- function() {
  tempest_require("shiny", "tempest_shiny_store() creates Shiny reactives.")
  tempest_shiny_module_env()$new_session_store()
}

#' Embed Tempest panels in a Shiny UI
#'
#' `r lifecycle::badge("experimental")`
#'
#' `tempest_shiny_ui()` is a compact Shiny module UI for host applications that
#' want to embed Tempest without sourcing files from `inst/shiny/R`. It reuses
#' the bundled app's panels while letting the host provide the page shell,
#' configuration, storage policy, and surrounding controls.
#'
#' @param id Shiny module id.
#' @param panels Character vector of panels to include. Use `"all"` for every
#'   panel. The default embeds the Co-STORM chat and durable research views.
#' @param show_config If `TRUE`, include the bundled configuration controls in
#'   the Chat settings drawer. Hosts that provide their own config should leave
#'   this as `FALSE`.
#' @return A Shiny tag object.
#' @examples
#' \dontrun{
#' ui <- bslib::page_fillable(tempest_shiny_ui("research"))
#' }
#' @export
tempest_shiny_ui <- function(
  id,
  panels = c("chat", "sources", "facts", "mindmap", "transcript", "report"),
  show_config = FALSE
) {
  panels <- tempest_shiny_panels(panels)
  if (isTRUE(show_config) && !"chat" %in% panels) {
    tempest_abort("{.arg show_config} requires the {.val chat} panel.")
  }
  tempest_shiny_require_ui(panels)

  env <- tempest_shiny_module_env()
  ns <- shiny::NS(id)
  config_ui <- if (isTRUE(show_config)) {
    env$mod_config_ui(ns("config"))
  } else {
    NULL
  }

  panel_tags <- lapply(panels, function(panel) {
    switch(
      panel,
      chat = env$mod_chat_ui(ns("chat"), config_ui = config_ui),
      sources = env$mod_sources_ui(ns("sources")),
      facts = env$mod_facts_ui(ns("facts")),
      mindmap = env$mod_mindmap_ui(ns("mindmap")),
      transcript = env$mod_transcript_ui(ns("transcript")),
      report = env$mod_report_ui(ns("report")),
      storm = env$mod_storm_ui(ns("storm"))
    )
  })

  do.call(bslib::navset_tab, c(panel_tags, list(id = ns("tabs"))))
}

#' Run embedded Tempest Shiny panels
#'
#' `r lifecycle::badge("experimental")`
#'
#' `tempest_shiny_server()` pairs with [tempest_shiny_ui()] and lets a host app
#' provide a [TempestConfig], process-local [tempest_runtime()], optional expert
#' profiles, a stable session id, and an optional shared store. The returned
#' handle exposes the shared store and reactive accessors for generic run,
#' event, approval, capability-grant, artifact, session, and report state.
#'
#' @param id Shiny module id.
#' @param config A `TempestConfig`, a reactive returning one, a function
#'   returning one, or `NULL` to use the bundled config module defaults.
#' @param store Optional store from [tempest_shiny_store()]. If `NULL`, a new
#'   store is created.
#' @param panels Character vector matching the UI panels.
#' @param experts Optional expert profiles passed to [tempest_session()] for
#'   new Co-STORM sessions. May be a value, function, or reactive.
#' @param runtime A [tempest_runtime()], function, or reactive used to resolve
#'   process-local operations, capabilities, and connections.
#' @param connection_permissions Named per-role or per-expert connection
#'   allow-lists passed to [tempest_session()].
#' @param session_id Optional stable session id passed to [tempest_session()]
#'   for new Co-STORM sessions. May be a value, function, or reactive.
#' @param run Optional `TempestRun`, function, or reactive. This lets a host
#'   expose a custom headless workflow through the same generic adapter
#'   reactives as built-in workflows.
#' @return A list with the shared `store`; reactive `run`, `status`, `events`,
#'   `approvals`, `assignments`, `artifacts`, `evidence`, `grants`, `session`,
#'   `report`, and `report_ready` accessors; and `approve()`, `cancel()`, and
#'   `touch()` controls.
#' @examples
#' \dontrun{
#' server <- function(input, output, session) {
#'   tempest_shiny_server("research", config = tempest_config())
#' }
#' }
#' @export
tempest_shiny_server <- function(
  id,
  config = tempest_config(),
  store = NULL,
  panels = c("chat", "sources", "facts", "mindmap", "transcript", "report"),
  experts = NULL,
  runtime = tempest_runtime(),
  connection_permissions = list(),
  session_id = NULL,
  run = NULL
) {
  panels <- tempest_shiny_panels(panels)
  tempest_shiny_require_server(panels)

  shiny::moduleServer(id, function(input, output, session) {
    env <- tempest_shiny_module_env()
    shared_store <- store %||% env$new_session_store()
    config_reactive <- if (is.null(config)) {
      env$mod_config_server("config")
    } else {
      tempest_shiny_as_reactive(config)
    }

    ready_signals <- list()
    if ("chat" %in% panels) {
      ready_signals$chat <- env$mod_chat_server(
        "chat",
        config = config_reactive,
        store = shared_store,
        experts = experts,
        runtime = runtime,
        connection_permissions = connection_permissions,
        session_id = session_id
      )
    }
    if ("storm" %in% panels) {
      ready_signals$storm <- env$mod_storm_server(
        "storm",
        config = config_reactive,
        store = shared_store
      )
    }
    if ("mindmap" %in% panels) {
      env$mod_mindmap_server("mindmap", store = shared_store)
    }
    if ("sources" %in% panels) {
      env$mod_sources_server("sources", store = shared_store)
    }
    if ("facts" %in% panels) {
      env$mod_facts_server("facts", store = shared_store)
    }
    if ("transcript" %in% panels) {
      env$mod_transcript_server("transcript", store = shared_store)
    }
    if ("report" %in% panels) {
      env$mod_report_server("report", store = shared_store)
    }

    report_ready <- shiny::reactive({
      sum(vapply(
        ready_signals,
        function(signal) {
          as.integer(signal() %||% 0L)
        },
        integer(1)
      ))
    })

    run_version <- shiny::reactiveVal(0L)
    supplied_run <- if (is.null(run)) {
      shared_store$get_run
    } else {
      tempest_shiny_as_reactive(run)
    }
    if (!is.null(run)) {
      shiny::observe({
        candidate <- supplied_run()
        if (!is.null(candidate) && !inherits(candidate, "TempestRun")) {
          tempest_abort(
            "{.arg run} must resolve to a TempestRun or `NULL`.",
            class = c("tempest_shiny_adapter_error", "tempest_error")
          )
        }
        shared_store$set_run(candidate, prefer_evidence = TRUE)
      })
    }
    run_reactive <- shiny::reactive({
      run_version()
      supplied_run()
    })
    current_run <- function() {
      candidate <- run_reactive()
      if (is.null(candidate)) {
        return(NULL)
      }
      if (!inherits(candidate, "TempestRun")) {
        tempest_abort(
          "{.arg run} must resolve to a TempestRun or `NULL`.",
          class = c("tempest_shiny_adapter_error", "tempest_error")
        )
      }
      candidate
    }
    run_status <- shiny::reactive({
      active <- current_run()
      if (is.null(active)) NULL else tempest_run_status(active)
    })
    run_events <- shiny::reactive({
      active <- current_run()
      if (is.null(active)) list() else tempest_execution_events(active)
    })
    run_approvals <- shiny::reactive({
      active <- current_run()
      if (is.null(active)) {
        return(list())
      }
      tempest_run_approvals(active, status = "pending")
    })
    run_assignments <- shiny::reactive({
      active <- current_run()
      if (is.null(active)) list() else active$assignments
    })
    run_artifacts <- shiny::reactive({
      active <- current_run()
      if (is.null(active)) {
        return(list())
      }
      tempest_run_artifacts(active, include_content = FALSE)
    })
    run_grants <- shiny::reactive({
      active <- current_run()
      if (is.null(active)) {
        return(list())
      }
      tempest_run_capability_grants(active)
    })
    run_evidence <- shiny::reactive({
      active <- current_run()
      if (is.null(active) || is.null(active$source_store)) {
        return(list(resources = list(), claims = list(), disputes = list()))
      }
      list(
        resources = lapply(
          active$source_store$list_resources(),
          tempest_resource_record,
          include_content = FALSE
        ),
        claims = lapply(
          active$source_store$list_claims(),
          tempest_claim_to_list
        ),
        disputes = lapply(
          active$source_store$list_disputes(),
          tempest_dispute_to_list
        )
      )
    })
    touch_run <- function() {
      run_version(shiny::isolate(run_version()) + 1L)
      shared_store$touch_run()
      invisible(NULL)
    }
    approve <- function(
      approval_id,
      decision = c("approved", "rejected"),
      note = NULL,
      metadata = list(),
      resume = TRUE
    ) {
      active <- shiny::isolate(current_run())
      if (is.null(active)) {
        tempest_abort(
          "No TempestRun is available for approval.",
          class = c("tempest_shiny_adapter_error", "tempest_error")
        )
      }
      decision <- match.arg(decision)
      tempest_run_record_approval(
        active,
        approval_id = approval_id,
        decision = decision,
        note = note,
        metadata = metadata,
        resume = resume
      )
      touch_run()
      invisible(active)
    }
    cancel <- function(reason = "Cancelled by host.") {
      active <- shiny::isolate(current_run())
      if (is.null(active)) {
        tempest_abort(
          "No TempestRun is available for cancellation.",
          class = c("tempest_shiny_adapter_error", "tempest_error")
        )
      }
      tempest_run_request_cancel(active, reason)
      touch_run()
      invisible(active)
    }

    list(
      store = shared_store,
      run = run_reactive,
      status = run_status,
      events = run_events,
      approvals = run_approvals,
      assignments = run_assignments,
      artifacts = run_artifacts,
      evidence = run_evidence,
      grants = run_grants,
      approve = approve,
      cancel = cancel,
      touch = touch_run,
      session = shared_store$get,
      report = shared_store$report,
      report_ready = report_ready
    )
  })
}
