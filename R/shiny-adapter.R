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
    tempest_require("shinychat", "the Tempest chat panel requires shinychat.")
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
    tempest_require("shinychat", "the Tempest chat panel requires shinychat.")
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
#'   the Chat sidebar. Hosts that provide their own config should leave this as
#'   `FALSE`.
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
#' provide a runtime [TempestConfig], optional expert personas, a stable session
#' id, and an optional shared store. The returned handle exposes the shared
#' store and reactive accessors for the current session, report, and report
#' readiness signal.
#'
#' @param id Shiny module id.
#' @param config A `TempestConfig`, a reactive returning one, a function
#'   returning one, or `NULL` to use the bundled config module defaults.
#' @param store Optional store from [tempest_shiny_store()]. If `NULL`, a new
#'   store is created.
#' @param panels Character vector matching the UI panels.
#' @param personas Optional expert definitions passed to [tempest_session()] for
#'   new Co-STORM sessions. May be a value, function, or reactive.
#' @param session_id Optional stable session id passed to [tempest_session()]
#'   for new Co-STORM sessions. May be a value, function, or reactive.
#' @return A list with `store`, `session`, `report`, and `report_ready`.
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
  personas = NULL,
  session_id = NULL
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
        personas = personas,
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

    list(
      store = shared_store,
      session = shared_store$get,
      report = shared_store$report,
      report_ready = report_ready
    )
  })
}
