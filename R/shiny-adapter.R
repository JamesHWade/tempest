# Bundled Shiny module loader and runtime dependency checks

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
  c(
    "chat",
    "sources",
    "facts",
    "mindmap",
    "transcript",
    "report",
    "storm",
    "review"
  )
}

tempest_shiny_require_ui <- function(panels) {
  tempest_require("shiny", "tempest_app() builds Shiny UI.")
  tempest_require("bslib", "tempest_app() builds a bslib UI.")
  if ("chat" %in% panels) {
    tempest_shinychat_require()
    tempest_require("zip", "Tempest session downloads require zip.")
  }
  invisible(NULL)
}

tempest_shiny_require_server <- function(panels) {
  tempest_require("shiny", "tempest_app() runs Shiny modules.")
  if (any(c("chat", "storm") %in% panels)) {
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
