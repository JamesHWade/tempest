# Shiny app launcher

#' Run the tempest Shiny chat application
#'
#' Launches an interactive app that provides:
#' - Co-STORM multi-agent chat with a live mind map and source list
#' - One-click report generation with citations
#'
#' @param ... Passed to `shiny::runApp()`.
#' @export
run_app <- function(...) {
  tempest_require("shiny", "run_app() launches a Shiny app.")
  app_dir <- system.file("shiny", package = "tempest")
  if (identical(app_dir, "")) tempest_abort("Shiny app not found in installed package.")
  shiny::runApp(app_dir, ...)
}
