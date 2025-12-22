# Shiny app launcher

#' Run the stormr Shiny chat application
#'
#' Launches an interactive app that provides:
#' - Co-STORM multi-agent chat with a live mind map and source list
#' - One-click report generation with citations
#'
#' @param ... Passed to `shiny::runApp()`.
#' @export
run_stormchat <- function(...) {
  stormr_require("shiny", "run_stormchat() launches a Shiny app.")
  app_dir <- system.file("shiny", package = "stormr")
  if (identical(app_dir, "")) stormr_abort("Shiny app not found in installed package.")
  shiny::runApp(app_dir, ...)
}
