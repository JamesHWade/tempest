# Shiny app launcher

#' Run the tempest Shiny chat application
#'
#' Launches an interactive app that provides:
#' - Co-STORM multi-agent chat with a live mind map and source list
#' - One-click report generation with citations
#'
#' @param ... Passed to `shiny::runApp()`.
#' @return A Shiny app object (invisibly, from `shiny::runApp()`).
#' @examples
#' \dontrun{
#' run_app()
#' }
#' @export
run_app <- function(...) {
  tempest_shiny_require_ui(tempest_shiny_panel_choices())
  tempest_shiny_require_server(tempest_shiny_panel_choices())
  app_dir <- system.file("shiny", package = "tempest")
  if (identical(app_dir, "")) {
    tempest_abort("Shiny app not found in installed package.")
  }
  runner <- getOption("tempest.app_runner", shiny::runApp)
  if (!is.function(runner)) {
    tempest_abort(
      "The configured Shiny app runner must be a function.",
      class = c("tempest_shiny_error", "tempest_error")
    )
  }
  runner(app_dir, ...)
}
