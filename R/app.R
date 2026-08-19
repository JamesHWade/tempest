# Shiny app launcher

#' Run the Tempest research application
#'
#' Launches an interactive app that provides:
#' - Co-STORM chat, sources, facts, mind map, transcript, and committed reports;
#' - asynchronous scripted STORM research and report publication; and
#' - bounded Co-STORM session archive download and upload without autosave.
#'
#' Live progress, persistence, and successful publication are announced through
#' polite status regions. Validation, cancellation, and publication failures
#' are announced as alerts.
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
  provider_timeout_s <- tempest_shiny_provider_timeout()
  old_options <- options(ellmer_timeout_s = provider_timeout_s)
  on.exit(options(old_options), add = TRUE)
  runner(app_dir, ...)
}

tempest_shiny_provider_timeout <- function() {
  timeout_s <- getOption("tempest.shiny.provider_timeout_s", 120)
  if (
    !is.numeric(timeout_s) ||
      length(timeout_s) != 1L ||
      is.na(timeout_s) ||
      !is.finite(timeout_s) ||
      timeout_s <= 0
  ) {
    tempest_abort(
      "{.option tempest.shiny.provider_timeout_s} must be a finite positive number.",
      class = c("tempest_shiny_error", "tempest_error")
    )
  }
  current <- getOption("ellmer_timeout_s")
  if (
    is.numeric(current) &&
      length(current) == 1L &&
      !is.na(current) &&
      is.finite(current) &&
      current > 0
  ) {
    timeout_s <- min(timeout_s, current)
  }
  timeout_s
}
