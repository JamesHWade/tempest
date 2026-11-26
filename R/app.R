# Shiny app launcher

#' Run the Tempest research application
#'
#' Launches an interactive app that provides:
#' - Co-STORM chat, sources, facts, mind map, transcript, and committed reports;
#' - asynchronous scripted STORM research and report publication;
#' - a bounded, read-only Run review with separately labeled live progress; and
#' - bounded Co-STORM session archive download and upload without autosave.
#'
#' Live progress, persistence, and successful publication are announced through
#' polite status regions. Validation, cancellation, and publication failures
#' are announced as alerts.
#'
#' @param demo If `TRUE`, open the deterministic ecosystem demonstration.
#'   Scripted model responses and synthetic local evidence exercise real
#'   deputy and dsprrr execution, a shinymcp research card, and explicit Graft
#'   review, correction, withdrawal, and fresh-process reuse. No credentials
#'   or network are needed. The example uses a local demo directory; set
#'   `options(tempest.demo_dir = "path")` to keep it across R sessions.
#' @param graft_store Optional local directory path or `graft::ArtifactStore`
#'   (including `graft::graft_store_postgres()`) for reviewed project memory.
#'   Defaults to `NULL` (disabled). When supplied, Workspace settings can pin
#'   an accepted stream for new sessions or attach read-only `graft_tools()`
#'   recall (with `shinychat` tool cards), and the Report tab adds explicit
#'   review, acceptance, correction, and withdrawal controls.
#' @param memory_purpose Consultation purpose recorded on Graft decisions and
#'   memory recall tools. Defaults to `"research"`.
#' @param memory_actor Default reviewer label shown in the Graft review panel.
#'   Defaults to `"local-reviewer"`.
#' @param memory_eligible Optional one-argument host eligibility function
#'   `function(stream)` returning `TRUE` or `FALSE`. Defaults to allowing all
#'   streams in the store. Multi-user hosts must supply an authenticated
#'   policy function.
#' @param ... Passed to `shiny::runApp()`.
#' @return A Shiny app object (invisibly, from `shiny::runApp()`).
#' @examples
#' \dontrun{
#' tempest_app()
#' tempest_app(graft_store = "research-memory")
#' }
#' @export
tempest_app <- function(
  ...,
  demo = FALSE,
  graft_store = NULL,
  memory_purpose = "research",
  memory_actor = NULL,
  memory_eligible = NULL
) {
  rlang::check_bool(demo)
  memory_ctx <- NULL
  if (demo) {
    for (package in c(
      "shiny",
      "bslib",
      "shinymcp",
      "graft",
      "callr",
      "promises",
      "later"
    )) {
      tempest_require(
        package,
        "The offline ecosystem demonstration needs this package."
      )
    }
    app_dir <- system.file("examples", "ellmerverse", package = "tempest")
  } else {
    tempest_shiny_require_ui(tempest_shiny_panel_choices())
    tempest_shiny_require_server(tempest_shiny_panel_choices())
    if (!is.null(graft_store)) {
      memory_purpose <- tempest_graft_memory_check_purpose(memory_purpose)
      memory_actor <- tempest_graft_memory_check_actor(memory_actor)
      memory_eligible_fn <- tempest_graft_memory_eligible(memory_eligible)
      store_handle <- tempest_graft_store_open(graft_store, create = TRUE)
      memory_ctx <- list(
        store = store_handle,
        store_path = tempest_graft_store_path(store_handle),
        purpose = memory_purpose,
        actor = memory_actor,
        eligible = memory_eligible_fn
      )
    }
    app_dir <- system.file("shiny", package = "tempest")
  }
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
  old_options <- options(
    ellmer_timeout_s = provider_timeout_s,
    tempest.shiny.memory = memory_ctx
  )
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
