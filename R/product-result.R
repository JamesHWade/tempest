# Cohesive Tempest product result

tempest_product_result_abort <- function(message, ..., parent = NULL) {
  tempest_abort(
    message,
    ...,
    class = c("tempest_product_result_error", "tempest_error"),
    parent = parent,
    .envir = rlang::caller_env()
  )
}

#' Completed Tempest research product
#'
#' The validated value returned by [tempest_run()]. Read it with
#' [tempest_report()], [tempest_sources()], [tempest_claims()],
#' [tempest_claim_supports()], [tempest_trajectory_review()], and the
#' promotion functions. Retrievers, mutable workspaces, manifests, and stage
#' state stay internal to validation, persistence, telemetry, and promotion.
#'
#' @keywords internal
TempestResult <- S7::new_class(
  "TempestResult",
  package = "tempest",
  properties = list(
    title = tempest_product_prop_chr(),
    topic = tempest_product_prop_chr(),
    run_id = tempest_product_prop_chr(),
    status = tempest_product_prop_chr(),
    report_md = S7::new_property(S7::class_any),
    output_dir = S7::new_property(S7::class_any),
    perspectives = S7::new_property(S7::class_list),
    experts = S7::new_property(S7::class_list),
    outline = S7::new_property(S7::class_any),
    draft_md = S7::new_property(S7::class_any),
    manifest = S7::new_property(S7::class_any),
    state = S7::new_property(S7::class_list),
    workspace = S7::new_property(S7::class_any),
    retriever = S7::new_property(S7::class_any)
  ),
  validator = function(self) {
    if (!self@status %in% c("succeeded", "failed", "cancelled", "running")) {
      return("@status must be one exact terminal or running product status.")
    }
    if (!inherits(self@workspace, "ResearchWorkspace")) {
      return("@workspace must be a ResearchWorkspace.")
    }
    if (!S7::S7_inherits(self@manifest, TempestResearchManifest)) {
      return("@manifest must be a TempestResearchManifest.")
    }
    if (!identical(self@manifest@research_run_id, self@run_id)) {
      return("@run_id must equal the bound research manifest run id.")
    }
    if (!identical(self@manifest@status, self@status)) {
      return("@status must equal the bound research manifest status.")
    }
    report <- self@report_md
    if (
      !is.null(report) &&
        (!rlang::is_string(report) || is.na(report) || !nzchar(report))
    ) {
      return("@report_md must be NULL or one non-empty string.")
    }
    if (identical(self@status, "succeeded") && is.null(report)) {
      return("A succeeded product requires a committed Markdown report.")
    }
    if (!identical(self@status, "succeeded") && !is.null(report)) {
      return("A partial product must remain report-free.")
    }
    NULL
  }
)

tempest_is_product_result <- function(x) {
  identical(S7::S7_class(x), TempestResult)
}

# Build the cohesive result from the internal STORM product values.
tempest_product_result <- function(
  title,
  topic,
  perspectives,
  experts,
  outline,
  draft_md,
  report_md,
  manifest,
  state,
  workspace,
  retriever,
  output_dir
) {
  TempestResult(
    title = tempest_product_scalar(title, "title"),
    topic = tempest_product_scalar(topic, "topic"),
    run_id = manifest@research_run_id,
    status = manifest@status,
    report_md = report_md,
    output_dir = output_dir,
    perspectives = perspectives %||% list(),
    experts = tempest_validate_experts(experts %||% list()),
    outline = outline,
    draft_md = draft_md,
    manifest = manifest,
    state = state,
    workspace = workspace,
    retriever = retriever
  )
}

# Exact internal workspace for a supported product read.
tempest_product_read_workspace <- function(x, arg = "x") {
  if (tempest_is_product_result(x)) {
    return(x@workspace)
  }
  if (inherits(x, "TempestSession")) {
    return(x$workspace)
  }
  tempest_product_result_abort(paste0(
    "{.arg {arg}} must be a completed {.fn tempest_run} product or a ",
    "{.cls TempestSession}."
  ))
}
