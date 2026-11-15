tempest_storm_capture_sources <- function(retriever, workspace, source_ids) {
  for (source_id in unique(source_ids)) {
    resource <- workspace$get_retrieved_resource(source_id)
    if (is.null(resource)) {
      tempest_stage_governance_abort(
        "Cannot capture an unknown research source."
      )
    }
    if (
      !identical(resource@resource_kind, "web") || !is.null(resource@content)
    ) {
      next
    }
    captured <- tryCatch(
      {
        retriever$fetch(resource@locator)
        TRUE
      },
      error = function(error) {
        if (
          inherits(
            error,
            c(
              "tempest_stage_governance_error",
              "tempest_research_workspace_error"
            )
          )
        ) {
          stop(error)
        }
        cli::cli_warn(
          "Source {.val {source_id}} could not be captured for verification.",
          class = "tempest_source_capture_warning",
          parent = error
        )
        FALSE
      }
    )
    if (
      captured && is.null(workspace$get_retrieved_resource(source_id)@content)
    ) {
      cli::cli_warn(
        "Source {.val {source_id}} returned no captured text for verification.",
        class = "tempest_source_capture_warning"
      )
    }
  }
  invisible(NULL)
}
