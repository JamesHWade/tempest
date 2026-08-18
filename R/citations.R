# Product evidence-table accessors

#' Return evidence resources as a tibble
#' @param workspace A [ResearchWorkspace] or [TempestRetriever].
#' @return A tibble with resource identity, kind, opaque locator, optional web
#'   URL, title, media type, content context, retrieval time, and metadata.
#' @examples
#' \dontrun{
#' result <- tempest_run("History of jazz", config = tempest_config())
#' tempest_sources(result$workspace)
#' }
#' @export
tempest_sources <- function(workspace) {
  if (inherits(workspace, "TempestRetriever")) {
    workspace <- workspace$workspace
  }
  if (!inherits(workspace, "ResearchWorkspace")) {
    tempest_research_workspace_abort(
      "{.arg workspace} must be a ResearchWorkspace or TempestRetriever."
    )
  }
  workspace$to_tibbles()$retrieved_resources
}

#' Return claims as a tibble
#' @param workspace A [ResearchWorkspace] or [TempestRetriever].
#' @return A tibble of claims with columns: claim_id, claim_text, claim_type,
#'   source_ids, confidence, verification_status, support_score, created_at.
#' @examples
#' \dontrun{
#' result <- tempest_run("History of jazz", config = tempest_config())
#' tempest_claims(result$workspace)
#' }
#' @export
tempest_claims <- function(workspace) {
  if (inherits(workspace, "TempestRetriever")) {
    workspace <- workspace$workspace
  }
  if (!inherits(workspace, "ResearchWorkspace")) {
    tempest_research_workspace_abort(
      "{.arg workspace} must be a ResearchWorkspace or TempestRetriever."
    )
  }
  workspace$to_tibbles()$proposed_claims
}
