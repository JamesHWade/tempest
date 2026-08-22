# Product evidence-table accessors

#' Return evidence resources as a tibble
#'
#' Reports every evidence resource consumed by a product, including accepted
#' organizational knowledge records supplied through [tempest_knowledge()].
#'
#' @param x A completed [tempest_run()] product or a `TempestSession`.
#' @return A tibble with resource identity, kind, opaque locator, optional web
#'   URL, title, media type, content context, retrieval time, and metadata.
#' @examples
#' \dontrun{
#' result <- tempest_run("History of jazz", config = tempest_config())
#' tempest_sources(result)
#' }
#' @export
tempest_sources <- function(x) {
  tempest_product_read_workspace(x)$to_tibbles()$retrieved_resources
}

#' Return claims as a tibble
#' @param x A completed [tempest_run()] product or a `TempestSession`.
#' @return A tibble of claims with columns: claim_id, claim_text, claim_type,
#'   source_ids, confidence, verification_status, support_score, created_at.
#' @examples
#' \dontrun{
#' result <- tempest_run("History of jazz", config = tempest_config())
#' tempest_claims(result)
#' }
#' @export
tempest_claims <- function(x) {
  tempest_product_read_workspace(x)$to_tibbles()$proposed_claims
}

# Internal workspace projections for the bundled app and other in-package
# consumers that already hold a validated ResearchWorkspace.
tempest_workspace_sources <- function(workspace) {
  workspace$to_tibbles()$retrieved_resources
}

tempest_workspace_claims <- function(workspace) {
  workspace$to_tibbles()$proposed_claims
}
