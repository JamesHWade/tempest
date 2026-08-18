# STORM polish stage

#' @keywords internal
tempest_polish_rules <- function(remove_duplicate = FALSE) {
  rules <- c(
    "- Do not add new facts.",
    "- Preserve all citations like [Sxxxxxxxxxxxx].",
    "- Improve clarity, flow, and headings."
  )
  if (isTRUE(remove_duplicate)) {
    rules <- c(
      rules,
      "- Remove duplicate or highly repetitive content while preserving unique cited claims."
    )
  }
  paste(rules, collapse = "\n")
}

#' @keywords internal
tempest_normalize_text_output <- function(x, field) {
  if (is.list(x) && !is.null(x[[field]])) {
    return(as.character(x[[field]]))
  }
  if (is.character(x) && length(x) > 0) {
    return(paste(x, collapse = "\n"))
  }
  NA_character_
}

#' @keywords internal
tempest_storm_polish_report <- function(
  title,
  draft_md,
  workspace,
  config
) {
  if (!rlang::is_string(draft_md) || !nzchar(tempest_trim(draft_md))) {
    tempest_stage_governance_abort(
      "STORM polish requires one non-empty Markdown draft."
    )
  }
  report_title <- tempest_report_title_validate(title)
  tempest_report_body_validate_reserved_sections(draft_md)
  tempest_report_md_render(
    title = report_title,
    body = draft_md,
    workspace = workspace,
    citation_policy = config@citation_policy,
    on_unsupported_claim = config@on_unsupported_claim,
    min_support_score = config@min_support_score,
    include_references = TRUE
  )
}
