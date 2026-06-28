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
