#' @keywords internal
tempest_suggested_questions_validate <- function(value) {
  if (
    !is.character(value) ||
      is.object(value) ||
      !is.null(names(value)) ||
      anyNA(value) ||
      any(!nzchar(value)) ||
      !identical(tempest_trim(value), value) ||
      anyDuplicated(value)
  ) {
    tempest_abort(
      paste0(
        "Suggested questions must be unique, exact, non-empty strings in an ",
        "unnamed character vector."
      ),
      class = "tempest_stage_output_error"
    )
  }
  value
}
