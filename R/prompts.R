# Prompt loading

#' Load a bundled prompt template
#'
#' @param name Prompt name (without extension), e.g. "writer_system".
#' @return A character string containing the prompt.
#' @keywords internal
tempest_prompt <- function(name) {
  path <- tempest_pkg_file("prompts", paste0(name, ".md"))
  if (identical(path, "")) {
    tempest_abort(
      "Unknown prompt {.val {name}}. Prompt file not found in inst/prompts."
    )
  }
  tempest_read_text(path)
}

#' @keywords internal
tempest_prompt_render <- function(name, ...) {
  tmpl <- tempest_prompt(name)
  glue::glue_data(list(...), tmpl, .open = "{{", .close = "}}")
}
