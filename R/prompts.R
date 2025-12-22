# Prompt loading

#' Load a bundled prompt template
#'
#' @param name Prompt name (without extension), e.g. "writer_system".
#' @return A character string containing the prompt.
#' @keywords internal
storm_prompt <- function(name) {
  path <- stormr_pkg_file("prompts", paste0(name, ".md"))
  if (identical(path, "")) {
    stormr_abort("Unknown prompt {.val {name}}. Prompt file not found in inst/prompts.")
  }
  stormr_read_text(path)
}

#' @keywords internal
storm_prompt_render <- function(name, ...) {
  tmpl <- storm_prompt(name)
  glue::glue_data(list(...), tmpl, .open = "{{", .close = "}}")
}
