# STORM outline stage

#' @keywords internal
tempest_draft_outline <- function(writer, topic, title, module = NULL) {
  module_result <- tempest_run_dsprrr_module(
    module,
    writer,
    inputs = list(topic = topic, title = title),
    step = "draft outline generation"
  )
  if (!is.null(module_result)) {
    return(tempest_normalize_outline(module_result, fallback_title = title))
  }

  draft_outline_prompt <- paste0(
    "Create a draft outline for a report based on your own knowledge.\n\n",
    "Topic: ",
    topic,
    "\n",
    "Desired report title: ",
    title,
    "\n\n",
    "Requirements:\n",
    "- Organize into 4-6 sections based on what you know about the topic.\n",
    "- Include subsections with bullet points.\n",
    "- This is a preliminary outline; it will be refined with research findings.\n",
    "- Return structured data.\n"
  )
  writer$chat_structured(
    draft_outline_prompt,
    type = tempest_type_outline(),
    echo = "none",
    convert = FALSE
  ) |>
    tempest_normalize_outline(fallback_title = title)
}

#' @keywords internal
tempest_refine_outline <- function(
  writer,
  topic,
  title,
  draft_outline,
  facts_txt,
  module = NULL
) {
  module_result <- tempest_run_dsprrr_module(
    module,
    writer,
    inputs = list(
      topic = topic,
      title = title,
      draft_outline = tempest_outline_summary(draft_outline),
      facts = facts_txt
    ),
    step = "outline refinement"
  )
  if (!is.null(module_result)) {
    return(tempest_normalize_outline(module_result, fallback_title = title))
  }

  refine_prompt <- paste0(
    "Refine this draft outline using verified research findings.\n\n",
    "Topic: ",
    topic,
    "\n",
    "Desired report title: ",
    title,
    "\n\n",
    "Draft outline sections:\n",
    tempest_outline_summary(draft_outline),
    "\n\n",
    "Available verified fact notes (each includes citations):\n",
    facts_txt,
    "\n\n",
    "Requirements:\n",
    "- Adjust sections based on available evidence.\n",
    "- Add, merge, or remove sections as needed.\n",
    "- Ensure each section has supporting facts.\n",
    "- Return structured data.\n"
  )
  writer$chat_structured(
    refine_prompt,
    type = tempest_type_outline(),
    echo = "none",
    convert = FALSE
  ) |>
    tempest_normalize_outline(fallback_title = title)
}

#' @keywords internal
tempest_prompt_lines <- function(x, empty = "(none)") {
  if (is.null(x) || length(x) == 0) {
    return(empty)
  }
  x <- unlist(x, recursive = TRUE, use.names = FALSE)
  x <- tempest_trim(as.character(x))
  x <- x[nzchar(x) & !is.na(x)]
  if (length(x) == 0) {
    return(empty)
  }
  paste(x, collapse = "\n")
}

#' @keywords internal
tempest_outline_summary <- function(outline) {
  sections <- outline$sections %||% list()
  if (length(sections) == 0) {
    return("(no sections)")
  }
  paste(
    purrr::map_chr(sections, function(s) {
      paste0(
        "- ",
        tempest_outline_text(s$title, "Section"),
        ": ",
        tempest_outline_text(s$summary, "")
      )
    }),
    collapse = "\n"
  )
}

#' @keywords internal
tempest_outline_text <- function(x, default = "") {
  values <- tempest_as_character_vector(x)
  if (length(values) == 0) {
    return(default)
  }
  paste(values, collapse = " ")
}

#' @keywords internal
tempest_subsections_markdown <- function(subsections) {
  if (is.null(subsections) || length(subsections) == 0) {
    return("(none)")
  }
  paste(
    purrr::map_chr(subsections, function(s) {
      bullets <- tempest_as_character_vector(s$bullets %||% character())
      bullet_md <- if (length(bullets) == 0) {
        ""
      } else {
        paste0("- ", bullets, collapse = "\n")
      }
      paste0(
        "### ",
        tempest_outline_text(s$title, "Subsection"),
        "\n",
        bullet_md
      )
    }),
    collapse = "\n\n"
  )
}

#' @keywords internal
tempest_normalize_outline <- function(x, fallback_title) {
  if (is.null(x) || !is.list(x)) {
    return(list(title = fallback_title, sections = list()))
  }
  if (is.list(x) && !is.null(x$outline) && is.list(x$outline)) {
    x <- x$outline
  }
  sections <- x$sections %||% list()
  if (is.data.frame(sections)) {
    sections <- split(sections, seq_len(nrow(sections)))
  }
  sections <- purrr::map(sections, function(s) {
    subsections <- s$subsections %||% list()
    if (is.data.frame(subsections)) {
      subsections <- split(subsections, seq_len(nrow(subsections)))
    }
    subsections <- purrr::map(subsections, function(sub) {
      list(
        title = tempest_outline_text(sub$title, "Subsection"),
        bullets = tempest_as_character_vector(sub$bullets %||% character()),
        needed = tempest_as_character_vector(sub$needed %||% character())
      )
    })
    list(
      title = tempest_outline_text(s$title, "Section"),
      summary = tempest_outline_text(s$summary, ""),
      subsections = subsections
    )
  })
  list(
    title = tempest_outline_text(x$title, fallback_title),
    sections = sections
  )
}
