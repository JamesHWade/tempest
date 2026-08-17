# STORM outline stage

#' @keywords internal
tempest_draft_outline <- function(
  writer,
  topic,
  title,
  module,
  knowledge_view = module$knowledge_view %||% NULL,
  record_stage = function(record, output = NULL) invisible(record)
) {
  stage_result <- tempest_execute_stage(
    module,
    writer,
    inputs = list(topic = topic, report_title = title),
    context = tempest_stage_context_knowledge_view(
      list(),
      module,
      knowledge_view
    ),
    record_stage = function(record, output = NULL) {
      record_stage(record, output)
    }
  )
  stage_result$output
}

#' @keywords internal
tempest_refine_outline <- function(
  writer,
  topic,
  title,
  draft_outline,
  facts_txt,
  module,
  workspace,
  evidence = list(),
  verified_evidence = list(),
  verified_facts = facts_txt,
  min_support_score = 0.7,
  knowledge_view = module$knowledge_view %||% NULL,
  record_stage = function(record, output = NULL) invisible(record)
) {
  stage_result <- tempest_execute_stage(
    module,
    writer,
    inputs = list(
      topic = topic,
      report_title = title,
      draft_outline = tempest_outline_summary(draft_outline),
      facts = facts_txt
    ),
    context = tempest_stage_context_knowledge_view(
      list(
        workspace = workspace,
        title = title,
        evidence = evidence,
        verified_evidence = verified_evidence,
        verified_facts = verified_facts,
        min_support_score = min_support_score
      ),
      module,
      knowledge_view
    ),
    record_stage = function(record, output = NULL) {
      record_stage(record, output)
    }
  )
  stage_result$output
}

#' @keywords internal
tempest_outline_summary <- function(outline) {
  outline <- tempest_normalize_outline(outline)
  paste(
    vapply(
      outline$sections,
      function(s) {
        paste0(
          "- ",
          s$title,
          ": ",
          s$summary
        )
      },
      character(1)
    ),
    collapse = "\n"
  )
}

#' @keywords internal
tempest_subsections_markdown <- function(subsections) {
  if (
    !is.list(subsections) ||
      is.data.frame(subsections) ||
      !is.null(names(subsections)) ||
      length(subsections) == 0L
  ) {
    tempest_abort(
      "Section writing requires a non-empty unnamed subsection list.",
      class = "tempest_stage_output_error"
    )
  }
  paste(
    vapply(
      subsections,
      function(s) {
        if (!is.list(s) || is.data.frame(s)) {
          tempest_abort(
            "Outline subsection entries must be records.",
            class = "tempest_stage_output_error"
          )
        }
        title <- tempest_stage_string(s$title, "title")
        bullets <- tempest_stage_string_array(s$bullets, "bullets")
        paste0(
          "### ",
          title,
          "\n",
          paste0("- ", bullets, collapse = "\n")
        )
      },
      character(1)
    ),
    collapse = "\n\n"
  )
}

#' @keywords internal
tempest_normalize_outline <- function(x) {
  if (is.null(x) || !is.list(x)) {
    tempest_abort(
      "Outline stage output must be a record.",
      class = "tempest_stage_output_error"
    )
  }
  sections <- x$sections
  if (
    !is.list(sections) ||
      is.data.frame(sections) ||
      !is.null(names(sections)) ||
      length(sections) == 0L
  ) {
    tempest_abort(
      "Outline stage output must contain a non-empty {.field sections} list.",
      class = "tempest_stage_output_error"
    )
  }
  sections <- purrr::map(sections, function(s) {
    if (!is.list(s) || is.data.frame(s)) {
      tempest_abort(
        "Outline section entries must be records.",
        class = "tempest_stage_output_error"
      )
    }
    title <- tempest_stage_string(s$title, "title")
    summary <- tempest_stage_string(s$summary, "summary")
    subsections <- s$subsections
    if (
      !is.list(subsections) ||
        is.data.frame(subsections) ||
        !is.null(names(subsections)) ||
        length(subsections) == 0L
    ) {
      tempest_abort(
        "Outline sections require a non-empty {.field subsections} list.",
        class = "tempest_stage_output_error"
      )
    }
    subsections <- purrr::map(subsections, function(sub) {
      if (!is.list(sub) || is.data.frame(sub)) {
        tempest_abort(
          "Outline subsection entries must be records.",
          class = "tempest_stage_output_error"
        )
      }
      sub_title <- tempest_stage_string(sub$title, "title")
      bullets <- tempest_stage_string_array(sub$bullets, "bullets")
      needed <- if (is.null(sub$needed)) {
        character()
      } else {
        tempest_stage_string_array(
          sub$needed,
          "needed",
          allow_empty = TRUE
        )
      }
      list(
        title = sub_title,
        bullets = bullets,
        needed = needed
      )
    })
    list(
      title = title,
      summary = summary,
      subsections = subsections
    )
  })
  title <- tempest_stage_string(x$title, "title")
  list(
    title = title,
    sections = sections
  )
}
