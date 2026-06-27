# STORM perspectives & personas stage

#' Generate Expert Personas for a Topic
#'
#' Uses an LLM to generate diverse expert personas who would naturally
#' approach the topic from different angles, following the STORM methodology.
#'
#' @param topic The research topic.
#' @param n Number of personas to generate.
#' @param config A `TempestConfig` object.
#' @param verbose Print progress.
#' @param module Optional dsprrr module used internally.
#' @return A list of persona objects.
#'
#' @examples
#' \dontrun{
#' personas <- tempest_generate_personas(
#'   topic = "Climate change adaptation",
#'   n = 3,
#'   config = tempest_config()
#' )
#' }
#' @export
tempest_generate_personas <- function(
  topic,
  n = 3,
  config = tempest_config(),
  verbose = FALSE,
  module = NULL
) {
  tempest_require("ellmer", "Persona generation requires ellmer.")

  chat <- config$make_chat(
    "coordinator",
    system_prompt = tempest_prompt("persona_generator_system"),
    echo = "none"
  )

  prompt <- paste0(
    "Topic: ",
    topic,
    "\n\n",
    "Generate exactly ",
    n,
    " diverse expert personas who would research this topic.\n\n",
    "Requirements:\n",
    "- Each persona should have a distinct professional background\n",
    "- Personas should complement each other, covering different angles\n",
    "- Include a mix of academic, industry, and practitioner perspectives where appropriate\n",
    "- Each persona's focus areas should be specific and non-overlapping\n",
    "- Initial questions should reflect their unique expertise and concerns\n"
  )

  if (verbose) {
    tempest_inform("Generating {n} expert personas for: {.val {topic}}")
  }

  result <- tempest_run_dsprrr_module(
    module,
    chat,
    inputs = list(
      topic = topic,
      n_experts = n,
      requirements = paste(
        "- Each persona should have a distinct professional background",
        "- Personas should complement each other, covering different angles",
        "- Include a mix of academic, industry, and practitioner perspectives where appropriate",
        "- Each persona's focus areas should be specific and non-overlapping",
        "- Initial questions should reflect their unique expertise and concerns",
        sep = "\n"
      )
    ),
    step = "persona generation"
  )
  if (is.null(result)) {
    result <- chat$chat_structured(
      prompt,
      type = tempest_type_personas(),
      echo = "none",
      convert = FALSE
    )
  }

  personas <- tempest_normalize_personas(result, n = n)

  # Add an id to each persona
  for (i in seq_along(personas)) {
    personas[[i]]$id <- i
  }

  if (verbose) {
    for (p in personas) {
      tempest_inform("  - {p$name}, {p$title}")
    }
  }

  personas
}

#' Format Persona Details for Prompt
#'
#' @param persona A persona object from `tempest_generate_personas()`.
#' @return A formatted string with persona details.
#' @keywords internal
tempest_format_persona_details <- function(persona) {
  parts <- character()

  if (!is.null(persona$affiliation) && nzchar(persona$affiliation)) {
    parts <- c(parts, paste0("Affiliation: ", persona$affiliation))
  }

  if (!is.null(persona$background) && nzchar(persona$background)) {
    parts <- c(parts, paste0("Background: ", persona$background))
  }

  if (!is.null(persona$focus_areas) && length(persona$focus_areas) > 0) {
    focus <- paste(persona$focus_areas, collapse = ", ")
    parts <- c(parts, paste0("Focus areas: ", focus))
  }

  if (!is.null(persona$perspective) && nzchar(persona$perspective)) {
    parts <- c(parts, paste0("Your perspective: ", persona$perspective))
  }

  paste(parts, collapse = "\n\n")
}

#' Render Expert System Prompt for a Persona
#'
#' @param persona A persona object, or NULL for a generic expert.
#' @param expert_id Fallback expert ID if no persona provided.
#' @return A rendered system prompt string.
#' @keywords internal
tempest_render_expert_prompt <- function(persona = NULL, expert_id = 1) {
  if (is.null(persona)) {
    # Fallback to generic expert
    tempest_prompt_render(
      "expert_system",
      persona_name = paste("Expert", expert_id),
      persona_title = "Research Specialist",
      persona_details = ""
    )
  } else {
    tempest_prompt_render(
      "expert_system",
      persona_name = persona$name %||% paste("Expert", expert_id),
      persona_title = persona$title %||% "Research Specialist",
      persona_details = tempest_format_persona_details(persona)
    )
  }
}

#' @keywords internal
tempest_generate_perspectives <- function(
  chat,
  topic,
  seed_context,
  n_experts,
  module = NULL
) {
  module_result <- tempest_run_dsprrr_module(
    module,
    chat,
    inputs = list(
      topic = topic,
      seed_context = seed_context,
      n_experts = n_experts
    ),
    step = "perspective generation"
  )
  if (!is.null(module_result)) {
    return(tempest_normalize_perspectives(
      module_result,
      topic,
      n_experts = n_experts
    ))
  }

  prompt <- paste0(
    "You are planning a comprehensive research report.\n",
    "Topic: ",
    topic,
    "\n\n",
    seed_context,
    "\n\n",
    "Propose exactly ",
    n_experts,
    " distinct perspectives to cover the topic. Each perspective should have 3-6 research questions.\n",
    "Return structured data."
  )
  plan <- chat$chat_structured(
    prompt,
    type = tempest_type_perspectives(),
    echo = "none",
    convert = FALSE
  )
  tempest_normalize_perspectives(plan, topic, n_experts = n_experts)
}

#' @keywords internal
tempest_normalize_personas <- function(x, n = NULL) {
  personas <- if (is.list(x) && !is.null(x$personas)) x$personas else x
  if (is.null(personas) || length(personas) == 0 || !is.list(personas)) {
    return(list())
  }
  if (is.data.frame(personas)) {
    personas <- split(personas, seq_len(nrow(personas)))
  }
  personas <- purrr::map(personas, function(p) {
    list(
      name = p$name %||% "Expert",
      title = p$title %||% "Research Specialist",
      affiliation = p$affiliation %||% "",
      background = p$background %||% "",
      focus_areas = tempest_as_character_vector(p$focus_areas %||% character()),
      perspective = p$perspective %||% "",
      initial_questions = tempest_as_character_vector(
        p$initial_questions %||% character()
      )
    )
  })
  if (!is.null(n) && length(personas) > n) {
    personas <- personas[seq_len(n)]
  }
  personas
}

#' @keywords internal
tempest_normalize_perspectives <- function(x, topic, n_experts = NULL) {
  perspectives <- if (is.list(x) && !is.null(x$perspectives)) {
    x$perspectives
  } else {
    list()
  }
  if (is.data.frame(perspectives)) {
    perspectives <- split(perspectives, seq_len(nrow(perspectives)))
  }
  if (!is.list(perspectives) || length(perspectives) == 0) {
    perspectives <- list(list(
      name = "Overview",
      description = "General overview",
      key_questions = topic
    ))
  }
  perspectives <- purrr::map(perspectives, function(p) {
    list(
      name = p$name %||% "Perspective",
      description = p$description %||% "",
      key_questions = tempest_as_character_vector(p$key_questions %||% topic)
    )
  })
  if (!is.null(n_experts) && length(perspectives) > n_experts) {
    perspectives <- perspectives[seq_len(n_experts)]
  }
  list(
    title = if (is.list(x)) x$title %||% topic else topic,
    perspectives = perspectives
  )
}
