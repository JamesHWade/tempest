# STORM perspectives and expert-profile stage

#' Generate expert profiles for a topic
#'
#' Uses an LLM to propose diverse experts who would naturally approach the
#' topic from different angles, then normalizes the provider response into
#' validated, versioned [tempest_expert()] profiles.
#'
#' @param topic The research topic.
#' @param n Number of experts to generate.
#' @param config A `TempestConfig` object.
#' @param verbose Print progress.
#' @param program_set A [TempestProgramSet] containing the exact `personas`
#'   program. If `NULL`, [tempest_program_set()] creates the builtin set.
#' @return A list of `tempest_expert` profiles.
#'
#' @examples
#' \dontrun{
#' experts <- tempest_generate_experts(
#'   topic = "Climate change adaptation",
#'   n = 3,
#'   config = tempest_config()
#' )
#' }
#' @export
tempest_generate_experts <- function(
  topic,
  n = 3,
  config = tempest_config(),
  verbose = FALSE,
  program_set = NULL
) {
  tempest_require("ellmer", "Expert generation requires ellmer.")
  topic <- tempest_config_string(topic, "topic")
  n <- tempest_config_count(n, "n")
  if (n > config@max_active_experts) {
    tempest_config_abort(
      "{.arg n} cannot exceed {.arg max_active_experts}."
    )
  }
  program_set <- program_set %||% tempest_program_set()
  module <- tempest_program_set_execution(
    program_set,
    "personas",
    trace_context = tempest_standalone_dsprrr_trace_context("personas")
  )
  tempest_generate_experts_with_program(
    topic = topic,
    n = n,
    config = config,
    verbose = verbose,
    module = module
  )
}

#' @keywords internal
tempest_generate_experts_with_program <- function(
  topic,
  n,
  config,
  verbose,
  module
) {
  tempest_require("ellmer", "Expert generation requires ellmer.")
  topic <- tempest_config_string(topic, "topic")
  n <- tempest_config_count(n, "n")
  if (n > config@max_active_experts) {
    tempest_config_abort(
      "{.arg n} cannot exceed {.arg max_active_experts}."
    )
  }

  chat <- tempest_make_chat(
    config,
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
    tempest_inform("Generating {n} expert profiles for: {.val {topic}}")
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
    step = "expert generation"
  )
  if (is.null(result)) {
    result <- chat$chat_structured(
      prompt,
      type = tempest_type_personas(),
      echo = "none",
      convert = FALSE
    )
  }

  experts <- tempest_normalize_experts(result, n = n)

  if (verbose) {
    for (expert in experts) {
      tempest_inform("  - {expert@name}, {expert@title}")
    }
  }

  experts
}

# Execute persona generation through a ProgramSet-bound dsprrr program without
# blocking the Shiny event loop.
tempest_generate_experts_async <- function(
  topic,
  n = 3,
  config = tempest_config(),
  program
) {
  tempest_require("ellmer", "Expert generation requires ellmer.")
  tempest_require("promises", "Async expert generation requires promises.")
  topic <- tempest_config_string(topic, "topic")
  n <- tempest_config_count(n, "n")
  if (n > config@max_active_experts) {
    tempest_config_abort(
      "{.arg n} cannot exceed {.arg max_active_experts}."
    )
  }
  chat <- tempest_make_chat(
    config,
    "coordinator",
    system_prompt = tempest_prompt("persona_generator_system"),
    echo = "none"
  )
  prompt <- paste0(
    "Topic: ",
    topic,
    "\n\nGenerate exactly ",
    n,
    " diverse expert personas who would research this topic.\n\n",
    "Requirements:\n",
    "- Each persona should have a distinct professional background\n",
    "- Personas should complement each other, covering different angles\n",
    "- Include a mix of academic, industry, and practitioner perspectives where appropriate\n",
    "- Each persona's focus areas should be specific and non-overlapping\n",
    "- Initial questions should reflect their unique expertise and concerns\n"
  )
  request <- tempest_run_dsprrr_module_async(
    program,
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
    step = "expert generation"
  )
  if (is.null(request)) {
    request <- chat$chat_structured_async(
      prompt,
      type = tempest_type_personas(),
      echo = "none",
      convert = FALSE
    )
  }
  promises::then(request, function(result) {
    tempest_normalize_experts(result, n = n)
  })
}

#' Format Persona Details for Prompt
#'
#' @param persona A plain runtime expert record.
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
  if (
    !is.null(persona) &&
      S7::S7_inherits(persona, TempestExpertProfile)
  ) {
    persona <- tempest_expert_runtime_record(persona)
  }
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
tempest_generated_expert_scalar <- function(value, default) {
  if (is.null(value) || length(value) == 0L) {
    return(default)
  }
  value <- as.character(value[[1]])
  if (length(value) != 1L || is.na(value)) {
    return(default)
  }
  value <- tempest_trim(value)
  if (!nzchar(value)) default else value
}

#' @keywords internal
tempest_generated_expert_profile <- function(value, index) {
  name <- tempest_generated_expert_scalar(value$name, "Expert")
  title <- tempest_generated_expert_scalar(
    value$title,
    "Research Specialist"
  )
  affiliation <- tempest_generated_expert_scalar(value$affiliation, "")
  background <- tempest_generated_expert_scalar(value$background, "")
  focus_areas <- tempest_as_character_vector(
    value$focus_areas %||% character()
  )
  description <- tempest_generated_expert_scalar(
    value$perspective,
    tempest_generated_expert_scalar(
      value$description,
      if (nzchar(background)) background else "General research perspective"
    )
  )
  instructions <- tempest_generated_expert_scalar(
    value$instructions,
    paste0("Research the topic from this perspective: ", description)
  )
  initial_questions <- tempest_as_character_vector(
    value$initial_questions %||% character()
  )
  initial_work_items <- tempest_as_character_vector(
    value$initial_work_items %||% character()
  )
  normalized <- list(
    name = name,
    title = title,
    affiliation = affiliation,
    background = background,
    focus_areas = focus_areas,
    description = description,
    instructions = instructions,
    initial_questions = initial_questions,
    initial_work_items = initial_work_items
  )
  metadata <- list()
  if (nzchar(affiliation)) {
    metadata$affiliation <- affiliation
  }
  if (nzchar(background)) {
    metadata$background <- background
  }

  tempest_expert(
    expert_id = tempest_generated_expert_id(normalized, index = index),
    version = "1",
    name = name,
    title = title,
    description = description,
    instructions = instructions,
    focus_areas = focus_areas,
    required_capability_ids = c(
      "tempest.research.web",
      "tempest.evidence.read",
      "tempest.evidence.write"
    ),
    optional_capability_ids = "tempest.retrieval.semantic",
    selection_metadata = list(
      origin = "tempest.generated",
      position = as.integer(index)
    ),
    initial_work_items = initial_work_items,
    initial_questions = initial_questions,
    metadata = metadata
  )
}

#' @keywords internal
tempest_fallback_expert_profile <- function(index) {
  tempest_expert(
    expert_id = paste0("expert.fallback-", as.integer(index)),
    name = paste("Expert", as.integer(index)),
    title = "Research Specialist",
    description = "General evidence-backed research perspective.",
    instructions = paste(
      "Inspect evidence, distinguish inference from sourced claims,",
      "and preserve uncertainty."
    ),
    required_capability_ids = c(
      "tempest.research.web",
      "tempest.evidence.read",
      "tempest.evidence.write"
    ),
    optional_capability_ids = "tempest.retrieval.semantic"
  )
}

#' @keywords internal
tempest_normalize_experts <- function(x, n = NULL) {
  values <- if (is.list(x) && !is.null(x$personas)) x$personas else x
  if (is.null(values) || length(values) == 0 || !is.list(values)) {
    return(list())
  }
  if (is.data.frame(values)) {
    values <- split(values, seq_len(nrow(values)))
  }
  if (!is.null(n) && length(values) > n) {
    values <- values[seq_len(n)]
  }
  purrr::map2(
    values,
    seq_along(values),
    tempest_generated_expert_profile
  )
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
