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
#' @param knowledge_view Optional pinned Graft view required by a governed
#'   `program_set`.
#' @return A list of `tempest_expert` profiles.
#'
#' @keywords internal
tempest_generate_experts <- function(
  topic,
  n = 3,
  config = tempest_config(),
  verbose = FALSE,
  program_set = NULL,
  knowledge_view = NULL
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
  knowledge <- tempest_product_knowledge_view(program_set, knowledge_view)
  module <- tempest_program_set_execution(
    program_set,
    "personas",
    trace_context = tempest_standalone_dsprrr_trace_context("personas")
  )
  module$knowledge_view <- knowledge$view
  tempest_generate_experts_with_program(
    topic = topic,
    n = n,
    config = config,
    verbose = verbose,
    module = module,
    knowledge_view = knowledge$view,
    record_stage = function(record, output = NULL) invisible(record)
  )
}

#' @keywords internal
tempest_persona_requirements <- function(requirements = NULL) {
  if (is.null(requirements)) {
    return(paste(
      "- Each persona should have a distinct professional background",
      "- Personas should complement each other, covering different angles",
      paste0(
        "- Include a mix of academic, industry, and practitioner ",
        "perspectives where appropriate"
      ),
      "- Each persona's focus areas should be specific and non-overlapping",
      "- Initial questions should reflect their unique expertise and concerns",
      sep = "\n"
    ))
  }
  tempest_config_string(requirements, "requirements")
}

#' @keywords internal
tempest_personas_ellmer_fallback <- function(chat, inputs, context) {
  prompt <- paste0(
    "Topic: ",
    inputs$topic,
    "\n\nGenerate exactly ",
    inputs$n_experts,
    " diverse expert personas who would research this topic.\n\n",
    "Requirements:\n",
    inputs$requirements,
    "\n"
  )
  chat$chat_structured(
    prompt,
    type = tempest_type_personas(),
    echo = "none",
    convert = FALSE
  )
}

#' @keywords internal
tempest_generate_experts_with_program <- function(
  topic,
  n,
  config,
  verbose,
  module,
  requirements = NULL,
  knowledge_view = module$knowledge_view %||% NULL,
  record_stage = function(record, output = NULL) invisible(record)
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

  requirements <- tempest_persona_requirements(requirements)

  if (verbose) {
    tempest_inform("Generating {n} expert profiles for: {.val {topic}}")
  }

  result <- tempest_execute_stage(
    module,
    chat,
    inputs = list(
      topic = topic,
      n_experts = n,
      requirements = requirements
    ),
    context = tempest_stage_context_knowledge_view(
      list(n_experts = n),
      module,
      knowledge_view
    ),
    record_stage = function(record, output = NULL) {
      record_stage(record, output)
    }
  )
  experts <- tempest_validate_experts(result$output)

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
  program,
  requirements = NULL,
  knowledge_view = program$knowledge_view %||% NULL,
  record_stage = function(record, output = NULL) invisible(record)
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
  requirements <- tempest_persona_requirements(requirements)
  request <- tempest_execute_stage_async(
    program,
    chat,
    inputs = list(
      topic = topic,
      n_experts = n,
      requirements = requirements
    ),
    context = tempest_stage_context_knowledge_view(
      list(n_experts = n),
      program,
      knowledge_view
    ),
    record_stage = function(record, output = NULL) {
      record_stage(record, output)
    }
  )
  promises::then(request, function(stage_result) {
    structure(
      list(
        experts = tempest_validate_experts(stage_result$output),
        record = stage_result$record
      ),
      class = "tempest_persona_stage_result"
    )
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

  if (
    !is.null(persona$perspective) &&
      nzchar(persona$perspective) &&
      !identical(persona$perspective, persona$background)
  ) {
    parts <- c(parts, paste0("Your perspective: ", persona$perspective))
  }

  paste(parts, collapse = "\n\n")
}

#' Render Expert System Prompt for a Persona
#'
#' @param persona A validated expert profile.
#' @param expert_id Expert ID used in the rendered prompt.
#' @return A rendered system prompt string.
#' @keywords internal
tempest_render_expert_prompt <- function(persona, expert_id = NULL) {
  if (!S7::S7_inherits(persona, TempestExpertProfile)) {
    tempest_abort(
      "{.arg persona} must be a validated Tempest expert profile.",
      class = "tempest_config_error"
    )
  }
  persona <- tempest_expert_runtime_record(persona)
  expert_id <- expert_id %||% persona$expert_id
  persona_details <- paste(
    tempest_format_persona_details(persona),
    paste0("Expert instructions:\n", persona$instructions),
    sep = "\n\n"
  )
  tempest_prompt_render(
    "expert_system",
    persona_name = persona$name,
    persona_title = persona$title,
    persona_details = persona_details
  )
}

#' @keywords internal
tempest_generate_perspectives <- function(
  chat,
  topic,
  seed_context,
  n_experts,
  module,
  knowledge_view = module$knowledge_view %||% NULL,
  record_stage = function(record, output = NULL) invisible(record)
) {
  stage_result <- tempest_execute_stage(
    module,
    chat,
    inputs = list(
      topic = topic,
      seed_context = seed_context,
      n_experts = n_experts
    ),
    context = tempest_stage_context_knowledge_view(
      list(topic = topic, n_experts = n_experts),
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
tempest_stage_string <- function(value, field, allow_empty = FALSE) {
  if (
    !is.character(value) ||
      is.object(value) ||
      !is.null(names(value)) ||
      length(value) != 1L ||
      is.na(value)
  ) {
    tempest_abort(
      "Stage output field {.field {field}} must be a plain scalar string.",
      class = "tempest_stage_output_error"
    )
  }
  value <- tempest_trim(value)
  if (!isTRUE(allow_empty) && !nzchar(value)) {
    tempest_abort(
      "Stage output field {.field {field}} must not be empty.",
      class = "tempest_stage_output_error"
    )
  }
  value
}

#' @keywords internal
tempest_stage_string_array <- function(
  value,
  field,
  allow_empty = FALSE
) {
  valid_scalar <- function(item) {
    is.character(item) &&
      !is.object(item) &&
      is.null(names(item)) &&
      length(item) == 1L &&
      !is.na(item)
  }
  if (is.list(value) && !is.data.frame(value) && is.null(names(value))) {
    if (!all(vapply(value, valid_scalar, logical(1)))) {
      value <- NULL
    } else {
      value <- vapply(value, identity, character(1))
      names(value) <- NULL
    }
  }
  if (
    is.null(value) ||
      !is.character(value) ||
      is.object(value) ||
      !is.null(names(value)) ||
      anyNA(value)
  ) {
    tempest_abort(
      paste0(
        "Stage output field {.field {field}} must be a flat unnamed array ",
        "of strings."
      ),
      class = "tempest_stage_output_error"
    )
  }
  value <- tempest_trim(value)
  if (any(!nzchar(value)) || (!isTRUE(allow_empty) && length(value) == 0L)) {
    tempest_abort(
      "Stage output field {.field {field}} must contain non-empty strings.",
      class = "tempest_stage_output_error"
    )
  }
  value
}

#' @keywords internal
tempest_generated_expert_scalar <- function(value, field, allow_empty = FALSE) {
  tempest_stage_string(value, field, allow_empty = allow_empty)
}

#' @keywords internal
tempest_generated_expert_profile <- function(value) {
  if (!is.list(value) || is.data.frame(value)) {
    tempest_abort(
      "Generated persona entries must be records.",
      class = "tempest_stage_output_error"
    )
  }
  name <- tempest_generated_expert_scalar(value$name, "name")
  title <- tempest_generated_expert_scalar(value$title, "title")
  affiliation <- tempest_generated_expert_scalar(
    value$affiliation,
    "affiliation",
    allow_empty = TRUE
  )
  background <- tempest_generated_expert_scalar(
    value$background,
    "background",
    allow_empty = TRUE
  )
  focus_areas <- tempest_stage_string_array(value$focus_areas, "focus_areas")
  perspective <- tempest_generated_expert_scalar(
    value$perspective,
    "perspective"
  )
  description <- paste(
    c(
      if (nzchar(affiliation)) paste0("Affiliation: ", affiliation),
      if (nzchar(background)) paste0("Background: ", background),
      paste0("Perspective: ", perspective)
    ),
    collapse = "\n"
  )
  instructions <- paste0(
    "Research the topic from this perspective: ",
    perspective
  )
  initial_questions <- tempest_stage_string_array(
    value$initial_questions,
    "initial_questions"
  )
  tempest_expert(
    name = name,
    title = title,
    description = description,
    instructions = instructions,
    focus_areas = focus_areas,
    initial_questions = initial_questions
  )
}

#' @keywords internal
tempest_normalize_experts <- function(x, n = NULL) {
  if (!is.list(x) || is.data.frame(x) || is.null(x$personas)) {
    tempest_abort(
      "Persona stage output must be a record containing {.field personas}.",
      class = "tempest_stage_output_error"
    )
  }
  values <- x$personas
  if (
    !is.list(values) ||
      is.data.frame(values) ||
      !is.null(names(values)) ||
      length(values) == 0L
  ) {
    tempest_abort(
      "Persona stage field {.field personas} must be a non-empty unnamed list.",
      class = "tempest_stage_output_error"
    )
  }
  if (!is.null(n) && !identical(length(values), as.integer(n))) {
    tempest_abort(
      "Persona stage output must contain exactly {as.integer(n)} personas.",
      class = "tempest_stage_output_error"
    )
  }
  purrr::map(values, tempest_generated_expert_profile)
}

#' @keywords internal
tempest_normalize_perspectives <- function(x, topic, n_experts = NULL) {
  if (!is.list(x) || is.data.frame(x)) {
    tempest_abort(
      "Perspective stage output must be a record.",
      class = "tempest_stage_output_error"
    )
  }
  title <- tempest_generated_expert_scalar(x$title, "title")
  perspectives <- x$perspectives
  if (
    !is.list(perspectives) ||
      is.data.frame(perspectives) ||
      !is.null(names(perspectives)) ||
      length(perspectives) == 0L
  ) {
    tempest_abort(
      paste0(
        "Perspective stage output must contain a non-empty ",
        "{.field perspectives} list."
      ),
      class = "tempest_stage_output_error"
    )
  }
  perspectives <- purrr::map(perspectives, function(p) {
    if (!is.list(p) || is.data.frame(p)) {
      tempest_abort(
        "Perspective entries must be records.",
        class = "tempest_stage_output_error"
      )
    }
    key_questions <- tempest_stage_string_array(
      p$key_questions,
      "key_questions"
    )
    list(
      name = tempest_generated_expert_scalar(p$name, "name"),
      description = tempest_generated_expert_scalar(
        p$description,
        "description"
      ),
      key_questions = key_questions
    )
  })
  if (
    !is.null(n_experts) &&
      !identical(length(perspectives), as.integer(n_experts))
  ) {
    tempest_abort(
      paste0(
        "Perspective stage output must contain exactly ",
        "{as.integer(n_experts)} perspectives."
      ),
      class = "tempest_stage_output_error"
    )
  }
  list(
    title = title,
    perspectives = perspectives
  )
}
