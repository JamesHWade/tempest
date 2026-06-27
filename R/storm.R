# STORM pipeline (scripted)

#' @keywords internal
tempest_type_perspectives <- function() {
  tempest_require("ellmer")
  ellmer::type_object(
    title = ellmer::type_string("Suggested title for the final report."),
    perspectives = ellmer::type_array(
      ellmer::type_object(
        name = ellmer::type_string("Perspective name (short)"),
        description = ellmer::type_string("What this perspective covers"),
        key_questions = ellmer::type_array(ellmer::type_string(
          "A research question"
        ))
      )
    )
  )
}

#' @keywords internal
tempest_type_personas <- function() {
  tempest_require("ellmer")
  ellmer::type_object(
    personas = ellmer::type_array(
      ellmer::type_object(
        name = ellmer::type_string(
          "Full name of the persona (e.g., 'Dr. Sarah Chen')"
        ),
        title = ellmer::type_string(
          "Professional title or role (e.g., 'Climate Scientist')"
        ),
        affiliation = ellmer::type_string(
          "Organization or institution (e.g., 'Arctic Research Institute')"
        ),
        background = ellmer::type_string(
          "2-3 sentence background: expertise, experience, what shaped their perspective"
        ),
        focus_areas = ellmer::type_array(ellmer::type_string(
          "Specific focus area or subtopic"
        )),
        perspective = ellmer::type_string(
          "How this persona uniquely approaches the topic - their angle or lens"
        ),
        initial_questions = ellmer::type_array(ellmer::type_string(
          "Questions this persona would naturally ask"
        ))
      )
    )
  )
}

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
tempest_type_outline <- function() {
  tempest_require("ellmer")
  subsection <- ellmer::type_object(
    title = ellmer::type_string("Subsection title"),
    bullets = ellmer::type_array(ellmer::type_string(
      "Bullet: a specific point to cover"
    )),
    needed = ellmer::type_array(
      ellmer::type_string("If needed: unanswered question"),
      required = FALSE
    )
  )
  section <- ellmer::type_object(
    title = ellmer::type_string("Section title"),
    summary = ellmer::type_string("1-2 sentence summary"),
    subsections = ellmer::type_array(subsection)
  )
  ellmer::type_object(
    title = ellmer::type_string("Report title"),
    sections = ellmer::type_array(section)
  )
}

#' @keywords internal
tempest_type_fact_extract <- function() {
  tempest_require("ellmer")
  src <- ellmer::type_object(
    source_id = ellmer::type_string("Source id like Sxxxxxxxxxxxx"),
    url = ellmer::type_string("URL if available", required = FALSE),
    quote = ellmer::type_string(
      "Short supporting quote (<=20 words) if available",
      required = FALSE
    )
  )
  fact <- ellmer::type_object(
    claim = ellmer::type_string("Atomic factual claim"),
    sources = ellmer::type_array(src),
    confidence = ellmer::type_enum(
      c("low", "medium", "high"),
      "Confidence",
      required = FALSE
    ),
    note = ellmer::type_string(
      "Nuance, scope conditions, or caveats.",
      required = FALSE
    )
  )
  ellmer::type_object(facts = ellmer::type_array(fact))
}

#' @keywords internal
tempest_type_next_question <- function() {
  tempest_require("ellmer")
  ellmer::type_object(
    question = ellmer::type_string("Next research question to ask the expert."),
    done = ellmer::type_boolean(
      "Whether research for this perspective is complete.",
      required = FALSE
    )
  )
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
    "Propose ",
    n_experts,
    "-",
    n_experts + 2,
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
tempest_generate_next_question <- function(
  writer_chat,
  topic,
  perspective,
  answered_md,
  facts_md,
  module = NULL
) {
  type <- tempest_type_next_question()
  module_result <- tempest_run_dsprrr_module(
    module,
    writer_chat,
    inputs = list(
      topic = topic,
      perspective = paste(
        perspective$name %||% "",
        perspective$description %||% "",
        sep = "\n"
      ),
      answered = answered_md,
      facts = facts_md
    ),
    step = "next-question generation"
  )
  if (!is.null(module_result)) {
    return(module_result)
  }

  prompt <- paste0(
    "You are interviewing an expert to build a factual knowledge base.\n\n",
    "Topic: ",
    topic,
    "\n",
    "Perspective: ",
    (perspective$name %||% ""),
    "\n",
    "Description: ",
    (perspective$description %||% ""),
    "\n\n",
    "Answered Q&A so far:\n",
    answered_md,
    "\n\n",
    "Current fact notes summary:\n",
    facts_md,
    "\n\n",
    "Propose the single most useful next question to ask.\n",
    "If this perspective is sufficiently covered, set done = true.\n",
    "Return structured data."
  )
  writer_chat$chat_structured(
    prompt,
    type = type,
    echo = "none",
    convert = FALSE
  )
}

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
tempest_write_section <- function(
  writer,
  section_title,
  section_summary,
  subsections,
  facts_txt,
  module = NULL,
  verbose = FALSE
) {
  subsections_txt <- tempest_subsections_markdown(subsections)
  module_result <- tempest_run_dsprrr_module(
    module,
    writer,
    inputs = list(
      section_title = section_title,
      section_summary = section_summary,
      subsections = subsections_txt,
      facts = facts_txt
    ),
    step = "section writing"
  )
  section_text <- if (!is.null(module_result)) {
    tempest_normalize_text_output(module_result, "section_text")
  } else {
    NA_character_
  }
  if (!is.na(section_text) && nzchar(section_text)) {
    return(section_text)
  }

  prompt <- paste0(
    "Write a report section in Markdown.\n\n",
    "Section title: ",
    section_title,
    "\n",
    "Section intent: ",
    section_summary,
    "\n\n",
    "Planned subsections:\n",
    subsections_txt,
    "\n\n",
    "Verified fact notes you MUST use (do not invent facts or fetch additional sources):\n",
    facts_txt,
    "\n\n",
    "Rules:\n",
    "- Every factual claim must end with one or more citations like [Sxxxxxxxxxxxx].\n",
    "- Use ONLY the facts provided above. Do NOT call tools or fetch new sources.\n",
    "- Keep the writing concise, technical, and well-structured.\n\n",
    "Write the section now:"
  )
  writer$chat(prompt, echo = if (verbose) "output" else "none")
}

#' @keywords internal
tempest_should_skip_section <- function(section_title) {
  grepl(
    "^(introduction|conclusion|summary|overview)$",
    tolower(tempest_trim(section_title %||% ""))
  )
}

#' @keywords internal
tempest_sections_to_write <- function(outline) {
  sections <- outline$sections %||% list()
  purrr::keep(sections, function(section) {
    !tempest_should_skip_section(section$title %||% "Section")
  })
}

#' @keywords internal
tempest_section_facts_text <- function(
  retriever,
  store,
  section_title,
  max_items
) {
  relevant <- tempest_semantic_filter_facts(
    retriever,
    query = section_title,
    store = store,
    max_items = max_items
  )
  if (length(relevant) == 0) {
    return("(no directly matched facts; use best judgement and call tools)")
  }

  paste(
    purrr::map_chr(relevant, function(f) {
      paste0("- ", f$claim, " [", paste(f$source_ids, collapse = ", "), "]")
    }),
    collapse = "\n"
  )
}

#' @keywords internal
tempest_section_jobs <- function(outline, retriever, store, retrieve_top_k) {
  sections <- tempest_sections_to_write(outline)
  purrr::imap(sections, function(section, i) {
    section_title <- section$title %||% "Section"
    list(
      index = i,
      title = section_title,
      summary = section$summary %||% "",
      subsections = section$subsections %||% list(),
      facts_text = tempest_section_facts_text(
        retriever,
        store,
        section_title,
        max_items = retrieve_top_k
      )
    )
  })
}

#' @keywords internal
tempest_run_section_job <- function(
  job,
  writer,
  module = NULL,
  verbose = FALSE
) {
  section_text <- tempest_write_section(
    writer,
    section_title = job$title,
    section_summary = job$summary,
    subsections = job$subsections,
    facts_txt = job$facts_text,
    module = module,
    verbose = verbose
  )
  list(
    index = job$index,
    title = job$title,
    section_text = section_text,
    markdown = paste0("## ", job$title, "\n\n", section_text)
  )
}

#' @keywords internal
tempest_write_sections_sequential <- function(
  jobs,
  writer,
  dsprrr_modules = NULL,
  verbose = FALSE
) {
  purrr::map(jobs, function(job) {
    tempest_run_section_job(
      job,
      writer,
      module = dsprrr_modules$section_writing,
      verbose = verbose
    )
  })
}

#' @keywords internal
tempest_write_sections_parallel <- function(
  jobs,
  config,
  dsprrr_modules = NULL
) {
  tempest_require("mirai", "Parallel section writing requires mirai.")

  results <- mirai::mirai_map(
    seq_along(jobs),
    function(i, jobs, config, dsprrr_modules) {
      job <- jobs[[i]]
      writer <- config$make_chat("writer", echo = "none")
      modules <- dsprrr_modules %||% tempest_make_dsprrr_modules(config)
      tempest_run_section_job(
        job,
        writer,
        module = modules$section_writing,
        verbose = FALSE
      )
    },
    .args = list(
      jobs = jobs,
      config = config,
      dsprrr_modules = dsprrr_modules
    )
  )

  purrr::map(seq_along(results), function(i) {
    val <- tryCatch(results[[i]]$data, error = function(e) NULL)
    if (inherits(val, "errorValue") || is.null(val)) {
      tempest_warn(
        "Parallel section writing failed for {.val {jobs[[i]]$title}}; retrying sequentially."
      )
      return(NULL)
    }
    val
  })
}

#' @keywords internal
tempest_write_section_jobs <- function(
  jobs,
  writer,
  config,
  dsprrr_modules = NULL,
  parallel = FALSE,
  verbose = FALSE
) {
  if (length(jobs) == 0) {
    return(list())
  }

  if (isTRUE(parallel) && length(jobs) > 1) {
    if (!tempest_has("mirai")) {
      tempest_warn(
        "Parallel section writing requires mirai; writing sections sequentially."
      )
    } else {
      parallel_results <- tempest_write_sections_parallel(
        jobs,
        config,
        dsprrr_modules = dsprrr_modules
      )
      failed <- which(vapply(parallel_results, is.null, logical(1)))
      if (length(failed) == 0) {
        return(parallel_results)
      }
      sequential_results <- tempest_write_sections_sequential(
        jobs[failed],
        writer,
        dsprrr_modules = dsprrr_modules,
        verbose = verbose
      )
      parallel_results[failed] <- sequential_results
      return(parallel_results)
    }
  }

  tempest_write_sections_sequential(
    jobs,
    writer,
    dsprrr_modules = dsprrr_modules,
    verbose = verbose
  )
}

#' @keywords internal
tempest_write_lead_section <- function(
  writer,
  topic,
  title,
  draft_md,
  facts_txt,
  module = NULL,
  verbose = FALSE
) {
  module_result <- tempest_run_dsprrr_module(
    module,
    writer,
    inputs = list(
      topic = topic,
      title = title,
      article_body = substr(draft_md, 1, 3000),
      facts = facts_txt
    ),
    step = "lead section generation"
  )
  lead <- if (!is.null(module_result)) {
    tempest_normalize_text_output(module_result, "lead_section")
  } else {
    NA_character_
  }
  if (!is.na(lead) && nzchar(lead)) {
    return(lead)
  }

  lead_prompt <- paste0(
    "Write a Wikipedia-style lead section (2-3 paragraphs) for this report.\n\n",
    "Topic: ",
    topic,
    "\n",
    "Title: ",
    title,
    "\n\n",
    "Article body (for context):\n",
    substr(draft_md, 1, 3000),
    "\n\n",
    "Key facts:\n",
    facts_txt,
    "\n\n",
    "Rules:\n",
    "- Summarize the most important points from the article.\n",
    "- Include citations like [Sxxxxxxxxxxxx] for key claims.\n",
    "- The lead should be self-contained.\n",
    "- Write 2-3 paragraphs.\n"
  )
  writer$chat(lead_prompt, echo = if (verbose) "output" else "none")
}

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

tempest_summarize_facts_for_prompt <- function(store, max_items = 60) {
  stopifnot(inherits(store, "SourceStore"))
  facts <- store$list_facts()
  if (length(facts) == 0) {
    return("(no facts yet)")
  }
  facts <- facts[seq_len(min(length(facts), max_items))]
  lines <- purrr::map_chr(facts, function(f) {
    cites <- paste0("[", paste(f$source_ids, collapse = ", "), "]")
    glue::glue("- {f$claim} {cites}")
  })
  paste(lines, collapse = "\n")
}

#' @keywords internal
tempest_keyword_filter_facts <- function(store, query, max_items = 30) {
  facts <- store$list_facts()
  if (length(facts) == 0) {
    return(list())
  }

  tokens <- unique(tolower(unlist(strsplit(tempest_trim(query), "\\s+"))))
  tokens <- tokens[nzchar(tokens)]
  if (length(tokens) == 0) {
    return(list())
  }

  scored <- vapply(
    facts,
    function(f) {
      claim <- tolower(f$claim %||% "")
      sum(vapply(tokens, function(t) grepl(t, claim, fixed = TRUE), logical(1)))
    },
    integer(1)
  )

  ord <- order(scored, decreasing = TRUE)
  keep <- facts[ord]
  keep <- keep[scored[ord] > 0]
  keep[seq_len(min(length(keep), max_items))]
}

#' Semantic fact retrieval
#'
#' When ragnar is configured, retrieves semantically similar chunks and maps
#' them back to facts. Falls back to keyword filtering otherwise.
#'
#' @param retriever A `TempestRetriever` object.
#' @param query Search query.
#' @param store A `SourceStore` object.
#' @param max_items Maximum facts to return.
#' @return A list of fact objects.
#' @keywords internal
tempest_semantic_filter_facts <- function(
  retriever,
  query,
  store,
  max_items = 30
) {
  # Fall back to keyword when ragnar is unavailable
  if (is.null(retriever$ragnar_store) || !tempest_has("ragnar")) {
    return(tempest_keyword_filter_facts(store, query, max_items = max_items))
  }

  facts <- store$list_facts()
  if (length(facts) == 0) {
    return(list())
  }

  # Retrieve semantically similar chunks
  chunks <- tryCatch(
    retriever$retrieve(query, k = max_items, method = "hybrid"),
    error = function(e) {
      tempest_warn(
        "Hybrid retrieval failed, falling back to BM25: {conditionMessage(e)}"
      )
      tryCatch(
        retriever$retrieve(query, k = max_items, method = "bm25"),
        error = function(e2) {
          tempest_warn(
            "BM25 retrieval also failed, falling back to keyword filter: {conditionMessage(e2)}"
          )
          NULL
        }
      )
    }
  )

  if (is.null(chunks) || nrow(chunks) == 0) {
    return(tempest_keyword_filter_facts(store, query, max_items = max_items))
  }

  # Map chunk source_ids back to facts
  chunk_source_ids <- unique(chunks$source_id)
  scored <- vapply(
    facts,
    function(f) {
      sum(f$source_ids %in% chunk_source_ids)
    },
    integer(1)
  )

  # Also add keyword overlap as tiebreaker
  tokens <- unique(tolower(unlist(strsplit(tempest_trim(query), "\\s+"))))
  tokens <- tokens[nzchar(tokens)]
  keyword_scores <- vapply(
    facts,
    function(f) {
      claim <- tolower(f$claim %||% "")
      sum(vapply(tokens, function(t) grepl(t, claim, fixed = TRUE), logical(1)))
    },
    integer(1)
  )

  combined <- scored * 10L + keyword_scores
  ord <- order(combined, decreasing = TRUE)
  keep <- facts[ord]
  keep <- keep[combined[ord] > 0]
  keep[seq_len(min(length(keep), max_items))]
}

#' @keywords internal
tempest_type_query_decomposition <- function() {
  tempest_require("ellmer")
  ellmer::type_object(
    queries = ellmer::type_array(
      ellmer::type_string("A targeted search query")
    )
  )
}

#' Decompose a research question into targeted search queries
#'
#' @param chat An ellmer chat object.
#' @param question The research question to decompose.
#' @param topic The overall research topic.
#' @param module Optional dsprrr module for query decomposition.
#' @param max_queries Maximum number of queries to return.
#' @return A list with a `queries` character vector.
#' @keywords internal
tempest_decompose_query <- function(
  chat,
  question,
  topic,
  module = NULL,
  max_queries = 3
) {
  type <- tempest_type_query_decomposition()
  module_result <- tempest_run_dsprrr_module(
    module,
    chat,
    inputs = list(question = question, topic = topic),
    step = "query decomposition"
  )
  if (!is.null(module_result)) {
    return(tempest_normalize_query_decomposition(
      module_result,
      question,
      max_queries = max_queries
    ))
  }

  prompt <- paste0(
    "Topic: ",
    topic,
    "\n\n",
    "Research question: ",
    question,
    "\n\n",
    "Decompose this into 2-3 targeted search queries.\n",
    "Each query should be specific and cover a different aspect.\n",
    "Return structured data."
  )
  out <- chat$chat_structured(
    prompt,
    type = type,
    echo = "none",
    convert = FALSE
  )
  tempest_normalize_query_decomposition(
    out,
    question,
    max_queries = max_queries
  )
}

#' Check if dsprrr is available
#' @return Logical.
#' @keywords internal
tempest_has_dsprrr <- function() {
  tempest_has("dsprrr")
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
      paste0("- ", s$title %||% "Section", ": ", s$summary %||% "")
    }),
    collapse = "\n"
  )
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
      paste0("### ", s$title %||% "Subsection", "\n", bullet_md)
    }),
    collapse = "\n\n"
  )
}

#' @keywords internal
tempest_as_character_vector <- function(x) {
  if (is.null(x)) {
    return(character())
  }
  if (is.data.frame(x)) {
    x <- unlist(x, recursive = TRUE, use.names = FALSE)
  }
  if (is.list(x)) {
    x <- unlist(x, recursive = TRUE, use.names = FALSE)
  }
  x <- as.character(x)
  x <- tempest_trim(x)
  unique(x[nzchar(x) & !is.na(x)])
}

#' @keywords internal
tempest_normalize_query_decomposition <- function(
  x,
  fallback,
  max_queries = 4
) {
  queries <- if (is.list(x) && !is.null(x$queries)) x$queries else x
  queries <- tempest_as_character_vector(queries)
  if (length(queries) == 0) {
    queries <- fallback
  }
  max_queries <- as.integer(max_queries %||% 4L)
  if (is.na(max_queries) || max_queries < 1) {
    max_queries <- 4L
  }
  list(queries = queries[seq_len(min(length(queries), max_queries))])
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
        title = sub$title %||% "Subsection",
        bullets = tempest_as_character_vector(sub$bullets %||% character()),
        needed = tempest_as_character_vector(sub$needed %||% character())
      )
    })
    list(
      title = s$title %||% "Section",
      summary = s$summary %||% "",
      subsections = subsections
    )
  })
  list(
    title = x$title %||% fallback_title,
    sections = sections
  )
}

#' @keywords internal
tempest_normalize_fact_output <- function(x) {
  facts <- if (is.list(x) && !is.null(x$facts)) x$facts else x
  if (is.null(facts) || length(facts) == 0 || !is.list(facts)) {
    return(list())
  }
  if (is.data.frame(facts)) {
    facts <- split(facts, seq_len(nrow(facts)))
  }
  purrr::map(facts, function(f) {
    sources <- f$sources %||% f$source_ids %||% list()
    if (is.data.frame(sources)) {
      sources <- split(sources, seq_len(nrow(sources)))
    }
    if (is.character(sources)) {
      sources <- purrr::map(sources, ~ list(source_id = .x))
    }
    list(
      claim = f$claim %||% "",
      sources = sources,
      confidence = f$confidence %||% NA_character_,
      note = f$note %||% NA_character_
    )
  })
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
tempest_run_dsprrr_module <- function(module, chat, inputs, step) {
  if (is.null(module) || !tempest_has_dsprrr()) {
    return(NULL)
  }

  tryCatch(
    do.call(
      dsprrr::run,
      c(
        list(module = module),
        inputs,
        list(.llm = chat, .return_format = "simple", .progress = FALSE)
      )
    ),
    error = function(e) {
      tempest_warn(
        "dsprrr {step} failed, falling back to ellmer: {conditionMessage(e)}"
      )
      NULL
    }
  )
}

#' Create dsprrr modules for structured steps
#'
#' Creates dsprrr modules for STORM structured extraction/generation steps
#' when dsprrr is available. Returns NULL otherwise.
#'
#' @param config A `TempestConfig` object.
#' @return A named list of dsprrr modules, or NULL.
#' @keywords internal
tempest_make_dsprrr_modules <- function(config) {
  if (!tempest_has_dsprrr() || !tempest_has("ellmer")) {
    return(NULL)
  }

  tryCatch(
    {
      query_type <- tempest_type_query_decomposition()
      personas_type <- tempest_type_personas()
      perspectives_type <- tempest_type_perspectives()
      facts_type <- tempest_type_fact_extract()
      outline_type <- tempest_type_outline()

      modules <- list(
        perspectives = dsprrr::module(
          dsprrr::signature(
            inputs = list(
              dsprrr::input("topic", "string"),
              dsprrr::input("seed_context", "string"),
              dsprrr::input("n_experts", "integer")
            ),
            output_type = perspectives_type,
            instructions = paste(
              "Plan a comprehensive STORM research report.",
              "Use seed sources and table-of-contents hints to discover distinct perspectives.",
              "Return a title and n_experts to n_experts + 2 perspectives.",
              "Each perspective needs 3-6 specific research questions.",
              sep = "\n"
            )
          ),
          type = "predict"
        ),
        personas = dsprrr::module(
          dsprrr::signature(
            inputs = list(
              dsprrr::input("topic", "string"),
              dsprrr::input("n_experts", "integer"),
              dsprrr::input("requirements", "string")
            ),
            output_type = personas_type,
            instructions = paste(
              "Generate diverse expert personas for STORM multi-perspective research.",
              "The personas must be complementary and have non-overlapping focus areas.",
              "Return exactly n_experts personas.",
              sep = "\n"
            )
          ),
          type = "predict"
        ),
        query_decomposition = dsprrr::module(
          dsprrr::signature(
            inputs = list(
              dsprrr::input("question", "string"),
              dsprrr::input("topic", "string")
            ),
            output_type = query_type,
            instructions = paste(
              "Decompose the research question into 2-3 targeted web search queries.",
              "Queries should cover different aspects of the question and stay anchored to the topic.",
              sep = "\n"
            )
          ),
          type = "predict"
        ),
        fact_extraction = dsprrr::module(
          dsprrr::signature(
            inputs = list(dsprrr::input("answer_text", "string")),
            output_type = facts_type,
            instructions = paste(
              "Extract atomic factual claims from the answer.",
              "Only extract claims explicitly supported by citations like [Sxxxxxxxxxxxx].",
              "Do not infer or invent facts.",
              sep = "\n"
            )
          ),
          type = "predict"
        ),
        next_question = dsprrr::module(
          dsprrr::signature(
            inputs = list(
              dsprrr::input("topic", "string"),
              dsprrr::input("perspective", "string"),
              dsprrr::input("answered", "string"),
              dsprrr::input("facts", "string")
            ),
            output_type = tempest_type_next_question(),
            instructions = paste(
              "Choose the single most useful next question for this perspective.",
              "Set done to true only when the perspective is sufficiently covered.",
              sep = "\n"
            )
          ),
          type = "predict"
        ),
        draft_outline = dsprrr::module(
          dsprrr::signature(
            inputs = list(
              dsprrr::input("topic", "string"),
              dsprrr::input("title", "string")
            ),
            output_type = outline_type,
            instructions = paste(
              "Create a preliminary STORM outline from parametric knowledge.",
              "Organize into 4-6 sections with subsections and bullet points.",
              "The outline will later be refined using verified facts.",
              sep = "\n"
            )
          ),
          type = "predict"
        ),
        refined_outline = dsprrr::module(
          dsprrr::signature(
            inputs = list(
              dsprrr::input("topic", "string"),
              dsprrr::input("title", "string"),
              dsprrr::input("draft_outline", "string"),
              dsprrr::input("facts", "string")
            ),
            output_type = outline_type,
            instructions = paste(
              "Refine the draft outline using verified fact notes.",
              "Adjust, merge, add, or remove sections based on available evidence.",
              "Ensure sections are supportable by cited facts.",
              sep = "\n"
            )
          ),
          type = "predict"
        ),
        section_writing = dsprrr::module(
          dsprrr::signature(
            inputs = list(
              dsprrr::input("section_title", "string"),
              dsprrr::input("section_summary", "string"),
              dsprrr::input("subsections", "string"),
              dsprrr::input("facts", "string")
            ),
            output_type = ellmer::type_object(
              section_text = ellmer::type_string(
                "Markdown section text with citations"
              )
            ),
            instructions = paste(
              "Write one concise Markdown report section.",
              "Use only the provided verified facts.",
              "Every factual claim must include source citations like [Sxxxxxxxxxxxx].",
              sep = "\n"
            )
          ),
          type = "predict"
        ),
        lead_section = dsprrr::module(
          dsprrr::signature(
            inputs = list(
              dsprrr::input("topic", "string"),
              dsprrr::input("title", "string"),
              dsprrr::input("article_body", "string"),
              dsprrr::input("facts", "string")
            ),
            output_type = ellmer::type_object(
              lead_section = ellmer::type_string(
                "A 2-3 paragraph lead section with citations"
              )
            ),
            instructions = paste(
              "Write a Wikipedia-style lead section for the article.",
              "It must be self-contained, summarize the most important points, and preserve citations.",
              sep = "\n"
            )
          ),
          type = "predict"
        )
      )
      modules
    },
    error = function(e) {
      tempest_warn("Failed to create dsprrr modules: {conditionMessage(e)}")
      NULL
    }
  )
}

#' @keywords internal
tempest_default_dsprrr_teleprompter <- function(
  trainset,
  k = 4L,
  seed = 123L
) {
  tempest_require("dsprrr", "dsprrr optimization requires dsprrr.")
  k <- as.integer(min(k, nrow(trainset)))
  if (is.na(k) || k < 1L) {
    k <- 1L
  }
  dsprrr::LabeledFewShot(k = k, seed = as.integer(seed))
}

#' @keywords internal
tempest_call_dsprrr_teleprompter_factory <- function(
  factory,
  module_name,
  module,
  trainset
) {
  args <- list(
    module_name = module_name,
    name = module_name,
    module = module,
    trainset = trainset
  )
  fmls <- names(formals(factory))
  if ("..." %in% fmls) {
    return(do.call(factory, args))
  }
  do.call(factory, args[intersect(names(args), fmls)])
}

#' @keywords internal
tempest_select_dsprrr_teleprompter <- function(
  teleprompter,
  module_name,
  module,
  trainset,
  k,
  seed
) {
  if (is.null(teleprompter)) {
    return(tempest_default_dsprrr_teleprompter(
      trainset,
      k = k,
      seed = seed
    ))
  }

  if (is.function(teleprompter)) {
    return(tempest_call_dsprrr_teleprompter_factory(
      teleprompter,
      module_name = module_name,
      module = module,
      trainset = trainset
    ))
  }

  if (
    is.list(teleprompter) && !inherits(teleprompter, "dsprrr::Teleprompter")
  ) {
    selected <- teleprompter[[module_name]] %||% teleprompter$default
    if (is.null(selected)) {
      return(tempest_default_dsprrr_teleprompter(
        trainset,
        k = k,
        seed = seed
      ))
    }
    if (is.function(selected)) {
      return(tempest_call_dsprrr_teleprompter_factory(
        selected,
        module_name = module_name,
        module = module,
        trainset = trainset
      ))
    }
    return(selected)
  }

  teleprompter
}

#' @keywords internal
tempest_dsprrr_modules_path <- function(path) {
  if (!rlang::is_string(path)) {
    tempest_abort(
      "{.arg path} must be a single string, not {.obj_type_friendly {path}}."
    )
  }
  if (grepl("\\.rds$", path, ignore.case = TRUE)) {
    return(path)
  }
  file.path(path, "dsprrr-modules.rds")
}

#' Save compiled dsprrr modules
#'
#' @param modules A named list of dsprrr modules.
#' @param path File path ending in `.rds`, or a directory where
#'   `dsprrr-modules.rds` will be written.
#' @return Invisibly returns the RDS path.
#' @export
tempest_save_dsprrr_modules <- function(modules, path) {
  rds_path <- tempest_dsprrr_modules_path(path)
  dir.create(dirname(rds_path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(modules, rds_path)
  invisible(rds_path)
}

#' Load compiled dsprrr modules
#'
#' @param path File path ending in `.rds`, or a directory containing
#'   `dsprrr-modules.rds`.
#' @return A named list of dsprrr modules.
#' @export
tempest_load_dsprrr_modules <- function(path) {
  rds_path <- tempest_dsprrr_modules_path(path)
  if (!file.exists(rds_path)) {
    tempest_abort(
      "Compiled dsprrr module file does not exist: {.path {rds_path}}"
    )
  }
  readRDS(rds_path)
}

#' Optimize STORM dsprrr modules
#'
#' Compiles selected STORM dsprrr modules against user-provided training data.
#' This is an explicit optimization step: run it with labeled examples, then
#' pass the returned module list to [tempest_run()] via `dsprrr_modules`.
#'
#' @param trainsets Named list of training data frames. Names must match module
#'   names from `tempest_make_dsprrr_modules()`, such as
#'   `"query_decomposition"`, `"fact_extraction"`, `"section_writing"`, or
#'   `"lead_section"`.
#' @param modules Optional named list of modules to optimize. Defaults to fresh
#'   modules from `tempest_make_dsprrr_modules(config)`.
#' @param config A `TempestConfig` object used when `modules` is `NULL`.
#' @param teleprompter Optional dsprrr teleprompter, named list of
#'   teleprompters, or factory function. Defaults to
#'   `dsprrr::LabeledFewShot()`.
#' @param valsets Optional named list of validation data frames.
#' @param .llm Optional ellmer chat object passed to dsprrr compilation.
#' @param save_dir Optional directory or `.rds` path for the optimized module
#'   list.
#' @param k Number of examples for the default `LabeledFewShot` teleprompter.
#' @param seed Random seed for the default teleprompter.
#' @param strict If `TRUE`, abort on the first compile failure. If `FALSE`,
#'   warn and keep the unoptimized module for that step.
#' @param verbose If `TRUE`, print optimization progress.
#' @return A named list of dsprrr modules.
#' @export
tempest_optimize_dsprrr_modules <- function(
  trainsets,
  modules = NULL,
  config = tempest_config(),
  teleprompter = NULL,
  valsets = NULL,
  .llm = NULL,
  save_dir = NULL,
  k = 4L,
  seed = 123L,
  strict = TRUE,
  verbose = TRUE
) {
  tempest_require("dsprrr", "Optimizing STORM modules requires dsprrr.")

  if (!is.list(trainsets) || is.null(names(trainsets))) {
    tempest_abort("{.arg trainsets} must be a named list of data frames.")
  }

  modules <- modules %||% tempest_make_dsprrr_modules(config)
  if (is.null(modules) || !is.list(modules) || is.null(names(modules))) {
    tempest_abort("No dsprrr modules are available to optimize.")
  }

  optimized <- modules
  compiled <- list()

  for (module_name in names(trainsets)) {
    if (!module_name %in% names(modules)) {
      msg <- c(
        "No dsprrr module named {.val {module_name}}.",
        i = "Available modules: {.val {names(modules)}}"
      )
      if (isTRUE(strict)) {
        tempest_abort(msg)
      }
      tempest_warn(msg)
      next
    }

    trainset <- trainsets[[module_name]]
    if (!is.data.frame(trainset)) {
      trainset <- tryCatch(
        as.data.frame(trainset),
        error = function(e) {
          tempest_abort(c(
            "Trainset for {.val {module_name}} must be a data frame.",
            x = conditionMessage(e)
          ))
        }
      )
    }
    if (nrow(trainset) == 0L) {
      tempest_warn(
        "Skipping {.val {module_name}} optimization: empty trainset."
      )
      next
    }

    tp <- tempest_select_dsprrr_teleprompter(
      teleprompter,
      module_name = module_name,
      module = modules[[module_name]],
      trainset = trainset,
      k = k,
      seed = seed
    )
    valset <- valsets[[module_name]] %||% NULL

    if (isTRUE(verbose)) {
      tempest_inform(
        "Optimizing dsprrr module {.val {module_name}} with {nrow(trainset)} examples"
      )
    }

    optimized_module <- tryCatch(
      dsprrr::compile_module(
        modules[[module_name]],
        tp,
        trainset,
        valset = valset,
        .llm = .llm
      ),
      error = function(e) {
        if (isTRUE(strict)) {
          tempest_abort(c(
            "Failed to optimize dsprrr module {.val {module_name}}.",
            x = conditionMessage(e)
          ))
        }
        tempest_warn(
          "Failed to optimize dsprrr module {.val {module_name}}; keeping original module: {conditionMessage(e)}"
        )
        modules[[module_name]]
      }
    )

    optimized[[module_name]] <- optimized_module
    compiled[[module_name]] <- list(
      n_train = nrow(trainset),
      teleprompter = class(tp)[1] %||% NA_character_,
      compiled = inherits(optimized_module, "Module") &&
        isTRUE(optimized_module$is_compiled())
    )
  }

  attr(optimized, "tempest_dsprrr_optimization") <- compiled
  if (!is.null(save_dir)) {
    tempfile_path <- tempest_save_dsprrr_modules(optimized, save_dir)
    attr(optimized, "tempest_dsprrr_modules_path") <- tempfile_path
  }

  optimized
}

#' @keywords internal
tempest_extract_facts_from_answer <- function(
  chat,
  answer_text,
  store,
  module = NULL
) {
  # Use a separate extraction call to minimize hallucinated facts.
  # We instruct the model to ONLY extract claims that are explicitly supported
  # by citations like [Sxxxxxxxxxxxx] in the answer.
  type <- tempest_type_fact_extract()
  out <- tempest_run_dsprrr_module(
    module,
    chat,
    inputs = list(answer_text = answer_text),
    step = "fact extraction"
  )
  if (is.null(out)) {
    prompt <- paste0(
      "Extract atomic factual claims from the following answer.\n\n",
      "Rules:\n",
      "- Only extract claims that are explicitly supported by one or more citations in the form [Sxxxxxxxxxxxx].\n",
      "- For each claim, list the source_id(s) that support it.\n",
      "- Do NOT invent or infer new facts.\n\n",
      "<answer>\n",
      answer_text,
      "\n</answer>\n"
    )
    out <- chat$chat_structured(
      prompt,
      type = type,
      echo = "none",
      convert = FALSE
    )
  }
  facts <- tempest_normalize_fact_output(out)
  if (length(facts) == 0) {
    return(invisible(NULL))
  }
  for (f in facts) {
    claim <- tempest_trim(f$claim %||% "")
    if (is.na(claim) || !nzchar(claim)) {
      next
    }
    src_ids <- purrr::map_chr(
      f$sources %||% list(),
      ~ .x$source_id %||% NA_character_
    )
    src_ids <- unique(src_ids[nzchar(src_ids) & !is.na(src_ids)])
    if (length(src_ids) == 0) {
      next
    }
    store$add_fact(tempest_fact(
      claim = claim,
      source_ids = src_ids,
      confidence = f$confidence %||% NA_character_,
      note = f$note %||% NA_character_
    ))
  }
  invisible(TRUE)
}

#' Run research in parallel using mirai
#' @keywords internal
tempest_research_parallel <- function(
  perspectives,
  personas,
  config,
  retriever,
  store,
  topic,
  research_strategy,
  max_rounds,
  max_questions_per_perspective,
  dsprrr_modules = NULL,
  verbose
) {
  tempest_require("mirai", "Parallel research requires the mirai package.")

  # Each perspective gets its own isolated SourceStore + retriever
  results <- mirai::mirai_map(
    seq_along(perspectives),
    function(
      i,
      perspectives,
      personas,
      config,
      topic,
      research_strategy,
      max_rounds,
      max_questions_per_perspective
    ) {
      p <- perspectives[[i]]
      persona <- if (i <= length(personas)) personas[[i]] else NULL

      # Create isolated store and retriever
      local_store <- tempest::SourceStore$new()
      local_retriever <- tempest::tempest_retriever(
        config = config,
        store = local_store
      )

      sp <- tempest_render_expert_prompt(
        persona = persona,
        expert_id = i
      )
      expert <- config$make_chat("expert", system_prompt = sp, echo = "none")
      tempest_register_default_tools(
        expert,
        local_retriever,
        model = config$models[["expert"]],
        search_provider = config$search_provider
      )
      writer <- config$make_chat("writer", echo = "none")
      extractor <- config$make_chat(
        "judge",
        system_prompt = tempest_prompt("fact_extractor_system"),
        echo = "none"
      )
      modules <- dsprrr_modules %||% tempest_make_dsprrr_modules(config)

      p_name <- p$name %||% "Perspective"
      p_desc <- p$description %||% ""
      qs <- p$key_questions %||% c(topic)

      if (identical(research_strategy, "key_questions")) {
        qs_limited <- utils::head(qs, max_questions_per_perspective)
        for (q in qs_limited) {
          decomposed <- tryCatch(
            tempest_decompose_query(
              writer,
              q,
              topic,
              module = modules$query_decomposition,
              max_queries = config$max_search_queries_per_turn
            ),
            error = function(e) {
              list(queries = list(q))
            }
          )
          search_instructions <- paste0(
            "Suggested search queries:\n",
            paste0("- ", decomposed$queries %||% q, collapse = "\n"),
            "\n\n"
          )

          prompt <- paste0(
            "Perspective: ",
            p_name,
            "\n",
            "Description: ",
            p_desc,
            "\n\n",
            "Question: ",
            q,
            "\n\n",
            search_instructions,
            "Instructions:\n",
            "- Use web_search + fetch_url as needed.\n",
            "- Only state factual claims that are supported by sources you fetched.\n",
            "- For each factual sentence, add one or more citations like [Sxxxxxxxxxxxx].\n",
            "- If evidence is weak or unclear, say so and do not overclaim.\n\n",
            "Answer:"
          )
          ans <- expert$chat(prompt, echo = "none")
          tempest_extract_facts_from_answer(
            extractor,
            ans,
            local_store,
            module = modules$fact_extraction
          )
        }
      }
      list(
        sources = local_store$list_sources(),
        facts = local_store$list_facts()
      )
    },
    .args = list(
      perspectives = perspectives,
      personas = personas,
      config = config,
      topic = topic,
      research_strategy = research_strategy,
      max_rounds = max_rounds,
      max_questions_per_perspective = max_questions_per_perspective,
      dsprrr_modules = dsprrr_modules
    )
  )

  # Merge results back into main store
  for (i in seq_along(results)) {
    val <- tryCatch(results[[i]]$data, error = function(e) NULL)
    if (inherits(val, "errorValue") || is.null(val)) {
      p_name <- perspectives[[i]]$name %||% paste("Perspective", i)
      tempest_warn("Parallel research failed for {.val {p_name}}")
      next
    }
    for (src in val$sources %||% list()) {
      store$upsert_source(src)
    }
    for (fact in val$facts %||% list()) {
      store$add_fact(fact)
    }
  }

  invisible(TRUE)
}

#' Run the STORM pipeline
#'
#' This is a scripted workflow that:
#' 1) discovers perspectives and research questions,
#' 2) runs a multi-perspective research loop (search/fetch + expert synthesis),
#' 3) creates an outline,
#' 4) writes a cited report in Markdown.
#'
#' @param topic Research topic or question.
#' @param config A `TempestConfig`.
#' @param retriever Optional `TempestRetriever`. If `NULL`, created from `config`.
#' @param n_experts Number of expert personas to use (default 3).
#' @param research_strategy Either "key_questions" (default, faster) or "conversation"
#'   (more thorough but slower). Key questions uses predefined questions; conversation
#'   dynamically generates follow-up questions.
#' @param max_rounds Maximum rounds per perspective for "conversation" strategy (default 3).
#' @param max_questions_per_perspective Maximum questions per perspective for "key_questions"
#'   strategy (default 3).
#' @param parallel_research If `TRUE`, run research perspectives in parallel using
#'   the mirai package. Requires mirai to be installed. Default `FALSE`.
#' @param parallel_writing If `TRUE`, write report sections in parallel using
#'   the mirai package. Failed parallel sections are retried sequentially.
#' @param dsprrr_modules Optional named list of dsprrr modules, typically from
#'   [tempest_optimize_dsprrr_modules()]. If `NULL`, fresh modules are created
#'   when dsprrr is installed.
#' @param steps Character vector controlling which steps to run. Defaults to all.
#' @param output_dir Optional directory for persisted STORM run artifacts. When
#'   supplied, artifacts are written under a topic-specific subdirectory.
#' @param resume If `TRUE` and `output_dir` contains a previous run, load saved
#'   artifacts and skip stages recorded as complete.
#' @param run_id Optional run directory name. Defaults to a slug of `topic`.
#' @param remove_duplicate If `TRUE`, ask the polish step to remove duplicate
#'   or highly repetitive content while preserving unique cited claims.
#' @param verbose If `TRUE`, prints progress messages.
#' @return A list with `title`, `outline`, `draft_md`, `report_md`, `store`,
#'   and `output_dir`.
#' @export
tempest_run <- function(
  topic,
  config = tempest_config(),
  retriever = NULL,
  n_experts = 3,
  research_strategy = c("key_questions", "conversation"),
  max_rounds = 3,
  max_questions_per_perspective = 3,
  parallel_research = FALSE,
  parallel_writing = FALSE,
  dsprrr_modules = NULL,
  steps = c("perspectives", "research", "outline", "write", "polish"),
  output_dir = NULL,
  resume = FALSE,
  run_id = NULL,
  remove_duplicate = FALSE,
  verbose = TRUE
) {
  tempest_require("ellmer", "tempest_run() requires ellmer.")
  topic <- tempest_trim(topic)
  if (is.na(topic) || topic == "") {
    tempest_abort("topic must be a non-empty string.")
  }

  research_strategy <- match.arg(research_strategy)
  max_rounds <- as.integer(max_rounds %||% 6)
  if (is.na(max_rounds) || max_rounds < 1) {
    max_rounds <- 6L
  }

  retriever <- retriever %||%
    tempest_retriever(config = config, store = SourceStore$new())
  store <- retriever$store
  run_dir <- tempest_prepare_run_dir(output_dir, topic, run_id = run_id)
  completed_stages <- character()
  if (!is.null(run_dir) && isTRUE(resume)) {
    loaded_run <- tempest_load_run_artifacts(run_dir, store)
    completed_stages <- loaded_run$completed_stages
    if (verbose && length(completed_stages) > 0) {
      tempest_inform(
        "Loaded persisted STORM stages from {.path {run_dir}}: {paste(completed_stages, collapse = ', ')}"
      )
    }
  }

  coordinator <- config$make_chat(
    "coordinator",
    echo = if (verbose) "output" else "none"
  )
  writer <- config$make_chat("writer", echo = if (verbose) "output" else "none")
  polisher <- config$make_chat(
    "writer",
    system_prompt = tempest_prompt("polisher_system"),
    echo = if (verbose) "output" else "none"
  )
  extractor <- config$make_chat(
    "judge",
    system_prompt = tempest_prompt("fact_extractor_system"),
    echo = "none"
  )
  dsprrr_modules <- dsprrr_modules %||% tempest_make_dsprrr_modules(config)

  tempest_register_default_tools(
    writer,
    retriever,
    model = config$models[["writer"]],
    search_provider = config$search_provider
  )

  title <- store$get_artifact("title") %||% topic
  perspectives <- NULL
  personas <- NULL
  expert_chats <- list() # Created per-perspective
  outline <- NULL
  draft_md <- NULL
  report_md <- NULL

  if (
    "perspectives" %in%
      steps &&
      !tempest_stage_complete(completed_stages, "perspectives")
  ) {
    if (verbose) {
      tempest_inform("Discovering perspectives for: {.val {topic}}")
    }
    seed <- retriever$search(topic, k = min(5, config$max_search_results))
    seed_txt <- paste0(
      "Seed sources:\n",
      paste0(
        "- ",
        seed$title,
        " (",
        seed$source_id,
        ") ",
        seed$url,
        collapse = "\n"
      )
    )

    # Enrich perspective discovery with ToC headings from top seed URLs
    toc_lines <- character()
    seed_urls <- head(seed$url, 3)
    for (u in seed_urls) {
      # Try Wikipedia sections first, then general ToC
      wiki_title <- sub("^https://en.wikipedia.org/wiki/", "", u)
      if (grepl("^https://en.wikipedia.org/", u)) {
        toc <- tempest_wiki_page_sections(gsub("_", " ", wiki_title))
      } else {
        toc <- tempest_extract_toc_from_url(u)
      }
      if (length(toc) > 0) {
        toc_lines <- c(toc_lines, paste0("\nToC from ", u, ":"), toc)
      }
    }
    if (length(toc_lines) > 0) {
      seed_txt <- paste0(seed_txt, "\n\n", paste(toc_lines, collapse = "\n"))
    }

    plan <- tempest_generate_perspectives(
      coordinator,
      topic,
      seed_txt,
      n_experts,
      module = dsprrr_modules$perspectives
    )
    title <- plan$title %||% topic
    perspectives <- plan$perspectives %||% list()
    store$set_artifact("perspectives", perspectives)
    store$set_artifact("title", title)

    # Generate personas aligned with perspectives
    if (verbose) {
      tempest_inform("Generating {n_experts} expert personas")
    }
    personas <- tempest_generate_personas(
      topic = topic,
      n = n_experts,
      config = config,
      verbose = verbose,
      module = dsprrr_modules$personas
    )
    store$set_artifact("personas", personas)
    completed_stages <- tempest_mark_stage_complete(
      completed_stages,
      "perspectives"
    )
    tempest_save_run_artifacts(
      run_dir,
      store,
      topic = topic,
      title = title,
      config = config,
      completed_stages = completed_stages,
      steps = steps,
      research_strategy = research_strategy,
      parallel_writing = parallel_writing,
      remove_duplicate = remove_duplicate
    )
  } else {
    if (
      verbose &&
        "perspectives" %in% steps &&
        tempest_stage_complete(completed_stages, "perspectives")
    ) {
      tempest_inform("Using persisted perspectives from {.path {run_dir}}")
    }
    title <- store$get_artifact("title") %||% title
    perspectives <- store$get_artifact("perspectives") %||% list()
    personas <- store$get_artifact("personas") %||% list()
  }

  if (
    "research" %in%
      steps &&
      !tempest_stage_complete(completed_stages, "research")
  ) {
    if (verbose) {
      tempest_inform("Research loop: {length(perspectives)} perspectives")
    }

    if (length(perspectives) == 0) {
      # Fallback: single perspective
      perspectives <- list(list(
        name = "Overview",
        description = "General overview",
        key_questions = c(topic)
      ))
    }

    if (
      isTRUE(parallel_research) &&
        tempest_has("mirai") &&
        identical(research_strategy, "key_questions")
    ) {
      if (verbose) {
        tempest_inform(
          "Running research in parallel ({length(perspectives)} perspectives)"
        )
      }
      tempest_research_parallel(
        perspectives,
        personas,
        config,
        retriever,
        store,
        topic,
        research_strategy,
        max_rounds,
        max_questions_per_perspective,
        dsprrr_modules,
        verbose
      )
    } else {
      # Sequential research loop
      # Create expert chats with personas (one per perspective)
      for (i in seq_along(perspectives)) {
        persona <- if (i <= length(personas)) personas[[i]] else NULL
        sp <- tempest_render_expert_prompt(persona = persona, expert_id = i)
        expert_chats[[i]] <- config$make_chat(
          "expert",
          system_prompt = sp,
          echo = if (verbose) "output" else "none"
        )
        tempest_register_default_tools(
          expert_chats[[i]],
          retriever,
          model = config$models[["expert"]],
          search_provider = config$search_provider
        )
      }

      for (i in seq_along(perspectives)) {
        p <- perspectives[[i]]
        p_name <- p$name %||% "Perspective"
        p_desc <- p$description %||% ""
        qs <- p$key_questions %||% c(topic)

        expert <- expert_chats[[i]]
        persona <- if (i <= length(personas)) personas[[i]] else NULL
        persona_name <- persona$name %||% paste("Expert", i)

        if (verbose) {
          tempest_inform("Perspective: {.val {p_name}}")
          if (!is.null(persona)) {
            tempest_inform(
              "  Expert: {persona_name} ({persona$title %||% 'Research Specialist'})"
            )
          }
        }

        if (identical(research_strategy, "key_questions")) {
          # Ask expert to answer each planned key question (limited)
          qs_limited <- head(qs, max_questions_per_perspective)
          if (verbose && length(qs) > length(qs_limited)) {
            tempest_inform(
              "  Limiting to {length(qs_limited)} of {length(qs)} questions"
            )
          }
          for (q in qs_limited) {
            # Decompose query into targeted search queries
            decomposed <- tryCatch(
              tempest_decompose_query(
                writer,
                q,
                topic,
                module = dsprrr_modules$query_decomposition,
                max_queries = config$max_search_queries_per_turn
              ),
              error = function(e) {
                tempest_warn(
                  "Query decomposition failed, using original query: {conditionMessage(e)}"
                )
                list(queries = list(q))
              }
            )
            search_instructions <- paste0(
              "Suggested search queries:\n",
              paste0("- ", decomposed$queries %||% q, collapse = "\n"),
              "\n\n"
            )

            prompt <- paste0(
              "Perspective: ",
              p_name,
              "\n",
              "Description: ",
              p_desc,
              "\n\n",
              "Question: ",
              q,
              "\n\n",
              search_instructions,
              "Instructions:\n",
              "- Use web_search + fetch_url as needed.\n",
              "- Only state factual claims that are supported by sources you fetched.\n",
              "- For each factual sentence, add one or more citations like [Sxxxxxxxxxxxx].\n",
              "- If evidence is weak or unclear, say so and do not overclaim.\n\n",
              "Answer:"
            )
            ans <- expert$chat(prompt, echo = if (verbose) "output" else "none")
            tempest_extract_facts_from_answer(
              extractor,
              ans,
              store,
              module = dsprrr_modules$fact_extraction
            )
          }
        } else {
          # Conversation-style interviewing: writer proposes the next best question,
          # expert answers with citations; repeat until done or max_rounds.
          answered <- character()
          for (round in seq_len(max_rounds)) {
            answered_md <- if (length(answered) == 0) {
              "(none yet)"
            } else {
              paste0("- ", answered, collapse = "\n")
            }
            facts_md <- tempest_summarize_facts_for_prompt(
              store,
              max_items = 60
            )

            nxt <- tempest_generate_next_question(
              writer,
              topic,
              p,
              answered_md = answered_md,
              facts_md = facts_md,
              module = dsprrr_modules$next_question
            )
            q <- tempest_trim(nxt$question %||% "")
            done <- isTRUE(nxt$done)

            if (is.na(q) || q == "") {
              break
            }

            # Decompose query into targeted search queries
            decomposed <- tryCatch(
              tempest_decompose_query(
                writer,
                q,
                topic,
                module = dsprrr_modules$query_decomposition,
                max_queries = config$max_search_queries_per_turn
              ),
              error = function(e) {
                tempest_warn(
                  "Query decomposition failed, using original query: {conditionMessage(e)}"
                )
                list(queries = list(q))
              }
            )
            search_instructions <- paste0(
              "Suggested search queries:\n",
              paste0("- ", decomposed$queries %||% q, collapse = "\n"),
              "\n\n"
            )

            prompt <- paste0(
              "Perspective: ",
              p_name,
              "\n",
              "Description: ",
              p_desc,
              "\n\n",
              "Question: ",
              q,
              "\n\n",
              search_instructions,
              "Instructions:\n",
              "- Use web_search + fetch_url as needed.\n",
              "- Only state factual claims that are supported by sources you fetched.\n",
              "- For each factual sentence, add one or more citations like [Sxxxxxxxxxxxx].\n",
              "- If evidence is weak or unclear, say so and do not overclaim.\n\n",
              "Answer:"
            )

            ans <- expert$chat(prompt, echo = if (verbose) "output" else "none")
            answered <- c(answered, paste0("Q: ", q, "\nA: ", ans))
            tempest_extract_facts_from_answer(
              extractor,
              ans,
              store,
              module = dsprrr_modules$fact_extraction
            )

            if (done) break
          }
        }
      }
    }

    store$set_artifact("facts", store$list_facts())
    completed_stages <- tempest_mark_stage_complete(
      completed_stages,
      "research"
    )
    tempest_save_run_artifacts(
      run_dir,
      store,
      topic = topic,
      title = title,
      config = config,
      completed_stages = completed_stages,
      steps = steps,
      research_strategy = research_strategy,
      parallel_writing = parallel_writing,
      remove_duplicate = remove_duplicate
    )
  } else if (
    verbose &&
      "research" %in% steps &&
      tempest_stage_complete(completed_stages, "research")
  ) {
    tempest_inform("Using persisted research artifacts from {.path {run_dir}}")
  }

  if (
    "outline" %in% steps && !tempest_stage_complete(completed_stages, "outline")
  ) {
    if (verbose) {
      tempest_inform("Generating outline (two-step)")
    }

    # Step 1: Draft outline from LLM knowledge alone
    draft_outline <- tempest_draft_outline(
      writer,
      topic,
      title,
      module = dsprrr_modules$draft_outline
    )
    store$set_artifact("draft_outline", draft_outline)

    # Step 2: Refined outline incorporating facts
    facts_txt <- tempest_summarize_facts_for_prompt(store, max_items = 80)
    outline <- tempest_refine_outline(
      writer,
      topic,
      title,
      draft_outline,
      facts_txt,
      module = dsprrr_modules$refined_outline
    )
    store$set_artifact("outline", outline)
    completed_stages <- tempest_mark_stage_complete(completed_stages, "outline")
    tempest_save_run_artifacts(
      run_dir,
      store,
      topic = topic,
      title = title,
      config = config,
      completed_stages = completed_stages,
      steps = steps,
      research_strategy = research_strategy,
      parallel_writing = parallel_writing,
      remove_duplicate = remove_duplicate
    )
  } else {
    if (
      verbose &&
        "outline" %in% steps &&
        tempest_stage_complete(completed_stages, "outline")
    ) {
      tempest_inform("Using persisted outline from {.path {run_dir}}")
    }
    outline <- store$get_artifact("outline")
  }

  if (
    "write" %in% steps && !tempest_stage_complete(completed_stages, "write")
  ) {
    if (verbose) {
      tempest_inform("Writing draft")
    }
    if (is.null(outline) || is.null(outline$sections)) {
      tempest_abort("No outline available; run steps including 'outline'.")
    }

    section_jobs <- tempest_section_jobs(
      outline,
      retriever,
      store,
      retrieve_top_k = config$retrieve_top_k
    )
    section_results <- tempest_write_section_jobs(
      section_jobs,
      writer,
      config = config,
      dsprrr_modules = dsprrr_modules,
      parallel = parallel_writing,
      verbose = verbose
    )
    parts <- purrr::map_chr(section_results, "markdown")

    for (section_result in section_results) {
      # Extract any newly-cited facts from the section itself
      tempest_extract_facts_from_answer(
        extractor,
        section_result$section_text,
        store,
        module = dsprrr_modules$fact_extraction
      )
    }

    draft_md <- paste(parts, collapse = "\n\n")

    # Generate Wikipedia-style lead section
    if (verbose) {
      tempest_inform("Generating lead section")
    }
    lead_facts <- tempest_summarize_facts_for_prompt(store, max_items = 40)
    lead_section <- tempest_write_lead_section(
      writer,
      topic,
      title,
      draft_md,
      facts_txt = lead_facts,
      module = dsprrr_modules$lead_section,
      verbose = verbose
    )
    draft_md <- paste0(lead_section, "\n\n", draft_md)
    store$set_artifact("lead_section", lead_section)

    store$set_artifact("draft_md", draft_md)
    completed_stages <- tempest_mark_stage_complete(completed_stages, "write")
    tempest_save_run_artifacts(
      run_dir,
      store,
      topic = topic,
      title = title,
      config = config,
      completed_stages = completed_stages,
      steps = steps,
      research_strategy = research_strategy,
      parallel_writing = parallel_writing,
      remove_duplicate = remove_duplicate
    )
  } else {
    if (
      verbose &&
        "write" %in% steps &&
        tempest_stage_complete(completed_stages, "write")
    ) {
      tempest_inform("Using persisted draft article from {.path {run_dir}}")
    }
    draft_md <- store$get_artifact("draft_md")
  }

  if (
    "polish" %in% steps && !tempest_stage_complete(completed_stages, "polish")
  ) {
    if (verbose) {
      tempest_inform("Polishing and consistency pass")
    }
    prompt <- paste0(
      "Polish the following Markdown report.\n\n",
      "Rules:\n",
      tempest_polish_rules(remove_duplicate = remove_duplicate),
      "\n\n",
      "<draft>\n",
      draft_md,
      "\n</draft>\n"
    )
    polished <- polisher$chat(prompt, echo = if (verbose) "output" else "none")
    report_md <- tempest_report_md(
      title = title,
      body = polished,
      store = store
    )
    store$set_artifact("report_md", report_md)
    completed_stages <- tempest_mark_stage_complete(completed_stages, "polish")
    tempest_save_run_artifacts(
      run_dir,
      store,
      topic = topic,
      title = title,
      config = config,
      completed_stages = completed_stages,
      steps = steps,
      research_strategy = research_strategy,
      parallel_writing = parallel_writing,
      remove_duplicate = remove_duplicate
    )
  } else {
    if (
      verbose &&
        "polish" %in% steps &&
        tempest_stage_complete(completed_stages, "polish")
    ) {
      tempest_inform("Using persisted polished report from {.path {run_dir}}")
    }
    report_md <- store$get_artifact("report_md")
  }

  list(
    title = title,
    perspectives = perspectives,
    personas = personas,
    outline = outline,
    draft_md = draft_md,
    report_md = report_md,
    store = store,
    retriever = retriever,
    output_dir = run_dir
  )
}

#' Run STORM asynchronously (Shiny-friendly)
#'
#' This is a thin wrapper around [tempest_run()] that returns a promise.
#' It is useful when calling STORM from a Shiny app without blocking the session.
#'
#' @param ... Arguments passed to [tempest_run()]. See [tempest_run()] for details
#'   on available parameters including `topic`, `config`, `retriever`,
#'   `n_experts`, `research_strategy`, `max_rounds`, `steps`, and `verbose`.
#' @return A `promises::promise` that resolves with the tempest_run result.
#' @seealso [tempest_run()] for the synchronous version.
#' @export
tempest_run_async <- function(...) {
  tempest_require("promises", "tempest_run_async() uses promises.")
  promises::promise(function(resolve, reject) {
    tryCatch(resolve(tempest_run(...)), error = reject)
  })
}
