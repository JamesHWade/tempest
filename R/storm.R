# STORM pipeline (scripted)

#' @keywords internal
storm_type_perspectives <- function() {
  stormr_require("ellmer")
  ellmer::type_object(
    title = ellmer::type_string("Suggested title for the final report."),
    perspectives = ellmer::type_array(
      ellmer::type_object(
        name = ellmer::type_string("Perspective name (short)"),
        description = ellmer::type_string("What this perspective covers"),
        key_questions = ellmer::type_array(ellmer::type_string("A research question"))
      )
    )
  )
}

#' @keywords internal
storm_type_personas <- function() {
  stormr_require("ellmer")
  ellmer::type_object(
    personas = ellmer::type_array(
      ellmer::type_object(
        name = ellmer::type_string("Full name of the persona (e.g., 'Dr. Sarah Chen')"),
        title = ellmer::type_string("Professional title or role (e.g., 'Climate Scientist')"),
        affiliation = ellmer::type_string("Organization or institution (e.g., 'Arctic Research Institute')"),
        background = ellmer::type_string("2-3 sentence background: expertise, experience, what shaped their perspective"),
        focus_areas = ellmer::type_array(ellmer::type_string("Specific focus area or subtopic")),
        perspective = ellmer::type_string("How this persona uniquely approaches the topic - their angle or lens"),
        initial_questions = ellmer::type_array(ellmer::type_string("Questions this persona would naturally ask"))
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
#' @param config A `StormConfig` object.
#' @param verbose Print progress.
#' @return A list of persona objects.
#'
#' @export
storm_generate_personas <- function(topic, n = 3, config = storm_config(), verbose = FALSE) {
  stormr_require("ellmer", "Persona generation requires ellmer.")

  chat <- config$make_chat(
    "coordinator",
    system_prompt = storm_prompt("persona_generator_system"),
    echo = "none"
  )

  prompt <- paste0(
    "Topic: ", topic, "\n\n",
    "Generate exactly ", n, " diverse expert personas who would research this topic.\n\n",
    "Requirements:\n",
    "- Each persona should have a distinct professional background\n",
    "- Personas should complement each other, covering different angles\n",
    "- Include a mix of academic, industry, and practitioner perspectives where appropriate\n",
    "- Each persona's focus areas should be specific and non-overlapping\n",
    "- Initial questions should reflect their unique expertise and concerns\n"
  )

  if (verbose) stormr_inform("Generating {n} expert personas for: {.val {topic}}")

  result <- chat$chat_structured(
    prompt,
    type = storm_type_personas(),
    echo = "none",
    convert = FALSE
  )

  personas <- result$personas %||% list()

 # Add an id to each persona
  for (i in seq_along(personas)) {
    personas[[i]]$id <- i
  }

  if (verbose) {
    for (p in personas) {
      stormr_inform("  - {p$name}, {p$title}")
    }
  }

  personas
}

#' Format Persona Details for Prompt
#'
#' @param persona A persona object from `storm_generate_personas()`.
#' @return A formatted string with persona details.
#' @keywords internal
storm_format_persona_details <- function(persona) {
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
storm_render_expert_prompt <- function(persona = NULL, expert_id = 1) {
  if (is.null(persona)) {
    # Fallback to generic expert
    storm_prompt_render(
      "expert_system",
      persona_name = paste("Expert", expert_id),
      persona_title = "Research Specialist",
      persona_details = ""
    )
  } else {
    storm_prompt_render(
      "expert_system",
      persona_name = persona$name %||% paste("Expert", expert_id),
      persona_title = persona$title %||% "Research Specialist",
      persona_details = storm_format_persona_details(persona)
    )
  }
}

#' @keywords internal
storm_type_outline <- function() {
  stormr_require("ellmer")
  subsection <- ellmer::type_object(
    title = ellmer::type_string("Subsection title"),
    bullets = ellmer::type_array(ellmer::type_string("Bullet: a specific point to cover")),
    needed = ellmer::type_array(ellmer::type_string("If needed: unanswered question"), required = FALSE)
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
storm_type_fact_extract <- function() {
  stormr_require("ellmer")
  src <- ellmer::type_object(
    source_id = ellmer::type_string("Source id like Sxxxxxxxxxxxx"),
    url = ellmer::type_string("URL if available", required = FALSE),
    quote = ellmer::type_string("Short supporting quote (<=20 words) if available", required = FALSE)
  )
  fact <- ellmer::type_object(
    claim = ellmer::type_string("Atomic factual claim"),
    sources = ellmer::type_array(src),
    confidence = ellmer::type_enum(c("low", "medium", "high"), "Confidence", required = FALSE),
    note = ellmer::type_string("Nuance, scope conditions, or caveats.", required = FALSE)
  )
  ellmer::type_object(facts = ellmer::type_array(fact))
}

#' @keywords internal
storm_type_next_question <- function() {
  stormr_require("ellmer")
  ellmer::type_object(
    question = ellmer::type_string("Next research question to ask the expert."),
    done = ellmer::type_boolean("Whether research for this perspective is complete.", required = FALSE)
  )
}

#' @keywords internal
storm_generate_next_question <- function(writer_chat, topic, perspective, answered_md, facts_md) {
  type <- storm_type_next_question()
  prompt <- paste0(
    "You are interviewing an expert to build a factual knowledge base.\n\n",
    "Topic: ", topic, "\n",
    "Perspective: ", (perspective$name %||% ""), "\n",
    "Description: ", (perspective$description %||% ""), "\n\n",
    "Answered Q&A so far:\n", answered_md, "\n\n",
    "Current fact notes summary:\n", facts_md, "\n\n",
    "Propose the single most useful next question to ask.\n",
    "If this perspective is sufficiently covered, set done = true.\n",
    "Return structured data."
  )
  writer_chat$chat_structured(prompt, type = type, echo = "none", convert = FALSE)
}

storm_summarize_facts_for_prompt <- function(store, max_items = 60) {
  stopifnot(inherits(store, "SourceStore"))
  facts <- store$list_facts()
  if (length(facts) == 0) return("(no facts yet)")
  facts <- facts[seq_len(min(length(facts), max_items))]
  lines <- purrr::map_chr(facts, function(f) {
    cites <- paste0("[", paste(f$source_ids, collapse = ", "), "]")
    glue::glue("- {f$claim} {cites}")
  })
  paste(lines, collapse = "\n")
}

#' @keywords internal
storm_keyword_filter_facts <- function(store, query, max_items = 30) {
  facts <- store$list_facts()
  if (length(facts) == 0) return(list())

  tokens <- unique(tolower(unlist(strsplit(stormr_trim(query), "\\s+"))))
  tokens <- tokens[nzchar(tokens)]
  if (length(tokens) == 0) return(list())

  scored <- vapply(facts, function(f) {
    claim <- tolower(f$claim %||% "")
    sum(vapply(tokens, function(t) grepl(t, claim, fixed = TRUE), logical(1)))
  }, integer(1))

  ord <- order(scored, decreasing = TRUE)
  keep <- facts[ord]
  keep <- keep[scored[ord] > 0]
  keep[seq_len(min(length(keep), max_items))]
}


#' @keywords internal
storm_extract_facts_from_answer <- function(chat, answer_text, store) {
  # Use a separate extraction call to minimize hallucinated facts.
  # We instruct the model to ONLY extract claims that are explicitly supported
  # by citations like [Sxxxxxxxxxxxx] in the answer.
  type <- storm_type_fact_extract()
  prompt <- paste0(
    "Extract atomic factual claims from the following answer.\n\n",
    "Rules:\n",
    "- Only extract claims that are explicitly supported by one or more citations in the form [Sxxxxxxxxxxxx].\n",
    "- For each claim, list the source_id(s) that support it.\n",
    "- Do NOT invent or infer new facts.\n\n",
    "<answer>\n", answer_text, "\n</answer>\n"
  )
  out <- chat$chat_structured(prompt, type = type, echo = "none", convert = FALSE)
  facts <- out$facts %||% list()
  if (length(facts) == 0) return(invisible(NULL))
  for (f in facts) {
    src_ids <- purrr::map_chr(f$sources %||% list(), "source_id")
    if (length(src_ids) == 0) next
    store$add_fact(storm_fact(
      claim = f$claim,
      source_ids = src_ids,
      confidence = f$confidence %||% NA_character_,
      note = f$note %||% NA_character_
    ))
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
#' @param config A `StormConfig`.
#' @param retriever Optional `StormRetriever`. If `NULL`, created from `config`.
#' @param n_experts Number of expert personas to use (default 3).
#' @param research_strategy Either "key_questions" (default, faster) or "conversation"
#'   (more thorough but slower). Key questions uses predefined questions; conversation
#'   dynamically generates follow-up questions.
#' @param max_rounds Maximum rounds per perspective for "conversation" strategy (default 3).
#' @param max_questions_per_perspective Maximum questions per perspective for "key_questions"
#'   strategy (default 3).
#' @param steps Character vector controlling which steps to run. Defaults to all.
#' @param verbose If `TRUE`, prints progress messages.
#' @return A list with `title`, `outline`, `draft_md`, `report_md`, and `store`.
#' @export
storm_run <- function(
  topic,
  config = storm_config(),
  retriever = NULL,
  n_experts = 3,
  research_strategy = c("key_questions", "conversation"),
  max_rounds = 3,
  max_questions_per_perspective = 3,
  steps = c("perspectives", "research", "outline", "write", "polish"),
  verbose = TRUE
) {
  stormr_require("ellmer", "storm_run() requires ellmer.")
  topic <- stormr_trim(topic)
  if (is.na(topic) || topic == "") stormr_abort("topic must be a non-empty string.")

  research_strategy <- match.arg(research_strategy)
  max_rounds <- as.integer(max_rounds %||% 6)
  if (is.na(max_rounds) || max_rounds < 1) max_rounds <- 6L

  retriever <- retriever %||% storm_retriever(config = config, store = SourceStore$new())
  store <- retriever$store

  coordinator <- config$make_chat("coordinator", echo = if (verbose) "output" else "none")
  writer <- config$make_chat("writer", echo = if (verbose) "output" else "none")
  polisher <- config$make_chat("writer", system_prompt = storm_prompt("polisher_system"), echo = if (verbose) "output" else "none")
  extractor <- config$make_chat("judge", system_prompt = storm_prompt("fact_extractor_system"), echo = "none")

  storm_register_default_tools(
    writer,
    retriever,
    model = config$models[["writer"]],
    search_provider = config$search_provider
  )

  title <- topic
  perspectives <- NULL
  personas <- NULL
  expert_chats <- list()  # Created per-perspective
  outline <- NULL
  draft_md <- NULL
  report_md <- NULL

  if ("perspectives" %in% steps) {
    if (verbose) stormr_inform("Discovering perspectives for: {.val {topic}}")
    seed <- retriever$search(topic, k = min(5, config$max_search_results))
    seed_txt <- paste0(
      "Seed sources:\n",
      paste0("- ", seed$title, " (", seed$source_id, ") ", seed$url, collapse = "\n")
    )
    prompt <- paste0(
      "You are planning a comprehensive research report.\n",
      "Topic: ", topic, "\n\n",
      seed_txt, "\n\n",
      "Propose ", n_experts, "-", n_experts + 2, " distinct perspectives to cover the topic. Each perspective should have 3-6 research questions.\n",
      "Return structured data."
    )
    type <- storm_type_perspectives()
    plan <- coordinator$chat_structured(prompt, type = type, echo = "none", convert = FALSE)
    title <- plan$title %||% topic
    perspectives <- plan$perspectives %||% list()
    # Limit perspectives to n_experts
    if (length(perspectives) > n_experts) {
      perspectives <- perspectives[seq_len(n_experts)]
    }
    store$set_artifact("perspectives", perspectives)
    store$set_artifact("title", title)

    # Generate personas aligned with perspectives
    if (verbose) stormr_inform("Generating {n_experts} expert personas")
    personas <- storm_generate_personas(
      topic = topic,
      n = n_experts,
      config = config,
      verbose = verbose
    )
    store$set_artifact("personas", personas)
  } else {
    perspectives <- store$get_artifact("perspectives") %||% list()
    personas <- store$get_artifact("personas") %||% list()
  }

  if ("research" %in% steps) {
    if (verbose) stormr_inform("Research loop: {length(perspectives)} perspectives")

    if (length(perspectives) == 0) {
      # Fallback: single perspective
      perspectives <- list(list(name = "Overview", description = "General overview", key_questions = c(topic)))
    }

    # Create expert chats with personas (one per perspective)
    for (i in seq_along(perspectives)) {
      persona <- if (i <= length(personas)) personas[[i]] else NULL
      sp <- storm_render_expert_prompt(persona = persona, expert_id = i)
      expert_chats[[i]] <- config$make_chat("expert", system_prompt = sp, echo = if (verbose) "output" else "none")
      storm_register_default_tools(
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
        stormr_inform("Perspective: {.val {p_name}}")
        if (!is.null(persona)) {
          stormr_inform("  Expert: {persona_name} ({persona$title %||% 'Research Specialist'})")
        }
      }

      if (identical(research_strategy, "key_questions")) {
        # Ask expert to answer each planned key question (limited)
        qs_limited <- head(qs, max_questions_per_perspective)
        if (verbose && length(qs) > length(qs_limited)) {
          stormr_inform("  Limiting to {length(qs_limited)} of {length(qs)} questions")
        }
        for (q in qs_limited) {
          prompt <- paste0(
            "Perspective: ", p_name, "\n",
            "Description: ", p_desc, "\n\n",
            "Question: ", q, "\n\n",
            "Instructions:\n",
            "- Use web_search + fetch_url as needed.\n",
            "- Only state factual claims that are supported by sources you fetched.\n",
            "- For each factual sentence, add one or more citations like [Sxxxxxxxxxxxx].\n",
            "- If evidence is weak or unclear, say so and do not overclaim.\n\n",
            "Answer:"
          )
          ans <- expert$chat(prompt, echo = if (verbose) "output" else "none")
          storm_extract_facts_from_answer(extractor, ans, store)
        }
      } else {
        # Conversation-style interviewing: writer proposes the next best question,
        # expert answers with citations; repeat until done or max_rounds.
        answered <- character()
        for (round in seq_len(max_rounds)) {
          answered_md <- if (length(answered) == 0) "(none yet)" else paste0("- ", answered, collapse = "\n")
          facts_md <- storm_summarize_facts_for_prompt(store, max_items = 60)

          nxt <- storm_generate_next_question(writer, topic, p, answered_md = answered_md, facts_md = facts_md)
          q <- stormr_trim(nxt$question %||% "")
          done <- isTRUE(nxt$done)

          if (is.na(q) || q == "") break

          prompt <- paste0(
            "Perspective: ", p_name, "\n",
            "Description: ", p_desc, "\n\n",
            "Question: ", q, "\n\n",
            "Instructions:\n",
            "- Use web_search + fetch_url as needed.\n",
            "- Only state factual claims that are supported by sources you fetched.\n",
            "- For each factual sentence, add one or more citations like [Sxxxxxxxxxxxx].\n",
            "- If evidence is weak or unclear, say so and do not overclaim.\n\n",
            "Answer:"
          )

          ans <- expert$chat(prompt, echo = if (verbose) "output" else "none")
          answered <- c(answered, paste0("Q: ", q, "\nA: ", ans))
          storm_extract_facts_from_answer(extractor, ans, store)

          if (done) break
        }
      }
    }

    store$set_artifact("facts", store$list_facts())
  }

  if ("outline" %in% steps) {
    if (verbose) stormr_inform("Generating outline")
    facts_txt <- storm_summarize_facts_for_prompt(store, max_items = 80)
    prompt <- paste0(
      "Create a detailed outline for a report.\n\n",
      "Topic: ", topic, "\n",
      "Desired report title: ", title, "\n\n",
      "Available verified fact notes (each includes citations):\n",
      facts_txt, "\n\n",
      "Requirements:\n",
      "- Organize into 4-6 sections (keep it focused).\n",
      "- Include subsections with bullet points.\n",
      "- Do not invent facts; plan based on available facts.\n",
      "- Return structured data.\n"
    )
    type <- storm_type_outline()
    outline <- writer$chat_structured(prompt, type = type, echo = "none", convert = FALSE)
    store$set_artifact("outline", outline)
  } else {
    outline <- store$get_artifact("outline")
  }

  if ("write" %in% steps) {
    if (verbose) stormr_inform("Writing draft")
    if (is.null(outline) || is.null(outline$sections)) {
      stormr_abort("No outline available; run steps including 'outline'.")
    }

    parts <- c()
    for (sec in outline$sections) {
      sec_title <- sec$title %||% "Section"
      sec_summary <- sec$summary %||% ""
      sub <- sec$subsections %||% list()

      relevant <- storm_keyword_filter_facts(store, query = sec_title, max_items = 25)
      rel_txt <- if (length(relevant) == 0) "(no directly matched facts; use best judgement and call tools)" else
        paste(purrr::map_chr(relevant, function(f) {
          paste0("- ", f$claim, " [", paste(f$source_ids, collapse = ", "), "]")
        }), collapse = "\n")

      sub_txt <- if (length(sub) == 0) "" else {
        paste(purrr::map_chr(sub, function(s) {
          b <- s$bullets %||% character()
          paste0("### ", s$title, "\n", paste0("- ", b, collapse = "\n"))
        }), collapse = "\n\n")
      }

      prompt <- paste0(
        "Write a report section in Markdown.\n\n",
        "Section title: ", sec_title, "\n",
        "Section intent: ", sec_summary, "\n\n",
        "Planned subsections:\n", sub_txt, "\n\n",
        "Verified fact notes you MUST use (do not invent facts or fetch additional sources):\n",
        rel_txt, "\n\n",
        "Rules:\n",
        "- Every factual claim must end with one or more citations like [Sxxxxxxxxxxxx].\n",
        "- Use ONLY the facts provided above. Do NOT call tools or fetch new sources.\n",
        "- Keep the writing concise, technical, and well-structured.\n\n",
        "Write the section now:"
      )

      sec_md <- writer$chat(prompt, echo = if (verbose) "output" else "none")
      parts <- c(parts, paste0("## ", sec_title, "\n\n", sec_md))
      # Extract any newly-cited facts from the section itself
      storm_extract_facts_from_answer(extractor, sec_md, store)
    }

    draft_md <- paste(parts, collapse = "\n\n")
    store$set_artifact("draft_md", draft_md)
  } else {
    draft_md <- store$get_artifact("draft_md")
  }

  if ("polish" %in% steps) {
    if (verbose) stormr_inform("Polishing and consistency pass")
    prompt <- paste0(
      "Polish the following Markdown report.\n\n",
      "Rules:\n",
      "- Do not add new facts.\n",
      "- Preserve all citations like [Sxxxxxxxxxxxx].\n",
      "- Improve clarity, flow, and headings.\n\n",
      "<draft>\n", draft_md, "\n</draft>\n"
    )
    polished <- polisher$chat(prompt, echo = if (verbose) "output" else "none")
    report_md <- storm_report_md(title = title, body = polished, store = store)
    store$set_artifact("report_md", report_md)
  } else {
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
    retriever = retriever
  )
}

#' Run STORM asynchronously (Shiny-friendly)
#'
#' This is a thin wrapper around [storm_run()] that returns a promise.
#' It is useful when calling STORM from a Shiny app without blocking the session.
#'
#' @param ... Arguments passed to [storm_run()]. See [storm_run()] for details
#'   on available parameters including `topic`, `config`, `retriever`,
#'   `n_experts`, `research_strategy`, `max_rounds`, `steps`, and `verbose`.
#' @return A `promises::promise` that resolves with the storm_run result.
#' @seealso [storm_run()] for the synchronous version.
#' @export
storm_run_async <- function(...) {
  stormr_require("promises", "storm_run_async() uses promises.")
  promises::promise(function(resolve, reject) {
    tryCatch(resolve(storm_run(...)), error = reject)
  })
}
