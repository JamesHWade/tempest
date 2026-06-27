# STORM pipeline (scripted)

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
#'   [tempest_optimize_dsprrr_modules()]. If `NULL`, fresh modules are created.
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
#' @examples
#' \dontrun{
#' cfg <- tempest_config()
#' result <- tempest_run("History of jazz", config = cfg)
#' cat(result$report_md)
#' }
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
            ans <- tryCatch(
              expert$chat(prompt, echo = if (verbose) "output" else "none"),
              error = function(e) {
                tempest_warn(
                  "Expert answer failed for {.val {p_name}}: {conditionMessage(e)}"
                )
                NULL
              }
            )
            if (!is.null(ans)) {
              tryCatch(
                tempest_extract_facts_from_answer(
                  extractor,
                  ans,
                  store,
                  module = dsprrr_modules$fact_extraction
                ),
                error = function(e) {
                  tempest_warn("Fact extraction failed: {conditionMessage(e)}")
                }
              )
            }
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

            ans <- tryCatch(
              expert$chat(prompt, echo = if (verbose) "output" else "none"),
              error = function(e) {
                tempest_warn(
                  "Expert answer failed for {.val {p_name}}: {conditionMessage(e)}"
                )
                NULL
              }
            )
            if (is.null(ans)) {
              if (done) {
                break
              }
              next
            }
            answered <- c(answered, paste0("Q: ", q, "\nA: ", ans))
            tryCatch(
              tempest_extract_facts_from_answer(
                extractor,
                ans,
                store,
                module = dsprrr_modules$fact_extraction
              ),
              error = function(e) {
                tempest_warn("Fact extraction failed: {conditionMessage(e)}")
              }
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
#' @examples
#' \dontrun{
#' tempest_run_async("History of jazz", config = tempest_config()) |>
#'   promises::then(function(result) cat(result$report_md))
#' }
#' @export
tempest_run_async <- function(...) {
  tempest_require("promises", "tempest_run_async() uses promises.")
  promises::promise(function(resolve, reject) {
    tryCatch(resolve(tempest_run(...)), error = reject)
  })
}
