# STORM research stage

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
tempest_answer_source_context <- function(
  answer_text,
  store,
  source_ids = NULL
) {
  sources <- store$list_sources()
  if (length(sources) == 0 || is.null(answer_text) || !nzchar(answer_text)) {
    return("")
  }
  source_ids <- unique(source_ids[!is.na(source_ids) & nzchar(source_ids)])
  cited <- purrr::keep(sources, function(source) {
    source_id <- source$id %||% ""
    url <- source$url %||% ""
    source_id %in%
      source_ids ||
      (!is.na(source_id) &&
        nzchar(source_id) &&
        grepl(source_id, answer_text, fixed = TRUE)) ||
      (!is.na(url) && nzchar(url) && grepl(url, answer_text, fixed = TRUE))
  })
  if (length(cited) == 0) {
    return("")
  }
  lines <- purrr::map_chr(cited, function(source) {
    title <- source$title %||% ""
    if (is.na(title)) {
      title <- ""
    }
    paste0(
      "- [",
      source$id,
      "] ",
      title,
      " ",
      source$url %||% ""
    )
  })
  paste(
    "Known source IDs for citations found in the answer:",
    paste(lines, collapse = "\n"),
    sep = "\n"
  )
}

#' @keywords internal
tempest_resolve_fact_source_ids <- function(source_refs, store) {
  source_refs <- unique(source_refs[!is.na(source_refs) & nzchar(source_refs)])
  if (length(source_refs) == 0) {
    return(character())
  }
  ids <- purrr::map_chr(source_refs, function(ref) {
    if (!is.null(store$get_source(ref))) {
      return(ref)
    }
    url <- tryCatch(tempest_normalize_url(ref), error = function(e) {
      NA_character_
    })
    if (is.na(url) || !nzchar(url)) {
      return(NA_character_)
    }
    id <- tempest_source_id(url)
    if (!is.null(store$get_source(id))) id else NA_character_
  })
  unique(ids[!is.na(ids) & nzchar(ids)])
}

#' @keywords internal
tempest_extract_facts_from_answer <- function(
  chat,
  answer_text,
  store,
  module = NULL,
  source_ids = NULL
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
    source_ids <- unique(source_ids[!is.na(source_ids) & nzchar(source_ids)])
    source_context <- tempest_answer_source_context(
      answer_text,
      store,
      source_ids = source_ids
    )
    source_rule <- if (nzchar(source_context)) {
      if (length(source_ids) > 0) {
        paste0(
          "- Sources listed in <known_sources> were attached to this answer turn; return the matching source_id when the answer text directly supports the claim.\n"
        )
      } else {
        paste0(
          "- URL citations listed in <known_sources> count as citations to their source_id; return the matching source_id.\n",
          "- Do not use a known source unless its URL or source_id appears in the answer text.\n"
        )
      }
    } else {
      ""
    }
    prompt <- paste0(
      "Extract atomic factual claims from the following answer.\n\n",
      "Rules:\n",
      "- Only extract claims that are explicitly supported by one or more citations in the form [Sxxxxxxxxxxxx].\n",
      source_rule,
      "- For each claim, list the source_id(s) that support it.\n",
      "- Do NOT invent or infer new facts.\n\n",
      "<answer>\n",
      answer_text,
      "\n</answer>\n",
      if (nzchar(source_context)) {
        paste0("\n<known_sources>\n", source_context, "\n</known_sources>\n")
      } else {
        ""
      }
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
    source_refs <- purrr::map_chr(
      f$sources %||% list(),
      ~ .x$source_id %||% .x$id %||% .x$url %||% NA_character_
    )
    src_ids <- tempest_resolve_fact_source_ids(source_refs, store)
    # Drop citations to sources that are not in the store (hallucinated ids).
    known <- src_ids[!purrr::map_lgl(src_ids, ~ is.null(store$get_source(.x)))]
    if (length(known) == 0) {
      next
    }
    # The LLM may omit the optional confidence; normalize anything invalid to
    # "medium" so the S7 enum validator never aborts (which would discard the
    # whole answer's claims).
    conf <- f$confidence
    if (
      length(conf) != 1 || is.na(conf) || !conf %in% c("low", "medium", "high")
    ) {
      conf <- "medium"
    }
    store$add_claim(tempest_claim(
      claim_text = claim,
      source_ids = known,
      claim_type = "finding",
      confidence = conf
    ))
  }
  invisible(TRUE)
}

#' Research a single perspective (search + expert synthesis)
#'
#' Shared by the parallel and sequential research fallbacks so both paths
#' behave identically. Returns the sources and facts gathered for one
#' perspective in an isolated store.
#' @keywords internal
tempest_research_one_perspective <- function(
  i,
  perspectives,
  personas,
  config,
  topic,
  research_strategy,
  max_questions_per_perspective,
  dsprrr_modules = NULL
) {
  p <- perspectives[[i]]
  persona <- if (i <= length(personas)) personas[[i]] else NULL

  local_store <- SourceStore$new()
  local_retriever <- tempest_retriever(config = config, store = local_store)

  sp <- tempest_render_expert_prompt(persona = persona, expert_id = i)
  expert <- tempest_make_chat(
    config,
    "expert",
    system_prompt = sp,
    echo = "none"
  )
  tempest_register_default_tools(
    expert,
    local_retriever,
    model = config@models[["expert"]],
    search_provider = config@search_provider
  )
  writer <- tempest_make_chat(config, "writer", echo = "none")
  extractor <- tempest_make_chat(
    config,
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
          max_queries = config@max_search_queries_per_turn
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
        expert$chat(prompt, echo = "none"),
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
            local_store,
            module = modules$extract_claims
          ),
          error = function(e) {
            tempest_warn("Fact extraction failed: {conditionMessage(e)}")
          }
        )
      }
    }
  }

  list(
    sources = local_store$list_sources(),
    claims = local_store$list_claims()
  )
}

#' Run research in parallel using mirai
#'
#' Falls back to sequential research when mirai daemons cannot be started or
#' the workers fail (for example when the installed 'tempest' package is not
#' available to the workers). Any perspective that fails in a worker is retried
#' sequentially so its evidence is never silently dropped.
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
  n <- length(perspectives)
  run_one <- function(i) {
    tempest_research_one_perspective(
      i,
      perspectives = perspectives,
      personas = personas,
      config = config,
      topic = topic,
      research_strategy = research_strategy,
      max_questions_per_perspective = max_questions_per_perspective,
      dsprrr_modules = dsprrr_modules
    )
  }

  collected <- vector("list", n)
  used_parallel <- FALSE
  if (tempest_has("mirai")) {
    ready <- tempest_setup_daemons(tempest_parallel_workers(n))
    if (isTRUE(ready)) {
      if (isTRUE(attr(ready, "started"))) {
        on.exit(try(mirai::daemons(0), silent = TRUE), add = TRUE)
      }
      mapped <- tryCatch(
        mirai::mirai_map(
          seq_len(n),
          function(
            i,
            perspectives,
            personas,
            config,
            topic,
            research_strategy,
            max_questions_per_perspective,
            dsprrr_modules,
            research_one
          ) {
            research_one(
              i,
              perspectives = perspectives,
              personas = personas,
              config = config,
              topic = topic,
              research_strategy = research_strategy,
              max_questions_per_perspective = max_questions_per_perspective,
              dsprrr_modules = dsprrr_modules
            )
          },
          .args = list(
            perspectives = perspectives,
            personas = personas,
            config = config,
            topic = topic,
            research_strategy = research_strategy,
            max_questions_per_perspective = max_questions_per_perspective,
            dsprrr_modules = dsprrr_modules,
            research_one = tempest_research_one_perspective
          )
        )[],
        error = function(e) e
      )
      if (inherits(mapped, "condition")) {
        tempest_warn(
          "Parallel research failed ({conditionMessage(mapped)}); researching sequentially."
        )
      } else {
        collected <- lapply(mapped, tempest_collect_parallel)
        used_parallel <- TRUE
      }
    } else {
      tempest_warn("Could not start mirai daemons; researching sequentially.")
    }
  }

  # Merge results, retrying any failed (or unrun) perspective sequentially so
  # no perspective's evidence is silently dropped.
  for (i in seq_len(n)) {
    val <- collected[[i]]
    if (is.null(val)) {
      p_name <- perspectives[[i]]$name %||% paste("Perspective", i)
      if (used_parallel) {
        tempest_warn(
          "Research for {.val {p_name}} failed in parallel; retrying sequentially."
        )
      }
      val <- tryCatch(
        run_one(i),
        error = function(e) {
          tempest_warn(
            "Research failed for {.val {p_name}}: {conditionMessage(e)}"
          )
          NULL
        }
      )
    }
    if (is.null(val)) {
      next
    }
    for (src in val$sources %||% list()) {
      store$upsert_source(src)
    }
    for (claim in val$claims %||% list()) {
      store$add_claim(claim)
    }
  }

  invisible(TRUE)
}
