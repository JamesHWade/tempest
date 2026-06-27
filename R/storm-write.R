# STORM writing stage

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
      paste0(
        "- ",
        f@claim_text,
        " [",
        paste(f@source_ids, collapse = ", "),
        "]"
      )
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

#' Choose a worker count for parallel execution
#' @keywords internal
tempest_parallel_workers <- function(n_items = NULL) {
  workers <- getOption("tempest.parallel_workers", NULL)
  if (is.null(workers)) {
    cores <- tryCatch(parallel::detectCores(), error = function(e) NA_integer_)
    workers <- if (is.na(cores) || cores < 2L) 2L else min(cores - 1L, 8L)
  }
  workers <- as.integer(workers)
  if (is.na(workers) || workers < 1L) {
    workers <- 1L
  }
  if (!is.null(n_items)) {
    workers <- min(workers, max(1L, as.integer(n_items)))
  }
  as.integer(workers)
}

#' Ensure mirai daemons are available
#'
#' Returns `TRUE` if daemons are ready, with a `started` attribute recording
#' whether this call created them (so the caller can tear them down). Returns
#' `FALSE` if mirai daemons could not be started.
#' @keywords internal
tempest_setup_daemons <- function(n) {
  started <- FALSE
  ok <- tryCatch(
    {
      if (mirai::status()$connections < 1L) {
        mirai::daemons(n)
        started <- TRUE
      }
      TRUE
    },
    error = function(e) FALSE
  )
  structure(isTRUE(ok), started = started)
}

#' @keywords internal
tempest_collect_parallel <- function(value) {
  # Convert a single mirai_map result element into a value or NULL on failure.
  if (
    is.null(value) ||
      inherits(value, "errorValue") ||
      inherits(value, "miraiError") ||
      inherits(value, "condition")
  ) {
    return(NULL)
  }
  value
}

#' @keywords internal
tempest_write_sections_parallel <- function(
  jobs,
  config,
  dsprrr_modules = NULL
) {
  # On any failure we return a list of NULLs so the caller writes the
  # affected sections sequentially. Real parallelism requires the installed
  # 'tempest' package to be available to the mirai workers; in environments
  # where it is not (e.g. `devtools::load_all()`), this falls back cleanly.
  fallback <- vector("list", length(jobs))
  if (!tempest_has("mirai")) {
    return(fallback)
  }

  ready <- tempest_setup_daemons(tempest_parallel_workers(length(jobs)))
  if (!isTRUE(ready)) {
    tempest_warn(
      "Could not start mirai daemons; writing sections sequentially."
    )
    return(fallback)
  }
  if (isTRUE(attr(ready, "started"))) {
    on.exit(try(mirai::daemons(0), silent = TRUE), add = TRUE)
  }

  collected <- tryCatch(
    mirai::mirai_map(
      seq_along(jobs),
      function(i, jobs, config, dsprrr_modules, run_section_job, make_modules) {
        job <- jobs[[i]]
        writer <- tempest_make_chat(config, "writer", echo = "none")
        modules <- if (is.null(dsprrr_modules)) {
          make_modules(config)
        } else {
          dsprrr_modules
        }
        run_section_job(
          job,
          writer,
          module = modules$section_writing,
          verbose = FALSE
        )
      },
      .args = list(
        jobs = jobs,
        config = config,
        dsprrr_modules = dsprrr_modules,
        run_section_job = tempest_run_section_job,
        make_modules = tempest_make_dsprrr_modules
      )
    )[],
    error = function(e) e
  )
  if (inherits(collected, "condition")) {
    tempest_warn(
      "Parallel section writing failed ({conditionMessage(collected)}); writing sections sequentially."
    )
    return(fallback)
  }

  lapply(collected, tempest_collect_parallel)
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

tempest_summarize_facts_for_prompt <- function(store, max_items = 60) {
  stopifnot(inherits(store, "SourceStore"))
  facts <- store$list_claims()
  if (length(facts) == 0) {
    return("(no facts yet)")
  }
  facts <- facts[seq_len(min(length(facts), max_items))]
  lines <- purrr::map_chr(facts, function(f) {
    cites <- paste0("[", paste(f@source_ids, collapse = ", "), "]")
    glue::glue("- {f@claim_text} {cites}")
  })
  paste(lines, collapse = "\n")
}

#' @keywords internal
tempest_keyword_filter_facts <- function(store, query, max_items = 30) {
  facts <- store$list_claims()
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
      claim <- tolower(f@claim_text %||% "")
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

  facts <- store$list_claims()
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
      sum(f@source_ids %in% chunk_source_ids)
    },
    integer(1)
  )

  # Also add keyword overlap as tiebreaker
  tokens <- unique(tolower(unlist(strsplit(tempest_trim(query), "\\s+"))))
  tokens <- tokens[nzchar(tokens)]
  keyword_scores <- vapply(
    facts,
    function(f) {
      claim <- tolower(f@claim_text %||% "")
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
