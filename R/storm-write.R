# STORM writing stage

#' @keywords internal
tempest_write_section <- function(
  writer,
  section_title,
  section_summary,
  subsections,
  facts_txt,
  module,
  workspace,
  evidence = list(),
  verified_evidence = list(),
  verified_facts = facts_txt,
  min_support_score = 0.7,
  verbose = FALSE,
  record_stage = function(record, output = NULL) invisible(record)
) {
  subsections_txt <- tempest_subsections_markdown(subsections)
  stage_result <- tempest_execute_stage(
    module,
    writer,
    inputs = list(
      section_title = section_title,
      section_summary = section_summary,
      subsections = subsections_txt,
      facts = facts_txt
    ),
    context = list(
      workspace = workspace,
      evidence = evidence,
      verified_evidence = verified_evidence,
      verified_facts = verified_facts,
      min_support_score = min_support_score
    ),
    record_stage = function(record, output = NULL) {
      record_stage(record, output)
    }
  )
  tempest_stage_string(stage_result$output, "section_text")
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
  max_items,
  min_support_score = 0.7
) {
  relevant <- tempest_semantic_filter_facts(
    retriever,
    query = section_title,
    store = store,
    max_items = max_items,
    min_support_score = min_support_score
  )
  if (length(relevant) == 0) {
    result <- "(no directly matched facts; do not add unsupported factual claims)"
    attr(result, "verified_evidence") <- list()
    attr(result, "verified_evidence_count") <- 0L
    return(result)
  }

  result <- paste(
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
  attr(result, "verified_evidence") <- relevant
  attr(result, "verified_evidence_count") <- as.integer(length(relevant))
  result
}

#' @keywords internal
tempest_section_jobs <- function(
  outline,
  retriever,
  store,
  retrieve_top_k,
  min_support_score = 0.7
) {
  sections <- tempest_sections_to_write(outline)
  purrr::imap(sections, function(section, i) {
    section_title <- section$title %||% "Section"
    facts_text <- tempest_section_facts_text(
      retriever,
      store,
      section_title,
      max_items = retrieve_top_k,
      min_support_score = min_support_score
    )
    list(
      index = i,
      title = section_title,
      summary = section$summary %||% "",
      subsections = section$subsections %||% list(),
      facts_text = as.character(facts_text),
      workspace = store,
      evidence = attr(facts_text, "verified_evidence"),
      verified_evidence = attr(facts_text, "verified_evidence"),
      min_support_score = min_support_score
    )
  })
}

#' @keywords internal
tempest_run_section_job <- function(
  job,
  writer,
  module,
  verbose = FALSE,
  record_stage = function(record, output = NULL) invisible(record)
) {
  section_text <- tempest_write_section(
    writer,
    section_title = job$title,
    section_summary = job$summary,
    subsections = job$subsections,
    facts_txt = job$facts_text,
    module = module,
    workspace = job$workspace,
    evidence = job$evidence,
    verified_evidence = job$verified_evidence,
    verified_facts = job$facts_text,
    min_support_score = job$min_support_score,
    verbose = verbose,
    record_stage = record_stage
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
  programs,
  verbose = FALSE,
  record_stage = function(record, output = NULL) invisible(record)
) {
  lapply(jobs, function(job) {
    tempest_run_section_job(
      job,
      writer,
      module = programs$section_writing,
      verbose = verbose,
      record_stage = record_stage
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
  if (
    is.list(value) &&
      !is.data.frame(value) &&
      setequal(names(value), c("ok", "value", "error", "records")) &&
      rlang::is_bool(value$ok)
  ) {
    return(value)
  }
  error <- if (inherits(value, "condition")) {
    value
  } else {
    message <- tryCatch(
      conditionMessage(value),
      error = function(error) paste(as.character(value), collapse = " ")
    )
    rlang::error_cnd(
      "tempest_parallel_worker_error",
      message = message
    )
  }
  list(ok = FALSE, value = NULL, error = error, records = list())
}

#' @keywords internal
tempest_parallel_records_import <- function(records, record_stage) {
  records <- tempest_stage_records_validate(records)
  for (record in records) {
    record_stage(record)
  }
  invisible(records)
}

#' @keywords internal
tempest_write_sections_parallel <- function(
  jobs,
  config,
  programs
) {
  if (!tempest_has("mirai")) {
    return(NULL)
  }

  ready <- tempest_setup_daemons(tempest_parallel_workers(length(jobs)))
  if (!isTRUE(ready)) {
    return(NULL)
  }
  if (isTRUE(attr(ready, "started"))) {
    on.exit(try(mirai::daemons(0), silent = TRUE), add = TRUE)
  }

  collected <- tryCatch(
    mirai::mirai_map(
      seq_along(jobs),
      function(i, jobs, config, programs, run_section_job) {
        job <- jobs[[i]]
        writer <- tempest_make_chat(config, "writer", echo = "none")
        records <- list()
        worker_record_stage <- function(record, output = NULL) {
          records <<- tempest_stage_records_upsert(records, record)
          invisible(record)
        }
        tryCatch(
          list(
            ok = TRUE,
            value = run_section_job(
              job,
              writer,
              module = programs$section_writing,
              verbose = FALSE,
              record_stage = worker_record_stage
            ),
            error = NULL,
            records = records
          ),
          error = function(error) {
            list(
              ok = FALSE,
              value = NULL,
              error = error,
              records = records
            )
          }
        )
      },
      .args = list(
        jobs = jobs,
        config = config,
        programs = programs,
        run_section_job = tempest_run_section_job
      )
    )[],
    error = function(e) e
  )
  if (inherits(collected, "condition")) {
    return(rep(
      list(tempest_collect_parallel(collected)),
      length(jobs)
    ))
  }

  lapply(collected, tempest_collect_parallel)
}

#' @keywords internal
tempest_write_section_jobs <- function(
  jobs,
  writer,
  config,
  programs,
  parallel = FALSE,
  verbose = FALSE,
  record_stage = function(record, output = NULL) invisible(record)
) {
  if (length(jobs) == 0) {
    return(list())
  }

  if (isTRUE(parallel) && length(jobs) > 1) {
    parallel_results <- tempest_write_sections_parallel(
      jobs,
      config,
      programs = programs
    )
    if (!is.null(parallel_results)) {
      results <- vector("list", length(jobs))
      for (index in seq_along(jobs)) {
        envelope <- parallel_results[[index]]
        tempest_parallel_records_import(envelope$records, record_stage)
        if (isTRUE(envelope$ok)) {
          results[[index]] <- envelope$value
          next
        }
        if (!tempest_stage_error_retryable(envelope$error)) {
          stop(envelope$error)
        }
        results[[index]] <- tempest_run_section_job(
          jobs[[index]],
          writer,
          module = programs$section_writing,
          verbose = verbose,
          record_stage = record_stage
        )
      }
      return(results)
    }
  }

  tempest_write_sections_sequential(
    jobs,
    writer,
    programs = programs,
    verbose = verbose,
    record_stage = record_stage
  )
}

#' @keywords internal
tempest_write_lead_section <- function(
  writer,
  topic,
  title,
  draft_md,
  facts_txt,
  module,
  workspace,
  evidence = list(),
  verified_evidence = list(),
  verified_facts = facts_txt,
  min_support_score = 0.7,
  verbose = FALSE,
  record_stage = function(record, output = NULL) invisible(record)
) {
  stage_result <- tempest_execute_stage(
    module,
    writer,
    inputs = list(
      topic = topic,
      title = title,
      article_body = substr(draft_md, 1, 3000),
      facts = facts_txt
    ),
    context = list(
      workspace = workspace,
      evidence = evidence,
      verified_evidence = verified_evidence,
      verified_facts = verified_facts,
      min_support_score = min_support_score
    ),
    record_stage = function(record, output = NULL) {
      record_stage(record, output)
    }
  )
  tempest_stage_string(stage_result$output, "lead_section")
}

tempest_supported_claims <- function(store, min_support_score = 0.7) {
  if (!inherits(store, "ResearchWorkspace")) {
    tempest_research_workspace_abort(
      "{.arg store} must be a ResearchWorkspace."
    )
  }
  min_support_score <- tempest_normalize_min_support_score(min_support_score)
  purrr::keep(
    store$list_proposed_claims(),
    function(claim) {
      identical(claim@verification_status, "supported") &&
        length(claim@support_score) == 1L &&
        !is.na(claim@support_score) &&
        is.finite(claim@support_score) &&
        claim@support_score >= min_support_score
    }
  )
}

tempest_summarize_facts_for_prompt <- function(
  store,
  max_items = 60,
  verified_only = FALSE,
  min_support_score = 0.7
) {
  if (!inherits(store, "ResearchWorkspace")) {
    tempest_research_workspace_abort(
      "{.arg store} must be a ResearchWorkspace."
    )
  }
  facts <- if (isTRUE(verified_only)) {
    tempest_supported_claims(
      store,
      min_support_score = min_support_score
    )
  } else {
    store$list_proposed_claims()
  }
  if (length(facts) == 0) {
    return(
      if (isTRUE(verified_only)) {
        "(no verified facts available)"
      } else {
        "(no facts yet)"
      }
    )
  }
  facts <- facts[seq_len(min(length(facts), max_items))]
  lines <- purrr::map_chr(facts, function(f) {
    cites <- paste0("[", paste(f@source_ids, collapse = ", "), "]")
    glue::glue("- {f@claim_text} {cites}")
  })
  paste(lines, collapse = "\n")
}

#' @keywords internal
tempest_keyword_filter_facts <- function(
  store,
  query,
  max_items = 30,
  min_support_score = 0.7
) {
  facts <- tempest_supported_claims(
    store,
    min_support_score = min_support_score
  )
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
#' @param store A [ResearchWorkspace].
#' @param max_items Maximum facts to return.
#' @return A list of fact objects.
#' @keywords internal
tempest_semantic_filter_facts <- function(
  retriever,
  query,
  store,
  max_items = 30,
  min_support_score = 0.7
) {
  # Fall back to keyword when ragnar is unavailable
  if (is.null(retriever$ragnar_store) || !tempest_has("ragnar")) {
    return(tempest_keyword_filter_facts(
      store,
      query,
      max_items = max_items,
      min_support_score = min_support_score
    ))
  }

  facts <- tempest_supported_claims(
    store,
    min_support_score = min_support_score
  )
  if (length(facts) == 0) {
    return(list())
  }

  chunks <- retriever$retrieve(query, k = max_items, method = "hybrid")

  if (is.null(chunks) || nrow(chunks) == 0) {
    return(list())
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
