# STORM research stage

tempest_storm_allowed_connection_ref_ids <- function(
  connection_permissions,
  expert_id,
  model_role
) {
  permission_ids <- unique(c(expert_id, model_role))
  permission_ids <- permission_ids[
    !is.na(permission_ids) & nzchar(permission_ids)
  ]
  unique(unlist(
    connection_permissions[
      intersect(permission_ids, names(connection_permissions))
    ],
    use.names = FALSE
  ))
}

#' @keywords internal
tempest_generate_next_question <- function(
  writer_chat,
  topic,
  perspective,
  answered_md,
  facts_md,
  module,
  knowledge_view = module$knowledge_view %||% NULL,
  record_stage = function(record, output = NULL) invisible(record)
) {
  stage_result <- tempest_execute_stage(
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
    context = tempest_stage_context_knowledge_view(
      list(),
      module,
      knowledge_view
    ),
    record_stage = record_stage
  )
  stage_result$output
}

#' @keywords internal
tempest_normalize_next_question <- function(output) {
  if (!is.list(output) || is.data.frame(output)) {
    tempest_abort(
      "Next-question stage output must be a record.",
      class = "tempest_stage_output_error"
    )
  }
  question <- tempest_stage_string(output$question, "question")
  done <- output$done %||% FALSE
  if (!is.logical(done) || length(done) != 1L || is.na(done)) {
    tempest_abort(
      "Next-question stage output field {.field done} must be `TRUE` or `FALSE`.",
      class = "tempest_stage_output_error"
    )
  }
  list(question = question, done = done)
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
  module,
  max_queries = 3,
  knowledge_view = module$knowledge_view %||% NULL,
  record_stage = function(record, output = NULL) invisible(record)
) {
  stage_result <- tempest_execute_stage(
    module,
    chat,
    inputs = list(question = question, topic = topic),
    context = tempest_stage_context_knowledge_view(
      list(max_queries = as.integer(max_queries)),
      module,
      knowledge_view
    ),
    record_stage = record_stage
  )
  stage_result$output
}

#' @keywords internal
tempest_normalize_query_decomposition <- function(
  x,
  max_queries = 4
) {
  queries <- if (is.list(x) && !is.null(x$queries)) x$queries else x
  queries <- tempest_stage_string_array(queries, "queries")
  max_queries <- as.integer(max_queries %||% 4L)
  if (is.na(max_queries) || max_queries < 1) {
    tempest_abort(
      "{.arg max_queries} must be a positive whole number.",
      class = "tempest_config_error"
    )
  }
  list(queries = queries[seq_len(min(length(queries), max_queries))])
}

#' @keywords internal
tempest_normalize_optional_score <- function(x) {
  if (is.null(x)) {
    return(NA_real_)
  }
  if (
    !is.numeric(x) ||
      length(x) != 1L ||
      (!is.na(x) && (!is.finite(x) || x < 0 || x > 1))
  ) {
    tempest_abort(
      "Claim-extraction support scores must be `NA` or finite values in [0, 1].",
      class = "tempest_stage_output_error"
    )
  }
  as.double(x)
}

#' @keywords internal
tempest_normalize_fact_output <- function(x) {
  if (!is.list(x) || is.data.frame(x) || is.null(x$facts)) {
    tempest_abort(
      "Claim-extraction stage output must contain a {.field facts} list.",
      class = "tempest_stage_output_error"
    )
  }
  facts <- x$facts
  if (!is.list(facts) || is.data.frame(facts) || !is.null(names(facts))) {
    tempest_abort(
      "Claim-extraction stage field {.field facts} must be a list.",
      class = "tempest_stage_output_error"
    )
  }
  purrr::map(facts, function(f) {
    if (!is.list(f) || is.data.frame(f)) {
      tempest_abort(
        "Claim-extraction fact entries must be records.",
        class = "tempest_stage_output_error"
      )
    }
    claim <- tempest_stage_string(f$claim, "claim")
    sources <- f$sources
    if (
      !is.list(sources) ||
        is.data.frame(sources) ||
        !is.null(names(sources)) ||
        length(sources) == 0L
    ) {
      tempest_abort(
        "Claim-extraction facts require a non-empty {.field sources} list.",
        class = "tempest_stage_output_error"
      )
    }
    confidence <- f$confidence %||% "medium"
    if (
      !rlang::is_string(confidence) ||
        !confidence %in% c("low", "medium", "high")
    ) {
      tempest_abort(
        paste0(
          "Claim-extraction confidence must be one of ",
          "{.val {c('low', 'medium', 'high')}}."
        ),
        class = "tempest_stage_output_error"
      )
    }
    note <- f$note %||% NA_character_
    if (
      !is.character(note) ||
        is.object(note) ||
        !is.null(names(note)) ||
        length(note) != 1L
    ) {
      tempest_abort(
        "Claim-extraction note must be a single string or `NA`.",
        class = "tempest_stage_output_error"
      )
    }
    list(
      claim = claim,
      sources = sources,
      confidence = confidence,
      support_score = tempest_normalize_optional_score(
        f$support_score %||% f$score
      ),
      note = note
    )
  })
}

#' @keywords internal
tempest_answer_source_context <- function(
  answer_text,
  store,
  source_ids = NULL
) {
  sources <- store$list_retrieved_sources()
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
tempest_claim_extraction_citation_mode <- function(
  answer_text,
  source_ids = NULL,
  source_context = ""
) {
  source_ids <- unique(source_ids[!is.na(source_ids) & nzchar(source_ids)])
  has_native <- length(source_ids) > 0
  has_inline <- length(tempest_extract_citation_ids(answer_text)) > 0
  has_url <- nzchar(source_context) &&
    grepl("https?://", answer_text %||% "", ignore.case = TRUE)

  n_modes <- sum(c(has_native, has_inline, has_url))
  if (n_modes > 1) {
    return("mixed")
  }
  if (has_native) {
    return("provider_native")
  }
  if (has_url) {
    return("url")
  }
  "tempest_inline"
}

#' @keywords internal
tempest_claim_extraction_inputs <- function(answer_text, store, source_ids) {
  source_context <- tempest_answer_source_context(
    answer_text,
    store,
    source_ids = source_ids
  )
  list(
    answer_text = answer_text,
    source_context = source_context,
    source_ids = paste(source_ids, collapse = "\n"),
    citation_mode = tempest_claim_extraction_citation_mode(
      answer_text,
      source_ids = source_ids,
      source_context = source_context
    )
  )
}

#' @keywords internal
tempest_extracted_evidence_spans <- function(claims) {
  spans <- attr(claims, "tempest_evidence_spans", exact = TRUE) %||% list()
  if (
    !is.list(spans) ||
      is.data.frame(spans) ||
      !is.null(names(spans)) ||
      !all(vapply(
        spans,
        \(span) S7::S7_inherits(span, tempest_evidence_span),
        logical(1)
      ))
  ) {
    tempest_stage_evaluator_abort(
      "Claim evaluator returned malformed evidence-span lineage."
    )
  }
  spans
}

#' @keywords internal
tempest_extract_claims_execution_bind <- function(
  module,
  session_id,
  expert_id,
  retrieval_step_id,
  perspective_id,
  section_id
) {
  module <- tempest_dsprrr_execution_verify(module, "extract_claims")
  claim_context <- list(
    claim_type = "finding",
    session_id = session_id,
    expert_id = expert_id,
    retrieval_step_id = retrieval_step_id,
    perspective_id = perspective_id,
    section_id = section_id
  )
  identifier_fields <- setdiff(names(claim_context), "claim_type")
  for (field in identifier_fields) {
    value <- claim_context[[field]]
    if (
      !is.character(value) ||
        length(value) != 1L ||
        (!is.na(value) && !tempest_opaque_identifier_valid(value))
    ) {
      tempest_stage_governance_abort(
        "Claim context field {.field {field}} is not a safe opaque identifier."
      )
    }
  }
  trace <- module$trace_context
  if (!is.na(session_id)) {
    if (!identical(trace$research_run_id %||% NULL, session_id)) {
      tempest_stage_governance_abort(
        paste0(
          "Claim session identity must match the ProgramSet research-run ",
          "trace before provider execution."
        )
      )
    }
  }
  dynamic_bindings <- c(
    expert_id = "expert_id",
    retrieval_step_id = "correlation_id"
  )
  for (claim_field in names(dynamic_bindings)) {
    value <- claim_context[[claim_field]]
    if (is.na(value)) {
      next
    }
    trace_field <- dynamic_bindings[[claim_field]]
    existing <- trace[[trace_field]] %||% NULL
    if (!is.null(existing) && !identical(existing, value)) {
      tempest_stage_governance_abort(
        "Claim context identity conflicts with the bound ProgramSet trace."
      )
    }
    trace[[trace_field]] <- value
  }
  module$trace_context <- tempest_research_manifest_canonical_value(
    trace,
    "trace_context"
  )
  list(module = module, claim_context = claim_context)
}

#' @keywords internal
tempest_extract_facts_from_answer <- function(
  chat,
  answer_text,
  store,
  module,
  source_ids = NULL,
  session_id = NA_character_,
  expert_id = NA_character_,
  retrieval_step_id = NA_character_,
  perspective_id = NA_character_,
  section_id = NA_character_,
  knowledge_view = module$knowledge_view %||% NULL,
  record_stage = function(record, output = NULL) invisible(record)
) {
  record_stage_callback <- record_stage %||% tempest_stage_record_discard
  binding <- tempest_extract_claims_execution_bind(
    module,
    session_id,
    expert_id,
    retrieval_step_id,
    perspective_id,
    section_id
  )
  module <- binding$module
  # Use a separate extraction call to minimize hallucinated facts. dsprrr gets
  # the same source context as the fallback prompt so native-provider citations
  # can still use the optimized module path when available.
  source_ids <- unique(source_ids[!is.na(source_ids) & nzchar(source_ids)])
  extraction_inputs <- tempest_claim_extraction_inputs(
    answer_text,
    store,
    source_ids
  )
  stage_result <- tempest_execute_stage(
    module,
    chat,
    inputs = extraction_inputs,
    context = tempest_stage_context_knowledge_view(
      list(
        workspace = store,
        known_source_ids = vapply(
          store$list_retrieved_sources(),
          `[[`,
          character(1),
          "id"
        ),
        claim_context = binding$claim_context
      ),
      module,
      knowledge_view
    ),
    output_reference = function(output, running_record, context) {
      tempest_stage_output_reference(
        "workspace_claims",
        vapply(output, \(claim) claim@claim_id, character(1)),
        content_digest = tempest_stage_claims_output_digest(
          output,
          running_record
        )
      )
    },
    record_stage = function(record, output = NULL) {
      if (is.null(output)) {
        return(record_stage_callback(record))
      }
      store$add_extracted_claim_batch(
        claims = output,
        evidence_spans = tempest_extracted_evidence_spans(output),
        commit = function() record_stage_callback(record, output)
      )
      invisible(record)
    }
  )
  invisible(stage_result)
}

#' @keywords internal
tempest_extract_facts_from_answer_async <- function(
  chat,
  answer_text,
  store,
  module,
  source_ids = NULL,
  session_id = NA_character_,
  expert_id = NA_character_,
  retrieval_step_id = NA_character_,
  perspective_id = NA_character_,
  section_id = NA_character_,
  knowledge_view = module$knowledge_view %||% NULL,
  commit_if = function() TRUE,
  record_stage = function(record, output = NULL) invisible(record)
) {
  tempest_require("promises", "Async fact extraction requires promises.")
  record_stage_callback <- record_stage %||% tempest_stage_record_discard
  binding <- tempest_extract_claims_execution_bind(
    module,
    session_id,
    expert_id,
    retrieval_step_id,
    perspective_id,
    section_id
  )
  module <- binding$module
  source_ids <- unique(source_ids[!is.na(source_ids) & nzchar(source_ids)])
  inputs <- tempest_claim_extraction_inputs(answer_text, store, source_ids)
  request <- tempest_execute_stage_async(
    module,
    chat,
    inputs = inputs,
    context = tempest_stage_context_knowledge_view(
      list(
        workspace = store,
        known_source_ids = vapply(
          store$list_retrieved_sources(),
          `[[`,
          character(1),
          "id"
        ),
        claim_context = binding$claim_context
      ),
      module,
      knowledge_view
    ),
    output_reference = function(output, running_record, context) {
      tempest_stage_output_reference(
        "workspace_claims",
        vapply(output, \(claim) claim@claim_id, character(1)),
        content_digest = tempest_stage_claims_output_digest(
          output,
          running_record
        )
      )
    },
    is_current = commit_if,
    record_stage = function(record, output = NULL) {
      if (is.null(output)) {
        return(record_stage_callback(record))
      }
      if (!tempest_async_is_current(commit_if)) {
        return(invisible(record))
      }
      store$add_extracted_claim_batch(
        claims = output,
        evidence_spans = tempest_extracted_evidence_spans(output),
        commit = function() record_stage_callback(record, output)
      )
      invisible(record)
    }
  )
  promises::then(request, function(stage_result) invisible(stage_result))
}

#' @keywords internal
tempest_turn_answer_and_sources <- function(expert, fallback_answer, store) {
  turn <- if (is.function(expert$last_turn)) expert$last_turn() else NULL
  answer_text <- if (!is.null(turn)) {
    ellmer::contents_markdown(turn)
  } else {
    fallback_answer
  }
  source_ids <- tempest_harvest_native_sources_from_turn(turn, store)
  list(answer_text = answer_text, source_ids = source_ids)
}

#' Research a single perspective (search + expert synthesis)
#'
#' Shared by the parallel and sequential research fallbacks so both paths
#' behave identically. Returns the retrieved resources and proposed claims
#' gathered for one perspective in an isolated workspace.
#' @keywords internal
tempest_research_one_perspective <- function(
  i,
  perspectives,
  experts,
  config,
  runtime = tempest_runtime(),
  connection_permissions = list(),
  topic,
  research_strategy,
  max_questions_per_perspective,
  programs,
  run_id = NA_character_,
  record_stage = function(record, output = NULL) invisible(record)
) {
  connection_permissions <- tempest_run_connection_permissions(
    connection_permissions,
    runtime
  )
  p <- perspectives[[i]]
  if (i > length(experts)) {
    tempest_abort(
      "Every research perspective requires an explicit expert profile.",
      class = "tempest_config_error"
    )
  }
  expert_profile <- experts[[i]]
  expert_record <- tempest_expert_runtime_record(expert_profile)
  expert_id <- expert_record$expert_id
  perspective_id <- as.character(p$id %||% i)

  knowledge_view <- programs$extract_claims$knowledge_view %||% NULL
  knowledge_snapshot <- if (is.null(knowledge_view)) {
    NULL
  } else {
    tempest_research_workspace_graft_snapshot(
      tempest_governed_procedure_view_snapshot(knowledge_view)
    )
  }
  local_workspace <- tempest_research_workspace(
    graft_snapshot = knowledge_snapshot
  )
  local_retriever <- tempest_retriever(
    config = config,
    workspace = local_workspace
  )

  sp <- tempest_render_expert_prompt(
    persona = expert_profile,
    expert_id = expert_id
  )
  model_role <- expert_record$model_role
  if (is.na(model_role)) {
    model_role <- "expert"
  }
  model <- tempest_runtime_model(config, model_role)
  expert_chat <- tempest_make_chat(
    config,
    model_role,
    system_prompt = sp,
    echo = "none"
  )
  capability_resolution <- runtime$resolve_expert(
    expert_profile,
    allowed_connection_ref_ids = tempest_storm_allowed_connection_ref_ids(
      connection_permissions,
      expert_id,
      model_role
    ),
    context = list(
      retriever = local_retriever,
      model = model,
      search_provider = config@search_provider,
      claim_provenance = list(
        session_id = run_id,
        expert_id = expert_id
      )
    )
  )
  runtime$attach(
    expert_chat,
    capability_resolution,
    context = list(
      run_id = run_id,
      expert_id = expert_id
    )
  )
  writer <- tempest_make_chat(config, "writer", echo = "none")
  extractor <- tempest_make_chat(
    config,
    "judge",
    system_prompt = tempest_prompt("fact_extractor_system"),
    echo = "none"
  )
  p_name <- p$name
  p_desc <- p$description
  qs <- p$key_questions

  if (identical(research_strategy, "key_questions")) {
    qs_limited <- utils::head(qs, max_questions_per_perspective)
    for (q in qs_limited) {
      decomposed <- tempest_decompose_query(
        writer,
        q,
        topic,
        module = programs$query_decomposition,
        max_queries = config@max_search_queries_per_turn,
        record_stage = record_stage
      )
      search_instructions <- paste0(
        "Suggested search queries:\n",
        paste0("- ", decomposed$queries, collapse = "\n"),
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
        paste0(
          "- If add_proposed_claim is available, record key source-backed ",
          "claims with it.\n"
        ),
        "- For each factual sentence, add one or more citations like [Sxxxxxxxxxxxx].\n",
        "- If evidence is weak or unclear, say so and do not overclaim.\n\n",
        "Answer:"
      )
      ans <- expert_chat$chat(prompt, echo = "none")
      harvest <- tempest_turn_answer_and_sources(
        expert_chat,
        ans,
        local_workspace
      )
      tempest_extract_facts_from_answer(
        extractor,
        harvest$answer_text,
        local_workspace,
        module = programs$extract_claims,
        source_ids = harvest$source_ids,
        session_id = run_id,
        expert_id = expert_id,
        perspective_id = perspective_id,
        record_stage = record_stage
      )
    }
  }

  list(
    retrieved_resources = local_workspace$list_retrieved_sources(),
    proposed_claims = local_workspace$list_proposed_claims(),
    evidence_spans = local_workspace$list_evidence_spans()
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
  experts,
  config,
  runtime,
  runtime_factory,
  connection_permissions,
  retriever,
  store,
  topic,
  research_strategy,
  max_rounds,
  max_questions_per_perspective,
  programs,
  verbose,
  run_id = NA_character_,
  record_stage = function(record, output = NULL) invisible(record)
) {
  n <- length(perspectives)
  run_one <- function(i, runtime_value = runtime) {
    records <- list()
    collect_record <- function(record, output = NULL) {
      records <<- tempest_stage_records_upsert(records, record)
      invisible(record)
    }
    tryCatch(
      list(
        ok = TRUE,
        value = tempest_research_one_perspective(
          i,
          perspectives = perspectives,
          experts = experts,
          config = config,
          runtime = runtime_value,
          connection_permissions = connection_permissions,
          topic = topic,
          research_strategy = research_strategy,
          max_questions_per_perspective = max_questions_per_perspective,
          programs = programs,
          run_id = run_id,
          record_stage = collect_record
        ),
        error = NULL,
        records = records
      ),
      error = function(error) {
        list(ok = FALSE, value = NULL, error = error, records = records)
      }
    )
  }

  collected <- NULL
  if (
    tempest_has("mirai") &&
      !tempest_programs_have_knowledge_view(programs)
  ) {
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
            experts,
            config,
            runtime_factory,
            connection_permissions,
            topic,
            research_strategy,
            max_questions_per_perspective,
            programs,
            run_id,
            research_one
          ) {
            records <- list()
            collect_record <- function(record, output = NULL) {
              records <<- tempest_stage_records_upsert(records, record)
              invisible(record)
            }
            tryCatch(
              list(
                ok = TRUE,
                value = research_one(
                  i,
                  perspectives = perspectives,
                  experts = experts,
                  config = config,
                  runtime = runtime_factory(),
                  connection_permissions = connection_permissions,
                  topic = topic,
                  research_strategy = research_strategy,
                  max_questions_per_perspective = max_questions_per_perspective,
                  programs = programs,
                  run_id = run_id,
                  record_stage = collect_record
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
            perspectives = perspectives,
            experts = experts,
            config = config,
            runtime_factory = runtime_factory,
            connection_permissions = connection_permissions,
            topic = topic,
            research_strategy = research_strategy,
            max_questions_per_perspective = max_questions_per_perspective,
            programs = programs,
            run_id = run_id,
            research_one = tempest_research_one_perspective
          )
        )[],
        error = function(e) e
      )
      if (inherits(mapped, "condition")) {
        collected <- rep(
          list(tempest_collect_parallel(mapped)),
          n
        )
      } else {
        collected <- lapply(mapped, tempest_collect_parallel)
      }
    }
  }
  if (is.null(collected)) {
    collected <- lapply(seq_len(n), run_one)
  }

  for (i in seq_len(n)) {
    envelope <- collected[[i]]
    if (!isTRUE(envelope$ok)) {
      tempest_parallel_records_import(envelope$records, record_stage)
      if (!tempest_stage_error_retryable(envelope$error)) {
        stop(envelope$error)
      }
      envelope <- run_one(i)
      if (!isTRUE(envelope$ok)) {
        tempest_parallel_records_import(envelope$records, record_stage)
        stop(envelope$error)
      }
    }
    value <- envelope$value
    for (src in value$retrieved_resources) {
      store$upsert_retrieved_resource(src)
    }
    store$add_extracted_claim_batch(
      claims = value$proposed_claims,
      evidence_spans = value$evidence_spans,
      commit = function() {
        tempest_parallel_records_import(envelope$records, record_stage)
      }
    )
  }

  invisible(TRUE)
}
