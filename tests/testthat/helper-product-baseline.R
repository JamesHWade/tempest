baseline_local_ids <- function(.local_envir = parent.frame()) {
  counters <- new.env(parent = emptyenv())
  testthat::local_mocked_bindings(
    tempest_uuid = function(prefix = "id") {
      count <- counters[[prefix]] %||% 0L
      count <- count + 1L
      counters[[prefix]] <- count
      paste0(prefix, "_", sprintf("%016x", count))
    },
    .env = .local_envir
  )
}

baseline_snapshot_json <- function(x) {
  cat(
    jsonlite::toJSON(
      x,
      auto_unbox = TRUE,
      null = "null",
      na = "null",
      pretty = TRUE
    ),
    "\n",
    sep = ""
  )
}

baseline_queue_chat <- function(structured_state = NULL, text_state = NULL) {
  pop <- function(state, kind, default = NULL) {
    if (is.null(state)) {
      if (!is.null(default)) {
        return(default)
      }
      stop("baseline_queue_chat: ", kind, " queue is unavailable")
    }
    if (length(state$values) == 0L) {
      stop("baseline_queue_chat: ", kind, " queue exhausted")
    }
    value <- state$values[[1L]]
    state$values <- state$values[-1L]
    value
  }
  queued <- function(state, kind) {
    if (is.null(state)) {
      return(list())
    }
    rep(
      list(function(prompt) pop(state, kind)),
      length(state$values)
    )
  }
  fake_chat(
    structured = queued(structured_state, "structured"),
    text = queued(text_state, "text")
  )
}

baseline_event_labels <- function(events) {
  vapply(
    events,
    function(event) {
      paste(
        event[["event_type"]],
        event[["stage"]],
        event[["step"]],
        event[["status"]],
        sep = ":"
      )
    },
    character(1)
  )
}

baseline_succeeded_stages <- function(events) {
  vapply(
    Filter(
      function(event) {
        identical(event[["event_type"]], "stage") &&
          identical(event[["status"]], "succeeded")
      },
      events
    ),
    `[[`,
    character(1),
    "stage"
  )
}

baseline_claim_records <- function(store) {
  claims <- store$list_proposed_claims()
  if (length(claims) == 0L) {
    return(list())
  }
  claim_ids <- vapply(claims, \(claim) claim@claim_id, character(1))
  claims <- claims[order(claim_ids, method = "radix")]
  lapply(
    claims,
    function(claim) {
      list(
        claim_id = claim@claim_id,
        claim_text = claim@claim_text,
        claim_type = claim@claim_type,
        source_ids = sort(claim@source_ids, method = "radix"),
        evidence_span_ids = sort(
          claim@evidence_span_ids,
          method = "radix"
        ),
        confidence = claim@confidence,
        verification_status = claim@verification_status,
        support_score = claim@support_score
      )
    }
  )
}

baseline_citation_semantics <- function(markdown) {
  lines <- strsplit(markdown, "\n", fixed = TRUE)[[1L]]
  definition_index <- grepl("^\\[\\^[^]]+\\]:", lines, perl = TRUE)
  definition_lines <- lines[definition_index]
  body <- paste(lines[!definition_index], collapse = "\n")
  use_matches <- regmatches(
    body,
    gregexpr("\\[\\^([^]]+)\\]", body, perl = TRUE)
  )[[1L]]
  uses <- if (length(use_matches) == 1L && identical(use_matches[[1L]], "")) {
    character()
  } else {
    sub("^\\[\\^([^]]+)\\]$", "\\1", use_matches)
  }
  definitions <- lapply(definition_lines, function(line) {
    list(
      citation_id = sub(
        "^\\[\\^([^]]+)\\]:.*$",
        "\\1",
        line,
        perl = TRUE
      ),
      reference = sub("^\\[\\^[^]]+\\]:\\s*", "", line, perl = TRUE)
    )
  })
  if (length(definitions) > 0L) {
    ids <- vapply(definitions, `[[`, character(1), "citation_id")
    definitions <- definitions[order(ids, method = "radix")]
  }
  list(
    uses = sort(unique(uses), method = "radix"),
    definitions = definitions
  )
}

baseline_inline_source_ids <- function(text) {
  sort(unique(tempest_extract_citation_ids(text)), method = "radix")
}

baseline_transcript_records <- function(transcript) {
  lapply(
    transcript,
    function(turn) {
      list(
        speaker = turn$speaker,
        role = turn$role,
        source_ids = baseline_inline_source_ids(turn$text)
      )
    }
  )
}

baseline_mindmap_records <- function(mindmap) {
  nodes <- mindmap$nodes %||% list()
  if (length(nodes) > 0L) {
    node_ids <- vapply(nodes, `[[`, character(1), "id")
    nodes <- nodes[order(node_ids, method = "radix")]
  }
  edges <- mindmap$edges %||% list()
  if (length(edges) > 0L) {
    edge_keys <- vapply(
      edges,
      \(edge) paste(edge$from, edge$to, edge$relation %||% "", sep = ":"),
      character(1)
    )
    edges <- edges[order(edge_keys, method = "radix")]
  }
  list(
    nodes = lapply(nodes, function(node) {
      list(
        id = node$id,
        label = node$label,
        parent = node$parent %||% NA_character_,
        source_ids = sort(
          node$source_ids %||% character(),
          method = "radix"
        )
      )
    }),
    edges = lapply(edges, function(edge) {
      list(
        from = edge$from,
        to = edge$to,
        relation = edge$relation %||% NA_character_
      )
    })
  )
}

baseline_report_sections <- function(markdown) {
  lines <- strsplit(markdown, "\n", fixed = TRUE)[[1]]
  headings <- grep("^#{1,6} ", lines, value = TRUE)
  sub("^#{1,6} +", "", headings)
}

storm_product_fixture <- function(.local_envir = parent.frame()) {
  previous_cache <- dsprrr::configure_cache(enable = FALSE)
  withr::defer(
    {
      if (is.null(previous_cache)) {
        dsprrr::configure_cache()
      } else {
        do.call(dsprrr::configure_cache, previous_cache)
      }
    },
    envir = .local_envir
  )
  fixture <- storm_progress_fixture(.local_envir = .local_envir)
  original_chat_fn <- fixture$config@chat_fn
  source_id <- fixture$source_id
  outline <- fixture$outline
  program_stages <- new.env(parent = emptyenv())
  program_stages$value <- character()
  record_program_stage <- function(stage) {
    program_stages$value <- c(program_stages$value, stage)
    invisible(stage)
  }
  expect_program_prompt <- function(chat, stage, prefix) {
    original_structured <- chat$chat_structured
    chat$chat_structured <- function(prompt, ...) {
      if (!startsWith(prompt, prefix)) {
        stop("Unexpected ", stage, " product-baseline prompt.")
      }
      record_program_stage(stage)
      original_structured(prompt, ...)
    }
    chat
  }
  fixture$config@chat_fn <- function(role, model, system_prompt, echo) {
    if (
      identical(role, "coordinator") &&
        identical(system_prompt, tempest_prompt("persona_generator_system"))
    ) {
      return(expect_program_prompt(
        original_chat_fn(role, model, system_prompt, echo),
        "personas",
        "Generate diverse expert personas for STORM"
      ))
    }
    if (identical(role, "coordinator")) {
      return(expect_program_prompt(
        original_chat_fn(role, model, system_prompt, echo),
        "perspectives",
        "Plan a comprehensive STORM research report."
      ))
    }
    if (identical(role, "writer")) {
      return(list(
        chat_structured = function(prompt, type = NULL, ...) {
          if (
            startsWith(
              prompt,
              "Decompose the research question into 2-3 targeted web search queries."
            )
          ) {
            stage <- "query_decomposition"
            value <- list(queries = "progress events")
          } else if (
            startsWith(
              prompt,
              "Create a preliminary STORM outline from parametric knowledge."
            )
          ) {
            stage <- "draft_outline"
            value <- outline
          } else if (
            startsWith(
              prompt,
              "Refine the draft outline using verified fact notes."
            )
          ) {
            stage <- "refined_outline"
            value <- outline
          } else if (
            startsWith(
              prompt,
              "Prepare one concise, decision-useful section as typed briefing items."
            )
          ) {
            stage <- "section_writing"
            value <- fake_briefing_output_from_prompt(
              prompt,
              "STORM progress emits stage events."
            )
          } else if (
            startsWith(
              prompt,
              "Prepare a compact at-a-glance decision brief as typed items."
            )
          ) {
            stage <- "lead_section"
            value <- fake_briefing_output_from_prompt(
              prompt,
              "STORM progress emits stage events."
            )
          } else {
            stop("Unexpected writer product-baseline prompt.")
          }
          record_program_stage(stage)
          value
        },
        chat = function(prompt, ...) {
          stop("Unexpected direct writer fallback in product baseline.")
        },
        register_tools = function(...) invisible(NULL)
      ))
    }
    if (
      identical(role, "judge") &&
        identical(system_prompt, tempest_prompt("fact_extractor_system"))
    ) {
      return(list(
        chat_structured = function(prompt, type = NULL, ...) {
          if (
            !startsWith(
              prompt,
              "Extract atomic factual claims from the answer."
            )
          ) {
            stop("Unexpected claim-extraction product-baseline prompt.")
          }
          record_program_stage("extract_claims")
          claim <- if (grepl("Expert answer", prompt, fixed = TRUE)) {
            "STORM progress emits stage events."
          } else if (
            grepl("STORM progress emits stage events", prompt, fixed = TRUE)
          ) {
            "STORM progress persists artifacts."
          } else {
            stop("Unexpected STORM product-baseline extraction prompt.")
          }
          list(
            facts = list(list(
              claim = claim,
              sources = list(list(
                source_id = source_id,
                quote = "Progress uses staged events and persisted artifacts."
              )),
              confidence = "high"
            ))
          )
        },
        chat = function(prompt, ...) "",
        register_tools = function(...) invisible(NULL)
      ))
    }
    if (identical(role, "judge")) {
      return(expect_program_prompt(
        original_chat_fn(role, model, system_prompt, echo),
        "verify_claim_support",
        "Judge whether the cited source excerpts support the claim."
      ))
    }
    original_chat_fn(role, model, system_prompt, echo)
  }
  fixture$program_stages <- function() program_stages$value
  fixture
}

storm_product_baseline_fixture <- function() {
  fixture <- storm_product_fixture()
  collector <- tempest_progress_collector(include_payload = TRUE)
  result <- tempest_run(
    "Progress events",
    config = fixture$config,
    retriever = fixture$retriever,
    n_experts = 1,
    max_questions_per_perspective = 1,
    output_dir = withr::local_tempdir(.local_envir = parent.frame()),
    run_id = "storm-product-baseline",
    progress = collector$record,
    verbose = FALSE
  )
  list(
    result = result,
    store = fixture$store,
    events = collector$data(),
    program_stages = fixture$program_stages()
  )
}

storm_resume_baseline_fixture <- function() {
  fixture <- storm_product_fixture()
  output_root <- withr::local_tempdir(.local_envir = parent.frame())
  first_events <- tempest_progress_collector(include_payload = TRUE)
  requested_steps <- tempest:::tempest_storm_stage_order()
  first <- tempest:::tempest_run_internal(
    "Progress events",
    config = fixture$config,
    retriever = fixture$retriever,
    n_experts = 1,
    max_questions_per_perspective = 1,
    steps = c("perspectives", "research"),
    .requested_steps = requested_steps,
    output_dir = output_root,
    run_id = "storm-resume-baseline",
    progress = first_events$record,
    verbose = FALSE
  )

  restored_store <- tempest_research_workspace()
  restored_events <- tempest_progress_collector(include_payload = TRUE)
  restored <- tempest:::tempest_run_internal(
    "Progress events",
    config = fixture$config,
    retriever = tempest_retriever(
      config = fixture$config,
      workspace = restored_store
    ),
    n_experts = 1,
    max_questions_per_perspective = 1,
    .requested_steps = requested_steps,
    output_dir = output_root,
    resume = TRUE,
    run_id = "storm-resume-baseline",
    progress = restored_events$record,
    verbose = FALSE
  )

  list(
    first = first,
    first_events = first_events$data(),
    restored = restored,
    restored_store = restored_store,
    restored_events = restored_events$data(),
    program_stages = fixture$program_stages()
  )
}

costorm_baseline_runtime <- function(
  source_id,
  mindmap,
  claim_texts,
  moderator_answers,
  n_mindmap_updates
) {
  claim_result <- function(claim) {
    list(
      facts = list(list(
        claim = claim,
        sources = list(list(
          source_id = source_id,
          quote = "Co-STORM preserves research evidence across dialogue."
        )),
        confidence = "high"
      ))
    )
  }
  extractor_state <- new.env(parent = emptyenv())
  extractor_state$values <- lapply(claim_texts, claim_result)
  moderator_state <- new.env(parent = emptyenv())
  moderator_state$values <- as.list(moderator_answers)
  moderator_chat <- baseline_queue_chat(text_state = moderator_state)
  config <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) {
      if (identical(role, "expert")) {
        return(fake_chat(
          text = list(paste0(
            "Expert orientation cites evidence [",
            source_id,
            "]."
          ))
        ))
      }
      if (identical(role, "mindmap")) {
        return(fake_chat(
          structured = rep(
            list(mindmap),
            n_mindmap_updates
          )
        ))
      }
      if (
        identical(role, "judge") &&
          identical(system_prompt, tempest_prompt("fact_extractor_system"))
      ) {
        return(baseline_queue_chat(structured_state = extractor_state))
      }
      if (
        identical(role, "coordinator") &&
          identical(system_prompt, tempest_prompt("question_suggester_system"))
      ) {
        return(fake_chat(
          structured = list(list(
            question = "Which evidence should be reviewed next?",
            done = FALSE
          ))
        ))
      }
      if (identical(role, "judge")) {
        return(fake_chat(
          structured = rep(
            list(list(
              status = "supported",
              score = 0.95,
              rationale = "The captured excerpt supports the exact claim."
            )),
            length(claim_texts)
          )
        ))
      }
      if (identical(role, "coordinator")) {
        return(moderator_chat)
      }
      if (identical(role, "writer")) {
        return(fake_chat(
          text = list(paste0(
            "## Findings\n\nReport cites evidence [",
            source_id,
            "]."
          ))
        ))
      }
      fake_chat()
    }
  )

  list(
    config = config,
    moderator_calls = moderator_chat$.calls
  )
}

costorm_product_baseline_fixture <- function() {
  source <- fake_source(
    url = "https://example.org/costorm-product-baseline",
    title = "Co-STORM baseline source",
    content_text = "Co-STORM preserves research evidence across dialogue."
  )
  source_id <- source@resource_id
  store <- test_research_workspace()
  store$upsert_retrieved_resource(source)
  collector <- tempest_progress_collector(include_payload = TRUE)
  mindmap <- list(
    nodes = list(list(
      id = "root",
      label = "Co-STORM baseline",
      notes = "Research evidence",
      source_ids = source_id
    )),
    edges = list()
  )
  runtime <- costorm_baseline_runtime(
    source_id = source_id,
    mindmap = mindmap,
    claim_texts = c(
      "Warmup research is preserved.",
      "Moderator research is preserved."
    ),
    moderator_answers = paste0(
      "Moderator research cites evidence [",
      source_id,
      "]."
    ),
    n_mindmap_updates = 2L
  )
  resume_runtime <- costorm_baseline_runtime(
    source_id = source_id,
    mindmap = mindmap,
    claim_texts = "Continued moderator research is preserved.",
    moderator_answers = paste0(
      "Continued moderator research cites evidence [",
      source_id,
      "]."
    ),
    n_mindmap_updates = 1L
  )
  config <- runtime$config
  session <- tempest_session(
    "Co-STORM baseline",
    config = config,
    retriever = tempest_retriever(config = config, workspace = store),
    experts = list(tempest_expert(
      name = "Dr. Baseline",
      title = "Research analyst",
      description = "Scientific product behavior",
      instructions = "Preserve evidence and uncertainty.",
      initial_questions = "What evidence establishes the baseline?"
    )),
    progress = collector$record,
    session_id = "costorm-product-baseline"
  )
  session$warmup(verbose = FALSE)
  session$step("What should we inspect?")
  questions <- session$suggest_questions(n = 1)
  tempest_verify_claims(
    session,
    verifier = fake_chat(
      structured = rep(
        list(list(
          status = "supported",
          score = 0.95,
          rationale = "The captured excerpt supports the exact claim."
        )),
        length(tempest:::tempest_session_workspace(
          session
        )$list_evidence_spans())
      )
    )
  )
  session$.__enclos_env__$private$reorganize_mindmap()
  report <- session$publish()

  list(
    config = config,
    resume_runtime = resume_runtime,
    session = session,
    store = store,
    events = collector$data(),
    questions = questions,
    report = report
  )
}

baseline_storm_semantics <- function(fixture) {
  result <- fixture$result
  events <- fixture$events
  outline_sections <- result@outline$sections
  list(
    completed_stages = baseline_succeeded_stages(events),
    program_stages = fixture$program_stages %||% character(),
    source_ids = sort(
      vapply(fixture$store$list_retrieved_sources(), `[[`, character(1), "id"),
      method = "radix"
    ),
    claims = baseline_claim_records(fixture$store),
    citations = baseline_citation_semantics(result@report_md),
    outline_sections = vapply(
      outline_sections,
      `[[`,
      character(1),
      "title"
    ),
    outline_subsections = unlist(lapply(
      outline_sections,
      function(section) {
        vapply(section$subsections, `[[`, character(1), "title")
      }
    )),
    report_sections = baseline_report_sections(result@report_md),
    terminal_status = tail(
      vapply(
        Filter(\(event) identical(event$event_type, "workflow"), events),
        `[[`,
        character(1),
        "status"
      ),
      1L
    ),
    event_sequence = baseline_event_labels(events)
  )
}

baseline_costorm_durable_state <- function(session, report) {
  list(
    session_id = session$session_id,
    source_ids = sort(
      vapply(
        tempest:::tempest_session_workspace(session)$list_retrieved_sources(),
        `[[`,
        character(1),
        "id"
      ),
      method = "radix"
    ),
    claims = baseline_claim_records(tempest:::tempest_session_workspace(
      session
    )),
    transcript = baseline_transcript_records(session$transcript),
    mindmap = baseline_mindmap_records(session$mindmap),
    report_sections = baseline_report_sections(report),
    report_citations = baseline_citation_semantics(report)
  )
}

baseline_costorm_semantics <- function(fixture) {
  session <- fixture$session
  c(
    baseline_costorm_durable_state(session, fixture$report),
    list(
      completed_stages = baseline_succeeded_stages(fixture$events),
      suggestion_count = length(fixture$questions),
      report_product_matches = identical(
        tempest_report(session),
        fixture$report
      ),
      terminal_status = tempest:::tempest_session_manifest(session)@status,
      event_sequence = baseline_event_labels(fixture$events)
    )
  )
}
