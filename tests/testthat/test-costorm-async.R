test_that("async fact extraction keeps the event loop responsive", {
  skip_if_not_installed("promises")
  skip_if_not_installed("later")
  skip_if_not_installed("ellmer")
  store <- tempest_research_workspace()
  store$upsert_retrieved_resource(fake_source("https://example.org/1"))
  source_id <- store$list_retrieved_sources()[[1]]$id
  resolve_request <- NULL
  heartbeat <- FALSE
  extractor <- list(
    chat_structured_async = function(...) {
      promises::promise(function(resolve, reject) {
        resolve_request <<- resolve
      })
    }
  )
  session <- list(
    session_id = "async-session",
    programs = test_program_executions(run_id = "async-session"),
    workspace = store,
    chats = list(extractor = extractor),
    emit_progress = function(
      event_type,
      status,
      stage = NA_character_,
      step = NA_character_,
      message = NA_character_,
      payload = list(),
      parent_event_id = NA_character_,
      correlation_id = NA_character_
    ) {
      tempest_progress_event(
        run_id = "async-session",
        workflow = "costorm",
        event_type = event_type,
        status = status,
        stage = stage,
        step = step,
        message = message,
        payload = payload,
        parent_event_id = parent_event_id,
        correlation_id = correlation_id
      )
    }
  )
  local_mocked_bindings(
    tempest_session_programs = function(session) session$programs
  )

  request <- tempest:::tempest_session_extract_facts_async(
    session,
    paste0("Responsive claim [", source_id, "]."),
    source_ids = source_id
  )
  later::later(function() heartbeat <<- TRUE, delay = 0)
  later::run_now(0.02)

  expect_equal(heartbeat, TRUE)
  expect_equal(is.null(resolve_request), FALSE)
  expect_length(store$list_proposed_claims(), 0L)
  resolve_request(list(
    facts = list(list(
      claim = "Responsive claim",
      sources = list(list(source_id = source_id)),
      confidence = "high",
      support_score = 0.9
    ))
  ))
  settled <- await_tempest_promise(request)

  expect_null(settled$error)
  expect_equal(store$list_proposed_claims()[[1]]@claim_text, "Responsive claim")
})

test_that("stale async evidence cannot commit or report success", {
  skip_if_not_installed("promises")
  skip_if_not_installed("later")
  skip_if_not_installed("ellmer")
  store <- fake_store_with_sources(1)
  source_id <- store$list_retrieved_sources()[[1]]$id
  resolve_request <- NULL
  current <- TRUE
  session <- list(
    session_id = "stale-session",
    programs = test_program_executions(run_id = "stale-session"),
    workspace = store,
    chats = list(
      extractor = list(
        chat_structured_async = function(...) {
          promises::promise(function(resolve, reject) {
            resolve_request <<- resolve
          })
        }
      )
    ),
    emit_progress = function(
      event_type,
      status,
      stage = NA_character_,
      step = NA_character_,
      message = NA_character_,
      payload = list(),
      parent_event_id = NA_character_,
      correlation_id = NA_character_
    ) {
      tempest_progress_event(
        run_id = "stale-session",
        workflow = "costorm",
        event_type = event_type,
        status = status,
        stage = stage,
        step = step,
        message = message,
        payload = payload,
        parent_event_id = parent_event_id,
        correlation_id = correlation_id
      )
    }
  )
  local_mocked_bindings(
    tempest_session_programs = function(session) session$programs
  )
  request <- tempest:::tempest_session_commit_evidence_async(
    session,
    paste0("Stale claim [", source_id, "]."),
    source_ids = source_id,
    is_current = function() current
  )
  current <- FALSE
  resolve_request(list(
    facts = list(list(
      claim = "Stale claim",
      sources = list(list(source_id = source_id)),
      confidence = "high"
    ))
  ))
  settled <- await_tempest_promise(request)

  expect_null(settled$error)
  expect_length(store$list_proposed_claims(), 0L)
  expect_identical(settled$value$cancelled, TRUE)
})

test_that("evidence commitment skips extraction when a turn cites no source", {
  skip_if_not_installed("promises")
  skip_if_not_installed("later")
  extractor_calls <- 0L
  events <- list()
  workspace <- tempest_research_workspace()
  session <- list(
    session_id = "unsupported-session",
    programs = test_program_executions(run_id = "unsupported-session"),
    workspace = workspace,
    chats = list(
      extractor = list(
        chat_structured_async = function(...) {
          extractor_calls <<- extractor_calls + 1L
          promises::promise_resolve(list(facts = list()))
        }
      )
    ),
    emit_progress = function(
      event_type,
      status,
      stage = NA_character_,
      step = NA_character_,
      message = NA_character_,
      payload = list(),
      parent_event_id = NA_character_,
      correlation_id = NA_character_
    ) {
      event <- tempest_progress_event(
        run_id = "unsupported-session",
        workflow = "costorm",
        event_type = event_type,
        status = status,
        stage = stage,
        step = step,
        message = message,
        payload = payload,
        parent_event_id = parent_event_id,
        correlation_id = correlation_id
      )
      events[[length(events) + 1L]] <<- event
      event
    }
  )
  local_mocked_bindings(
    tempest_session_programs = function(session) session$programs
  )

  request <- tempest:::tempest_session_commit_evidence_async(
    session,
    "An unsupported answer.",
    correlation_id = "turn-unsupported"
  )
  settled <- await_tempest_promise(request)

  expect_null(settled$error)
  expect_equal(extractor_calls, 0L)
  expect_identical(settled$value$extraction_skipped, "no_cited_sources")
  expect_length(settled$value$source_ids, 0L)
  expect_equal(events[[1]]@status, "skipped")
  expect_identical(events[[1]]@payload$reason, "no_cited_sources")
})

test_that("evidence commitment extracts claims from cited session sources", {
  skip_if_not_installed("promises")
  skip_if_not_installed("later")
  store <- fake_store_with_sources(1)
  source_id <- store$list_retrieved_sources()[[1]]$id
  extractor <- list(
    chat_structured_async = function(...) {
      promises::promise_resolve(list(
        facts = list(list(
          claim = "A cited claim",
          sources = list(list(source_id = source_id)),
          confidence = "high"
        ))
      ))
    }
  )
  session <- list(
    session_id = "cited-session",
    programs = test_program_executions(run_id = "cited-session"),
    workspace = store,
    chats = list(extractor = extractor),
    emit_progress = function(
      event_type,
      status,
      stage = NA_character_,
      step = NA_character_,
      message = NA_character_,
      payload = list(),
      parent_event_id = NA_character_,
      correlation_id = NA_character_
    ) {
      tempest_progress_event(
        run_id = "cited-session",
        workflow = "costorm",
        event_type = event_type,
        status = status,
        stage = stage,
        step = step,
        message = message,
        payload = payload,
        parent_event_id = parent_event_id,
        correlation_id = correlation_id
      )
    }
  )
  local_mocked_bindings(
    tempest_session_programs = function(session) session$programs
  )

  request <- tempest:::tempest_session_commit_evidence_async(
    session,
    paste0("A cited claim [", source_id, "].")
  )
  settled <- await_tempest_promise(request)

  expect_null(settled$error)
  expect_equal(settled$value$source_ids, source_id)
  expect_equal(settled$value$claims_added, 1L)
  expect_equal(store$list_proposed_claims()[[1]]@claim_text, "A cited claim")
})

test_that("async ProgramSet execution preserves authoritative metadata", {
  skip_if_not_installed("promises")
  skip_if_not_installed("later")
  config <- tempest_config()
  program_set <- tempest_program_set()
  manifest <- tempest_research_manifest(
    research_run_id = "costorm-async-metadata",
    mode = "costorm",
    config = config,
    programs = tempest:::tempest_program_set_manifest_programs(program_set)
  )
  program <- tempest:::tempest_bind_program_set(
    program_set,
    manifest
  )$extract_claims
  chat <- list(
    chat_structured_async = function(...) {
      promises::promise_resolve(list(facts = list()))
    }
  )

  request <- tempest:::tempest_run_dsprrr_module_async(
    program,
    chat,
    inputs = list(
      answer_text = "No cited claim.",
      source_context = "",
      source_ids = "",
      citation_mode = "tempest_inline"
    ),
    step = "extract_claims"
  )
  metadata <- attr(request, "dsprrr_trace_context", exact = TRUE)
  settled <- await_tempest_promise(request)

  expect_null(settled$error)
  expect_identical(metadata$program_artifact_id, program$program_artifact_id)
  expect_identical(metadata$trace_context, program$trace_context)
  expect_identical(metadata$trace_context$mode, "costorm")
  expect_identical(
    metadata$trace_context$research_run_id,
    "costorm-async-metadata"
  )
  expect_disjoint(names(metadata$trace_context), "program_artifact_id")
})

test_that("async ProgramSet execution rejects tampered handle metadata", {
  skip_if_not_installed("promises")
  config <- tempest_config()
  program_set <- tempest_program_set()
  manifest <- tempest_research_manifest(
    research_run_id = "costorm-async-tamper",
    mode = "costorm",
    config = config,
    programs = tempest:::tempest_program_set_manifest_programs(program_set)
  )
  program <- tempest:::tempest_bind_program_set(
    program_set,
    manifest
  )$extract_claims
  tamper <- "program_artifact_id"
  local_mocked_bindings(
    tempest_dsprrr_run_async = function(
      module,
      ...,
      .llm = NULL,
      .trace_context = list()
    ) {
      request <- promises::promise_resolve(list(facts = list()))
      metadata <- list(
        program_artifact_id = dsprrr::program_artifact_id(module),
        trace_context = .trace_context
      )
      if (identical(tamper, "program_artifact_id")) {
        metadata$program_artifact_id <- paste0("sha256:", strrep("0", 64L))
      } else {
        metadata$trace_context$stage <- "personas"
      }
      attr(request, "dsprrr_trace_context") <- metadata
      request
    }
  )
  run <- function() {
    tempest:::tempest_run_dsprrr_module_async(
      program,
      chat = list(),
      inputs = list(),
      step = "extract_claims"
    )
  }

  expect_error(
    run(),
    class = "tempest_ecosystem_contract_error",
    regexp = "bound program artifact"
  )
  tamper <- "trace_context"
  expect_error(
    run(),
    class = "tempest_ecosystem_contract_error",
    regexp = "bound Tempest trace context"
  )
})

test_that("async personas and Shiny startup use one bound ProgramSet", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("promises")
  skip_if_not_installed("later")
  generated <- list(
    personas = list(list(
      name = "Dr. Async",
      title = "Systems researcher",
      affiliation = "Independent",
      background = "Studies asynchronous research systems.",
      focus_areas = list("runtime contracts"),
      perspective = "Execution integrity",
      initial_questions = list("Which program executed?")
    ))
  )
  calls <- 0L
  chat <- list(
    chat_structured_async = function(...) {
      calls <<- calls + 1L
      promises::promise_resolve(generated)
    }
  )
  config <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) chat
  )
  program_set <- tempest_program_set()
  program <- tempest:::tempest_costorm_program_execution(
    program_set,
    "personas",
    "shiny-async-personas"
  )

  settled <- await_tempest_promise(
    tempest:::tempest_generate_experts_async(
      "Async research systems",
      n = 1L,
      config = config,
      program = program
    )
  )

  expect_null(settled$error)
  expect_equal(calls, 1L)
  expect_s3_class(settled$value, "tempest_persona_stage_result")
  expect_length(settled$value$experts, 1L)
  expect_identical(settled$value$experts[[1]]@name, "Dr. Async")
  expect_identical(settled$value$record@status, "succeeded")
  expect_identical(settled$value$record@stage, "personas")
  expect_identical(
    program$trace_context$research_run_id,
    "shiny-async-personas"
  )
  expect_identical(program$trace_context$stage, "personas")
  expect_disjoint(names(program$trace_context), "program_artifact_id")

  shiny_path <- system.file("shiny", "R", "mod_chat.R", package = "tempest")
  skip_if(identical(shiny_path, ""), "Shiny app module is unavailable")
  shiny_code <- paste(readLines(shiny_path, warn = FALSE), collapse = "\n")
  expect_match(
    shiny_code,
    "program = personas_program",
    fixed = TRUE
  )
  expect_match(
    shiny_code,
    "program_set = program_set_value",
    fixed = TRUE
  )
  expect_match(
    shiny_code,
    "tempest_session_set_stage_records(value, stage_records)",
    fixed = TRUE
  )
  session_config <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  session <- tempest_session(
    "Async research systems",
    config = session_config,
    experts = settled$value$experts,
    session_id = "shiny-async-personas",
    program_set = program_set
  )
  tempest:::tempest_session_set_stage_records(
    session,
    list(settled$value$record)
  )
  seeded <- tempest:::tempest_session_stage_records(session)
  expect_length(seeded, 1L)
  expect_identical(seeded[[1]]@attempt_id, settled$value$record@attempt_id)
  expect_no_error(tempest_session_snapshot(session))
})

test_that("stale async extraction persists one cancelled attempt", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("later")
  skip_if_not_installed("promises")
  resolve_request <- NULL
  current <- TRUE
  extractor <- list(
    chat_structured_async = function(...) {
      promises::promise(function(resolve, reject) {
        resolve_request <<- resolve
      })
    }
  )
  config <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) {
      if (identical(role, "judge")) extractor else fake_chat()
    }
  )
  session <- tempest_session(
    "Cancelled evidence",
    config = config,
    experts = list(test_expert(
      expert_id = "expert.cancelled",
      name = "Cancellation expert"
    )),
    session_id = "costorm-cancelled-extraction"
  )
  source <- fake_source("https://example.org/cancelled")
  session$workspace$upsert_retrieved_resource(source)

  request <- tempest:::tempest_session_extract_facts_async(
    session,
    paste0("Cancelled claim [", source$id, "]."),
    source_ids = source$id,
    is_current = function() current
  )
  later::run_now(0.02)
  running <- tempest:::tempest_session_stage_records(session)

  expect_length(running, 1L)
  expect_identical(running[[1]]@status, "running")
  current <- FALSE
  resolve_request(list(
    facts = list(list(
      claim = "Cancelled claim",
      sources = list(list(source_id = source$id)),
      confidence = "high",
      support_score = 0.9
    ))
  ))
  settled <- await_tempest_promise(request)
  terminal <- tempest:::tempest_session_stage_records(session)

  expect_null(settled$error)
  expect_length(terminal, 1L)
  expect_identical(terminal[[1]]@attempt_id, running[[1]]@attempt_id)
  expect_identical(terminal[[1]]@status, "cancelled")
  expect_identical(
    terminal[[1]]@failure_message,
    "Stage execution was cancelled."
  )
  expect_length(session$workspace$list_proposed_claims(), 0L)
})

test_that("failed async extraction remains durable without raw errors", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("later")
  skip_if_not_installed("promises")
  extractor <- list(
    chat_structured_async = function(...) {
      promises::promise_reject(simpleError(
        "Authorization: Bearer sk-live-secret"
      ))
    }
  )
  config <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) {
      if (identical(role, "judge")) extractor else fake_chat()
    }
  )
  session <- tempest_session(
    "Failed evidence",
    config = config,
    experts = list(test_expert(
      expert_id = "expert.failed",
      name = "Failure expert"
    )),
    session_id = "costorm-failed-extraction"
  )
  source <- fake_source("https://example.org/failed")
  session$workspace$upsert_retrieved_resource(source)

  settled <- await_tempest_promise(
    tempest:::tempest_session_extract_facts_async(
      session,
      paste0("Failed claim [", source$id, "]."),
      source_ids = source$id
    )
  )
  records <- tempest:::tempest_session_stage_records(session)

  expect_s3_class(settled$error, "tempest_session_error")
  expect_no_match(
    conditionMessage(settled$error),
    "sk-live-secret",
    fixed = TRUE
  )
  printed <- paste(capture.output(print(settled$error)), collapse = "\n")
  expect_no_match(printed, "sk-live-secret", fixed = TRUE)
  expect_length(records, 1L)
  expect_identical(records[[1]]@status, "failed")
  expect_identical(
    records[[1]]@failure_message,
    "Primary stage execution failed."
  )
  expect_no_match(
    jsonlite::toJSON(tempest:::tempest_stage_record_data(records[[1]])),
    "sk-live-secret",
    fixed = TRUE
  )
  expect_no_match(
    tempest:::tempest_canonical_json(lapply(
      Filter(
        \(event) S7::S7_inherits(event, tempest_progress_event),
        session$events
      ),
      tempest_progress_event_data
    )),
    "sk-live-secret",
    fixed = TRUE
  )
  expect_length(session$workspace$list_proposed_claims(), 0L)
})

test_that("post-turn processing owns sequencing and returns typed results", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("later")
  skip_if_not_installed("promises")
  calls <- character()
  events <- list()
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  session <- tempest_session(
    "Typed async turns",
    config = cfg,
    experts = list(test_expert(
      expert_id = "expert.turn",
      name = "Dr. Turn"
    )),
    progress = function(event) {
      events[[length(events) + 1L]] <<- event
    }
  )
  source <- fake_source()
  session$workspace$upsert_retrieved_resource(source)
  source_id <- source$id
  local_mocked_bindings(
    tempest_session_commit_evidence_async = function(session, ...) {
      calls <<- c(calls, "evidence")
      session$workspace$add_proposed_claim(tempest_claim(
        claim_text = "A cited answer",
        source_ids = source_id
      ))
      promises::promise_resolve(list(
        source_count = 1L,
        claim_count = 1L,
        source_ids = source_id,
        sources_added = 1L,
        claims_added = 1L,
        cancelled = FALSE
      ))
    },
    tempest_session_update_mindmap_async = function(session, ...) {
      calls <<- c(calls, "mindmap")
      session$mindmap <- list(
        nodes = list(list(
          id = "root",
          label = "Typed async turns",
          parent = NULL,
          notes = "Updated",
          source_ids = source_id
        )),
        edges = list()
      )
      promises::promise_resolve(TRUE)
    },
    tempest_session_suggest_questions_async = function(...) {
      calls <<- c(calls, "suggestions")
      promises::promise_resolve(c(
        "What evidence remains?",
        "How can it be verified?"
      ))
    }
  )

  request <- tempest_session_process_turn_async(
    session,
    user_text = "What is known?",
    assistant_text = paste0("A cited answer [", source_id, "]."),
    turn_id = "turn-typed"
  )
  settled <- await_tempest_promise(request)

  expect_null(settled$error)
  expect_equal(calls, c("evidence", "mindmap", "suggestions"))
  expect_equal(
    vapply(session$transcript, `[[`, character(1), "role"),
    c("user", "assistant")
  )
  expect_equal(
    S7::S7_inherits(
      settled$value,
      tempest:::TempestSessionTurnResult
    ),
    TRUE
  )
  expect_equal(settled$value@status, "succeeded")
  expect_equal(settled$value@evidence_status, "committed")
  expect_equal(settled$value@mindmap_status, "updated")
  expect_equal(settled$value@suggestion_status, "generated")
  expect_equal(
    settled$value@suggestions,
    c("What evidence remains?", "How can it be verified?")
  )
  expect_equal(settled$value@claims_added, 1L)
  expect_equal(
    vapply(
      Filter(
        \(event) identical(event@correlation_id, "turn-typed"),
        events
      ),
      \(event) event@status,
      character(1)
    ),
    c("started", "succeeded", "succeeded", "succeeded")
  )
  expect_no_error(tempest:::tempest_canonical_json(
    tempest:::tempest_session_turn_result_data(settled$value)
  ))
})

test_that("post-turn processing exposes evidence gaps without UI callbacks", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("later")
  skip_if_not_installed("promises")
  exchange <- NULL
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  session <- tempest_session(
    "Evidence gaps",
    config = cfg,
    experts = list(test_expert(
      expert_id = "expert.gap",
      name = "Dr. Gap"
    ))
  )
  local_mocked_bindings(
    tempest_session_commit_evidence_async = function(...) {
      promises::promise_resolve(list(
        source_count = 0L,
        claim_count = 0L,
        source_ids = character(),
        sources_added = 0L,
        claims_added = 0L,
        cancelled = FALSE
      ))
    },
    tempest_session_update_mindmap_async = function(
      session,
      last_exchange,
      ...
    ) {
      exchange <<- last_exchange
      promises::promise_resolve(TRUE)
    }
  )

  request <- tempest_session_process_turn_async(
    session,
    user_text = "What is known?",
    assistant_text = "An unsupported factual answer.",
    suggest = FALSE,
    turn_id = "turn-gap"
  )
  settled <- await_tempest_promise(request)

  expect_null(settled$error)
  expect_equal(settled$value@status, "succeeded")
  expect_equal(settled$value@evidence_status, "gap")
  expect_equal(settled$value@suggestion_status, "skipped")
  expect_equal(settled$value@notices[[1]]@code, "evidence_gap")
  expect_equal(settled$value@notices[[1]]@severity, "info")
  expect_match(exchange, "Record only the question", fixed = TRUE)
  expect_no_match(exchange, "unsupported factual answer", fixed = TRUE)
})

test_that("post-turn enrichment failures are typed and best effort", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("later")
  skip_if_not_installed("promises")
  calls <- character()
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  session <- tempest_session(
    "Partial async turn",
    config = cfg,
    experts = list(test_expert(
      expert_id = "expert.partial",
      name = "Dr. Partial"
    ))
  )
  local_mocked_bindings(
    tempest_session_commit_evidence_async = function(...) {
      calls <<- c(calls, "evidence")
      promises::promise_reject(simpleError("extractor failed"))
    },
    tempest_session_update_mindmap_async = function(...) {
      calls <<- c(calls, "mindmap")
      promises::promise_reject(simpleError("mind map failed"))
    },
    tempest_session_suggest_questions_async = function(...) {
      calls <<- c(calls, "suggestions")
      promises::promise_reject(simpleError("suggestions failed"))
    }
  )

  request <- tempest_session_process_turn_async(
    session,
    user_text = "Continue?",
    assistant_text = "A response.",
    turn_id = "turn-partial"
  )
  settled <- await_tempest_promise(request)

  expect_null(settled$error)
  expect_equal(calls, c("evidence", "mindmap", "suggestions"))
  expect_equal(settled$value@status, "partial")
  expect_equal(settled$value@evidence_status, "failed")
  expect_equal(settled$value@mindmap_status, "failed")
  expect_equal(settled$value@suggestion_status, "failed")
  expect_equal(
    vapply(settled$value@notices, \(notice) notice@code, character(1)),
    c("evidence_failed", "mindmap_failed", "suggestions_failed")
  )
  expect_equal(
    vapply(
      settled$value@notices,
      \(notice) is.list(notice@details),
      logical(1)
    ),
    rep(TRUE, 3L)
  )
})

test_that("stale post-turn work cannot run later enrichment stages", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("later")
  skip_if_not_installed("promises")
  resolve_evidence <- NULL
  current <- TRUE
  later_stage_calls <- 0L
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  session <- tempest_session(
    "Stale async turn",
    config = cfg,
    experts = list(test_expert(
      expert_id = "expert.stale-turn",
      name = "Dr. Stale"
    ))
  )
  local_mocked_bindings(
    tempest_session_commit_evidence_async = function(...) {
      promises::promise(function(resolve, reject) {
        resolve_evidence <<- resolve
      })
    },
    tempest_session_update_mindmap_async = function(...) {
      later_stage_calls <<- later_stage_calls + 1L
      promises::promise_resolve(TRUE)
    },
    tempest_session_suggest_questions_async = function(...) {
      later_stage_calls <<- later_stage_calls + 1L
      promises::promise_resolve("Late suggestion")
    }
  )

  request <- tempest_session_process_turn_async(
    session,
    user_text = "Will this be stale?",
    assistant_text = "This turn has completed.",
    is_current = function() current,
    turn_id = "turn-stale"
  )
  later::run_now(0.02)
  current <- FALSE
  resolve_evidence(list(
    source_count = 0L,
    claim_count = 0L,
    source_ids = character(),
    sources_added = 0L,
    claims_added = 0L,
    cancelled = FALSE
  ))
  settled <- await_tempest_promise(request)

  expect_null(settled$error)
  expect_equal(settled$value@status, "cancelled")
  expect_equal(settled$value@suggestion_status, "cancelled")
  expect_length(settled$value@suggestions, 0L)
  expect_equal(later_stage_calls, 0L)
  expect_length(session$transcript, 2L)
})

test_that("stale mind-map failures are recorded as cancellation", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("later")
  skip_if_not_installed("promises")
  reject_request <- NULL
  current <- TRUE
  events <- list()
  session <- list(
    session_id = "stale-map-session",
    topic = "Stale map",
    mindmap = tempest:::tempest_mindmap_init("Stale map"),
    chats = list(
      mindmap = list(
        chat_structured_async = function(...) {
          promises::promise(function(resolve, reject) {
            reject_request <<- reject
          })
        }
      )
    ),
    emit_progress = function(
      event_type,
      status,
      stage = NA_character_,
      step = NA_character_,
      message = NA_character_,
      payload = list(),
      parent_event_id = NA_character_,
      correlation_id = NA_character_
    ) {
      event <- tempest_progress_event(
        run_id = "stale-map-session",
        workflow = "costorm",
        event_type = event_type,
        status = status,
        stage = stage,
        step = step,
        message = message,
        payload = payload,
        parent_event_id = parent_event_id,
        correlation_id = correlation_id
      )
      events[[length(events) + 1L]] <<- event
      event
    }
  )

  request <- tempest:::tempest_session_update_mindmap_async(
    session,
    "Late exchange",
    is_current = function() current
  )
  current <- FALSE
  reject_request(simpleError("provider timed out"))
  settled <- await_tempest_promise(request)

  expect_null(settled$error)
  expect_equal(
    vapply(events, \(event) event@status, character(1)),
    c("started", "cancelled")
  )
})

test_that("async mind-map updates reject unbound evidence atomically", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("later")
  skip_if_not_installed("promises")
  invalid_map <- list(
    nodes = list(
      list(id = "root", label = "Async map", source_ids = character()),
      list(
        id = "finding",
        label = "Unbound finding",
        parent = "root",
        source_ids = "Sffffffffffff"
      )
    ),
    edges = list(list(from = "root", to = "finding", relation = "contains"))
  )
  config <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  session <- tempest_session(
    "Async map",
    config = config,
    experts = list(test_expert(
      expert_id = "expert.async-map",
      name = "Async Map Expert"
    )),
    session_id = "async-map-integrity"
  )
  session$chats$mindmap$chat_structured_async <- function(...) {
    promises::promise_resolve(invalid_map)
  }
  original <- session$mindmap

  settled <- await_tempest_promise(
    tempest:::tempest_session_update_mindmap_async(
      session,
      "An unsupported finding appeared."
    )
  )

  expect_s3_class(settled$error, "tempest_session_mindmap_error")
  expect_identical(session$mindmap, original)
  mindmap_events <- Filter(
    function(event) {
      identical(event$stage, "mindmap") &&
        identical(event$step, "update")
    },
    session$events
  )
  expect_equal(
    vapply(
      mindmap_events,
      function(event) {
        event$status
      },
      character(1)
    ),
    c("started", "failed")
  )
})

test_that("stale suggestion failures are recorded as cancellation", {
  skip_if_not_installed("later")
  skip_if_not_installed("promises")
  reject_request <- NULL
  current <- TRUE
  events <- list()
  session <- list(
    session_id = "stale-suggestions",
    topic = "Stale suggestions",
    transcript = list(),
    config = tempest_config(),
    transcript_markdown = function(max_turns = 12) "Conversation",
    emit_progress = function(
      event_type,
      status,
      stage = NA_character_,
      step = NA_character_,
      message = NA_character_,
      payload = list(),
      parent_event_id = NA_character_,
      correlation_id = NA_character_
    ) {
      event <- tempest_progress_event(
        run_id = "stale-suggestions",
        workflow = "costorm",
        event_type = event_type,
        status = status,
        stage = stage,
        step = step,
        message = message,
        payload = payload,
        parent_event_id = parent_event_id,
        correlation_id = correlation_id
      )
      events[[length(events) + 1L]] <<- event
      event
    }
  )
  local_mocked_bindings(
    tempest_suggest_questions_async = function(...) {
      promises::promise(function(resolve, reject) {
        reject_request <<- reject
      })
    }
  )

  request <- tempest:::tempest_session_suggest_questions_async(
    session,
    is_current = function() current
  )
  current <- FALSE
  reject_request(simpleError("provider timed out"))
  settled <- await_tempest_promise(request)

  expect_null(settled$error)
  expect_length(settled$value, 0L)
  expect_equal(
    vapply(events, \(event) event@status, character(1)),
    c("started", "cancelled")
  )
})

test_that("async report generation commits only after provider settlement", {
  skip_if_not_installed("promises")
  skip_if_not_installed("later")
  store <- fake_store_with_sources(1)
  source_id <- store$list_retrieved_sources()[[1]]$id
  store$add_proposed_claim(tempest_claim(
    claim_text = "Report claim",
    source_ids = source_id,
    verification_status = "supported",
    support_score = 0.9
  ))
  resolve_report <- NULL
  heartbeat <- FALSE
  async_prompt <- NULL
  artifacts <- new.env(parent = emptyenv())
  artifact_catalog <- tempest_artifact_catalog()
  cfg <- tempest_config()
  session <- list(
    topic = "Async report",
    title = "Async report",
    config = cfg,
    workspace = store,
    mindmap = tempest:::tempest_mindmap_init("Async report"),
    transcript = list(),
    artifacts = artifacts,
    transcript_markdown = function(max_turns = 80) "Conversation",
    chats = list(
      reporter = list(
        chat_async = function(prompt) {
          async_prompt <<- prompt
          promises::promise(function(resolve, reject) {
            resolve_report <<- resolve
          })
        }
      )
    ),
    emit_progress = function(
      event_type,
      status,
      stage = NA_character_,
      step = NA_character_,
      message = NA_character_,
      payload = list(),
      parent_event_id = NA_character_,
      correlation_id = NA_character_
    ) {
      tempest_progress_event(
        run_id = "report-session",
        workflow = "costorm",
        event_type = event_type,
        status = status,
        stage = stage,
        step = step,
        message = message,
        payload = payload,
        parent_event_id = parent_event_id,
        correlation_id = correlation_id
      )
    }
  )
  local_mocked_bindings(
    tempest_costorm_report_context = function(
      session,
      style,
      include_references
    ) {
      list(
        prompt = tempest:::tempest_costorm_report_prompt(session, style),
        title = session$title,
        workspace = session$workspace,
        include_references = include_references,
        citation_policy = session$config@citation_policy,
        on_unsupported_claim = session$config@on_unsupported_claim,
        min_support_score = session$config@min_support_score,
        execution_review = "",
        style = style
      )
    }
  )

  request <- tempest:::tempest_session_report_async(
    session,
    .artifact_catalog = artifact_catalog
  )
  later::later(function() heartbeat <<- TRUE, delay = 0)
  later::run_now(0.02)

  expect_equal(heartbeat, TRUE)
  expect_null(artifacts[["report_md"]])
  expect_identical(artifact_catalog$has("report_md"), FALSE)
  resolve_report(paste0("Report claim [", source_id, "]."))
  settled <- await_tempest_promise(request)

  expect_null(settled$error)
  expect_equal(
    async_prompt,
    tempest:::tempest_costorm_report_prompt(session, "technical")
  )
  report_md <- artifact_catalog$get("report_md")@content
  expect_null(artifacts[["report"]])
  expect_null(artifacts[["report_md"]])
  expect_match(report_md, "# Async report", fixed = TRUE)
  expect_match(
    report_md,
    paste0("[^", source_id, "]"),
    fixed = TRUE
  )
  expect_equal(settled$value, report_md)
})

test_that("Co-STORM report generation requires the session catalog", {
  expect_error(
    tempest:::tempest_costorm_artifact_catalog(list()),
    class = "tempest_deliverable_execution_error"
  )
})

test_that("synchronous post-turn setup failures return typed notices", {
  skip_if_not_installed("later")
  skip_if_not_installed("promises")
  stage <- "evidence"
  local_mocked_bindings(
    tempest_session_commit_evidence_async = function(...) {
      if (identical(stage, "evidence")) {
        stop("synchronous evidence failure")
      }
      promises::promise_resolve(list(
        source_count = 0L,
        claim_count = 0L,
        source_ids = character(),
        sources_added = 0L,
        claims_added = 0L,
        cancelled = FALSE
      ))
    },
    tempest_session_update_mindmap_async = function(...) {
      if (identical(stage, "mindmap")) {
        stop("synchronous mind-map failure")
      }
      promises::promise_resolve(TRUE)
    },
    tempest_session_suggest_questions_async = function(...) {
      if (identical(stage, "suggestions")) {
        stop("synchronous suggestion failure")
      }
      promises::promise_resolve("What next?")
    }
  )
  expected_codes <- c(
    evidence = "evidence_failed",
    mindmap = "mindmap_failed",
    suggestions = "suggestions_failed"
  )

  for (stage_name in names(expected_codes)) {
    stage <- stage_name
    cfg <- tempest_config(
      chat_fn = function(role, model, system_prompt, echo) fake_chat()
    )
    session <- tempest_session(
      paste("Synchronous", stage_name, "failure"),
      config = cfg,
      experts = list(test_expert(
        expert_id = paste0("expert.sync-", stage_name),
        name = "Dr. Sync"
      ))
    )
    settled <- await_tempest_promise(tempest_session_process_turn_async(
      session,
      user_text = "Continue?",
      assistant_text = "A response.",
      turn_id = paste0("turn-sync-", stage_name)
    ))

    expect_null(settled$error)
    expect_identical(settled$value@status, "partial")
    expect_contains(
      vapply(
        settled$value@notices,
        \(notice) notice@code,
        character(1)
      ),
      expected_codes[[stage_name]]
    )
  }
})

test_that("turn result validators reject contradictory records", {
  notice_error <- tryCatch(
    tempest:::tempest_session_turn_notice(
      code = "evidence_failed",
      stage = "mindmap",
      message = "Evidence failed."
    ),
    error = identity
  )
  expect_s3_class(notice_error, "error")
  expect_match(conditionMessage(notice_error), "code and stage")

  evidence_notice <- tempest:::tempest_session_turn_notice(
    code = "evidence_failed",
    stage = "evidence",
    message = "Evidence failed."
  )
  succeeded_error <- tryCatch(
    tempest:::tempest_session_turn_result(
      session_id = "validation-session",
      turn_id = "validation-turn",
      status = "succeeded",
      evidence_status = "failed",
      mindmap_status = "updated",
      suggestion_status = "skipped",
      notices = list(evidence_notice)
    ),
    error = identity
  )
  expect_s3_class(succeeded_error, "error")
  expect_match(conditionMessage(succeeded_error), "succeeded results")

  missing_notice_error <- tryCatch(
    tempest:::tempest_session_turn_result(
      session_id = "validation-session",
      turn_id = "validation-turn",
      status = "partial",
      evidence_status = "failed",
      mindmap_status = "updated",
      suggestion_status = "skipped"
    ),
    error = identity
  )
  expect_s3_class(missing_notice_error, "error")
  expect_match(conditionMessage(missing_notice_error), "matching warning")
})

test_that("async report finalization failures emit failed progress", {
  skip_if_not_installed("promises")
  skip_if_not_installed("later")
  resolve_report <- NULL
  events <- list()
  artifacts <- new.env(parent = emptyenv())
  artifact_catalog <- tempest_artifact_catalog()
  cfg <- tempest_config()
  workspace <- tempest_research_workspace()
  session <- list(
    session_id = "failed-report",
    topic = "Failed report",
    title = "Failed report",
    config = cfg,
    workspace = workspace,
    mindmap = tempest:::tempest_mindmap_init("Failed report"),
    artifacts = artifacts,
    transcript_markdown = function(max_turns = 80) "Conversation",
    chats = list(
      reporter = list(
        chat_async = function(...) {
          promises::promise(function(resolve, reject) {
            resolve_report <<- resolve
          })
        }
      )
    ),
    emit_progress = function(
      event_type,
      status,
      stage = NA_character_,
      step = NA_character_,
      message = NA_character_,
      payload = list(),
      parent_event_id = NA_character_,
      correlation_id = NA_character_
    ) {
      event <- tempest_progress_event(
        run_id = "failed-report",
        workflow = "costorm",
        event_type = event_type,
        status = status,
        stage = stage,
        step = step,
        message = message,
        payload = payload,
        parent_event_id = parent_event_id,
        correlation_id = correlation_id
      )
      events <<- c(events, list(event))
      event
    }
  )
  report_spec <- tempest:::tempest_costorm_report_spec(session)
  existing <- tempest_artifact(
    report_spec,
    content = "# Existing report",
    artifact_id = "report_md",
    producer_operation_id = "tempest.renderer.markdown_report",
    run_id = "another-run",
    step_id = "report",
    status = "valid"
  )
  artifact_catalog$register(report_spec)
  artifact_catalog$add(existing)
  local_mocked_bindings(
    tempest_costorm_report_context = function(
      session,
      style,
      include_references
    ) {
      list(
        prompt = tempest:::tempest_costorm_report_prompt(session, style),
        title = session$title,
        workspace = session$workspace,
        include_references = include_references,
        citation_policy = session$config@citation_policy,
        on_unsupported_claim = session$config@on_unsupported_claim,
        min_support_score = session$config@min_support_score,
        execution_review = "",
        style = style
      )
    }
  )

  request <- tempest:::tempest_session_report_async(
    session,
    .artifact_catalog = artifact_catalog
  )
  resolve_report("Replacement report body")
  settled <- await_tempest_promise(request)

  expect_s3_class(settled$error, "tempest_deliverable_execution_error")
  expect_identical(artifact_catalog$get("report_md"), existing)
  expect_null(artifacts[["report"]])
  expect_null(artifacts[["report_md"]])
  failed <- Filter(
    function(event) {
      identical(event@stage, "report") &&
        identical(event@status, "failed")
    },
    events
  )
  expect_length(failed, 1L)
  succeeded <- Filter(
    function(event) {
      identical(event@stage, "report") &&
        identical(event@status, "succeeded")
    },
    events
  )
  expect_length(succeeded, 0L)
  available <- Filter(
    function(event) {
      identical(event@event_type, "artifact") &&
        identical(event@status, "available")
    },
    events
  )
  expect_length(available, 0L)
})

test_that("stale async reports do not publish artifacts", {
  skip_if_not_installed("promises")
  skip_if_not_installed("later")
  resolve_report <- NULL
  reject_report <- NULL
  report_calls <- 0L
  current <- TRUE
  events <- list()
  artifacts <- new.env(parent = emptyenv())
  artifact_catalog <- tempest_artifact_catalog()
  cfg <- tempest_config()
  workspace <- tempest_research_workspace()
  session <- list(
    session_id = "stale-report",
    topic = "Stale report",
    title = "Stale report",
    config = cfg,
    workspace = workspace,
    mindmap = tempest:::tempest_mindmap_init("Stale report"),
    artifacts = artifacts,
    transcript_markdown = function(max_turns = 80) "Conversation",
    chats = list(
      reporter = list(
        chat_async = function(...) {
          report_calls <<- report_calls + 1L
          promises::promise(function(resolve, reject) {
            resolve_report <<- resolve
            reject_report <<- reject
          })
        }
      )
    ),
    emit_progress = function(
      event_type,
      status,
      stage = NA_character_,
      step = NA_character_,
      message = NA_character_,
      payload = list(),
      parent_event_id = NA_character_,
      correlation_id = NA_character_
    ) {
      event <- tempest_progress_event(
        run_id = "stale-report",
        workflow = "costorm",
        event_type = event_type,
        status = status,
        stage = stage,
        step = step,
        message = message,
        payload = payload,
        parent_event_id = parent_event_id,
        correlation_id = correlation_id
      )
      events <<- c(events, list(event))
      event
    }
  )
  local_mocked_bindings(
    tempest_costorm_report_context = function(
      session,
      style,
      include_references
    ) {
      list(
        prompt = tempest:::tempest_costorm_report_prompt(session, style),
        title = session$title,
        workspace = session$workspace,
        include_references = include_references,
        citation_policy = session$config@citation_policy,
        on_unsupported_claim = session$config@on_unsupported_claim,
        min_support_score = session$config@min_support_score,
        execution_review = "",
        style = style
      )
    }
  )

  request <- tempest:::tempest_session_report_async(
    session,
    is_current = function() current,
    .artifact_catalog = artifact_catalog
  )
  current <- FALSE
  resolve_report("Stale body")
  settled <- await_tempest_promise(request)

  expect_null(settled$error)
  expect_null(artifacts[["report"]])
  expect_null(artifacts[["report_md"]])
  expect_identical(artifact_catalog$has("report_md"), FALSE)
  expect_contains(
    vapply(events, function(event) event@status, character(1)),
    "cancelled"
  )
  expect_false(any(
    vapply(
      events,
      function(event) {
        identical(event@event_type, "artifact") &&
          identical(event@status, "available")
      },
      logical(1)
    )
  ))

  current <- TRUE
  rejected_request <- tempest:::tempest_session_report_async(
    session,
    is_current = function() current,
    .artifact_catalog = artifact_catalog
  )
  current <- FALSE
  reject_report(simpleError("Authorization: Bearer sk-live-secret"))
  rejected <- await_tempest_promise(rejected_request)

  expect_null(rejected$error)
  expect_identical(artifact_catalog$has("report_md"), FALSE)
  statuses <- vapply(events, function(event) event@status, character(1))
  expect_identical(sum(statuses == "cancelled"), 2L)
  expect_identical(sum(statuses == "failed"), 0L)

  stale_at_entry <- tempest:::tempest_session_report_async(
    session,
    is_current = function() FALSE,
    .artifact_catalog = artifact_catalog
  )
  entry_result <- await_tempest_promise(stale_at_entry)

  expect_null(entry_result$error)
  expect_null(entry_result$value)
  expect_identical(report_calls, 2L)
})
