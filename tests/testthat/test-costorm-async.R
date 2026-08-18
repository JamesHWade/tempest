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
  source <- fake_source("https://example.org/cancelled")
  moderator <- fake_chat(
    text = list(paste0("Cancelled claim [", source$id, "]."))
  )
  config <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) {
      if (identical(role, "judge")) {
        extractor
      } else if (identical(role, "coordinator")) {
        moderator
      } else {
        fake_chat()
      }
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
  session$workspace$upsert_retrieved_resource(source)
  completion <- await_tempest_promise(
    session$request_completion_async("Inspect the cancelled claim.")
  )
  expect_null(completion$error)
  work_id <- tempest:::tempest_session_async_work_start(
    session,
    "dialogue",
    work_id = paste0("turn-", completion$value)
  )
  claim <- tempest:::tempest_session_agent_completion_claim(
    session,
    completion$value
  )
  tempest:::tempest_session_agent_completion_consume(session, claim)

  request <- tempest:::tempest_session_extract_facts_async(
    session,
    claim,
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
  tempest:::tempest_session_async_work_finish(session, work_id)
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
  source <- fake_source("https://example.org/failed")
  moderator <- fake_chat(
    text = list(paste0("Failed claim [", source$id, "]."))
  )
  config <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) {
      if (identical(role, "judge")) {
        extractor
      } else if (identical(role, "coordinator")) {
        moderator
      } else {
        fake_chat()
      }
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
  session$workspace$upsert_retrieved_resource(source)
  completion <- await_tempest_promise(
    session$request_completion_async("Inspect the failed claim.")
  )
  expect_null(completion$error)
  work_id <- tempest:::tempest_session_async_work_start(
    session,
    "dialogue",
    work_id = paste0("turn-", completion$value)
  )
  claim <- tempest:::tempest_session_agent_completion_claim(
    session,
    completion$value
  )
  tempest:::tempest_session_agent_completion_consume(session, claim)

  settled <- await_tempest_promise(
    tempest:::tempest_session_extract_facts_async(
      session,
      claim
    )
  )
  tempest:::tempest_session_async_work_finish(session, work_id)
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
    tempest:::tempest_product_canonical_json(lapply(
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

test_that("agent-derived evidence rejects missing and forged claims", {
  skip_if_not_installed("deputy")
  skip_if_not_installed("ellmer")
  session <- tempest_session(
    "Bound evidence claims",
    config = tempest_config(
      chat_fn = function(role, model, system_prompt, echo) {
        if (identical(role, "coordinator")) {
          return(fake_chat(text = list("Bound response.")))
        }
        fake_chat()
      }
    ),
    experts = list(test_expert(expert_id = "expert.bound-evidence"))
  )
  workspace_before <- tempest:::tempest_research_workspace_snapshot(
    session$workspace
  )

  expect_error(
    tempest:::tempest_session_commit_evidence_async(session, NULL),
    class = "tempest_agent_completion_binding_error"
  )
  completion <- await_tempest_promise(
    session$request_completion_async("Bind this prompt.")
  )
  expect_null(completion$error)
  claim <- tempest:::tempest_session_agent_completion_claim(
    session,
    completion$value
  )
  tempest:::tempest_session_agent_completion_consume(session, claim)
  forged <- claim
  forged$response <- "Different response."

  expect_error(
    tempest:::tempest_session_commit_evidence_async(session, forged),
    class = "tempest_agent_completion_binding_error"
  )
  expect_error(
    tempest:::tempest_session_commit_evidence_async(session, claim),
    class = "tempest_session_async_work_error"
  )
  expect_identical(
    tempest:::tempest_research_workspace_snapshot(session$workspace),
    workspace_before
  )
  expect_length(tempest:::tempest_session_stage_records(session), 0L)
  expect_length(session$transcript, 0L)
  expect_length(tempest_session_pending_deputy_runs(session), 0L)
})

test_that("stale provider turns cannot splice evidence into a completion", {
  skip_if_not_installed("deputy")
  skip_if_not_installed("ellmer")
  skip_if_not_installed("later")
  skip_if_not_installed("promises")

  stale_turn <- ellmer::AssistantTurn(
    contents = list(ellmer::ContentText("Old assistant evidence.")),
    json = list(
      output = list(list(
        type = "message",
        content = list(list(
          type = "output_text",
          text = "Old assistant evidence.",
          annotations = list(list(
            type = "url_citation",
            title = "Old source",
            url = "https://example.org/old-source"
          ))
        ))
      ))
    )
  )
  moderator <- fake_chat(
    text = list("Fresh assistant response with no citation.")
  )
  moderator$set_turns(list(stale_turn))
  moderator$last_turn <- function(role = "assistant") stale_turn
  config <- tempest_config(chat_fn = function(
    role,
    model,
    system_prompt,
    echo
  ) {
    if (
      identical(role, "coordinator") &&
        !identical(
          system_prompt,
          tempest_prompt("question_suggester_system")
        )
    ) {
      return(moderator)
    }
    fake_chat()
  })
  session <- tempest_session(
    "Provider turn freshness",
    config = config,
    experts = list(test_expert(expert_id = "expert.provider-freshness"))
  )
  workspace_before <- tempest:::tempest_research_workspace_snapshot(
    session$workspace
  )

  result <- await_tempest_promise(
    session$request_completion_async("Give a fresh answer.")
  )

  expect_s3_class(result$error, "tempest_agent_completion_binding_error")
  expect_identical(
    tempest:::tempest_research_workspace_snapshot(session$workspace),
    workspace_before
  )
  expect_length(tempest:::tempest_session_stage_records(session), 0L)
  expect_length(session$transcript, 0L)
  expect_length(tempest_session_pending_deputy_runs(session), 0L)
  expect_length(
    tempest:::tempest_session_agent_completion_active(session),
    0L
  )
  traces <- tempest:::tempest_session_deputy_traces(session)
  expect_length(traces, 1L)
  expect_identical(traces[[1L]]$status, "complete")
  expect_identical(traces[[1L]]$completion_disposition, "discarded")
  expect_error(
    tempest:::tempest_session_turn_deputy_execution(session, traces[[1L]]),
    class = "tempest_session_turn_error"
  )
  expect_no_error(tempest_session_snapshot(session))
})

test_that("post-turn processing owns sequencing and returns typed results", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("later")
  skip_if_not_installed("promises")
  calls <- character()
  evidence_correlation <- NULL
  events <- list()
  source <- fake_source()
  source_id <- source$id
  moderator <- fake_chat(
    text = list(paste0("A cited answer [", source_id, "]."))
  )
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) {
      if (identical(role, "coordinator")) {
        return(moderator)
      }
      fake_chat()
    }
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
  session$workspace$upsert_retrieved_resource(source)
  local_mocked_bindings(
    tempest_session_commit_evidence_async = function(
      session,
      claim,
      ...
    ) {
      calls <<- c(calls, "evidence")
      checked <- tempest:::tempest_session_agent_completion_assert_claim(
        session,
        claim,
        state = "consumed"
      )
      evidence_correlation <<- checked$deputy_execution$correlation_id
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
      tempest:::tempest_session_commit_mindmap(
        session,
        list(
          nodes = list(list(
            id = "root",
            label = "Typed async turns",
            parent = NULL,
            notes = "Updated",
            source_ids = source_id
          )),
          edges = list()
        )
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

  completion <- await_tempest_promise(
    session$request_completion_async("What is known?")
  )
  expect_null(completion$error)
  request <- tempest_session_process_turn_async(
    session,
    completion$value
  )
  settled <- await_tempest_promise(request)

  expect_null(settled$error)
  expect_equal(calls, c("evidence", "mindmap", "suggestions"))
  expect_identical(evidence_correlation, settled$value@turn_id)
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
  traces <- tempest:::tempest_session_deputy_traces(session)
  trace <- traces[[match(
    settled$value@deputy_run_id,
    vapply(traces, `[[`, character(1), "deputy_run_id")
  )]]
  expect_identical(settled$value@turn_id, trace$correlation_id)
  expect_identical(settled$value@deputy_session_id, trace$deputy_session_id)
  expect_equal(
    vapply(
      Filter(
        \(event) identical(event@correlation_id, settled$value@turn_id),
        events
      ),
      \(event) event@status,
      character(1)
    ),
    c("started", "succeeded", "succeeded", "succeeded")
  )
  expect_no_error(tempest:::tempest_product_canonical_json(
    tempest:::tempest_session_turn_result_data(settled$value)
  ))
})

test_that("post-turn rejects a completion owned by another session", {
  skip_if_not_installed("deputy")
  skip_if_not_installed("ellmer")
  skip_if_not_installed("later")
  skip_if_not_installed("promises")
  cfg <- tempest_config(chat_fn = function(role, model, system_prompt, echo) {
    if (identical(role, "coordinator")) {
      return(fake_chat(text = list("A completed response.")))
    }
    fake_chat()
  })
  source_session <- tempest_session(
    "Source async turn",
    config = cfg,
    experts = list(test_expert(
      expert_id = "expert.turn-source",
      name = "Dr. Source"
    ))
  )
  session <- tempest_session(
    "Mismatched async turn",
    config = cfg,
    experts = list(test_expert(
      expert_id = "expert.turn-mismatch",
      name = "Dr. Mismatch"
    ))
  )
  completion <- await_tempest_promise(
    source_session$request_completion_async("What is known?")
  )
  evidence_calls <- 0L
  event_count <- length(session$events)
  local_mocked_bindings(
    tempest_session_commit_evidence_async = function(...) {
      evidence_calls <<- evidence_calls + 1L
      promises::promise_resolve(NULL)
    }
  )

  expect_null(completion$error)
  expect_error(
    tempest_session_process_turn_async(
      session,
      completion$value
    ),
    class = "tempest_agent_completion_binding_error"
  )
  expect_length(session$transcript, 0L)
  expect_length(session$events, event_count)
  expect_identical(evidence_calls, 0L)
})

test_that("post-turn processing exposes evidence gaps without UI callbacks", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("later")
  skip_if_not_installed("promises")
  exchange <- NULL
  cfg <- tempest_config(chat_fn = function(role, model, system_prompt, echo) {
    if (identical(role, "coordinator")) {
      return(fake_chat(text = list("An unsupported factual answer.")))
    }
    fake_chat()
  })
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

  completion <- await_tempest_promise(
    session$request_completion_async("What is known?")
  )
  expect_null(completion$error)
  request <- tempest_session_process_turn_async(
    session,
    completion$value,
    suggest = FALSE
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
  cfg <- tempest_config(chat_fn = function(role, model, system_prompt, echo) {
    if (identical(role, "coordinator")) {
      return(fake_chat(text = list("A response.")))
    }
    fake_chat()
  })
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

  completion <- await_tempest_promise(
    session$request_completion_async("Continue?")
  )
  expect_null(completion$error)
  request <- tempest_session_process_turn_async(
    session,
    completion$value
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
  cfg <- tempest_config(chat_fn = function(role, model, system_prompt, echo) {
    if (identical(role, "coordinator")) {
      return(fake_chat(text = list("This turn has completed.")))
    }
    fake_chat()
  })
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

  completion <- await_tempest_promise(
    session$request_completion_async("Will this be stale?")
  )
  expect_null(completion$error)
  request <- tempest_session_process_turn_async(
    session,
    completion$value,
    is_current = function() current
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
    cfg <- tempest_config(chat_fn = function(role, model, system_prompt, echo) {
      if (identical(role, "coordinator")) {
        return(fake_chat(text = list("A response.")))
      }
      fake_chat()
    })
    session <- tempest_session(
      paste("Synchronous", stage_name, "failure"),
      config = cfg,
      experts = list(test_expert(
        expert_id = paste0("expert.sync-", stage_name),
        name = "Dr. Sync"
      ))
    )
    completion <- await_tempest_promise(
      session$request_completion_async("Continue?")
    )
    expect_null(completion$error)
    settled <- await_tempest_promise(tempest_session_process_turn_async(
      session,
      completion$value
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
  local_mocked_bindings(
    tempest_canonical_json = function(...) {
      stop("generic canonical JSON must not be called", call. = FALSE)
    }
  )
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
      deputy_run_id = "deputy-run-validation",
      deputy_session_id = "deputy-session-validation",
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
      deputy_run_id = "deputy-run-validation",
      deputy_session_id = "deputy-session-validation",
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

test_that("default Co-STORM reporting verifies and publishes atomically", {
  skip_if_not_installed("deputy")
  skip_if_not_installed("ellmer")
  skip_if_not_installed("later")
  skip_if_not_installed("promises")
  claim_text <- paste0(
    "Captured [evidence]\n## References cannot replace the report boundary."
  )
  quote <- claim_text
  source_id <- "source.default-report"
  extractor <- fake_chat(
    structured = list(
      list(
        facts = list(list(
          claim = claim_text,
          sources = list(list(source_id = source_id, quote = quote)),
          confidence = "high"
        ))
      )
    )
  )
  moderator <- fake_chat(
    text = list(paste0(claim_text, " [", source_id, "]."))
  )
  config <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) {
      if (identical(system_prompt, tempest_prompt("fact_extractor_system"))) {
        return(extractor)
      }
      if (identical(role, "coordinator")) {
        return(moderator)
      }
      fake_chat()
    }
  )
  expect_identical(config@citation_policy, "source_attributed")
  session <- tempest_session(
    "Default report publication",
    config = config,
    experts = list(test_expert(expert_id = "expert.default-report"))
  )
  session$workspace$upsert_retrieved_resource(tempest_resource(
    resource_kind = "web.page",
    locator = "https://example.org/default-report",
    title = "Default report evidence",
    media_type = "text/plain",
    content = quote,
    resource_id = source_id,
    retrieved_at = "2026-08-18T12:00:00Z"
  ))
  completion <- await_tempest_promise(
    session$request_completion_async("What does the captured evidence show?")
  )
  expect_null(completion$error)
  evidence <- await_tempest_promise(
    tempest_session_process_turn_async(
      session,
      completion$value,
      suggest = FALSE
    )
  )
  expect_null(evidence$error)
  expect_identical(evidence$value@claims_added, 1L)
  expect_length(session$workspace$list_claim_supports(), 0L)

  resolve_verification <- NULL
  verification_attempt <- 0L
  local_mocked_bindings(
    tempest_dsprrr_run_async = function(
      module,
      ...,
      .llm = NULL,
      .trace_context = list()
    ) {
      verification_attempt <<- verification_attempt + 1L
      request <- if (identical(verification_attempt, 1L)) {
        promises::promise(function(resolve, reject) {
          reject(rlang::error_cnd(
            "tempest_program_execution_error",
            message = "verification failed before publication"
          ))
        })
      } else {
        promises::promise(function(resolve, reject) {
          resolve_verification <<- resolve
        })
      }
      attr(request, "dsprrr_trace_context") <- list(
        program_artifact_id = dsprrr::program_artifact_id(module),
        trace_context = .trace_context
      )
      request
    }
  )
  failed_publication <- await_tempest_promise(
    tempest:::tempest_session_report_async(session)
  )
  expect_s3_class(
    failed_publication$error,
    "tempest_session_error"
  )
  expect_identical(session$manifest@status, "running")
  expect_null(tempest:::tempest_session_report_value(session))
  expect_identical(
    tempest:::tempest_research_workspace_mutation_state(session$workspace),
    "open"
  )
  title_before <- session$title
  transcript_before <- session$transcript
  mindmap_before <- session$mindmap
  experts_before <- session$experts
  injected <- tempest_resource(
    resource_kind = "web.page",
    locator = "https://example.org/workspace-injected",
    title = "Injected during publication",
    media_type = "text/plain",
    content = "This source must never enter the publication workspace.",
    resource_id = "source.workspace-injected",
    retrieved_at = "2026-08-18T12:01:00Z"
  )
  mutation_errors <- list()
  progress_callback <- function(event) {
    event_data <- tempest_progress_event_data(event)
    if (
      identical(event_data$stage, "report") &&
        identical(event_data$status, "started")
    ) {
      probes <- list(
        title = function() session$title <- "Injected title",
        transcript = function() {
          session$transcript[[2L]]$text <- "Injected assistant text"
        },
        mindmap = function() {
          session$mindmap$nodes[[1L]]$label <- "Injected map"
        },
        experts = function() session$experts <- list(),
        events = function() session$events <- list(),
        progress = function() session$progress <- NULL,
        workspace = function() {
          session$workspace$upsert_retrieved_resource(injected)
        }
      )
      mutation_errors <<- lapply(probes, function(probe) {
        tryCatch(
          {
            probe()
            NULL
          },
          error = identity
        )
      })
    }
    stop("host progress observer failed", call. = FALSE)
  }
  tempest:::tempest_session_set_progress(session, progress_callback)
  request <- tempest:::tempest_session_report_async(session)
  for (index in seq_len(100L)) {
    later::run_now(timeoutSecs = 0.01)
    if (!is.null(resolve_verification)) {
      break
    }
  }
  expect_identical(is.function(resolve_verification), TRUE)
  expect_error(
    session$request_completion_async("Overlapping moderator request."),
    class = "tempest_session_async_work_error"
  )
  expect_error(
    session$step("Overlapping synchronous moderator request."),
    class = "tempest_session_async_work_error"
  )
  expect_error(
    session$add_expert("Overlapping publication expert"),
    class = "tempest_session_async_work_error"
  )
  expect_error(
    tempest_verify_claims(session, verifier = fake_chat()),
    class = "tempest_session_async_work_error"
  )
  expect_length(tempest_session_pending_deputy_runs(session), 0L)
  expect_length(
    tempest:::tempest_session_agent_completion_active(session),
    0L
  )
  resolve_verification(list(
    status = "supported",
    score = 0.95,
    rationale = "The exact captured excerpt supports the claim."
  ))
  publication <- await_tempest_promise(request)

  expect_null(publication$error)
  expect_match(
    publication$value,
    paste0(
      "Captured \\[evidence\\] \\#\\# References cannot replace the report ",
      "boundary\\."
    ),
    fixed = TRUE
  )
  expect_no_match(
    publication$value,
    "\n## References cannot replace the report boundary.",
    fixed = TRUE
  )
  expect_identical(session$manifest@status, "succeeded")
  expect_identical(tempest_session_report_md(session), publication$value)
  expect_identical(session$title, title_before)
  expect_identical(session$transcript, transcript_before)
  expect_identical(session$mindmap, mindmap_before)
  expect_identical(session$experts, experts_before)
  expect_identical(session$progress, progress_callback)
  expect_length(mutation_errors, 7L)
  expect_all_true(vapply(
    mutation_errors[c(
      "title",
      "transcript",
      "mindmap",
      "experts",
      "events",
      "progress"
    )],
    inherits,
    logical(1),
    what = "tempest_session_error"
  ))
  expect_s3_class(
    mutation_errors$workspace,
    "tempest_research_workspace_error"
  )
  expect_null(
    session$workspace$get_retrieved_resource("source.workspace-injected")
  )
  expect_identical(
    tempest:::tempest_research_workspace_mutation_state(session$workspace),
    "sealed"
  )
  expect_error(
    session$transcript[[2L]]$text <- "Post-publication replacement",
    class = "tempest_session_error"
  )
  expect_error(
    session$workspace$upsert_retrieved_resource(injected),
    class = "tempest_research_workspace_error"
  )
  snapshot <- tempest_session_snapshot(session)
  restored <- tempest_session_restore(snapshot, config = config)
  expect_identical(restored$manifest@status, "succeeded")
  expect_identical(restored$transcript, transcript_before)
  expect_identical(
    tempest:::tempest_research_workspace_mutation_state(restored$workspace),
    "sealed"
  )
  expect_error(
    restored$transcript[[2L]]$text <- "Restored transcript replacement",
    class = "tempest_session_error"
  )
  expect_error(
    restored$workspace$upsert_retrieved_resource(injected),
    class = "tempest_research_workspace_error"
  )
  expect_length(session$workspace$list_claim_supports(), 1L)
  verification <- Filter(
    \(record) identical(record@stage, "verify_claim_support"),
    tempest:::tempest_session_stage_records(session)
  )
  expect_length(verification, 2L)
  expect_identical(
    vapply(verification, \(record) record@status, character(1)),
    c("failed", "succeeded")
  )
  report_events <- Filter(
    \(event) identical(event$stage, "report"),
    tempest_execution_events(session)
  )
  expect_identical(
    vapply(report_events, \(event) event$status, character(1)),
    c("started", "failed", "started", "available", "succeeded")
  )
  expect_no_error(tempest:::tempest_session_async_work_assert_quiescent(
    session
  ))
  expect_error(
    session$add_turn("User", "user", "Try to resume."),
    class = "tempest_session_error"
  )
})

test_that("post-turn processing has the exact completion-only signature", {
  expected <- c(
    "session",
    "completion_id",
    "suggest",
    "n_suggestions",
    "is_current"
  )
  removed <- c(
    "user_text",
    "assistant_text",
    "deputy_execution",
    "provider_turn",
    "turn_id"
  )
  actual <- names(formals(tempest_session_process_turn_async))

  expect_identical(actual, expected)
  expect_identical(intersect(actual, removed), character())
})

test_that("queued turns are claimed by completion ID instead of latest run", {
  skip_if_not_installed("deputy")
  skip_if_not_installed("ellmer")
  skip_if_not_installed("later")
  skip_if_not_installed("promises")

  moderator <- fake_chat(
    text = list(
      enc2utf8("First queued response — café."),
      enc2utf8("Second queued response — 東京.")
    )
  )
  config <- tempest_config(chat_fn = function(
    role,
    model,
    system_prompt,
    echo
  ) {
    if (identical(role, "coordinator")) {
      return(moderator)
    }
    fake_chat()
  })
  session <- tempest_session(
    "Queued capability turns",
    config = config,
    experts = list(test_expert(
      expert_id = "expert.queued-capability",
      name = "Dr. Queue"
    ))
  )
  first <- await_tempest_promise(
    session$request_completion_async(
      enc2utf8("First queued prompt — naïve.")
    )
  )
  second <- await_tempest_promise(
    session$request_completion_async(
      enc2utf8("Second queued prompt — résumé.")
    )
  )
  evidence <- list()
  local_mocked_bindings(
    tempest_session_commit_evidence_async = function(
      session,
      claim,
      ...
    ) {
      claim <- tempest:::tempest_session_agent_completion_assert_claim(
        session,
        claim,
        state = "consumed"
      )
      evidence[[length(evidence) + 1L]] <<- list(
        text = claim$response,
        turn = claim$provider_turn,
        deputy_execution = claim$deputy_execution
      )
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
      promises::promise_resolve(NULL)
    }
  )

  expect_null(first$error)
  expect_null(second$error)
  expect_identical(first$value == second$value, FALSE)
  expect_identical(
    tempest:::tempest_session_agent_completion_status(
      session,
      first$value
    ),
    "issued"
  )
  expect_identical(
    tempest:::tempest_session_agent_completion_status(
      session,
      second$value
    ),
    "issued"
  )
  completion_context <- tempest:::tempest_session_agent_completion_context(
    session
  )
  for (completion_id in c(first$value, second$value)) {
    entry <- tempest:::tempest_agent_completion_entry(
      completion_context$registry,
      completion_id,
      completion_context$owner
    )
    duplicated <- tempest:::tempest_agent_completion_claim_value(entry)
    expect_identical(
      tempest:::tempest_agent_completion_digest(
        duplicated$prompt,
        duplicated$response,
        duplicated$provider_turn,
        duplicated$deputy_execution
      ),
      entry$digest
    )
  }

  first_result <- await_tempest_promise(
    tempest_session_process_turn_async(
      session,
      first$value,
      suggest = FALSE
    )
  )

  expect_null(first_result$error)
  expect_length(evidence, 1L)
  expect_identical(
    charToRaw(evidence[[1L]]$text),
    charToRaw(enc2utf8("First queued response — café."))
  )
  expect_identical(
    ellmer::contents_markdown(evidence[[1L]]$turn),
    enc2utf8("First queued response — café.")
  )
  expect_identical(
    tempest:::tempest_session_agent_completion_status(
      session,
      first$value
    ),
    "consumed"
  )
  expect_identical(
    tempest:::tempest_session_agent_completion_status(
      session,
      second$value
    ),
    "issued"
  )

  second_result <- await_tempest_promise(
    tempest_session_process_turn_async(
      session,
      second$value,
      suggest = FALSE
    )
  )

  expect_null(second_result$error)
  expect_length(evidence, 2L)
  expect_identical(
    vapply(evidence, `[[`, character(1), "text"),
    c(
      enc2utf8("First queued response — café."),
      enc2utf8("Second queued response — 東京.")
    )
  )
  expect_identical(
    vapply(session$transcript, `[[`, character(1), "text"),
    c(
      enc2utf8("First queued prompt — naïve."),
      enc2utf8("First queued response — café."),
      enc2utf8("Second queued prompt — résumé."),
      enc2utf8("Second queued response — 東京.")
    )
  )
  expect_identical(
    vapply(
      evidence,
      \(item) item$deputy_execution$deputy_run_id,
      character(1)
    ) ==
      first_result$value@deputy_run_id,
    c(TRUE, FALSE)
  )
  expect_identical(
    evidence[[2L]]$deputy_execution$deputy_run_id,
    second_result$value@deputy_run_id
  )
})

test_that("turn processing preserves the original consume failure", {
  skip_if_not_installed("deputy")
  skip_if_not_installed("ellmer")
  skip_if_not_installed("later")
  skip_if_not_installed("promises")

  moderator <- fake_chat(text = list("Bound response."))
  config <- tempest_config(chat_fn = function(
    role,
    model,
    system_prompt,
    echo
  ) {
    if (identical(role, "coordinator")) {
      return(moderator)
    }
    fake_chat()
  })
  session <- tempest_session(
    "Consume failure identity",
    config = config,
    experts = list(test_expert(
      expert_id = "expert.consume-failure",
      name = "Dr. Consume"
    ))
  )
  completion <- await_tempest_promise(
    session$request_completion_async("Keep the original failure.")
  )
  expect_null(completion$error)
  local_mocked_bindings(
    tempest_session_agent_completion_consume = function(...) {
      rlang::abort(
        "original consume failure",
        class = "tempest_test_consume_error"
      )
    },
    tempest_session_agent_completion_release = function(...) {
      tempest:::tempest_agent_completion_binding_abort()
    }
  )

  error <- tryCatch(
    tempest_session_process_turn_async(
      session,
      completion$value,
      suggest = FALSE
    ),
    error = identity
  )

  expect_s3_class(error, "tempest_test_consume_error")
  expect_identical(conditionMessage(error), "original consume failure")
})

test_that("promise waits time out only while the event loop is idle", {
  skip_if_not_installed("later")
  skip_if_not_installed("promises")

  request <- promises::promise(function(resolve, reject) {
    later::later(function() {
      Sys.sleep(0.03)
      later::later(\() resolve("settled"), delay = 0.001)
    })
  })

  settled <- await_tempest_promise(request, timeout_s = 0.02)

  expect_null(settled$error)
  expect_identical(settled$value, "settled")
})

test_that("promise waits retain a non-resetting hard deadline", {
  skip_if_not_installed("later")
  skip_if_not_installed("promises")

  active <- TRUE
  heartbeat_count <- 0L
  heartbeat <- function() {
    if (!active) {
      return(invisible(NULL))
    }
    heartbeat_count <<- heartbeat_count + 1L
    later::later(heartbeat, delay = 0.001)
  }
  later::later(heartbeat)
  request <- promises::promise(function(resolve, reject) invisible(NULL))

  error <- tryCatch(
    await_tempest_promise(
      request,
      timeout_s = 0.02,
      hard_timeout_s = 0.06
    ),
    error = identity
  )
  active <- FALSE
  later::run_now(0.01)

  expect_s3_class(error, "simpleError")
  expect_identical(
    conditionMessage(error),
    "Promise did not settle before the test timeout."
  )
  expect_gte(heartbeat_count, 2L)
})

test_that("stale work cancels before mutation and later failure stays consumed", {
  skip_if_not_installed("deputy")
  skip_if_not_installed("ellmer")
  skip_if_not_installed("later")
  skip_if_not_installed("promises")

  moderator <- fake_chat(
    text = list(
      "Discarded response.",
      "Committed response."
    )
  )
  config <- tempest_config(chat_fn = function(
    role,
    model,
    system_prompt,
    echo
  ) {
    if (identical(role, "coordinator")) {
      return(moderator)
    }
    fake_chat()
  })
  session <- tempest_session(
    "Completion cancellation boundary",
    config = config,
    experts = list(test_expert(
      expert_id = "expert.completion-cancel",
      name = "Dr. Cancellation"
    ))
  )
  discarded <- await_tempest_promise(
    session$request_completion_async("Discard this prompt.")
  )
  committed <- await_tempest_promise(
    session$request_completion_async("Commit this prompt.")
  )
  transcript_count <- length(session$transcript)
  event_count <- length(session$events)

  stale <- await_tempest_promise(tempest_session_process_turn_async(
    session,
    discarded$value,
    suggest = FALSE,
    is_current = \() FALSE
  ))

  expect_null(discarded$error)
  expect_null(committed$error)
  expect_null(stale$error)
  expect_identical(stale$value@status, "cancelled")
  expect_identical(
    tempest:::tempest_session_agent_completion_status(
      session,
      discarded$value
    ),
    "cancelled"
  )
  expect_length(session$transcript, transcript_count)
  expect_length(session$events, event_count)

  local_mocked_bindings(
    tempest_session_commit_evidence_async = function(...) {
      promises::promise_reject(structure(
        list(message = "enrichment failed after transcript mutation"),
        class = c("test_enrichment_error", "error", "condition")
      ))
    },
    tempest_session_update_mindmap_async = function(...) {
      promises::promise_resolve(NULL)
    }
  )
  failed_enrichment <- await_tempest_promise(
    tempest_session_process_turn_async(
      session,
      committed$value,
      suggest = FALSE
    )
  )

  expect_null(failed_enrichment$error)
  expect_identical(failed_enrichment$value@status, "partial")
  expect_identical(
    tempest:::tempest_session_agent_completion_status(
      session,
      committed$value
    ),
    "consumed"
  )
  expect_identical(
    vapply(session$transcript, `[[`, character(1), "text"),
    c("Commit this prompt.", "Committed response.")
  )
  replay <- tryCatch(
    tempest_session_process_turn_async(
      session,
      committed$value,
      suggest = FALSE
    ),
    error = identity
  )
  expect_s3_class(replay, "tempest_agent_completion_state_error")
})

test_that("progress callback failure keeps the consumed dialogue coherent", {
  skip_if_not_installed("deputy")
  skip_if_not_installed("ellmer")
  skip_if_not_installed("later")
  skip_if_not_installed("promises")

  moderator <- fake_chat(text = list("Atomic response."))
  config <- tempest_config(chat_fn = function(
    role,
    model,
    system_prompt,
    echo
  ) {
    if (identical(role, "coordinator")) {
      return(moderator)
    }
    fake_chat()
  })
  session <- tempest_session(
    "Progress callback atomicity",
    config = config,
    experts = list(test_expert(
      expert_id = "expert.progress-atomicity",
      name = "Dr. Progress"
    ))
  )
  completion <- await_tempest_promise(
    session$request_completion_async("Atomic prompt.")
  )
  expect_null(completion$error)
  tempest:::tempest_session_set_progress(session, function(event) {
    stop("private host progress detail")
  })
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
    tempest_session_update_mindmap_async = function(...) {
      promises::promise_resolve(NULL)
    }
  )

  settled <- await_tempest_promise(tempest_session_process_turn_async(
    session,
    completion$value,
    suggest = FALSE
  ))

  expect_null(settled$error)
  expect_identical(settled$value@status, "partial")
  progress_notices <- Filter(
    \(notice) identical(notice@code, "progress_failed"),
    settled$value@notices
  )
  expect_length(progress_notices, 1L)
  expect_identical(progress_notices[[1L]]@stage, "dialogue")
  expect_identical(progress_notices[[1L]]@severity, "warning")
  expect_identical(
    progress_notices[[1L]]@message,
    "The host progress callback failed."
  )
  expect_identical(
    progress_notices[[1L]]@details,
    list(
      error_class = "tempest_operation_error",
      error_message = "The operation failed."
    )
  )
  expect_no_match(
    tempest:::tempest_product_canonical_json(
      tempest:::tempest_session_turn_result_data(settled$value)
    ),
    "private host progress detail",
    fixed = TRUE
  )
  expect_identical(
    vapply(session$transcript, `[[`, character(1), "role"),
    c("user", "assistant")
  )
  expect_identical(
    vapply(session$transcript, `[[`, character(1), "text"),
    c("Atomic prompt.", "Atomic response.")
  )
  expect_identical(
    tempest:::tempest_session_agent_completion_status(
      session,
      completion$value
    ),
    "consumed"
  )
  replay <- tryCatch(
    tempest_session_process_turn_async(
      session,
      completion$value,
      suggest = FALSE
    ),
    error = identity
  )
  expect_s3_class(replay, "tempest_agent_completion_state_error")
})
