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
})
