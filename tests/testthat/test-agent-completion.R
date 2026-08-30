test_that("agent completions preserve exact process-local bindings", {
  skip_if_not_installed("ellmer")

  owner <- new.env(parent = emptyenv())
  registry <- tempest:::tempest_agent_completion_registry(owner)
  prompt <- enc2utf8("naïve café — 東京 🧪\r\nquestion")
  response <- enc2utf8("résumé e\u0301 — exact bytes\r\nanswer")
  provider_turn <- ellmer::AssistantTurn(
    list(ellmer::ContentText(response)),
    tokens = c(7, 5, 0),
    cost = 0
  )
  direct <- test_costorm_deputy_trace(
    run_id = "deputy-run-completion-direct",
    session_id = "deputy-session-completion-direct",
    correlation_id = "completion-correlation-direct"
  )
  delegated <- test_costorm_deputy_trace(
    run_id = "deputy-run-completion-child",
    session_id = "deputy-session-completion-child",
    stage = "dialogue",
    role = "expert",
    correlation_id = "completion-correlation-delegated",
    expert_id = "expert.completion"
  )
  delegated$parent_run_id <- "deputy-run-completion-parent"
  delegated$delegation_id <- "delegation-completion"
  delegated$tool_call_id <- "tool-call-completion"
  first_id <- tempest:::tempest_agent_completion_new_id(registry)
  second_id <- tempest:::tempest_agent_completion_new_id(registry)

  issued <- tempest:::tempest_agent_completion_issue(
    registry,
    completion_id = first_id,
    prompt = prompt,
    response = response,
    provider_turn = provider_turn,
    deputy_execution = direct
  )
  tempest:::tempest_agent_completion_issue(
    registry,
    completion_id = second_id,
    prompt = prompt,
    response = response,
    provider_turn = provider_turn,
    deputy_execution = delegated
  )

  expect_identical(issued, first_id)
  expect_identical(first_id == second_id, FALSE)
  expect_identical(
    tempest:::tempest_opaque_identifier_valid(first_id),
    TRUE
  )
  expect_identical(
    tempest:::tempest_agent_completion_status(registry, first_id, owner),
    "issued"
  )
  expect_setequal(
    tempest:::tempest_agent_completion_active(registry),
    c(first_id, second_id)
  )

  claim <- tempest:::tempest_agent_completion_claim(
    registry,
    first_id,
    owner
  )
  delegated_claim <- tempest:::tempest_agent_completion_claim(
    registry,
    second_id,
    owner
  )

  expect_identical(charToRaw(claim$prompt), charToRaw(prompt))
  expect_identical(charToRaw(claim$response), charToRaw(response))
  expect_identical(claim$provider_turn, provider_turn)
  expect_identical(claim$deputy_execution, direct)
  expect_identical(
    delegated_claim$deputy_execution[
      c(
        "parent_run_id",
        "delegation_id",
        "tool_call_id",
        "deputy_run_id",
        "deputy_session_id",
        "expert_id",
        "correlation_id"
      )
    ],
    list(
      parent_run_id = "deputy-run-completion-parent",
      delegation_id = "delegation-completion",
      tool_call_id = "tool-call-completion",
      deputy_run_id = "deputy-run-completion-child",
      deputy_session_id = "deputy-session-completion-child",
      expert_id = "expert.completion",
      correlation_id = "completion-correlation-delegated"
    )
  )
  expect_identical(
    any(c("digest", "owner", "registry") %in% names(claim)),
    FALSE
  )
  expect_identical(
    any(
      c("prompt", "response", "provider_turn", "digest") %in%
        names(claim$deputy_execution)
    ),
    FALSE
  )

  substituted <- rlang::duplicate(claim, shallow = FALSE)
  substituted$response <- enc2utf8("résumé é — normalized substitute")
  substitution_error <- tryCatch(
    tempest:::tempest_agent_completion_consume(
      registry,
      substituted,
      owner
    ),
    error = identity
  )
  expect_s3_class(
    substitution_error,
    "tempest_agent_completion_binding_error"
  )
  expect_identical(
    tempest:::tempest_agent_completion_status(registry, first_id, owner),
    "processing"
  )

  tempest:::tempest_agent_completion_release(registry, claim, owner)
  tempest:::tempest_agent_completion_cancel(registry, second_id, owner)
})

test_that("agent completions preserve boundaries between visible text", {
  skip_if_not_installed("ellmer")

  provider_turn <- ellmer::AssistantTurn(list(
    ellmer::ContentText("First paragraph."),
    ellmer::ContentThinking("private reasoning"),
    ellmer::ContentText("Second paragraph.")
  ))

  expect_identical(
    tempest:::tempest_agent_completion_response_from_turn(provider_turn),
    "First paragraph.\n\nSecond paragraph."
  )
})

test_that("agent completion lifecycle is one-use and owner-bound", {
  skip_if_not_installed("ellmer")

  owner <- new.env(parent = emptyenv())
  other_owner <- new.env(parent = emptyenv())
  registry <- tempest:::tempest_agent_completion_registry(owner)
  other_registry <- tempest:::tempest_agent_completion_registry(other_owner)
  provider_turn <- ellmer::AssistantTurn(
    list(ellmer::ContentText("same response")),
    tokens = c(1, 1, 0),
    cost = 0
  )
  trace <- test_costorm_deputy_trace(
    run_id = "deputy-run-completion-lifecycle",
    correlation_id = "completion-correlation-lifecycle"
  )
  completion_id <- tempest:::tempest_agent_completion_new_id(registry)
  tempest:::tempest_agent_completion_issue(
    registry,
    completion_id,
    "same prompt",
    "same response",
    provider_turn,
    trace
  )

  wrong_owner <- tryCatch(
    tempest:::tempest_agent_completion_claim(
      registry,
      completion_id,
      other_owner
    ),
    error = identity
  )
  expect_s3_class(
    wrong_owner,
    "tempest_agent_completion_binding_error"
  )
  cross_session <- tryCatch(
    tempest:::tempest_agent_completion_claim(
      other_registry,
      completion_id,
      other_owner
    ),
    error = identity
  )
  expect_s3_class(
    cross_session,
    "tempest_agent_completion_binding_error"
  )
  unknown <- tryCatch(
    tempest:::tempest_agent_completion_claim(
      registry,
      paste0(
        substr(completion_id, 1L, nchar(completion_id) - 1L),
        if (endsWith(completion_id, "0")) "1" else "0"
      ),
      owner
    ),
    error = identity
  )
  expect_s3_class(unknown, "tempest_agent_completion_id_error")

  first_claim <- tempest:::tempest_agent_completion_claim(
    registry,
    completion_id,
    owner
  )
  expect_identical(
    tempest:::tempest_agent_completion_status(
      registry,
      completion_id,
      owner
    ),
    "processing"
  )
  concurrent <- tryCatch(
    tempest:::tempest_agent_completion_claim(
      registry,
      completion_id,
      owner
    ),
    error = identity
  )
  expect_s3_class(concurrent, "tempest_agent_completion_state_error")

  tempest:::tempest_agent_completion_release(registry, first_claim, owner)
  expect_identical(
    tempest:::tempest_agent_completion_status(
      registry,
      completion_id,
      owner
    ),
    "issued"
  )
  retry_claim <- tempest:::tempest_agent_completion_claim(
    registry,
    completion_id,
    owner
  )
  tempest:::tempest_agent_completion_consume(registry, retry_claim, owner)
  expect_identical(
    tempest:::tempest_agent_completion_status(
      registry,
      completion_id,
      owner
    ),
    "consumed"
  )
  consumed <- tempest:::tempest_agent_completion_entry(
    registry,
    completion_id,
    owner
  )
  expect_named(consumed, c("completion_id", "digest", "state"))
  expect_identical(consumed$completion_id, completion_id)
  expect_identical(consumed$state, "consumed")
  replay <- tryCatch(
    tempest:::tempest_agent_completion_claim(
      registry,
      completion_id,
      owner
    ),
    error = identity
  )
  expect_s3_class(replay, "tempest_agent_completion_state_error")

  cancelled_id <- tempest:::tempest_agent_completion_new_id(registry)
  tempest:::tempest_agent_completion_issue(
    registry,
    cancelled_id,
    "discarded prompt",
    "discarded response",
    provider_turn,
    test_costorm_deputy_trace(
      run_id = "deputy-run-completion-cancelled",
      correlation_id = "completion-correlation-cancelled"
    )
  )
  tempest:::tempest_agent_completion_claim(
    registry,
    cancelled_id,
    owner
  )
  tempest:::tempest_agent_completion_cancel(
    registry,
    cancelled_id,
    owner
  )
  expect_identical(
    tempest:::tempest_agent_completion_status(
      registry,
      cancelled_id,
      owner
    ),
    "cancelled"
  )
  cancelled <- tempest:::tempest_agent_completion_entry(
    registry,
    cancelled_id,
    owner
  )
  expect_named(cancelled, c("completion_id", "digest", "state"))
  expect_identical(cancelled$completion_id, cancelled_id)
  expect_identical(cancelled$state, "cancelled")
  cancelled_replay <- tryCatch(
    tempest:::tempest_agent_completion_claim(
      registry,
      cancelled_id,
      owner
    ),
    error = identity
  )
  expect_s3_class(
    cancelled_replay,
    "tempest_agent_completion_state_error"
  )
  expect_length(tempest:::tempest_agent_completion_active(registry), 0L)
  expect_no_error(
    tempest:::tempest_agent_completion_assert_quiescent(registry)
  )

  active_id <- tempest:::tempest_agent_completion_new_id(registry)
  tempest:::tempest_agent_completion_issue(
    registry,
    active_id,
    "active prompt",
    "active response",
    provider_turn,
    test_costorm_deputy_trace(
      run_id = "deputy-run-completion-active",
      correlation_id = "completion-correlation-active"
    )
  )
  active_error <- tryCatch(
    tempest:::tempest_agent_completion_assert_quiescent(registry),
    error = identity
  )
  expect_s3_class(active_error, "tempest_agent_completion_state_error")
})

test_that("session completion settlement is atomic with pending run clearance", {
  skip_if_not_installed("deputy")
  skip_if_not_installed("ellmer")
  skip_if_not_installed("later")
  skip_if_not_installed("promises")

  chat <- fake_chat(text = list("Settled response."))
  config <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) {
      if (identical(role, "coordinator")) {
        return(chat)
      }
      fake_chat()
    }
  )
  session <- tempest_session(
    "Atomic completion settlement",
    config = config,
    experts = list(test_expert(
      expert_id = "expert.atomic-completion",
      name = "Dr. Atomic"
    ))
  )
  request <- await_tempest_promise(
    session$.__enclos_env__$private$request_completion_async(
      "Settle this exact prompt."
    )
  )

  expect_null(request$error)
  expect_identical(
    tempest:::tempest_opaque_identifier_valid(request$value),
    TRUE
  )
  expect_type(request$value, "character")
  expect_length(request$value, 1L)
  expect_length(tempest:::tempest_session_pending_deputy_runs(session), 0L)
  expect_length(tempest:::tempest_session_deputy_traces(session), 1L)
  expect_identical(
    tempest:::tempest_session_agent_completion_status(
      session,
      request$value
    ),
    "issued"
  )
})

test_that("moderator routes each own one bounded completion span", {
  skip_if_not_installed("deputy")
  skip_if_not_installed("ellmer")
  skip_if_not_installed("later")
  skip_if_not_installed("promises")
  local_otel_opt_in()
  state <- local_fake_otel()
  moderator <- fake_chat(
    text = list(
      "Synchronous response.",
      "Request response.",
      "Raw Shiny response.",
      "Direct async response.",
      "Direct stream response."
    )
  )
  config <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) {
      if (identical(role, "coordinator")) {
        return(moderator)
      }
      fake_chat()
    }
  )
  session <- tempest_session(
    "Completion telemetry routes",
    config = config,
    experts = list(test_expert(
      expert_id = "expert.completion-telemetry",
      name = "Dr. Telemetry"
    ))
  )

  step <- session$step("Synchronous prompt with private content.")
  request <- await_tempest_promise(
    session$.__enclos_env__$private$request_completion_async(
      "Request prompt with private content."
    )
  )
  raw <- tempest:::tempest_session_chat(session, "moderator")$stream_async(
    "Raw Shiny prompt with private content."
  )
  raw_id <- tempest:::tempest_agent_completion_id(raw)
  raw_chunks <- character()
  repeat {
    value <- await_tempest_promise(raw())
    expect_null(value$error)
    if (coro::is_exhausted(value$value)) {
      break
    }
    raw_chunks <- c(
      raw_chunks,
      tempest:::tempest_deputy_adapter_content_text(value$value)
    )
  }
  client <- tempest:::tempest_session_chat(session, "moderator")
  direct_async <- await_tempest_promise(client$chat_async(
    "Direct async prompt with private content."
  ))
  direct_stream <- client$stream(
    "Direct stream prompt with private content."
  )
  direct_stream_id <- tempest:::tempest_agent_completion_id(direct_stream)
  direct_stream_chunk <- direct_stream()
  direct_stream_end <- direct_stream()

  completion_spans <- Filter(
    \(span) identical(span$name, "tempest.costorm.completion"),
    state$spans
  )
  turn_spans <- Filter(
    \(span) identical(span$name, "tempest.costorm.turn.commit"),
    state$spans
  )
  expect_identical(step$answer, "Synchronous response.")
  expect_null(request$error)
  expect_identical(
    tempest:::tempest_opaque_identifier_valid(request$value),
    TRUE
  )
  expect_identical(paste(raw_chunks, collapse = ""), "Raw Shiny response.")
  expect_identical(tempest:::tempest_opaque_identifier_valid(raw_id), TRUE)
  expect_null(direct_async$error)
  expect_identical(as.character(direct_async$value), "Direct async response.")
  expect_type(direct_async$value, "character")
  expect_identical(
    tempest:::tempest_opaque_identifier_valid(
      tempest:::tempest_agent_completion_id(direct_async$value)
    ),
    TRUE
  )
  expect_s3_class(direct_stream, "coro_generator_instance")
  expect_s3_class(direct_stream_chunk, "ellmer::ContentText")
  expect_identical(
    tempest:::tempest_deputy_adapter_content_text(direct_stream_chunk),
    "Direct stream response."
  )
  expect_identical(coro::is_exhausted(direct_stream_end), TRUE)
  expect_identical(
    tempest:::tempest_opaque_identifier_valid(direct_stream_id),
    TRUE
  )
  expect_length(completion_spans, 5L)
  expect_length(turn_spans, 1L)
  expect_identical(
    turn_spans[[1L]]$attributes[["tempest.status"]],
    "succeeded"
  )
  expect_identical(turn_spans[[1L]]$statuses, "ok")
  expect_identical(turn_spans[[1L]]$end_count, 1L)
  expect_identical(
    vapply(completion_spans, \(span) span$end_count, integer(1)),
    rep(1L, 5L)
  )
  attributes_json <- jsonlite::toJSON(
    lapply(completion_spans, \(span) span$attributes),
    auto_unbox = TRUE
  )
  expect_no_match(attributes_json, "private content", fixed = TRUE)
  expect_no_match(attributes_json, raw_id, fixed = TRUE)
})

test_that("recorder failure rolls back completion and leaves its run pending", {
  skip_if_not_installed("deputy")
  skip_if_not_installed("ellmer")
  skip_if_not_installed("later")
  skip_if_not_installed("promises")

  config <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) {
      if (identical(role, "coordinator")) {
        return(fake_chat(text = list("Unrecorded response.")))
      }
      fake_chat()
    }
  )
  session <- tempest_session(
    "Failed completion settlement",
    config = config,
    experts = list(test_expert(
      expert_id = "expert.failed-completion",
      name = "Dr. Failure"
    ))
  )
  local_mocked_bindings(
    tempest_session_record_deputy_trace = function(...) {
      stop("private recorder detail")
    }
  )

  request <- await_tempest_promise(
    session$.__enclos_env__$private$request_completion_async(
      "Fail exact settlement."
    )
  )

  expect_s3_class(request$error, "tempest_agent_completion_record_error")
  expect_no_match(
    conditionMessage(request$error),
    "private recorder detail",
    fixed = TRUE
  )
  pending <- tempest:::tempest_session_pending_deputy_runs(session)
  expect_length(pending, 1L)
  expect_identical(
    tempest:::tempest_opaque_identifier_valid(pending[[1L]]$completion_id),
    TRUE
  )
  expect_length(tempest:::tempest_session_deputy_traces(session), 0L)
  status_error <- tryCatch(
    tempest:::tempest_session_agent_completion_status(
      session,
      pending[[1L]]$completion_id
    ),
    error = identity
  )
  expect_s3_class(status_error, "tempest_agent_completion_id_error")
})

test_that("non-complete moderator terminals settle without completions", {
  skip_if_not_installed("deputy")
  skip_if_not_installed("ellmer")
  skip_if_not_installed("later")
  skip_if_not_installed("promises")

  moderator <- fake_chat()
  moderator$stream <- function(...) {
    coro::generator(function() {
      coro::yield(ellmer::ContentText("Partial moderator response"))
      stop("private sync provider failure")
    })()
  }
  moderator$stream_async <- function(...) {
    coro::async_generator(function() {
      coro::yield(ellmer::ContentText("Partial moderator response"))
      stop("private async provider failure")
    })()
  }
  moderator$clone <- function() moderator
  config <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) {
      if (identical(role, "coordinator")) {
        return(moderator)
      }
      fake_chat()
    }
  )
  session <- tempest_session(
    "Terminal moderator settlement",
    config = config,
    experts = list(test_expert(
      expert_id = "expert.terminal-settlement",
      name = "Dr. Terminal"
    ))
  )

  sync_error <- tryCatch(
    tempest:::tempest_session_chat(session, "moderator")$chat(
      "Fail synchronously."
    ),
    error = identity
  )
  async_result <- await_tempest_promise(
    session$.__enclos_env__$private$request_completion_async(
      "Fail asynchronously."
    )
  )

  expect_s3_class(sync_error, "tempest_deputy_adapter_error")
  expect_s3_class(async_result$error, "tempest_deputy_adapter_error")
  expect_null(async_result$value)
  expect_length(tempest:::tempest_session_pending_deputy_runs(session), 0L)
  traces <- tempest:::tempest_session_deputy_traces(session)
  expect_length(traces, 2L)
  expect_setequal(
    vapply(traces, `[[`, character(1), "status"),
    c("error", "error")
  )
  expect_length(
    tempest:::tempest_session_agent_completion_active(session),
    0L
  )
  expect_no_error(
    tempest:::tempest_session_agent_completion_assert_quiescent(session)
  )
  expect_no_error(tempest_session_snapshot(session))
})

test_that("terminal recorder failure remains pending and issues no ID", {
  skip_if_not_installed("deputy")
  skip_if_not_installed("ellmer")
  skip_if_not_installed("later")
  skip_if_not_installed("promises")

  moderator <- fake_chat()
  moderator$stream_async <- function(...) {
    coro::async_generator(function() {
      coro::yield(ellmer::ContentText("Unrecorded partial response"))
      stop("private provider failure")
    })()
  }
  moderator$clone <- function() moderator
  config <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) {
      if (identical(role, "coordinator")) {
        return(moderator)
      }
      fake_chat()
    }
  )
  session <- tempest_session(
    "Failed terminal settlement",
    config = config,
    experts = list(test_expert(
      expert_id = "expert.failed-terminal",
      name = "Dr. Recorder"
    ))
  )
  local_mocked_bindings(
    tempest_session_record_deputy_trace = function(...) {
      stop("private recorder detail")
    }
  )

  request <- await_tempest_promise(
    session$.__enclos_env__$private$request_completion_async(
      "Fail terminal recording."
    )
  )

  expect_s3_class(request$error, "tempest_agent_completion_record_error")
  expect_null(request$value)
  expect_no_match(
    conditionMessage(request$error),
    "private recorder detail",
    fixed = TRUE
  )
  pending <- tempest:::tempest_session_pending_deputy_runs(session)
  expect_length(pending, 1L)
  expect_length(tempest:::tempest_session_deputy_traces(session), 0L)
  expect_length(
    tempest:::tempest_session_agent_completion_active(session),
    0L
  )
  status_error <- tryCatch(
    tempest:::tempest_session_agent_completion_status(
      session,
      pending[[1L]]$completion_id
    ),
    error = identity
  )
  expect_s3_class(status_error, "tempest_agent_completion_id_error")
  expect_error(
    tempest_session_snapshot(session),
    class = "tempest_session_snapshot_error"
  )
})
