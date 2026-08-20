test_that("OpenTelemetry names and allowlists are exact", {
  fixture <- function(name) {
    readLines(test_path("fixtures", name), warn = FALSE)
  }

  expect_identical(
    unname(tempest:::tempest_otel_span_names()),
    fixture("otel-span-names.txt")
  )
  expect_identical(
    names(tempest:::tempest_otel_span_names()),
    c(
      "storm.run",
      "costorm.completion",
      "costorm.turn.commit",
      "costorm.warmup",
      "costorm.report",
      "stage.execute",
      "retrieval.search",
      "retrieval.fetch"
    )
  )
  expect_identical(
    tempest:::tempest_otel_event_names(),
    fixture("otel-event-names.txt")
  )
  expect_identical(
    tempest:::tempest_otel_attribute_keys(),
    fixture("otel-attribute-keys.txt")
  )
  expect_identical(
    tempest:::tempest_otel_progress_stages(),
    c(
      "dialogue",
      "evidence",
      "mindmap",
      "outline",
      "persistence",
      "perspectives",
      "polish",
      "report",
      "research",
      "session",
      "suggestions",
      "verification",
      "warmup",
      "write"
    )
  )
  expect_identical(
    tempest:::tempest_otel_progress_steps(),
    c(
      "created",
      "delegate_to_expert",
      "expert_fanout",
      "fact_extraction",
      "generate",
      "moderator_response",
      "question_generation",
      "report_md",
      "turn",
      "update",
      "user_turn",
      "perspectives_artifacts",
      "research_artifacts",
      "outline_artifacts",
      "write_artifacts",
      "polish_artifacts"
    )
  )
  expect_identical(
    tempest:::tempest_otel_error_classes(),
    unique(c(
      tempest:::tempest_stage_failure_classes(),
      "tempest_run_cancelled",
      "tempest_run_error",
      "tempest_progress_callback_error",
      "tempest_async_cancelled",
      "tempest_async_error",
      "tempest_operation_error"
    ))
  )
  expect_identical(
    vapply(
      c("succeeded", "failed", "cancelled", "partial"),
      tempest:::tempest_otel_status_code,
      character(1)
    ),
    c(
      succeeded = "ok",
      failed = "error",
      cancelled = "unset",
      partial = "unset"
    )
  )
})

test_that("OpenTelemetry privacy gates fail before tracer lookup", {
  state <- local_fake_otel()
  withr::local_options(tempest.otel.enabled = FALSE)
  withr::local_envvar(c(
    OTEL_R_EMIT_SCOPES = "io.github.jameshwade.tempest",
    OTEL_INSTRUMENTATION_GENAI_CAPTURE_MESSAGE_CONTENT = "false"
  ))

  expect_null(tempest:::tempest_otel_context_start("storm.run"))
  options(tempest.otel.enabled = 1)
  expect_null(tempest:::tempest_otel_context_start("storm.run"))
  options(tempest.otel.enabled = TRUE)
  Sys.setenv(OTEL_R_EMIT_SCOPES = "*")
  expect_null(tempest:::tempest_otel_context_start("storm.run"))
  Sys.setenv(OTEL_R_EMIT_SCOPES = "io.github.jameshwade.tempest")
  Sys.setenv(OTEL_INSTRUMENTATION_GENAI_CAPTURE_MESSAGE_CONTENT = "true")
  expect_null(tempest:::tempest_otel_context_start("storm.run"))

  expect_identical(state$tracer_calls, 0L)
  expect_identical(state$enabled_calls, 0L)
  expect_identical(state$start_calls, 0L)
})

test_that("OpenTelemetry accepts only the bounded opt-in profile", {
  local_otel_opt_in()
  state <- local_fake_otel(installed = FALSE)

  expect_null(tempest:::tempest_otel_context_start("storm.run"))
  expect_identical(state$tracer_calls, 0L)

  Sys.setenv(
    OTEL_INSTRUMENTATION_GENAI_CAPTURE_MESSAGE_CONTENT = "  FALSE  "
  )
  expect_identical(tempest:::tempest_otel_worker_intent(), TRUE)
  Sys.unsetenv("OTEL_INSTRUMENTATION_GENAI_CAPTURE_MESSAGE_CONTENT")
  expect_identical(tempest:::tempest_otel_worker_intent(), TRUE)
})

test_that("a disabled tracer starts no span", {
  local_otel_opt_in()
  state <- local_fake_otel(enabled = FALSE)

  value <- tempest:::tempest_otel_trace("storm.run", 42L)

  expect_identical(value, 42L)
  expect_identical(state$tracer_calls, 1L)
  expect_identical(state$enabled_calls, 1L)
  expect_identical(state$start_calls, 0L)
})

test_that("package-gate failure is an exact product no-op", {
  local_otel_opt_in()
  state <- local_fake_otel(provider_errors = "has")

  value <- tempest:::tempest_otel_trace("storm.run", 42L)

  expect_identical(value, 42L)
  expect_identical(state$tracer_calls, 0L)
  expect_identical(state$start_calls, 0L)
})

test_that("startup interrupts stop sync and async product dispatch", {
  local_otel_opt_in()
  state <- local_fake_otel(provider_interrupts = "start_span")
  sync_calls <- 0L
  promise_calls <- 0L
  generator_calls <- 0L

  sync_error <- tryCatch(
    tempest:::tempest_otel_trace("storm.run", {
      sync_calls <- sync_calls + 1L
      42L
    }),
    interrupt = identity
  )
  promise_error <- tryCatch(
    tempest:::tempest_otel_trace_promise("costorm.completion", {
      promise_calls <- promise_calls + 1L
      promises::promise_resolve(42L)
    }),
    interrupt = identity
  )
  generator_error <- tryCatch(
    tempest:::tempest_otel_trace_generator("costorm.completion", {
      generator_calls <- generator_calls + 1L
      source <- coro::async_generator(function() {
        coro::yield("exact chunk")
        coro::exhausted()
      })()
      tempest:::tempest_agent_completion_tag(
        source,
        "completion-startup-interrupt"
      )
    }),
    interrupt = identity
  )

  expect_identical(sync_error, state$interrupt)
  expect_identical(promise_error, state$interrupt)
  expect_identical(generator_error, state$interrupt)
  expect_identical(sync_calls, 0L)
  expect_identical(promise_calls, 0L)
  expect_identical(generator_calls, 0L)
  expect_length(state$spans, 0L)
})

test_that("executor contexts reject progress-only stage names", {
  local_otel_opt_in()
  state <- local_fake_otel()

  context <- tempest:::tempest_otel_context_start(
    "stage.execute",
    stage = "dialogue"
  )

  expect_null(context)
  expect_identical(state$tracer_calls, 0L)
  expect_identical(state$start_calls, 0L)
})

test_that("owned spans project only closed progress fields", {
  local_otel_opt_in()
  state <- local_fake_otel()
  callback_event <- NULL
  state$order <- character()

  result <- tempest:::tempest_otel_trace("storm.run", {
    event <- tempest:::tempest_emit_progress(
      function(value) {
        state$order <- c(state$order, "callback")
        callback_event <<- value
        invisible(value)
      },
      run_id = "secret-run-id",
      workflow = "storm",
      event_type = "step",
      status = "running",
      stage = "dialogue",
      step = "user_turn",
      message = "Authorization: Bearer secret",
      payload = list(url = "https://secret.example", prompt = "secret"),
      event_id = "secret-event-id",
      timestamp = "2026-08-19T00:00:00.000000Z",
      correlation_id = "secret-correlation-id"
    )
    unknown <- tempest:::tempest_emit_progress(
      NULL,
      run_id = "another-secret",
      workflow = "storm",
      event_type = "step",
      status = "running",
      stage = "unreviewed-stage",
      step = "unreviewed-step"
    )
    list(event = event, unknown = unknown)
  })

  span <- state$spans[[1L]]
  expect_identical(callback_event, result$event)
  expect_identical(state$order, c("telemetry", "callback", "telemetry"))
  expect_identical(span$name, "tempest.storm.run")
  expect_identical(
    span$events[[1L]],
    list(
      name = "tempest.progress",
      attributes = list(
        "tempest.workflow" = "storm",
        "tempest.event_type" = "step",
        "tempest.status" = "running",
        "tempest.stage" = "dialogue",
        "tempest.step" = "user_turn"
      )
    )
  )
  expect_identical(
    span$events[[2L]]$attributes,
    list(
      "tempest.workflow" = "storm",
      "tempest.event_type" = "step",
      "tempest.status" = "running"
    )
  )
  projected <- jsonlite::toJSON(span$events, auto_unbox = TRUE)
  expect_no_match(projected, "secret", fixed = TRUE)
  expect_no_match(projected, "https://", fixed = TRUE)
  expect_identical(span$attributes[["tempest.operation"]], "storm.run")
  expect_identical(span$attributes[["tempest.workflow"]], "storm")
  expect_identical(span$attributes[["tempest.status"]], "succeeded")
  expect_identical(span$statuses, "ok")
  expect_identical(span$activate_count, 1L)
  expect_identical(span$deactivate_count, 1L)
  expect_identical(span$end_count, 1L)
})

test_that("nearest owned context receives progress without ambient lookup", {
  local_otel_opt_in()
  state <- local_fake_otel()

  tempest:::tempest_otel_trace("storm.run", {
    tempest:::tempest_otel_trace(
      "stage.execute",
      tempest:::tempest_emit_progress(
        NULL,
        run_id = "run-1",
        workflow = "storm",
        event_type = "stage",
        status = "started",
        stage = "research"
      ),
      stage = "query_decomposition"
    )
    tempest:::tempest_emit_progress(
      NULL,
      run_id = "run-1",
      workflow = "storm",
      event_type = "workflow",
      status = "succeeded"
    )
  })

  expect_length(state$spans, 2L)
  expect_identical(
    state$spans[[1L]]$events[[1L]]$attributes,
    list(
      "tempest.workflow" = "storm",
      "tempest.event_type" = "workflow",
      "tempest.status" = "succeeded"
    )
  )
  expect_identical(
    state$spans[[2L]]$events[[1L]]$attributes,
    list(
      "tempest.workflow" = "storm",
      "tempest.event_type" = "stage",
      "tempest.status" = "started",
      "tempest.stage" = "research"
    )
  )
  expect_identical(state$spans[[1L]]$end_count, 1L)
  expect_identical(state$spans[[2L]]$end_count, 1L)
})

test_that("progress without an owned operation never creates telemetry", {
  local_otel_opt_in()
  state <- local_fake_otel()

  event <- tempest:::tempest_emit_progress(
    NULL,
    run_id = "run-1",
    workflow = "storm",
    event_type = "workflow",
    status = "started"
  )

  expect_s7_class(event, tempest_progress_event)
  expect_identical(state$tracer_calls, 0L)
  expect_length(state$spans, 0L)
})

test_that("projection-internal failure preserves the host callback", {
  local_otel_opt_in()
  local_fake_otel()
  local_mocked_bindings(
    tempest_otel_current_context = function() stop("adapter failure")
  )
  callback_calls <- 0L

  event <- tempest:::tempest_emit_progress(
    function(value) {
      callback_calls <<- callback_calls + 1L
      invisible(value)
    },
    run_id = "run-1",
    workflow = "storm",
    event_type = "workflow",
    status = "started"
  )

  expect_s7_class(event, tempest_progress_event)
  expect_identical(callback_calls, 1L)
})

test_that("terminal errors use only safe classes and exact statuses", {
  local_otel_opt_in()
  state <- local_fake_otel()
  original <- rlang::error_cnd(
    "tempest_progress_callback_error",
    message = "Authorization: Bearer secret"
  )

  caught <- tryCatch(
    tempest:::tempest_otel_trace("storm.run", stop(original)),
    error = identity
  )
  span <- state$spans[[1L]]

  expect_identical(caught, original)
  expect_identical(span$attributes[["tempest.status"]], "failed")
  expect_identical(
    span$attributes[["tempest.error_class"]],
    "tempest_progress_callback_error"
  )
  expect_identical(span$statuses, "error")
  expect_identical(span$end_count, 1L)
  expect_no_match(
    jsonlite::toJSON(span$attributes, auto_unbox = TRUE),
    "secret",
    fixed = TRUE
  )
})

test_that("unknown failures and cancellation have bounded projections", {
  local_otel_opt_in()
  state <- local_fake_otel()

  tryCatch(
    tempest:::tempest_otel_trace(
      "storm.run",
      rlang::abort("secret", class = "provider_secret_error")
    ),
    error = identity
  )
  tryCatch(
    tempest:::tempest_otel_trace(
      "storm.run",
      rlang::abort(
        "secret",
        class = c("tempest_run_cancelled", "tempest_run_error")
      )
    ),
    error = identity
  )

  failed <- state$spans[[1L]]
  cancelled <- state$spans[[2L]]
  expect_identical(
    failed$attributes[["tempest.error_class"]],
    "tempest_operation_error"
  )
  expect_identical(failed$statuses, "error")
  expect_identical(cancelled$attributes[["tempest.status"]], "cancelled")
  expect_identical(cancelled$attributes[["tempest.cancelled"]], TRUE)
  expect_null(cancelled$attributes[["tempest.error_class"]])
  expect_identical(cancelled$statuses, "unset")
  expect_identical(cancelled$end_count, 1L)
})

test_that("provider conditions and failures cannot alter product behavior", {
  local_otel_opt_in()
  state <- local_fake_otel(
    provider_errors = c("add_event", "set_attribute", "set_status", "end"),
    provider_conditions = c(
      "activate",
      "add_event",
      "set_attribute",
      "set_status",
      "deactivate",
      "end"
    )
  )
  callback_calls <- 0L

  expect_silent(
    value <- tempest:::tempest_otel_trace("storm.run", {
      event <- tempest:::tempest_emit_progress(
        function(event) {
          callback_calls <<- callback_calls + 1L
          invisible(event)
        },
        run_id = "run-1",
        workflow = "storm",
        event_type = "workflow",
        status = "started"
      )
      list(value = 42L, event = event)
    })
  )

  expect_identical(value$value, 42L)
  expect_identical(callback_calls, 1L)
  expect_identical(state$spans[[1L]]$activate_count, 1L)
  expect_warning(
    tempest:::tempest_otel_trace("storm.run", {
      warning("product warning")
      1L
    }),
    "product warning"
  )
})

test_that("activation failure ends the span without an invented scope", {
  local_otel_opt_in()
  state <- local_fake_otel(provider_errors = "activate")

  value <- tempest:::tempest_otel_trace("storm.run", 42L)
  span <- state$spans[[1L]]

  expect_identical(value, 42L)
  expect_identical(span$activate_count, 1L)
  expect_identical(span$deactivate_count, 0L)
  expect_identical(span$end_count, 1L)
})

test_that("activation interrupt cancels and ends before product execution", {
  local_otel_opt_in()
  state <- local_fake_otel(provider_interrupts = "activate")
  body_calls <- 0L

  caught <- tryCatch(
    tempest:::tempest_otel_trace("storm.run", {
      body_calls <- body_calls + 1L
      42L
    }),
    interrupt = identity
  )
  span <- state$spans[[1L]]

  expect_identical(caught, state$interrupt)
  expect_identical(body_calls, 0L)
  expect_identical(span$activate_count, 1L)
  expect_identical(span$deactivate_count, 0L)
  expect_identical(span$attributes[["tempest.status"]], "cancelled")
  expect_identical(span$attributes[["tempest.cancelled"]], TRUE)
  expect_identical(span$statuses, "unset")
  expect_identical(span$end_count, 1L)
})

test_that("deactivation interrupt propagates after one end attempt", {
  local_otel_opt_in()
  state <- local_fake_otel(provider_interrupts = "deactivate")

  caught <- tryCatch(
    tempest:::tempest_otel_trace("storm.run", 42L),
    interrupt = identity
  )
  span <- state$spans[[1L]]

  expect_identical(caught, state$interrupt)
  expect_identical(span$attributes[["tempest.status"]], "succeeded")
  expect_identical(span$statuses, "ok")
  expect_identical(span$deactivate_count, 0L)
  expect_identical(span$end_count, 1L)
})

test_that("terminal telemetry interrupts cannot replace product errors", {
  local_otel_opt_in()
  state <- local_fake_otel(provider_interrupts = "set_attribute")
  original <- rlang::error_cnd(
    "tempest_run_error",
    message = "original product error"
  )

  caught <- tryCatch(
    tempest:::tempest_otel_trace("storm.run", stop(original)),
    error = identity
  )
  span <- state$spans[[1L]]

  expect_identical(caught, original)
  expect_identical(span$deactivate_count, 1L)
  expect_identical(span$end_count, 1L)
})

test_that("cleanup interrupts cannot replace product errors", {
  local_otel_opt_in()
  state <- local_fake_otel(provider_interrupts = "deactivate")
  original <- rlang::error_cnd(
    "tempest_run_error",
    message = "original product error"
  )

  caught <- tryCatch(
    tempest:::tempest_otel_trace("storm.run", stop(original)),
    error = identity
  )
  span <- state$spans[[1L]]

  expect_identical(caught, original)
  expect_identical(span$deactivate_count, 0L)
  expect_identical(span$end_count, 1L)
})

test_that("synchronous stage spans start only after validated preflight", {
  local_otel_opt_in()
  state <- local_fake_otel()
  execution <- tempest:::tempest_program_set_execution(
    tempest_program_set(),
    "query_decomposition"
  )

  expect_error(
    tempest:::tempest_execute_stage(
      execution,
      chat = NULL,
      inputs = list(),
      context = list()
    ),
    class = "tempest_stage_error"
  )
  expect_identical(state$start_calls, 0L)
})

test_that("synchronous stage fallback is traced without product drift", {
  local_otel_opt_in()
  state <- local_fake_otel()
  local_mocked_bindings(
    tempest_run_dsprrr_module_structured = function(...) {
      stop("primary provider secret")
    }
  )
  execution <- tempest:::tempest_program_set_execution(
    tempest_program_set(),
    "query_decomposition"
  )

  result <- tempest:::tempest_execute_stage(
    execution,
    chat = NULL,
    inputs = list(question = "Question", topic = "Topic"),
    context = list(
      max_queries = 3L,
      attempt_id = "attempt-fixed",
      now = function() "2026-08-19T00:00:00.000000Z"
    )
  )
  span <- state$spans[[1L]]

  expect_s3_class(result, "tempest_stage_result")
  expect_identical(result$record@fallback_taken, TRUE)
  expect_identical(span$name, "tempest.stage.execute")
  expect_identical(
    span$attributes[["tempest.operation"]],
    "stage.execute"
  )
  expect_identical(
    span$attributes[["tempest.stage"]],
    "query_decomposition"
  )
  expect_identical(span$attributes[["tempest.status"]], "succeeded")
  expect_identical(span$attributes[["tempest.fallback_taken"]], TRUE)
  expect_identical(span$statuses, "ok")
  expect_identical(span$end_count, 1L)
})

test_that("enabled stage telemetry preserves authoritative bytes", {
  local_otel_opt_in()
  state <- local_fake_otel()
  local_mocked_bindings(
    tempest_run_dsprrr_module_structured = function(...) {
      list(output = list(queries = "Question"))
    }
  )
  run_stage <- function() {
    execution <- tempest:::tempest_program_set_execution(
      tempest_program_set(),
      "query_decomposition"
    )
    tempest:::tempest_execute_stage(
      execution,
      chat = NULL,
      inputs = list(question = "Question", topic = "Topic"),
      context = list(
        max_queries = 3L,
        attempt_id = "attempt-fixed",
        now = function() "2026-08-19T00:00:00.000000Z"
      )
    )
  }

  options(tempest.otel.enabled = FALSE)
  disabled <- run_stage()
  options(tempest.otel.enabled = TRUE)
  enabled <- run_stage()

  expect_identical(serialize(enabled, NULL), serialize(disabled, NULL))
  expect_length(state$spans, 1L)
})

test_that("tempest_run owns one context without authoritative drift", {
  local_otel_opt_in()
  state <- local_fake_otel()
  authoritative <- list(report_md = "fixed report", state = list(version = 1L))
  local_mocked_bindings(
    tempest_run_internal = function(...) {
      tempest:::tempest_emit_progress(
        NULL,
        run_id = "run-1",
        workflow = "storm",
        event_type = "workflow",
        status = "succeeded"
      )
      authoritative
    }
  )

  options(tempest.otel.enabled = FALSE)
  disabled <- tempest_run("Topic", config = NULL, verbose = FALSE)
  options(tempest.otel.enabled = TRUE)
  enabled <- tempest_run("Topic", config = NULL, verbose = FALSE)

  expect_identical(serialize(enabled, NULL), serialize(disabled, NULL))
  expect_identical(enabled, authoritative)
  expect_length(state$spans, 1L)
  expect_length(state$spans[[1L]]$events, 1L)
  expect_identical(state$spans[[1L]]$end_count, 1L)
})

test_that("worker opt-in is Boolean and worker-local options are restored", {
  withr::local_options(tempest.otel.enabled = "host-value")
  seen <- NULL
  value <- tempest:::tempest_otel_worker_call(
    function(value) {
      seen <<- getOption("tempest.otel.enabled")
      value
    },
    list(value = 42L),
    TRUE
  )

  expect_identical(value, 42L)
  expect_identical(seen, TRUE)
  expect_identical(getOption("tempest.otel.enabled"), "host-value")

  original <- simpleError("worker error")
  caught <- tryCatch(
    tempest:::tempest_otel_worker_call(
      function() stop(original),
      list(),
      FALSE
    ),
    error = identity
  )
  expect_identical(caught, original)
  expect_identical(getOption("tempest.otel.enabled"), "host-value")
  expect_error(
    tempest:::tempest_otel_worker_call(identity, list(NULL), 1),
    class = "tempest_async_error"
  )
})

test_that("retrieval state accepts only bounded branch-local fields", {
  state <- tempest:::tempest_otel_retrieval_state()

  expect_identical(
    tempest:::tempest_otel_retrieval_state_valid(state),
    TRUE
  )
  expect_identical(
    tempest:::tempest_otel_retrieval_search_result_count(
      data.frame(value = 1:3),
      2L
    ),
    2L
  )
  expect_null(
    tempest:::tempest_otel_retrieval_search_result_count(
      list(value = "not a data frame"),
      2L
    )
  )

  state$cache_hit <- 1L
  expect_identical(
    tempest:::tempest_otel_retrieval_state_valid(state),
    FALSE
  )
})

test_that("retrieval tracing projects successful and returned failure states", {
  local_otel_opt_in()
  state <- local_fake_otel()
  success <- tempest:::tempest_otel_retrieval_state()
  success$cache_hit <- TRUE
  success$result_count <- 2L
  success$fallback_taken <- TRUE
  success$outcome <- "succeeded"
  success_value <- list(value = "exact success")

  traced_success <- tempest:::tempest_otel_trace_retrieval(
    "retrieval.search",
    success_value,
    success
  )

  failure <- tempest:::tempest_otel_retrieval_state()
  failure$result_count <- 0L
  failure$outcome <- "failed"
  failure_value <- list(value = "exact failure record")
  traced_failure <- tempest:::tempest_otel_trace_retrieval(
    "retrieval.fetch",
    failure_value,
    failure
  )

  expect_identical(traced_success, success_value)
  expect_identical(traced_failure, failure_value)
  expect_identical(
    state$spans[[1L]]$attributes,
    list(
      "tempest.operation" = "retrieval.search",
      "tempest.cache_hit" = TRUE,
      "tempest.result_count" = 2L,
      "tempest.status" = "succeeded",
      "tempest.fallback_taken" = TRUE
    )
  )
  expect_identical(state$spans[[1L]]$statuses, "ok")
  expect_identical(state$spans[[1L]]$end_count, 1L)
  expect_identical(
    state$spans[[2L]]$attributes,
    list(
      "tempest.operation" = "retrieval.fetch",
      "tempest.cache_hit" = FALSE,
      "tempest.result_count" = 0L,
      "tempest.status" = "failed",
      "tempest.error_class" = "tempest_operation_error"
    )
  )
  expect_identical(state$spans[[2L]]$statuses, "error")
  expect_identical(state$spans[[2L]]$end_count, 1L)
})

test_that("retrieval tracing preserves exact errors and cancellation", {
  local_otel_opt_in()
  state <- local_fake_otel()
  error_state <- tempest:::tempest_otel_retrieval_state()
  error_state$cache_hit <- TRUE
  original <- structure(
    list(message = "private retrieval detail", call = NULL),
    class = c("private_retrieval_error", "error", "condition")
  )

  caught <- tryCatch(
    tempest:::tempest_otel_trace_retrieval(
      "retrieval.fetch",
      stop(original),
      error_state
    ),
    error = identity
  )

  cancel_state <- tempest:::tempest_otel_retrieval_state()
  cancelled <- structure(
    list(message = "private cancellation detail", call = NULL),
    class = c("interrupt", "condition")
  )
  caught_cancel <- tryCatch(
    tempest:::tempest_otel_trace_retrieval(
      "retrieval.search",
      signalCondition(cancelled),
      cancel_state
    ),
    interrupt = identity
  )

  expect_identical(caught, original)
  expect_identical(caught_cancel, cancelled)
  expect_identical(
    state$spans[[1L]]$attributes[["tempest.cache_hit"]],
    TRUE
  )
  expect_identical(
    state$spans[[1L]]$attributes[["tempest.error_class"]],
    "tempest_operation_error"
  )
  expect_null(state$spans[[1L]]$attributes[["tempest.result_count"]])
  expect_identical(
    state$spans[[2L]]$attributes[["tempest.status"]],
    "cancelled"
  )
  expect_identical(
    state$spans[[2L]]$attributes[["tempest.cancelled"]],
    TRUE
  )
  expect_identical(state$spans[[2L]]$statuses, "unset")
  expect_null(state$spans[[2L]]$attributes[["tempest.result_count"]])
  expect_identical(
    grepl(
      "private",
      jsonlite::toJSON(lapply(state$spans, \(span) span$attributes)),
      fixed = TRUE
    ),
    FALSE
  )
})

test_that("async promise spans deactivate at dispatch and end at settlement", {
  local_otel_opt_in()
  state <- local_fake_otel()
  control <- new.env(parent = emptyenv())
  original <- list(value = 42L)
  request <- promises::promise(function(resolve, reject) {
    control$resolve <- resolve
    control$reject <- reject
  })

  traced <- tempest:::tempest_otel_trace_promise(
    "costorm.completion",
    request
  )
  span <- state$spans[[1L]]

  expect_identical(span$activate_count, 1L)
  expect_identical(span$deactivate_count, 1L)
  expect_identical(span$end_count, 0L)

  control$resolve(original)
  settled <- await_tempest_promise(traced)

  expect_null(settled$error)
  expect_identical(settled$value, original)
  expect_identical(span$attributes[["tempest.status"]], "succeeded")
  expect_identical(span$statuses, "ok")
  expect_identical(span$end_count, 1L)
})

test_that("async rejection preserves the exact condition and closes once", {
  local_otel_opt_in()
  state <- local_fake_otel()
  control <- new.env(parent = emptyenv())
  original <- rlang::error_cnd(
    "provider_private_error",
    message = "private response detail"
  )
  request <- promises::promise(function(resolve, reject) {
    control$reject <- reject
  })

  traced <- tempest:::tempest_otel_trace_promise(
    "costorm.completion",
    request
  )
  control$reject(original)
  settled <- await_tempest_promise(traced)
  span <- state$spans[[1L]]

  expect_identical(settled$error, original)
  expect_identical(
    span$attributes[["tempest.error_class"]],
    "tempest_operation_error"
  )
  expect_identical(span$attributes[["tempest.status"]], "failed")
  expect_identical(span$statuses, "error")
  expect_identical(span$end_count, 1L)
  expect_no_match(
    jsonlite::toJSON(span$attributes, auto_unbox = TRUE),
    "private response detail",
    fixed = TRUE
  )
})

test_that("invalid async result shapes fail telemetry without product drift", {
  local_otel_opt_in()
  state <- local_fake_otel()
  operations <- c(
    "costorm.turn.commit",
    "costorm.warmup",
    "costorm.report",
    "stage.execute"
  )
  original <- list(exact = 42L)

  for (operation in operations) {
    traced <- tempest:::tempest_otel_trace_promise(
      operation,
      promises::promise_resolve(original),
      stage = if (identical(operation, "stage.execute")) {
        "query_decomposition"
      }
    )
    settled <- await_tempest_promise(traced)
    span <- state$spans[[length(state$spans)]]

    expect_identical(settled$value, original)
    expect_null(settled$error)
    expect_identical(span$attributes[["tempest.status"]], "failed")
    expect_identical(
      span$attributes[["tempest.error_class"]],
      "tempest_operation_error"
    )
    expect_identical(span$statuses, "error")
    expect_identical(span$end_count, 1L)
  }
})

test_that("late async progress stays with its captured operation context", {
  local_otel_opt_in()
  state <- local_fake_otel()
  control <- new.env(parent = emptyenv())
  callback_calls <- 0L
  request <- promises::promise(function(resolve, reject) {
    control$resolve <- resolve
  })
  traced <- tempest:::tempest_otel_trace_promise(
    "costorm.turn.commit",
    tempest:::tempest_otel_then(request, function(value) {
      callback_calls <<- callback_calls + 1L
      tempest:::tempest_emit_progress(
        NULL,
        run_id = "private-run-a",
        workflow = "costorm",
        event_type = "stage",
        status = "succeeded",
        stage = "dialogue",
        step = "turn"
      )
      value
    })
  )

  value_b <- tempest:::tempest_otel_trace("costorm.report", {
    control$resolve("exact result a")
    later::run_now(timeoutSecs = 0.1)
    "exact result b"
  })
  settled <- await_tempest_promise(traced)
  span_a <- state$spans[[1L]]
  span_b <- state$spans[[2L]]

  expect_identical(value_b, "exact result b")
  expect_identical(settled$value, "exact result a")
  expect_null(settled$error)
  expect_identical(callback_calls, 1L)
  expect_length(span_a$events, 1L)
  expect_identical(
    span_a$events[[1L]]$attributes,
    list(
      "tempest.workflow" = "costorm",
      "tempest.event_type" = "stage",
      "tempest.status" = "succeeded",
      "tempest.stage" = "dialogue",
      "tempest.step" = "turn"
    )
  )
  expect_length(span_b$events, 0L)
  expect_identical(span_a$end_count, 1L)
  expect_identical(span_b$end_count, 1L)
})

test_that("failed inner context startup blocks ambient async progress", {
  local_otel_opt_in()
  state <- local_fake_otel(
    start_span_errors = "tempest.costorm.turn.commit"
  )
  callback_calls <- 0L

  products <- tempest:::tempest_otel_trace("storm.run", {
    inner_context <- tempest:::tempest_otel_fail_open(
      tempest:::tempest_otel_context_start("costorm.turn.commit")
    )
    execute <- function() {
      .tempest_otel_context <- inner_context
      tempest:::tempest_otel_then(
        promises::promise_resolve("exact inner"),
        function(value) {
          callback_calls <<- callback_calls + 1L
          tempest:::tempest_emit_progress(
            NULL,
            run_id = "private-inner-run",
            workflow = "costorm",
            event_type = "stage",
            status = "succeeded",
            stage = "dialogue",
            step = "turn"
          )
          value
        }
      )
    }
    inner <- tempest:::tempest_otel_trace_promise(
      "costorm.turn.commit",
      execute(),
      context = inner_context
    )
    inner_settled <- await_tempest_promise(inner)
    list(outer = "exact outer", inner = inner_settled$value)
  })
  outer_span <- state$spans[[1L]]

  expect_identical(
    products,
    list(outer = "exact outer", inner = "exact inner")
  )
  expect_identical(callback_calls, 1L)
  expect_identical(state$start_calls, 2L)
  expect_length(state$spans, 1L)
  expect_length(outer_span$events, 0L)
  expect_identical(outer_span$end_count, 1L)
})

test_that("completion owners isolate cancelled and newer async spans", {
  local_otel_opt_in()
  state <- local_fake_otel()
  owner <- tempest:::tempest_otel_owner()
  old_control <- new.env(parent = emptyenv())
  new_control <- new.env(parent = emptyenv())
  old_request <- promises::promise(function(resolve, reject) {
    old_control$resolve <- resolve
  })
  old_traced <- tempest:::tempest_otel_trace_promise(
    "costorm.completion",
    old_request,
    owner = owner
  )

  expect_identical(tempest:::tempest_otel_owner_cancel(owner), TRUE)
  old_span <- state$spans[[1L]]
  expect_identical(old_span$attributes[["tempest.status"]], "cancelled")
  expect_identical(old_span$end_count, 1L)

  new_request <- promises::promise(function(resolve, reject) {
    new_control$resolve <- resolve
  })
  new_traced <- tempest:::tempest_otel_trace_promise(
    "costorm.completion",
    new_request,
    owner = owner
  )
  new_span <- state$spans[[2L]]
  old_control$resolve("old exact value")
  old_settled <- await_tempest_promise(old_traced)

  expect_identical(old_settled$value, "old exact value")
  expect_identical(old_span$attributes[["tempest.status"]], "cancelled")
  expect_identical(old_span$end_count, 1L)
  expect_identical(new_span$end_count, 0L)

  new_control$resolve("new exact value")
  new_settled <- await_tempest_promise(new_traced)

  expect_identical(new_settled$value, "new exact value")
  expect_identical(new_span$attributes[["tempest.status"]], "succeeded")
  expect_identical(new_span$end_count, 1L)
})

test_that("owner cancellation ends spans after provider interrupts", {
  local_otel_opt_in()
  state <- local_fake_otel(provider_interrupts = "set_attribute")
  owner <- tempest:::tempest_otel_owner()
  control <- new.env(parent = emptyenv())
  request <- promises::promise(function(resolve, reject) {
    control$resolve <- resolve
  })
  traced <- tempest:::tempest_otel_trace_promise(
    "costorm.completion",
    request,
    owner = owner
  )
  key <- ls(owner$contexts, all.names = TRUE)
  context <- get(key, envir = owner$contexts, inherits = FALSE)
  span <- state$spans[[1L]]

  expect_identical(tempest:::tempest_otel_owner_cancel(owner), TRUE)
  expect_identical(span$end_count, 1L)
  expect_identical(tempest:::tempest_otel_context_live(context), FALSE)
  expect_length(ls(owner$contexts, all.names = TRUE), 0L)

  control$resolve("exact late value")
  settled <- await_tempest_promise(traced)

  expect_identical(settled$value, "exact late value")
  expect_null(settled$error)
  expect_identical(span$end_count, 1L)
  expect_length(ls(owner$contexts, all.names = TRUE), 0L)
})

test_that("retired completion owners reject late telemetry registration", {
  local_otel_opt_in()
  state <- local_fake_otel()
  owner <- tempest:::tempest_otel_owner()
  control <- new.env(parent = emptyenv())
  request <- promises::promise(function(resolve, reject) {
    control$resolve <- resolve
  })
  active <- tempest:::tempest_otel_trace_promise(
    "costorm.completion",
    request,
    owner = owner
  )
  active_span <- state$spans[[1L]]

  expect_identical(tempest:::tempest_otel_owner_retire(owner), TRUE)
  expect_identical(active_span$end_count, 1L)
  expect_length(ls(owner$contexts, all.names = TRUE), 0L)
  start_calls <- state$start_calls
  tracer_calls <- state$tracer_calls
  late <- tempest:::tempest_otel_trace_promise(
    "costorm.completion",
    promises::promise_resolve("exact late result"),
    owner = owner
  )
  late_settled <- await_tempest_promise(late)

  expect_identical(late_settled$value, "exact late result")
  expect_identical(state$start_calls, start_calls)
  expect_identical(state$tracer_calls, tracer_calls)
  expect_length(ls(owner$contexts, all.names = TRUE), 0L)

  control$resolve("exact active result")
  active_settled <- await_tempest_promise(active)
  expect_identical(active_settled$value, "exact active result")
  expect_identical(active_span$end_count, 1L)
})

test_that("async dispatch errors clean up after provider interrupts", {
  local_otel_opt_in()
  state <- local_fake_otel(provider_interrupts = "set_attribute")
  owner <- tempest:::tempest_otel_owner()
  original <- rlang::error_cnd(
    "private_dispatch_error",
    message = "private dispatch detail"
  )

  caught <- tryCatch(
    tempest:::tempest_otel_trace_promise(
      "costorm.completion",
      stop(original),
      owner = owner
    ),
    error = identity
  )
  span <- state$spans[[1L]]

  expect_identical(caught, original)
  expect_identical(span$end_count, 1L)
  expect_length(ls(owner$contexts, all.names = TRUE), 0L)
})

test_that("invalid traced generators fail telemetry without product drift", {
  local_otel_opt_in()
  state <- local_fake_otel()
  owner <- tempest:::tempest_otel_owner()
  source <- coro::async_generator(function() {
    coro::yield("exact raw value")
    coro::exhausted()
  })()

  traced <- tempest:::tempest_otel_trace_generator(
    "costorm.completion",
    source,
    owner = owner
  )
  span <- state$spans[[1L]]
  first <- await_tempest_promise(traced())

  expect_identical(traced, source)
  expect_identical(first$value, "exact raw value")
  expect_null(first$error)
  expect_identical(span$attributes[["tempest.status"]], "failed")
  expect_identical(
    span$attributes[["tempest.error_class"]],
    "tempest_operation_error"
  )
  expect_identical(span$statuses, "error")
  expect_identical(span$end_count, 1L)
  expect_length(ls(owner$contexts, all.names = TRUE), 0L)
})

test_that("async generator spans close on exhaustion and explicit disposal", {
  local_otel_opt_in()
  state <- local_fake_otel()
  owner <- tempest:::tempest_otel_owner()
  make_source <- function(completion_id, values, calls) {
    source <- coro::async_generator(function() {
      for (value in values) {
        coro::yield(value)
      }
      coro::exhausted()
    })()
    observed <- function(arg, close = FALSE) {
      if (close) {
        calls$close <- calls$close + 1L
      }
      if (missing(arg)) {
        source(close = close)
      } else {
        source(arg, close = close)
      }
    }
    class(observed) <- class(source)
    tempest:::tempest_agent_completion_tag(observed, completion_id)
  }
  success_calls <- new.env(parent = emptyenv())
  success_calls$close <- 0L
  source <- make_source(
    "completion-generator-success",
    "exact chunk",
    success_calls
  )

  traced <- tempest:::tempest_otel_trace_generator(
    "costorm.completion",
    source,
    owner = owner
  )
  span <- state$spans[[1L]]
  chunk <- await_tempest_promise(traced())
  exhausted <- await_tempest_promise(traced())

  expect_identical(chunk$value, "exact chunk")
  expect_identical(coro::is_exhausted(exhausted$value), TRUE)
  expect_identical(
    tempest:::tempest_agent_completion_id(traced),
    "completion-generator-success"
  )
  expect_identical(span$attributes[["tempest.status"]], "succeeded")
  expect_identical(span$end_count, 1L)
  expect_identical(success_calls$close, 0L)

  abandoned_calls <- new.env(parent = emptyenv())
  abandoned_calls$close <- 0L
  abandoned <- make_source(
    "completion-generator-abandoned",
    c("first chunk", "second chunk"),
    abandoned_calls
  )
  abandoned <- tempest:::tempest_otel_trace_generator(
    "costorm.completion",
    abandoned,
    owner = owner
  )
  abandoned_span <- state$spans[[2L]]
  expect_identical(
    await_tempest_promise(abandoned())$value,
    "first chunk"
  )
  closed <- abandoned(close = TRUE)
  if (promises::is.promising(closed)) {
    await_tempest_promise(closed)
  }

  expect_identical(
    abandoned_span$attributes[["tempest.status"]],
    "cancelled"
  )
  expect_identical(abandoned_span$attributes[["tempest.cancelled"]], TRUE)
  expect_identical(abandoned_span$end_count, 1L)
  expect_identical(abandoned_calls$close, 1L)

  closed_again <- abandoned(close = TRUE)
  expect_identical(coro::is_exhausted(closed_again), TRUE)
  expect_identical(abandoned_calls$close, 2L)
  expect_identical(abandoned_span$end_count, 1L)

  unopened_calls <- new.env(parent = emptyenv())
  unopened_calls$close <- 0L
  unopened <- make_source(
    "completion-generator-unopened",
    "never yielded",
    unopened_calls
  )
  unopened <- tempest:::tempest_otel_trace_generator(
    "costorm.completion",
    unopened,
    owner = owner
  )
  unopened_span <- state$spans[[3L]]
  unopened_close <- unopened(close = 1)

  expect_identical(coro::is_exhausted(unopened_close), TRUE)
  expect_identical(unopened_calls$close, 1L)
  expect_identical(
    unopened_span$attributes[["tempest.status"]],
    "cancelled"
  )
  expect_identical(unopened_span$end_count, 1L)

  retry_calls <- new.env(parent = emptyenv())
  retry_calls$close <- 0L
  retry_error <- rlang::error_cnd(
    "test_raw_close_error",
    message = "private raw close detail"
  )
  retry_source <- coro::async_generator(function() {
    coro::yield("retry value")
    coro::exhausted()
  })()
  retry_observed <- function(arg, close = FALSE) {
    if (close) {
      retry_calls$close <- retry_calls$close + 1L
      if (identical(retry_calls$close, 1L)) {
        stop(retry_error)
      }
    }
    if (missing(arg)) {
      retry_source(close = close)
    } else {
      retry_source(arg, close = close)
    }
  }
  class(retry_observed) <- class(retry_source)
  retry_observed <- tempest:::tempest_agent_completion_tag(
    retry_observed,
    "completion-generator-retry-close"
  )
  retry <- tempest:::tempest_otel_trace_generator(
    "costorm.completion",
    retry_observed,
    owner = owner
  )
  retry_span <- state$spans[[4L]]
  first_close_error <- tryCatch(retry(close = TRUE), error = identity)

  expect_identical(first_close_error, retry_error)
  expect_identical(retry_calls$close, 1L)
  expect_identical(retry_span$end_count, 1L)
  retry_close <- retry(close = TRUE)
  expect_identical(coro::is_exhausted(retry_close), TRUE)
  expect_identical(retry_calls$close, 2L)
  expect_identical(retry_span$end_count, 1L)

  invalid_calls <- new.env(parent = emptyenv())
  invalid_calls$close <- 0L
  invalid <- make_source(
    "completion-generator-invalid-close",
    "still available",
    invalid_calls
  )
  invalid <- tempest:::tempest_otel_trace_generator(
    "costorm.completion",
    invalid,
    owner = owner
  )
  invalid_span <- state$spans[[5L]]
  invalid_error <- tryCatch(invalid(close = NA), error = identity)

  expect_s3_class(invalid_error, "simpleError")
  expect_identical(
    conditionMessage(invalid_error),
    "missing value where TRUE/FALSE needed"
  )
  expect_identical(invalid_calls$close, 0L)
  expect_identical(invalid_span$end_count, 0L)
  invalid(close = TRUE)
  expect_identical(invalid_span$end_count, 1L)
})

test_that("traced generators forward resume values to their source", {
  local_otel_opt_in()
  state <- local_fake_otel()
  source <- coro::async_generator(function() {
    resumed <- coro::yield("first value")
    coro::yield(resumed)
    coro::exhausted()
  })()
  source <- tempest:::tempest_agent_completion_tag(
    source,
    "completion-generator-resume"
  )
  stream <- tempest:::tempest_otel_trace_generator(
    "costorm.completion",
    source,
    owner = tempest:::tempest_otel_owner()
  )

  first <- await_tempest_promise(stream())
  resumed <- await_tempest_promise(stream("sent value"))
  exhausted <- await_tempest_promise(stream())

  expect_identical(first$value, "first value")
  expect_identical(resumed$value, "sent value")
  expect_identical(coro::is_exhausted(exhausted$value), TRUE)
  expect_identical(
    state$spans[[1L]]$attributes[["tempest.status"]],
    "succeeded"
  )
  expect_identical(state$spans[[1L]]$end_count, 1L)
})

test_that("traced generators queue concurrent pre-yield resumes", {
  local_otel_opt_in()
  state <- local_fake_otel()
  control <- new.env(parent = emptyenv())
  gate <- promises::promise(function(resolve, reject) {
    control$resolve <- resolve
  })
  source <- coro::async_generator(function() {
    first <- coro::await(gate)
    resumed <- coro::yield(first)
    coro::yield(resumed)
    coro::exhausted()
  })()
  source <- tempest:::tempest_agent_completion_tag(
    source,
    "completion-generator-concurrent-resume"
  )
  stream <- tempest:::tempest_otel_trace_generator(
    "costorm.completion",
    source,
    owner = tempest:::tempest_otel_owner()
  )

  first_request <- stream()
  resumed_request <- stream("concurrent")
  control$resolve("first")
  first <- await_tempest_promise(first_request)
  resumed <- await_tempest_promise(resumed_request)
  exhausted <- await_tempest_promise(stream())

  expect_identical(first$value, "first")
  expect_identical(resumed$value, "concurrent")
  expect_identical(coro::is_exhausted(exhausted$value), TRUE)
  expect_identical(
    state$spans[[1L]]$attributes[["tempest.status"]],
    "succeeded"
  )
  expect_identical(state$spans[[1L]]$end_count, 1L)
})

test_that("traced generator failures preserve the exact condition", {
  local_otel_opt_in()
  state <- local_fake_otel()
  original <- rlang::error_cnd(
    "private_generator_error",
    message = "private generator detail"
  )
  sources <- list(
    coro::async_generator(function() {
      coro::yield("first value")
      stop(original)
    })(),
    coro::async_generator(function() {
      coro::yield("first value")
      coro::await(promises::promise_reject(original))
      coro::exhausted()
    })()
  )

  for (index in seq_along(sources)) {
    source <- tempest:::tempest_agent_completion_tag(
      sources[[index]],
      paste0("completion-generator-error-", index)
    )
    stream <- tempest:::tempest_otel_trace_generator(
      "costorm.completion",
      source,
      owner = tempest:::tempest_otel_owner()
    )

    first <- await_tempest_promise(stream())
    failed <- tryCatch(
      await_tempest_promise(stream()),
      error = function(error) list(value = NULL, error = error)
    )
    span <- state$spans[[index]]

    expect_identical(first$value, "first value")
    expect_identical(failed$error, original)
    expect_identical(span$attributes[["tempest.status"]], "failed")
    expect_identical(
      span$attributes[["tempest.error_class"]],
      "tempest_operation_error"
    )
    expect_identical(span$statuses, "error")
    expect_identical(span$end_count, 1L)
  }
})

test_that("completion client wrapping preserves shape and disabled results", {
  withr::local_options(tempest.otel.enabled = FALSE)
  owner <- tempest:::tempest_otel_owner()
  response <- tempest:::tempest_agent_completion_tag(
    "exact response",
    "completion-shape"
  )
  source <- coro::async_generator(function() {
    coro::yield("exact chunk")
    coro::exhausted()
  })()
  source <- tempest:::tempest_agent_completion_tag(
    source,
    "completion-shape"
  )
  client <- NULL
  client <- structure(
    list(
      chat = function(
        prompt,
        echo = "none",
        run_context = list(),
        ...
      ) {
        response
      },
      stream = function(
        prompt = NULL,
        stream = c("text", "content"),
        controller = NULL,
        run_context = list(),
        ...
      ) {
        source
      },
      chat_async = function(
        prompt,
        echo = "none",
        run_context = list(),
        ...
      ) {
        promises::promise_resolve(response)
      },
      stream_async = function(
        prompt = NULL,
        stream = c("text", "content"),
        controller = NULL,
        run_context = list(),
        ...
      ) {
        source
      },
      clone = function() client,
      get_model = function() "fixed-model"
    ),
    class = c("TempestDeputyChatAdapter", "Chat", "list")
  )

  wrapped <- tempest:::tempest_otel_wrap_completion_client(client, owner)

  expect_identical(class(wrapped), class(client))
  expect_identical(wrapped$get_model(), client$get_model())
  expect_identical(wrapped$chat("prompt"), response)
  expect_identical(wrapped$stream_async("prompt"), source)
  expect_identical(wrapped$clone(), wrapped)
  expect_identical(
    names(formals(wrapped$chat)),
    c("prompt", "echo", "run_context", "...")
  )
  expect_identical(
    names(formals(wrapped$stream_async)),
    c("prompt", "stream", "controller", "run_context", "...")
  )
  expect_null(names(formals(wrapped$clone)))
  expect_identical(
    tempest:::tempest_otel_completion_owner(wrapped),
    owner
  )
})

test_that("adapter source and opt-in docs retain static privacy gates", {
  context <- test_source_inventory_context()
  if (!identical(context$mode, "source")) {
    expect_identical(context$mode, "installed")
    return(invisible(NULL))
  }
  root <- context$root
  adapter <- paste(
    readLines(file.path(root, "R", "otel.R"), warn = FALSE),
    collapse = "\n"
  )
  search_code <- paste(
    deparse(body(tempest:::TempestRetriever$public_methods$search)),
    collapse = "\n"
  )
  fetch_code <- paste(
    deparse(body(tempest:::TempestRetriever$public_methods$fetch)),
    collapse = "\n"
  )
  readme <- paste(
    readLines(file.path(root, "README.md"), warn = FALSE),
    collapse = "\n"
  )
  forbidden <- c(
    "otel::as_attributes",
    "record_exception",
    "conditionMessage",
    "event@message",
    "event@payload",
    "event@run_id",
    "event@event_id",
    "event@timestamp",
    "event@parent_event_id",
    "event@correlation_id",
    "tempest_search_cache_key",
    "tempest_fetch_cache_key",
    "cache_stats",
    "Logfire"
  )
  present <- forbidden[vapply(
    forbidden,
    grepl,
    logical(1),
    x = adapter,
    fixed = TRUE
  )]

  expect_identical(present, character())
  expect_no_match(search_code, "cache_stats", fixed = TRUE)
  expect_no_match(fetch_code, "cache_stats", fixed = TRUE)
  expect_match(
    adapter,
    "OTEL_R_EMIT_SCOPES",
    fixed = TRUE
  )
  expect_match(
    readme,
    "OTEL_R_EMIT_SCOPES=io.github.jameshwade.tempest",
    fixed = TRUE
  )
  expect_match(
    readme,
    "OTEL_INSTRUMENTATION_GENAI_CAPTURE_MESSAGE_CONTENT=false",
    fixed = TRUE
  )
})
