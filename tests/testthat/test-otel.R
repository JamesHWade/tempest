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
