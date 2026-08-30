# Experimental OpenTelemetry projection ------------------------------------

tempest_otel_scope <- function() {
  "io.github.jameshwade.tempest"
}

tempest_otel_span_names <- function() {
  c(
    "storm.run" = "tempest.storm.run",
    "costorm.completion" = "tempest.costorm.completion",
    "costorm.turn.commit" = "tempest.costorm.turn.commit",
    "costorm.warmup" = "tempest.costorm.warmup",
    "costorm.report" = "tempest.costorm.report",
    "stage.execute" = "tempest.stage.execute",
    "retrieval.search" = "tempest.retrieval.search",
    "retrieval.fetch" = "tempest.retrieval.fetch"
  )
}

tempest_otel_event_names <- function() {
  "tempest.progress"
}

tempest_otel_attribute_keys <- function() {
  c(
    "tempest.workflow",
    "tempest.operation",
    "tempest.event_type",
    "tempest.stage",
    "tempest.step",
    "tempest.status",
    "tempest.cache_hit",
    "tempest.result_count",
    "tempest.fallback_taken",
    "tempest.cancelled",
    "tempest.error_class"
  )
}

tempest_otel_progress_stages <- function() {
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
}

tempest_otel_progress_steps <- function() {
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
}

tempest_otel_error_classes <- function() {
  unique(c(
    tempest_stage_failure_classes(),
    "tempest_run_cancelled",
    "tempest_run_error",
    "tempest_progress_callback_error",
    "tempest_async_cancelled",
    "tempest_async_error",
    "tempest_operation_error"
  ))
}

tempest_otel_status_code <- function(status) {
  if (identical(status, "succeeded")) {
    return("ok")
  }
  if (identical(status, "failed")) {
    return("error")
  }
  "unset"
}

tempest_otel_worker_intent <- function() {
  enabled <- identical(getOption("tempest.otel.enabled", FALSE), TRUE)
  scope <- Sys.getenv("OTEL_R_EMIT_SCOPES", unset = NA_character_)
  capture <- Sys.getenv(
    "OTEL_INSTRUMENTATION_GENAI_CAPTURE_MESSAGE_CONTENT",
    unset = NA_character_
  )
  capture_safe <- is.na(capture) ||
    identical(tolower(trimws(capture)), "false")

  enabled && identical(scope, tempest_otel_scope()) && capture_safe
}

tempest_otel_has <- function() {
  requireNamespace("otel", quietly = TRUE)
}

tempest_otel_get_tracer <- function() {
  otel::get_tracer(name = tempest_otel_scope())
}

tempest_otel_is_tracing_enabled <- function(tracer) {
  otel::is_tracing_enabled(tracer)
}

tempest_otel_start_span <- function(tracer, name, attributes) {
  otel::start_span(
    name = name,
    attributes = attributes,
    tracer = tracer
  )
}

tempest_otel_provider_call <- function(expr, default = NULL) {
  tryCatch(
    withCallingHandlers(
      force(expr),
      warning = function(...) {
        tryInvokeRestart("muffleWarning")
      },
      message = function(...) {
        tryInvokeRestart("muffleMessage")
      }
    ),
    error = function(...) default
  )
}

tempest_otel_attribute_valid <- function(key, value) {
  if (identical(key, "tempest.workflow")) {
    return(rlang::is_string(value) && value %in% tempest_progress_workflows())
  }
  if (identical(key, "tempest.operation")) {
    return(
      rlang::is_string(value) && value %in% names(tempest_otel_span_names())
    )
  }
  if (identical(key, "tempest.event_type")) {
    return(
      rlang::is_string(value) && value %in% tempest_progress_event_types()
    )
  }
  if (identical(key, "tempest.stage")) {
    stages <- unique(c(
      tempest_program_set_stages(),
      tempest_otel_progress_stages()
    ))
    return(rlang::is_string(value) && value %in% stages)
  }
  if (identical(key, "tempest.step")) {
    return(
      rlang::is_string(value) && value %in% tempest_otel_progress_steps()
    )
  }
  if (identical(key, "tempest.status")) {
    statuses <- unique(c(tempest_progress_statuses(), "partial"))
    return(rlang::is_string(value) && value %in% statuses)
  }
  if (
    key %in%
      c(
        "tempest.cache_hit",
        "tempest.fallback_taken",
        "tempest.cancelled"
      )
  ) {
    return(rlang::is_bool(value))
  }
  if (identical(key, "tempest.result_count")) {
    return(
      is.numeric(value) &&
        length(value) == 1L &&
        !is.na(value) &&
        is.finite(value) &&
        value >= 0 &&
        value == floor(value)
    )
  }
  if (identical(key, "tempest.error_class")) {
    return(
      rlang::is_string(value) && value %in% tempest_otel_error_classes()
    )
  }
  FALSE
}

tempest_otel_attributes <- function(attributes) {
  if (!is.list(attributes) || is.null(names(attributes))) {
    return(list())
  }
  kept <- list()
  for (key in names(attributes)) {
    value <- attributes[[key]]
    if (
      key %in%
        tempest_otel_attribute_keys() &&
        tempest_otel_attribute_valid(key, value)
    ) {
      kept[[key]] <- value
    }
  }
  kept
}

tempest_otel_workflow <- function(operation) {
  if (identical(operation, "storm.run")) {
    return("storm")
  }
  if (
    operation %in%
      c(
        "costorm.completion",
        "costorm.turn.commit",
        "costorm.warmup",
        "costorm.report"
      )
  ) {
    return("costorm")
  }
  NULL
}

tempest_otel_context_sentinel <- new.env(parent = emptyenv())

tempest_otel_no_context_sentinel <- new.env(parent = emptyenv())

tempest_otel_owner_sentinel <- new.env(parent = emptyenv())

tempest_otel_retrieval_state_sentinel <- new.env(parent = emptyenv())

tempest_otel_retrieval_state <- function() {
  state <- new.env(parent = emptyenv())
  state$sentinel <- tempest_otel_retrieval_state_sentinel
  state$cache_hit <- FALSE
  state$result_count <- NULL
  state$fallback_taken <- FALSE
  state$outcome <- NULL
  state
}

tempest_otel_retrieval_state_valid <- function(state) {
  is.environment(state) &&
    identical(state$sentinel, tempest_otel_retrieval_state_sentinel) &&
    rlang::is_bool(state$cache_hit) &&
    (is.null(state$result_count) ||
      tempest_otel_attribute_valid(
        "tempest.result_count",
        state$result_count
      )) &&
    rlang::is_bool(state$fallback_taken) &&
    (is.null(state$outcome) ||
      (rlang::is_string(state$outcome) &&
        state$outcome %in% c("succeeded", "failed")))
}

tempest_otel_retrieval_search_result_count <- function(result, maximum) {
  tempest_otel_fail_open({
    if (
      !is.data.frame(result) ||
        !is.numeric(maximum) ||
        length(maximum) != 1L ||
        is.na(maximum) ||
        !is.finite(maximum) ||
        maximum < 0 ||
        maximum != floor(maximum)
    ) {
      return(NULL)
    }
    count <- nrow(result)
    if (
      !is.numeric(count) ||
        length(count) != 1L ||
        is.na(count) ||
        !is.finite(count) ||
        count < 0 ||
        count != floor(count)
    ) {
      return(NULL)
    }
    min(count, maximum)
  })
}

tempest_otel_owner <- function() {
  owner <- new.env(parent = emptyenv())
  owner$sentinel <- tempest_otel_owner_sentinel
  owner$contexts <- new.env(hash = TRUE, parent = emptyenv())
  owner$next_id <- 0L
  owner$retired <- FALSE
  owner
}

tempest_otel_owner_valid <- function(owner) {
  is.environment(owner) &&
    identical(owner$sentinel, tempest_otel_owner_sentinel) &&
    is.environment(owner$contexts) &&
    is.integer(owner$next_id) &&
    length(owner$next_id) == 1L &&
    rlang::is_bool(owner$retired)
}

tempest_otel_owner_register <- function(owner, context) {
  if (
    !tempest_otel_owner_valid(owner) ||
      isTRUE(owner$retired) ||
      !is.environment(context)
  ) {
    return(invisible(context))
  }
  owner$next_id <- owner$next_id + 1L
  key <- as.character(owner$next_id)
  context$owner <- owner
  context$owner_key <- key
  assign(key, context, envir = owner$contexts)
  invisible(context)
}

tempest_otel_owner_unregister <- function(context) {
  if (!is.environment(context)) {
    return(invisible(context))
  }
  owner <- context$owner %||% NULL
  key <- context$owner_key %||% NULL
  if (
    tempest_otel_owner_valid(owner) &&
      rlang::is_string(key) &&
      exists(key, envir = owner$contexts, inherits = FALSE)
  ) {
    rm(list = key, envir = owner$contexts)
  }
  context$owner <- NULL
  context$owner_key <- NULL
  invisible(context)
}

tempest_otel_context_start <- function(operation, stage = NULL, owner = NULL) {
  if (
    !rlang::is_string(operation) ||
      !operation %in% names(tempest_otel_span_names()) ||
      !tempest_otel_worker_intent()
  ) {
    return(NULL)
  }
  if (tempest_otel_owner_valid(owner) && isTRUE(owner$retired)) {
    return(NULL)
  }
  if (
    identical(operation, "stage.execute") &&
      (!rlang::is_string(stage) || !stage %in% tempest_program_set_stages())
  ) {
    return(NULL)
  }
  installed <- tempest_otel_provider_call(
    tempest_otel_has(),
    default = FALSE
  )
  if (!identical(installed, TRUE)) {
    return(NULL)
  }

  tracer <- tempest_otel_provider_call(tempest_otel_get_tracer())
  if (is.null(tracer)) {
    return(NULL)
  }
  enabled <- tempest_otel_provider_call(
    tempest_otel_is_tracing_enabled(tracer),
    default = FALSE
  )
  if (!identical(enabled, TRUE)) {
    return(NULL)
  }

  attributes <- list("tempest.operation" = operation)
  workflow <- tempest_otel_workflow(operation)
  if (!is.null(workflow)) {
    attributes[["tempest.workflow"]] <- workflow
  }
  if (!is.null(stage)) {
    attributes[["tempest.stage"]] <- stage
  }
  attributes <- tempest_otel_attributes(attributes)
  span <- tempest_otel_provider_call(tempest_otel_start_span(
    tracer,
    unname(tempest_otel_span_names()[[operation]]),
    attributes
  ))
  if (is.null(span)) {
    return(NULL)
  }

  context <- new.env(parent = emptyenv())
  context$sentinel <- tempest_otel_context_sentinel
  context$tracer <- tracer
  context$span <- span
  context$generation <- new.env(parent = emptyenv())
  context$ending <- FALSE
  context$ended <- FALSE
  context$deactivation_attempted <- FALSE
  context$end_attempted <- FALSE
  context$activation_scope <- NULL
  context$owner <- NULL
  context$owner_key <- NULL
  tempest_otel_owner_register(owner, context)
  context
}

tempest_otel_context_live <- function(context) {
  is.environment(context) &&
    identical(context$sentinel, tempest_otel_context_sentinel) &&
    is.environment(context$generation) &&
    identical(context$ending, FALSE) &&
    identical(context$ended, FALSE)
}

tempest_otel_context_activate <- function(context) {
  if (!tempest_otel_context_live(context)) {
    return(invisible(context))
  }
  context$activation_scope <- tempest_otel_provider_call(
    context$span$activate(
      activation_scope = NULL,
      end_on_exit = FALSE
    ),
    default = NULL
  )
  invisible(context)
}

tempest_otel_context_set_attribute <- function(context, key, value) {
  if (
    !tempest_otel_context_live(context) ||
      !tempest_otel_attribute_valid(key, value)
  ) {
    return(invisible(context))
  }
  tempest_otel_provider_call(context$span$set_attribute(key, value))
  invisible(context)
}

tempest_otel_context_set_status <- function(context, status) {
  if (
    !tempest_otel_context_live(context) ||
      !status %in% c("unset", "ok", "error")
  ) {
    return(invisible(context))
  }
  tempest_otel_provider_call(context$span$set_status(status))
  invisible(context)
}

tempest_otel_context_deactivate <- function(context) {
  if (
    !is.environment(context) ||
      !identical(context$sentinel, tempest_otel_context_sentinel) ||
      is.null(context$activation_scope) ||
      identical(context$deactivation_attempted, TRUE)
  ) {
    return(invisible(context))
  }
  context$deactivation_attempted <- TRUE
  tempest_otel_provider_call(
    context$span$deactivate(context$activation_scope)
  )
  invisible(context)
}

tempest_otel_context_end <- function(context) {
  if (!tempest_otel_context_live(context)) {
    return(invisible(context))
  }
  deactivate_span <- function() {
    tryCatch(
      {
        tempest_otel_context_deactivate(context)
        NULL
      },
      interrupt = identity
    )
  }
  end_span <- function() {
    if (identical(context$end_attempted, TRUE)) {
      return(NULL)
    }
    context$end_attempted <- TRUE
    tryCatch(
      {
        tempest_otel_provider_call(context$span$end())
        NULL
      },
      interrupt = identity
    )
  }
  on.exit(
    {
      if (!identical(context$deactivation_attempted, TRUE)) {
        deactivate_span()
      }
      if (!identical(context$end_attempted, TRUE)) {
        end_span()
      }
      tempest_otel_owner_unregister(context)
      context$ended <- TRUE
      context$ending <- FALSE
    },
    add = TRUE
  )
  context$ending <- TRUE

  deactivation_interrupt <- deactivate_span()
  end_interrupt <- end_span()
  pending_interrupt <- deactivation_interrupt %||% end_interrupt
  if (!is.null(pending_interrupt)) {
    stop(pending_interrupt)
  }
  invisible(context)
}

tempest_otel_current_context <- function() {
  frames <- sys.frames()
  if (length(frames) == 0L) {
    return(NULL)
  }
  for (index in rev(seq_along(frames))) {
    frame <- frames[[index]]
    repeat {
      if (exists(".tempest_otel_context", envir = frame, inherits = FALSE)) {
        context <- get(
          ".tempest_otel_context",
          envir = frame,
          inherits = FALSE
        )
        if (identical(context, tempest_otel_no_context_sentinel)) {
          return(NULL)
        }
        if (is.null(context)) {
          return(NULL)
        }
        if (tempest_otel_context_live(context)) {
          return(context)
        }
        if (
          is.environment(context) &&
            identical(context$sentinel, tempest_otel_context_sentinel)
        ) {
          return(NULL)
        }
      }
      if (identical(frame, emptyenv())) {
        break
      }
      frame <- parent.env(frame)
    }
  }
  NULL
}

tempest_otel_callback <- function(callback, context = NULL) {
  if (is.null(callback)) {
    return(NULL)
  }
  if (!is.function(callback)) {
    return(callback)
  }
  if (missing(context)) {
    context <- tempest_otel_current_context()
  }
  if (is.null(context)) {
    context <- tempest_otel_no_context_sentinel
  }
  force(callback)
  force(context)
  function(...) {
    .tempest_otel_context <- context
    callback(...)
  }
}

tempest_otel_then <- function(
  promise,
  onFulfilled = NULL,
  onRejected = NULL
) {
  promises::then(
    promise,
    onFulfilled = tempest_otel_callback(onFulfilled),
    onRejected = tempest_otel_callback(onRejected)
  )
}

tempest_otel_catch <- function(promise, onRejected) {
  promises::catch(
    promise,
    onRejected = tempest_otel_callback(onRejected)
  )
}

tempest_otel_finally <- function(promise, onFinally) {
  promises::finally(
    promise,
    onFinally = tempest_otel_callback(onFinally)
  )
}

tempest_otel_progress_attributes <- function(event) {
  attributes <- list(
    "tempest.workflow" = event@workflow,
    "tempest.event_type" = event@event_type,
    "tempest.status" = event@status
  )
  if (!is.na(event@stage) && event@stage %in% tempest_otel_progress_stages()) {
    attributes[["tempest.stage"]] <- event@stage
  }
  if (!is.na(event@step) && event@step %in% tempest_otel_progress_steps()) {
    attributes[["tempest.step"]] <- event@step
  }
  tempest_otel_attributes(attributes)
}

tempest_otel_progress_event <- function(event) {
  tempest_otel_provider_call(
    {
      context <- tempest_otel_current_context()
      if (
        !is.null(context) &&
          S7::S7_inherits(event, tempest_progress_event)
      ) {
        attributes <- tempest_otel_progress_attributes(event)
        tempest_otel_provider_call(
          context$span$add_event(tempest_otel_event_names(), attributes)
        )
      }
      invisible(event)
    },
    default = invisible(event)
  )
  invisible(event)
}

tempest_otel_safe_error_class <- function(error) {
  known <- class(error)
  known <- known[known %in% tempest_otel_error_classes()]
  if (length(known) > 0L) {
    return(known[[1L]])
  }
  "tempest_operation_error"
}

tempest_otel_cancelled <- function(error) {
  any(vapply(
    c(
      "tempest_stage_cancelled",
      "tempest_run_cancelled",
      "tempest_async_cancelled",
      "interrupt"
    ),
    inherits,
    logical(1),
    x = error
  ))
}

tempest_otel_fail_open <- function(expr, default = NULL) {
  tryCatch(
    expr,
    error = function(...) default,
    interrupt = function(...) default
  )
}

tempest_otel_mark_status <- function(
  context,
  status,
  fallback_taken = FALSE,
  error = NULL
) {
  if (
    !status %in% c("succeeded", "failed", "cancelled", "partial", "skipped")
  ) {
    return(invisible(context))
  }
  tempest_otel_context_set_attribute(context, "tempest.status", status)
  if (identical(status, "succeeded") && identical(fallback_taken, TRUE)) {
    tempest_otel_context_set_attribute(
      context,
      "tempest.fallback_taken",
      TRUE
    )
  }
  if (identical(status, "cancelled")) {
    tempest_otel_context_set_attribute(context, "tempest.cancelled", TRUE)
  }
  if (identical(status, "failed") && !is.null(error)) {
    tempest_otel_context_set_attribute(
      context,
      "tempest.error_class",
      tempest_otel_safe_error_class(error)
    )
  }
  tempest_otel_context_set_status(
    context,
    tempest_otel_status_code(status)
  )
  invisible(context)
}

tempest_otel_complete_success <- function(context, fallback_taken = FALSE) {
  on.exit(tempest_otel_context_end(context), add = TRUE)
  tempest_otel_mark_status(
    context,
    "succeeded",
    fallback_taken = fallback_taken
  )
  tempest_otel_context_end(context)
}

tempest_otel_complete_status <- function(
  context,
  status,
  fallback_taken = FALSE
) {
  on.exit(tempest_otel_context_end(context), add = TRUE)
  tempest_otel_mark_status(
    context,
    status,
    fallback_taken = fallback_taken
  )
  tempest_otel_context_end(context)
}

tempest_otel_complete_error <- function(context, error) {
  on.exit(tempest_otel_context_end(context), add = TRUE)
  status <- if (tempest_otel_cancelled(error)) "cancelled" else "failed"
  tempest_otel_mark_status(context, status, error = error)
  tempest_otel_context_end(context)
}

tempest_otel_owner_cancel <- function(owner) {
  if (!tempest_otel_owner_valid(owner)) {
    return(invisible(FALSE))
  }
  keys <- ls(owner$contexts, all.names = TRUE)
  for (key in keys) {
    context <- get(key, envir = owner$contexts, inherits = FALSE)
    tempest_otel_fail_open(
      tempest_otel_mark_status(context, "cancelled")
    )
    tempest_otel_fail_open(tempest_otel_context_end(context))
  }
  invisible(length(keys) > 0L)
}

tempest_otel_owner_retire <- function(owner) {
  if (!tempest_otel_owner_valid(owner)) {
    return(invisible(FALSE))
  }
  was_retired <- owner$retired
  owner$retired <- TRUE
  tempest_otel_owner_cancel(owner)
  invisible(!was_retired)
}

tempest_otel_result_status <- function(operation, result) {
  if (identical(operation, "costorm.completion")) {
    return(list(status = "succeeded", fallback_taken = FALSE))
  }
  if (
    identical(operation, "costorm.turn.commit") &&
      S7::S7_inherits(result, TempestSessionTurnResult)
  ) {
    return(list(status = result@status, fallback_taken = FALSE))
  }
  if (
    identical(operation, "costorm.warmup") &&
      S7::S7_inherits(result, TempestWarmupResult)
  ) {
    return(list(status = result@status, fallback_taken = FALSE))
  }
  if (identical(operation, "costorm.report")) {
    if (is.null(result)) {
      return(list(status = "cancelled", fallback_taken = FALSE))
    }
    if (is.character(result) && length(result) == 1L && !is.na(result)) {
      return(list(status = "succeeded", fallback_taken = FALSE))
    }
    return(NULL)
  }
  if (
    identical(operation, "stage.execute") &&
      inherits(result, "tempest_stage_result")
  ) {
    return(list(
      status = "succeeded",
      fallback_taken = tempest_otel_result_fallback(operation, result)
    ))
  }
  NULL
}

tempest_otel_mark_result <- function(context, operation, result) {
  terminal <- tempest_otel_fail_open(
    tempest_otel_result_status(operation, result)
  )
  if (!is.list(terminal)) {
    tempest_otel_mark_status(
      context,
      "failed",
      error = simpleError("Operation result has no terminal projection.")
    )
    return(invisible(context))
  }
  tempest_otel_mark_status(
    context,
    terminal$status,
    fallback_taken = terminal$fallback_taken
  )
}

tempest_otel_trace_promise <- function(
  operation,
  code,
  stage = NULL,
  owner = NULL,
  context = NULL
) {
  if (missing(context)) {
    context <- tempest_otel_provider_call(
      tempest_otel_context_start(operation, stage = stage, owner = owner)
    )
  }
  if (is.null(context)) {
    .tempest_otel_context <- tempest_otel_no_context_sentinel
    return(force(code))
  }

  .tempest_otel_context <- context
  handle_setup_error <- function(error) {
    tempest_otel_fail_open(tempest_otel_complete_error(context, error))
    stop(error)
  }
  tryCatch(
    {
      tempest_otel_context_activate(context)
      value <- force(code)
      tempest_otel_context_deactivate(context)
      settled <- promises::then(
        value,
        onFulfilled = function(value) {
          tempest_otel_fail_open(
            tempest_otel_mark_result(context, operation, value)
          )
          value
        },
        onRejected = function(error) {
          tempest_otel_fail_open(tempest_otel_mark_status(
            context,
            if (tempest_otel_cancelled(error)) "cancelled" else "failed",
            error = error
          ))
          stop(error)
        }
      )
      promises::finally(
        settled,
        function() {
          tempest_otel_fail_open(tempest_otel_context_end(context))
          invisible(NULL)
        }
      )
    },
    error = handle_setup_error,
    interrupt = handle_setup_error
  )
}

tempest_async_generator_fifo <- function(generator, before_call) {
  state <- new.env(parent = emptyenv())
  state$active <- FALSE
  state$started <- FALSE
  state$queue <- list()
  pump <- NULL
  pump <- function() {
    if (state$active) {
      return(invisible(NULL))
    }
    repeat {
      if (length(state$queue) == 0L) {
        return(invisible(NULL))
      }
      request <- state$queue[[1L]]
      state$queue[[1L]] <- NULL
      state$active <- TRUE
      condition <- NULL
      value <- tryCatch(
        {
          before_call(request$value, state$started)
          state$started <- TRUE
          generator()
        },
        error = function(error) {
          condition <<- error
          NULL
        },
        interrupt = function(error) {
          condition <<- error
          NULL
        }
      )
      if (!is.null(condition)) {
        state$active <- FALSE
        request$reject(condition)
        next
      }
      if (!promises::is.promising(value)) {
        state$active <- FALSE
        request$resolve(value)
        next
      }
      promises::then(
        value,
        onFulfilled = function(value) {
          state$active <- FALSE
          request$resolve(value)
          pump()
          invisible(NULL)
        },
        onRejected = function(error) {
          state$active <- FALSE
          request$reject(error)
          pump()
          invisible(NULL)
        }
      )
      return(invisible(NULL))
    }
  }
  function(value) {
    promises::promise(function(resolve, reject) {
      state$queue[[length(state$queue) + 1L]] <- list(
        value = value,
        resolve = resolve,
        reject = reject
      )
      pump()
    })
  }
}

tempest_otel_trace_generator <- function(operation, code, owner = NULL) {
  context <- tempest_otel_provider_call(
    tempest_otel_context_start(operation, owner = owner)
  )
  if (is.null(context)) {
    .tempest_otel_context <- tempest_otel_no_context_sentinel
    return(force(code))
  }

  .tempest_otel_context <- context
  source <- tryCatch(
    {
      tempest_otel_context_activate(context)
      value <- force(code)
      tempest_otel_context_deactivate(context)
      value
    },
    error = function(error) {
      tempest_otel_fail_open(tempest_otel_complete_error(context, error))
      stop(error)
    },
    interrupt = function(error) {
      tempest_otel_fail_open(tempest_otel_complete_error(context, error))
      stop(error)
    }
  )
  completion_condition <- NULL
  completion_id <- tryCatch(
    tempest_agent_completion_id(source),
    error = function(error) {
      completion_condition <<- error
      NULL
    },
    interrupt = function(error) {
      completion_condition <<- error
      NULL
    }
  )
  if (is.null(completion_id)) {
    if (is.null(completion_condition)) {
      completion_condition <- simpleError(
        "Agent completion identifier is unavailable."
      )
    }
    tempest_otel_fail_open(
      tempest_otel_complete_error(context, completion_condition)
    )
    return(source)
  }

  state <- new.env(parent = emptyenv())
  state$terminal <- FALSE
  state$resume <- list(supplied = FALSE, value = NULL)
  generator <- coro::async_generator(function() {
    .tempest_otel_context <- context
    on.exit(
      {
        if (!state$terminal) {
          tempest_otel_fail_open(
            tempest_otel_mark_status(context, "cancelled")
          )
          state$terminal <- TRUE
        }
        tempest_otel_fail_open(tempest_otel_context_end(context))
      },
      add = TRUE
    )
    tryCatch(
      {
        source_arg_supplied <- FALSE
        source_arg <- NULL
        repeat {
          if (source_arg_supplied) {
            value <- source(source_arg)
          } else {
            value <- source()
          }
          if (promises::is.promising(value)) {
            value <- coro::await(value)
          }
          if (coro::is_exhausted(value)) {
            tempest_otel_fail_open(
              tempest_otel_mark_status(context, "succeeded")
            )
            state$terminal <- TRUE
            break
          }
          coro::yield(value)
          source_arg_supplied <- state$resume$supplied
          source_arg <- state$resume$value
        }
      },
      error = function(error) {
        tempest_otel_fail_open(tempest_otel_mark_status(
          context,
          if (tempest_otel_cancelled(error)) "cancelled" else "failed",
          error = error
        ))
        state$terminal <- TRUE
        stop(error)
      },
      interrupt = function(error) {
        tempest_otel_fail_open(
          tempest_otel_mark_status(context, "cancelled", error = error)
        )
        state$terminal <- TRUE
        stop(error)
      }
    )
    coro::exhausted()
  })()
  dispatch <- tempest_async_generator_fifo(
    generator,
    function(resume, started) {
      if (started) {
        state$resume <- resume
      } else {
        state$resume <- list(supplied = FALSE, value = NULL)
      }
    }
  )
  stream <- function(arg, close = FALSE) {
    if (close) {
      if (!state$terminal) {
        state$terminal <- TRUE
        tempest_otel_fail_open(
          tempest_otel_mark_status(context, "cancelled")
        )
      }
      source_condition <- NULL
      tryCatch(
        {
          if (missing(arg)) {
            source(close = TRUE)
          } else {
            source(arg, close = TRUE)
          }
        },
        error = function(error) source_condition <<- error,
        interrupt = function(error) source_condition <<- error
      )
      tempest_otel_fail_open(tempest_otel_context_end(context))
      generator_condition <- NULL
      value <- tryCatch(
        generator(close = TRUE),
        error = function(error) generator_condition <<- error,
        interrupt = function(error) generator_condition <<- error
      )
      if (!is.null(source_condition)) {
        stop(source_condition)
      }
      if (!is.null(generator_condition)) {
        stop(generator_condition)
      }
      return(value)
    }
    dispatch(list(
      supplied = !missing(arg),
      value = if (missing(arg)) NULL else arg
    ))
  }
  class(stream) <- class(generator)
  tempest_agent_completion_tag(stream, completion_id)
}

tempest_otel_completion_owner <- function(client) {
  owner <- attr(client, ".tempest_otel_completion_owner", exact = TRUE)
  if (tempest_otel_owner_valid(owner)) owner else NULL
}

tempest_otel_wrap_completion_client <- function(client, owner) {
  if (
    !inherits(client, "TempestDeputyChatAdapter") ||
      !tempest_otel_owner_valid(owner)
  ) {
    return(client)
  }
  existing <- tempest_otel_completion_owner(client)
  if (tempest_otel_owner_valid(existing)) {
    return(client)
  }

  chat_call <- client$chat
  stream_call <- client$stream
  chat_async_call <- client$chat_async
  stream_async_call <- client$stream_async
  proxy <- tempest_deputy_chat_proxy(client)
  proxy$chat <- function(
    prompt,
    echo = "none",
    run_context = list(),
    ...
  ) {
    tempest_otel_trace(
      "costorm.completion",
      chat_call(prompt, echo = echo, run_context = run_context, ...),
      owner = owner
    )
  }
  proxy$stream <- function(
    prompt = NULL,
    stream = c("text", "content"),
    controller = NULL,
    run_context = list(),
    ...
  ) {
    tempest_otel_trace(
      "costorm.completion",
      stream_call(
        prompt,
        stream = stream,
        controller = controller,
        run_context = run_context,
        ...
      ),
      owner = owner
    )
  }
  proxy$chat_async <- function(
    prompt,
    echo = "none",
    run_context = list(),
    ...
  ) {
    tempest_otel_trace_promise(
      "costorm.completion",
      chat_async_call(prompt, echo = echo, run_context = run_context, ...),
      owner = owner
    )
  }
  proxy$stream_async <- function(
    prompt = NULL,
    stream = c("text", "content"),
    controller = NULL,
    run_context = list(),
    ...
  ) {
    tempest_otel_trace_generator(
      "costorm.completion",
      stream_async_call(
        prompt,
        stream = stream,
        controller = controller,
        run_context = run_context,
        ...
      ),
      owner = owner
    )
  }
  proxy$clone <- function() proxy
  attr(proxy, ".tempest_otel_completion_owner") <- owner
  proxy
}

tempest_otel_result_fallback <- function(operation, result) {
  if (
    !identical(operation, "stage.execute") ||
      !inherits(result, "tempest_stage_result")
  ) {
    return(FALSE)
  }
  tryCatch(isTRUE(result$record@fallback_taken), error = function(...) FALSE)
}

tempest_otel_retrieval_project <- function(context, state) {
  if (!tempest_otel_retrieval_state_valid(state)) {
    return(invisible(context))
  }
  tempest_otel_context_set_attribute(
    context,
    "tempest.cache_hit",
    state$cache_hit
  )
  if (!is.null(state$result_count)) {
    tempest_otel_context_set_attribute(
      context,
      "tempest.result_count",
      state$result_count
    )
  }
  invisible(context)
}

tempest_otel_complete_retrieval <- function(context, state) {
  on.exit(tempest_otel_context_end(context), add = TRUE)
  tempest_otel_retrieval_project(context, state)
  if (
    tempest_otel_retrieval_state_valid(state) &&
      identical(state$outcome, "succeeded")
  ) {
    tempest_otel_mark_status(
      context,
      "succeeded",
      fallback_taken = state$fallback_taken
    )
  } else {
    tempest_otel_mark_status(
      context,
      "failed",
      error = simpleError("Retrieval has no successful terminal result.")
    )
  }
  tempest_otel_context_end(context)
}

tempest_otel_trace_retrieval <- function(operation, code, state) {
  if (
    !rlang::is_string(operation) ||
      !operation %in% c("retrieval.search", "retrieval.fetch") ||
      !tempest_otel_retrieval_state_valid(state)
  ) {
    return(force(code))
  }
  tempest_otel_trace(operation, code, retrieval_state = state)
}

tempest_otel_trace <- function(
  operation,
  code,
  stage = NULL,
  owner = NULL,
  retrieval_state = NULL
) {
  context <- tempest_otel_provider_call(
    tempest_otel_context_start(operation, stage = stage, owner = owner)
  )
  if (is.null(context)) {
    .tempest_otel_context <- tempest_otel_no_context_sentinel
    return(force(code))
  }

  .tempest_otel_context <- context
  on.exit(tempest_otel_context_end(context), add = TRUE)
  handle_error <- function(error) {
    on.exit(
      tryCatch(
        tempest_otel_context_end(context),
        error = function(...) NULL,
        interrupt = function(...) NULL
      ),
      add = TRUE
    )
    if (!is.null(retrieval_state)) {
      tempest_otel_fail_open(
        tempest_otel_retrieval_project(context, retrieval_state)
      )
    }
    tryCatch(
      tempest_otel_complete_error(context, error),
      error = function(...) NULL,
      interrupt = function(...) NULL
    )
    stop(error)
  }
  result <- tryCatch(
    {
      tempest_otel_context_activate(context)
      force(code)
    },
    error = handle_error,
    interrupt = handle_error
  )
  if (is.null(retrieval_state)) {
    tempest_otel_complete_success(
      context,
      fallback_taken = tempest_otel_result_fallback(operation, result)
    )
  } else {
    tempest_otel_complete_retrieval(context, retrieval_state)
  }
  result
}

tempest_otel_worker_call <- function(runner, args, enabled) {
  if (!is.function(runner) || !is.list(args) || !rlang::is_bool(enabled)) {
    tempest_abort(
      "The OpenTelemetry worker boundary received an invalid invocation.",
      class = c("tempest_async_error", "tempest_error")
    )
  }
  previous <- options(tempest.otel.enabled = enabled)
  on.exit(options(previous), add = TRUE)
  do.call(runner, args)
}
