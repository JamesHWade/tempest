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

tempest_otel_context_start <- function(operation, stage = NULL) {
  if (
    !rlang::is_string(operation) ||
      !operation %in% names(tempest_otel_span_names()) ||
      !tempest_otel_worker_intent()
  ) {
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

tempest_otel_context_end <- function(context) {
  if (!tempest_otel_context_live(context)) {
    return(invisible(context))
  }
  deactivate_span <- function() {
    if (
      is.null(context$activation_scope) ||
        identical(context$deactivation_attempted, TRUE)
    ) {
      return(NULL)
    }
    context$deactivation_attempted <- TRUE
    tryCatch(
      {
        tempest_otel_provider_call(
          context$span$deactivate(context$activation_scope)
        )
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
    if (!exists(".tempest_otel_context", envir = frame, inherits = FALSE)) {
      next
    }
    context <- get(
      ".tempest_otel_context",
      envir = frame,
      inherits = FALSE
    )
    if (tempest_otel_context_live(context)) {
      return(context)
    }
  }
  NULL
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

tempest_otel_complete_success <- function(context, fallback_taken = FALSE) {
  tempest_otel_context_set_attribute(context, "tempest.status", "succeeded")
  if (identical(fallback_taken, TRUE)) {
    tempest_otel_context_set_attribute(
      context,
      "tempest.fallback_taken",
      TRUE
    )
  }
  tempest_otel_context_set_status(
    context,
    tempest_otel_status_code("succeeded")
  )
  tempest_otel_context_end(context)
}

tempest_otel_complete_error <- function(context, error) {
  if (tempest_otel_cancelled(error)) {
    tempest_otel_context_set_attribute(
      context,
      "tempest.status",
      "cancelled"
    )
    tempest_otel_context_set_attribute(context, "tempest.cancelled", TRUE)
    tempest_otel_context_set_status(
      context,
      tempest_otel_status_code("cancelled")
    )
  } else {
    tempest_otel_context_set_attribute(context, "tempest.status", "failed")
    tempest_otel_context_set_attribute(
      context,
      "tempest.error_class",
      tempest_otel_safe_error_class(error)
    )
    tempest_otel_context_set_status(
      context,
      tempest_otel_status_code("failed")
    )
  }
  tempest_otel_context_end(context)
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

tempest_otel_trace <- function(operation, code, stage = NULL) {
  context <- tempest_otel_provider_call(
    tempest_otel_context_start(operation, stage = stage)
  )
  if (is.null(context)) {
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
  tempest_otel_complete_success(
    context,
    fallback_taken = tempest_otel_result_fallback(operation, result)
  )
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
