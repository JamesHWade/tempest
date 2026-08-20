local_otel_opt_in <- function(.local_envir = parent.frame()) {
  withr::local_options(
    tempest.otel.enabled = TRUE,
    .local_envir = .local_envir
  )
  withr::local_envvar(
    c(
      OTEL_R_EMIT_SCOPES = "io.github.jameshwade.tempest",
      OTEL_INSTRUMENTATION_GENAI_CAPTURE_MESSAGE_CONTENT = "false"
    ),
    .local_envir = .local_envir
  )
}

local_fake_otel <- function(
  enabled = TRUE,
  installed = TRUE,
  provider_errors = character(),
  provider_conditions = character(),
  provider_interrupts = character(),
  .local_envir = parent.frame()
) {
  state <- new.env(parent = emptyenv())
  state$tracer_calls <- 0L
  state$enabled_calls <- 0L
  state$start_calls <- 0L
  state$spans <- list()
  state$order <- character()
  state$interrupt <- structure(
    list(message = "provider interrupt", call = NULL),
    class = c("interrupt", "condition")
  )
  tracer <- new.env(parent = emptyenv())

  provider_effect <- function(operation) {
    if (operation %in% provider_conditions) {
      message("provider message")
      warning("provider warning")
    }
    if (operation %in% provider_interrupts) {
      signalCondition(state$interrupt)
    }
    if (operation %in% provider_errors) {
      stop("provider error")
    }
    invisible(NULL)
  }

  new_span <- function(name, attributes) {
    span <- new.env(parent = emptyenv())
    span$name <- name
    span$attributes <- attributes
    span$events <- list()
    span$statuses <- character()
    span$activate_count <- 0L
    span$deactivate_count <- 0L
    span$end_count <- 0L
    span$activation_scope <- new.env(parent = emptyenv())
    span$activate <- function(activation_scope = NULL, end_on_exit = FALSE) {
      if (!is.null(activation_scope)) {
        stop("activation scope must be requested from the provider")
      }
      span$activate_count <- span$activate_count + 1L
      provider_effect("activate")
      span$activation_scope
    }
    span$deactivate <- function(activation_scope) {
      if (!identical(activation_scope, span$activation_scope)) {
        stop("unknown activation scope")
      }
      provider_effect("deactivate")
      span$deactivate_count <- span$deactivate_count + 1L
      invisible(span)
    }
    span$set_attribute <- function(name, value) {
      provider_effect("set_attribute")
      span$attributes[[name]] <- value
      invisible(span)
    }
    span$add_event <- function(name, attributes = NULL) {
      provider_effect("add_event")
      span$events[[length(span$events) + 1L]] <- list(
        name = name,
        attributes = attributes
      )
      state$order <- c(state$order, "telemetry")
      invisible(span)
    }
    span$set_status <- function(status_code, description = NULL) {
      provider_effect("set_status")
      span$statuses <- c(span$statuses, status_code)
      invisible(span)
    }
    span$end <- function(options = NULL, status_code = NULL) {
      provider_effect("end")
      span$end_count <- span$end_count + 1L
      invisible(span)
    }
    span
  }

  testthat::local_mocked_bindings(
    tempest_otel_has = function() {
      provider_effect("has")
      installed
    },
    tempest_otel_get_tracer = function() {
      provider_effect("get_tracer")
      state$tracer_calls <- state$tracer_calls + 1L
      tracer
    },
    tempest_otel_is_tracing_enabled = function(value) {
      provider_effect("is_enabled")
      state$enabled_calls <- state$enabled_calls + 1L
      identical(value, tracer) && enabled
    },
    tempest_otel_start_span = function(value, name, attributes) {
      provider_effect("start_span")
      state$start_calls <- state$start_calls + 1L
      if (!identical(value, tracer)) {
        stop("unknown tracer")
      }
      span <- new_span(name, attributes)
      state$spans[[length(state$spans) + 1L]] <- span
      span
    },
    .env = .local_envir
  )
  state
}
