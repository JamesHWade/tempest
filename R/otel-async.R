# Experimental OpenTelemetry async promise and generator tracing

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
