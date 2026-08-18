# Internal Deputy-backed Chat boundary

tempest_deputy_adapter_error <- function() {
  tempest_abort(
    "Deputy-backed chat execution failed.",
    class = c("tempest_deputy_adapter_error", "tempest_error")
  )
}

tempest_deputy_adapter_guard <- function(expr) {
  tryCatch(
    expr,
    error = function(error) {
      if (
        inherits(error, "tempest_deputy_adapter_error") ||
          inherits(error, "tempest_agent_completion_error")
      ) {
        stop(error)
      }
      tempest_deputy_adapter_error()
    }
  )
}

tempest_deputy_adapter_tool_names <- function(chat) {
  tools <- chat$get_tools()
  if (!is.list(tools)) {
    tempest_deputy_adapter_error()
  }
  supplied_names <- names(tools) %||% rep("", length(tools))
  tool_names <- vapply(
    seq_along(tools),
    function(index) {
      name <- supplied_names[[index]]
      if (!nzchar(name)) {
        name <- tryCatch(
          tools[[index]]@name,
          error = function(error) ""
        )
      }
      if (
        length(name) != 1L ||
          is.na(name) ||
          !nzchar(trimws(name))
      ) {
        tempest_deputy_adapter_error()
      }
      trimws(name)
    },
    character(1)
  )
  if (anyDuplicated(tool_names)) {
    tempest_deputy_adapter_error()
  }
  sort(tool_names, method = "radix")
}

tempest_deputy_adapter_agent_id <- function(run_context) {
  canonical <- tempest_research_manifest_canonical_json(run_context)
  paste0(
    "tempest-agent-",
    substr(
      digest::digest(canonical, algo = "sha256", serialize = FALSE),
      1L,
      32L
    )
  )
}

tempest_deputy_adapter_permissions <- function(tool_allowlist, max_requests) {
  deputy::Permissions$new(
    mode = "default",
    file_read = FALSE,
    file_write = FALSE,
    bash = FALSE,
    r_code = FALSE,
    web = FALSE,
    install_packages = FALSE,
    max_turns = max_requests,
    tool_allowlist = tool_allowlist,
    can_use_tool = function(tool_name, tool_input, context) {
      deputy::PermissionResultAllow()
    }
  )
}

tempest_deputy_adapter_permission_reference <- function(permissions) {
  list(
    bash = permissions$bash,
    file_read = permissions$file_read,
    file_write = permissions$file_write,
    install_packages = permissions$install_packages,
    mode = permissions$mode,
    r_code = permissions$r_code,
    tool_allowlist = permissions$tool_allowlist %||% character(),
    web = permissions$web
  )
}

tempest_deputy_adapter_status <- function(reason) {
  allowed <- c(
    "abandoned",
    "complete",
    "cost_limit",
    "error",
    "hook_requested_stop",
    "input_token_limit",
    "interrupted",
    "output_token_limit",
    "provider_error",
    "request_limit",
    "tool_call_limit",
    "total_token_limit"
  )
  if (
    !is.character(reason) ||
      length(reason) != 1L ||
      is.na(reason) ||
      !reason %in% allowed
  ) {
    return("error")
  }
  reason
}

tempest_deputy_adapter_trace <- function(
  reason,
  context,
  fallback_context,
  deputy_session_id,
  agent_id
) {
  run_context <- context$run_context %||% fallback_context
  trace <- list(
    trace_id = context$run_id,
    trace_type = "deputy_run",
    stage = run_context$stage,
    role = run_context$role,
    status = tempest_deputy_adapter_status(reason),
    deputy_run_id = context$run_id,
    deputy_session_id = context$session_id %||% deputy_session_id,
    agent_id = context$agent_id %||% agent_id
  )
  if (!is.null(run_context$expert_id)) {
    trace$expert_id <- run_context$expert_id
  }
  if (!is.null(run_context$correlation_id)) {
    trace$correlation_id <- run_context$correlation_id
  }
  tempest_research_manifest_canonical_value(trace, "terminal_deputy_trace")
}

tempest_deputy_adapter_pending_run <- function(
  context,
  fallback_context,
  deputy_session_id,
  agent_id,
  completion_id
) {
  run_context <- context$run_context %||% fallback_context
  pending <- list(
    agent_id = tempest_research_manifest_id(
      context$agent_id %||% agent_id,
      "pending_deputy_run$agent_id"
    ),
    completion_id = tempest_research_manifest_id(
      completion_id,
      "pending_deputy_run$completion_id"
    ),
    correlation_id = tempest_research_manifest_id(
      run_context$correlation_id,
      "pending_deputy_run$correlation_id"
    ),
    deputy_run_id = tempest_research_manifest_id(
      context$run_id,
      "pending_deputy_run$deputy_run_id"
    ),
    deputy_session_id = tempest_research_manifest_id(
      context$session_id %||% deputy_session_id,
      "pending_deputy_run$deputy_session_id"
    ),
    role = tempest_research_manifest_string(
      run_context$role,
      "pending_deputy_run$role"
    ),
    stage = tempest_research_manifest_string(
      run_context$stage,
      "pending_deputy_run$stage"
    )
  )
  if (!is.null(run_context$expert_id)) {
    pending$expert_id <- tempest_research_manifest_id(
      run_context$expert_id,
      "pending_deputy_run$expert_id"
    )
  }
  tempest_research_manifest_canonical_value(
    pending,
    "pending_deputy_run"
  )
}

tempest_deputy_adapter_run_context <- function(run_context) {
  if (is.null(run_context)) {
    run_context <- list()
  }
  if (
    !is.list(run_context) ||
      is.data.frame(run_context) ||
      (length(run_context) > 0L &&
        (is.null(names(run_context)) ||
          anyNA(names(run_context)) ||
          any(!nzchar(names(run_context))) ||
          anyDuplicated(names(run_context)) ||
          length(setdiff(
            names(run_context),
            c(
              "correlation_id",
              "role",
              "stage"
            )
          )) >
            0L))
  ) {
    tempest_deputy_adapter_error()
  }
  if (is.null(run_context$correlation_id)) {
    run_context$correlation_id <- tempest_uuid("deputy-correlation")
  } else {
    run_context$correlation_id <- tempest_research_manifest_id(
      run_context$correlation_id,
      "run_context$correlation_id"
    )
  }
  for (field in intersect(c("role", "stage"), names(run_context))) {
    run_context[[field]] <- tempest_research_manifest_string(
      run_context[[field]],
      paste0("run_context$", field)
    )
  }
  tempest_research_manifest_canonical_value(run_context, "run_context")
}

tempest_deputy_adapter_assert_terminal <- function(
  state,
  starts_before,
  terminals_before,
  completion_id,
  deputy_run_id = NULL
) {
  started <- identical(state$start_count, starts_before + 1L) &&
    !isTRUE(state$start_hook_failed) &&
    exists(completion_id, state$pending_runs, inherits = FALSE)
  terminal <- identical(state$terminal_count, terminals_before + 1L) &&
    !isTRUE(state$hook_failed) &&
    exists(completion_id, state$terminal_traces, inherits = FALSE)
  pending_run <- if (started) {
    get(completion_id, state$pending_runs, inherits = FALSE)
  } else {
    NULL
  }
  trace <- if (terminal) {
    get(completion_id, state$terminal_traces, inherits = FALSE)
  } else {
    NULL
  }
  if (
    started &&
      !is.null(deputy_run_id) &&
      !identical(pending_run$deputy_run_id, deputy_run_id)
  ) {
    started <- FALSE
  }
  if (
    terminal &&
      (!identical(trace$deputy_run_id, pending_run$deputy_run_id) ||
        (!is.null(deputy_run_id) &&
          !identical(trace$deputy_run_id, deputy_run_id)))
  ) {
    terminal <- FALSE
  }
  if (!started || !terminal) {
    tempest_deputy_adapter_error()
  }
  trace
}

tempest_deputy_adapter_content_text <- function(content) {
  if (is.character(content)) {
    return(paste(content, collapse = ""))
  }
  if (inherits(content, "ellmer::ContentText")) {
    return(content@text)
  }
  ""
}

tempest_deputy_adapter_turns <- function(value) {
  if (!is.list(value) || is.data.frame(value)) {
    tempest_agent_completion_binding_abort()
  }
  value
}

tempest_deputy_adapter_turn_boundary <- function(state) {
  turns <- tryCatch(
    state$agent$turns(),
    error = function(error) NULL
  )
  tempest_deputy_adapter_turns(turns)
}

tempest_deputy_adapter_provider_turn <- function(
  state,
  turns_before,
  turns_after,
  response
) {
  turns_before <- tempest_deputy_adapter_turns(turns_before)
  turns_after <- tempest_deputy_adapter_turns(turns_after)
  before_count <- length(turns_before)
  advanced <- length(turns_after) > before_count
  preserved <- before_count == 0L ||
    identical(
      turns_after[seq_len(before_count)],
      turns_before
    )
  if (!advanced || !preserved) {
    tempest_agent_completion_binding_abort()
  }
  provider_turn <- utils::tail(turns_after, 1L)[[1L]]
  provider_turn <- tempest_agent_completion_provider_turn(provider_turn)
  selected_turn <- tryCatch(
    state$agent$last_turn(role = "assistant"),
    error = function(error) NULL
  )
  if (!identical(selected_turn, provider_turn)) {
    tempest_agent_completion_binding_abort()
  }
  turn_response <- tryCatch(
    tempest_agent_completion_text(provider_turn@text),
    error = function(error) NULL
  )
  if (is.null(turn_response) || !identical(turn_response, response)) {
    tempest_agent_completion_binding_abort()
  }
  provider_turn
}

tempest_deputy_adapter_settle_trace <- function(
  state,
  completion_id,
  starts_before,
  terminals_before,
  disposition,
  deputy_run_id = NULL
) {
  trace <- tempest_deputy_adapter_assert_terminal(
    state,
    starts_before,
    terminals_before,
    completion_id,
    deputy_run_id = deputy_run_id
  )
  disposition <- tempest_research_manifest_choice(
    disposition,
    "completion_disposition",
    c("issued", "discarded", "terminal")
  )
  status_valid <- if (identical(disposition, "terminal")) {
    !identical(trace$status, "complete")
  } else {
    identical(trace$status, "complete")
  }
  if (!status_valid) {
    tempest_deputy_adapter_error()
  }
  trace$completion_disposition <- disposition
  trace <- trace[c(
    "agent_id",
    "completion_disposition",
    setdiff(names(trace), c("agent_id", "completion_disposition"))
  )]
  trace <- tempest_agent_completion_trace(trace)
  state$on_run(trace)
  trace
}

tempest_deputy_adapter_terminal_without_completion <- function(
  state,
  completion_id,
  starts_before,
  terminals_before,
  deputy_run_id = NULL
) {
  trace <- tempest_deputy_adapter_settle_trace(
    state,
    completion_id,
    starts_before,
    terminals_before,
    disposition = "terminal",
    deputy_run_id = deputy_run_id
  )
  state$on_terminal(list(
    completion_id = completion_id,
    deputy_execution = trace,
    disposition = "terminal"
  ))
  rm(list = completion_id, envir = state$pending_runs)
  rm(list = completion_id, envir = state$terminal_traces)
  tempest_deputy_adapter_error()
}

tempest_deputy_adapter_discard_completion <- function(
  state,
  completion_id,
  starts_before,
  terminals_before,
  deputy_run_id = NULL
) {
  trace <- tempest_deputy_adapter_settle_trace(
    state,
    completion_id,
    starts_before,
    terminals_before,
    disposition = "discarded",
    deputy_run_id = deputy_run_id
  )
  state$on_terminal(list(
    completion_id = completion_id,
    deputy_execution = trace,
    disposition = "discarded"
  ))
  rm(list = completion_id, envir = state$pending_runs)
  rm(list = completion_id, envir = state$terminal_traces)
  tempest_agent_completion_binding_abort()
}

tempest_deputy_adapter_complete <- function(
  state,
  completion_id,
  prompt,
  response,
  starts_before,
  terminals_before,
  turns_before,
  turns_after,
  deputy_run_id = NULL
) {
  trace <- tempest_deputy_adapter_assert_terminal(
    state,
    starts_before,
    terminals_before,
    completion_id,
    deputy_run_id = deputy_run_id
  )
  if (!identical(trace$status, "complete")) {
    return(tempest_deputy_adapter_terminal_without_completion(
      state,
      completion_id,
      starts_before,
      terminals_before,
      deputy_run_id = deputy_run_id
    ))
  }
  prompt <- tempest_agent_completion_text(prompt)
  response <- tempest_agent_completion_text(response)
  provider_turn <- tryCatch(
    tempest_deputy_adapter_provider_turn(
      state,
      turns_before,
      turns_after,
      response
    ),
    error = function(error) {
      tempest_deputy_adapter_discard_completion(
        state,
        completion_id,
        starts_before,
        terminals_before,
        deputy_run_id = deputy_run_id
      )
    }
  )
  trace <- tempest_deputy_adapter_settle_trace(
    state,
    completion_id,
    starts_before,
    terminals_before,
    disposition = "issued",
    deputy_run_id = deputy_run_id
  )
  completion <- list(
    completion_id = completion_id,
    prompt = prompt,
    response = response,
    provider_turn = provider_turn,
    deputy_execution = trace
  )
  state$on_completion(completion)
  state$completion_count <- state$completion_count + 1L
  rm(list = completion_id, envir = state$pending_runs)
  rm(list = completion_id, envir = state$terminal_traces)
  tempest_agent_completion_tag(response, completion_id)
}

tempest_deputy_adapter_async_stream <- function(
  state,
  prompt,
  run_context
) {
  prompt <- tempest_agent_completion_text(prompt)
  completion_id <- tempest_agent_completion_new_id(
    state$completion_registry
  )
  run_context$completion_id <- completion_id
  source <- tempest_deputy_adapter_guard(state$agent$run_shiny(
    prompt = prompt,
    max_tool_calls = state$max_tool_calls,
    run_context = run_context
  ))
  if (!inherits(source, "coro_generator_instance")) {
    tempest_deputy_adapter_error()
  }
  stream <- coro::async_generator(function() {
    starts_before <- state$start_count
    terminals_before <- state$terminal_count
    turns_before <- tempest_deputy_adapter_turn_boundary(state)
    state$start_hook_failed <- FALSE
    state$hook_failed <- FALSE
    text <- character()
    repeat {
      stream_error <- FALSE
      content <- tryCatch(
        {
          value <- source()
          if (promises::is.promising(value)) {
            value <- coro::await(value)
          }
          value
        },
        error = function(error) {
          stream_error <<- TRUE
          NULL
        }
      )
      if (isTRUE(stream_error)) {
        tempest_deputy_adapter_guard(tempest_deputy_adapter_terminal_without_completion(
          state,
          completion_id,
          starts_before,
          terminals_before
        ))
      }
      if (coro::is_exhausted(content)) {
        break
      }
      text <- c(text, tempest_deputy_adapter_content_text(content))
      coro::yield(content)
    }
    tempest_deputy_adapter_guard(tempest_deputy_adapter_complete(
      state,
      completion_id,
      prompt,
      paste(text, collapse = ""),
      starts_before,
      terminals_before,
      turns_before,
      tempest_deputy_adapter_turn_boundary(state)
    ))
    coro::exhausted()
  })()
  tempest_agent_completion_tag(stream, completion_id)
}

tempest_deputy_chat_adapter <- function(
  chat,
  manifest,
  deputy_session_id,
  agent_id = NULL,
  agent_name = NULL,
  stage,
  role,
  expert_id = NULL,
  completion_registry = NULL,
  on_start = function(pending_run) invisible(pending_run),
  on_run = function(trace) invisible(trace),
  on_completion = function(completion) invisible(completion),
  on_terminal = function(terminal) invisible(terminal),
  max_requests = 25L,
  max_tool_calls = 25L
) {
  tempest_deputy_adapter_guard({
    if (
      !inherits(chat, "Chat") ||
        !is.function(on_start) ||
        !is.function(on_run) ||
        !is.function(on_completion) ||
        !is.function(on_terminal)
    ) {
      tempest_deputy_adapter_error()
    }
    if (is.null(completion_registry)) {
      completion_registry <- tempest_agent_completion_registry(
        new.env(parent = emptyenv())
      )
    } else {
      completion_registry <- tempest_agent_completion_registry_validate(
        completion_registry
      )
    }
    required_methods <- c(
      "get_model",
      "get_provider",
      "get_system_prompt",
      "get_tokens",
      "get_tools",
      "get_turns",
      "last_turn",
      "on_tool_request",
      "on_tool_result",
      "set_system_prompt",
      "set_turns",
      "stream",
      "stream_async"
    )
    if (any(!vapply(chat[required_methods], is.function, logical(1)))) {
      tempest_deputy_adapter_error()
    }

    base_run_context <- tempest_deputy_run_context(
      manifest,
      stage = stage,
      role = role,
      expert_id = expert_id
    )
    tool_allowlist <- tempest_deputy_adapter_tool_names(chat)
    agent_id <- agent_id %||%
      tempest_deputy_adapter_agent_id(base_run_context)
    permissions <- tempest_deputy_adapter_permissions(
      tool_allowlist,
      max_requests
    )
    usage_limits <- deputy::UsageLimits(
      max_requests = max_requests,
      max_tool_calls = max_tool_calls,
      on_exceed = "stop"
    )
    agent <- deputy::Agent$new(
      chat = chat,
      tools = list(),
      permissions = permissions,
      usage_limits = usage_limits,
      run_context = base_run_context,
      agent_id = agent_id,
      agent_name = agent_name
    )
    agent$configure_sdk_compat(list(
      persist_session = FALSE,
      session_id = deputy_session_id
    ))

    state <- new.env(parent = emptyenv())
    state$agent <- agent
    state$base_run_context <- base_run_context
    state$deputy_session_id <- deputy_session_id
    state$agent_id <- agent$agent_id
    state$agent_name <- agent$agent_name
    state$max_tool_calls <- max_tool_calls
    state$completion_registry <- completion_registry
    state$on_run <- on_run
    state$on_completion <- on_completion
    state$on_terminal <- on_terminal
    state$start_count <- 0L
    state$terminal_count <- 0L
    state$completion_count <- 0L
    state$start_hook_failed <- FALSE
    state$hook_failed <- FALSE
    state$pending_runs <- new.env(hash = TRUE, parent = emptyenv())
    state$terminal_traces <- new.env(hash = TRUE, parent = emptyenv())
    state$identity <- tempest_research_manifest_canonical_value(list(
      agent_id = agent$agent_id,
      agent_name = agent$agent_name,
      deputy_session_id = deputy_session_id,
      run_context = base_run_context,
      tool_allowlist = tool_allowlist
    ))
    state$permission_reference <-
      tempest_deputy_adapter_permission_reference(permissions)

    start_hook <- deputy::HookMatcher$new(
      event = "SessionStart",
      timeout = 0,
      callback = function(context) {
        started <- tryCatch(
          {
            completion_id <- context$run_context$completion_id %||% NULL
            pending_run <- tempest_deputy_adapter_pending_run(
              context,
              fallback_context = state$base_run_context,
              deputy_session_id = state$deputy_session_id,
              agent_id = state$agent_id,
              completion_id = completion_id
            )
            on_start(pending_run)
            assign(completion_id, pending_run, state$pending_runs)
            state$start_count <- state$start_count + 1L
            TRUE
          },
          error = function(error) FALSE
        )
        state$start_hook_failed <- !started
        deputy::HookResultSessionStart()
      }
    )
    terminal_hook <- deputy::HookMatcher$new(
      event = "SessionEnd",
      timeout = 0,
      callback = function(reason, context) {
        completed <- tryCatch(
          {
            completion_id <- context$run_context$completion_id %||% NULL
            trace <- tempest_deputy_adapter_trace(
              reason,
              context,
              fallback_context = state$base_run_context,
              deputy_session_id = state$deputy_session_id,
              agent_id = state$agent_id
            )
            assign(completion_id, trace, state$terminal_traces)
            state$terminal_count <- state$terminal_count + 1L
            TRUE
          },
          error = function(error) FALSE
        )
        state$hook_failed <- !completed
        deputy::HookResultSessionEnd()
      }
    )
    agent$add_hook(start_hook)
    agent$add_hook(terminal_hook)

    adapter <- NULL
    adapter <- structure(
      list(
        chat = function(
          prompt,
          echo = "none",
          run_context = list(),
          ...
        ) {
          tempest_deputy_adapter_guard({
            prompt <- tempest_agent_completion_text(prompt)
            run_context <- tempest_deputy_adapter_run_context(run_context)
            completion_id <- tempest_agent_completion_new_id(
              state$completion_registry
            )
            run_context$completion_id <- completion_id
            starts_before <- state$start_count
            terminals_before <- state$terminal_count
            turns_before <- tempest_deputy_adapter_turn_boundary(state)
            state$start_hook_failed <- FALSE
            state$hook_failed <- FALSE
            result <- tryCatch(
              suppressWarnings(state$agent$run_sync(
                task = prompt,
                include_partial_messages = FALSE,
                run_context = run_context
              )),
              error = function(error) {
                tempest_deputy_adapter_terminal_without_completion(
                  state,
                  completion_id,
                  starts_before,
                  terminals_before
                )
              }
            )
            response <- result$response %||% ""
            tempest_deputy_adapter_complete(
              state,
              completion_id,
              prompt,
              response,
              starts_before,
              terminals_before,
              turns_before,
              result$turns,
              deputy_run_id = result$run_id
            )
          })
        },
        chat_async = function(
          prompt,
          echo = "none",
          run_context = list(),
          ...
        ) {
          promises::promise_resolve(NULL) |>
            promises::then(onFulfilled = function(value) {
              stream <- adapter$stream_async(
                prompt,
                run_context = run_context
              )
              coro::async(function() {
                completion_id <- tempest_agent_completion_id(stream)
                text <- character()
                repeat {
                  content <- stream()
                  if (promises::is.promising(content)) {
                    content <- coro::await(content)
                  }
                  if (coro::is_exhausted(content)) {
                    break
                  }
                  text <- c(
                    text,
                    tempest_deputy_adapter_content_text(content)
                  )
                }
                tempest_agent_completion_tag(
                  paste(text, collapse = ""),
                  completion_id
                )
              })()
            })
        },
        stream = function(
          prompt = NULL,
          stream = c("text", "content"),
          controller = NULL,
          run_context = list(),
          ...
        ) {
          response <- adapter$chat(prompt, run_context = run_context)
          completion_id <- tempest_agent_completion_id(response)
          output <- coro::generator(function() {
            coro::yield(ellmer::ContentText(as.character(response)))
          })()
          tempest_agent_completion_tag(output, completion_id)
        },
        stream_async = function(
          prompt = NULL,
          stream = c("text", "content"),
          controller = NULL,
          run_context = list(),
          ...
        ) {
          tempest_deputy_adapter_guard({
            run_context <- tempest_deputy_adapter_run_context(run_context)
            tempest_deputy_adapter_async_stream(
              state,
              prompt,
              run_context
            )
          })
        },
        chat_structured = function(...) {
          tempest_deputy_adapter_error()
        },
        chat_structured_async = function(...) {
          tempest_deputy_adapter_error()
        },
        get_turns = function() state$agent$turns(),
        set_turns = function(turns) state$agent$chat$set_turns(turns),
        last_turn = function(role = "assistant") {
          state$agent$last_turn(role = role)
        },
        get_provider = function() state$agent$provider(),
        get_model = function() state$agent$provider()$model,
        get_tools = function() state$agent$chat$get_tools(),
        register_tool = function(...) tempest_deputy_adapter_error(),
        register_tools = function(...) tempest_deputy_adapter_error(),
        get_system_prompt = function() {
          state$agent$chat$get_system_prompt()
        },
        set_system_prompt = function(prompt) {
          state$agent$chat$set_system_prompt(prompt)
        },
        get_tokens = function() state$agent$chat$get_tokens(),
        on_tool_request = function(...) tempest_deputy_adapter_error(),
        on_tool_result = function(...) tempest_deputy_adapter_error(),
        clone = function() adapter,
        cancel = function(reason = "interrupted") {
          state$agent$interrupt(reason = "interrupted")
        },
        stop = function(reason = "interrupted") {
          state$agent$interrupt(reason = "interrupted")
        },
        .tempest_deputy_identity = function() state$identity,
        .tempest_deputy_permissions = function() state$permission_reference
      ),
      class = c("TempestDeputyChatAdapter", "Chat", "list")
    )
    adapter
  })
}

tempest_deputy_chat_identity <- function(x) {
  if (
    !inherits(x, "TempestDeputyChatAdapter") ||
      !inherits(x, "Chat") ||
      !is.function(x$.tempest_deputy_identity)
  ) {
    tempest_deputy_adapter_error()
  }
  tempest_research_manifest_canonical_value(
    x$.tempest_deputy_identity(),
    "deputy_chat_identity"
  )
}

tempest_deputy_chat_permissions <- function(x) {
  if (
    !inherits(x, "TempestDeputyChatAdapter") ||
      !is.function(x$.tempest_deputy_permissions)
  ) {
    tempest_deputy_adapter_error()
  }
  x$.tempest_deputy_permissions()
}

tempest_deputy_chat_last_execution <- function(x) {
  tempest_deputy_adapter_error()
}
