test_that("Deputy adapter freezes the current Chat tool permissions", {
  skip_if_not_installed("deputy")

  chat <- fake_chat(text = list("ready"))
  chat$register_tools(list(
    ellmer::tool(
      function(claim_id) claim_id,
      "Inspect one evidence claim.",
      arguments = list(claim_id = ellmer::type_string()),
      name = "inspect_evidence"
    ),
    ellmer::tool(
      function(query) query,
      "Search the in-memory workspace.",
      arguments = list(query = ellmer::type_string()),
      name = "search_workspace"
    )
  ))
  manifest <- tempest_research_manifest(
    "deputy-adapter-permissions",
    mode = "costorm",
    config = tempest_config()
  )

  adapter <- tempest:::tempest_deputy_chat_adapter(
    chat,
    manifest,
    deputy_session_id = "deputy-session-permissions",
    agent_id = "tempest-agent-moderator",
    agent_name = "Co-STORM moderator",
    stage = "dialogue",
    role = "moderator"
  )

  expect_s3_class(adapter, "TempestDeputyChatAdapter")
  expect_s3_class(adapter, "Agent")
  expect_s3_class(adapter, "Chat")
  expect_identical(
    tempest:::tempest_deputy_chat_permissions(adapter),
    list(
      bash = FALSE,
      file_read = FALSE,
      file_write = FALSE,
      install_packages = FALSE,
      mode = "standard",
      r_code = FALSE,
      tool_allowlist = c("inspect_evidence", "search_workspace"),
      web = TRUE
    )
  )
  expect_identical(
    names(adapter$get_tools()),
    c("inspect_evidence", "search_workspace")
  )
  expect_identical(adapter$register_tools(list()), adapter)
})

test_that("Deputy adapter is a real Agent for R6 ellmer clients", {
  skip_if_not_installed("deputy")

  chat <- suppressWarnings(ellmer::chat_openai(
    model = "gpt-4o-mini",
    api_key = "test-only-not-a-credential"
  ))
  chat$register_tool(ellmer::openai_tool_web_search())
  manifest <- tempest_research_manifest(
    "deputy-adapter-r6-chat",
    mode = "costorm",
    config = tempest_config()
  )

  adapter <- tempest:::tempest_deputy_chat_adapter(
    chat,
    manifest,
    deputy_session_id = "deputy-session-r6-chat",
    stage = "dialogue",
    role = "moderator"
  )
  proxy <- tempest:::tempest_deputy_chat_proxy(adapter)

  expect_s3_class(adapter, "TempestDeputyChatAdapter")
  expect_s3_class(adapter, "Agent")
  expect_s3_class(adapter, "Chat")
  expect_identical(is.environment(adapter), TRUE)
  expect_identical(adapter$get_model(), "gpt-4o-mini")
  expect_identical(names(adapter$get_tools()), "web_search")
  expect_identical(is.list(proxy), TRUE)
  expect_identical(proxy$get_model(), "gpt-4o-mini")
  expect_null(proxy$.__enclos_env__)
  expect_identical(
    tempest:::tempest_deputy_adapter_prompt(list(list(
      ellmer::ContentText("Review the attached evidence."),
      ellmer::ContentText("Include uncertainties.")
    ))),
    "Review the attached evidence.\n\nInclude uncertainties."
  )
})

test_that("provider reasoning stays outside the canonical response", {
  turn <- ellmer::AssistantTurn(list(
    ellmer::ContentThinking("private reasoning"),
    ellmer::ContentText("Visible answer.")
  ))

  expect_identical(
    tempest:::tempest_agent_completion_response_from_turn(turn),
    "Visible answer."
  )
  expect_identical(
    tempest:::tempest_agent_completion_response_matches_turn(
      "Visible answer.\n",
      turn
    ),
    TRUE
  )
  expect_identical(
    tempest:::tempest_agent_completion_response_matches_turn(
      "Different answer.",
      turn
    ),
    FALSE
  )
  expect_identical(
    tempest:::tempest_storm_completion_answer(list(
      provider_turn = turn,
      response = "Visible answer."
    )),
    list(answer_text = "Visible answer.", provider_turn = turn)
  )
})

test_that("Deputy terminal reasons retain current safety semantics", {
  expect_identical(
    tempest:::tempest_deputy_adapter_status("cost_unavailable"),
    "cost_unavailable"
  )
  expect_identical(
    tempest:::tempest_deputy_adapter_status("tool_loop"),
    "tool_loop"
  )
})

test_that("allowlisted tools pass with ambient execution disabled", {
  skip_if_not_installed("deputy")

  tool <- ellmer::tool(
    function(query) paste("workspace", query),
    "Search the current Tempest workspace.",
    arguments = list(query = ellmer::type_string()),
    name = "search_workspace",
    annotations = list(open_world_hint = TRUE)
  )
  permissions <- tempest:::tempest_deputy_adapter_permissions(
    "search_workspace"
  )

  allowed <- permissions$check(
    tool@name,
    list(query = "claim"),
    context = list(tool_annotations = tool@annotations)
  )
  denied <- permissions$check(
    "unlisted_workspace_tool",
    list(query = "claim"),
    context = list(tool_annotations = tool@annotations)
  )

  expect_s3_class(allowed, "PermissionResultAllow")
  expect_identical(tool(query = "claim"), "workspace claim")
  expect_s3_class(denied, "PermissionResultDeny")
  expect_match(denied$reason, "not in allowlist", fixed = TRUE)
  expect_identical(permissions$file_read, FALSE)
  expect_identical(permissions$file_write, FALSE)
  expect_identical(permissions$bash, FALSE)
  expect_identical(permissions$r_code, FALSE)
  expect_identical(permissions$web, TRUE)
  expect_identical(permissions$install_packages, FALSE)
})

test_that("sync runs reuse one Deputy session and record exact traces", {
  skip_if_not_installed("deputy")

  chat <- fake_chat(text = list("First answer.", "Second answer."))
  manifest <- tempest_research_manifest(
    "deputy-adapter-sync",
    mode = "costorm",
    config = tempest_config()
  )
  starts <- list()
  traces <- list()
  adapter <- tempest:::tempest_deputy_chat_adapter(
    chat,
    manifest,
    deputy_session_id = "deputy-session-sync",
    agent_id = "tempest-agent-sync",
    stage = "dialogue",
    role = "moderator",
    on_start = function(pending_run) {
      starts[[length(starts) + 1L]] <<- pending_run
      invisible(pending_run)
    },
    on_run = function(trace) {
      traces[[length(traces) + 1L]] <<- trace
      invisible(trace)
    }
  )

  first <- adapter$chat(
    "First question",
    run_context = list(
      stage = "dialogue",
      role = "moderator",
      correlation_id = "dialogue-turn-1"
    )
  )
  second <- adapter$chat("Second question")

  expect_identical(as.character(first), "First answer.")
  expect_identical(as.character(second), "Second answer.")
  expect_length(starts, 2L)
  expect_length(traces, 2L)
  expect_identical(
    names(starts[[1L]]),
    c(
      "agent_id",
      "completion_id",
      "correlation_id",
      "deputy_run_id",
      "deputy_session_id",
      "role",
      "stage"
    )
  )
  expect_identical(
    vapply(starts, `[[`, character(1), "deputy_run_id"),
    vapply(traces, `[[`, character(1), "deputy_run_id")
  )
  expect_identical(
    vapply(starts, `[[`, character(1), "correlation_id"),
    vapply(traces, `[[`, character(1), "correlation_id")
  )
  expect_identical(
    names(traces[[1L]]),
    c(
      "agent_id",
      "completion_disposition",
      "correlation_id",
      "deputy_run_id",
      "deputy_session_id",
      "role",
      "stage",
      "status",
      "trace_id",
      "trace_type"
    )
  )
  expect_identical(traces[[1L]]$trace_id, traces[[1L]]$deputy_run_id)
  expect_identical(traces[[1L]]$trace_type, "deputy_run")
  expect_identical(traces[[1L]]$status, "complete")
  expect_identical(traces[[1L]]$completion_disposition, "issued")
  expect_identical(traces[[1L]]$correlation_id, "dialogue-turn-1")
  expect_identical(
    tempest:::tempest_opaque_identifier_valid(traces[[2L]]$correlation_id),
    TRUE
  )
  expect_identical(
    traces[[2L]]$correlation_id == traces[[1L]]$correlation_id,
    FALSE
  )
  expect_identical(
    vapply(traces, `[[`, character(1), "deputy_session_id"),
    rep("deputy-session-sync", 2L)
  )
  expect_identical(
    vapply(starts, `[[`, character(1), "deputy_session_id"),
    rep("deputy-session-sync", 2L)
  )
  expect_identical(
    vapply(traces, `[[`, character(1), "agent_id"),
    rep("tempest-agent-sync", 2L)
  )
  expect_identical(
    length(unique(vapply(traces, `[[`, character(1), "deputy_run_id"))),
    2L
  )
  expect_null(adapter$last_execution)
  expect_identical(
    tempest:::tempest_agent_completion_id(first) ==
      tempest:::tempest_agent_completion_id(second),
    FALSE
  )
  expect_identical(
    vapply(chat$.calls(), `[[`, character(1), "transport"),
    rep("stream_async", 2L)
  )
  expect_identical(
    ellmer::contents_markdown(adapter$last_turn()),
    "Second answer."
  )
})

test_that("async chat and streams execute through Deputy Chat", {
  skip_if_not_installed("deputy")
  skip_if_not_installed("coro")
  skip_if_not_installed("promises")
  skip_if_not_installed("later")

  chat <- fake_chat(text = list(c("Warmup ", "answer."), "Dialogue answer."))
  manifest <- tempest_research_manifest(
    "deputy-adapter-async",
    mode = "costorm",
    config = tempest_config()
  )
  traces <- list()
  adapter <- tempest:::tempest_deputy_chat_adapter(
    chat,
    manifest,
    deputy_session_id = "deputy-session-expert",
    agent_id = "tempest-agent-expert",
    stage = "warmup",
    role = "expert",
    expert_id = "expert-climate",
    on_run = function(trace) {
      traces[[length(traces) + 1L]] <<- trace
      invisible(trace)
    }
  )

  chat_result <- await_tempest_promise(adapter$chat_async(
    "Prepare an orientation",
    run_context = list(
      stage = "warmup",
      role = "expert"
    )
  ))
  stream <- adapter$stream_async(
    "Answer the moderator",
    run_context = list(
      stage = "dialogue",
      role = "expert"
    )
  )
  stream_result <- await_tempest_promise(coro::async(function() {
    text <- character()
    repeat {
      content <- stream()
      if (promises::is.promising(content)) {
        content <- coro::await(content)
      }
      if (coro::is_exhausted(content)) {
        break
      }
      if (inherits(content, "ellmer::ContentText")) {
        text <- c(text, content@text)
      }
    }
    paste(text, collapse = "")
  })())

  expect_null(chat_result$error)
  expect_identical(as.character(chat_result$value), "Warmup answer.")
  expect_null(stream_result$error)
  expect_identical(stream_result$value, "Dialogue answer.")
  expect_length(traces, 2L)
  expect_identical(
    vapply(traces, `[[`, character(1), "stage"),
    c("warmup", "dialogue")
  )
  correlations <- vapply(traces, `[[`, character(1), "correlation_id")
  expect_all_true(vapply(
    correlations,
    tempest:::tempest_opaque_identifier_valid,
    logical(1)
  ))
  expect_identical(
    length(unique(correlations)),
    2L
  )
  expect_identical(
    vapply(traces, `[[`, character(1), "expert_id"),
    rep("expert-climate", 2L)
  )
  expect_identical(
    vapply(chat$.calls(), `[[`, character(1), "transport"),
    rep("stream_async", 2L)
  )
})

test_that("provider and recording errors fail with a fixed safe condition", {
  skip_if_not_installed("deputy")
  skip_if_not_installed("coro")
  skip_if_not_installed("promises")
  skip_if_not_installed("later")

  secret <- "sk-proj-SUPERSECRET0123456789"
  provider_error <- function(prompt) stop(secret)
  manifest <- tempest_research_manifest(
    "deputy-adapter-errors",
    mode = "costorm",
    config = tempest_config()
  )
  sync_adapter <- tempest:::tempest_deputy_chat_adapter(
    fake_chat(text = list(provider_error, provider_error)),
    manifest,
    deputy_session_id = "deputy-session-sync-error",
    stage = "dialogue",
    role = "moderator"
  )

  sync_error <- tryCatch(
    sync_adapter$chat("Fail safely"),
    error = identity
  )
  expect_s3_class(sync_error, "tempest_deputy_adapter_error")
  expect_identical(
    conditionMessage(sync_error),
    "Deputy-backed chat execution failed."
  )
  expect_null(sync_error$parent)
  expect_no_match(conditionMessage(sync_error), secret, fixed = TRUE)

  async_adapter <- tempest:::tempest_deputy_chat_adapter(
    fake_chat(text = list(provider_error)),
    manifest,
    deputy_session_id = "deputy-session-async-error",
    stage = "dialogue",
    role = "moderator"
  )
  async_result <- await_tempest_promise(
    async_adapter$chat_async("Fail safely")
  )
  expect_s3_class(async_result$error, "tempest_deputy_adapter_error")
  expect_identical(
    conditionMessage(async_result$error),
    "Deputy-backed chat execution failed."
  )
  expect_null(async_result$error$parent)
  expect_no_match(conditionMessage(async_result$error), secret, fixed = TRUE)

  recording_adapter <- tempest:::tempest_deputy_chat_adapter(
    fake_chat(text = list("Unrecorded sync", "Unrecorded async")),
    manifest,
    deputy_session_id = "deputy-session-recording-error",
    stage = "dialogue",
    role = "moderator",
    on_run = function(trace) stop(secret)
  )
  recording_error <- tryCatch(
    recording_adapter$chat("Complete but fail recording"),
    error = identity
  )
  expect_s3_class(recording_error, "tempest_deputy_adapter_error")
  expect_null(recording_adapter$last_execution)
  async_recording <- await_tempest_promise(
    recording_adapter$chat_async("Complete but fail recording")
  )
  expect_s3_class(async_recording$error, "tempest_deputy_adapter_error")
  expect_null(recording_adapter$last_execution)
})

test_that("terminal stream errors settle without issuing completions", {
  skip_if_not_installed("deputy")
  skip_if_not_installed("coro")
  skip_if_not_installed("ellmer")
  skip_if_not_installed("later")
  skip_if_not_installed("promises")

  terminal_chat <- function(async = FALSE) {
    chat <- fake_chat()
    chat$stream <- function(...) {
      coro::generator(function() {
        coro::yield(ellmer::ContentText("Partial response"))
        stop("private provider failure")
      })()
    }
    chat$stream_async <- function(...) {
      coro::async_generator(function() {
        coro::yield(ellmer::ContentText("Partial response"))
        stop("private provider failure")
      })()
    }
    chat$clone <- function() chat
    chat
  }
  manifest <- tempest_research_manifest(
    "deputy-adapter-terminal-errors",
    mode = "costorm",
    config = tempest_config()
  )
  starts <- list()
  terminals <- list()
  completions <- list()
  callbacks <- list(
    on_start = function(pending_run) {
      starts[[length(starts) + 1L]] <<- pending_run
    },
    on_terminal = function(terminal) {
      terminals[[length(terminals) + 1L]] <<- terminal
    },
    on_completion = function(completion) {
      completions[[length(completions) + 1L]] <<- completion
    }
  )
  make_adapter <- function(chat, session_id) {
    tempest:::tempest_deputy_chat_adapter(
      chat,
      manifest,
      deputy_session_id = session_id,
      stage = "dialogue",
      role = "moderator",
      on_start = callbacks$on_start,
      on_terminal = callbacks$on_terminal,
      on_completion = callbacks$on_completion
    )
  }
  sync <- make_adapter(
    terminal_chat(),
    "deputy-session-terminal-sync"
  )
  async <- make_adapter(
    terminal_chat(async = TRUE),
    "deputy-session-terminal-async"
  )

  sync_error <- tryCatch(sync$chat("Fail after sync output"), error = identity)
  async_result <- await_tempest_promise(
    async$chat_async("Fail after async output")
  )

  expect_s3_class(sync_error, "tempest_deputy_adapter_error")
  expect_s3_class(async_result$error, "tempest_deputy_adapter_error")
  expect_length(starts, 2L)
  expect_length(terminals, 2L)
  expect_length(completions, 0L)
  expect_identical(
    vapply(terminals, \(terminal) terminal$completion_id, character(1)),
    vapply(starts, `[[`, character(1), "completion_id")
  )
  expect_identical(
    vapply(
      terminals,
      \(terminal) terminal$deputy_execution$status,
      character(1)
    ),
    rep("error", 2L)
  )
  expect_identical(
    vapply(
      terminals,
      \(terminal) terminal$deputy_execution$completion_disposition,
      character(1)
    ),
    rep("terminal", 2L)
  )
})

test_that("cancellation interrupts the active Deputy run", {
  skip_if_not_installed("deputy")
  skip_if_not_installed("coro")
  skip_if_not_installed("promises")
  skip_if_not_installed("later")

  chat <- fake_chat(text = list(c("First chunk", "ignored chunk")))
  manifest <- tempest_research_manifest(
    "deputy-adapter-cancel",
    mode = "costorm",
    config = tempest_config()
  )
  traces <- list()
  terminals <- list()
  completions <- list()
  adapter <- tempest:::tempest_deputy_chat_adapter(
    chat,
    manifest,
    deputy_session_id = "deputy-session-cancel",
    stage = "dialogue",
    role = "moderator",
    on_run = function(trace) {
      traces[[length(traces) + 1L]] <<- trace
      invisible(trace)
    },
    on_terminal = function(terminal) {
      terminals[[length(terminals) + 1L]] <<- terminal
      invisible(terminal)
    },
    on_completion = function(completion) {
      completions[[length(completions) + 1L]] <<- completion
      invisible(completion)
    }
  )
  stream <- adapter$stream_async("Start a cancellable response")
  completion_id <- tempest:::tempest_agent_completion_id(stream)

  first <- await_tempest_promise(stream())
  expect_null(first$error)
  expect_s3_class(first$value, "ellmer::ContentText")
  expect_identical(first$value@text, "First chunk")
  expect_identical(adapter$cancel(), TRUE)
  terminal_error <- tryCatch(
    {
      terminal <- await_tempest_promise(stream())
      terminal$error
    },
    error = identity
  )

  expect_s3_class(terminal_error, "tempest_deputy_adapter_error")
  expect_identical(traces[[1L]]$status, "interrupted")
  expect_length(terminals, 1L)
  expect_length(completions, 0L)
  expect_identical(terminals[[1L]]$completion_id, completion_id)
  expect_identical(terminals[[1L]]$deputy_execution, traces[[1L]])
})

test_that("adapter identity and execution references never expose Agent", {
  skip_if_not_installed("deputy")

  chat <- fake_chat(text = list("Serializable response"))
  manifest <- tempest_research_manifest(
    "deputy-adapter-identity",
    mode = "costorm",
    config = tempest_config()
  )
  first <- tempest:::tempest_deputy_chat_adapter(
    chat,
    manifest,
    deputy_session_id = "deputy-session-identity",
    stage = "dialogue",
    role = "moderator"
  )
  second <- tempest:::tempest_deputy_chat_adapter(
    fake_chat(),
    manifest,
    deputy_session_id = "deputy-session-identity",
    stage = "dialogue",
    role = "moderator"
  )

  identity <- tempest:::tempest_deputy_chat_identity(first)
  expect_identical(
    identity$agent_id,
    tempest:::tempest_deputy_chat_identity(second)$agent_id
  )
  expect_identical(identity$deputy_session_id, "deputy-session-identity")
  expect_identical(
    first$session_id(),
    identity$deputy_session_id
  )
  expect_identical("agent" %in% names(identity), FALSE)
  expect_identical(
    unserialize(serialize(identity, NULL)),
    identity
  )
  expect_no_match(
    jsonlite::toJSON(identity, auto_unbox = TRUE, null = "null"),
    "R6Class|AgentGenerator|<Agent>",
    perl = TRUE
  )

  first$chat("Record a terminal reference")
  execution_error <- tryCatch(
    tempest:::tempest_deputy_chat_last_execution(first),
    error = function(error) error
  )
  expect_s3_class(execution_error, "tempest_deputy_adapter_error")
})

test_that("sync, async, and stream transports carry exact completion IDs", {
  skip_if_not_installed("deputy")
  skip_if_not_installed("coro")
  skip_if_not_installed("ellmer")
  skip_if_not_installed("later")
  skip_if_not_installed("promises")

  chat <- fake_chat(
    text = list(
      "Same response.",
      "Same response.",
      c("Same ", "response.")
    )
  )
  manifest <- tempest_research_manifest(
    "deputy-adapter-completions",
    mode = "costorm",
    config = tempest_config()
  )
  starts <- list()
  completions <- list()
  adapter <- tempest:::tempest_deputy_chat_adapter(
    chat,
    manifest,
    deputy_session_id = "deputy-session-completions",
    agent_id = "tempest-agent-completions",
    stage = "dialogue",
    role = "moderator",
    on_start = function(pending_run) {
      starts[[length(starts) + 1L]] <<- pending_run
      invisible(pending_run)
    },
    on_completion = function(completion) {
      completions[[length(completions) + 1L]] <<- completion
      invisible(completion)
    }
  )

  sync <- adapter$chat(
    enc2utf8("Exact sync prompt — 🧪"),
    run_context = list(correlation_id = "completion-sync")
  )
  async <- await_tempest_promise(adapter$chat_async(
    enc2utf8("Exact async prompt — 東京"),
    run_context = list(correlation_id = "completion-async")
  ))
  stream <- adapter$stream_async(
    enc2utf8("Exact stream prompt — café"),
    run_context = list(correlation_id = "completion-stream")
  )
  streamed <- await_tempest_promise(coro::async(function() {
    text <- character()
    repeat {
      content <- stream()
      if (promises::is.promising(content)) {
        content <- coro::await(content)
      }
      if (coro::is_exhausted(content)) {
        break
      }
      text <- c(text, content@text)
    }
    paste(text, collapse = "")
  })())

  expect_null(async$error)
  expect_null(streamed$error)
  expect_identical(as.character(sync), "Same response.")
  expect_identical(as.character(async$value), "Same response.")
  expect_identical(streamed$value, "Same response.")
  ids <- c(
    tempest:::tempest_agent_completion_id(sync),
    tempest:::tempest_agent_completion_id(async$value),
    tempest:::tempest_agent_completion_id(stream)
  )
  expect_length(unique(ids), 3L)
  expect_all_true(vapply(
    ids,
    tempest:::tempest_opaque_identifier_valid,
    logical(1)
  ))
  expect_length(starts, 3L)
  expect_length(completions, 3L)
  expect_identical(
    vapply(starts, `[[`, character(1), "completion_id"),
    ids
  )
  expect_identical(
    vapply(completions, `[[`, character(1), "completion_id"),
    ids
  )
  expect_identical(
    vapply(completions, `[[`, character(1), "response"),
    rep("Same response.", 3L)
  )
  expect_identical(
    vapply(completions, `[[`, character(1), "prompt"),
    c(
      enc2utf8("Exact sync prompt — 🧪"),
      enc2utf8("Exact async prompt — 東京"),
      enc2utf8("Exact stream prompt — café")
    )
  )
  expect_identical(
    vapply(
      completions,
      \(completion) ellmer::contents_markdown(completion$provider_turn),
      character(1)
    ),
    rep("Same response.", 3L)
  )
  expect_identical(
    vapply(
      completions,
      \(completion) completion$deputy_execution$correlation_id,
      character(1)
    ),
    c("completion-sync", "completion-async", "completion-stream")
  )
  expect_all_false(vapply(
    completions,
    function(completion) {
      any(
        c(
          "prompt",
          "response",
          "provider_turn",
          "completion_id"
        ) %in%
          names(completion$deputy_execution)
      )
    },
    logical(1)
  ))
  identity_json <- tempest:::tempest_research_manifest_canonical_json(list(
    pending = starts,
    traces = lapply(completions, `[[`, "deputy_execution")
  ))
  expect_no_match(identity_json, "Same response.", fixed = TRUE)
  expect_no_match(identity_json, "provider_turn", fixed = TRUE)
  expect_null(adapter$last_execution)
  expect_null(adapter$.tempest_deputy_last_execution)
  last_execution_error <- tryCatch(
    tempest:::tempest_deputy_chat_last_execution(adapter),
    error = identity
  )
  expect_s3_class(
    last_execution_error,
    "tempest_deputy_adapter_error"
  )
})

test_that("all transports discard stale provider turns", {
  skip_if_not_installed("deputy")
  skip_if_not_installed("coro")
  skip_if_not_installed("ellmer")
  skip_if_not_installed("later")
  skip_if_not_installed("promises")

  response <- "Same rendered response."
  stale_turn <- ellmer::AssistantTurn(
    contents = list(ellmer::ContentText(response)),
    json = list(
      output = list(list(
        type = "message",
        content = list(list(
          type = "output_text",
          text = response,
          annotations = list(list(
            type = "url_citation",
            title = "Stale source",
            url = "https://example.org/stale-source"
          ))
        ))
      ))
    )
  )
  chat <- fake_chat(text = rep(list(response), 3L))
  chat$set_turns(list(stale_turn))
  chat$last_turn <- function(role = "assistant") stale_turn
  manifest <- tempest_research_manifest(
    "deputy-adapter-stale-turn",
    mode = "costorm",
    config = tempest_config()
  )
  completions <- list()
  terminals <- list()
  adapter <- tempest:::tempest_deputy_chat_adapter(
    chat,
    manifest,
    deputy_session_id = "deputy-session-stale-turn",
    stage = "dialogue",
    role = "moderator",
    on_completion = function(completion) {
      completions[[length(completions) + 1L]] <<- completion
    },
    on_terminal = function(terminal) {
      terminals[[length(terminals) + 1L]] <<- terminal
    }
  )

  sync_error <- tryCatch(
    adapter$chat("Reject stale sync turn."),
    error = identity
  )
  async_result <- await_tempest_promise(
    adapter$chat_async("Reject stale async turn.")
  )
  stream <- adapter$stream_async("Reject stale streaming turn.")
  stream_result <- await_tempest_promise(coro::async(function() {
    repeat {
      content <- stream()
      if (promises::is.promising(content)) {
        content <- coro::await(content)
      }
      if (coro::is_exhausted(content)) {
        break
      }
    }
  })())

  expect_s3_class(sync_error, "tempest_agent_completion_binding_error")
  expect_s3_class(
    async_result$error,
    "tempest_agent_completion_binding_error"
  )
  expect_s3_class(
    stream_result$error,
    "tempest_agent_completion_binding_error"
  )
  expect_length(completions, 0L)
  expect_length(terminals, 3L)
  expect_identical(
    vapply(terminals, `[[`, character(1), "disposition"),
    rep("discarded", 3L)
  )
  expect_identical(
    vapply(
      terminals,
      \(terminal) terminal$deputy_execution$status,
      character(1)
    ),
    rep("complete", 3L)
  )
  expect_identical(
    vapply(
      terminals,
      \(terminal) terminal$deputy_execution$completion_disposition,
      character(1)
    ),
    rep("discarded", 3L)
  )
})
