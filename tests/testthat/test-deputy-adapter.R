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
  expect_s3_class(adapter, "Chat")
  expect_identical(
    tempest:::tempest_deputy_chat_permissions(adapter),
    list(
      bash = FALSE,
      file_read = FALSE,
      file_write = FALSE,
      install_packages = FALSE,
      mode = "default",
      r_code = FALSE,
      tool_allowlist = c("inspect_evidence", "search_workspace"),
      web = FALSE
    )
  )
  expect_identical(
    names(adapter$get_tools()),
    c("inspect_evidence", "search_workspace")
  )
  registration_error <- tryCatch(
    adapter$register_tools(list()),
    error = identity
  )
  expect_s3_class(registration_error, "tempest_deputy_adapter_error")
})

test_that("allowlisted annotated tools pass after ambient access is disabled", {
  skip_if_not_installed("deputy")

  tool <- ellmer::tool(
    function(query) paste("workspace", query),
    "Search the current Tempest workspace.",
    arguments = list(query = ellmer::type_string()),
    name = "search_workspace",
    annotations = list(open_world_hint = TRUE)
  )
  permissions <- tempest:::tempest_deputy_adapter_permissions(
    "search_workspace",
    25L
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
  expect_identical(permissions$web, FALSE)
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

  expect_identical(first, "First answer.")
  expect_identical(second, "Second answer.")
  expect_length(starts, 2L)
  expect_length(traces, 2L)
  expect_identical(
    names(starts[[1L]]),
    c(
      "agent_id",
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
    vapply(traces, `[[`, character(1), "agent_id"),
    rep("tempest-agent-sync", 2L)
  )
  expect_identical(
    length(unique(vapply(traces, `[[`, character(1), "deputy_run_id"))),
    2L
  )
  expect_identical(adapter$last_execution(), traces[[2L]])
  expect_identical(
    vapply(chat$.calls(), `[[`, character(1), "transport"),
    rep("stream", 2L)
  )
  expect_identical(
    ellmer::contents_markdown(adapter$last_turn()),
    "Second answer."
  )
})

test_that("async chat and streams execute through Deputy run_shiny", {
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
  expect_identical(chat_result$value, "Warmup answer.")
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
  expect_null(recording_adapter$last_execution())
  async_recording <- await_tempest_promise(
    recording_adapter$chat_async("Complete but fail recording")
  )
  expect_s3_class(async_recording$error, "tempest_deputy_adapter_error")
  expect_null(recording_adapter$last_execution())
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
  adapter <- tempest:::tempest_deputy_chat_adapter(
    chat,
    manifest,
    deputy_session_id = "deputy-session-cancel",
    stage = "dialogue",
    role = "moderator"
  )
  stream <- adapter$stream_async("Start a cancellable response")

  first <- await_tempest_promise(stream())
  expect_null(first$error)
  expect_s3_class(first$value, "ellmer::ContentText")
  expect_identical(first$value@text, "First chunk")
  expect_identical(adapter$cancel(), TRUE)
  terminal <- await_tempest_promise(stream())

  expect_null(terminal$error)
  expect_identical(coro::is_exhausted(terminal$value), TRUE)
  expect_identical(adapter$last_execution()$status, "interrupted")
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
  execution <- tempest:::tempest_deputy_chat_last_execution(first)
  expect_identical(
    unserialize(serialize(execution, NULL)),
    execution
  )
  expect_identical("agent" %in% names(execution), FALSE)
})
