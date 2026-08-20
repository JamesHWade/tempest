test_that("shinychat adapter rejects incompatible backends and handles", {
  missing_backend <- list(version = "0.4.0.9000")
  backend_error <- tryCatch(
    tempest_shinychat_validate_backend(missing_backend),
    error = identity
  )

  expect_s3_class(backend_error, "tempest_shinychat_error")
  expect_match(conditionMessage(backend_error), "chat_server")

  bad_signature <- list(
    version = "0.4.0.9000",
    chat_ui = function(id, greeting) NULL,
    chat_greeting = function(content) NULL,
    chat_server = function(id, client) NULL
  )
  signature_error <- tryCatch(
    tempest_shinychat_validate_backend(bad_signature),
    error = identity
  )

  expect_s3_class(signature_error, "tempest_shinychat_error")
  expect_match(conditionMessage(signature_error), "chat_server")

  bad_handle <- list(
    append = function(response, role) NULL,
    clear = function(messages, greeting, client_history) NULL
  )
  handle_error <- tryCatch(
    tempest_shinychat_validate_handle(bad_handle, "0.4.0.9000"),
    error = identity
  )

  expect_s3_class(handle_error, "tempest_shinychat_error")
  expect_match(conditionMessage(handle_error), "set_client")

  environment_handle <- list2env(
    list(
      append = function(response, role = "assistant") NULL,
      clear = function(
        messages = NULL,
        greeting = FALSE,
        client_history = "clear"
      ) {
        NULL
      },
      last_input = function() NULL,
      last_turn = function() NULL,
      set_client = function(client, sync = TRUE) NULL,
      slash_command = function(name, description, handler) NULL,
      status = function() "idle"
    ),
    parent = emptyenv()
  )

  expect_identical(
    tempest_shinychat_validate_handle(environment_handle),
    environment_handle
  )
})

test_that("shinychat adapter owns client and restoration lifecycle", {
  skip_if_not_installed("shiny")
  state <- new.env(parent = emptyenv())
  state$server_args <- NULL
  state$set_clients <- list()
  state$clears <- list()
  state$appends <- list()
  state$command_cancels <- 0L
  state$last_input <- shiny::reactiveVal(NULL)
  state$last_turn <- shiny::reactiveVal(NULL)
  state$status <- shiny::reactiveVal("idle")
  backend <- list(
    version = "0.4.0.9000",
    chat_ui = function(id, greeting, ...) list(id = id, greeting = greeting),
    chat_greeting = function(content, ...) content,
    chat_server = function(
      id,
      client,
      greeting = NULL,
      history = TRUE,
      session = shiny::getDefaultReactiveDomain()
    ) {
      state$server_args <- list(
        id = id,
        client = client,
        history = history,
        session = session
      )
      list(
        append = function(response, role = "assistant", icon = NULL) {
          state$appends[[length(state$appends) + 1L]] <- list(
            content = response,
            role = role
          )
        },
        clear = function(
          messages = NULL,
          greeting = FALSE,
          client_history = c("clear", "set", "append", "keep")
        ) {
          state$clears[[length(state$clears) + 1L]] <- list(
            messages = messages,
            greeting = greeting,
            client_history = client_history[[1L]]
          )
        },
        last_input = shiny::reactive(state$last_input()),
        last_turn = shiny::reactive(state$last_turn()),
        set_client = function(new_client, sync = TRUE) {
          state$set_clients[[length(state$set_clients) + 1L]] <- list(
            client = new_client,
            sync = sync
          )
        },
        slash_command = function(name, description, handler, ...) {
          function() state$command_cancels <- state$command_cancels + 1L
        },
        status = shiny::reactive(state$status())
      )
    }
  )
  shell <- new.env(parent = emptyenv())
  fresh <- new.env(parent = emptyenv())
  restored <- new.env(parent = emptyenv())

  server <- function(id) {
    shiny::moduleServer(id, function(input, output, session) {
      adapter <- tempest_shinychat_adapter(
        "chat",
        initial_client = shell,
        session = session,
        on_turn = function(...) NULL,
        backend = backend
      )
    })
  }

  shiny::testServer(server, {
    session$flushReact()
    expect_identical(state$server_args$history, FALSE)
    expect_identical(state$server_args$client, shell)

    expect_identical(
      adapter$bind(
        fresh,
        list(list(role = "assistant", content = "Fresh session")),
        client_history = "clear"
      ),
      TRUE
    )
    expect_identical(state$set_clients[[1L]]$client, fresh)
    expect_identical(state$set_clients[[1L]]$sync, FALSE)
    expect_null(state$clears[[1L]]$messages)
    expect_identical(state$clears[[1L]]$client_history, "clear")
    expect_identical(state$clears[[1L]]$greeting, FALSE)
    expect_identical(state$appends[[1L]]$content, "Fresh session")

    messages <- tempest_shinychat_restore_messages(
      list(
        list(role = "user", speaker = "User", text = "Question"),
        list(role = "assistant", speaker = "Moderator", text = "Answer")
      ),
      topic = "Restored topic",
      report_available = TRUE
    )
    adapter$bind(restored, messages, client_history = "keep")
    expect_identical(state$set_clients[[2L]]$client, restored)
    expect_identical(state$set_clients[[2L]]$sync, FALSE)
    expect_null(state$clears[[2L]]$messages)
    expect_identical(state$clears[[2L]]$client_history, "keep")
    expect_identical(state$clears[[2L]]$greeting, FALSE)
    restored_roles <- vapply(
      tail(state$appends, 4L),
      `[[`,
      character(1),
      "role"
    )
    expect_identical(
      restored_roles,
      c("assistant", "user", "assistant", "assistant")
    )

    append_count <- length(state$appends)
    adapter$reset()
    expect_identical(state$set_clients[[3L]]$client, shell)
    expect_null(state$clears[[3L]]$messages)
    expect_identical(state$clears[[3L]]$client_history, "clear")
    expect_identical(state$clears[[3L]]$greeting, FALSE)
    expect_length(state$appends, append_count)

    adapter$register_commands(list(
      help = list(
        description = "Show help.",
        handler = function() NULL
      )
    ))
    expect_identical(adapter$dispose(), TRUE)
    expect_identical(adapter$dispose(), FALSE)
    expect_identical(state$command_cancels, 1L)
    expect_identical(adapter$status(), "disposed")
  })
})

test_that("shinychat adapter binds outside a reactive consumer", {
  skip_if_not_installed("shiny")
  state <- new.env(parent = emptyenv())
  state$last_input <- shiny::reactiveVal(NULL)
  state$last_turn <- shiny::reactiveVal(NULL)
  state$status <- shiny::reactiveVal("idle")
  backend <- list(
    version = "0.4.0.9000",
    chat_ui = function(id, greeting, ...) NULL,
    chat_greeting = function(content, ...) content,
    chat_server = function(id, client, history, session, ...) {
      list(
        append = function(response, role = "assistant") NULL,
        clear = function(
          messages = NULL,
          greeting = FALSE,
          client_history = "clear"
        ) {
          NULL
        },
        last_input = shiny::reactive(state$last_input()),
        last_turn = shiny::reactive(state$last_turn()),
        set_client = function(client, sync = TRUE) {
          state$status()
          NULL
        },
        slash_command = function(name, description, handler) NULL,
        status = shiny::reactive(state$status())
      )
    }
  )
  session <- shiny::MockShinySession$new()
  withr::defer(session$close())
  adapter <- shiny::withReactiveDomain(
    session,
    tempest_shinychat_adapter(
      "chat",
      initial_client = new.env(parent = emptyenv()),
      session = session,
      on_turn = function(...) NULL,
      backend = backend
    )
  )
  withr::defer(adapter$dispose())

  expect_identical(
    adapter$bind(new.env(parent = emptyenv())),
    TRUE
  )
})

test_that("shinychat adapter suppresses stale turns and defers client binding", {
  skip_if_not_installed("shiny")
  state <- new.env(parent = emptyenv())
  state$last_input <- shiny::reactiveVal(NULL)
  state$last_turn <- shiny::reactiveVal(NULL)
  state$status <- shiny::reactiveVal("idle")
  state$set_clients <- list()
  state$clears <- list()
  state$appends <- list()
  backend <- list(
    version = "0.4.0.9000",
    chat_ui = function(id, greeting, ...) NULL,
    chat_greeting = function(content, ...) content,
    chat_server = function(
      id,
      client,
      greeting = NULL,
      history = TRUE,
      session = shiny::getDefaultReactiveDomain()
    ) {
      list(
        append = function(response, role = "assistant", icon = NULL) {
          state$appends[[length(state$appends) + 1L]] <- response
        },
        clear = function(
          messages = NULL,
          greeting = FALSE,
          client_history = c("clear", "set", "append", "keep")
        ) {
          state$clears[[length(state$clears) + 1L]] <- client_history[[1L]]
        },
        last_input = shiny::reactive(state$last_input()),
        last_turn = shiny::reactive(state$last_turn()),
        set_client = function(new_client, sync = TRUE) {
          state$set_clients[[length(state$set_clients) + 1L]] <- new_client
        },
        slash_command = function(name, description, handler, ...) NULL,
        status = shiny::reactive(state$status())
      )
    }
  )
  calls <- list()
  disposed <- 0L
  shell <- new.env(parent = emptyenv())
  issued <- 0L
  replacement <- structure(
    list(
      stream_async = function(...) {
        issued <<- issued + 1L
        completion_id <- paste0("completion-", issued)
        stream <- coro::generator(function() {
          coro::yield(paste0("answer-", issued))
          coro::exhausted()
        })()
        tempest:::tempest_agent_completion_tag(stream, completion_id)
      }
    ),
    class = c("TempestDeputyChatAdapter", "Chat", "list")
  )
  drain <- function(stream) {
    settled <- FALSE
    failure <- NULL
    task <- coro::async(function() {
      repeat {
        value <- stream()
        if (promises::is.promising(value)) {
          value <- coro::await(value)
        }
        if (coro::is_exhausted(value)) {
          break
        }
      }
      NULL
    })()
    promises::then(
      task,
      onFulfilled = function(value) settled <<- TRUE,
      onRejected = function(error) {
        failure <<- error
        settled <<- TRUE
      }
    )
    for (index in seq_len(100L)) {
      later::run_now(timeoutSecs = 0.01)
      if (settled) {
        break
      }
    }
    expect_identical(settled, TRUE)
    expect_null(failure)
    invisible(NULL)
  }

  server <- function(id) {
    shiny::moduleServer(id, function(input, output, session) {
      adapter <- tempest_shinychat_adapter(
        "chat",
        initial_client = shell,
        session = session,
        on_turn = function(completion_id, is_current) {
          calls[[length(calls) + 1L]] <<- list(
            completion_id = completion_id,
            current = is_current(),
            domain = shiny::getDefaultReactiveDomain()
          )
        },
        on_dispose = function() disposed <<- disposed + 1L,
        backend = backend
      )
    })
  }

  shiny::testServer(server, {
    session$flushReact()
    state$last_input("Old question")
    session$flushReact()
    adapter$invalidate()
    state$last_turn("Old answer")
    session$flushReact()
    expect_length(calls, 0L)

    state$status("streaming")
    session$flushReact()
    expect_identical(
      adapter$bind(
        replacement,
        list(list(role = "assistant", content = "Replacement"))
      ),
      FALSE
    )
    expect_length(state$set_clients, 0L)
    state$status("idle")
    session$flushReact()
    bound_client <- state$set_clients[[1L]]
    expect_s3_class(bound_client, "TempestShinyChatCompletionClient")
    expect_identical(state$clears[[1L]], "clear")
    expect_identical(state$appends[[1L]], "Replacement")

    state$last_input("New question")
    session$flushReact()
    drain(bound_client$stream_async("New question"))
    state$last_turn("New answer")
    session$flushReact()
    expect_length(calls, 1L)
    expect_identical(calls[[1L]]$completion_id, "completion-1")
    expect_identical(calls[[1L]]$current, TRUE)
    expect_identical(calls[[1L]]$domain, session)

    state$last_input("Stale question")
    session$flushReact()
    drain(bound_client$stream_async("Stale question"))
    adapter$invalidate()
    state$last_turn("Stale answer")
    session$flushReact()
    expect_length(calls, 2L)
    expect_identical(calls[[2L]]$completion_id, "completion-2")
    expect_identical(calls[[2L]]$current, FALSE)

    state$last_turn("Duplicate notification")
    session$flushReact()
    expect_length(calls, 2L)

    adapter$dispose()
    state$last_input("Disposed question")
    state$last_turn("Disposed answer")
    session$flushReact()
    expect_length(calls, 2L)
    expect_identical(disposed, 1L)
  })
})

test_that("shinychat completion client preserves one-use stream identities", {
  issued <- 0L
  fail_next <- FALSE
  client <- structure(
    list(
      stream_async = function(...) {
        issued <<- issued + 1L
        completion_id <- paste0("completion-", issued)
        should_fail <- fail_next
        fail_next <<- FALSE
        source <- coro::generator(function() {
          if (should_fail) {
            stop("stream interrupted", call. = FALSE)
          }
          coro::yield(paste0("answer-", completion_id))
          coro::exhausted()
        })()
        tempest:::tempest_agent_completion_tag(source, completion_id)
      }
    ),
    class = c("TempestDeputyChatAdapter", "Chat", "list")
  )
  ready <- character()
  proxy <- tempest:::tempest_shinychat_completion_client(
    client,
    function(completion_id) ready <<- c(ready, completion_id)
  )
  drain <- function(stream) {
    settled <- FALSE
    failure <- NULL
    request <- tryCatch(
      coro::async(function() {
        repeat {
          value <- stream()
          if (promises::is.promising(value)) {
            value <- coro::await(value)
          }
          if (coro::is_exhausted(value)) {
            break
          }
        }
        NULL
      })(),
      error = function(error) {
        failure <<- error
        settled <<- TRUE
        NULL
      }
    )
    if (is.null(request)) {
      return(list(settled = settled, error = failure))
    }
    promises::then(
      request,
      onFulfilled = function(value) settled <<- TRUE,
      onRejected = function(error) {
        failure <<- error
        settled <<- TRUE
      }
    )
    for (index in seq_len(100L)) {
      later::run_now(timeoutSecs = 0.01)
      if (settled) {
        break
      }
    }
    list(settled = settled, error = failure)
  }

  first <- proxy$stream_async("first")
  second <- proxy$stream_async("second")
  expect_identical(
    tempest:::tempest_agent_completion_id(first),
    "completion-1"
  )
  expect_identical(
    tempest:::tempest_agent_completion_id(second),
    "completion-2"
  )
  expect_null(drain(second)$error)
  expect_null(drain(first)$error)
  expect_identical(ready, c("completion-2", "completion-1"))

  fail_next <- TRUE
  interrupted <- proxy$stream_async("interrupted")
  result <- drain(interrupted)
  expect_identical(result$settled, TRUE)
  expect_s3_class(result$error, "error")
  expect_identical(ready, c("completion-2", "completion-1"))
})

test_that("shinychat completion streams forward resume values", {
  source <- coro::async_generator(function() {
    resumed <- coro::yield("first value")
    coro::yield(resumed)
    coro::exhausted()
  })()
  source <- tempest:::tempest_agent_completion_tag(
    source,
    "completion-shiny-resume"
  )
  client <- structure(
    list(stream_async = function(...) source),
    class = c("TempestDeputyChatAdapter", "Chat", "list")
  )
  completed <- character()
  proxy <- tempest:::tempest_shinychat_completion_client(
    client,
    function(completion_id) completed <<- c(completed, completion_id)
  )
  stream <- proxy$stream_async("prompt")

  first <- await_tempest_promise(stream())
  resumed <- await_tempest_promise(stream("sent value"))
  exhausted <- await_tempest_promise(stream())

  expect_identical(first$value, "first value")
  expect_identical(resumed$value, "sent value")
  expect_identical(coro::is_exhausted(exhausted$value), TRUE)
  expect_identical(completed, "completion-shiny-resume")
})

test_that("shinychat completion streams queue concurrent pre-yield resumes", {
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
    "completion-shiny-concurrent-resume"
  )
  client <- structure(
    list(stream_async = function(...) source),
    class = c("TempestDeputyChatAdapter", "Chat", "list")
  )
  completed <- character()
  proxy <- tempest:::tempest_shinychat_completion_client(
    client,
    function(completion_id) completed <<- c(completed, completion_id)
  )
  stream <- proxy$stream_async("prompt")

  first_request <- stream()
  resumed_request <- stream("concurrent")
  control$resolve("first")
  first <- await_tempest_promise(first_request)
  resumed <- await_tempest_promise(resumed_request)
  exhausted <- await_tempest_promise(stream())

  expect_identical(first$value, "first")
  expect_identical(resumed$value, "concurrent")
  expect_identical(coro::is_exhausted(exhausted$value), TRUE)
  expect_identical(completed, "completion-shiny-concurrent-resume")
})

test_that("shinychat lifecycle closes only replaced or disposed telemetry owners", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("promises")
  local_otel_opt_in()
  otel <- local_fake_otel()
  make_client <- function(owner, label) {
    client <- NULL
    client <- structure(
      list(
        chat = function(
          prompt,
          echo = "none",
          run_context = list(),
          ...
        ) {
          tempest:::tempest_agent_completion_tag(label, paste0(label, "-chat"))
        },
        stream = function(
          prompt = NULL,
          stream = c("text", "content"),
          controller = NULL,
          run_context = list(),
          ...
        ) {
          source <- coro::generator(function() {
            coro::yield(label)
            coro::exhausted()
          })()
          tempest:::tempest_agent_completion_tag(
            source,
            paste0(label, "-stream")
          )
        },
        chat_async = function(
          prompt,
          echo = "none",
          run_context = list(),
          ...
        ) {
          promises::promise_resolve(
            tempest:::tempest_agent_completion_tag(
              label,
              paste0(label, "-chat-async")
            )
          )
        },
        stream_async = function(
          prompt = NULL,
          stream = c("text", "content"),
          controller = NULL,
          run_context = list(),
          ...
        ) {
          source <- coro::async_generator(function() {
            coro::yield(label)
            coro::exhausted()
          })()
          tempest:::tempest_agent_completion_tag(
            source,
            paste0(label, "-stream-async")
          )
        },
        clone = function() client
      ),
      class = c("TempestDeputyChatAdapter", "Chat", "list")
    )
    tempest:::tempest_otel_wrap_completion_client(client, owner)
  }
  first_owner <- tempest:::tempest_otel_owner()
  second_owner <- tempest:::tempest_otel_owner()
  first <- make_client(first_owner, "first")
  second <- make_client(second_owner, "second")
  state <- new.env(parent = emptyenv())
  state$last_input <- shiny::reactiveVal(NULL)
  state$last_turn <- shiny::reactiveVal(NULL)
  state$status <- shiny::reactiveVal("idle")
  state$client <- NULL
  state$turn_count <- 0L
  backend <- list(
    version = "0.4.0.9000",
    chat_ui = function(id, greeting, ...) NULL,
    chat_greeting = function(content, ...) content,
    chat_server = function(id, client, history, session, ...) {
      list(
        append = function(response, role = "assistant") NULL,
        clear = function(
          messages = NULL,
          greeting = FALSE,
          client_history = "clear"
        ) {
          NULL
        },
        last_input = shiny::reactive(state$last_input()),
        last_turn = shiny::reactive(state$last_turn()),
        set_client = function(client, sync = TRUE) {
          state$client <- client
        },
        slash_command = function(name, description, handler) NULL,
        status = shiny::reactive(state$status())
      )
    }
  )
  server <- function(id) {
    shiny::moduleServer(id, function(input, output, session) {
      adapter <- tempest_shinychat_adapter(
        "chat",
        initial_client = first,
        session = session,
        on_turn = function(...) state$turn_count <- state$turn_count + 1L,
        backend = backend
      )
    })
  }

  shiny::testServer(server, {
    expect_identical(adapter$bind(first), TRUE)
    old_client <- state$client
    closed_stream <- state$client$stream_async("closed")
    closed_span <- otel$spans[[1L]]
    closed <- closed_stream(close = TRUE)
    expect_identical(coro::is_exhausted(closed), TRUE)
    expect_identical(
      closed_span$attributes[["tempest.status"]],
      "cancelled"
    )
    expect_identical(closed_span$end_count, 1L)
    state$last_turn("closed")
    session$flushReact()
    expect_identical(state$turn_count, 0L)

    old_stream <- state$client$stream_async("old")
    old_span <- otel$spans[[2L]]
    adapter$invalidate()
    expect_identical(old_span$end_count, 0L)

    expect_identical(adapter$bind(first), TRUE)
    expect_identical(old_span$end_count, 0L)

    state$status("streaming")
    session$flushReact()
    expect_identical(adapter$bind(second), FALSE)
    expect_identical(old_span$end_count, 0L)
    state$status("idle")
    session$flushReact()
    expect_identical(old_span$attributes[["tempest.status"]], "cancelled")
    expect_identical(old_span$end_count, 1L)
    expect_identical(await_tempest_promise(old_stream())$value, "first")
    expect_identical(
      coro::is_exhausted(await_tempest_promise(old_stream())$value),
      TRUE
    )
    expect_identical(old_span$end_count, 1L)
    old_start_calls <- otel$start_calls
    old_tracer_calls <- otel$tracer_calls
    late_old <- old_client$stream_async("late old")
    expect_identical(otel$start_calls, old_start_calls)
    expect_identical(otel$tracer_calls, old_tracer_calls)
    expect_identical(await_tempest_promise(late_old())$value, "first")
    expect_identical(
      coro::is_exhausted(await_tempest_promise(late_old())$value),
      TRUE
    )
    late_old_chat <- old_client$chat("late old sync")
    late_old_stream <- old_client$stream("late old sync")
    expect_identical(as.character(late_old_chat), "first")
    expect_identical(late_old_stream(), "first")
    expect_identical(otel$start_calls, old_start_calls)
    expect_identical(otel$tracer_calls, old_tracer_calls)

    disposed_client <- state$client
    new_stream <- state$client$stream_async("new")
    new_span <- otel$spans[[3L]]
    expect_identical(new_span$end_count, 0L)
    expect_identical(adapter$dispose(), TRUE)
    expect_identical(new_span$attributes[["tempest.status"]], "cancelled")
    expect_identical(new_span$attributes[["tempest.cancelled"]], TRUE)
    expect_identical(new_span$end_count, 1L)
    expect_identical(await_tempest_promise(new_stream())$value, "second")
    expect_identical(new_span$end_count, 1L)
    disposed_start_calls <- otel$start_calls
    disposed_tracer_calls <- otel$tracer_calls
    late_disposed <- disposed_client$stream_async("late disposed")
    expect_identical(otel$start_calls, disposed_start_calls)
    expect_identical(otel$tracer_calls, disposed_tracer_calls)
    expect_identical(
      await_tempest_promise(late_disposed())$value,
      "second"
    )
    expect_identical(
      coro::is_exhausted(await_tempest_promise(late_disposed())$value),
      TRUE
    )
    late_disposed_chat <- disposed_client$chat("late disposed sync")
    late_disposed_stream <- disposed_client$stream("late disposed sync")
    expect_identical(as.character(late_disposed_chat), "second")
    expect_identical(late_disposed_stream(), "second")
    expect_identical(otel$start_calls, disposed_start_calls)
    expect_identical(otel$tracer_calls, disposed_tracer_calls)
  })
})

test_that("shinychat adapter normalizes content and suggestion cards", {
  marker <- paste0(
    intToUtf8(0xE200),
    "cite",
    intToUtf8(0xE202),
    "turn0search0",
    intToUtf8(0xE201)
  )
  expect_identical(
    tempest_shinychat_sanitize_text(paste("Visible", marker, "text.")),
    "Visible text."
  )
  cards <- tempest_shinychat_suggestion_cards(stats::setNames(
    c("What differs?", "What evidence is missing?"),
    c("Compare", "")
  ))
  expect_s3_class(cards, "tempest_shinychat_suggestions")
  expect_match(cards, 'title="Compare"', fixed = TRUE)
  expect_match(cards, 'title="Key uncertainty"', fixed = TRUE)
  expect_match(cards, "suggestion submit", fixed = TRUE)
})

test_that("shinychat citation sanitizer is scoped to the adapter root", {
  skip_if_not_installed("shiny")
  script <- paste(
    as.character(tempest_shinychat_citation_sanitizer("host-chat")),
    collapse = ""
  )

  expect_match(script, "host-chat", fixed = TRUE)
  expect_match(script, "MutationObserver", fixed = TRUE)
  expect_match(script, "tempestCitationSanitizer", fixed = TRUE)
})
