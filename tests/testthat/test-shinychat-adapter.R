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
  replacement <- new.env(parent = emptyenv())

  server <- function(id) {
    shiny::moduleServer(id, function(input, output, session) {
      adapter <- tempest_shinychat_adapter(
        "chat",
        initial_client = shell,
        session = session,
        on_turn = function(
          user_text,
          assistant_text,
          assistant_turn,
          is_current
        ) {
          calls[[length(calls) + 1L]] <<- list(
            user = user_text,
            assistant = assistant_text,
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

    state$last_input("New question")
    session$flushReact()
    state$last_turn("New answer")
    session$flushReact()
    expect_length(calls, 1L)
    expect_identical(calls[[1L]]$user, "New question")
    expect_identical(calls[[1L]]$assistant, "New answer")
    expect_identical(calls[[1L]]$current, TRUE)
    expect_identical(calls[[1L]]$domain, session)

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
    expect_identical(state$set_clients[[1L]], replacement)
    expect_identical(state$clears[[1L]], "clear")
    expect_identical(state$appends[[1L]], "Replacement")

    adapter$dispose()
    state$last_input("Disposed question")
    state$last_turn("Disposed answer")
    session$flushReact()
    expect_length(calls, 1L)
    expect_identical(disposed, 1L)
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
  expect_identical(
    tempest_shinychat_input_text(list("Question", "Attachment text")),
    "Question\nAttachment text"
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
