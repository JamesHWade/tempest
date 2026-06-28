# Tests for the bundled Shiny app's modules. The app files live in
# inst/shiny/R and are sourced on demand.

source_shiny_modules <- function() {
  dir <- system.file("shiny", "R", package = "tempest")
  skip_if(identical(dir, ""), "Shiny app files not found")
  env <- new.env(parent = globalenv())
  for (f in sort(list.files(dir, pattern = "[.][Rr]$", full.names = TRUE))) {
    sys.source(f, envir = env)
  }
  env
}

test_that("every module UI builds a Shiny tag", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("shinychat")
  app <- source_shiny_modules()

  expect_s3_class(app$mod_config_ui("config"), "shiny.tag")
  expect_s3_class(
    app$mod_chat_ui("chat", app$mod_config_ui("config")),
    "shiny.tag"
  )
  expect_s3_class(app$mod_storm_ui("storm"), "shiny.tag")
  expect_s3_class(app$mod_mindmap_ui("mindmap"), "shiny.tag")
  expect_s3_class(app$mod_sources_ui("sources"), "shiny.tag")
  expect_s3_class(app$mod_facts_ui("facts"), "shiny.tag")
  expect_s3_class(app$mod_transcript_ui("transcript"), "shiny.tag")
  expect_s3_class(app$mod_report_ui("report"), "shiny.tag")
})

test_that("module UIs namespace their input ids", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  app <- source_shiny_modules()
  html <- as.character(app$mod_storm_ui("storm"))
  expect_match(paste(html, collapse = ""), "storm-run")
  expect_match(paste(html, collapse = ""), "storm-topic")
  chat_html <- paste(
    as.character(app$mod_chat_ui("chat", app$mod_config_ui("config"))),
    collapse = ""
  )
  expect_match(chat_html, "chat-progress")
})

test_that("config UI uses current OpenAI model defaults", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  app <- source_shiny_modules()
  html <- paste(as.character(app$mod_config_ui("config")), collapse = "")
  expect_match(html, 'value="openai/gpt-5.4"', fixed = TRUE)
  expect_match(html, 'value="openai/gpt-5.4-mini"', fixed = TRUE)
})

test_that("the About popover links the papers and upstream repos", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  app <- source_shiny_modules()
  html <- paste(as.character(app$about_nav_item()), collapse = "")
  expect_match(html, "arxiv.org/abs/2402.14207")
  expect_match(html, "arxiv.org/abs/2408.15232")
  expect_match(html, "arxiv.org/abs/2310.03714")
  expect_match(html, "github.com/stanford-oval/storm")
  expect_match(html, "github.com/stanfordnlp/dspy")
})

test_that("the chat module provides a landing welcome message", {
  app <- source_shiny_modules()
  msg <- app$welcome_message()
  expect_type(msg, "character")
  expect_length(msg, 1)
  expect_match(msg, "Welcome to tempest")
  expect_match(msg, "Start Session")
})

test_that("nav panels carry explicit string values for nav_select()", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  app <- source_shiny_modules()
  report_html <- paste(as.character(app$mod_report_ui("report")), collapse = "")
  expect_match(report_html, 'data-value="Report"')
})

test_that("the report module renders markdown and an empty state", {
  skip_if_not_installed("shiny")
  app <- source_shiny_modules()
  store <- app$new_session_store()

  shiny::testServer(app$mod_report_server, args = list(store = store), {
    expect_match(as.character(output$body$html), "Generate a report")
    store$set_report("# Findings\n\nSome text.", topic = "My Topic")
    session$flushReact()
    expect_match(as.character(output$body$html), "Findings")
  })
})

test_that("sources and facts modules show empty states until data exists", {
  skip_if_not_installed("shiny")
  app <- source_shiny_modules()
  store <- app$new_session_store()

  shiny::testServer(app$mod_sources_server, args = list(store = store), {
    expect_match(as.character(output$body$html), "Start a session")

    empty_session <- list(store = SourceStore$new())
    store$set(empty_session)
    session$flushReact()
    expect_match(as.character(output$body$html), "No sources collected yet")

    populated_session <- list(store = fake_store_with_sources(1))
    store$set(populated_session)
    session$flushReact()
    sources_body <- as.character(output$body$html)
    expect_match(sources_body, "<div")
    expect_no_match(sources_body, "No sources collected yet")
  })

  shiny::testServer(app$mod_facts_server, args = list(store = store), {
    store$set(NULL)
    session$flushReact()
    expect_match(as.character(output$body$html), "Start a session")

    empty_session <- list(store = SourceStore$new())
    store$set(empty_session)
    session$flushReact()
    expect_match(as.character(output$body$html), "No facts extracted yet")

    populated_store <- fake_store_with_sources(1)
    source_id <- populated_store$list_sources()[[1]]$id
    populated_store$add_claim(tempest_claim(
      claim_text = "Example evidence exists.",
      source_ids = source_id
    ))
    populated_session <- list(store = populated_store)
    store$set(populated_session)
    session$flushReact()
    facts_body <- as.character(output$body$html)
    expect_match(facts_body, "<div")
    expect_no_match(facts_body, "No facts extracted yet")
  })
})

test_that("session store mutations work outside reactive consumers", {
  skip_if_not_installed("shiny")
  app <- source_shiny_modules()
  store <- app$new_session_store()
  ses <- list(topic = "Test topic")

  expect_silent(store$touch())
  expect_silent(store$set(ses))
  expect_identical(shiny::isolate(store$get()), ses)
})

test_that("the transcript module shows recent turns from the store", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ellmer")
  app <- source_shiny_modules()
  store <- app$new_session_store()
  ses <- tempest_session(
    "Test topic",
    config = tempest_config(),
    personas = list(list(
      id = 1,
      name = "Dr. A",
      title = "Sci",
      perspective = "X"
    ))
  )

  shiny::testServer(app$mod_transcript_server, args = list(store = store), {
    expect_match(as.character(output$body$html), "Start a conversation")
    ses$add_turn("user", "user", "hello world")
    store$set(ses)
    session$flushReact()
    expect_match(as.character(output$body$html), "hello world")
  })
})

test_that("the mind map module counts nodes, sources, facts, and turns", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ellmer")
  app <- source_shiny_modules()
  store <- app$new_session_store()
  ses <- tempest_session(
    "Test topic",
    config = tempest_config(),
    personas = list(list(
      id = 1,
      name = "Dr. A",
      title = "Sci",
      perspective = "X"
    ))
  )
  ses$add_turn("user", "user", "q1")

  shiny::testServer(app$mod_mindmap_server, args = list(store = store), {
    store$set(ses)
    session$flushReact()
    expect_equal(output$n_turns, "1")
    expect_match(output$n_sources, "^[0-9]+$")
    expect_match(output$n_facts, "^[0-9]+$")
  })
})

test_that("shared fake Co-STORM session populates evidence tabs", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ellmer")
  app <- source_shiny_modules()
  shared <- app$new_session_store()
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  source_store <- fake_store_with_sources(1)
  source_id <- source_store$list_sources()[[1]]$id
  source_store$add_claim(tempest_claim(
    claim_text = "Integrated evidence is available.",
    source_ids = source_id,
    confidence = "high"
  ))
  retriever <- tempest_retriever(config = cfg, store = source_store)
  ses <- tempest_session(
    "Integration topic",
    config = cfg,
    personas = list(tempest_expert(
      name = "Integration Expert",
      title = "Researcher",
      perspective = "End-to-end app behavior"
    )),
    retriever = retriever
  )
  ses$mindmap <- list(
    nodes = list(
      list(
        id = "root",
        label = "Integration topic",
        parent = NULL,
        notes = "",
        source_ids = character()
      ),
      list(
        id = "evidence",
        label = "Evidence",
        parent = "root",
        notes = "Integrated evidence node.",
        source_ids = source_id
      )
    ),
    edges = list(list(from = "root", to = "evidence", relation = "supports"))
  )
  ses$add_turn("user", "user", "What evidence exists?")
  ses$add_turn(
    "Moderator",
    "assistant",
    paste0("Integrated evidence is available [", source_id, "].")
  )
  shared$set(ses)
  shared$set_report("# Integrated report", topic = ses$topic)

  shiny::testServer(app$mod_sources_server, args = list(store = shared), {
    session$flushReact()
    expect_no_match(as.character(output$body$html), "No sources collected yet")
  })
  shiny::testServer(app$mod_facts_server, args = list(store = shared), {
    session$flushReact()
    expect_no_match(as.character(output$body$html), "No facts extracted yet")
  })
  shiny::testServer(app$mod_mindmap_server, args = list(store = shared), {
    session$flushReact()
    expect_equal(output$n_nodes, "2")
    expect_equal(output$n_sources, "1")
    expect_equal(output$n_facts, "1")
    expect_equal(output$n_turns, "2")
  })
  shiny::testServer(app$mod_transcript_server, args = list(store = shared), {
    session$flushReact()
    expect_match(as.character(output$body$html), "Integrated evidence")
  })
  shiny::testServer(app$mod_report_server, args = list(store = shared), {
    session$flushReact()
    expect_match(as.character(output$body$html), "Integrated report")
  })
})

test_that("suggestion_cards builds a shinychat submit-card list", {
  app <- source_shiny_modules()
  md <- app$suggestion_cards(c("What is X?", "How does Y work?"))
  expect_type(md, "character")
  expect_match(md, "You might ask")
  expect_match(md, '- <span class="suggestion submit">What is X\\?</span>')
  expect_match(
    md,
    '- <span class="suggestion submit">How does Y work\\?</span>'
  )
})

test_that("suggestion_cards returns NULL for no questions", {
  app <- source_shiny_modules()
  expect_null(app$suggestion_cards(character()))
  expect_null(app$suggestion_cards(c("", NA_character_)))
})

test_that("suggestion_cards keeps only valid questions, in order", {
  app <- source_shiny_modules()
  md <- app$suggestion_cards(c("Keep this?", "", NA_character_, "And this"))
  n_items <- length(gregexpr('class="suggestion submit"', md, fixed = TRUE)[[
    1
  ]])
  expect_equal(n_items, 2)
  expect_lt(
    regexpr("Keep this", md, fixed = TRUE),
    regexpr("And this", md, fixed = TRUE)
  )
})

test_that("suggestion_cards honors a custom lead", {
  app <- source_shiny_modules()
  md <- app$suggestion_cards("Q1", lead = "**Try asking:**")
  expect_match(md, "Try asking", fixed = TRUE)
})

test_that("append_suggestions gates on the toggle and swallows generator errors", {
  app <- source_shiny_modules()
  calls <- character()
  rec <- function(x) calls[[length(calls) + 1L]] <<- x
  ok <- list(suggest_questions = function(n) c("Q1?", "Q2?"))
  err <- list(suggest_questions = function(n) stop("boom"))

  app$append_suggestions(ok, TRUE, rec)
  expect_match(calls[[1]], "suggestion submit")

  calls <- character()
  app$append_suggestions(ok, FALSE, rec)
  expect_length(calls, 0)

  calls <- character()
  expect_silent(app$append_suggestions(err, TRUE, rec))
  expect_length(calls, 0)

  calls <- character()
  app$append_suggestions(NULL, TRUE, rec)
  expect_length(calls, 0)
})

test_that("start suggestions wait for warmup when experts are available", {
  app <- source_shiny_modules()

  expect_equal(
    app$should_delay_start_suggestions(TRUE, list(list(name = "Dr. A"))),
    TRUE
  )
  expect_equal(
    app$should_delay_start_suggestions(FALSE, list(list(name = "Dr. A"))),
    FALSE
  )
  expect_equal(app$should_delay_start_suggestions(TRUE, list()), FALSE)
})

test_that("the chat sidebar offers a follow-up-questions toggle", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("shinychat")
  app <- source_shiny_modules()
  html <- paste(
    as.character(app$mod_chat_ui("chat", app$mod_config_ui("config"))),
    collapse = ""
  )
  expect_match(html, "chat-suggest")
  expect_match(html, "Suggest follow-up questions")
})

test_that("workflow_progress_ui renders reducer state", {
  skip_if_not_installed("shiny")
  app <- source_shiny_modules()
  state <- tempest_progress_state(list(
    tempest_progress_event(
      run_id = "session-1",
      workflow = "costorm",
      event_type = "workflow",
      status = "started",
      stage = "session",
      step = "created"
    ),
    tempest_progress_event(
      run_id = "session-1",
      workflow = "costorm",
      event_type = "stage",
      status = "started",
      stage = "warmup",
      step = "expert_fanout"
    ),
    tempest_progress_event(
      run_id = "session-1",
      workflow = "costorm",
      event_type = "expert",
      status = "started",
      stage = "warmup",
      step = "expert_fanout",
      payload = list(expert_name = "Dr. Flow")
    )
  ))

  html <- paste(
    as.character(app$workflow_progress_ui(state, app$costorm_stage_labels())),
    collapse = ""
  )

  expect_match(html, "Co-STORM")
  expect_match(html, "Running")
  expect_match(html, "Warmup")
  expect_match(html, "Dr. Flow")
})

test_that("storm_progress_state renders failed and running states", {
  skip_if_not_installed("shiny")
  app <- source_shiny_modules()
  running <- app$storm_progress_state(list(app$storm_running_event("Topic")))
  failed <- app$storm_progress_state(list(), "error")

  expect_equal(running$status, "running")
  expect_equal(failed$status, "failed")
  html <- paste(
    as.character(app$workflow_progress_ui(failed, app$storm_stage_labels())),
    collapse = ""
  )
  expect_match(html, "Failed")
  expect_match(html, "STORM pipeline failed")
})

await_promise <- function(promise, timeout_s = 2) {
  done <- FALSE
  value <- NULL
  error <- NULL
  promises::then(
    promise,
    onFulfilled = function(x) {
      value <<- x
      done <<- TRUE
    },
    onRejected = function(e) {
      error <<- e
      done <<- TRUE
    }
  )

  deadline <- Sys.time() + timeout_s
  while (!done && Sys.time() < deadline) {
    later::run_now(0.01)
  }
  expect_equal(done, TRUE)
  if (!is.null(error)) {
    stop(error)
  }
  value
}

fake_warmup_session <- function(
  chat_async,
  stream_async = NULL,
  personas = NULL,
  progress = NULL
) {
  turns <- new.env(parent = emptyenv())
  turns$n <- 0L
  if (is.null(personas)) {
    personas <- list(list(
      name = "Dr. A",
      title = "Expert",
      initial_questions = c("What is X?", "How does Y work?")
    ))
  }

  call_chat_async <- function(prompt, persona) {
    arg_names <- names(formals(chat_async))
    if ("..." %in% arg_names || length(arg_names) >= 2L) {
      return(chat_async(prompt, persona))
    }
    chat_async(prompt)
  }

  call_stream_async <- function(prompt, stream, persona) {
    arg_names <- names(formals(stream_async))
    if ("..." %in% arg_names || length(arg_names) >= 3L) {
      return(stream_async(prompt, stream, persona))
    }
    if (length(arg_names) >= 2L) {
      return(stream_async(prompt, stream))
    }
    stream_async(prompt)
  }

  list(
    topic = "Test topic",
    personas = personas,
    expert_session_manager = list(
      get_or_create = function(persona) {
        chat <- list(
          chat_async = function(prompt) call_chat_async(prompt, persona)
        )
        if (!is.null(stream_async)) {
          chat$stream_async <- function(prompt, stream) {
            call_stream_async(prompt, stream, persona)
          }
        }
        list(chat = chat)
      },
      extract_facts = function(response) NULL
    ),
    add_turn = function(...) {
      turns$n <- turns$n + 1L
      invisible(NULL)
    },
    update_mindmap = function(...) invisible(NULL),
    store = list(
      list_claims = function() list(),
      list_sources = function() list()
    ),
    emit_progress = function(
      event_type,
      status,
      stage = NA_character_,
      step = NA_character_,
      message = NA_character_,
      payload = list(),
      parent_event_id = NA_character_,
      correlation_id = NA_character_
    ) {
      event <- tempest_progress_event(
        run_id = "fake-session",
        workflow = "costorm",
        event_type = event_type,
        status = status,
        stage = stage,
        step = step,
        message = message,
        payload = payload,
        parent_event_id = parent_event_id,
        correlation_id = correlation_id
      )
      if (!is.null(progress)) {
        progress(event)
      }
      event
    },
    turns = turns
  )
}

fake_warmup_store <- function() {
  touches <- new.env(parent = emptyenv())
  touches$n <- 0L
  list(
    touch = function() {
      touches$n <- touches$n + 1L
      invisible(NULL)
    },
    count = function() touches$n
  )
}

test_that("run_warmup reports per-question progress and finishes", {
  skip_if_not_installed("later")
  skip_if_not_installed("promises")
  app <- source_shiny_modules()
  messages <- character()
  store <- fake_warmup_store()
  ses <- fake_warmup_session(function(prompt) {
    promises::promise_resolve("Supported answer [S123456789abc].")
  })

  await_promise(app$run_warmup(
    ses,
    store,
    function(x) messages[[length(messages) + 1L]] <<- x,
    timeout_s = 1,
    max_questions_per_expert = 1
  ))

  transcript <- paste(messages, collapse = "\n")
  expect_match(transcript, "Running warmup phase")
  expect_match(transcript, "warmup question 1/1")
  expect_match(transcript, "Warmup complete")
  expect_equal(store$count(), 2)
  expect_equal(ses$turns$n, 1)
})

test_that("run_warmup records progress events for reducer rendering", {
  skip_if_not_installed("later")
  skip_if_not_installed("promises")
  app <- source_shiny_modules()
  collector <- tempest_progress_collector(include_payload = TRUE)
  store <- fake_warmup_store()
  ses <- fake_warmup_session(
    function(prompt) promises::promise_resolve("Supported answer."),
    progress = collector$record
  )

  await_promise(app$run_warmup(
    ses,
    store,
    function(x) x,
    timeout_s = 1,
    max_questions_per_expert = 1
  ))

  labels <- vapply(
    collector$data(),
    function(event) {
      paste(
        event$event_type,
        event$stage,
        event$step,
        event$status,
        sep = ":"
      )
    },
    character(1)
  )
  state <- tempest_progress_state(collector$events())

  expect_contains(
    labels,
    c(
      "stage:warmup:expert_fanout:started",
      "expert:warmup:expert_fanout:started",
      "tool:warmup:expert_question:started",
      "tool:warmup:expert_question:succeeded",
      "expert:warmup:expert_fanout:succeeded",
      "stage:warmup:expert_fanout:succeeded"
    )
  )
  expect_equal(state$completed_stages, "warmup")
  expect_length(state$active$tools, 0L)
})

test_that("run_warmup does not stream warmup answers into the main chat", {
  skip_if_not_installed("later")
  skip_if_not_installed("promises")
  app <- source_shiny_modules()
  messages <- character()
  store <- fake_warmup_store()
  used_stream <- FALSE
  used_chat <- FALSE
  ses <- fake_warmup_session(
    chat_async = function(prompt) {
      used_chat <<- TRUE
      promises::promise_resolve("Private answer [S123456789abc].")
    },
    stream_async = function(prompt, stream) {
      used_stream <<- TRUE
      promises::promise_resolve("Visible streamed answer [S123456789abc].")
    }
  )

  await_promise(app$run_warmup(
    ses,
    store,
    function(x) {
      messages[[length(messages) + 1L]] <<- x
      x
    },
    timeout_s = 1,
    max_questions_per_expert = 1
  ))

  transcript <- paste(messages, collapse = "\n")
  expect_equal(used_chat, TRUE)
  expect_equal(used_stream, FALSE)
  expect_no_match(transcript, "Private answer")
  expect_no_match(transcript, "Visible streamed answer")
  expect_equal(ses$turns$n, 1)
})

test_that("run_warmup starts independent experts in parallel", {
  skip_if_not_installed("later")
  skip_if_not_installed("promises")
  app <- source_shiny_modules()
  messages <- character()
  store <- fake_warmup_store()
  resolve_first <- NULL
  second_started <- FALSE
  personas <- list(
    list(name = "Dr. A", title = "Expert", initial_questions = "A?"),
    list(name = "Dr. B", title = "Expert", initial_questions = "B?")
  )
  ses <- fake_warmup_session(
    function(prompt, persona) {
      if (identical(persona$name, "Dr. A")) {
        return(promises::promise(function(resolve, reject) {
          resolve_first <<- resolve
        }))
      }
      second_started <<- TRUE
      promises::promise_resolve("Second answer [S123456789abc].")
    },
    personas = personas
  )

  promise <- app$run_warmup(
    ses,
    store,
    function(x) messages[[length(messages) + 1L]] <<- x,
    timeout_s = 1,
    max_questions_per_expert = 1,
    max_parallel_experts = 2
  )
  deadline <- Sys.time() + 1
  while ((is.null(resolve_first) || !second_started) && Sys.time() < deadline) {
    later::run_now(0.01)
  }

  expect_equal(is.null(resolve_first), FALSE)
  expect_equal(second_started, TRUE)
  if (is.null(resolve_first)) {
    stop("First expert did not start.")
  }
  resolve_first("First answer [S123456789abc].")
  await_promise(promise)

  transcript <- paste(messages, collapse = "\n")
  expect_match(transcript, "Dr. A")
  expect_match(transcript, "Dr. B")
  expect_equal(ses$turns$n, 2)
})

test_that("run_warmup times out a stuck question and continues", {
  skip_if_not_installed("later")
  skip_if_not_installed("promises")
  app <- source_shiny_modules()
  messages <- character()
  store <- fake_warmup_store()
  ses <- fake_warmup_session(function(prompt) {
    promises::promise(function(resolve, reject) {})
  })

  await_promise(app$run_warmup(
    ses,
    store,
    function(x) messages[[length(messages) + 1L]] <<- x,
    timeout_s = 0.01,
    max_questions_per_expert = 1
  ))

  transcript <- paste(messages, collapse = "\n")
  expect_match(transcript, "timed out")
  expect_match(transcript, "Warmup complete")
  expect_equal(store$count(), 1)
  expect_equal(ses$turns$n, 0)
})

test_that("run_warmup ignores stale callbacks after the session is gone", {
  skip_if_not_installed("later")
  skip_if_not_installed("promises")
  app <- source_shiny_modules()
  messages <- character()
  store <- fake_warmup_store()
  resolve_chat <- NULL
  active <- TRUE
  ses <- fake_warmup_session(function(prompt) {
    promises::promise(function(resolve, reject) {
      resolve_chat <<- resolve
    })
  })

  promise <- app$run_warmup(
    ses,
    store,
    function(x) messages[[length(messages) + 1L]] <<- x,
    is_current = function() active,
    timeout_s = 1,
    max_questions_per_expert = 1
  )
  while (is.null(resolve_chat)) {
    later::run_now(0.01)
  }

  active <- FALSE
  resolve_chat("Late answer [S123456789abc].")
  await_promise(promise)

  transcript <- paste(messages, collapse = "\n")
  expect_match(transcript, "warmup question 1/1")
  expect_no_match(transcript, "Warmup complete")
  expect_equal(store$count(), 0)
  expect_equal(ses$turns$n, 0)
})
