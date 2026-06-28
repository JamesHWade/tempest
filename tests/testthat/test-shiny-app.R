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

test_that("record_warmup_turn populates facts and sources from native turns", {
  skip_if_not_installed("ellmer")
  app <- source_shiny_modules()
  fixture <- native_evidence_session("native warmup claim")
  expert_chat <- list(last_turn = function() fixture$turn)

  app$record_warmup_turn(
    fixture$session,
    "Dr. Native",
    "What did the source say?",
    fixture$claim_text,
    expert_chat = expert_chat,
    session_id = "expert-session-1",
    persona_id = "1",
    correlation_id = "warmup-question-1"
  )

  store <- fixture$session$store
  expect_equal(store$get_source(fixture$source_id)$title, "Native app source")
  claims <- store$list_claims()
  expect_length(claims, 1L)
  expect_equal(claims[[1]]@claim_text, fixture$claim_text)
  expect_equal(claims[[1]]@source_ids, fixture$source_id)
  expect_equal(claims[[1]]@session_id, "expert-session-1")
  expect_equal(claims[[1]]@persona_id, "1")
  expect_equal(claims[[1]]@retrieval_step_id, "warmup-question-1")
})

test_that("extract_chat_turn_facts populates facts and sources from native turns", {
  skip_if_not_installed("ellmer")
  app <- source_shiny_modules()
  fixture <- native_evidence_session("native chat claim")

  ids <- app$extract_chat_turn_facts(
    fixture$session,
    fixture$claim_text,
    turn = fixture$turn,
    session_id = "costorm-session-1",
    persona_id = "moderator",
    correlation_id = "chat-turn-1"
  )

  expect_equal(ids, fixture$source_id)
  store <- fixture$session$store
  expect_equal(store$get_source(fixture$source_id)$title, "Native app source")
  claims <- store$list_claims()
  expect_length(claims, 1L)
  expect_equal(claims[[1]]@claim_text, fixture$claim_text)
  expect_equal(claims[[1]]@source_ids, fixture$source_id)
  expect_equal(claims[[1]]@session_id, "costorm-session-1")
  expect_equal(claims[[1]]@persona_id, "moderator")
  expect_equal(claims[[1]]@retrieval_step_id, "chat-turn-1")
})

test_that("suggestion_cards builds a shinychat submit-card list", {
  app <- source_shiny_modules()
  md <- app$suggestion_cards(c("What is X?", "How does Y work?"))
  expect_type(md, "character")
  expect_match(md, "Research next")
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

test_that("workflow_progress_ui renders compact Co-STORM answer labels", {
  skip_if_not_installed("shiny")
  app <- source_shiny_modules()
  state <- tempest_progress_state(list(
    tempest_progress_event(
      run_id = "session-1",
      workflow = "costorm",
      event_type = "stage",
      status = "started",
      stage = "dialogue",
      step = "turn",
      correlation_id = "turn-1"
    ),
    tempest_progress_event(
      run_id = "session-1",
      workflow = "costorm",
      event_type = "step",
      status = "started",
      stage = "dialogue",
      step = "moderator_response",
      correlation_id = "turn-1"
    )
  ))

  html <- paste(
    as.character(app$workflow_progress_ui(state, app$costorm_stage_labels())),
    collapse = ""
  )

  expect_match(html, "Current:")
  expect_match(html, "Answer")
  expect_match(html, "Moderator")
  expect_match(html, "Next")
  expect_no_match(html, "Dialogue")
  expect_no_match(html, "Moderator Response")
})

test_that("async Co-STORM progress renders warmup state before completion", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("later")
  app <- source_shiny_modules()

  progress_server <- function(id, app) {
    shiny::moduleServer(id, function(input, output, session) {
      progress_events <- shiny::reactiveVal(list())
      output$progress <- shiny::renderUI({
        state <- app$costorm_progress_state(progress_events())
        app$workflow_progress_ui(state, app$costorm_stage_labels())
      })
      record <- function(event) {
        app$record_costorm_progress_event(progress_events, event, session)
      }
    })
  }

  shiny::testServer(progress_server, args = list(app = app), {
    event <- tempest_progress_event(
      run_id = "async-progress",
      workflow = "costorm",
      event_type = "stage",
      status = "started",
      stage = "warmup",
      step = "expert_fanout"
    )
    later::later(
      function() {
        record(event)
      },
      delay = 0
    )
    later::run_now(0.01)
    session$flushReact()

    html <- paste(as.character(output$progress$html), collapse = "")
    expect_match(html, "Co-STORM")
    expect_match(html, "Running")
    expect_match(html, "Warmup")
    expect_match(html, "fa-spinner")
  })
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

test_that("STORM worker uses a local serializable progress collector", {
  app <- source_shiny_modules()
  collector <- app$storm_worker_progress_collector(include_payload = TRUE)
  event <- tempest_progress_event(
    event_id = "worker-research-start",
    run_id = "worker-run",
    workflow = "storm",
    event_type = "stage",
    status = "started",
    stage = "research",
    payload = list(expert = "Dr. Flow")
  )

  collector$record(event)
  data <- collector$data()

  expect_length(data, 1L)
  expect_equal(data[[1]]$run_id, "worker-run")
  expect_equal(data[[1]]$stage, "research")
  expect_equal(data[[1]]$payload$expert, "Dr. Flow")
  expect_equal(tempest_progress_state(data)$current_stage, "research")

  module_source <- readLines(system.file(
    "shiny",
    "R",
    "mod_storm.R",
    package = "tempest"
  ))
  expect_no_match(
    paste(module_source, collapse = "\n"),
    "tempest::tempest_progress_collector",
    fixed = TRUE
  )
})

test_that("STORM worker writes progress events to a stream", {
  app <- source_shiny_modules()
  stream_path <- withr::local_tempfile(fileext = ".ndjson")
  event <- tempest_progress_event(
    event_id = "stream-research-start",
    run_id = "worker-run",
    workflow = "storm",
    event_type = "stage",
    status = "started",
    stage = "research",
    payload = list(expert = "Dr. Flow")
  )
  collector <- app$storm_worker_progress_collector(
    include_payload = TRUE,
    stream_path = stream_path
  )

  collector$record(event)
  streamed <- app$storm_read_progress_stream(stream_path)

  expect_length(streamed, 1L)
  expect_equal(streamed[[1]]$event_id, "stream-research-start")
  expect_equal(streamed[[1]]$stage, "research")
  expect_equal(streamed[[1]]$step, NA_character_)
  expect_equal(streamed[[1]]$payload$expert, "Dr. Flow")
  expect_equal(tempest_progress_state(streamed)$current_stage, "research")
  expect_length(app$storm_merge_progress_events(list(event), streamed), 1L)
})

test_that("STORM progress stream skips malformed lines", {
  app <- source_shiny_modules()
  stream_path <- withr::local_tempfile(fileext = ".ndjson")
  good <- jsonlite::toJSON(
    list(event_id = "ok", stage = "research"),
    auto_unbox = TRUE
  )
  writeLines(c(good, "{not valid json", ""), stream_path)

  streamed <- app$storm_read_progress_stream(stream_path)

  expect_length(streamed, 1L)
  expect_equal(streamed[[1]]$event_id, "ok")
})

test_that("STORM progress stream reads only newly appended lines", {
  app <- source_shiny_modules()
  stream_path <- withr::local_tempfile(fileext = ".ndjson")
  cursor <- app$storm_progress_stream_cursor()
  first <- jsonlite::toJSON(list(event_id = "one"), auto_unbox = TRUE)
  cat(first, "\n", file = stream_path, sep = "")

  initial <- app$storm_read_progress_stream_incremental(stream_path, cursor)
  expect_length(initial, 1L)
  expect_equal(initial[[1]]$event_id, "one")
  expect_length(
    app$storm_read_progress_stream_incremental(stream_path, cursor),
    0L
  )

  second <- jsonlite::toJSON(list(event_id = "two"), auto_unbox = TRUE)
  cat(second, "\n", file = stream_path, append = TRUE, sep = "")
  appended <- app$storm_read_progress_stream_incremental(stream_path, cursor)
  expect_length(appended, 1L)
  expect_equal(appended[[1]]$event_id, "two")
})

test_that("STORM progress stream defers a partially-written line", {
  app <- source_shiny_modules()
  stream_path <- withr::local_tempfile(fileext = ".ndjson")
  cursor <- app$storm_progress_stream_cursor()
  cat('{"event_id":"partial"', file = stream_path, sep = "")

  expect_length(
    app$storm_read_progress_stream_incremental(stream_path, cursor),
    0L
  )

  cat(',"stage":"research"}\n', file = stream_path, append = TRUE, sep = "")
  completed <- app$storm_read_progress_stream_incremental(stream_path, cursor)
  expect_length(completed, 1L)
  expect_equal(completed[[1]]$event_id, "partial")
})

test_that("STORM progress stream renders while a task is still active", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("later")
  app <- source_shiny_modules()
  stream_path <- withr::local_tempfile(fileext = ".ndjson")

  progress_server <- function(id, app, stream_path) {
    shiny::moduleServer(id, function(input, output, session) {
      active <- TRUE
      progress_events <- shiny::reactiveVal(list(
        app$storm_running_event("Topic", "worker-run")
      ))
      app$storm_poll_progress_stream(
        path = stream_path,
        progress_events = progress_events,
        session = session,
        is_current = function() active,
        interval = 1
      )
      stop_polling <- function() {
        active <<- FALSE
      }
    })
  }

  shiny::testServer(
    progress_server,
    args = list(app = app, stream_path = stream_path),
    {
      collector <- app$storm_worker_progress_collector(
        include_payload = TRUE,
        stream_path = stream_path
      )
      collector$record(tempest_progress_event(
        event_id = "stream-ui-research-start",
        run_id = "worker-run",
        workflow = "storm",
        event_type = "stage",
        status = "started",
        stage = "research"
      ))

      later::run_now(0.05)
      session$flushReact()

      state <- tempest_progress_state(progress_events())
      html <- paste(
        as.character(app$workflow_progress_ui(state, app$storm_stage_labels())),
        collapse = ""
      )
      stop_polling()

      expect_equal(state$current_stage, "research")
      expect_match(html, "STORM")
      expect_match(html, "Research")
      expect_match(html, "fa-spinner")
    }
  )
})

test_that("STORM worker omits progress for older tempest_run signatures", {
  app <- source_shiny_modules()
  calls <- list()
  old_run <- function(
    topic,
    config,
    n_experts,
    research_strategy,
    max_rounds,
    parallel_research,
    verbose
  ) {
    calls[[length(calls) + 1L]] <<- as.list(match.call())[-1]
    list(report_md = "old run")
  }

  value <- app$storm_run_with_progress(
    topic = "Topic",
    cfg = tempest_config(),
    n_experts = 1,
    strategy = "key_questions",
    max_rounds = 1,
    parallel = FALSE,
    tempest_run = old_run
  )

  expect_equal(value$result$report_md, "old run")
  expect_equal(intersect("progress", names(calls[[1]])), character())
  expect_length(value$progress, 0L)

  seen_run_id <- NULL
  new_run <- function(
    topic,
    config,
    n_experts,
    research_strategy,
    max_rounds,
    parallel_research,
    run_id,
    progress,
    verbose
  ) {
    seen_run_id <<- run_id
    progress(tempest_progress_event(
      run_id = run_id,
      workflow = "storm",
      event_type = "stage",
      status = "started",
      stage = "research"
    ))
    list(report_md = "new run")
  }
  value <- app$storm_run_with_progress(
    topic = "Topic",
    cfg = tempest_config(),
    n_experts = 1,
    strategy = "key_questions",
    max_rounds = 1,
    parallel = FALSE,
    progress_run_id = "shiny-run",
    tempest_run = new_run
  )

  expect_equal(value$result$report_md, "new run")
  expect_equal(seen_run_id, "shiny-run")
  expect_equal(value$progress[[1]]$run_id, "shiny-run")
  expect_equal(value$progress[[1]]$stage, "research")
})

test_that("STORM worker streams progress before mirai resolves", {
  skip_if_not_installed("mirai")
  app <- source_shiny_modules()
  stream_path <- withr::local_tempfile(fileext = ".ndjson")
  release_path <- withr::local_tempfile(fileext = ".release")
  unlink(release_path)
  event <- tempest_progress_event(
    event_id = "mirai-research-start",
    run_id = "worker-run",
    workflow = "storm",
    event_type = "stage",
    status = "started",
    stage = "research"
  )

  value <- mirai::mirai(
    {
      fake_run <- function(
        topic,
        config,
        n_experts,
        research_strategy,
        max_rounds,
        parallel_research,
        progress,
        verbose
      ) {
        progress(event)
        deadline <- Sys.time() + 10
        while (!file.exists(release_path) && Sys.time() < deadline) {
          Sys.sleep(0.02)
        }
        list(report_md = paste("worker run", topic))
      }

      storm_runner(
        topic = topic,
        cfg = cfg,
        n_experts = n_experts,
        strategy = strategy,
        max_rounds = max_rounds,
        parallel = parallel,
        progress_stream_path = progress_stream_path,
        progress_collector = progress_collector,
        tempest_run = fake_run
      )
    },
    topic = "Topic",
    cfg = list(),
    n_experts = 1,
    strategy = "key_questions",
    max_rounds = 1,
    parallel = FALSE,
    progress_stream_path = stream_path,
    progress_collector = app$storm_worker_progress_collector,
    storm_runner = app$storm_run_with_progress,
    event = event,
    release_path = release_path
  )

  seen_stream <- FALSE
  deadline <- Sys.time() + 10
  while (
    mirai::unresolved(value) &&
      !seen_stream &&
      Sys.time() < deadline
  ) {
    seen_stream <- length(app$storm_read_progress_stream(stream_path)) > 0L
    Sys.sleep(0.02)
  }

  expect_equal(seen_stream, TRUE)
  expect_equal(mirai::unresolved(value), TRUE)
  invisible(file.create(release_path))
  result <- value[]
  expect_equal(result$result$report_md, "worker run Topic")
  expect_equal(result$progress[[1]]$stage, "research")
})

test_that("STORM worker adapter tolerates older tempest_run signatures in mirai", {
  skip_if_not_installed("mirai")
  app <- source_shiny_modules()
  old_run <- function(
    topic,
    config,
    n_experts,
    research_strategy,
    max_rounds,
    parallel_research,
    verbose
  ) {
    list(report_md = paste("worker run", topic))
  }

  value <- mirai::mirai(
    {
      storm_runner(
        topic = topic,
        cfg = cfg,
        n_experts = n_experts,
        strategy = strategy,
        max_rounds = max_rounds,
        parallel = parallel,
        progress_collector = progress_collector,
        tempest_run = old_run
      )
    },
    topic = "Topic",
    cfg = list(),
    n_experts = 1,
    strategy = "key_questions",
    max_rounds = 1,
    parallel = FALSE,
    progress_collector = app$storm_worker_progress_collector,
    storm_runner = app$storm_run_with_progress,
    old_run = old_run
  )[]

  expect_equal(value$result$report_md, "worker run Topic")
  expect_length(value$progress, 0L)
})

test_that("workflow_progress_ui hides recorded failures once succeeded", {
  skip_if_not_installed("shiny")
  app <- source_shiny_modules()
  state <- tempest_progress_state(list(
    tempest_progress_event(
      run_id = "run-1",
      workflow = "storm",
      event_type = "tool",
      status = "failed",
      stage = "research",
      step = "web_search",
      payload = list(error_message = "transient tool error")
    ),
    tempest_progress_event(
      run_id = "run-1",
      workflow = "storm",
      event_type = "workflow",
      status = "succeeded"
    )
  ))

  expect_equal(state$status, "succeeded")
  expect_length(state$failures, 1L)
  html <- paste(
    as.character(app$workflow_progress_ui(state, app$storm_stage_labels())),
    collapse = ""
  )
  expect_no_match(html, "transient tool error")
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

test_that("run_warmup exposes running progress before expert answers resolve", {
  skip_if_not_installed("later")
  skip_if_not_installed("promises")
  app <- source_shiny_modules()
  collector <- tempest_progress_collector(include_payload = TRUE)
  store <- fake_warmup_store()
  resolve_chat <- NULL
  ses <- fake_warmup_session(
    function(prompt) {
      promises::promise(function(resolve, reject) {
        resolve_chat <<- resolve
      })
    },
    progress = collector$record
  )

  promise <- app$run_warmup(
    ses,
    store,
    function(x) x,
    timeout_s = 1,
    max_questions_per_expert = 1
  )
  deadline <- Sys.time() + 1
  while (is.null(resolve_chat) && Sys.time() < deadline) {
    later::run_now(0.01)
  }

  expect_equal(is.null(resolve_chat), FALSE)
  running <- tempest_progress_state(collector$events())
  expect_equal(running$status, "running")
  expect_equal(running$current_stage, "warmup")
  expect_length(running$active$tools, 1L)
  html <- paste(
    as.character(app$workflow_progress_ui(running, app$costorm_stage_labels())),
    collapse = ""
  )
  expect_match(html, "Warmup")
  expect_match(html, "fa-spinner")

  resolve_chat("Warmup answer [S123456789abc].")
  await_promise(promise)
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
