test_that("async fact extraction keeps the event loop responsive", {
  skip_if_not_installed("promises")
  skip_if_not_installed("later")
  skip_if_not_installed("ellmer")
  store <- fake_store_with_sources(1)
  source_id <- store$list_sources()[[1]]$id
  resolve_request <- NULL
  heartbeat <- FALSE
  extractor <- list(
    chat_structured_async = function(...) {
      promises::promise(function(resolve, reject) {
        resolve_request <<- resolve
      })
    }
  )
  session <- list(
    session_id = "async-session",
    store = store,
    chats = list(extractor = extractor),
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
      tempest_progress_event(
        run_id = "async-session",
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
    }
  )

  request <- tempest:::tempest_session_extract_facts_async(
    session,
    paste0("Responsive claim [", source_id, "]."),
    source_ids = source_id
  )
  later::later(function() heartbeat <<- TRUE, delay = 0)
  later::run_now(0.02)

  expect_equal(heartbeat, TRUE)
  expect_equal(is.null(resolve_request), FALSE)
  expect_length(store$list_claims(), 0L)
  resolve_request(list(
    facts = list(list(
      claim = "Responsive claim",
      sources = list(list(source_id = source_id)),
      confidence = "high",
      support_score = 0.9
    ))
  ))
  settled <- await_tempest_promise(request)

  expect_null(settled$error)
  expect_equal(store$list_claims()[[1]]@claim_text, "Responsive claim")
})

test_that("stale async fact extraction cannot commit", {
  skip_if_not_installed("promises")
  skip_if_not_installed("later")
  skip_if_not_installed("ellmer")
  store <- fake_store_with_sources(1)
  source_id <- store$list_sources()[[1]]$id
  resolve_request <- NULL
  current <- TRUE
  session <- list(
    session_id = "stale-session",
    store = store,
    chats = list(
      extractor = list(
        chat_structured_async = function(...) {
          promises::promise(function(resolve, reject) {
            resolve_request <<- resolve
          })
        }
      )
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
      tempest_progress_event(
        run_id = "stale-session",
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
    }
  )
  request <- tempest:::tempest_session_extract_facts_async(
    session,
    paste0("Stale claim [", source_id, "]."),
    source_ids = source_id,
    is_current = function() current
  )
  current <- FALSE
  resolve_request(list(
    facts = list(list(
      claim = "Stale claim",
      sources = list(list(source_id = source_id)),
      confidence = "high"
    ))
  ))
  settled <- await_tempest_promise(request)

  expect_null(settled$error)
  expect_length(store$list_claims(), 0L)
})

test_that("async report generation commits only after provider settlement", {
  skip_if_not_installed("promises")
  skip_if_not_installed("later")
  store <- fake_store_with_sources(1)
  source_id <- store$list_sources()[[1]]$id
  store$add_claim(tempest_claim(
    claim_text = "Report claim",
    source_ids = source_id,
    verification_status = "supported",
    support_score = 0.9
  ))
  resolve_report <- NULL
  heartbeat <- FALSE
  artifacts <- new.env(parent = emptyenv())
  cfg <- tempest_config()
  session <- list(
    topic = "Async report",
    title = "Async report",
    config = cfg,
    store = store,
    mindmap = tempest:::tempest_mindmap_init("Async report"),
    transcript = list(),
    artifacts = artifacts,
    transcript_markdown = function(max_turns = 80) "Conversation",
    chats = list(
      reporter = list(
        chat_async = function(...) {
          promises::promise(function(resolve, reject) {
            resolve_report <<- resolve
          })
        }
      )
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
      tempest_progress_event(
        run_id = "report-session",
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
    }
  )

  request <- tempest:::tempest_session_report_async(session)
  later::later(function() heartbeat <<- TRUE, delay = 0)
  later::run_now(0.02)

  expect_equal(heartbeat, TRUE)
  expect_null(artifacts[["report_md"]])
  resolve_report(paste0("Report claim [", source_id, "]."))
  settled <- await_tempest_promise(request)

  expect_null(settled$error)
  expect_match(artifacts[["report_md"]], "# Async report", fixed = TRUE)
  expect_match(
    artifacts[["report_md"]],
    paste0("[^", source_id, "]"),
    fixed = TRUE
  )
})
