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
  async_prompt <- NULL
  artifacts <- new.env(parent = emptyenv())
  artifact_catalog <- tempest_artifact_catalog()
  cfg <- tempest_config()
  session <- list(
    topic = "Async report",
    title = "Async report",
    config = cfg,
    store = store,
    mindmap = tempest:::tempest_mindmap_init("Async report"),
    transcript = list(),
    artifacts = artifacts,
    artifact_catalog = artifact_catalog,
    transcript_markdown = function(max_turns = 80) "Conversation",
    chats = list(
      reporter = list(
        chat_async = function(prompt, ...) {
          async_prompt <<- prompt
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
  expect_identical(artifact_catalog$has("report_md"), FALSE)
  resolve_report(paste0("Report claim [", source_id, "]."))
  settled <- await_tempest_promise(request)

  expect_null(settled$error)
  expect_equal(
    async_prompt,
    tempest:::tempest_costorm_report_prompt(session, "technical")
  )
  expect_match(artifacts[["report_md"]], "# Async report", fixed = TRUE)
  expect_match(
    artifacts[["report_md"]],
    paste0("[^", source_id, "]"),
    fixed = TRUE
  )
  expect_equal(
    artifact_catalog$get("report_md")@content,
    artifacts[["report_md"]]
  )
})

test_that("async report finalization failures emit failed progress", {
  skip_if_not_installed("promises")
  skip_if_not_installed("later")
  resolve_report <- NULL
  events <- list()
  artifacts <- new.env(parent = emptyenv())
  artifact_catalog <- tempest_artifact_catalog()
  cfg <- tempest_config()
  session <- list(
    session_id = "failed-report",
    topic = "Failed report",
    title = "Failed report",
    config = cfg,
    store = SourceStore$new(),
    mindmap = tempest:::tempest_mindmap_init("Failed report"),
    artifacts = artifacts,
    artifact_catalog = artifact_catalog,
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
      event <- tempest_progress_event(
        run_id = "failed-report",
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
      events <<- c(events, list(event))
      event
    }
  )
  report_spec <- tempest:::tempest_costorm_report_spec(session)
  existing <- tempest_artifact(
    report_spec,
    content = "# Existing report",
    artifact_id = "report_md",
    producer_operation_id = "tempest.renderer.markdown_report",
    run_id = "another-run",
    step_id = "report",
    status = "valid"
  )
  artifact_catalog$register(report_spec)
  artifact_catalog$add(existing)

  request <- tempest:::tempest_session_report_async(session)
  resolve_report("Replacement report body")
  settled <- await_tempest_promise(request)

  expect_s3_class(settled$error, "tempest_deliverable_execution_error")
  expect_identical(artifact_catalog$get("report_md"), existing)
  expect_null(artifacts[["report"]])
  expect_null(artifacts[["report_md"]])
  failed <- Filter(
    function(event) {
      identical(event@stage, "report") &&
        identical(event@status, "failed")
    },
    events
  )
  expect_length(failed, 1L)
  succeeded <- Filter(
    function(event) {
      identical(event@stage, "report") &&
        identical(event@status, "succeeded")
    },
    events
  )
  expect_length(succeeded, 0L)
  available <- Filter(
    function(event) {
      identical(event@event_type, "artifact") &&
        identical(event@status, "available")
    },
    events
  )
  expect_length(available, 0L)
})

test_that("stale async reports do not publish artifacts", {
  skip_if_not_installed("promises")
  skip_if_not_installed("later")
  resolve_report <- NULL
  current <- TRUE
  events <- list()
  artifacts <- new.env(parent = emptyenv())
  artifact_catalog <- tempest_artifact_catalog()
  cfg <- tempest_config()
  session <- list(
    session_id = "stale-report",
    topic = "Stale report",
    title = "Stale report",
    config = cfg,
    store = SourceStore$new(),
    mindmap = tempest:::tempest_mindmap_init("Stale report"),
    artifacts = artifacts,
    artifact_catalog = artifact_catalog,
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
      event <- tempest_progress_event(
        run_id = "stale-report",
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
      events <<- c(events, list(event))
      event
    }
  )

  request <- tempest:::tempest_session_report_async(
    session,
    is_current = function() current
  )
  current <- FALSE
  resolve_report("Stale body")
  settled <- await_tempest_promise(request)

  expect_null(settled$error)
  expect_null(artifacts[["report"]])
  expect_null(artifacts[["report_md"]])
  expect_identical(artifact_catalog$has("report_md"), FALSE)
  expect_contains(
    vapply(events, function(event) event@status, character(1)),
    "cancelled"
  )
  expect_false(any(
    vapply(
      events,
      function(event) {
        identical(event@event_type, "artifact") &&
          identical(event@status, "available")
      },
      logical(1)
    )
  ))
})
