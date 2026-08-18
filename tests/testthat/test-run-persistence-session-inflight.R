test_that("pending Deputy runs block schema 9 persistence", {
  skip_if_not_installed("coro")
  skip_if_not_installed("deputy")
  skip_if_not_installed("ellmer")
  skip_if_not_installed("jsonlite")
  skip_if_not_installed("later")
  skip_if_not_installed("promises")

  raw_chat <- fake_chat()
  raw_chat$stream_async <- function(
    prompt = NULL,
    stream = c("text", "content"),
    controller = NULL
  ) {
    coro::async_generator(function() {
      chunk <- coro::await(promises::promise(function(resolve, reject) {
        later::later(
          function() {
            resolve(ellmer::ContentText("Late expert answer"))
          },
          0.25
        )
      }))
      coro::yield(chunk)
      coro::exhausted()
    })()
  }
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) {
      if (identical(role, "expert")) {
        return(raw_chat)
      }
      fake_chat()
    }
  )
  expert <- test_expert(
    expert_id = "expert.pending-timeout",
    name = "Pending Timeout Expert",
    initial_questions = "What remains pending?"
  )
  session <- tempest_session(
    "Pending Deputy persistence",
    config = cfg,
    experts = list(expert),
    session_id = "pending-deputy-persistence"
  )

  settled <- await_tempest_promise(tempest_session_warmup_async(
    session,
    timeout_s = 0.02,
    max_parallel_experts = 1L
  ))
  expect_null(settled$error)
  expect_identical(
    settled$value@orientations[[1L]]$failure_kind,
    "timeout"
  )
  expect_length(tempest:::tempest_session_deputy_traces(session), 0L)
  expect_length(
    tempest:::tempest_session_pending_deputy_runs(session),
    1L
  )
  expect_error(
    tempest_session_snapshot(session),
    class = "tempest_session_snapshot_error"
  )
  immediate_dir <- file.path(withr::local_tempdir(), "pending")
  expect_error(
    tempest_session_save(session, immediate_dir),
    class = "tempest_session_save_error"
  )
  expect_identical(dir.exists(immediate_dir), FALSE)

  deadline <- Sys.time() + 2
  while (
    length(tempest:::tempest_session_pending_deputy_runs(session)) > 0L &&
      Sys.time() < deadline
  ) {
    later::run_now(0.05)
  }
  traces <- tempest:::tempest_session_deputy_traces(session)
  expect_length(
    tempest:::tempest_session_pending_deputy_runs(session),
    0L
  )
  expect_length(traces, 1L)
  expect_identical(traces[[1L]]$status, "interrupted")

  snapshot <- tempest_session_snapshot(session)
  expect_identical("pending_deputy_runs" %in% names(snapshot), FALSE)
  expect_identical(
    Filter(
      \(trace) identical(trace$trace_type, "deputy_run"),
      snapshot$research_manifest$traces
    ),
    traces
  )
  bundle_dir <- file.path(withr::local_tempdir(), "settled")
  tempest_session_save(session, bundle_dir)
  resumed <- tempest_session_resume(bundle_dir, config = cfg)
  expect_identical(
    tempest:::tempest_session_deputy_traces(resumed),
    traces
  )
  expect_length(
    tempest:::tempest_session_pending_deputy_runs(resumed),
    0L
  )
})

test_that("issued and processing completions block schema 9 persistence", {
  skip_if_not_installed("deputy")
  skip_if_not_installed("ellmer")
  skip_if_not_installed("jsonlite")

  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  session <- tempest_session(
    "Agent completion persistence",
    config = cfg,
    experts = list(test_expert(
      expert_id = "expert.completion-persistence"
    )),
    session_id = "completion-persistence"
  )
  context <- tempest:::tempest_session_agent_completion_context(session)
  issue <- function(suffix) {
    completion_id <- tempest:::tempest_agent_completion_new_id(
      context$registry
    )
    tempest:::tempest_agent_completion_issue(
      context$registry,
      completion_id,
      paste0("private prompt ", suffix),
      paste0("private response ", suffix),
      ellmer::AssistantTurn(
        list(ellmer::ContentText(paste0("private response ", suffix))),
        tokens = c(1, 1, 0),
        cost = 0
      ),
      test_costorm_deputy_trace(
        run_id = paste0("deputy-run-persistence-", suffix),
        session_id = paste0("deputy-session-persistence-", suffix),
        correlation_id = paste0("completion-persistence-", suffix)
      )
    )
    completion_id
  }

  issued_id <- issue("issued")
  issued_dir <- file.path(withr::local_tempdir(), "issued")
  expect_error(
    tempest_session_snapshot(session),
    class = "tempest_session_snapshot_error"
  )
  expect_error(
    tempest_session_save(session, issued_dir),
    class = "tempest_session_save_error"
  )
  expect_identical(dir.exists(issued_dir), FALSE)

  tempest:::tempest_session_agent_completion_cancel(session, issued_id)
  expect_no_error(tempest_session_snapshot(session))

  processing_id <- issue("processing")
  claim <- tempest:::tempest_session_agent_completion_claim(
    session,
    processing_id
  )
  processing_dir <- file.path(withr::local_tempdir(), "processing")
  expect_error(
    tempest_session_snapshot(session),
    class = "tempest_session_snapshot_error"
  )
  expect_error(
    tempest_session_save(session, processing_dir),
    class = "tempest_session_save_error"
  )
  expect_identical(dir.exists(processing_dir), FALSE)

  tempest:::tempest_session_agent_completion_consume(session, claim)
  snapshot <- tempest_session_snapshot(session)
  expect_identical(
    any(grepl("completion", names(snapshot), fixed = TRUE)),
    FALSE
  )
  bundle_dir <- file.path(withr::local_tempdir(), "quiescent")
  tempest_session_save(session, bundle_dir)
  bundle_text <- paste(
    vapply(
      list.files(bundle_dir, recursive = TRUE, full.names = TRUE),
      \(path) paste(readLines(path, warn = FALSE), collapse = "\n"),
      character(1)
    ),
    collapse = "\n"
  )
  expect_no_match(bundle_text, "private prompt", fixed = TRUE)
  expect_no_match(bundle_text, "private response", fixed = TRUE)
  expect_no_match(bundle_text, issued_id, fixed = TRUE)
  expect_no_match(bundle_text, processing_id, fixed = TRUE)

  resumed <- tempest_session_resume(bundle_dir, config = cfg)
  restored_context <- tempest:::tempest_session_agent_completion_context(
    resumed
  )
  expect_length(
    ls(restored_context$registry$entries, all.names = TRUE),
    0L
  )
})
