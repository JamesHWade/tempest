test_that("Run review UI is namespaced and accessible", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  env <- tempest:::tempest_shiny_module_env()

  html <- paste(as.character(env$mod_run_review_ui("review")), collapse = "")

  expect_match(html, "Run review", fixed = TRUE)
  expect_match(html, "review-product_source", fixed = TRUE)
  expect_match(html, "review-stage_filter", fixed = TRUE)
  expect_match(html, "review-status_filter", fixed = TRUE)
  expect_match(html, "review-attention_only", fixed = TRUE)
  expect_match(html, "review-stage_detail", fixed = TRUE)
  expect_match(html, 'role="status"', fixed = TRUE)
  expect_match(html, 'aria-live="polite"', fixed = TRUE)
  expect_match(html, 'aria-atomic="true"', fixed = TRUE)
  expect_match(html, "Authoritative stage records", fixed = TRUE)
  expect_match(html, "Unlinked untimed Deputy references", fixed = TRUE)
  expect_match(html, "Live progress observations (untrusted)", fixed = TRUE)
})

test_that("Run review renders escaped fields and exact authority joins", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  env <- tempest:::tempest_shiny_module_env()
  secret <- "Authorization: Bearer provider-secret"
  stage <- test_run_review_stage(
    stage = "outline<img src=x onerror=alert(1)>",
    trace_id = "cross-type-collision"
  )
  stage$failure_message <- secret
  stage$program_artifact_id <- secret
  review <- test_run_review_value(
    stages = list(stage),
    agents = list(
      test_run_review_agent(trace_id = "different-trace"),
      test_run_review_agent(2L, trace_id = "cross-type-collision")
    )
  )
  product <- shiny::reactiveVal(list(
    state = "completed",
    review = review,
    prompt = secret,
    source_content = secret
  ))
  events <- shiny::reactiveVal(list())

  shiny::testServer(
    env$mod_run_review_server,
    args = list(
      costorm_product = shiny::reactive(NULL),
      storm_product = product,
      storm_events = events,
      review_builder = function(value) value$review
    ),
    {
      session$setInputs(
        product_source = "storm",
        stage_filter = "",
        status_filter = "",
        attention_only = FALSE,
        stage_detail = "attempt-1"
      )
      session$flushReact()

      summary_html <- paste(as.character(output$summary$html), collapse = "")
      stage_html <- paste(as.character(output$stage_table$html), collapse = "")
      detail_html <- paste(
        as.character(output$stage_detail_body$html),
        collapse = ""
      )
      unlinked_html <- paste(
        as.character(output$unlinked_agents$html),
        collapse = ""
      )

      expect_match(summary_html, "run-safe", fixed = TRUE)
      expect_match(summary_html, "sha256:review-safe", fixed = TRUE)
      expect_match(summary_html, "sha256:config-safe", fixed = TRUE)
      expect_match(summary_html, "sha256:report-safe", fixed = TRUE)
      expect_match(
        stage_html,
        "Authoritative StageRecord trajectory",
        fixed = TRUE
      )
      expect_match(stage_html, "Needs attention", fixed = TRUE)
      expect_no_match(stage_html, "<img", fixed = TRUE)
      expect_match(stage_html, "&lt;img", fixed = TRUE)
      expect_match(detail_html, "deputy-run-1", fixed = TRUE)
      expect_match(
        detail_html,
        "not placed in chronological order",
        fixed = TRUE
      )
      expect_no_match(detail_html, "deputy-run-2", fixed = TRUE)
      expect_match(unlinked_html, "deputy-run-2", fixed = TRUE)
      expect_no_match(
        paste(summary_html, stage_html, detail_html, unlinked_html),
        secret,
        fixed = TRUE
      )

      session$setInputs(
        stage_filter = stage$stage,
        status_filter = "succeeded",
        attention_only = TRUE
      )
      session$flushReact()
      filtered_html <- paste(
        as.character(output$stage_table$html),
        collapse = ""
      )
      expect_match(filtered_html, "attempt-1", fixed = TRUE)
      session$setInputs(status_filter = "failed")
      session$flushReact()
      empty_filter_html <- paste(
        as.character(output$stage_table$html),
        collapse = ""
      )
      expect_match(
        empty_filter_html,
        "No StageRecord rows match",
        fixed = TRUE
      )

      session$flushReact()
      aws_secret <- "AKIAIOSFODNN7EXAMPLE"
      github_secret <- "ghp_0123456789abcdefghijklmnopqrstuvwxyz"
      relative_path <- "private/key.pem"
      events(list(
        list(
          event_id = "event-failed",
          run_id = "run-safe",
          workflow = "storm",
          event_type = "stage",
          stage = aws_secret,
          step = relative_path,
          status = "failed",
          timestamp = "2026-08-19T12:02:00Z",
          message = secret,
          payload = list(prompt = secret, path = relative_path)
        ),
        list(
          event_id = "event-running",
          run_id = "run-safe",
          workflow = "storm",
          event_type = github_secret,
          stage = "outline",
          step = "report_md",
          status = "running",
          timestamp = "2026-08-19T12:03:00Z"
        )
      ))
      session$flushReact()
      progress_html <- paste(
        as.character(output$live_progress$html),
        collapse = ""
      )
      alert_html <- paste(
        as.character(output$new_failure_alert$html),
        collapse = ""
      )

      expect_match(progress_html, "untrusted observation", fixed = TRUE)
      expect_match(progress_html, "Same run id", fixed = TRUE)
      expect_match(progress_html, "Redacted", fixed = TRUE)
      expect_no_match(progress_html, secret, fixed = TRUE)
      expect_no_match(progress_html, aws_secret, fixed = TRUE)
      expect_no_match(progress_html, github_secret, fixed = TRUE)
      expect_no_match(progress_html, relative_path, fixed = TRUE)
      expect_match(alert_html, 'role="alert"', fixed = TRUE)
      expect_match(alert_html, "newly observed", fixed = TRUE)
      expect_no_match(alert_html, secret, fixed = TRUE)
    }
  )
})

test_that("Run review fails closed on incomplete or ambiguous Deputy joins", {
  env <- tempest:::tempest_shiny_module_env()
  stage <- test_run_review_stage(trace_id = "cross-type-collision")
  agents <- list(
    test_run_review_agent(trace_id = "different-trace"),
    test_run_review_agent(2L, trace_id = "cross-type-collision")
  )

  expect_identical(
    env$run_review_authoritative_agent_indices(
      stage,
      list(test_run_review_agent_join()),
      agents
    ),
    1L
  )
  expect_identical(
    env$run_review_authoritative_agent_indices(stage, list(), agents),
    integer()
  )
  expect_identical(
    env$run_review_authoritative_agent_indices(
      stage,
      list(test_run_review_agent_join(
        matched_fields = "deputy_run_id"
      )),
      agents
    ),
    integer()
  )

  ambiguous <- agents
  ambiguous[[2L]]$deputy_run_id <- ambiguous[[1L]]$deputy_run_id
  expect_identical(
    env$run_review_authoritative_agent_indices(
      stage,
      list(test_run_review_agent_join()),
      ambiguous
    ),
    integer()
  )
})

test_that("Run review consumes the real bounded trajectory projection", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  env <- tempest:::tempest_shiny_module_env()
  fixture <- test_promotion_fixture("storm")
  review <- tempest:::tempest_trajectory_review(fixture$research)

  shiny::testServer(
    env$mod_run_review_server,
    args = list(
      costorm_product = shiny::reactive(NULL),
      storm_product = shiny::reactive(fixture$research)
    ),
    {
      session$setInputs(
        product_source = "storm",
        stage_filter = "",
        status_filter = "",
        attention_only = FALSE
      )
      session$flushReact()

      summary_html <- paste(as.character(output$summary$html), collapse = "")
      stage_html <- paste(as.character(output$stage_table$html), collapse = "")

      expect_match(summary_html, review@review_id, fixed = TRUE)
      expect_match(
        summary_html,
        review@product$research_run_id,
        fixed = TRUE
      )
      expect_match(
        stage_html,
        review@stages$items[[1L]]$attempt_id,
        fixed = TRUE
      )
    }
  )
})

test_that("Run review rejects non-completed product transitions safely", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  env <- tempest:::tempest_shiny_module_env()
  product <- shiny::reactiveVal(NULL)
  secret <- "sk-live-should-not-render"

  shiny::testServer(
    env$mod_run_review_server,
    args = list(
      costorm_product = shiny::reactive(NULL),
      storm_product = product,
      review_builder = function(value) {
        if (!identical(value$state, "completed")) {
          stop(secret)
        }
        value$review
      }
    ),
    {
      session$flushReact()
      blank_html <- paste(as.character(output$summary$html), collapse = "")
      expect_match(blank_html, "Complete a STORM run", fixed = TRUE)

      for (state in c("running", "failed", "cancelled", "tampered", "stale")) {
        product(list(state = state))
        session$setInputs(product_source = "storm")
        session$flushReact()
        unavailable_html <- paste(
          as.character(output$summary$html),
          collapse = ""
        )
        expect_match(
          unavailable_html,
          "No authoritative review is available",
          fixed = TRUE
        )
        expect_no_match(unavailable_html, secret, fixed = TRUE)
      }

      product(list(state = "completed", review = test_run_review_value()))
      session$setInputs(product_source = "storm")
      session$flushReact()
      completed_html <- paste(
        as.character(output$summary$html),
        collapse = ""
      )
      expect_match(completed_html, "sha256:review-safe", fixed = TRUE)
    }
  )
})

test_that("Run review enforces exact visible row caps and omitted counts", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  env <- tempest:::tempest_shiny_module_env()
  stages <- lapply(seq_len(300L), function(index) {
    test_run_review_stage(
      index,
      stage = paste0("stage_", index),
      trace_id = paste0("trace-", index)
    )
  })
  agents <- lapply(seq_len(250L), test_run_review_agent)
  review <- test_run_review_value(
    stages = stages,
    agents = agents,
    joins = list()
  )
  review$stages$total <- 300L
  review$stages$retained <- 300L
  review$stages$omitted <- 0L
  review$agent_runs$total <- 300L
  review$agent_runs$omitted <- 50L
  product <- shiny::reactiveVal(list(review = review))
  events <- shiny::reactiveVal(lapply(seq_len(275L), function(index) {
    list(
      event_id = paste0("event-", index),
      run_id = "run-safe",
      workflow = "storm",
      event_type = "stage",
      stage = if (index <= 25L) "AKIAIOSFODNN7EXAMPLE" else "outline",
      step = if (index <= 25L) "private/key.pem" else "report_md",
      status = "running",
      timestamp = "2026-08-19T12:02:00Z"
    )
  }))

  shiny::testServer(
    env$mod_run_review_server,
    args = list(
      costorm_product = shiny::reactive(NULL),
      storm_product = product,
      storm_events = events,
      review_builder = function(value) value$review
    ),
    {
      session$setInputs(
        product_source = "storm",
        stage_filter = "",
        status_filter = "",
        attention_only = FALSE
      )
      session$flushReact()

      summary_html <- paste(as.character(output$summary$html), collapse = "")
      stage_html <- paste(as.character(output$stage_table$html), collapse = "")
      progress_html <- paste(
        as.character(output$live_progress$html),
        collapse = ""
      )
      unlinked_html <- paste(
        as.character(output$unlinked_agents$html),
        collapse = ""
      )

      expect_match(
        summary_html,
        "250 authoritative stages retained; 50 omitted",
        fixed = TRUE
      )
      expect_match(
        stage_html,
        "Showing 250 filtered rows from 250 retained records; 50 complete-projection rows omitted",
        fixed = TRUE
      )
      expect_match(
        progress_html,
        "Showing 250 of 275 live rows; 25 omitted",
        fixed = TRUE
      )
      expect_no_match(
        progress_html,
        "AKIAIOSFODNN7EXAMPLE",
        fixed = TRUE
      )
      expect_no_match(progress_html, "private/key.pem", fixed = TRUE)
      expect_match(
        unlinked_html,
        paste0(
          "Showing 250 rows in this section from 250 retained Deputy ",
          "references; 50 complete-projection rows omitted"
        ),
        fixed = TRUE
      )
      expect_lte(length(shiny::isolate(observed_event_ids())), 250L)
    }
  )
})

test_that("STORM keeps the last valid product across later run outcomes", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("later")
  skip_if_not_installed("mirai")
  local_mirai_coverage_dir()
  env <- tempest:::tempest_shiny_module_env()
  accepted <- character()
  store <- list(
    publish_storm_report = function(result, config) {
      if (!isTRUE(result$publishable)) {
        stop("provider credential must stay private")
      }
      accepted <<- c(accepted, result$report_md)
      result$report_md
    }
  )
  original_poll <- env$storm_poll_progress_stream
  original_run_id <- env$storm_result_run_id
  original_runner <- env$storm_run_with_progress
  withr::defer({
    env$storm_poll_progress_stream <- original_poll
    env$storm_result_run_id <- original_run_id
    env$storm_run_with_progress <- original_runner
  })
  env$storm_poll_progress_stream <- function(...) invisible(NULL)
  env$storm_result_run_id <- function(result) result$run_id
  env$storm_run_with_progress <- function(
    topic,
    cfg,
    progress_run_id,
    ...
  ) {
    if (identical(topic, "Running")) {
      Sys.sleep(5)
    }
    list(
      result = list(
        run_id = if (identical(topic, "Stale")) {
          "stale-run"
        } else {
          progress_run_id
        },
        report_md = topic,
        publishable = !identical(topic, "Tampered")
      ),
      progress = list()
    )
  }

  shiny::testServer(
    env$mod_storm_server,
    args = list(
      config = shiny::reactive(tempest_config()),
      store = store
    ),
    {
      wait_for_terminal <- function() {
        await_tempest_extended_task(storm_task, session)
      }

      expect_null(shiny::isolate(session$returned$last_successful_product()))

      session$setInputs(topic = "Accepted", run = 1)
      session$flushReact()
      expect_identical(wait_for_terminal(), "success")
      expect_identical(
        shiny::isolate(session$returned$last_successful_product())$report_md,
        "Accepted"
      )

      session$setInputs(topic = "Tampered", run = 2)
      session$flushReact()
      expect_identical(wait_for_terminal(), "success")
      expect_identical(
        shiny::isolate(session$returned$last_successful_product())$report_md,
        "Accepted"
      )

      session$setInputs(topic = "Stale", run = 3)
      session$flushReact()
      expect_identical(wait_for_terminal(), "success")
      expect_identical(
        shiny::isolate(session$returned$last_successful_product())$report_md,
        "Accepted"
      )

      session$setInputs(topic = "Running", run = 4)
      session$flushReact()
      expect_identical(shiny::isolate(storm_task$status()), "running")
      expect_identical(
        shiny::isolate(session$returned$last_successful_product())$report_md,
        "Accepted"
      )
      cancelled_job <- worker_state$job
      session$setInputs(cancel = 1)
      session$flushReact()
      deadline <- Sys.time() + 2
      task_status <- shiny::isolate(storm_task$status())
      while (
        (mirai::unresolved(cancelled_job) ||
          identical(task_status, "running")) &&
          Sys.time() < deadline
      ) {
        later::run_now(0.05)
        session$flushReact()
        task_status <- shiny::isolate(storm_task$status())
      }
      expect_identical(mirai::unresolved(cancelled_job), FALSE)
      expect_identical(task_status, "success")
      expect_identical(
        shiny::isolate(session$returned$last_successful_product())$report_md,
        "Accepted"
      )
      expect_identical(accepted, "Accepted")
    }
  )
})
