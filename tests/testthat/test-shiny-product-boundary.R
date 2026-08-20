test_that("Shiny adapter excludes generic research state", {
  expect_identical("run" %in% names(formals(tempest_shiny_server)), FALSE)
  server_code <- paste(deparse(body(tempest_shiny_server)), collapse = "\n")

  expect_no_match(server_code, "TempestRun", fixed = TRUE)
  expect_no_match(server_code, "tempest_run_", fixed = TRUE)
  expect_no_match(server_code, "set_run", fixed = TRUE)
  expect_no_match(server_code, "report_ready", fixed = TRUE)
})

test_that("STORM publication uses the exact launch configuration", {
  env <- tempest:::tempest_shiny_module_env()
  server_code <- paste(deparse(body(env$mod_storm_server)), collapse = "\n")
  runner_code <- paste(
    deparse(body(env$storm_run_with_progress)),
    collapse = "\n"
  )

  expect_match(
    server_code,
    "worker_state$config <- shiny::isolate(config())",
    fixed = TRUE
  )
  expect_match(server_code, "store$publish_storm_report", fixed = TRUE)
  expect_match(server_code, "config = worker_state$config", fixed = TRUE)
  expect_match(server_code, "report_navigation_event", fixed = TRUE)
  expect_match(
    server_code,
    "last_successful_product(envelope$result)",
    fixed = TRUE
  )
  expect_no_match(server_code, "last_successful_product(NULL)", fixed = TRUE)
  expect_no_match(server_code, "report_ready", fixed = TRUE)
  expect_no_match(runner_code, "parallel_research", fixed = TRUE)
  expect_no_match(runner_code, "tempest_run =", fixed = TRUE)
  expect_match(
    server_code,
    "otel_enabled <- tempest:::tempest_otel_worker_intent()",
    fixed = TRUE
  )
  expect_match(server_code, "otel_enabled = otel_enabled", fixed = TRUE)
  expect_match(
    runner_code,
    "tempest:::tempest_otel_worker_call(",
    fixed = TRUE
  )
  expect_identical(
    tail(names(formals(env$storm_run_with_progress)), 1L),
    "otel_enabled"
  )
  expect_no_match(server_code, "worker_state$otel", fixed = TRUE)
  telemetry_code <- paste(server_code, runner_code, sep = "\n")
  forbidden <- c(
    "tempest_otel_context",
    "tempest_otel_get_tracer",
    "traceparent",
    "tracestate",
    "OTEL_EXPORTER",
    "Sys.getenv",
    "credentials",
    "headers"
  )
  present <- forbidden[vapply(
    forbidden,
    grepl,
    logical(1),
    x = telemetry_code,
    fixed = TRUE
  )]
  expect_identical(present, character())
})

test_that("STORM launch snapshots only validated telemetry intent", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("later")
  skip_if_not_installed("mirai")
  local_mirai_coverage_dir()
  withr::local_options(tempest.otel.enabled = TRUE)
  withr::local_envvar(c(
    OTEL_R_EMIT_SCOPES = "io.github.jameshwade.tempest",
    OTEL_INSTRUMENTATION_GENAI_CAPTURE_MESSAGE_CONTENT = "false"
  ))
  env <- tempest:::tempest_shiny_module_env()
  published <- list()
  store <- list(
    publish_storm_report = function(result, config) {
      published[[length(published) + 1L]] <<- result
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
  env$storm_poll_progress_stream <- function(path, ...) invisible(path)
  env$storm_result_run_id <- function(result) result$run_id
  env$storm_run_with_progress <- function(
    topic,
    progress_run_id,
    otel_enabled,
    ...
  ) {
    list(
      result = list(
        run_id = progress_run_id,
        report_md = topic,
        otel_enabled = otel_enabled,
        otel_type = typeof(otel_enabled)
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
      session$setInputs(topic = "Enabled launch", run = 1)
      session$flushReact()
      expect_identical(
        await_tempest_extended_task(storm_task, session),
        "success"
      )
      expect_identical(published[[1L]]$otel_enabled, TRUE)
      expect_identical(published[[1L]]$otel_type, "logical")
      expect_identical(getOption("tempest.otel.enabled"), TRUE)

      options(tempest.otel.enabled = FALSE)
      session$setInputs(topic = "Disabled launch", run = 2)
      session$flushReact()
      expect_identical(
        await_tempest_extended_task(storm_task, session),
        "success"
      )
      expect_identical(published[[2L]]$otel_enabled, FALSE)
      expect_identical(published[[2L]]$otel_type, "logical")
      expect_identical(getOption("tempest.otel.enabled"), FALSE)
      expect_length(published, 2L)
      expect_null(worker_state$job)
      expect_null(worker_state$config)
    }
  )
})

test_that("host telemetry docs keep the privacy-safe Collector boundary", {
  context <- test_source_inventory_context()
  if (!identical(context$mode, "source")) {
    expect_identical(context$mode, "installed")
    return(invisible(NULL))
  }
  readme <- paste(
    readLines(file.path(context$root, "README.md"), warn = FALSE),
    collapse = "\n"
  )
  required <- c(
    "OTEL_TRACES_EXPORTER=http",
    "OTEL_EXPORTER_OTLP_ENDPOINT=http://127.0.0.1:4318",
    "OTEL_R_EMIT_SCOPES=io.github.jameshwade.tempest",
    "OTEL_INSTRUMENTATION_GENAI_CAPTURE_MESSAGE_CONTENT=false",
    "Shiny, Mirai, ellmer, and httr2",
    "url.full",
    "GOOGLE_SEARCH_API_KEY",
    "optional operator-selected destination downstream of the Collector"
  )
  missing <- required[
    !vapply(
      required,
      grepl,
      logical(1),
      x = readme,
      fixed = TRUE
    )
  ]

  expect_identical(missing, character())
  expect_no_match(readme, "logfire-us.pydantic.dev", fixed = TRUE)
  expect_no_match(readme, "Authorization=", fixed = TRUE)
})

test_that("Run review is internal and leaves public adapter shapes exact", {
  skip_if_not_installed("shiny")
  expect_identical(
    tempest:::tempest_shiny_panel_choices(),
    c(
      "chat",
      "sources",
      "facts",
      "mindmap",
      "transcript",
      "report",
      "storm",
      "review"
    )
  )
  server_code <- paste(deparse(body(tempest_shiny_server)), collapse = "\n")
  store_code <- paste(deparse(body(tempest_shiny_store)), collapse = "\n")

  expect_match(server_code, "mod_run_review_server", fixed = TRUE)
  expect_match(server_code, "last_successful_product", fixed = TRUE)
  expect_no_match(server_code, "review =", fixed = TRUE)
  expect_no_match(store_code, "review", fixed = TRUE)
  expect_identical(length(tempest_shiny_store()), 13L)
})

test_that("Run review panel does not widen the public server handle", {
  skip_if_not_installed("shiny")
  store <- tempest_shiny_store()

  shiny::testServer(
    tempest_shiny_server,
    args = list(
      config = tempest_config(),
      store = store,
      panels = "review"
    ),
    {
      expect_named(
        session$returned,
        c(
          "store",
          "costorm_session",
          "costorm_events",
          "costorm_evidence",
          "storm_events",
          "report_md",
          "report_workspace",
          "report_topic",
          "report_navigation_event",
          "touch_costorm_session"
        )
      )
      expect_disjoint(
        names(session$returned),
        c("review", "review_id", "storm_product")
      )
    }
  )
})

test_that("STORM run identity mismatches fail closed and clean worker state", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("later")
  skip_if_not_installed("mirai")
  local_mirai_coverage_dir()
  env <- tempest:::tempest_shiny_module_env()
  stream_path <- NULL
  prior_report <- "# Previously published report"
  publish_calls <- 0L
  store <- list(
    publish_storm_report = function(...) {
      publish_calls <<- publish_calls + 1L
      prior_report <<- "mutated"
    }
  )

  if (!methods::isClass("TempestTestMismatchedManifest")) {
    methods::setClass(
      "TempestTestMismatchedManifest",
      slots = c(research_run_id = "character")
    )
  }
  original_poll <- env$storm_poll_progress_stream
  original_runner <- env$storm_run_with_progress
  withr::defer({
    env$storm_poll_progress_stream <- original_poll
    env$storm_run_with_progress <- original_runner
  })
  env$storm_poll_progress_stream <- function(path, ...) {
    stream_path <<- path
    invisible(path)
  }
  env$storm_run_with_progress <- function(topic, cfg, ...) {
    if (
      !methods::isClass(
        "TempestTestMismatchedManifest",
        where = globalenv()
      )
    ) {
      methods::setClass(
        "TempestTestMismatchedManifest",
        slots = c(research_run_id = "character"),
        where = globalenv()
      )
    }
    list(
      result = list(
        manifest = methods::new(
          "TempestTestMismatchedManifest",
          research_run_id = "shiny-storm-stale"
        )
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
      session$setInputs(topic = "Run identity", run = 1)
      session$flushReact()
      status <- await_tempest_extended_task(storm_task, session)

      expect_identical(status, "success")
      expect_identical(
        shiny::isolate(publication_error()),
        "The STORM result could not be published."
      )
      expect_null(shiny::isolate(published_result()))
      expect_identical(progress_stream$active, FALSE)
      expect_null(progress_stream$path)
      expect_null(worker_state$job)
      expect_null(worker_state$topic)
      expect_null(worker_state$run_id)
      expect_null(worker_state$config)
      expect_identical(file.exists(stream_path), FALSE)
      expect_identical(publish_calls, 0L)
      expect_identical(prior_report, "# Previously published report")
      expect_identical(
        shiny::isolate(session$returned$report_navigation_event()),
        0L
      )
      result_html <- paste(as.character(output$result$html), collapse = "")
      expect_match(result_html, 'role="alert"', fixed = TRUE)
      expect_match(result_html, "could not be published", fixed = TRUE)
      expect_no_match(result_html, "Pipeline complete", fixed = TRUE)
    }
  )
})

test_that("STORM authority rejection fails closed and cleans worker state", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("later")
  skip_if_not_installed("mirai")
  local_mirai_coverage_dir()
  env <- tempest:::tempest_shiny_module_env()
  stream_path <- NULL
  prior_report <- "# Previously published report"
  publish_calls <- 0L
  store <- list(
    publish_storm_report = function(...) {
      publish_calls <<- publish_calls + 1L
      stop("Authorization: Bearer provider-secret")
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
  env$storm_poll_progress_stream <- function(path, ...) {
    stream_path <<- path
    invisible(path)
  }
  env$storm_result_run_id <- function(result) result$run_id
  env$storm_run_with_progress <- function(
    topic,
    cfg,
    progress_run_id,
    ...
  ) {
    list(
      result = list(run_id = progress_run_id),
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
      session$setInputs(topic = "Authority rejection", run = 1)
      session$flushReact()
      status <- await_tempest_extended_task(storm_task, session)

      expect_identical(status, "success")
      expect_identical(
        shiny::isolate(publication_error()),
        "The STORM report failed product integrity validation."
      )
      expect_null(shiny::isolate(published_result()))
      expect_identical(progress_stream$active, FALSE)
      expect_null(progress_stream$path)
      expect_null(worker_state$job)
      expect_null(worker_state$topic)
      expect_null(worker_state$run_id)
      expect_null(worker_state$config)
      expect_identical(file.exists(stream_path), FALSE)
      expect_identical(publish_calls, 1L)
      expect_identical(prior_report, "# Previously published report")
      expect_identical(
        shiny::isolate(session$returned$report_navigation_event()),
        0L
      )
      result_html <- paste(as.character(output$result$html), collapse = "")
      expect_match(result_html, 'role="alert"', fixed = TRUE)
      expect_match(result_html, "product integrity validation", fixed = TRUE)
      expect_no_match(result_html, "provider-secret", fixed = TRUE)
      expect_no_match(result_html, "Pipeline complete", fixed = TRUE)
    }
  )
})

test_that("STORM cancellation clears active worker state", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("later")
  skip_if_not_installed("mirai")
  local_mirai_coverage_dir()
  env <- tempest:::tempest_shiny_module_env()
  stream_path <- NULL
  prior_report <- "# Previously published report"
  publish_calls <- 0L
  store <- list(
    publish_storm_report = function(...) {
      publish_calls <<- publish_calls + 1L
      prior_report <<- "mutated"
    }
  )

  original_poll <- env$storm_poll_progress_stream
  original_runner <- env$storm_run_with_progress
  withr::defer({
    env$storm_poll_progress_stream <- original_poll
    env$storm_run_with_progress <- original_runner
  })
  env$storm_poll_progress_stream <- function(path, ...) {
    stream_path <<- path
    invisible(path)
  }
  env$storm_run_with_progress <- function(...) {
    Sys.sleep(5)
    list(result = list(), progress = list())
  }

  shiny::testServer(
    env$mod_storm_server,
    args = list(
      config = shiny::reactive(tempest_config()),
      store = store
    ),
    {
      session$setInputs(topic = "Cancellation", run = 1)
      session$flushReact()
      expect_s3_class(worker_state$job, "mirai")
      expect_identical(file.exists(stream_path), TRUE)
      cancelled_job <- worker_state$job

      session$setInputs(cancel = 1)
      session$flushReact()
      expect_identical(worker_state$job, cancelled_job)

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

      expect_identical(worker_state$cancelled, TRUE)
      expect_identical(mirai::unresolved(cancelled_job), FALSE)
      expect_identical(task_status, "success")
      expect_identical(progress_stream$active, FALSE)
      expect_null(progress_stream$path)
      expect_null(worker_state$job)
      expect_null(worker_state$topic)
      expect_null(worker_state$run_id)
      expect_null(worker_state$config)
      expect_identical(file.exists(stream_path), FALSE)
      expect_identical(publish_calls, 0L)
      expect_identical(prior_report, "# Previously published report")
      expect_identical(
        shiny::isolate(session$returned$report_navigation_event()),
        0L
      )
    }
  )
})

test_that("STORM rejects a queued launch without orphaning worker state", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("later")
  skip_if_not_installed("mirai")
  local_mirai_coverage_dir()
  env <- tempest:::tempest_shiny_module_env()
  stream_paths <- character()
  published <- character()
  store <- list(
    publish_storm_report = function(result, config) {
      published <<- c(published, result$report_md)
      invisible(result$report_md)
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
  env$storm_poll_progress_stream <- function(path, ...) {
    stream_paths <<- c(stream_paths, path)
    invisible(path)
  }
  env$storm_result_run_id <- function(result) result$run_id
  env$storm_run_with_progress <- function(
    topic,
    progress_run_id,
    ...
  ) {
    Sys.sleep(0.25)
    list(
      result = list(run_id = progress_run_id, report_md = topic),
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

      session$setInputs(topic = "First", run = 1)
      session$flushReact()
      first_job <- worker_state$job
      first_stream <- progress_stream$path
      expect_s3_class(first_job, "mirai")

      session$setInputs(topic = "Second", run = 2)
      session$flushReact()
      expect_identical(worker_state$job, first_job)
      expect_identical(worker_state$topic, "First")
      expect_identical(progress_stream$path, first_stream)
      expect_length(stream_paths, 1L)

      expect_identical(wait_for_terminal(), "success")
      expect_identical(mirai::unresolved(first_job), FALSE)
      expect_identical(published, "First")
      expect_identical(
        shiny::isolate(session$returned$last_successful_product())$report_md,
        "First"
      )
      expect_identical(file.exists(first_stream), FALSE)

      session$setInputs(topic = "Second", run = 3)
      session$flushReact()
      second_job <- worker_state$job
      expect_s3_class(second_job, "mirai")
      expect_identical(wait_for_terminal(), "success")
      expect_identical(mirai::unresolved(second_job), FALSE)
      expect_identical(published, c("First", "Second"))
      expect_identical(file.exists(stream_paths), c(FALSE, FALSE))
    }
  )
})

test_that("Shiny archive transport delegates product validation", {
  env <- tempest:::tempest_shiny_module_env()
  extract_code <- paste(
    deparse(body(env$session_archive_extract)),
    collapse = "\n"
  )

  expect_match(
    extract_code,
    "tempest:::tempest_costorm_archive_read(root)",
    fixed = TRUE
  )
  expect_no_match(extract_code, "schema_version", fixed = TRUE)
  expect_no_match(
    extract_code,
    "tempest_session_bundle_validate_manifest",
    fixed = TRUE
  )
  expect_identical(
    exists("session_archive_manifest_files", envir = env, inherits = FALSE),
    FALSE
  )
  expect_identical(
    exists("session_archive_read_manifest", envir = env, inherits = FALSE),
    FALSE
  )
})

test_that("public Shiny store exposes only product-named state", {
  skip_if_not_installed("shiny")
  store <- tempest_shiny_store()

  expect_named(
    store,
    c(
      "peek_costorm_session",
      "costorm_session",
      "costorm_workspace",
      "set_costorm_session",
      "touch_costorm_session",
      "save_costorm_session",
      "resume_costorm_session",
      "costorm_persistence_status",
      "report_md",
      "report_workspace",
      "report_topic",
      "publish_costorm_report",
      "publish_storm_report"
    )
  )
  expect_disjoint(
    names(store),
    c(
      "peek",
      "autosave_trigger",
      "get",
      "evidence_store",
      "set",
      "touch",
      "save",
      "restore",
      "persistence",
      "set_persistence",
      "report",
      "report_store",
      "set_session_report",
      "set_storm_result"
    )
  )
})

test_that("Shiny publishes only authority-validated Co-STORM reports", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ellmer")
  config <- tempest_config(chat_fn = function(...) fake_chat())
  session <- tempest_session(
    "Shiny costorm authority",
    config = config,
    experts = list(test_expert(expert_id = "expert.shiny-costorm"))
  )
  store <- tempest_shiny_store()
  store$set_costorm_session(session)
  evidence <- test_persistence_add_costorm_evidence(
    session,
    key = "shiny-costorm"
  )
  report_md <- paste0(
    "# Shiny costorm authority\n\n",
    evidence$claim@claim_text,
    " [",
    evidence$source@resource_id,
    "]."
  )
  report_md <- test_persistence_commit_costorm_report(session, report_md)

  expect_null(shiny::isolate(store$report_md()))
  expect_identical(store$publish_costorm_report(session), report_md)
  store$set_costorm_session(NULL)
  private <- session$.__enclos_env__$private
  private$report_md_value <- paste0(report_md, "\n")
  expect_error(
    store$set_costorm_session(session),
    class = "tempest_product_report_error"
  )
  expect_null(store$peek_costorm_session())
  expect_null(shiny::isolate(store$report_md()))
})

test_that("Shiny publishes only authority-validated STORM reports", {
  skip_if_not_installed("shiny")
  config <- tempest_config()
  fixture <- test_persistence_complete_storm_product(
    "Shiny authority",
    "shiny-authority",
    config,
    tempest_program_set(),
    manifest_status = "running"
  )
  manifest <- tempest:::tempest_product_authority_finalize_manifest(
    manifest = fixture$manifest,
    stage_records = fixture$state$stage_records,
    workspace = fixture$workspace,
    report_md = fixture$state$report_md,
    config = config,
    experts = fixture$state$experts,
    product_state = fixture$state,
    status = "succeeded",
    require_publishable = TRUE
  )
  result <- list(
    title = fixture$state$title,
    experts = fixture$state$experts,
    report_md = fixture$state$report_md,
    manifest = manifest,
    state = fixture$state,
    workspace = fixture$workspace
  )
  store <- tempest_shiny_store()
  env <- tempest:::tempest_shiny_module_env()

  expect_identical(
    env$storm_result_run_id(result),
    result$manifest@research_run_id
  )
  expect_null(env$storm_result_run_id(list()))

  expect_identical(
    store$publish_storm_report(result, config),
    result$report_md
  )
  before <- shiny::isolate(store$report_md())
  tampered <- result
  tampered$report_md <- paste0(result$report_md, "\n\nTampered.")
  expect_error(
    store$publish_storm_report(tampered, config),
    class = "tempest_product_report_error"
  )
  expect_identical(shiny::isolate(store$report_md()), before)
})

test_that("Shiny server handle separates Co-STORM and STORM state", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ellmer")
  config <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  store <- tempest_shiny_store()
  product_session <- tempest_session(
    "Shiny product boundary",
    config = config,
    experts = list(test_expert()),
    retriever = tempest_retriever(
      config = config,
      workspace = test_research_workspace()
    )
  )

  shiny::testServer(
    tempest_shiny_server,
    args = list(config = config, store = store, panels = "sources"),
    {
      handle <- session$returned
      expect_named(
        handle,
        c(
          "store",
          "costorm_session",
          "costorm_events",
          "costorm_evidence",
          "storm_events",
          "report_md",
          "report_workspace",
          "report_topic",
          "report_navigation_event",
          "touch_costorm_session"
        )
      )
      expect_identical(handle$store, store)
      shared_store$set_costorm_session(product_session)
      session$flushReact()

      expect_identical(current_costorm_session(), product_session)
      expect_identical(
        shiny::isolate(costorm_events()),
        tempest_execution_events(product_session)
      )
      expect_named(
        shiny::isolate(costorm_evidence()),
        c("resources", "claims", "disputes")
      )
      expect_identical(shiny::isolate(storm_events()), list())
      expect_identical(shiny::isolate(report_navigation_event()), 0L)
    }
  )
})
