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
  expect_match(
    server_code,
    "identical(result_run_id, worker_state$run_id)",
    fixed = TRUE
  )
  expect_match(server_code, "isTRUE(worker_state$cancelled)", fixed = TRUE)
  expect_match(server_code, "report_navigation_event", fixed = TRUE)
  expect_no_match(server_code, "report_ready", fixed = TRUE)
  expect_no_match(runner_code, "parallel_research", fixed = TRUE)
  expect_no_match(runner_code, "tempest_run =", fixed = TRUE)
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
