test_that("Shiny adapter excludes the generic run surface", {
  expect_identical("run" %in% names(formals(tempest_shiny_server)), FALSE)
  server_code <- paste(deparse(body(tempest_shiny_server)), collapse = "\n")

  expect_no_match(server_code, "TempestRun", fixed = TRUE)
  expect_no_match(server_code, "tempest_run_", fixed = TRUE)
  expect_no_match(server_code, "set_run", fixed = TRUE)
})

test_that("public Shiny store exposes only product state", {
  skip_if_not_installed("shiny")
  store <- tempest_shiny_store()

  expect_named(
    store,
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
      "report_topic",
      "set_session_report",
      "set_storm_result"
    )
  )
  expect_identical(
    intersect(
      names(store),
      c("run", "get_run", "peek_run", "set_run", "touch_run", "set_report")
    ),
    character()
  )
})

test_that("Shiny accepts only an authority-validated STORM result", {
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

  expect_identical(
    store$set_storm_result(result, config),
    result$report_md
  )
  before <- shiny::isolate(store$report())
  tampered <- result
  tampered$report_md <- paste0(result$report_md, "\n\nTampered.")
  expect_error(
    store$set_storm_result(tampered, config),
    class = "tempest_product_report_error"
  )
  expect_identical(shiny::isolate(store$report()), before)
})

test_that("Shiny adapter exposes product session state", {
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
      shared_store$set(product_session)
      session$flushReact()

      expect_identical(current_session(), product_session)
      expect_identical(
        shiny::isolate(product_events()),
        tempest_execution_events(product_session)
      )
      expect_named(
        shiny::isolate(product_evidence()),
        c("resources", "claims", "disputes")
      )
    }
  )
})
