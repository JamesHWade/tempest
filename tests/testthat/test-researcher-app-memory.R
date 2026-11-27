test_that("researcher memory sidecar stays outside enriched report manifest", {
  root <- withr::local_tempdir()
  fixture <- test_promotion_fixture("storm")
  report <- tempest_enrich_report(
    fixture$research,
    file.path(root, "research-run-a1b2"),
    plan = test_enriched_plan(fixture$resource@resource_id, analyses = FALSE),
    allow_code = FALSE
  )

  record_path <- tempest:::tempest_researcher_memory_write_record(
    output_dir = root,
    report_id = "research-run-a1b2",
    selection_id = strrep("a", 64L),
    research_run_id = "run-1",
    bundle_id = "bundle-1"
  )
  expect_true(file.exists(record_path))
  # The enriched report directory itself remains unmodified and valid.
  expect_s7_class(
    tempest_read_enriched_report(report@path),
    TempestEnrichedReport
  )

  parsed <- tempest:::tempest_researcher_memory_read_record(
    root,
    "research-run-a1b2"
  )
  expect_identical(parsed$selection_id, strrep("a", 64L))
  expect_identical(parsed$bundle_id, "bundle-1")
  expect_null(
    tempest:::tempest_researcher_memory_read_record(root, "research-run-9999")
  )
})

test_that("researcher worker attaches Graft recall tools, pins basis, and publishes candidates", {
  skip_if_not_installed("graft")
  skip_if_not_installed("ellmer")
  skip_if_not("graft_store" %in% getNamespaceExports("graft"))

  store_path <- file.path(withr::local_tempdir(), "memory")
  store <- graft::graft_store(store_path, create = TRUE)
  prior <- test_promotion_storm_fixture(
    run_id = "worker-prior-1",
    evidence_text = "Baseline pilot achieved 82% recovery."
  )
  prior_sel <- tempest_publish_artifact_research(prior$research, store)
  tempest:::tempest_graft_accept_selection(
    store,
    selection_id = prior_sel,
    stream = "pilot-batteries",
    actor = "reviewer",
    reason = "Accepted prior baseline",
    purpose = "research"
  )

  fresh <- test_promotion_storm_fixture(
    run_id = "worker-fresh-2",
    evidence_text = "Updated pilot achieved 74% recovery."
  )
  events <- new.env(parent = emptyenv())
  output_dir <- file.path(withr::local_tempdir(), "report")
  local_mocked_bindings(
    tempest_researcher_tools_config = function(config, tools) {
      events$tool_names <- vapply(tools, \(t) t@name, character(1))
      config
    },
    tempest_researcher_app_run_research = function(...) {
      events$run <- list(...)
      list(
        research = fresh$research,
        report = TempestEnrichedReport(
          path = output_dir,
          status = "complete",
          manifest = list(status = "complete")
        )
      )
    }
  )

  request <- list(
    topic = "Pilot battery update",
    model = "openai/gpt-6.1-sol",
    n_experts = 2L,
    max_rounds = 2L,
    max_questions = 2L,
    allow_code = FALSE,
    allow_images = FALSE,
    max_analyses = 0L,
    max_illustrations = 0L,
    selected_tools = character(),
    mcp_servers = character(),
    selected_mcp = character(),
    use_memory_tools = TRUE,
    memory_basis_stream = "pilot-batteries"
  )

  res <- tempest:::tempest_researcher_worker(
    kind = "research",
    request = request,
    config = tempest_config(chat_fn = function(...) NULL),
    tools = list(),
    mcp_config = NULL,
    output_dir = output_dir,
    progress_path = tempfile(fileext = ".jsonl"),
    graft_store_path = store_path,
    memory_purpose = "research"
  )

  expect_true("recall_project_memory" %in% events$tool_names)
  expect_true(S7::S7_inherits(events$run$knowledge, TempestKnowledge))
  expect_true(rlang::is_string(res$graft$selection_id))

  # Candidate is published in Graft as provisional, not accepted automatically.
  st <- tempest:::tempest_graft_selection_status(store, res$graft$selection_id)
  expect_identical(st$status, "provisional")
})

test_that("researcher worker preserves completed report when Graft promotion fails", {
  skip_if_not_installed("graft")
  skip_if_not("graft_store" %in% getNamespaceExports("graft"))

  store_path <- file.path(withr::local_tempdir(), "memory")
  store <- graft::graft_store(store_path, create = TRUE)
  fresh <- test_promotion_storm_fixture(
    run_id = "worker-fail-promote",
    evidence_text = "Completed report whose promotion fails."
  )
  output_dir <- file.path(withr::local_tempdir(), "research-run-feed")
  dir.create(output_dir, recursive = TRUE)

  local_mocked_bindings(
    tempest_researcher_tools_config = function(config, tools) config,
    tempest_researcher_app_run_research = function(...) {
      list(
        research = fresh$research,
        report = TempestEnrichedReport(
          path = output_dir,
          status = "complete",
          manifest = list(status = "complete")
        )
      )
    },
    tempest_promotion_bundle = function(...) {
      stop("promotion closure rejected unsupported claim")
    }
  )

  request <- list(
    topic = "Strict promotion test",
    model = "openai/gpt-6.1-sol",
    n_experts = 2L,
    max_rounds = 1L,
    max_questions = 1L,
    allow_code = FALSE,
    allow_images = FALSE,
    max_analyses = 0L,
    max_illustrations = 0L,
    selected_tools = character(),
    mcp_servers = character(),
    selected_mcp = character(),
    use_memory_tools = FALSE,
    memory_basis_stream = NULL
  )

  res <- tempest:::tempest_researcher_worker(
    kind = "research",
    request = request,
    config = tempest_config(chat_fn = function(...) NULL),
    tools = list(),
    mcp_config = NULL,
    output_dir = output_dir,
    progress_path = tempfile(fileext = ".jsonl"),
    graft_store_path = store_path,
    memory_purpose = "research"
  )

  expect_identical(res$status, "complete")
  expect_identical(res$path, output_dir)
  expect_null(res$graft$selection_id)
  expect_match(
    res$graft$publish_error,
    "promotion closure rejected unsupported claim",
    fixed = TRUE
  )

  # Sidecar preserves the explicit failure reason and review panel renders it.
  reports_root <- withr::local_tempdir()
  tempest:::tempest_researcher_memory_write_record(
    output_dir = reports_root,
    report_id = "research-run-feed",
    publish_error = res$graft$publish_error
  )
  rec <- tempest:::tempest_researcher_memory_read_record(
    reports_root,
    "research-run-feed"
  )
  expect_null(rec$selection_id)
  expect_match(rec$publish_error, "promotion closure", fixed = TRUE)

  current <- shiny::reactiveVal(list(
    path = file.path(reports_root, "research-run-feed"),
    status = "complete"
  ))
  shiny::testServer(
    function(id) {
      tempest:::tempest_researcher_memory_review_server(
        id,
        store = store,
        output_dir = reports_root,
        current_report = current,
        purpose = "research"
      )
    },
    {
      session$flushReact()
      expect_match(output$panel$html, "Not published to Graft", fixed = TRUE)
      expect_match(
        output$panel$html,
        "promotion closure rejected unsupported claim",
        fixed = TRUE
      )
    }
  )
})

test_that("researcher review panel rejects stale re-acceptance when stream head changed", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("graft")
  skip_if_not("graft_store" %in% getNamespaceExports("graft"))

  root <- withr::local_tempdir()
  store <- graft::graft_store(file.path(root, "memory"), create = TRUE)
  reports_dir <- file.path(root, "reports")
  dir.create(reports_dir)

  f1 <- test_promotion_storm_fixture(
    run_id = "stale-accept-1",
    evidence_text = "First report evidence for stale accept test."
  )
  f2 <- test_promotion_storm_fixture(
    run_id = "stale-accept-2",
    evidence_text = "Second report evidence superseding first."
  )
  report <- tempest_enrich_report(
    f1$research,
    file.path(reports_dir, "research-run-1111"),
    plan = test_enriched_plan(f1$resource@resource_id, analyses = FALSE),
    allow_code = FALSE
  )
  sel1 <- tempest_publish_artifact_research(f1$research, store)
  sel2 <- tempest_publish_artifact_research(f2$research, store)
  tempest:::tempest_researcher_memory_write_record(
    output_dir = reports_dir,
    report_id = "research-run-1111",
    selection_id = sel1,
    research_run_id = "stale-accept-1",
    bundle_id = "bundle-1"
  )

  mem_rev <- shiny::reactiveVal(0L)
  current <- shiny::reactiveVal(list(path = report@path, status = "complete"))

  shiny::testServer(
    function(id) {
      tempest:::tempest_researcher_memory_review_server(
        id,
        store = store,
        output_dir = reports_dir,
        current_report = current,
        purpose = "research",
        default_actor = "alice",
        memory_revision = mem_rev
      )
    },
    {
      session$flushReact()
      session$setInputs(
        stream = "shared-battery-stream",
        actor = "alice",
        reason = "Initial accept",
        reviewed = TRUE,
        accept = 1
      )
      session$flushReact()
      expect_identical(mem_rev(), 1L)

      # Out-of-band correction advances the stream head before alice re-accepts.
      tempest:::tempest_graft_accept_selection(
        store,
        selection_id = sel2,
        stream = "shared-battery-stream",
        actor = "bob",
        reason = "Superseding correction",
        purpose = "research"
      )

      session$setInputs(
        existing_stream = "shared-battery-stream",
        reason = "Stale re-accept",
        reviewed = TRUE,
        accept = 2
      )
      session$flushReact()
      expect_identical(mem_rev(), 1L)
      expect_match(
        output$panel$html,
        "changed since it was displayed",
        fixed = TRUE
      )
      expect_identical(
        tempest:::tempest_graft_selection_status(store, sel2)$status,
        "accepted"
      )
    }
  )
})

test_that("researcher memory review module and library integrate accept and withdraw", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("graft")
  skip_if_not("graft_store" %in% getNamespaceExports("graft"))

  root <- withr::local_tempdir()
  store <- graft::graft_store(file.path(root, "memory"), create = TRUE)
  reports_dir <- file.path(root, "reports")
  dir.create(reports_dir)

  fixture <- test_promotion_fixture("storm")
  report <- tempest_enrich_report(
    fixture$research,
    file.path(reports_dir, "research-run-a1b2"),
    plan = test_enriched_plan(fixture$resource@resource_id, analyses = FALSE),
    allow_code = FALSE
  )
  bundle <- tempest_promotion_bundle(fixture$research)
  sel <- tempest_publish_artifact_research(
    bundle,
    store,
    report = tempest_report(fixture$research)
  )
  tempest:::tempest_researcher_memory_write_record(
    output_dir = reports_dir,
    report_id = "research-run-a1b2",
    selection_id = sel,
    research_run_id = bundle@research_run_id,
    bundle_id = bundle@bundle_id
  )

  mem_rev <- shiny::reactiveVal(0L)
  current <- shiny::reactiveVal(list(path = report@path, status = "complete"))

  shiny::testServer(
    function(id) {
      tempest:::tempest_researcher_memory_review_server(
        id,
        store = store,
        output_dir = reports_dir,
        current_report = current,
        purpose = "research",
        default_actor = "reviewer",
        memory_revision = mem_rev
      )
    },
    {
      session$flushReact()
      expect_match(output$panel$html, "Provisional draft", fixed = TRUE)

      # Must confirm review before accepting.
      session$setInputs(
        stream = "grid-batteries",
        actor = "alice",
        reason = "Checked claims",
        reviewed = FALSE,
        accept = 1
      )
      session$flushReact()
      expect_match(output$panel$html, "Confirm that you reviewed", fixed = TRUE)

      session$setInputs(reviewed = TRUE, accept = 2)
      session$flushReact()
      expect_match(output$panel$html, "Accepted", fixed = TRUE)
      expect_identical(mem_rev(), 1L)

      # Library catalog now shows this report as accepted in Graft.
      cat_acc <- tempest:::tempest_researcher_library_catalog(
        reports_dir,
        graft_store = store,
        memory_purpose = "research"
      )
      expect_identical(cat_acc$reports$memory_status, "accepted")
      expect_identical(cat_acc$reports$memory_stream, "grid-batteries")

      # Withdraw from active memory.
      session$setInputs(reason = "Superseded by new lab data", withdraw = 1)
      session$flushReact()
      expect_match(output$panel$html, "Withdrawn", fixed = TRUE)
      expect_identical(mem_rev(), 2L)
    }
  )

  # Launcher wires memory_enabled when graft_store is supplied.
  withr::local_options(tempest.app_runner = function(app, ...) app)
  observed <- new.env(parent = emptyenv())
  local_mocked_bindings(
    tempest_researcher_ui = function(..., memory_enabled) {
      observed$ui <- memory_enabled
      shiny::div()
    },
    tempest_researcher_app_server = function(..., graft_store) {
      observed$server <- !is.null(graft_store)
      function(input, output, session) {}
    }
  )
  tempest_researcher_app(
    output_dir = reports_dir,
    graft_store = file.path(root, "memory")
  )
  expect_identical(observed$ui, TRUE)
  expect_identical(observed$server, TRUE)
})

test_that("researcher app and worker support store factory and Postgres handoff cleanly", {
  skip_if_not_installed("graft")
  skip_if_not_installed("ellmer")
  skip_if_not("graft_store" %in% getNamespaceExports("graft"))

  store_path <- file.path(withr::local_tempdir(), "pg-sim-memory")
  backing_store <- graft::graft_store(store_path, create = TRUE)
  fresh <- test_promotion_storm_fixture(
    run_id = "worker-pg-handoff-1",
    evidence_text = "Postgres-handed-off report evidence."
  )
  output_dir <- file.path(withr::local_tempdir(), "report-pg")
  closed_stores <- 0L

  local_mocked_bindings(
    tempest_researcher_tools_config = function(config, tools) config,
    tempest_researcher_app_run_research = function(...) {
      list(
        research = fresh$research,
        report = TempestEnrichedReport(
          path = output_dir,
          status = "complete",
          manifest = list(status = "complete")
        )
      )
    },
    tempest_graft_store_connect_postgres = function(handoff) {
      expect_identical(handoff$scope, "team-research")
      backing_store
    },
    tempest_graft_store_close = function(store) {
      closed_stores <<- closed_stores + 1L
      invisible(TRUE)
    }
  )

  request <- list(
    topic = "Postgres worker handoff test",
    model = "openai/gpt-6.1-sol",
    n_experts = 2L,
    max_rounds = 1L,
    max_questions = 1L,
    allow_code = FALSE,
    allow_images = FALSE,
    max_analyses = 0L,
    max_illustrations = 0L,
    selected_tools = character(),
    mcp_servers = character(),
    selected_mcp = character(),
    use_memory_tools = FALSE,
    memory_basis_stream = NULL
  )

  pg_handoff <- list(
    kind = "postgres",
    scope = "team-research",
    max_bytes = 64L * 1024L * 1024L,
    max_revision_bytes = 1024L * 1024L,
    connection_info = list(dbname = "tempest", host = "localhost")
  )
  res <- tempest:::tempest_researcher_worker(
    kind = "research",
    request = request,
    config = tempest_config(chat_fn = function(...) NULL),
    tools = list(),
    mcp_config = NULL,
    output_dir = output_dir,
    progress_path = tempfile(fileext = ".jsonl"),
    graft_store_handoff = pg_handoff,
    memory_purpose = "research"
  )

  expect_identical(res$status, "complete")
  expect_true(rlang::is_string(res$graft$selection_id))
  # Owned Postgres worker connection is closed in on.exit().
  expect_identical(closed_stores, 1L)

  # Launcher accepts a zero-argument store factory function cleanly.
  reports_dir <- withr::local_tempdir()
  withr::local_options(tempest.app_runner = function(app, ...) app)
  seen_handoff <- NULL
  local_mocked_bindings(
    tempest_researcher_ui = function(..., memory_enabled) shiny::div(),
    tempest_researcher_app_server = function(..., graft_store_handoff) {
      seen_handoff <<- graft_store_handoff
      function(input, output, session) {}
    }
  )
  tempest_researcher_app(
    output_dir = reports_dir,
    graft_store = function() graft::graft_store(store_path, create = FALSE)
  )
  expect_identical(seen_handoff$kind, "factory")
  expect_true(is.function(seen_handoff$factory))
})
