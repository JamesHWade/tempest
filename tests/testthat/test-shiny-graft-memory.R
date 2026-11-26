test_that("tempest_app() configures optional Graft memory context cleanly", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("graft")
  skip_if_not("graft_store" %in% getNamespaceExports("graft"))

  store_dir <- file.path(withr::local_tempdir(), "app-memory")
  observed <- new.env(parent = emptyenv())
  withr::local_options(tempest.app_runner = function(app_dir, ...) {
    observed$app_dir <- app_dir
    observed$memory <- getOption("tempest.shiny.memory")
    app_dir
  })

  tempest_app(
    graft_store = store_dir,
    memory_purpose = "briefing",
    memory_actor = "alice"
  )

  expect_true(dir.exists(store_dir))
  expect_true(S7::S7_inherits(observed$memory$store, graft::LocalArtifactStore))
  expect_identical(observed$memory$purpose, "briefing")
  expect_identical(observed$memory$actor, "alice")
  # Scoped option is restored after runner exits.
  expect_null(getOption("tempest.shiny.memory"))
})

test_that("Co-STORM memory config wraps chat_fn with read-only graft tools and pins streams", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("graft")
  skip_if_not_installed("ellmer")
  skip_if_not("graft_tools" %in% getNamespaceExports("graft"))

  store <- graft::graft_store(
    file.path(withr::local_tempdir(), "chat-memory"),
    create = TRUE
  )
  fixture <- test_promotion_storm_fixture(
    run_id = "chat-mem-1",
    evidence_text = "Verified baseline metric for chat recall."
  )
  sel <- tempest_publish_artifact_research(fixture$research, store)
  tempest:::tempest_graft_accept_selection(
    store,
    selection_id = sel,
    stream = "baseline-metric",
    actor = "reviewer",
    reason = "Verified baseline",
    purpose = "research"
  )

  env <- tempest:::tempest_shiny_module_env()
  ctx <- list(
    store = store,
    store_path = tempest:::tempest_graft_store_path(store),
    purpose = "research",
    actor = "reviewer",
    eligible = function(stream) TRUE
  )

  registered_by_role <- list()
  make_fake_role_chat <- function(role) {
    list(
      get_tools = function() list(),
      register_tools = function(tools) {
        registered_by_role[[role]] <<- c(
          registered_by_role[[role]],
          vapply(tools, \(t) t@name, character(1))
        )
      }
    )
  }
  base_cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) {
      make_fake_role_chat(role)
    }
  )
  wrapped_cfg <- env$shiny_memory_wrap_config(base_cfg, ctx, use_tools = TRUE)
  tempest:::tempest_make_chat(wrapped_cfg, "coordinator", system_prompt = "sys")
  tempest:::tempest_make_chat(wrapped_cfg, "expert", system_prompt = "sys")
  tempest:::tempest_make_chat(wrapped_cfg, "judge", system_prompt = "sys")
  expect_setequal(
    registered_by_role$coordinator,
    c("list_project_memory", "recall_project_memory")
  )
  expect_setequal(
    registered_by_role$expert,
    c("list_project_memory", "recall_project_memory")
  )
  expect_null(registered_by_role$judge)

  mem_rev <- shiny::reactiveVal(0L)
  shiny::testServer(
    function(id) {
      env$mod_memory_config_server(
        id,
        memory_ctx = ctx,
        memory_revision = mem_rev
      )
    },
    {
      session$setInputs(
        use_memory_tools = TRUE,
        pinned_stream = "baseline-metric"
      )
      session$flushReact()
      expect_identical(session$returned$use_tools(), TRUE)
      expect_true(S7::S7_inherits(
        session$returned$knowledge(),
        TempestKnowledge
      ))
      expect_match(output$stream_status$html, "1 accepted stream", fixed = TRUE)
    }
  )
})

test_that("Co-STORM report memory panel publishes, accepts, and withdraws a product", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("graft")
  skip_if_not("graft_store" %in% getNamespaceExports("graft"))

  store <- graft::graft_store(
    file.path(withr::local_tempdir(), "report-memory"),
    create = TRUE
  )
  fixture <- test_promotion_storm_fixture(
    run_id = "report-mem-1",
    evidence_text = "Published report evidence."
  )
  env <- tempest:::tempest_shiny_module_env()
  ctx <- list(
    store = store,
    store_path = tempest:::tempest_graft_store_path(store),
    purpose = "research",
    actor = "alice",
    eligible = function(stream) TRUE
  )
  prod <- shiny::reactiveVal(fixture$research)
  mem_rev <- shiny::reactiveVal(0L)

  shiny::testServer(
    function(id) {
      env$mod_memory_report_server(
        id,
        memory_ctx = ctx,
        product = prod,
        memory_revision = mem_rev
      )
    },
    {
      session$flushReact()
      expect_match(output$panel$html, "Provisional draft", fixed = TRUE)

      session$setInputs(
        stream = "grid-storage",
        actor = "alice",
        reason = "Checked sources",
        reviewed = TRUE,
        accept = 1
      )
      session$flushReact()
      expect_match(output$panel$html, "Accepted", fixed = TRUE)
      expect_identical(mem_rev(), 1L)

      session$setInputs(reason = "Withdrawn after follow-up", withdraw = 1)
      session$flushReact()
      expect_match(output$panel$html, "Withdrawn", fixed = TRUE)
      expect_identical(mem_rev(), 2L)
    }
  )
})

test_that("tempest_mcp_app() preserves read-only Graft input summary fields", {
  skip_if_not_installed("shinymcp")
  skip_if_not_installed("shiny")
  fixture <- test_promotion_storm_fixture(
    run_id = "mcp-graft-1",
    evidence_text = "Read-only MCP card evidence."
  )
  card <- tempest_mcp_app(fixture$research)
  expect_r6_class(card, "McpApp")
  overview <- card$run_tool("review_tempest_research")
  expect_true("input_selection_id" %in% names(overview$structuredContent))
  expect_true("input_purpose" %in% names(overview$structuredContent))
})

test_that("shiny_active_report_product() tracks published STORM and restored Co-STORM reports", {
  skip_if_not_installed("shiny")
  env <- tempest:::tempest_shiny_module_env()
  storm_fixture <- test_promotion_storm_fixture(
    run_id = "active-storm-1",
    evidence_text = "Storm active report evidence."
  )
  costorm_fixture <- test_promotion_costorm_fixture()
  storm_prod <- storm_fixture$research
  costorm_ses <- costorm_fixture$research

  expect_null(env$shiny_active_report_product(
    report_md = NULL,
    report_workspace = NULL,
    costorm_session = costorm_ses,
    storm_product = storm_prod
  ))

  # STORM report matches immediately upon publication without a navigation click.
  expect_identical(
    env$shiny_active_report_product(
      report_md = storm_prod@report_md,
      report_workspace = storm_prod@workspace,
      costorm_session = costorm_ses,
      storm_product = storm_prod
    ),
    storm_prod
  )

  # Restored or published Co-STORM report matches its exact workspace and markdown.
  expect_identical(
    env$shiny_active_report_product(
      report_md = tempest_report(costorm_ses),
      report_workspace = tempest:::tempest_session_workspace(costorm_ses),
      costorm_session = costorm_ses,
      storm_product = storm_prod
    ),
    costorm_ses
  )
})

test_that("shiny_product_graft_consultations() reads moderator and expert manager chats", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ellmer")
  env <- tempest:::tempest_shiny_module_env()
  costorm_ses <- test_promotion_costorm_fixture()$research

  make_graft_chat <- function(stream, selection_id) {
    tr <- ellmer::ContentToolResult(
      value = paste("Recalled", stream),
      extra = list(
        graft = list(
          status = "accepted",
          stream = stream,
          selection_id = selection_id,
          decision_id = paste0("dec-", stream),
          purpose = "research"
        )
      )
    )
    turn <- ellmer::UserTurn(contents = list(tr))
    chat_env <- new.env(parent = emptyenv())
    chat_env$get_turns <- function() list(turn)
    chat_env
  }

  ses_priv <- costorm_ses$.__enclos_env__$private
  ses_priv$chats_value$moderator <- make_graft_chat("mod-stream", "sel-mod-1")
  mgr <- tempest:::tempest_session_expert_manager(costorm_ses)
  mgr_priv <- mgr$.__enclos_env__$private
  assign(
    "expert-session-1",
    make_graft_chat("expert-stream", "sel-exp-1"),
    envir = mgr_priv$sessions
  )

  consultations <- env$shiny_product_graft_consultations(costorm_ses)
  expect_setequal(consultations$stream, c("mod-stream", "expert-stream"))
  expect_setequal(consultations$selection_id, c("sel-mod-1", "sel-exp-1"))
})

test_that("Co-STORM report memory panel rejects stale withdrawal when stream head changed", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("graft")
  skip_if_not("graft_store" %in% getNamespaceExports("graft"))

  store <- graft::graft_store(
    file.path(withr::local_tempdir(), "stale-withdraw-memory"),
    create = TRUE
  )
  f1 <- test_promotion_storm_fixture(
    run_id = "stale-1",
    evidence_text = "First report evidence."
  )
  f2 <- test_promotion_storm_fixture(
    run_id = "stale-2",
    evidence_text = "Second report evidence."
  )
  env <- tempest:::tempest_shiny_module_env()
  ctx <- list(
    store = store,
    store_path = tempest:::tempest_graft_store_path(store),
    purpose = "research",
    actor = "alice",
    eligible = function(stream) TRUE
  )
  prod <- shiny::reactiveVal(f1$research)
  mem_rev <- shiny::reactiveVal(0L)

  shiny::testServer(
    function(id) {
      env$mod_memory_report_server(
        id,
        memory_ctx = ctx,
        product = prod,
        memory_revision = mem_rev
      )
    },
    {
      session$setInputs(
        stream = "shared-stream",
        actor = "alice",
        reason = "Initial accept",
        reviewed = TRUE,
        accept = 1
      )
      session$flushReact()
      expect_identical(mem_rev(), 1L)

      # Out-of-band correction advances the stream head before withdrawal.
      sel2 <- tempest_publish_artifact_research(f2$research, store)
      tempest:::tempest_graft_accept_selection(
        store,
        selection_id = sel2,
        stream = "shared-stream",
        actor = "bob",
        reason = "Superseding correction",
        purpose = "research"
      )

      session$setInputs(reason = "Stale withdraw", withdraw = 1)
      session$flushReact()
      # Must fail closed without advancing memory_revision or withdrawing bob's head.
      expect_identical(mem_rev(), 1L)
      active <- tempest:::tempest_graft_selection_status(
        store,
        selection_id = sel2,
        purpose = "research"
      )
      expect_identical(active$status, "accepted")

      # Stale re-acceptance from the older displayed view must also fail closed.
      session$setInputs(
        existing_stream = "shared-stream",
        reason = "Stale re-accept",
        reviewed = TRUE,
        accept = 2
      )
      session$flushReact()
      expect_identical(mem_rev(), 1L)
      active_after <- tempest:::tempest_graft_selection_status(
        store,
        selection_id = sel2,
        purpose = "research"
      )
      expect_identical(active_after$status, "accepted")
    }
  )
})

test_that("Co-STORM report memory panel recovers retained selection in a fresh Shiny session", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("graft")
  skip_if_not("graft_store" %in% getNamespaceExports("graft"))

  store <- graft::graft_store(
    file.path(withr::local_tempdir(), "restore-memory"),
    create = TRUE
  )
  fixture <- test_promotion_storm_fixture(
    run_id = "restore-mem-1",
    evidence_text = "Restored session accepted evidence."
  )
  sel <- tempest_publish_artifact_research(fixture$research, store)
  tempest:::tempest_graft_accept_selection(
    store,
    selection_id = sel,
    stream = "restored-stream",
    actor = "alice",
    reason = "Accepted in prior session",
    purpose = "research"
  )

  env <- tempest:::tempest_shiny_module_env()
  ctx <- list(
    store = store,
    store_path = tempest:::tempest_graft_store_path(store),
    purpose = "research",
    actor = "alice",
    eligible = function(stream) TRUE
  )
  prod <- shiny::reactiveVal(fixture$research)
  mem_rev <- shiny::reactiveVal(0L)

  # Fresh Shiny session with an empty session-local cache immediately recovers
  # the accepted selection, shows decision history, and allows withdrawal.
  shiny::testServer(
    function(id) {
      env$mod_memory_report_server(
        id,
        memory_ctx = ctx,
        product = prod,
        memory_revision = mem_rev
      )
    },
    {
      session$flushReact()
      expect_identical(session$returned$selection(), sel)
      expect_match(output$panel$html, "Accepted", fixed = TRUE)
      expect_match(output$panel$html, "Decision history (1)", fixed = TRUE)
      expect_match(
        output$panel$html,
        "Withdraw from active memory",
        fixed = TRUE
      )

      session$setInputs(reason = "Withdrawn after restore", withdraw = 1)
      session$flushReact()
      expect_match(output$panel$html, "Withdrawn", fixed = TRUE)
      expect_identical(mem_rev(), 1L)
    }
  )
})
