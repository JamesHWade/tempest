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

  registered <- character()
  fake_role_chat <- list(
    get_tools = function() list(),
    register_tools = function(tools) {
      registered <<- c(registered, vapply(tools, \(t) t@name, character(1)))
    }
  )
  base_cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_role_chat
  )
  wrapped_cfg <- env$shiny_memory_wrap_config(base_cfg, ctx, use_tools = TRUE)
  wrapped_cfg@chat_fn("moderator", "openai/gpt-5.6-sol", "sys", "none")
  expect_setequal(
    registered,
    c("list_project_memory", "recall_project_memory")
  )

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
