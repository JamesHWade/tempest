test_that("actual ecosystem execution retains, corrects, reopens and withdraws research", {
  skip_if_not_installed("graft")
  skip_if_not_installed("callr")
  skip_if_not_installed("shinymcp")
  skip_if_not_installed("shiny")
  helper <- ecosystem_demo_helper()
  directory <- tempfile("tempest-ecosystem-")
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  first <- helper$tempest_demo_research(directory)
  review <- tempest_trajectory_review_data(tempest_trajectory_review(first))
  expect_gte(review$agent_runs$total, 1L)
  expect_gte(review$stages$total, 5L)
  expect_identical(
    vapply(
      review$stages$items,
      function(stage) stage$fallback_taken,
      logical(1)
    ),
    rep(FALSE, length(review$stages$items))
  )
  expect_all_true(vapply(
    review$stages$items,
    function(stage) nzchar(stage$program_artifact_id),
    logical(1)
  ))
  expect_identical(
    tempest_claims(first)$claim_text,
    "The synthetic pilot recovered 82% of the input polymer."
  )

  store <- graft::graft_store(file.path(directory, "evidence"), create = TRUE)
  selection <- tempest_publish_artifact_research(first, store)
  worker <- new.env(parent = globalenv())
  sys.source(
    system.file("examples", "ellmerverse", "worker.R", package = "tempest"),
    worker
  )
  # A worker restores the reviewed product rather than serializing its authority.
  accepted <- callr::r(
    worker$tempest_demo_worker,
    args = list(
      operation = "accept",
      directory = directory,
      candidate = list(
        run_id = "pilot-initial",
        correction = FALSE,
        input_decision_id = NULL
      ),
      review_id = review$review_id,
      reason = "Reviewed synthetic evidence",
      expected = NULL,
      package_path = if (pkgload::is_dev_package("tempest")) {
        pkgload::pkg_path()
      } else {
        NULL
      }
    ),
    libpath = .libPaths()
  )$value
  expect_identical(accepted@selection, selection)
  knowledge <- tempest_reuse_artifact_research(
    store,
    "synthetic-pilot",
    accepted@id,
    "demo-briefing",
    eligible = helper$tempest_demo_eligible
  )
  correction <- helper$tempest_demo_research(
    directory,
    correction = TRUE,
    knowledge = knowledge
  )
  first_card <- tempest_mcp_app(first)
  correction_card <- tempest_mcp_app(correction)
  expect_identical(
    identical(first_card$resource_uri(), correction_card$resource_uri()),
    FALSE
  )
  expect_identical(
    correction_card$call_tool(
      "inspect_tempest_claim",
      list(
        claim_id = tempest_claims(correction)$claim_id[[1L]]
      )
    )$data$evidence$quote,
    tempest_claim_supports(correction)$quote
  )
  proposal <- tempest_publish_artifact_research(correction, store)
  expect_identical(
    graft::graft_recall(
      store,
      "synthetic-pilot",
      "demo-briefing",
      TRUE
    )@decision@id,
    accepted@id
  )
  expect_identical(
    tempest_read_artifact_research(store, selection)$report_md,
    tempest_report(first)
  )
  expect_match(tempest_report(correction), "74%", fixed = TRUE)
  expect_identical(identical(proposal, selection), FALSE)
  expect_error(
    graft::graft_accept(
      store,
      graft::graft_read_selection(store, proposal),
      "synthetic-pilot",
      expected = NULL,
      key = "stale",
      actor = "local-demo-reviewer",
      reason = "Stale review",
      purpose = "demo-briefing"
    ),
    class = "graft_artifact_error"
  )
  corrected <- graft::graft_accept(
    store,
    graft::graft_read_selection(store, proposal),
    "synthetic-pilot",
    expected = accepted@id,
    key = "corrected",
    actor = "local-demo-reviewer",
    reason = "Reviewed the correction",
    purpose = "demo-briefing"
  )

  proof <- callr::r(
    function(directory, decision_id, package_path) {
      if (nzchar(package_path)) {
        pkgload::load_all(package_path, quiet = TRUE)
      }
      helper <- new.env(parent = globalenv())
      sys.source(
        system.file(
          "examples",
          "ellmerverse",
          "fixture.R",
          package = "tempest"
        ),
        helper
      )
      helper$tempest_demo_reopen(directory, decision_id)
    },
    args = list(
      directory = directory,
      decision_id = corrected@id,
      package_path = if (pkgload::is_dev_package("tempest")) {
        pkgload::pkg_path()
      } else {
        ""
      }
    ),
    libpath = .libPaths()
  )
  expect_identical(proof$exact_restart, TRUE)
  expect_identical(proof$selection_id, proposal)
  expect_match(
    paste(proof$sources$content_text, collapse = "\n"),
    "74%",
    fixed = TRUE
  )

  current <- tempest_reuse_artifact_research(
    store,
    "synthetic-pilot",
    corrected@id,
    "demo-briefing",
    eligible = helper$tempest_demo_eligible
  )
  graft::graft_withdraw(
    store,
    "synthetic-pilot",
    expected = corrected@id,
    key = "withdraw",
    actor = "reviewer",
    reason = "Review again"
  )
  expect_error(
    helper$tempest_demo_reopen(directory, corrected@id),
    class = "tempest_knowledge_error"
  )
  inputs <- helper$tempest_demo_inputs()
  expect_error(
    tempest_session(
      "Cached acceptance",
      config = inputs$config,
      experts = inputs$experts,
      knowledge = current
    ),
    class = "tempest_knowledge_error"
  )
  expect_identical(
    tempest_read_artifact_research(store, proposal)$report_md,
    tempest_report(correction)
  )
  resumed <- helper$tempest_demo_research(directory)
  expect_identical(tempest_report(resumed), tempest_report(first))
})

test_that("the replay invokes Deputy permission checks before tool execution", {
  helper <- ecosystem_demo_helper()
  calls <- 0L
  tool <- ellmer::tool(
    function(source_id) {
      calls <<- calls + 1L
      list(source_id = source_id)
    },
    name = "add_proposed_claim",
    description = "Record a provisional claim in this test workspace.",
    arguments = list(source_id = ellmer::type_string()),
    annotations = ellmer::tool_annotations(
      read_only_hint = FALSE,
      destructive_hint = FALSE,
      open_world_hint = FALSE
    )
  )
  chat <- helper$TempestReplayChat$new(
    "Synthetic statement",
    "source-1",
    tool_name = "add_proposed_claim"
  )
  agent <- deputy::Agent$new(
    chat = chat,
    tools = list(tool),
    permissions = deputy::Permissions(
      mode = "plan",
      file_read = FALSE,
      file_write = FALSE,
      bash = FALSE,
      r_code = FALSE,
      web = FALSE,
      install_packages = FALSE,
      tool_allowlist = "add_proposed_claim"
    ),
    working_dir = withr::local_tempdir()
  )
  result <- agent$run_sync("Attempt a provisional write")
  expect_identical(calls, 0L)
  expect_length(deputy::result_tool_calls(result), 1L)
  denied <- Filter(
    function(event) {
      identical(event$type, "permission") && identical(event$decision, "deny")
    },
    result$events
  )
  expect_length(denied, 1L)
})

test_that("the demo refuses evidence accepted outside its reviewer policy", {
  skip_if_not_installed("graft")
  helper <- ecosystem_demo_helper()
  directory <- withr::local_tempdir()
  store <- graft::graft_store(file.path(directory, "evidence"), create = TRUE)
  research <- ecosystem_demo_product()
  selection <- tempest_publish_artifact_research(research, store)
  decision <- graft::graft_accept(
    store,
    graft::graft_read_selection(store, selection),
    "synthetic-pilot",
    expected = NULL,
    key = "outside-demo-reviewer",
    actor = "another-reviewer",
    reason = "Reviewed elsewhere",
    purpose = "demo-briefing"
  )
  expect_error(
    helper$tempest_demo_reopen(directory, decision@id),
    class = "graft_ineligible_error"
  )
  expect_identical(
    tempest_read_artifact_research(store, selection)$report_md,
    tempest_report(research)
  )
})
