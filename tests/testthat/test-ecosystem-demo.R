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
    eligible = function(event) TRUE
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
      actor = "reviewer",
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
    actor = "reviewer",
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
    eligible = function(event) TRUE
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
