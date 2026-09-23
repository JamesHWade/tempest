test_that("contradictory research executes and preserves the prior evidence", {
  skip_if_not_installed("graft")
  first <- storm_product_fixture()
  original <- tempest_run(
    "Progress events",
    config = first$config,
    retriever = first$retriever,
    n_experts = 1,
    max_questions_per_perspective = 1,
    run_id = "original-research",
    verbose = FALSE
  )
  directory <- withr::local_tempdir()
  store <- test_graft_store(
    file.path(directory, "artifacts"),
    create = TRUE
  )
  old_selection <- tempest_publish_artifact_research(original, store)
  accept <- function(key, expected, selection) {
    test_graft_decide(
      store,
      "daily",
      key,
      expected,
      selection,
      "accept",
      "reviewer",
      "Reviewed exact research evidence",
      "briefing"
    )
  }
  accepted <- accept("original", NULL, old_selection)
  old <- tempest_read_artifact_research(store, old_selection)
  expect_match(
    tempest_report(original),
    "STORM progress emits stage events.",
    fixed = TRUE
  )
  expect_identical(
    old$bundle@records$Claim[[1L]]$statement_text,
    "STORM progress emits stage events."
  )
  old_refs <- test_graft_read_selection(store, old_selection)
  store <- test_graft_store(file.path(directory, "artifacts"))
  knowledge <- tempest_reuse_artifact_research(
    store,
    "daily",
    accepted$id,
    "briefing",
    eligible = \(event) TRUE
  )
  next_day <- artifact_correction_fixture()
  correction <- tempest_run(
    "Progress events",
    config = next_day$config,
    retriever = next_day$retriever,
    knowledge = knowledge,
    n_experts = 1,
    max_questions_per_perspective = 1,
    output_dir = file.path(directory, "runs"),
    run_id = "corrected-research",
    verbose = FALSE
  )
  expect_identical(correction@manifest@status, "succeeded")
  expect_match(tempest_report(correction), next_day$statement, fixed = TRUE)
  expect_identical(
    grepl("No material change", tempest_report(correction), fixed = TRUE),
    FALSE
  )
  expect_identical(
    correction@workspace$artifact_selection,
    knowledge@artifact_selection
  )
  expect_identical(
    correction@manifest@artifact_selection,
    knowledge@artifact_selection
  )
  corrected_selection <- tempest_publish_artifact_research(correction, store)
  proposed <- tempest_read_artifact_research(store, corrected_selection)
  expect_identical(
    test_graft_read_decision(store, "daily"),
    accepted
  )
  expect_identical(
    proposed$bundle@records$Claim[[1L]]$statement_text,
    next_day$statement
  )
  expect_match(
    proposed$contents[[paste0("Source:", next_day$source@resource_id)]],
    next_day$source@content,
    fixed = TRUE
  )
  expect_identical(
    proposed$bundle@research_manifest$artifact_selection,
    knowledge@artifact_selection
  )
  reviewed <- accept("correction", accepted$id, corrected_selection)
  expect_identical(identical(reviewed$selection, accepted$selection), FALSE)
  expect_error(
    tempest_knowledge_argument(knowledge),
    "current acceptance",
    class = "tempest_knowledge_error"
  )
  expect_identical(tempest_read_artifact_research(store, old_selection), old)
  expect_identical(
    test_graft_read_selection(store, old_selection),
    old_refs
  )
  expect_identical(
    test_graft_read_decision(store, "daily", accepted$id),
    accepted
  )
  expect_identical(length(old$bundle@records$ClaimSupport) > 0L, TRUE)
  old_supports <- vapply(
    old$bundle@records$ClaimSupport,
    `[[`,
    character(1),
    "tempest_claim_support_id"
  )
  new_supports <- vapply(
    proposed$bundle@records$ClaimSupport,
    `[[`,
    character(1),
    "tempest_claim_support_id"
  )
  expect_length(intersect(old_supports, new_supports), 0L)
  fresh <- tempest_reuse_artifact_research(
    store,
    "daily",
    reviewed$id,
    "briefing",
    eligible = \(event) TRUE
  )
  expect_identical(fresh@artifact_selection$selection_id, corrected_selection)
  expect_identical(accept("original", NULL, old_selection), accepted)
  expect_identical(
    test_graft_read_decision(store, "daily"),
    reviewed
  )
  reopened <- callr::r(
    function(checkout, path, old_selection, decision) {
      if (!is.null(checkout)) {
        pkgload::load_all(checkout, quiet = TRUE)
      }
      store <- test_graft_store(path)
      old <- tempest::tempest_read_artifact_research(store, old_selection)
      current <- tempest::tempest_reuse_artifact_research(
        store,
        "daily",
        decision,
        "briefing",
        eligible = \(event) TRUE
      )
      list(
        report = old$report_md,
        contents = old$contents,
        supports = old$bundle@records$ClaimSupport,
        selection = current@artifact_selection
      )
    },
    args = list(
      checkout = if (pkgload::is_dev_package("tempest")) {
        normalizePath(test_path("../.."))
      } else {
        NULL
      },
      path = file.path(directory, "artifacts"),
      old_selection = old_selection,
      decision = reviewed$id
    )
  )
  expect_identical(reopened$report, old$report_md)
  expect_identical(reopened$contents, old$contents)
  expect_identical(reopened$supports, old$bundle@records$ClaimSupport)
  expect_identical(reopened$selection, fresh@artifact_selection)
  review <- tempest_trajectory_review(
    correction,
    store = store,
    selection = corrected_selection,
    stream = "daily",
    decision = reviewed$id
  )
  data <- tempest_trajectory_review_data(review)
  expect_identical(data$knowledge$input_selection$selection_id, old_selection)
  expect_identical(
    data$knowledge$input_selection$reported_decision$decision_id,
    accepted$id
  )
  expect_identical(
    data$knowledge$input_selection$reported_decision$verification,
    "unverified"
  )
  expect_identical(data$knowledge$proposal$selection_id, corrected_selection)
  expect_identical(data$knowledge$acceptance$decision_id, reviewed$id)
  withdrawal <- test_graft_decide(
    store,
    "daily",
    "withdraw",
    reviewed$id,
    corrected_selection,
    "withdraw",
    "host",
    "Reconsidered",
    "briefing"
  )
  expect_identical(
    tempest_trajectory_review(
      correction,
      store = store,
      selection = corrected_selection,
      stream = "daily",
      decision = reviewed$id
    ),
    review
  )
  expect_error(
    tempest_reuse_artifact_research(
      store,
      "daily",
      reviewed$id,
      "briefing",
      eligible = \(event) TRUE
    ),
    class = "tempest_knowledge_error"
  )
})
