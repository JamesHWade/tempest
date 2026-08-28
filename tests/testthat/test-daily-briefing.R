test_that("the daily briefing composes review, diagnostics, and acceptance", {
  skip_if_not_installed("graft")
  skip_if_not_installed("scans", "0.0.0.9000")
  fixture <- test_promotion_bundle()
  store <- test_promotion_store()
  withr::defer(graft::graft_close(store))

  report <- tempest_report(fixture$research)
  sources <- tempest_sources(fixture$research)
  claims <- tempest_claims(fixture$research)
  supports <- tempest_claim_supports(fixture$research)
  plan <- tempest_graft_plan(store, fixture$bundle)
  proposed <- tempest_trajectory_review(
    fixture$research,
    promotion_bundle = fixture$bundle
  )
  trajectory <- scans::as_trajectory_tempest(proposed)

  expect_type(report, "character")
  expect_gt(nrow(sources), 0L)
  expect_gt(nrow(claims), 0L)
  expect_gt(nrow(supports), 0L)
  expect_identical(proposed@knowledge$promotion_state, "proposed")
  expect_identical(
    scans::trajectory_info(trajectory)$source_type,
    "tempest"
  )
  expect_s3_class(scans::summarize_trajectories(trajectory), "data.frame")
  expect_s3_class(scans::scan_trajectories(trajectory), "data.frame")

  commit_result <- graft::graft_commit(store, plan)
  receipt <- tempest_promotion_receipt(
    store,
    fixture$bundle,
    plan,
    commit_result
  )
  accepted <- tempest_trajectory_review(
    fixture$research,
    promotion_bundle = fixture$bundle,
    promotion_receipt = receipt
  )

  expect_identical(accepted@knowledge$promotion_state, "accepted")

  next_plan <- tempest_graft_plan(store, fixture$bundle)
  expect_identical(unique(next_plan@changes$action), "match")

  next_view <- graft::graft_at(store, graft::graft_snapshot(store))
  evidence_revisions <- Filter(
    \(revision) {
      revision$class %in%
        c("Claim", "ClaimSupport", "EvidenceSpan", "Source")
    },
    receipt@record_revisions
  )
  record_ids <- vapply(
    evidence_revisions,
    `[[`,
    character(1),
    "record_id"
  )
  knowledge <- tempest_knowledge(next_view, record_ids = record_ids)

  expect_setequal(knowledge@record_ids, record_ids)
})
