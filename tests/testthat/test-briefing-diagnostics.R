test_that("completed briefing reviews compose with scans diagnostics", {
  skip_if_not_installed("scans", "0.0.0.9000")
  fixture <- test_promotion_bundle()
  review <- tempest_trajectory_review(
    fixture$research,
    promotion_bundle = fixture$bundle
  )
  trajectory <- scans::as_trajectory_tempest(review)

  expect_identical(scans::trajectory_info(trajectory)$source_type, "tempest")
  expect_s3_class(scans::summarize_trajectories(trajectory), "data.frame")
  expect_s3_class(scans::scan_trajectories(trajectory), "data.frame")
})
