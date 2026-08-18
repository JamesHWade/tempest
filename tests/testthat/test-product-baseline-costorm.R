test_that("Co-STORM warmup and one moderator turn are frozen", {
  baseline_local_ids()
  fixture <- costorm_product_baseline_fixture()
  semantics <- baseline_costorm_semantics(fixture)
  definition_ids <- vapply(
    semantics$report_citations$definitions,
    `[[`,
    character(1),
    "citation_id"
  )

  expect_identical(semantics$report_citations$uses, semantics$source_ids)
  expect_identical(definition_ids, semantics$source_ids)
  expect_snapshot(baseline_snapshot_json(semantics))
})
