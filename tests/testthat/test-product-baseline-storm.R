test_that("default dsprrr STORM semantic outcomes are frozen", {
  baseline_local_ids()
  fixture <- storm_product_baseline_fixture()
  semantics <- baseline_storm_semantics(fixture)
  definition_ids <- vapply(
    semantics$citations$definitions,
    `[[`,
    character(1),
    "citation_id"
  )

  expect_identical(semantics$citations$uses, semantics$source_ids)
  expect_identical(definition_ids, semantics$source_ids)
  expect_equal(
    intersect(
      names(fixture$result),
      c("store", "artifact_catalog", "workflow_run")
    ),
    character()
  )
  expect_identical(fixture$result$workspace, fixture$store)
  expect_identical(
    fixture$result$manifest@research_run_id,
    "storm-product-baseline"
  )
  expect_identical(fixture$result$manifest@status, "succeeded")
  expect_snapshot(baseline_snapshot_json(semantics))
})
