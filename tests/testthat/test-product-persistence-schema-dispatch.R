test_that("persistence schema dispatch rejects fractional versions", {
  cfg <- tempest_config()
  session_class <- "tempest_session_restore_error"
  run_class <- "tempest_run_restore_error"

  expect_error(
    tempest_session_restore(list(schema_version = 7.5)),
    class = session_class
  )
  expect_error(
    tempest:::tempest_session_bundle_validate_manifest(
      withr::local_tempdir(),
      list(schema_version = 7.5)
    ),
    class = session_class
  )
  expect_error(
    tempest:::tempest_storm_bundle_validate_manifest(
      withr::local_tempdir(),
      list(schema_version = 6.5)
    ),
    class = run_class
  )
  expect_error(
    tempest:::tempest_storm_restore_workspace(
      list(schema_version = 6.5)
    ),
    class = run_class
  )
  expect_error(
    tempest:::tempest_storm_restore_manifest(
      list(schema_version = 6.5),
      tempest_research_workspace(),
      tempest:::tempest_storm_state("Fractional schema"),
      cfg,
      program_set = NULL
    ),
    class = run_class
  )
})
