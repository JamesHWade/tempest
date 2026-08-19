test_that("product bundle installation rolls back through one narrow seam", {
  root <- withr::local_tempdir()
  bundle_dir <- file.path(root, "bundle")
  staging_dir <- file.path(root, "staging")
  dir.create(bundle_dir)
  dir.create(staging_dir)
  writeLines("original", file.path(bundle_dir, "value.txt"))
  writeLines("replacement", file.path(staging_dir, "value.txt"))

  local_mocked_bindings(
    tempest_product_install_bundle = function(staging_dir, bundle_dir) FALSE
  )

  expect_error(
    tempest:::tempest_product_atomic_commit_bundle(
      staging_dir,
      bundle_dir,
      class = c("tempest_test_install_error", "tempest_error")
    ),
    class = "tempest_test_install_error"
  )
  expect_identical(readLines(file.path(bundle_dir, "value.txt")), "original")
  remnants <- list.files(root, all.files = TRUE, no.. = TRUE)
  expect_disjoint(remnants, grep("backup", remnants, value = TRUE))
})

test_that("product persistence does not consult retired global options", {
  functions <- list(
    tempest:::tempest_session_bundle_write_json,
    tempest:::tempest_session_bundle_write_text,
    tempest:::tempest_storm_bundle_write_json,
    tempest:::tempest_storm_bundle_write_text,
    tempest:::tempest_session_bundle_optional_json,
    tempest:::tempest_session_resume_internal
  )
  implementation <- paste(
    unlist(lapply(functions, function(fn) deparse(body(fn)))),
    collapse = "\n"
  )

  expect_no_match(implementation, "tempest.session_write_hook", fixed = TRUE)
  expect_no_match(implementation, "tempest.run_write_hook", fixed = TRUE)
  expect_no_match(
    implementation,
    "tempest.session_partial_recovery",
    fixed = TRUE
  )
})
