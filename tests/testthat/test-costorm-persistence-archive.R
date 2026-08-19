test_that("extracted Co-STORM archives require the exact current product", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("jsonlite")
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  root <- withr::local_tempdir()
  make_bundle <- function(name) {
    path <- file.path(root, name)
    session <- tempest_session(
      "Archive validation",
      config = cfg,
      experts = list(test_expert(expert_id = paste0("expert.", name)))
    )
    tempest_session_save(session, path)
    path
  }

  current <- make_bundle("current")
  expect_identical(
    tempest:::tempest_costorm_archive_read(current),
    normalizePath(current, winslash = "/", mustWork = TRUE)
  )

  invalid_headers <- list(
    old_schema = function(manifest) {
      manifest$schema_version <- 8L
      manifest
    },
    future_schema = function(manifest) {
      manifest$schema_version <- 10L
      manifest
    },
    wrong_mode = function(manifest) {
      manifest$bundle_type <- "storm"
      manifest
    },
    wrong_status = function(manifest) {
      manifest$bundle_status <- "partial"
      manifest
    },
    extra_field = function(manifest) {
      manifest$runtime <- list()
      manifest
    }
  )
  for (name in names(invalid_headers)) {
    bundle <- make_bundle(paste0("header-", name))
    manifest_path <- file.path(bundle, "session.json")
    manifest <- tempest:::tempest_product_read_json(manifest_path)
    manifest <- invalid_headers[[name]](manifest)
    tempest:::tempest_product_write_json(manifest_path, manifest)
    expect_error(
      tempest:::tempest_costorm_archive_read(bundle),
      class = "tempest_session_restore_error",
      info = name
    )
  }

  tampered <- make_bundle("tampered")
  writeLines("[]", file.path(tampered, "experts.json"))
  expect_error(
    tempest:::tempest_costorm_archive_read(tampered),
    class = "tempest_session_restore_error"
  )

  unsafe <- make_bundle("unsafe-path")
  manifest_path <- file.path(unsafe, "session.json")
  manifest <- tempest:::tempest_product_read_json(manifest_path)
  old_file <- manifest$files[[1L]]
  manifest$files[[1L]] <- "../outside.json"
  names(manifest$checksums)[names(manifest$checksums) == old_file] <-
    "../outside.json"
  tempest:::tempest_product_write_json(manifest_path, manifest)
  expect_error(
    tempest:::tempest_costorm_archive_read(unsafe),
    class = "tempest_session_restore_error"
  )

  extra <- make_bundle("extra-file")
  writeLines("undeclared", file.path(extra, "extra.txt"))
  expect_error(
    tempest:::tempest_costorm_archive_read(extra),
    class = "tempest_session_restore_error"
  )
})
