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
      manifest$schema_version <- 9L
      manifest
    },
    future_schema = function(manifest) {
      manifest$schema_version <- 12L
      manifest
    },
    wrong_bundle_type = function(manifest) {
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
    },
    old_manifest_schema = function(manifest) {
      manifest$research_manifest$schema_version <- 2L
      manifest
    },
    future_manifest_schema = function(manifest) {
      manifest$research_manifest$schema_version <- 4L
      manifest
    },
    wrong_manifest_mode = function(manifest) {
      manifest$research_manifest$mode <- "storm"
      manifest
    },
    wrong_manifest_session = function(manifest) {
      manifest$research_manifest$research_run_id <- "session.other"
      manifest
    },
    invalid_manifest_config = function(manifest) {
      manifest$research_manifest$config_digest <- "sha256:invalid"
      manifest
    },
    mismatched_workspace = function(manifest) {
      manifest$workspace$base_snapshot_id <- "snapshot.other"
      manifest
    },
    credential_manifest = function(manifest) {
      manifest$topic <- "sk-proj-archive-secret-abcdefghijklmnopqrstuvwxyz"
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

  credential_sidecar <- make_bundle("credential-sidecar")
  manifest_path <- file.path(credential_sidecar, "session.json")
  experts_path <- file.path(credential_sidecar, "experts.json")
  manifest <- tempest:::tempest_product_read_json(manifest_path)
  experts <- tempest:::tempest_product_read_json(experts_path)
  experts[[1L]]$instructions <-
    "Use sk-proj-sidecar-secret-abcdefghijklmnopqrstuvwxyz"
  tempest:::tempest_product_write_json(experts_path, experts)
  manifest$checksums[["experts.json"]] <-
    tempest:::tempest_product_bundle_checksum(
      credential_sidecar,
      "experts.json"
    )
  tempest:::tempest_product_write_json(manifest_path, manifest)
  expect_error(
    tempest:::tempest_costorm_archive_read(credential_sidecar),
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

test_that("extracted Co-STORM archive roots cannot be symbolic links", {
  skip_on_os("windows")
  skip_if_not_installed("ellmer")
  skip_if_not_installed("jsonlite")
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  root <- withr::local_tempdir()
  bundle <- file.path(root, "bundle")
  alias <- file.path(root, "bundle-alias")
  session <- tempest_session(
    "Archive root symlink",
    config = cfg,
    experts = list(test_expert(expert_id = "expert.archive-symlink"))
  )
  tempest_session_save(session, bundle)
  expect_identical(file.symlink(bundle, alias), TRUE)

  expect_error(
    tempest:::tempest_costorm_archive_read(alias),
    class = "tempest_session_restore_error"
  )
})
