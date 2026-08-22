test_that("session save refuses to overwrite a non-bundle directory", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("jsonlite")
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  session <- tempest_session(
    "Guarded save",
    config = cfg,
    experts = list(tempest_expert(
      name = "Guard Expert",
      title = "Persistence guard",
      description = "Protect bundle replacement.",
      instructions = "Refuse unsafe replacement paths."
    ))
  )

  not_a_bundle <- file.path(withr::local_tempdir(), "important")
  dir.create(not_a_bundle)
  keep <- file.path(not_a_bundle, "keep.txt")
  writeLines("do not delete", keep)

  expect_error(
    tempest_session_save(session, not_a_bundle, overwrite = TRUE),
    class = "tempest_session_save_error"
  )
  expect_true(file.exists(keep))

  invalid_bundle <- file.path(dirname(not_a_bundle), "invalid-bundle")
  dir.create(invalid_bundle)
  manifest_path <- file.path(invalid_bundle, "session.json")
  precious_path <- file.path(invalid_bundle, "precious.txt")
  writeLines("{", manifest_path)
  writeLines("preserve these bytes", precious_path)
  bundle_bytes <- function(path) {
    files <- sort(list.files(path, recursive = TRUE, all.files = TRUE))
    stats::setNames(
      lapply(files, function(file) {
        file_path <- file.path(path, file)
        readBin(file_path, what = "raw", n = file.info(file_path)$size)
      }),
      files
    )
  }
  before <- bundle_bytes(invalid_bundle)

  expect_error(
    tempest_session_save(session, invalid_bundle, overwrite = TRUE),
    class = "tempest_session_save_error"
  )
  expect_identical(bundle_bytes(invalid_bundle), before)
})

test_that("session saves reject symbolic-link bundle roots", {
  skip_on_os("windows")
  skip_if_not_installed("ellmer")
  skip_if_not_installed("jsonlite")
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  root <- withr::local_tempdir()

  session_target <- file.path(root, "session-target")
  original_session <- tempest_session(
    "Original session root",
    config = cfg,
    experts = list(test_expert(expert_id = "expert.root-original"))
  )
  tempest_session_save(original_session, session_target)
  session_alias <- file.path(root, "session-alias")
  expect_identical(file.symlink(session_target, session_alias), TRUE)
  replacement_session <- tempest_session(
    "Replacement session root",
    config = cfg,
    experts = list(test_expert(expert_id = "expert.root-replacement"))
  )

  expect_error(
    tempest_session_save(
      replacement_session,
      session_alias,
      overwrite = TRUE
    ),
    class = "tempest_session_save_error"
  )
  expect_identical(Sys.readlink(session_alias), session_target)
  restored_session <- tempest_session_resume(session_target, config = cfg)
  expect_identical(restored_session$topic, "Original session root")
})

test_that("Tempest session bundle resume reports classed file errors", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("jsonlite")
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  expert <- tempest_expert(
    name = "Dr. Broken",
    title = "Persistence expert",
    description = "Failure handling",
    instructions = "Exercise classed persistence failures."
  )
  session <- tempest_session(
    "Broken bundle",
    config = cfg,
    experts = list(expert)
  )
  root <- withr::local_tempdir()
  fresh_bundle <- function(name) {
    bundle_dir <- file.path(root, name)
    tempest_session_save(session, bundle_dir)
    bundle_dir
  }

  bundle_dir <- fresh_bundle("missing-experts")
  unlink(file.path(bundle_dir, "experts.json"))
  expect_error(
    tempest_session_resume(bundle_dir, config = cfg),
    class = "tempest_session_restore_error"
  )

  bundle_dir <- fresh_bundle("malformed-claims")
  writeLines("{", file.path(bundle_dir, "workspace/proposed_claims.json"))
  expect_error(
    tempest_session_resume(bundle_dir, config = cfg),
    class = "tempest_session_restore_error"
  )

  bundle_dir <- fresh_bundle("missing-transcript")
  unlink(file.path(bundle_dir, "transcript.json"))
  expect_error(
    tempest_session_resume(bundle_dir, config = cfg),
    class = "tempest_session_restore_error"
  )

  bundle_dir <- fresh_bundle("unsupported-schema")
  manifest_path <- file.path(bundle_dir, "session.json")
  manifest <- tempest:::tempest_product_read_json(manifest_path)
  manifest$schema_version <- 999L
  tempest:::tempest_product_write_json(manifest_path, manifest)
  expect_error(
    tempest_session_resume(bundle_dir, config = cfg),
    class = "tempest_session_restore_error"
  )
})
