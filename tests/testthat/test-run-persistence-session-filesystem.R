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
      expert_id = "expert.guard",
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

test_that("session and STORM saves reject symbolic-link bundle roots", {
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

  program_set <- tempest_program_set()
  storm_target <- file.path(root, "storm-target")
  storm_manifest <- tempest_research_manifest(
    "storm-root-symlink",
    config = cfg,
    programs = tempest:::tempest_program_set_manifest_programs(program_set)
  )
  storm_workspace <- tempest_research_workspace()
  tempest:::tempest_save_run_artifacts(
    storm_target,
    storm_workspace,
    tempest:::tempest_storm_state(
      "STORM root symlink",
      title = "Original STORM title"
    ),
    storm_manifest,
    program_set = program_set,
    config = cfg,
    steps = "research",
    research_strategy = "key_questions"
  )
  storm_alias <- file.path(root, "storm-alias")
  expect_identical(file.symlink(storm_target, storm_alias), TRUE)

  expect_error(
    tempest:::tempest_save_run_artifacts(
      paste0(storm_alias, .Platform$file.sep),
      storm_workspace,
      tempest:::tempest_storm_state(
        "STORM root symlink",
        title = "Replacement STORM title"
      ),
      storm_manifest,
      program_set = program_set,
      config = cfg,
      steps = "research",
      research_strategy = "key_questions"
    ),
    class = "tempest_run_persistence_error"
  )
  expect_identical(Sys.readlink(storm_alias), storm_target)
  restored_storm <- tempest:::tempest_load_run_artifacts(
    storm_target,
    config = cfg,
    program_set = program_set,
    run_id = "storm-root-symlink"
  )
  expect_identical(restored_storm$state$title, "Original STORM title")
})

test_that("Tempest session bundle resume reports classed file errors", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("jsonlite")
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  expert <- tempest_expert(
    expert_id = "expert.broken",
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
  manifest <- tempest:::tempest_read_json_strict(manifest_path)
  manifest$schema_version <- 999L
  tempest:::tempest_write_json(manifest_path, manifest)
  expect_error(
    tempest_session_resume(bundle_dir, config = cfg),
    class = "tempest_session_restore_error"
  )
})

test_that("failed session replacement preserves the previous bundle", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("jsonlite")
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  session <- tempest_session(
    "Original bundle",
    config = cfg,
    experts = list(tempest_expert(
      expert_id = "expert.atomic",
      name = "Dr. Atomic",
      title = "Atomic persistence expert",
      description = "Atomic bundle replacement.",
      instructions = "Keep the last complete bundle intact."
    ))
  )
  root <- withr::local_tempdir()
  bundle_dir <- file.path(root, "bundle")
  tempest_session_save(session, bundle_dir)
  session$add_turn("Replacement", "user", "Replace this bundle.")
  withr::local_options(
    tempest.session_write_hook = function(file) {
      if (identical(file, "workspace/proposed_claims.json")) {
        stop("injected write failure")
      }
    }
  )

  expect_error(
    tempest_session_save(session, bundle_dir, overwrite = TRUE),
    class = "tempest_session_save_error"
  )
  restored <- tempest_session_resume(bundle_dir, config = cfg)

  expect_equal(restored$topic, "Original bundle")
  expect_equal(
    list.files(root, pattern = "staging", all.files = TRUE),
    character()
  )
})

test_that("failed STORM replacement preserves the previous bundle byte-for-byte", {
  skip_if_not_installed("jsonlite")
  root <- withr::local_tempdir()
  run_dir <- file.path(root, "atomic-run")
  cfg <- tempest_config()
  program_set <- tempest_program_set()
  manifest <- tempest_research_manifest(
    "atomic-run",
    config = cfg,
    programs = tempest:::tempest_program_set_manifest_programs(program_set)
  )
  workspace <- tempest_research_workspace()
  original <- tempest:::tempest_storm_state(
    "Atomic STORM",
    title = "Original title"
  )
  tempest:::tempest_save_run_artifacts(
    run_dir,
    workspace,
    original,
    manifest,
    program_set = program_set,
    config = cfg,
    steps = "research",
    research_strategy = "key_questions"
  )
  bundle_bytes <- function(path) {
    files <- sort(list.files(path, recursive = TRUE, all.files = TRUE))
    stats::setNames(
      lapply(files, function(file) {
        readBin(
          file.path(path, file),
          what = "raw",
          n = file.info(
            file.path(path, file)
          )$size
        )
      }),
      files
    )
  }
  before <- bundle_bytes(run_dir)
  replacement <- tempest:::tempest_storm_state(
    "Atomic STORM",
    title = "Replacement title"
  )
  withr::local_options(
    tempest.run_write_hook = function(file) {
      if (identical(file, "stage_records.json")) {
        stop("injected STORM write failure")
      }
    }
  )

  expect_error(
    tempest:::tempest_save_run_artifacts(
      run_dir,
      workspace,
      replacement,
      manifest,
      program_set = program_set,
      config = cfg,
      steps = "research",
      research_strategy = "key_questions"
    ),
    class = "tempest_run_persistence_error"
  )

  expect_identical(bundle_bytes(run_dir), before)
  remnants <- list.files(root, all.files = TRUE, no.. = TRUE)
  remnants <- remnants[grepl("^\\.atomic-run-(staging|backup)-", remnants)]
  expect_identical(remnants, character())
  restored <- tempest:::tempest_load_run_artifacts(
    run_dir,
    config = cfg,
    program_set = program_set,
    run_id = "atomic-run"
  )
  expect_identical(restored$state$title, "Original title")
})
