test_that("STORM persistence verifies complete ProgramSet identity on resume", {
  root <- withr::local_tempdir()
  run_dir <- file.path(root, "run")
  cfg <- tempest_config()
  program_set <- tempest_program_set()
  program_references <-
    tempest:::tempest_program_set_manifest_programs(program_set)
  manifest <- tempest_research_manifest(
    "program-set-resume",
    config = cfg,
    programs = program_references
  )
  state <- tempest:::tempest_storm_state("ProgramSet resume")

  tempest:::tempest_storm_save_artifacts(
    run_dir,
    tempest_research_workspace(),
    state,
    manifest,
    program_set = program_set,
    config = cfg,
    steps = "research",
    research_strategy = "key_questions"
  )
  manifest_path <- file.path(run_dir, "run_config.json")
  persisted <- tempest:::tempest_product_read_json(manifest_path)
  restored_manifest <- tempest:::tempest_research_manifest_from_record(
    persisted$research_manifest
  )

  expect_identical(restored_manifest@programs, program_references)
  expect_identical(
    test_contains_runtime_value(persisted$research_manifest),
    FALSE
  )
  program_files <- unlist(persisted$files, use.names = FALSE)
  expect_length(program_files[startsWith(program_files, "programs/")], 0L)
  expect_error(
    tempest:::tempest_storm_load_artifacts(
      run_dir,
      config = cfg,
      run_id = "program-set-resume"
    ),
    class = "tempest_run_restore_error",
    regexp = "explicit complete TempestProgramSet"
  )

  file_program_set <- tempest_save_program_set(
    program_set,
    file.path(root, "program-set")
  )
  relocated <- tempest:::tempest_storm_load_artifacts(
    run_dir,
    config = cfg,
    program_set = file_program_set,
    run_id = "program-set-resume"
  )
  expect_s7_class(relocated$program_set, TempestProgramSet)
  expect_identical(
    tempest:::tempest_program_set_identity_equal(
      relocated$program_set,
      restored_manifest@programs
    ),
    TRUE
  )

  tampered <- persisted
  tampered$research_manifest$programs$personas <- NULL
  tempest:::tempest_product_write_json(manifest_path, tampered)
  expect_error(
    tempest:::tempest_storm_load_artifacts(
      run_dir,
      config = cfg,
      program_set = program_set,
      run_id = "program-set-resume"
    ),
    class = "tempest_run_restore_error",
    regexp = "every exact ProgramSet stage"
  )

  tampered <- persisted
  tampered$research_manifest$programs$perspectives$program_artifact_id <-
    paste0("sha256:", strrep("0", 64L))
  tempest:::tempest_product_write_json(manifest_path, tampered)
  expect_error(
    tempest:::tempest_storm_load_artifacts(
      run_dir,
      config = cfg,
      program_set = program_set,
      run_id = "program-set-resume"
    ),
    class = "tempest_run_restore_error",
    regexp = "identity does not match"
  )

  tampered <- persisted
  tampered$research_manifest$programs$perspectives$evaluator_version <- "999"
  tempest:::tempest_product_write_json(manifest_path, tampered)
  expect_error(
    tempest:::tempest_storm_load_artifacts(
      run_dir,
      config = cfg,
      program_set = program_set,
      run_id = "program-set-resume"
    ),
    class = "tempest_run_restore_error",
    regexp = "identity does not match"
  )

  tampered <- persisted
  tampered$research_manifest$programs$perspectives$artifact_reference <- NULL
  tempest:::tempest_product_write_json(manifest_path, tampered)
  expect_error(
    tempest:::tempest_storm_load_artifacts(
      run_dir,
      config = cfg,
      program_set = program_set,
      run_id = "program-set-resume"
    ),
    class = "tempest_run_restore_error",
    regexp = "research manifest is invalid"
  )
  tempest:::tempest_product_write_json(manifest_path, persisted)

  corrupt_program_set <- tempest_program_set()
  corrupt_program <- tempest:::tempest_program_set_program(
    corrupt_program_set,
    "perspectives"
  )
  corrupt_program$config$identity_corruption <- "changed"
  expect_error(
    tempest:::tempest_storm_load_artifacts(
      run_dir,
      config = cfg,
      program_set = corrupt_program_set,
      run_id = "program-set-resume"
    ),
    class = "tempest_run_restore_error"
  )

  missing_save_dir <- file.path(root, "missing-program-set")
  expect_error(
    tempest:::tempest_storm_save_artifacts(
      missing_save_dir,
      tempest_research_workspace(),
      state,
      manifest,
      config = cfg,
      steps = "research",
      research_strategy = "key_questions"
    ),
    class = "tempest_run_persistence_error",
    regexp = "explicit complete TempestProgramSet"
  )
  expect_identical(
    file.exists(file.path(missing_save_dir, "run_config.json")),
    FALSE
  )
})
