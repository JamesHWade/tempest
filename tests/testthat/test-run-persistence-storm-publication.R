test_that("STORM requested steps persist canonically and remain immutable", {
  skip_if_not_installed("jsonlite")
  root <- withr::local_tempdir()
  run_dir <- file.path(root, "requested-steps")
  cfg <- tempest_config()
  program_set <- tempest_program_set()
  workspace <- tempest_research_workspace()
  manifest <- tempest_research_manifest(
    "requested-steps",
    config = cfg,
    programs = tempest:::tempest_program_set_manifest_programs(program_set)
  )
  state <- tempest:::tempest_storm_state("Requested steps")

  tempest:::tempest_storm_save_artifacts(
    run_dir,
    workspace,
    state,
    manifest,
    program_set = program_set,
    config = cfg,
    steps = c("polish", "research", "write"),
    research_strategy = "key_questions"
  )
  loaded <- tempest:::tempest_storm_load_artifacts(
    run_dir,
    config = cfg,
    program_set = program_set,
    run_id = "requested-steps"
  )
  expect_identical(
    loaded$state$requested_steps,
    c("research", "write", "polish")
  )
  expect_identical(loaded$completed_stages, character())

  tempest:::tempest_storm_save_artifacts(
    run_dir,
    workspace,
    state,
    manifest,
    program_set = program_set,
    config = cfg,
    steps = c("write", "polish", "research"),
    research_strategy = "key_questions"
  )
  expect_error(
    tempest:::tempest_storm_save_artifacts(
      run_dir,
      workspace,
      state,
      manifest,
      program_set = program_set,
      config = cfg,
      steps = c("research", "outline"),
      research_strategy = "key_questions"
    ),
    class = "tempest_run_persistence_error"
  )
  restored <- tempest:::tempest_storm_load_artifacts(
    run_dir,
    config = cfg,
    program_set = program_set,
    run_id = "requested-steps"
  )
  expect_identical(restored$state$requested_steps, loaded$state$requested_steps)
})

test_that("succeeded STORM publication requires the full dependency chain", {
  skip_if_not_installed("jsonlite")
  cfg <- tempest_config()
  program_set <- tempest_program_set()
  expect_error(
    tempest:::tempest_storm_save_artifacts(
      withr::local_tempdir(),
      tempest_research_workspace(),
      tempest:::tempest_storm_state("Partial publication"),
      tempest_research_manifest(
        "partial-publication",
        mode = "storm",
        config = cfg,
        programs = tempest:::tempest_program_set_manifest_programs(program_set),
        status = "succeeded"
      ),
      program_set = program_set,
      config = cfg,
      steps = "research",
      research_strategy = "key_questions"
    ),
    class = "tempest_run_persistence_error"
  )
})
