test_that("schema 7 STORM manifests have an exact product envelope", {
  program_set <- tempest_program_set()
  program_references <-
    tempest:::tempest_program_set_manifest_programs(program_set)
  make_bundle <- function() {
    dir <- tempfile("tempest-exact-storm-")
    dir.create(dir)
    cfg <- tempest_config()
    state <- tempest:::tempest_storm_state(
      "Exact STORM",
      perspectives = list(list(
        name = "Overview",
        description = "General overview",
        key_questions = "What is the durable state?"
      )),
      experts = list(tempest_expert(
        expert_id = "expert.exact-storm",
        name = "Exact STORM Expert",
        title = "Persistence analyst",
        description = "Checks the exact STORM envelope.",
        instructions = "Reject ambiguous bundle metadata."
      )),
      completed_stages = c("perspectives", "research")
    )
    workspace <- tempest_research_workspace()
    manifest <- tempest_research_manifest(
      "exact-storm",
      config = cfg,
      programs = program_references
    )
    bound <- test_persistence_bind_storm_records(state, workspace, manifest)
    state <- bound$state
    manifest <- bound$manifest
    tempest:::tempest_storm_save_artifacts(
      dir,
      workspace,
      state,
      manifest,
      program_set = program_set,
      config = cfg,
      steps = c("perspectives", "research")
    )
    list(dir = dir, config = cfg)
  }

  bundle <- make_bundle()
  manifest_path <- file.path(bundle$dir, "run_config.json")
  original <- tempest:::tempest_product_read_json(manifest_path)
  expect_identical(
    names(original),
    tempest:::tempest_storm_bundle_manifest_fields()
  )

  reordered <- original[rev(names(original))]
  tempest:::tempest_product_write_json(manifest_path, reordered)
  expect_error(
    tempest:::tempest_storm_load_artifacts(
      bundle$dir,
      config = bundle$config,
      program_set = program_set,
      run_id = "exact-storm"
    ),
    class = "tempest_run_restore_error"
  )

  reordered_workspace <- original
  reordered_workspace$workspace <- reordered_workspace$workspace[
    rev(names(reordered_workspace$workspace))
  ]
  tempest:::tempest_product_write_json(manifest_path, reordered_workspace)
  expect_error(
    tempest:::tempest_storm_load_artifacts(
      bundle$dir,
      config = bundle$config,
      program_set = program_set,
      run_id = "exact-storm"
    ),
    class = "tempest_run_restore_error"
  )

  for (field in c("topic", "title")) {
    invalid <- original
    invalid[[field]] <- NULL
    tempest:::tempest_product_write_json(manifest_path, invalid)
    expect_error(
      tempest:::tempest_storm_load_artifacts(
        bundle$dir,
        config = bundle$config,
        program_set = program_set,
        run_id = "exact-storm"
      ),
      class = "tempest_run_restore_error"
    )
  }

  invalid <- original
  invalid$workflow_run <- list()
  tempest:::tempest_product_write_json(manifest_path, invalid)
  expect_error(
    tempest:::tempest_storm_load_artifacts(
      bundle$dir,
      config = bundle$config,
      program_set = program_set,
      run_id = "exact-storm"
    ),
    class = "tempest_run_restore_error"
  )

  nested_files <- original
  nested_files$files[[1]] <- list(nested_files$files[[1]])
  tempest:::tempest_product_write_json(manifest_path, nested_files)
  expect_error(
    tempest:::tempest_storm_load_artifacts(
      bundle$dir,
      config = bundle$config,
      program_set = program_set,
      run_id = "exact-storm"
    ),
    class = "tempest_run_restore_error"
  )

  nested_checksums <- original
  first_checksum <- names(nested_checksums$checksums)[[1]]
  nested_checksums$checksums[[first_checksum]] <- list(
    nested_checksums$checksums[[first_checksum]]
  )
  tempest:::tempest_product_write_json(manifest_path, nested_checksums)
  expect_error(
    tempest:::tempest_storm_load_artifacts(
      bundle$dir,
      config = bundle$config,
      program_set = program_set,
      run_id = "exact-storm"
    ),
    class = "tempest_run_restore_error"
  )

  explicit_empty <- original
  explicit_empty$completed_stages <- character()
  tempest:::tempest_product_write_json(manifest_path, explicit_empty)
  restored <- tempest:::tempest_storm_load_artifacts(
    bundle$dir,
    config = bundle$config,
    program_set = program_set,
    run_id = "exact-storm"
  )
  expect_identical(restored$completed_stages, character())
})
