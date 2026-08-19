test_that("schema 7 STORM declared JSON fails closed", {
  program_set <- tempest_program_set()
  program_references <-
    tempest:::tempest_program_set_manifest_programs(program_set)
  make_bundle <- function() {
    dir <- tempfile("tempest-strict-storm-")
    dir.create(dir)
    cfg <- tempest_config()
    state <- tempest:::tempest_storm_state(
      "Strict STORM",
      perspectives = list(list(
        name = "Overview",
        description = "General overview",
        key_questions = "Is every artifact valid JSON?"
      )),
      experts = list(tempest_expert(
        expert_id = "expert.strict-storm",
        name = "Strict STORM Expert",
        title = "Persistence analyst",
        description = "Checks strict STORM product JSON.",
        instructions = "Reject malformed declared artifacts."
      )),
      completed_stages = "perspectives"
    )
    workspace <- tempest_research_workspace()
    manifest <- tempest_research_manifest(
      "strict-storm",
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
      steps = "perspectives",
      research_strategy = "key_questions"
    )
    list(dir = dir, config = cfg)
  }

  for (file in c(
    "perspectives.json",
    "references.json",
    "stage_records.json"
  )) {
    bundle <- make_bundle()
    writeLines("{", file.path(bundle$dir, file))
    manifest_path <- file.path(bundle$dir, "run_config.json")
    manifest <- tempest:::tempest_product_read_json(manifest_path)
    manifest$checksums[[file]] <-
      tempest:::tempest_product_bundle_checksum(bundle$dir, file)
    tempest:::tempest_product_write_json(manifest_path, manifest)

    expect_error(
      tempest:::tempest_storm_load_artifacts(
        bundle$dir,
        config = bundle$config,
        program_set = program_set,
        run_id = "strict-storm"
      ),
      class = "tempest_run_restore_error"
    )
  }
})

test_that("schema 7 manifests require files implied by completed stages", {
  program_set <- tempest_program_set()
  program_references <-
    tempest:::tempest_program_set_manifest_programs(program_set)
  make_bundle <- function() {
    dir <- tempfile("tempest-stage-files-")
    dir.create(dir)
    cfg <- tempest_config()
    state <- tempest:::tempest_storm_state(
      "Stage files",
      perspectives = list(list(
        name = "Overview",
        description = "General overview",
        key_questions = "Which files prove completion?"
      )),
      experts = list(tempest_expert(
        expert_id = "expert.stage-files",
        name = "Stage File Expert",
        title = "Persistence reviewer",
        description = "Checks stage-specific persisted product files.",
        instructions = "Require the files certified by completed stages."
      )),
      completed_stages = "perspectives"
    )
    workspace <- tempest_research_workspace()
    manifest <- tempest_research_manifest(
      "stage-files",
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
      steps = "perspectives",
      research_strategy = "key_questions"
    )
    dir
  }
  remove_perspectives <- function(dir) {
    manifest_path <- file.path(dir, "run_config.json")
    manifest <- tempest:::tempest_product_read_json(manifest_path)
    manifest$files <- setdiff(
      unlist(manifest$files, use.names = FALSE),
      "perspectives.json"
    )
    manifest$checksums[["perspectives.json"]] <- NULL
    unlink(file.path(dir, "perspectives.json"))
    tempest:::tempest_product_write_json(manifest_path, manifest)
    dir
  }

  expect_setequal(
    tempest:::tempest_storm_stage_required_files(
      c("perspectives", "research", "outline", "write", "polish")
    ),
    c(
      "perspectives.json",
      "experts.json",
      "workspace.json",
      "direct_gen_outline.json",
      "storm_gen_outline.json",
      "storm_gen_article.md",
      "storm_gen_article_polished.md",
      "references.json"
    )
  )

  current_dir <- remove_perspectives(make_bundle())
  expect_error(
    tempest:::tempest_storm_load_artifacts(
      current_dir,
      config = tempest_config(),
      program_set = program_set,
      run_id = "stage-files"
    ),
    class = "tempest_run_restore_error"
  )
})
