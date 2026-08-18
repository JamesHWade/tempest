test_that("run restore rejects undeclared product files", {
  dir <- withr::local_tempdir()
  cfg <- tempest_config()
  program_set <- tempest_program_set()
  workspace <- tempest_research_workspace()
  state <- tempest:::tempest_storm_state("t")
  manifest <- tempest_research_manifest(
    "undeclared-run",
    config = cfg,
    programs = tempest:::tempest_program_set_manifest_programs(program_set)
  )
  tempest:::tempest_save_run_artifacts(
    dir,
    workspace,
    state,
    manifest,
    program_set = program_set,
    config = cfg,
    steps = "polish",
    research_strategy = "key_questions"
  )
  writeLines("UNDECLARED", file.path(dir, "storm_gen_article_polished.md"))

  expect_error(
    tempest:::tempest_load_run_artifacts(
      dir,
      config = cfg,
      program_set = program_set,
      run_id = "undeclared-run"
    ),
    class = "tempest_run_restore_error"
  )
})

test_that("run save refuses pre-existing unowned files", {
  dir <- withr::local_tempdir()
  credentials <- file.path(dir, "credentials.json")
  writeLines("user-owned", credentials)
  cfg <- tempest_config()
  program_set <- tempest_program_set()

  expect_error(
    tempest:::tempest_save_run_artifacts(
      dir,
      tempest_research_workspace(),
      tempest:::tempest_storm_state("Unowned files"),
      tempest_research_manifest(
        "unowned-files",
        config = cfg,
        programs = tempest:::tempest_program_set_manifest_programs(program_set)
      ),
      program_set = program_set,
      config = cfg,
      steps = "research",
      research_strategy = "key_questions"
    ),
    class = "tempest_run_persistence_error"
  )
  expect_identical(readLines(credentials), "user-owned")
  expect_false(file.exists(file.path(dir, "run_config.json")))
})

test_that("run manifest failures are classed and reject escaping symlinks", {
  program_set <- tempest_program_set()
  program_references <-
    tempest:::tempest_program_set_manifest_programs(program_set)
  make_bundle <- function() {
    dir <- tempfile("tempest-run-")
    dir.create(dir)
    cfg <- tempest_config()
    tempest:::tempest_save_run_artifacts(
      dir,
      tempest_research_workspace(),
      tempest:::tempest_storm_state("t"),
      tempest_research_manifest(
        "manifest-run",
        config = cfg,
        programs = program_references
      ),
      program_set = program_set,
      config = cfg,
      steps = "polish",
      research_strategy = "key_questions"
    )
    dir
  }

  missing_checksum_dir <- make_bundle()
  manifest_path <- file.path(missing_checksum_dir, "run_config.json")
  manifest <- tempest:::tempest_read_json_strict(manifest_path)
  manifest$checksums[[manifest$files[[1]]]] <- NULL
  tempest:::tempest_write_json(manifest_path, manifest)
  expect_error(
    tempest:::tempest_load_run_artifacts(
      missing_checksum_dir,
      config = tempest_config(),
      program_set = program_set,
      run_id = "manifest-run"
    ),
    class = "tempest_run_restore_error"
  )

  skip_on_os("windows")
  symlink_dir <- make_bundle()
  source_path <- file.path(symlink_dir, "workspace.json")
  outside_path <- tempfile("outside-workspace-", tmpdir = dirname(symlink_dir))
  expect_true(file.copy(source_path, outside_path))
  unlink(source_path)
  expect_true(file.symlink(outside_path, source_path))
  expect_error(
    tempest:::tempest_load_run_artifacts(
      symlink_dir,
      config = tempest_config(),
      program_set = program_set,
      run_id = "manifest-run"
    ),
    class = "tempest_run_restore_error"
  )
})

test_that("tempest_atomic_write_lines writes content and leaves no temp files", {
  dir <- withr::local_tempdir()
  path <- file.path(dir, "out.txt")

  tempest:::tempest_atomic_write_lines(c("a", "b"), path)
  expect_equal(readLines(path), c("a", "b"))

  tempest:::tempest_atomic_write_lines("c", path)
  expect_equal(readLines(path), "c")

  expect_length(list.files(dir, pattern = "\\.tmp$"), 0L)
})

test_that("the run manifest is written after the artifacts it certifies", {
  skip_if_not_installed("jsonlite")
  dir <- withr::local_tempdir()
  paths <- tempest:::tempest_run_artifact_paths(dir)
  cfg <- tempest_config()
  program_set <- tempest_program_set()
  program_references <-
    tempest:::tempest_program_set_manifest_programs(program_set)
  workspace <- tempest_research_workspace()
  source <- tempest:::tempest_source(
    "https://example.com/write-order",
    title = "Write order source",
    content_text = "Durable evidence is written before its manifest."
  )
  workspace$upsert_retrieved_resource(source)
  span_id <- workspace$add_evidence_span(tempest_evidence_span(
    evidence_span_id = "span-write-order",
    source_id = source$id,
    quote = "Durable evidence is written before its manifest.",
    extracted_by = program_references$extract_claims$program_artifact_id
  ))
  claim_id <- workspace$add_proposed_claim(tempest_claim(
    claim_id = "claim-write-order",
    claim_text = "Durable evidence is written before its manifest.",
    source_ids = source$id,
    evidence_span_ids = span_id,
    supporting_quotes = list(
      "Durable evidence is written before its manifest."
    ),
    verification_status = "supported",
    support_score = 0.9
  ))
  workspace$verify_proposed_claims_batch(
    list(tempest_claim_support(
      claim_id = claim_id,
      evidence_span_id = span_id,
      source_id = source$id,
      verification_status = "supported",
      support_score = 0.9,
      rationale = "The exact source supports the durable claim."
    )),
    verified_at = "2026-08-16T00:00:00Z"
  )
  state <- tempest:::tempest_storm_state(
    "t",
    outline = list(
      title = "Draft",
      sections = list(list(title = "Findings", summary = "Draft"))
    ),
    lead_section = "Durable evidence is written before its manifest.",
    draft_md = paste0(
      "Durable evidence is written before its manifest.\n\n",
      "## Findings\n\n",
      "Durable evidence is written before its manifest."
    ),
    completed_stages = c("research", "write")
  )
  manifest <- tempest_research_manifest(
    "write-order",
    config = cfg,
    programs = program_references
  )
  bound <- test_persistence_bind_storm_records(state, workspace, manifest)
  state <- bound$state
  manifest <- bound$manifest

  tempest:::tempest_save_run_artifacts(
    dir,
    workspace,
    state,
    manifest,
    program_set = program_set,
    config = cfg,
    steps = c("research", "write"),
    research_strategy = "key_questions"
  )

  # If the manifest claims "write" is complete, its artifact must exist.
  expect_equal(
    file.exists(c(paths$run_config, paths$draft_md)),
    c(TRUE, TRUE)
  )
})

test_that("references.json holds only the cited sources and reloads", {
  skip_if_not_installed("jsonlite")
  dir <- withr::local_tempdir()
  cfg <- tempest_config()
  program_set <- tempest_program_set()
  s2 <- tempest:::tempest_source(url = "https://example.com/b", title = "B")
  fixture <- test_persistence_complete_storm_product(
    "References",
    "references-run",
    cfg,
    program_set,
    extra_sources = list(s2)
  )
  workspace <- fixture$workspace
  state <- fixture$state
  research_manifest <- fixture$manifest
  s1 <- fixture$source

  tempest:::tempest_save_run_artifacts(
    dir,
    workspace,
    state,
    research_manifest,
    program_set = program_set,
    config = cfg,
    steps = state$completed_stages,
    research_strategy = "key_questions"
  )

  refs <- tempest:::tempest_read_json_strict(
    file.path(dir, "references.json")
  )
  expect_setequal(vapply(refs, function(r) r$id, character(1)), s1$id)

  loaded <- tempest:::tempest_load_run_artifacts(
    dir,
    config = cfg,
    program_set = program_set,
    run_id = "references-run"
  )
  expect_length(loaded$state$references, 1L)

  refs[[1]]$title <- "Forged title"
  references_path <- file.path(dir, "references.json")
  tempest:::tempest_write_json(references_path, refs)
  manifest_path <- file.path(dir, "run_config.json")
  bundle_manifest <- tempest:::tempest_read_json_strict(manifest_path)
  bundle_manifest$checksums[["references.json"]] <-
    tempest:::tempest_session_bundle_checksum(dir, "references.json")
  tempest:::tempest_write_json(manifest_path, bundle_manifest)
  expect_error(
    tempest:::tempest_load_run_artifacts(
      dir,
      config = cfg,
      program_set = program_set,
      run_id = "references-run"
    ),
    class = "tempest_run_restore_error"
  )

  tempest:::tempest_save_run_artifacts(
    dir,
    workspace,
    state,
    research_manifest,
    program_set = program_set,
    config = cfg,
    steps = state$completed_stages,
    research_strategy = "key_questions"
  )
  report_path <- file.path(dir, "storm_gen_article_polished.md")
  writeLines(paste0("Unknown citation [", s2$id, "]."), report_path)
  manifest <- tempest:::tempest_read_json_strict(manifest_path)
  manifest$checksums[["storm_gen_article_polished.md"]] <-
    tempest:::tempest_session_bundle_checksum(
      dir,
      "storm_gen_article_polished.md"
    )
  tempest:::tempest_write_json(manifest_path, manifest)
  expect_error(
    tempest:::tempest_load_run_artifacts(
      dir,
      config = cfg,
      program_set = program_set,
      run_id = "references-run"
    ),
    class = "tempest_run_restore_error"
  )
})

test_that("schema 6 STORM bundles fail closed", {
  dir <- withr::local_tempdir()
  tempest:::tempest_write_json(
    file.path(dir, "run_config.json"),
    list(schema_version = 6L)
  )
  expect_error(
    tempest:::tempest_load_run_artifacts(dir),
    class = "tempest_unsupported_format_error"
  )
  expect_error(
    tempest:::tempest_storm_restore_workspace(
      list(
        schema_version = 6L,
        workspace = list(
          base_snapshot_id = NULL,
          max_sources = "unbounded",
          accepted_graft_references = list()
        )
      ),
      tempest_config()
    ),
    class = "tempest_unsupported_format_error"
  )
})

test_that("schema 7 resume protects run and config identity", {
  dir <- withr::local_tempdir()
  cfg <- tempest_config()
  program_set <- tempest_program_set()
  workspace <- tempest_research_workspace()
  manifest <- tempest_research_manifest(
    "protected-run",
    config = cfg,
    programs = tempest:::tempest_program_set_manifest_programs(program_set),
    status = "failed"
  )
  tempest:::tempest_save_run_artifacts(
    dir,
    workspace,
    tempest:::tempest_storm_state("Protected run"),
    manifest,
    program_set = program_set,
    config = cfg,
    steps = "research",
    research_strategy = "key_questions"
  )

  expect_error(
    tempest:::tempest_load_run_artifacts(
      dir,
      config = cfg,
      program_set = program_set,
      run_id = "different-run"
    ),
    class = "tempest_run_restore_error"
  )
  expect_error(
    tempest:::tempest_load_run_artifacts(
      dir,
      config = tempest_config(max_search_results = 4L),
      program_set = program_set,
      run_id = "protected-run"
    ),
    class = "tempest_run_restore_error"
  )
  expect_error(
    tempest:::tempest_load_run_artifacts(
      dir,
      workspace = tempest_research_workspace(
        base_snapshot_id = "snapshot-b"
      ),
      config = cfg,
      program_set = program_set,
      run_id = "protected-run"
    ),
    class = "tempest_run_restore_error"
  )

  restored <- tempest:::tempest_load_run_artifacts(
    dir,
    config = cfg,
    program_set = program_set,
    run_id = "protected-run"
  )
  expect_identical(restored$research_manifest@status, "failed")
  expect_identical(restored$research_manifest@programs, manifest@programs)
})
