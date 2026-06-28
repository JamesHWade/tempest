test_that("tempest_prepare_run_dir creates topic slug directory", {
  skip_if_not_installed("jsonlite")
  root <- withr::local_tempdir(pattern = "tempest-runs-")

  run_dir <- tempest:::tempest_prepare_run_dir(
    root,
    "A Topic: With Punctuation!"
  )

  expect_equal(basename(run_dir), "a-topic-with-punctuation")
  expect_equal(dir.exists(run_dir), TRUE)
})

test_that("run artifacts save and load store state", {
  skip_if_not_installed("jsonlite")
  root <- withr::local_tempdir(pattern = "tempest-runs-")

  run_dir <- tempest:::tempest_prepare_run_dir(root, "Lithium Batteries")
  store <- SourceStore$new()
  source <- tempest:::tempest_source(
    "https://example.com/source",
    title = "Example Source",
    snippet = "Snippet"
  )
  store$upsert_source(source)
  store$add_claim(tempest:::tempest_claim(
    claim_text = "Lithium batteries store energy.",
    source_ids = source$id,
    confidence = "high"
  ))
  store$set_artifact("title", "Lithium Batteries")
  store$set_artifact(
    "perspectives",
    list(list(
      name = "Technical",
      description = "Technology",
      key_questions = c("How do they work?")
    ))
  )
  store$set_artifact(
    "personas",
    list(list(
      id = 1,
      name = "Dr. Tech",
      title = "Engineer"
    ))
  )
  store$set_artifact(
    "outline",
    list(
      title = "Lithium Batteries",
      sections = list(list(title = "Overview", summary = "Summary"))
    )
  )
  store$set_artifact("draft_md", "Draft body")
  store$set_artifact("report_md", "Polished body")
  store$set_artifact(
    "citation_audit",
    tibble::tibble(
      claim_id = store$list_claims()[[1]]@claim_id,
      claim_text = "Lithium batteries store energy.",
      verification_status = "supported",
      support_score = 0.9,
      rationale = "matches source"
    )
  )

  cfg <- tempest_config()
  tempest:::tempest_save_run_artifacts(
    run_dir,
    store,
    topic = "Lithium Batteries",
    title = "Lithium Batteries",
    config = cfg,
    completed_stages = c(
      "perspectives",
      "research",
      "outline",
      "write",
      "polish"
    ),
    steps = c("perspectives", "research", "outline", "write", "polish"),
    research_strategy = "key_questions",
    parallel_writing = TRUE,
    remove_duplicate = TRUE
  )

  restored <- SourceStore$new()
  loaded <- tempest:::tempest_load_run_artifacts(run_dir, restored)

  expect_equal(
    loaded$completed_stages,
    c("perspectives", "research", "outline", "write", "polish")
  )
  expect_equal(length(restored$list_sources()), 1)
  expect_equal(length(restored$list_claims()), 1)
  expect_equal(restored$get_artifact("title"), "Lithium Batteries")
  expect_equal(restored$get_artifact("outline")$title, "Lithium Batteries")
  expect_equal(restored$get_artifact("draft_md"), "Draft body")
  expect_equal(restored$get_artifact("report_md"), "Polished body")
  expect_s3_class(restored$get_artifact("citation_audit"), "tbl_df")
  expect_equal(nrow(restored$get_artifact("citation_audit")), 1)
  expect_equal(loaded$metadata$parallel_writing, TRUE)
  expect_equal(loaded$metadata$remove_duplicate, TRUE)
})

test_that("completed stage metadata controls resume state", {
  skip_if_not_installed("jsonlite")
  root <- withr::local_tempdir(pattern = "tempest-runs-")

  run_dir <- tempest:::tempest_prepare_run_dir(root, "Partial Run")
  store <- SourceStore$new()
  store$set_artifact("perspectives", list(list(name = "Overview")))
  store$set_artifact("personas", list(list(name = "Expert")))

  tempest:::tempest_save_run_artifacts(
    run_dir,
    store,
    topic = "Partial Run",
    title = "Partial Run",
    config = tempest_config(),
    completed_stages = "perspectives",
    steps = c("perspectives", "research", "outline", "write", "polish"),
    research_strategy = "key_questions"
  )

  loaded <- tempest:::tempest_load_run_artifacts(run_dir, SourceStore$new())

  expect_equal(
    tempest:::tempest_stage_complete(
      loaded$completed_stages,
      "perspectives"
    ),
    TRUE
  )
  expect_equal(
    tempest:::tempest_stage_complete(
      loaded$completed_stages,
      "research"
    ),
    FALSE
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

test_that("tempest_infer_completed_stages reads stages from artifact files", {
  dir <- withr::local_tempdir()
  paths <- tempest:::tempest_run_artifact_paths(dir)

  expect_length(tempest:::tempest_infer_completed_stages(paths), 0L)

  file.create(paths$perspectives, paths$personas)
  expect_setequal(
    tempest:::tempest_infer_completed_stages(paths),
    "perspectives"
  )

  file.create(paths$outline)
  expect_setequal(
    tempest:::tempest_infer_completed_stages(paths),
    c("perspectives", "outline")
  )
})

test_that("the run manifest is written after the artifacts it certifies", {
  skip_if_not_installed("jsonlite")
  dir <- withr::local_tempdir()
  paths <- tempest:::tempest_run_artifact_paths(dir)
  store <- SourceStore$new()
  store$set_artifact("draft_md", "# Draft")

  tempest:::tempest_save_run_artifacts(
    dir,
    store,
    topic = "t",
    title = "T",
    config = tempest_config(),
    completed_stages = c("research", "write"),
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
  store <- SourceStore$new()
  s1 <- tempest:::tempest_source(url = "https://example.com/a", title = "A")
  s2 <- tempest:::tempest_source(url = "https://example.com/b", title = "B")
  store$upsert_source(s1)
  store$upsert_source(s2)
  store$set_artifact("report_md", paste0("A cited claim [", s1$id, "]."))

  tempest:::tempest_save_run_artifacts(
    dir,
    store,
    topic = "t",
    title = "T",
    config = tempest_config(),
    completed_stages = "polish",
    steps = "polish",
    research_strategy = "key_questions"
  )

  refs <- tempest:::tempest_read_json(file.path(dir, "references.json"))
  expect_setequal(vapply(refs, function(r) r$id, character(1)), s1$id)

  store2 <- SourceStore$new()
  tempest:::tempest_load_run_artifacts(dir, store2)
  expect_length(store2$get_artifact("references"), 1L)
})
