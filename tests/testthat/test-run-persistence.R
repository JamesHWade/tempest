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
  store$add_fact(tempest:::tempest_fact(
    "Lithium batteries store energy.",
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
  expect_equal(length(restored$list_facts()), 1)
  expect_equal(restored$get_artifact("title"), "Lithium Batteries")
  expect_equal(restored$get_artifact("outline")$title, "Lithium Batteries")
  expect_equal(restored$get_artifact("draft_md"), "Draft body")
  expect_equal(restored$get_artifact("report_md"), "Polished body")
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
