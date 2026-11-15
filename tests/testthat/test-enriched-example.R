test_that("the botanical example executes paired analyses and verifies its report", {
  example_dir <- system.file(
    "examples",
    "enriched-research",
    package = "tempest"
  )
  fixture <- new.env(parent = globalenv())
  sys.source(file.path(example_dir, "fixture.R"), envir = fixture)
  sys.source(file.path(example_dir, "plan.R"), envir = fixture)
  research <- fixture$tempest_co2_replay_research()
  expect_equal(nrow(tempest_sources(research)), 2L)
  plan <- fixture$tempest_co2_enrichment_plan(research, example_dir)
  destination <- file.path(withr::local_tempdir(), "report")
  report <- tempest_enrich_report(
    research,
    destination,
    plan = plan,
    allow_code = TRUE,
    executor = tempest_r_executor(timeout = 120),
    image_generator = function(task, directory) {
      file.copy(
        file.path(example_dir, "greenhouse-illustration.png"),
        file.path(directory, "illustration.png")
      )
      list(
        file = "illustration.png",
        metadata = list(provider = "offline fixture")
      )
    }
  )
  expect_identical(report@status, "complete")
  expect_identical(
    tempest_read_enriched_report(destination)@manifest,
    report@manifest
  )
  bundle <- jsonlite::read_json(file.path(destination, "report.json"))
  expect_match(bundle$analyses[[1L]]$output, "-7.60", fixed = TRUE)
  expect_match(bundle$analyses[[2L]]$output, "-7342.5", fixed = TRUE)
  tables <- lapply(
    Filter(function(path) endsWith(path, ".csv"), bundle$analyses[[1L]]$files),
    function(path) read.csv(file.path(destination, path))
  )
  pairs <- Filter(function(data) "chilled_plant" %in% names(data), tables)[[1L]]
  expect_identical(
    pairs$chilled_plant,
    c("Qc3", "Qc1", "Qc2", "Mc3", "Mc2", "Mc1")
  )
  expect_equal(mean(pairs$difference), -7.6)
  html <- paste(
    readLines(file.path(destination, "index.html")),
    collapse = "\n"
  )
  expect_match(html, "Illustration prompt and provenance", fixed = TRUE)
})
