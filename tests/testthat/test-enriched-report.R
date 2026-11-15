test_that("enrichment preserves canonical STORM and Co-STORM authority", {
  for (mode in c("storm", "costorm")) {
    fixture <- test_promotion_fixture(mode)
    before <- tempest_report(fixture$research)
    plan <- test_enriched_plan(fixture$resource@resource_id)
    destination <- file.path(withr::local_tempdir(), "report")
    report <- tempest_enrich_report(
      fixture$research,
      destination,
      plan = plan,
      allow_code = TRUE,
      executor = test_enriched_executor
    )
    expect_s7_class(report, TempestEnrichedReport)
    expect_identical(report@status, "complete")
    expect_identical(tempest_report(fixture$research), before)
    expect_identical(
      tempest_read_enriched_report(destination)@manifest,
      report@manifest
    )
    content <- jsonlite::read_json(file.path(destination, "report.json"))
    expect_identical(content$analyses[[1L]]$status, "succeeded")
    expect_identical(content$analyses[[1L]]$output, "[1] 2")
    expect_identical(
      content$analyses[[1L]]$file_map[[1L]],
      list(
        original = "values.csv",
        archived = "analyses/comparison/output-1.csv"
      )
    )
    expect_identical(
      readLines(file.path(destination, "analyses/comparison/analysis.R")),
      plan$analyses[[1L]]$code
    )
    expect_identical(length(report@manifest$files) >= 8L, TRUE)
    html <- paste(
      readLines(file.path(destination, "index.html")),
      collapse = "\n"
    )
    expect_match(html, 'href="manifest.json"', fixed = TRUE)
    expect_match(html, "Enrichment status", fixed = TRUE)
  }
})

test_that("the complete plan is rejected before execution or directory creation", {
  fixture <- test_promotion_fixture("storm")
  plan <- test_enriched_plan(fixture$resource@resource_id)
  destination <- file.path(withr::local_tempdir(), "report")
  plan$analyses[[1L]]$source_ids <- "unknown-source"
  expect_error(
    tempest_enrich_report(
      fixture$research,
      destination,
      plan = plan,
      executor = function(...) stop("must not run"),
      allow_code = TRUE
    ),
    class = "tempest_input_error"
  )
  expect_identical(dir.exists(destination), FALSE)
  plan <- test_enriched_plan(fixture$resource@resource_id)
  expect_error(
    tempest_enrich_report(
      fixture$research,
      destination,
      plan = plan,
      max_analyses = 0
    ),
    class = "tempest_input_error"
  )
  plan$analyses[[1L]]$id <- "../escape"
  expect_error(
    tempest_enrich_report(fixture$research, destination, plan = plan),
    class = "tempest_input_error"
  )
})

test_that("code execution is opt in and provider failures remain visible", {
  fixture <- test_promotion_fixture("storm")
  plan <- test_enriched_plan(fixture$resource@resource_id)
  calls <- 0L
  executor <- function(...) {
    calls <<- calls + 1L
    stop("SECRET provider credential")
  }
  root <- withr::local_tempdir()
  skipped <- tempest_enrich_report(
    fixture$research,
    file.path(root, "skipped"),
    plan = plan,
    executor = executor
  )
  expect_identical(calls, 0L)
  expect_identical(skipped@status, "partial")
  failed <- tempest_enrich_report(
    fixture$research,
    file.path(root, "failed"),
    plan = plan,
    executor = executor,
    allow_code = TRUE
  )
  expect_identical(calls, 1L)
  expect_identical(failed@status, "partial")
  text <- paste(
    readLines(file.path(failed@path, "report.json")),
    collapse = "\n"
  )
  expect_no_match(text, "SECRET")
  expect_match(text, "No result was accepted", fixed = TRUE)
})

test_that("escaping output paths and image impostors are rejected", {
  root <- withr::local_tempdir()
  task <- test_enriched_plan("source")$analyses[[1L]]
  result <- tempest_enriched_task(
    task,
    "analyses",
    function(task, directory) {
      list(output = "forged", files = "../secret.csv", metadata = list())
    },
    root
  )
  expect_identical(result$status, "failed")
  expect_identical(result$files, character())
  writeLines("<script>bad</script>", file.path(root, "impostor.png"))
  expect_error(
    tempest_enriched_file("impostor.png", root, image = TRUE),
    class = "tempest_execution_error"
  )
  if (.Platform$OS.type != "windows") {
    file.symlink(file.path(root, "impostor.png"), file.path(root, "linked.png"))
    expect_error(
      tempest_enriched_file("linked.png", root),
      class = "tempest_execution_error"
    )
  }
})

test_that("report reads detect edits and extra files without overwriting reports", {
  fixture <- test_promotion_fixture("storm")
  plan <- test_enriched_plan(fixture$resource@resource_id, analyses = FALSE)
  destination <- file.path(withr::local_tempdir(), "report")
  tempest_enrich_report(fixture$research, destination, plan = plan)
  expect_error(
    tempest_enrich_report(fixture$research, destination, plan = plan),
    class = "tempest_input_error"
  )
  writeLines("extra", file.path(destination, "extra.txt"))
  expect_error(
    tempest_read_enriched_report(destination),
    class = "tempest_persistence_error"
  )
  unlink(file.path(destination, "extra.txt"))
  writeLines("changed", file.path(destination, "research.md"))
  expect_error(
    tempest_read_enriched_report(destination),
    class = "tempest_persistence_error"
  )
  writeLines('{"schema_version":1}', file.path(destination, "manifest.json"))
  expect_error(
    tempest_read_enriched_report(destination),
    class = "tempest_persistence_error"
  )
})

test_that("custom planning gets exact research and respects budgets", {
  fixture <- test_promotion_fixture("storm")
  planner <- function(report, sources, max_analyses, max_illustrations) {
    expect_identical(report, tempest_report(fixture$research))
    expect_identical(sources, tempest_sources(fixture$research))
    expect_identical(max_analyses, 0L)
    test_enriched_plan(fixture$resource@resource_id, analyses = FALSE)
  }
  result <- tempest_enrich_report(
    fixture$research,
    file.path(withr::local_tempdir(), "report"),
    planner = planner,
    max_analyses = 0L
  )
  expect_identical(result@status, "complete")
  destination <- file.path(withr::local_tempdir(), "failed-plan")
  expect_error(
    tempest_enrich_report(
      fixture$research,
      destination,
      planner = function(...) stop("Provider unavailable")
    ),
    class = "tempest_execution_error"
  )
  expect_identical(dir.exists(destination), FALSE)
})

test_that("special files and undeclared directory links are rejected before reading", {
  skip_on_os("windows")
  root <- withr::local_tempdir()
  fifo <- file.path(root, "pipe.csv")
  expect_identical(system2("mkfifo", shQuote(fifo)), 0L)
  expect_error(
    tempest_enriched_file("pipe.csv", root),
    class = "tempest_execution_error"
  )
  expect_error(
    tempest_enriched_inventory(root),
    class = "tempest_persistence_error"
  )
  unlink(fifo)
  empty <- withr::local_tempdir()
  expect_identical(file.symlink(empty, file.path(root, "unlisted")), TRUE)
  expect_error(
    tempest_enriched_inventory(root),
    class = "tempest_persistence_error"
  )
  unlink(file.path(root, "unlisted"))
  expect_identical(
    system2("mkfifo", shQuote(file.path(root, "manifest.json"))),
    0L
  )
  expect_error(
    tempest_read_enriched_report(root),
    class = "tempest_persistence_error"
  )
})

test_that("the dsprrr planner returns a bounded typed plan from an offline chat", {
  fixture <- test_promotion_fixture("storm")
  plan <- test_enriched_plan(fixture$resource@resource_id)
  context <- list(
    report_md = tempest_report(fixture$research),
    config = tempest_config(chat_fn = function(...) {
      fake_chat(structured = list(plan))
    })
  )
  actual <- tempest_enriched_plan(
    context,
    tempest_sources(fixture$research),
    1L,
    0L
  )
  expect_identical(actual, plan)
  expect_invisible(tempest_enriched_plan_validate(
    actual,
    fixture$resource@resource_id,
    1L,
    0L
  ))
})

test_that("the one-call workflow forwards research and enrichment independently", {
  fixture <- test_promotion_fixture("storm")
  plan <- test_enriched_plan(fixture$resource@resource_id, analyses = FALSE)
  local_mocked_bindings(tempest_run = function(topic, verbose) {
    expect_identical(topic, "Test research")
    expect_identical(verbose, FALSE)
    fixture$research
  })
  result <- tempest_researcher(
    "Test research",
    file.path(withr::local_tempdir(), "report"),
    verbose = FALSE,
    enrichment = list(plan = plan)
  )
  expect_identical(result$research, fixture$research)
  expect_identical(result$report@status, "complete")
})
