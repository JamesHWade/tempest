test_that("tempest_sections_to_write skips lead-style sections", {
  outline <- list(
    sections = list(
      list(title = "Introduction"),
      list(title = "Mechanisms"),
      list(title = "Summary"),
      list(title = "Applications")
    )
  )

  sections <- tempest:::tempest_sections_to_write(outline)

  expect_equal(
    vapply(sections, function(section) section$title, character(1)),
    c("Mechanisms", "Applications")
  )
})

test_that("empty section fact context does not tell the writer to call tools", {
  local_mocked_bindings(tempest_semantic_filter_facts = function(...) list())

  facts <- tempest:::tempest_section_facts_text(
    retriever = NULL,
    store = SourceStore$new(),
    section_title = "Mechanisms",
    max_items = 5
  )

  expect_match(facts, "no directly matched facts")
  expect_no_match(facts, "call tools")
})

test_that("tempest_run_section_job wraps section text as markdown", {
  writer <- list(chat = function(prompt, echo = "none") {
    "Section body [S123456789abc]"
  })
  job <- list(
    index = 1,
    title = "Mechanisms",
    summary = "How it works",
    subsections = list(),
    facts_text = "- Fact [S123456789abc]"
  )

  result <- tempest:::tempest_run_section_job(job, writer)

  expect_equal(result$title, "Mechanisms")
  expect_equal(result$section_text, "Section body [S123456789abc]")
  expect_equal(
    result$markdown,
    "## Mechanisms\n\nSection body [S123456789abc]"
  )
})

test_that("section prompts collapse vector subsection titles", {
  writer <- list(chat = function(prompt, echo = "none") {
    expect_match(prompt, "### Setup Details", fixed = TRUE)
    "Section body [S123456789abc]"
  })
  job <- list(
    index = 1,
    title = "Mechanisms",
    summary = "How it works",
    subsections = list(list(
      title = c("Setup", "Details"),
      bullets = c("Fact A", "Fact B")
    )),
    facts_text = "- Fact [S123456789abc]"
  )

  result <- tempest:::tempest_run_section_job(job, writer)

  expect_equal(result$section_text, "Section body [S123456789abc]")
})

test_that("tempest_write_sections_sequential preserves job order", {
  writer <- list(chat = function(prompt, echo = "none") {
    if (grepl("First", prompt, fixed = TRUE)) "First body" else "Second body"
  })
  jobs <- list(
    list(
      index = 1,
      title = "First",
      summary = "",
      subsections = list(),
      facts_text = "Facts"
    ),
    list(
      index = 2,
      title = "Second",
      summary = "",
      subsections = list(),
      facts_text = "Facts"
    )
  )

  results <- tempest:::tempest_write_sections_sequential(jobs, writer)

  expect_equal(
    vapply(results, function(result) result$title, character(1)),
    c("First", "Second")
  )
  expect_equal(
    vapply(results, function(result) result$section_text, character(1)),
    c("First body", "Second body")
  )
})

test_that("tempest_parallel_workers respects option and item cap", {
  withr::local_options(tempest.parallel_workers = 4)
  expect_identical(tempest:::tempest_parallel_workers(), 4L)
  expect_identical(tempest:::tempest_parallel_workers(2), 2L)
  expect_identical(tempest:::tempest_parallel_workers(10), 4L)
})

test_that("tempest_parallel_workers defaults to a positive integer", {
  withr::local_options(tempest.parallel_workers = NULL)
  workers <- tempest:::tempest_parallel_workers()
  expect_type(workers, "integer")
  expect_gte(workers, 1L)
})

test_that("tempest_collect_parallel keeps values and drops failures", {
  expect_equal(tempest:::tempest_collect_parallel(list(a = 1)), list(a = 1))
  expect_null(tempest:::tempest_collect_parallel(NULL))
  expect_null(tempest:::tempest_collect_parallel(simpleError("boom")))
  err <- structure(
    list(message = "x"),
    class = c("miraiError", "errorValue", "try-error")
  )
  expect_null(tempest:::tempest_collect_parallel(err))
})

test_that("parallel section writing returns NULLs when mirai is unavailable", {
  local_mocked_bindings(tempest_has = function(pkg) FALSE)
  jobs <- list(
    list(index = 1, title = "First", summary = "", subsections = list()),
    list(index = 2, title = "Second", summary = "", subsections = list())
  )
  results <- tempest:::tempest_write_sections_parallel(jobs, tempest_config())
  expect_length(results, 2)
  expect_equal(vapply(results, is.null, logical(1)), c(TRUE, TRUE))
})

test_that("requesting parallel writing still produces sections via fallback", {
  local_mocked_bindings(tempest_has = function(pkg) FALSE)
  writer <- list(chat = function(prompt, echo = "none") "Body")
  jobs <- list(
    list(
      index = 1,
      title = "First",
      summary = "",
      subsections = list(),
      facts_text = "Facts"
    ),
    list(
      index = 2,
      title = "Second",
      summary = "",
      subsections = list(),
      facts_text = "Facts"
    )
  )

  results <- suppressWarnings(tempest:::tempest_write_section_jobs(
    jobs,
    writer,
    config = tempest_config(),
    parallel = TRUE
  ))

  expect_equal(
    vapply(results, function(result) result$title, character(1)),
    c("First", "Second")
  )
})
