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
