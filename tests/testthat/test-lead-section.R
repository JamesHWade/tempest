test_that("intro/conclusion section skip regex works", {
  skip_pattern <- "^(introduction|conclusion|summary|overview)$"

  skipped_titles <- c(
    "introduction",
    "Introduction",
    "conclusion",
    "Conclusion",
    "summary",
    "Summary",
    "overview",
    "Overview"
  )
  expect_match(skipped_titles, skip_pattern, ignore.case = TRUE)

  section_titles <- c(
    "Technical Background",
    "Introduction to Quantum",
    "Methods and Results",
    "Key Findings",
    "Summary of Findings",
    "Future Work and Conclusion"
  )
  expect_no_match(section_titles, skip_pattern, ignore.case = TRUE)
})

test_that("lead_section_system prompt exists", {
  prompt <- tempest:::tempest_prompt("lead_section_system")
  expect_type(prompt, "character")
  expect_gt(nchar(prompt), 0)
  expect_match(prompt, "lead", ignore.case = TRUE)
})
