test_that("intro/conclusion section skip regex works", {
  # These should match the skip pattern
  skip_pattern <- "^(introduction|conclusion|summary|overview)$"

  expect_true(grepl(skip_pattern, "introduction", ignore.case = TRUE))
  expect_true(grepl(skip_pattern, "Introduction", ignore.case = TRUE))
  expect_true(grepl(skip_pattern, "conclusion", ignore.case = TRUE))
  expect_true(grepl(skip_pattern, "Conclusion", ignore.case = TRUE))
  expect_true(grepl(skip_pattern, "summary", ignore.case = TRUE))
  expect_true(grepl(skip_pattern, "Summary", ignore.case = TRUE))
  expect_true(grepl(skip_pattern, "overview", ignore.case = TRUE))
  expect_true(grepl(skip_pattern, "Overview", ignore.case = TRUE))

  # These should NOT match
  expect_false(grepl(skip_pattern, "Technical Background", ignore.case = TRUE))
  expect_false(grepl(skip_pattern, "Introduction to Quantum", ignore.case = TRUE))
  expect_false(grepl(skip_pattern, "Methods and Results", ignore.case = TRUE))
  expect_false(grepl(skip_pattern, "Key Findings", ignore.case = TRUE))
  expect_false(grepl(skip_pattern, "Summary of Findings", ignore.case = TRUE))
  expect_false(grepl(skip_pattern, "Future Work and Conclusion", ignore.case = TRUE))
})

test_that("lead_section_system prompt exists", {
  prompt <- tempest:::tempest_prompt("lead_section_system")
  expect_true(is.character(prompt))
  expect_true(nzchar(prompt))
  expect_true(grepl("lead", prompt, ignore.case = TRUE))
})
