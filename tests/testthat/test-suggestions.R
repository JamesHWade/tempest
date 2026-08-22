test_that("session suggestions require exact current strings", {
  questions <- c("What evidence is strongest?", "What remains uncertain?")
  expect_identical(
    tempest:::tempest_suggested_questions_validate(questions),
    questions
  )
  invalid <- list(
    list("List input is not current."),
    c(named = "Named input is not current."),
    c(" padded input "),
    c("Duplicate", "Duplicate"),
    NA_character_
  )
  for (value in invalid) {
    expect_error(
      tempest:::tempest_suggested_questions_validate(value),
      class = "tempest_stage_output_error"
    )
  }
})

test_that("moderator session prompt names the real delegation tool and roster", {
  expert <- test_expert(
    name = "Dr. Safety",
    title = "Safety engineer"
  )

  prompt <- tempest:::tempest_moderator_system_prompt(
    "Adaptive animatronics",
    list(expert)
  )

  expect_match(prompt, "delegate_to_expert", fixed = TRUE)
  expect_match(prompt, expert@expert_id, fixed = TRUE)
  expect_match(prompt, "Adaptive animatronics", fixed = TRUE)
  expect_match(prompt, "at least once before answering", fixed = TRUE)
  expect_match(prompt, "at most once per moderator", fixed = TRUE)
  expect_match(prompt, "one narrow evidence question", fixed = TRUE)
  expect_match(prompt, "Do not append generic next-step menus", fixed = TRUE)
  expect_match(prompt, "clickable follow-up question cards", fixed = TRUE)
  expect_match(prompt, "specific evidence gap", fixed = TRUE)
  expect_no_match(prompt, "ask_*", fixed = TRUE)
  expect_no_match(prompt, "ask_expert", fixed = TRUE)
})
