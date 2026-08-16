test_that("SimulatedUser initializes correctly", {
  skip_if_not_installed("ellmer")

  chat <- fake_chat(text = list("What should we investigate?"))
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) chat
  )
  su <- SimulatedUser$new("Quantum computing", config = cfg, max_turns = 5L)

  expect_s3_class(su, "SimulatedUser")
  expect_equal(su$topic, "Quantum computing")
  expect_equal(su$max_turns, 5L)
  expect_equal(su$turn_count, 0L)
  expect_identical(su$chat, chat)
})

test_that("SimulatedUser respects max_turns", {
  skip_if_not_installed("ellmer")

  chat <- fake_chat(text = list("This response must not be requested."))
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) chat
  )
  su <- SimulatedUser$new("Test topic", config = cfg, max_turns = 0L)

  question <- su$generate_question()
  expect_null(question)
  expect_length(chat$.calls(), 0L)
})

test_that("SimulatedUser turn_count increments", {
  skip_if_not_installed("ellmer")

  chat <- fake_chat(text = list("What evidence supports the claim?", "DONE"))
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) chat
  )
  su <- SimulatedUser$new("AI safety", config = cfg, max_turns = 3L)

  question <- su$generate_question()
  expect_identical(question, "What evidence supports the claim?")
  expect_equal(su$turn_count, 1L)

  expect_null(su$generate_question())
  expect_equal(su$turn_count, 1L)
  expect_length(chat$.calls(), 2L)
})

test_that("SimulatedUser class is exported", {
  expect_no_error(getExportedValue("tempest", "SimulatedUser"))
})
