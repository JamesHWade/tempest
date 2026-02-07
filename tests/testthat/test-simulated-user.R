test_that("SimulatedUser initializes correctly", {
  skip_if_not_installed("ellmer")
  skip_if(
    Sys.getenv("OPENAI_API_KEY") == "" && Sys.getenv("ANTHROPIC_API_KEY") == "",
    "No API key available"
  )

  cfg <- tempest_config()
  su <- SimulatedUser$new("Quantum computing", config = cfg, max_turns = 5L)

  expect_s3_class(su, "SimulatedUser")
  expect_equal(su$topic, "Quantum computing")
  expect_equal(su$max_turns, 5L)
  expect_equal(su$turn_count, 0L)
  expect_true(!is.null(su$chat))
})

test_that("SimulatedUser respects max_turns", {
  skip_if_not_installed("ellmer")
  skip_if(
    Sys.getenv("OPENAI_API_KEY") == "" && Sys.getenv("ANTHROPIC_API_KEY") == "",
    "No API key available"
  )

  cfg <- tempest_config()
  su <- SimulatedUser$new("Test topic", config = cfg, max_turns = 0L)

  # With max_turns=0, should immediately return NULL
  question <- su$generate_question()
  expect_null(question)
})

test_that("SimulatedUser turn_count increments", {
  skip_if_not_installed("ellmer")
  skip_if(
    Sys.getenv("OPENAI_API_KEY") == "" && Sys.getenv("ANTHROPIC_API_KEY") == "",
    "No API key available"
  )

  cfg <- tempest_config()
  su <- SimulatedUser$new("AI safety", config = cfg, max_turns = 3L)

  question <- su$generate_question()
  if (!is.null(question)) {
    expect_equal(su$turn_count, 1L)
    expect_true(is.character(question))
    expect_true(nzchar(question))
  }
})

test_that("SimulatedUser class is exported", {
  expect_true(exists("SimulatedUser", envir = asNamespace("tempest")))
})
