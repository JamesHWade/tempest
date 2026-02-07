test_that("tempest_type_turn_policy returns valid type", {
  skip_if_not_installed("ellmer")

  type <- tempest:::tempest_type_turn_policy()
  expect_true(!is.null(type))
})

test_that("DiscourseManager initializes correctly", {
  skip_if_not_installed("ellmer")
  skip_if(
    Sys.getenv("OPENAI_API_KEY") == "" && Sys.getenv("ANTHROPIC_API_KEY") == "",
    "No API key available"
  )

  cfg <- tempest_config(enable_discourse_manager = TRUE)
  dm <- tempest:::DiscourseManager$new(cfg)

  expect_s3_class(dm, "DiscourseManager")
  expect_true(!is.null(dm$chat))
  expect_true(!is.null(dm$config))
})

test_that("TempestSession creates discourse manager when enabled", {
  skip_if_not_installed("ellmer")
  skip_if(
    Sys.getenv("OPENAI_API_KEY") == "" && Sys.getenv("ANTHROPIC_API_KEY") == "",
    "No API key available"
  )

  mock_personas <- list(
    list(id = 1, name = "Dr. Alice", title = "Scientist", perspective = "Technical")
  )

  cfg <- tempest_config(enable_discourse_manager = TRUE)
  session <- tempest_session("Test topic", config = cfg, personas = mock_personas)

  expect_true(!is.null(session$discourse_manager))
  expect_s3_class(session$discourse_manager, "DiscourseManager")
})

test_that("TempestSession discourse manager is NULL when disabled", {
  skip_if_not_installed("ellmer")
  skip_if(
    Sys.getenv("OPENAI_API_KEY") == "" && Sys.getenv("ANTHROPIC_API_KEY") == "",
    "No API key available"
  )

  mock_personas <- list(
    list(id = 1, name = "Dr. Alice", title = "Scientist", perspective = "Technical")
  )

  cfg <- tempest_config(enable_discourse_manager = FALSE)
  session <- tempest_session("Test topic", config = cfg, personas = mock_personas)

  expect_null(session$discourse_manager)
})

test_that("execute_turn_decision handles end_round", {
  skip_if_not_installed("ellmer")
  skip_if(
    Sys.getenv("OPENAI_API_KEY") == "" && Sys.getenv("ANTHROPIC_API_KEY") == "",
    "No API key available"
  )

  mock_personas <- list(
    list(id = 1, name = "Dr. Alice", title = "Scientist", perspective = "Technical")
  )

  cfg <- tempest_config()
  session <- tempest_session("Test topic", config = cfg, personas = mock_personas)

  decision <- list(action = "end_round", instruction = "", rationale = "test")
  result <- session$execute_turn_decision(decision)

  expect_equal(result$speaker, "System")
  expect_equal(result$answer, "Round complete.")
})
