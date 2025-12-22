# Extracted from test-personas.R:154

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "stormr", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
skip_if_not_installed("ellmer")
skip_if(
    Sys.getenv("OPENAI_API_KEY") == "" && Sys.getenv("ANTHROPIC_API_KEY") == "",
    "No API key available"
  )
mock_personas <- list(
    list(id = 1, name = "Dr. Alice Smith", title = "Scientist"),
    list(id = 2, name = "Prof. Bob Jones", title = "Ethicist")
  )
cfg <- storm_config()
session <- costorm_session(
    topic = "Test topic",
    config = cfg,
    personas = mock_personas
  )
expect_equal(session$find_expert_index("Dr. Alice Smith"), 1)
expect_equal(session$find_expert_index("Prof. Bob Jones"), 2)
expect_equal(session$find_expert_index("dr. alice smith"), 1)
expect_equal(session$find_expert_index("Alice"), 1)
expect_equal(session$find_expert_index("Bob"), 2)
