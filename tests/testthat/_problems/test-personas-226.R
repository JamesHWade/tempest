# Extracted from test-personas.R:226

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
persona <- list(
    id = 1,
    name = "Dr. Sarah Chen",
    title = "Climate Scientist",
    perspective = "Physical science perspective"
  )
cfg <- storm_config()
store <- stormr:::SourceStore$new()
retriever <- stormr:::storm_retriever(config = cfg, store = store)
mgr <- stormr:::ExpertSessionManager$new(cfg, retriever)
tool <- stormr:::storm_create_expert_tool(persona, mgr, "Climate change")
expect_equal(tool$name, "ask_dr_sarah_chen")
