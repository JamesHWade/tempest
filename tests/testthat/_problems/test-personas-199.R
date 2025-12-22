# Extracted from test-personas.R:199

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
cfg <- storm_config()
store <- stormr:::SourceStore$new()
retriever <- stormr:::storm_retriever(config = cfg, store = store)
mgr <- stormr:::ExpertSessionManager$new(cfg, retriever)
sid <- mgr$generate_session_id("Dr. Sarah Chen")
