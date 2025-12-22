# Extracted from test-ragnar.R:59

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "stormr", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
skip_if_not_installed("ragnar")
mock_embed <- function(x) matrix(rnorm(length(x) * 3), ncol = 3)
cfg <- storm_config(embed_fn = mock_embed)
retriever <- storm_retriever(config = cfg)
n_chunks <- retriever$ingest_to_ragnar(
    source_id = "S123abc123abc",
    url = "https://example.com/test",
    title = "Test Document",
    text = paste(rep("This is test content for chunking. ", 50), collapse = ""),
    fetched_at = "2025-01-01T00:00:00Z",
    content_type = "html",
    perspective = "test_perspective"
  )
