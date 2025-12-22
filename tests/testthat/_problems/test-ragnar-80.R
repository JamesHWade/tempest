# Extracted from test-ragnar.R:80

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "stormr", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
skip_if_not_installed("ragnar")
mock_embed <- function(x) matrix(rnorm(length(x) * 3), ncol = 3)
cfg <- storm_config(embed_fn = mock_embed)
retriever <- storm_retriever(config = cfg)
retriever$ingest_to_ragnar(
    source_id = "S123abc123abc",
    url = "https://example.com/test",
    title = "Test Document About Widgets",
    text = "Widgets are mechanical devices used in manufacturing. They come in various sizes and shapes. The widget industry has grown significantly in recent decades.",
    fetched_at = "2025-01-01T00:00:00Z",
    content_type = "html",
    perspective = NA_character_
  )
