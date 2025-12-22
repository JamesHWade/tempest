# Extracted from test-ragnar.R:8

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "stormr", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
skip_if_not_installed("ragnar")
mock_embed <- function(x) matrix(rnorm(length(x) * 3), ncol = 3)
store <- storm_create_ragnar_store(mock_embed, cache_dir = NULL)
expect_true(inherits(store, "RagnarStore"))
