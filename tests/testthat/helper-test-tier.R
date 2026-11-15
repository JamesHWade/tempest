test_tier_fixture <- function(root = test_path("..", "..")) {
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  script <- test_path("fixtures", "test-tier.R")
  if (!file.exists(script)) {
    stop("The test-tier fixture is missing from the installed test bundle.")
  }
  tier <- new.env(parent = globalenv())
  sys.source(script, envir = tier)
  list(root = root, tier = tier)
}
