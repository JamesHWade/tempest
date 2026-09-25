test_tier_fixture <- function() {
  root <- normalizePath(test_path("..", ".."), mustWork = TRUE)
  script <- file.path(root, "tools", "test-tier.R")
  if (!file.exists(script)) {
    root <- file.path(root, "00_pkg_src", "tempest")
    script <- file.path(root, "tools", "test-tier.R")
  }
  if (!file.exists(script)) {
    stop("The test-tier script is missing from the package source.")
  }
  tier <- new.env(parent = globalenv())
  sys.source(script, envir = tier)
  list(root = root, tier = tier)
}
