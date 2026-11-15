test_tier_package_root <- function(test_root = test_path("..", "..")) {
  test_root <- normalizePath(test_root, winslash = "/", mustWork = TRUE)
  candidates <- c(
    test_root,
    file.path(test_root, "00_pkg_src", "tempest")
  )
  valid <- vapply(
    candidates,
    function(candidate) {
      description <- file.path(candidate, "DESCRIPTION")
      testthat_dir <- file.path(candidate, "tests", "testthat")
      if (!file.exists(description) || !dir.exists(testthat_dir)) {
        return(FALSE)
      }
      package <- tryCatch(
        suppressWarnings(read.dcf(description, fields = "Package")[[1L]]),
        error = function(error) NA_character_
      )
      identical(package, "tempest")
    },
    logical(1)
  )
  if (!any(valid)) {
    stop(
      "Could not find Tempest at the test root or its `00_pkg_src/tempest` child."
    )
  }
  normalizePath(
    candidates[[which(valid)[[1L]]]],
    winslash = "/",
    mustWork = TRUE
  )
}

test_tier_fixture <- function(root = test_tier_package_root()) {
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  script <- test_path("fixtures", "test-tier.R")
  if (!file.exists(script)) {
    stop("The test-tier fixture is missing from the installed test bundle.")
  }
  tier <- new.env(parent = globalenv())
  sys.source(script, envir = tier)
  list(root = root, tier = tier)
}
