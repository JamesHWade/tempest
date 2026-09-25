test_that("the inventory is assigned to exactly one test tier", {
  fixture <- test_tier_fixture()
  tier <- fixture$tier
  manifest <- tier$test_tier_manifest(fixture$root)

  expect_gt(length(manifest$inventory), 0L)
  expect_setequal(
    c(manifest$unit, manifest$full),
    manifest$inventory
  )
  expect_length(intersect(manifest$unit, manifest$full), 0L)
  expect_contains(manifest$unit, "test-public-api.R")
  expect_contains(manifest$full, "test-costorm-deputy.R")
  expect_contains(manifest$full, "test-test-tier.R")
})

test_that("the exact filter keeps similarly named test contexts separate", {
  fixture <- test_tier_fixture()
  tier <- fixture$tier
  filter <- tier$test_tier_filter(c(
    "test-config.r",
    "test-config-s7.R"
  ))

  expect_identical(grepl(filter, "config"), TRUE)
  expect_identical(grepl(filter, "config-s7"), TRUE)
  expect_identical(grepl(filter, "config-s7-extra"), FALSE)
})

test_that("allowlist validation rejects duplicate and unknown files", {
  fixture <- test_tier_fixture()
  tier <- fixture$tier
  inventory <- c("test-a.R", "test-b.R")

  expect_error(
    tier$test_tier_validate(
      inventory,
      unit_files = c("test-a.R", "test-a.R")
    ),
    class = "tempest_test_tier_error",
    regexp = "duplicates"
  )
  expect_error(
    tier$test_tier_validate(
      inventory,
      unit_files = c("test-a.R", "test-unknown.R")
    ),
    class = "tempest_test_tier_error",
    regexp = "unknown"
  )
})

test_that("inventory validation rejects duplicate and empty inputs", {
  fixture <- test_tier_fixture()
  tier <- fixture$tier
  expect_error(
    tier$test_tier_validate(
      c("test-a.R", "test-a.R"),
      unit_files = "test-a.R"
    ),
    class = "tempest_test_tier_error",
    regexp = "inventory contains duplicates"
  )
  expect_error(
    tier$test_tier_validate(character(), unit_files = "test-a.R"),
    class = "tempest_test_tier_error",
    regexp = "inventory is empty"
  )
  expect_error(
    tier$test_tier_validate("test-a.R", unit_files = character()),
    class = "tempest_test_tier_error",
    regexp = "allowlist is empty"
  )
})

test_that("the selected files contain no live session or provider entry point", {
  fixture <- test_tier_fixture()
  tier <- fixture$tier
  manifest <- tier$test_tier_manifest(fixture$root)
  root <- fixture$root
  paths <- file.path(root, "tests", "testthat", manifest$unit)
  forbidden <- c(
    "tempest_session[[:space:]]*\\(",
    "tempest_run[[:space:]]*\\(",
    "storm_product_fixture[[:space:]]*\\(",
    "storm_progress_fixture[[:space:]]*\\(",
    "test_promotion_(storm|costorm|fixture|bundle)[[:space:]]*\\(",
    "native_evidence_session[[:space:]]*\\(",
    "local_tempest_session[[:space:]]*\\(",
    "costorm_product_baseline_fixture[[:space:]]*\\(",
    "ellmer::chat_[A-Za-z_]+[[:space:]]*\\(",
    "httr2::request[[:space:]]*\\(",
    "curl::"
  )
  matches <- unlist(
    lapply(paths, function(path) {
      lines <- readLines(path, warn = FALSE)
      hits <- vapply(
        forbidden,
        function(pattern) any(grepl(pattern, lines, perl = TRUE)),
        logical(1)
      )
      if (any(hits)) basename(path) else character()
    }),
    use.names = FALSE
  )

  expect_length(matches, 0L)
})

test_that("repository and command validation fail with actionable errors", {
  fixture <- test_tier_fixture()
  tier <- fixture$tier
  expect_error(
    tier$test_tier_inventory(tempdir()),
    class = "tempest_test_tier_error",
    regexp = "Tempest repository root"
  )
  expect_error(
    tier$test_tier_main(character()),
    class = "tempest_test_tier_error",
    regexp = "Usage"
  )
})

test_that("list and validate reports name both tiers", {
  fixture <- test_tier_fixture()
  tier <- fixture$tier
  manifest <- tier$test_tier_manifest(fixture$root)

  expect_output(
    tier$test_tier_report(manifest, mode = "validate"),
    "Test-tier manifest is valid"
  )
  expect_output(
    tier$test_tier_report(manifest, mode = "list"),
    "Integration/full tier"
  )
})
