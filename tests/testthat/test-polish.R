test_that("tempest_polish_rules preserves citations and optionally deduplicates", {
  base_rules <- tempest:::tempest_polish_rules(remove_duplicate = FALSE)
  dedupe_rules <- tempest:::tempest_polish_rules(remove_duplicate = TRUE)

  expect_match(base_rules, "Preserve all citations", fixed = TRUE)
  expect_no_match(base_rules, "duplicate", fixed = TRUE)
  expect_match(dedupe_rules, "Remove duplicate", fixed = TRUE)
})
