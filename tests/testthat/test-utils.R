test_that("storm_source_id is deterministic", {
  id1 <- stormr:::storm_source_id("https://example.com/a")
  id2 <- stormr:::storm_source_id("https://example.com/a")
  expect_identical(id1, id2)
  expect_true(grepl("^S[0-9a-f]{12}$", id1))
})

test_that("citation extraction finds source ids", {
  txt <- "A claim [S123abc123abc] and another [S123abc123abc] and [Sdeadbeefdead]."
  ids <- stormr:::storm_extract_citation_ids(txt)
  expect_equal(sort(ids), sort(c("S123abc123abc", "Sdeadbeefdead")))
})
