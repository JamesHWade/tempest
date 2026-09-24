test_that("tempest_source_id is deterministic", {
  id1 <- tempest:::tempest_source_id("https://example.com/a")
  id2 <- tempest:::tempest_source_id("https://example.com/a")
  expect_identical(id1, id2)
  expect_match(id1, "^S[0-9a-f]{12}$")
})

test_that("tempest_source_id rejects malformed public URL inputs", {
  for (url in list(42, "not a URL", "ftp://example.org/a", "https://")) {
    expect_error(
      tempest_source_id(url),
      class = "tempest_source_id_error"
    )
  }
})

test_that("citation extraction finds source ids", {
  txt <- "A claim [S123abc123abc] and another [S123abc123abc] and [Sdeadbeefdead]."
  ids <- tempest:::tempest_extract_citation_ids(txt)
  expect_equal(sort(ids), sort(c("S123abc123abc", "Sdeadbeefdead")))
})
