test_that("tempest_extract_toc_from_url returns character vector", {
  skip_if_not_installed("xml2")
  skip_if_not_installed("rvest")

  # Should return character() on invalid/unreachable URLs (warns as expected)
  result <- suppressWarnings(tempest:::tempest_extract_toc_from_url(
    "https://this-will-not-resolve-12345.example.com"
  ))
  expect_type(result, "character")
  expect_length(result, 0)
})

test_that("tempest_extract_toc_from_url handles NA gracefully", {
  result <- tempest:::tempest_extract_toc_from_url(NA_character_)
  expect_type(result, "character")
  expect_length(result, 0)
})

test_that("tempest_extract_toc_from_url returns empty when xml2 is unavailable", {
  # Simulate xml2 being absent: the function must short-circuit to an empty
  # result without making any network request.
  local_mocked_bindings(tempest_has = function(pkg) FALSE)
  result <- tempest:::tempest_extract_toc_from_url("https://example.com")
  expect_identical(result, character())
})

test_that("tempest_wiki_page_sections returns character vector", {
  skip_on_cran()
  skip_if_offline()

  result <- tempest:::tempest_wiki_page_sections("Quantum computing")
  expect_type(result, "character")
  # Wikipedia should have sections
  if (length(result) > 0) {
    expect_match(result, "^\\s*- ")
  }
})

test_that("tempest_wiki_page_sections handles nonexistent page", {
  skip_on_cran()
  skip_if_offline()

  result <- tempest:::tempest_wiki_page_sections(
    "Thispagereallydoesnotexist12345xyz"
  )
  expect_type(result, "character")
  # Should be empty or gracefully handled
})

test_that("tempest_wiki_page_sections handles empty title", {
  result <- tempest:::tempest_wiki_page_sections("")
  expect_type(result, "character")
})
