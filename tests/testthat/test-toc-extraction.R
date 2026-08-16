test_that("tempest_extract_toc_from_url extracts nested headings", {
  skip_if_not_installed("xml2")

  html <- paste0(
    "<html><body>",
    "<h2>Overview</h2>",
    "<h3> Evidence </h3>",
    "<h4>   </h4>",
    "<h2>Conclusions</h2>",
    "</body></html>"
  )
  local_mocked_bindings(
    tempest_normalize_url = function(url) url,
    tempest_http_get = function(url, timeout_s) {
      httr2::response(
        headers = list(`content-type` = "text/html"),
        body = charToRaw(html)
      )
    }
  )

  result <- tempest:::tempest_extract_toc_from_url("https://example.org/page")

  expect_identical(
    result,
    c("- Overview", "  - Evidence", "- Conclusions")
  )
})

test_that("tempest_extract_toc_from_url handles NA gracefully", {
  result <- tempest:::tempest_extract_toc_from_url(NA_character_)
  expect_type(result, "character")
  expect_length(result, 0)
})

test_that("tempest_extract_toc_from_url returns empty when xml2 is unavailable", {
  requested <- FALSE
  local_mocked_bindings(
    tempest_has = function(pkg) FALSE,
    tempest_http_get = function(...) {
      requested <<- TRUE
      stop("network request should not occur")
    }
  )
  result <- tempest:::tempest_extract_toc_from_url("https://example.com")

  expect_identical(result, character())
  expect_identical(requested, FALSE)
})

test_that("tempest_wiki_page_sections formats nested sections", {
  params <- NULL
  local_mocked_bindings(
    tempest_wikipedia_api = function(value) {
      params <<- value
      list(
        parse = list(
          sections = data.frame(
            toclevel = c(1L, 2L, 3L, 1L),
            line = c("History", "Early work", "Modern era", "Applications")
          )
        )
      )
    }
  )

  result <- tempest:::tempest_wiki_page_sections("Quantum computing")

  expect_identical(
    result,
    c(
      "- History",
      "  - Early work",
      "    - Modern era",
      "- Applications"
    )
  )
  expect_identical(params$page, "Quantum computing")
  expect_identical(params$prop, "sections")
})

test_that("tempest_wiki_page_sections handles a missing page", {
  local_mocked_bindings(
    tempest_wikipedia_api = function(params) {
      list(error = list(code = "missingtitle"))
    }
  )

  result <- tempest:::tempest_wiki_page_sections("Missing page")

  expect_identical(result, character())
})

test_that("tempest_wiki_page_sections handles empty title", {
  requested_title <- NULL
  local_mocked_bindings(
    tempest_wikipedia_api = function(params) {
      requested_title <<- params$page
      list(parse = list(sections = list()))
    }
  )

  result <- tempest:::tempest_wiki_page_sections("")

  expect_identical(result, character())
  expect_identical(requested_title, "")
})
