test_that("search result helpers return the standard retriever shape", {
  empty <- tempest:::tempest_empty_search_results()
  expect_s3_class(empty, "tbl_df")
  expect_named(empty, c("title", "url", "snippet"))
  expect_equal(nrow(empty), 0L)

  results <- tempest:::tempest_search_results(
    title = "Result",
    url = "https://example.com"
  )
  expect_equal(nrow(results), 1L)
  expect_equal(results$title, "Result")
  expect_equal(results$url, "https://example.com")
  expect_equal(results$snippet, NA_character_)
})

test_that("search() drops missing/unsafe URLs instead of aborting", {
  cfg <- tempest_config(cache_dir = withr::local_tempdir())
  retriever <- tempest_retriever(config = cfg, store = SourceStore$new())

  local_mocked_bindings(
    tempest_wiki_search = function(query, limit = 8) {
      tempest:::tempest_search_results(
        title = c("Good", "Local", "Missing"),
        url = c("https://example.com/a", "http://localhost/secret", NA),
        snippet = c("s1", "s2", "s3")
      )
    }
  )

  result <- retriever$search("anything", provider = "wikipedia")

  expect_equal(nrow(result), 1L)
  expect_equal(result$url, "https://example.com/a")
  expect_contains(names(result), "source_id")
  expect_equal(which(is.na(result$source_id)), integer())
})

test_that("DuckDuckGo redirect URLs are decoded before normalization", {
  url <- paste0(
    "//duckduckgo.com/l/?uddg=",
    utils::URLencode("https://example.com/a page?x=1&y=2", reserved = TRUE),
    "&rut=abc"
  )

  expect_equal(
    tempest:::tempest_duckduckgo_result_url(url),
    "https://example.com/a page?x=1&y=2"
  )
  expect_equal(
    tempest:::tempest_duckduckgo_result_url("https://example.com"),
    "https://example.com"
  )
})

test_that("API-backed search providers fail before network calls without configuration", {
  withr::local_envvar(c(
    YDC_API_KEY = "",
    BING_SEARCH_API_KEY = "",
    SERPER_API_KEY = "",
    BRAVE_API_KEY = "",
    TAVILY_API_KEY = "",
    SEARXNG_API_URL = "",
    GOOGLE_SEARCH_API_KEY = "",
    GOOGLE_CSE_ID = "",
    AZURE_AI_SEARCH_API_KEY = "",
    AZURE_AI_SEARCH_ENDPOINT = "",
    AZURE_AI_SEARCH_URL = "",
    AZURE_AI_SEARCH_INDEX_NAME = ""
  ))

  expect_error(tempest:::tempest_search_you("test"), "YDC_API_KEY")
  expect_error(tempest:::tempest_search_bing("test"), "BING_SEARCH_API_KEY")
  expect_error(tempest:::tempest_search_serper("test"), "SERPER_API_KEY")
  expect_error(tempest:::tempest_search_brave("test"), "BRAVE_API_KEY")
  expect_error(tempest:::tempest_search_tavily("test"), "TAVILY_API_KEY")
  expect_error(tempest:::tempest_search_searxng("test"), "SEARXNG_API_URL")
  expect_error(
    tempest:::tempest_search_google("test"),
    "GOOGLE_SEARCH_API_KEY"
  )
  expect_error(
    tempest:::tempest_search_azure_ai_search("test"),
    "AZURE_AI_SEARCH_API_KEY"
  )
})

test_that("Azure AI Search supports upstream endpoint env alias", {
  withr::local_envvar(c(
    AZURE_AI_SEARCH_API_KEY = "fake-key",
    AZURE_AI_SEARCH_ENDPOINT = "",
    AZURE_AI_SEARCH_URL = "",
    AZURE_AI_SEARCH_INDEX_NAME = "fake-index"
  ))

  expect_error(
    tempest:::tempest_search_azure_ai_search("test"),
    "AZURE_AI_SEARCH_ENDPOINT"
  )
})
