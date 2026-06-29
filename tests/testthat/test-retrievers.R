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

test_that("search() caches repeated equivalent searches", {
  cfg <- tempest_config(cache_dir = withr::local_tempdir())
  retriever <- tempest_retriever(config = cfg, store = SourceStore$new())
  calls <- 0L

  local_mocked_bindings(
    tempest_wiki_search = function(query, limit = 8) {
      calls <<- calls + 1L
      tempest:::tempest_search_results(
        title = paste("Result", calls),
        url = paste0("https://example.com/", calls),
        snippet = paste("Snippet", calls)
      )
    }
  )

  first <- retriever$search(" cache me ", provider = "wikipedia")
  second <- retriever$search("cache me", provider = "wikipedia")
  forced <- retriever$search("cache me", provider = "wikipedia", force = TRUE)
  stats <- retriever$cache_stats()
  search_stats <- stats[stats$kind == "search", ]

  expect_equal(calls, 2L)
  expect_equal(second$url, first$url)
  expect_equal(forced$url, "https://example.com/2")
  expect_equal(search_stats$misses, 1L)
  expect_equal(search_stats$hits, 1L)
  expect_equal(search_stats$bypasses, 1L)
  expect_equal(search_stats$writes, 2L)
})

test_that("search() does not count writes when the cache write fails", {
  cfg <- tempest_config(cache_dir = withr::local_tempdir())
  retriever <- tempest_retriever(config = cfg, store = SourceStore$new())

  local_mocked_bindings(
    tempest_wiki_search = function(query, limit = 8) {
      tempest:::tempest_search_results(
        title = "Result",
        url = "https://example.com/no-write",
        snippet = "Snippet"
      )
    },
    tempest_cache_set = function(cache_dir, key, value) invisible(FALSE)
  )

  retriever$search("no write", provider = "wikipedia")
  search_stats <- retriever$cache_stats()
  search_stats <- search_stats[search_stats$kind == "search", ]

  expect_equal(search_stats$writes, 0L)
  expect_equal(search_stats$misses, 1L)
})

test_that("search cache respects TTL and can be disabled", {
  cache_dir <- withr::local_tempdir()
  cfg <- tempest_config(cache_dir = cache_dir, cache_ttl = 60)
  retriever <- tempest_retriever(config = cfg, store = SourceStore$new())
  calls <- 0L

  local_mocked_bindings(
    tempest_wiki_search = function(query, limit = 8) {
      calls <<- calls + 1L
      tempest:::tempest_search_results(
        title = paste("Result", calls),
        url = paste0("https://example.com/ttl-", calls),
        snippet = paste("Snippet", calls)
      )
    }
  )

  retriever$search("ttl", provider = "wikipedia")
  cache_file <- list.files(cache_dir, pattern = "\\.rds$", full.names = TRUE)
  expect_length(cache_file, 1L)
  Sys.setFileTime(cache_file, Sys.time() - 3600)
  retriever$search("ttl", provider = "wikipedia")

  disabled <- tempest_retriever(
    config = tempest_config(
      cache_dir = withr::local_tempdir(),
      cache_enabled = FALSE
    ),
    store = SourceStore$new()
  )
  disabled$search("ttl", provider = "wikipedia")
  disabled$search("ttl", provider = "wikipedia")

  stats <- retriever$cache_stats()
  search_stats <- stats[stats$kind == "search", ]
  disabled_stats <- disabled$cache_stats()
  disabled_search_stats <- disabled_stats[disabled_stats$kind == "search", ]

  expect_equal(calls, 4L)
  expect_equal(search_stats$misses, 1L)
  expect_equal(search_stats$expired, 1L)
  expect_equal(search_stats$writes, 2L)
  expect_equal(disabled_search_stats$bypasses, 2L)
  expect_equal(disabled_search_stats$writes, 0L)
})

test_that("search cache keys include provider options", {
  withr::local_envvar(DUCKDUCKGO_REGION = "us-en")
  us_key <- tempest:::tempest_search_cache_key("duckduckgo", "topic", 8)
  withr::local_envvar(DUCKDUCKGO_REGION = "uk-en")
  uk_key <- tempest:::tempest_search_cache_key("duckduckgo", "topic", 8)

  expect_equal(identical(us_key, uk_key), FALSE)
})

test_that("fetch() caches content and force refreshes", {
  cfg <- tempest_config(cache_dir = withr::local_tempdir())
  retriever <- tempest_retriever(config = cfg, store = SourceStore$new())
  calls <- 0L

  local_mocked_bindings(
    tempest_fetch_url_text = function(url, user_agent = NULL) {
      calls <<- calls + 1L
      list(
        kind = "html",
        text = paste("Body", calls),
        title = paste("Title", calls),
        error = NULL
      )
    }
  )

  first <- retriever$fetch("https://example.com/page")
  second <- retriever$fetch("https://example.com/page")
  forced <- retriever$fetch("https://example.com/page", force = TRUE)
  stats <- retriever$cache_stats()
  fetch_stats <- stats[stats$kind == "fetch", ]

  expect_equal(calls, 2L)
  expect_equal(second$title, first$title)
  expect_equal(forced$title, "Title 2")
  expect_equal(fetch_stats$misses, 1L)
  expect_equal(fetch_stats$hits, 1L)
  expect_equal(fetch_stats$bypasses, 1L)
  expect_equal(fetch_stats$writes, 2L)
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

  expect_error(
    tempest:::tempest_search_you("test"),
    class = "tempest_missing_envvar_error"
  )
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

test_that("URL safety errors are classed", {
  expect_error(
    tempest:::tempest_normalize_url("file:///etc/passwd"),
    class = "tempest_retriever_url_error"
  )
  expect_error(
    tempest:::tempest_normalize_url("http://localhost/private"),
    class = "tempest_retriever_url_error"
  )
  expect_error(
    tempest:::tempest_fetch_url_text(""),
    class = "tempest_retriever_url_error"
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
