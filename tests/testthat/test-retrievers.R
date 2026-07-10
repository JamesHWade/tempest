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

test_that("retriever enforces search and source budgets", {
  cfg <- tempest_config(
    cache_dir = withr::local_tempdir(),
    max_search_results = 2L,
    max_sources = 1L
  )
  retriever <- tempest_retriever(config = cfg, store = SourceStore$new())

  expect_equal(retriever$store$max_sources, 1L)
  expect_error(
    retriever$search("too many", k = 3L, provider = "wikipedia"),
    class = "tempest_config_error"
  )
  retriever$store$upsert_source(fake_source("https://example.org/one"))
  expect_error(
    retriever$store$upsert_source(fake_source("https://example.org/two")),
    class = "tempest_source_store_integrity_error"
  )
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

test_that("fetch() retries transient failures without force", {
  cfg <- tempest_config(cache_dir = withr::local_tempdir())
  retriever <- tempest_retriever(config = cfg, store = SourceStore$new())
  calls <- 0L

  local_mocked_bindings(
    tempest_fetch_url_text = function(url, user_agent = NULL) {
      calls <<- calls + 1L
      if (calls == 1L) {
        return(list(
          kind = "html",
          text = NA_character_,
          title = NA_character_,
          error = "temporary timeout"
        ))
      }
      list(kind = "html", text = "Recovered", title = "Recovered", error = NULL)
    }
  )

  failed <- retriever$fetch("https://example.com/retry")
  recovered <- retriever$fetch("https://example.com/retry")
  stats <- retriever$cache_stats()
  fetch_stats <- stats[stats$kind == "fetch", ]

  expect_equal(failed$meta$error, "temporary timeout")
  expect_equal(recovered$content_text, "Recovered")
  expect_equal(calls, 2L)
  expect_equal(fetch_stats$misses, 2L)
  expect_equal(fetch_stats$writes, 1L)
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

test_that("API provider adapters parse success, empty, and malformed fixtures", {
  withr::local_envvar(c(
    YDC_API_KEY = "test",
    BING_SEARCH_API_KEY = "test",
    SERPER_API_KEY = "test",
    BRAVE_API_KEY = "test",
    TAVILY_API_KEY = "test",
    SEARXNG_API_URL = "https://search.example.org/search",
    GOOGLE_SEARCH_API_KEY = "test",
    GOOGLE_CSE_ID = "test",
    AZURE_AI_SEARCH_API_KEY = "test",
    AZURE_AI_SEARCH_ENDPOINT = "https://azure.example.org",
    AZURE_AI_SEARCH_INDEX_NAME = "test"
  ))
  current_body <- list()
  local_mocked_bindings(
    tempest_search_perform = function(req, timeout_s = 30L) {
      httr2::response_json(body = current_body)
    }
  )
  providers <- list(
    you = list(
      fn = tempest:::tempest_search_you,
      success = list(
        hits = list(list(
          title = "You result",
          url = "https://example.org/you",
          snippets = list("You snippet")
        ))
      ),
      empty = list(hits = list()),
      malformed = list(hits = "bad")
    ),
    bing = list(
      fn = tempest:::tempest_search_bing,
      success = list(
        webPages = list(
          value = list(list(
            name = "Bing result",
            url = "https://example.org/bing",
            snippet = "Bing snippet"
          ))
        )
      ),
      empty = list(webPages = list(value = list())),
      malformed = list(webPages = list(value = "bad"))
    ),
    serper = list(
      fn = tempest:::tempest_search_serper,
      success = list(
        organic = list(list(
          title = "Serper result",
          link = "https://example.org/serper",
          snippet = "Serper snippet"
        ))
      ),
      empty = list(organic = list()),
      malformed = list(organic = "bad")
    ),
    brave = list(
      fn = tempest:::tempest_search_brave,
      success = list(
        web = list(
          results = list(list(
            title = "Brave result",
            url = "https://example.org/brave",
            description = "Brave snippet"
          ))
        )
      ),
      empty = list(web = list(results = list())),
      malformed = list(web = list(results = "bad"))
    ),
    tavily = list(
      fn = tempest:::tempest_search_tavily,
      success = list(
        results = list(list(
          title = "Tavily result",
          url = "https://example.org/tavily",
          content = "Tavily snippet"
        ))
      ),
      empty = list(results = list()),
      malformed = list(results = "bad")
    ),
    searxng = list(
      fn = tempest:::tempest_search_searxng,
      success = list(
        results = list(list(
          title = "SearXNG result",
          url = "https://example.org/searxng",
          content = "SearXNG snippet"
        ))
      ),
      empty = list(results = list()),
      malformed = list(results = "bad")
    ),
    google = list(
      fn = tempest:::tempest_search_google,
      success = list(
        items = list(list(
          title = "Google result",
          link = "https://example.org/google",
          snippet = "Google snippet"
        ))
      ),
      empty = list(items = list()),
      malformed = list(items = "bad")
    ),
    azure = list(
      fn = tempest:::tempest_search_azure_ai_search,
      success = list(
        value = list(list(
          title = "Azure result",
          url = "https://example.org/azure",
          content = "Azure snippet"
        ))
      ),
      empty = list(value = list()),
      malformed = list(value = "bad")
    )
  )

  for (provider in providers) {
    current_body <- provider$success
    success <- provider$fn("fixture", k = 2L)
    expect_equal(nrow(success), 1L)
    expect_named(success, c("title", "url", "snippet"))
    expect_match(success$url, "^https://example[.]org/")

    current_body <- provider$empty
    empty <- provider$fn("fixture", k = 2L)
    expect_equal(nrow(empty), 0L)

    current_body <- provider$malformed
    expect_error(
      provider$fn("fixture", k = 2L),
      class = "tempest_search_response_error"
    )
  }
})

test_that("search request policy retries only bounded transient statuses", {
  req <- tempest:::tempest_search_request_policy(
    httr2::request("https://example.org/search"),
    timeout_s = 7
  )
  transient <- req$policies$retry_is_transient

  expect_equal(req$options$timeout_ms, 7000)
  expect_equal(req$policies$retry_max_tries, 3)
  for (status in c(429L, 500L, 502L, 503L, 504L)) {
    expect_equal(transient(httr2::response(status_code = status)), TRUE)
  }
  for (status in c(400L, 401L, 403L, 404L)) {
    expect_equal(transient(httr2::response(status_code = status)), FALSE)
  }
})

test_that("DuckDuckGo adapter parses deterministic HTML fixtures", {
  skip_if_not_installed("xml2")
  skip_if_not_installed("rvest")
  html <- paste0(
    '<div class="result"><a class="result__a" ',
    'href="https://example.org/ddg">DDG result</a>',
    '<div class="result__snippet">DDG snippet</div></div>'
  )
  current_html <- html
  local_mocked_bindings(
    tempest_search_perform = function(req, timeout_s = 30L) {
      httr2::response(
        headers = list(`content-type` = "text/html"),
        body = charToRaw(current_html)
      )
    }
  )

  result <- tempest:::tempest_search_duckduckgo("fixture", k = 2L)
  expect_equal(result$title, "DDG result")
  expect_equal(result$url, "https://example.org/ddg")
  expect_equal(result$snippet, "DDG snippet")

  current_html <- "<html><body>No results</body></html>"
  expect_equal(nrow(tempest:::tempest_search_duckduckgo("empty")), 0L)
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

test_that("URL safety rejects obfuscated and resolved private addresses", {
  blocked <- c(
    "http://169.254.169.254/latest/meta-data",
    "http://2130706433",
    "http://0x7f000001",
    "http://0177.0.0.1",
    "http://100.64.0.1",
    "http://[::1]/",
    "http://[::ffff:127.0.0.1]/"
  )
  for (url in blocked) {
    expect_error(
      tempest:::tempest_normalize_url(url),
      class = "tempest_retriever_url_error"
    )
  }

  expect_equal(tempest:::tempest_url_is_safe(NA_character_), FALSE)
  expect_equal(tempest:::tempest_url_is_safe(""), FALSE)
  expect_error(
    tempest:::tempest_validate_fetch_url(
      "https://research.example/page",
      resolver = function(host) "10.0.0.2"
    ),
    class = "tempest_retriever_url_error"
  )
  expect_equal(
    tempest:::tempest_validate_fetch_url(
      "https://research.example/page",
      resolver = function(host) "93.184.216.34"
    ),
    "https://research.example/page"
  )
})

test_that("bounded HTTP fetch revalidates redirects and response limits", {
  validate <- function(url) tempest:::tempest_normalize_url(url)
  redirect_calls <- 0L
  redirect_perform <- function(req) {
    redirect_calls <<- redirect_calls + 1L
    if (redirect_calls == 1L) {
      return(httr2::response(
        status_code = 302L,
        headers = list(location = "http://127.0.0.1/private")
      ))
    }
    httr2::response(body = charToRaw("private"))
  }
  expect_error(
    tempest:::tempest_http_get(
      "https://example.org/start",
      perform = redirect_perform,
      validate = validate
    ),
    class = "tempest_retriever_url_error"
  )
  expect_equal(redirect_calls, 1L)

  expect_error(
    tempest:::tempest_http_get(
      "https://example.org/large",
      max_bytes = 2,
      perform = function(req) httr2::response(body = charToRaw("large")),
      validate = validate
    ),
    class = "tempest_retriever_url_error"
  )
  expect_error(
    tempest:::tempest_http_get(
      "https://example.org/archive",
      perform = function(req) {
        httr2::response(
          headers = list(`content-type` = "application/zip"),
          body = charToRaw("zip")
        )
      },
      validate = validate
    ),
    class = "tempest_retriever_url_error"
  )

  captured <- NULL
  response <- tempest:::tempest_http_get(
    "https://example.org/public",
    timeout_s = 3,
    perform = function(req) {
      captured <<- req
      httr2::response(
        headers = list(`content-type` = "text/html"),
        body = charToRaw("<p>public</p>")
      )
    },
    validate = validate
  )
  expect_s3_class(response, "httr2_response")
  expect_equal(captured$options$timeout_ms, 3000)
  expect_equal(captured$options$connecttimeout, 3)
  expect_equal(captured$options$followlocation, FALSE)
})

test_that("bounded HTTP fetch validates option values", {
  invalid <- list(
    list(timeout_s = 0),
    list(timeout_s = Inf),
    list(max_bytes = NA_real_),
    list(max_redirects = -1),
    list(max_redirects = c(1, 2))
  )
  for (args in invalid) {
    expect_error(
      do.call(
        tempest:::tempest_http_get,
        c(list(url = "https://example.org"), args)
      ),
      class = "tempest_error"
    )
  }
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
