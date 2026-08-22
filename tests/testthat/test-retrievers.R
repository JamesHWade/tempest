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

test_that("retrievers own one authoritative research workspace", {
  cfg <- tempest_config(cache_dir = withr::local_tempdir())
  retriever <- tempest_retriever(config = cfg)

  expect_r6_class(retriever$workspace, "ResearchWorkspace")
  expect_equal("store" %in% names(retriever), FALSE)

  workspace <- test_research_workspace()
  retriever <- tempest_retriever(config = cfg, workspace = workspace)
  expect_identical(retriever$workspace, workspace)
  expect_equal("store" %in% names(retriever), FALSE)

  expect_error(
    tempest_retriever(config = cfg, workspace = new.env()),
    class = "tempest_research_workspace_error"
  )
})

test_that("retriever config identity is canonical and custom retrievers stay opaque", {
  first_config <- tempest_config(max_search_results = 2L)
  second_config <- tempest_config(max_search_results = 3L)
  retriever <- tempest_retriever(config = first_config)
  custom_retriever <- list(workspace = tempest_research_workspace())

  expect_identical(
    tempest:::tempest_retriever_config_digest(retriever),
    tempest:::tempest_research_config_digest(first_config)
  )
  expect_equal(
    tempest:::tempest_retriever_config_digest(retriever) ==
      tempest:::tempest_research_config_digest(second_config),
    FALSE
  )
  expect_null(
    tempest:::tempest_retriever_config_digest(custom_retriever)
  )
})

test_that("retriever correlation identities cannot be rebound", {
  config <- tempest_config(cache_dir = withr::local_tempdir())
  retriever <- tempest_retriever(config = config)
  workspace <- retriever$workspace
  replacement <- tempest_research_workspace()

  expect_error(
    retriever$config <- tempest_config(cache_dir = withr::local_tempdir()),
    class = "tempest_retriever_identity_error"
  )
  expect_error(
    retriever$workspace <- replacement,
    class = "tempest_retriever_identity_error"
  )
  expect_error(
    retriever$config@max_sources <- config@max_sources + 1L,
    class = "tempest_retriever_identity_error"
  )
  expect_error(
    retriever$ragnar_store <- new.env(parent = emptyenv()),
    class = "tempest_retriever_identity_error"
  )
  expect_error(
    retriever$cache_dir <- withr::local_tempdir(),
    class = "tempest_retriever_identity_error"
  )
  expect_error(
    retriever$cache_enabled <- !config@cache_enabled,
    class = "tempest_retriever_identity_error"
  )
  expect_error(
    retriever$cache_ttl <- config@cache_ttl + 1,
    class = "tempest_retriever_identity_error"
  )

  expect_identical(retriever$config, config)
  expect_identical(retriever$workspace, workspace)
  expect_identical(retriever$ragnar_store, config@ragnar_store)
  expect_identical(retriever$cache_dir, config@cache_dir)
  expect_identical(retriever$cache_enabled, isTRUE(config@cache_enabled))
  expect_identical(retriever$cache_ttl, config@cache_ttl)

  resource <- test_typed_web_resource(
    "https://example.org/mutable-workspace"
  )
  expect_no_error(retriever$workspace$upsert_retrieved_resource(resource))
  expect_identical(
    retriever$workspace$get_retrieved_resource(resource@resource_id),
    resource
  )
})

test_that("search() drops missing/unsafe URLs instead of aborting", {
  cfg <- tempest_config(cache_dir = withr::local_tempdir())
  retriever <- tempest_retriever(config = cfg)

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
  retriever <- tempest_retriever(config = cfg)

  expect_equal(retriever$workspace$max_sources, 1L)
  expect_error(
    retriever$search("too many", k = 3L, provider = "wikipedia"),
    class = "tempest_config_error"
  )
  retriever$workspace$upsert_retrieved_resource(test_typed_web_resource(
    "https://example.org/one"
  ))
  expect_error(
    retriever$workspace$upsert_retrieved_resource(test_typed_web_resource(
      "https://example.org/two"
    )),
    class = "tempest_research_workspace_integrity_error"
  )
})

test_that("retrieval telemetry starts only after validated preflight", {
  local_otel_opt_in()
  state <- local_fake_otel()
  retriever <- tempest_retriever(
    config = tempest_config(
      cache_dir = withr::local_tempdir(),
      max_search_results = 2L
    )
  )

  expect_error(
    retriever$search("too many", k = 3L, provider = "wikipedia"),
    class = "tempest_config_error"
  )
  expect_error(
    retriever$fetch("http://localhost/private"),
    class = "tempest_retriever_url_error"
  )
  expect_identical(state$start_calls, 0L)
})

test_that("search() caches repeated equivalent searches", {
  cfg <- tempest_config(cache_dir = withr::local_tempdir())
  retriever <- tempest_retriever(config = cfg)
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

test_that("search telemetry is branch-local, bounded, and content-free", {
  local_otel_opt_in()
  state <- local_fake_otel()
  config <- tempest_config(
    cache_dir = withr::local_tempdir(),
    max_search_results = 2L
  )
  retriever <- tempest_retriever(config = config)
  query_secret <- "query-secret-7d291"
  url_secret <- "url-secret-9f481"
  title_secret <- "title-secret-3c712"
  local_mocked_bindings(
    tempest_wiki_search = function(query, limit = 8L) {
      tempest:::tempest_search_results(
        title = rep(title_secret, 3L),
        url = paste0(
          "https://example.com/",
          1:3,
          "?token=",
          url_secret
        ),
        snippet = rep("private source content", 3L)
      )
    }
  )

  first <- retriever$search(query_secret, k = 2L, provider = "wikipedia")
  second <- retriever$search(query_secret, k = 2L, provider = "wikipedia")
  stats <- retriever$cache_stats()
  search_stats <- stats[stats$kind == "search", ]
  attributes <- lapply(state$spans, \(span) span$attributes)
  projected <- jsonlite::toJSON(attributes, auto_unbox = TRUE)

  expect_identical(second, first)
  expect_identical(nrow(first), 3L)
  expect_identical(
    vapply(
      state$spans,
      \(span) span$attributes[["tempest.cache_hit"]],
      logical(1)
    ),
    c(FALSE, TRUE)
  )
  expect_identical(
    vapply(
      state$spans,
      \(span) span$attributes[["tempest.result_count"]],
      integer(1)
    ),
    c(2L, 2L)
  )
  expect_identical(
    vapply(state$spans, \(span) span$name, character(1)),
    rep("tempest.retrieval.search", 2L)
  )
  expect_identical(
    vapply(state$spans, \(span) span$end_count, integer(1)),
    c(1L, 1L)
  )
  expect_null(state$spans[[1L]]$attributes[["tempest.fallback_taken"]])
  expect_identical(search_stats$hits, 1L)
  expect_identical(search_stats$misses, 1L)
  expect_identical(search_stats$writes, 1L)
  for (secret in c(
    query_secret,
    url_secret,
    title_secret,
    first$source_id
  )) {
    expect_no_match(projected, secret, fixed = TRUE)
  }
})

test_that("native search fallback is recorded only when that branch runs", {
  local_otel_opt_in()
  state <- local_fake_otel()
  retriever <- tempest_retriever(
    config = tempest_config(cache_dir = withr::local_tempdir())
  )
  local_mocked_bindings(
    tempest_wiki_search = function(query, limit = 8L) {
      if (identical(query, "failed native")) {
        stop("private native failure")
      }
      tempest:::tempest_search_results(
        title = "Fallback result",
        url = "https://example.com/native-fallback",
        snippet = "Fallback snippet"
      )
    }
  )

  first <- retriever$search("native fallback", provider = "native")
  second <- retriever$search("native fallback", provider = "native")
  expect_error(
    retriever$search("failed native", provider = "native"),
    class = "tempest_retriever_error"
  )

  expect_identical(second, first)
  expect_identical(
    state$spans[[1L]]$attributes[["tempest.fallback_taken"]],
    TRUE
  )
  expect_null(state$spans[[2L]]$attributes[["tempest.fallback_taken"]])
  expect_identical(
    state$spans[[2L]]$attributes[["tempest.cache_hit"]],
    TRUE
  )
  expect_null(state$spans[[3L]]$attributes[["tempest.fallback_taken"]])
  expect_identical(
    state$spans[[3L]]$attributes[["tempest.status"]],
    "failed"
  )
})

test_that("a cached NULL status is not a retrieval cache hit", {
  local_otel_opt_in()
  state <- local_fake_otel()
  config <- tempest_config(cache_dir = withr::local_tempdir())
  retriever <- tempest_retriever(config = config)
  key <- tempest:::tempest_search_cache_key(
    "wikipedia",
    "cached null",
    config@max_search_results
  )
  tempest:::tempest_cache_set(config@cache_dir, key, NULL)
  local_mocked_bindings(
    tempest_wiki_search = function(query, limit = 8L) {
      tempest:::tempest_search_results(
        title = "Fresh result",
        url = "https://example.com/fresh-after-null",
        snippet = "Fresh snippet"
      )
    }
  )

  result <- retriever$search("cached null", provider = "wikipedia")
  stats <- retriever$cache_stats()
  search_stats <- stats[stats$kind == "search", ]

  expect_identical(nrow(result), 1L)
  expect_identical(
    state$spans[[1L]]$attributes[["tempest.cache_hit"]],
    FALSE
  )
  expect_identical(search_stats$hits, 1L)
  expect_identical(search_stats$writes, 1L)
})

test_that("search() does not count writes when the cache write fails", {
  cfg <- tempest_config(cache_dir = withr::local_tempdir())
  retriever <- tempest_retriever(config = cfg)

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
  retriever <- tempest_retriever(config = cfg)
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
    )
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
  retriever <- tempest_retriever(config = cfg)
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
  expect_identical(S7::S7_inherits(first, tempest:::TempestResource), TRUE)
  expect_identical(S7::S7_inherits(second, tempest:::TempestResource), TRUE)
  expect_identical(first@resource_kind, "web")
  expect_identical(first@locator, "https://example.com/page")
  expect_identical(first@media_type, "text/html")
  expect_identical(
    first@resource_id,
    tempest:::tempest_source_id("https://example.com/page")
  )
  expect_identical(first@content, "Body 1")
  expect_identical(first@metadata$kind, "html")
  expect_identical(first@metadata$snippet, "Body 1")
  expect_equal(second@title, first@title)
  expect_equal(forced@title, "Title 2")
  expect_identical(
    first@content_hash,
    "f52748fc8a9dfb6adc2dbe7e5a4171a89dbadfda31bd4b786a2dec9c81700ba8"
  )
  expect_identical(
    retriever$workspace$get_retrieved_resource(first@resource_id),
    forced
  )
  expect_equal(fetch_stats$misses, 1L)
  expect_equal(fetch_stats$hits, 1L)
  expect_equal(fetch_stats$bypasses, 1L)
  expect_equal(fetch_stats$writes, 2L)
})

test_that("fetch() rejects legacy list values from the cache", {
  retriever <- tempest_retriever(
    config = tempest_config(cache_dir = withr::local_tempdir())
  )
  legacy <- list(
    id = tempest:::tempest_source_id("https://example.com/legacy-cache"),
    url = "https://example.com/legacy-cache",
    title = "Legacy cache value"
  )
  local_mocked_bindings(
    tempest_cache_lookup = function(...) list(value = legacy, status = "hit")
  )

  expect_error(
    retriever$fetch("https://example.com/legacy-cache"),
    class = "tempest_retriever_cache_error"
  )
  expect_length(retriever$workspace$list_retrieved_resources(), 0L)
})

test_that("fetch() rejects TempestResource subclasses from the cache", {
  retriever <- tempest_retriever(
    config = tempest_config(cache_dir = withr::local_tempdir())
  )
  resource <- test_subclassed_resource(
    test_typed_web_resource("https://example.com/subclass-cache")
  )
  local_mocked_bindings(
    tempest_cache_lookup = function(...) list(value = resource, status = "hit")
  )

  expect_error(
    retriever$fetch("https://example.com/subclass-cache"),
    class = "tempest_retriever_cache_error"
  )
  expect_length(retriever$workspace$list_retrieved_resources(), 0L)
})

test_that("fetch() rejects exact resources cached under the wrong URL key", {
  retriever <- tempest_retriever(
    config = tempest_config(cache_dir = withr::local_tempdir())
  )
  wrong <- test_typed_web_resource("https://example.com/wrong-cache-value")
  local_mocked_bindings(
    tempest_cache_lookup = function(...) list(value = wrong, status = "hit")
  )

  expect_error(
    retriever$fetch("https://example.com/requested-cache-value"),
    class = "tempest_retriever_cache_error"
  )
  expect_length(retriever$workspace$list_retrieved_resources(), 0L)
})

test_that("fetch() rejects cached resources with a noncurrent live schema", {
  retriever <- tempest_retriever(
    config = tempest_config(cache_dir = withr::local_tempdir())
  )
  resource <- test_typed_web_resource(
    "https://example.com/noncurrent-cache-schema"
  )
  attr(resource, "schema_version") <- 999L
  local_mocked_bindings(
    tempest_cache_lookup = function(...) list(value = resource, status = "hit")
  )

  expect_error(
    retriever$fetch("https://example.com/noncurrent-cache-schema"),
    class = "tempest_retriever_cache_error"
  )
  expect_length(retriever$workspace$list_retrieved_resources(), 0L)
})

test_that("fetch() rejects cached resources with mismatched inline hashes", {
  retriever <- tempest_retriever(
    config = tempest_config(cache_dir = withr::local_tempdir())
  )
  resource <- test_typed_web_resource(
    "https://example.com/mismatched-cache-content-hash"
  )
  attr(resource, "content_hash") <- strrep("0", 64L)
  local_mocked_bindings(
    tempest_cache_lookup = function(...) list(value = resource, status = "hit")
  )

  expect_error(
    retriever$fetch("https://example.com/mismatched-cache-content-hash"),
    class = "tempest_retriever_cache_error"
  )
  expect_length(retriever$workspace$list_retrieved_resources(), 0L)
})

test_that("fetch() wraps cached-resource workspace admission failures", {
  workspace <- tempest_research_workspace(max_sources = 1L)
  existing <- test_typed_web_resource("https://example.com/existing-resource")
  workspace$upsert_retrieved_resource(existing)
  retriever <- tempest_retriever(
    config = tempest_config(
      cache_dir = withr::local_tempdir(),
      max_sources = 1L
    ),
    workspace = workspace
  )
  cached <- test_typed_web_resource("https://example.com/cached-resource")
  local_mocked_bindings(
    tempest_cache_lookup = function(...) list(value = cached, status = "hit")
  )

  expect_error(
    retriever$fetch("https://example.com/cached-resource"),
    class = "tempest_retriever_cache_error"
  )
  expect_identical(workspace$list_retrieved_resources(), list(existing))
})

test_that("fetch() retries transient failures without force", {
  cfg <- tempest_config(cache_dir = withr::local_tempdir())
  retriever <- tempest_retriever(config = cfg)
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

  expect_identical(S7::S7_inherits(failed, tempest:::TempestResource), TRUE)
  expect_identical(failed@resource_kind, "web")
  expect_identical(failed@locator, "https://example.com/retry")
  expect_identical(failed@title, "https://example.com/retry")
  expect_identical(failed@media_type, "text/html")
  expect_identical(failed@metadata$error, "temporary timeout")
  expect_null(failed@content)
  expect_identical(recovered@content, "Recovered")
  expect_equal(calls, 2L)
  expect_equal(fetch_stats$misses, 2L)
  expect_equal(fetch_stats$writes, 1L)
})

test_that("fetch telemetry preserves products and projects error records", {
  local_otel_opt_in()
  state <- local_fake_otel()
  retriever <- tempest_retriever(
    config = tempest_config(cache_dir = withr::local_tempdir())
  )
  error_secret <- "fetch-error-secret-4e819"
  local_mocked_bindings(
    tempest_now_utc = function() "2026-08-20T12:00:00.000000Z",
    tempest_fetch_url_text = function(url, user_agent = NULL) {
      if (grepl("failed", url, fixed = TRUE)) {
        return(list(
          kind = "html",
          text = NA_character_,
          title = NA_character_,
          error = error_secret
        ))
      }
      list(
        kind = "html",
        text = "private fetched content",
        title = "Private fetched title",
        error = NULL
      )
    }
  )

  first <- retriever$fetch("https://example.com/fetched-url-secret")
  second <- retriever$fetch("https://example.com/fetched-url-secret")
  options(tempest.otel.enabled = FALSE)
  disabled_failed <- retriever$fetch(
    "https://example.com/failed-other-secret"
  )
  options(tempest.otel.enabled = TRUE)
  failed <- retriever$fetch("https://example.com/failed-other-secret")
  stats <- retriever$cache_stats()
  fetch_stats <- stats[stats$kind == "fetch", ]
  projected <- jsonlite::toJSON(
    lapply(state$spans, \(span) span$attributes),
    auto_unbox = TRUE
  )

  expect_identical(second, first)
  expect_identical(
    serialize(failed, NULL),
    serialize(disabled_failed, NULL)
  )
  expect_identical(failed@metadata$error, error_secret)
  expect_identical(
    vapply(
      state$spans,
      \(span) span$attributes[["tempest.cache_hit"]],
      logical(1)
    ),
    c(FALSE, TRUE, FALSE)
  )
  expect_identical(
    vapply(
      state$spans,
      \(span) span$attributes[["tempest.result_count"]],
      integer(1)
    ),
    c(1L, 1L, 0L)
  )
  expect_identical(
    vapply(
      state$spans,
      \(span) span$attributes[["tempest.status"]],
      character(1)
    ),
    c("succeeded", "succeeded", "failed")
  )
  expect_identical(
    state$spans[[3L]]$attributes[["tempest.error_class"]],
    "tempest_operation_error"
  )
  expect_identical(state$spans[[3L]]$statuses, "error")
  expect_identical(fetch_stats$hits, 1L)
  expect_identical(fetch_stats$misses, 3L)
  expect_identical(fetch_stats$writes, 1L)
  for (secret in c(
    "url-secret",
    "other-secret",
    error_secret,
    first@resource_id,
    failed@resource_id,
    "private fetched content",
    "Private fetched title"
  )) {
    expect_no_match(projected, secret, fixed = TRUE)
  }
})

test_that("retrieval telemetry failures cannot alter product state", {
  local_otel_opt_in()
  state <- local_fake_otel(
    provider_errors = c("set_attribute", "set_status", "end"),
    provider_conditions = c("set_attribute", "set_status", "end")
  )
  retriever <- tempest_retriever(
    config = tempest_config(
      cache_dir = withr::local_tempdir(),
      cache_enabled = FALSE
    )
  )
  local_mocked_bindings(
    tempest_now_utc = function() "2026-08-20T12:00:00.000000Z",
    tempest_wiki_search = function(query, limit = 8L) {
      tempest:::tempest_search_results(
        title = "Exact result",
        url = "https://example.com/exact",
        snippet = "Exact snippet"
      )
    },
    tempest_fetch_url_text = function(url, user_agent = NULL) {
      list(
        kind = "html",
        text = "Exact body",
        title = "Exact title",
        error = NULL
      )
    }
  )

  options(tempest.otel.enabled = FALSE)
  disabled_search <- retriever$search(
    "exact",
    provider = "wikipedia",
    force = TRUE
  )
  disabled_fetch <- retriever$fetch(
    "https://example.com/exact-fetch",
    force = TRUE
  )
  disabled_sources <- retriever$workspace$list_retrieved_sources()
  options(tempest.otel.enabled = TRUE)
  expect_silent(
    enabled_search <- retriever$search(
      "exact",
      provider = "wikipedia",
      force = TRUE
    )
  )
  expect_silent(
    enabled_fetch <- retriever$fetch(
      "https://example.com/exact-fetch",
      force = TRUE
    )
  )
  enabled_sources <- retriever$workspace$list_retrieved_sources()
  stats <- retriever$cache_stats()

  expect_identical(
    serialize(enabled_search, NULL),
    serialize(disabled_search, NULL)
  )
  expect_identical(
    serialize(enabled_fetch, NULL),
    serialize(disabled_fetch, NULL)
  )
  expect_identical(
    serialize(enabled_sources, NULL),
    serialize(disabled_sources, NULL)
  )
  expect_identical(stats$bypasses, c(2L, 2L))
  expect_identical(stats$hits, c(0L, 0L))
  expect_identical(stats$misses, c(0L, 0L))
  expect_length(state$spans, 2L)
})

test_that("terminal telemetry interrupts cannot replace retrieval errors", {
  local_otel_opt_in()
  state <- local_fake_otel(provider_interrupts = "set_attribute")
  retriever <- tempest_retriever(
    config = tempest_config(cache_dir = withr::local_tempdir())
  )
  original <- structure(
    list(message = "original private cache error", call = NULL),
    class = c("private_cache_error", "error", "condition")
  )
  local_mocked_bindings(
    tempest_cache_lookup = function(...) stop(original)
  )

  caught <- tryCatch(
    retriever$search("error identity", provider = "wikipedia"),
    error = identity
  )

  expect_identical(caught, original)
  expect_identical(state$spans[[1L]]$end_count, 1L)
})

test_that("nested search requests retain their own cache branch", {
  local_otel_opt_in()
  state <- local_fake_otel()
  retriever <- tempest_retriever(
    config = tempest_config(cache_dir = withr::local_tempdir())
  )
  cached <- tempest:::tempest_search_results(
    title = "Nested cached result",
    url = "https://example.com/nested-search",
    snippet = "Nested snippet"
  )
  cached$source_id <- vapply(
    cached$url,
    tempest:::tempest_source_id,
    character(1)
  )
  lookup_calls <- 0L
  nested_result <- NULL
  reverse_completion <- NULL
  local_mocked_bindings(
    tempest_cache_lookup = function(...) {
      lookup_calls <<- lookup_calls + 1L
      if (lookup_calls == 1L) {
        nested_result <<- retriever$search(
          "nested request",
          provider = "wikipedia"
        )
        return(list(value = NULL, status = "miss"))
      }
      list(value = cached, status = "hit")
    },
    tempest_cache_set = function(...) invisible(TRUE),
    tempest_wiki_search = function(query, limit = 8L) {
      reverse_completion <<- c(
        outer = state$spans[[1L]]$end_count,
        nested = state$spans[[2L]]$end_count
      )
      tempest:::tempest_search_results(
        title = "Outer fresh result",
        url = "https://example.com/outer-search",
        snippet = "Outer snippet"
      )
    }
  )

  outer_result <- retriever$search(
    "outer request",
    provider = "wikipedia"
  )
  stats <- retriever$cache_stats()
  search_stats <- stats[stats$kind == "search", ]

  expect_identical(nested_result, cached)
  expect_identical(outer_result$title, "Outer fresh result")
  expect_identical(reverse_completion, c(outer = 0L, nested = 1L))
  expect_identical(
    vapply(
      state$spans,
      \(span) span$attributes[["tempest.cache_hit"]],
      logical(1)
    ),
    c(FALSE, TRUE)
  )
  expect_identical(
    vapply(state$spans, \(span) span$end_count, integer(1)),
    c(1L, 1L)
  )
  expect_identical(search_stats$hits, 1L)
  expect_identical(search_stats$misses, 1L)
  expect_identical(search_stats$writes, 1L)
})

test_that("nested fetch requests retain their own cache branch", {
  local_otel_opt_in()
  state <- local_fake_otel()
  retriever <- tempest_retriever(
    config = tempest_config(cache_dir = withr::local_tempdir())
  )
  cached <- test_typed_web_resource("https://example.org/nested-fetch")
  lookup_calls <- 0L
  nested_result <- NULL
  reverse_completion <- NULL
  local_mocked_bindings(
    tempest_cache_lookup = function(...) {
      lookup_calls <<- lookup_calls + 1L
      if (lookup_calls == 1L) {
        nested_result <<- retriever$fetch(
          "https://example.org/nested-fetch"
        )
        return(list(value = NULL, status = "miss"))
      }
      list(value = cached, status = "hit")
    },
    tempest_cache_set = function(...) invisible(TRUE),
    tempest_now_utc = function() "2026-08-20T12:00:00.000000Z",
    tempest_fetch_url_text = function(url, user_agent = NULL) {
      reverse_completion <<- c(
        outer = state$spans[[1L]]$end_count,
        nested = state$spans[[2L]]$end_count
      )
      list(
        kind = "html",
        text = "Outer body",
        title = "Outer title",
        error = NULL
      )
    }
  )

  outer_result <- retriever$fetch("https://example.org/outer-fetch")
  stats <- retriever$cache_stats()
  fetch_stats <- stats[stats$kind == "fetch", ]

  expect_identical(nested_result, cached)
  expect_identical(outer_result@title, "Outer title")
  expect_identical(reverse_completion, c(outer = 0L, nested = 1L))
  expect_identical(
    vapply(
      state$spans,
      \(span) span$attributes[["tempest.cache_hit"]],
      logical(1)
    ),
    c(FALSE, TRUE)
  )
  expect_identical(
    vapply(state$spans, \(span) span$end_count, integer(1)),
    c(1L, 1L)
  )
  expect_identical(fetch_stats$hits, 1L)
  expect_identical(fetch_stats$misses, 1L)
  expect_identical(fetch_stats$writes, 1L)
  expect_length(retriever$workspace$list_retrieved_sources(), 2L)
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

test_that("Azure AI Search requires its canonical endpoint environment variable", {
  withr::local_envvar(c(
    AZURE_AI_SEARCH_API_KEY = "fake-key",
    AZURE_AI_SEARCH_ENDPOINT = "",
    AZURE_AI_SEARCH_URL = "https://retired-alias.example.org",
    AZURE_AI_SEARCH_INDEX_NAME = "fake-index"
  ))

  expect_error(
    tempest:::tempest_search_azure_ai_search("test"),
    "AZURE_AI_SEARCH_ENDPOINT"
  )
})

test_that("public retriever failures never expose credential material", {
  secret <- "Authorization: Bearer sk-live-secret"
  config <- tempest_config(
    cache_dir = withr::local_tempdir(),
    cache_enabled = FALSE
  )
  retriever <- tempest_retriever(config = config)

  local_mocked_bindings(
    tempest_wiki_search = function(...) stop(secret)
  )
  search_error <- expect_error(
    retriever$search("evidence", provider = "wikipedia", force = TRUE),
    class = "tempest_retriever_error"
  )
  expect_no_match(
    conditionMessage(search_error),
    "sk-live-secret",
    fixed = TRUE
  )
  search_print <- paste(capture.output(print(search_error)), collapse = "\n")
  expect_no_match(search_print, "sk-live-secret", fixed = TRUE)

  local_mocked_bindings(
    tempest_has = function(package) FALSE,
    tempest_http_get = function(...) stop(secret)
  )
  fetch_error <- expect_error(
    retriever$fetch("https://8.8.8.8/evidence", force = TRUE),
    class = "tempest_retriever_error"
  )
  expect_no_match(conditionMessage(fetch_error), "sk-live-secret", fixed = TRUE)
  fetch_print <- paste(capture.output(print(fetch_error)), collapse = "\n")
  expect_no_match(fetch_print, "sk-live-secret", fixed = TRUE)

  for (url in c(
    "http://alice:supersecret@localhost/private",
    paste0(
      "http://localhost/private?token=",
      "abcdefghijklmnopqrstuvwxyz1234567890"
    )
  )) {
    blocked <- expect_error(
      retriever$fetch(url, force = TRUE),
      class = "tempest_retriever_url_error"
    )
    expect_no_match(conditionMessage(blocked), "supersecret", fixed = TRUE)
    expect_no_match(
      conditionMessage(blocked),
      "abcdefghijklmnopqrstuvwxyz1234567890",
      fixed = TRUE
    )
  }
})

test_that("default ragnar retrieval uses hybrid top-k semantics safely", {
  skip_if_not_installed("ragnar")
  store <- list(store = "test")
  retriever <- tempest_retriever(
    config = tempest_config(ragnar_store = store)
  )
  received <- NULL
  local_mocked_bindings(
    tempest_ragnar_retrieve = function(store, query, top_k, method) {
      received <<- list(
        store = store,
        query = query,
        top_k = top_k,
        method = method
      )
      data.frame(text = "evidence")
    }
  )

  result <- retriever$retrieve("evidence", k = 7L)

  expect_identical(received$store, store)
  expect_identical(received$query, "evidence")
  expect_identical(received$top_k, 7L)
  expect_identical(received$method, "hybrid")
  expect_identical(result$text, "evidence")

  secret <- "Authorization: Bearer sk-live-secret"
  local_mocked_bindings(
    tempest_ragnar_retrieve = function(...) stop(secret)
  )
  error <- expect_error(
    retriever$retrieve("evidence"),
    class = "tempest_retriever_error"
  )
  expect_no_match(conditionMessage(error), "sk-live-secret", fixed = TRUE)
  printed <- paste(capture.output(print(error)), collapse = "\n")
  expect_no_match(printed, "sk-live-secret", fixed = TRUE)
})
