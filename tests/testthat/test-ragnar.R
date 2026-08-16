test_that("tempest_create_ragnar_store creates store with correct schema", {
  skip_if_not_installed("ragnar")

  # Use in-memory store
  mock_embed <- function(x) matrix(stats::rnorm(length(x) * 3), ncol = 3)
  store <- tempest_create_ragnar_store(mock_embed, cache_dir = NULL)

  expect_s7_class(store, getFromNamespace("DuckDBRagnarStore", "ragnar"))
  expect_type(store@embed, "closure")
})

test_that("ragnar creation failures never expose credential material", {
  skip_if_not_installed("ragnar")
  secret <- "Authorization: Bearer sk-live-ragnar-secret"
  local_mocked_bindings(
    tempest_ragnar_store_create = function(...) stop(secret)
  )

  error <- expect_error(
    tempest_create_ragnar_store(function(x) matrix(1, ncol = 1)),
    class = "tempest_config_error"
  )

  expect_no_match(
    conditionMessage(error),
    "sk-live-ragnar-secret",
    fixed = TRUE
  )
  printed <- paste(capture.output(print(error)), collapse = "\n")
  expect_no_match(printed, "sk-live-ragnar-secret", fixed = TRUE)
})

test_that("ragnar embedding failures never expose credential material", {
  skip_if_not_installed("ragnar")
  cache_dir <- withr::local_tempdir()
  embed <- function(x) matrix(seq_len(length(x) * 3), ncol = 3)
  store <- tempest_create_ragnar_store(embed, cache_dir = cache_dir)
  DBI::dbDisconnect(store@con, shutdown = TRUE)
  secret <- "Authorization: Bearer sk-live-embedding-secret"

  error <- expect_error(
    tempest_create_ragnar_store(
      function(...) stop(secret),
      cache_dir = cache_dir
    ),
    class = "tempest_config_error"
  )

  expect_no_match(
    conditionMessage(error),
    "sk-live-embedding-secret",
    fixed = TRUE
  )
  printed <- paste(capture.output(print(error)), collapse = "\n")
  expect_no_match(printed, "sk-live-embedding-secret", fixed = TRUE)
})

test_that("TempestConfig creates ragnar_store from embed_fn", {
  skip_if_not_installed("ragnar")

  mock_embed <- function(x) matrix(stats::rnorm(length(x) * 3), ncol = 3)
  cfg <- tempest_config(embed_fn = mock_embed)

  expect_s7_class(
    cfg@ragnar_store,
    getFromNamespace("DuckDBRagnarStore", "ragnar")
  )
  expect_type(cfg@embed_fn, "closure")
})

test_that("TempestConfig accepts pre-built ragnar_store", {
  skip_if_not_installed("ragnar")

  mock_embed <- function(x) matrix(stats::rnorm(length(x) * 3), ncol = 3)
  pre_store <- tempest_create_ragnar_store(mock_embed, cache_dir = NULL)
  cfg <- tempest_config(ragnar_store = pre_store)

  expect_identical(cfg@ragnar_store, pre_store)
})

test_that("TempestRetriever inherits ragnar_store from config", {
  skip_if_not_installed("ragnar")

  mock_embed <- function(x) matrix(stats::rnorm(length(x) * 3), ncol = 3)
  cfg <- tempest_config(embed_fn = mock_embed)
  retriever <- tempest_retriever(config = cfg)

  expect_s7_class(
    retriever$ragnar_store,
    getFromNamespace("DuckDBRagnarStore", "ragnar")
  )
  expect_identical(retriever$ragnar_store, cfg@ragnar_store)
})

test_that("persistent Ragnar stores are reused unless reset is explicit", {
  skip_if_not_installed("ragnar")

  cache_dir <- withr::local_tempdir()
  mock_embed <- function(x) matrix(seq_len(length(x) * 3), ncol = 3)
  first <- tempest_create_ragnar_store(mock_embed, cache_dir = cache_dir)
  retriever <- tempest_retriever(
    config = tempest_config(ragnar_store = first)
  )
  retriever$ingest_to_ragnar(
    source_id = "S123abc123abc",
    url = "https://example.org/persistent",
    title = "Persistent",
    text = "Persistent knowledge remains available after reconnecting.",
    fetched_at = "2026-01-01T00:00:00Z",
    content_type = "html"
  )
  before <- DBI::dbGetQuery(first@con, "SELECT COUNT(*) AS n FROM documents")$n
  DBI::dbDisconnect(first@con, shutdown = TRUE)

  reopened <- tempest_create_ragnar_store(mock_embed, cache_dir = cache_dir)
  expect_s7_class(
    reopened,
    getFromNamespace("DuckDBRagnarStore", "ragnar")
  )
  after <- DBI::dbGetQuery(
    reopened@con,
    "SELECT COUNT(*) AS n FROM documents"
  )$n
  expect_equal(after, before)
  DBI::dbDisconnect(reopened@con, shutdown = TRUE)
  expect_error(
    tempest_create_ragnar_store(
      function(x) matrix(seq_len(length(x) * 4), ncol = 4),
      cache_dir = cache_dir
    ),
    class = "tempest_config_error"
  )
  reset <- tempest_create_ragnar_store(
    mock_embed,
    cache_dir = cache_dir,
    reset = TRUE
  )
  expect_s7_class(reset, getFromNamespace("DuckDBRagnarStore", "ragnar"))
})

test_that("ingest_to_ragnar chunks text and adds metadata", {
  skip_if_not_installed("ragnar")

  mock_embed <- function(x) matrix(stats::rnorm(length(x) * 3), ncol = 3)
  cfg <- tempest_config(embed_fn = mock_embed)
  retriever <- tempest_retriever(config = cfg)

  # Ingest a sample document
  n_chunks <- retriever$ingest_to_ragnar(
    source_id = "S123abc123abc",
    url = "https://example.com/test",
    title = "Test Document",
    text = paste(rep("This is test content for chunking. ", 50), collapse = ""),
    fetched_at = "2025-01-01T00:00:00Z",
    content_type = "html",
    perspective = "test_perspective"
  )

  expect_gt(n_chunks, 0)
})

test_that("ragnar insert and build failures never expose credentials", {
  skip_if_not_installed("ragnar")
  secret <- "Authorization: Bearer sk-live-ragnar-secret"
  retriever <- tempest_retriever(
    config = tempest_config(ragnar_store = list(store = "test"))
  )
  insert_calls <- 0L
  local_mocked_bindings(
    tempest_ragnar_store_insert = function(...) {
      insert_calls <<- insert_calls + 1L
      stop(secret)
    }
  )

  insert_error <- expect_error(
    retriever$ingest_to_ragnar(
      source_id = "S123abc123abc",
      url = "https://example.org/provider-error",
      title = "Provider error",
      text = paste(rep("Captured evidence text. ", 50), collapse = ""),
      fetched_at = "2026-01-01T00:00:00Z",
      content_type = "html"
    ),
    class = "tempest_retriever_error"
  )
  expect_identical(insert_calls, 1L)
  expect_no_match(
    conditionMessage(insert_error),
    "sk-live-ragnar-secret",
    fixed = TRUE
  )
  insert_print <- paste(capture.output(print(insert_error)), collapse = "\n")
  expect_no_match(insert_print, "sk-live-ragnar-secret", fixed = TRUE)

  build_calls <- 0L
  local_mocked_bindings(
    tempest_ragnar_store_build_index = function(...) {
      build_calls <<- build_calls + 1L
      stop(secret)
    }
  )
  build_error <- expect_error(
    retriever$build_ragnar_index(),
    class = "tempest_retriever_error"
  )
  expect_identical(build_calls, 1L)
  expect_no_match(
    conditionMessage(build_error),
    "sk-live-ragnar-secret",
    fixed = TRUE
  )
  build_print <- paste(capture.output(print(build_error)), collapse = "\n")
  expect_no_match(build_print, "sk-live-ragnar-secret", fixed = TRUE)
})

test_that("retrieve returns results from ragnar store", {
  skip_if_not_installed("ragnar")

  mock_embed <- function(x) matrix(stats::rnorm(length(x) * 3), ncol = 3)
  cfg <- tempest_config(embed_fn = mock_embed)
  retriever <- tempest_retriever(config = cfg)

  # Ingest content
  retriever$ingest_to_ragnar(
    source_id = "S123abc123abc",
    url = "https://example.com/test",
    title = "Test Document About Widgets",
    text = "Widgets are mechanical devices used in manufacturing. They come in various sizes and shapes. The widget industry has grown significantly in recent decades.",
    fetched_at = "2025-01-01T00:00:00Z",
    content_type = "html",
    perspective = NA_character_
  )

  # Build index before retrieval
  retriever$build_ragnar_index()

  # Try retrieval (BM25 doesn't need embeddings to match)
  results <- retriever$retrieve("widgets manufacturing", k = 5, method = "bm25")

  expect_s3_class(results, "data.frame")
})

test_that("retrieve errors without ragnar store", {
  cfg <- tempest_config() # No embed_fn
  retriever <- tempest_retriever(config = cfg)

  expect_null(retriever$ragnar_store)
  expect_error(retriever$retrieve("test query"), "No ragnar store configured")
})
