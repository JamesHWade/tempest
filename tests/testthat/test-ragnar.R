test_that("tempest_create_ragnar_store creates store with correct schema", {
  skip_if_not_installed("ragnar")

  # Use in-memory store
  mock_embed <- function(x) matrix(stats::rnorm(length(x) * 3), ncol = 3)
  store <- tempest_create_ragnar_store(mock_embed, cache_dir = NULL)

  # Check it's a ragnar store (S7 class)
  expect_true(!is.null(store))
  expect_true("embed" %in% names(attributes(store)) || is.function(store@embed))
})

test_that("TempestConfig creates ragnar_store from embed_fn", {
  skip_if_not_installed("ragnar")

  mock_embed <- function(x) matrix(stats::rnorm(length(x) * 3), ncol = 3)
  cfg <- tempest_config(embed_fn = mock_embed)

  expect_true(!is.null(cfg$ragnar_store))
  expect_true(!is.null(cfg$embed_fn))
})

test_that("TempestConfig accepts pre-built ragnar_store", {
  skip_if_not_installed("ragnar")

  mock_embed <- function(x) matrix(stats::rnorm(length(x) * 3), ncol = 3)
  pre_store <- tempest_create_ragnar_store(mock_embed, cache_dir = NULL)
  cfg <- tempest_config(ragnar_store = pre_store)

  expect_identical(cfg$ragnar_store, pre_store)
})

test_that("TempestRetriever inherits ragnar_store from config", {
  skip_if_not_installed("ragnar")

  mock_embed <- function(x) matrix(stats::rnorm(length(x) * 3), ncol = 3)
  cfg <- tempest_config(embed_fn = mock_embed)
  retriever <- tempest_retriever(config = cfg)

  expect_true(!is.null(retriever$ragnar_store))
  expect_identical(retriever$ragnar_store, cfg$ragnar_store)
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

  expect_true(n_chunks > 0)
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

  expect_true(is.data.frame(results))
})

test_that("retrieve errors without ragnar store", {
  cfg <- tempest_config()  # No embed_fn
  retriever <- tempest_retriever(config = cfg)

  expect_null(retriever$ragnar_store)
  expect_error(retriever$retrieve("test query"), "No ragnar store configured")
})
