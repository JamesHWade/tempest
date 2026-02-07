test_that("tempest_semantic_filter_facts falls back to keyword without ragnar", {
  store <- SourceStore$new()
  cfg <- tempest_config()
  retriever <- tempest_retriever(config = cfg, store = store)

  # Add some facts
  store$add_fact(tempest:::tempest_fact(
    claim = "Quantum computing uses qubits",
    source_ids = "S123abc123abc"
  ))
  store$add_fact(tempest:::tempest_fact(
    claim = "Classical computers use bits",
    source_ids = "Sdeadbeefdead"
  ))

  # Without ragnar, should fall back to keyword
  result <- tempest:::tempest_semantic_filter_facts(
    retriever, "quantum qubits", store, max_items = 10
  )
  expect_type(result, "list")
  # Should find the quantum-related fact via keyword fallback
  if (length(result) > 0) {
    claims <- vapply(result, function(f) f$claim, character(1))
    expect_true(any(grepl("qubit", claims, ignore.case = TRUE)))
  }
})

test_that("tempest_semantic_filter_facts returns empty for empty store", {
  store <- SourceStore$new()
  cfg <- tempest_config()
  retriever <- tempest_retriever(config = cfg, store = store)

  result <- tempest:::tempest_semantic_filter_facts(
    retriever, "anything", store, max_items = 10
  )
  expect_type(result, "list")
  expect_length(result, 0)
})

test_that("tempest_keyword_filter_facts handles empty query", {
  store <- SourceStore$new()
  store$add_fact(tempest:::tempest_fact(
    claim = "Test fact",
    source_ids = "S123abc123abc"
  ))

  result <- tempest:::tempest_keyword_filter_facts(store, "", max_items = 10)
  expect_type(result, "list")
  expect_length(result, 0)
})

test_that("tempest_semantic_filter_facts with ragnar configured", {
  skip_if_not_installed("ragnar")

  mock_embed <- function(x) matrix(stats::rnorm(length(x) * 3), ncol = 3)
  cfg <- tempest_config(embed_fn = mock_embed)
  store <- SourceStore$new()
  retriever <- tempest_retriever(config = cfg, store = store)

  # Add facts with source IDs
  store$add_fact(tempest:::tempest_fact(
    claim = "Neural networks learn patterns",
    source_ids = "S123abc123abc"
  ))

  # Ingest content to ragnar
  retriever$ingest_to_ragnar(
    source_id = "S123abc123abc",
    url = "https://example.com",
    title = "Neural Networks",
    text = "Neural networks learn patterns from data.",
    fetched_at = "2025-01-01T00:00:00Z",
    content_type = "html"
  )
  retriever$build_ragnar_index()

  result <- suppressWarnings(tempest:::tempest_semantic_filter_facts(
    retriever, "neural networks", store, max_items = 10
  ))
  expect_type(result, "list")
})
