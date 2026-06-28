test_that("tempest_semantic_filter_facts falls back to keyword without ragnar", {
  store <- SourceStore$new()
  cfg <- tempest_config()
  retriever <- tempest_retriever(config = cfg, store = store)

  # Add some claims
  store$add_claim(tempest:::tempest_claim(
    claim_text = "Quantum computing uses qubits",
    source_ids = "S123abc123abc"
  ))
  store$add_claim(tempest:::tempest_claim(
    claim_text = "Classical computers use bits",
    source_ids = "Sdeadbeefdead"
  ))

  # Without ragnar, should fall back to keyword
  result <- tempest:::tempest_semantic_filter_facts(
    retriever,
    "quantum qubits",
    store,
    max_items = 10
  )
  expect_type(result, "list")
  # The keyword fallback must actually find the quantum-related fact.
  expect_gt(length(result), 0)
  claims <- vapply(result, function(f) f@claim_text, character(1))
  expect_match(claims, "qubit", ignore.case = TRUE, all = FALSE)
})

test_that("tempest_semantic_filter_facts returns empty for empty store", {
  store <- SourceStore$new()
  cfg <- tempest_config()
  retriever <- tempest_retriever(config = cfg, store = store)

  result <- tempest:::tempest_semantic_filter_facts(
    retriever,
    "anything",
    store,
    max_items = 10
  )
  expect_type(result, "list")
  expect_length(result, 0)
})

test_that("tempest_keyword_filter_facts handles empty query", {
  store <- SourceStore$new()
  store$add_claim(tempest:::tempest_claim(
    claim_text = "Test fact",
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

  # Add claims with source IDs
  store$add_claim(tempest:::tempest_claim(
    claim_text = "Neural networks learn patterns",
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
    retriever,
    "neural networks",
    store,
    max_items = 10
  ))
  expect_type(result, "list")
})
