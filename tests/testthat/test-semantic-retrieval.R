test_that("tempest_semantic_filter_facts falls back to keyword without ragnar", {
  store <- fake_store_with_sources(2)
  source_ids <- vapply(store$list_retrieved_sources(), `[[`, character(1), "id")
  cfg <- tempest_config()
  retriever <- tempest_retriever(config = cfg, workspace = store)

  # Add some claims
  store$add_proposed_claim(tempest:::tempest_claim(
    claim_text = "Quantum computing uses qubits",
    source_ids = source_ids[[1]],
    verification_status = "supported",
    support_score = 0.9
  ))
  store$add_proposed_claim(tempest:::tempest_claim(
    claim_text = "Classical computers use bits",
    source_ids = source_ids[[2]],
    verification_status = "supported",
    support_score = 0.9
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
  store <- test_research_workspace()
  cfg <- tempest_config()
  retriever <- tempest_retriever(config = cfg, workspace = store)

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
  store <- fake_store_with_sources(1)
  source_id <- store$list_retrieved_sources()[[1]]$id
  store$add_proposed_claim(tempest:::tempest_claim(
    claim_text = "Test fact",
    source_ids = source_id
  ))

  result <- tempest:::tempest_keyword_filter_facts(store, "", max_items = 10)
  expect_type(result, "list")
  expect_length(result, 0)
})

test_that("tempest_semantic_filter_facts with ragnar configured", {
  skip_if_not_installed("ragnar")

  mock_embed <- function(x) matrix(stats::rnorm(length(x) * 3), ncol = 3)
  cfg <- tempest_config(embed_fn = mock_embed)
  store <- test_research_workspace()
  source <- fake_source("https://example.com")
  store$upsert_retrieved_resource(source)
  retriever <- tempest_retriever(config = cfg, workspace = store)

  # Add claims with source IDs
  store$add_proposed_claim(tempest:::tempest_claim(
    claim_text = "Neural networks learn patterns",
    source_ids = source$id
  ))

  # Ingest content to ragnar
  retriever$ingest_to_ragnar(
    source_id = source$id,
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
