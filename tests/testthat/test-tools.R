test_that("retrieval tools expose claim tools and fact aliases", {
  skip_if_not_installed("ellmer")
  store <- fake_store_with_sources(1)
  source_id <- store$list_sources()[[1]]$id
  retriever <- tempest_retriever(
    config = tempest_config(cache_dir = withr::local_tempdir()),
    store = store
  )
  tools <- tempest:::tempest_tools_retrieval(retriever)
  tool_names <- vapply(tools, function(tool) tool@name, character(1))
  by_name <- function(name) tools[[match(name, tool_names)]]

  expect_contains(
    tool_names,
    c(
      "web_search",
      "fetch_url",
      "list_claims",
      "add_claim",
      "get_claim",
      "get_evidence_for_claim",
      "list_unsupported_claims",
      "list_facts",
      "add_fact"
    )
  )

  added <- by_name("add_claim")(
    claim_text = "Claim tools store source-backed claims.",
    source_ids = source_id,
    confidence = "high"
  )
  expect_contains(
    names(added),
    c(
      "claim_id",
      "claim_text",
      "source_ids",
      "confidence",
      "verification_status",
      "created_at"
    )
  )
  expect_equal(added$claim_text, "Claim tools store source-backed claims.")
  expect_equal(added$source_ids, source_id)
  expect_equal(added$confidence, "high")
  expect_equal(added$verification_status, "unverified")
  expect_equal(added$support_score, NA_real_)

  claims <- by_name("list_claims")()
  expect_length(claims, 1)
  expect_equal(claims[[1]]$claim_id, added$claim_id)

  claim <- by_name("get_claim")(added$claim_id)
  expect_equal(claim$claim_id, added$claim_id)
  expect_equal(claim$sources[[1]]$source_id, source_id)
  expect_equal(by_name("get_claim")("Cmissing"), NULL)

  span <- tempest:::tempest_evidence_span(
    source_id = source_id,
    quote = "supporting excerpt",
    relevance_score = 0.8
  )
  span_id <- store$add_evidence_span(span)
  store$link_evidence(added$claim_id, span_id)
  evidence <- by_name("get_evidence_for_claim")(added$claim_id)
  expect_equal(evidence$claim$claim_id, added$claim_id)
  expect_equal(evidence$evidence_spans[[1]]$quote, "supporting excerpt")
  expect_equal(evidence$cited_sources[[1]]$source_id, source_id)
  expect_equal(by_name("get_evidence_for_claim")("Cmissing"), NULL)

  expect_length(by_name("list_unsupported_claims")(), 0)
  store$verify_claim(added$claim_id, status = "unsupported", score = 0.2)
  unsupported <- by_name("list_unsupported_claims")()
  expect_length(unsupported, 1)
  expect_equal(unsupported[[1]]$claim_id, added$claim_id)

  legacy <- by_name("add_fact")(
    claim = "Fact aliases still write claim records.",
    source_ids = source_id
  )
  expect_equal(legacy$fact_id, legacy$claim_id)
  expect_equal(legacy$claim, "Fact aliases still write claim records.")

  facts <- by_name("list_facts")()
  expect_length(facts, 2)
  expect_contains(
    names(facts[[1]]),
    c("fact_id", "claim", "claim_id", "claim_text", "verification_status")
  )
})

test_that("source-management tools expose claim tools without web tools", {
  skip_if_not_installed("ellmer")
  store <- fake_store_with_sources(1)
  source_id <- store$list_sources()[[1]]$id
  retriever <- tempest_retriever(
    config = tempest_config(cache_dir = withr::local_tempdir()),
    store = store
  )
  tools <- tempest:::tempest_tools_source_management(retriever)
  tool_names <- vapply(tools, function(tool) tool@name, character(1))
  by_name <- function(name) tools[[match(name, tool_names)]]

  expect_contains(
    tool_names,
    c(
      "get_source",
      "list_sources",
      "list_claims",
      "add_claim",
      "get_claim",
      "get_evidence_for_claim",
      "list_unsupported_claims",
      "list_facts",
      "add_fact"
    )
  )
  expect_equal(intersect(tool_names, c("web_search", "fetch_url")), character())

  added <- by_name("add_claim")(
    claim_text = "Native source management stores claims.",
    source_ids = source_id
  )
  expect_equal(added$claim_text, "Native source management stores claims.")
  expect_equal(added$source_ids, source_id)

  claims <- by_name("list_claims")()
  expect_length(claims, 1)
  expect_equal(claims[[1]]$claim_id, added$claim_id)
})

test_that("source-management tools can be registered read-only", {
  skip_if_not_installed("ellmer")
  retriever <- tempest_retriever(
    config = tempest_config(cache_dir = withr::local_tempdir()),
    store = fake_store_with_sources(1)
  )
  tools <- tempest:::tempest_tools_source_management(
    retriever,
    allow_claim_writes = FALSE
  )
  tool_names <- vapply(tools, function(tool) tool@name, character(1))

  expect_contains(
    tool_names,
    c(
      "get_source",
      "list_sources",
      "list_claims",
      "get_claim",
      "get_evidence_for_claim",
      "list_unsupported_claims"
    )
  )
  expect_equal(intersect(tool_names, c("add_claim", "add_fact")), character())
})

test_that("default tool registration respects read-only evidence roles", {
  skip_if_not_installed("ellmer")
  store <- fake_store_with_sources(1)
  retriever <- tempest_retriever(
    config = tempest_config(cache_dir = withr::local_tempdir()),
    store = store
  )
  registered <- list()
  chat <- list(
    register_tools = function(tools) {
      registered[[length(registered) + 1L]] <<- tools
      invisible(NULL)
    }
  )

  tempest:::tempest_register_default_tools(
    chat,
    retriever,
    search_provider = "wikipedia",
    allow_claim_writes = FALSE
  )

  tool_names <- vapply(registered[[1]], function(tool) tool@name, character(1))
  expect_contains(
    tool_names,
    c("web_search", "fetch_url", "get_claim", "list_unsupported_claims")
  )
  expect_equal(intersect(tool_names, c("add_claim", "add_fact")), character())
})
