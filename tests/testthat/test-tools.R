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

  claims <- by_name("list_claims")()
  expect_length(claims, 1)
  expect_equal(claims[[1]]$claim_id, added$claim_id)

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
