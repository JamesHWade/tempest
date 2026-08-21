test_that("retrieval tools expose canonical research vocabulary", {
  skip_if_not_installed("ellmer")
  store <- tempest_research_workspace()
  store$upsert_retrieved_resource(test_typed_web_resource(
    "https://example.org/1"
  ))
  source_id <- store$list_retrieved_sources()[[1]]$id
  retriever <- tempest_retriever(
    config = tempest_config(cache_dir = withr::local_tempdir()),
    workspace = store
  )
  tools <- tempest:::tempest_tools_retrieval(retriever)
  tool_names <- vapply(tools, function(tool) tool@name, character(1))
  by_name <- function(name) tools[[match(name, tool_names)]]

  expect_contains(
    tool_names,
    c(
      "web_search",
      "fetch_url",
      "get_retrieved_source",
      "list_retrieved_sources",
      "list_proposed_claims",
      "add_proposed_claim",
      "get_proposed_claim",
      "get_evidence_for_proposed_claim",
      "list_unsupported_proposed_claims"
    )
  )

  added <- by_name("add_proposed_claim")(
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

  claims <- by_name("list_proposed_claims")()
  expect_length(claims, 1)
  expect_equal(claims[[1]]$claim_id, added$claim_id)

  claim <- by_name("get_proposed_claim")(added$claim_id)
  expect_equal(claim$claim_id, added$claim_id)
  expect_equal(claim$retrieved_resources[[1]]$source_id, source_id)
  expect_equal(by_name("get_proposed_claim")("Cmissing"), NULL)

  span <- tempest:::tempest_evidence_span(
    source_id = source_id,
    quote = "Photosynthesis",
    relevance_score = 0.8
  )
  span_id <- store$add_evidence_span(span)
  store$link_evidence_to_proposed_claim(added$claim_id, span_id)
  evidence <- by_name("get_evidence_for_proposed_claim")(added$claim_id)
  expect_equal(evidence$claim$claim_id, added$claim_id)
  expect_equal(evidence$evidence_spans[[1]]$quote, "Photosynthesis")
  expect_equal(evidence$cited_sources[[1]]$source_id, source_id)
  expect_equal(by_name("get_evidence_for_proposed_claim")("Cmissing"), NULL)

  expect_length(by_name("list_unsupported_proposed_claims")(), 0)
  store$verify_proposed_claims_batch(
    list(tempest_claim_support(
      claim_id = added$claim_id,
      evidence_span_id = span_id,
      source_id = source_id,
      verification_status = "unsupported",
      support_score = 0.2,
      rationale = "The exact evidence span does not support the claim."
    )),
    verified_at = "2026-08-16T12:03:00Z"
  )
  unsupported <- by_name("list_unsupported_proposed_claims")()
  expect_length(unsupported, 1)
  expect_equal(unsupported[[1]]$claim_id, added$claim_id)

  expect_equal(
    intersect(
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
    ),
    character()
  )
})

test_that("web tools produce only their delegated retriever spans", {
  skip_if_not_installed("ellmer")
  local_otel_opt_in()
  state <- local_fake_otel()
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
        title = "Tool result",
        url = "https://example.com/tool-result",
        snippet = "Tool snippet"
      )
    },
    tempest_fetch_url_text = function(url, user_agent = NULL) {
      list(
        kind = "html",
        text = "Tool body",
        title = "Tool source",
        error = NULL
      )
    }
  )
  tools <- tempest:::tempest_tools_web(
    retriever,
    search_provider = "wikipedia"
  )
  tool_names <- vapply(tools, \(tool) tool@name, character(1))
  by_name <- function(name) tools[[match(name, tool_names)]]

  search <- by_name("web_search")("private tool query", k = 1L)
  fetched <- by_name("fetch_url")("https://example.com/private-tool-url")

  expect_identical(nrow(search), 1L)
  expect_identical(fetched$title, "Tool source")
  expect_identical(
    vapply(state$spans, \(span) span$name, character(1)),
    c("tempest.retrieval.search", "tempest.retrieval.fetch")
  )
  expect_identical(
    vapply(state$spans, \(span) span$end_count, integer(1)),
    c(1L, 1L)
  )
})

test_that("native harvesting inserts and merges typed evidence resources", {
  workspace <- tempest_research_workspace()
  url <- "https://example.org/native-typed-evidence"
  source_id <- tempest:::tempest_source_id(url)
  local_mocked_bindings(
    tempest_now_utc = function() "2026-08-20T12:00:00.000000Z"
  )

  first_ids <- tempest:::tempest_harvest_native_sources_from_json(
    list(
      text = "Provider answer context",
      annotations = list(list(
        url = url,
        title = "Native evidence",
        snippet = "Native evidence snippet",
        content = "Native evidence body"
      ))
    ),
    workspace
  )
  first <- workspace$get_retrieved_resource(source_id)

  expect_identical(first_ids, source_id)
  expect_identical(S7::S7_inherits(first, tempest:::TempestResource), TRUE)
  expect_identical(first@resource_kind, "web")
  expect_identical(first@locator, url)
  expect_identical(first@title, "Native evidence")
  expect_identical(first@media_type, "text/html")
  expect_identical(first@resource_id, source_id)
  expect_identical(first@content, "Provider answer context")
  expect_identical(first@metadata$kind, "native_search")
  expect_identical(first@metadata$provider_tool, "native")
  expect_identical(first@metadata$snippet, "Native evidence snippet")
  expect_identical(first@metadata$content_text, "Native evidence body")
  expect_identical(first@metadata$context_text, "Provider answer context")

  second_ids <- tempest:::tempest_harvest_native_sources_from_json(
    list(url = url),
    workspace
  )
  merged <- workspace$get_retrieved_resource(source_id)
  presented <- workspace$get_retrieved_source(source_id)

  expect_identical(second_ids, source_id)
  expect_identical(merged@title, first@title)
  expect_identical(merged@content, first@content)
  expect_identical(merged@metadata, first@metadata)
  expect_identical(presented$id, source_id)
  expect_identical(presented$content_text, "Provider answer context")
  expect_identical(presented$snippet, "Native evidence snippet")
})

test_that("native evidence merging rejects TempestResource subclasses", {
  resource <- test_typed_web_resource("https://example.org/merge")
  subclass <- test_subclassed_resource(resource)

  expect_error(
    tempest:::tempest_merge_resource_record(resource, subclass),
    class = "tempest_research_workspace_integrity_error"
  )
  expect_error(
    tempest:::tempest_merge_resource_record(subclass, resource),
    class = "tempest_research_workspace_integrity_error"
  )
})

test_that("native merging preserves the strongest explicit evidence text", {
  url <- "https://example.org/merge-evidence-precedence"
  old_body <- tempest_resource(
    resource_kind = "web",
    locator = url,
    title = "Old full body",
    media_type = "text/html",
    content = "Old full evidence body",
    metadata = list(content_text = "Old full evidence body")
  )
  new_snippet <- tempest_resource(
    resource_kind = "web",
    locator = url,
    title = "New snippet",
    media_type = "text/html",
    content = "New short snippet",
    metadata = list(snippet = "New short snippet")
  )

  merged <- tempest:::tempest_merge_resource_record(old_body, new_snippet)
  expect_identical(merged@content, "Old full evidence body")
  expect_identical(
    merged@content_hash,
    tempest:::tempest_product_content_hash(
      "Old full evidence body",
      "text/html"
    )
  )

  old_context <- tempest_resource(
    resource_kind = "web",
    locator = url,
    title = "Old context",
    media_type = "text/html",
    content = "Old provider context",
    metadata = list(context_text = "Old provider context")
  )
  new_body <- tempest_resource(
    resource_kind = "web",
    locator = url,
    title = "New body",
    media_type = "text/html",
    content = "New full evidence body",
    metadata = list(content_text = "New full evidence body")
  )

  merged <- tempest:::tempest_merge_resource_record(old_context, new_body)
  expect_identical(merged@content, "Old provider context")
  expect_identical(
    merged@content_hash,
    tempest:::tempest_product_content_hash(
      "Old provider context",
      "text/html"
    )
  )
})

test_that("review tools keep nonscalar metadata out of source payloads", {
  workspace <- tempest_research_workspace()
  resource <- tempest_resource(
    resource_kind = "web",
    locator = "https://example.org/nonscalar-tool-metadata",
    title = "Nonscalar tool metadata",
    media_type = "text/html",
    content = "Captured tool evidence",
    metadata = list(
      snippet = list("list snippet"),
      context_text = c("first context", "second context")
    )
  )
  workspace$upsert_retrieved_resource(resource)
  restored <- tempest:::tempest_research_workspace_restore(
    tempest:::tempest_research_workspace_snapshot(workspace)
  )
  tools <- tempest:::tempest_tool_review_functions(restored)

  summaries <- tools$list_retrieved_sources()
  detail <- tools$get_retrieved_source(resource@resource_id)
  expect_identical(summaries[[1]]$snippet, NA_character_)
  expect_identical(detail$excerpt, "Captured tool evidence")
})

test_that("source-management tools expose claim tools without web tools", {
  skip_if_not_installed("ellmer")
  store <- producer_test_workspace(1)
  source_id <- store$list_retrieved_sources()[[1]]$id
  retriever <- tempest_retriever(
    config = tempest_config(cache_dir = withr::local_tempdir()),
    workspace = store
  )
  tools <- tempest:::tempest_tools_source_management(retriever)
  tool_names <- vapply(tools, function(tool) tool@name, character(1))
  by_name <- function(name) tools[[match(name, tool_names)]]

  expect_contains(
    tool_names,
    c(
      "get_retrieved_source",
      "list_retrieved_sources",
      "list_proposed_claims",
      "add_proposed_claim",
      "get_proposed_claim",
      "get_evidence_for_proposed_claim",
      "list_unsupported_proposed_claims"
    )
  )
  expect_equal(intersect(tool_names, c("web_search", "fetch_url")), character())

  added <- by_name("add_proposed_claim")(
    claim_text = "Native source management stores claims.",
    source_ids = source_id
  )
  expect_equal(added$claim_text, "Native source management stores claims.")
  expect_equal(added$source_ids, source_id)

  claims <- by_name("list_proposed_claims")()
  expect_length(claims, 1)
  expect_equal(claims[[1]]$claim_id, added$claim_id)
})

test_that("claim write tools record dynamic provenance", {
  skip_if_not_installed("ellmer")
  store <- producer_test_workspace(1)
  source_id <- store$list_retrieved_sources()[[1]]$id
  retriever <- tempest_retriever(
    config = tempest_config(cache_dir = withr::local_tempdir()),
    workspace = store
  )
  expert <- test_expert(name = "Climate Expert")
  current <- list(
    session_id = "expert-session-1",
    expert_id = expert@expert_id,
    retrieval_step_id = "tool-turn-1"
  )
  tools <- tempest:::tempest_tools_source_management(
    retriever,
    claim_provenance = function() current
  )
  tool_names <- vapply(tools, function(tool) tool@name, character(1))
  by_name <- function(name) tools[[match(name, tool_names)]]

  added <- by_name("add_proposed_claim")(
    claim_text = "Dynamic provenance is recorded.",
    source_ids = source_id
  )

  expect_equal(added$session_id, "expert-session-1")
  expect_equal(added$expert_id, expert@expert_id)
  expect_equal(added$retrieval_step_id, "tool-turn-1")
  claim <- store$list_proposed_claims()[[1]]
  expect_equal(claim@session_id, "expert-session-1")
  expect_equal(claim@expert_id, expert@expert_id)
  expect_equal(claim@retrieval_step_id, "tool-turn-1")
})

test_that("source-management tools can be registered read-only", {
  skip_if_not_installed("ellmer")
  retriever <- tempest_retriever(
    config = tempest_config(cache_dir = withr::local_tempdir()),
    workspace = producer_test_workspace(1)
  )
  tools <- tempest:::tempest_tools_source_management(
    retriever,
    allow_claim_writes = FALSE
  )
  tool_names <- vapply(tools, function(tool) tool@name, character(1))

  expect_contains(
    tool_names,
    c(
      "get_retrieved_source",
      "list_retrieved_sources",
      "list_proposed_claims",
      "get_proposed_claim",
      "get_evidence_for_proposed_claim",
      "list_unsupported_proposed_claims"
    )
  )
  expect_equal(
    intersect(tool_names, c("add_proposed_claim", "add_fact")),
    character()
  )
})

test_that("default tool registration respects read-only evidence roles", {
  skip_if_not_installed("ellmer")
  store <- producer_test_workspace(1)
  retriever <- tempest_retriever(
    config = tempest_config(cache_dir = withr::local_tempdir()),
    workspace = store
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
    c(
      "web_search",
      "fetch_url",
      "get_proposed_claim",
      "list_unsupported_proposed_claims"
    )
  )
  expect_equal(
    intersect(tool_names, c("add_proposed_claim", "add_fact")),
    character()
  )
})
test_that("single expert generation returns deterministic scoped profiles", {
  skip_if_not_installed("ellmer")
  generated <- list(
    personas = list(list(
      name = "Dr. Rivera",
      title = "Battery policy analyst",
      affiliation = "Independent",
      background = "Studies battery policy.",
      focus_areas = list("recycling", "incentives"),
      perspective = "Policy and market incentives",
      initial_questions = list("Which incentives affect recycling?")
    ))
  )
  chat <- fake_chat(structured = list(generated, generated))
  config <- tempest_config(
    models = list(
      coordinator = "test/coordinator",
      expert = "test/expert"
    ),
    chat_fn = function(role, model, system_prompt, echo) chat
  )

  first <- tempest:::tempest_generate_single_expert(
    "Battery circularity",
    "Policy analysis",
    list(),
    config,
    module = test_program_executions(
      config,
      "tools-single-expert"
    )$personas
  )
  second <- tempest:::tempest_generate_single_expert(
    "Battery circularity",
    "Policy analysis",
    list(),
    config,
    module = test_program_executions(
      config,
      "tools-single-expert"
    )$personas
  )

  expect_identical(
    S7::S7_inherits(first, tempest:::TempestExpertProfile),
    TRUE
  )
  expect_identical(first, second)
  expect_match(first@expert_id, "^expert::[a-f0-9]{64}$")
  expect_match(first@version, "^sha256-[a-f0-9]{64}$")
  expect_identical(first@name, "Dr. Rivera")
  expect_identical(first@title, "Battery policy analyst")
  expect_match(first@description, "Affiliation: Independent", fixed = TRUE)
  expect_match(
    first@description,
    "Background: Studies battery policy.",
    fixed = TRUE
  )
  expect_match(
    first@description,
    "Perspective: Policy and market incentives",
    fixed = TRUE
  )
  expect_match(first@instructions, "Policy and market incentives", fixed = TRUE)
  expect_identical(first@focus_areas, c("recycling", "incentives"))
  expect_identical(
    first@initial_questions,
    "Which incentives affect recycling?"
  )
})
