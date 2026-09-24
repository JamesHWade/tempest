test_that("research model selection uses fixed product roles", {
  config <- tempest_config(
    models = list(
      coordinator = "test/coordinator",
      expert = "test/expert",
      writer = "test/writer",
      mindmap = "test/mindmap",
      judge = "test/judge"
    )
  )

  expect_identical(
    tempest:::tempest_research_model(config, "expert"),
    "test/expert"
  )
})

test_that("fixed research tools preserve every product search provider", {
  providers <- c(
    "native",
    "wikipedia",
    "you",
    "bing",
    "serper",
    "brave",
    "duckduckgo",
    "tavily",
    "searxng",
    "google",
    "azure_ai_search"
  )
  observed <- character()
  retriever <- structure(list(), class = "TempestRetriever")
  local_mocked_bindings(
    tempest_tools_web = function(
      retriever,
      model,
      search_provider,
      max_search_results
    ) {
      observed <<- c(observed, search_provider)
      list(web = "web")
    },
    tempest_tools_evidence_read = function(retriever) {
      list(read = "read")
    },
    tempest_tools_evidence_write = function(retriever, claim_provenance) {
      list(write = "write")
    }
  )

  for (provider in providers) {
    tools <- tempest:::tempest_research_tools(
      retriever,
      role = "expert",
      model = "test/expert",
      search_provider = provider
    )
    expect_named(tools, c("web", "read", "write"), info = provider)
  }

  expect_identical(observed, providers)
})

test_that("host retrievers attach research tools with the run's search budget", {
  workspace <- tempest_research_workspace()
  calls <- list()
  retriever <- list(
    workspace = workspace,
    search = function(query, k) {
      calls <<- append(calls, list(list(query = query, k = k)))
      data.frame(
        title = "Host result",
        url = "https://example.org/host-result"
      )
    },
    fetch = function(url) {
      resource <- tempest::tempest_resource(
        resource_kind = "web",
        locator = url,
        title = "Host page",
        media_type = "text/html",
        content = "Host evidence"
      )
      workspace$upsert_retrieved_resource(resource)
      resource
    }
  )
  chat <- fake_chat()

  tempest:::tempest_research_attach_tools(
    chat,
    retriever,
    role = "coordinator",
    model = "openai/gpt-5.6-sol",
    search_provider = "native",
    max_search_results = 2L
  )

  tools <- chat$get_tools()
  expect_named(
    tools,
    c(
      "web_search",
      "fetch_url",
      "list_retrieved_sources",
      "get_retrieved_source",
      "list_proposed_claims",
      "get_proposed_claim",
      "get_evidence_for_proposed_claim",
      "list_unsupported_proposed_claims"
    ),
    ignore.order = TRUE
  )
  search_results <- tools$web_search("host query", k = 2L)
  expect_identical(calls, list(list(query = "host query", k = 2L)))
  expect_identical(
    search_results$source_id,
    tempest_source_id("https://example.org/host-result")
  )
  expect_identical(search_results$snippet, NA_character_)
  url <- "https://example.org/host-tool"
  fetched <- tools$fetch_url(url)
  source_id <- tempest::tempest_source_id(url)
  expect_identical(fetched$source_id, source_id)
  expect_identical(fetched$excerpt, "Host evidence")
  expect_s7_class(
    workspace$get_retrieved_resource(source_id),
    tempest:::TempestResource
  )
  expect_error(
    tools$web_search("host query", k = 3L),
    class = "tempest_config_error"
  )
})

test_that("host search results reject malformed rows before research uses them", {
  for (results in list(
    list(list(title = "List result", url = "https://example.org/list")),
    data.frame(title = "Missing URL"),
    data.frame(title = "Bad URL", url = "not a URL"),
    data.frame(
      title = "Wrong ID",
      url = "https://example.org/id",
      source_id = "S000000000000"
    )
  )) {
    retriever <- list(search = function(query, k) results)
    expect_error(
      tempest:::tempest_retriever_search(retriever, "host query", 2L),
      class = "tempest_retriever_search_error"
    )
  }
})

test_that("research tool roles have fixed attachment sets", {
  retriever <- structure(list(), class = "TempestRetriever")
  local_mocked_bindings(
    tempest_tools_web = function(...) list(web = "web"),
    tempest_tools_evidence_read = function(...) list(read = "read"),
    tempest_tools_evidence_write = function(...) list(write = "write")
  )
  tool_names <- function(role) {
    names(tempest:::tempest_research_tools(retriever, role))
  }

  expect_identical(tool_names("coordinator"), c("web", "read"))
  expect_identical(tool_names("expert"), c("web", "read", "write"))
  expect_identical(tool_names("writer"), "read")
  expect_identical(tool_names("mindmap"), "read")
  expect_identical(tool_names("judge"), "read")
})
