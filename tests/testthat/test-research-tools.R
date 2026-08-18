test_that("research model selection is detached from generic runtime", {
  reject_generic <- function(...) {
    rlang::abort("generic helper reached", class = "test_generic_reached")
  }
  local_mocked_bindings(
    tempest_contract_id = reject_generic,
    tempest_runtime_abort = reject_generic,
    tempest_runtime = reject_generic
  )
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
    tempest_tools_web = function(retriever, model, search_provider) {
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
