test_that("tempest_config creates valid config", {
  cfg <- tempest_config()
  expect_s3_class(cfg, "TempestConfig")

  expect_true(!is.null(cfg$models))
  expect_true(!is.null(cfg$models$coordinator))
  expect_true(!is.null(cfg$search_provider))
  expect_equal(cfg$max_search_queries_per_turn, 3L)
  expect_equal(cfg$retrieve_top_k, 25L)
})

test_that("tempest_config validates STORM numeric controls", {
  cfg <- tempest_config(
    max_search_queries_per_turn = 2,
    retrieve_top_k = 9
  )
  expect_equal(cfg$max_search_queries_per_turn, 2L)
  expect_equal(cfg$retrieve_top_k, 9L)

  fallback <- tempest_config(
    max_search_queries_per_turn = 0,
    retrieve_top_k = NA
  )
  expect_equal(fallback$max_search_queries_per_turn, 3L)
  expect_equal(fallback$retrieve_top_k, 25L)
})

test_that("tempest_config accepts upstream-style search providers", {
  providers <- tempest:::tempest_search_provider_choices()
  for (provider in providers) {
    cfg <- tempest_config(search_provider = provider)
    expect_equal(cfg$search_provider, provider)
  }

  expect_equal(
    tempest_config(search_provider = "ddg")$search_provider,
    "duckduckgo"
  )
  expect_equal(
    tempest_config(search_provider = "google_search")$search_provider,
    "google"
  )
  expect_equal(
    tempest_config(search_provider = "azure")$search_provider,
    "azure_ai_search"
  )
  expect_equal(
    tempest_config(search_provider = "you.com")$search_provider,
    "you"
  )

  expect_error(
    tempest_config(search_provider = "not-a-provider"),
    "Unknown search provider"
  )
})

test_that("tempest_config accepts custom models as list", {
  cfg <- tempest_config(
    models = list(
      coordinator = "anthropic/claude-sonnet-4-20250514",
      writer = "anthropic/claude-sonnet-4-20250514",
      expert = "anthropic/claude-sonnet-4-20250514"
    )
  )
  expect_equal(cfg$models$coordinator, "anthropic/claude-sonnet-4-20250514")
})

test_that("tempest_config accepts single model string for all roles", {
  cfg <- tempest_config(models = "anthropic/claude-sonnet-4-20250514")
  expect_equal(cfg$models$coordinator, "anthropic/claude-sonnet-4-20250514")
  expect_equal(cfg$models$writer, "anthropic/claude-sonnet-4-20250514")
  expect_equal(cfg$models$expert, "anthropic/claude-sonnet-4-20250514")
})

test_that("tempest_config accepts custom chat_fn", {
  mock_chat_fn <- function(role, model, system_prompt, echo) {
    list(
      role = role,
      model = model,
      system_prompt = system_prompt,
      echo = echo,
      mock = TRUE
    )
  }

  cfg <- tempest_config(chat_fn = mock_chat_fn)
  chat <- cfg$make_chat("coordinator")

  expect_true(chat$mock)
  expect_equal(chat$role, "coordinator")
  expect_true(grepl("coordinator", chat$model) || grepl("gpt", chat$model))
})

test_that("tempest_config accepts tools as list or function", {
  cfg_list <- tempest_config(tools = list())
  expect_type(cfg_list$tools, "list")

  cfg_fn <- tempest_config(tools = function() list())
  expect_type(cfg_fn$tools, "closure")
})

test_that("make_chat creates ellmer chat object", {
  skip_if_not_installed("ellmer")
  skip_if(
    Sys.getenv("OPENAI_API_KEY") == "" && Sys.getenv("ANTHROPIC_API_KEY") == "",
    "No API key available"
  )

  cfg <- tempest_config()
  chat <- cfg$make_chat("coordinator")
  expect_s3_class(chat, "Chat")
})
