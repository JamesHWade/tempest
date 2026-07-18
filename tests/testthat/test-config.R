test_that("tempest_config creates valid config", {
  cfg <- tempest_config()
  expect_identical(S7::S7_inherits(cfg, TempestConfig), TRUE)

  expect_mapequal(
    cfg@models,
    list(
      coordinator = "openai/gpt-5.4",
      writer = "openai/gpt-5.4",
      expert = "openai/gpt-5.4-mini",
      mindmap = "openai/gpt-5.4-mini",
      judge = "openai/gpt-5.4-mini"
    )
  )
  expect_type(cfg@search_provider, "character")
  expect_equal(cfg@cache_enabled, TRUE)
  expect_equal(cfg@cache_ttl, Inf)
  expect_equal(cfg@max_search_queries_per_turn, 3L)
  expect_equal(cfg@retrieve_top_k, 25L)
})

test_that("tempest_config accepts cache controls", {
  cfg <- tempest_config(
    cache_dir = withr::local_tempdir(),
    cache_enabled = FALSE,
    cache_ttl = 60
  )

  expect_equal(cfg@cache_enabled, FALSE)
  expect_equal(cfg@cache_ttl, 60)

  expect_error(tempest_config(cache_ttl = -1), class = "rlang_error")
})

test_that("tempest_config validates STORM numeric controls", {
  cfg <- tempest_config(
    max_search_queries_per_turn = 2,
    retrieve_top_k = 9
  )
  expect_equal(cfg@max_search_queries_per_turn, 2L)
  expect_equal(cfg@retrieve_top_k, 9L)

  invalid <- list(
    list(max_search_results = 0),
    list(max_search_results = -1),
    list(max_search_results = NA_real_),
    list(max_search_results = Inf),
    list(max_search_results = c(1, 2)),
    list(max_search_queries_per_turn = 0),
    list(retrieve_top_k = NA_real_),
    list(max_sources = c(1, 2)),
    list(max_active_experts = NA_integer_)
  )
  for (args in invalid) {
    expect_error(
      do.call(tempest_config, args),
      class = "tempest_config_error"
    )
  }
})

test_that("tempest_config validates logical and model controls", {
  expect_error(
    tempest_config(cache_enabled = NA),
    class = "tempest_config_error"
  )
  expect_error(
    tempest_config(enable_discourse_manager = c(TRUE, FALSE)),
    class = "tempest_config_error"
  )
  expect_error(
    tempest_config(models = list(coordinator = NA_character_)),
    class = "tempest_config_error"
  )
})

test_that("tempest_config accepts upstream-style search providers", {
  providers <- tempest:::tempest_search_provider_choices()
  for (provider in providers) {
    cfg <- tempest_config(search_provider = provider)
    expect_equal(cfg@search_provider, provider)
  }

  expect_equal(
    tempest_config(search_provider = "ddg")@search_provider,
    "duckduckgo"
  )
  expect_equal(
    tempest_config(search_provider = "google_search")@search_provider,
    "google"
  )
  expect_equal(
    tempest_config(search_provider = "azure")@search_provider,
    "azure_ai_search"
  )
  expect_equal(
    tempest_config(search_provider = "you.com")@search_provider,
    "you"
  )

  expect_error(
    tempest_config(search_provider = "not-a-provider"),
    class = "tempest_search_provider_error"
  )
})

test_that("tempest_config reports classed provider errors", {
  expect_snapshot(
    tempest_config(search_provider = "not-a-provider"),
    error = TRUE
  )
  expect_error(
    tempest_config(search_provider = "not-a-provider"),
    class = "tempest_config_error"
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
  expect_equal(cfg@models$coordinator, "anthropic/claude-sonnet-4-20250514")
})

test_that("tempest_config accepts single model string for all roles", {
  cfg <- tempest_config(models = "anthropic/claude-sonnet-4-20250514")
  expect_equal(cfg@models$coordinator, "anthropic/claude-sonnet-4-20250514")
  expect_equal(cfg@models$writer, "anthropic/claude-sonnet-4-20250514")
  expect_equal(cfg@models$expert, "anthropic/claude-sonnet-4-20250514")
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
  chat <- tempest_make_chat(cfg, "coordinator")

  expect_identical(chat$mock, TRUE)
  expect_equal(chat$role, "coordinator")
  expect_match(chat$model, "coordinator|gpt")
})

test_that("tempest_make_chat wraps custom chat factory failures", {
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) {
      stop("factory unavailable")
    }
  )

  err <- expect_error(
    tempest_make_chat(cfg, "coordinator"),
    class = "tempest_chat_error"
  )
  expect_match(conditionMessage(err$parent), "factory unavailable")
})

test_that("tempest_config does not accept globally registered tools", {
  expect_error(
    tempest_config(tools = list()),
    regexp = "unused argument"
  )
})

test_that("make_chat creates ellmer chat object", {
  skip_if_not_installed("ellmer")
  skip_if(
    Sys.getenv("OPENAI_API_KEY") == "" && Sys.getenv("ANTHROPIC_API_KEY") == "",
    "No API key available"
  )

  cfg <- tempest_config()
  chat <- tempest_make_chat(cfg, "coordinator")
  expect_s3_class(chat, "Chat")
})

test_that("missing optional packages are classed", {
  expect_error(
    tempest:::tempest_require("tempestDefinitelyMissingPackage"),
    class = "tempest_missing_package_error"
  )
})
