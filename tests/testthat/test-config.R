test_that("tempest_config creates valid config", {
  cfg <- tempest_config()
  expect_identical(S7::S7_inherits(cfg, TempestConfig), TRUE)

  expect_mapequal(
    cfg@models,
    list(
      coordinator = "openai/gpt-5.6-sol",
      writer = "openai/gpt-5.6-sol",
      expert = "openai/gpt-5.6-luna",
      mindmap = "openai/gpt-5.6-luna",
      judge = "openai/gpt-5.6-luna"
    )
  )
  expect_type(cfg@search_provider, "character")
  expect_equal(cfg@cache_enabled, TRUE)
  expect_equal(cfg@cache_ttl, Inf)
  expect_equal(cfg@max_search_queries_per_turn, 3L)
  expect_equal(cfg@retrieve_top_k, 25L)
  expect_null(cfg@chat)
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

test_that("tempest_config uses a provider/model from tempest.chat", {
  withr::local_options(
    tempest.chat = "anthropic/claude-sonnet-4-20250514"
  )

  cfg <- tempest_config()

  expect_equal(
    unname(unlist(cfg@models)),
    rep("anthropic/claude-sonnet-4-20250514", 5L)
  )
  expect_null(cfg@chat)
})

test_that("tempest_config captures and clones a Chat from tempest.chat", {
  ChatState <- R6::R6Class(
    "ChatState",
    public = list(
      values = NULL,
      initialize = function() {
        self$values <- character()
      }
    )
  )
  MockChat <- R6::R6Class(
    "Chat",
    public = list(
      model = NULL,
      system_prompt = NULL,
      state = NULL,
      initialize = function(model, system_prompt) {
        self$model <- model
        self$system_prompt <- system_prompt
        self$state <- ChatState$new()
      },
      get_model = function() self$model,
      get_system_prompt = function() self$system_prompt,
      set_system_prompt = function(value) {
        stopifnot(is.character(value), length(value) == 1L, !is.na(value))
        self$system_prompt <- value
        invisible(self)
      }
    )
  )
  default_chat <- MockChat$new("claude-sonnet-4", "Use terse prose.")
  withr::local_options(tempest.chat = default_chat)

  cfg <- tempest_config()
  first <- tempest_make_chat(cfg, "coordinator", system_prompt = "Coordinate.")
  second <- tempest_make_chat(cfg, "writer", system_prompt = "Write.")

  expect_equal(cfg@models$coordinator, "claude-sonnet-4")
  expect_identical(cfg@chat, default_chat)
  expect_identical(identical(first, default_chat), FALSE)
  expect_identical(identical(first, second), FALSE)
  expect_equal(
    first$get_system_prompt(),
    "Coordinate.\n\n---\n\nUse terse prose."
  )
  expect_equal(second$get_system_prompt(), "Write.\n\n---\n\nUse terse prose.")
  expect_equal(default_chat$get_system_prompt(), "Use terse prose.")
  expect_identical(identical(first$state, second$state), FALSE)
  first$state$values <- "first"
  expect_length(second$state$values, 0L)
  expect_length(default_chat$state$values, 0L)
})

test_that("explicit chat configuration overrides tempest.chat", {
  withr::local_options(tempest.chat = 1)

  model_cfg <- tempest_config(models = "openai/gpt-5.6-luna")
  factory_cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )

  expect_equal(model_cfg@models$coordinator, "openai/gpt-5.6-luna")
  expect_null(model_cfg@chat)
  expect_null(factory_cfg@chat)
  expect_type(tempest_make_chat(factory_cfg, "coordinator"), "list")
})

test_that("default OpenAI chats use ChatGPT subscription authentication", {
  captured <- NULL
  local_mocked_bindings(
    tempest_chat_openai = function(...) {
      captured <<- list(...)
      list(mock = TRUE)
    }
  )

  chat <- tempest_make_chat(
    tempest_config(),
    "expert",
    system_prompt = "Research carefully.",
    echo = "all"
  )

  expect_identical(chat$mock, TRUE)
  expect_equal(captured$model, "gpt-5.6-luna")
  expect_equal(captured$system_prompt, "Research carefully.")
  expect_equal(captured$params, list())
  expect_equal(captured$echo, "all")
  expect_equal(captured$auth, "codex")
})

test_that("subscription chats tune auxiliary reasoning and honor params", {
  expect_equal(
    tempest_subscription_chat_params("mindmap", list()),
    list(reasoning_effort = "low")
  )
  expect_equal(
    tempest_subscription_chat_params(
      "mindmap",
      list(reasoning_effort = "high")
    ),
    list(reasoning_effort = "high")
  )
  expect_equal(
    tempest_subscription_chat_params("writer", list()),
    list()
  )
})

test_that("tempest_config validates tempest.chat", {
  withr::local_options(tempest.chat = 1)

  expect_snapshot(tempest_config(), error = TRUE)
  expect_error(tempest_config(), class = "tempest_config_error")
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
  skip_if(!"auth" %in% names(formals(ellmer::chat_openai)))

  cfg <- tempest_config()
  chat <- tempest_make_chat(cfg, "coordinator")
  expect_s3_class(chat, "Chat")
  expect_equal(chat$get_model(), "gpt-5.6-sol")
})

test_that("missing optional packages are classed", {
  expect_error(
    tempest:::tempest_require("tempestDefinitelyMissingPackage"),
    class = "tempest_missing_package_error"
  )
})
