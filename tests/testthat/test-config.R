test_that("storm_config creates valid config", {
  cfg <- storm_config()
  expect_s3_class(cfg, "StormConfig")

  expect_true(!is.null(cfg$models))
  expect_true(!is.null(cfg$models$coordinator))
  expect_true(!is.null(cfg$search_provider))
})

test_that("storm_config accepts custom models as list", {
  cfg <- storm_config(models = list(
    coordinator = "anthropic/claude-sonnet-4-20250514",
    writer = "anthropic/claude-sonnet-4-20250514",
    expert = "anthropic/claude-sonnet-4-20250514"
  ))
  expect_equal(cfg$models$coordinator, "anthropic/claude-sonnet-4-20250514")
})

test_that("storm_config accepts single model string for all roles", {
  cfg <- storm_config(models = "anthropic/claude-sonnet-4-20250514")
  expect_equal(cfg$models$coordinator, "anthropic/claude-sonnet-4-20250514")
  expect_equal(cfg$models$writer, "anthropic/claude-sonnet-4-20250514")
  expect_equal(cfg$models$expert, "anthropic/claude-sonnet-4-20250514")
})

test_that("storm_config accepts custom chat_fn", {
  mock_chat_fn <- function(role, model, system_prompt, echo) {
    list(
      role = role,
      model = model,
      system_prompt = system_prompt,
      echo = echo,
      mock = TRUE
    )
  }

  cfg <- storm_config(chat_fn = mock_chat_fn)
  chat <- cfg$make_chat("coordinator")

  expect_true(chat$mock)
  expect_equal(chat$role, "coordinator")
  expect_true(grepl("coordinator", chat$model) || grepl("gpt", chat$model))
})

test_that("storm_config accepts tools as list or function", {
  cfg_list <- storm_config(tools = list())
  expect_type(cfg_list$tools, "list")

  cfg_fn <- storm_config(tools = function() list())
  expect_type(cfg_fn$tools, "closure")
})

test_that("make_chat creates ellmer chat object", {
  skip_if_not_installed("ellmer")
  skip_if(
    Sys.getenv("OPENAI_API_KEY") == "" && Sys.getenv("ANTHROPIC_API_KEY") == "",
    "No API key available"
  )

  cfg <- storm_config()
  chat <- cfg$make_chat("coordinator")
  expect_s3_class(chat, "Chat")
})
