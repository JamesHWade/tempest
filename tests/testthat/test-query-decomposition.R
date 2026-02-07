test_that("tempest_type_query_decomposition returns valid type", {
  skip_if_not_installed("ellmer")

  type <- tempest:::tempest_type_query_decomposition()
  expect_true(!is.null(type))
})

test_that("tempest_decompose_query returns structured output", {
  skip_if_not_installed("ellmer")
  skip_if(
    Sys.getenv("OPENAI_API_KEY") == "" && Sys.getenv("ANTHROPIC_API_KEY") == "",
    "No API key available"
  )

  cfg <- tempest_config()
  chat <- cfg$make_chat("coordinator", echo = "none")

  result <- tempest:::tempest_decompose_query(
    chat,
    "What are the environmental impacts of electric vehicles?",
    "Electric vehicles"
  )

  expect_type(result, "list")
  expect_true(!is.null(result$queries))
  expect_true(length(result$queries) >= 1)
  expect_true(length(result$queries) <= 4)
})
