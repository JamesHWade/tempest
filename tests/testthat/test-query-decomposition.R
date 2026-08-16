test_that("tempest_type_query_decomposition returns valid type", {
  skip_if_not_installed("ellmer")

  type <- tempest:::tempest_type_query_decomposition()
  expect_s7_class(type, getFromNamespace("TypeObject", "ellmer"))
})

test_that("tempest_decompose_query returns structured output", {
  skip_if_not_installed("ellmer")
  chat <- fake_chat(
    structured = list(list(
      queries = c(
        "electric vehicle lifecycle emissions",
        "electric vehicle battery environmental impacts",
        "electric vehicle grid electricity mix"
      )
    ))
  )

  result <- tempest:::tempest_decompose_query(
    chat,
    "What are the environmental impacts of electric vehicles?",
    "Electric vehicles"
  )

  expect_type(result, "list")
  expect_equal(
    result$queries,
    c(
      "electric vehicle lifecycle emissions",
      "electric vehicle battery environmental impacts",
      "electric vehicle grid electricity mix"
    )
  )
  expect_length(chat$.calls(), 1L)
  expect_identical(chat$.calls()[[1]]$kind, "structured")
  expect_match(chat$.calls()[[1]]$prompt, "Electric vehicles", fixed = TRUE)
})
