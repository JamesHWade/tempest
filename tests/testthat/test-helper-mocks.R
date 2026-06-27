test_that("fake_chat scripts structured and text returns", {
  ch <- fake_chat(structured = list(list(facts = list())), text = list("hi"))
  expect_equal(ch$chat_structured("p", type = NULL), list(facts = list()))
  expect_equal(ch$chat("p"), "hi")
  expect_length(ch$.calls(), 2)
})

test_that("fake_store_with_sources builds a populated store", {
  store <- fake_store_with_sources(3)
  expect_s3_class(store, "SourceStore")
  expect_length(store$list_sources(), 3)
})
