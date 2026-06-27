test_that("extraction adds validated claims and rejects unknown sources", {
  store <- fake_store_with_sources(1)  # has source id for https://example.org/1
  s1 <- store$list_sources()[[1]]$id
  chat <- fake_chat(structured = list(list(facts = list(
    list(claim = "real claim", sources = list(list(source_id = s1)), confidence = "high"),
    list(claim = "ghost claim", sources = list(list(source_id = "Sdeadbeef0000")))
  ))))
  tempest_extract_facts_from_answer(chat, "answer text", store)
  claims <- store$list_claims()
  texts <- vapply(claims, function(c) c@claim_text, character(1))
  expect_true("real claim" %in% texts)
  expect_false("ghost claim" %in% texts)  # unknown source id is dropped, not stored
})
