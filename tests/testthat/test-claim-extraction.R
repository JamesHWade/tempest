test_that("extraction adds validated claims and rejects unknown sources", {
  store <- fake_store_with_sources(1) # has source id for https://example.org/1
  s1 <- store$list_sources()[[1]]$id
  chat <- fake_chat(
    structured = list(list(
      facts = list(
        list(
          claim = "real claim",
          sources = list(list(source_id = s1)),
          confidence = "high"
        ),
        list(
          claim = "ghost claim",
          sources = list(list(source_id = "Sdeadbeef0000"))
        )
      )
    ))
  )
  tempest_extract_facts_from_answer(chat, "answer text", store)
  claims <- store$list_claims()
  texts <- vapply(claims, function(c) c@claim_text, character(1))
  expect_contains(texts, "real claim")
  expect_equal(intersect(texts, "ghost claim"), character())
})

test_that("extraction keeps claims when the optional confidence is omitted", {
  store <- fake_store_with_sources(1)
  s1 <- store$list_sources()[[1]]$id
  chat <- fake_chat(
    structured = list(list(
      facts = list(
        list(
          claim = "no-confidence claim",
          sources = list(list(source_id = s1))
        )
      )
    ))
  )
  tempest_extract_facts_from_answer(chat, "answer text", store)
  claims <- store$list_claims()
  expect_length(claims, 1)
  expect_equal(claims[[1]]@claim_text, "no-confidence claim")
  expect_equal(claims[[1]]@confidence, "medium") # NA confidence defaults, not aborts
})

test_that("extraction normalizes factor confidence values", {
  store <- fake_store_with_sources(1)
  s1 <- store$list_sources()[[1]]$id
  chat <- fake_chat(
    structured = list(list(
      facts = list(
        list(
          claim = factor("factor-confidence claim"),
          sources = list(list(source_id = s1)),
          confidence = factor("high", levels = c("low", "medium", "high"))
        )
      )
    ))
  )

  tempest_extract_facts_from_answer(chat, "answer text", store)

  claims <- store$list_claims()
  expect_length(claims, 1)
  expect_equal(claims[[1]]@claim_text, "factor-confidence claim")
  expect_equal(claims[[1]]@confidence, "high")
})

test_that("extraction defaults empty confidence instead of erroring", {
  store <- fake_store_with_sources(1)
  s1 <- store$list_sources()[[1]]$id
  chat <- fake_chat(
    structured = list(list(
      facts = list(list(
        claim = "empty-confidence claim",
        sources = list(list(source_id = s1)),
        confidence = character()
      ))
    ))
  )

  tempest_extract_facts_from_answer(chat, "answer text", store)

  claims <- store$list_claims()
  expect_length(claims, 1)
  expect_equal(claims[[1]]@confidence, "medium")
})

test_that("extraction skips facts with empty claim text without aborting", {
  store <- fake_store_with_sources(1)
  s1 <- store$list_sources()[[1]]$id
  chat <- fake_chat(
    structured = list(list(
      facts = list(
        list(
          claim = character(),
          sources = list(list(source_id = s1)),
          confidence = "high"
        ),
        list(
          claim = "valid claim",
          sources = list(list(source_id = s1)),
          confidence = "high"
        )
      )
    ))
  )

  tempest_extract_facts_from_answer(chat, "answer text", store)

  claims <- store$list_claims()
  expect_length(claims, 1)
  expect_equal(claims[[1]]@claim_text, "valid claim")
})

test_that("extraction accepts vector source references", {
  store <- fake_store_with_sources(2)
  ids <- vapply(store$list_sources(), `[[`, character(1), "id")
  chat <- fake_chat(
    structured = list(list(
      facts = list(
        list(
          claim = "multi-source claim",
          sources = list(list(source_id = ids)),
          confidence = "high"
        )
      )
    ))
  )

  tempest_extract_facts_from_answer(chat, "answer text", store)

  claims <- store$list_claims()
  expect_length(claims, 1)
  expect_equal(claims[[1]]@source_ids, ids)
})

test_that("extraction resolves cited source URLs to stored source ids", {
  store <- fake_store_with_sources(1)
  source <- store$list_sources()[[1]]
  chat <- fake_chat(
    structured = list(list(
      facts = list(list(
        claim = "url-backed claim",
        sources = list(list(url = source$url)),
        confidence = "high"
      ))
    ))
  )

  tempest_extract_facts_from_answer(
    chat,
    paste("url-backed claim", source$url),
    store
  )

  claims <- store$list_claims()
  expect_length(claims, 1)
  expect_equal(claims[[1]]@source_ids, source$id)

  prompts <- vapply(chat$.calls(), function(call) call$prompt, character(1))
  expect_match(prompts[[1]], "<known_sources>", fixed = TRUE)
  expect_match(prompts[[1]], source$id, fixed = TRUE)
})

test_that("extraction uses source ids attached to provider-native turns", {
  store <- fake_store_with_sources(1)
  source <- store$list_sources()[[1]]
  chat <- fake_chat(
    structured = list(list(
      facts = list(list(
        claim = "native-backed claim",
        sources = list(list(source_id = source$id)),
        confidence = "high"
      ))
    ))
  )

  tempest_extract_facts_from_answer(
    chat,
    "native-backed claim.",
    store,
    source_ids = source$id
  )

  claims <- store$list_claims()
  expect_length(claims, 1)
  expect_equal(claims[[1]]@claim_text, "native-backed claim")
  expect_equal(claims[[1]]@source_ids, source$id)

  prompts <- vapply(chat$.calls(), function(call) call$prompt, character(1))
  expect_match(prompts[[1]], "provider-native citation markers", fixed = TRUE)
  expect_match(prompts[[1]], source$id, fixed = TRUE)
})

test_that("extraction bypasses dsprrr module when native sources are attached", {
  store <- fake_store_with_sources(1)
  source <- store$list_sources()[[1]]
  chat <- fake_chat(
    structured = list(list(
      facts = list(list(
        claim = "prompt-backed native claim",
        sources = list(list(source_id = source$id)),
        confidence = "high"
      ))
    ))
  )
  local_mocked_bindings(
    tempest_run_dsprrr_module = function(...) {
      list(
        facts = list(list(
          claim = "module-backed claim",
          sources = list(list(source_id = source$id)),
          confidence = "high"
        ))
      )
    }
  )

  tempest_extract_facts_from_answer(
    chat,
    "prompt-backed native claim.",
    store,
    module = list(),
    source_ids = source$id
  )

  claims <- store$list_claims()
  expect_length(claims, 1)
  expect_equal(claims[[1]]@claim_text, "prompt-backed native claim")
})

test_that("extraction drops claims citing URLs that match no stored source", {
  store <- fake_store_with_sources(1)
  unknown_url <- "https://unknown.example/not-in-store"
  chat <- fake_chat(
    structured = list(list(
      facts = list(list(
        claim = "unverifiable claim",
        sources = list(list(url = unknown_url)),
        confidence = "high"
      ))
    ))
  )

  tempest_extract_facts_from_answer(
    chat,
    paste("unverifiable claim", unknown_url),
    store
  )

  expect_length(store$list_claims(), 0)
})
