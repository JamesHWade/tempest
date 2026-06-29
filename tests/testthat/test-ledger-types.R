test_that("tempest_claim validates enums and scores", {
  cl <- tempest_claim(
    claim_text = "Water boils at 100C",
    source_ids = "S0123456789ab"
  )
  expect_equal(cl@claim_type, "finding")
  expect_equal(cl@verification_status, "unverified")
  expect_match(cl@claim_id, "^C")

  expect_error(
    tempest_claim(claim_text = "x", claim_type = "nonsense"),
    "claim_type"
  )
  expect_error(
    tempest_claim(claim_text = "x", claim_type = NA_character_),
    "claim_type"
  )
  expect_error(
    tempest_claim(claim_text = "x", verification_status = "bogus"),
    "verification_status"
  )
  expect_error(
    tempest_claim(claim_text = "x", support_score = 1.5),
    "\\[0, 1\\]"
  )
  expect_error(tempest_claim(claim_text = ""), "claim_text")
})

test_that("tempest_evidence_span and tempest_dispute construct", {
  sp <- tempest_evidence_span(
    source_id = "S0123456789ab",
    quote = "boils at 100C"
  )
  expect_match(sp@evidence_span_id, "^E")

  d <- tempest_dispute(
    topic = "boiling point",
    claim_ids = c("C1", "C2"),
    evidence_balance = "mixed"
  )
  expect_match(d@dispute_id, "^D")
  expect_error(
    tempest_dispute(topic = "x", evidence_balance = "weird"),
    "evidence_balance"
  )
})

test_that("tempest_source_record carries fields and meta", {
  s <- tempest_source_record(url = "https://example.org", title = "Ex")
  expect_match(s@id, "^S")
  expect_type(s@meta, "list")
})

test_that("source tibbles derive snippets and context text", {
  store <- SourceStore$new()
  source <- tempest_source(
    url = "https://example.org/context",
    title = "Context",
    content_text = "Full source body gives the table context."
  )
  store$upsert_source(source)
  native <- tempest_source(
    url = "https://example.org/native",
    title = "Native",
    meta = list(context_text = "Native citation context supports the answer.")
  )
  store$upsert_source(native)

  sources <- tempest_sources(store)
  expect_contains(names(sources), c("snippet", "content_text", "context_text"))
  expect_equal(
    sources$snippet[sources$id == source$id],
    "Full source body gives the table context."
  )
  expect_equal(
    sources$context_text[sources$id == native$id],
    "Native citation context supports the answer."
  )
  expect_equal(
    sources$snippet[sources$id == native$id],
    "Native citation context supports the answer."
  )
})

test_that("claim round-trips through list", {
  cl <- tempest_claim(
    claim_text = "x",
    source_ids = c("S1", "S2"),
    claim_type = "method",
    session_id = "session-1"
  )
  lst <- tempest_claim_to_list(cl)
  expect_equal(lst$claim_text, "x")
  expect_equal(lst$claim_type, "method")
  expect_equal(lst$session_id, "session-1")
  cl2 <- tempest_claim_from_list(lst)
  expect_equal(cl2@claim_id, cl@claim_id)
  expect_equal(cl2@source_ids, c("S1", "S2"))
  expect_equal(cl2@session_id, "session-1")
})

test_that("evidence spans and disputes round-trip through lists", {
  span <- tempest_evidence_span(
    source_id = "S0123456789ab",
    quote = "supporting quote",
    start_offset = 2L,
    end_offset = 18L,
    relevance_score = 0.8
  )
  span2 <- tempest_evidence_span_from_list(tempest_evidence_span_to_list(span))
  expect_equal(span2@evidence_span_id, span@evidence_span_id)
  expect_equal(span2@source_id, "S0123456789ab")
  expect_equal(span2@quote, "supporting quote")
  expect_equal(span2@start_offset, 2L)
  expect_equal(span2@relevance_score, 0.8)

  dispute <- tempest_dispute(
    topic = "evidence conflict",
    claim_ids = c("C1", "C2"),
    axis_of_disagreement = "measurement",
    unresolved_questions = c("Which sample?"),
    evidence_balance = "conflict"
  )
  dispute2 <- tempest_dispute_from_list(tempest_dispute_to_list(dispute))
  expect_equal(dispute2@dispute_id, dispute@dispute_id)
  expect_equal(dispute2@claim_ids, c("C1", "C2"))
  expect_equal(dispute2@unresolved_questions, "Which sample?")
  expect_equal(dispute2@evidence_balance, "conflict")
})

test_that("claims convert to a tibble", {
  cls <- list(
    tempest_claim(claim_text = "a", source_ids = "S1"),
    tempest_claim(
      claim_text = "b",
      source_ids = "S2",
      verification_status = "supported"
    )
  )
  tb <- tempest_claims_tibble(cls)
  expect_s3_class(tb, "tbl_df")
  expect_equal(nrow(tb), 2)
  expect_contains(
    names(tb),
    c(
      "claim_id",
      "claim_text",
      "verification_status",
      "support_score",
      "session_id"
    )
  )
})
