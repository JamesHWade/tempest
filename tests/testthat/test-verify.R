test_that("verify_claims labels each claim and returns an audit tibble", {
  store <- fake_store_with_sources(1)
  s1 <- store$list_sources()[[1]]$id
  store$add_claim(tempest_claim(
    claim_text = "supported claim",
    source_ids = s1
  ))
  store$add_claim(tempest_claim(claim_text = "weak claim", source_ids = s1))

  judge <- fake_chat(
    structured = list(
      list(status = "supported", score = 0.92, rationale = "stated verbatim"),
      list(status = "unsupported", score = 0.10, rationale = "not in source")
    )
  )

  audit <- tempest_verify_claims(store, verifier = judge)
  expect_s3_class(audit, "tbl_df")
  expect_equal(nrow(audit), 2)
  expect_setequal(audit$verification_status, c("supported", "unsupported"))

  statuses <- vapply(
    store$list_claims(),
    function(c) c@verification_status,
    character(1)
  )
  expect_contains(statuses, c("supported", "unsupported"))
})

test_that("verify_claims enforces min_support_score", {
  store <- fake_store_with_sources(1)
  s1 <- store$list_sources()[[1]]$id
  store$add_claim(tempest_claim(
    claim_text = "weakly scored claim",
    source_ids = s1
  ))

  judge <- fake_chat(
    structured = list(
      list(status = "supported", score = 0.60, rationale = "weak match")
    )
  )

  audit <- tempest_verify_claims(
    store,
    verifier = judge,
    min_support_score = 0.7
  )
  expect_equal(audit$verification_status, "unsupported")
  expect_equal(store$list_claims()[[1]]@verification_status, "unsupported")
  expect_equal(store$list_claims()[[1]]@support_score, 0.60)
})

test_that("verify_claims skips when policy is none/source_attributed", {
  store <- fake_store_with_sources(1)
  store$add_claim(tempest_claim(
    claim_text = "c",
    source_ids = store$list_sources()[[1]]$id
  ))
  store$set_artifact("citation_audit", tibble::tibble(claim_id = "stale"))
  audit <- tempest_verify_claims(
    store,
    verifier = fake_chat(),
    policy = "source_attributed"
  )
  expect_equal(nrow(audit), 0)
  expect_equal(nrow(store$get_artifact("citation_audit")), 0)
})

test_that("verify_claims sanitizes out-of-range, string, and invalid judge output", {
  store <- fake_store_with_sources(1)
  s1 <- store$list_sources()[[1]]$id
  store$add_claim(tempest_claim(claim_text = "a", source_ids = s1))
  store$add_claim(tempest_claim(claim_text = "b", source_ids = s1))
  judge <- fake_chat(
    structured = list(
      list(status = "supported", score = 1.5, rationale = "over range"),
      list(
        status = "weird_status",
        score = "0.7",
        rationale = "string score, bad status"
      )
    )
  )

  audit <- tempest_verify_claims(store, verifier = judge) # must not abort
  expect_equal(nrow(audit), 2)

  cl <- store$list_claims()
  scores <- vapply(cl, function(c) c@support_score, numeric(1))
  bounded_scores <- scores[!is.na(scores)]
  expect_equal(
    which(bounded_scores < 0 | bounded_scores > 1),
    integer()
  )
  statuses <- vapply(cl, function(c) c@verification_status, character(1))
  valid <- c(
    "supported",
    "partially_supported",
    "unsupported",
    "contradicted",
    "unverifiable"
  )
  expect_equal(setdiff(statuses, valid), character())
})
