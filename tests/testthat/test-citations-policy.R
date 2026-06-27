test_that("report renders verification badges under claim_verified", {
  store <- fake_store_with_sources(1)
  s1 <- store$list_sources()[[1]]$id
  store$add_claim(tempest_claim(claim_text = "c", source_ids = s1,
                                verification_status = "supported", support_score = 0.9))
  body <- paste0("A verified sentence [", s1, "].")
  md <- tempest_report_md("Title", body, store, citation_policy = "claim_verified")
  expect_match(md, "✓")  # check mark badge for supported
})

test_that("strict policy flags unsupported citations", {
  store <- fake_store_with_sources(1)
  s1 <- store$list_sources()[[1]]$id
  store$add_claim(tempest_claim(claim_text = "c", source_ids = s1,
                                verification_status = "unsupported", support_score = 0.1))
  body <- paste0("A questionable sentence [", s1, "].")
  md <- tempest_report_md("Title", body, store, citation_policy = "strict",
                          on_unsupported_claim = "flag")
  expect_match(md, "unsupported", ignore.case = TRUE)
})

test_that("default policy is unchanged source-attributed output", {
  store <- fake_store_with_sources(1)
  s1 <- store$list_sources()[[1]]$id
  body <- paste0("Plain sentence [", s1, "].")
  md <- tempest_report_md("Title", body, store)
  expect_match(md, "## References")
  expect_no_match(md, "✓")
})
