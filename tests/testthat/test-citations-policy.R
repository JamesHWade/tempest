test_that("report renders verification badges under claim_verified", {
  store <- fake_store_with_sources(1)
  s1 <- store$list_sources()[[1]]$id
  store$add_claim(tempest_claim(
    claim_text = "A questionable sentence",
    source_ids = s1,
    verification_status = "supported",
    support_score = 0.9
  ))
  body <- paste0("A verified sentence [", s1, "].")
  md <- tempest_report_md(
    "Title",
    body,
    store,
    citation_policy = "claim_verified"
  )
  expect_match(md, "✓") # check mark badge for supported
})

test_that("citation helpers consume a ResearchWorkspace directly", {
  workspace <- tempest_research_workspace()
  source <- fake_source("https://example.org/workspace")
  workspace$upsert_source(source)
  claim <- tempest_claim(
    claim_text = "Workspace evidence is provisional.",
    source_ids = source$id
  )
  workspace$add_claim(claim)

  expect_equal(tempest_sources(workspace)$id, source$id)
  expect_equal(tempest_claims(workspace)$claim_id, claim@claim_id)
  expect_match(
    tempest_report_md(
      "Workspace report",
      paste0("Workspace evidence [", source$id, "]."),
      workspace
    ),
    "## References",
    fixed = TRUE
  )
})

test_that("strict policy flags unsupported citations", {
  store <- fake_store_with_sources(1)
  s1 <- store$list_sources()[[1]]$id
  store$add_claim(tempest_claim(
    claim_text = "c",
    source_ids = s1,
    verification_status = "unsupported",
    support_score = 0.1
  ))
  body <- paste0("A questionable sentence [", s1, "].")
  md <- tempest_report_md(
    "Title",
    body,
    store,
    citation_policy = "strict",
    on_unsupported_claim = "flag"
  )
  expect_match(md, "unsupported", ignore.case = TRUE)
})

test_that("none policy leaves citations unfootnoted and omits references", {
  store <- fake_store_with_sources(1)
  s1 <- store$list_sources()[[1]]$id
  body <- paste0("Plain sentence [", s1, "].")

  md <- tempest_report_md("Title", body, store, citation_policy = "none")

  expect_match(md, paste0("\\[", s1, "\\]"))
  expect_no_match(md, "\\[\\^")
  expect_no_match(md, "## References")
})

test_that("strict drop suppresses footnotes for dropped citations", {
  store <- fake_store_with_sources(1)
  s1 <- store$list_sources()[[1]]$id
  store$add_claim(tempest_claim(
    claim_text = "Unsupported claim",
    source_ids = s1,
    verification_status = "unsupported",
    support_score = 0.1
  ))
  body <- paste0("Unsupported claim [", s1, "].")

  md <- tempest_report_md(
    "Title",
    body,
    store,
    citation_policy = "strict",
    on_unsupported_claim = "drop"
  )

  expect_no_match(md, paste0("\\[\\^", s1, "\\]"))
  expect_no_match(md, paste0("\\[\\^", s1, "\\]:"))
  expect_no_match(md, "## References")
  expect_no_match(md, "Unsupported claim", fixed = TRUE)
})

test_that("strict policy applies status to the matching cited claim", {
  store <- fake_store_with_sources(1)
  s1 <- store$list_sources()[[1]]$id
  store$add_claim(tempest_claim(
    claim_text = "Supported claim",
    source_ids = s1,
    verification_status = "supported",
    support_score = 0.9
  ))
  store$add_claim(tempest_claim(
    claim_text = "Unsupported claim",
    source_ids = s1,
    verification_status = "unsupported",
    support_score = 0.1
  ))
  body <- paste0("Supported claim [", s1, "]. Unsupported claim [", s1, "].")

  md <- tempest_report_md(
    "Title",
    body,
    store,
    citation_policy = "strict",
    on_unsupported_claim = "drop"
  )

  expect_match(md, paste0("Supported claim \\[\\^", s1, "\\]\\."))
  expect_no_match(md, "Unsupported claim", fixed = TRUE)
  expect_match(md, paste0("\\[\\^", s1, "\\]:"))
})

test_that("strict revise replaces unsupported assertions distinctly", {
  store <- fake_store_with_sources(1)
  source_id <- store$list_sources()[[1]]$id
  store$add_claim(tempest_claim(
    claim_text = "Unsupported assertion",
    source_ids = source_id,
    verification_status = "unsupported",
    support_score = 0.1
  ))
  body <- paste0("Unsupported assertion [", source_id, "].")

  revised <- tempest_report_md(
    "Title",
    body,
    store,
    citation_policy = "strict",
    on_unsupported_claim = "revise"
  )

  expect_no_match(revised, "Unsupported assertion", fixed = TRUE)
  expect_match(revised, "withheld pending revision", fixed = TRUE)
  expect_no_match(revised, "## References", fixed = TRUE)
})

test_that("default policy is unchanged source-attributed output", {
  store <- fake_store_with_sources(1)
  s1 <- store$list_sources()[[1]]$id
  body <- paste0("Plain sentence [", s1, "].")
  md <- tempest_report_md("Title", body, store)
  expect_match(md, "## References")
  expect_no_match(md, "✓")
})

test_that("session report Markdown comes from the session artifact catalog", {
  skip_if_not_installed("ellmer")
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  session <- tempest_session(
    "Canonical report",
    config = cfg,
    experts = list(test_expert(
      expert_id = "expert.canonical-report",
      name = "Dr. Canonical"
    ))
  )
  report_md <- "# Canonical report\n\nDurable body."
  report_spec <- tempest:::tempest_costorm_report_spec(session)
  session$artifact_catalog$register(report_spec)
  session$artifact_catalog$add(tempest_artifact(
    report_spec,
    content = report_md,
    artifact_id = "report_md",
    status = "valid"
  ))
  catalog_before <- session$artifact_catalog$snapshot(include_content = TRUE)
  local_mocked_bindings(
    tempest_artifact_catalog = function(...) {
      stop("detached catalog created")
    }
  )

  expect_identical(tempest_session_report_md(session), report_md)
  expect_identical(
    session$artifact_catalog$snapshot(include_content = TRUE),
    catalog_before
  )
})

test_that("session report Markdown requires a canonical report artifact", {
  skip_if_not_installed("ellmer")
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  session <- tempest_session(
    "Missing report",
    config = cfg,
    experts = list(test_expert(
      expert_id = "expert.missing-report",
      name = "Dr. Missing"
    ))
  )

  expect_error(
    tempest_session_report_md(session),
    class = "tempest_artifact_catalog_error"
  )
})
