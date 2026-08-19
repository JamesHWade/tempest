test_that("ResearchWorkspace record schemas and identities are exact", {
  workspace <- tempest_research_workspace(max_sources = 8L)
  source <- tempest:::tempest_source(
    "https://example.com/exact-records",
    title = "Exact record source",
    content_text = "Exact evidence"
  )
  workspace$upsert_retrieved_resource(source)
  span_id <- workspace$add_evidence_span(tempest_evidence_span(
    evidence_span_id = "span-exact",
    source_id = source$id,
    quote = "Exact evidence"
  ))
  claim_id <- workspace$add_proposed_claim(tempest_claim(
    claim_id = "claim-exact",
    claim_text = "Workspace records have exact schemas.",
    source_ids = source$id,
    evidence_span_ids = span_id,
    supporting_quotes = list("Exact evidence")
  ))
  workspace$add_dispute(tempest_dispute(
    dispute_id = "dispute-exact",
    topic = "Exact schemas",
    claim_ids = claim_id
  ))
  workspace$verify_proposed_claims_batch(
    list(tempest_claim_support(
      claim_id = claim_id,
      evidence_span_id = span_id,
      source_id = source$id,
      verification_status = "unverifiable",
      support_score = NA_real_,
      rationale = "Not yet reviewed"
    )),
    verified_at = "2026-08-16T12:03:00Z"
  )
  snapshot <- tempest:::tempest_research_workspace_snapshot(workspace)

  for (field in c(
    "retrieved_resources",
    "proposed_claims",
    "evidence_spans",
    "claim_supports",
    "disputes"
  )) {
    duplicated <- snapshot
    duplicated[[field]] <- c(duplicated[[field]], duplicated[[field]][1])
    expect_error(
      tempest:::tempest_research_workspace_restore(duplicated),
      class = "tempest_research_workspace_restore_error"
    )

    missing <- snapshot
    record <- missing[[field]][[1]]
    missing[[field]][[1]] <- record[-1]
    expect_error(
      tempest:::tempest_research_workspace_restore(missing),
      class = "tempest_research_workspace_restore_error"
    )

    extra <- snapshot
    extra[[field]][[1]]$runtime <- "unsupported"
    expect_error(
      tempest:::tempest_research_workspace_restore(extra),
      class = "tempest_research_workspace_restore_error"
    )
  }

  resource_schema_values <- list(
    null = NULL,
    string = "1",
    unknown = 999L
  )
  for (name in names(resource_schema_values)) {
    malformed <- snapshot
    malformed$retrieved_resources[[1]]["schema_version"] <- list(
      resource_schema_values[[name]]
    )
    malformed$retrieved_resources[[1]]$fingerprint <-
      tempest:::tempest_resource_fingerprint(
        malformed$retrieved_resources[[1]]
      )
    expect_error(
      tempest:::tempest_research_workspace_restore(malformed),
      class = "tempest_research_workspace_restore_error",
      info = name
    )
  }

  missing_support <- snapshot
  missing_support$claim_supports[[1]]$rationale <- NULL
  expect_error(
    tempest:::tempest_research_workspace_restore(missing_support),
    class = "tempest_research_workspace_restore_error"
  )
  extra_support <- snapshot
  extra_support$claim_supports[[1]]$runtime <- "unsupported"
  expect_error(
    tempest:::tempest_research_workspace_restore(extra_support),
    class = "tempest_research_workspace_restore_error"
  )

  malformed <- snapshot
  malformed$proposed_claims[[1]]$source_ids <- source$id
  expect_error(
    tempest:::tempest_research_workspace_restore(malformed),
    class = "tempest_research_workspace_restore_error"
  )
  malformed <- snapshot
  malformed$proposed_claims[[1]]$source_ids <- list(list(source$id))
  expect_error(
    tempest:::tempest_research_workspace_restore(malformed),
    class = "tempest_research_workspace_restore_error"
  )
  malformed <- snapshot
  malformed$proposed_claims[[1]]["created_at"] <- list(NULL)
  expect_error(
    tempest:::tempest_research_workspace_restore(malformed),
    class = "tempest_research_workspace_restore_error"
  )
  malformed <- snapshot
  malformed$evidence_spans[[1]]$start_offset <- 1.5
  expect_error(
    tempest:::tempest_research_workspace_restore(malformed),
    class = "tempest_research_workspace_restore_error"
  )
  malformed <- snapshot
  malformed$evidence_spans[[1]]$start_offset <- 1.0
  expect_error(
    tempest:::tempest_research_workspace_restore(malformed),
    class = "tempest_research_workspace_restore_error"
  )
  malformed <- snapshot
  malformed$evidence_spans[[1]]["extracted_by"] <- list(NULL)
  expect_error(
    tempest:::tempest_research_workspace_restore(malformed),
    class = "tempest_research_workspace_restore_error"
  )
  malformed <- snapshot
  malformed$disputes[[1]]$claim_ids <- claim_id
  expect_error(
    tempest:::tempest_research_workspace_restore(malformed),
    class = "tempest_research_workspace_restore_error"
  )
  malformed <- snapshot
  malformed$disputes[[1]]$claim_ids <- list(list(claim_id))
  expect_error(
    tempest:::tempest_research_workspace_restore(malformed),
    class = "tempest_research_workspace_restore_error"
  )
  malformed <- snapshot
  malformed$disputes[[1]]["evidence_balance"] <- list(NULL)
  expect_error(
    tempest:::tempest_research_workspace_restore(malformed),
    class = "tempest_research_workspace_restore_error"
  )
})

test_that("ResearchWorkspace caller restore is transactional", {
  persisted <- tempest_research_workspace(max_sources = 8L)
  source <- tempest:::tempest_source(
    "https://example.com/transaction-persisted",
    title = "Persisted source"
  )
  persisted$upsert_retrieved_resource(source)
  claim_id <- persisted$add_proposed_claim(tempest_claim(
    claim_id = "claim-transaction",
    claim_text = "Late failures do not mutate caller state.",
    source_ids = source$id
  ))
  persisted$add_dispute(tempest_dispute(
    dispute_id = "dispute-transaction",
    topic = "Transactional restore",
    claim_ids = claim_id,
    evidence_balance = "mixed"
  ))
  malformed <- tempest:::tempest_research_workspace_snapshot(persisted)
  malformed$disputes[[1L]]$claim_ids <- list("claim.unknown")

  caller <- tempest_research_workspace(max_sources = 2L)
  caller$upsert_retrieved_resource(tempest:::tempest_source(
    "https://example.com/transaction-caller",
    title = "Caller source"
  ))
  before <- serialize(
    tempest:::tempest_research_workspace_snapshot(caller),
    NULL,
    version = 3L
  )

  expect_error(
    tempest:::tempest_research_workspace_restore(
      malformed,
      workspace = caller
    ),
    class = "tempest_research_workspace_restore_error"
  )
  after <- serialize(
    tempest:::tempest_research_workspace_snapshot(caller),
    NULL,
    version = 3L
  )

  expect_identical(after, before)
  expect_identical(caller$max_sources, 2L)
})
