test_that("durable integer validation accepts the exact non-NA range", {
  for (value in c(
    -.Machine$integer.max,
    0L,
    .Machine$integer.max
  )) {
    expect_identical(
      tempest:::tempest_exact_integer_scalar(value, "test integer"),
      value
    )
  }
  expect_error(
    tempest:::tempest_exact_integer_scalar(0.0, "test integer"),
    class = "tempest_error"
  )
  expect_error(
    tempest:::tempest_exact_integer_scalar(NA_integer_, "test integer"),
    class = "tempest_error"
  )
})

test_that("ResearchWorkspace snapshots restore artifact-free product state", {
  skip_if_not_installed("jsonlite")
  workspace <- tempest_research_workspace(
    base_snapshot_id = "snapshot-a",
    max_sources = 4L,
    accepted_graft_references = list(
      list(record_id = "record-z", revision_id = "revision-2"),
      list(record_id = "record-a", revision_id = "revision-1")
    )
  )
  source <- test_typed_web_resource(
    "https://example.com/workspace",
    title = "Workspace source",
    snippet = "Workspace snippet",
    content = "workspace evidence"
  )
  resource <- tempest_resource(
    resource_kind = "scientific.document",
    locator = "protocols/workspace",
    title = "Workspace protocol",
    media_type = "text/plain",
    content = "Preserve the provisional evidence ledger.",
    retrieved_at = "2026-08-15T12:00:00Z",
    metadata = list(revision = "reviewed")
  )
  workspace$upsert_retrieved_resource(source)
  workspace$upsert_retrieved_resource(resource)
  span_id <- workspace$add_evidence_span(tempest_evidence_span(
    evidence_span_id = "span-a",
    source_id = source@resource_id,
    quote = "workspace evidence"
  ))
  claim_id <- workspace$add_proposed_claim(tempest_claim(
    claim_id = "claim-a",
    claim_text = "Workspaces preserve provisional evidence.",
    source_ids = source@resource_id,
    evidence_span_ids = span_id,
    supporting_quotes = list("workspace evidence"),
    confidence = "high",
    verification_status = "supported",
    support_score = 0.95
  ))
  workspace$add_dispute(tempest_dispute(
    dispute_id = "dispute-a",
    topic = "workspace persistence",
    claim_ids = claim_id,
    evidence_balance = "agreement"
  ))
  workspace$verify_proposed_claims_batch(
    list(tempest_claim_support(
      claim_id = claim_id,
      evidence_span_id = span_id,
      source_id = source@resource_id,
      verification_status = "supported",
      support_score = 0.95,
      rationale = "Direct support"
    )),
    verified_at = "2026-08-16T12:03:00Z"
  )

  now_calls <- 0L
  local_mocked_bindings(
    tempest_now_utc = function() {
      now_calls <<- now_calls + 1L
      "2099-01-01T00:00:00Z"
    }
  )
  snapshot <- tempest:::tempest_research_workspace_snapshot(workspace)
  for (field in c(
    "source_ids",
    "evidence_span_ids",
    "supporting_quotes",
    "contradicting_source_ids"
  )) {
    expect_type(snapshot$proposed_claims[[1]][[field]], "list")
  }
  expect_type(snapshot$disputes[[1]]$claim_ids, "list")
  expect_type(snapshot$disputes[[1]]$unresolved_questions, "list")
  snapshot_again <- tempest:::tempest_research_workspace_snapshot(workspace)

  expect_identical(snapshot, snapshot_again)
  expect_identical(now_calls, 0L)
  expect_identical(snapshot$schema_version, 5L)
  expect_identical(snapshot$max_sources, 4L)
  expect_named(
    snapshot,
    c(
      "schema_version",
      "base_snapshot_id",
      "max_sources",
      "accepted_graft_references",
      "retrieved_resources",
      "proposed_claims",
      "evidence_spans",
      "claim_supports",
      "disputes"
    )
  )
  expect_length(snapshot$retrieved_resources, 2L)
  resource_ids <- vapply(
    snapshot$retrieved_resources,
    `[[`,
    character(1),
    "resource_id"
  )
  expect_contains(resource_ids, c(resource@resource_id, source@resource_id))
  expect_equal(
    vapply(
      snapshot$accepted_graft_references,
      `[[`,
      character(1),
      "record_id"
    ),
    c("record-a", "record-z")
  )

  path <- withr::local_tempfile(fileext = ".json")
  tempest:::tempest_product_write_json(path, snapshot)
  restored <- tempest:::tempest_research_workspace_restore(
    tempest:::tempest_product_read_json(path)
  )
  restored_snapshot <- tempest:::tempest_research_workspace_snapshot(restored)

  expect_identical(restored_snapshot, snapshot)
  expect_r6_class(restored, "ResearchWorkspace")
  expect_identical(restored$base_snapshot_id, "snapshot-a")
  expect_equal(restored$max_sources, 4L)
  expect_equal(
    restored$list_accepted_graft_references(),
    workspace$list_accepted_graft_references()
  )
  expect_s7_class(
    restored$get_retrieved_resource(source@resource_id),
    tempest:::TempestResource
  )
  expect_equal(
    restored$get_retrieved_source(source@resource_id)$title,
    "Workspace source"
  )
  expect_equal(
    restored$get_proposed_claim(claim_id)@claim_text,
    "Workspaces preserve provisional evidence."
  )
  expect_equal(
    restored$get_evidence_for_proposed_claim(claim_id)[[1]]@quote,
    "workspace evidence"
  )
  expect_equal(restored$list_disputes()[[1]]@dispute_id, "dispute-a")
  expect_equal(restored$citation_audit$support_score, 0.95)
  expect_equal("artifacts" %in% names(restored), FALSE)

  workspace$record_accepted_graft_reference(list(record_id = "record-new"))
  sealed_before <- tempest:::tempest_research_workspace_snapshot(workspace)
  expect_error(
    workspace$upsert_retrieved_resource(test_typed_web_resource(
      "https://example.com/workspace",
      title = "Changed after snapshot",
      content = "workspace evidence"
    )),
    class = "tempest_research_workspace_integrity_error"
  )
  expect_identical(
    tempest:::tempest_research_workspace_snapshot(workspace),
    sealed_before
  )
  expect_equal(length(snapshot$accepted_graft_references), 2L)
  titles <- vapply(
    snapshot$retrieved_resources,
    `[[`,
    character(1),
    "title"
  )
  expect_contains(titles, c("Workspace source", "Workspace protocol"))
  expect_equal(snapshot$claim_supports[[1]]$support_score, 0.95)

  snapshot$accepted_graft_references[[1]]$record_id <- "tampered"
  snapshot$retrieved_resources[[1]]$title <- "Tampered snapshot"
  expect_equal(
    restored$accepted_graft_references[[1]]$record_id,
    "record-a"
  )
  expect_equal(
    restored$get_retrieved_source(source@resource_id)$title,
    "Workspace source"
  )
})

test_that("ResearchWorkspace rejects pre-hard-cut snapshots", {
  expect_error(
    tempest:::tempest_research_workspace_restore(list(schema_version = 4L)),
    class = "tempest_unsupported_format_error"
  )
})

test_that("ResearchWorkspace snapshots encode unbounded source limits", {
  skip_if_not_installed("jsonlite")
  snapshot <- tempest:::tempest_research_workspace_snapshot(
    tempest_research_workspace()
  )

  expect_identical(snapshot$max_sources, "unbounded")
  expect_no_error(jsonlite::toJSON(
    snapshot,
    auto_unbox = TRUE,
    null = "null",
    na = "null"
  ))
  restored <- tempest:::tempest_research_workspace_restore(snapshot)
  expect_identical(restored$max_sources, Inf)
})

test_that("ResearchWorkspace restore validates schema and pinned state", {
  snapshot <- tempest:::tempest_research_workspace_snapshot(
    tempest_research_workspace(
      base_snapshot_id = "snapshot-a",
      accepted_graft_references = list(list(record_id = "record-a"))
    )
  )
  foreign <- tempest_research_workspace(
    base_snapshot_id = "snapshot-b",
    accepted_graft_references = list(list(record_id = "record-a"))
  )

  expect_error(
    tempest:::tempest_research_workspace_restore(
      snapshot,
      workspace = foreign
    ),
    class = "tempest_research_workspace_restore_error"
  )
  foreign <- tempest_research_workspace(
    base_snapshot_id = "snapshot-a",
    accepted_graft_references = list(list(record_id = "record-b"))
  )
  expect_error(
    tempest:::tempest_research_workspace_restore(
      snapshot,
      workspace = foreign
    ),
    class = "tempest_research_workspace_restore_error"
  )

  malformed <- snapshot
  malformed$retrieved_resources <- list(list(resource_id = "foreign-record"))
  expect_error(
    tempest:::tempest_research_workspace_restore(malformed),
    class = "tempest_research_workspace_restore_error"
  )

  snapshot$artifacts <- list(report_md = "not workspace state")
  expect_error(
    tempest:::tempest_research_workspace_restore(snapshot),
    class = "tempest_research_workspace_restore_error"
  )
  snapshot$artifacts <- NULL
  snapshot$runtime <- list(client_id = "not workspace state")
  expect_error(
    tempest:::tempest_research_workspace_restore(snapshot),
    class = "tempest_research_workspace_restore_error"
  )
  snapshot$runtime <- NULL
  snapshot$max_sources <- Inf
  expect_error(
    tempest:::tempest_research_workspace_restore(snapshot),
    class = "tempest_research_workspace_restore_error"
  )
  snapshot$max_sources <- "unbounded"
  double_schema <- snapshot
  double_schema$schema_version <- 5.0
  expect_error(
    tempest:::tempest_research_workspace_restore(double_schema),
    class = "tempest_research_workspace_restore_error"
  )
  double_max_sources <- tempest:::tempest_research_workspace_snapshot(
    tempest_research_workspace(max_sources = 8L)
  )
  double_max_sources$max_sources <- 8.0
  expect_error(
    tempest:::tempest_research_workspace_restore(double_max_sources),
    class = "tempest_research_workspace_restore_error"
  )
  snapshot$schema_version <- 4L
  expect_error(
    tempest:::tempest_research_workspace_restore(snapshot),
    class = "tempest_unsupported_format_error"
  )
})

test_that("ResearchWorkspace restore rejects orphan evidence records", {
  workspace <- tempest_research_workspace(max_sources = 8L)
  source <- test_typed_web_resource(
    "https://example.com/orphan-integrity",
    title = "Integrity source",
    content = "Evidence remains linked."
  )
  workspace$upsert_retrieved_resource(source)
  span_id <- workspace$add_evidence_span(tempest_evidence_span(
    evidence_span_id = "span-integrity",
    source_id = source@resource_id,
    quote = "Evidence remains linked."
  ))
  claim_id <- workspace$add_proposed_claim(tempest_claim(
    claim_id = "claim-integrity",
    claim_text = "Evidence records remain linked.",
    source_ids = source@resource_id,
    evidence_span_ids = span_id,
    supporting_quotes = list("Evidence remains linked.")
  ))
  workspace$add_dispute(tempest_dispute(
    dispute_id = "dispute-integrity",
    topic = "Evidence integrity",
    claim_ids = claim_id,
    evidence_balance = "mixed"
  ))
  snapshot <- tempest:::tempest_research_workspace_snapshot(workspace)

  orphan_span <- snapshot
  orphan_span$evidence_spans[[1L]]$source_id <- "resource.unknown"
  expect_error(
    tempest:::tempest_research_workspace_restore(orphan_span),
    class = "tempest_research_workspace_restore_error"
  )

  orphan_claim <- snapshot
  orphan_claim$proposed_claims[[1L]]$source_ids <- list("resource.unknown")
  expect_error(
    tempest:::tempest_research_workspace_restore(orphan_claim),
    class = "tempest_research_workspace_restore_error"
  )

  orphan_dispute <- snapshot
  orphan_dispute$disputes[[1L]]$claim_ids <- list("claim.unknown")
  expect_error(
    tempest:::tempest_research_workspace_restore(orphan_dispute),
    class = "tempest_research_workspace_restore_error"
  )
})
