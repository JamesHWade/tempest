test_that("workspace schema 5 round-trips authoritative claim supports", {
  fixture <- test_verified_workspace()
  snapshot <- tempest:::tempest_research_workspace_snapshot(
    fixture$workspace
  )

  expect_identical(snapshot$schema_version, 5L)
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
  expect_length(snapshot$claim_supports, 1L)
  expect_null(snapshot$citation_audit)

  restored <- tempest:::tempest_research_workspace_restore(snapshot)

  expect_identical(
    tempest:::tempest_research_workspace_snapshot(restored),
    snapshot
  )
  expect_identical(
    tempest:::tempest_claim_supports_resolved(restored),
    tempest:::tempest_claim_supports_resolved(fixture$workspace)
  )
  expect_identical(restored$citation_audit, fixture$workspace$citation_audit)
})

test_that("workspace JSON round-trips exact boundary support scores", {
  fixture <- test_verified_workspace(scores = 1)
  snapshot <- tempest:::tempest_research_workspace_snapshot(
    fixture$workspace
  )
  path <- withr::local_tempfile(fileext = ".json")
  tempest:::tempest_product_write_json(path, snapshot)
  encoded <- tempest:::tempest_product_read_json(path)

  restored <- tempest:::tempest_research_workspace_restore(encoded)

  claim <- restored$list_proposed_claims()[[1]]
  expect_identical(claim@support_score, 1)
  expect_identical(
    tempest:::tempest_research_workspace_snapshot(restored),
    snapshot
  )
})

test_that("workspace restore rejects every aggregate and old schema shape", {
  snapshot <- tempest:::tempest_research_workspace_snapshot(
    test_verified_workspace()$workspace
  )

  old_schema <- snapshot
  old_schema$schema_version <- 4L
  expect_error(
    tempest:::tempest_research_workspace_restore(old_schema),
    class = "tempest_unsupported_format_error"
  )

  missing_supports <- snapshot
  missing_supports$claim_supports <- NULL
  expect_error(
    tempest:::tempest_research_workspace_restore(missing_supports),
    class = "tempest_unsupported_format_error"
  )

  for (field in c(
    "retrieved_resources",
    "proposed_claims",
    "evidence_spans",
    "claim_supports",
    "disputes"
  )) {
    literal_null <- snapshot
    literal_null[field] <- list(NULL)
    expect_error(
      tempest:::tempest_research_workspace_restore(literal_null),
      class = "tempest_research_workspace_restore_error",
      info = field
    )
  }

  null_references <- snapshot
  null_references["accepted_graft_references"] <- list(NULL)
  expect_error(
    tempest:::tempest_research_workspace_restore(null_references),
    class = "tempest_research_workspace_restore_error"
  )

  aggregate_shape <- snapshot
  aggregate_shape$citation_audit <- list()
  expect_error(
    tempest:::tempest_research_workspace_restore(aggregate_shape),
    class = "tempest_unsupported_format_error"
  )
})

test_that("workspace restore validates support identity and derived summaries", {
  snapshot <- tempest:::tempest_research_workspace_snapshot(
    test_verified_workspace()$workspace
  )

  forged_id <- snapshot
  forged_id$claim_supports[[1]]$claim_support_id <- paste0(
    "sha256:",
    strrep("f", 64L)
  )
  expect_error(
    tempest:::tempest_research_workspace_restore(forged_id),
    class = "tempest_research_workspace_restore_error"
  )

  reversed_fields <- snapshot$claim_supports[[1]][
    rev(names(snapshot$claim_supports[[1]]))
  ]
  expect_error(
    tempest:::tempest_claim_support_from_list(reversed_fields),
    class = "tempest_claim_support_error"
  )
  reversed_record <- snapshot
  reversed_record$claim_supports[[1]] <- reversed_fields
  expect_error(
    tempest:::tempest_research_workspace_restore(reversed_record),
    class = "tempest_research_workspace_restore_error"
  )

  forged_summary <- snapshot
  forged_summary$proposed_claims[[1]]$support_score <- 0.8
  expect_error(
    tempest:::tempest_research_workspace_restore(forged_summary),
    class = "tempest_research_workspace_restore_error"
  )
})

test_that("workspace schema 5 and every fixed row preserve writer order", {
  workspace <- tempest_research_workspace()
  fixture <- test_add_verifiable_claim(workspace)
  workspace$add_dispute(tempest_dispute(
    topic = "Exact workspace row order",
    claim_ids = fixture$claim@claim_id,
    dispute_id = "dispute.workspace-order",
    created_at = "2026-08-16T12:02:30Z"
  ))
  support <- test_claim_support(fixture$claim, fixture$span)
  workspace$verify_proposed_claims_batch(
    list(support),
    verified_at = "2026-08-16T12:03:00Z",
    verifier = "judge.fixture"
  )
  snapshot <- tempest:::tempest_research_workspace_snapshot(workspace)

  reversed_envelope <- snapshot[rev(names(snapshot))]
  expect_error(
    tempest:::tempest_research_workspace_restore(reversed_envelope),
    class = "tempest_research_workspace_restore_error"
  )

  decoders <- list(
    retrieved_resources = tempest:::tempest_resource_from_data,
    proposed_claims = tempest:::tempest_claim_from_list,
    evidence_spans = tempest:::tempest_evidence_span_from_list,
    claim_supports = tempest:::tempest_claim_support_from_list,
    disputes = tempest:::tempest_dispute_from_list
  )
  for (field in names(decoders)) {
    reversed_row <- snapshot[[field]][[1]][
      rev(names(snapshot[[field]][[1]]))
    ]
    expect_error(decoders[[field]](reversed_row), info = field)

    reversed_snapshot <- snapshot
    reversed_snapshot[[field]][[1]] <- reversed_row
    expect_error(
      tempest:::tempest_research_workspace_restore(reversed_snapshot),
      class = "tempest_research_workspace_restore_error",
      info = field
    )
  }
})

test_that("failed support restore leaves an existing workspace unchanged", {
  target <- test_verified_workspace(
    statuses = "unsupported",
    scores = 0.1
  )$workspace
  before <- tempest:::tempest_research_workspace_snapshot(target)
  replacement <- tempest:::tempest_research_workspace_snapshot(
    test_verified_workspace()$workspace
  )
  replacement$claim_supports[[1]]$support_score <- 0.1

  expect_error(
    tempest:::tempest_research_workspace_restore(
      replacement,
      workspace = target
    ),
    class = "tempest_research_workspace_restore_error"
  )
  expect_identical(
    tempest:::tempest_research_workspace_snapshot(target),
    before
  )
})

test_that("verification records bind pair digest and exact threshold", {
  workspace <- test_verified_workspace()$workspace
  support <- workspace$list_claim_supports()[[1]]
  claim <- workspace$get_proposed_claim(support@claim_id)
  span <- workspace$get_evidence_span(support@evidence_span_id)
  running <- tempest:::tempest_stage_record_start(
    "verify_claim_support",
    paste0("sha256:", strrep("a", 64L)),
    trace_references = list(
      min_support_score = "0.7",
      verified_at = "2026-08-16T12:03:00Z",
      verifier_model = "judge.fixture"
    ),
    attempt_id = "attempt-pair-persistence",
    started_at = "2026-08-16T12:10:00Z"
  )
  record <- tempest:::tempest_stage_record_succeed(
    running,
    tempest:::tempest_stage_output_reference(
      "claim_supports",
      support@claim_support_id,
      content_digest = tempest:::tempest_stage_verification_output_digest(
        support,
        running,
        claim,
        span,
        workspace
      )
    ),
    support_status = "verified",
    completed_at = "2026-08-16T12:11:00Z"
  )

  expect_no_error(tempest:::tempest_stage_records_validate_workspace(
    list(record),
    workspace,
    min_support_score = 0.7
  ))

  threshold_trace <- record@trace_references
  threshold_trace$min_support_score <- "0.8"
  wrong_threshold <- S7::set_props(
    record,
    trace_references = threshold_trace
  )
  expect_error(
    tempest:::tempest_stage_records_validate_workspace(
      list(wrong_threshold),
      workspace,
      min_support_score = 0.7
    ),
    class = "tempest_stage_record_error"
  )

  altered <- tempest:::tempest_research_workspace_snapshot(workspace)
  altered$claim_supports[[1]]$rationale <- "A different exact assessment."
  altered <- tempest:::tempest_research_workspace_restore(altered)
  expect_error(
    tempest:::tempest_stage_records_validate_workspace(
      list(record),
      altered,
      min_support_score = 0.7
    ),
    class = "tempest_stage_record_error"
  )

  expect_error(
    tempest:::tempest_stage_output_reference(
      "citation_audit",
      support@claim_support_id
    ),
    class = "tempest_stage_record_error"
  )
})

test_that("persisted pair status must be normalized at the traced threshold", {
  workspace <- test_verified_workspace(
    statuses = "supported",
    scores = 0.6,
    min_support_score = 0
  )$workspace
  support <- workspace$list_claim_supports()[[1]]
  claim <- workspace$get_proposed_claim(support@claim_id)
  span <- workspace$get_evidence_span(support@evidence_span_id)
  running <- tempest:::tempest_stage_record_start(
    "verify_claim_support",
    paste0("sha256:", strrep("a", 64L)),
    trace_references = list(
      min_support_score = "0.7",
      verified_at = claim@verified_at,
      verifier_model = claim@verifier_model
    ),
    attempt_id = "attempt-impossible-support-threshold",
    started_at = "2026-08-16T12:10:00Z"
  )
  record <- tempest:::tempest_stage_record_succeed(
    running,
    tempest:::tempest_stage_output_reference(
      "claim_supports",
      support@claim_support_id,
      content_digest = tempest:::tempest_stage_verification_output_digest(
        support,
        running,
        claim,
        span,
        workspace
      )
    ),
    support_status = "unsupported",
    completed_at = "2026-08-16T12:11:00Z"
  )

  expect_error(
    tempest:::tempest_stage_records_validate_workspace(
      list(record),
      workspace,
      min_support_score = 0.7
    ),
    class = "tempest_stage_record_error"
  )
})

test_that("verification batch time cannot postdate its stage attempts", {
  workspace <- test_verified_workspace(
    verified_at = "2099-01-01T00:00:00Z"
  )$workspace
  support <- workspace$list_claim_supports()[[1]]
  claim <- workspace$get_proposed_claim(support@claim_id)
  span <- workspace$get_evidence_span(support@evidence_span_id)
  running <- tempest:::tempest_stage_record_start(
    "verify_claim_support",
    paste0("sha256:", strrep("a", 64L)),
    trace_references = list(
      min_support_score = "0.7",
      verified_at = claim@verified_at,
      verifier_model = claim@verifier_model
    ),
    attempt_id = "attempt-future-verification-batch",
    started_at = "2026-08-16T12:10:00Z"
  )
  record <- tempest:::tempest_stage_record_succeed(
    running,
    tempest:::tempest_stage_output_reference(
      "claim_supports",
      support@claim_support_id,
      content_digest = tempest:::tempest_stage_verification_output_digest(
        support,
        running,
        claim,
        span,
        workspace
      )
    ),
    support_status = "verified",
    completed_at = "2026-08-16T12:11:00Z"
  )

  expect_error(
    tempest:::tempest_stage_records_validate_workspace(
      list(record),
      workspace,
      min_support_score = 0.7
    ),
    class = "tempest_stage_record_error"
  )
})

test_that("stage output digests use only schema 3", {
  running <- tempest:::tempest_stage_record_start(
    "query_decomposition",
    paste0("sha256:", strrep("a", 64L)),
    attempt_id = "attempt-output-digest-v3",
    started_at = "2026-08-16T12:10:00Z"
  )
  records <- list(value = "exact output")
  current <- tempest:::tempest_stage_record_output_digest(
    records,
    running,
    "content_digest"
  )
  payload <- list(
    schema_version = 3L,
    stage = running@stage,
    attempt_id = running@attempt_id,
    program_artifact_id = running@program_artifact_id,
    trace_references = running@trace_references,
    record_kind = "content_digest",
    records = records
  )
  expected <- tempest:::tempest_stage_content_digest_id(payload)
  future <- payload
  future$schema_version <- 4L
  future <- tempest:::tempest_stage_content_digest_id(future)

  expect_named(
    payload,
    c(
      "schema_version",
      "stage",
      "attempt_id",
      "program_artifact_id",
      "trace_references",
      "record_kind",
      "records"
    )
  )
  expect_identical(
    expected,
    paste0(
      "sha256:",
      "c27b08986158557201023543aa1031375babc40127e1ff735f520ec7627afbd2"
    )
  )
  expect_identical(current, expected)
  expect_identical(identical(current, future), FALSE)
})
