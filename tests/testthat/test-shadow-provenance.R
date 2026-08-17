test_that("shadow provenance joins all four package boundaries observationally", {
  skip_if_not_installed("deputy")
  skip_if_not_installed("dsprrr")
  skip_if_not_installed("graft")
  fixture <- test_shadow_provenance_fixture()
  workspace_before <- list(
    claims = fixture$workspace$list_proposed_claims(),
    spans = fixture$workspace$list_evidence_spans(),
    supports = fixture$workspace$list_claim_supports()
  )
  manifest_before <- tempest_research_manifest_record(fixture$manifest)
  stages_before <- tempest:::tempest_stage_records_data(
    fixture$stage_records
  )

  projection <- tempest:::tempest_claim_provenance_projection(
    fixture$manifest,
    fixture$stage_records,
    fixture$workspace
  )

  expect_named(
    projection,
    c(
      "schema_version",
      "binding_scope",
      "research_run_id",
      "mode",
      "config_digest",
      "knowledge_snapshot",
      "programs",
      "deputy_runs",
      "claim_pairs"
    )
  )
  expect_identical(projection$schema_version, 1L)
  expect_identical(projection$binding_scope, "execution_identity")
  expect_identical(projection$research_run_id, fixture$run_id)
  expect_identical(
    projection$config_digest,
    fixture$manifest@config_digest
  )
  expect_identical(
    projection$knowledge_snapshot,
    tempest:::tempest_snapshot_reference(fixture$knowledge$snapshot)
  )
  expect_identical(
    names(projection$programs),
    c("extract_claims", "verify_claim_support")
  )
  expect_identical(
    projection$programs$extract_claims$program_artifact_id,
    fixture$programs$extract_claims$program_artifact_id
  )
  expect_identical(
    projection$programs$verify_claim_support$program_artifact_id,
    fixture$programs$verify_claim_support$program_artifact_id
  )
  expect_setequal(
    vapply(
      projection$deputy_runs,
      \(trace) trace$deputy_run_id,
      character(1)
    ),
    c(fixture$deputy$parent$run_id, fixture$deputy$child$run_id)
  )
  delegated <- Filter(
    \(trace) identical(trace$trace_type, "deputy_delegation"),
    projection$deputy_runs
  )[[1L]]
  expect_identical(delegated$parent_run_id, fixture$deputy$parent$run_id)
  expect_identical(
    delegated$delegation_id,
    fixture$deputy$child$delegation_id
  )
  expect_identical(
    delegated$tool_call_id,
    fixture$deputy$parent_call$tool_call_id
  )

  support <- fixture$workspace$list_claim_supports()[[1L]]
  pair <- projection$claim_pairs[[1L]]
  expect_identical(pair$claim_support_id, support@claim_support_id)
  expect_identical(pair$claim_id, support@claim_id)
  expect_identical(pair$evidence_span_id, support@evidence_span_id)
  expect_identical(pair$source_id, support@source_id)
  expect_identical(pair$verification_status, "supported")
  expect_identical(pair$support_score, 0.95)
  expect_identical(
    pair$extraction$deputy_run_id,
    fixture$deputy$child$run_id
  )
  expect_identical(
    pair$extraction$deputy_session_id,
    fixture$deputy$child$session_id
  )
  expect_identical(
    pair$verification$program_artifact_id,
    fixture$programs$verify_claim_support$program_artifact_id
  )
  expect_identical(pair$verification$min_support_score, "0.7")
  expect_identical(pair$verification$verifier_model, "test::shadow-verifier")

  encoded <- as.character(jsonlite::toJSON(
    projection,
    auto_unbox = TRUE,
    null = "null"
  ))
  expect_no_match(encoded, fixture$source$content_text, fixed = TRUE)
  decoded <- jsonlite::fromJSON(encoded, simplifyVector = FALSE)
  expect_identical(
    as.character(jsonlite::toJSON(
      decoded,
      auto_unbox = TRUE,
      null = "null"
    )),
    encoded
  )
  expect_identical(
    tempest:::tempest_claim_provenance_projection(
      fixture$manifest,
      fixture$stage_records,
      fixture$workspace
    ),
    projection
  )
  expect_identical(
    list(
      claims = fixture$workspace$list_proposed_claims(),
      spans = fixture$workspace$list_evidence_spans(),
      supports = fixture$workspace$list_claim_supports()
    ),
    workspace_before
  )
  expect_identical(
    tempest_research_manifest_record(fixture$manifest),
    manifest_before
  )
  expect_identical(
    tempest:::tempest_stage_records_data(fixture$stage_records),
    stages_before
  )
})

test_that("shadow provenance rejects cross-paired Deputy identities", {
  skip_if_not_installed("deputy")
  fixture <- test_shadow_provenance_fixture()
  expect_identical(
    identical(
      fixture$deputy$parent$session_id,
      fixture$deputy$child$session_id
    ),
    FALSE
  )
  cross_paired <- test_shadow_rebind_extraction_session(
    fixture,
    fixture$deputy$parent$session_id
  )

  expect_error(
    tempest:::tempest_claim_provenance_projection(
      fixture$manifest,
      cross_paired,
      fixture$workspace
    ),
    class = "tempest_claim_provenance_error"
  )
})

test_that("shadow provenance cannot pass an empty slice vacuously", {
  skip_if_not_installed("graft")
  knowledge <- test_knowledge_view()
  workspace <- tempest_research_workspace(graft_snapshot = knowledge$snapshot)
  program_set <- tempest_program_set()
  manifest <- tempest_research_manifest(
    research_run_id = "research-shadow-empty",
    mode = "costorm",
    config = tempest_config(),
    programs = tempest:::tempest_program_set_manifest_programs(program_set),
    knowledge_snapshot = tempest:::tempest_snapshot_reference(
      knowledge$snapshot
    )
  )

  expect_error(
    tempest:::tempest_claim_provenance_projection(
      manifest,
      list(),
      workspace
    ),
    class = "tempest_claim_provenance_error"
  )
})

test_that("shadow provenance encodes an unverifiable score as null", {
  skip_if_not_installed("deputy")
  fixture <- test_shadow_provenance_fixture(
    verifier_status = "unverifiable",
    verifier_score = NA_real_
  )

  projection <- tempest:::tempest_claim_provenance_projection(
    fixture$manifest,
    fixture$stage_records,
    fixture$workspace
  )

  expect_identical(
    projection$claim_pairs[[1L]]$verification_status,
    "unverifiable"
  )
  expect_null(projection$claim_pairs[[1L]]$support_score)
  expect_identical(
    projection$claim_pairs[[1L]]$verification$support_status,
    "unknown"
  )
})

test_that("shadow provenance rejects tuple, snapshot, program, and digest tampering", {
  skip_if_not_installed("deputy")
  fixture <- test_shadow_provenance_fixture()

  stages <- vapply(
    fixture$stage_records,
    \(record) record@stage,
    character(1)
  )
  expect_error(
    tempest:::tempest_claim_provenance_projection(
      fixture$manifest,
      fixture$stage_records[stages != "verify_claim_support"],
      fixture$workspace
    ),
    class = "tempest_claim_provenance_error"
  )

  manifest_data <- tempest_research_manifest_record(fixture$manifest)
  child_index <- which(vapply(
    manifest_data$traces,
    \(trace) identical(trace$trace_type, "deputy_delegation"),
    logical(1)
  ))
  tuple_data <- manifest_data
  tuple_data$traces[[child_index]]$tool_call_id <- NULL
  tuple_manifest <- tempest:::tempest_research_manifest_from_record(tuple_data)
  expect_error(
    tempest:::tempest_claim_provenance_projection(
      tuple_manifest,
      fixture$stage_records,
      fixture$workspace
    ),
    class = "tempest_claim_provenance_error"
  )

  parent_index <- which(vapply(
    manifest_data$traces,
    \(trace) identical(trace$trace_type, "deputy_run"),
    logical(1)
  ))
  correlation_data <- manifest_data
  correlation_data$traces[[parent_index]]$correlation_id <-
    "correlation-spliced-parent"
  correlation_manifest <- tempest:::tempest_research_manifest_from_record(
    correlation_data
  )
  expect_error(
    tempest:::tempest_claim_provenance_projection(
      correlation_manifest,
      fixture$stage_records,
      fixture$workspace
    ),
    class = "tempest_claim_provenance_error"
  )

  snapshot_data <- manifest_data
  snapshot_data$knowledge_snapshot$snapshot_id <- "snapshot:tampered"
  snapshot_manifest <- tempest:::tempest_research_manifest_from_record(
    snapshot_data
  )
  expect_error(
    tempest:::tempest_claim_provenance_projection(
      snapshot_manifest,
      fixture$stage_records,
      fixture$workspace
    ),
    class = "tempest_claim_provenance_error"
  )

  program_data <- manifest_data
  program_data$programs$extract_claims$program_artifact_id <- paste0(
    "sha256:",
    strrep("0", 64L)
  )
  program_manifest <- tempest:::tempest_research_manifest_from_record(
    program_data
  )
  expect_error(
    tempest:::tempest_claim_provenance_projection(
      program_manifest,
      fixture$stage_records,
      fixture$workspace
    ),
    class = "tempest_claim_provenance_error"
  )

  stage_data <- tempest:::tempest_stage_records_data(fixture$stage_records)
  extraction_index <- which(vapply(
    stage_data,
    \(record) identical(record$stage, "extract_claims"),
    logical(1)
  ))
  stage_data[[extraction_index]]$output_reference$content_digest <- paste0(
    "sha256:",
    strrep("f", 64L)
  )
  digest_records <- tempest:::tempest_stage_records_from_data(
    stage_data,
    allow_running = FALSE
  )
  expect_error(
    tempest:::tempest_claim_provenance_projection(
      fixture$manifest,
      digest_records,
      fixture$workspace
    ),
    class = "tempest_claim_provenance_error"
  )
})
