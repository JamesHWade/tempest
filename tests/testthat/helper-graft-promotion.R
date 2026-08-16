test_promotion_fixture <- function() {
  run_id <- "research-promotion-1"
  program_set <- test_program_set()
  programs <- tempest:::tempest_program_set_manifest_programs(program_set)
  resource <- tempest_resource(
    resource_kind = "scientific.document",
    locator = "document:promotion-1",
    title = "Promotion evidence",
    media_type = "text/plain",
    resource_id = "source-promotion-1",
    content = "The intervention improved the measured outcome.",
    retrieved_at = "2026-08-16T12:00:00Z"
  )
  workspace <- tempest_research_workspace()
  workspace$upsert_retrieved_resource(resource)
  span <- tempest:::tempest_evidence_span(
    source_id = resource@resource_id,
    quote = "The intervention improved the measured outcome.",
    chunk_id = "chunk-promotion-1",
    start_offset = 0L,
    end_offset = 47L,
    relevance_score = 0.95,
    extracted_by = programs$extract_claims$program_artifact_id,
    evidence_span_id = "evidence-promotion-1",
    created_at = "2026-08-16T12:01:00Z"
  )
  claim <- tempest:::tempest_claim(
    claim_text = "The intervention improved the measured outcome.",
    source_ids = resource@resource_id,
    evidence_span_ids = span@evidence_span_id,
    supporting_quotes = list(span@quote),
    claim_type = "finding",
    confidence = "high",
    claim_id = "claim-promotion-1",
    created_at = "2026-08-16T12:02:00Z"
  )
  workspace$add_extracted_claim_batch(list(claim), list(span))
  support <- tempest_claim_support(
    claim_id = claim@claim_id,
    evidence_span_id = span@evidence_span_id,
    source_id = resource@resource_id,
    verification_status = "supported",
    support_score = 0.95,
    rationale = "The exact source excerpt directly states the claim."
  )
  workspace$verify_proposed_claims_batch(
    list(support),
    verified_at = "2026-08-16T12:03:00Z",
    min_support_score = 0.7,
    verifier = "verifier-promotion-1"
  )

  manifest <- tempest_research_manifest(
    research_run_id = run_id,
    mode = "storm",
    config = tempest_config(),
    programs = programs,
    traces = list(
      list(
        trace_id = "attempt-promotion-extraction",
        stage = "extract_claims"
      ),
      list(
        trace_id = "attempt-promotion-verification",
        stage = "verify_claim_support"
      )
    ),
    status = "succeeded"
  )
  trace <- list(research_run_id = run_id, mode = "storm", role = "program")
  extraction <- tempest:::tempest_stage_record_start(
    "extract_claims",
    programs$extract_claims$program_artifact_id,
    programs$extract_claims$governed_procedure_ref$revision_id,
    trace_references = trace,
    attempt_id = "attempt-promotion-extraction",
    started_at = "2026-08-16T12:03:00Z"
  )
  exact_claim <- workspace$get_proposed_claim(claim@claim_id)
  extraction <- tempest:::tempest_stage_record_succeed(
    extraction,
    tempest:::tempest_stage_output_reference(
      "workspace_claims",
      claim@claim_id,
      content_digest = tempest:::tempest_stage_claims_output_digest(
        list(exact_claim),
        extraction,
        list(span)
      )
    ),
    support_status = "unknown",
    completed_at = "2026-08-16T12:04:00Z"
  )
  verification <- tempest:::tempest_stage_record_start(
    "verify_claim_support",
    programs$verify_claim_support$program_artifact_id,
    programs$verify_claim_support$governed_procedure_ref$revision_id,
    trace_references = c(
      trace,
      list(
        min_support_score = tempest:::tempest_stage_support_threshold_string(
          0.7
        ),
        verified_at = "2026-08-16T12:03:00Z",
        verifier_model = "verifier-promotion-1"
      )
    ),
    attempt_id = "attempt-promotion-verification",
    started_at = "2026-08-16T12:05:00Z"
  )
  verification <- tempest:::tempest_stage_record_succeed(
    verification,
    tempest:::tempest_stage_output_reference(
      "claim_supports",
      support@claim_support_id,
      content_digest = tempest:::tempest_stage_verification_output_digest(
        support,
        verification,
        exact_claim,
        span,
        workspace
      )
    ),
    support_status = "verified",
    completed_at = "2026-08-16T12:06:00Z"
  )
  stage_records <- list(extraction, verification)
  list(
    workspace = workspace,
    manifest = manifest,
    stage_records = stage_records,
    claim = exact_claim,
    span = span,
    resource = resource,
    support = support,
    programs = programs
  )
}

test_promotion_bundle <- function() {
  fixture <- test_promotion_fixture()
  fixture$bundle <- tempest_promotion_bundle(
    fixture$workspace,
    fixture$manifest,
    fixture$stage_records
  )
  fixture
}

test_promotion_store <- function() {
  graft::graft_open(
    tempest_graft_schema(),
    path = ":memory:",
    okf = "disabled"
  )
}

test_promotion_resign_data <- function(data) {
  payload <- data[setdiff(names(data), "bundle_id")]
  data$bundle_id <- tempest:::tempest_promotion_digest(payload)
  data
}

test_promotion_rewrite_saved <- function(path, data) {
  data <- test_promotion_resign_data(data)
  bundle_path <- file.path(path, "bundle.json")
  tempest:::tempest_promotion_write_json(bundle_path, data)
  manifest <- tempest:::tempest_promotion_manifest_core(
    data$bundle_id,
    tempest:::tempest_promotion_file_checksum(bundle_path)
  )
  manifest$manifest_digest <- tempest:::tempest_promotion_digest(manifest)
  tempest:::tempest_promotion_write_json(
    file.path(path, "manifest.json"),
    manifest
  )
  invisible(data)
}
