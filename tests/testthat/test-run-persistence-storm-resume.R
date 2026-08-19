test_that("STORM resume accepts only an equivalent supplied workspace", {
  skip_if_not_installed("graft")
  dir <- withr::local_tempdir()
  cfg <- tempest_config()
  program_set <- tempest_program_set()
  knowledge <- test_knowledge_view()
  workspace <- tempest_research_workspace(
    graft_snapshot = knowledge$snapshot,
    max_sources = 8L,
    accepted_graft_references = list(
      list(record_id = "accepted-a", revision_id = "revision-a")
    )
  )
  source <- tempest:::tempest_source(
    "https://example.com/authoritative",
    title = "Authoritative source",
    snippet = "Persisted source",
    content_text = "Persisted source. This span was not persisted."
  )
  resource <- tempest_resource(
    resource_kind = "scientific.document",
    locator = "protocols/authoritative",
    title = "Authoritative protocol",
    media_type = "text/plain",
    resource_id = "resource.authoritative",
    content = "The persisted protocol is authoritative.",
    retrieved_at = "2026-08-15T14:00:00Z",
    metadata = list(revision_id = "protocol-a")
  )
  workspace$upsert_retrieved_resource(source)
  workspace$upsert_retrieved_resource(resource)
  claim <- tempest_claim(
    claim_id = "claim-authoritative",
    claim_text = "Persisted state is authoritative.",
    source_ids = source$id,
    evidence_span_ids = "span-authoritative",
    supporting_quotes = list("Persisted source."),
    confidence = "high"
  )
  workspace$add_extracted_claim_batch(
    list(claim),
    list(tempest_evidence_span(
      source_id = source$id,
      quote = "Persisted source.",
      evidence_span_id = "span-authoritative",
      extracted_by = tempest:::tempest_program_set_manifest_programs(
        program_set
      )$extract_claims$program_artifact_id
    ))
  )
  workspace$verify_proposed_claims_batch(
    list(tempest_claim_support(
      claim_id = claim@claim_id,
      evidence_span_id = "span-authoritative",
      source_id = source$id,
      verification_status = "supported",
      support_score = 0.9,
      rationale = "The persisted source directly supports the claim."
    )),
    verified_at = "2026-08-16T00:00:00Z"
  )
  manifest <- tempest_research_manifest(
    "authoritative-workspace",
    config = cfg,
    programs = tempest:::tempest_program_set_manifest_programs(program_set),
    knowledge_snapshot = tempest:::tempest_snapshot_reference(
      knowledge$snapshot
    )
  )
  state <- tempest:::tempest_storm_state(
    "Authoritative workspace",
    completed_stages = "research"
  )
  bound <- test_persistence_bind_storm_records(state, workspace, manifest)
  state <- bound$state
  manifest <- bound$manifest
  tempest:::tempest_storm_save_artifacts(
    dir,
    workspace,
    state,
    manifest,
    program_set = program_set,
    config = cfg,
    steps = "research",
    research_strategy = "key_questions"
  )

  snapshot <- tempest:::tempest_research_workspace_snapshot(workspace)
  persisted_record <-
    tempest:::tempest_storm_workspace_equivalence_record(workspace)
  expect_identical(
    tempest:::tempest_storm_workspace_is_empty(workspace),
    FALSE
  )
  equivalent <- tempest:::tempest_research_workspace_restore(
    snapshot,
    graft_snapshot = knowledge$snapshot
  )
  loaded <- tempest:::tempest_storm_load_artifacts(
    dir,
    workspace = equivalent,
    config = cfg,
    program_set = program_set,
    run_id = "authoritative-workspace"
  )
  expect_identical(loaded$workspace, equivalent)
  expect_identical(
    tempest:::tempest_storm_workspace_equivalence_record(loaded$workspace),
    persisted_record
  )

  empty <- tempest_research_workspace(
    graft_snapshot = knowledge$snapshot,
    max_sources = 2L
  )
  loaded_empty <- tempest:::tempest_storm_load_artifacts(
    dir,
    workspace = empty,
    config = cfg,
    program_set = program_set,
    run_id = "authoritative-workspace"
  )
  expect_identical(loaded_empty$workspace, empty)
  expect_identical(
    tempest:::tempest_storm_workspace_equivalence_record(empty),
    persisted_record
  )

  resource_ids <- vapply(
    snapshot$retrieved_resources,
    `[[`,
    character(1),
    "resource_id"
  )
  source_index <- match(source$id, resource_ids)
  resource_index <- match(resource@resource_id, resource_ids)

  extra_source <- tempest:::tempest_source(
    "https://example.com/extra",
    title = "Extra source"
  )
  extra_source_record <- snapshot
  extra_source_record$retrieved_resources <- c(
    extra_source_record$retrieved_resources,
    list(tempest:::tempest_resource_record(
      tempest:::tempest_source_as_resource(extra_source)
    ))
  )
  extra_source_record$retrieved_resources <-
    extra_source_record$retrieved_resources[
      order(vapply(
        extra_source_record$retrieved_resources,
        `[[`,
        character(1),
        "resource_id"
      ))
    ]

  changed_source_record <- snapshot
  changed_source_record$retrieved_resources[[source_index]]$title <-
    "Changed source"
  changed_source_record$retrieved_resources[[source_index]]$fingerprint <-
    tempest:::tempest_resource_fingerprint(
      changed_source_record$retrieved_resources[[source_index]]
    )

  changed_source_metadata_record <- snapshot
  changed_source_metadata_record$retrieved_resources[[source_index]]$metadata <-
    list(revision_id = "changed")
  changed_source_metadata_record$retrieved_resources[[
    source_index
  ]]$fingerprint <-
    tempest:::tempest_resource_fingerprint(
      changed_source_metadata_record$retrieved_resources[[source_index]]
    )

  changed_source_timestamp_record <- snapshot
  changed_source_timestamp_record$retrieved_resources[[
    source_index
  ]]$retrieved_at <-
    "2026-08-15T15:00:00Z"
  changed_source_timestamp_record$retrieved_resources[[
    source_index
  ]]$fingerprint <-
    tempest:::tempest_resource_fingerprint(
      changed_source_timestamp_record$retrieved_resources[[source_index]]
    )

  changed_resource_metadata_record <- snapshot
  changed_resource_metadata_record$retrieved_resources[[
    resource_index
  ]]$metadata <-
    list(revision_id = "protocol-b")
  changed_resource_metadata_record$retrieved_resources[[
    resource_index
  ]]$fingerprint <-
    tempest:::tempest_resource_fingerprint(
      changed_resource_metadata_record$retrieved_resources[[resource_index]]
    )

  changed_resource_timestamp_record <- snapshot
  changed_resource_timestamp_record$retrieved_resources[[
    resource_index
  ]]$retrieved_at <-
    "2026-08-15T16:00:00Z"
  changed_resource_timestamp_record$retrieved_resources[[
    resource_index
  ]]$fingerprint <-
    tempest:::tempest_resource_fingerprint(
      changed_resource_timestamp_record$retrieved_resources[[resource_index]]
    )

  extra_claim_record <- snapshot
  extra_claim_record$proposed_claims <- c(
    extra_claim_record$proposed_claims,
    list(tempest:::tempest_research_workspace_claim_record(tempest_claim(
      claim_id = "claim-extra",
      claim_text = "This claim was not persisted.",
      source_ids = source$id
    )))
  )

  extra_span_record <- snapshot
  extra_span_record$evidence_spans <- c(
    extra_span_record$evidence_spans,
    list(tempest:::tempest_evidence_span_to_list(tempest_evidence_span(
      evidence_span_id = "span-extra",
      source_id = source$id,
      quote = "This span was not persisted."
    )))
  )

  extra_dispute_record <- snapshot
  extra_dispute_record$disputes <- c(
    extra_dispute_record$disputes,
    list(tempest:::tempest_research_workspace_dispute_record(
      tempest_dispute(
        dispute_id = "dispute-extra",
        topic = "Unpersisted dispute",
        claim_ids = claim@claim_id
      )
    ))
  )

  extra_reference_record <- snapshot
  extra_reference_record$accepted_graft_references <- c(
    extra_reference_record$accepted_graft_references,
    list(list(
      record_id = "accepted-extra",
      revision_id = "revision-extra"
    ))
  )

  changed_supports_snapshot <- snapshot
  changed_supports_snapshot$claim_supports[[1]]$rationale <-
    "A different exact support rationale."
  changed_supports <- tempest:::tempest_research_workspace_restore(
    changed_supports_snapshot,
    graft_snapshot = knowledge$snapshot
  )
  divergent_records <- list(
    extra_source = extra_source_record,
    changed_source = changed_source_record,
    changed_source_metadata = changed_source_metadata_record,
    changed_source_timestamp = changed_source_timestamp_record,
    changed_resource_metadata = changed_resource_metadata_record,
    changed_resource_timestamp = changed_resource_timestamp_record,
    extra_claim = extra_claim_record,
    extra_span = extra_span_record,
    extra_dispute = extra_dispute_record,
    extra_reference = extra_reference_record,
    changed_supports = changed_supports_snapshot
  )

  expect_error(
    tempest:::tempest_storm_load_artifacts(
      dir,
      workspace = changed_supports,
      config = cfg,
      program_set = program_set,
      run_id = "authoritative-workspace"
    ),
    class = "tempest_run_restore_error"
  )

  supplied_marker <- new.env(parent = emptyenv())
  persisted_marker <- new.env(parent = emptyenv())
  supplied_marker$graft_snapshot <- NULL
  persisted_marker$graft_snapshot <- NULL
  candidate_record <- NULL
  local_mocked_bindings(
    tempest_storm_workspace_equivalence_record = function(workspace) {
      if (identical(workspace, supplied_marker)) {
        return(candidate_record)
      }
      if (identical(workspace, persisted_marker)) {
        return(persisted_record)
      }
      stop("Unexpected workspace marker.")
    }
  )
  for (divergence in names(divergent_records)) {
    candidate_record <- divergent_records[[divergence]]
    expect_error(
      tempest:::tempest_storm_assert_workspace_equivalent(
        supplied_marker,
        persisted_marker
      ),
      class = "tempest_run_restore_error",
      info = paste("Divergence type:", divergence)
    )
  }
})
