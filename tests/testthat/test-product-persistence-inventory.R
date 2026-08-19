test_that("former run persistence definitions have one product owner", {
  original <- c(
    "tempest_topic_slug",
    "tempest_prepare_run_dir",
    "tempest_run_artifact_paths",
    "tempest_graft_snapshot_relative_path",
    "tempest_graft_snapshot_field_names",
    "tempest_graft_snapshot_abort",
    "tempest_graft_snapshot_validate",
    "tempest_graft_snapshot_assert_binding",
    "tempest_graft_snapshot_write",
    "tempest_graft_snapshot_read",
    "tempest_write_json",
    "tempest_persistence_error_class",
    "tempest_session_persistence_error_class",
    "tempest_unsupported_format_abort",
    "tempest_persistence_schema_version",
    "tempest_persistence_manifest_files",
    "tempest_persistence_manifest_checksums",
    "tempest_persistence_leaf_path_is_symlink",
    "tempest_persistence_bundle_path_has_symlink",
    "tempest_persistence_require_regular_bundle_files",
    "tempest_persistence_credential_audit",
    "tempest_read_json_strict",
    "tempest_env_values",
    "tempest_env_snapshot",
    "tempest_research_workspace_snapshot_abort",
    "tempest_research_workspace_max_sources_data",
    "tempest_research_workspace_array_record",
    "tempest_research_workspace_claim_record",
    "tempest_research_workspace_dispute_record",
    "tempest_research_workspace_validate_noncontent",
    "tempest_research_workspace_snapshot_fields",
    "tempest_research_workspace_snapshot",
    "tempest_research_workspace_restore_abort",
    "tempest_research_workspace_restore_schema",
    "tempest_stage_records_verification_projection",
    "tempest_stage_records_validate_workspace",
    "tempest_stage_records_validate_workspace_coverage",
    "tempest_stage_records_validate_generated_experts",
    "tempest_stage_records_validate_claim_provenance",
    "tempest_stage_records_validate_persisted_trust",
    "tempest_research_workspace_require_current_schema",
    "tempest_research_workspace_restore_max_sources",
    "tempest_research_workspace_restore_records",
    "tempest_research_workspace_record_fields",
    "tempest_research_workspace_exact_records",
    "tempest_persistence_record_string",
    "tempest_persistence_record_string_array",
    "tempest_persistence_record_number",
    "tempest_persistence_record_timestamp",
    "tempest_research_workspace_validate_claim_record",
    "tempest_research_workspace_validate_span_record",
    "tempest_research_workspace_validate_dispute_record",
    "tempest_research_workspace_unique_record_ids",
    "tempest_research_workspace_restore_metadata",
    "tempest_research_workspace_restore",
    "tempest_session_restore_abort",
    "tempest_expert_records",
    "tempest_expert_record_fields",
    "tempest_persistence_exact_records",
    "tempest_experts_from_records",
    "tempest_expert_session_record_fields",
    "tempest_expert_session_snapshot_record",
    "tempest_expert_session_records_from_json",
    "tempest_expert_sessions_snapshot",
    "tempest_session_snapshot_value_abort",
    "tempest_session_snapshot_record",
    "tempest_session_credential_free_value",
    "tempest_session_transcript_record",
    "tempest_session_mindmap_record",
    "tempest_session_mindmap_binding_abort",
    "tempest_session_mindmap_assert_binding",
    "tempest_session_portable_snapshot",
    "tempest_session_suggested_questions",
    "tempest_session_snapshot_fields",
    "tempest_session_report_record",
    "tempest_persistence_execution_review_candidates",
    "tempest_persistence_report_without_execution_review",
    "tempest_persistence_report_for_records",
    "tempest_persistence_report_reference",
    "tempest_persistence_validate_report_reference",
    "tempest_persistence_stage_manifest_traces",
    "tempest_persistence_deputy_manifest_traces",
    "tempest_persistence_expert_session_trace_bindings",
    "tempest_persistence_manifest_runtime_from_traces",
    "tempest_persistence_authoritative_extraction_attempt_ids",
    "tempest_persistence_manifest_validate_trace_ids",
    "tempest_persistence_manifest_existing_traces",
    "tempest_persistence_manifest_bind_stage_records",
    "tempest_persistence_manifest_validate_stage_records",
    "tempest_persistence_manifest_bind_report",
    "tempest_persistence_manifest_validate_report",
    "tempest_persistence_report_inline_citations",
    "tempest_persistence_validate_report_policy",
    "tempest_session_assert_no_pending_deputy_runs",
    "tempest_session_assert_persistence_quiescent",
    "tempest_session_snapshot",
    "tempest_session_restore_expert_sessions",
    "tempest_session_restore",
    "tempest_session_restore_internal",
    "tempest_session_bundle_path",
    "tempest_session_bundle_write_json",
    "tempest_session_bundle_write_text",
    "tempest_session_prepare_bundle_dir",
    "tempest_session_bundle_checksum",
    "tempest_atomic_commit_bundle",
    "tempest_session_commit_bundle",
    "tempest_run_bundle_write_json",
    "tempest_run_bundle_write_text",
    "tempest_session_save",
    "tempest_session_bundle_optional_json",
    "tempest_session_progress_event_fields",
    "tempest_session_restore_progress_events",
    "tempest_session_bundle_require_files",
    "tempest_session_bundle_optional_presentation_files",
    "tempest_session_bundle_manifest_fields",
    "tempest_session_bundle_workspace_fields",
    "tempest_session_bundle_validate_manifest",
    "tempest_session_resume",
    "tempest_session_resume_internal",
    "tempest_storm_stage_required_files",
    "tempest_storm_persistence_abort",
    "tempest_storm_record_strings",
    "tempest_storm_perspective_fields",
    "tempest_storm_outline_fields",
    "tempest_storm_outline_section_fields",
    "tempest_storm_outline_subsection_fields",
    "tempest_storm_validate_perspectives",
    "tempest_storm_validate_outline",
    "tempest_storm_reference_fields",
    "tempest_storm_validate_references",
    "tempest_storm_validate_persisted_state",
    "tempest_run_bundle_manifest_fields",
    "tempest_run_bundle_owned_files",
    "tempest_storm_require_current_schema",
    "tempest_run_bundle_validate_manifest",
    "tempest_storm_workspace_identity_record",
    "tempest_storm_snapshot_reference",
    "tempest_storm_run_restore_abort",
    "tempest_storm_program_set_abort",
    "tempest_storm_program_set_validate",
    "tempest_storm_restore_workspace",
    "tempest_storm_workspace_equivalence_record",
    "tempest_storm_workspace_is_empty",
    "tempest_storm_assert_workspace_equivalent",
    "tempest_storm_restore_manifest",
    "tempest_storm_read_state",
    "tempest_load_run_artifacts",
    "tempest_save_run_artifacts",
    "tempest_stage_complete",
    "tempest_mark_stage_complete"
  )
  owners <- rep(NA_character_, length(original))
  owners[c(11:22, 59, 104, 105)] <- "product-persistence.R"
  owners[c(25:34, 41:55)] <- "research-workspace-persistence.R"
  owners[c(56, 63, 65:75, 94:103, 106, 109:112, 114:119)] <-
    "costorm-persistence.R"
  owners[c(1:3, 107:108, 120:148)] <- "storm-persistence.R"
  owners[c(4:10, 81:91)] <- "product-authority.R"
  owners[c(76:78, 92:93)] <- "product-report.R"
  owners[c(57:58, 60:62, 64)] <- "research-expert.R"
  owners[35:40] <- "stage-record.R"
  owners[149:150] <- "storm-state.R"

  renames <- c(
    tempest_topic_slug = "tempest_storm_topic_slug",
    tempest_prepare_run_dir = "tempest_storm_prepare_run_dir",
    tempest_run_artifact_paths = "tempest_storm_artifact_paths",
    tempest_write_json = "tempest_product_write_json",
    tempest_unsupported_format_abort = "tempest_product_unsupported_format_abort",
    tempest_read_json_strict = "tempest_product_read_json",
    tempest_persistence_record_string = "tempest_research_workspace_record_string",
    tempest_persistence_record_string_array = "tempest_research_workspace_record_string_array",
    tempest_persistence_record_number = "tempest_research_workspace_record_number",
    tempest_persistence_record_timestamp = "tempest_research_workspace_record_timestamp",
    tempest_persistence_execution_review_candidates = "tempest_product_report_execution_review_candidates",
    tempest_persistence_report_without_execution_review = "tempest_product_report_without_execution_review",
    tempest_persistence_report_for_records = "tempest_product_report_for_stage_records",
    tempest_persistence_stage_manifest_traces = "tempest_product_authority_stage_manifest_traces",
    tempest_persistence_deputy_manifest_traces = "tempest_product_authority_deputy_manifest_traces",
    tempest_persistence_expert_session_trace_bindings = "tempest_product_authority_expert_session_trace_bindings",
    tempest_persistence_manifest_runtime_from_traces = "tempest_product_authority_manifest_runtime_from_traces",
    tempest_persistence_authoritative_extraction_attempt_ids = "tempest_product_authority_extraction_attempt_ids",
    tempest_persistence_manifest_validate_trace_ids = "tempest_product_authority_manifest_validate_trace_ids",
    tempest_persistence_manifest_existing_traces = "tempest_product_authority_manifest_existing_traces",
    tempest_persistence_manifest_bind_stage_records = "tempest_product_authority_bind_stage_records",
    tempest_persistence_manifest_validate_stage_records = "tempest_product_authority_validate_stage_records",
    tempest_persistence_manifest_bind_report = "tempest_product_authority_bind_report",
    tempest_persistence_manifest_validate_report = "tempest_product_authority_validate_report",
    tempest_persistence_report_inline_citations = "tempest_product_report_inline_citations",
    tempest_persistence_validate_report_policy = "tempest_product_report_validate_policy",
    tempest_session_bundle_checksum = "tempest_product_bundle_checksum",
    tempest_atomic_commit_bundle = "tempest_product_atomic_commit_bundle",
    tempest_run_bundle_write_json = "tempest_storm_bundle_write_json",
    tempest_run_bundle_write_text = "tempest_storm_bundle_write_text",
    tempest_run_bundle_manifest_fields = "tempest_storm_bundle_manifest_fields",
    tempest_run_bundle_owned_files = "tempest_storm_bundle_owned_files",
    tempest_run_bundle_validate_manifest = "tempest_storm_bundle_validate_manifest",
    tempest_load_run_artifacts = "tempest_storm_load_artifacts",
    tempest_save_run_artifacts = "tempest_storm_save_artifacts",
    tempest_stage_complete = "tempest_storm_stage_complete",
    tempest_mark_stage_complete = "tempest_storm_mark_stage_complete"
  )
  consolidated <- c(
    tempest_persistence_report_reference = "tempest_product_report_reference",
    tempest_persistence_validate_report_reference = "tempest_product_report_reference_validate"
  )
  deleted <- c(
    "tempest_env_values",
    "tempest_env_snapshot",
    "tempest_session_bundle_require_files"
  )

  retained <- !original %in% c(deleted, names(consolidated))
  final_names <- original
  final_names[match(names(renames), original)] <- unname(renames)
  expected_counts <- c(
    "costorm-persistence.R" = 34L,
    "product-authority.R" = 18L,
    "product-persistence.R" = 15L,
    "product-report.R" = 5L,
    "research-expert.R" = 6L,
    "research-workspace-persistence.R" = 25L,
    "stage-record.R" = 6L,
    "storm-persistence.R" = 34L,
    "storm-state.R" = 2L
  )

  context <- test_source_inventory_context()
  definitions <- if (identical(context$mode, "source")) {
    test_source_inventory_definitions(context)
  } else {
    NULL
  }

  expect_identical(length(original), 150L)
  expect_identical(anyDuplicated(original), 0L)
  expect_identical(sum(retained), 145L)
  expect_identical(
    as.integer(table(factor(
      owners[retained],
      levels = names(expected_counts)
    ))),
    unname(expected_counts)
  )
  if (identical(context$mode, "source")) {
    r_dir <- file.path(context$root, "R")
    expect_identical(
      file.exists(file.path(r_dir, "run-persistence.R")),
      FALSE
    )

    retained_counts <- vapply(
      final_names[retained],
      function(name) {
        sum(definitions$name == name)
      },
      integer(1)
    )
    expect_identical(unname(retained_counts), rep(1L, 145L))
    retained_rows <- match(final_names[retained], definitions$name)
    expect_identical(
      definitions$owner[retained_rows],
      unname(owners[retained])
    )

    consolidated_counts <- vapply(
      consolidated,
      function(name) {
        sum(definitions$name == name)
      },
      integer(1)
    )
    expect_identical(unname(consolidated_counts), c(1L, 1L))
    consolidated_rows <- match(unname(consolidated), definitions$name)
    expect_identical(
      definitions$owner[consolidated_rows],
      rep("product-report.R", 2L)
    )
    expect_identical(
      any(
        definitions$name %in%
          c(deleted, names(renames), names(consolidated))
      ),
      FALSE
    )
    expect_identical(
      vapply(
        c("tempest_stage_complete_success", "tempest_stage_complete_failure"),
        function(name) sum(definitions$name == name),
        integer(1)
      ),
      c(
        tempest_stage_complete_success = 1L,
        tempest_stage_complete_failure = 1L
      )
    )
    expect_identical(
      any(
        definitions$name %in%
          c(
            "tempest_storm_stage_complete_success",
            "tempest_storm_stage_complete_failure"
          )
      ),
      FALSE
    )
  } else {
    namespace <- asNamespace("tempest")
    current <- c(final_names[retained], unname(consolidated))
    current_functions <- vapply(
      current,
      function(name) {
        exists(name, envir = namespace, inherits = FALSE) &&
          is.function(get(name, envir = namespace, inherits = FALSE))
      },
      logical(1)
    )
    retired <- unique(c(
      deleted,
      names(renames),
      names(consolidated),
      "tempest_storm_stage_complete_success",
      "tempest_storm_stage_complete_failure"
    ))
    retired_present <- vapply(
      retired,
      exists,
      logical(1),
      envir = namespace,
      inherits = FALSE
    )

    expect_identical(anyDuplicated(current), 0L)
    expect_identical(unname(current_functions), rep(TRUE, length(current)))
    expect_identical(unname(retired_present), rep(FALSE, length(retired)))
    expect_identical(
      vapply(
        c("tempest_stage_complete_success", "tempest_stage_complete_failure"),
        exists,
        logical(1),
        envir = namespace,
        inherits = FALSE
      ),
      c(
        tempest_stage_complete_success = TRUE,
        tempest_stage_complete_failure = TRUE
      )
    )
  }
  expect_disjoint(
    names(formals(tempest:::tempest_storm_save_artifacts)),
    c("research_strategy", "parallel_writing", "remove_duplicate")
  )
  expect_disjoint(
    names(formals(tempest:::tempest_storm_restore_workspace)),
    "config"
  )
  expect_disjoint(
    names(formals(tempest:::tempest_storm_restore_manifest)),
    "run_dir"
  )
  expect_disjoint(
    names(formals(tempest:::tempest_storm_read_state)),
    "run_dir"
  )

  description <- read.dcf(
    test_source_inventory_description(context),
    fields = "Collate"
  )[[1L]]
  collate <- strsplit(description, "[[:space:]]+")[[1L]]
  collate <- gsub("^'|'$", "", collate)
  owner_order <- c(
    "product-hash.R",
    "product-validation.R",
    "product-persistence.R",
    "research-expert.R",
    "models.R",
    "research-manifest.R",
    "program-set.R",
    "stage-record.R",
    "product-report.R",
    "storm-state.R",
    "product-authority.R",
    "retriever.R",
    "research-workspace-persistence.R",
    "costorm-persistence.R",
    "storm-persistence.R",
    "promotion-types.R"
  )
  expect_identical(
    match(owner_order, collate),
    sort(match(owner_order, collate))
  )
  expect_identical("run-persistence.R" %in% collate, FALSE)
})
