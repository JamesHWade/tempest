# Observational four-package claim provenance
#
# This shadow projection proves exact execution-identity joins. Deputy does not
# yet attest the bytes of its response as an extractor input, so the projection
# deliberately makes no content-causation claim.

tempest_claim_provenance_abort <- function(message) {
  tempest_abort(
    message,
    class = c("tempest_claim_provenance_error", "tempest_error")
  )
}

tempest_claim_provenance_terminal_trace <- function(
  record,
  traces,
  manifest
) {
  references <- record@trace_references
  run_id <- references$deputy_run_id %||% NULL
  session_id <- references$deputy_session_id %||% NULL
  if (is.null(run_id) || is.null(session_id)) {
    tempest_claim_provenance_abort(
      "Shadow extraction provenance requires one Deputy run and session."
    )
  }
  terminal <- Filter(
    function(trace) {
      identical(trace$deputy_run_id %||% NULL, run_id) &&
        identical(trace$deputy_session_id %||% NULL, session_id)
    },
    traces
  )
  if (length(terminal) != 1L) {
    tempest_claim_provenance_abort(
      "Shadow extraction provenance must resolve one exact Deputy tuple."
    )
  }
  terminal <- terminal[[1L]]
  runtime_run_ids <- unlist(
    manifest@runtime$deputy_run_ids %||% list(),
    use.names = FALSE
  )
  runtime_session_ids <- unlist(
    manifest@runtime$deputy_session_ids %||% list(),
    use.names = FALSE
  )
  if (
    !identical(terminal$trace_id %||% NULL, run_id) ||
      !identical(terminal$status %||% NULL, "complete") ||
      !run_id %in% runtime_run_ids ||
      !session_id %in% runtime_session_ids
  ) {
    tempest_claim_provenance_abort(
      "Shadow extraction provenance requires one exact completed Deputy run."
    )
  }
  expert_id <- references$expert_id %||% NULL
  if (identical(terminal$role %||% NULL, "moderator")) {
    if (
      !identical(terminal$stage %||% NULL, "dialogue") ||
        !identical(expert_id, "moderator") ||
        !is.null(terminal$expert_id %||% NULL)
    ) {
      tempest_claim_provenance_abort(
        "Shadow extraction provenance has a mismatched moderator identity."
      )
    }
  } else if (
    !identical(terminal$role %||% NULL, "expert") ||
      !terminal$stage %in% c("dialogue", "warmup") ||
      !identical(terminal$expert_id %||% NULL, expert_id)
  ) {
    tempest_claim_provenance_abort(
      "Shadow extraction provenance has a mismatched expert identity."
    )
  }
  if (
    !identical(
      terminal$correlation_id %||% NULL,
      references$correlation_id %||% NULL
    )
  ) {
    tempest_claim_provenance_abort(
      "Shadow extraction provenance has a mismatched correlation identity."
    )
  }

  relation_fields <- c("parent_run_id", "delegation_id", "tool_call_id")
  relation <- references[relation_fields]
  relation_present <- !vapply(relation, is.null, logical(1))
  terminal_relation <- terminal[relation_fields]
  terminal_present <- !vapply(terminal_relation, is.null, logical(1))
  if (
    any(relation_present) &&
      !all(relation_present) ||
      any(terminal_present) && !all(terminal_present) ||
      !identical(relation, terminal_relation)
  ) {
    tempest_claim_provenance_abort(
      "Shadow extraction provenance has an incomplete Deputy delegation tuple."
    )
  }
  if (!all(relation_present)) {
    if (!identical(terminal$trace_type %||% NULL, "deputy_run")) {
      tempest_claim_provenance_abort(
        "Direct shadow provenance requires a terminal Deputy run trace."
      )
    }
    expected_agent_id <- tempest_deputy_adapter_agent_id(
      tempest_deputy_run_context(
        manifest,
        stage = "dialogue",
        role = terminal$role,
        expert_id = terminal$expert_id %||% NULL
      )
    )
    if (!identical(terminal$agent_id %||% NULL, expected_agent_id)) {
      tempest_claim_provenance_abort(
        "Direct shadow provenance has a mismatched Deputy agent identity."
      )
    }
    return(list(terminal))
  }
  if (!identical(terminal$trace_type %||% NULL, "deputy_delegation")) {
    tempest_claim_provenance_abort(
      "Delegated shadow provenance requires a Deputy delegation trace."
    )
  }
  if (identical(relation$parent_run_id, run_id)) {
    tempest_claim_provenance_abort(
      "A delegated shadow execution cannot be its own parent run."
    )
  }
  parent_agent_id <- terminal$parent_agent_id %||% NULL
  if (is.null(parent_agent_id)) {
    tempest_claim_provenance_abort(
      "Shadow delegation provenance is missing its parent agent."
    )
  }
  parent <- Filter(
    function(trace) {
      identical(trace$deputy_run_id %||% NULL, relation$parent_run_id) &&
        identical(trace$trace_id %||% NULL, relation$parent_run_id) &&
        identical(trace$agent_id %||% NULL, parent_agent_id) &&
        identical(trace$status %||% NULL, "complete") &&
        identical(trace$trace_type %||% NULL, "deputy_run") &&
        identical(trace$stage %||% NULL, "dialogue") &&
        identical(trace$role %||% NULL, "moderator") &&
        identical(
          trace$correlation_id %||% NULL,
          terminal$correlation_id %||% NULL
        ) &&
        is.null(trace$expert_id %||% NULL)
    },
    traces
  )
  if (length(parent) != 1L) {
    tempest_claim_provenance_abort(
      "Shadow delegation provenance must resolve one exact completed parent."
    )
  }
  parent <- parent[[1L]]
  parent_run_id <- parent$deputy_run_id %||% NULL
  parent_session_id <- parent$deputy_session_id %||% NULL
  if (
    is.null(parent_run_id) ||
      is.null(parent_session_id) ||
      !parent_run_id %in% runtime_run_ids ||
      !parent_session_id %in% runtime_session_ids
  ) {
    tempest_claim_provenance_abort(
      "Shadow delegation provenance is absent from manifest runtime identity."
    )
  }
  expected_parent_agent_id <- tempest_deputy_adapter_agent_id(
    tempest_deputy_run_context(
      manifest,
      stage = "dialogue",
      role = "moderator"
    )
  )
  if (!identical(parent_agent_id, expected_parent_agent_id)) {
    tempest_claim_provenance_abort(
      "Shadow delegation provenance has a mismatched parent agent identity."
    )
  }
  list(parent, terminal)
}

tempest_claim_provenance_projection_impl <- function(
  manifest,
  stage_records,
  workspace
) {
  if (!S7::S7_inherits(manifest, TempestResearchManifest)) {
    tempest_claim_provenance_abort(
      "Shadow claim provenance requires a Tempest research manifest."
    )
  }
  if (!inherits(workspace, "ResearchWorkspace")) {
    tempest_claim_provenance_abort(
      "Shadow claim provenance requires a ResearchWorkspace."
    )
  }
  stage_records <- tempest_stage_records_validate(
    stage_records,
    allow_running = FALSE
  )
  extraction <- Filter(
    \(record) {
      identical(record@stage, "extract_claims") &&
        identical(record@status, "succeeded")
    },
    stage_records
  )
  verification <- Filter(
    \(record) {
      identical(record@stage, "verify_claim_support") &&
        identical(record@status, "succeeded")
    },
    stage_records
  )
  claims <- workspace$list_proposed_claims()
  supports <- workspace$list_claim_supports()
  if (
    length(extraction) == 0L ||
      length(verification) == 0L ||
      length(claims) == 0L ||
      length(supports) == 0L
  ) {
    tempest_claim_provenance_abort(
      "Shadow claim provenance requires a non-empty extracted and verified slice."
    )
  }

  snapshot <- workspace$graft_snapshot
  if (is.null(snapshot)) {
    tempest_claim_provenance_abort(
      "Shadow claim provenance requires a real immutable Graft snapshot."
    )
  }
  snapshot_reference <- tempest_snapshot_reference(snapshot)
  if (!identical(snapshot_reference, manifest@knowledge_snapshot)) {
    tempest_claim_provenance_abort(
      "Shadow claim provenance does not match the exact Graft snapshot."
    )
  }

  tempest_stage_records_validate_manifest(stage_records, manifest)
  thresholds <- vapply(
    verification,
    function(record) record@trace_references$min_support_score %||% "",
    character(1)
  )
  if (length(unique(thresholds)) != 1L || !nzchar(thresholds[[1L]])) {
    tempest_claim_provenance_abort(
      "Shadow verification provenance requires one exact support threshold."
    )
  }
  threshold <- tempest_stage_support_threshold_value(thresholds[[1L]])
  tempest_stage_records_validate_workspace(
    stage_records,
    workspace,
    min_support_score = threshold
  )
  tempest_stage_records_validate_workspace_coverage(
    stage_records,
    workspace,
    require_extraction = TRUE,
    require_verification = TRUE
  )

  required_programs <- c("extract_claims", "verify_claim_support")
  programs <- tempest_research_manifest_program_identity_records(
    manifest@programs
  )
  if (!all(required_programs %in% names(programs))) {
    tempest_claim_provenance_abort(
      "Shadow claim provenance is missing an exact dsprrr program identity."
    )
  }
  programs <- programs[required_programs]
  terminal_traces <- Filter(
    function(trace) {
      trace$trace_type %in% c("deputy_run", "deputy_delegation")
    },
    manifest@traces
  )
  terminal_traces <- tempest_research_manifest_traces(terminal_traces)

  support_ids <- vapply(
    supports,
    \(support) support@claim_support_id,
    character(1)
  )
  supports <- supports[order(support_ids, method = "radix")]
  deputy_by_run <- new.env(hash = TRUE, parent = emptyenv())
  pairs <- lapply(supports, function(support) {
    claim <- workspace$get_proposed_claim(support@claim_id)
    span <- workspace$get_evidence_span(support@evidence_span_id)
    if (is.null(claim) || is.null(span)) {
      tempest_claim_provenance_abort(
        "Shadow claim provenance references missing durable evidence."
      )
    }
    extraction_record <- Filter(
      function(record) {
        support@claim_id %in%
          unlist(record@output_reference$ids, use.names = FALSE)
      },
      extraction
    )
    verification_record <- Filter(
      function(record) {
        identical(
          unlist(record@output_reference$ids, use.names = FALSE),
          support@claim_support_id
        )
      },
      verification
    )
    if (length(extraction_record) != 1L || length(verification_record) != 1L) {
      tempest_claim_provenance_abort(
        "Each shadow claim pair requires one extraction and verification attempt."
      )
    }
    extraction_record <- extraction_record[[1L]]
    verification_record <- verification_record[[1L]]
    extracted_at <- tempest_stage_time_parse(extraction_record@completed_at)
    verified_at <- tempest_stage_time_parse(verification_record@started_at)
    if (
      is.na(extracted_at) ||
        is.na(verified_at) ||
        as.numeric(extracted_at) > as.numeric(verified_at)
    ) {
      tempest_claim_provenance_abort(
        "Shadow claim provenance requires extraction before verification."
      )
    }

    resolved <- tempest_claim_provenance_terminal_trace(
      extraction_record,
      terminal_traces,
      manifest
    )
    for (trace in resolved) {
      run_id <- trace$deputy_run_id
      existing <- deputy_by_run[[run_id]] %||% NULL
      if (!is.null(existing) && !identical(existing, trace)) {
        tempest_claim_provenance_abort(
          "Shadow claim provenance contains conflicting Deputy run records."
        )
      }
      deputy_by_run[[run_id]] <- trace
    }
    verifier_model <- verification_record@trace_references$verifier_model %||%
      NULL
    list(
      claim_support_id = support@claim_support_id,
      claim_id = support@claim_id,
      evidence_span_id = support@evidence_span_id,
      source_id = support@source_id,
      verification_status = support@verification_status,
      support_score = if (is.na(support@support_score)) {
        NULL
      } else {
        support@support_score
      },
      extraction = list(
        attempt_id = extraction_record@attempt_id,
        program_artifact_id = extraction_record@program_artifact_id,
        output_content_digest = extraction_record@output_reference$content_digest,
        deputy_run_id = extraction_record@trace_references$deputy_run_id,
        deputy_session_id = extraction_record@trace_references$deputy_session_id
      ),
      verification = list(
        attempt_id = verification_record@attempt_id,
        program_artifact_id = verification_record@program_artifact_id,
        output_content_digest = verification_record@output_reference$content_digest,
        support_status = verification_record@support_status,
        min_support_score = verification_record@trace_references$min_support_score,
        verified_at = verification_record@trace_references$verified_at,
        verifier_model = verifier_model
      )
    )
  })
  deputy_runs <- unname(as.list(deputy_by_run, all.names = TRUE))
  deputy_run_ids <- vapply(
    deputy_runs,
    `[[`,
    character(1),
    "deputy_run_id"
  )
  deputy_runs <- deputy_runs[order(deputy_run_ids, method = "radix")]
  projection <- list(
    schema_version = 1L,
    binding_scope = "execution_identity",
    research_run_id = manifest@research_run_id,
    mode = manifest@mode,
    config_digest = manifest@config_digest,
    knowledge_snapshot = snapshot_reference,
    programs = programs,
    deputy_runs = deputy_runs,
    claim_pairs = pairs
  )
  tryCatch(
    tempest_research_manifest_canonical_value(
      projection,
      "claim_provenance_projection"
    ),
    error = function(error) {
      tempest_claim_provenance_abort(
        "Shadow claim provenance is not canonical and credential-free."
      )
    }
  )
  projection
}

tempest_claim_provenance_projection <- function(
  manifest,
  stage_records,
  workspace
) {
  tryCatch(
    tempest_claim_provenance_projection_impl(
      manifest,
      stage_records,
      workspace
    ),
    error = function(error) {
      if (inherits(error, "tempest_claim_provenance_error")) {
        stop(error)
      }
      tempest_claim_provenance_abort(
        "Could not derive the shadow claim-provenance projection."
      )
    }
  )
}
