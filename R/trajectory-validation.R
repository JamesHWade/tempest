# Trajectory review S7 self-validation helpers

tempest_trajectory_validate_product <- function(product) {
  tempest_trajectory_exact_record(
    product,
    tempest_trajectory_product_fields(),
    "Trajectory product"
  )
  tempest_trajectory_scalar_string(
    product$research_run_id,
    "Trajectory product research_run_id"
  )
  tempest_trajectory_validate_opaque_identifier(
    product$research_run_id,
    "Trajectory product research_run_id"
  )
  tempest_trajectory_scalar_string(product$mode, "Trajectory product mode")
  tempest_trajectory_scalar_string(product$status, "Trajectory product status")
  if (!product$mode %in% c("storm", "costorm")) {
    tempest_trajectory_review_abort("Trajectory product mode is invalid.")
  }
  if (!identical(product$status, "succeeded")) {
    tempest_trajectory_review_abort("Trajectory product must be succeeded.")
  }
  tempest_trajectory_validate_sha256(
    product$config_digest,
    "Trajectory product config_digest"
  )
  tempest_trajectory_exact_record(
    product$report_reference,
    c("report_id", "sha256"),
    "Trajectory report reference"
  )
  tempest_trajectory_scalar_string(
    product$report_reference$report_id,
    "Trajectory report reference report_id"
  )
  tempest_trajectory_validate_sha256(
    product$report_reference$sha256,
    "Trajectory report reference sha256"
  )
  if (!identical(product$report_reference$report_id, "report_md")) {
    tempest_trajectory_review_abort(
      "Trajectory report reference is invalid."
    )
  }
  invisible(product)
}

tempest_trajectory_deputy_session_owned_by <- function(
  session_id,
  expert_id,
  product
) {
  if (identical(product$mode, "storm")) {
    return(identical(
      session_id,
      tempest_storm_deputy_session_id(
        product$research_run_id,
        expert_id
      )
    ))
  }
  if (identical(expert_id, "moderator")) {
    return(identical(
      session_id,
      tempest_costorm_deputy_session_id(
        product$research_run_id,
        "moderator"
      )
    ))
  }
  tempest_deputy_expert_session_owned_by(
    session_id,
    product$research_run_id,
    expert_id
  )
}

tempest_trajectory_validate_stage <- function(stage, programs, product) {
  tempest_trajectory_exact_record(
    stage,
    tempest_trajectory_stage_fields(),
    "Trajectory stage"
  )
  tempest_trajectory_exact_record(
    stage$output,
    tempest_trajectory_stage_output_fields(),
    "Trajectory stage output"
  )
  if (!stage$stage %in% tempest_program_set_stages()) {
    tempest_trajectory_review_abort("Trajectory stage name is invalid.")
  }
  if (!stage$status %in% c("succeeded", "failed", "cancelled")) {
    tempest_trajectory_review_abort("Trajectory stage status is not terminal.")
  }
  for (field in c(
    "stage",
    "attempt_id",
    "trace_id",
    "status",
    "started_at",
    "program_artifact_id",
    "fallback_policy",
    "execution_path",
    "support_status"
  )) {
    tempest_trajectory_scalar_string(
      stage[[field]],
      paste("Trajectory stage", field)
    )
  }
  for (field in c("attempt_id", "trace_id")) {
    tempest_trajectory_validate_opaque_identifier(
      stage[[field]],
      paste("Trajectory stage", field)
    )
  }
  tempest_trajectory_validate_sha256(
    stage$program_artifact_id,
    "Trajectory stage program_artifact_id"
  )
  program <- programs[[stage$stage]]
  if (
    is.null(program) ||
      !identical(stage$program_artifact_id, program$program_artifact_id)
  ) {
    tempest_trajectory_review_abort(
      "A trajectory stage is not bound to its declared program artifact."
    )
  }
  governed_reference <- program$governed_procedure_ref
  governed_revision <- if (is.null(governed_reference)) {
    NULL
  } else {
    governed_reference$revision_id
  }
  if (!identical(stage$governed_procedure_revision_id, governed_revision)) {
    tempest_trajectory_review_abort(
      "A trajectory stage governed revision does not match its program."
    )
  }
  for (field in c(
    "completed_at",
    "governed_procedure_revision_id",
    "failure_class",
    "fallback_implementation"
  )) {
    tempest_trajectory_scalar_string(
      stage[[field]],
      paste("Trajectory stage", field),
      nullable = TRUE
    )
  }
  tempest_trajectory_validate_opaque_identifier(
    stage$governed_procedure_revision_id,
    "Trajectory stage governed_procedure_revision_id",
    nullable = TRUE
  )
  if (
    !is.null(stage$failure_class) &&
      !stage$failure_class %in% tempest_stage_failure_classes()
  ) {
    tempest_trajectory_review_abort(
      "A trajectory stage failure class is not a controlled source value."
    )
  }
  binding <- stage$deputy_binding
  if (!is.null(binding)) {
    tempest_trajectory_exact_record(
      binding,
      tempest_trajectory_stage_deputy_fields(),
      "Trajectory stage Deputy binding"
    )
    for (field in tempest_trajectory_stage_deputy_fields()) {
      tempest_trajectory_scalar_string(
        binding[[field]],
        paste("Trajectory stage Deputy binding", field),
        nullable = !field %in% c("run_id", "session_id", "expert_id")
      )
      tempest_trajectory_validate_opaque_identifier(
        binding[[field]],
        paste("Trajectory stage Deputy binding", field),
        nullable = !field %in% c("run_id", "session_id", "expert_id")
      )
    }
    lineage <- c("parent_run_id", "delegation_id", "tool_call_id")
    lineage_present <- !vapply(binding[lineage], is.null, logical(1))
    if (any(lineage_present) && !all(lineage_present)) {
      tempest_trajectory_review_abort(
        "A trajectory stage Deputy binding has partial delegation lineage."
      )
    }
    if (
      !tempest_trajectory_deputy_session_owned_by(
        binding$session_id,
        binding$expert_id,
        product
      )
    ) {
      tempest_trajectory_review_abort(
        paste0(
          "A trajectory stage Deputy session ID does not match its product ",
          "and expert context."
        )
      )
    }
  }
  started_at <- tempest_stage_time_parse(stage$started_at)
  completed_at <- tempest_stage_time_parse(stage$completed_at)
  if (is.na(started_at) || is.na(completed_at) || completed_at < started_at) {
    tempest_trajectory_review_abort(
      "A terminal trajectory stage must retain its valid completion interval."
    )
  }
  if (!identical(stage$trace_id, stage$attempt_id)) {
    tempest_trajectory_review_abort(
      "A trajectory stage trace ID must match its attempt ID."
    )
  }
  if (
    !rlang::is_bool(stage$fallback_taken) ||
      !rlang::is_bool(stage$publication_allowed)
  ) {
    tempest_trajectory_review_abort(
      "Trajectory stage trust flags must be non-missing logicals."
    )
  }
  if (!is.null(stage$output$kind)) {
    tempest_trajectory_scalar_string(
      stage$output$kind,
      "Trajectory stage output kind"
    )
  }
  tempest_trajectory_whole_number(
    stage$output$count,
    "Trajectory stage output count"
  )
  if (is.null(stage$output$kind) && stage$output$count != 0L) {
    tempest_trajectory_review_abort(
      "A trajectory output without a kind must be empty."
    )
  }
  if (
    is.null(stage$output$kind) &&
      identical(stage$output$count, 0L) &&
      !identical(stage$output$digest, tempest_trajectory_digest(list()))
  ) {
    tempest_trajectory_review_abort(
      "A trajectory empty output must retain its canonical digest."
    )
  }
  if (identical(stage$status, "succeeded")) {
    output_contract <- tempest_stage_output_reference_contract(stage$stage)
    projected_ids <- tempest_trajectory_stage_projected_output_ids(stage)
    fixed_count <- if (is.null(projected_ids)) {
      NULL
    } else {
      as.integer(length(projected_ids))
    }
    if (
      !identical(stage$output$kind, output_contract$kind) ||
        (!is.null(fixed_count) &&
          !identical(stage$output$count, fixed_count))
    ) {
      tempest_trajectory_review_abort(
        "A trajectory stage output does not match its stage contract."
      )
    }
  }
  tempest_trajectory_validate_sha256(
    stage$output$digest,
    "Trajectory stage output digest"
  )
  tempest_trajectory_validate_stage_trust(stage, governed_reference)
  invisible(stage)
}

tempest_trajectory_validate_stage_trust <- function(
  stage,
  governed_reference = NULL
) {
  policy <- tempest_stage_policy(stage$stage)
  if (!identical(stage$fallback_policy, policy$fallback_policy)) {
    tempest_trajectory_review_abort(
      "A trajectory stage fallback policy does not match its stage."
    )
  }
  if (
    !stage$execution_path %in% tempest_execution_paths() ||
      !stage$support_status %in% tempest_support_statuses()
  ) {
    tempest_trajectory_review_abort(
      "A trajectory stage has invalid execution trust state."
    )
  }
  if (!is.null(stage$failure_class)) {
    if (!stage$failure_class %in% tempest_stage_failure_classes()) {
      tempest_trajectory_review_abort(
        "A trajectory stage has an invalid failure class."
      )
    }
  }
  if (identical(stage$status, "succeeded")) {
    if (
      is.null(stage$output$kind) ||
        !stage$output$kind %in% tempest_stage_output_kinds() ||
        stage$output$count < 1L
    ) {
      tempest_trajectory_review_abort(
        "A succeeded trajectory stage must retain its output summary."
      )
    }
    unknown_support_stages <- c(
      "perspectives",
      "personas",
      "query_decomposition",
      "extract_claims",
      "next_question",
      "draft_outline",
      "refined_outline"
    )
    if (
      stage$stage %in%
        unknown_support_stages &&
        !identical(stage$support_status, "unknown")
    ) {
      tempest_trajectory_review_abort(
        "A trajectory planning stage must retain unknown support state."
      )
    }
    if (
      stage$stage %in%
        c("section_writing", "lead_section") &&
        !identical(stage$support_status, "verified")
    ) {
      tempest_trajectory_review_abort(
        "A trajectory writing stage must retain verified support state."
      )
    }
    if (isTRUE(stage$fallback_taken)) {
      if (is.null(stage$failure_class)) {
        tempest_trajectory_review_abort(
          "A successful trajectory fallback must retain its primary failure."
        )
      }
    } else if (!is.null(stage$failure_class)) {
      tempest_trajectory_review_abort(
        "A direct successful trajectory stage cannot retain a failure."
      )
    }
  } else {
    if (
      !is.null(stage$output$kind) ||
        !identical(stage$output$count, 0L) ||
        is.null(stage$failure_class) ||
        !identical(stage$support_status, "unknown")
    ) {
      tempest_trajectory_review_abort(
        "A failed trajectory stage has inconsistent result state."
      )
    }
  }
  if (
    identical(stage$status, "cancelled") &&
      !identical(stage$failure_class, "tempest_stage_cancelled")
  ) {
    tempest_trajectory_review_abort(
      "A cancelled trajectory stage must retain its controlled failure."
    )
  }
  if (
    identical(stage$fallback_policy, "fail_closed") &&
      isTRUE(stage$fallback_taken)
  ) {
    tempest_trajectory_review_abort(
      "A fail-closed trajectory stage cannot take a fallback."
    )
  }
  expected_implementation <- policy$fallback_implementation
  if (isTRUE(stage$fallback_taken)) {
    if (
      is.na(expected_implementation) ||
        !identical(stage$fallback_implementation, expected_implementation)
    ) {
      tempest_trajectory_review_abort(
        "A trajectory fallback implementation does not match its stage."
      )
    }
  } else if (!is.null(stage$fallback_implementation)) {
    tempest_trajectory_review_abort(
      "A trajectory fallback implementation requires a taken fallback."
    )
  }
  expected_execution_path <- tempest_stage_execution_path_derive(
    stage$status,
    policy,
    stage$governed_procedure_revision_id %||% NA_character_,
    if (is.null(governed_reference)) {
      list()
    } else {
      list(governed_procedure = governed_reference)
    },
    stage$fallback_taken
  )
  if (!identical(stage$execution_path, expected_execution_path)) {
    tempest_trajectory_review_abort(
      "A trajectory execution path is not derived from terminal authority."
    )
  }
  expected_publication <- tempest_stage_publication_allowed(
    stage$status,
    stage$execution_path,
    stage$support_status
  )
  if (!identical(stage$publication_allowed, expected_publication)) {
    tempest_trajectory_review_abort(
      "A trajectory publication decision does not match its trust state."
    )
  }
  invisible(stage)
}

tempest_trajectory_validate_agent <- function(
  agent,
  product,
  knowledge
) {
  tempest_trajectory_exact_record(
    agent,
    tempest_trajectory_agent_fields(),
    "Trajectory agent run"
  )
  required <- c(
    "trace_id",
    "trace_type",
    "stage",
    "role",
    "status",
    "completion_disposition",
    "agent_id",
    "deputy_run_id",
    "deputy_session_id",
    "correlation_id"
  )
  for (field in tempest_trajectory_agent_fields()) {
    tempest_trajectory_scalar_string(
      agent[[field]],
      paste("Trajectory agent", field),
      nullable = !field %in% required
    )
  }
  identifier_fields <- c(
    "trace_id",
    "agent_id",
    "expert_id",
    "deputy_run_id",
    "deputy_session_id",
    "parent_agent_id",
    "parent_run_id",
    "delegation_id",
    "tool_call_id",
    "correlation_id"
  )
  for (field in identifier_fields) {
    tempest_trajectory_validate_opaque_identifier(
      agent[[field]],
      paste("Trajectory agent", field),
      nullable = !field %in% required
    )
  }
  if (!agent$trace_type %in% c("deputy_run", "deputy_delegation")) {
    tempest_trajectory_review_abort("Trajectory agent trace_type is invalid.")
  }
  if (!is.null(agent$program_artifact_id)) {
    tempest_trajectory_review_abort(
      "A trajectory Deputy agent cannot retain a program artifact."
    )
  }
  statuses <- tempest_product_authority_deputy_terminal_statuses()
  if (!agent$status %in% statuses) {
    tempest_trajectory_review_abort("Trajectory agent status is invalid.")
  }
  if (!agent$completion_disposition %in% c("issued", "discarded", "terminal")) {
    tempest_trajectory_review_abort(
      "Trajectory agent completion_disposition is invalid."
    )
  }
  terminal <- identical(agent$completion_disposition, "terminal")
  if (xor(terminal, !identical(agent$status, "complete"))) {
    tempest_trajectory_review_abort(
      "Trajectory agent status and completion_disposition are inconsistent."
    )
  }
  if (!identical(agent$trace_id, agent$deputy_run_id)) {
    tempest_trajectory_review_abort(
      "A trajectory Deputy trace must use its run ID as trace ID."
    )
  }
  valid_context <- if (identical(product$mode, "storm")) {
    identical(c(agent$stage, agent$role), c("research", "expert"))
  } else if (identical(product$mode, "costorm")) {
    identical(c(agent$stage, agent$role), c("dialogue", "moderator")) ||
      identical(c(agent$stage, agent$role), c("dialogue", "expert")) ||
      identical(c(agent$stage, agent$role), c("warmup", "expert"))
  } else {
    FALSE
  }
  if (!valid_context) {
    tempest_trajectory_review_abort(
      "A trajectory Deputy trace has an invalid product context."
    )
  }
  if (identical(agent$role, "expert")) {
    if (is.null(agent$expert_id)) {
      tempest_trajectory_review_abort(
        "An expert trajectory Deputy trace must bind an expert ID."
      )
    }
  } else if (!is.null(agent$expert_id)) {
    tempest_trajectory_review_abort(
      "A moderator trajectory Deputy trace cannot bind an expert ID."
    )
  }
  session_expert_id <- if (identical(agent$role, "moderator")) {
    "moderator"
  } else {
    agent$expert_id
  }
  valid_session <- tempest_trajectory_deputy_session_owned_by(
    agent$deputy_session_id,
    session_expert_id,
    product
  )
  if (!valid_session) {
    tempest_trajectory_review_abort(
      "A trajectory Deputy session ID does not match its product context."
    )
  }
  relation_fields <- c(
    "parent_agent_id",
    "parent_run_id",
    "delegation_id",
    "tool_call_id"
  )
  relation_present <- !vapply(
    agent[relation_fields],
    is.null,
    logical(1)
  )
  if (
    (any(relation_present) && !all(relation_present)) ||
      xor(
        identical(agent$trace_type, "deputy_delegation"),
        all(relation_present)
      ) ||
      (all(relation_present) &&
        identical(agent$parent_run_id, agent$deputy_run_id))
  ) {
    tempest_trajectory_review_abort(
      "A trajectory Deputy delegation must retain its complete lineage tuple."
    )
  }
  agent_stage <- if (
    identical(product$mode, "costorm") &&
      identical(agent$role, "expert")
  ) {
    "dialogue"
  } else {
    agent$stage
  }
  run_context <- list(
    product = "tempest",
    research_run_id = product$research_run_id,
    mode = product$mode,
    stage = agent_stage,
    role = agent$role
  )
  if (!is.null(agent$expert_id)) {
    run_context$expert_id <- agent$expert_id
  }
  expected_agent_id <- tempest_deputy_adapter_agent_id(run_context)
  if (!identical(agent$agent_id, expected_agent_id)) {
    tempest_trajectory_review_abort(
      "A trajectory Deputy agent ID does not match its product context."
    )
  }
  invisible(agent)
}

tempest_trajectory_validate_programs <- function(programs) {
  stages <- tempest_program_set_stages()
  expected_evaluators <- tempest_program_set_default_evaluators()
  if (
    !is.list(programs) ||
      is.data.frame(programs) ||
      is.object(programs) ||
      !identical(names(programs), stages)
  ) {
    tempest_trajectory_review_abort(
      "Trajectory programs must be the fixed ten-stage record."
    )
  }
  for (stage in stages) {
    program <- tempest_trajectory_exact_record(
      programs[[stage]],
      tempest_trajectory_program_fields(),
      "Trajectory program"
    )
    if (!identical(program$stage, stage)) {
      tempest_trajectory_review_abort(
        "A trajectory program does not match its named stage."
      )
    }
    evaluator_fields <- c("evaluator_id", "evaluator_version")
    for (field in evaluator_fields) {
      tempest_trajectory_scalar_string(
        program[[field]],
        paste("Trajectory program", field)
      )
    }
    tempest_trajectory_whole_number(
      program$contract_version,
      "Trajectory program contract_version"
    )
    if (
      !identical(program$contract_version, 1L) ||
        !identical(
          program[evaluator_fields],
          expected_evaluators[[stage]]
        )
    ) {
      tempest_trajectory_review_abort(
        "A trajectory program does not match its fixed evaluator contract."
      )
    }
    tempest_trajectory_validate_sha256(
      program$program_artifact_id,
      "Trajectory program program_artifact_id"
    )
    if (!is.null(program$governed_procedure_ref)) {
      reference <- tryCatch(
        tempest_governed_procedure_record(program$governed_procedure_ref),
        error = function(error) {
          tempest_trajectory_review_abort(
            "A trajectory governed-procedure reference is invalid.",
            parent = error
          )
        }
      )
      if (
        !identical(reference$stage, program$stage) ||
          !identical(
            reference$program_artifact_id,
            program$program_artifact_id
          ) ||
          !identical(reference$evaluator_id, program$evaluator_id) ||
          !identical(reference$evaluator_version, program$evaluator_version) ||
          !identical(reference$contract_version, program$contract_version)
      ) {
        tempest_trajectory_review_abort(
          "A trajectory governed-procedure reference is not bound to its program."
        )
      }
    }
  }
  invisible(programs)
}


tempest_trajectory_validate_knowledge <- function(knowledge, research_run_id) {
  tempest_trajectory_exact_record(
    knowledge,
    tempest_trajectory_knowledge_fields(),
    "Trajectory knowledge"
  )

  tempest_trajectory_validate_input_selection(knowledge$input_selection)
  tempest_trajectory_validate_collection(
    knowledge$consultations,
    tempest_trajectory_consultation_fields(),
    "Trajectory memory consultations"
  )
  invisible(lapply(
    knowledge$consultations$items,
    tempest_trajectory_validate_consultation
  ))
  tempest_trajectory_scalar_string(
    knowledge$promotion_state,
    "Trajectory promotion state"
  )
  if (!knowledge$promotion_state %in% c("none", "proposed", "accepted")) {
    tempest_trajectory_review_abort(
      "Trajectory promotion state is invalid."
    )
  }
  if (identical(knowledge$promotion_state, "none")) {
    if (!is.null(knowledge$proposal) || !is.null(knowledge$acceptance)) {
      tempest_trajectory_review_abort(
        "A trajectory without promotion cannot retain proposal state."
      )
    }
    return(invisible(knowledge))
  }
  proposal <- tempest_trajectory_exact_record(
    knowledge$proposal,
    tempest_trajectory_proposal_fields(),
    "Trajectory proposal"
  )
  tempest_trajectory_validate_sha256(
    proposal$bundle_id,
    "Trajectory proposal bundle_id"
  )
  tempest_trajectory_scalar_string(
    proposal$research_run_id,
    "Trajectory proposal research_run_id"
  )
  tempest_trajectory_validate_sha256(
    proposal$schema_build_digest,
    "Trajectory proposal schema_build_digest"
  )
  if (
    !identical(proposal$research_run_id, research_run_id) ||
      !identical(
        proposal$schema_build_digest,
        tempest_promotion_schema_build_digest
      )
  ) {
    tempest_trajectory_review_abort(
      "Trajectory proposal does not match its research run and schema digest."
    )
  }
  selection <- tempest_trajectory_exact_record(
    proposal$claim_selection,
    tempest_trajectory_selection_fields(),
    "Trajectory claim selection"
  )
  tempest_trajectory_scalar_string(
    selection$kind,
    "Trajectory proposal selection kind"
  )
  if (!identical(selection$kind, "claim_ids")) {
    tempest_trajectory_review_abort(
      "Trajectory proposal selection kind is invalid."
    )
  }
  tempest_trajectory_whole_number(
    selection$count,
    "Trajectory proposal claim count"
  )
  if (selection$count < 1L) {
    tempest_trajectory_review_abort(
      "A trajectory promotion proposal must retain at least one claim."
    )
  }
  tempest_trajectory_validate_sha256(
    selection$digest,
    "Trajectory proposal selection digest"
  )
  if (!is.null(proposal$selection_id)) {
    tempest_trajectory_artifact_digest(
      proposal$selection_id,
      "Publication selection"
    )
  }
  if (identical(knowledge$promotion_state, "proposed")) {
    if (!is.null(knowledge$acceptance)) {
      tempest_trajectory_review_abort(
        "A proposed trajectory cannot retain acceptance state."
      )
    }
    return(invisible(knowledge))
  }
  tempest_trajectory_validate_decision(knowledge$acceptance)
  if (!identical(knowledge$acceptance$selection_id, proposal$selection_id)) {
    tempest_trajectory_review_abort(
      "Trajectory acceptance does not bind its publication."
    )
  }
  invisible(knowledge)
}

tempest_trajectory_validate_claim_selection <- function(knowledge, evidence) {
  if (
    identical(knowledge$promotion_state, "none") ||
      !identical(evidence$omitted, 0L)
  ) {
    return(invisible(knowledge))
  }
  claim_ids <- vapply(
    Filter(
      \(item) identical(item$record_type, "claim"),
      evidence$items
    ),
    `[[`,
    character(1),
    "record_id"
  )
  claim_count <- length(claim_ids)
  if (knowledge$proposal$claim_selection$count > claim_count) {
    tempest_trajectory_review_abort(
      "Trajectory proposal claim count exceeds the complete evidence lane."
    )
  }
  if (
    knowledge$proposal$claim_selection$count == claim_count &&
      !identical(
        knowledge$proposal$claim_selection$digest,
        tempest_trajectory_claim_selection_digest(claim_ids)
      )
  ) {
    tempest_trajectory_review_abort(
      "Trajectory proposal claim selection digest does not match all claims."
    )
  }
  invisible(knowledge)
}

tempest_trajectory_validate_evidence <- function(evidence) {
  tempest_trajectory_scalar_string(
    evidence$record_type,
    "Trajectory evidence record_type"
  )
  if (!evidence$record_type %in% tempest_trajectory_evidence_types()) {
    tempest_trajectory_review_abort("Trajectory evidence type is invalid.")
  }
  tempest_trajectory_validate_opaque_identifier(
    evidence$record_id,
    "Trajectory evidence record_id"
  )
  invisible(evidence)
}

tempest_trajectory_validate_join <- function(join) {
  proof <- tempest_trajectory_exact_record(
    join$proof,
    tempest_trajectory_proof_fields(),
    "Trajectory join proof"
  )
  for (field in c("from_type", "from_id", "relation", "to_type", "to_id")) {
    tempest_trajectory_scalar_string(
      join[[field]],
      paste("Trajectory join", field)
    )
  }
  for (field in c("from_id", "to_id")) {
    tempest_trajectory_validate_opaque_identifier(
      join[[field]],
      paste("Trajectory join", field)
    )
  }
  tempest_trajectory_scalar_string(
    proof$kind,
    "Trajectory join proof kind"
  )
  if (!join$relation %in% tempest_trajectory_relations()) {
    tempest_trajectory_review_abort("Trajectory join relation is invalid.")
  }
  if (!proof$kind %in% tempest_trajectory_proof_kinds()) {
    tempest_trajectory_review_abort("Trajectory join proof kind is invalid.")
  }
  if (
    !is.list(proof$matched_fields) ||
      is.data.frame(proof$matched_fields) ||
      is.object(proof$matched_fields) ||
      !is.null(names(proof$matched_fields)) ||
      length(proof$matched_fields) == 0L ||
      !all(vapply(
        proof$matched_fields,
        \(field) {
          rlang::is_string(field) &&
            !is.na(field) &&
            nzchar(trimws(field))
        },
        logical(1)
      ))
  ) {
    tempest_trajectory_review_abort(
      "Trajectory join matched_fields must be an exact string collection."
    )
  }
  if (
    xor(
      identical(join$relation, "correlated_with"),
      identical(proof$kind, "correlation_only")
    )
  ) {
    tempest_trajectory_review_abort(
      "Correlation joins must use only correlation_only proof."
    )
  }
  invisible(join)
}

tempest_trajectory_mandatory_joins <- function(review) {
  run_id <- review@product$research_run_id
  expected <- list()
  add <- function(join) {
    expected[[length(expected) + 1L]] <<- join
  }
  for (stage in review@stages$items) {
    add(tempest_trajectory_join(
      "product",
      run_id,
      "contains",
      "stage_attempt",
      stage$attempt_id,
      "authority_validated",
      c("research_run_id", "attempt_id")
    ))
    add(tempest_trajectory_join(
      "stage_attempt",
      stage$attempt_id,
      "executed_as",
      "program_artifact",
      stage$program_artifact_id,
      "authority_validated",
      c(
        "stage",
        "program_artifact_id",
        "contract_version",
        "evaluator_id",
        "evaluator_version"
      )
    ))
    binding <- stage$deputy_binding
    if (!is.null(binding)) {
      add(tempest_trajectory_join(
        "stage_attempt",
        stage$attempt_id,
        "executed_as",
        "deputy_run",
        binding$run_id,
        "authority_validated",
        c("deputy_run_id", "deputy_session_id")
      ))
      if (!is.null(binding$correlation_id)) {
        add(tempest_trajectory_join(
          "stage_attempt",
          stage$attempt_id,
          "correlated_with",
          "deputy_run",
          binding$run_id,
          "correlation_only",
          "correlation_id"
        ))
      }
    }
    output_contract <- tempest_stage_output_reference_contract(stage$stage)
    output_ids <- tempest_trajectory_stage_projected_output_ids(stage)
    if (!is.null(stage$output$kind) && !is.null(output_ids)) {
      output_type <- tempest_trajectory_stage_output_type(output_contract$kind)
      for (output_id in output_ids) {
        add(tempest_trajectory_join(
          "stage_attempt",
          stage$attempt_id,
          "contains",
          output_type,
          output_id,
          "exact_identity",
          c("output_reference.kind", "output_reference.ids")
        ))
      }
    }
  }
  for (evidence in review@evidence$items) {
    add(tempest_trajectory_join(
      "product",
      run_id,
      "contains",
      evidence$record_type,
      evidence$record_id,
      "authority_validated",
      c("research_run_id", "record_id")
    ))
  }
  agents_complete <- identical(review@agent_runs$omitted, 0L)
  if (agents_complete) {
    for (child in review@agent_runs$items) {
      if (!identical(child$trace_type, "deputy_delegation")) {
        next
      }
      parents <- Filter(
        function(parent) {
          identical(parent$trace_type, "deputy_run") &&
            identical(parent$deputy_run_id, child$parent_run_id) &&
            identical(parent$agent_id, child$parent_agent_id) &&
            identical(parent$correlation_id, child$correlation_id)
        },
        review@agent_runs$items
      )
      if (length(parents) != 1L) {
        next
      }
      parent <- parents[[1L]]
      add(tempest_trajectory_join(
        "deputy_run",
        parent$deputy_run_id,
        "parent_of",
        "deputy_run",
        child$deputy_run_id,
        "exact_identity",
        c(
          "parent_agent_id",
          "parent_run_id",
          "delegation_id",
          "tool_call_id"
        )
      ))
      add(tempest_trajectory_join(
        "deputy_run",
        parent$deputy_run_id,
        "correlated_with",
        "deputy_run",
        child$deputy_run_id,
        "correlation_only",
        "correlation_id"
      ))
    }
  }
  proposal <- review@knowledge$proposal
  if (!is.null(proposal)) {
    add(tempest_trajectory_join(
      "product",
      run_id,
      "proposed_as",
      "promotion_bundle",
      proposal$bundle_id,
      "authority_validated",
      c("research_run_id", "bundle_id", "claim_ids")
    ))
  }
  for (join in tempest_trajectory_artifact_joins(review@knowledge, run_id)) {
    add(join)
  }
  tempest_trajectory_unique_records(expected)
}

tempest_trajectory_validate_output_joins <- function(review) {
  joins_complete <- identical(review@joins$omitted, 0L)
  for (stage in review@stages$items) {
    output_joins <- Filter(
      function(join) {
        identical(join$from_type, "stage_attempt") &&
          identical(join$from_id, stage$attempt_id) &&
          identical(join$relation, "contains")
      },
      review@joins$items
    )
    count_invalid <- if (joins_complete) {
      length(output_joins) != stage$output$count
    } else {
      length(output_joins) > stage$output$count
    }
    if (count_invalid) {
      tempest_trajectory_review_abort(
        "Trajectory stage output joins do not match its declared count."
      )
    }
    if (length(output_joins) == 0L) {
      next
    }
    output_type <- tempest_trajectory_stage_output_type(stage$output$kind)
    valid <- vapply(
      output_joins,
      function(join) {
        identical(join$to_type, output_type) &&
          identical(join$proof$kind, "exact_identity") &&
          identical(
            join$proof$matched_fields,
            as.list(c("output_reference.kind", "output_reference.ids"))
          )
      },
      logical(1)
    )
    if (!all(valid)) {
      tempest_trajectory_review_abort(
        "Trajectory stage output joins do not match its output contract."
      )
    }
    expected_ids <- tempest_trajectory_stage_projected_output_ids(stage)
    if (!is.null(expected_ids)) {
      output_ids <- vapply(output_joins, `[[`, character(1), "to_id")
      identities_invalid <- if (joins_complete) {
        !setequal(output_ids, expected_ids)
      } else {
        !all(output_ids %in% expected_ids)
      }
      if (identities_invalid) {
        tempest_trajectory_review_abort(
          "Trajectory stage output joins do not match its fixed identities."
        )
      }
    }
  }
  complete_source_lanes <- all(vapply(
    c("stages", "evidence", "joins"),
    \(lane) identical(S7::prop(review, lane)$omitted, 0L),
    logical(1)
  ))
  if (complete_source_lanes) {
    coverage_contracts <- list(
      extract_claims = "claim",
      verify_claim_support = "claim_support"
    )
    for (stage_name in names(coverage_contracts)) {
      record_type <- coverage_contracts[[stage_name]]
      stage_ids <- vapply(
        Filter(
          \(stage) identical(stage$stage, stage_name),
          review@stages$items
        ),
        `[[`,
        character(1),
        "attempt_id"
      )
      output_ids <- vapply(
        Filter(
          function(join) {
            identical(join$from_type, "stage_attempt") &&
              join$from_id %in% stage_ids &&
              identical(join$relation, "contains") &&
              identical(join$to_type, record_type)
          },
          review@joins$items
        ),
        `[[`,
        character(1),
        "to_id"
      )
      evidence_ids <- vapply(
        Filter(
          \(item) identical(item$record_type, record_type),
          review@evidence$items
        ),
        `[[`,
        character(1),
        "record_id"
      )
      if (length(evidence_ids) == 0L || !setequal(output_ids, evidence_ids)) {
        tempest_trajectory_review_abort(
          paste0(
            "Trajectory stage outputs do not exactly cover their complete ",
            "workspace evidence."
          )
        )
      }
    }
  }
  invisible(review)
}

tempest_trajectory_validate_closed_joins <- function(review, expected) {
  key <- function(joins) {
    vapply(joins, tempest_product_canonical_json, character(1))
  }
  actual_keys <- key(review@joins$items)
  expected_keys <- key(expected)
  if (!all(expected_keys %in% actual_keys)) {
    tempest_trajectory_review_abort(
      "Trajectory joins omit a mandatory projected relation."
    )
  }
  compare_subset <- function(actual, required, noun) {
    if (!setequal(key(actual), key(required))) {
      tempest_trajectory_review_abort(
        "Trajectory {noun} joins do not match the complete projection."
      )
    }
  }
  stages_complete <- identical(review@stages$omitted, 0L)
  evidence_complete <- identical(review@evidence$omitted, 0L)
  agents_complete <- identical(review@agent_runs$omitted, 0L)
  if (stages_complete && evidence_complete) {
    compare_subset(
      Filter(\(join) identical(join$from_type, "product"), review@joins$items),
      Filter(\(join) identical(join$from_type, "product"), expected),
      "product"
    )
  }
  if (stages_complete) {
    is_program_join <- function(join) {
      identical(join$relation, "executed_as") &&
        identical(join$to_type, "program_artifact")
    }
    compare_subset(
      Filter(is_program_join, review@joins$items),
      Filter(is_program_join, expected),
      "program"
    )
    is_stage_deputy_join <- function(join) {
      identical(join$from_type, "stage_attempt") &&
        identical(join$to_type, "deputy_run") &&
        join$relation %in% c("executed_as", "correlated_with")
    }
    compare_subset(
      Filter(is_stage_deputy_join, review@joins$items),
      Filter(is_stage_deputy_join, expected),
      "stage Deputy binding"
    )
  }
  if (agents_complete) {
    is_lineage_join <- function(join) {
      identical(join$from_type, "deputy_run") &&
        join$relation %in% c("parent_of", "correlated_with")
    }
    compare_subset(
      Filter(is_lineage_join, review@joins$items),
      Filter(is_lineage_join, expected),
      "Deputy lineage"
    )
  }
  {
    compare_subset(
      Filter(
        \(join) join$relation %in% c("published_as", "accepted_as"),
        review@joins$items
      ),
      Filter(
        \(join) join$relation %in% c("published_as", "accepted_as"),
        expected
      ),
      "promotion acceptance"
    )
  }
  invisible(review)
}

tempest_trajectory_validate_join_graph <- function(review) {
  evidence_types <- tempest_trajectory_evidence_types()
  evidence_ids <- stats::setNames(
    lapply(evidence_types, \(type) character()),
    evidence_types
  )
  for (item in review@evidence$items) {
    evidence_ids[[item$record_type]] <- c(
      evidence_ids[[item$record_type]],
      item$record_id
    )
  }
  proposal <- review@knowledge$proposal
  acceptance <- review@knowledge$acceptance
  identities <- c(
    list(
      product = review@product$research_run_id,
      stage_attempt = vapply(
        review@stages$items,
        `[[`,
        character(1),
        "attempt_id"
      ),
      deputy_run = vapply(
        review@agent_runs$items,
        `[[`,
        character(1),
        "deputy_run_id"
      ),
      program_artifact = vapply(
        review@programs,
        `[[`,
        character(1),
        "program_artifact_id"
      ),
      promotion_bundle = if (is.null(proposal)) {
        character()
      } else {
        proposal$bundle_id
      },
      artifact_selection = unique(c(
        review@knowledge$input_selection$selection_id %||% character(),
        proposal$selection_id %||% character()
      )),
      artifact_decision = acceptance$decision_id %||% character()
    ),
    evidence_ids
  )
  complete <- stats::setNames(
    as.list(rep(TRUE, length(identities))),
    names(identities)
  )
  complete$stage_attempt <- identical(review@stages$omitted, 0L)
  complete$deputy_run <- identical(review@agent_runs$omitted, 0L)
  for (type in evidence_types) {
    complete[[type]] <- identical(review@evidence$omitted, 0L)
  }
  unprojected_output_types <- c("product_field", "output_digest")
  endpoint_resolves <- function(type, id) {
    if (type %in% unprojected_output_types) {
      return(TRUE)
    }
    known <- identities[[type]]
    if (is.null(known)) {
      return(FALSE)
    }
    id %in% known || !isTRUE(complete[[type]])
  }
  for (stage in review@stages$items) {
    binding <- stage$deputy_binding
    if (is.null(binding)) {
      next
    }
    agents <- Filter(
      \(agent) identical(agent$deputy_run_id, binding$run_id),
      review@agent_runs$items
    )
    if (length(agents) == 0L && !isTRUE(complete$deputy_run)) {
      next
    }
    if (length(agents) == 1L) {
      agent <- agents[[1L]]
    }
    expected_expert <- if (
      length(agents) == 1L && identical(agent$role, "moderator")
    ) {
      "moderator"
    } else if (length(agents) == 1L) {
      agent$expert_id
    } else {
      NULL
    }
    lineage <- c("parent_run_id", "delegation_id", "tool_call_id")
    valid_binding <- length(agents) == 1L &&
      identical(agent$status, "complete") &&
      identical(agent$completion_disposition, "issued") &&
      identical(agent$deputy_session_id, binding$session_id) &&
      identical(expected_expert, binding$expert_id) &&
      (is.null(binding$correlation_id) ||
        identical(agent$correlation_id, binding$correlation_id)) &&
      identical(unname(agent[lineage]), unname(binding[lineage])) &&
      (!identical(review@product$mode, "storm") ||
        (identical(stage$stage, "extract_claims") &&
          !is.null(binding$correlation_id) &&
          identical(agent$stage, "research") &&
          identical(agent$role, "expert")))
    if (!valid_binding) {
      tempest_trajectory_review_abort(
        "A trajectory stage Deputy binding does not resolve its exact run."
      )
    }
  }
  if (
    identical(review@product$mode, "storm") &&
      identical(review@stages$omitted, 0L)
  ) {
    bound_runs <- vapply(
      review@stages$items,
      \(stage) stage$deputy_binding$run_id %||% NA_character_,
      character(1)
    )
    if (anyDuplicated(bound_runs[!is.na(bound_runs)])) {
      tempest_trajectory_review_abort(
        "Trajectory STORM stage Deputy bindings must be unique."
      )
    }
  }
  for (join in review@joins$items) {
    expected_proof <- tempest_trajectory_join_contract(
      join$from_type,
      join$relation,
      join$to_type
    )
    if (is.null(expected_proof)) {
      tempest_trajectory_review_abort(
        "A trajectory join uses an invalid typed relation."
      )
    }
    if (!identical(join$proof, expected_proof)) {
      tempest_trajectory_review_abort(
        "A retained trajectory join does not match its fixed proof contract."
      )
    }
    if (
      !endpoint_resolves(join$from_type, join$from_id) ||
        !endpoint_resolves(join$to_type, join$to_id)
    ) {
      tempest_trajectory_review_abort(
        "A trajectory join endpoint does not resolve a projected identity."
      )
    }
    if (
      identical(join$relation, "executed_as") &&
        identical(join$to_type, "program_artifact")
    ) {
      stage_matches <- Filter(
        \(stage) identical(stage$attempt_id, join$from_id),
        review@stages$items
      )
      if (
        length(stage_matches) == 1L &&
          (!identical(
            join$to_id,
            stage_matches[[1L]]$program_artifact_id
          ) ||
            !identical(join$proof$kind, "authority_validated") ||
            !identical(
              join$proof$matched_fields,
              as.list(c(
                "stage",
                "program_artifact_id",
                "contract_version",
                "evaluator_id",
                "evaluator_version"
              ))
            ))
      ) {
        tempest_trajectory_review_abort(
          "A trajectory program join does not match its stage binding."
        )
      }
    }
    if (
      identical(join$from_type, "stage_attempt") &&
        identical(join$to_type, "deputy_run") &&
        join$relation %in% c("executed_as", "correlated_with")
    ) {
      stage_matches <- Filter(
        \(stage) identical(stage$attempt_id, join$from_id),
        review@stages$items
      )
      if (length(stage_matches) == 1L) {
        stage <- stage_matches[[1L]]
        binding <- stage$deputy_binding
        expected_proof <- if (identical(join$relation, "executed_as")) {
          list(
            kind = "authority_validated",
            matched_fields = as.list(c("deputy_run_id", "deputy_session_id"))
          )
        } else {
          list(
            kind = "correlation_only",
            matched_fields = list("correlation_id")
          )
        }
        if (
          is.null(binding) ||
            !identical(join$to_id, binding$run_id) ||
            !identical(join$proof, expected_proof) ||
            (identical(join$relation, "correlated_with") &&
              is.null(binding$correlation_id))
        ) {
          tempest_trajectory_review_abort(
            "A trajectory stage Deputy join does not match its retained binding."
          )
        }
      }
    }
  }
  tempest_trajectory_validate_output_joins(review)
  expected <- tempest_trajectory_mandatory_joins(review)
  retained_stage_bindings <- Filter(
    function(join) {
      identical(join$from_type, "stage_attempt") &&
        identical(join$to_type, "deputy_run") &&
        join$relation %in% c("executed_as", "correlated_with") &&
        join$from_id %in%
          vapply(
            review@stages$items,
            `[[`,
            character(1),
            "attempt_id"
          )
    },
    review@joins$items
  )
  expected_keys <- vapply(
    expected,
    tempest_product_canonical_json,
    character(1)
  )
  retained_stage_keys <- vapply(
    retained_stage_bindings,
    tempest_product_canonical_json,
    character(1)
  )
  if (!all(retained_stage_keys %in% expected_keys)) {
    tempest_trajectory_review_abort(
      "A retained trajectory stage Deputy join is not authoritative."
    )
  }
  if (identical(review@agent_runs$omitted, 0L)) {
    retained_lineage <- Filter(
      function(join) {
        identical(join$from_type, "deputy_run") &&
          join$relation %in% c("parent_of", "correlated_with")
      },
      review@joins$items
    )
    retained_keys <- vapply(
      retained_lineage,
      tempest_product_canonical_json,
      character(1)
    )
    if (!all(retained_keys %in% expected_keys)) {
      tempest_trajectory_review_abort(
        "A retained trajectory Deputy lineage join is not authoritative."
      )
    }
  }
  if (identical(review@joins$omitted, 0L)) {
    tempest_trajectory_validate_closed_joins(review, expected)
  }
  invisible(review)
}

tempest_trajectory_validate_finding <- function(finding) {
  severities <- tempest_trajectory_finding_severities()
  tempest_trajectory_scalar_string(
    finding$code,
    "Trajectory finding code"
  )
  tempest_trajectory_scalar_string(
    finding$severity,
    "Trajectory finding severity"
  )
  if (
    !finding$code %in% names(severities) ||
      !identical(finding$severity, unname(severities[[finding$code]]))
  ) {
    tempest_trajectory_review_abort(
      "Trajectory finding code and severity are not the closed mapping."
    )
  }
  tempest_trajectory_scalar_string(
    finding$ref_type,
    "Trajectory finding ref_type"
  )
  tempest_trajectory_scalar_string(
    finding$ref_id,
    "Trajectory finding ref_id"
  )
  tempest_trajectory_validate_opaque_identifier(
    finding$ref_id,
    "Trajectory finding ref_id"
  )
  invisible(finding)
}

tempest_trajectory_validate_finding_graph <- function(review) {
  stage_ids <- vapply(
    review@stages$items,
    `[[`,
    character(1),
    "attempt_id"
  )
  agent_ids <- vapply(
    review@agent_runs$items,
    `[[`,
    character(1),
    "deputy_run_id"
  )
  for (finding in review@findings$items) {
    expected_type <- if (identical(finding$code, "unmatched_reference")) {
      "deputy_run"
    } else {
      "stage_attempt"
    }
    if (!identical(finding$ref_type, expected_type)) {
      tempest_trajectory_review_abort(
        "A trajectory finding has an invalid reference type."
      )
    }
    known_ids <- if (identical(expected_type, "stage_attempt")) {
      stage_ids
    } else {
      agent_ids
    }
    complete <- if (identical(expected_type, "stage_attempt")) {
      identical(review@stages$omitted, 0L)
    } else {
      identical(review@agent_runs$omitted, 0L)
    }
    if (!finding$ref_id %in% known_ids && complete) {
      tempest_trajectory_review_abort(
        "A trajectory finding does not resolve a projected identity."
      )
    }
  }
  severities <- tempest_trajectory_finding_severities()
  canonical_findings <- function(items) {
    tempest_trajectory_collection(
      tempest_trajectory_unique_records(items),
      preserve_order = FALSE
    )$items
  }
  finding_keys <- function(items) {
    vapply(items, tempest_product_canonical_json, character(1))
  }
  expected_stage <- unname(unlist(
    lapply(review@stages$items, function(stage) {
      conditions <- c(
        stage_failed = identical(stage$status, "failed"),
        stage_cancelled = identical(stage$status, "cancelled"),
        fallback_taken = isTRUE(stage$fallback_taken),
        exploratory_execution = identical(
          stage$execution_path,
          "exploratory"
        ),
        support_unverified = !identical(stage$support_status, "verified"),
        publication_blocked = !isTRUE(stage$publication_allowed)
      )
      lapply(names(conditions)[conditions], function(code) {
        list(
          code = code,
          severity = unname(severities[[code]]),
          ref_type = "stage_attempt",
          ref_id = stage$attempt_id
        )
      })
    }),
    recursive = FALSE
  ))
  expected_stage <- canonical_findings(expected_stage)
  stage_findings <- Filter(
    \(finding) !identical(finding$code, "unmatched_reference"),
    review@findings$items
  )
  retained_stage_findings <- Filter(
    \(finding) finding$ref_id %in% stage_ids,
    stage_findings
  )
  if (
    !all(
      finding_keys(retained_stage_findings) %in%
        finding_keys(expected_stage)
    ) ||
      (identical(review@stages$omitted, 0L) &&
        identical(review@findings$omitted, 0L) &&
        !identical(stage_findings, expected_stage))
  ) {
    tempest_trajectory_review_abort(
      "Trajectory stage findings do not match the projected trust state."
    )
  }

  agents_complete <- identical(review@agent_runs$omitted, 0L)
  joins_complete <- identical(review@joins$omitted, 0L)
  if (agents_complete && joins_complete) {
    linked_agent_ids <- character()
    for (join in review@joins$items) {
      if (
        identical(join$relation, "executed_as") &&
          identical(join$to_type, "deputy_run")
      ) {
        linked_agent_ids <- c(linked_agent_ids, join$to_id)
      }
      if (identical(join$relation, "parent_of")) {
        linked_agent_ids <- c(linked_agent_ids, join$from_id, join$to_id)
      }
    }
    unmatched <- setdiff(agent_ids, unique(linked_agent_ids))
    expected_unmatched <- lapply(unmatched, function(run_id) {
      list(
        code = "unmatched_reference",
        severity = "info",
        ref_type = "deputy_run",
        ref_id = run_id
      )
    })
    expected_unmatched <- canonical_findings(expected_unmatched)
    actual_unmatched <- Filter(
      \(finding) identical(finding$code, "unmatched_reference"),
      review@findings$items
    )
    if (
      !all(
        finding_keys(actual_unmatched) %in% finding_keys(expected_unmatched)
      ) ||
        (identical(review@findings$omitted, 0L) &&
          !identical(actual_unmatched, expected_unmatched))
    ) {
      tempest_trajectory_review_abort(
        "Trajectory unmatched findings do not match the projected join state."
      )
    }
  }
  invisible(review)
}

tempest_trajectory_review_payload <- function(
  schema_version,
  product,
  stages,
  agent_runs,
  programs,
  knowledge,
  evidence,
  joins,
  findings
) {
  list(
    schema_version = schema_version,
    product = product,
    stages = stages,
    agent_runs = agent_runs,
    programs = programs,
    knowledge = knowledge,
    evidence = evidence,
    joins = joins,
    findings = findings
  )
}

tempest_trajectory_review_validation_message <- function(self) {
  tryCatch(
    {
      if (!identical(self@schema_version, tempest_trajectory_schema_version)) {
        stop("schema_version must be the current trajectory projection")
      }
      tempest_trajectory_validate_sha256(
        self@review_id,
        "Trajectory review review_id"
      )
      tempest_trajectory_validate_product(self@product)
      tempest_trajectory_validate_knowledge(
        self@knowledge,
        self@product$research_run_id
      )
      tempest_trajectory_validate_programs(
        self@programs
      )
      tempest_trajectory_validate_collection(
        self@stages,
        tempest_trajectory_stage_fields(),
        "Trajectory stages"
      )
      invisible(lapply(
        self@stages$items,
        tempest_trajectory_validate_stage,
        programs = self@programs,
        product = self@product
      ))
      stage_attempt_ids <- vapply(
        self@stages$items,
        `[[`,
        character(1),
        "attempt_id"
      )
      if (anyDuplicated(stage_attempt_ids)) {
        tempest_trajectory_review_abort(
          "Trajectory stage attempt identities must be unique."
        )
      }
      stage_order <- tempest_stage_records_order_fields(
        vapply(
          self@stages$items,
          `[[`,
          character(1),
          "started_at"
        ),
        stage_attempt_ids
      )
      if (!identical(stage_order, seq_along(self@stages$items))) {
        tempest_trajectory_review_abort(
          paste0(
            "Trajectory stages must retain canonical started_at and ",
            "attempt_id order."
          )
        )
      }
      tempest_trajectory_validate_collection(
        self@agent_runs,
        tempest_trajectory_agent_fields(),
        "Trajectory agent runs"
      )
      tempest_trajectory_validate_canonical_set(
        self@agent_runs,
        "Trajectory agent runs"
      )
      invisible(lapply(
        self@agent_runs$items,
        tempest_trajectory_validate_agent,
        product = self@product,
        knowledge = self@knowledge
      ))
      deputy_run_ids <- vapply(
        self@agent_runs$items,
        `[[`,
        character(1),
        "deputy_run_id"
      )
      if (anyDuplicated(deputy_run_ids)) {
        tempest_trajectory_review_abort(
          "Trajectory Deputy run identities must be unique."
        )
      }
      deputy_trace_ids <- vapply(
        self@agent_runs$items,
        `[[`,
        character(1),
        "trace_id"
      )
      stage_trace_ids <- vapply(
        self@stages$items,
        `[[`,
        character(1),
        "trace_id"
      )
      if (anyDuplicated(c(stage_trace_ids, deputy_trace_ids))) {
        tempest_trajectory_review_abort(
          "Trajectory trace identities must be unique across source lanes."
        )
      }
      if (identical(self@agent_runs$omitted, 0L)) {
        tempest_product_authority_validate_delegation_parents(
          self@agent_runs$items,
          tempest_trajectory_review_abort,
          "A trajectory delegation does not resolve one exact parent."
        )
      }
      tempest_trajectory_validate_collection(
        self@evidence,
        tempest_trajectory_evidence_fields(),
        "Trajectory evidence"
      )
      tempest_trajectory_validate_canonical_set(
        self@evidence,
        "Trajectory evidence"
      )
      invisible(lapply(
        self@evidence$items,
        tempest_trajectory_validate_evidence
      ))
      tempest_trajectory_validate_claim_selection(
        self@knowledge,
        self@evidence
      )
      tempest_trajectory_validate_collection(
        self@joins,
        tempest_trajectory_join_fields(),
        "Trajectory joins"
      )
      tempest_trajectory_validate_canonical_set(
        self@joins,
        "Trajectory joins"
      )
      invisible(lapply(self@joins$items, tempest_trajectory_validate_join))
      tempest_trajectory_validate_join_graph(self)
      tempest_trajectory_validate_collection(
        self@findings,
        tempest_trajectory_finding_fields(),
        "Trajectory findings"
      )
      tempest_trajectory_validate_canonical_set(
        self@findings,
        "Trajectory findings"
      )
      invisible(lapply(
        self@findings$items,
        tempest_trajectory_validate_finding
      ))
      tempest_trajectory_validate_finding_graph(self)
      payload <- tempest_trajectory_review_payload(
        self@schema_version,
        self@product,
        self@stages,
        self@agent_runs,
        self@programs,
        self@knowledge,
        self@evidence,
        self@joins,
        self@findings
      )
      tempest_trajectory_plain_value(payload)
      if (!identical(self@review_id, tempest_trajectory_digest(payload))) {
        stop("review_id does not match the complete bounded projection")
      }
      NULL
    },
    error = conditionMessage
  )
}
