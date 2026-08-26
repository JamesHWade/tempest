# Bounded, read-only trajectory review for completed Tempest products.

tempest_trajectory_schema_version <- 1L
tempest_trajectory_max_records <- 250L

tempest_trajectory_review_abort <- function(
  message,
  ...,
  class = character(),
  parent = NULL,
  .envir = rlang::caller_env()
) {
  tempest_abort(
    message,
    ...,
    class = unique(c(
      class,
      "tempest_trajectory_review_error",
      "tempest_error"
    )),
    parent = parent,
    .envir = .envir
  )
}

tempest_trajectory_review_fields <- function() {
  c(
    "schema_version",
    "review_id",
    "product",
    "stages",
    "agent_runs",
    "programs",
    "knowledge",
    "evidence",
    "joins",
    "findings"
  )
}

tempest_trajectory_collection_fields <- function() {
  c("total", "retained", "omitted", "digest", "items")
}

tempest_trajectory_product_fields <- function() {
  c(
    "research_run_id",
    "mode",
    "status",
    "config_digest",
    "report_reference"
  )
}

tempest_trajectory_stage_fields <- function() {
  c(
    "stage",
    "attempt_id",
    "trace_id",
    "deputy_binding",
    "status",
    "started_at",
    "completed_at",
    "output",
    "program_artifact_id",
    "governed_procedure_revision_id",
    "failure_class",
    "fallback_policy",
    "fallback_implementation",
    "fallback_taken",
    "execution_path",
    "support_status",
    "publication_allowed"
  )
}

tempest_trajectory_stage_output_fields <- function() {
  c("kind", "count", "digest")
}

tempest_trajectory_stage_deputy_fields <- function() {
  c(
    "run_id",
    "session_id",
    "expert_id",
    "correlation_id",
    "parent_run_id",
    "delegation_id",
    "tool_call_id"
  )
}

tempest_trajectory_agent_fields <- function() {
  c(
    "trace_id",
    "trace_type",
    "stage",
    "role",
    "status",
    "completion_disposition",
    "agent_id",
    "expert_id",
    "deputy_run_id",
    "deputy_session_id",
    "parent_agent_id",
    "parent_run_id",
    "delegation_id",
    "tool_call_id",
    "program_artifact_id",
    "correlation_id"
  )
}

tempest_trajectory_program_fields <- function() {
  c(
    "stage",
    "contract_version",
    "program_artifact_id",
    "evaluator_id",
    "evaluator_version",
    "governed_procedure_ref"
  )
}

tempest_trajectory_knowledge_fields <- function() {
  c("input_snapshot", "promotion_state", "proposal", "acceptance")
}

tempest_trajectory_snapshot_fields <- function() {
  c(
    "schema_version",
    "snapshot_id",
    "store_id",
    "store_format_version",
    "schema_build_digest",
    "commit_order",
    "batch_id",
    "committed_at",
    "history_complete"
  )
}

tempest_trajectory_proposal_fields <- function() {
  c(
    "bundle_id",
    "research_run_id",
    "schema_build_digest",
    "claim_selection"
  )
}

tempest_trajectory_selection_fields <- function() {
  c("kind", "count", "digest")
}

tempest_trajectory_acceptance_fields <- function() {
  c(
    "receipt_id",
    "bundle_id",
    "plan_id",
    "plan_digest",
    "batch_id",
    "store_id",
    "schema_build_digest",
    "snapshot",
    "counts",
    "record_revisions"
  )
}

tempest_trajectory_evidence_fields <- function() {
  c("record_type", "record_id")
}

tempest_trajectory_join_fields <- function() {
  c("from_type", "from_id", "relation", "to_type", "to_id", "proof")
}

tempest_trajectory_proof_fields <- function() {
  c("kind", "matched_fields")
}

tempest_trajectory_finding_fields <- function() {
  c("code", "severity", "ref_type", "ref_id")
}

tempest_trajectory_relations <- function() {
  c(
    "contains",
    "executed_as",
    "correlated_with",
    "read_from",
    "proposed_as",
    "accepted_as",
    "parent_of"
  )
}

tempest_trajectory_proof_kinds <- function() {
  c("authority_validated", "exact_identity", "correlation_only")
}

tempest_trajectory_finding_severities <- function() {
  c(
    stage_failed = "error",
    stage_cancelled = "warning",
    fallback_taken = "warning",
    exploratory_execution = "info",
    support_unverified = "warning",
    publication_blocked = "warning",
    unmatched_reference = "info"
  )
}

tempest_trajectory_evidence_types <- function() {
  c("resource", "claim", "evidence_span", "claim_support", "dispute")
}

tempest_trajectory_digest <- function(value) {
  paste0("sha256:", tempest_product_record_hash(value))
}

tempest_trajectory_nullable <- function(value) {
  if (
    is.null(value) ||
      (length(value) == 1L && is.atomic(value) && is.na(value))
  ) {
    return(NULL)
  }
  unname(value)
}

tempest_trajectory_exact_record <- function(value, fields, noun) {
  if (
    !is.list(value) ||
      is.data.frame(value) ||
      is.object(value) ||
      !identical(names(value), fields)
  ) {
    tempest_trajectory_review_abort(
      "{noun} must contain exactly the closed trajectory fields."
    )
  }
  value
}

tempest_trajectory_path_like_values <- function(value, path = "review") {
  if (is.list(value)) {
    value_names <- names(value)
    child_paths <- if (is.null(value_names)) {
      paste0(path, "[[", seq_along(value), "]]")
    } else {
      paste0(path, "$", value_names)
    }
    return(unlist(
      Map(tempest_trajectory_path_like_values, value, child_paths),
      use.names = FALSE
    ))
  }
  if (!is.character(value) || length(value) == 0L) {
    return(character())
  }
  path_like <- grepl(
    "^(?:/|~[/\\\\]|[A-Za-z]:[/\\\\]|file://|\\\\\\\\)",
    value,
    perl = TRUE
  )
  if (any(path_like, na.rm = TRUE)) path else character()
}

tempest_trajectory_plain_value <- function(value, path = "review") {
  tryCatch(
    tempest_product_canonical_value(value),
    error = function(error) {
      tempest_trajectory_review_abort(
        "Trajectory values must be plain canonical records.",
        parent = error
      )
    }
  )
  unsafe <- c(
    tempest_contract_sensitive_names(value, path),
    tempest_contract_sensitive_values(value, path),
    tempest_trajectory_path_like_values(value, path)
  )
  if (length(unsafe) > 0L) {
    tempest_trajectory_review_abort(
      "Trajectory values contain credential-like content or a local path."
    )
  }
  invisible(value)
}

tempest_trajectory_scalar_string <- function(value, noun, nullable = FALSE) {
  if (is.null(value) && isTRUE(nullable)) {
    return(invisible(value))
  }
  if (
    !rlang::is_string(value) ||
      is.na(value) ||
      !nzchar(trimws(value))
  ) {
    tempest_trajectory_review_abort("{noun} must be one non-empty string.")
  }
  invisible(value)
}

tempest_trajectory_validate_sha256 <- function(value, noun) {
  tempest_trajectory_scalar_string(value, noun)
  if (!grepl("^sha256:[a-f0-9]{64}$", value)) {
    tempest_trajectory_review_abort("{noun} must be one SHA-256 identity.")
  }
  invisible(value)
}

tempest_trajectory_whole_number <- function(value, noun) {
  if (
    !is.integer(value) ||
      is.object(value) ||
      !is.null(names(value)) ||
      length(value) != 1L ||
      is.na(value) ||
      value < 0L
  ) {
    tempest_trajectory_review_abort("{noun} must be one nonnegative integer.")
  }
  invisible(value)
}

tempest_trajectory_exact_whole_number <- function(
  value,
  noun,
  minimum = 0
) {
  if (
    !is.numeric(value) ||
      is.object(value) ||
      !is.null(names(value)) ||
      length(value) != 1L ||
      is.na(value) ||
      !is.finite(value) ||
      value < minimum ||
      value != trunc(value)
  ) {
    tempest_trajectory_review_abort(
      "{noun} must be one whole number no less than {minimum}."
    )
  }
  invisible(value)
}

tempest_trajectory_collection <- function(items, preserve_order = TRUE) {
  if (!rlang::is_bool(preserve_order)) {
    tempest_trajectory_review_abort(
      "{.arg preserve_order} must be `TRUE` or `FALSE`."
    )
  }
  if (
    !is.list(items) ||
      is.data.frame(items) ||
      is.object(items) ||
      !is.null(names(items))
  ) {
    tempest_trajectory_review_abort(
      "Trajectory collections must be unnamed lists of plain records."
    )
  }
  tempest_trajectory_plain_value(items, "collection")
  if (length(items) > .Machine$integer.max) {
    tempest_trajectory_review_abort("A trajectory collection is too large.")
  }
  if (!preserve_order && length(items) > 1L) {
    keys <- vapply(items, tempest_product_canonical_json, character(1))
    items <- items[order(keys, method = "radix")]
  }
  total <- as.integer(length(items))
  retained <- min(total, tempest_trajectory_max_records)
  list(
    total = total,
    retained = as.integer(retained),
    omitted = as.integer(total - retained),
    digest = tempest_trajectory_digest(items),
    items = unname(utils::head(items, retained))
  )
}

tempest_trajectory_validate_collection <- function(
  value,
  item_fields,
  noun
) {
  tempest_trajectory_exact_record(
    value,
    tempest_trajectory_collection_fields(),
    noun
  )
  tempest_trajectory_whole_number(value$total, paste(noun, "total"))
  tempest_trajectory_whole_number(value$retained, paste(noun, "retained"))
  tempest_trajectory_whole_number(value$omitted, paste(noun, "omitted"))
  if (
    value$total != value$retained + value$omitted ||
      value$retained != min(value$total, tempest_trajectory_max_records) ||
      !is.list(value$items) ||
      is.data.frame(value$items) ||
      is.object(value$items) ||
      !is.null(names(value$items)) ||
      length(value$items) != value$retained
  ) {
    tempest_trajectory_review_abort(
      "{noun} counts do not match its bounded retained items."
    )
  }
  tempest_trajectory_scalar_string(value$digest, paste(noun, "digest"))
  if (!grepl("^sha256:[a-f0-9]{64}$", value$digest)) {
    tempest_trajectory_review_abort("{noun} has an invalid complete digest.")
  }
  invisible(lapply(
    value$items,
    tempest_trajectory_exact_record,
    fields = item_fields,
    noun = paste(noun, "item")
  ))
  if (
    identical(value$omitted, 0L) &&
      !identical(value$digest, tempest_trajectory_digest(value$items))
  ) {
    tempest_trajectory_review_abort(
      "{noun} digest does not match its complete retained items."
    )
  }
  invisible(value)
}

tempest_trajectory_validate_canonical_set <- function(value, noun) {
  if (length(value$items) < 2L) {
    return(invisible(value))
  }
  keys <- vapply(
    value$items,
    tempest_product_canonical_json,
    character(1)
  )
  if (anyDuplicated(keys)) {
    tempest_trajectory_review_abort(
      "{noun} retained items must be unique."
    )
  }
  if (!identical(order(keys, method = "radix"), seq_along(keys))) {
    tempest_trajectory_review_abort(
      "{noun} retained items must use canonical order."
    )
  }
  invisible(value)
}

tempest_trajectory_unique_records <- function(records) {
  if (length(records) < 2L) {
    return(records)
  }
  keys <- vapply(records, tempest_product_canonical_json, character(1))
  records[!duplicated(keys)]
}

tempest_trajectory_snapshot <- function(snapshot) {
  if (length(snapshot) == 0L) {
    return(NULL)
  }
  fields <- tempest_trajectory_snapshot_fields()
  stats::setNames(
    lapply(fields, \(field) tempest_trajectory_nullable(snapshot[[field]])),
    fields
  )
}

tempest_trajectory_stage_output <- function(record) {
  reference <- record@output_reference
  if (length(reference) == 0L) {
    return(list(
      kind = NULL,
      count = 0L,
      digest = tempest_trajectory_digest(list())
    ))
  }
  list(
    kind = reference$kind,
    count = as.integer(length(reference$ids)),
    digest = reference$content_digest
  )
}

tempest_trajectory_stage_deputy_binding <- function(record) {
  references <- record@trace_references
  if (is.null(references$deputy_run_id)) {
    return(NULL)
  }
  list(
    run_id = references$deputy_run_id,
    session_id = references$deputy_session_id,
    expert_id = tempest_trajectory_nullable(references$expert_id),
    correlation_id = tempest_trajectory_nullable(references$correlation_id),
    parent_run_id = tempest_trajectory_nullable(references$parent_run_id),
    delegation_id = tempest_trajectory_nullable(references$delegation_id),
    tool_call_id = tempest_trajectory_nullable(references$tool_call_id)
  )
}

tempest_trajectory_stage_item <- function(record) {
  references <- record@trace_references
  list(
    stage = record@stage,
    attempt_id = record@attempt_id,
    trace_id = references$trace_id,
    deputy_binding = tempest_trajectory_stage_deputy_binding(record),
    status = record@status,
    started_at = record@started_at,
    completed_at = tempest_trajectory_nullable(record@completed_at),
    output = tempest_trajectory_stage_output(record),
    program_artifact_id = record@program_artifact_id,
    governed_procedure_revision_id = tempest_trajectory_nullable(
      record@governed_procedure_revision_id
    ),
    failure_class = tempest_trajectory_nullable(record@failure_class),
    fallback_policy = record@fallback_policy,
    fallback_implementation = tempest_trajectory_nullable(
      record@fallback_implementation
    ),
    fallback_taken = record@fallback_taken,
    execution_path = record@execution_path,
    support_status = record@support_status,
    publication_allowed = record@publication_allowed
  )
}

tempest_trajectory_agent_item <- function(trace) {
  fields <- tempest_trajectory_agent_fields()
  stats::setNames(
    lapply(fields, \(field) tempest_trajectory_nullable(trace[[field]])),
    fields
  )
}

tempest_trajectory_programs <- function(manifest) {
  stages <- tempest_program_set_stages()
  references <- tempest_research_manifest_programs(manifest@programs)
  if (!identical(names(references), sort(stages, method = "radix"))) {
    tempest_trajectory_review_abort(
      "Trajectory review requires the complete ten-stage Manifest ProgramSet."
    )
  }
  references <- references[stages]
  stats::setNames(
    lapply(stages, function(stage) {
      reference <- references[[stage]]
      list(
        stage = reference$stage,
        contract_version = reference$contract_version,
        program_artifact_id = reference$program_artifact_id,
        evaluator_id = reference$evaluator_id,
        evaluator_version = reference$evaluator_version,
        governed_procedure_ref = reference$governed_procedure_ref
      )
    }),
    stages
  )
}

tempest_trajectory_evidence_items <- function(workspace) {
  collections <- list(
    resource = list(
      values = workspace$list_retrieved_resources(),
      property = "resource_id"
    ),
    claim = list(
      values = workspace$list_proposed_claims(),
      property = "claim_id"
    ),
    evidence_span = list(
      values = workspace$list_evidence_spans(),
      property = "evidence_span_id"
    ),
    claim_support = list(
      values = workspace$list_claim_supports(),
      property = "claim_support_id"
    ),
    dispute = list(
      values = workspace$list_disputes(),
      property = "dispute_id"
    )
  )
  unname(unlist(
    lapply(names(collections), function(record_type) {
      collection <- collections[[record_type]]
      lapply(collection$values, function(value) {
        list(
          record_type = record_type,
          record_id = S7::prop(value, collection$property)
        )
      })
    }),
    recursive = FALSE
  ))
}

tempest_trajectory_bundle_data <- function(research, promotion_bundle) {
  if (is.null(promotion_bundle)) {
    return(NULL)
  }
  supplied <- tryCatch(
    tempest_promotion_bundle_data(promotion_bundle),
    error = function(error) {
      tempest_trajectory_review_abort(
        "{.arg promotion_bundle} is not an exact promotion bundle.",
        parent = error
      )
    }
  )
  expected <- tryCatch(
    tempest_promotion_bundle(
      research,
      claim_ids = promotion_bundle@claim_ids
    ),
    error = function(error) {
      tempest_trajectory_review_abort(
        "Could not rebind {.arg promotion_bundle} to the completed product.",
        parent = error
      )
    }
  )
  expected <- tempest_promotion_bundle_data(expected)
  if (
    !identical(
      tempest_product_canonical_json(supplied),
      tempest_product_canonical_json(expected)
    )
  ) {
    tempest_trajectory_review_abort(
      "{.arg promotion_bundle} does not belong to the completed product."
    )
  }
  supplied
}

tempest_trajectory_receipt_data <- function(
  promotion_bundle,
  promotion_receipt
) {
  if (is.null(promotion_receipt)) {
    return(NULL)
  }
  if (is.null(promotion_bundle)) {
    tempest_trajectory_review_abort(
      "{.arg promotion_receipt} requires its exact {.arg promotion_bundle}."
    )
  }
  receipt <- tryCatch(
    tempest_promotion_receipt_data(promotion_receipt),
    error = function(error) {
      tempest_trajectory_review_abort(
        "{.arg promotion_receipt} is not an exact acceptance receipt.",
        parent = error
      )
    }
  )
  if (!identical(receipt$bundle_id, promotion_bundle$bundle_id)) {
    tempest_trajectory_review_abort(
      "{.arg promotion_receipt} does not accept the supplied bundle."
    )
  }
  receipt
}

tempest_trajectory_knowledge <- function(
  manifest,
  promotion_bundle,
  promotion_receipt
) {
  proposal <- if (is.null(promotion_bundle)) {
    NULL
  } else {
    list(
      bundle_id = promotion_bundle$bundle_id,
      research_run_id = promotion_bundle$research_run_id,
      schema_build_digest = promotion_bundle$schema_build_digest,
      claim_selection = list(
        kind = "claim_ids",
        count = as.integer(length(promotion_bundle$claim_ids)),
        digest = tempest_trajectory_digest(promotion_bundle$claim_ids)
      )
    )
  }
  acceptance <- if (is.null(promotion_receipt)) {
    NULL
  } else {
    revisions <- lapply(
      promotion_receipt$record_revisions,
      \(revision) revision[tempest_promotion_receipt_revision_fields()]
    )
    list(
      receipt_id = promotion_receipt$receipt_id,
      bundle_id = promotion_receipt$bundle_id,
      plan_id = promotion_receipt$plan_id,
      plan_digest = promotion_receipt$plan_digest,
      batch_id = promotion_receipt$batch_id,
      store_id = promotion_receipt$store_id,
      schema_build_digest = promotion_receipt$schema_build_digest,
      snapshot = promotion_receipt$snapshot,
      counts = promotion_receipt$counts,
      record_revisions = tempest_trajectory_collection(
        unname(revisions),
        preserve_order = FALSE
      )
    )
  }
  state <- if (!is.null(acceptance)) {
    "accepted"
  } else if (!is.null(proposal)) {
    "proposed"
  } else {
    "none"
  }
  list(
    input_snapshot = tempest_trajectory_snapshot(manifest@knowledge_snapshot),
    promotion_state = state,
    proposal = proposal,
    acceptance = acceptance
  )
}

tempest_trajectory_join_contract <- function(from_type, relation, to_type) {
  evidence_types <- tempest_trajectory_evidence_types()
  output_types <- c("product_field", "claim", "claim_support", "output_digest")
  contract <- if (
    identical(from_type, "product") &&
      identical(relation, "contains") &&
      identical(to_type, "stage_attempt")
  ) {
    c("authority_validated", "research_run_id", "attempt_id")
  } else if (
    identical(from_type, "product") &&
      identical(relation, "contains") &&
      to_type %in% evidence_types
  ) {
    c("authority_validated", "research_run_id", "record_id")
  } else if (
    identical(from_type, "stage_attempt") &&
      identical(relation, "contains") &&
      to_type %in% output_types
  ) {
    c("exact_identity", "output_reference.kind", "output_reference.ids")
  } else if (
    identical(from_type, "stage_attempt") &&
      identical(relation, "executed_as") &&
      identical(to_type, "program_artifact")
  ) {
    c(
      "authority_validated",
      "stage",
      "program_artifact_id",
      "contract_version",
      "evaluator_id",
      "evaluator_version"
    )
  } else if (
    identical(from_type, "stage_attempt") &&
      identical(relation, "executed_as") &&
      identical(to_type, "deputy_run")
  ) {
    c("authority_validated", "deputy_run_id", "deputy_session_id")
  } else if (
    from_type %in%
      c("stage_attempt", "deputy_run") &&
      identical(relation, "correlated_with") &&
      identical(to_type, "deputy_run")
  ) {
    c("correlation_only", "correlation_id")
  } else if (
    identical(from_type, "product") &&
      identical(relation, "read_from") &&
      identical(to_type, "graft_snapshot")
  ) {
    c(
      "authority_validated",
      "snapshot_id",
      "store_id",
      "schema_build_digest",
      "commit_order"
    )
  } else if (
    identical(from_type, "product") &&
      identical(relation, "proposed_as") &&
      identical(to_type, "promotion_bundle")
  ) {
    c("authority_validated", "research_run_id", "bundle_id", "claim_ids")
  } else if (
    identical(from_type, "promotion_bundle") &&
      identical(relation, "accepted_as") &&
      identical(to_type, "promotion_receipt")
  ) {
    c("exact_identity", "bundle_id")
  } else if (
    identical(from_type, "promotion_receipt") &&
      identical(relation, "accepted_as") &&
      identical(to_type, "graft_revision")
  ) {
    c(
      "exact_identity",
      "record_id",
      "revision_id",
      "batch_id",
      "content_digest",
      "schema_build_digest"
    )
  } else if (
    identical(from_type, "deputy_run") &&
      identical(relation, "parent_of") &&
      identical(to_type, "deputy_run")
  ) {
    c(
      "exact_identity",
      "parent_agent_id",
      "parent_run_id",
      "delegation_id",
      "tool_call_id"
    )
  } else {
    NULL
  }
  if (is.null(contract)) {
    return(NULL)
  }
  list(
    kind = contract[[1L]],
    matched_fields = unname(as.list(contract[-1L]))
  )
}

tempest_trajectory_join <- function(
  from_type,
  from_id,
  relation,
  to_type,
  to_id,
  proof_kind,
  matched_fields
) {
  proof <- list(
    kind = proof_kind,
    matched_fields = unname(as.list(matched_fields))
  )
  expected_proof <- tempest_trajectory_join_contract(
    from_type,
    relation,
    to_type
  )
  if (is.null(expected_proof) || !identical(proof, expected_proof)) {
    tempest_trajectory_review_abort(
      "An internal trajectory join does not match its fixed proof contract."
    )
  }
  list(
    from_type = from_type,
    from_id = from_id,
    relation = relation,
    to_type = to_type,
    to_id = to_id,
    proof = proof
  )
}

tempest_trajectory_stage_output_type <- function(kind) {
  switch(
    kind,
    state_field = "product_field",
    workspace_claims = "claim",
    claim_supports = "claim_support",
    content_digest = "output_digest",
    tempest_trajectory_review_abort(
      "A stage output has an unsupported trajectory identity."
    )
  )
}

tempest_trajectory_trace_key <- function(run_id, session_id) {
  tempest_product_canonical_json(list(
    deputy_run_id = run_id,
    deputy_session_id = session_id
  ))
}

tempest_trajectory_parent_key <- function(run_id, agent_id, correlation_id) {
  tempest_product_canonical_json(list(
    deputy_run_id = run_id,
    agent_id = agent_id,
    correlation_id = correlation_id
  ))
}

tempest_trajectory_joins <- function(
  context,
  agents,
  programs,
  evidence,
  bundle,
  receipt
) {
  run_id <- context$manifest@research_run_id
  joins <- list()
  add <- function(join) {
    joins[[length(joins) + 1L]] <<- join
  }
  for (record in context$stage_records) {
    add(tempest_trajectory_join(
      "product",
      run_id,
      "contains",
      "stage_attempt",
      record@attempt_id,
      "authority_validated",
      c("research_run_id", "attempt_id")
    ))
    program <- programs[[record@stage]]
    add(tempest_trajectory_join(
      "stage_attempt",
      record@attempt_id,
      "executed_as",
      "program_artifact",
      record@program_artifact_id,
      "authority_validated",
      c(
        "stage",
        "program_artifact_id",
        "contract_version",
        "evaluator_id",
        "evaluator_version"
      )
    ))
    reference <- record@output_reference
    if (length(reference) > 0L) {
      to_type <- tempest_trajectory_stage_output_type(reference$kind)
      for (output_id in unlist(reference$ids, use.names = FALSE)) {
        add(tempest_trajectory_join(
          "stage_attempt",
          record@attempt_id,
          "contains",
          to_type,
          output_id,
          "exact_identity",
          c("output_reference.kind", "output_reference.ids")
        ))
      }
    }
  }
  for (item in evidence) {
    add(tempest_trajectory_join(
      "product",
      run_id,
      "contains",
      item$record_type,
      item$record_id,
      "authority_validated",
      c("research_run_id", "record_id")
    ))
  }
  snapshot <- tempest_trajectory_snapshot(context$manifest@knowledge_snapshot)
  if (!is.null(snapshot)) {
    add(tempest_trajectory_join(
      "product",
      run_id,
      "read_from",
      "graft_snapshot",
      snapshot$snapshot_id,
      "authority_validated",
      c(
        "snapshot_id",
        "store_id",
        "schema_build_digest",
        "commit_order"
      )
    ))
  }

  linked_agents <- rep(FALSE, length(agents))
  if (length(agents) > 0L) {
    agent_keys <- vapply(
      agents,
      \(agent) {
        tempest_trajectory_trace_key(
          agent$deputy_run_id,
          agent$deputy_session_id
        )
      },
      character(1)
    )
    for (record in context$stage_records) {
      references <- record@trace_references
      run <- references$deputy_run_id %||% NULL
      session <- references$deputy_session_id %||% NULL
      if (is.null(run)) {
        next
      }
      matches <- which(agent_keys == tempest_trajectory_trace_key(run, session))
      if (length(matches) != 1L) {
        next
      }
      index <- matches[[1L]]
      agent <- agents[[index]]
      linked_agents[[index]] <- TRUE
      add(tempest_trajectory_join(
        "stage_attempt",
        record@attempt_id,
        "executed_as",
        "deputy_run",
        agent$deputy_run_id,
        "authority_validated",
        c("deputy_run_id", "deputy_session_id")
      ))
      correlation <- references$correlation_id %||% NULL
      if (
        !is.null(correlation) &&
          identical(correlation, agent$correlation_id)
      ) {
        add(tempest_trajectory_join(
          "stage_attempt",
          record@attempt_id,
          "correlated_with",
          "deputy_run",
          agent$deputy_run_id,
          "correlation_only",
          "correlation_id"
        ))
      }
    }
    parent_keys <- vapply(
      agents,
      \(agent) {
        tempest_trajectory_parent_key(
          agent$deputy_run_id,
          agent$agent_id,
          agent$correlation_id
        )
      },
      character(1)
    )
    for (index in seq_along(agents)) {
      child <- agents[[index]]
      if (!identical(child$trace_type, "deputy_delegation")) {
        next
      }
      parent_key <- tempest_trajectory_parent_key(
        child$parent_run_id,
        child$parent_agent_id,
        child$correlation_id
      )
      matches <- which(parent_keys == parent_key)
      if (length(matches) != 1L) {
        next
      }
      parent_index <- matches[[1L]]
      parent <- agents[[parent_index]]
      linked_agents[c(parent_index, index)] <- TRUE
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
  if (!is.null(bundle)) {
    add(tempest_trajectory_join(
      "product",
      run_id,
      "proposed_as",
      "promotion_bundle",
      bundle$bundle_id,
      "authority_validated",
      c("research_run_id", "bundle_id", "claim_ids")
    ))
  }
  if (!is.null(receipt)) {
    add(tempest_trajectory_join(
      "promotion_bundle",
      bundle$bundle_id,
      "accepted_as",
      "promotion_receipt",
      receipt$receipt_id,
      "exact_identity",
      "bundle_id"
    ))
    for (revision in receipt$record_revisions) {
      add(tempest_trajectory_join(
        "promotion_receipt",
        receipt$receipt_id,
        "accepted_as",
        "graft_revision",
        revision$revision_id,
        "exact_identity",
        c(
          "record_id",
          "revision_id",
          "batch_id",
          "content_digest",
          "schema_build_digest"
        )
      ))
    }
  }
  list(
    joins = tempest_trajectory_unique_records(joins),
    linked_agents = linked_agents
  )
}

tempest_trajectory_stage_finding_conditions <- function(record) {
  c(
    stage_failed = identical(record@status, "failed"),
    stage_cancelled = identical(record@status, "cancelled"),
    fallback_taken = isTRUE(record@fallback_taken),
    exploratory_execution = identical(record@execution_path, "exploratory"),
    support_unverified = !identical(record@support_status, "verified"),
    publication_blocked = !isTRUE(record@publication_allowed)
  )
}

tempest_trajectory_stage_finding_counts <- function(stage_records) {
  stage_records <- tempest_stage_records_validate(
    stage_records,
    allow_running = FALSE
  )
  codes <- names(tempest_trajectory_finding_severities())[1:6]
  stages <- tempest_program_set_stages()
  counts <- stats::setNames(
    lapply(stages, \(stage) stats::setNames(as.list(rep(0L, 6L)), codes)),
    stages
  )
  for (record in stage_records) {
    conditions <- tempest_trajectory_stage_finding_conditions(record)
    for (code in names(conditions)[conditions]) {
      counts[[record@stage]][[code]] <- counts[[record@stage]][[code]] + 1L
    }
  }
  counts
}

tempest_trajectory_findings <- function(stage_records, agents, linked_agents) {
  severities <- tempest_trajectory_finding_severities()
  findings <- unname(unlist(
    lapply(stage_records, function(record) {
      conditions <- tempest_trajectory_stage_finding_conditions(record)
      lapply(names(conditions)[conditions], function(code) {
        list(
          code = code,
          severity = unname(severities[[code]]),
          ref_type = "stage_attempt",
          ref_id = record@attempt_id
        )
      })
    }),
    recursive = FALSE
  ))
  if (length(agents) > 0L) {
    unmatched <- which(!linked_agents)
    findings <- c(
      findings,
      lapply(unmatched, function(index) {
        list(
          code = "unmatched_reference",
          severity = "info",
          ref_type = "deputy_run",
          ref_id = agents[[index]]$deputy_run_id
        )
      })
    )
  }
  tempest_trajectory_unique_records(findings)
}

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

tempest_trajectory_validate_stage <- function(stage, programs) {
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
    }
    lineage <- c("parent_run_id", "delegation_id", "tool_call_id")
    lineage_present <- !vapply(binding[lineage], is.null, logical(1))
    if (any(lineage_present) && !all(lineage_present)) {
      tempest_trajectory_review_abort(
        "A trajectory stage Deputy binding has partial delegation lineage."
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
    fixed_count <- if (is.null(output_contract$ids)) {
      NULL
    } else {
      as.integer(length(output_contract$ids))
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
  tempest_trajectory_validate_stage_trust(stage)
  invisible(stage)
}

tempest_trajectory_validate_stage_trust <- function(stage) {
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
  if (identical(stage$execution_path, "governed")) {
    if (
      !identical(stage$status, "succeeded") ||
        isTRUE(stage$fallback_taken) ||
        is.null(stage$governed_procedure_revision_id)
    ) {
      tempest_trajectory_review_abort(
        "A governed trajectory path requires terminal governed authority."
      )
    }
  } else if (!identical(stage$execution_path, policy$execution_path)) {
    tempest_trajectory_review_abort(
      "A trajectory execution path does not match its stage policy."
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
  programs,
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
  if (!agent$trace_type %in% c("deputy_run", "deputy_delegation")) {
    tempest_trajectory_review_abort("Trajectory agent trace_type is invalid.")
  }
  if (!is.null(agent$program_artifact_id)) {
    tempest_trajectory_validate_sha256(
      agent$program_artifact_id,
      "Trajectory agent program_artifact_id"
    )
    program <- programs[[agent$stage]]
    if (
      is.null(program) ||
        !identical(agent$program_artifact_id, program$program_artifact_id)
    ) {
      tempest_trajectory_review_abort(
        "A trajectory agent is not bound to its declared program artifact."
      )
    }
  }
  statuses <- c(
    "abandoned",
    "complete",
    "cost_limit",
    "error",
    "hook_requested_stop",
    "input_token_limit",
    "interrupted",
    "output_token_limit",
    "provider_error",
    "request_limit",
    "tool_call_limit",
    "total_token_limit"
  )
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
  valid_session <- if (identical(product$mode, "storm")) {
    identical(
      agent$deputy_session_id,
      tempest_storm_deputy_session_id(
        product$research_run_id,
        agent$expert_id
      )
    )
  } else if (identical(agent$role, "moderator")) {
    identical(
      agent$deputy_session_id,
      tempest_costorm_deputy_session_id(
        product$research_run_id,
        "moderator"
      )
    )
  } else {
    grepl(
      "^expert-session_[a-f0-9]{16}$",
      agent$deputy_session_id,
      perl = TRUE
    )
  }
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
  snapshot_id <- knowledge$input_snapshot$snapshot_id %||% NULL
  if (!is.null(snapshot_id)) {
    run_context$knowledge_snapshot_id <- snapshot_id
  }
  if (!is.null(agent$program_artifact_id)) {
    run_context$program_artifact_id <- agent$program_artifact_id
  }
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

tempest_trajectory_validate_snapshot <- function(
  snapshot,
  nullable = FALSE,
  complete = FALSE,
  digest_identity = FALSE
) {
  if (is.null(snapshot) && isTRUE(nullable)) {
    return(invisible(snapshot))
  }
  tempest_trajectory_exact_record(
    snapshot,
    tempest_trajectory_snapshot_fields(),
    "Trajectory snapshot"
  )
  source_snapshot <- tryCatch(
    tempest_research_manifest_knowledge_snapshot(
      snapshot[!vapply(snapshot, is.null, logical(1))]
    ),
    error = function(error) {
      tempest_trajectory_review_abort(
        "A trajectory snapshot is not a valid manifest snapshot reference.",
        parent = error
      )
    }
  )
  canonical_snapshot <- stats::setNames(
    lapply(
      tempest_trajectory_snapshot_fields(),
      \(field) tempest_trajectory_nullable(source_snapshot[[field]])
    ),
    tempest_trajectory_snapshot_fields()
  )
  if (!identical(snapshot, canonical_snapshot)) {
    tempest_trajectory_review_abort(
      "A trajectory snapshot does not retain its canonical source shape."
    )
  }
  if (digest_identity) {
    tempest_trajectory_validate_sha256(
      snapshot$snapshot_id,
      "Trajectory snapshot snapshot_id"
    )
  } else {
    tempest_trajectory_scalar_string(
      snapshot$snapshot_id,
      "Trajectory snapshot snapshot_id"
    )
  }
  for (field in c(
    "store_id",
    "store_format_version",
    "schema_build_digest",
    "batch_id",
    "committed_at"
  )) {
    tempest_trajectory_scalar_string(
      snapshot[[field]],
      paste("Trajectory snapshot", field),
      nullable = !complete
    )
  }
  if (digest_identity) {
    tempest_trajectory_validate_sha256(
      snapshot$schema_build_digest,
      "Trajectory snapshot schema_build_digest"
    )
  }
  for (field in c("schema_version", "commit_order")) {
    if (!is.null(snapshot[[field]]) || complete) {
      tempest_trajectory_exact_whole_number(
        snapshot[[field]],
        paste("Trajectory snapshot", field)
      )
    }
  }
  if (
    (!is.null(snapshot$history_complete) || complete) &&
      !rlang::is_bool(snapshot$history_complete)
  ) {
    tempest_trajectory_review_abort(
      "Trajectory snapshot history_complete must be one logical."
    )
  }
  if (
    complete &&
      (!identical(snapshot$schema_version, 1L) ||
        !identical(snapshot$store_format_version, "3.0.0") ||
        snapshot$commit_order < 1L ||
        !tempest_ledger_timestamp_valid(snapshot$committed_at))
  ) {
    tempest_trajectory_review_abort(
      "A complete trajectory snapshot has invalid receipt metadata."
    )
  }
  invisible(snapshot)
}

tempest_trajectory_validate_acceptance_counts <- function(counts) {
  actions <- c("inserted", "updated", "matched", "observed")
  tempest_trajectory_exact_record(
    counts,
    actions,
    "Trajectory acceptance counts"
  )
  classes <- tempest_promotion_receipt_classes()
  if (
    !is.list(counts$observed) ||
      is.data.frame(counts$observed) ||
      is.object(counts$observed) ||
      !identical(names(counts$observed), classes)
  ) {
    tempest_trajectory_review_abort(
      "Trajectory acceptance counts have invalid classes."
    )
  }
  for (action in actions) {
    row <- counts[[action]]
    if (
      !is.list(row) ||
        is.data.frame(row) ||
        is.object(row) ||
        !identical(names(row), classes)
    ) {
      tempest_trajectory_review_abort(
        "Trajectory acceptance count rows are malformed."
      )
    }
    invisible(lapply(
      classes,
      \(record_class) {
        tempest_trajectory_whole_number(
          row[[record_class]],
          paste("Trajectory acceptance", action, record_class)
        )
      }
    ))
  }
  expected <- Map(
    \(inserted, updated, matched) {
      as.double(inserted) + as.double(updated) + as.double(matched)
    },
    counts$inserted,
    counts$updated,
    counts$matched
  )
  if (!identical(as.numeric(expected), as.numeric(counts$observed))) {
    tempest_trajectory_review_abort(
      "Trajectory acceptance counts do not reconcile."
    )
  }
  counts
}

tempest_trajectory_validate_accepted_revisions <- function(
  revisions,
  acceptance,
  counts
) {
  classes <- names(counts$observed)
  for (revision in revisions$items) {
    for (field in c("class", "record_id", "revision_id", "batch_id")) {
      tempest_trajectory_scalar_string(
        revision[[field]],
        paste("Trajectory accepted revision", field)
      )
    }
    for (field in c("content_digest", "schema_build_digest")) {
      tempest_trajectory_validate_sha256(
        revision[[field]],
        paste("Trajectory accepted revision", field)
      )
    }
    tempest_trajectory_scalar_string(
      revision$action,
      "Trajectory accepted revision action"
    )
    if (!revision$action %in% c("insert", "update", "match")) {
      tempest_trajectory_review_abort(
        "Trajectory accepted revision action is invalid."
      )
    }
    if (!revision$class %in% classes) {
      tempest_trajectory_review_abort(
        "Trajectory accepted revision class is absent from its counts."
      )
    }
    tempest_trajectory_exact_whole_number(
      revision$revision_number,
      "Trajectory accepted revision revision_number",
      minimum = 1
    )
    if (
      !identical(
        revision$schema_build_digest,
        acceptance$schema_build_digest
      ) ||
        (revision$action %in%
          c("insert", "update") &&
          !identical(revision$batch_id, acceptance$batch_id))
    ) {
      tempest_trajectory_review_abort(
        "A trajectory accepted revision is not bound to its receipt."
      )
    }
  }
  revision_ids <- vapply(
    revisions$items,
    `[[`,
    character(1),
    "revision_id"
  )
  record_keys <- vapply(
    revisions$items,
    \(revision) paste(revision$class, revision$record_id, sep = "\u001f"),
    character(1)
  )
  if (anyDuplicated(revision_ids) || anyDuplicated(record_keys)) {
    tempest_trajectory_review_abort(
      "Trajectory accepted revision identities must be unique."
    )
  }
  if (
    sum(as.numeric(unlist(counts$observed, use.names = FALSE))) !=
      as.double(revisions$total)
  ) {
    tempest_trajectory_review_abort(
      "Trajectory accepted revision counts do not match the collection total."
    )
  }
  for (record_class in classes) {
    class_revisions <- Filter(
      \(revision) identical(revision$class, record_class),
      revisions$items
    )
    retained <- c(
      inserted = sum(vapply(
        class_revisions,
        \(revision) identical(revision$action, "insert"),
        logical(1)
      )),
      updated = sum(vapply(
        class_revisions,
        \(revision) identical(revision$action, "update"),
        logical(1)
      )),
      matched = sum(vapply(
        class_revisions,
        \(revision) identical(revision$action, "match"),
        logical(1)
      )),
      observed = length(class_revisions)
    )
    receipt <- vapply(
      names(retained),
      \(action) as.numeric(counts[[action]][[record_class]]),
      numeric(1)
    )
    if (
      any(retained > receipt) ||
        (identical(revisions$omitted, 0L) &&
          !identical(as.numeric(retained), unname(receipt)))
    ) {
      tempest_trajectory_review_abort(
        "Trajectory accepted revisions do not match their class counts."
      )
    }
  }
  invisible(revisions)
}

tempest_trajectory_validate_knowledge <- function(knowledge, research_run_id) {
  tempest_trajectory_exact_record(
    knowledge,
    tempest_trajectory_knowledge_fields(),
    "Trajectory knowledge"
  )
  tempest_trajectory_validate_snapshot(
    knowledge$input_snapshot,
    nullable = TRUE
  )
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
  if (identical(knowledge$promotion_state, "proposed")) {
    if (!is.null(knowledge$acceptance)) {
      tempest_trajectory_review_abort(
        "A proposed trajectory cannot retain acceptance state."
      )
    }
    return(invisible(knowledge))
  }
  acceptance <- tempest_trajectory_exact_record(
    knowledge$acceptance,
    tempest_trajectory_acceptance_fields(),
    "Trajectory acceptance"
  )
  for (field in c(
    "receipt_id",
    "bundle_id",
    "plan_digest",
    "schema_build_digest"
  )) {
    tempest_trajectory_validate_sha256(
      acceptance[[field]],
      paste("Trajectory acceptance", field)
    )
  }
  for (field in c("plan_id", "batch_id", "store_id")) {
    tempest_trajectory_scalar_string(
      acceptance[[field]],
      paste("Trajectory acceptance", field)
    )
  }
  if (
    !grepl("^graft:[A-Z0-9]+$", acceptance$plan_id) ||
      !tempest_promotion_receipt_store_id_valid(acceptance$store_id) ||
      !identical(
        acceptance$schema_build_digest,
        tempest_promotion_schema_build_digest
      ) ||
      !identical(acceptance$bundle_id, proposal$bundle_id) ||
      !identical(
        acceptance$schema_build_digest,
        proposal$schema_build_digest
      ) ||
      !identical(acceptance$batch_id, acceptance$plan_id)
  ) {
    tempest_trajectory_review_abort(
      "Trajectory acceptance does not bind its proposal."
    )
  }
  snapshot <- tempest_trajectory_validate_snapshot(
    acceptance$snapshot,
    complete = TRUE,
    digest_identity = TRUE
  )
  if (
    !identical(snapshot$store_id, acceptance$store_id) ||
      !identical(snapshot$batch_id, acceptance$batch_id) ||
      !identical(
        snapshot$schema_build_digest,
        acceptance$schema_build_digest
      ) ||
      !identical(snapshot$history_complete, TRUE)
  ) {
    tempest_trajectory_review_abort(
      "Trajectory acceptance snapshot is not bound to its receipt."
    )
  }
  counts <- tempest_trajectory_validate_acceptance_counts(acceptance$counts)
  tempest_trajectory_validate_collection(
    acceptance$record_revisions,
    tempest_promotion_receipt_revision_fields(),
    "Trajectory accepted revisions"
  )
  tempest_trajectory_validate_canonical_set(
    acceptance$record_revisions,
    "Trajectory accepted revisions"
  )
  tempest_trajectory_validate_accepted_revisions(
    acceptance$record_revisions,
    acceptance,
    counts
  )
  if (identical(acceptance$record_revisions$omitted, 0L)) {
    tryCatch(
      do.call(
        TempestPromotionReceipt,
        c(
          list(schema_version = tempest_promotion_schema_version),
          acceptance[setdiff(names(acceptance), "record_revisions")],
          list(record_revisions = acceptance$record_revisions$items)
        )
      ),
      error = function(error) {
        tempest_trajectory_review_abort(
          "Trajectory acceptance is not an exact promotion receipt.",
          parent = error
        )
      }
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
  claim_count <- sum(vapply(
    evidence$items,
    \(item) identical(item$record_type, "claim"),
    logical(1)
  ))
  if (knowledge$proposal$claim_selection$count > claim_count) {
    tempest_trajectory_review_abort(
      "Trajectory proposal claim count exceeds the complete evidence lane."
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
  tempest_trajectory_scalar_string(
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
    if (!is.null(stage$output$kind) && !is.null(output_contract$ids)) {
      output_type <- tempest_trajectory_stage_output_type(output_contract$kind)
      for (output_id in output_contract$ids) {
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
  snapshot <- review@knowledge$input_snapshot
  if (!is.null(snapshot)) {
    add(tempest_trajectory_join(
      "product",
      run_id,
      "read_from",
      "graft_snapshot",
      snapshot$snapshot_id,
      "authority_validated",
      c("snapshot_id", "store_id", "schema_build_digest", "commit_order")
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
  acceptance <- review@knowledge$acceptance
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
  if (!is.null(acceptance)) {
    add(tempest_trajectory_join(
      "promotion_bundle",
      proposal$bundle_id,
      "accepted_as",
      "promotion_receipt",
      acceptance$receipt_id,
      "exact_identity",
      "bundle_id"
    ))
    for (revision in acceptance$record_revisions$items) {
      add(tempest_trajectory_join(
        "promotion_receipt",
        acceptance$receipt_id,
        "accepted_as",
        "graft_revision",
        revision$revision_id,
        "exact_identity",
        c(
          "record_id",
          "revision_id",
          "batch_id",
          "content_digest",
          "schema_build_digest"
        )
      ))
    }
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
    output_contract <- tempest_stage_output_reference_contract(stage$stage)
    if (!is.null(output_contract$ids)) {
      output_ids <- vapply(output_joins, `[[`, character(1), "to_id")
      identities_invalid <- if (joins_complete) {
        !setequal(output_ids, output_contract$ids)
      } else {
        !all(output_ids %in% output_contract$ids)
      }
      if (identities_invalid) {
        tempest_trajectory_review_abort(
          "Trajectory stage output joins do not match its fixed identities."
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
  revisions <- review@knowledge$acceptance$record_revisions %||% NULL
  revisions_complete <- is.null(revisions) || identical(revisions$omitted, 0L)
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
  if (revisions_complete) {
    compare_subset(
      Filter(
        \(join) identical(join$relation, "accepted_as"),
        review@joins$items
      ),
      Filter(\(join) identical(join$relation, "accepted_as"), expected),
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
  input_snapshot <- review@knowledge$input_snapshot
  proposal <- review@knowledge$proposal
  acceptance <- review@knowledge$acceptance
  revisions <- if (is.null(acceptance)) {
    NULL
  } else {
    acceptance$record_revisions
  }
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
      graft_snapshot = if (is.null(input_snapshot)) {
        character()
      } else {
        input_snapshot$snapshot_id
      },
      promotion_bundle = if (is.null(proposal)) {
        character()
      } else {
        proposal$bundle_id
      },
      promotion_receipt = if (is.null(acceptance)) {
        character()
      } else {
        acceptance$receipt_id
      },
      graft_revision = if (is.null(revisions)) {
        character()
      } else {
        vapply(revisions$items, `[[`, character(1), "revision_id")
      }
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
  complete$graft_revision <- is.null(revisions) ||
    identical(revisions$omitted, 0L)
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
      tempest_trajectory_validate_programs(self@programs)
      tempest_trajectory_validate_collection(
        self@stages,
        tempest_trajectory_stage_fields(),
        "Trajectory stages"
      )
      invisible(lapply(
        self@stages$items,
        tempest_trajectory_validate_stage,
        programs = self@programs
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
        programs = self@programs,
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
        delegated <- Filter(
          \(agent) identical(agent$trace_type, "deputy_delegation"),
          self@agent_runs$items
        )
        for (agent in delegated) {
          parents <- Filter(
            function(parent) {
              identical(parent$trace_type, "deputy_run") &&
                identical(parent$deputy_run_id, agent$parent_run_id) &&
                identical(parent$agent_id, agent$parent_agent_id) &&
                identical(parent$status, "complete") &&
                identical(parent$completion_disposition, "issued") &&
                identical(parent$correlation_id, agent$correlation_id)
            },
            self@agent_runs$items
          )
          if (length(parents) != 1L) {
            tempest_trajectory_review_abort(
              "A trajectory delegation does not resolve one exact parent."
            )
          }
        }
      }
      tempest_trajectory_validate_knowledge(
        self@knowledge,
        self@product$research_run_id
      )
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

#' A bounded review of one completed Tempest product
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Builds a deterministic, read-only projection of the exact execution,
#' program, knowledge, and evidence identities retained by a completed STORM
#' or Co-STORM product. Correlation identifiers are grouping evidence only;
#' they never establish causation. The returned projection is reconstructable
#' in memory and is not a persistence or acceptance authority.
#'
#' The value contains exactly `schema_version`, `review_id`, `product`,
#' `stages`, `agent_runs`, `programs`, `knowledge`, `evidence`, `joins`, and
#' `findings`. The `stages` lane retains authoritative StageRecord order. The
#' `agent_runs`, `evidence`, `joins`, and `findings` lanes are canonical sets;
#' accepted promotion revisions use the same canonical envelope beneath
#' `knowledge$acceptance$record_revisions`. Every variable lane contains
#' exactly `total`, `retained`, `omitted`, `digest`, and `items`, retains at most
#' 250 items, and binds the complete lane digest. Mutable progress events,
#' prompts, responses, source content, paths, credentials, capabilities, and
#' live objects are excluded.
#'
#' Joins distinguish authority-validated bindings, exact identity, and
#' correlation-only grouping. A `correlation_id` can support only a
#' `correlated_with` relation and never claims causation or authorship. An
#' exact promotion bundle adds proposed state; its exact matching receipt adds
#' accepted state. A receipt alone or a cross-product combination is rejected.
#'
#' @param research One exact completed result returned by [tempest_run()] or a
#'   succeeded, quiescent `TempestSession` returned by [tempest_session()].
#' @param promotion_bundle Optional exact bundle returned by
#'   [tempest_promotion_bundle()].
#' @param promotion_receipt Optional exact receipt returned by
#'   [tempest_promotion_receipt()]. Requires `promotion_bundle`.
#' @return A validated `TempestTrajectoryReview` S7 value containing the closed
#'   ten-field bounded projection. The class is internal and the review is not
#'   persisted.
#' @examples
#' \dontrun{
#' result <- tempest_run("Grid-scale battery recycling")
#' review <- tempest_trajectory_review(result)
#' review@product
#' review@stages
#' }
#' @export
tempest_trajectory_review <- function(
  research,
  promotion_bundle = NULL,
  promotion_receipt = NULL
) {
  context <- tempest_completed_product_context(
    research = research,
    abort = tempest_trajectory_review_abort,
    boundary_class = "tempest_trajectory_review_error",
    purpose = "Trajectory review"
  )
  if (identical(context$manifest@mode, "costorm")) {
    tryCatch(
      {
        program_set <- tempest_program_set_assert(
          tempest_session_program_set(research)
        )
        if (
          !tempest_program_set_identity_equal(
            program_set,
            context$manifest@programs
          )
        ) {
          tempest_trajectory_review_abort(
            "The completed Co-STORM product changed its live ProgramSet."
          )
        }
      },
      error = function(error) {
        if (inherits(error, "tempest_trajectory_review_error")) {
          stop(error)
        }
        tempest_trajectory_review_abort(
          "The completed Co-STORM product lacks its exact live ProgramSet.",
          parent = error
        )
      }
    )
  }
  bundle <- tempest_trajectory_bundle_data(research, promotion_bundle)
  receipt <- tempest_trajectory_receipt_data(bundle, promotion_receipt)
  tryCatch(
    {
      manifest <- context$manifest
      product <- list(
        research_run_id = manifest@research_run_id,
        mode = manifest@mode,
        status = manifest@status,
        config_digest = manifest@config_digest,
        report_reference = context$report_reference
      )
      stage_items <- unname(lapply(
        context$stage_records,
        tempest_trajectory_stage_item
      ))
      traces <- Filter(
        \(trace) trace$trace_type %in% c("deputy_run", "deputy_delegation"),
        manifest@traces
      )
      agent_items <- unname(lapply(traces, tempest_trajectory_agent_item))
      programs <- tempest_trajectory_programs(manifest)
      evidence_items <- tempest_trajectory_evidence_items(context$workspace)
      knowledge <- tempest_trajectory_knowledge(manifest, bundle, receipt)
      joined <- tempest_trajectory_joins(
        context,
        agent_items,
        programs,
        evidence_items,
        bundle,
        receipt
      )
      findings <- tempest_trajectory_findings(
        context$stage_records,
        agent_items,
        joined$linked_agents
      )
      payload <- tempest_trajectory_review_payload(
        tempest_trajectory_schema_version,
        product,
        tempest_trajectory_collection(stage_items, preserve_order = TRUE),
        tempest_trajectory_collection(agent_items, preserve_order = FALSE),
        programs,
        knowledge,
        tempest_trajectory_collection(evidence_items, preserve_order = FALSE),
        tempest_trajectory_collection(
          joined$joins,
          preserve_order = FALSE
        ),
        tempest_trajectory_collection(findings, preserve_order = FALSE)
      )
      do.call(
        TempestTrajectoryReview,
        c(
          list(
            schema_version = payload$schema_version,
            review_id = tempest_trajectory_digest(payload)
          ),
          payload[setdiff(names(payload), "schema_version")]
        )
      )
    },
    error = function(error) {
      if (inherits(error, "tempest_trajectory_review_error")) {
        stop(error)
      }
      tempest_trajectory_review_abort(
        "Could not construct the completed product trajectory review.",
        parent = error
      )
    }
  )
}

#' Extract a validated trajectory review projection
#'
#' `tempest_trajectory_review_data()` exposes the closed, plain-data projection
#' from a value returned by [tempest_trajectory_review()]. It validates the
#' exact Tempest review class and its content-bound review identity before
#' returning data. It never reaches back into a product, session, workspace,
#' provider, or other live object.
#'
#' @param x A value returned by [tempest_trajectory_review()].
#'
#' @returns A named list containing the validated, schema-versioned trajectory
#'   review projection.
#' @export
tempest_trajectory_review_data <- function(x) {
  x_class <- S7::S7_class(x)
  if (
    !S7::S7_inherits(x, TempestTrajectoryReview) ||
      !identical(S7::prop(x_class, "name"), "TempestTrajectoryReview") ||
      !identical(S7::prop(x_class, "package"), "tempest")
  ) {
    tempest_trajectory_review_abort(
      c(
        "{.arg x} must be returned by {.fn tempest_trajectory_review}.",
        "x" = "It is {.obj_type_friendly {x}}."
      ),
      class = "tempest_trajectory_review_input_error"
    )
  }
  properties <- S7::props(x)[tempest_trajectory_review_fields()]
  problem <- tempest_trajectory_review_validation_message(x)
  if (!is.null(problem)) {
    tempest_trajectory_review_abort(
      c(
        "{.arg x} must be a valid Tempest trajectory review.",
        "x" = problem
      ),
      class = "tempest_trajectory_review_input_error"
    )
  }
  properties
}

# Internal validated value boundary; intentionally not exported or documented.
TempestTrajectoryReview <- S7::new_class(
  "TempestTrajectoryReview",
  properties = list(
    schema_version = S7::new_property(S7::class_integer),
    review_id = S7::new_property(S7::class_character),
    product = S7::new_property(S7::class_list),
    stages = S7::new_property(S7::class_list),
    agent_runs = S7::new_property(S7::class_list),
    programs = S7::new_property(S7::class_list),
    knowledge = S7::new_property(S7::class_list),
    evidence = S7::new_property(S7::class_list),
    joins = S7::new_property(S7::class_list),
    findings = S7::new_property(S7::class_list)
  ),
  validator = tempest_trajectory_review_validation_message
)
