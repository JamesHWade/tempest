# Bounded, read-only trajectory review for completed Tempest products.

tempest_trajectory_schema_version <- 4L
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
  c(
    "input_selection",
    "consultations",
    "promotion_state",
    "proposal",
    "acceptance"
  )
}

tempest_trajectory_consultation_fields <- function() {
  c(
    "status",
    "stream",
    "purpose",
    "decision_id",
    "selection_id",
    "artifact_count",
    "text_bytes",
    "truncated",
    "omitted_count"
  )
}


tempest_trajectory_proposal_fields <- function() {
  c(
    "bundle_id",
    "research_run_id",
    "schema_build_digest",
    "claim_selection",
    "selection_id"
  )
}

tempest_trajectory_selection_fields <- function() {
  c("kind", "count", "digest")
}

tempest_trajectory_acceptance_fields <- function() {
  c("decision_id", "stream", "sequence", "selection_id", "purpose")
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
    "published_as",
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
  tempest_product_sha256_id(value)
}

tempest_trajectory_nullable <- function(value) {
  tempest_product_nullable(value)
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

tempest_trajectory_validate_opaque_identifier <- function(
  value,
  noun,
  nullable = FALSE
) {
  if (is.null(value) && isTRUE(nullable)) {
    return(invisible(value))
  }
  if (!tempest_opaque_identifier_valid(value)) {
    tempest_trajectory_review_abort(
      "{noun} must be a bounded opaque identifier, not prose or credentials."
    )
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

tempest_trajectory_claim_selection_digest <- function(claim_ids) {
  claim_ids <- unname(unlist(claim_ids, use.names = FALSE))
  tempest_trajectory_digest(
    unname(as.list(sort(claim_ids, method = "radix")))
  )
}

tempest_trajectory_consultation_item <- function(row) {
  normalize_opt <- function(value) {
    if (
      is.null(value) || length(value) != 1L || is.na(value) || !nzchar(value)
    ) {
      NULL
    } else {
      as.character(value)
    }
  }
  item <- list(
    status = as.character(row$status),
    stream = as.character(row$stream),
    purpose = normalize_opt(row$purpose),
    decision_id = normalize_opt(row$decision_id),
    selection_id = normalize_opt(row$selection_id),
    artifact_count = as.integer(row$artifact_count),
    text_bytes = as.integer(row$text_bytes),
    truncated = isTRUE(row$truncated),
    omitted_count = as.integer(row$omitted_count)
  )
  tryCatch(
    {
      tempest_trajectory_validate_consultation(item)
      item
    },
    error = function(error) NULL
  )
}

tempest_trajectory_consultations <- function(research) {
  frame <- tryCatch(
    tempest_product_graft_consultations(research),
    error = function(error) NULL
  )
  if (!is.data.frame(frame) || nrow(frame) == 0L) {
    return(list())
  }
  items <- lapply(seq_len(nrow(frame)), function(i) {
    tempest_trajectory_consultation_item(frame[i, , drop = FALSE])
  })
  unname(Filter(Negate(is.null), items))
}

tempest_trajectory_validate_consultation <- function(item) {
  tempest_trajectory_exact_record(
    item,
    tempest_trajectory_consultation_fields(),
    "Trajectory memory consultation"
  )
  tempest_trajectory_scalar_string(
    item$status,
    "Trajectory consultation status"
  )
  if (!item$status %in% c("accepted", "withdrawn", "missing")) {
    tempest_trajectory_review_abort(
      "Trajectory consultation status must be accepted, withdrawn, or missing."
    )
  }
  tempest_trajectory_artifact_label(
    item$stream,
    "Trajectory consultation stream"
  )
  if (!is.null(item$purpose)) {
    tempest_trajectory_artifact_label(
      item$purpose,
      "Trajectory consultation purpose"
    )
  }
  if (identical(item$status, "accepted")) {
    tempest_trajectory_artifact_digest(
      item$decision_id,
      "Trajectory consultation decision_id"
    )
    tempest_trajectory_artifact_digest(
      item$selection_id,
      "Trajectory consultation selection_id"
    )
  } else if (identical(item$status, "withdrawn")) {
    if (!is.null(item$decision_id)) {
      tempest_trajectory_artifact_digest(
        item$decision_id,
        "Trajectory consultation decision_id"
      )
    }
    if (!is.null(item$selection_id)) {
      tempest_trajectory_review_abort(
        "A withdrawn consultation cannot retain a selection identity."
      )
    }
  } else {
    if (!is.null(item$decision_id) || !is.null(item$selection_id)) {
      tempest_trajectory_review_abort(
        "A missing consultation cannot retain decision or selection identities."
      )
    }
  }
  tempest_trajectory_whole_number(
    item$artifact_count,
    "Trajectory consultation artifact_count"
  )
  tempest_trajectory_whole_number(
    item$text_bytes,
    "Trajectory consultation text_bytes"
  )
  if (!rlang::is_bool(item$truncated)) {
    tempest_trajectory_review_abort(
      "Trajectory consultation truncated must be TRUE or FALSE."
    )
  }
  tempest_trajectory_whole_number(
    item$omitted_count,
    "Trajectory consultation omitted_count"
  )
  invisible(item)
}

tempest_trajectory_knowledge <- function(
  manifest,
  promotion_bundle,
  publication,
  consultations = list()
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
        digest = tempest_trajectory_claim_selection_digest(
          promotion_bundle$claim_ids
        )
      ),
      selection_id = publication$selection %||% NULL
    )
  }
  acceptance <- publication$acceptance %||% NULL
  state <- if (!is.null(acceptance)) {
    "accepted"
  } else if (!is.null(proposal)) {
    "proposed"
  } else {
    "none"
  }
  list(
    input_selection = tempest_trajectory_input_selection(
      manifest@artifact_selection
    ),
    consultations = tempest_trajectory_collection(
      consultations,
      preserve_order = TRUE
    ),
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
      identical(relation, "proposed_as") &&
      identical(to_type, "promotion_bundle")
  ) {
    c("authority_validated", "research_run_id", "bundle_id", "claim_ids")
  } else if (
    identical(from_type, "product") &&
      identical(relation, "read_from") &&
      identical(to_type, "artifact_selection")
  ) {
    c("exact_identity", "selection_id", "selection_digest")
  } else if (
    identical(from_type, "promotion_bundle") &&
      identical(relation, "published_as") &&
      identical(to_type, "artifact_selection")
  ) {
    c("authority_validated", "bundle_id", "selection_id")
  } else if (
    identical(from_type, "artifact_selection") &&
      identical(relation, "accepted_as") &&
      identical(to_type, "artifact_decision")
  ) {
    c("exact_identity", "selection_id", "decision_id", "stream")
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

tempest_trajectory_stage_projected_output_ids <- function(stage) {
  contract <- tempest_stage_output_reference_contract(stage$stage)
  if (!is.null(contract$ids)) {
    return(unname(contract$ids))
  }
  if (identical(contract$kind, "content_digest")) {
    return(stage$output$digest)
  }
  NULL
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
  knowledge
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
  for (join in tempest_trajectory_artifact_joins(knowledge, run_id)) {
    add(join)
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
#' input artifact identities use the same canonical envelope beneath
#' `knowledge$input_selection$records`. Every variable lane contains
#' exactly `total`, `retained`, `omitted`, `digest`, and `items`, retains at most
#' 250 items, and binds the complete lane digest. Mutable progress events,
#' prompts, responses, source content, paths, credentials, capabilities, and
#' live objects are excluded.
#'
#' Joins distinguish authority-validated bindings, exact identity, and
#' correlation-only grouping. A `correlation_id` can support only a
#' `correlated_with` relation and never claims causation or authorship. An
#' exact promotion bundle or verified publication adds proposed state. An exact
#' recorded accept decision adds historical accepted state. This remains
#' inspectable after withdrawal and never grants current reuse permission.
#' Input selection identities and their full metadata digest remain distinct
#' from output publication. Input `reported_decision` is unverified host
#' provenance, never authenticated acceptance; malformed metadata is omitted.
#' Decision actor, reason, key and other arbitrary input provenance are
#' excluded. Schema 4 adds `knowledge$consultations`, a bounded, read-only lane
#' of plain-data `@extra$graft` recall consultation metadata emitted by Graft
#' memory tools; consultations remain unverified provenance distinct from
#' pinned input admission, output acceptance, and claim-support evidence.
#'
#' @param research One exact completed result returned by [tempest_run()] or a
#'   succeeded, quiescent `TempestSession` returned by [tempest_session()].
#' @param promotion_bundle Optional exact bundle returned by
#'   [tempest_promotion_bundle()].
#' @param store Artifact store used to verify an output publication. Required
#'   with `selection`. The store is never retained in the review.
#' @param selection Exact output selection returned by
#'   [tempest_publish_artifact_research()]. The retained bundle and report must
#'   match this completed research product.
#' @param stream Decision stream containing `decision`. Supply both together.
#' @param decision Exact historical accept decision digest. Omit to inspect a
#'   publication without asserting acceptance. No latest-decision lookup or
#'   current admission check is performed.
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
  store = NULL,
  selection = NULL,
  stream = NULL,
  decision = NULL
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
  publication <- tempest_trajectory_publication(
    research,
    promotion_bundle,
    store,
    selection,
    stream,
    decision
  )
  if (!is.null(publication)) {
    bundle <- publication$bundle
  }
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
      consultation_items <- tempest_trajectory_consultations(research)
      knowledge <- tempest_trajectory_knowledge(
        manifest,
        bundle,
        publication,
        consultations = consultation_items
      )
      joined <- tempest_trajectory_joins(
        context,
        agent_items,
        programs,
        evidence_items,
        bundle,
        knowledge
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

# Authority-join filtering and untrusted progress observation redaction helpers

tempest_trajectory_record_prop <- function(value, field) {
  if (S7::S7_inherits(value)) {
    return(tryCatch(S7::prop(value, field), error = function(error) NULL))
  }
  if (is.list(value) && !is.data.frame(value) && field %in% names(value)) {
    return(value[[field]])
  }
  NULL
}

tempest_trajectory_record_field <- function(value, field) {
  raw <- tempest_trajectory_record_prop(value, field)
  if (is.null(raw) || length(raw) != 1L || is.na(raw)) {
    return("")
  }
  if (is.logical(raw)) {
    return(if (isTRUE(raw)) "Yes" else "No")
  }
  if (is.numeric(raw)) {
    return(as.character(raw))
  }
  if (!is.character(raw)) {
    return("")
  }
  if (
    isTRUE(tempest_contract_sensitive_scalar(raw)) ||
      grepl("[[:cntrl:]]", raw, perl = TRUE)
  ) {
    return("redacted")
  }
  raw
}

tempest_trajectory_join_matched_fields <- function(join) {
  proof <- tempest_trajectory_record_prop(join, "proof")
  fields <- tempest_trajectory_record_prop(proof, "matched_fields")
  if (
    !is.list(fields) ||
      is.data.frame(fields) ||
      length(fields) == 0L ||
      !all(vapply(
        fields,
        \(field) is.character(field) && length(field) == 1L && !is.na(field),
        logical(1)
      ))
  ) {
    return(character())
  }
  unname(unlist(fields, use.names = FALSE))
}

tempest_trajectory_authoritative_agent_indices <- function(
  stage,
  joins,
  agents
) {
  attempt_id <- tempest_trajectory_record_field(stage, "attempt_id")
  if (!nzchar(attempt_id) || length(joins) == 0L || length(agents) == 0L) {
    return(integer())
  }
  exact_joins <- Filter(
    function(join) {
      proof <- tempest_trajectory_record_prop(join, "proof")
      identical(
        tempest_trajectory_record_field(join, "from_type"),
        "stage_attempt"
      ) &&
        identical(
          tempest_trajectory_record_field(join, "from_id"),
          attempt_id
        ) &&
        identical(
          tempest_trajectory_record_field(join, "relation"),
          "executed_as"
        ) &&
        identical(
          tempest_trajectory_record_field(join, "to_type"),
          "deputy_run"
        ) &&
        identical(
          tempest_trajectory_record_field(proof, "kind"),
          "authority_validated"
        ) &&
        identical(
          tempest_trajectory_join_matched_fields(join),
          c("deputy_run_id", "deputy_session_id")
        )
    },
    joins
  )
  run_ids <- unique(vapply(
    exact_joins,
    tempest_trajectory_record_field,
    character(1),
    field = "to_id"
  ))
  run_ids <- run_ids[nzchar(run_ids)]
  linked <- unlist(lapply(run_ids, function(run_id) {
    matches <- which(vapply(
      agents,
      function(agent) {
        identical(
          tempest_trajectory_record_field(agent, "deputy_run_id"),
          run_id
        ) &&
          nzchar(tempest_trajectory_record_field(agent, "deputy_session_id"))
      },
      logical(1)
    ))
    if (length(matches) == 1L) matches else integer()
  }))
  unique(as.integer(linked))
}

tempest_trajectory_progress_closed_value <- function(value, allowed) {
  if (!nzchar(value)) {
    return("")
  }
  if (value %in% allowed) value else "redacted"
}

tempest_trajectory_progress_stage_value <- function(value, workflow, kind) {
  if (!nzchar(value)) {
    return("")
  }
  if (!workflow %in% c("storm", "costorm")) {
    return("redacted")
  }
  allowed <- names(tempest_progress_labels(workflow, kind = kind))
  tempest_trajectory_progress_closed_value(value, allowed)
}

tempest_trajectory_progress_record <- function(event) {
  workflow <- tempest_trajectory_progress_closed_value(
    tempest_trajectory_record_field(event, "workflow"),
    c("storm", "costorm")
  )
  timestamp <- tempest_trajectory_record_field(event, "timestamp")
  if (
    nzchar(timestamp) &&
      !isTRUE(tempest_ledger_timestamp_valid(timestamp))
  ) {
    timestamp <- ""
  }
  list(
    event_id = tempest_trajectory_record_field(event, "event_id"),
    run_id = tempest_trajectory_record_field(event, "run_id"),
    workflow = workflow,
    event_type = tempest_trajectory_progress_closed_value(
      tempest_trajectory_record_field(event, "event_type"),
      tempest_progress_event_types()
    ),
    stage = tempest_trajectory_progress_stage_value(
      tempest_trajectory_record_field(event, "stage"),
      workflow,
      "stage"
    ),
    step = tempest_trajectory_progress_stage_value(
      tempest_trajectory_record_field(event, "step"),
      workflow,
      "step"
    ),
    status = tempest_trajectory_progress_closed_value(
      tempest_trajectory_record_field(event, "status"),
      tempest_progress_statuses()
    ),
    timestamp = timestamp,
    sequence = tempest_trajectory_record_field(event, "sequence"),
    parent_event_id = tempest_trajectory_record_field(event, "parent_event_id"),
    correlation_id = tempest_trajectory_record_field(event, "correlation_id")
  )
}

tempest_trajectory_progress_records <- function(events, max_rows = 250L) {
  if (!is.list(events) || is.data.frame(events)) {
    return(list())
  }
  total <- length(events)
  retained_events <- utils::tail(events, max_rows)
  records <- lapply(retained_events, tempest_trajectory_progress_record)
  attr(records, "total") <- total
  attr(records, "omitted") <- max(0L, total - length(records))
  records
}
