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
  if (!rlang::is_string(value) || is.na(value) || !nzchar(value)) {
    tempest_trajectory_review_abort("{noun} must be one non-empty string.")
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

tempest_trajectory_stage_item <- function(record) {
  references <- record@trace_references
  list(
    stage = record@stage,
    attempt_id = record@attempt_id,
    trace_id = references$trace_id,
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

tempest_trajectory_join <- function(
  from_type,
  from_id,
  relation,
  to_type,
  to_id,
  proof_kind,
  matched_fields
) {
  list(
    from_type = from_type,
    from_id = from_id,
    relation = relation,
    to_type = to_type,
    to_id = to_id,
    proof = list(
      kind = proof_kind,
      matched_fields = unname(as.list(matched_fields))
    )
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
  if (!product$mode %in% c("storm", "costorm")) {
    tempest_trajectory_review_abort("Trajectory product mode is invalid.")
  }
  if (!identical(product$status, "succeeded")) {
    tempest_trajectory_review_abort("Trajectory product must be succeeded.")
  }
  if (!grepl("^sha256:[a-f0-9]{64}$", product$config_digest)) {
    tempest_trajectory_review_abort(
      "Trajectory product config_digest is invalid."
    )
  }
  tempest_trajectory_exact_record(
    product$report_reference,
    c("report_id", "sha256"),
    "Trajectory report reference"
  )
  if (
    !identical(product$report_reference$report_id, "report_md") ||
      !grepl(
        "^sha256:[a-f0-9]{64}$",
        product$report_reference$sha256
      )
  ) {
    tempest_trajectory_review_abort(
      "Trajectory report reference is invalid."
    )
  }
  invisible(product)
}

tempest_trajectory_validate_stage <- function(stage) {
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
  if (!grepl("^sha256:[a-f0-9]{64}$", stage$output$digest)) {
    tempest_trajectory_review_abort(
      "Trajectory stage output digest is invalid."
    )
  }
  invisible(stage)
}

tempest_trajectory_validate_agent <- function(agent) {
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
  invisible(agent)
}

tempest_trajectory_validate_programs <- function(programs) {
  stages <- tempest_program_set_stages()
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
    if (!is.null(program$governed_procedure_ref)) {
      tryCatch(
        tempest_governed_procedure_record(program$governed_procedure_ref),
        error = function(error) {
          tempest_trajectory_review_abort(
            "A trajectory governed-procedure reference is invalid.",
            parent = error
          )
        }
      )
    }
  }
  invisible(programs)
}

tempest_trajectory_validate_snapshot <- function(snapshot, nullable = FALSE) {
  if (is.null(snapshot) && isTRUE(nullable)) {
    return(invisible(snapshot))
  }
  tempest_trajectory_exact_record(
    snapshot,
    tempest_trajectory_snapshot_fields(),
    "Trajectory snapshot"
  )
  tempest_trajectory_scalar_string(
    snapshot$snapshot_id,
    "Trajectory snapshot snapshot_id"
  )
  invisible(snapshot)
}

tempest_trajectory_validate_knowledge <- function(knowledge) {
  tempest_trajectory_exact_record(
    knowledge,
    tempest_trajectory_knowledge_fields(),
    "Trajectory knowledge"
  )
  tempest_trajectory_validate_snapshot(
    knowledge$input_snapshot,
    nullable = TRUE
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
  selection <- tempest_trajectory_exact_record(
    proposal$claim_selection,
    tempest_trajectory_selection_fields(),
    "Trajectory claim selection"
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
  if (!grepl("^sha256:[a-f0-9]{64}$", selection$digest)) {
    tempest_trajectory_review_abort(
      "Trajectory proposal selection digest is invalid."
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
  acceptance <- tempest_trajectory_exact_record(
    knowledge$acceptance,
    tempest_trajectory_acceptance_fields(),
    "Trajectory acceptance"
  )
  if (!identical(acceptance$bundle_id, proposal$bundle_id)) {
    tempest_trajectory_review_abort(
      "Trajectory acceptance does not bind its proposal."
    )
  }
  tempest_trajectory_validate_snapshot(acceptance$snapshot)
  tempest_trajectory_validate_collection(
    acceptance$record_revisions,
    tempest_promotion_receipt_revision_fields(),
    "Trajectory accepted revisions"
  )
  invisible(knowledge)
}

tempest_trajectory_validate_evidence <- function(evidence) {
  if (
    !evidence$record_type %in%
      c(
        "resource",
        "claim",
        "evidence_span",
        "claim_support",
        "dispute"
      )
  ) {
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
        \(field) rlang::is_string(field) && !is.na(field) && nzchar(field),
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

tempest_trajectory_validate_finding <- function(finding) {
  severities <- tempest_trajectory_finding_severities()
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
      if (!grepl("^sha256:[a-f0-9]{64}$", self@review_id)) {
        stop("review_id must be one SHA-256 identity")
      }
      tempest_trajectory_validate_product(self@product)
      tempest_trajectory_validate_collection(
        self@stages,
        tempest_trajectory_stage_fields(),
        "Trajectory stages"
      )
      invisible(lapply(
        self@stages$items,
        tempest_trajectory_validate_stage
      ))
      tempest_trajectory_validate_collection(
        self@agent_runs,
        tempest_trajectory_agent_fields(),
        "Trajectory agent runs"
      )
      invisible(lapply(
        self@agent_runs$items,
        tempest_trajectory_validate_agent
      ))
      tempest_trajectory_validate_programs(self@programs)
      tempest_trajectory_validate_knowledge(self@knowledge)
      tempest_trajectory_validate_collection(
        self@evidence,
        tempest_trajectory_evidence_fields(),
        "Trajectory evidence"
      )
      invisible(lapply(
        self@evidence$items,
        tempest_trajectory_validate_evidence
      ))
      tempest_trajectory_validate_collection(
        self@joins,
        tempest_trajectory_join_fields(),
        "Trajectory joins"
      )
      invisible(lapply(self@joins$items, tempest_trajectory_validate_join))
      tempest_trajectory_validate_collection(
        self@findings,
        tempest_trajectory_finding_fields(),
        "Trajectory findings"
      )
      invisible(lapply(
        self@findings$items,
        tempest_trajectory_validate_finding
      ))
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
  S7::props(x)
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
