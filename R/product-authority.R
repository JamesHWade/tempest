# Derived product authority over durable execution identities.
#
# This validator joins product records by exact execution identity. It does not
# claim that particular response bytes caused a persisted output.

tempest_product_authority_abort <- function(message, parent = NULL) {
  tempest_abort(
    message,
    class = c("tempest_product_authority_error", "tempest_error"),
    parent = parent
  )
}

tempest_product_authority_expert_ids <- function(experts) {
  if (length(experts) == 0L) {
    return(NULL)
  }
  experts <- tryCatch(
    tempest_validate_experts(experts),
    error = function(error) {
      tempest_product_authority_abort(
        "Product authority requires an exact durable expert roster.",
        parent = error
      )
    }
  )
  sort(
    vapply(experts, \(expert) expert@expert_id, character(1)),
    method = "radix"
  )
}

tempest_product_authority_governed <- function(manifest, stage_records) {
  governed_program <- any(vapply(
    manifest@programs,
    function(reference) {
      !is.null(reference$governed_procedure_ref %||% NULL)
    },
    logical(1)
  ))
  governed_stage <- any(vapply(
    stage_records,
    function(record) {
      identical(record@execution_path, "governed") ||
        !is.na(record@governed_procedure_revision_id) ||
        !is.null(record@trace_references$governed_procedure %||% NULL)
    },
    logical(1)
  ))
  governed_program || governed_stage
}

tempest_product_authority_threshold <- function(stage_records, config) {
  if (!is.null(config) && !S7::S7_inherits(config, TempestConfig)) {
    tempest_product_authority_abort(
      "Product authority requires a TempestConfig or `NULL`."
    )
  }
  verification <- Filter(
    \(record) {
      identical(record@stage, "verify_claim_support") &&
        identical(record@status, "succeeded")
    },
    stage_records
  )
  thresholds <- unique(vapply(
    verification,
    function(record) {
      record@trace_references$min_support_score %||% NA_character_
    },
    character(1)
  ))
  if (length(thresholds) > 1L || anyNA(thresholds)) {
    tempest_product_authority_abort(
      "Product authority requires one exact verification threshold."
    )
  }
  configured <- if (is.null(config)) {
    NULL
  } else {
    config@min_support_score
  }
  if (length(thresholds) == 0L) {
    return(configured %||% 0.7)
  }
  persisted <- tryCatch(
    tempest_stage_support_threshold_value(thresholds[[1L]]),
    error = function(error) {
      tempest_product_authority_abort(
        "Product authority found an invalid verification threshold.",
        parent = error
      )
    }
  )
  if (!is.null(configured) && !identical(persisted, configured)) {
    tempest_product_authority_abort(
      "Product authority verification threshold does not match configuration."
    )
  }
  persisted
}

tempest_product_authority_validate <- function(
  manifest,
  stage_records,
  workspace,
  report_md = NULL,
  report_reference = NULL,
  config = NULL,
  experts = list(),
  expert_sessions = list(),
  product_state = NULL,
  require_publishable = FALSE
) {
  if (!rlang::is_bool(require_publishable)) {
    tempest_product_authority_abort(
      "{.arg require_publishable} must be `TRUE` or `FALSE`."
    )
  }
  if (!S7::S7_inherits(manifest, TempestResearchManifest)) {
    tempest_product_authority_abort(
      "Product authority requires a TempestResearchManifest."
    )
  }
  tryCatch(
    S7::validate(manifest),
    error = function(error) {
      tempest_product_authority_abort(
        "Product authority received an invalid research manifest.",
        parent = error
      )
    }
  )
  if (!manifest@mode %in% c("storm", "costorm")) {
    tempest_product_authority_abort(
      "Product authority supports only STORM and Co-STORM manifests."
    )
  }
  if (!inherits(workspace, "ResearchWorkspace")) {
    tempest_product_authority_abort(
      "Product authority requires a ResearchWorkspace."
    )
  }
  stage_records <- tryCatch(
    tempest_stage_records_validate(stage_records, allow_running = FALSE),
    error = function(error) {
      tempest_product_authority_abort(
        "Product authority requires quiescent terminal stage records.",
        parent = error
      )
    }
  )
  expert_ids <- tempest_product_authority_expert_ids(experts)
  report_bound <- !is.null(report_md) && !is.null(report_reference)
  manifest_report <- manifest@deliverables$report_md %||% NULL
  if (xor(is.null(report_md), is.null(report_reference))) {
    tempest_product_authority_abort(
      "A product report and its exact reference must be present together."
    )
  }
  if (
    !identical(manifest@status, "succeeded") &&
      (!is.null(report_md) ||
        !is.null(report_reference) ||
        !is.null(manifest_report))
  ) {
    tempest_product_authority_abort(
      "A partial product must remain report-free and nonpublishable."
    )
  }
  if (identical(manifest@status, "succeeded") && is.null(manifest_report)) {
    tempest_product_authority_abort(
      "A succeeded product requires a canonical durable report reference."
    )
  }
  if (
    !is.null(manifest_report) &&
      (!is.list(manifest_report) ||
        is.data.frame(manifest_report) ||
        !identical(
          names(manifest_report),
          c("report_id", "sha256", "status")
        ) ||
        !identical(manifest_report$report_id, "report_md") ||
        !rlang::is_string(manifest_report$sha256) ||
        !grepl("^sha256:[a-f0-9]{64}$", manifest_report$sha256) ||
        !identical(manifest_report$status, "durable"))
  ) {
    tempest_product_authority_abort(
      "Product authority requires the exact canonical durable report binding."
    )
  }
  completed <- identical(manifest@status, "succeeded") &&
    !is.null(manifest_report)
  publishable <- isTRUE(completed) && isTRUE(report_bound)
  if (
    isTRUE(require_publishable) &&
      !isTRUE(publishable)
  ) {
    tempest_product_authority_abort(
      "Publication authority requires a succeeded product with an exact report binding."
    )
  }
  if (
    manifest@status %in%
      c("failed", "cancelled") &&
      (!is.null(report_md) || !is.null(report_reference))
  ) {
    tempest_product_authority_abort(
      "A failed or cancelled product cannot retain a published report."
    )
  }

  threshold <- tempest_product_authority_threshold(stage_records, config)
  tryCatch(
    {
      if (!is.null(config)) {
        if (
          !identical(
            manifest@config_digest,
            tempest_research_config_digest(config)
          )
        ) {
          tempest_stage_record_abort(
            "The manifest does not match the exact product configuration."
          )
        }
      }
      tempest_product_authority_validate_stage_records(
        manifest,
        stage_records,
        expert_ids = expert_ids,
        expert_sessions = expert_sessions
      )
      tempest_stage_records_validate_workspace(
        stage_records,
        workspace,
        min_support_score = threshold
      )
      tempest_stage_records_validate_persisted_trust(
        stage_records,
        workspace,
        min_support_score = threshold
      )
      claims <- workspace$list_proposed_claims()
      supports <- workspace$list_claim_supports()
      passing_support <- vapply(
        supports,
        function(support) {
          S7::S7_inherits(support, TempestClaimSupport) &&
            identical(support@verification_status, "supported") &&
            !is.na(support@support_score) &&
            is.finite(support@support_score) &&
            support@support_score >= threshold
        },
        logical(1)
      )
      if (isTRUE(completed) && length(claims) == 0L) {
        tempest_stage_record_abort(
          "Publication authority requires at least one extracted claim."
        )
      }
      if (isTRUE(completed) && !any(passing_support)) {
        tempest_stage_record_abort(
          paste0(
            "Publication authority requires at least one verified ",
            "threshold-passing claim-support pair."
          )
        )
      }
      tempest_stage_records_validate_workspace_coverage(
        stage_records,
        workspace,
        require_extraction = isTRUE(completed),
        require_verification = isTRUE(completed)
      )
      tempest_stage_records_validate_claim_provenance(
        stage_records,
        workspace,
        manifest@research_run_id,
        experts = experts
      )
      if (length(experts) > 0L) {
        tempest_stage_records_validate_generated_experts(
          stage_records,
          experts
        )
      }
      if (!is.null(product_state) && identical(manifest@mode, "storm")) {
        tempest_stage_records_validate_storm_coverage(
          stage_records,
          product_state
        )
      }
      if (isTRUE(report_bound)) {
        tempest_product_authority_validate_report(
          manifest,
          report_reference,
          report_md
        )
      }
      if (!is.null(report_md)) {
        if (is.null(config)) {
          tempest_stage_record_abort(
            "Report authority requires the exact product configuration."
          )
        }
        title <- if (is.null(product_state)) {
          NULL
        } else {
          product_state$title %||% NULL
        }
        if (is.null(title)) {
          tempest_stage_record_abort(
            "Report authority requires the exact trusted product title."
          )
        }
        tempest_product_report_validate_policy(
          report_md,
          title,
          workspace,
          config,
          stage_records
        )
      }
    },
    error = function(error) {
      if (inherits(error, "tempest_product_authority_error")) {
        stop(error)
      }
      tempest_product_authority_abort(
        "Durable product execution identities do not form exact authority.",
        parent = error
      )
    }
  )

  governed <- tempest_product_authority_governed(manifest, stage_records)
  accepted_references <- workspace$list_accepted_graft_references()
  if (
    (isTRUE(governed) || length(accepted_references) > 0L) &&
      is.null(workspace$graft_snapshot)
  ) {
    tempest_product_authority_abort(
      "Accepted or governed product context requires one real immutable Graft snapshot."
    )
  }
  tryCatch(
    tempest_graft_snapshot_assert_binding(
      workspace$graft_snapshot,
      manifest@knowledge_snapshot,
      workspace,
      c("tempest_product_authority_error", "tempest_error"),
      "Product authority Graft snapshot"
    ),
    error = function(error) {
      if (inherits(error, "tempest_product_authority_error")) {
        stop(error)
      }
      tempest_product_authority_abort(
        "Product authority does not match the immutable Graft boundary.",
        parent = error
      )
    }
  )

  list(
    binding_scope = "execution_identity",
    mode = manifest@mode,
    research_run_id = manifest@research_run_id,
    status = manifest@status,
    completed = completed,
    report_reference_bound = !is.null(manifest_report),
    publishable = publishable,
    stage_attempt_ids = vapply(
      stage_records,
      \(record) record@attempt_id,
      character(1)
    )
  )
}

tempest_product_authority_finalize_manifest <- function(
  manifest,
  stage_records,
  workspace,
  deputy_traces = NULL,
  report_md = NULL,
  config = NULL,
  experts = list(),
  expert_sessions = list(),
  product_state = NULL,
  status = manifest@status,
  require_publishable = FALSE
) {
  if (!S7::S7_inherits(manifest, TempestResearchManifest)) {
    tempest_product_authority_abort(
      "Product finalization requires a TempestResearchManifest."
    )
  }
  stage_records <- tempest_stage_records_validate(
    stage_records,
    allow_running = FALSE
  )
  if (is.null(deputy_traces)) {
    trace_types <- vapply(
      manifest@traces,
      \(trace) trace$trace_type %||% NA_character_,
      character(1)
    )
    if (anyNA(trace_types)) {
      tempest_product_authority_abort(
        "Product finalization found an untyped execution trace."
      )
    }
    deputy_traces <- manifest@traces[
      trace_types %in% c("deputy_run", "deputy_delegation")
    ]
  }
  expert_ids <- tempest_product_authority_expert_ids(experts)
  candidate <- tryCatch(
    {
      value <- tempest_research_manifest_update(manifest, status = status)
      value <- tempest_product_authority_bind_stage_records(
        value,
        stage_records,
        deputy_traces = deputy_traces,
        expert_ids = expert_ids,
        expert_sessions = expert_sessions
      )
      tempest_product_authority_bind_report(value, report_md)
    },
    error = function(error) {
      tempest_product_authority_abort(
        "Could not bind the candidate product manifest atomically.",
        parent = error
      )
    }
  )
  reference <- tempest_product_report_reference(report_md)
  tempest_product_authority_validate(
    candidate,
    stage_records,
    workspace,
    report_md = report_md,
    report_reference = reference,
    config = config,
    experts = experts,
    expert_sessions = expert_sessions,
    product_state = product_state,
    require_publishable = require_publishable
  )
  candidate
}


#' @keywords internal
tempest_graft_snapshot_relative_path <- function() {
  "knowledge/graft-snapshot.rds"
}

#' @keywords internal
tempest_graft_snapshot_field_names <- function() {
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

#' @keywords internal
tempest_graft_snapshot_abort <- function(message, class, parent = NULL) {
  tempest_abort(message, class = class, parent = parent)
}

#' @keywords internal
tempest_graft_snapshot_validate <- function(
  snapshot,
  class,
  what = "Graft snapshot"
) {
  if (is.null(snapshot)) {
    return(NULL)
  }
  snapshot <- tryCatch(
    tempest_research_workspace_graft_snapshot(snapshot),
    error = function(error) {
      tempest_graft_snapshot_abort(
        paste0(what, " is not a real valid graft::GraftSnapshot."),
        class,
        parent = error
      )
    }
  )
  fields <- tempest_graft_snapshot_field_names()
  if (!identical(S7::prop_names(snapshot), fields)) {
    tempest_graft_snapshot_abort(
      paste0(what, " does not expose the complete public Graft boundary."),
      class
    )
  }
  values <- stats::setNames(
    lapply(fields, \(field) S7::prop(snapshot, field)),
    fields
  )
  reference <- tryCatch(
    tempest_snapshot_reference(snapshot),
    error = function(error) {
      tempest_graft_snapshot_abort(
        paste0(what, " cannot be represented by the Tempest manifest."),
        class,
        parent = error
      )
    }
  )
  list(snapshot = snapshot, values = values, reference = reference)
}

#' @keywords internal
tempest_graft_snapshot_assert_binding <- function(
  snapshot,
  manifest_reference,
  workspace,
  class,
  what = "Persisted Graft snapshot"
) {
  manifest_reference <- manifest_reference %||% list()
  if (!is.list(manifest_reference) || is.data.frame(manifest_reference)) {
    tempest_graft_snapshot_abort(
      "The research manifest knowledge snapshot must be a reference record.",
      class
    )
  }
  if (!inherits(workspace, "ResearchWorkspace")) {
    tempest_graft_snapshot_abort(
      "The Graft snapshot must be bound to a ResearchWorkspace.",
      class
    )
  }
  if (is.null(snapshot)) {
    if (
      length(manifest_reference) > 0L ||
        !is.null(workspace$base_snapshot_id) ||
        !is.null(workspace$graft_snapshot)
    ) {
      tempest_graft_snapshot_abort(
        paste0(
          "Pinned accepted knowledge requires an actual path-free ",
          "graft::GraftSnapshot."
        ),
        class
      )
    }
    return(invisible(NULL))
  }
  validated <- tempest_graft_snapshot_validate(snapshot, class, what)
  workspace_validated <- tempest_graft_snapshot_validate(
    workspace$graft_snapshot,
    class,
    "ResearchWorkspace Graft snapshot"
  )
  if (is.null(workspace_validated)) {
    tempest_graft_snapshot_abort(
      "The ResearchWorkspace does not retain the persisted Graft snapshot.",
      class
    )
  }
  if (!identical(validated$values, workspace_validated$values)) {
    tempest_graft_snapshot_abort(
      "The persisted Graft snapshot does not match the ResearchWorkspace.",
      class
    )
  }
  manifest_reference <- tryCatch(
    tempest_research_manifest_knowledge_snapshot(manifest_reference),
    error = function(error) {
      tempest_graft_snapshot_abort(
        "The research manifest knowledge snapshot is invalid.",
        class,
        parent = error
      )
    }
  )
  if (!identical(validated$reference, manifest_reference)) {
    tempest_graft_snapshot_abort(
      paste0(
        "The research manifest does not match all immutable Graft snapshot ",
        "fields."
      ),
      class
    )
  }
  if (
    !identical(
      workspace$base_snapshot_id,
      validated$reference$snapshot_id
    )
  ) {
    tempest_graft_snapshot_abort(
      "The ResearchWorkspace base snapshot does not match the Graft snapshot.",
      class
    )
  }
  matching_references <- Filter(
    \(reference) {
      identical(
        reference$snapshot_id %||% NULL,
        validated$reference$snapshot_id
      )
    },
    workspace$list_accepted_graft_references()
  )
  complete_references <- Filter(
    \(reference) identical(reference, validated$reference),
    matching_references
  )
  if (length(complete_references) != 1L) {
    tempest_graft_snapshot_abort(
      paste0(
        "The ResearchWorkspace does not retain the complete immutable ",
        "Graft snapshot reference."
      ),
      class
    )
  }
  invisible(validated$reference)
}

#' @keywords internal
tempest_graft_snapshot_write <- function(root, snapshot, class) {
  if (is.null(snapshot)) {
    return(character())
  }
  validated <- tempest_graft_snapshot_validate(
    snapshot,
    class,
    "Graft snapshot sidecar"
  )
  relative_path <- tempest_graft_snapshot_relative_path()
  path <- file.path(root, relative_path)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  staging <- tempfile(
    pattern = ".graft-snapshot-staging-",
    tmpdir = dirname(path),
    fileext = ".rds"
  )
  installed <- FALSE
  on.exit(
    if (!installed && file.exists(staging)) {
      unlink(staging)
    },
    add = TRUE
  )
  tryCatch(
    saveRDS(validated$snapshot, staging, version = 3L),
    error = function(error) {
      tempest_graft_snapshot_abort(
        "Could not serialize the Graft snapshot sidecar.",
        class,
        parent = error
      )
    }
  )
  tryCatch(
    tempest_graft_snapshot_validate(
      readRDS(staging),
      class,
      "Serialized Graft snapshot sidecar"
    ),
    error = function(error) {
      tempest_graft_snapshot_abort(
        "Could not verify the serialized Graft snapshot sidecar.",
        class,
        parent = error
      )
    }
  )
  if (!file.rename(staging, path)) {
    if (!file.copy(staging, path, overwrite = TRUE)) {
      tempest_graft_snapshot_abort(
        "Could not install the Graft snapshot sidecar.",
        class
      )
    }
    unlink(staging)
  }
  installed <- TRUE
  relative_path
}

#' @keywords internal
tempest_graft_snapshot_read <- function(
  root,
  declared_files,
  manifest_reference,
  class
) {
  relative_path <- tempest_graft_snapshot_relative_path()
  declared <- relative_path %in% declared_files
  pinned <- length(manifest_reference %||% list()) > 0L
  if (!identical(declared, pinned)) {
    tempest_graft_snapshot_abort(
      paste0(
        "The Graft snapshot sidecar and research manifest must either both ",
        "be present or both be absent."
      ),
      class
    )
  }
  if (!pinned) {
    return(NULL)
  }
  if (!requireNamespace("graft", quietly = TRUE)) {
    tempest_graft_snapshot_abort(
      paste0(
        "Restoring pinned accepted knowledge requires the optional ",
        "graft package."
      ),
      class
    )
  }
  snapshot <- tryCatch(
    readRDS(file.path(root, relative_path)),
    error = function(error) {
      tempest_graft_snapshot_abort(
        "Could not read the Graft snapshot sidecar.",
        class,
        parent = error
      )
    }
  )
  tempest_graft_snapshot_validate(
    snapshot,
    class,
    "Persisted Graft snapshot sidecar"
  )$snapshot
}

#' @keywords internal
tempest_product_authority_stage_manifest_traces <- function(records) {
  records <- tempest_stage_records_validate(records)
  traces <- lapply(records, function(record) {
    references <- record@trace_references
    if (!identical(references$trace_id %||% NULL, record@attempt_id)) {
      tempest_stage_record_abort(
        paste0(
          "Every durable stage attempt must use its attempt id as the exact ",
          "canonical trace id."
        )
      )
    }
    trace_fields <- c(
      "deputy_run_id",
      "deputy_session_id",
      "parent_run_id",
      "delegation_id",
      "tool_call_id",
      "trace_id",
      "expert_id",
      "correlation_id",
      "role"
    )
    trace <- references[intersect(trace_fields, names(references))]
    trace$program_artifact_id <- record@program_artifact_id
    trace$stage <- record@stage
    trace$status <- record@status
    trace$trace_type <- "stage_attempt"
    trace
  })
  tempest_research_manifest_traces(traces)
}

#' @keywords internal
tempest_product_authority_deputy_manifest_traces <- function(
  traces,
  manifest,
  expert_ids = NULL
) {
  if (
    !is.list(traces) ||
      is.data.frame(traces) ||
      !is.null(names(traces))
  ) {
    tempest_stage_record_abort(
      "Deputy traces must be an exact unnamed list of plain records."
    )
  }
  if (!is.null(expert_ids)) {
    if (
      !is.character(expert_ids) ||
        is.object(expert_ids) ||
        !is.null(names(expert_ids)) ||
        anyNA(expert_ids) ||
        anyDuplicated(expert_ids)
    ) {
      tempest_stage_record_abort(
        "Deputy trace validation requires exact unique session expert IDs."
      )
    }
    expert_ids <- sort(expert_ids, method = "radix")
  }
  if (length(traces) == 0L) {
    return(list())
  }

  allowed_fields <- c(
    "agent_id",
    "completion_disposition",
    "correlation_id",
    "delegation_id",
    "deputy_run_id",
    "deputy_session_id",
    "expert_id",
    "parent_agent_id",
    "parent_run_id",
    "role",
    "stage",
    "status",
    "tool_call_id",
    "trace_id",
    "trace_type"
  )
  required_fields <- c(
    "agent_id",
    "completion_disposition",
    "correlation_id",
    "deputy_run_id",
    "deputy_session_id",
    "role",
    "stage",
    "status",
    "trace_id",
    "trace_type"
  )
  traces <- lapply(seq_along(traces), function(index) {
    trace <- traces[[index]]
    fields <- names(trace)
    expected_fields <- allowed_fields[allowed_fields %in% fields]
    if (
      !is.list(trace) ||
        is.data.frame(trace) ||
        is.object(trace) ||
        is.null(fields) ||
        anyNA(fields) ||
        anyDuplicated(fields) ||
        !all(required_fields %in% fields) ||
        !identical(fields, expected_fields)
    ) {
      tempest_stage_record_abort(
        "Deputy traces must use the exact current plain-record fields."
      )
    }
    canonical <- tryCatch(
      tempest_research_manifest_traces(list(trace))[[1L]],
      error = function(error) {
        tempest_stage_record_abort(
          "Deputy traces contain an invalid canonical manifest reference.",
          parent = error
        )
      }
    )
    if (!identical(trace, canonical)) {
      tempest_stage_record_abort(
        "Deputy traces must already use exact canonical values and field order."
      )
    }
    if (
      !trace$trace_type %in% c("deputy_run", "deputy_delegation") ||
        !identical(trace$trace_id, trace$deputy_run_id)
    ) {
      tempest_stage_record_abort(
        "A Deputy run trace must use its exact run ID as trace ID."
      )
    }
    if (
      !trace$status %in%
        c(
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
    ) {
      tempest_stage_record_abort(
        "A Deputy run trace must carry one exact terminal status."
      )
    }
    valid_context <- if (identical(manifest@mode, "storm")) {
      identical(c(trace$stage, trace$role), c("research", "expert"))
    } else if (identical(manifest@mode, "costorm")) {
      identical(c(trace$stage, trace$role), c("dialogue", "moderator")) ||
        identical(c(trace$stage, trace$role), c("dialogue", "expert")) ||
        identical(c(trace$stage, trace$role), c("warmup", "expert"))
    } else {
      FALSE
    }
    if (!valid_context) {
      tempest_stage_record_abort(
        "A Deputy run trace has an unknown product-mode stage and role binding."
      )
    }
    if (identical(trace$role, "expert")) {
      if (
        is.null(trace$expert_id) ||
          (!is.null(expert_ids) && !trace$expert_id %in% expert_ids)
      ) {
        tempest_stage_record_abort(
          "An expert Deputy run trace must bind one active session expert."
        )
      }
    } else if (!is.null(trace$expert_id)) {
      tempest_stage_record_abort(
        "A moderator Deputy run trace cannot carry an expert ID."
      )
    }
    relation_fields <- c(
      "parent_agent_id",
      "parent_run_id",
      "delegation_id",
      "tool_call_id"
    )
    relation_present <- relation_fields %in% fields
    if (
      (any(relation_present) && !all(relation_present)) ||
        xor(
          identical(trace$trace_type, "deputy_delegation"),
          all(relation_present)
        ) ||
        (all(relation_present) &&
          identical(trace$parent_run_id, trace$deputy_run_id))
    ) {
      tempest_stage_record_abort(
        "A Deputy delegation trace must retain its complete exact lineage tuple."
      )
    }
    agent_stage <- if (
      identical(manifest@mode, "costorm") &&
        identical(trace$role, "expert")
    ) {
      # One persistent expert Agent serves dialogue and warmup runs. Its
      # immutable Agent identity is created from the dialogue base context;
      # each run still binds its exact dialogue or warmup trace context.
      "dialogue"
    } else {
      trace$stage
    }
    expected_agent_id <- tempest_deputy_adapter_agent_id(
      tempest_deputy_run_context(
        manifest,
        stage = agent_stage,
        role = trace$role,
        expert_id = trace$expert_id %||% NULL
      )
    )
    if (!identical(trace$agent_id, expected_agent_id)) {
      tempest_stage_record_abort(
        paste0(
          "A Deputy run trace agent ID does not match its deterministic ",
          "product execution context."
        )
      )
    }
    trace
  })
  trace_ids <- vapply(traces, `[[`, character(1), "trace_id")
  if (anyDuplicated(trace_ids)) {
    tempest_stage_record_abort("Deputy run trace IDs must be unique.")
  }
  ordered <- order(trace_ids, method = "radix")
  if (!identical(ordered, seq_along(traces))) {
    tempest_stage_record_abort(
      "Deputy run traces must be ordered canonically by trace ID."
    )
  }
  traces
}

#' @keywords internal
tempest_product_authority_expert_session_trace_bindings <- function(
  records,
  expert_ids
) {
  records <- tryCatch(
    tempest_persistence_exact_records(
      records,
      tempest_expert_session_record_fields(),
      "session expert-session records",
      tempest_session_persistence_error_class(
        "tempest_session_restore_error"
      )
    ),
    error = function(error) {
      tempest_stage_record_abort(
        "Expert-session trace bindings must use exact current records.",
        parent = error
      )
    }
  )
  if (length(records) == 0L) {
    return(character())
  }
  session_ids <- vapply(records, `[[`, character(1), "session_id")
  bound_expert_ids <- vapply(records, `[[`, character(1), "expert_id")
  if (
    anyNA(session_ids) ||
      anyNA(bound_expert_ids) ||
      any(
        !grepl(
          "^expert-session_[a-f0-9]{16}$",
          session_ids,
          perl = TRUE
        )
      ) ||
      any(!bound_expert_ids %in% expert_ids) ||
      anyDuplicated(session_ids) ||
      anyDuplicated(bound_expert_ids)
  ) {
    tempest_stage_record_abort(
      paste0(
        "Expert-session trace bindings require unique manager-owned session ",
        "IDs for active session experts."
      )
    )
  }
  stats::setNames(bound_expert_ids, session_ids)
}

#' @keywords internal
tempest_product_authority_manifest_runtime_from_traces <- function(traces) {
  ids <- function(field) {
    values <- vapply(
      traces,
      function(trace) trace[[field]] %||% NA_character_,
      character(1)
    )
    as.list(sort(unique(values[!is.na(values)]), method = "radix"))
  }
  list(
    deputy_run_ids = ids("deputy_run_id"),
    deputy_session_ids = ids("deputy_session_id")
  )
}

#' @keywords internal
tempest_product_authority_extraction_attempt_ids <- function(records) {
  vapply(
    Filter(
      function(record) {
        identical(record@stage, "extract_claims") &&
          identical(record@status, "succeeded") &&
          identical(record@output_reference$kind, "workspace_claims") &&
          length(unlist(record@output_reference$ids, use.names = FALSE)) > 0L
      },
      records
    ),
    \(record) record@attempt_id,
    character(1)
  )
}

#' @keywords internal
tempest_product_authority_manifest_validate_trace_ids <- function(
  stage_traces,
  deputy_traces,
  manifest,
  expert_session_bindings = character(),
  authoritative_extraction_attempt_ids = character()
) {
  if (
    !is.character(authoritative_extraction_attempt_ids) ||
      is.object(authoritative_extraction_attempt_ids) ||
      !is.null(names(authoritative_extraction_attempt_ids)) ||
      anyNA(authoritative_extraction_attempt_ids) ||
      any(!nzchar(authoritative_extraction_attempt_ids)) ||
      anyDuplicated(authoritative_extraction_attempt_ids)
  ) {
    tempest_stage_record_abort(
      "Authoritative extraction attempt IDs must be exact and unique."
    )
  }
  traces <- c(stage_traces, deputy_traces)
  trace_ids <- vapply(traces, `[[`, character(1), "trace_id")
  if (anyDuplicated(trace_ids)) {
    tempest_stage_record_abort(
      "Manifest trace IDs must be unique across stage and Deputy traces."
    )
  }
  stage_trace_ids <- vapply(stage_traces, `[[`, character(1), "trace_id")
  if (
    length(setdiff(authoritative_extraction_attempt_ids, stage_trace_ids)) > 0L
  ) {
    tempest_stage_record_abort(
      "Authoritative extraction IDs must resolve exact durable stage traces."
    )
  }
  for (trace in deputy_traces) {
    if (
      identical(manifest@mode, "costorm") &&
        identical(trace$role, "moderator")
    ) {
      moderator_session_id <- tempest_costorm_deputy_session_id(
        manifest@research_run_id,
        "moderator"
      )
      if (!identical(trace$deputy_session_id, moderator_session_id)) {
        tempest_stage_record_abort(
          paste0(
            "A moderator Deputy trace must bind the deterministic session ",
            "identity for its research run."
          )
        )
      }
    } else if (identical(manifest@mode, "storm")) {
      expected_session_id <- tempest_storm_deputy_session_id(
        manifest@research_run_id,
        trace$expert_id
      )
      if (!identical(trace$deputy_session_id, expected_session_id)) {
        tempest_stage_record_abort(
          paste0(
            "A STORM expert Deputy trace must bind its deterministic run and ",
            "expert session identity."
          )
        )
      }
    } else if (
      !grepl(
        "^expert-session_[a-f0-9]{16}$",
        trace$deputy_session_id,
        perl = TRUE
      )
    ) {
      tempest_stage_record_abort(
        "An expert Deputy trace must bind a manager-owned session identity."
      )
    } else {
      active_expert_id <- if (
        trace$deputy_session_id %in% names(expert_session_bindings)
      ) {
        expert_session_bindings[[trace$deputy_session_id]]
      } else {
        NULL
      }
      if (
        !is.null(active_expert_id) &&
          !identical(trace$expert_id, active_expert_id)
      ) {
        tempest_stage_record_abort(
          paste0(
            "An expert Deputy trace session ID is owned by a different ",
            "active persisted expert-session binding."
          )
        )
      }
    }
  }
  delegated <- Filter(
    \(trace) identical(trace$trace_type, "deputy_delegation"),
    deputy_traces
  )
  for (trace in delegated) {
    parents <- Filter(
      function(parent) {
        identical(parent$trace_type, "deputy_run") &&
          identical(parent$deputy_run_id, trace$parent_run_id) &&
          identical(parent$agent_id, trace$parent_agent_id) &&
          identical(parent$status, "complete") &&
          identical(parent$completion_disposition, "issued") &&
          identical(parent$correlation_id, trace$correlation_id)
      },
      deputy_traces
    )
    if (length(parents) != 1L) {
      tempest_stage_record_abort(
        "A Deputy delegation tuple must resolve one exact completed parent run."
      )
    }
  }
  for (trace in stage_traces) {
    run_id <- trace$deputy_run_id %||% NULL
    session_id <- trace$deputy_session_id %||% NULL
    extraction_output <- trace$trace_id %in%
      authoritative_extraction_attempt_ids
    if (xor(is.null(run_id), is.null(session_id))) {
      tempest_stage_record_abort(
        "A stage trace must bind both Deputy run and session IDs or neither."
      )
    }
    if (is.null(run_id)) {
      if (isTRUE(extraction_output)) {
        tempest_stage_record_abort(
          paste0(
            "A succeeded product claim extraction must bind its exact Deputy ",
            "run, session, expert, and correlation identity."
          )
        )
      }
      next
    }
    matches <- vapply(
      deputy_traces,
      function(deputy_trace) {
        identical(deputy_trace$deputy_run_id, run_id) &&
          identical(deputy_trace$deputy_session_id, session_id)
      },
      logical(1)
    )
    if (sum(matches) != 1L) {
      tempest_stage_record_abort(
        paste0(
          "A stage trace Deputy identity must resolve to one exact terminal ",
          "Deputy run trace."
        )
      )
    }
    deputy_trace <- deputy_traces[[which(matches)]]
    if (
      !identical(deputy_trace$status, "complete") ||
        !identical(deputy_trace$completion_disposition, "issued")
    ) {
      tempest_stage_record_abort(
        paste0(
          "A durable stage trace can bind only an issued, successfully ",
          "completed Deputy run."
        )
      )
    }
    expert_id <- trace$expert_id %||% NULL
    correlation_id <- trace$correlation_id %||% NULL
    if (
      isTRUE(extraction_output) &&
        (is.null(expert_id) || is.null(correlation_id))
    ) {
      tempest_stage_record_abort(
        paste0(
          "A succeeded product claim extraction must bind its exact Deputy ",
          "run, session, expert, and correlation identity."
        )
      )
    }
    if (identical(deputy_trace$role, "moderator")) {
      if (!identical(expert_id, "moderator")) {
        tempest_stage_record_abort(
          paste0(
            "A stage trace backed by a moderator run must use the exact ",
            "moderator expert sentinel."
          )
        )
      }
    } else if (
      is.null(expert_id) ||
        !identical(deputy_trace$expert_id, expert_id)
    ) {
      tempest_stage_record_abort(
        "A stage trace expert does not match its terminal Deputy run trace."
      )
    }
    if (
      !is.null(correlation_id) &&
        !identical(
          deputy_trace$correlation_id %||% NULL,
          correlation_id
        )
    ) {
      tempest_stage_record_abort(
        paste0(
          "A stage trace correlation ID does not match its terminal Deputy ",
          "run trace."
        )
      )
    }
    relation_fields <- c("parent_run_id", "delegation_id", "tool_call_id")
    stage_relation <- trace[relation_fields]
    deputy_relation <- deputy_trace[relation_fields]
    stage_present <- !vapply(stage_relation, is.null, logical(1))
    deputy_present <- !vapply(deputy_relation, is.null, logical(1))
    if (
      (any(stage_present) && !all(stage_present)) ||
        (any(deputy_present) && !all(deputy_present)) ||
        !identical(stage_relation, deputy_relation)
    ) {
      tempest_stage_record_abort(
        "A stage trace does not match its exact Deputy delegation tuple."
      )
    }
    if (identical(manifest@mode, "storm")) {
      if (
        !identical(trace$stage, "extract_claims") ||
          is.null(expert_id) ||
          is.null(correlation_id) ||
          !identical(deputy_trace$stage, "research") ||
          !identical(deputy_trace$role, "expert")
      ) {
        tempest_stage_record_abort(
          paste0(
            "A STORM Deputy run can bind only one exact expert research ",
            "extraction attempt."
          )
        )
      }
    }
  }
  if (identical(manifest@mode, "storm")) {
    bound_run_ids <- vapply(
      stage_traces,
      \(trace) trace$deputy_run_id %||% NA_character_,
      character(1)
    )
    bound_run_ids <- bound_run_ids[!is.na(bound_run_ids)]
    if (anyDuplicated(bound_run_ids)) {
      tempest_stage_record_abort(
        paste0(
          "A STORM Deputy run cannot authorize more than one durable expert ",
          "extraction attempt."
        )
      )
    }
  }
  invisible(traces)
}

#' @keywords internal
tempest_product_authority_manifest_existing_traces <- function(
  manifest,
  stage_traces,
  deputy_traces,
  expert_ids
) {
  traces <- manifest@traces
  if (
    !is.list(traces) ||
      is.data.frame(traces) ||
      !is.null(names(traces))
  ) {
    tempest_stage_record_abort(
      "The live manifest trace collection must be one exact unnamed list."
    )
  }
  if (length(traces) == 0L) {
    return(invisible(NULL))
  }
  types <- vapply(
    traces,
    function(trace) trace$trace_type %||% NA_character_,
    character(1)
  )
  if (
    anyNA(types) ||
      any(!types %in% c("stage_attempt", "deputy_run", "deputy_delegation"))
  ) {
    tempest_stage_record_abort(
      "The live manifest contains an unknown or untyped trace."
    )
  }
  existing_deputy <- tempest_product_authority_deputy_manifest_traces(
    traces[types %in% c("deputy_run", "deputy_delegation")],
    manifest = manifest,
    expert_ids = expert_ids
  )
  if (length(existing_deputy) > 0L) {
    expected_deputy_ids <- vapply(
      deputy_traces,
      `[[`,
      character(1),
      "trace_id"
    )
    existing_deputy_ids <- vapply(
      existing_deputy,
      `[[`,
      character(1),
      "trace_id"
    )
    expected_deputy_indexes <- match(
      existing_deputy_ids,
      expected_deputy_ids
    )
    if (
      anyNA(expected_deputy_indexes) ||
        is.unsorted(expected_deputy_indexes, strictly = TRUE) ||
        !identical(
          existing_deputy,
          deputy_traces[expected_deputy_indexes]
        )
    ) {
      tempest_stage_record_abort(
        paste0(
          "The live manifest Deputy traces are not an exact immutable ",
          "subset of the session accumulator."
        )
      )
    }
  }
  existing_stage <- traces[types == "stage_attempt"]
  expected_ids <- vapply(stage_traces, `[[`, character(1), "trace_id")
  existing_ids <- vapply(
    existing_stage,
    function(trace) {
      trace$trace_id %||% NA_character_
    },
    character(1)
  )
  if (
    anyNA(existing_ids) ||
      anyDuplicated(existing_ids) ||
      any(!existing_ids %in% expected_ids)
  ) {
    tempest_stage_record_abort(
      "The live manifest contains an orphan or duplicate stage trace."
    )
  }
  expected_indexes <- match(existing_ids, expected_ids)
  if (
    is.unsorted(expected_indexes, strictly = TRUE) ||
      !identical(existing_stage, stage_traces[expected_indexes])
  ) {
    tempest_stage_record_abort(
      "The live manifest contains a mismatched stage trace projection."
    )
  }
  final_runtime <- tempest_product_authority_manifest_runtime_from_traces(
    c(stage_traces, deputy_traces)
  )
  for (field in names(final_runtime)) {
    existing <- unlist(manifest@runtime[[field]] %||% list(), use.names = FALSE)
    allowed <- unlist(final_runtime[[field]], use.names = FALSE)
    if (length(setdiff(existing, allowed)) > 0L) {
      tempest_stage_record_abort(
        "The live manifest runtime contains an orphan Deputy identity."
      )
    }
  }
  invisible(NULL)
}

#' @keywords internal
tempest_product_authority_bind_stage_records <- function(
  manifest,
  records,
  deputy_traces = list(),
  expert_ids = NULL,
  expert_sessions = list()
) {
  stage_traces <- tempest_product_authority_stage_manifest_traces(records)
  deputy_traces <- tempest_product_authority_deputy_manifest_traces(
    deputy_traces,
    manifest = manifest,
    expert_ids = expert_ids
  )
  expert_session_bindings <-
    tempest_product_authority_expert_session_trace_bindings(
      expert_sessions,
      expert_ids %||% character()
    )
  tempest_product_authority_manifest_existing_traces(
    manifest,
    stage_traces,
    deputy_traces,
    expert_ids
  )
  traces <- c(stage_traces, deputy_traces)
  tempest_product_authority_manifest_validate_trace_ids(
    stage_traces,
    deputy_traces,
    manifest,
    expert_session_bindings = expert_session_bindings,
    authoritative_extraction_attempt_ids = tempest_product_authority_extraction_attempt_ids(
      records
    )
  )
  runtime <- tempest_product_authority_manifest_runtime_from_traces(traces)
  tempest_research_manifest_update(
    manifest,
    runtime = runtime,
    traces = traces
  )
}

#' @keywords internal
tempest_product_authority_validate_stage_records <- function(
  manifest,
  records,
  deputy_traces = NULL,
  expert_ids = NULL,
  expert_sessions = list()
) {
  stage_traces <- tempest_product_authority_stage_manifest_traces(records)
  traces <- manifest@traces
  if (
    !is.list(traces) ||
      is.data.frame(traces) ||
      !is.null(names(traces))
  ) {
    tempest_stage_record_abort(
      "The research manifest traces must be one exact unnamed list."
    )
  }
  types <- vapply(
    traces,
    function(trace) trace$trace_type %||% NA_character_,
    character(1)
  )
  if (
    anyNA(types) ||
      any(!types %in% c("stage_attempt", "deputy_run", "deputy_delegation"))
  ) {
    tempest_stage_record_abort(
      "The research manifest contains an unknown or untyped trace."
    )
  }
  actual_deputy <- tempest_product_authority_deputy_manifest_traces(
    traces[types %in% c("deputy_run", "deputy_delegation")],
    manifest = manifest,
    expert_ids = expert_ids
  )
  if (!is.null(deputy_traces)) {
    deputy_traces <- tempest_product_authority_deputy_manifest_traces(
      deputy_traces,
      manifest = manifest,
      expert_ids = expert_ids
    )
    if (!identical(actual_deputy, deputy_traces)) {
      tempest_stage_record_abort(
        "The research manifest does not bind the exact session Deputy traces."
      )
    }
  }
  expected <- c(stage_traces, actual_deputy)
  if (!identical(traces, expected)) {
    tempest_stage_record_abort(
      paste0(
        "The research manifest trace declarations do not exactly match its ",
        "durable stage attempts followed by canonical Deputy traces."
      )
    )
  }
  tempest_product_authority_manifest_validate_trace_ids(
    stage_traces,
    actual_deputy,
    manifest,
    expert_session_bindings = tempest_product_authority_expert_session_trace_bindings(
      expert_sessions,
      expert_ids %||% character()
    ),
    authoritative_extraction_attempt_ids = tempest_product_authority_extraction_attempt_ids(
      records
    )
  )
  expected_runtime <- tempest_product_authority_manifest_runtime_from_traces(
    traces
  )
  if (!identical(manifest@runtime, expected_runtime)) {
    tempest_stage_record_abort(
      paste0(
        "The research manifest runtime IDs do not exactly cover its stage ",
        "and Deputy traces."
      )
    )
  }
  tempest_stage_records_validate_manifest(records, manifest)
}

#' @keywords internal
tempest_product_authority_bind_report <- function(manifest, report_md) {
  reference <- tempest_product_report_reference(report_md)
  deliverables <- manifest@deliverables
  deliverables$report_md <- if (is.null(reference)) {
    NULL
  } else {
    c(reference, list(status = "durable"))
  }
  tempest_research_manifest_update(
    manifest,
    deliverables = deliverables
  )
}

#' @keywords internal
tempest_product_authority_validate_report <- function(
  manifest,
  reference,
  report_md
) {
  tempest_product_report_reference_validate(reference, report_md)
  expected <- if (is.null(reference)) {
    NULL
  } else {
    c(reference, list(status = "durable"))
  }
  if (!identical(manifest@deliverables$report_md %||% NULL, expected)) {
    tempest_stage_record_abort(
      "The research manifest does not bind the exact durable report."
    )
  }
  invisible(reference)
}
