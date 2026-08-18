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
    tempest_validate_experts(experts, active_only = FALSE),
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
      tempest_persistence_manifest_validate_stage_records(
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
        tempest_persistence_manifest_validate_report(
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
        tempest_persistence_validate_report_policy(
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
      value <- tempest_persistence_manifest_bind_stage_records(
        value,
        stage_records,
        deputy_traces = deputy_traces,
        expert_ids = expert_ids,
        expert_sessions = expert_sessions
      )
      tempest_persistence_manifest_bind_report(value, report_md)
    },
    error = function(error) {
      tempest_product_authority_abort(
        "Could not bind the candidate product manifest atomically.",
        parent = error
      )
    }
  )
  reference <- tempest_persistence_report_reference(report_md)
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
