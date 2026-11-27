# Shared completed-product context readers for STORM and Co-STORM

tempest_completed_product_report_reference <- function(
  manifest,
  report_md,
  abort
) {
  reference <- manifest@deliverables$report_md %||% NULL
  expected <- c(
    tempest_product_report_reference(report_md),
    list(status = "durable")
  )
  if (!identical(reference, expected)) {
    abort(
      "The research product does not bind its exact durable report."
    )
  }
  tryCatch(
    tempest_product_report_reference_validate(
      reference[c("report_id", "sha256")],
      report_md
    ),
    error = function(error) {
      abort(
        "The research product report does not match its exact content reference.",
        parent = error
      )
    }
  )
  reference[c("report_id", "sha256")]
}

tempest_completed_product_assert_sealed_workspace <- function(
  workspace,
  abort,
  purpose
) {
  if (
    !inherits(workspace, "ResearchWorkspace") ||
      !identical(
        tempest_research_workspace_mutation_state(workspace),
        "sealed"
      )
  ) {
    abort(
      paste0(
        purpose,
        " requires the completed product's sealed ResearchWorkspace."
      )
    )
  }
  invisible(workspace)
}

tempest_completed_product_storm_outline <- function(outline, abort) {
  tryCatch(
    tempest_storm_state_structured(outline, "outline"),
    error = function(error) {
      abort(
        "The STORM result does not retain its exact public outline projection.",
        parent = error
      )
    }
  )
}

tempest_completed_product_storm_context <- function(
  research,
  abort,
  purpose
) {
  purpose_lower <- tolower(purpose)
  if (!tempest_is_product_result(research)) {
    abort(
      paste0(
        "{.arg research} must be one exact completed result returned by ",
        "{.fn tempest_run} or a succeeded TempestSession."
      )
    )
  }
  manifest <- research@manifest
  if (
    !S7::S7_inherits(manifest, TempestResearchManifest) ||
      !identical(manifest@mode, "storm") ||
      !identical(manifest@status, "succeeded")
  ) {
    abort(
      paste0(
        "A STORM ",
        purpose_lower,
        " requires its succeeded STORM research Manifest."
      )
    )
  }
  state <- tryCatch(
    tempest_storm_state_validate(research@state),
    error = function(error) {
      abort(
        paste0(
          "A STORM ",
          purpose_lower,
          " requires its exact current product state."
        ),
        parent = error
      )
    }
  )
  if (!tempest_storm_state_is_complete(state)) {
    abort(
      paste0(
        "A STORM ",
        purpose_lower,
        " requires every product stage and its final report."
      )
    )
  }
  projected <- list(
    title = state$title,
    perspectives = state$perspectives,
    experts = state$experts,
    outline = state$outline,
    draft_md = state$draft_md,
    report_md = state$report_md
  )
  public_projection <- list(
    title = research@title,
    perspectives = research@perspectives,
    experts = research@experts,
    outline = tempest_completed_product_storm_outline(research@outline, abort),
    draft_md = research@draft_md,
    report_md = research@report_md
  )
  if (!identical(public_projection, projected)) {
    abort(
      "The STORM result projections do not match its exact product state."
    )
  }
  if (
    !tempest_retriever_compatible(research@retriever) ||
      !identical(research@retriever$workspace, research@workspace) ||
      !S7::S7_inherits(research@config, TempestConfig) ||
      (inherits(research@retriever, "TempestRetriever") &&
        !identical(
          tempest_retriever_config_digest(research@retriever),
          tempest_research_config_digest(research@config)
        ))
  ) {
    abort(
      "The STORM result does not retain its exact product retriever identity."
    )
  }
  if (
    !is.null(research@output_dir) &&
      (!rlang::is_string(research@output_dir) ||
        is.na(research@output_dir) ||
        !nzchar(research@output_dir))
  ) {
    abort(
      "The STORM result has an invalid product output location."
    )
  }
  tempest_completed_product_assert_sealed_workspace(
    research@workspace,
    abort,
    purpose
  )
  report_reference <- tempest_completed_product_report_reference(
    manifest,
    research@report_md,
    abort
  )
  list(
    manifest = manifest,
    stage_records = state$stage_records,
    workspace = research@workspace,
    report_md = research@report_md,
    report_reference = report_reference,
    config = research@config,
    experts = state$experts,
    expert_sessions = list(),
    product_state = state
  )
}

tempest_completed_product_costorm_context <- function(
  research,
  abort,
  boundary_class,
  purpose
) {
  purpose_lower <- tolower(purpose)
  if (!inherits(research, "TempestSession") || !inherits(research, "R6")) {
    abort(
      "{.arg research} must be a succeeded TempestSession."
    )
  }
  manifest <- tempest_session_manifest(research)
  if (
    !S7::S7_inherits(manifest, TempestResearchManifest) ||
      !identical(manifest@mode, "costorm") ||
      !identical(manifest@status, "succeeded")
  ) {
    abort(
      paste0(
        "A Co-STORM ",
        purpose_lower,
        " requires its succeeded Co-STORM research Manifest."
      )
    )
  }
  stage_records <- tryCatch(
    {
      if (length(tempest_session_pending_deputy_runs(research)) > 0L) {
        abort(
          paste0(
            "A Co-STORM ",
            purpose_lower,
            " cannot retain pending Deputy execution."
          )
        )
      }
      tempest_session_agent_completion_assert_quiescent(research)
      tempest_session_async_work_assert_quiescent(research)
      tempest_stage_records_validate(
        tempest_session_stage_records(research),
        allow_running = FALSE
      )
    },
    error = function(error) {
      if (inherits(error, boundary_class)) {
        stop(error)
      }
      abort(
        paste0(
          "A Co-STORM ",
          purpose_lower,
          " requires quiescent terminal execution."
        ),
        parent = error
      )
    }
  )
  execution_identity <- tryCatch(
    {
      deputy_traces <- tempest_session_deputy_traces(research)
      experts <- research$experts
      expert_sessions <- tempest_expert_sessions_snapshot(research)
      tempest_product_authority_validate_stage_records(
        manifest,
        stage_records,
        deputy_traces = deputy_traces,
        expert_ids = tempest_product_authority_expert_ids(experts),
        expert_sessions = expert_sessions
      )
      list(
        experts = experts,
        expert_sessions = expert_sessions
      )
    },
    error = function(error) {
      abort(
        paste0(
          "A Co-STORM ",
          purpose_lower,
          " requires its exact live Deputy trace ledger."
        ),
        parent = error
      )
    }
  )
  tempest_completed_product_assert_sealed_workspace(
    tempest_session_workspace(research),
    abort,
    purpose
  )
  report_md <- tryCatch(
    tempest_session_report_read(research),
    error = function(error) {
      abort(
        paste0(
          "A Co-STORM ",
          purpose_lower,
          " requires its exact committed report."
        ),
        parent = error
      )
    }
  )
  report_reference <- tempest_completed_product_report_reference(
    manifest,
    report_md,
    abort
  )
  list(
    manifest = manifest,
    stage_records = stage_records,
    workspace = tempest_session_workspace(research),
    report_md = report_md,
    report_reference = report_reference,
    config = tempest_session_config(research),
    experts = execution_identity$experts,
    expert_sessions = execution_identity$expert_sessions,
    product_state = list(title = tempest_session_title(research))
  )
}

tempest_completed_product_context <- function(
  research,
  abort,
  boundary_class,
  purpose
) {
  if (
    !is.function(abort) ||
      !rlang::is_string(boundary_class) ||
      !rlang::is_string(purpose)
  ) {
    stop("The completed-product reader requires an internal error boundary.")
  }
  context <- tryCatch(
    {
      if (inherits(research, "TempestSession")) {
        tempest_completed_product_costorm_context(
          research,
          abort,
          boundary_class,
          purpose
        )
      } else {
        tempest_completed_product_storm_context(research, abort, purpose)
      }
    },
    error = function(error) {
      if (inherits(error, boundary_class)) {
        stop(error)
      }
      abort(
        "Could not read the exact completed research product.",
        parent = error
      )
    }
  )
  tryCatch(
    tempest_product_authority_validate(
      manifest = context$manifest,
      stage_records = context$stage_records,
      workspace = context$workspace,
      report_md = context$report_md,
      report_reference = context$report_reference,
      config = context$config,
      experts = context$experts,
      expert_sessions = context$expert_sessions,
      product_state = context$product_state,
      require_publishable = TRUE
    ),
    error = function(error) {
      abort(
        "The completed research product lacks exact publication authority.",
        parent = error
      )
    }
  )
  context
}
