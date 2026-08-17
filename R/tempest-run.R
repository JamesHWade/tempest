# Generic workflow run state and execution

tempest_run_abort <- function(
  message,
  ...,
  class = "tempest_run_error",
  parent = NULL
) {
  tempest_abort(
    message,
    ...,
    class = unique(c(class, "tempest_run_error", "tempest_error")),
    parent = parent,
    .envir = rlang::caller_env()
  )
}

tempest_run_failure_classes <- function() {
  unique(c(
    tempest_stage_failure_classes(),
    "tempest_approval_error",
    "tempest_approval_rejected_error",
    "tempest_policy_denied_error",
    "tempest_policy_error",
    "tempest_run_completion_error",
    "tempest_run_progress_error",
    "tempest_step_dependency_error",
    "tempest_step_execution_error",
    "tempest_step_input_error",
    "tempest_step_output_error",
    "tempest_step_output_validation_error"
  ))
}

tempest_run_failure_class <- function(
  error,
  default = "tempest_run_error"
) {
  known <- intersect(class(error), tempest_run_failure_classes())
  if (length(known) == 0L) {
    return(default)
  }
  known[[1]]
}

tempest_run_error_record <- function(error) {
  list(
    class = tempest_run_failure_class(
      error,
      default = "tempest_operation_error"
    ),
    message = "The operation failed."
  )
}

tempest_run_signal_failure <- function(error, run) {
  rlang::abort(
    "The workflow run failed.",
    class = unique(c(
      tempest_run_failure_class(error),
      "tempest_run_error",
      "tempest_error"
    )),
    run = run,
    run_id = run$run_id
  )
}

tempest_run_statuses <- function() {
  c(
    "pending",
    "running",
    "awaiting_approval",
    "succeeded",
    "failed",
    "cancel_requested",
    "cancelled",
    "partially_recovered"
  )
}

TempestCancelToken <- R6::R6Class(
  "TempestCancelToken",
  public = list(
    requested = FALSE,
    reason = NA_character_,
    requested_at = NA_character_,

    request = function(reason = "Cancellation requested.") {
      reason <- tempest_workflow_scalar(reason, "reason")
      if (!self$requested) {
        self$requested <- TRUE
        self$reason <- reason
        self$requested_at <- tempest_now_utc()
      }
      invisible(self)
    },

    is_requested = function() {
      isTRUE(self$requested)
    },

    snapshot = function() {
      list(
        requested = self$requested,
        reason = if (is.na(self$reason)) NULL else self$reason,
        requested_at = if (is.na(self$requested_at)) {
          NULL
        } else {
          self$requested_at
        }
      )
    }
  ),
  cloneable = FALSE
)

tempest_run_runtime_operations <- function(runtime) {
  operations <- if (inherits(runtime, "TempestRuntime")) {
    runtime$operations
  } else if (inherits(runtime, "TempestOperationRegistry")) {
    runtime
  } else if (is.list(runtime) && !is.data.frame(runtime)) {
    runtime$operations %||% runtime$operation_registry %||% NULL
  } else {
    NULL
  }
  if (!inherits(operations, "TempestOperationRegistry")) {
    tempest_run_abort(
      paste0(
        "{.arg runtime} must be a {.cls TempestRuntime}, an operation ",
        "registry, or a list containing an operation registry."
      ),
      class = "tempest_run_preflight_error"
    )
  }
  operations
}

tempest_run_policy_adapter <- function(adapter) {
  if (is.null(adapter) || is.function(adapter)) {
    return(adapter)
  }
  evaluate <- tryCatch(adapter$evaluate, error = function(error) NULL)
  if (!is.function(evaluate)) {
    tempest_run_abort(
      "{.arg policy_adapter} must be NULL, a function, or expose an `evaluate()` method.",
      class = "tempest_run_preflight_error"
    )
  }
  adapter
}

tempest_run_connection_permissions <- function(
  connection_permissions,
  runtime
) {
  if (
    !is.list(connection_permissions) ||
      is.data.frame(connection_permissions)
  ) {
    tempest_run_abort(
      "{.arg connection_permissions} must be a named list.",
      class = "tempest_run_preflight_error"
    )
  }
  if (length(connection_permissions) == 0L) {
    return(list())
  }
  permission_ids <- names(connection_permissions)
  if (
    is.null(permission_ids) ||
      anyNA(permission_ids) ||
      any(!nzchar(permission_ids)) ||
      anyDuplicated(permission_ids)
  ) {
    tempest_run_abort(
      "{.arg connection_permissions} must be uniquely named by expert or role.",
      class = "tempest_run_preflight_error"
    )
  }
  permission_ids <- vapply(
    permission_ids,
    tempest_contract_id,
    character(1),
    arg = "connection_permissions"
  )
  normalized <- lapply(connection_permissions, function(connection_ids) {
    connection_ids <- tempest_contract_ids(
      connection_ids,
      "connection_permissions"
    )
    if (anyDuplicated(connection_ids)) {
      tempest_run_abort(
        "Connection permissions cannot contain duplicate connection ids.",
        class = "tempest_run_preflight_error"
      )
    }
    sort(connection_ids)
  })
  names(normalized) <- permission_ids
  normalized <- normalized[order(names(normalized))]

  requested <- unique(unlist(normalized, use.names = FALSE))
  if (length(requested) == 0L) {
    return(normalized)
  }
  if (!inherits(runtime, "TempestRuntime")) {
    tempest_run_abort(
      "Connection permissions require a TempestRuntime.",
      class = "tempest_run_preflight_error"
    )
  }
  connection_records <- runtime$connections$list()
  unavailable <- setdiff(requested, names(connection_records))
  if (length(unavailable) > 0L) {
    tempest_run_abort(
      "Connection {.val {unavailable[[1]]}} is not registered in the runtime.",
      class = "tempest_run_preflight_error"
    )
  }
  inactive <- requested[vapply(
    requested,
    function(connection_id) {
      !identical(connection_records[[connection_id]]$state, "active")
    },
    logical(1)
  )]
  if (length(inactive) > 0L) {
    tempest_run_abort(
      "Connection {.val {inactive[[1]]}} is not active in the runtime.",
      class = "tempest_run_preflight_error"
    )
  }
  normalized
}

tempest_run_restored_connection_permissions <- function(
  snapshot,
  runtime,
  connection_permissions
) {
  saved <- tempest_run_connection_permissions(
    snapshot$connection_permissions %||% list(),
    runtime
  )
  if (is.null(connection_permissions)) {
    return(saved)
  }
  restored <- tempest_run_connection_permissions(
    connection_permissions,
    runtime
  )
  for (permission_id in names(restored)) {
    added <- setdiff(
      restored[[permission_id]],
      saved[[permission_id]] %||% character()
    )
    if (length(added) > 0L) {
      tempest_run_abort(
        paste0(
          "Restored connection permissions cannot add grants that were not ",
          "saved in the run snapshot."
        ),
        class = "tempest_run_restore_error"
      )
    }
  }
  restored
}

tempest_run_objective_data <- function(objective) {
  tempest_contract_data(objective, TempestObjective, "objective")
}

tempest_run_objective_fingerprint <- function(objective_or_data) {
  data <- if (S7::S7_inherits(objective_or_data, TempestObjective)) {
    tempest_run_objective_data(objective_or_data)
  } else {
    objective_or_data
  }
  data$fingerprint <- NULL
  digest::digest(
    tempest_canonical_json(data),
    algo = "sha256",
    serialize = FALSE
  )
}

tempest_run_objective_record <- function(objective) {
  data <- tempest_run_objective_data(objective)
  data$fingerprint <- tempest_run_objective_fingerprint(objective)
  data
}

tempest_run_objective_from_data <- function(data) {
  restored <- tempest_contract_restore_data(data, "objective")
  objective <- tryCatch(
    tempest_objective(
      description = restored$data$description,
      title = restored$data$title,
      objective_id = restored$data$objective_id,
      context = tempest_codec_list(restored$data$context),
      constraints = tempest_codec_character(restored$data$constraints),
      acceptance_criteria = tempest_codec_character(
        restored$data$acceptance_criteria
      ),
      input_resource_ids = tempest_codec_character(
        restored$data$input_resource_ids
      ),
      deliverable_ids = tempest_codec_character(
        restored$data$deliverable_ids
      ),
      metadata = tempest_codec_list(restored$data$metadata),
      created_at = restored$data$created_at,
      schema_version = as.integer(
        restored$data$schema_version %||% 1L
      )
    ),
    error = function(error) {
      tempest_run_abort(
        "Could not restore the run objective.",
        class = "tempest_run_restore_error",
        parent = error
      )
    }
  )
  if (
    !identical(
      tempest_run_objective_fingerprint(objective),
      restored$fingerprint
    )
  ) {
    tempest_run_abort(
      "Objective fingerprint validation failed.",
      class = "tempest_run_restore_error"
    )
  }
  objective
}

tempest_run_expert_map <- function(experts) {
  experts <- tempest_validate_experts(experts %||% list())
  stats::setNames(
    experts,
    vapply(experts, \(expert) expert@expert_id, character(1))
  )
}

tempest_run_assignments <- function(workflow, experts) {
  expert_ids <- sort(names(experts))
  assignments <- lapply(workflow@steps, function(step) {
    rule <- step@assignment_rule
    assigned <- switch(
      rule$type,
      none = character(),
      all = expert_ids,
      exact = rule$expert_ids
    )
    missing <- setdiff(assigned, expert_ids)
    if (length(missing) > 0L) {
      tempest_run_abort(
        "Step {.val {step@step_id}} assigns unknown expert {.val {missing[[1]]}}.",
        class = "tempest_run_preflight_error"
      )
    }
    assigned
  })
  stats::setNames(
    assignments,
    vapply(
      workflow@steps,
      \(step) step@step_id,
      character(1)
    )
  )
}

tempest_run_deliverables <- function(deliverables) {
  tempest_artifact_catalog_deliverables(deliverables %||% list())
}

tempest_run_expert_capability_ids <- function(expert, runtime) {
  if (!inherits(runtime, "TempestRuntime")) {
    return(unique(c(
      expert@required_capability_ids,
      expert@optional_capability_ids
    )))
  }
  resolution <- runtime$skills$resolve_for_expert(expert)
  unique(c(
    resolution$required_capability_ids,
    resolution$optional_capability_ids
  ))
}

tempest_run_side_effecting_capability_ids <- function(
  step,
  experts,
  runtime
) {
  if (!inherits(runtime, "TempestRuntime")) {
    return(character())
  }
  requested <- unique(c(
    step@required_capability_ids,
    step@optional_capability_ids,
    unlist(
      lapply(
        experts,
        tempest_run_expert_capability_ids,
        runtime = runtime
      ),
      use.names = FALSE
    )
  ))
  if (length(requested) == 0L) {
    return(character())
  }
  specifications <- runtime$capabilities$list()
  requested[vapply(
    requested,
    function(capability_id) {
      record <- specifications[[capability_id]] %||% NULL
      !is.null(record) &&
        isTRUE(record$specification$side_effecting)
    },
    logical(1)
  )]
}

tempest_run_result_artifacts <- function(result) {
  if (is.null(result)) {
    return(list())
  }
  if (S7::S7_inherits(result, TempestArtifact)) {
    return(list(result))
  }
  if (inherits(result, "tempest_deliverable_result")) {
    return(result$artifacts %||% list())
  }
  if (
    is.list(result) &&
      !is.data.frame(result) &&
      !is.null(result$artifacts)
  ) {
    return(tempest_artifact_catalog_artifacts(result$artifacts))
  }
  list()
}

tempest_run_result_snapshot <- function(result) {
  if (is.null(result)) {
    return(NULL)
  }
  if (inherits(result, "tempest_run_result_snapshot")) {
    return(tempest_run_normalize_snapshot_value(unclass(result)))
  }
  artifacts <- tempest_run_result_artifacts(result)
  if (length(artifacts) > 0L) {
    return(tempest_run_normalize_snapshot_value(list(
      type = "artifacts",
      artifact_ids = vapply(
        artifacts,
        \(artifact) artifact@artifact_id,
        character(1)
      )
    )))
  }
  value <- tryCatch(
    {
      tempest_canonical_json(result)
      result
    },
    error = function(error) NULL
  )
  snapshot <- if (is.null(value)) {
    list(type = "omitted_runtime_value")
  } else {
    list(type = "value", value = value)
  }
  tempest_run_normalize_snapshot_value(snapshot)
}

tempest_run_owned_published_artifacts <- function(run, deliverable_id) {
  publication_events <- Filter(
    \(event) {
      identical(event$event_type %||% "", "artifact.published") &&
        identical(event$run_id %||% "", run$run_id)
    },
    run$events
  )
  artifact_ids <- unique(vapply(
    publication_events,
    \(event) event$artifact_id %||% "",
    character(1)
  ))
  artifact_ids <- artifact_ids[nzchar(artifact_ids)]
  artifacts <- stats::setNames(
    lapply(
      artifact_ids,
      run$artifact_catalog$get,
      error = FALSE
    ),
    artifact_ids
  )
  keep <- vapply(
    artifacts,
    function(artifact) {
      if (
        is.null(artifact) ||
          !identical(artifact@deliverable_id, deliverable_id) ||
          !identical(artifact@run_id, run$run_id)
      ) {
        return(FALSE)
      }
      any(vapply(
        publication_events,
        \(event) {
          identical(event$artifact_id %||% "", artifact@artifact_id) &&
            identical(event$step_id %||% "", artifact@step_id)
        },
        logical(1)
      ))
    },
    logical(1)
  )
  artifacts[keep]
}

tempest_run_has_usable_requested_deliverable <- function(
  run,
  deliverable_id
) {
  artifacts <- tempest_run_owned_published_artifacts(run, deliverable_id)
  if (length(artifacts) == 0L) {
    return(FALSE)
  }
  any(vapply(
    artifacts,
    function(artifact) {
      deliverable <- run$artifact_catalog$get_deliverable(
        artifact@deliverable_id,
        artifact@deliverable_version
      )
      if (isTRUE(deliverable@requires_approval)) {
        identical(artifact@status, "approved")
      } else {
        artifact@status %in% c("valid", "approved")
      }
    },
    logical(1)
  ))
}

tempest_run_event <- function(
  sequence,
  run_id,
  workflow_id,
  event_type,
  status,
  step_id = NULL,
  attempt = NULL,
  expert_id = NULL,
  artifact_id = NULL,
  approval_id = NULL,
  message = NULL,
  payload = list(),
  timestamp = NULL,
  event_id = NULL
) {
  payload <- tempest_workflow_serializable_list(payload, "payload")
  optional_string <- function(value, arg) {
    if (is.null(value)) {
      return(NULL)
    }
    tempest_workflow_scalar(value, arg)
  }
  list(
    event_id = event_id %||% tempest_uuid("event"),
    sequence = as.integer(sequence),
    run_id = run_id,
    workflow_id = workflow_id,
    event_type = tempest_workflow_scalar(event_type, "event_type"),
    status = tempest_workflow_scalar(status, "status"),
    timestamp = timestamp %||% tempest_now_utc(),
    step_id = optional_string(step_id, "step_id"),
    attempt = attempt,
    expert_id = optional_string(expert_id, "expert_id"),
    artifact_id = optional_string(artifact_id, "artifact_id"),
    approval_id = optional_string(approval_id, "approval_id"),
    message = optional_string(message, "message"),
    payload = payload
  )
}

TempestRun <- R6::R6Class(
  "TempestRun",
  public = list(
    run_id = NULL,
    objective = NULL,
    workflow = NULL,
    runtime = NULL,
    experts = NULL,
    connection_permissions = NULL,
    deliverables = NULL,
    artifact_catalog = NULL,
    source_store = NULL,
    runtime_context = NULL,
    policy_adapter = NULL,
    status = "pending",
    step_states = NULL,
    assignments = NULL,
    events = NULL,
    approvals = NULL,
    policy_decisions = NULL,
    capability_grants = NULL,
    cancel_token = NULL,
    created_at = NULL,
    updated_at = NULL,

    initialize = function(
      objective,
      workflow,
      runtime,
      experts = list(),
      connection_permissions = list(),
      deliverables = list(),
      artifact_catalog = NULL,
      source_store = NULL,
      runtime_context = list(),
      policy_adapter = NULL,
      run_id = NULL,
      progress = NULL,
      restored = FALSE,
      restore_snapshot = NULL,
      partial_recovery = FALSE
    ) {
      if (!S7::S7_inherits(objective, TempestObjective)) {
        tempest_run_abort(
          "{.arg objective} must be created by {.fn tempest_objective}.",
          class = "tempest_run_preflight_error"
        )
      }
      if (!S7::S7_inherits(workflow, TempestWorkflowSpec)) {
        tempest_run_abort(
          "{.arg workflow} must be created by {.fn tempest_workflow_spec}.",
          class = "tempest_run_preflight_error"
        )
      }
      private$operations <- tempest_run_runtime_operations(runtime)
      self$runtime <- runtime
      self$objective <- objective
      self$workflow <- workflow
      self$experts <- tempest_run_expert_map(experts)
      self$connection_permissions <- tempest_run_connection_permissions(
        connection_permissions,
        runtime
      )
      self$deliverables <- tempest_run_deliverables(deliverables)
      self$assignments <- tempest_run_assignments(
        workflow,
        self$experts
      )
      self$policy_adapter <- tempest_run_policy_adapter(policy_adapter)
      self$run_id <- tempest_workflow_scalar(
        run_id %||% tempest_uuid("run"),
        "run_id"
      )
      if (!is.null(progress) && !is.function(progress)) {
        tempest_run_abort(
          "{.arg progress} must be NULL or a function.",
          class = "tempest_run_preflight_error"
        )
      }
      private$progress <- progress
      if (
        !is.null(source_store) &&
          !inherits(source_store, "ResearchWorkspace")
      ) {
        tempest_run_abort(
          "{.arg source_store} must be NULL or a ResearchWorkspace.",
          class = "tempest_run_preflight_error"
        )
      }
      self$source_store <- source_store
      if (!is.list(runtime_context) || is.data.frame(runtime_context)) {
        tempest_run_abort(
          "{.arg runtime_context} must be a process-local list.",
          class = "tempest_run_preflight_error"
        )
      }
      if (
        length(runtime_context) > 0L &&
          (is.null(names(runtime_context)) ||
            anyNA(names(runtime_context)) ||
            any(!nzchar(names(runtime_context))) ||
            anyDuplicated(names(runtime_context)))
      ) {
        tempest_run_abort(
          "{.arg runtime_context} must contain uniquely named services.",
          class = "tempest_run_preflight_error"
        )
      }
      self$runtime_context <- runtime_context
      if (is.null(artifact_catalog)) {
        artifact_catalog <- tempest_artifact_catalog(
          deliverables = self$deliverables
        )
      }
      if (!inherits(artifact_catalog, "TempestArtifactCatalog")) {
        tempest_run_abort(
          "{.arg artifact_catalog} must be a TempestArtifactCatalog.",
          class = "tempest_run_preflight_error"
        )
      }
      for (deliverable in self$deliverables) {
        artifact_catalog$register(deliverable)
      }
      self$artifact_catalog <- artifact_catalog
      self$cancel_token <- TempestCancelToken$new()
      self$events <- list()
      self$approvals <- list()
      self$policy_decisions <- list()
      self$capability_grants <- list()
      self$created_at <- tempest_now_utc()
      self$updated_at <- self$created_at
      private$preflight()
      self$step_states <- stats::setNames(
        lapply(workflow@steps, function(step) {
          list(
            step_id = step@step_id,
            status = "pending",
            attempts = list(),
            result = NULL,
            error = NULL,
            started_at = NULL,
            completed_at = NULL
          )
        }),
        vapply(workflow@steps, \(step) step@step_id, character(1))
      )
      if (isTRUE(restored)) {
        if (!is.list(restore_snapshot) || is.data.frame(restore_snapshot)) {
          tempest_run_abort(
            "Restored runs require a validated snapshot.",
            class = "tempest_run_restore_error"
          )
        }
        if (
          !identical(
            names(self$step_states),
            names(restore_snapshot$step_states)
          )
        ) {
          tempest_run_abort(
            "Run snapshot step state does not match the workflow.",
            class = "tempest_run_restore_error"
          )
        }
        if (!identical(self$assignments, restore_snapshot$assignments)) {
          tempest_run_abort(
            "Run snapshot expert assignments do not match the workflow.",
            class = "tempest_run_restore_error"
          )
        }
        private$restore_state(
          restore_snapshot,
          partial_recovery = tempest_workflow_flag(
            partial_recovery,
            "partial_recovery"
          )
        )
      } else {
        if (!is.null(restore_snapshot) || isTRUE(partial_recovery)) {
          tempest_run_abort(
            "Restore state can only be supplied for a restored run.",
            class = "tempest_run_restore_error"
          )
        }
        private$emit("workflow.created", "pending")
      }
      invisible(self)
    },

    resume = function() {
      if (self$status %in% c("succeeded", "failed", "cancelled")) {
        return(invisible(self))
      }
      if (self$cancel_token$is_requested()) {
        private$finish_cancelled()
        return(invisible(self))
      }
      if (private$has_pending_approval()) {
        self$status <- "awaiting_approval"
        return(invisible(self))
      }

      self$status <- "running"
      private$emit("workflow.running", "running")
      for (step in self$workflow@steps) {
        step_id <- step@step_id
        state <- self$step_states[[step_id]]
        if (state$status %in% c("succeeded", "failed", "cancelled")) {
          next
        }
        if (self$cancel_token$is_requested()) {
          private$finish_cancelled()
          return(invisible(self))
        }
        dependency_states <- vapply(
          step@dependency_ids,
          \(id) self$step_states[[id]]$status,
          character(1)
        )
        if (any(dependency_states != "succeeded")) {
          state$status <- "failed"
          state$error <- list(
            class = "tempest_step_dependency_error",
            message = "A dependency did not succeed."
          )
          state$completed_at <- tempest_now_utc()
          self$step_states[[step_id]] <- state
          private$emit(
            "step.failed",
            "failed",
            step_id = step_id,
            message = state$error$message
          )
          if (identical(step@failure_policy, "stop")) {
            self$status <- "failed"
            tempest_run_abort(
              "Step {.val {step_id}} has an unsuccessful dependency.",
              class = "tempest_step_dependency_error"
            )
          }
          next
        }
        missing_inputs <- step@required_input_artifact_ids[
          !vapply(
            step@required_input_artifact_ids,
            self$artifact_catalog$has,
            logical(1)
          )
        ]
        if (length(missing_inputs) > 0L) {
          self$status <- "failed"
          tempest_run_abort(
            "Step {.val {step_id}} is missing artifact {.val {missing_inputs[[1]]}}.",
            class = "tempest_step_input_error"
          )
        }
        decision <- private$authorize(step)
        if (identical(decision$decision, "require_approval")) {
          private$request_approval(step, decision)
          return(invisible(self))
        }
        if (identical(decision$decision, "deny")) {
          state$status <- "failed"
          state$error <- list(
            class = "tempest_policy_denied_error",
            message = decision$reason
          )
          state$completed_at <- tempest_now_utc()
          self$step_states[[step_id]] <- state
          self$status <- "failed"
          private$emit(
            "policy.denied",
            "failed",
            step_id = step_id,
            message = decision$reason
          )
          tempest_run_abort(
            "Policy denied step {.val {step_id}}.",
            class = "tempest_policy_denied_error"
          )
        }
        outcome <- private$execute_step(step)
        if (inherits(outcome, "tempest_run_awaiting_approval")) {
          return(invisible(self))
        }
        if (!is.null(outcome)) {
          if (identical(step@failure_policy, "stop")) {
            self$status <- "failed"
            failure <- tempest_run_error_record(outcome)
            private$emit(
              "workflow.failed",
              "failed",
              step_id = step_id,
              message = failure$message
            )
            tempest_run_abort(
              "Workflow step execution failed.",
              class = "tempest_step_execution_error"
            )
          }
        }
      }

      if (self$cancel_token$is_requested()) {
        private$finish_cancelled()
      } else {
        states <- vapply(
          self$step_states,
          \(state) state$status,
          character(1)
        )
        if (!any(states == "failed")) {
          completion_error <- tryCatch(
            {
              private$validate_requested_deliverables()
              NULL
            },
            error = function(error) error
          )
          if (!is.null(completion_error)) {
            self$status <- "failed"
            failure <- tempest_run_error_record(completion_error)
            private$emit(
              "workflow.failed",
              "failed",
              message = failure$message
            )
            tempest_run_abort(
              "Workflow deliverable validation failed.",
              class = "tempest_run_completion_error"
            )
          }
        }
        self$status <- if (
          any(states == "failed") &&
            any(states == "succeeded")
        ) {
          "partially_recovered"
        } else if (any(states == "failed")) {
          "failed"
        } else {
          "succeeded"
        }
        private$emit(
          "workflow.completed",
          self$status,
          message = if (identical(self$status, "partially_recovered")) {
            "Workflow completed with recoverable step failures."
          } else {
            NULL
          }
        )
      }
      invisible(self)
    },

    record_approval = function(
      approval_id,
      decision = c("approved", "rejected"),
      note = NULL,
      metadata = list()
    ) {
      approval_id <- tempest_workflow_scalar(approval_id, "approval_id")
      decision <- match.arg(decision)
      if (!is.null(note)) {
        note <- tempest_workflow_scalar(note, "note")
      }
      metadata <- tempest_contract_serializable_list(metadata, "metadata")
      if (is.null(self$approvals[[approval_id]])) {
        tempest_run_abort(
          "Unknown approval request {.val {approval_id}}.",
          class = "tempest_approval_error"
        )
      }
      approval <- self$approvals[[approval_id]]
      if (!identical(approval$status, "pending")) {
        tempest_run_abort(
          "Approval request {.val {approval_id}} is already resolved.",
          class = "tempest_approval_error"
        )
      }
      step_id <- approval$step_id
      approval_kind <- approval$approval_kind %||% "step"
      if (identical(approval_kind, "artifact")) {
        artifact_status <- if (identical(decision, "approved")) {
          "approved"
        } else {
          "rejected"
        }
        private$transition_artifacts(
          approval$artifact_ids %||% character(),
          artifact_status,
          step_id
        )
      }
      approval$status <- decision
      approval$note <- note
      approval$metadata <- metadata
      approval$decided_at <- tempest_now_utc()
      self$approvals[[approval_id]] <- approval
      if (identical(approval_kind, "artifact")) {
        if (identical(decision, "approved")) {
          self$step_states[[step_id]]$status <- "succeeded"
          self$step_states[[step_id]]$error <- NULL
          self$status <- "pending"
        } else {
          self$step_states[[step_id]]$status <- "failed"
          self$step_states[[step_id]]$error <- list(
            class = "tempest_approval_rejected_error",
            message = note %||% "Artifact approval was rejected."
          )
          self$step_states[[step_id]]$completed_at <- tempest_now_utc()
          self$status <- "failed"
        }
      } else if (identical(decision, "approved")) {
        private$approved_steps[[step_id]] <- TRUE
        self$step_states[[step_id]]$status <- "pending"
        self$status <- "pending"
      } else {
        self$step_states[[step_id]]$status <- "failed"
        self$step_states[[step_id]]$error <- list(
          class = "tempest_approval_rejected_error",
          message = note %||% "Approval was rejected."
        )
        self$step_states[[step_id]]$completed_at <- tempest_now_utc()
        self$status <- "failed"
      }
      private$emit(
        "approval.resolved",
        decision,
        step_id = step_id,
        approval_id = approval_id,
        message = note,
        payload = list(
          approval_kind = approval_kind,
          artifact_ids = approval$artifact_ids %||% character()
        )
      )
      if (identical(decision, "rejected")) {
        private$emit(
          "step.failed",
          "failed",
          step_id = step_id,
          message = self$step_states[[step_id]]$error$message
        )
        private$emit(
          "workflow.failed",
          "failed",
          step_id = step_id,
          message = self$step_states[[step_id]]$error$message
        )
      } else if (identical(approval_kind, "artifact")) {
        attempts <- self$step_states[[step_id]]$attempts
        private$emit(
          "step.succeeded",
          "succeeded",
          step_id = step_id,
          attempt = if (length(attempts) == 0L) {
            NULL
          } else {
            attempts[[length(attempts)]]$attempt
          }
        )
      }
      invisible(self)
    },

    cancel = function(reason = "Cancellation requested.") {
      if (self$status %in% c("succeeded", "failed", "cancelled")) {
        return(invisible(self))
      }
      self$cancel_token$request(reason)
      self$status <- "cancel_requested"
      private$emit(
        "cancellation.requested",
        "cancel_requested",
        message = reason
      )
      invisible(self)
    },

    artifact = function(artifact_id) {
      self$artifact_catalog$get(artifact_id)
    },

    artifacts = function(...) {
      self$artifact_catalog$list(...)
    },

    snapshot = function() {
      tempest_run_snapshot(self)
    }
  ),
  private = list(
    operations = NULL,
    resolved_operations = NULL,
    progress = NULL,
    sequence = 0L,
    approved_steps = list(),

    preflight = function() {
      deliverable_ids <- vapply(
        self$deliverables,
        \(deliverable) deliverable@deliverable_id,
        character(1)
      )
      missing_deliverables <- setdiff(
        self$objective@deliverable_ids,
        deliverable_ids
      )
      if (length(missing_deliverables) > 0L) {
        tempest_run_abort(
          "Objective requests unavailable deliverable {.val {missing_deliverables[[1]]}}.",
          class = "tempest_run_preflight_error"
        )
      }
      if (length(self$objective@input_resource_ids) > 0L) {
        if (!inherits(self$source_store, "ResearchWorkspace")) {
          tempest_run_abort(
            "Objective input resources require a ResearchWorkspace.",
            class = "tempest_run_preflight_error"
          )
        }
        resource_ids <- vapply(
          self$source_store$list_retrieved_resources(),
          \(resource) resource@resource_id,
          character(1)
        )
        missing_resources <- setdiff(
          self$objective@input_resource_ids,
          resource_ids
        )
        if (length(missing_resources) > 0L) {
          tempest_run_abort(
            "Objective references unavailable input resource {.val {missing_resources[[1]]}}.",
            class = "tempest_run_preflight_error"
          )
        }
      }
      objective_type <- self$objective@metadata$objective_type %||%
        "tempest_objective"
      if (!objective_type %in% self$workflow@supported_objective_types) {
        tempest_run_abort(
          "Workflow does not support objective type {.val {objective_type}}.",
          class = "tempest_run_preflight_error"
        )
      }
      if (length(self$workflow@supported_deliverable_types) > 0L) {
        unsupported <- vapply(
          self$deliverables,
          \(deliverable) {
            !deliverable@content_type %in%
              self$workflow@supported_deliverable_types
          },
          logical(1)
        )
        if (any(unsupported)) {
          tempest_run_abort(
            "Workflow does not support deliverable content type {.val {self$deliverables[[which(unsupported)[[1]]]]@content_type}}.",
            class = "tempest_run_preflight_error"
          )
        }
      }
      for (deliverable in self$deliverables) {
        tryCatch(
          tempest_deliverable_plan(
            deliverable = deliverable,
            registry = private$operations,
            catalog = self$artifact_catalog
          ),
          error = function(error) {
            tempest_run_abort(
              "Could not preflight deliverable operations for {.val {deliverable@deliverable_id}}.",
              class = "tempest_run_preflight_error",
              parent = error
            )
          }
        )
      }
      private$resolved_operations <- stats::setNames(
        lapply(self$workflow@steps, function(step) {
          tryCatch(
            list(
              implementation = private$operations$resolve(
                step@operation_id,
                version = step@operation_version,
                kind = "step"
              ),
              descriptor = private$operations$describe(
                step@operation_id,
                version = step@operation_version,
                kind = "step"
              )
            ),
            error = function(error) {
              tempest_run_abort(
                "Could not preflight step operation {.val {step@operation_id}}.",
                class = "tempest_run_preflight_error",
                parent = error
              )
            }
          )
        }),
        vapply(
          self$workflow@steps,
          \(step) step@step_id,
          character(1)
        )
      )
      uses_scoped_runtime <- any(vapply(
        self$workflow@steps,
        function(step) {
          assigned <- self$experts[self$assignments[[step@step_id]]]
          length(step@required_capability_ids) > 0L ||
            length(step@optional_capability_ids) > 0L ||
            any(vapply(
              assigned,
              function(expert) {
                length(expert@skill_ids) > 0L ||
                  length(expert@required_capability_ids) > 0L ||
                  length(expert@optional_capability_ids) > 0L
              },
              logical(1)
            ))
        },
        logical(1)
      ))
      if (uses_scoped_runtime && !inherits(self$runtime, "TempestRuntime")) {
        tempest_run_abort(
          "Expert skills and scoped capabilities require a TempestRuntime.",
          class = "tempest_run_preflight_error"
        )
      }
      for (step in self$workflow@steps) {
        if (
          length(step@required_capability_ids) > 0L ||
            length(step@optional_capability_ids) > 0L
        ) {
          private$step_model_role(step)
        }
      }
      invisible(self)
    },

    emit = function(
      event_type,
      status,
      step_id = NULL,
      attempt = NULL,
      expert_id = NULL,
      artifact_id = NULL,
      approval_id = NULL,
      message = NULL,
      payload = list()
    ) {
      private$sequence <- private$sequence + 1L
      event <- tempest_run_event(
        sequence = private$sequence,
        run_id = self$run_id,
        workflow_id = self$workflow@workflow_id,
        event_type = event_type,
        status = status,
        step_id = step_id,
        attempt = attempt,
        expert_id = expert_id,
        artifact_id = artifact_id,
        approval_id = approval_id,
        message = message,
        payload = payload
      )
      self$events[[length(self$events) + 1L]] <- event
      self$updated_at <- event$timestamp
      if (!is.null(private$progress)) {
        tryCatch(
          private$progress(event),
          error = function(error) {
            tempest_run_abort(
              "Run progress callback failed.",
              class = "tempest_run_progress_error"
            )
          }
        )
      }
      invisible(event)
    },

    authorize = function(step) {
      if (isTRUE(private$approved_steps[[step@step_id]])) {
        return(list(decision = "allow", reason = "Approved by host."))
      }
      assigned_experts <- self$experts[self$assignments[[step@step_id]]]
      side_effecting_capability_ids <-
        tempest_run_side_effecting_capability_ids(
          step,
          assigned_experts,
          self$runtime
        )
      needs_policy <- step@side_effecting ||
        step@approval_checkpoint ||
        length(side_effecting_capability_ids) > 0L
      if (!needs_policy) {
        return(list(decision = "allow", reason = "No policy gate."))
      }
      if (is.null(self$policy_adapter)) {
        decision <- list(
          decision = "require_approval",
          reason = "Step or capability requires explicit host approval.",
          metadata = list()
        )
      } else {
        arguments <- list(
          objective = self$objective,
          workflow = self$workflow,
          step = step,
          run_id = self$run_id,
          expert_ids = self$assignments[[step@step_id]],
          side_effecting = step@side_effecting,
          approval_checkpoint = step@approval_checkpoint,
          side_effecting_capability_ids = side_effecting_capability_ids
        )
        value <- tryCatch(
          {
            if (is.function(self$policy_adapter)) {
              tempest_call_operation(self$policy_adapter, arguments)
            } else {
              tempest_call_operation(
                self$policy_adapter$evaluate,
                arguments
              )
            }
          },
          error = function(error) {
            tempest_run_abort(
              "Policy adapter evaluation failed.",
              class = "tempest_policy_error"
            )
          }
        )
        decision <- if (is.character(value)) {
          list(decision = value, reason = NA_character_, metadata = list())
        } else {
          value
        }
        if (!is.list(decision) || is.data.frame(decision)) {
          tempest_run_abort(
            "Policy adapter returned an invalid decision.",
            class = "tempest_policy_error"
          )
        }
        decision$decision <- tempest_workflow_scalar(
          decision$decision,
          "policy decision"
        )
        if (!decision$decision %in% c("allow", "deny", "require_approval")) {
          tempest_run_abort(
            "Policy decision must be allow, deny, or require_approval.",
            class = "tempest_policy_error"
          )
        }
        decision$reason <- decision$reason %||%
          switch(
            decision$decision,
            allow = "Policy allowed the step.",
            deny = "Policy denied the step.",
            require_approval = "Policy requires host approval."
          )
        decision$reason <- tempest_workflow_scalar(
          decision$reason,
          "policy reason"
        )
        decision$metadata <- tempest_contract_serializable_list(
          decision$metadata %||% list(),
          "policy metadata"
        )
      }
      record <- c(
        list(
          decision_id = tempest_uuid("policy"),
          step_id = step@step_id,
          side_effecting_capability_ids = side_effecting_capability_ids,
          created_at = tempest_now_utc()
        ),
        decision
      )
      self$policy_decisions[[length(self$policy_decisions) + 1L]] <- record
      record
    },

    request_approval = function(step, decision) {
      existing <- Filter(
        \(approval) {
          identical(approval$step_id, step@step_id) &&
            identical(approval$status, "pending")
        },
        self$approvals
      )
      if (length(existing) > 0L) {
        self$status <- "awaiting_approval"
        return(invisible(existing[[1]]))
      }
      approval_id <- tempest_uuid("approval")
      approval <- list(
        approval_id = approval_id,
        approval_kind = "step",
        step_id = step@step_id,
        artifact_ids = character(),
        status = "pending",
        reason = decision$reason,
        policy_decision_id = decision$decision_id,
        requested_at = tempest_now_utc(),
        decided_at = NULL,
        note = NULL,
        metadata = decision$metadata %||% list()
      )
      self$approvals[[approval_id]] <- approval
      self$step_states[[step@step_id]]$status <- "awaiting_approval"
      self$status <- "awaiting_approval"
      private$emit(
        "approval.requested",
        "awaiting_approval",
        step_id = step@step_id,
        approval_id = approval_id,
        message = decision$reason
      )
      invisible(approval)
    },

    request_artifact_approval = function(step, artifact_ids) {
      artifact_ids <- sort(unique(artifact_ids))
      existing <- Filter(
        \(approval) {
          identical(approval$approval_kind %||% "step", "artifact") &&
            identical(approval$step_id, step@step_id) &&
            identical(
              sort(approval$artifact_ids %||% character()),
              artifact_ids
            ) &&
            identical(approval$status, "pending")
        },
        self$approvals
      )
      if (length(existing) > 0L) {
        self$status <- "awaiting_approval"
        return(invisible(existing[[1]]))
      }
      approval_id <- tempest_uuid("approval")
      approval <- list(
        approval_id = approval_id,
        approval_kind = "artifact",
        step_id = step@step_id,
        artifact_ids = artifact_ids,
        status = "pending",
        reason = "Deliverable output requires explicit host approval.",
        policy_decision_id = NULL,
        requested_at = tempest_now_utc(),
        decided_at = NULL,
        note = NULL,
        metadata = list()
      )
      self$approvals[[approval_id]] <- approval
      self$step_states[[step@step_id]]$status <- "awaiting_approval"
      self$status <- "awaiting_approval"
      private$emit(
        "approval.requested",
        "awaiting_approval",
        step_id = step@step_id,
        approval_id = approval_id,
        message = approval$reason,
        payload = list(
          approval_kind = "artifact",
          artifact_ids = artifact_ids
        )
      )
      invisible(approval)
    },

    has_pending_approval = function() {
      any(vapply(
        self$approvals,
        \(approval) identical(approval$status, "pending"),
        logical(1)
      ))
    },

    allowed_connection_ref_ids = function(
      expert_ids = character(),
      model_role = NULL
    ) {
      permission_ids <- unique(c(
        expert_ids,
        if (is.null(model_role)) character() else model_role
      ))
      permission_ids <- permission_ids[
        !is.na(permission_ids) & nzchar(permission_ids)
      ]
      unique(unlist(
        self$connection_permissions[
          intersect(permission_ids, names(self$connection_permissions))
        ],
        use.names = FALSE
      ))
    },

    step_model_role = function(step) {
      expert_ids <- self$assignments[[step@step_id]]
      experts <- self$experts[expert_ids]
      if (length(experts) == 0L) {
        return(NULL)
      }
      roles <- vapply(
        experts,
        \(expert) expert@model_role,
        character(1)
      )
      if (anyNA(roles)) {
        tempest_run_abort(
          c(
            "Step-scoped capabilities require one concrete shared model role.",
            x = "Step {.val {step@step_id}} assigns an expert whose host model policy has not been resolved.",
            i = "Resolve every {.arg model_policy_ref} to the same {.arg model_role} before starting the run."
          ),
          class = "tempest_run_preflight_error"
        )
      }
      if (length(unique(roles)) > 1L) {
        tempest_run_abort(
          c(
            "Step-scoped capabilities require one shared model role.",
            x = "Step {.val {step@step_id}} assigns experts with heterogeneous model roles.",
            i = "Split the step or assign experts with the same model role."
          ),
          class = "tempest_run_preflight_error"
        )
      }
      roles[[1]]
    },

    record_capability_grants = function(
      step_id,
      attempt,
      expert_grants,
      step_grants
    ) {
      record <- tempest_run_normalize_snapshot_value(list(
        attempt = as.integer(attempt),
        experts = expert_grants,
        step = step_grants,
        recorded_at = tempest_now_utc()
      ))
      history <- self$capability_grants[[step_id]]$attempts %||% list()
      history[[as.character(attempt)]] <- record
      self$capability_grants[[
        step_id
      ]] <- tempest_run_normalize_snapshot_value(c(
        record,
        list(attempts = history)
      ))
      invisible(record)
    },

    operation_context = function(step, attempt) {
      expert_ids <- self$assignments[[step@step_id]]
      experts <- self$experts[expert_ids]
      input_artifacts <- stats::setNames(
        lapply(
          step@required_input_artifact_ids,
          self$artifact_catalog$get
        ),
        step@required_input_artifact_ids
      )
      expert_resolutions <- list()
      capability_resolution <- NULL
      capability_context <- utils::modifyList(
        self$runtime_context,
        list(
          run_id = self$run_id,
          step_id = step@step_id,
          objective = self$objective,
          source_store = self$source_store,
          cancel_token = self$cancel_token
        )
      )
      if (inherits(self$runtime, "TempestRuntime")) {
        for (expert_id in names(experts)) {
          expert <- experts[[expert_id]]
          resolution <- tryCatch(
            self$runtime$resolve_expert(
              expert,
              allowed_connection_ref_ids = private$allowed_connection_ref_ids(
                expert_ids = expert@expert_id,
                model_role = expert@model_role
              ),
              context = capability_context
            ),
            error = function(error) error
          )
          if (inherits(resolution, "error")) {
            expert_grants <- lapply(
              expert_resolutions,
              \(value) value$grants
            )
            expert_grants[[expert_id]] <- list()
            private$record_capability_grants(
              step@step_id,
              attempt,
              expert_grants,
              list()
            )
            tempest_run_abort(
              "Runtime expert resolution failed.",
              class = "tempest_step_execution_error"
            )
          }
          expert_resolutions[[expert_id]] <- resolution
        }
        if (
          length(step@required_capability_ids) > 0L ||
            length(step@optional_capability_ids) > 0L
        ) {
          model_role <- private$step_model_role(step)
          capability_resolution <- tryCatch(
            self$runtime$capabilities$resolve(
              required_capability_ids = step@required_capability_ids,
              optional_capability_ids = step@optional_capability_ids,
              allowed_connection_ref_ids = private$allowed_connection_ref_ids(
                expert_ids = expert_ids,
                model_role = model_role
              ),
              model_role = model_role,
              context = capability_context
            ),
            error = function(error) error
          )
          if (inherits(capability_resolution, "error")) {
            private$record_capability_grants(
              step@step_id,
              attempt,
              lapply(
                expert_resolutions,
                \(resolution) resolution$grants
              ),
              list()
            )
            tempest_run_abort(
              "Runtime capability resolution failed.",
              class = "tempest_step_execution_error"
            )
          }
        }
      }
      private$record_capability_grants(
        step@step_id,
        attempt,
        lapply(
          expert_resolutions,
          \(resolution) resolution$grants
        ),
        if (is.null(capability_resolution)) {
          list()
        } else {
          capability_resolution$grants
        }
      )
      utils::modifyList(
        self$runtime_context,
        list(
          run_id = self$run_id,
          attempt = attempt,
          objective = self$objective,
          workflow = self$workflow,
          step = step,
          experts = experts,
          expert = if (length(experts) == 1L) experts[[1]] else NULL,
          expert_ids = expert_ids,
          expert_id = if (length(expert_ids) == 1L) {
            expert_ids[[1]]
          } else {
            NULL
          },
          expert_resolutions = expert_resolutions,
          capability_resolution = capability_resolution,
          input_artifacts = input_artifacts,
          artifact_catalog = self$artifact_catalog,
          source_store = self$source_store,
          runtime = self$runtime,
          cancel_token = self$cancel_token,
          runtime_context = self$runtime_context
        )
      )
    },

    validate_requested_deliverables = function() {
      for (deliverable_id in self$objective@deliverable_ids) {
        if (
          !tempest_run_has_usable_requested_deliverable(
            self,
            deliverable_id
          )
        ) {
          tempest_run_abort(
            "Workflow did not produce a usable artifact for requested deliverable {.val {deliverable_id}}.",
            class = "tempest_run_completion_error"
          )
        }
      }
      invisible(self)
    },

    export_approved_artifact = function(artifact, step_id) {
      deliverable <- self$artifact_catalog$get_deliverable(
        artifact@deliverable_id,
        artifact@deliverable_version
      )
      if (length(deliverable@exporter_ids) == 0L) {
        return(artifact)
      }
      context <- utils::modifyList(
        self$runtime_context,
        list(
          run_id = self$run_id,
          objective = self$objective,
          workflow = self$workflow,
          step = self$workflow@steps[[step_id]],
          source_store = self$source_store,
          artifact_catalog = self$artifact_catalog
        )
      )
      exported <- artifact
      for (exporter_id in deliverable@exporter_ids) {
        exporter <- tempest_deliverable_resolve(
          private$operations,
          deliverable,
          exporter_id,
          "exporter"
        )
        value <- tempest_deliverable_run_operation(
          exporter,
          "approved export",
          list(
            artifact = exported,
            deliverable = deliverable,
            context = context,
            runtime = self$runtime_context,
            validation_results = exported@validation_results
          )
        )
        if (is.null(value)) {
          next
        }
        if (!S7::S7_inherits(value, TempestArtifact)) {
          tempest_run_abort(
            "Exporter {.val {exporter_id}} must return a typed artifact or `NULL`.",
            class = "tempest_approval_error"
          )
        }
        immutable_changes <-
          tempest_deliverable_export_immutable_changes(
            artifact,
            value
          )
        if (length(immutable_changes) > 0L) {
          tempest_run_abort(
            "Exporter {.val {exporter_id}} changed immutable approved artifact field {.val {immutable_changes[[1]]}}.",
            class = "tempest_approval_error"
          )
        }
        exported <- value
      }
      exported
    },

    enforce_artifact_approval = function(artifact) {
      deliverable <- self$artifact_catalog$get_deliverable(
        artifact@deliverable_id,
        artifact@deliverable_version
      )
      if (
        !isTRUE(deliverable@requires_approval) ||
          !artifact@status %in% c("valid", "approved")
      ) {
        return(artifact)
      }
      record <- tempest_artifact_data(artifact)
      record$status <- "awaiting_approval"
      tempest_artifact_from_data(record, deliverable)
    },

    assert_artifact_ownership = function(artifact, step) {
      if (
        !identical(artifact@run_id, self$run_id) ||
          !identical(artifact@step_id, step@step_id)
      ) {
        tempest_run_abort(
          "Step {.val {step@step_id}} published artifact {.val {artifact@artifact_id}} with foreign or missing run or step provenance.",
          class = "tempest_step_output_error"
        )
      }
      invisible(artifact)
    },

    publish_result_artifacts = function(step, result) {
      artifacts <- tempest_run_result_artifacts(result)
      artifacts <- lapply(
        artifacts,
        function(artifact) {
          private$assert_artifact_ownership(artifact, step)
          private$enforce_artifact_approval(artifact)
        }
      )
      for (artifact in artifacts) {
        if (self$artifact_catalog$has(artifact@artifact_id)) {
          existing <- self$artifact_catalog$get(artifact@artifact_id)
          enforced_existing <- private$enforce_artifact_approval(existing)
          if (
            !identical(
              tempest_artifact_data(enforced_existing),
              tempest_artifact_data(artifact)
            )
          ) {
            tempest_run_abort(
              "Step {.val {step@step_id}} published conflicting artifact {.val {artifact@artifact_id}}.",
              class = "tempest_step_output_error"
            )
          }
          if (
            !identical(
              tempest_artifact_data(existing),
              tempest_artifact_data(enforced_existing)
            )
          ) {
            self$artifact_catalog$add(
              enforced_existing,
              replace = TRUE
            )
          }
        } else {
          self$artifact_catalog$add(artifact)
        }
      }
      missing <- step@produced_artifact_ids[
        !vapply(
          step@produced_artifact_ids,
          self$artifact_catalog$has,
          logical(1)
        )
      ]
      if (length(missing) > 0L) {
        tempest_run_abort(
          "Step {.val {step@step_id}} did not publish artifact {.val {missing[[1]]}}.",
          class = "tempest_step_output_error"
        )
      }
      published_ids <- unique(c(
        vapply(
          artifacts,
          \(artifact) artifact@artifact_id,
          character(1)
        ),
        step@produced_artifact_ids
      ))
      published <- stats::setNames(
        lapply(published_ids, self$artifact_catalog$get),
        published_ids
      )
      published <- lapply(published, function(artifact) {
        private$assert_artifact_ownership(artifact, step)
        enforced <- private$enforce_artifact_approval(artifact)
        if (
          !identical(
            tempest_artifact_data(artifact),
            tempest_artifact_data(enforced)
          )
        ) {
          self$artifact_catalog$add(enforced, replace = TRUE)
        }
        enforced
      })
      for (artifact in published) {
        private$emit(
          "artifact.published",
          artifact@status,
          step_id = step@step_id,
          artifact_id = artifact@artifact_id
        )
      }
      statuses <- vapply(
        published,
        \(artifact) artifact@status,
        character(1)
      )
      unusable <- names(statuses)[
        statuses %in% c("draft", "invalid", "rejected")
      ]
      if (length(unusable) > 0L) {
        tempest_run_abort(
          "Step {.val {step@step_id}} published unusable artifact {.val {unusable[[1]]}} with status {.val {statuses[[unusable[[1]]]]}}.",
          class = "tempest_step_output_validation_error"
        )
      }
      list(
        artifacts = artifacts,
        awaiting_approval_ids = names(statuses)[
          statuses == "awaiting_approval"
        ]
      )
    },

    execute_step = function(step) {
      step_id <- step@step_id
      state <- self$step_states[[step_id]]
      state$status <- "running"
      state$started_at <- state$started_at %||% tempest_now_utc()
      self$step_states[[step_id]] <- state
      private$emit("step.running", "running", step_id = step_id)
      max_attempts <- step@retry_policy$max_attempts
      completed_attempts <- vapply(
        state$attempts,
        \(attempt) as.integer(attempt$attempt %||% NA_integer_),
        integer(1)
      )
      grant_attempt_names <- names(
        self$capability_grants[[step_id]]$attempts %||% list()
      )
      grant_attempts <- suppressWarnings(as.integer(grant_attempt_names))
      recorded_attempts <- c(completed_attempts, grant_attempts)
      recorded_attempts <- recorded_attempts[
        !is.na(recorded_attempts) & recorded_attempts > 0L
      ]
      first_attempt <- max(c(0L, recorded_attempts)) + 1L
      attempt_sequence <- if (first_attempt <= max_attempts) {
        seq.int(first_attempt, max_attempts)
      } else {
        integer()
      }
      last_error <- rlang::error_cnd(
        "tempest_retry_exhausted_error",
        message = "Recovered step has exhausted its retry budget."
      )

      for (attempt in attempt_sequence) {
        if (self$cancel_token$is_requested()) {
          return(NULL)
        }
        started_at <- tempest_now_utc()
        private$emit(
          "step.attempt.started",
          "running",
          step_id = step_id,
          attempt = attempt
        )
        result <- tryCatch(
          {
            context <- private$operation_context(step, attempt)
            operation <- private$resolved_operations[[step_id]]$implementation
            value <- tempest_call_operation(
              operation,
              c(
                context,
                list(
                  context = context,
                  run = self
                )
              )
            )
            if (self$cancel_token$is_requested()) {
              structure(
                list(message = "Cancellation requested."),
                class = c("tempest_cancelled_operation", "error", "condition")
              )
            } else {
              publication <- private$publish_result_artifacts(step, value)
              structure(
                list(
                  value = value,
                  awaiting_approval_ids = publication$awaiting_approval_ids
                ),
                class = "tempest_step_execution_result"
              )
            }
          },
          error = function(error) error
        )
        completed_at <- tempest_now_utc()
        if (inherits(result, "tempest_cancelled_operation")) {
          self$cancel_token$request(result$message)
          return(NULL)
        }
        state <- self$step_states[[step_id]]
        if (!inherits(result, "error")) {
          awaiting_approval <-
            length(result$awaiting_approval_ids) > 0L
          attempt_status <- if (awaiting_approval) {
            "awaiting_approval"
          } else {
            "succeeded"
          }
          state$attempts[[length(state$attempts) + 1L]] <- list(
            attempt = attempt,
            status = attempt_status,
            started_at = started_at,
            completed_at = completed_at,
            result = tempest_run_result_snapshot(result$value)
          )
          state$status <- attempt_status
          state$result <- result$value
          state$error <- NULL
          state$completed_at <- completed_at
          self$step_states[[step_id]] <- state
          if (awaiting_approval) {
            private$emit(
              "step.awaiting_approval",
              "awaiting_approval",
              step_id = step_id,
              attempt = attempt
            )
            private$request_artifact_approval(
              step,
              result$awaiting_approval_ids
            )
            return(structure(
              list(),
              class = "tempest_run_awaiting_approval"
            ))
          }
          private$emit(
            "step.succeeded",
            "succeeded",
            step_id = step_id,
            attempt = attempt
          )
          return(NULL)
        }
        last_error <- result
        failure <- tempest_run_error_record(result)
        state$attempts[[length(state$attempts) + 1L]] <- list(
          attempt = attempt,
          status = "failed",
          started_at = started_at,
          completed_at = completed_at,
          error = failure
        )
        self$step_states[[step_id]] <- state
        private$emit(
          "step.attempt.failed",
          "failed",
          step_id = step_id,
          attempt = attempt,
          message = failure$message
        )
      }
      state <- self$step_states[[step_id]]
      state$status <- "failed"
      state$error <- tempest_run_error_record(last_error)
      state$completed_at <- tempest_now_utc()
      self$step_states[[step_id]] <- state
      private$emit(
        "step.failed",
        "failed",
        step_id = step_id,
        attempt = max_attempts,
        message = state$error$message
      )
      last_error
    },

    transition_artifacts = function(artifact_ids, status, step_id) {
      artifacts <- stats::setNames(
        lapply(artifact_ids, self$artifact_catalog$get),
        artifact_ids
      )
      invalid <- names(artifacts)[
        !vapply(
          artifacts,
          \(artifact) artifact@status %in% c("awaiting_approval", status),
          logical(1)
        )
      ]
      if (length(invalid) > 0L) {
        tempest_run_abort(
          "Artifact {.val {invalid[[1]]}} is not awaiting approval.",
          class = "tempest_approval_error"
        )
      }
      updated_artifacts <- lapply(artifacts, function(artifact) {
        if (identical(artifact@status, status)) {
          return(artifact)
        }
        record <- tempest_artifact_data(artifact)
        record$status <- status
        record$updated_at <- tempest_now_utc()
        deliverable <- self$artifact_catalog$get_deliverable(
          artifact@deliverable_id,
          artifact@deliverable_version
        )
        updated <- tempest_artifact_from_data(record, deliverable)
        if (identical(status, "approved")) {
          updated <- private$export_approved_artifact(updated, step_id)
        }
        updated
      })
      for (artifact_id in artifact_ids) {
        artifact <- artifacts[[artifact_id]]
        if (identical(artifact@status, status)) {
          next
        }
        if (!identical(artifact@status, "awaiting_approval")) {
          tempest_run_abort(
            "Artifact {.val {artifact_id}} is not awaiting approval.",
            class = "tempest_approval_error"
          )
        }
        updated <- updated_artifacts[[artifact_id]]
        self$artifact_catalog$add(updated, replace = TRUE)
        private$emit(
          paste0("artifact.", status),
          status,
          step_id = step_id,
          artifact_id = artifact_id
        )
      }
      invisible(artifact_ids)
    },

    finish_cancelled = function() {
      for (approval_id in names(self$approvals)) {
        if (identical(self$approvals[[approval_id]]$status, "pending")) {
          self$approvals[[approval_id]]$status <- "cancelled"
          self$approvals[[approval_id]]$note <- self$cancel_token$reason
          self$approvals[[approval_id]]$decided_at <- tempest_now_utc()
        }
      }
      for (step_id in names(self$step_states)) {
        if (
          self$step_states[[step_id]]$status %in%
            c("pending", "running", "awaiting_approval")
        ) {
          self$step_states[[step_id]]$status <- "cancelled"
          self$step_states[[step_id]]$completed_at <- tempest_now_utc()
        }
      }
      self$status <- "cancelled"
      private$emit(
        "cancellation.confirmed",
        "cancelled",
        message = self$cancel_token$reason
      )
      invisible(self)
    },

    restore_state = function(snapshot, partial_recovery = FALSE) {
      self$status <- snapshot$status
      self$step_states <- snapshot$step_states
      self$step_states <- lapply(self$step_states, function(state) {
        if (!is.null(state$result)) {
          class(state$result) <- unique(c(
            "tempest_run_result_snapshot",
            class(state$result)
          ))
        }
        state
      })
      self$assignments <- snapshot$assignments
      self$events <- snapshot$events
      self$approvals <- snapshot$approvals
      self$policy_decisions <- snapshot$policy_decisions
      self$capability_grants <- snapshot$capability_grants %||% list()
      private$sequence <- as.integer(snapshot$sequence)
      self$created_at <- snapshot$created_at
      self$updated_at <- snapshot$updated_at
      for (approval in self$approvals) {
        if (
          identical(approval$status, "approved") &&
            identical(approval$approval_kind %||% "step", "step")
        ) {
          private$approved_steps[[approval$step_id]] <- TRUE
        }
      }
      if (isTRUE(snapshot$cancel_token$requested)) {
        self$cancel_token$request(
          snapshot$cancel_token$reason %||% "Cancellation requested."
        )
      }
      running <- vapply(
        self$step_states,
        \(state) identical(state$status, "running"),
        logical(1)
      )
      if (
        isTRUE(partial_recovery) &&
          (identical(self$status, "running") || any(running))
      ) {
        self$status <- "partially_recovered"
        for (step_id in names(self$step_states)[running]) {
          state <- self$step_states[[step_id]]
          started <- Filter(
            function(event) {
              identical(event$event_type, "step.attempt.started") &&
                identical(event$step_id, step_id)
            },
            self$events
          )
          if (length(started) > 0L) {
            interrupted <- started[[length(started)]]
            interrupted_attempt <- suppressWarnings(
              as.integer(interrupted$attempt %||% NA_integer_)
            )
            recorded <- vapply(
              state$attempts,
              \(attempt) as.integer(attempt$attempt),
              integer(1)
            )
            if (
              length(interrupted_attempt) == 1L &&
                !is.na(interrupted_attempt) &&
                !interrupted_attempt %in% recorded
            ) {
              state$attempts[[length(state$attempts) + 1L]] <- list(
                attempt = interrupted_attempt,
                status = "interrupted",
                started_at = interrupted$timestamp,
                completed_at = tempest_now_utc(),
                error = list(
                  class = "tempest_partial_recovery",
                  message = "Attempt was interrupted before recovery."
                )
              )
            }
          }
          state$status <- "pending"
          self$step_states[[step_id]] <- state
        }
      }
      invisible(self)
    }
  ),
  cloneable = FALSE
)

#' Execute an application-neutral Tempest workflow
#'
#' `r lifecycle::badge("experimental")`
#'
#' This experimental API is frozen and scheduled for removal in Tempest 0.2.0.
#' No compatibility shim is planned; see
#' [tempest-generic-kernel-retirement].
#'
#' Construction preflights every step operation before execution. The function
#' returns a mutable run in a terminal state or in nonblocking
#' `awaiting_approval` state. If execution fails after construction, the
#' classed error contains the inspectable failed run in `condition$run` and its
#' identity in `condition$run_id`. Construction and preflight errors occur
#' before a run exists.
#'
#' @param objective A [tempest_objective()].
#' @param workflow A [tempest_workflow_spec()].
#' @param runtime A [tempest_runtime()], operation registry, or runtime list
#'   containing an operation registry.
#' @param experts Exact pool of selected [tempest_expert()] profiles.
#' @param connection_permissions Named list mapping expert or model-role ids to
#'   opaque connection ids allowed for this run. Step-scoped capabilities use
#'   the union of assigned expert-id permissions and their one shared model-role
#'   permission.
#' @param deliverables Deliverable specifications available to the run.
#' @param artifact_catalog Optional typed artifact catalog.
#' @param source_store Optional [ResearchWorkspace] evidence ledger. The
#'   argument name is retained for the frozen generic API.
#' @param runtime_context Process-local named services, such as a retriever or
#'   expert-session manager, made available to operations and capability
#'   factories. Approved-output exporters also receive these services as their
#'   runtime. Runtime context is never serialized.
#' @param policy_adapter Optional policy function or object with `evaluate()`.
#' @param run_id Optional stable run identifier.
#' @param progress Optional generic event callback.
#' @return A mutable `TempestRun`.
#' @export
tempest_run_workflow <- function(
  objective,
  workflow,
  runtime,
  experts = list(),
  connection_permissions = list(),
  deliverables = list(),
  artifact_catalog = NULL,
  source_store = NULL,
  runtime_context = list(),
  policy_adapter = NULL,
  run_id = NULL,
  progress = NULL
) {
  run <- TempestRun$new(
    objective = objective,
    workflow = workflow,
    runtime = runtime,
    experts = experts,
    connection_permissions = connection_permissions,
    deliverables = deliverables,
    artifact_catalog = artifact_catalog,
    source_store = source_store,
    runtime_context = runtime_context,
    policy_adapter = policy_adapter,
    run_id = run_id,
    progress = progress
  )
  execution_error <- tryCatch(
    {
      run$resume()
      NULL
    },
    error = function(error) error
  )
  if (!is.null(execution_error)) {
    tempest_run_signal_failure(execution_error, run)
  }
  run
}

tempest_run_evidence_snapshot <- function(workspace) {
  if (inherits(workspace, "ResearchWorkspace")) {
    return(tempest_research_workspace_snapshot(workspace))
  }
  tempest_run_abort(
    "The run evidence ledger must be a ResearchWorkspace.",
    class = "tempest_run_preflight_error"
  )
}

tempest_run_evidence_restore <- function(snapshot) {
  if (!is.list(snapshot) || is.data.frame(snapshot)) {
    tempest_run_abort(
      "Run evidence must be a workspace snapshot record.",
      class = "tempest_run_restore_error"
    )
  }
  schema_version <- tempest_persistence_schema_version(
    snapshot$schema_version %||% NA_integer_,
    "Run evidence schema version",
    c("tempest_run_restore_error", "tempest_run_error", "tempest_error")
  )
  if (identical(schema_version, 4L)) {
    return(tempest_research_workspace_restore(snapshot))
  }
  tempest_unsupported_format_abort(
    "generic run evidence format",
    schema_version,
    c("tempest_run_restore_error", "tempest_run_error", "tempest_error")
  )
}

#' Snapshot generic Tempest run state
#'
#' `r lifecycle::badge("experimental")`
#'
#' This experimental API is frozen and scheduled for removal in Tempest 0.2.0.
#' No compatibility shim is planned; see
#' [tempest-generic-kernel-retirement].
#'
#' Runtime functions, clients, capabilities, and credentials are deliberately
#' excluded. Restore requires an explicit runtime.
#'
#' @param run A `TempestRun`.
#' @return An in-memory serializable run record.
#' @export
tempest_run_snapshot <- function(run) {
  if (!inherits(run, "TempestRun")) {
    tempest_run_abort("{.arg run} must be a TempestRun.")
  }
  step_states <- lapply(run$step_states, function(state) {
    state$result <- tempest_run_result_snapshot(state$result)
    state
  })
  list(
    schema_version = 2L,
    run_id = run$run_id,
    status = run$status,
    objective = tempest_run_objective_record(run$objective),
    workflow = tempest_workflow_spec_record(run$workflow),
    experts = unname(lapply(run$experts, tempest_expert_profile_record)),
    connection_permissions = run$connection_permissions,
    deliverables = unname(lapply(
      run$deliverables,
      tempest_deliverable_spec_record
    )),
    step_states = step_states,
    assignments = run$assignments,
    events = run$events,
    sequence = length(run$events),
    approvals = run$approvals,
    policy_decisions = run$policy_decisions,
    capability_grants = tempest_contract_serializable_list(
      run$capability_grants,
      "capability_grants"
    ),
    cancel_token = run$cancel_token$snapshot(),
    artifact_catalog = run$artifact_catalog$snapshot(),
    source_store = if (is.null(run$source_store)) {
      NULL
    } else {
      tempest_run_evidence_snapshot(run$source_store)
    },
    created_at = run$created_at,
    updated_at = run$updated_at
  )
}

tempest_run_normalize_snapshot_value <- function(value) {
  if (!is.list(value) || is.data.frame(value)) {
    return(value)
  }
  value <- lapply(value, tempest_run_normalize_snapshot_value)
  if (!is.null(names(value))) {
    value <- value[order(names(value))]
  }
  value
}

tempest_run_snapshot_is_string <- function(value, optional = FALSE) {
  if (is.null(value)) {
    return(isTRUE(optional))
  }
  is.character(value) &&
    length(value) == 1L &&
    !is.na(value) &&
    nzchar(value)
}

tempest_run_snapshot_is_whole_number <- function(value, minimum = 0L) {
  is.numeric(value) &&
    length(value) == 1L &&
    !is.na(value) &&
    is.finite(value) &&
    value >= minimum &&
    value == as.integer(value)
}

tempest_run_result_snapshot_artifact_ids <- function(result) {
  if (is.null(result)) {
    return(character())
  }
  if (!is.list(result) || is.data.frame(result)) {
    tempest_run_abort(
      "Run snapshot contains a malformed step result.",
      class = "tempest_run_restore_error"
    )
  }
  type <- result$type %||% NULL
  if (
    !tempest_run_snapshot_is_string(type) ||
      !type %in% c("artifacts", "value", "omitted_runtime_value")
  ) {
    tempest_run_abort(
      "Run snapshot contains an invalid step result type.",
      class = "tempest_run_restore_error"
    )
  }
  if (identical(type, "artifacts")) {
    artifact_ids <- result$artifact_ids %||% character()
    if (
      !is.character(artifact_ids) ||
        length(artifact_ids) == 0L ||
        anyNA(artifact_ids) ||
        any(!nzchar(artifact_ids)) ||
        anyDuplicated(artifact_ids)
    ) {
      tempest_run_abort(
        "Run snapshot contains malformed result artifact ids.",
        class = "tempest_run_restore_error"
      )
    }
    return(artifact_ids)
  }
  if (identical(type, "value")) {
    if (is.null(names(result)) || !"value" %in% names(result)) {
      tempest_run_abort(
        "Run snapshot value result is missing its value.",
        class = "tempest_run_restore_error"
      )
    }
    tryCatch(
      tempest_canonical_json(result$value),
      error = function(error) {
        tempest_run_abort(
          "Run snapshot contains a non-serializable step result.",
          class = "tempest_run_restore_error",
          parent = error
        )
      }
    )
  }
  character()
}

tempest_run_assert_equivalent_snapshot <- function(saved, current, what) {
  saved <- tempest_run_normalize_snapshot_value(saved)
  current <- tempest_run_normalize_snapshot_value(current)
  if (!identical(saved, current)) {
    tempest_run_abort(
      paste0(
        "Restored ",
        what,
        " does not match the saved run snapshot."
      ),
      class = "tempest_run_restore_error"
    )
  }
  invisible(current)
}

tempest_run_validate_restored_outputs <- function(run) {
  for (step in run$workflow@steps) {
    state <- run$step_states[[step@step_id]]
    if (is.null(state)) {
      tempest_run_abort(
        "Run snapshot is missing step state {.val {step@step_id}}.",
        class = "tempest_run_restore_error"
      )
    }
    attempt_numbers <- vapply(
      state$attempts,
      \(attempt) as.integer(attempt$attempt),
      integer(1)
    )
    attempt_statuses <- vapply(
      state$attempts,
      \(attempt) attempt$status,
      character(1)
    )
    if (
      anyDuplicated(attempt_numbers) ||
        !identical(attempt_numbers, sort(attempt_numbers)) ||
        any(attempt_numbers > step@retry_policy$max_attempts) ||
        any(
          !attempt_statuses %in%
            c("failed", "succeeded", "awaiting_approval", "interrupted")
        )
    ) {
      tempest_run_abort(
        "Run snapshot contains invalid attempt history for step {.val {step@step_id}}.",
        class = "tempest_run_restore_error"
      )
    }
    result_artifact_ids <- tempest_run_result_snapshot_artifact_ids(
      state$result
    )
    pending_artifact_approval <- any(vapply(
      run$approvals,
      function(approval) {
        identical(approval$status, "pending") &&
          identical(approval$step_id, step@step_id) &&
          identical(approval$approval_kind %||% "step", "artifact")
      },
      logical(1)
    ))
    approved_artifact_approval <- any(vapply(
      run$approvals,
      function(approval) {
        identical(approval$status, "approved") &&
          identical(approval$step_id, step@step_id) &&
          identical(approval$approval_kind %||% "step", "artifact")
      },
      logical(1)
    ))
    if (identical(state$status, "succeeded")) {
      if (length(attempt_statuses) == 0L) {
        tempest_run_abort(
          "Succeeded step {.val {step@step_id}} has no completed attempt.",
          class = "tempest_run_restore_error"
        )
      }
      final_attempt_status <- attempt_statuses[[length(attempt_statuses)]]
      if (
        !identical(final_attempt_status, "succeeded") &&
          !(identical(final_attempt_status, "awaiting_approval") &&
            approved_artifact_approval)
      ) {
        tempest_run_abort(
          "Succeeded step {.val {step@step_id}} has inconsistent attempt state.",
          class = "tempest_run_restore_error"
        )
      }
    }
    if (
      identical(state$status, "succeeded") ||
        (identical(state$status, "awaiting_approval") &&
          pending_artifact_approval)
    ) {
      expected_output_ids <- unique(c(
        step@produced_artifact_ids,
        result_artifact_ids
      ))
      missing <- expected_output_ids[
        !vapply(
          expected_output_ids,
          run$artifact_catalog$has,
          logical(1)
        )
      ]
      if (length(missing) > 0L) {
        tempest_run_abort(
          "Restored step {.val {step@step_id}} is missing output artifact {.val {missing[[1]]}}.",
          class = "tempest_run_restore_error"
        )
      }
      if (identical(state$status, "succeeded")) {
        unresolved <- expected_output_ids[
          vapply(
            expected_output_ids,
            function(artifact_id) {
              run$artifact_catalog$get(artifact_id)@status %in%
                c("draft", "invalid", "awaiting_approval", "rejected")
            },
            logical(1)
          )
        ]
        if (length(unresolved) > 0L) {
          tempest_run_abort(
            "Succeeded step {.val {step@step_id}} contains unresolved output artifact {.val {unresolved[[1]]}}.",
            class = "tempest_run_restore_error"
          )
        }
      }
    }
  }
  for (approval in run$approvals) {
    if (!identical(approval$approval_kind %||% "step", "artifact")) {
      if (
        identical(approval$status, "rejected") &&
          !identical(
            run$step_states[[approval$step_id]]$status,
            "failed"
          )
      ) {
        tempest_run_abort(
          "Rejected step approval does not match restored step state.",
          class = "tempest_run_restore_error"
        )
      }
      next
    }
    artifacts <- lapply(
      approval$artifact_ids,
      run$artifact_catalog$get
    )
    artifact_statuses <- vapply(
      artifacts,
      \(artifact) artifact@status,
      character(1)
    )
    expected_statuses <- switch(
      approval$status,
      pending = c("awaiting_approval", "approved", "rejected"),
      approved = "approved",
      rejected = "rejected",
      cancelled = "awaiting_approval"
    )
    conflicting_pending_transition <-
      identical(approval$status, "pending") &&
      all(c("approved", "rejected") %in% artifact_statuses)
    if (
      any(!artifact_statuses %in% expected_statuses) ||
        conflicting_pending_transition
    ) {
      tempest_run_abort(
        "Restored artifact approval does not match artifact state.",
        class = "tempest_run_restore_error"
      )
    }
    expected_step_status <- switch(
      approval$status,
      pending = "awaiting_approval",
      approved = "succeeded",
      rejected = "failed",
      cancelled = "cancelled"
    )
    if (
      !identical(
        run$step_states[[approval$step_id]]$status,
        expected_step_status
      )
    ) {
      tempest_run_abort(
        "Restored artifact approval does not match step state.",
        class = "tempest_run_restore_error"
      )
    }
  }
  if (identical(run$status, "succeeded")) {
    output_ids <- unique(unlist(
      lapply(
        run$workflow@steps,
        \(step) step@produced_artifact_ids
      ),
      use.names = FALSE
    ))
    output_ids <- output_ids[vapply(
      output_ids,
      run$artifact_catalog$has,
      logical(1)
    )]
    statuses <- vapply(
      output_ids,
      \(artifact_id) run$artifact_catalog$get(artifact_id)@status,
      character(1)
    )
    unusable <- output_ids[
      statuses %in% c("draft", "invalid", "awaiting_approval", "rejected")
    ]
    if (length(unusable) > 0L) {
      tempest_run_abort(
        "Succeeded run contains unresolved output artifact {.val {unusable[[1]]}}.",
        class = "tempest_run_restore_error"
      )
    }
    for (deliverable_id in run$objective@deliverable_ids) {
      if (
        !tempest_run_has_usable_requested_deliverable(
          run,
          deliverable_id
        )
      ) {
        tempest_run_abort(
          "Succeeded run is missing requested deliverable {.val {deliverable_id}}.",
          class = "tempest_run_restore_error"
        )
      }
    }
  }
  invisible(run)
}

tempest_run_validate_snapshot <- function(
  snapshot,
  partial_recovery = FALSE
) {
  partial_recovery <- tempest_workflow_flag(
    partial_recovery,
    "partial_recovery"
  )
  if (
    !is.list(snapshot) ||
      is.data.frame(snapshot) ||
      !tempest_run_snapshot_is_whole_number(
        snapshot$schema_version,
        minimum = 1L
      ) ||
      !identical(as.integer(snapshot$schema_version), 2L)
  ) {
    tempest_run_abort(
      "Unsupported or malformed Tempest run snapshot.",
      class = "tempest_run_restore_error"
    )
  }
  if (
    !is.character(snapshot$status) ||
      length(snapshot$status) != 1L ||
      is.na(snapshot$status) ||
      !snapshot$status %in% tempest_run_statuses()
  ) {
    tempest_run_abort(
      "Run snapshot contains an invalid status.",
      class = "tempest_run_restore_error"
    )
  }
  workflow_record <- snapshot$workflow
  if (
    !tempest_run_snapshot_is_string(snapshot$run_id) ||
      !is.list(workflow_record) ||
      is.data.frame(workflow_record) ||
      !tempest_run_snapshot_is_string(workflow_record$workflow_id)
  ) {
    tempest_run_abort(
      "Run snapshot contains invalid run or workflow identity.",
      class = "tempest_run_restore_error"
    )
  }
  cancel_token <- snapshot$cancel_token
  if (
    !is.list(cancel_token) ||
      is.data.frame(cancel_token) ||
      !is.logical(cancel_token$requested) ||
      length(cancel_token$requested) != 1L ||
      is.na(cancel_token$requested) ||
      !tempest_run_snapshot_is_string(
        cancel_token$reason,
        optional = TRUE
      ) ||
      !tempest_run_snapshot_is_string(
        cancel_token$requested_at,
        optional = TRUE
      ) ||
      (isTRUE(cancel_token$requested) &&
        (is.null(cancel_token$reason) ||
          is.null(cancel_token$requested_at))) ||
      (!isTRUE(cancel_token$requested) &&
        (!is.null(cancel_token$reason) ||
          !is.null(cancel_token$requested_at))) ||
      (isTRUE(cancel_token$requested) &&
        !snapshot$status %in% c("cancel_requested", "cancelled")) ||
      (!isTRUE(cancel_token$requested) &&
        snapshot$status %in% c("cancel_requested", "cancelled"))
  ) {
    tempest_run_abort(
      "Run snapshot contains malformed cancellation state.",
      class = "tempest_run_restore_error"
    )
  }
  step_states <- snapshot$step_states %||% list()
  step_ids <- names(step_states)
  if (
    !is.list(step_states) ||
      is.data.frame(step_states) ||
      length(step_states) == 0L ||
      is.null(step_ids) ||
      anyNA(step_ids) ||
      any(!nzchar(step_ids)) ||
      anyDuplicated(step_ids) ||
      any(
        !vapply(
          step_states,
          \(state) is.list(state) && !is.data.frame(state),
          logical(1)
        )
      )
  ) {
    tempest_run_abort(
      "Run snapshot contains invalid step state.",
      class = "tempest_run_restore_error"
    )
  }
  step_statuses <- vapply(
    step_states,
    function(state) {
      status <- state$status %||% NA_character_
      if (
        !is.character(status) ||
          length(status) != 1L ||
          is.na(status)
      ) {
        return(NA_character_)
      }
      status
    },
    character(1)
  )
  valid_step_statuses <- c(
    "pending",
    "running",
    "awaiting_approval",
    "succeeded",
    "failed",
    "cancelled"
  )
  if (anyNA(step_statuses) || any(!step_statuses %in% valid_step_statuses)) {
    tempest_run_abort(
      "Run snapshot contains an invalid step status.",
      class = "tempest_run_restore_error"
    )
  }
  invalid_step_records <- names(step_states)[
    !vapply(
      seq_along(step_states),
      function(index) {
        state <- step_states[[index]]
        tempest_run_result_snapshot_artifact_ids(state$result)
        identical(state$step_id %||% "", step_ids[[index]]) &&
          is.list(state$attempts %||% list()) &&
          !is.data.frame(state$attempts %||% list()) &&
          all(vapply(
            state$attempts %||% list(),
            function(attempt) {
              is.list(attempt) &&
                !is.data.frame(attempt) &&
                is.numeric(attempt$attempt) &&
                length(attempt$attempt) == 1L &&
                !is.na(attempt$attempt) &&
                is.finite(attempt$attempt) &&
                attempt$attempt >= 1L &&
                attempt$attempt == as.integer(attempt$attempt) &&
                tempest_run_snapshot_is_string(attempt$status) &&
                attempt$status %in%
                  c(
                    "failed",
                    "succeeded",
                    "awaiting_approval",
                    "interrupted"
                  ) &&
                tempest_run_snapshot_is_string(attempt$started_at) &&
                tempest_run_snapshot_is_string(attempt$completed_at) &&
                {
                  tempest_run_result_snapshot_artifact_ids(
                    attempt$result
                  )
                  TRUE
                } &&
                if (attempt$status %in% c("failed", "interrupted")) {
                  is.list(attempt$error) &&
                    !is.data.frame(attempt$error) &&
                    tempest_run_snapshot_is_string(
                      attempt$error$class
                    ) &&
                    tempest_run_snapshot_is_string(
                      attempt$error$message
                    )
                } else {
                  is.null(attempt$error)
                }
            },
            logical(1)
          ))
      },
      logical(1)
    )
  ]
  if (length(invalid_step_records) > 0L) {
    tempest_run_abort(
      "Run snapshot contains a malformed step record.",
      class = "tempest_run_restore_error"
    )
  }
  in_flight <- identical(snapshot$status, "running") ||
    any(step_statuses == "running")
  if (in_flight && !partial_recovery) {
    tempest_run_abort(
      paste0(
        "In-flight run snapshots require ",
        "{.arg partial_recovery = TRUE}."
      ),
      class = "tempest_run_restore_error"
    )
  }
  approvals <- snapshot$approvals %||% list()
  approval_ids <- names(approvals)
  if (
    !is.list(approvals) ||
      is.data.frame(approvals) ||
      (length(approvals) > 0L &&
        (is.null(approval_ids) ||
          anyNA(approval_ids) ||
          any(!nzchar(approval_ids)) ||
          anyDuplicated(approval_ids))) ||
      any(
        !vapply(
          approvals,
          \(approval) is.list(approval) && !is.data.frame(approval),
          logical(1)
        )
      )
  ) {
    tempest_run_abort(
      "Run snapshot contains invalid approval state.",
      class = "tempest_run_restore_error"
    )
  }
  approval_statuses <- vapply(
    approvals,
    function(approval) {
      status <- approval$status %||% NA_character_
      if (
        !is.character(status) ||
          length(status) != 1L ||
          is.na(status)
      ) {
        return(NA_character_)
      }
      status
    },
    character(1)
  )
  if (
    anyNA(approval_statuses) ||
      any(
        !approval_statuses %in%
          c("pending", "approved", "rejected", "cancelled")
      )
  ) {
    tempest_run_abort(
      "Run snapshot contains an invalid approval status.",
      class = "tempest_run_restore_error"
    )
  }
  approval_kinds <- vapply(
    approvals,
    function(approval) {
      kind <- approval$approval_kind %||% "step"
      if (
        !is.character(kind) ||
          length(kind) != 1L ||
          is.na(kind)
      ) {
        return(NA_character_)
      }
      kind
    },
    character(1)
  )
  approval_step_ids <- vapply(
    approvals,
    function(approval) {
      step_id <- approval$step_id %||% NA_character_
      if (
        !is.character(step_id) ||
          length(step_id) != 1L ||
          is.na(step_id)
      ) {
        return(NA_character_)
      }
      step_id
    },
    character(1)
  )
  malformed_approvals <- length(approvals) > 0L &&
    any(vapply(
      seq_along(approvals),
      function(index) {
        approval <- approvals[[index]]
        artifact_ids <- approval$artifact_ids %||% character()
        metadata_valid <- tryCatch(
          {
            tempest_contract_serializable_list(
              approval$metadata %||% list(),
              "approval metadata"
            )
            TRUE
          },
          error = \(error) FALSE
        )
        !identical(approval$approval_id %||% "", approval_ids[[index]]) ||
          !approval_kinds[[index]] %in% c("step", "artifact") ||
          !approval_step_ids[[index]] %in% step_ids ||
          !is.character(artifact_ids) ||
          anyNA(artifact_ids) ||
          any(!nzchar(artifact_ids)) ||
          anyDuplicated(artifact_ids) ||
          (identical(approval_kinds[[index]], "step") &&
            length(artifact_ids) > 0L) ||
          (identical(approval_kinds[[index]], "artifact") &&
            length(artifact_ids) == 0L) ||
          !tempest_run_snapshot_is_string(approval$reason) ||
          !tempest_run_snapshot_is_string(approval$requested_at) ||
          !tempest_run_snapshot_is_string(
            approval$note,
            optional = TRUE
          ) ||
          !metadata_valid ||
          (identical(approval_statuses[[index]], "pending") &&
            !is.null(approval$decided_at)) ||
          (!identical(approval_statuses[[index]], "pending") &&
            !tempest_run_snapshot_is_string(approval$decided_at)) ||
          (identical(approval_kinds[[index]], "step") &&
            !tempest_run_snapshot_is_string(
              approval$policy_decision_id
            )) ||
          (identical(approval_kinds[[index]], "artifact") &&
            !is.null(approval$policy_decision_id))
      },
      logical(1)
    ))
  if (
    anyNA(approval_kinds) ||
      anyNA(approval_step_ids) ||
      malformed_approvals
  ) {
    tempest_run_abort(
      "Run snapshot contains a malformed approval record.",
      class = "tempest_run_restore_error"
    )
  }
  policy_decisions <- snapshot$policy_decisions %||% list()
  if (
    !is.list(policy_decisions) ||
      is.data.frame(policy_decisions) ||
      any(
        !vapply(
          policy_decisions,
          \(decision) is.list(decision) && !is.data.frame(decision),
          logical(1)
        )
      )
  ) {
    tempest_run_abort(
      "Run snapshot contains malformed policy decisions.",
      class = "tempest_run_restore_error"
    )
  }
  policy_decision_ids <- vapply(
    policy_decisions,
    function(decision) {
      if (tempest_run_snapshot_is_string(decision$decision_id)) {
        decision$decision_id
      } else {
        NA_character_
      }
    },
    character(1)
  )
  malformed_policy_decisions <- length(policy_decisions) > 0L &&
    any(vapply(
      policy_decisions,
      function(decision) {
        metadata_valid <- tryCatch(
          {
            tempest_contract_serializable_list(
              decision$metadata %||% list(),
              "policy metadata"
            )
            TRUE
          },
          error = \(error) FALSE
        )
        side_effecting_capability_ids <-
          decision$side_effecting_capability_ids %||% character()
        !tempest_run_snapshot_is_string(decision$decision_id) ||
          !tempest_run_snapshot_is_string(decision$step_id) ||
          !decision$step_id %in% step_ids ||
          !tempest_run_snapshot_is_string(decision$decision) ||
          !decision$decision %in% c("allow", "deny", "require_approval") ||
          !tempest_run_snapshot_is_string(decision$reason) ||
          !tempest_run_snapshot_is_string(decision$created_at) ||
          !is.character(side_effecting_capability_ids) ||
          anyNA(side_effecting_capability_ids) ||
          any(!nzchar(side_effecting_capability_ids)) ||
          anyDuplicated(side_effecting_capability_ids) ||
          !metadata_valid
      },
      logical(1)
    ))
  if (
    anyNA(policy_decision_ids) ||
      anyDuplicated(policy_decision_ids) ||
      malformed_policy_decisions
  ) {
    tempest_run_abort(
      "Run snapshot contains malformed policy decisions.",
      class = "tempest_run_restore_error"
    )
  }
  for (index in seq_along(approvals)) {
    if (!identical(approval_kinds[[index]], "step")) {
      next
    }
    policy_decision_id <- approvals[[index]]$policy_decision_id
    matching_policy <- which(policy_decision_ids == policy_decision_id)
    if (
      length(matching_policy) != 1L ||
        !identical(
          policy_decisions[[matching_policy]]$step_id,
          approval_step_ids[[index]]
        ) ||
        !identical(
          policy_decisions[[matching_policy]]$decision,
          "require_approval"
        )
    ) {
      tempest_run_abort(
        "Run snapshot step approval does not match its policy decision.",
        class = "tempest_run_restore_error"
      )
    }
  }
  pending_approval <- any(approval_statuses == "pending")
  if (
    pending_approval &&
      !snapshot$status %in% c("awaiting_approval", "cancel_requested")
  ) {
    tempest_run_abort(
      "Run snapshot has pending approval outside an approval state.",
      class = "tempest_run_restore_error"
    )
  }
  pending_ids <- which(approval_statuses == "pending")
  if (length(pending_ids) > 1L) {
    tempest_run_abort(
      "Run snapshot contains multiple pending approval requests.",
      class = "tempest_run_restore_error"
    )
  }
  awaiting_step_ids <- step_ids[step_statuses == "awaiting_approval"]
  if (
    length(pending_ids) == 1L &&
      !identical(
        awaiting_step_ids,
        approval_step_ids[[pending_ids]]
      )
  ) {
    tempest_run_abort(
      "Run snapshot approval state does not match its awaiting step.",
      class = "tempest_run_restore_error"
    )
  }
  if (length(pending_ids) == 0L && length(awaiting_step_ids) > 0L) {
    tempest_run_abort(
      "Run snapshot has an awaiting step without a pending approval.",
      class = "tempest_run_restore_error"
    )
  }
  if (
    identical(snapshot$status, "succeeded") &&
      any(step_statuses != "succeeded")
  ) {
    tempest_run_abort(
      "Succeeded run snapshot contains an incomplete step.",
      class = "tempest_run_restore_error"
    )
  }
  if (
    identical(snapshot$status, "awaiting_approval") &&
      !pending_approval
  ) {
    tempest_run_abort(
      "Run snapshot is awaiting approval without a pending request.",
      class = "tempest_run_restore_error"
    )
  }
  tryCatch(
    tempest_contract_serializable_list(
      snapshot$capability_grants %||% list(),
      "capability_grants"
    ),
    error = function(error) {
      tempest_run_abort(
        "Run snapshot contains invalid capability grant records.",
        class = "tempest_run_restore_error",
        parent = error
      )
    }
  )
  events <- snapshot$events %||% list()
  if (
    !is.list(events) ||
      is.data.frame(events) ||
      any(
        !vapply(
          events,
          \(event) is.list(event) && !is.data.frame(event),
          logical(1)
        )
      )
  ) {
    tempest_run_abort(
      "Run snapshot contains malformed event state.",
      class = "tempest_run_restore_error"
    )
  }
  malformed_events <- length(events) > 0L &&
    any(vapply(
      events,
      function(event) {
        payload_valid <- tryCatch(
          {
            tempest_contract_serializable_list(
              event$payload %||% list(),
              "event payload"
            )
            TRUE
          },
          error = \(error) FALSE
        )
        !tempest_run_snapshot_is_string(event$event_id) ||
          !tempest_run_snapshot_is_whole_number(
            event$sequence,
            minimum = 1L
          ) ||
          !identical(event$run_id, snapshot$run_id) ||
          !identical(
            event$workflow_id,
            workflow_record$workflow_id
          ) ||
          !tempest_run_snapshot_is_string(event$event_type) ||
          !tempest_run_snapshot_is_string(event$status) ||
          !tempest_run_snapshot_is_string(event$timestamp) ||
          !tempest_run_snapshot_is_string(
            event$step_id,
            optional = TRUE
          ) ||
          (!is.null(event$step_id) &&
            !event$step_id %in% step_ids) ||
          (!is.null(event$attempt) &&
            (!tempest_run_snapshot_is_whole_number(
              event$attempt,
              minimum = 1L
            ) ||
              is.null(event$step_id))) ||
          !tempest_run_snapshot_is_string(
            event$expert_id,
            optional = TRUE
          ) ||
          !tempest_run_snapshot_is_string(
            event$artifact_id,
            optional = TRUE
          ) ||
          !tempest_run_snapshot_is_string(
            event$approval_id,
            optional = TRUE
          ) ||
          !tempest_run_snapshot_is_string(
            event$message,
            optional = TRUE
          ) ||
          !payload_valid
      },
      logical(1)
    ))
  if (malformed_events) {
    tempest_run_abort(
      "Run snapshot contains malformed event records.",
      class = "tempest_run_restore_error"
    )
  }
  event_ids <- vapply(
    events,
    \(event) event$event_id,
    character(1)
  )
  if (anyDuplicated(event_ids)) {
    tempest_run_abort(
      "Run snapshot contains duplicate event ids.",
      class = "tempest_run_restore_error"
    )
  }
  sequences <- vapply(
    events,
    \(event) as.integer(event$sequence),
    integer(1)
  )
  if (
    !identical(sequences, seq_along(sequences)) ||
      !tempest_run_snapshot_is_whole_number(snapshot$sequence) ||
      !identical(as.integer(snapshot$sequence), length(sequences))
  ) {
    tempest_run_abort(
      "Run snapshot event sequence is not monotonic.",
      class = "tempest_run_restore_error"
    )
  }
  if (
    length(events) == 0L ||
      !identical(events[[1]]$event_type, "workflow.created") ||
      !identical(events[[1]]$status, "pending")
  ) {
    tempest_run_abort(
      "Run snapshot is missing its workflow creation event.",
      class = "tempest_run_restore_error"
    )
  }
  if (
    identical(snapshot$status, "succeeded") &&
      (!identical(events[[length(events)]]$event_type, "workflow.completed") ||
        !identical(events[[length(events)]]$status, "succeeded"))
  ) {
    tempest_run_abort(
      "Succeeded run snapshot is missing its completion event.",
      class = "tempest_run_restore_error"
    )
  }
  for (index in seq_along(approvals)) {
    approval_id <- approval_ids[[index]]
    requested <- vapply(
      events,
      function(event) {
        identical(event$event_type %||% "", "approval.requested") &&
          identical(event$approval_id %||% "", approval_id)
      },
      logical(1)
    )
    resolved <- vapply(
      events,
      function(event) {
        identical(event$event_type %||% "", "approval.resolved") &&
          identical(event$approval_id %||% "", approval_id)
      },
      logical(1)
    )
    if (sum(requested) != 1L) {
      tempest_run_abort(
        "Run snapshot approval is missing its request event.",
        class = "tempest_run_restore_error"
      )
    }
    request_index <- which(requested)
    request_event <- events[[request_index]]
    request_payload <- request_event$payload %||% list()
    request_kind <- request_payload$approval_kind %||%
      if (identical(approval_kinds[[index]], "step")) {
        "step"
      } else {
        NA_character_
      }
    request_artifact_ids <- request_payload$artifact_ids %||% character()
    if (
      !identical(request_event$step_id, approval_step_ids[[index]]) ||
        !identical(request_event$status, "awaiting_approval") ||
        !tempest_run_snapshot_is_string(request_kind) ||
        !is.character(request_artifact_ids) ||
        anyNA(request_artifact_ids) ||
        any(!nzchar(request_artifact_ids)) ||
        anyDuplicated(request_artifact_ids) ||
        !identical(request_kind, approval_kinds[[index]]) ||
        !identical(
          sort(request_artifact_ids),
          sort(approvals[[index]]$artifact_ids %||% character())
        )
    ) {
      tempest_run_abort(
        "Run snapshot approval request does not match its approval record.",
        class = "tempest_run_restore_error"
      )
    }
    if (approval_statuses[[index]] %in% c("approved", "rejected")) {
      matching_resolution <- resolved &
        vapply(
          events,
          \(event) {
            identical(
              event$status %||% "",
              approval_statuses[[index]]
            )
          },
          logical(1)
        )
      if (sum(resolved) != 1L || sum(matching_resolution) != 1L) {
        tempest_run_abort(
          "Run snapshot approval resolution is inconsistent with its events.",
          class = "tempest_run_restore_error"
        )
      }
      resolution_index <- which(matching_resolution)
      resolution_event <- events[[resolution_index]]
      resolution_payload <- resolution_event$payload %||% list()
      resolution_kind <-
        resolution_payload$approval_kind %||% NA_character_
      resolution_artifact_ids <-
        resolution_payload$artifact_ids %||% character()
      if (
        resolution_index <= request_index ||
          !identical(
            resolution_event$step_id,
            approval_step_ids[[index]]
          ) ||
          !tempest_run_snapshot_is_string(resolution_kind) ||
          !is.character(resolution_artifact_ids) ||
          anyNA(resolution_artifact_ids) ||
          any(!nzchar(resolution_artifact_ids)) ||
          anyDuplicated(resolution_artifact_ids) ||
          !identical(
            resolution_kind,
            approval_kinds[[index]]
          ) ||
          !identical(
            sort(resolution_artifact_ids),
            sort(approvals[[index]]$artifact_ids %||% character())
          )
      ) {
        tempest_run_abort(
          "Run snapshot approval resolution does not match its approval record.",
          class = "tempest_run_restore_error"
        )
      }
    } else if (
      approval_statuses[[index]] %in% c("pending", "cancelled") && any(resolved)
    ) {
      tempest_run_abort(
        "Unresolved run approval already has a resolution event.",
        class = "tempest_run_restore_error"
      )
    }
  }
  invisible(snapshot)
}

# Frozen generic-kernel deletion seam. Internal until section-10 removal.
tempest_run_restore <- function(
  snapshot,
  runtime,
  artifact_catalog = NULL,
  source_store = NULL,
  runtime_context = list(),
  connection_permissions = NULL,
  partial_recovery = FALSE,
  policy_adapter = NULL,
  progress = NULL
) {
  partial_recovery <- tempest_workflow_flag(
    partial_recovery,
    "partial_recovery"
  )
  tempest_run_validate_snapshot(
    snapshot,
    partial_recovery = partial_recovery
  )
  objective <- tempest_run_objective_from_data(snapshot$objective)
  workflow <- tempest_workflow_spec_from_data(snapshot$workflow)
  experts <- lapply(
    snapshot$experts %||% list(),
    tempest_expert_profile_from_data
  )
  deliverables <- lapply(
    snapshot$deliverables %||% list(),
    tempest_deliverable_spec_from_data
  )
  connection_permissions <- tempest_run_restored_connection_permissions(
    snapshot,
    runtime,
    connection_permissions
  )
  if (is.null(snapshot$source_store)) {
    if (!is.null(source_store)) {
      tempest_run_abort(
        "A source-store override cannot be attached to a run saved without one.",
        class = "tempest_run_restore_error"
      )
    }
  } else if (is.null(source_store)) {
    source_store <- tempest_run_evidence_restore(snapshot$source_store)
  } else {
    if (!inherits(source_store, "ResearchWorkspace")) {
      tempest_run_abort(
        "{.arg source_store} must be a ResearchWorkspace.",
        class = "tempest_run_restore_error"
      )
    }
    saved_source_store <- tempest_run_evidence_restore(snapshot$source_store)
    tempest_run_assert_equivalent_snapshot(
      tempest_run_evidence_snapshot(saved_source_store),
      tempest_run_evidence_snapshot(source_store),
      "source store"
    )
  }
  if (is.null(artifact_catalog)) {
    artifact_catalog <- tempest_artifact_catalog_restore(
      snapshot$artifact_catalog,
      evidence_store = source_store
    )
  } else {
    if (!inherits(artifact_catalog, "TempestArtifactCatalog")) {
      tempest_run_abort(
        "{.arg artifact_catalog} must be a TempestArtifactCatalog.",
        class = "tempest_run_restore_error"
      )
    }
    saved_artifact_catalog <- tempest_artifact_catalog_restore(
      snapshot$artifact_catalog,
      evidence_store = source_store
    )
    tempest_run_assert_equivalent_snapshot(
      saved_artifact_catalog$snapshot(include_content = TRUE),
      artifact_catalog$snapshot(include_content = TRUE),
      "artifact catalog"
    )
  }
  run <- TempestRun$new(
    objective = objective,
    workflow = workflow,
    runtime = runtime,
    experts = experts,
    connection_permissions = connection_permissions,
    deliverables = deliverables,
    artifact_catalog = artifact_catalog,
    source_store = source_store,
    runtime_context = runtime_context,
    policy_adapter = policy_adapter,
    run_id = snapshot$run_id,
    progress = progress,
    restored = TRUE,
    restore_snapshot = snapshot,
    partial_recovery = partial_recovery
  )
  tempest_run_validate_restored_outputs(run)
  run
}

tempest_generic_run_bundle_error_class <- function(
  specific = character()
) {
  unique(c(
    specific,
    "tempest_run_bundle_error",
    "tempest_run_error",
    "tempest_persistence_error",
    "tempest_error"
  ))
}

tempest_generic_run_bundle_abort <- function(
  message,
  ...,
  class = character(),
  parent = NULL
) {
  tempest_run_abort(
    message,
    ...,
    class = tempest_generic_run_bundle_error_class(class),
    parent = parent
  )
}

tempest_generic_run_bundle_json_value <- function(value) {
  if (is.list(value)) {
    return(lapply(value, tempest_generic_run_bundle_json_value))
  }
  if (
    is.atomic(value) &&
      length(value) == 1L &&
      (is.na(value) ||
        (is.numeric(value) && !is.finite(value)))
  ) {
    return(NULL)
  }
  value
}

tempest_generic_run_bundle_write_json <- function(path, value, what) {
  tryCatch(
    {
      json <- tempest_canonical_json(
        tempest_generic_run_bundle_json_value(value)
      )
      tempest_atomic_write_lines(json, path)
    },
    error = function(...) {
      tempest_generic_run_bundle_abort(
        paste0("Could not write ", what, "."),
        class = "tempest_run_save_error"
      )
    }
  )
  invisible(path)
}

tempest_generic_run_bundle_is_symlink <- function(paths) {
  links <- Sys.readlink(paths)
  !is.na(links) & nzchar(links)
}

tempest_generic_run_bundle_assert_manifest <- function(
  manifest_path,
  class
) {
  info <- file.info(manifest_path)
  if (
    !file.exists(manifest_path) ||
      anyNA(info$isdir) ||
      info$isdir ||
      anyNA(info$size) ||
      info$size < 0 ||
      info$size > 1024^2 ||
      tempest_generic_run_bundle_is_symlink(manifest_path)
  ) {
    tempest_generic_run_bundle_abort(
      "Tempest run bundle manifest is missing, unsafe, or too large.",
      class = class
    )
  }
  invisible(manifest_path)
}

tempest_generic_run_bundle_prepare_dir <- function(
  path,
  overwrite = FALSE
) {
  if (!rlang::is_string(path) || !nzchar(tempest_trim(path))) {
    tempest_generic_run_bundle_abort(
      "{.arg path} must be a single non-empty path string.",
      class = "tempest_run_save_error"
    )
  }
  overwrite <- tempest_workflow_flag(overwrite, "overwrite")
  bundle_dir <- normalizePath(
    path.expand(path),
    winslash = "/",
    mustWork = FALSE
  )
  if (tempest_generic_run_bundle_is_symlink(bundle_dir)) {
    tempest_generic_run_bundle_abort(
      "{.arg path} cannot be a symbolic link.",
      class = "tempest_run_save_error"
    )
  }
  if (file.exists(bundle_dir)) {
    if (!dir.exists(bundle_dir)) {
      tempest_generic_run_bundle_abort(
        "{.arg path} must point to a directory.",
        class = "tempest_run_save_error"
      )
    }
    entries <- list.files(
      bundle_dir,
      all.files = TRUE,
      no.. = TRUE
    )
    if (length(entries) > 0L && !overwrite) {
      tempest_generic_run_bundle_abort(
        c(
          "Tempest run bundle directory already exists.",
          i = "Use {.code overwrite = TRUE} to replace it.",
          x = paste0("Path: ", bundle_dir, ".")
        ),
        class = "tempest_run_save_error"
      )
    }
    if (length(entries) > 0L && overwrite) {
      manifest_path <- file.path(bundle_dir, "manifest.json")
      tempest_generic_run_bundle_assert_manifest(
        manifest_path,
        "tempest_run_save_error"
      )
      manifest <- tryCatch(
        tempest_read_json_strict(
          manifest_path,
          what = "Tempest run bundle manifest",
          class = tempest_generic_run_bundle_error_class(
            "tempest_run_save_error"
          )
        ),
        error = function(...) {
          tempest_generic_run_bundle_abort(
            paste0(
              "Refusing to overwrite a directory that is not a recognized ",
              "Tempest run bundle."
            ),
            class = "tempest_run_save_error"
          )
        }
      )
      if (
        !is.list(manifest) ||
          !identical(manifest$bundle_type %||% "", "tempest_run")
      ) {
        tempest_generic_run_bundle_abort(
          paste0(
            "Refusing to overwrite a directory that is not a recognized ",
            "Tempest run bundle."
          ),
          class = "tempest_run_save_error"
        )
      }
      tryCatch(
        tempest_generic_run_bundle_validate(bundle_dir, manifest),
        error = function(error) {
          tempest_generic_run_bundle_abort(
            paste0(
              "Refusing to overwrite an invalid Tempest run bundle."
            ),
            class = "tempest_run_save_error",
            parent = error
          )
        }
      )
    }
  }
  dir.create(
    dirname(bundle_dir),
    recursive = TRUE,
    showWarnings = FALSE
  )
  bundle_dir
}

tempest_generic_run_bundle_commit <- function(staging_dir, bundle_dir) {
  backup_dir <- NULL
  if (file.exists(bundle_dir)) {
    backup_dir <- tempfile(
      pattern = paste0(".", basename(bundle_dir), "-backup-"),
      tmpdir = dirname(bundle_dir)
    )
    if (!file.rename(bundle_dir, backup_dir)) {
      tempest_generic_run_bundle_abort(
        "Could not stage the previous Tempest run bundle for replacement.",
        class = "tempest_run_save_error"
      )
    }
  }
  if (!file.rename(staging_dir, bundle_dir)) {
    if (!is.null(backup_dir)) {
      file.rename(backup_dir, bundle_dir)
    }
    tempest_generic_run_bundle_abort(
      "Could not atomically install the completed Tempest run bundle.",
      class = "tempest_run_save_error"
    )
  }
  if (!is.null(backup_dir)) {
    unlink(backup_dir, recursive = TRUE, force = TRUE)
  }
  normalizePath(bundle_dir, winslash = "/", mustWork = TRUE)
}

tempest_generic_run_bundle_validate <- function(bundle_dir, manifest) {
  if (
    !is.list(manifest) ||
      is.data.frame(manifest) ||
      is.null(names(manifest)) ||
      anyNA(names(manifest)) ||
      any(!nzchar(names(manifest))) ||
      anyDuplicated(names(manifest)) ||
      !identical(
        as.integer(manifest$schema_version %||% NA_integer_),
        1L
      ) ||
      !identical(manifest$bundle_type %||% "", "tempest_run")
  ) {
    tempest_generic_run_bundle_abort(
      "Tempest run bundle manifest is malformed or unsupported.",
      class = "tempest_run_resume_error"
    )
  }
  if (!identical(manifest$status %||% "", "complete")) {
    tempest_generic_run_bundle_abort(
      "Tempest run bundle manifest is not complete.",
      class = "tempest_run_resume_error"
    )
  }
  if (
    !is.list(manifest$files) ||
      is.data.frame(manifest$files) ||
      (!is.null(names(manifest$files)) &&
        any(nzchar(names(manifest$files))))
  ) {
    tempest_generic_run_bundle_abort(
      "Tempest run bundle file inventory is malformed.",
      class = "tempest_run_resume_error"
    )
  }
  files <- as.character(unlist(manifest$files, use.names = FALSE))
  problems <- c(
    if (length(files) == 0L) {
      "Manifest declares no files."
    },
    if (anyDuplicated(files)) {
      "Manifest declares duplicate file paths."
    },
    if (
      length(files) > 0L &&
        any(
          !vapply(
            files,
            tempest_artifact_bundle_path_is_safe,
            logical(1)
          )
        )
    ) {
      "Manifest contains unsafe file paths."
    },
    if (!identical(files, "snapshot.json")) {
      "Manifest must declare exactly snapshot.json."
    }
  )
  checksums <- manifest$checksums
  if (
    !is.list(checksums) ||
      is.data.frame(checksums) ||
      length(checksums) == 0L ||
      is.null(names(checksums)) ||
      anyNA(names(checksums)) ||
      any(!nzchar(names(checksums))) ||
      anyDuplicated(names(checksums))
  ) {
    problems <- c(
      problems,
      "Manifest checksum inventory is malformed."
    )
    checksums <- list()
  }
  checksum_values <- unlist(checksums, use.names = TRUE)
  if (
    length(checksum_values) > 0L &&
      (!is.character(checksum_values) ||
        anyNA(checksum_values) ||
        any(!grepl("^[a-f0-9]{64}$", checksum_values)))
  ) {
    problems <- c(problems, "Manifest contains invalid checksums.")
  }
  if (
    !setequal(names(checksum_values), files) ||
      length(checksum_values) != length(files)
  ) {
    problems <- c(
      problems,
      "Manifest checksum inventory does not match its file inventory."
    )
  }

  inventory <- sort(list.files(
    bundle_dir,
    all.files = TRUE,
    no.. = TRUE,
    recursive = FALSE
  ))
  expected_inventory <- sort(c("manifest.json", files))
  if (
    !identical(inventory, expected_inventory) ||
      anyDuplicated(inventory)
  ) {
    problems <- c(
      problems,
      "Bundle contents do not match the manifest inventory."
    )
  }
  inventory_paths <- file.path(bundle_dir, inventory)
  if (length(inventory_paths) > 0L) {
    info <- file.info(inventory_paths)
    if (
      anyNA(info$isdir) ||
        any(info$isdir) ||
        any(tempest_generic_run_bundle_is_symlink(inventory_paths))
    ) {
      problems <- c(
        problems,
        "Bundle inventory must contain regular files only."
      )
    }
    if (
      anyNA(info$size) ||
        any(info$size < 0) ||
        any(info$size > 50 * 1024^2) ||
        sum(info$size) > 51 * 1024^2
    ) {
      problems <- c(
        problems,
        "Tempest run bundle exceeds its size limits."
      )
    }
  }
  if (length(problems) > 0L) {
    tempest_generic_run_bundle_abort(
      c("Cannot resume Tempest run bundle.", x = unique(problems)),
      class = "tempest_run_resume_error"
    )
  }

  root <- paste0(
    normalizePath(bundle_dir, winslash = "/", mustWork = TRUE),
    "/"
  )
  resolved <- normalizePath(
    file.path(bundle_dir, files),
    winslash = "/",
    mustWork = TRUE
  )
  if (any(!startsWith(resolved, root))) {
    tempest_generic_run_bundle_abort(
      "Tempest run bundle file resolves outside the bundle.",
      class = "tempest_run_resume_error"
    )
  }
  mismatched <- files[vapply(
    files,
    function(file) {
      expected <- checksum_values[[file]]
      actual <- tempest_session_bundle_checksum(bundle_dir, file)
      !identical(actual, expected)
    },
    logical(1)
  )]
  if (length(mismatched) > 0L) {
    tempest_generic_run_bundle_abort(
      "Tempest run bundle checksum validation failed.",
      class = "tempest_run_resume_error"
    )
  }
  files
}

tempest_generic_run_snapshot_from_json <- function(snapshot) {
  if (!is.list(snapshot) || is.data.frame(snapshot)) {
    return(snapshot)
  }
  snapshot$schema_version <- as.integer(snapshot$schema_version)
  snapshot$sequence <- as.integer(snapshot$sequence)
  assignments <- snapshot$assignments %||% list()
  if (is.list(assignments) && !is.data.frame(assignments)) {
    snapshot$assignments <- lapply(
      assignments,
      tempest_codec_character
    )
  }
  snapshot$events <- lapply(snapshot$events %||% list(), function(event) {
    if (!is.list(event) || is.data.frame(event)) {
      return(event)
    }
    event$sequence <- as.integer(event$sequence)
    if (!is.null(event$attempt)) {
      event$attempt <- as.integer(event$attempt)
    }
    if (
      is.list(event$payload) &&
        !is.data.frame(event$payload) &&
        "artifact_ids" %in% names(event$payload)
    ) {
      event$payload$artifact_ids <- tempest_codec_character(
        event$payload$artifact_ids
      )
    }
    event
  })
  snapshot$approvals <- lapply(
    snapshot$approvals %||% list(),
    function(approval) {
      approval$artifact_ids <- tempest_codec_character(
        approval$artifact_ids
      )
      approval
    }
  )
  snapshot$policy_decisions <- lapply(
    snapshot$policy_decisions %||% list(),
    function(decision) {
      decision$side_effecting_capability_ids <-
        tempest_codec_character(
          decision$side_effecting_capability_ids
        )
      decision
    }
  )
  decode_grants <- function(grants) {
    lapply(grants %||% list(), function(grant) {
      if (!is.list(grant) || is.data.frame(grant)) {
        return(grant)
      }
      grant$connection_ref_ids <- tempest_codec_character(
        grant$connection_ref_ids
      )
      tempest_run_normalize_snapshot_value(grant[c(
        "capability_id",
        "capability_version",
        "operation_id",
        "operation_version",
        "required",
        "status",
        "connection_ref_ids",
        "reason_code",
        "reason",
        "metadata"
      )])
    })
  }
  decode_grant_attempt <- function(record, include_history = FALSE) {
    if (!is.list(record) || is.data.frame(record)) {
      return(record)
    }
    if (!is.null(record$attempt)) {
      record$attempt <- as.integer(record$attempt)
    }
    record$experts <- lapply(
      record$experts %||% list(),
      decode_grants
    )
    record$step <- decode_grants(record$step)
    if (include_history) {
      record$attempts <- lapply(
        record$attempts %||% list(),
        decode_grant_attempt,
        include_history = FALSE
      )
    }
    fields <- c("attempt", "experts", "step", "recorded_at")
    if (include_history) {
      fields <- c(fields, "attempts")
    }
    tempest_run_normalize_snapshot_value(record[fields])
  }
  snapshot$capability_grants <- lapply(
    snapshot$capability_grants %||% list(),
    decode_grant_attempt,
    include_history = TRUE
  )
  snapshot$step_states <- lapply(
    snapshot$step_states %||% list(),
    function(state) {
      if (!is.list(state) || is.data.frame(state)) {
        return(state)
      }
      decode_result <- function(result) {
        if (is.null(result) || !is.list(result) || is.data.frame(result)) {
          return(result)
        }
        if (identical(result$type %||% "", "artifacts")) {
          result$artifact_ids <- tempest_codec_character(
            result$artifact_ids
          )
        }
        tempest_run_normalize_snapshot_value(result)
      }
      state$result <- decode_result(state$result)
      state$attempts <- lapply(state$attempts %||% list(), function(attempt) {
        if (!is.list(attempt) || is.data.frame(attempt)) {
          return(attempt)
        }
        if (!is.null(attempt$attempt)) {
          attempt$attempt <- as.integer(attempt$attempt)
        }
        attempt$result <- decode_result(attempt$result)
        attempt
      })
      state
    }
  )
  snapshot
}

#' Save a generic Tempest run bundle
#'
#' `r lifecycle::badge("experimental")`
#'
#' This experimental API is frozen and scheduled for removal in Tempest 0.2.0.
#' No compatibility shim is planned; see
#' [tempest-generic-kernel-retirement].
#'
#' The bundle contains one canonical run snapshot plus a checksummed manifest
#' written last. Runtime operations, clients, connection bindings, policy
#' adapters, callbacks, credentials, and other executable values are excluded.
#'
#' @param run A `TempestRun`.
#' @param path Destination directory.
#' @param overwrite Whether to atomically replace an existing recognized
#'   Tempest run bundle.
#' @return The normalized bundle directory, invisibly.
#' @export
tempest_run_save <- function(run, path, overwrite = FALSE) {
  if (!inherits(run, "TempestRun")) {
    tempest_generic_run_bundle_abort(
      "{.arg run} must be a TempestRun.",
      class = "tempest_run_save_error"
    )
  }
  bundle_dir <- tempest_generic_run_bundle_prepare_dir(path, overwrite)
  snapshot <- tempest_run_snapshot(run)
  sensitive <- setdiff(
    tempest_contract_sensitive_names(snapshot, "snapshot"),
    "snapshot$cancel_token"
  )
  if (length(sensitive) > 0L) {
    tempest_generic_run_bundle_abort(
      c(
        "Tempest run snapshots cannot contain credential or secret fields.",
        x = paste0("Sensitive field: ", sensitive[[1]], ".")
      ),
      class = "tempest_run_save_error"
    )
  }
  staging_dir <- tempfile(
    pattern = paste0(".", basename(bundle_dir), "-staging-"),
    tmpdir = dirname(bundle_dir)
  )
  dir.create(
    staging_dir,
    recursive = TRUE,
    showWarnings = FALSE,
    mode = "0700"
  )
  installed <- FALSE
  on.exit(
    if (!installed && dir.exists(staging_dir)) {
      unlink(staging_dir, recursive = TRUE, force = TRUE)
    },
    add = TRUE
  )

  snapshot_path <- file.path(staging_dir, "snapshot.json")
  tempest_generic_run_bundle_write_json(
    snapshot_path,
    snapshot,
    "Tempest run snapshot"
  )
  Sys.chmod(snapshot_path, mode = "0600")
  checksum <- tempest_session_bundle_checksum(
    staging_dir,
    "snapshot.json"
  )
  manifest <- list(
    schema_version = 1L,
    bundle_type = "tempest_run",
    status = "complete",
    run_id = run$run_id,
    snapshot_schema_version = snapshot$schema_version,
    package_version = tryCatch(
      as.character(utils::packageVersion("tempest")),
      error = \(error) "unknown"
    ),
    saved_at = tempest_now_utc(),
    files = list("snapshot.json"),
    checksums = list(snapshot.json = checksum)
  )
  manifest_path <- file.path(staging_dir, "manifest.json")
  tempest_generic_run_bundle_write_json(
    manifest_path,
    manifest,
    "Tempest run bundle manifest"
  )
  Sys.chmod(manifest_path, mode = "0600")
  result <- tempest_generic_run_bundle_commit(staging_dir, bundle_dir)
  installed <- TRUE
  invisible(result)
}

# Frozen generic-kernel deletion seam. Internal until section-10 removal.
tempest_run_resume <- function(
  path,
  runtime,
  artifact_catalog = NULL,
  source_store = NULL,
  runtime_context = list(),
  connection_permissions = NULL,
  partial_recovery = FALSE,
  policy_adapter = NULL,
  progress = NULL
) {
  if (!rlang::is_string(path) || !nzchar(tempest_trim(path))) {
    tempest_generic_run_bundle_abort(
      "{.arg path} must be a single non-empty path string.",
      class = "tempest_run_resume_error"
    )
  }
  bundle_dir <- normalizePath(
    path.expand(path),
    winslash = "/",
    mustWork = FALSE
  )
  if (
    !dir.exists(bundle_dir) ||
      tempest_generic_run_bundle_is_symlink(bundle_dir)
  ) {
    tempest_generic_run_bundle_abort(
      "Tempest run bundle directory does not exist or is unsafe.",
      class = "tempest_run_resume_error"
    )
  }
  manifest_path <- file.path(bundle_dir, "manifest.json")
  tempest_generic_run_bundle_assert_manifest(
    manifest_path,
    "tempest_run_resume_error"
  )
  manifest <- tempest_read_json_strict(
    manifest_path,
    what = "Tempest run bundle manifest",
    class = tempest_generic_run_bundle_error_class(
      "tempest_run_resume_error"
    )
  )
  files <- tempest_generic_run_bundle_validate(bundle_dir, manifest)
  snapshot <- tempest_read_json_strict(
    file.path(bundle_dir, files[[1]]),
    what = "Tempest run snapshot",
    class = tempest_generic_run_bundle_error_class(
      "tempest_run_resume_error"
    )
  )
  snapshot <- tempest_generic_run_snapshot_from_json(snapshot)
  if (
    !identical(
      as.integer(manifest$snapshot_schema_version %||% NA_integer_),
      as.integer(snapshot$schema_version %||% NA_integer_)
    ) ||
      !identical(manifest$run_id %||% "", snapshot$run_id %||% "")
  ) {
    tempest_generic_run_bundle_abort(
      "Tempest run manifest does not match its snapshot.",
      class = "tempest_run_resume_error"
    )
  }
  tempest_run_restore(
    snapshot,
    runtime = runtime,
    artifact_catalog = artifact_catalog,
    source_store = source_store,
    runtime_context = runtime_context,
    connection_permissions = connection_permissions,
    partial_recovery = partial_recovery,
    policy_adapter = policy_adapter,
    progress = progress
  )
}
