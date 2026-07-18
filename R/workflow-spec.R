# Serializable workflow and step specifications

tempest_workflow_definition_abort <- function(message, ..., parent = NULL) {
  tempest_abort(
    message,
    ...,
    class = c("tempest_workflow_definition_error", "tempest_error"),
    parent = parent,
    .envir = rlang::caller_env()
  )
}

tempest_workflow_assignment_rule <- function(value) {
  if (is.null(value)) {
    value <- list(type = "none", expert_ids = character())
  } else if (is.character(value)) {
    value <- list(type = "exact", expert_ids = value)
  }
  value <- tempest_contract_serializable_list(value, "assignment_rule")
  type <- tempest_workflow_scalar(
    value$type %||% "none",
    "assignment_rule$type"
  )
  if (!type %in% c("none", "exact", "all")) {
    tempest_workflow_definition_abort(
      "{.arg assignment_rule$type} must be one of {.val {c('none', 'exact', 'all')}}."
    )
  }
  expert_ids <- tempest_contract_ids(
    tempest_codec_character(value$expert_ids),
    "assignment_rule$expert_ids"
  )
  if (identical(type, "exact") && length(expert_ids) == 0L) {
    tempest_workflow_definition_abort(
      "An exact assignment rule must name at least one expert."
    )
  }
  if (!identical(type, "exact") && length(expert_ids) > 0L) {
    tempest_workflow_definition_abort(
      "Only an exact assignment rule may include expert ids."
    )
  }
  list(type = type, expert_ids = sort(unique(expert_ids)))
}

tempest_workflow_retry_policy <- function(value) {
  value <- tempest_contract_serializable_list(
    value %||% list(),
    "retry_policy"
  )
  max_attempts <- value$max_attempts %||% 1L
  if (
    !is.numeric(max_attempts) ||
      length(max_attempts) != 1L ||
      is.na(max_attempts) ||
      max_attempts < 1L ||
      max_attempts != as.integer(max_attempts)
  ) {
    tempest_workflow_definition_abort(
      "{.arg retry_policy$max_attempts} must be a positive whole number."
    )
  }
  list(max_attempts = as.integer(max_attempts))
}

TempestWorkflowStep <- S7::new_class(
  "tempest_workflow_step",
  properties = list(
    step_id = tempest_contract_prop_chr(),
    version = tempest_contract_prop_chr("1"),
    title = tempest_contract_prop_chr(),
    purpose = tempest_contract_prop_chr(),
    dependency_ids = tempest_contract_prop_chr_vec(),
    operation_id = tempest_contract_prop_chr(),
    operation_version = tempest_contract_prop_chr("1"),
    required_input_artifact_ids = tempest_contract_prop_chr_vec(),
    produced_artifact_ids = tempest_contract_prop_chr_vec(),
    assignment_rule = tempest_contract_prop_list(),
    required_capability_ids = tempest_contract_prop_chr_vec(),
    optional_capability_ids = tempest_contract_prop_chr_vec(),
    retry_policy = tempest_contract_prop_list(),
    failure_policy = prop_enum(c("stop", "continue"), "stop"),
    approval_checkpoint = S7::new_property(
      S7::class_logical,
      default = FALSE
    ),
    side_effecting = S7::new_property(
      S7::class_logical,
      default = FALSE
    ),
    metadata = tempest_contract_prop_list(),
    schema_version = S7::new_property(S7::class_integer, default = 1L)
  )
)

TempestWorkflowSpec <- S7::new_class(
  "tempest_workflow_spec",
  properties = list(
    workflow_id = tempest_contract_prop_chr(),
    version = tempest_contract_prop_chr("1"),
    title = tempest_contract_prop_chr(),
    purpose = tempest_contract_prop_chr(),
    supported_objective_types = tempest_contract_prop_chr_vec(),
    supported_deliverable_types = tempest_contract_prop_chr_vec(),
    steps = tempest_contract_prop_list(),
    metadata = tempest_contract_prop_list(),
    schema_version = S7::new_property(S7::class_integer, default = 1L)
  )
)

#' Create a Tempest workflow step
#'
#' `r lifecycle::badge("experimental")`
#'
#' A workflow step is a serializable declaration. Its executable operation is
#' resolved from the run's runtime registry before any step begins.
#'
#' @param step_id Stable step identifier.
#' @param title Display title.
#' @param purpose What the step accomplishes.
#' @param operation_id Runtime step-operation identifier.
#' @param version,operation_version Stable definition and operation versions.
#' @param dependency_ids Step identifiers that must succeed first.
#' @param required_input_artifact_ids Artifact ids required by the step.
#' @param produced_artifact_ids Artifact ids the step promises to publish.
#' @param assignment_rule `"none"`, `"all"`, an exact character vector of
#'   expert ids, or a list with `type` and `expert_ids`.
#' @param required_capability_ids,optional_capability_ids Scoped capabilities.
#' @param retry_policy A serializable list with positive `max_attempts`.
#' @param failure_policy Either `"stop"` or `"continue"`.
#' @param approval_checkpoint Whether approval is required before execution.
#' @param side_effecting Whether the operation may change external state.
#' @param metadata Canonical JSON-compatible metadata without credentials.
#' @param schema_version Serializable record schema version.
#' @return A `tempest_workflow_step` S7 object.
#' @export
tempest_workflow_step <- function(
  step_id,
  title,
  purpose,
  operation_id,
  version = "1",
  operation_version = "1",
  dependency_ids = character(),
  required_input_artifact_ids = character(),
  produced_artifact_ids = character(),
  assignment_rule = NULL,
  required_capability_ids = character(),
  optional_capability_ids = character(),
  retry_policy = list(max_attempts = 1L),
  failure_policy = c("stop", "continue"),
  approval_checkpoint = FALSE,
  side_effecting = FALSE,
  metadata = list(),
  schema_version = 1L
) {
  step_id <- tempest_contract_id(step_id, "step_id")
  version <- tempest_workflow_version(version)
  title <- tempest_workflow_scalar(title, "title")
  purpose <- tempest_workflow_scalar(purpose, "purpose")
  dependency_ids <- tempest_contract_ids(dependency_ids, "dependency_ids")
  if (step_id %in% dependency_ids) {
    tempest_workflow_definition_abort("A step cannot depend on itself.")
  }
  operation_id <- tempest_contract_id(operation_id, "operation_id")
  operation_version <- tempest_workflow_version(operation_version)
  required_input_artifact_ids <- tempest_contract_ids(
    required_input_artifact_ids,
    "required_input_artifact_ids"
  )
  produced_artifact_ids <- tempest_contract_ids(
    produced_artifact_ids,
    "produced_artifact_ids"
  )
  assignment_rule <- tempest_workflow_assignment_rule(assignment_rule)
  required_capability_ids <- tempest_contract_ids(
    required_capability_ids,
    "required_capability_ids"
  )
  optional_capability_ids <- tempest_contract_ids(
    optional_capability_ids,
    "optional_capability_ids"
  )
  overlap <- intersect(required_capability_ids, optional_capability_ids)
  if (length(overlap) > 0L) {
    tempest_workflow_definition_abort(
      "Required and optional step capabilities must be disjoint."
    )
  }
  retry_policy <- tempest_workflow_retry_policy(retry_policy)
  failure_policy <- match.arg(failure_policy)
  approval_checkpoint <- tempest_workflow_flag(
    approval_checkpoint,
    "approval_checkpoint"
  )
  side_effecting <- tempest_workflow_flag(side_effecting, "side_effecting")
  metadata <- tempest_contract_serializable_list(metadata, "metadata")
  schema_version <- tempest_contract_schema_version(schema_version)

  TempestWorkflowStep(
    step_id = step_id,
    version = version,
    title = title,
    purpose = purpose,
    dependency_ids = unique(dependency_ids),
    operation_id = operation_id,
    operation_version = operation_version,
    required_input_artifact_ids = unique(required_input_artifact_ids),
    produced_artifact_ids = unique(produced_artifact_ids),
    assignment_rule = assignment_rule,
    required_capability_ids = unique(required_capability_ids),
    optional_capability_ids = unique(optional_capability_ids),
    retry_policy = retry_policy,
    failure_policy = failure_policy,
    approval_checkpoint = approval_checkpoint,
    side_effecting = side_effecting,
    metadata = metadata,
    schema_version = schema_version
  )
}

tempest_workflow_steps <- function(steps) {
  if (
    !is.list(steps) ||
      length(steps) == 0L ||
      any(
        !vapply(
          steps,
          S7::S7_inherits,
          logical(1),
          class = TempestWorkflowStep
        )
      )
  ) {
    tempest_workflow_definition_abort(
      "{.arg steps} must contain at least one {.cls tempest_workflow_step}."
    )
  }
  ids <- vapply(steps, \(step) step@step_id, character(1))
  if (anyDuplicated(ids)) {
    tempest_workflow_definition_abort(
      "Workflow step ids must be unique."
    )
  }
  stats::setNames(unname(steps), ids)
}

tempest_workflow_validate_graph <- function(steps) {
  ids <- names(steps)
  for (step in steps) {
    unknown <- setdiff(step@dependency_ids, ids)
    if (length(unknown) > 0L) {
      tempest_workflow_definition_abort(
        "Step {.val {step@step_id}} depends on unknown step {.val {unknown[[1]]}}."
      )
    }
  }

  indegree <- stats::setNames(
    vapply(steps, \(step) length(step@dependency_ids), integer(1)),
    ids
  )
  ordered <- character()
  ready <- sort(names(indegree)[indegree == 0L])
  while (length(ready) > 0L) {
    current <- ready[[1]]
    ready <- ready[-1]
    ordered <- c(ordered, current)
    children <- ids[vapply(
      steps,
      \(step) current %in% step@dependency_ids,
      logical(1)
    )]
    for (child in children) {
      indegree[[child]] <- indegree[[child]] - 1L
      if (indegree[[child]] == 0L) {
        ready <- sort(unique(c(ready, child)))
      }
    }
  }
  if (length(ordered) != length(ids)) {
    tempest_workflow_definition_abort(
      "Workflow step dependencies must not contain a cycle."
    )
  }

  producers <- list()
  for (step in steps) {
    for (artifact_id in step@produced_artifact_ids) {
      if (!is.null(producers[[artifact_id]])) {
        tempest_workflow_definition_abort(
          "Artifact {.val {artifact_id}} is produced by more than one step."
        )
      }
      producers[[artifact_id]] <- step@step_id
    }
  }
  ancestors <- function(step_id) {
    direct <- steps[[step_id]]@dependency_ids
    unique(c(
      direct,
      unlist(lapply(direct, ancestors), use.names = FALSE)
    ))
  }
  for (step in steps) {
    for (artifact_id in step@required_input_artifact_ids) {
      producer <- producers[[artifact_id]] %||% NULL
      if (is.null(producer)) {
        tempest_workflow_definition_abort(
          "Step {.val {step@step_id}} requires unknown artifact {.val {artifact_id}}."
        )
      }
      if (!producer %in% ancestors(step@step_id)) {
        tempest_workflow_definition_abort(
          "Step {.val {step@step_id}} must depend on the producer of artifact {.val {artifact_id}}."
        )
      }
    }
  }
  ordered
}

#' Create a Tempest workflow specification
#'
#' `r lifecycle::badge("experimental")`
#'
#' @param workflow_id Stable workflow identifier.
#' @param title Display title.
#' @param purpose What the workflow accomplishes.
#' @param steps Non-empty list of [tempest_workflow_step()] objects.
#' @param version Stable workflow version.
#' @param supported_objective_types Objective type ids accepted by the workflow.
#' @param supported_deliverable_types Deliverable content types accepted by the
#'   workflow.
#' @param metadata Canonical JSON-compatible metadata without credentials.
#' @param schema_version Serializable record schema version.
#' @return A validated `tempest_workflow_spec` S7 object.
#' @export
tempest_workflow_spec <- function(
  workflow_id,
  title,
  purpose,
  steps,
  version = "1",
  supported_objective_types = "tempest_objective",
  supported_deliverable_types = character(),
  metadata = list(),
  schema_version = 1L
) {
  workflow_id <- tempest_contract_id(workflow_id, "workflow_id")
  version <- tempest_workflow_version(version)
  title <- tempest_workflow_scalar(title, "title")
  purpose <- tempest_workflow_scalar(purpose, "purpose")
  supported_objective_types <- tempest_contract_ids(
    supported_objective_types,
    "supported_objective_types"
  )
  if (length(supported_objective_types) == 0L) {
    tempest_workflow_definition_abort(
      "{.arg supported_objective_types} must not be empty."
    )
  }
  supported_deliverable_types <- tempest_contract_ids(
    supported_deliverable_types,
    "supported_deliverable_types"
  )
  steps <- tempest_workflow_steps(steps)
  execution_order <- tempest_workflow_validate_graph(steps)
  steps <- steps[execution_order]
  metadata <- tempest_contract_serializable_list(metadata, "metadata")
  schema_version <- tempest_contract_schema_version(schema_version)

  TempestWorkflowSpec(
    workflow_id = workflow_id,
    version = version,
    title = title,
    purpose = purpose,
    supported_objective_types = unique(supported_objective_types),
    supported_deliverable_types = unique(supported_deliverable_types),
    steps = steps,
    metadata = metadata,
    schema_version = schema_version
  )
}

tempest_workflow_step_data <- function(step) {
  tempest_contract_data(step, TempestWorkflowStep, "step")
}

tempest_workflow_step_record <- function(step) {
  tempest_workflow_step_data(step)
}

tempest_workflow_step_from_data <- function(data) {
  if (!is.list(data) || is.data.frame(data)) {
    tempest_workflow_definition_abort(
      "{.arg data} must be a workflow-step record."
    )
  }
  tryCatch(
    tempest_workflow_step(
      step_id = data$step_id,
      title = data$title,
      purpose = data$purpose,
      operation_id = data$operation_id,
      version = data$version %||% "1",
      operation_version = data$operation_version %||% "1",
      dependency_ids = tempest_codec_character(data$dependency_ids),
      required_input_artifact_ids = tempest_codec_character(
        data$required_input_artifact_ids
      ),
      produced_artifact_ids = tempest_codec_character(
        data$produced_artifact_ids
      ),
      assignment_rule = tempest_codec_list(data$assignment_rule),
      required_capability_ids = tempest_codec_character(
        data$required_capability_ids
      ),
      optional_capability_ids = tempest_codec_character(
        data$optional_capability_ids
      ),
      retry_policy = tempest_codec_list(data$retry_policy),
      failure_policy = data$failure_policy %||% "stop",
      approval_checkpoint = isTRUE(data$approval_checkpoint),
      side_effecting = isTRUE(data$side_effecting),
      metadata = tempest_codec_list(data$metadata),
      schema_version = as.integer(data$schema_version %||% 1L)
    ),
    error = function(error) {
      if (inherits(error, "tempest_workflow_definition_error")) {
        stop(error)
      }
      tempest_workflow_definition_abort(
        "Could not restore a workflow-step record.",
        parent = error
      )
    }
  )
}

tempest_workflow_spec_data <- function(workflow) {
  data <- tempest_contract_data(
    workflow,
    TempestWorkflowSpec,
    "workflow"
  )
  data$steps <- lapply(workflow@steps, tempest_workflow_step_record)
  data
}

tempest_workflow_fingerprint <- function(workflow_or_data) {
  data <- if (S7::S7_inherits(workflow_or_data, TempestWorkflowSpec)) {
    tempest_workflow_spec_data(workflow_or_data)
  } else {
    workflow_or_data
  }
  data$fingerprint <- NULL
  digest::digest(
    tempest_canonical_json(data),
    algo = "sha256",
    serialize = FALSE
  )
}

tempest_workflow_spec_record <- function(workflow) {
  data <- tempest_workflow_spec_data(workflow)
  data$fingerprint <- tempest_workflow_fingerprint(workflow)
  data
}

tempest_workflow_spec_from_data <- function(data) {
  restored <- tempest_contract_restore_data(data, "workflow")
  workflow <- tryCatch(
    tempest_workflow_spec(
      workflow_id = restored$data$workflow_id,
      title = restored$data$title,
      purpose = restored$data$purpose,
      steps = lapply(
        restored$data$steps %||% list(),
        tempest_workflow_step_from_data
      ),
      version = restored$data$version %||% "1",
      supported_objective_types = tempest_codec_character(
        restored$data$supported_objective_types
      ),
      supported_deliverable_types = tempest_codec_character(
        restored$data$supported_deliverable_types
      ),
      metadata = tempest_codec_list(restored$data$metadata),
      schema_version = as.integer(
        restored$data$schema_version %||% 1L
      )
    ),
    error = function(error) {
      if (inherits(error, "tempest_workflow_definition_error")) {
        stop(error)
      }
      tempest_workflow_definition_abort(
        "Could not restore a workflow specification.",
        parent = error
      )
    }
  )
  if (
    !identical(
      tempest_workflow_fingerprint(workflow),
      restored$fingerprint
    )
  ) {
    tempest_workflow_definition_abort(
      "Workflow specification fingerprint validation failed."
    )
  }
  workflow
}
