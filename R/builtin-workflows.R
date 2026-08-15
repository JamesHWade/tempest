# Built-in STORM and Co-STORM workflow specifications

tempest_builtin_workflow_abort <- function(message, ..., parent = NULL) {
  tempest_abort(
    message,
    ...,
    class = c(
      "tempest_builtin_workflow_error",
      "tempest_run_error",
      "tempest_error"
    ),
    parent = parent,
    .envir = rlang::caller_env()
  )
}

#' Create the built-in STORM workflow specification
#'
#' `r lifecycle::badge("experimental")`
#'
#' This experimental API is frozen and scheduled for removal in Tempest 0.2.0.
#' No compatibility shim is planned; see
#' [tempest-generic-kernel-retirement].
#'
#' The specification declares the five durable STORM stages. Executable
#' operations are supplied by [tempest_builtin_workflow_operation_registry()].
#'
#' @return A `tempest_workflow_spec`.
#' @examples
#' workflow <- tempest_storm_workflow_spec()
#' names(workflow@steps)
#' @export
tempest_storm_workflow_spec <- function() {
  tempest_workflow_spec(
    workflow_id = "tempest.storm",
    title = "STORM research workflow",
    purpose = paste(
      "Research a topic from multiple perspectives and produce an",
      "evidence-backed report."
    ),
    supported_deliverable_types = c("text", "workflow_checkpoint"),
    metadata = list(
      family = "storm",
      interaction_model = "scripted"
    ),
    steps = list(
      tempest_workflow_step(
        step_id = "perspectives",
        title = "Discover perspectives",
        purpose = "Select research perspectives and the expert pool.",
        operation_id = "tempest.step.storm.perspectives",
        produced_artifact_ids = "storm.perspectives"
      ),
      tempest_workflow_step(
        step_id = "research",
        title = "Research the topic",
        purpose = "Collect source-backed findings for each perspective.",
        operation_id = "tempest.step.storm.research",
        dependency_ids = "perspectives",
        required_input_artifact_ids = "storm.perspectives",
        produced_artifact_ids = "storm.research",
        assignment_rule = list(type = "all", expert_ids = character())
      ),
      tempest_workflow_step(
        step_id = "outline",
        title = "Create the outline",
        purpose = "Organize the verified research into a report outline.",
        operation_id = "tempest.step.storm.outline",
        dependency_ids = "research",
        required_input_artifact_ids = "storm.research",
        produced_artifact_ids = "storm.outline"
      ),
      tempest_workflow_step(
        step_id = "write",
        title = "Write the draft",
        purpose = "Draft the report from the outline and evidence ledger.",
        operation_id = "tempest.step.storm.write",
        dependency_ids = "outline",
        required_input_artifact_ids = "storm.outline",
        produced_artifact_ids = "storm.draft"
      ),
      tempest_workflow_step(
        step_id = "polish",
        title = "Polish the report",
        purpose = "Validate citations and produce the final report.",
        operation_id = "tempest.step.storm.polish",
        dependency_ids = "write",
        required_input_artifact_ids = "storm.draft",
        produced_artifact_ids = "report_md"
      )
    )
  )
}

#' Create the built-in Co-STORM workflow specification
#'
#' `r lifecycle::badge("experimental")`
#'
#' This experimental API is frozen and scheduled for removal in Tempest 0.2.0.
#' No compatibility shim is planned; see
#' [tempest-generic-kernel-retirement].
#'
#' The interactive dialogue is an approval checkpoint. Starting the workflow
#' runs warmup and then returns in `awaiting_approval`. A host conducts as many
#' [TempestSession] turns as needed before approving the dialogue checkpoint
#' and resuming the run to produce the report.
#'
#' @return A `tempest_workflow_spec`.
#' @examples
#' workflow <- tempest_costorm_workflow_spec()
#' workflow@steps$dialogue@approval_checkpoint
#' @export
tempest_costorm_workflow_spec <- function() {
  tempest_workflow_spec(
    workflow_id = "tempest.costorm",
    title = "Co-STORM interactive research workflow",
    purpose = paste(
      "Warm an expert panel, conduct an open-ended dialogue, and",
      "synthesize the session into an evidence-backed report."
    ),
    supported_deliverable_types = c("text", "workflow_checkpoint"),
    metadata = list(
      family = "costorm",
      interaction_model = "approval_gated_dialogue"
    ),
    steps = list(
      tempest_workflow_step(
        step_id = "warmup",
        title = "Warm the expert panel",
        purpose = "Run each expert's initial research work.",
        operation_id = "tempest.step.costorm.warmup",
        produced_artifact_ids = "costorm.warmup"
      ),
      tempest_workflow_step(
        step_id = "dialogue",
        title = "Conduct the dialogue",
        purpose = paste(
          "Keep the session open for host-driven expert and user turns",
          "until the host approves report generation."
        ),
        operation_id = "tempest.step.costorm.dialogue",
        dependency_ids = "warmup",
        required_input_artifact_ids = "costorm.warmup",
        produced_artifact_ids = "costorm.dialogue",
        approval_checkpoint = TRUE
      ),
      tempest_workflow_step(
        step_id = "report",
        title = "Generate the report",
        purpose = "Synthesize the approved session into the final report.",
        operation_id = "tempest.step.costorm.report",
        dependency_ids = "dialogue",
        required_input_artifact_ids = "costorm.dialogue",
        produced_artifact_ids = "report_md"
      )
    )
  )
}

tempest_workflow_checkpoint_spec <- function() {
  tempest_deliverable_spec(
    deliverable_id = "tempest-workflow-checkpoint",
    title = "Workflow checkpoint",
    purpose = "Record the durable boundary between workflow operations.",
    instructions = paste(
      "Record only serializable stage metadata and references to state owned",
      "by the run."
    ),
    evidence_policy = "none",
    generator_id = "tempest.generator.provided_content",
    renderer_ids = "tempest.renderer.workflow_checkpoint",
    operation_versions = c(
      "tempest.generator.provided_content" = "1",
      "tempest.renderer.workflow_checkpoint" = "1"
    ),
    content_type = "workflow_checkpoint",
    media_types = "application/json",
    metadata = list(internal = TRUE)
  )
}

tempest_builtin_workflow_checkpoint_renderer <- function(content) {
  tempest_artifact_representation(
    content = content,
    artifact_kind = "checkpoint",
    media_type = "application/json"
  )
}

tempest_workflow_checkpoint_artifact <- function(
  artifact_id,
  context,
  state = list()
) {
  deliverable <- tempest_workflow_checkpoint_spec()
  context$artifact_catalog$register(deliverable)
  state <- tempest_contract_serializable_list(state, "state")
  source_count <- if (inherits(context$source_store, "SourceStore")) {
    length(context$source_store$list_sources())
  } else {
    0L
  }
  claim_count <- if (inherits(context$source_store, "SourceStore")) {
    length(context$source_store$list_claims())
  } else {
    0L
  }
  tempest_artifact(
    deliverable = deliverable,
    content = list(
      workflow_id = context$workflow@workflow_id,
      step_id = context$step@step_id,
      objective_id = context$objective@objective_id,
      attempt = context$attempt,
      source_count = source_count,
      claim_count = claim_count,
      state = state
    ),
    artifact_id = artifact_id,
    artifact_kind = "checkpoint",
    media_type = "application/json",
    producer_operation_id = context$step@operation_id,
    run_id = context$run_id,
    step_id = context$step@step_id,
    parent_artifact_ids = names(context$input_artifacts),
    status = "valid"
  )
}

tempest_builtin_workflow_adapter <- function(
  adapter,
  workflow,
  stage
) {
  force(adapter)
  force(workflow)
  force(stage)
  function(context, run) {
    if (!is.function(adapter)) {
      tempest_builtin_workflow_abort(
        paste0(
          "Operation for {.val {workflow}} stage {.val {stage}} requires ",
          "a runtime adapter."
        )
      )
    }
    value <- tempest_call_operation(
      adapter,
      list(
        stage = stage,
        context = context,
        run = run
      )
    )
    artifacts <- tempest_run_result_artifacts(value)
    artifact_ids <- vapply(
      artifacts,
      \(artifact) artifact@artifact_id,
      character(1)
    )
    expected_ids <- context$step@produced_artifact_ids
    present <- vapply(
      expected_ids,
      function(artifact_id) {
        artifact_id %in%
          artifact_ids ||
          context$artifact_catalog$has(artifact_id)
      },
      logical(1)
    )
    missing_ids <- expected_ids[!present]
    # Intermediate stages may return legacy values rather than typed artifacts.
    # Preserve their output ids as checkpoints; the final report is mandatory.
    missing_reports <- intersect(missing_ids, "report_md")
    if (length(missing_reports) > 0L) {
      tempest_builtin_workflow_abort(
        paste0(
          "Operation for {.val {workflow}} stage {.val {stage}} did not ",
          "produce the final report artifact {.val report_md}."
        )
      )
    }
    checkpoints <- lapply(
      missing_ids,
      tempest_workflow_checkpoint_artifact,
      context = context,
      state = if (
        is.list(value) &&
          !is.data.frame(value) &&
          is.list(value$checkpoint)
      ) {
        value$checkpoint
      } else {
        list()
      }
    )
    existing <- expected_ids[
      vapply(
        expected_ids,
        context$artifact_catalog$has,
        logical(1)
      )
    ]
    catalog_artifacts <- lapply(
      setdiff(existing, artifact_ids),
      context$artifact_catalog$get
    )
    list(
      artifacts = c(artifacts, catalog_artifacts, checkpoints),
      value = value
    )
  }
}

#' Create a registry for Tempest's built-in research workflows
#'
#' `r lifecycle::badge("experimental")`
#'
#' This experimental API is frozen and scheduled for removal in Tempest 0.2.0.
#' No compatibility shim is planned; see
#' [tempest-generic-kernel-retirement].
#'
#' The returned registry resolves every operation used by
#' [tempest_storm_workflow_spec()] and [tempest_costorm_workflow_spec()].
#' Adapters bind the serializable definitions to process-local STORM or
#' Co-STORM execution state.
#'
#' @param storm_adapter Optional function created by
#'   [tempest_storm_workflow_adapter()] or a compatible function accepting
#'   `stage`, `context`, and `run`.
#' @param costorm_adapter Optional function created by
#'   [tempest_costorm_workflow_adapter()] or a compatible function accepting
#'   `stage`, `context`, and `run`.
#' @param registry Optional operation registry to extend. By default the
#'   built-in deliverable operations are included.
#' @return A `TempestOperationRegistry`.
#' @export
tempest_builtin_workflow_operation_registry <- function(
  storm_adapter = NULL,
  costorm_adapter = NULL,
  registry = tempest_builtin_operation_registry()
) {
  if (!is.null(storm_adapter) && !is.function(storm_adapter)) {
    tempest_builtin_workflow_abort(
      "{.arg storm_adapter} must be NULL or a function."
    )
  }
  if (!is.null(costorm_adapter) && !is.function(costorm_adapter)) {
    tempest_builtin_workflow_abort(
      "{.arg costorm_adapter} must be NULL or a function."
    )
  }
  if (!inherits(registry, "TempestOperationRegistry")) {
    tempest_builtin_workflow_abort(
      "{.arg registry} must be a TempestOperationRegistry."
    )
  }
  registry$register(
    id = "tempest.renderer.workflow_checkpoint",
    implementation = tempest_builtin_workflow_checkpoint_renderer,
    version = "1",
    kind = "renderer",
    metadata = list(
      media_type = "application/json",
      artifact_kind = "checkpoint"
    )
  )
  for (stage in c("perspectives", "research", "outline", "write", "polish")) {
    registry$register(
      id = paste0("tempest.step.storm.", stage),
      implementation = tempest_builtin_workflow_adapter(
        storm_adapter,
        "storm",
        stage
      ),
      version = "1",
      kind = "step",
      metadata = list(workflow = "storm", stage = stage)
    )
  }
  for (stage in c("warmup", "dialogue", "report")) {
    registry$register(
      id = paste0("tempest.step.costorm.", stage),
      implementation = tempest_builtin_workflow_adapter(
        costorm_adapter,
        "costorm",
        stage
      ),
      version = "1",
      kind = "step",
      metadata = list(workflow = "costorm", stage = stage)
    )
  }
  registry
}

tempest_builtin_workflow_runtime <- function(runtime, registry) {
  if (!inherits(runtime, "TempestRuntime")) {
    tempest_builtin_workflow_abort(
      "{.arg runtime} must be created by {.fn tempest_runtime}."
    )
  }
  if (!inherits(registry, "TempestOperationRegistry")) {
    tempest_builtin_workflow_abort(
      "{.arg registry} must be a TempestOperationRegistry."
    )
  }
  TempestRuntime$new(
    operations = registry,
    skills = runtime$skills,
    capabilities = runtime$capabilities,
    connections = runtime$connections
  )
}

tempest_builtin_workflow_runtime_scope_matches <- function(left, right) {
  inherits(left, "TempestRuntime") &&
    inherits(right, "TempestRuntime") &&
    identical(left$skills, right$skills) &&
    identical(left$capabilities, right$capabilities) &&
    identical(left$connections, right$connections)
}

tempest_storm_workflow_reserved_arguments <- function() {
  c(
    "topic",
    "config",
    "retriever",
    "n_experts",
    "experts",
    "runtime",
    "runtime_factory",
    "connection_permissions",
    "steps",
    "output_dir",
    "resume",
    "run_id",
    "progress",
    "verbose",
    "artifact_catalog",
    "workflow_run"
  )
}

tempest_storm_workflow_extra_arguments <- function(arguments) {
  if (!is.list(arguments) || is.data.frame(arguments)) {
    tempest_builtin_workflow_abort(
      "Forwarded STORM arguments must be a list."
    )
  }
  argument_names <- names(arguments) %||% character()
  if (
    length(arguments) > 0L &&
      (length(argument_names) != length(arguments) ||
        any(!nzchar(argument_names)) ||
        anyDuplicated(argument_names))
  ) {
    tempest_builtin_workflow_abort(
      "Forwarded STORM arguments must be uniquely named."
    )
  }
  reserved <- intersect(
    argument_names,
    tempest_storm_workflow_reserved_arguments()
  )
  if (length(reserved) > 0L) {
    tempest_builtin_workflow_abort(
      paste0(
        "Forwarded STORM argument {.val {reserved[[1]]}} is managed by ",
        "the adapter."
      )
    )
  }
  arguments
}

tempest_storm_workflow_set_deliverable <- function(
  run,
  catalog,
  deliverable
) {
  catalog$register(deliverable)
  keep <- vapply(
    run$deliverables,
    \(value) !identical(value@deliverable_id, deliverable@deliverable_id),
    logical(1)
  )
  run$deliverables <- c(run$deliverables[keep], list(deliverable))
  invisible(deliverable)
}

tempest_storm_workflow_checkpoint <- function(stage, result) {
  store_artifact_ids <- switch(
    stage,
    perspectives = c("title", "perspectives", "experts"),
    research = "claims",
    outline = "outline",
    write = "draft_md",
    polish = "report_md",
    character()
  )
  checkpoint <- list(
    store_artifact_ids = store_artifact_ids,
    title = result$title %||% NA_character_,
    expert_ids = vapply(
      result$experts %||% list(),
      \(expert) expert@expert_id,
      character(1)
    )
  )
  if (identical(stage, "perspectives")) {
    checkpoint$perspective_count <- length(result$perspectives %||% list())
  } else if (identical(stage, "outline")) {
    checkpoint$section_count <- length(
      result$outline$sections %||% list()
    )
  } else if (identical(stage, "write")) {
    checkpoint$draft_characters <- nchar(result$draft_md %||% "")
  }
  checkpoint
}

tempest_storm_workflow_update_experts <- function(run, experts, stage) {
  expert_map <- tempest_run_expert_map(experts)
  if (!identical(stage, "perspectives")) {
    if (!identical(expert_map, run$experts)) {
      tempest_builtin_workflow_abort(
        "The STORM expert pool cannot change after perspective discovery."
      )
    }
    return(invisible(run$experts))
  }

  assignments <- tempest_run_assignments(run$workflow, expert_map)
  changed <- vapply(
    names(assignments),
    \(step_id) !identical(assignments[[step_id]], run$assignments[[step_id]]),
    logical(1)
  )
  locked <- vapply(
    names(assignments),
    function(step_id) {
      !run$step_states[[step_id]]$status %in% c("pending", "running")
    },
    logical(1)
  )
  unsafe <- names(assignments)[changed & locked]
  if (length(unsafe) > 0L) {
    tempest_builtin_workflow_abort(
      paste0(
        "Cannot reassign completed STORM step ",
        "{.val {unsafe[[1]]}} after expert generation."
      )
    )
  }

  run$experts <- expert_map
  run$assignments <- assignments
  invisible(expert_map)
}

#' Create an adapter for the built-in STORM workflow
#'
#' `r lifecycle::badge("experimental")`
#'
#' This experimental API is frozen and scheduled for removal in Tempest 0.2.0.
#' No compatibility shim is planned; see
#' [tempest-generic-kernel-retirement].
#'
#' The adapter executes one existing [tempest_run()] stage at a time against
#' the generic run's shared evidence store and artifact catalog. Runtime and
#' connection permissions always come from the owning `TempestRun`.
#'
#' @param config A `TempestConfig`.
#' @param retriever Optional retriever whose store must be the run's
#'   `source_store`.
#' @param n_experts Number of experts to generate when the run starts without
#'   an explicit expert pool.
#' @param runtime_factory Existing STORM runtime factory.
#' @param stage_progress Optional legacy STORM progress callback.
#' @param verbose Whether existing STORM stages print progress.
#' @param ... Additional named [tempest_run()] arguments such as
#'   `research_strategy` or `max_rounds`.
#' @return A process-local workflow adapter function.
#' @export
tempest_storm_workflow_adapter <- function(
  config = tempest_config(),
  retriever = NULL,
  n_experts = 3,
  runtime_factory = function() tempest_runtime(),
  stage_progress = NULL,
  verbose = TRUE,
  ...
) {
  if (!S7::S7_inherits(config, TempestConfig)) {
    tempest_builtin_workflow_abort(
      "{.arg config} must be created by {.fn tempest_config}."
    )
  }
  n_experts <- tempest_config_count(n_experts, "n_experts")
  if (!is.function(runtime_factory)) {
    tempest_builtin_workflow_abort(
      "{.arg runtime_factory} must be a function."
    )
  }
  if (!is.null(stage_progress) && !is.function(stage_progress)) {
    tempest_builtin_workflow_abort(
      "{.arg stage_progress} must be NULL or a function."
    )
  }
  verbose <- tempest_config_flag(verbose, "verbose")
  arguments <- tempest_storm_workflow_extra_arguments(list(...))
  function(stage, context, run) {
    if (!inherits(run, "TempestRun")) {
      tempest_builtin_workflow_abort(
        "The STORM adapter requires an owning TempestRun."
      )
    }
    if (!inherits(run$runtime, "TempestRuntime")) {
      tempest_builtin_workflow_abort(
        "The STORM adapter requires the run to own a TempestRuntime."
      )
    }
    active_retriever <- retriever
    if (is.null(active_retriever)) {
      if (!inherits(context$source_store, "SourceStore")) {
        tempest_builtin_workflow_abort(
          "The STORM adapter requires a shared SourceStore."
        )
      }
      active_retriever <- tempest_retriever(
        config = config,
        store = context$source_store
      )
    }
    if (
      is.null(active_retriever$store) ||
        !identical(active_retriever$store, context$source_store)
    ) {
      tempest_builtin_workflow_abort(
        "The STORM adapter retriever must use the run's SourceStore."
      )
    }
    selected_experts <- unname(run$experts)
    call_arguments <- c(
      list(
        topic = context$objective@description,
        config = config,
        retriever = active_retriever,
        n_experts = if (length(selected_experts) > 0L) {
          length(selected_experts)
        } else {
          n_experts
        },
        experts = if (length(selected_experts) > 0L) {
          selected_experts
        } else {
          NULL
        },
        runtime = run$runtime,
        runtime_factory = runtime_factory,
        connection_permissions = run$connection_permissions,
        steps = stage,
        output_dir = NULL,
        resume = FALSE,
        run_id = context$run_id,
        progress = stage_progress,
        verbose = verbose,
        artifact_catalog = context$artifact_catalog,
        workflow_run = run
      ),
      arguments
    )
    result <- do.call(tempest_run, call_arguments)
    if (length(result$experts %||% list()) > 0L) {
      tempest_storm_workflow_update_experts(run, result$experts, stage)
    }
    report_ids <- vapply(
      run$deliverables,
      \(deliverable) deliverable@deliverable_id,
      character(1)
    )
    if (!"storm-report" %in% report_ids) {
      deliverable <- tempest_storm_report_spec(
        title = result$title %||% context$objective@title,
        config = config,
        remove_duplicate = arguments$remove_duplicate %||% FALSE
      )
      tempest_storm_workflow_set_deliverable(
        run,
        context$artifact_catalog,
        deliverable
      )
    }
    result$checkpoint <- tempest_storm_workflow_checkpoint(stage, result)
    result
  }
}

#' Run STORM through the generic Tempest workflow kernel
#'
#' `r lifecycle::badge("experimental")`
#'
#' This experimental API is frozen and scheduled for removal in Tempest 0.2.0.
#' No compatibility shim is planned; see
#' [tempest-generic-kernel-retirement].
#'
#' This is the generic-run counterpart to [tempest_run()]. It returns the
#' `TempestRun`; each step result retains the corresponding legacy STORM result
#' under `$value`, including its `$workflow_run` handle.
#'
#' @inheritParams tempest_storm_workflow_adapter
#' @param topic Research objective.
#' @param experts Optional exact expert pool.
#' @param runtime A `TempestRuntime` containing process-local adapters.
#' @param connection_permissions Named list mapping expert or model-role ids to
#'   opaque connection ids allowed for the run.
#' @param artifact_catalog Optional shared typed artifact catalog.
#' @param run_id Optional stable generic run id.
#' @param progress Optional generic run event callback.
#' @return A `TempestRun`.
#' @export
tempest_storm_workflow_run <- function(
  topic,
  config = tempest_config(),
  retriever = NULL,
  n_experts = 3,
  experts = NULL,
  runtime = tempest_runtime(),
  runtime_factory = function() tempest_runtime(),
  connection_permissions = list(),
  artifact_catalog = NULL,
  run_id = NULL,
  progress = NULL,
  stage_progress = NULL,
  verbose = TRUE,
  ...
) {
  topic <- tempest_workflow_scalar(topic, "topic")
  arguments <- tempest_storm_workflow_extra_arguments(list(...))
  if (is.null(retriever)) {
    source_store <- SourceStore$new()
    retriever <- tempest_retriever(config = config, store = source_store)
  } else {
    source_store <- retriever$store %||% NULL
    if (!inherits(source_store, "SourceStore")) {
      tempest_builtin_workflow_abort(
        "{.arg retriever} must expose a SourceStore at {.field store}."
      )
    }
  }
  if (!is.null(experts)) {
    experts <- tempest_validate_experts(experts)
  }
  artifact_catalog <- artifact_catalog %||%
    tempest_artifact_catalog(store = config@artifact_store)
  if (!inherits(artifact_catalog, "TempestArtifactCatalog")) {
    tempest_builtin_workflow_abort(
      "{.arg artifact_catalog} must be a TempestArtifactCatalog."
    )
  }
  adapter <- do.call(
    tempest_storm_workflow_adapter,
    c(
      list(
        config = config,
        retriever = retriever,
        n_experts = n_experts,
        runtime_factory = runtime_factory,
        stage_progress = stage_progress,
        verbose = verbose
      ),
      arguments
    )
  )
  checkpoint <- tempest_workflow_checkpoint_spec()
  report <- tempest_storm_report_spec(
    title = topic,
    config = config,
    remove_duplicate = arguments$remove_duplicate %||% FALSE
  )
  workflow_runtime <- tempest_builtin_workflow_runtime(
    runtime,
    tempest_builtin_workflow_operation_registry(
      storm_adapter = adapter
    )
  )
  tempest_run_workflow(
    objective = tempest_objective(
      description = topic,
      deliverable_ids = "storm-report",
      metadata = list(workflow = "storm")
    ),
    workflow = tempest_storm_workflow_spec(),
    runtime = workflow_runtime,
    experts = experts %||% list(),
    connection_permissions = connection_permissions,
    deliverables = list(checkpoint, report),
    artifact_catalog = artifact_catalog,
    source_store = source_store,
    runtime_context = list(
      config = config,
      retriever = retriever
    ),
    run_id = run_id,
    progress = progress
  )
}

#' Create an adapter for the built-in Co-STORM workflow
#'
#' `r lifecycle::badge("experimental")`
#'
#' This experimental API is frozen and scheduled for removal in Tempest 0.2.0.
#' No compatibility shim is planned; see
#' [tempest-generic-kernel-retirement].
#'
#' Warmup and report operations call the existing `TempestSession` methods.
#' The dialogue operation records the current session boundary; individual
#' interactive turns remain host-driven while the run awaits approval.
#'
#' @param session A `TempestSession`.
#' @param style Report style.
#' @param include_references Whether the report includes references.
#' @param reorganize Whether to reorganize the mind map before reporting.
#' @param verbose Whether warmup prints progress.
#' @return A process-local workflow adapter function.
#' @export
tempest_costorm_workflow_adapter <- function(
  session,
  style = c("technical", "executive"),
  include_references = TRUE,
  reorganize = TRUE,
  verbose = TRUE
) {
  if (!inherits(session, "TempestSession")) {
    tempest_builtin_workflow_abort(
      "{.arg session} must be a TempestSession."
    )
  }
  style <- match.arg(style)
  include_references <- tempest_config_flag(
    include_references,
    "include_references"
  )
  reorganize <- tempest_config_flag(reorganize, "reorganize")
  verbose <- tempest_config_flag(verbose, "verbose")
  function(stage, context, run) {
    if (!inherits(run, "TempestRun")) {
      tempest_builtin_workflow_abort(
        "The Co-STORM adapter requires an owning TempestRun."
      )
    }
    if (
      !identical(session$store, context$source_store) ||
        !identical(session$artifact_catalog, context$artifact_catalog)
    ) {
      tempest_builtin_workflow_abort(
        paste0(
          "The Co-STORM adapter must share the session store and artifact ",
          "catalog."
        )
      )
    }
    if (
      !identical(
        tempest_run_expert_map(session$experts),
        run$experts
      )
    ) {
      tempest_builtin_workflow_abort(
        "The Co-STORM adapter expert pool must match the owning run."
      )
    }
    if (
      !tempest_builtin_workflow_runtime_scope_matches(
        session$runtime,
        run$runtime
      )
    ) {
      tempest_builtin_workflow_abort(
        "The Co-STORM adapter runtime must match the owning run."
      )
    }
    session_permissions <- tempest_run_connection_permissions(
      session$connection_permissions,
      run$runtime
    )
    if (!identical(session_permissions, run$connection_permissions)) {
      tempest_builtin_workflow_abort(
        "The Co-STORM adapter connection permissions must match the owning run."
      )
    }
    session$workflow_run <- run
    switch(
      stage,
      warmup = {
        result <- session$warmup(verbose = verbose)
        list(
          checkpoint = list(
            expert_ids = vapply(
              session$experts,
              \(expert) expert@expert_id,
              character(1)
            ),
            result_count = length(result)
          ),
          value = result
        )
      },
      dialogue = list(
        checkpoint = list(
          transcript_turns = length(session$transcript),
          mindmap_nodes = length(session$mindmap$nodes %||% list())
        )
      ),
      report = session$report(
        style = style,
        include_references = include_references,
        reorganize = reorganize
      ),
      tempest_builtin_workflow_abort(
        "Unknown Co-STORM stage {.val {stage}}."
      )
    )
  }
}

#' Attach and start the generic workflow for a Co-STORM session
#'
#' `r lifecycle::badge("experimental")`
#'
#' This experimental API is frozen and scheduled for removal in Tempest 0.2.0.
#' No compatibility shim is planned; see
#' [tempest-generic-kernel-retirement].
#'
#' The run executes warmup and then waits at the dialogue approval checkpoint.
#' Conduct session turns normally, then approve the pending checkpoint with
#' [tempest_run_record_approval()] to snapshot the dialogue and generate the
#' report.
#'
#' @inheritParams tempest_costorm_workflow_adapter
#' @param run_id Optional run id. Defaults to the session id.
#' @param progress Optional generic run event callback.
#' @return The session-owned `TempestRun` in `awaiting_approval`.
#' @export
tempest_costorm_workflow_run <- function(
  session,
  style = c("technical", "executive"),
  include_references = TRUE,
  reorganize = TRUE,
  verbose = TRUE,
  run_id = session$session_id,
  progress = NULL
) {
  if (!inherits(session, "TempestSession")) {
    tempest_builtin_workflow_abort(
      "{.arg session} must be a TempestSession."
    )
  }
  existing <- session$workflow_run %||% NULL
  if (
    inherits(existing, "TempestRun") &&
      !existing$status %in% c("succeeded", "failed", "cancelled")
  ) {
    tempest_builtin_workflow_abort(
      "The session already owns an active generic workflow run."
    )
  }
  adapter <- tempest_costorm_workflow_adapter(
    session = session,
    style = style,
    include_references = include_references,
    reorganize = reorganize,
    verbose = verbose
  )
  checkpoint <- tempest_workflow_checkpoint_spec()
  report <- tempest_costorm_report_spec(session)
  workflow_runtime <- tempest_builtin_workflow_runtime(
    session$runtime,
    tempest_builtin_workflow_operation_registry(
      costorm_adapter = adapter
    )
  )
  run <- tempest_run_workflow(
    objective = tempest_objective(
      description = session$topic,
      title = session$title %||% session$topic,
      deliverable_ids = report@deliverable_id,
      metadata = list(
        workflow = "costorm",
        costorm_options = list(
          style = match.arg(style),
          include_references = include_references,
          reorganize = reorganize,
          verbose = verbose
        )
      )
    ),
    workflow = tempest_costorm_workflow_spec(),
    runtime = workflow_runtime,
    experts = session$experts,
    connection_permissions = session$connection_permissions,
    deliverables = list(checkpoint, report),
    artifact_catalog = session$artifact_catalog,
    source_store = session$store,
    runtime_context = list(
      config = session$config,
      retriever = session$retriever,
      expert_session_manager = session$expert_session_manager,
      topic = session$topic
    ),
    run_id = run_id,
    progress = progress
  )
  session$workflow_run <- run
  run
}
