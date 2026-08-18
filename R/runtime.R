# Runtime composition and built-in scoped capabilities

tempest_runtime_abort <- function(message, ..., parent = NULL) {
  tempest_abort(
    message,
    ...,
    class = c("tempest_runtime_error", "tempest_error"),
    parent = parent,
    .envir = rlang::caller_env()
  )
}

tempest_runtime_retriever <- function(context, capability_id) {
  retriever <- context$retriever %||% NULL
  if (!inherits(retriever, "TempestRetriever")) {
    tempest_runtime_abort(
      paste0(
        "Capability {.val {capability_id}} requires a ",
        "{.cls TempestRetriever} in runtime context."
      )
    )
  }
  retriever
}

tempest_builtin_capability_specs <- function() {
  list(
    tempest_capability_spec(
      "tempest.research.web",
      purpose = "Discover and inspect public web evidence.",
      instructions = paste(
        "Search only when the execution context grants web research.",
        "Inspect sources before relying on them."
      ),
      operation_id = "tempest.capability.research.web",
      model_roles = c("expert", "coordinator")
    ),
    tempest_capability_spec(
      "tempest.evidence.read",
      purpose = "Read sources, claims, and evidence in the run ledger.",
      instructions = "Use only evidence already available to this run.",
      operation_id = "tempest.capability.evidence.read",
      model_roles = c("expert", "coordinator", "writer", "mindmap", "judge")
    ),
    tempest_capability_spec(
      "tempest.evidence.write",
      purpose = "Record source-backed claims in the run ledger.",
      instructions = "Record only atomic claims backed by inspected sources.",
      operation_id = "tempest.capability.evidence.write",
      model_roles = "expert"
    ),
    tempest_capability_spec(
      "tempest.retrieval.semantic",
      purpose = "Retrieve approved evidence from the semantic store.",
      instructions = "Retrieve only from the run-scoped semantic store.",
      operation_id = "tempest.capability.retrieval.semantic",
      model_roles = c("expert", "writer", "mindmap")
    )
  )
}

tempest_builtin_capability_implementations <- function() {
  list(
    "tempest.research.web" = function(
      capability_spec,
      connections,
      context
    ) {
      retriever <- tempest_runtime_retriever(
        context,
        capability_spec@capability_id
      )
      list(
        tools = tempest_tools_web(
          retriever,
          model = context$model %||% NULL,
          search_provider = context$search_provider %||% "native"
        ),
        registrars = list(),
        metadata = list(tool_scope = "web")
      )
    },
    "tempest.evidence.read" = function(
      capability_spec,
      connections,
      context
    ) {
      retriever <- tempest_runtime_retriever(
        context,
        capability_spec@capability_id
      )
      list(
        tools = tempest_tools_evidence_read(retriever),
        registrars = list(),
        metadata = list(access = "read")
      )
    },
    "tempest.evidence.write" = function(
      capability_spec,
      connections,
      context
    ) {
      retriever <- tempest_runtime_retriever(
        context,
        capability_spec@capability_id
      )
      list(
        tools = tempest_tools_evidence_write(
          retriever,
          claim_provenance = context$claim_provenance %||% list()
        ),
        registrars = list(),
        metadata = list(access = "write")
      )
    },
    "tempest.retrieval.semantic" = function(
      capability_spec,
      connections,
      context
    ) {
      retriever <- tempest_runtime_retriever(
        context,
        capability_spec@capability_id
      )
      registrar <- tempest_semantic_retrieval_registrar(retriever)
      if (is.null(registrar)) {
        tempest_runtime_abort(
          "Semantic retrieval is unavailable in this execution context."
        )
      }
      list(
        tools = list(),
        registrars = list(registrar),
        metadata = list(store = "run_scoped")
      )
    }
  )
}

tempest_named_contracts <- function(values, id_property, arg) {
  values <- values %||% list()
  if (!is.list(values) || is.data.frame(values)) {
    tempest_runtime_abort("{.arg {arg}} must be a list.")
  }
  if (length(values) == 0L) {
    return(values)
  }
  ids <- vapply(
    values,
    \(value) S7::prop(value, id_property),
    character(1)
  )
  if (anyDuplicated(ids)) {
    tempest_runtime_abort(
      "Duplicate definition {.val {ids[duplicated(ids)][[1]]}} in {.arg {arg}}."
    )
  }
  stats::setNames(unname(values), ids)
}

TempestRuntime <- R6::R6Class(
  "TempestRuntime",
  public = list(
    operations = NULL,
    skills = NULL,
    capabilities = NULL,
    connections = NULL,

    initialize = function(
      operations,
      skills,
      capabilities,
      connections
    ) {
      if (!inherits(operations, "TempestOperationRegistry")) {
        tempest_runtime_abort(
          "{.arg operations} must be a Tempest operation registry."
        )
      }
      if (!inherits(skills, "TempestSkillRegistry")) {
        tempest_runtime_abort(
          "{.arg skills} must be a Tempest skill registry."
        )
      }
      if (!inherits(capabilities, "TempestCapabilityResolver")) {
        tempest_runtime_abort(
          "{.arg capabilities} must be a Tempest capability resolver."
        )
      }
      if (!inherits(connections, "TempestConnectionProvider")) {
        tempest_runtime_abort(
          "{.arg connections} must be a Tempest connection provider."
        )
      }
      self$operations <- operations
      self$skills <- skills
      self$capabilities <- capabilities
      self$connections <- connections
      invisible(self)
    },

    resolve_role = function(
      role,
      required_capability_ids = character(),
      optional_capability_ids = character(),
      allowed_connection_ref_ids = character(),
      context = list()
    ) {
      role <- tempest_contract_id(role, "role")
      resolution <- self$capabilities$resolve(
        required_capability_ids = required_capability_ids,
        optional_capability_ids = optional_capability_ids,
        allowed_connection_ref_ids = allowed_connection_ref_ids,
        model_role = role,
        context = utils::modifyList(context, list(model_role = role))
      )
      structure(
        list(
          role = role,
          skills = NULL,
          capabilities = resolution,
          grants = resolution$grants
        ),
        class = c("tempest_runtime_resolution", "list")
      )
    },

    resolve_expert = function(
      expert,
      allowed_connection_ref_ids = character(),
      context = list()
    ) {
      tempest_validate_experts(list(expert))
      skill_resolution <- self$skills$resolve(
        skill_ids = expert@skill_ids,
        versions = expert@skill_versions,
        required_capability_ids = expert@required_capability_ids,
        optional_capability_ids = expert@optional_capability_ids
      )
      role <- expert@model_role
      if (is.na(role)) {
        tempest_runtime_abort(
          paste0(
            "Expert {.val {expert@expert_id}} uses a model policy reference ",
            "that the host must resolve before chat creation."
          )
        )
      }
      capability_resolution <- self$capabilities$resolve(
        required_capability_ids = skill_resolution$required_capability_ids,
        optional_capability_ids = skill_resolution$optional_capability_ids,
        allowed_connection_ref_ids = allowed_connection_ref_ids,
        model_role = role,
        context = utils::modifyList(
          context,
          list(
            model_role = role,
            expert = expert,
            expert_id = expert@expert_id
          )
        )
      )
      structure(
        list(
          role = role,
          expert_id = expert@expert_id,
          expert_version = expert@version,
          expert_fingerprint = tempest_expert_profile_fingerprint(expert),
          skills = skill_resolution,
          capabilities = capability_resolution,
          grants = capability_resolution$grants,
          instructions = skill_resolution$prompt
        ),
        class = c(
          "tempest_expert_runtime_resolution",
          "tempest_runtime_resolution",
          "list"
        )
      )
    },

    attach = function(chat, resolution, context = list()) {
      if (!inherits(resolution, "tempest_runtime_resolution")) {
        tempest_runtime_abort(
          "{.arg resolution} must come from this runtime."
        )
      }
      tempest_register_capabilities(
        chat,
        resolution$capabilities,
        context = context
      )
    }
  )
)

#' Create a Tempest runtime
#'
#' `r lifecycle::badge("experimental")`
#'
#' This experimental API is frozen and scheduled for removal in Tempest 0.2.0.
#' No compatibility shim is planned; see
#' [tempest-generic-kernel-retirement].
#'
#' A runtime binds durable workflow definitions to process-local operations,
#' skills, capabilities, and authenticated connections. Runtime factories and
#' clients are deliberately excluded from snapshots.
#'
#' @param operations A [tempest_operation_registry()].
#' @param skill_specs List of [tempest_skill()] specifications.
#' @param capability_specs List of [tempest_capability_spec()] specifications.
#' @param capability_implementations Named runtime capability factories.
#' @param connection_refs List of [tempest_connection_ref()] specifications.
#' @param connection_bindings Named runtime connection factories or clients.
#' @param include_builtins Whether to include Tempest's narrow web, evidence,
#'   semantic-retrieval, and expert-delegation capabilities.
#' @return A mutable `TempestRuntime` with role and expert resolution methods.
#' @examples
#' runtime <- tempest_runtime()
#' runtime$capabilities$list()
#' @export
#' @noRd
tempest_runtime <- function(
  operations = tempest_operation_registry(),
  skill_specs = list(),
  capability_specs = list(),
  capability_implementations = list(),
  connection_refs = list(),
  connection_bindings = list(),
  include_builtins = TRUE
) {
  include_builtins <- tempest_workflow_flag(
    include_builtins,
    "include_builtins"
  )
  connection_provider <- tempest_connection_provider(
    connections = connection_refs,
    bindings = connection_bindings
  )
  specifications <- tempest_named_contracts(
    capability_specs,
    "capability_id",
    "capability_specs"
  )
  implementations <- capability_implementations
  if (
    !is.list(implementations) ||
      (length(implementations) > 0L &&
        (is.null(names(implementations)) ||
          any(!nzchar(names(implementations)))))
  ) {
    tempest_runtime_abort(
      "{.arg capability_implementations} must be named by capability id."
    )
  }
  if (include_builtins) {
    builtins <- tempest_named_contracts(
      tempest_builtin_capability_specs(),
      "capability_id",
      "built-in capabilities"
    )
    specifications <- utils::modifyList(builtins, specifications)
    implementations <- utils::modifyList(
      tempest_builtin_capability_implementations(),
      implementations
    )
  }
  resolved_implementations <- list()
  for (capability_id in names(specifications)) {
    specification <- specifications[[capability_id]]
    operation_id <- specification@operation_id
    operation_version <- specification@operation_version
    if (
      !operations$has(
        operation_id,
        version = operation_version,
        kind = "capability"
      )
    ) {
      if (operations$has(operation_id)) {
        tryCatch(
          operations$describe(
            operation_id,
            version = operation_version,
            kind = "capability"
          ),
          error = function(error) {
            tempest_runtime_abort(
              "Capability {.val {capability_id}} has an incompatible runtime operation.",
              parent = error
            )
          }
        )
      }
      implementation <- implementations[[capability_id]] %||% NULL
      if (!is.function(implementation) && is.list(implementation)) {
        implementation <- implementation$factory %||% NULL
      }
      if (!is.function(implementation)) {
        tempest_runtime_abort(c(
          "Capability {.val {capability_id}} has no runtime operation.",
          x = "Missing operation {.val {operation_id}} version {.val {operation_version}}."
        ))
      }
      operations$register(
        operation_id,
        implementation,
        version = operation_version,
        kind = "capability"
      )
    }
    operation <- operations$resolve(
      operation_id,
      version = operation_version,
      kind = "capability"
    )
    descriptor <- implementations[[capability_id]] %||% list()
    authorize <- if (is.list(descriptor) && !is.data.frame(descriptor)) {
      descriptor$authorize %||% NULL
    } else {
      NULL
    }
    resolved_implementations[[capability_id]] <- list(
      factory = operation,
      authorize = authorize
    )
  }
  capability_resolver <- tempest_capability_resolver(
    specifications = unname(specifications),
    implementations = resolved_implementations,
    connection_provider = connection_provider
  )
  skill_registry <- tempest_skill_registry(
    skills = skill_specs,
    operations = operations
  )
  TempestRuntime$new(
    operations = operations,
    skills = skill_registry,
    capabilities = capability_resolver,
    connections = connection_provider
  )
}

# Expert Subagent Pattern (inspired by btw)

tempest_expert_session_abort <- function(message, ..., parent = NULL) {
  tempest_abort(
    message,
    ...,
    class = c("tempest_expert_session_error", "tempest_error"),
    parent = parent,
    .envir = rlang::caller_env()
  )
}

tempest_expert_session_connection_ids <- function(value, field) {
  ids <- if (is.character(value) && is.null(names(value))) {
    value
  } else if (
    is.list(value) &&
      !is.data.frame(value) &&
      is.null(names(value))
  ) {
    if (length(value) == 0L) {
      character()
    } else {
      valid <- vapply(
        value,
        \(item) rlang::is_string(item) && !is.na(item),
        logical(1)
      )
      if (!all(valid)) {
        tempest_expert_session_abort(
          "Saved {.field {field}} must be a flat string array."
        )
      }
      unlist(value, use.names = FALSE)
    }
  } else {
    tempest_expert_session_abort(
      "Saved {.field {field}} must be a flat string array."
    )
  }
  if (
    anyNA(ids) ||
      any(!nzchar(ids)) ||
      !identical(ids, tempest_trim(ids)) ||
      anyDuplicated(ids)
  ) {
    tempest_expert_session_abort(
      "Saved {.field {field}} contains invalid or duplicate identifiers."
    )
  }
  unname(ids)
}

tempest_expert_session_grants <- function(grants) {
  grants <- tryCatch(
    tempest_contract_serializable_list(grants %||% list(), "binding$grants"),
    error = function(error) {
      tempest_expert_session_abort(
        "Saved capability grants must be canonical non-secret audit records.",
        parent = error
      )
    }
  )
  if (length(grants) == 0L) {
    if (!is.null(names(grants))) {
      names(grants) <- NULL
    }
    return(grants)
  }
  grant_ids <- names(grants)
  if (
    is.null(grant_ids) ||
      anyNA(grant_ids) ||
      any(!nzchar(grant_ids)) ||
      anyDuplicated(grant_ids)
  ) {
    tempest_expert_session_abort(
      "Saved capability grants must be uniquely named by capability id."
    )
  }
  fields <- c(
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
  )
  for (index in seq_along(grants)) {
    grant <- grants[[index]]
    grant_fields <- names(grant)
    if (
      !is.list(grant) ||
        is.data.frame(grant) ||
        is.null(grant_fields) ||
        anyNA(grant_fields) ||
        anyDuplicated(grant_fields) ||
        !setequal(grant_fields, fields)
    ) {
      tempest_expert_session_abort(
        "Saved capability-grant records do not match the current schema."
      )
    }
    capability_id <- tryCatch(
      tempest_contract_id(grant$capability_id, "capability_id"),
      error = function(error) {
        tempest_expert_session_abort(
          "Saved capability grant has an invalid capability id.",
          parent = error
        )
      }
    )
    if (!identical(grant_ids[[index]], capability_id)) {
      tempest_expert_session_abort(
        "Saved capability-grant names must match their capability ids."
      )
    }
    for (field in c(
      "capability_version",
      "operation_id",
      "operation_version",
      "reason_code",
      "reason"
    )) {
      value <- grant[[field]]
      if (!is.null(value) && (!rlang::is_string(value) || is.na(value))) {
        tempest_expert_session_abort(
          "Saved capability grant has an invalid {.field {field}}."
        )
      }
    }
    if (
      !is.logical(grant$required) ||
        length(grant$required) != 1L ||
        is.na(grant$required) ||
        !rlang::is_string(grant$status) ||
        !grant$status %in% c("granted", "denied")
    ) {
      tempest_expert_session_abort(
        "Saved capability grant has an invalid requirement or status."
      )
    }
    tryCatch(
      tempest_contract_ids(
        tempest_expert_session_connection_ids(
          grant$connection_ref_ids,
          "connection_ref_ids"
        ),
        "connection_ref_ids"
      ),
      error = function(error) {
        tempest_expert_session_abort(
          "Saved capability grant has invalid connection ids.",
          parent = error
        )
      }
    )
    tryCatch(
      tempest_contract_serializable_list(grant$metadata, "grant$metadata"),
      error = function(error) {
        tempest_expert_session_abort(
          "Saved capability-grant metadata is invalid.",
          parent = error
        )
      }
    )
  }
  grants
}

tempest_expert_connection_grants <- function(
  experts,
  allowed_connection_ref_ids,
  runtime
) {
  allowed_connection_ref_ids <- allowed_connection_ref_ids %||% list()
  if (
    !is.list(allowed_connection_ref_ids) ||
      is.data.frame(allowed_connection_ref_ids)
  ) {
    tempest_expert_session_abort(
      "{.arg allowed_connection_ref_ids} must be a named list."
    )
  }
  expert_ids <- purrr::map_chr(experts, \(expert) expert@expert_id)
  if (length(allowed_connection_ref_ids) > 0L) {
    grant_ids <- names(allowed_connection_ref_ids)
    if (
      is.null(grant_ids) ||
        anyNA(grant_ids) ||
        any(!nzchar(grant_ids)) ||
        anyDuplicated(grant_ids)
    ) {
      tempest_expert_session_abort(
        "{.arg allowed_connection_ref_ids} must be named by expert id."
      )
    }
    unknown <- setdiff(grant_ids, expert_ids)
    if (length(unknown) > 0L) {
      tempest_expert_session_abort(
        "Connection grants identify unknown expert {.val {unknown[[1]]}}."
      )
    }
  }
  grants <- stats::setNames(
    lapply(expert_ids, function(expert_id) {
      connection_ids <- tempest_contract_ids(
        allowed_connection_ref_ids[[expert_id]] %||% character(),
        paste0("allowed_connection_ref_ids$", expert_id)
      )
      unavailable <- connection_ids[
        !vapply(
          connection_ids,
          runtime$connections$has,
          logical(1)
        )
      ]
      if (length(unavailable) > 0L) {
        tempest_expert_session_abort(c(
          "Expert {.val {expert_id}} has an unavailable connection grant.",
          x = "Connection {.val {unavailable[[1]]}} is not registered."
        ))
      }
      connection_ids
    }),
    expert_ids
  )
  grants
}

tempest_expert_system_prompt <- function(expert, resolution) {
  parts <- c(
    tempest_render_expert_prompt(expert, expert_id = expert@expert_id),
    paste0("Expert instructions:\n", expert@instructions)
  )
  if (nzchar(resolution$instructions %||% "")) {
    parts <- c(
      parts,
      paste0("Assigned skill instructions:\n", resolution$instructions)
    )
  }
  paste(parts, collapse = "\n\n")
}

#' Expert Session Manager
#'
#' Manages capability-scoped chats for validated expert profiles.
#'
#' @field sessions Environment storing active chat sessions keyed by session ID.
#' @field session_profiles Environment storing serializable session bindings.
#' @field config A `TempestConfig` object for creating chats.
#' @field retriever A `TempestRetriever` for registering tools.
#' @field runtime A `TempestRuntime` used to resolve skills and capabilities.
#' @field experts Environment of expert profiles keyed by stable expert id.
#' @field expert_connection_ref_ids Environment of allowed connection ids by
#'   expert.
#' @field extractor Chat object for fact extraction (optional).
#' @field extract_claims_program ProgramSet-bound claim-extraction execution.
#' @field workspace A [ResearchWorkspace] for extracted facts (optional).
#' @field progress Optional progress callback.
#' @field run_id Shared Co-STORM session id for progress events.
#' @field session_provenance Environments keyed by expert session id for
#'   claim-write provenance.
#'
#' @keywords internal
ExpertSessionManager <- R6::R6Class(
  "ExpertSessionManager",
  public = list(
    sessions = NULL,
    session_profiles = NULL,
    config = NULL,
    retriever = NULL,
    runtime = NULL,
    experts = NULL,
    expert_connection_ref_ids = NULL,
    extractor = NULL,
    extract_claims_program = NULL,
    workspace = NULL,
    progress = NULL,
    run_id = NULL,
    session_provenance = NULL,

    #' @description
    #' Create a new ExpertSessionManager.
    #' @param experts Validated `tempest_expert` profiles.
    #' @param runtime A `TempestRuntime`.
    #' @param config A `TempestConfig` object.
    #' @param retriever A `TempestRetriever` object.
    #' @param allowed_connection_ref_ids Named list of allowed connection ids by
    #'   expert id.
    #' @param extractor Optional chat object for fact extraction.
    #' @param extract_claims_program ProgramSet-bound claim-extraction
    #'   execution. Required when `extractor` is supplied.
    #' @param workspace Optional [ResearchWorkspace] for extracted facts.
    #' @param progress Optional progress callback.
    #' @param run_id Shared Co-STORM session id for progress events.
    #' @param stage_recorder Optional callback accepting a stage record and its
    #'   evaluated output.
    #' @param manifest Research manifest that owns Deputy execution identity.
    #' @param on_start Callback accepting one pending Deputy run record.
    #' @param on_run Callback accepting one terminal Deputy run trace.
    initialize = function(
      experts,
      runtime,
      config,
      retriever,
      allowed_connection_ref_ids = list(),
      extractor = NULL,
      extract_claims_program = NULL,
      workspace = NULL,
      progress = NULL,
      run_id = NULL,
      stage_recorder = NULL,
      manifest = NULL,
      on_start = function(pending_run) invisible(pending_run),
      on_run = function(trace) invisible(trace)
    ) {
      experts <- tryCatch(
        tempest_validate_experts(experts, active_only = FALSE),
        error = function(error) {
          tempest_expert_session_abort(
            "{.arg experts} must contain validated expert profiles.",
            parent = error
          )
        }
      )
      if (!inherits(runtime, "TempestRuntime")) {
        tempest_expert_session_abort(
          "{.arg runtime} must be created by {.fn tempest_runtime}."
        )
      }
      if (!S7::S7_inherits(config, TempestConfig)) {
        tempest_expert_session_abort(
          "{.arg config} must be created by {.fn tempest_config}."
        )
      }
      if (!inherits(retriever, "TempestRetriever")) {
        tempest_expert_session_abort(
          "{.arg retriever} must be a {.cls TempestRetriever}."
        )
      }
      self$sessions <- new.env(parent = emptyenv())
      self$session_profiles <- new.env(parent = emptyenv())
      self$session_provenance <- new.env(parent = emptyenv())
      self$experts <- new.env(parent = emptyenv())
      self$expert_connection_ref_ids <- new.env(parent = emptyenv())
      grants <- tempest_expert_connection_grants(
        experts,
        allowed_connection_ref_ids,
        runtime
      )
      for (expert in experts) {
        expert_id <- expert@expert_id
        assign(expert_id, expert, envir = self$experts)
        assign(
          expert_id,
          grants[[expert_id]],
          envir = self$expert_connection_ref_ids
        )
      }
      self$runtime <- runtime
      self$config <- config
      self$retriever <- retriever
      self$extractor <- extractor
      if (!is.null(extractor) || !is.null(extract_claims_program)) {
        extract_claims_program <- tempest_dsprrr_execution_require(
          extract_claims_program,
          "fact extraction"
        )
      }
      self$extract_claims_program <- extract_claims_program
      workspace <- workspace %||% retriever$workspace
      if (!inherits(workspace, "ResearchWorkspace")) {
        tempest_expert_session_abort(
          "{.arg workspace} must be a ResearchWorkspace or `NULL`."
        )
      }
      self$workspace <- workspace
      self$progress <- tempest_progress_callback(progress)
      self$run_id <- run_id %||%
        if (S7::S7_inherits(manifest, TempestResearchManifest)) {
          manifest@research_run_id
        } else {
          tempest_uuid("session")
        }
      if (!is.null(stage_recorder) && !is.function(stage_recorder)) {
        tempest_expert_session_abort(
          "{.arg stage_recorder} must be a function or {.code NULL}."
        )
      }
      private$stage_recorder_value <- stage_recorder
      private$manifest_value <- if (is.null(manifest)) {
        tempest_research_manifest(
          research_run_id = self$run_id,
          mode = "costorm",
          config = self$config,
          knowledge_snapshot = tempest_costorm_manifest_snapshot_reference(
            self$workspace
          ),
          runtime = list(),
          traces = list(),
          deliverables = list(),
          status = "running"
        )
      } else {
        tempest_costorm_manifest_validate(
          manifest,
          self$run_id,
          self$config,
          self$workspace
        )
      }
      if (!is.function(on_start) || !is.function(on_run)) {
        tempest_expert_session_abort(
          "{.arg on_start} and {.arg on_run} must be functions."
        )
      }
      private$on_start_value <- on_start
      private$on_run_value <- on_run
      invisible(self)
    },

    #' @description
    #' Emit a Co-STORM expert progress event.
    #' @param event_type Progress event type.
    #' @param status Progress event status.
    #' @param stage Optional workflow stage.
    #' @param step Optional workflow step.
    #' @param message Optional progress message.
    #' @param payload Optional progress metadata.
    #' @param parent_event_id Optional parent event id.
    #' @param correlation_id Optional correlation id.
    emit_progress = function(
      event_type,
      status,
      stage = NA_character_,
      step = NA_character_,
      message = NA_character_,
      payload = list(),
      parent_event_id = NA_character_,
      correlation_id = NA_character_
    ) {
      tempest_emit_progress(
        self$progress,
        run_id = self$run_id,
        workflow = "costorm",
        event_type = event_type,
        status = status,
        stage = stage,
        step = step,
        message = message,
        payload = payload,
        parent_event_id = parent_event_id,
        correlation_id = correlation_id
      )
    },

    #' @description
    #' Extract facts from an expert response.
    #' @param response Character string response from expert.
    #' @param turn Optional ellmer turn to inspect for provider-native sources.
    #' @param source_ids Optional source ids already harvested for the turn.
    #' @param session_id Optional manager-owned expert session id. This is
    #'   delegation metadata only; extracted claims use the manager's exact
    #'   research run id.
    #' @param expert_id Optional stable expert id.
    #' @param correlation_id Optional progress correlation id for the turn.
    #' @param deputy_execution Optional terminal Deputy trace for the answer.
    #' @return Invisibly returns NULL.
    extract_facts = function(
      response,
      turn = NULL,
      source_ids = NULL,
      session_id = NA_character_,
      expert_id = NA_character_,
      correlation_id = NA_character_,
      deputy_execution = NULL
    ) {
      if (!is.null(deputy_execution)) {
        deputy_execution <- tempest_costorm_deputy_trace(deputy_execution)
      }
      if (!is.null(self$extractor) && !is.null(self$workspace)) {
        event <- self$emit_progress(
          "step",
          "started",
          stage = "evidence",
          step = "fact_extraction",
          correlation_id = correlation_id
        )
        tryCatch(
          {
            # Only re-harvest when the caller did not already do so; callers that
            # pass source_ids have harvested the turn into the store already.
            harvested <- if (is.null(source_ids)) {
              tempest_harvest_native_sources_from_turn(turn, self$workspace)
            } else {
              character()
            }
            source_ids <- tempest_session_answer_source_ids(
              list(workspace = self$workspace),
              response,
              unique(c(source_ids, harvested))
            )
            if (length(source_ids) == 0L) {
              self$emit_progress(
                "step",
                "skipped",
                stage = "evidence",
                step = "fact_extraction",
                parent_event_id = event@event_id,
                correlation_id = event@correlation_id,
                payload = list(reason = "no_cited_sources")
              )
              return(invisible(FALSE))
            }
            tempest_extract_facts_from_answer(
              self$extractor,
              response,
              self$workspace,
              module = self$extract_claims_program,
              source_ids = source_ids,
              session_id = self$run_id,
              expert_id = expert_id,
              retrieval_step_id = correlation_id,
              deputy_run_id = deputy_execution$deputy_run_id %||%
                NA_character_,
              deputy_session_id = deputy_execution$deputy_session_id %||%
                NA_character_,
              parent_run_id = deputy_execution$parent_run_id %||%
                NA_character_,
              delegation_id = deputy_execution$delegation_id %||%
                NA_character_,
              tool_call_id = deputy_execution$tool_call_id %||%
                NA_character_,
              record_stage = private$stage_recorder_value
            )
            self$emit_progress(
              "step",
              "succeeded",
              stage = "evidence",
              step = "fact_extraction",
              parent_event_id = event@event_id,
              correlation_id = event@correlation_id,
              payload = list(
                claim_count = length(self$workspace$list_proposed_claims())
              )
            )
          },
          error = function(e) {
            self$emit_progress(
              "step",
              "failed",
              stage = "evidence",
              step = "fact_extraction",
              parent_event_id = event@event_id,
              correlation_id = event@correlation_id,
              payload = tempest_progress_error_payload(e)
            )
            tempest_rethrow_operation(
              e,
              class = "tempest_expert_session_error"
            )
          }
        )
      }
      invisible(NULL)
    },

    #' @description
    #' Add an active expert profile to the live roster.
    #' @param expert A validated `tempest_expert`.
    #' @param allowed_connection_ref_ids Connection ids granted to this expert.
    #' @param replace Whether to replace an existing profile with the same id.
    #' @return The stable expert id, invisibly.
    add_expert = function(
      expert,
      allowed_connection_ref_ids = character(),
      replace = FALSE
    ) {
      tryCatch(
        tempest_validate_experts(list(expert), active_only = TRUE),
        error = function(error) {
          tempest_expert_session_abort(
            "{.arg expert} must be an active validated expert profile.",
            parent = error
          )
        }
      )
      replace <- tempest_product_flag(replace, "replace")
      expert_id <- expert@expert_id
      present <- exists(expert_id, envir = self$experts, inherits = FALSE)
      if (present && !replace) {
        tempest_expert_session_abort(c(
          "Expert {.val {expert_id}} is already in the live roster.",
          i = "Set {.arg replace} to `TRUE` to replace it explicitly."
        ))
      }
      grant <- tempest_expert_connection_grants(
        list(expert),
        stats::setNames(
          list(allowed_connection_ref_ids),
          expert_id
        ),
        self$runtime
      )[[expert_id]]
      if (present) {
        private$retire_expert_sessions(expert_id)
      }
      assign(expert_id, expert, envir = self$experts)
      assign(
        expert_id,
        grant,
        envir = self$expert_connection_ref_ids
      )
      invisible(expert_id)
    },

    #' @description
    #' Retire an expert and all chats bound to that profile.
    #' @param expert_id Stable expert id.
    #' @return Whether the expert was present.
    retire_expert = function(expert_id) {
      expert_id <- private$expert_id(expert_id)
      if (!exists(expert_id, envir = self$experts, inherits = FALSE)) {
        return(FALSE)
      }
      expert <- get(expert_id, envir = self$experts, inherits = FALSE)
      if (!identical(expert@state, "retired")) {
        expert <- tempest_expert_update(expert, state = "retired")
        assign(expert_id, expert, envir = self$experts)
      }
      private$retire_expert_sessions(expert_id)
      TRUE
    },

    #' @description
    #' Look up an expert by exact stable id.
    #' @param expert_id Stable expert id.
    #' @param active_only Whether retired profiles should be rejected.
    #' @return A validated expert profile.
    profile = function(expert_id, active_only = TRUE) {
      expert_id <- private$expert_id(expert_id)
      active_only <- tempest_product_flag(active_only, "active_only")
      if (!exists(expert_id, envir = self$experts, inherits = FALSE)) {
        tempest_expert_session_abort(
          "Expert {.val {expert_id}} is not in the live roster."
        )
      }
      expert <- get(expert_id, envir = self$experts, inherits = FALSE)
      if (active_only && !identical(expert@state, "active")) {
        tempest_expert_session_abort(
          "Expert {.val {expert_id}} is retired and cannot run."
        )
      }
      expert
    },

    #' @description
    #' List expert profiles in stable-id order.
    #' @param active_only Whether to omit retired profiles.
    #' @return A list of validated expert profiles.
    list_experts = function(active_only = TRUE) {
      active_only <- tempest_product_flag(active_only, "active_only")
      expert_ids <- sort(ls(self$experts, all.names = TRUE))
      experts <- lapply(
        expert_ids,
        \(expert_id) get(expert_id, envir = self$experts, inherits = FALSE)
      )
      if (active_only) {
        experts <- Filter(
          \(expert) identical(expert@state, "active"),
          experts
        )
      }
      unname(experts)
    },

    #' @description
    #' Get an expert's existing session or create a scoped chat.
    #' @param expert_id Stable expert id or matching expert profile.
    #' @param session_id Optional existing, manager-owned session id to resume.
    #' @return Chat, session binding, grants, provenance, and creation status.
    get_or_create = function(expert_id, session_id = NULL) {
      expert <- private$resolve_expert(expert_id)
      if (is.null(session_id)) {
        session_id <- private$session_for_expert(expert@expert_id)
        if (!is.null(session_id)) {
          return(private$resume(expert, session_id))
        }
        return(private$create(expert))
      }
      session_id <- private$session_id(session_id)
      if (!exists(session_id, envir = self$sessions, inherits = FALSE)) {
        tempest_expert_session_abort(c(
          "Expert session {.val {session_id}} is not active.",
          i = "Only manager-owned session ids can be resumed."
        ))
      }
      private$resume(expert, session_id)
    },

    #' @description
    #' Restore a saved session binding through fresh runtime authorization.
    #' @param binding Serializable session profile containing the opaque
    #'   session id and exact expert identity fields.
    #' @return The same result shape as `get_or_create()`.
    restore_session = function(binding) {
      binding <- tryCatch(
        tempest_contract_serializable_list(binding, "binding"),
        error = function(error) {
          tempest_expert_session_abort(
            "{.arg binding} must be a serializable session profile.",
            parent = error
          )
        }
      )
      required_fields <- c(
        "session_id",
        "expert_id",
        "expert_version",
        "expert_fingerprint",
        "model_role",
        "allowed_connection_ref_ids",
        "grants",
        "created_at"
      )
      binding_fields <- names(binding)
      if (
        is.null(binding_fields) ||
          anyNA(binding_fields) ||
          anyDuplicated(binding_fields) ||
          !setequal(binding_fields, required_fields)
      ) {
        tempest_expert_session_abort(
          "Session binding does not match the current eight-field schema."
        )
      }
      session_id <- private$session_id(binding$session_id)
      if (!grepl("^expert-session_[a-f0-9]{16}$", session_id)) {
        tempest_expert_session_abort(
          "Restored session ids must be opaque Tempest expert-session ids."
        )
      }
      if (
        exists(session_id, envir = self$sessions, inherits = FALSE) ||
          exists(
            session_id,
            envir = self$session_profiles,
            inherits = FALSE
          )
      ) {
        tempest_expert_session_abort(
          "Expert session {.val {session_id}} is already active."
        )
      }
      expert <- self$profile(binding$expert_id)
      expert_version <- tryCatch(
        tempest_product_version(
          binding$expert_version,
          "expert_version"
        ),
        error = function(error) {
          tempest_expert_session_abort(
            "Session binding has an invalid expert version.",
            parent = error
          )
        }
      )
      fingerprint <- binding$expert_fingerprint
      if (
        !rlang::is_string(fingerprint) ||
          !grepl("^[a-f0-9]{64}$", fingerprint)
      ) {
        tempest_expert_session_abort(
          "Session binding has an invalid expert fingerprint."
        )
      }
      current_fingerprint <- tempest_expert_profile_fingerprint(expert)
      if (
        !identical(expert_version, expert@version) ||
          !identical(fingerprint, current_fingerprint)
      ) {
        tempest_expert_session_abort(c(
          "Expert session {.val {session_id}} cannot be restored.",
          x = paste0(
            "The saved expert version or fingerprint does not match the ",
            "live profile."
          )
        ))
      }
      model_role <- tryCatch(
        tempest_contract_id(binding$model_role, "binding$model_role"),
        error = function(error) {
          tempest_expert_session_abort(
            "Session binding has an invalid model role.",
            parent = error
          )
        }
      )
      allowed_connection_ref_ids <- tryCatch(
        tempest_contract_ids(
          tempest_expert_session_connection_ids(
            binding$allowed_connection_ref_ids,
            "allowed_connection_ref_ids"
          ),
          "binding$allowed_connection_ref_ids"
        ),
        error = function(error) {
          tempest_expert_session_abort(
            "Session binding has invalid allowed connection ids.",
            parent = error
          )
        }
      )
      created_at <- binding$created_at
      if (
        !rlang::is_string(created_at) ||
          is.na(created_at) ||
          !nzchar(tempest_trim(created_at))
      ) {
        tempest_expert_session_abort(
          "Session binding has an invalid creation timestamp."
        )
      }
      parsed_created_at <- suppressWarnings(as.POSIXct(created_at, tz = "UTC"))
      if (is.na(parsed_created_at)) {
        tempest_expert_session_abort(
          "Session binding has an invalid creation timestamp."
        )
      }
      prior_grants <- tempest_expert_session_grants(binding$grants)
      private$create(
        expert,
        session_id = session_id,
        prior_grants = prior_grants
      )
      restored <- self$session_profile(session_id)
      if (
        !identical(restored$model_role, model_role) ||
          !identical(
            unname(restored$allowed_connection_ref_ids),
            unname(allowed_connection_ref_ids)
          )
      ) {
        self$retire_session(session_id)
        tempest_expert_session_abort(
          paste0(
            "Expert session {.val {session_id}} cannot be restored because ",
            "its live authorization differs from the saved binding."
          )
        )
      }
      restored$created_at <- created_at
      assign(session_id, restored, envir = self$session_profiles)
      private$result(session_id, is_new = TRUE)
    },

    #' @description
    #' Return the serializable binding for an active session.
    #' @param session_id Manager-owned expert session id.
    #' @return Session binding including expert fingerprint and grants.
    session_profile = function(session_id) {
      session_id <- private$session_id(session_id)
      if (
        !exists(
          session_id,
          envir = self$session_profiles,
          inherits = FALSE
        )
      ) {
        tempest_expert_session_abort(
          "Expert session {.val {session_id}} is not active."
        )
      }
      get(
        session_id,
        envir = self$session_profiles,
        inherits = FALSE
      )
    },

    #' @description
    #' List all active session IDs.
    #' @return Character vector of session IDs.
    list_sessions = function() {
      sort(ls(envir = self$sessions, all.names = TRUE))
    },

    #' @description
    #' Retire a stateful expert chat so it cannot be reused after timeout or
    #' cancellation.
    #' @param session_id Session id returned by `get_or_create()`.
    #' @return A list describing whether the session existed and whether a
    #'   provider cancellation method was available.
    retire_session = function(session_id) {
      if (is.null(session_id)) {
        return(list(retired = FALSE, cancellation_supported = FALSE))
      }
      session_id <- private$session_id(session_id)
      if (!exists(session_id, envir = self$sessions, inherits = FALSE)) {
        return(list(retired = FALSE, cancellation_supported = FALSE))
      }
      chat <- get(session_id, envir = self$sessions, inherits = FALSE)
      cancel <- tryCatch(
        chat$cancel %||% chat$stop %||% NULL,
        error = \(error) NULL
      )
      cancellation_supported <- is.function(cancel)
      if (cancellation_supported) {
        try(cancel(), silent = TRUE)
      }
      rm(list = session_id, envir = self$sessions)
      for (records in list(
        self$session_profiles,
        self$session_provenance
      )) {
        if (exists(session_id, envir = records, inherits = FALSE)) {
          rm(list = session_id, envir = records)
        }
      }
      list(
        retired = TRUE,
        cancellation_supported = cancellation_supported
      )
    }
  ),
  private = list(
    stage_recorder_value = NULL,
    manifest_value = NULL,
    on_start_value = NULL,
    on_run_value = NULL,
    expert_id = function(expert_id) {
      tryCatch(
        tempest_contract_id(expert_id, "expert_id"),
        error = function(error) {
          tempest_expert_session_abort(
            "{.arg expert_id} must be a valid stable expert id.",
            parent = error
          )
        }
      )
    },

    session_id = function(session_id) {
      tryCatch(
        tempest_product_scalar(session_id, "session_id"),
        error = function(error) {
          tempest_expert_session_abort(
            "{.arg session_id} must be a manager-owned session id.",
            parent = error
          )
        }
      )
    },

    resolve_expert = function(expert_or_id) {
      if (S7::S7_inherits(expert_or_id, TempestExpertProfile)) {
        tryCatch(
          tempest_validate_experts(list(expert_or_id), active_only = TRUE),
          error = function(error) {
            tempest_expert_session_abort(
              "{.arg expert_id} identifies an invalid expert profile.",
              parent = error
            )
          }
        )
        current <- self$profile(expert_or_id@expert_id)
        matches <- identical(current@version, expert_or_id@version) &&
          identical(
            tempest_expert_profile_fingerprint(current),
            tempest_expert_profile_fingerprint(expert_or_id)
          )
        if (!matches) {
          tempest_expert_session_abort(
            paste0(
              "Expert {.val {expert_or_id@expert_id}} does not match the ",
              "profile in the live roster."
            )
          )
        }
        return(current)
      }
      self$profile(expert_or_id)
    },

    session_for_expert = function(expert_id) {
      session_ids <- sort(ls(self$session_profiles, all.names = TRUE))
      for (session_id in session_ids) {
        binding <- get(
          session_id,
          envir = self$session_profiles,
          inherits = FALSE
        )
        if (identical(binding$expert_id, expert_id)) {
          return(session_id)
        }
      }
      NULL
    },

    result = function(session_id, is_new) {
      list(
        chat = get(session_id, envir = self$sessions, inherits = FALSE),
        session_id = session_id,
        is_new = is_new,
        provenance = get(
          session_id,
          envir = self$session_provenance,
          inherits = FALSE
        ),
        profile = get(
          session_id,
          envir = self$session_profiles,
          inherits = FALSE
        ),
        grants = get(
          session_id,
          envir = self$session_profiles,
          inherits = FALSE
        )$grants
      )
    },

    create = function(
      expert,
      session_id = NULL,
      prior_grants = list()
    ) {
      supplied_session_id <- !is.null(session_id)
      session_id <- session_id %||% tempest_uuid("expert-session")
      while (
        exists(session_id, envir = self$sessions, inherits = FALSE) ||
          exists(
            session_id,
            envir = self$session_profiles,
            inherits = FALSE
          )
      ) {
        if (supplied_session_id) {
          tempest_expert_session_abort(
            "Expert session {.val {session_id}} is already active."
          )
        }
        session_id <- tempest_uuid("expert-session")
      }
      provenance <- new.env(parent = emptyenv())
      provenance$base <- list(
        session_id = self$run_id,
        expert_id = expert@expert_id
      )
      provenance$current <- list()
      model_role <- expert@model_role
      if (is.na(model_role)) {
        tempest_expert_session_abort(
          paste0(
            "Expert {.val {expert@expert_id}} requires a host model-policy ",
            "resolver before chat creation."
          )
        )
      }
      model <- tempest_research_model(self$config, model_role)
      context <- list(
        retriever = self$retriever,
        expert = expert,
        expert_id = expert@expert_id,
        model = model,
        search_provider = self$config@search_provider,
        claim_provenance = function() {
          utils::modifyList(
            provenance$base,
            provenance$current %||% list()
          )
        }
      )
      allowed_connection_ref_ids <- get(
        expert@expert_id,
        envir = self$expert_connection_ref_ids,
        inherits = FALSE
      )
      resolution <- tryCatch(
        self$runtime$resolve_expert(
          expert,
          allowed_connection_ref_ids = allowed_connection_ref_ids,
          context = context
        ),
        error = function(error) {
          tempest_expert_session_abort(
            "Expert {.val {expert@expert_id}} could not be resolved."
          )
        }
      )
      system_prompt <- tempest_expert_system_prompt(expert, resolution)
      chat <- tempest_make_chat(
        self$config,
        model_role,
        system_prompt = system_prompt,
        echo = "none"
      )
      tryCatch(
        self$runtime$attach(chat, resolution, context = context),
        error = function(error) {
          tempest_expert_session_abort(
            "Expert {.val {expert@expert_id}} tools could not be attached."
          )
        }
      )
      chat <- tryCatch(
        tempest_deputy_chat_adapter(
          chat,
          manifest = private$manifest_value,
          deputy_session_id = session_id,
          agent_name = expert@name,
          stage = "dialogue",
          role = "expert",
          expert_id = expert@expert_id,
          on_start = private$on_start_value,
          on_run = private$on_run_value
        ),
        error = function(error) {
          tempest_expert_session_abort(
            "Expert {.val {expert@expert_id}} execution session could not be created."
          )
        }
      )
      binding <- list(
        session_id = session_id,
        expert_id = expert@expert_id,
        expert_version = expert@version,
        expert_fingerprint = resolution$expert_fingerprint,
        model_role = model_role,
        allowed_connection_ref_ids = allowed_connection_ref_ids,
        grants = resolution$grants,
        prior_grants = prior_grants,
        created_at = tempest_now_utc()
      )
      assign(session_id, chat, envir = self$sessions)
      assign(
        session_id,
        binding,
        envir = self$session_profiles
      )
      assign(
        session_id,
        provenance,
        envir = self$session_provenance
      )
      private$result(session_id, is_new = TRUE)
    },

    resume = function(expert, session_id) {
      binding <- self$session_profile(session_id)
      current_fingerprint <- tempest_expert_profile_fingerprint(expert)
      if (
        !identical(binding$expert_id, expert@expert_id) ||
          !identical(binding$expert_version, expert@version) ||
          !identical(binding$expert_fingerprint, current_fingerprint)
      ) {
        tempest_expert_session_abort(c(
          "Expert session {.val {session_id}} cannot be resumed.",
          x = paste0(
            "The session is bound to a different expert id, version, ",
            "or profile fingerprint."
          )
        ))
      }
      private$result(session_id, is_new = FALSE)
    },

    retire_expert_sessions = function(expert_id) {
      session_ids <- ls(self$session_profiles, all.names = TRUE)
      for (session_id in session_ids) {
        binding <- get(
          session_id,
          envir = self$session_profiles,
          inherits = FALSE
        )
        if (identical(binding$expert_id, expert_id)) {
          self$retire_session(session_id)
        }
      }
      invisible(NULL)
    }
  )
)

#' Create a scoped Tempest expert-session manager
#'
#' `r lifecycle::badge("experimental")`
#'
#' The manager owns a validated live expert roster and creates one
#' capability-scoped chat per expert. Runtime tools and authenticated
#' connections are resolved before chat creation and are never inferred from
#' display names.
#'
#' @param experts List of [tempest_expert()] profiles.
#' @param runtime A [tempest_runtime()].
#' @param config A [tempest_config()].
#' @param retriever A `TempestRetriever`.
#' @param allowed_connection_ref_ids Named list of allowed connection ids by
#'   stable expert id.
#' @param extractor Optional fact-extraction chat.
#' @param extract_claims_program ProgramSet-bound claim-extraction execution.
#'   Required when `extractor` is supplied.
#' @param workspace Optional [ResearchWorkspace]; defaults to the retriever
#'   workspace.
#' @param progress Optional progress callback.
#' @param run_id Optional shared workflow run id.
#' @param stage_recorder Optional callback accepting a stage record and its
#'   evaluated output.
#' @param manifest Research manifest that owns Deputy execution identity.
#' @param on_run Callback accepting one terminal Deputy run trace.
#' @return An `ExpertSessionManager`.
#' @noRd
tempest_expert_session_manager <- function(
  experts,
  runtime,
  config,
  retriever,
  allowed_connection_ref_ids = list(),
  extractor = NULL,
  extract_claims_program = NULL,
  workspace = NULL,
  progress = NULL,
  run_id = NULL,
  stage_recorder = NULL,
  manifest = NULL,
  on_run = function(trace) invisible(trace)
) {
  ExpertSessionManager$new(
    experts = experts,
    runtime = runtime,
    config = config,
    retriever = retriever,
    allowed_connection_ref_ids = allowed_connection_ref_ids,
    extractor = extractor,
    extract_claims_program = extract_claims_program,
    workspace = workspace,
    progress = progress,
    run_id = run_id,
    stage_recorder = stage_recorder,
    manifest = manifest,
    on_run = on_run
  )
}

#' Create the expert delegation tool
#'
#' Creates one generic tool that resolves the manager's live roster by exact
#' stable expert id.
#'
#' @param session_manager An `ExpertSessionManager` instance.
#' @param topic The research topic (for context).
#' @param experts Optional selected expert profiles. These are validated for
#'   runtime composition, while calls resolve the manager's live roster.
#' @return An ellmer tool.
#' @keywords internal
tempest_create_expert_delegation_tool <- function(
  session_manager,
  topic,
  experts = NULL
) {
  tempest_require("ellmer", "Expert tools require ellmer.")
  if (!inherits(session_manager, "ExpertSessionManager")) {
    tempest_expert_session_abort(
      "{.arg session_manager} must be an {.cls ExpertSessionManager}."
    )
  }
  topic <- tryCatch(
    tempest_product_scalar(topic, "topic"),
    error = function(error) {
      tempest_expert_session_abort(
        "{.arg topic} must be a single non-empty string.",
        parent = error
      )
    }
  )
  if (!is.null(experts)) {
    tryCatch(
      tempest_validate_experts(experts, active_only = FALSE),
      error = function(error) {
        tempest_expert_session_abort(
          "{.arg experts} must contain validated expert profiles.",
          parent = error
        )
      }
    )
  }
  mgr <- session_manager
  roster <- mgr$list_experts()
  roster_text <- paste(
    vapply(
      roster,
      function(expert) {
        paste0(
          expert@expert_id,
          " (",
          expert@name,
          ", ",
          expert@title,
          ")"
        )
      },
      character(1)
    ),
    collapse = "; "
  )
  source_ids_in_store <- function() {
    if (!inherits(mgr$workspace, "ResearchWorkspace")) {
      return(character())
    }
    vapply(
      mgr$workspace$list_retrieved_sources(),
      \(source) source$id,
      character(1)
    )
  }
  claim_ids_in_store <- function() {
    if (!inherits(mgr$workspace, "ResearchWorkspace")) {
      return(character())
    }
    vapply(
      mgr$workspace$list_proposed_claims(),
      \(claim) claim@claim_id,
      character(1)
    )
  }

  delegate_to_expert <- function(expert_id, question) {
    expert <- mgr$profile(expert_id)
    result <- mgr$get_or_create(expert@expert_id)
    chat <- result$chat
    sid <- result$session_id
    provenance <- result$provenance
    expert_name <- expert@name
    correlation_id <- tempest_uuid("tool")
    prior_source_ids <- source_ids_in_store()
    prior_claim_ids <- claim_ids_in_store()
    tool_event <- mgr$emit_progress(
      "tool",
      "started",
      stage = "dialogue",
      step = "delegate_to_expert",
      correlation_id = correlation_id,
      payload = list(
        expert_id = expert@expert_id,
        expert_name = expert_name,
        session_id = sid
      )
    )

    prompt <- paste0(
      "Topic: ",
      topic,
      "\n\n",
      "Question: ",
      question,
      "\n\n",
      "Instructions:\n",
      "- Follow your expert profile and assigned skill instructions.\n",
      "- Use only the capabilities and connections granted to this session.\n",
      "- Start with evidence already in the shared session by using ",
      paste0(
        "list_retrieved_sources, get_retrieved_source, or retrieve when ",
        "available.\n"
      ),
      "- If shared evidence cannot answer the question, make exactly one web ",
      "search and set k = 2 when the search tool accepts k.\n",
      "- Inspect no more than two search results and make no more than two ",
      "retrieval or fetch calls in total.\n",
      "- Stop when those bounds are reached. Do not expand into an exhaustive ",
      "survey; state the remaining evidence gap instead.\n",
      "- Only state factual claims supported by sources you inspected.\n",
      "- Cite source IDs like [Sxxxxxxxxxxxx] when evidence is available.\n",
      "- Do not call add_proposed_claim; the host commits evidence after ",
      "your response.\n",
      "- If evidence is weak or unclear, say so.\n",
      "- Respond in no more than 250 words.\n\n",
      "Respond now:"
    )

    old_provenance <- provenance$current %||% list()
    provenance$current <- list(
      expert_id = expert@expert_id,
      retrieval_step_id = correlation_id
    )
    response <- tryCatch(
      chat$chat(
        prompt,
        echo = "none",
        run_context = list(
          correlation_id = correlation_id,
          role = "expert",
          stage = "dialogue"
        )
      ),
      error = function(e) {
        mgr$emit_progress(
          "tool",
          "failed",
          stage = "dialogue",
          step = "delegate_to_expert",
          parent_event_id = tool_event@event_id,
          correlation_id = correlation_id,
          payload = c(
            list(
              expert_id = expert@expert_id,
              expert_name = expert_name,
              session_id = sid
            ),
            tempest_progress_error_payload(e)
          )
        )
        tempest_rethrow_operation(
          e,
          class = "tempest_expert_session_error"
        )
      },
      finally = {
        provenance$current <- old_provenance
      }
    )
    deputy_execution <- tempest_generic_kernel_cutover_abort(
      "tempest_create_expert_delegation_tool"
    )

    last_turn <- tryCatch(chat$last_turn(), error = function(e) NULL)
    response_text <- if (
      is.null(last_turn) || length(last_turn@contents) == 0
    ) {
      if (is.character(response) && length(response) > 0) {
        paste(response, collapse = "\n")
      } else {
        "(Expert completed but returned no message.)"
      }
    } else {
      ellmer::contents_markdown(last_turn)
    }

    native_source_ids <- if (inherits(mgr$workspace, "ResearchWorkspace")) {
      tempest_harvest_native_sources_from_turn(last_turn, mgr$workspace)
    } else {
      character()
    }
    mgr$extract_facts(
      response_text,
      turn = last_turn,
      source_ids = native_source_ids,
      session_id = sid,
      expert_id = expert@expert_id,
      correlation_id = correlation_id,
      deputy_execution = deputy_execution
    )
    current_source_ids <- source_ids_in_store()
    cited_source_ids <- intersect(
      tempest_extract_citation_ids(response_text),
      current_source_ids
    )
    evidence_source_ids <- unique(c(
      native_source_ids,
      cited_source_ids,
      setdiff(current_source_ids, prior_source_ids)
    ))
    evidence_claim_ids <- setdiff(claim_ids_in_store(), prior_claim_ids)
    mgr$emit_progress(
      "tool",
      "succeeded",
      stage = "dialogue",
      step = "delegate_to_expert",
      parent_event_id = tool_event@event_id,
      correlation_id = correlation_id,
      payload = list(
        expert_id = expert@expert_id,
        expert_name = expert_name,
        session_id = sid,
        deputy_run_id = deputy_execution$deputy_run_id,
        deputy_session_id = deputy_execution$deputy_session_id
      )
    )

    list(
      expert_id = expert@expert_id,
      expert = expert_name,
      response = response_text,
      session_id = sid,
      deputy_run_id = deputy_execution$deputy_run_id,
      deputy_session_id = deputy_execution$deputy_session_id,
      source_ids = evidence_source_ids,
      claim_ids = evidence_claim_ids
    )
  }

  ellmer::tool(
    delegate_to_expert,
    name = "delegate_to_expert",
    description = paste(
      "Delegate a question to one active expert from the live roster.",
      "Use the expert's exact stable expert_id.",
      "Active experts:",
      roster_text
    ),
    arguments = list(
      expert_id = ellmer::type_string(
        "Exact stable id of an active expert in the live roster."
      ),
      question = ellmer::type_string(
        paste(
          "One narrow, answerable evidence question.",
          "Do not request an exhaustive survey or multiple deliverables."
        )
      )
    )
  )
}
