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

tempest_runtime_model <- function(config, role) {
  if (!S7::S7_inherits(config, TempestConfig)) {
    tempest_runtime_abort(
      "{.arg config} must be created by {.fn tempest_config}."
    )
  }
  role <- tempest_contract_id(role, "role")
  model <- config@models[[role]] %||% NULL
  if (!rlang::is_string(model) || !nzchar(model)) {
    tempest_runtime_abort(c(
      "No model is configured for role {.val {role}}.",
      i = "Add the role to {.arg models} in {.fn tempest_config}."
    ))
  }
  model
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
    ),
    tempest_capability_spec(
      "tempest.expert.delegate",
      purpose = "Delegate work to one active expert by stable expert id.",
      instructions = "Delegate only to active experts selected for this run.",
      operation_id = "tempest.capability.expert.delegate",
      model_roles = "coordinator"
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
    },
    "tempest.expert.delegate" = function(
      capability_spec,
      connections,
      context
    ) {
      manager <- context$expert_session_manager %||% NULL
      experts <- context$experts %||% list()
      topic <- context$topic %||% NULL
      if (
        !inherits(manager, "ExpertSessionManager") ||
          !rlang::is_string(topic) ||
          length(experts) == 0L
      ) {
        tempest_runtime_abort(
          "Expert delegation requires a manager, topic, and selected experts."
        )
      }
      list(
        tools = list(tempest_create_expert_delegation_tool(
          experts = experts,
          session_manager = manager,
          topic = topic
        )),
        registrars = list(),
        metadata = list(
          expert_ids = purrr::map_chr(
            experts,
            \(expert) expert@expert_id
          )
        )
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
