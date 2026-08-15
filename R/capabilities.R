# Runtime skill, capability, and connection resolution

tempest_skill_registry_abort <- function(message, ..., parent = NULL) {
  tempest_abort(
    message,
    ...,
    class = c("tempest_skill_registry_error", "tempest_error"),
    parent = parent,
    .envir = rlang::caller_env()
  )
}

tempest_connection_provider_abort <- function(message, ..., parent = NULL) {
  tempest_abort(
    message,
    ...,
    class = c("tempest_connection_provider_error", "tempest_error"),
    parent = parent,
    .envir = rlang::caller_env()
  )
}

tempest_capability_resolution_abort <- function(
  message,
  ...,
  parent = NULL,
  class = "tempest_capability_resolution_error"
) {
  tempest_abort(
    message,
    ...,
    class = c(class, "tempest_error"),
    parent = parent,
    .envir = rlang::caller_env()
  )
}

tempest_registry_entries <- function(value, arg) {
  value <- value %||% list()
  if (!is.list(value) || is.data.frame(value)) {
    tempest_workflow_abort("{.arg {arg}} must be a list.")
  }
  value
}

tempest_runtime_context <- function(context) {
  context <- context %||% list()
  if (!is.list(context) || is.data.frame(context)) {
    tempest_capability_resolution_abort(
      "{.arg context} must be a list."
    )
  }
  context
}

tempest_skill_operation_version <- function(skill, operation_id) {
  versions <- skill@operation_versions
  if (
    length(versions) == 0L ||
      is.null(names(versions)) ||
      !operation_id %in% names(versions)
  ) {
    return(NULL)
  }
  unname(versions[[operation_id]])
}

TempestSkillRegistry <- R6::R6Class(
  "TempestSkillRegistry",
  public = list(
    initialize = function(skills = list(), operations) {
      if (!inherits(operations, "TempestOperationRegistry")) {
        tempest_skill_registry_abort(
          paste0(
            "{.arg operations} must be a ",
            "{.cls TempestOperationRegistry}."
          )
        )
      }
      private$skills <- new.env(parent = emptyenv())
      private$operations <- operations
      self$register_many(skills)
      invisible(self)
    },

    register = function(skill, replace = FALSE) {
      private$validate_skill(skill)
      replace <- tempest_workflow_flag(replace, "replace")
      skill_id <- skill@skill_id
      if (
        exists(skill_id, envir = private$skills, inherits = FALSE) &&
          !replace
      ) {
        tempest_skill_registry_abort(c(
          "Skill {.val {skill_id}} is already registered.",
          i = "Set {.arg replace} to `TRUE` to replace it explicitly."
        ))
      }
      assign(skill_id, skill, envir = private$skills)
      invisible(skill_id)
    },

    register_many = function(skills) {
      skills <- tempest_registry_entries(skills, "skills")
      if (length(skills) == 0L) {
        return(invisible(self))
      }
      skill_names <- names(skills)
      if (!is.null(skill_names) && anyNA(skill_names)) {
        tempest_skill_registry_abort(
          "{.arg skills} cannot have missing entry names."
        )
      }
      for (i in seq_along(skills)) {
        skill <- skills[[i]]
        if (
          !is.null(skill_names) &&
            nzchar(skill_names[[i]] %||% "") &&
            S7::S7_inherits(skill, TempestSkill) &&
            !identical(skill_names[[i]], skill@skill_id)
        ) {
          tempest_skill_registry_abort(c(
            "Named skill entries must match their stable skill id.",
            x = "Entry {.val {skill_names[[i]]}} contains {.val {skill@skill_id}}."
          ))
        }
        self$register(skill)
      }
      invisible(self)
    },

    get = function(skill_id, version = NULL) {
      skill_id <- tempest_contract_id(skill_id, "skill_id")
      if (!exists(skill_id, envir = private$skills, inherits = FALSE)) {
        tempest_skill_registry_abort(
          "Skill {.val {skill_id}} is not registered."
        )
      }
      skill <- get(skill_id, envir = private$skills, inherits = FALSE)
      if (
        !is.null(version) &&
          !identical(
            skill@version,
            tempest_workflow_version(version, "version")
          )
      ) {
        tempest_skill_registry_abort(c(
          "Skill {.val {skill_id}} has an incompatible version.",
          x = "Requested {.val {version}}, registered {.val {skill@version}}."
        ))
      }
      private$validate_skill(skill)
      skill
    },

    has = function(skill_id, version = NULL) {
      tryCatch(
        {
          self$get(skill_id, version = version)
          TRUE
        },
        error = function(error) FALSE
      )
    },

    list = function() {
      skill_ids <- sort(ls(private$skills, all.names = TRUE))
      stats::setNames(
        lapply(skill_ids, function(skill_id) {
          skill <- get(skill_id, envir = private$skills, inherits = FALSE)
          tempest_skill_record(skill)
        }),
        skill_ids
      )
    },

    resolve = function(
      skill_ids,
      versions = character(),
      required_capability_ids = character(),
      optional_capability_ids = character()
    ) {
      skill_ids <- tempest_contract_ids(skill_ids, "skill_ids")
      versions <- private$validate_versions(versions, skill_ids)
      required_capability_ids <- tempest_contract_ids(
        required_capability_ids,
        "required_capability_ids"
      )
      optional_capability_ids <- tempest_contract_ids(
        optional_capability_ids,
        "optional_capability_ids"
      )
      caller_overlap <- intersect(
        required_capability_ids,
        optional_capability_ids
      )
      if (length(caller_overlap) > 0L) {
        tempest_skill_registry_abort(c(
          "Required and optional capability requests must be disjoint.",
          x = "Duplicated capability: {.val {caller_overlap[[1]]}}."
        ))
      }

      skills <- lapply(skill_ids, function(skill_id) {
        version <- if (skill_id %in% names(versions)) {
          unname(versions[[skill_id]])
        } else {
          NULL
        }
        skill <- self$get(skill_id, version = version)
        if (!identical(skill@state, "active")) {
          tempest_skill_registry_abort(
            "Skill {.val {skill_id}} is not active."
          )
        }
        skill
      })
      names(skills) <- skill_ids

      skill_required <- unlist(
        lapply(skills, \(skill) skill@required_capability_ids),
        use.names = FALSE
      )
      skill_required <- skill_required %||% character()
      required <- unique(c(required_capability_ids, skill_required))
      # A selected skill can strengthen an enclosing optional request. The
      # returned capability sets remain disjoint, with required taking priority.
      optional <- setdiff(optional_capability_ids, required)
      instructions <- stats::setNames(
        vapply(skills, \(skill) skill@instructions, character(1)),
        skill_ids
      )
      operation_ids <- unique(unlist(
        lapply(skills, \(skill) skill@operation_ids),
        use.names = FALSE
      ))
      operation_ids <- operation_ids %||% character()
      operation_versions <- character()
      for (skill in skills) {
        for (operation_id in skill@operation_ids) {
          version <- tempest_skill_operation_version(skill, operation_id)
          if (!is.null(version)) {
            operation_versions[[operation_id]] <- version
          }
        }
      }

      list(
        skill_ids = skill_ids,
        skill_versions = stats::setNames(
          vapply(skills, \(skill) skill@version, character(1)),
          skill_ids
        ),
        skill_records = unname(lapply(skills, tempest_skill_record)),
        instructions = instructions,
        prompt = paste(unname(instructions), collapse = "\n\n"),
        required_capability_ids = required,
        optional_capability_ids = optional,
        operation_ids = operation_ids,
        operation_versions = operation_versions
      )
    },

    resolve_for_expert = function(expert) {
      if (!S7::S7_inherits(expert, TempestExpertProfile)) {
        tempest_skill_registry_abort(
          "{.arg expert} must be a {.cls tempest_expert} object."
        )
      }
      if (!identical(expert@state, "active")) {
        tempest_skill_registry_abort(
          "Expert {.val {expert@expert_id}} is not active."
        )
      }
      resolved <- self$resolve(
        skill_ids = expert@skill_ids,
        versions = expert@skill_versions,
        required_capability_ids = expert@required_capability_ids,
        optional_capability_ids = expert@optional_capability_ids
      )
      c(
        list(
          expert_id = expert@expert_id,
          expert_version = expert@version
        ),
        resolved
      )
    }
  ),
  private = list(
    skills = NULL,
    operations = NULL,

    validate_skill = function(skill) {
      if (!S7::S7_inherits(skill, TempestSkill)) {
        tempest_skill_registry_abort(
          "{.arg skill} must be a {.cls tempest_skill} object."
        )
      }
      for (operation_id in skill@operation_ids) {
        operation_version <- tempest_skill_operation_version(
          skill,
          operation_id
        )
        tryCatch(
          private$operations$describe(
            operation_id,
            version = operation_version,
            kind = "skill"
          ),
          error = function(error) {
            tempest_skill_registry_abort(
              c(
                "Skill {.val {skill@skill_id}} has an unavailable operation.",
                x = "Could not resolve {.val {operation_id}} as a skill operation."
              ),
              parent = error
            )
          }
        )
      }
      invisible(skill)
    },

    validate_versions = function(versions, skill_ids) {
      if (!is.character(versions) || anyNA(versions)) {
        tempest_skill_registry_abort(
          paste0(
            "{.arg versions} must be a named character vector without ",
            "missing values."
          )
        )
      }
      if (length(versions) == 0L) {
        return(character())
      }
      version_ids <- names(versions)
      if (
        is.null(version_ids) ||
          anyNA(version_ids) ||
          any(!nzchar(version_ids)) ||
          anyDuplicated(version_ids) ||
          any(!version_ids %in% skill_ids)
      ) {
        tempest_skill_registry_abort(c(
          "{.arg versions} must be named by requested skill id.",
          i = "Every name must identify a skill in {.arg skill_ids}."
        ))
      }
      stats::setNames(
        vapply(
          versions,
          tempest_workflow_version,
          character(1),
          arg = "versions"
        ),
        version_ids
      )
    }
  )
)

#' Create a Tempest skill registry
#'
#' `r lifecycle::badge("experimental")`
#'
#' This experimental API is frozen and scheduled for removal in Tempest 0.2.0.
#' No compatibility shim is planned; see
#' [tempest-generic-kernel-retirement].
#'
#' The registry binds serializable skill specifications to versioned runtime
#' operations. Every operation declared by a skill must be available from
#' `operations` with kind `"skill"`.
#'
#' @param skills Optional list of [tempest_skill()] specifications.
#' @param operations A [tempest_operation_registry()] containing runtime skill
#'   operations.
#' @return A mutable registry with `register()`, `get()`, `has()`, `list()`,
#'   `resolve()`, and `resolve_for_expert()` methods. Resolution combines skill
#'   instructions with the required and optional capability sets supplied by
#'   the caller or declared by the expert. Caller-supplied sets must be
#'   disjoint. When a selected skill requires an otherwise optional capability,
#'   the requirement is promoted and the returned sets remain disjoint.
#' @examples
#' operations <- tempest_operation_registry()
#' operations$register(
#'   "skill.compare",
#'   function(left, right) identical(left, right),
#'   kind = "skill"
#' )
#' skills <- tempest_skill_registry(
#'   list(tempest_skill(
#'     "compare",
#'     purpose = "Compare two values",
#'     instructions = "Compare the supplied values.",
#'     operation_ids = "skill.compare"
#'   )),
#'   operations = operations
#' )
#' skills$resolve("compare")$prompt
#' @export
tempest_skill_registry <- function(
  skills = list(),
  operations = tempest_operation_registry()
) {
  TempestSkillRegistry$new(skills = skills, operations = operations)
}

TempestConnectionProvider <- R6::R6Class(
  "TempestConnectionProvider",
  public = list(
    initialize = function(connections = list(), bindings = list()) {
      private$connections <- new.env(parent = emptyenv())
      connections <- tempest_registry_entries(connections, "connections")
      bindings <- tempest_registry_entries(bindings, "bindings")
      binding_ids <- names(bindings)
      if (
        length(bindings) > 0L &&
          (is.null(binding_ids) ||
            anyNA(binding_ids) ||
            any(!nzchar(binding_ids)) ||
            anyDuplicated(binding_ids))
      ) {
        tempest_connection_provider_abort(
          "{.arg bindings} must be named by connection id."
        )
      }
      if (length(connections) > 0L) {
        connection_names <- names(connections)
        if (!is.null(connection_names) && anyNA(connection_names)) {
          tempest_connection_provider_abort(
            "{.arg connections} cannot have missing entry names."
          )
        }
        for (i in seq_along(connections)) {
          connection <- connections[[i]]
          connection_id <- if (
            S7::S7_inherits(connection, TempestConnectionRef)
          ) {
            connection@connection_id
          } else {
            NULL
          }
          binding <- NULL
          if (!is.null(connection_id) && connection_id %in% binding_ids) {
            binding <- bindings[[connection_id]]
          } else if (
            !is.null(connection_names) &&
              nzchar(connection_names[[i]] %||% "") &&
              connection_names[[i]] %in% binding_ids
          ) {
            binding <- bindings[[connection_names[[i]]]]
          }
          if (is.null(binding)) {
            tempest_connection_provider_abort(
              "Connection {.val {connection_id %||% i}} has no runtime binding."
            )
          }
          self$register(connection, binding)
        }
      }
      unused <- setdiff(
        binding_ids %||% character(),
        ls(private$connections, all.names = TRUE)
      )
      if (length(unused) > 0L) {
        tempest_connection_provider_abort(
          "Runtime binding {.val {unused[[1]]}} has no connection reference."
        )
      }
      invisible(self)
    },

    register = function(connection, binding, replace = FALSE) {
      if (!S7::S7_inherits(connection, TempestConnectionRef)) {
        tempest_connection_provider_abort(
          paste0(
            "{.arg connection} must be a ",
            "{.cls tempest_connection_ref} object."
          )
        )
      }
      if (is.null(binding)) {
        tempest_connection_provider_abort(
          "{.arg binding} must be a runtime factory or client."
        )
      }
      replace <- tempest_workflow_flag(replace, "replace")
      connection_id <- connection@connection_id
      if (
        exists(
          connection_id,
          envir = private$connections,
          inherits = FALSE
        ) &&
          !replace
      ) {
        tempest_connection_provider_abort(c(
          "Connection {.val {connection_id}} is already registered.",
          i = "Set {.arg replace} to `TRUE` to replace it explicitly."
        ))
      }
      assign(
        connection_id,
        list(connection = connection, binding = binding),
        envir = private$connections
      )
      invisible(connection_id)
    },

    has = function(connection_id) {
      connection_id <- tryCatch(
        tempest_contract_id(connection_id, "connection_id"),
        error = function(error) NULL
      )
      !is.null(connection_id) &&
        exists(
          connection_id,
          envir = private$connections,
          inherits = FALSE
        )
    },

    list = function() {
      connection_ids <- sort(ls(private$connections, all.names = TRUE))
      stats::setNames(
        lapply(connection_ids, function(connection_id) {
          entry <- get(
            connection_id,
            envir = private$connections,
            inherits = FALSE
          )
          tempest_connection_ref_record(entry$connection)
        }),
        connection_ids
      )
    },

    preflight = function(connection_ids, allowed_ref_ids) {
      connection_ids <- tempest_contract_ids(
        connection_ids,
        "connection_ids"
      )
      allowed_ref_ids <- tempest_contract_ids(
        allowed_ref_ids,
        "allowed_ref_ids"
      )
      denied <- setdiff(connection_ids, allowed_ref_ids)
      if (length(denied) > 0L) {
        tempest_connection_provider_abort(c(
          "Connection {.val {denied[[1]]}} is not allowed in this context.",
          i = "Only explicitly allowed connection references may be resolved."
        ))
      }
      entries <- lapply(connection_ids, function(connection_id) {
        if (
          !exists(
            connection_id,
            envir = private$connections,
            inherits = FALSE
          )
        ) {
          tempest_connection_provider_abort(
            "Connection {.val {connection_id}} is not registered."
          )
        }
        entry <- get(
          connection_id,
          envir = private$connections,
          inherits = FALSE
        )
        if (!identical(entry$connection@state, "active")) {
          tempest_connection_provider_abort(
            "Connection {.val {connection_id}} is not active."
          )
        }
        entry
      })
      names(entries) <- connection_ids
      invisible(entries)
    },

    resolve = function(connection_ids, allowed_ref_ids, context = list()) {
      context <- tempest_runtime_context(context)
      entries <- self$preflight(connection_ids, allowed_ref_ids)
      clients <- lapply(names(entries), function(connection_id) {
        entry <- entries[[connection_id]]
        tryCatch(
          {
            client <- if (is.function(entry$binding)) {
              entry$binding(
                connection_ref = entry$connection,
                context = context
              )
            } else {
              entry$binding
            }
            if (is.null(client)) {
              tempest_connection_provider_abort(
                "Connection {.val {connection_id}} resolved to `NULL`."
              )
            }
            client
          },
          tempest_connection_provider_error = function(error) {
            stop(error)
          },
          error = function(error) {
            tempest_connection_provider_abort(
              "Connection {.val {connection_id}} could not be resolved.",
              parent = error
            )
          }
        )
      })
      names(clients) <- names(entries)
      clients
    }
  ),
  private = list(
    connections = NULL
  )
)

#' Create a Tempest runtime connection provider
#'
#' `r lifecycle::badge("experimental")`
#'
#' This experimental API is frozen and scheduled for removal in Tempest 0.2.0.
#' No compatibility shim is planned; see
#' [tempest-generic-kernel-retirement].
#'
#' A connection provider keeps authenticated clients and factories outside
#' durable [tempest_connection_ref()] records. Resolution requires an explicit
#' allow-list, so a workflow cannot acquire connections that were not granted
#' for its current execution context.
#'
#' @param connections Optional list of serializable connection references.
#' @param bindings Named runtime factories or pre-built clients. A factory is
#'   called with `connection_ref` and `context`. Binding names must match the
#'   stable connection identifiers in `connections`.
#' @return A mutable provider with `register()`, `has()`, `list()`,
#'   `preflight()`, and `resolve()` methods. Listings never expose bindings.
#' @examples
#' reference <- tempest_connection_ref(
#'   "documents",
#'   provider_id = "host",
#'   connection_type = "search",
#'   title = "Documents",
#'   description = "Approved document index"
#' )
#' provider <- tempest_connection_provider(
#'   list(reference),
#'   bindings = list(documents = list(endpoint = "local"))
#' )
#' provider$resolve("documents", allowed_ref_ids = "documents")
#' @export
tempest_connection_provider <- function(
  connections = list(),
  bindings = list()
) {
  TempestConnectionProvider$new(
    connections = connections,
    bindings = bindings
  )
}

tempest_capability_implementation <- function(
  implementation,
  authorize = NULL
) {
  if (is.list(implementation) && !is.data.frame(implementation)) {
    authorize <- implementation$authorize %||% authorize
    implementation <- implementation$factory %||% NULL
  }
  if (!is.function(implementation)) {
    tempest_capability_resolution_abort(
      "{.arg implementation} must contain a runtime factory."
    )
  }
  if (!is.null(authorize) && !is.function(authorize)) {
    tempest_capability_resolution_abort(
      "{.arg authorize} must be `NULL` or a function."
    )
  }
  list(factory = implementation, authorize = authorize)
}

tempest_capability_authorization <- function(
  implementation,
  capability,
  context
) {
  authorize <- implementation$authorize
  if (is.null(authorize)) {
    return(list(granted = TRUE, reason = NULL))
  }
  result <- authorize(capability_spec = capability, context = context)
  if (is.logical(result) && length(result) == 1L && !is.na(result)) {
    return(list(
      granted = isTRUE(result),
      reason = if (isTRUE(result)) NULL else "Authorization denied."
    ))
  }
  if (
    is.list(result) &&
      !is.data.frame(result) &&
      is.logical(result$granted) &&
      length(result$granted) == 1L &&
      !is.na(result$granted)
  ) {
    reason <- result$reason %||% NULL
    if (
      !is.null(reason) &&
        (!is.character(reason) || length(reason) != 1L || is.na(reason))
    ) {
      tempest_capability_resolution_abort(
        "Authorization reasons must be a single string or `NULL`."
      )
    }
    return(list(granted = result$granted, reason = reason))
  }
  tempest_capability_resolution_abort(
    paste0(
      "Authorization must return `TRUE`, `FALSE`, or a list with a ",
      "logical {.field granted} field."
    )
  )
}

tempest_capability_grant_record <- function(
  capability_id,
  required,
  status,
  reason_code = NULL,
  reason = NULL,
  capability = NULL,
  metadata = list()
) {
  if (!status %in% c("granted", "denied")) {
    tempest_capability_resolution_abort(
      "Capability grant status must be {.val granted} or {.val denied}."
    )
  }
  metadata <- tryCatch(
    tempest_contract_serializable_list(metadata, "metadata"),
    error = function(error) {
      tempest_capability_resolution_abort(
        "Capability grant metadata must be non-secret serializable data.",
        parent = error
      )
    }
  )
  record <- list(
    capability_id = capability_id,
    capability_version = if (is.null(capability)) {
      NULL
    } else {
      capability@version
    },
    operation_id = if (is.null(capability)) {
      NULL
    } else {
      capability@operation_id
    },
    operation_version = if (is.null(capability)) {
      NULL
    } else {
      capability@operation_version
    },
    required = required,
    status = status,
    connection_ref_ids = if (is.null(capability)) {
      character()
    } else {
      capability@connection_ref_ids
    },
    reason_code = reason_code,
    reason = reason,
    metadata = metadata
  )
  tryCatch(
    {
      tempest_canonical_json(record)
      record
    },
    error = function(error) {
      tempest_capability_resolution_abort(
        "Capability grant records must be serializable.",
        parent = error
      )
    }
  )
}

tempest_capability_denied_grant <- function(
  plan,
  reason_code = plan$reason_code,
  reason = plan$reason
) {
  stats::setNames(
    list(tempest_capability_grant_record(
      capability_id = plan$capability_id,
      required = plan$required,
      status = "denied",
      reason_code = reason_code,
      reason = reason,
      capability = plan$capability
    )),
    plan$capability_id
  )
}

tempest_capability_factory_result <- function(result) {
  if (!is.list(result) || is.data.frame(result)) {
    tempest_capability_resolution_abort(
      paste0(
        "Capability factories must return a list with {.field tools}, ",
        "{.field registrars}, and optional {.field metadata}."
      )
    )
  }
  tools <- result$tools %||% list()
  if (!is.list(tools) || is.data.frame(tools)) {
    tempest_capability_resolution_abort(
      "Capability factory {.field tools} must be a list."
    )
  }
  registrars <- result$registrars %||% list()
  if (is.function(registrars)) {
    registrars <- list(registrars)
  }
  if (
    !is.list(registrars) ||
      is.data.frame(registrars) ||
      any(!vapply(registrars, is.function, logical(1)))
  ) {
    tempest_capability_resolution_abort(
      "Capability factory {.field registrars} must contain only functions."
    )
  }
  metadata <- tryCatch(
    tempest_contract_serializable_list(
      result$metadata %||% list(),
      "metadata"
    ),
    error = function(error) {
      tempest_capability_resolution_abort(
        "Capability factory metadata must be non-secret serializable data.",
        parent = error
      )
    }
  )
  list(tools = tools, registrars = registrars, metadata = metadata)
}

TempestCapabilityResolver <- R6::R6Class(
  "TempestCapabilityResolver",
  public = list(
    initialize = function(
      specifications = list(),
      implementations = list(),
      connection_provider = NULL
    ) {
      if (
        !is.null(connection_provider) &&
          !inherits(connection_provider, "TempestConnectionProvider")
      ) {
        tempest_capability_resolution_abort(
          paste0(
            "{.arg connection_provider} must be `NULL` or a ",
            "{.cls TempestConnectionProvider}."
          )
        )
      }
      private$specifications <- new.env(parent = emptyenv())
      private$implementations <- new.env(parent = emptyenv())
      private$connection_provider <- connection_provider
      self$register_specifications(specifications)
      self$register_implementations(implementations)
      invisible(self)
    },

    register_specification = function(capability, replace = FALSE) {
      if (!S7::S7_inherits(capability, TempestCapabilitySpec)) {
        tempest_capability_resolution_abort(
          paste0(
            "{.arg capability} must be a ",
            "{.cls tempest_capability_spec} object."
          )
        )
      }
      replace <- tempest_workflow_flag(replace, "replace")
      capability_id <- capability@capability_id
      if (
        exists(
          capability_id,
          envir = private$specifications,
          inherits = FALSE
        ) &&
          !replace
      ) {
        tempest_capability_resolution_abort(c(
          "Capability {.val {capability_id}} is already registered.",
          i = "Set {.arg replace} to `TRUE` to replace it explicitly."
        ))
      }
      assign(
        capability_id,
        capability,
        envir = private$specifications
      )
      invisible(capability_id)
    },

    register_specifications = function(specifications) {
      specifications <- tempest_registry_entries(
        specifications,
        "specifications"
      )
      if (length(specifications) == 0L) {
        return(invisible(self))
      }
      specification_names <- names(specifications)
      if (!is.null(specification_names) && anyNA(specification_names)) {
        tempest_capability_resolution_abort(
          "{.arg specifications} cannot have missing entry names."
        )
      }
      for (i in seq_along(specifications)) {
        capability <- specifications[[i]]
        if (
          !is.null(specification_names) &&
            nzchar(specification_names[[i]] %||% "") &&
            S7::S7_inherits(capability, TempestCapabilitySpec) &&
            !identical(
              specification_names[[i]],
              capability@capability_id
            )
        ) {
          tempest_capability_resolution_abort(c(
            "Named capability entries must match their stable id.",
            x = paste0(
              "Entry {.val {specification_names[[i]]}} contains ",
              "{.val {capability@capability_id}}."
            )
          ))
        }
        self$register_specification(capability)
      }
      invisible(self)
    },

    register_implementation = function(
      capability_id,
      implementation,
      authorize = NULL,
      replace = FALSE
    ) {
      capability_id <- tempest_contract_id(
        capability_id,
        "capability_id"
      )
      if (
        !exists(
          capability_id,
          envir = private$specifications,
          inherits = FALSE
        )
      ) {
        tempest_capability_resolution_abort(
          "Capability {.val {capability_id}} has no registered specification."
        )
      }
      implementation <- tempest_capability_implementation(
        implementation,
        authorize = authorize
      )
      replace <- tempest_workflow_flag(replace, "replace")
      if (
        exists(
          capability_id,
          envir = private$implementations,
          inherits = FALSE
        ) &&
          !replace
      ) {
        tempest_capability_resolution_abort(c(
          "Capability {.val {capability_id}} has a runtime implementation.",
          i = "Set {.arg replace} to `TRUE` to replace it explicitly."
        ))
      }
      assign(
        capability_id,
        implementation,
        envir = private$implementations
      )
      invisible(capability_id)
    },

    register_implementations = function(implementations) {
      implementations <- tempest_registry_entries(
        implementations,
        "implementations"
      )
      if (length(implementations) == 0L) {
        return(invisible(self))
      }
      capability_ids <- names(implementations)
      if (
        is.null(capability_ids) ||
          anyNA(capability_ids) ||
          any(!nzchar(capability_ids)) ||
          anyDuplicated(capability_ids)
      ) {
        tempest_capability_resolution_abort(
          "{.arg implementations} must be named by capability id."
        )
      }
      for (capability_id in capability_ids) {
        implementation <- implementations[[capability_id]]
        authorize <- if (
          is.list(implementation) &&
            !is.data.frame(implementation)
        ) {
          implementation$authorize %||% NULL
        } else {
          NULL
        }
        self$register_implementation(
          capability_id,
          implementation,
          authorize = authorize
        )
      }
      invisible(self)
    },

    register = function(
      capability,
      implementation,
      authorize = NULL,
      replace = FALSE
    ) {
      implementation <- tempest_capability_implementation(
        implementation,
        authorize = authorize
      )
      self$register_specification(capability, replace = replace)
      self$register_implementation(
        capability@capability_id,
        implementation,
        replace = replace
      )
      invisible(capability@capability_id)
    },

    has = function(capability_id, implementation = FALSE) {
      capability_id <- tryCatch(
        tempest_contract_id(capability_id, "capability_id"),
        error = function(error) NULL
      )
      if (is.null(capability_id)) {
        return(FALSE)
      }
      has_specification <- exists(
        capability_id,
        envir = private$specifications,
        inherits = FALSE
      )
      if (!isTRUE(implementation)) {
        return(has_specification)
      }
      has_specification &&
        exists(
          capability_id,
          envir = private$implementations,
          inherits = FALSE
        )
    },

    list = function() {
      capability_ids <- sort(
        ls(private$specifications, all.names = TRUE)
      )
      stats::setNames(
        lapply(capability_ids, function(capability_id) {
          capability <- get(
            capability_id,
            envir = private$specifications,
            inherits = FALSE
          )
          list(
            specification = tempest_capability_spec_record(capability),
            implementation_registered = exists(
              capability_id,
              envir = private$implementations,
              inherits = FALSE
            )
          )
        }),
        capability_ids
      )
    },

    resolve = function(
      required_capability_ids = character(),
      optional_capability_ids = character(),
      allowed_connection_ref_ids = character(),
      model_role = NULL,
      context = list()
    ) {
      required_capability_ids <- tempest_contract_ids(
        required_capability_ids,
        "required_capability_ids"
      )
      optional_capability_ids <- tempest_contract_ids(
        optional_capability_ids,
        "optional_capability_ids"
      )
      overlap <- intersect(
        required_capability_ids,
        optional_capability_ids
      )
      if (length(overlap) > 0L) {
        tempest_capability_resolution_abort(c(
          "Required and optional capability requests must be disjoint.",
          x = "Duplicated capability: {.val {overlap[[1]]}}."
        ))
      }
      allowed_connection_ref_ids <- tempest_contract_ids(
        allowed_connection_ref_ids,
        "allowed_connection_ref_ids"
      )
      if (!is.null(model_role)) {
        model_role <- tempest_contract_id(model_role, "model_role")
      }
      context <- tempest_runtime_context(context)
      requested <- c(
        stats::setNames(
          rep(TRUE, length(required_capability_ids)),
          required_capability_ids
        ),
        stats::setNames(
          rep(FALSE, length(optional_capability_ids)),
          optional_capability_ids
        )
      )

      plans <- lapply(names(requested), function(capability_id) {
        private$preflight_one(
          capability_id = capability_id,
          required = unname(requested[[capability_id]]),
          allowed_connection_ref_ids = allowed_connection_ref_ids,
          model_role = model_role,
          context = context
        )
      })
      names(plans) <- names(requested)

      denied_required <- Filter(
        \(plan) plan$required && !plan$granted,
        plans
      )
      if (length(denied_required) > 0L) {
        denied <- denied_required[[1]]
        tempest_capability_resolution_abort(
          c(
            "Required capability {.val {denied$capability_id}} was denied.",
            x = "{denied$reason}"
          ),
          capability_grants = tempest_capability_denied_grant(denied)
        )
      }

      connection_clients <- list()
      for (capability_id in names(plans)) {
        plan <- plans[[capability_id]]
        if (!plan$granted || length(plan$connection_ref_ids) == 0L) {
          next
        }
        missing_connections <- setdiff(
          plan$connection_ref_ids,
          names(connection_clients)
        )
        if (length(missing_connections) == 0L) {
          next
        }
        resolved <- tryCatch(
          private$connection_provider$resolve(
            missing_connections,
            allowed_ref_ids = allowed_connection_ref_ids,
            context = context
          ),
          error = function(error) error
        )
        if (inherits(resolved, "error")) {
          if (plan$required) {
            tempest_capability_resolution_abort(
              paste0(
                "Connections for required capability ",
                "{.val {capability_id}} could not be resolved."
              ),
              parent = resolved,
              capability_grants = tempest_capability_denied_grant(
                plan,
                reason_code = "connection_resolution_failed",
                reason = conditionMessage(resolved)
              )
            )
          }
          plan$granted <- FALSE
          plan$reason_code <- "connection_resolution_failed"
          plan$reason <- conditionMessage(resolved)
          plans[[capability_id]] <- plan
          next
        }
        connection_clients <- c(connection_clients, resolved)
      }

      tools <- list()
      registrars <- list()
      grants <- list()
      for (capability_id in names(plans)) {
        plan <- plans[[capability_id]]
        if (!plan$granted) {
          grants[[capability_id]] <- tempest_capability_grant_record(
            capability_id = capability_id,
            required = plan$required,
            status = "denied",
            reason_code = plan$reason_code,
            reason = plan$reason,
            capability = plan$capability
          )
          next
        }
        clients <- connection_clients[plan$connection_ref_ids]
        result <- tryCatch(
          plan$implementation$factory(
            capability_spec = plan$capability,
            connections = clients,
            context = context
          ) |>
            tempest_capability_factory_result(),
          error = function(error) error
        )
        if (inherits(result, "error")) {
          if (plan$required) {
            tempest_capability_resolution_abort(
              "Required capability {.val {capability_id}} failed to initialize.",
              parent = result,
              capability_grants = tempest_capability_denied_grant(
                plan,
                reason_code = "factory_failed",
                reason = conditionMessage(result)
              )
            )
          }
          grants[[capability_id]] <- tempest_capability_grant_record(
            capability_id = capability_id,
            required = FALSE,
            status = "denied",
            reason_code = "factory_failed",
            reason = conditionMessage(result),
            capability = plan$capability
          )
          next
        }
        tools <- c(tools, result$tools)
        registrars <- c(registrars, result$registrars)
        grants[[capability_id]] <- tempest_capability_grant_record(
          capability_id = capability_id,
          required = plan$required,
          status = "granted",
          capability = plan$capability,
          metadata = result$metadata
        )
      }

      structure(
        list(
          tools = tools,
          registrars = registrars,
          grants = grants
        ),
        class = c("tempest_capability_resolution", "list")
      )
    }
  ),
  private = list(
    specifications = NULL,
    implementations = NULL,
    connection_provider = NULL,

    preflight_one = function(
      capability_id,
      required,
      allowed_connection_ref_ids,
      model_role,
      context
    ) {
      denied <- function(
        reason_code,
        reason,
        capability = NULL,
        implementation = NULL
      ) {
        list(
          capability_id = capability_id,
          required = required,
          granted = FALSE,
          reason_code = reason_code,
          reason = reason,
          capability = capability,
          implementation = implementation,
          connection_ref_ids = if (is.null(capability)) {
            character()
          } else {
            capability@connection_ref_ids
          }
        )
      }
      if (
        !exists(
          capability_id,
          envir = private$specifications,
          inherits = FALSE
        )
      ) {
        return(denied(
          "specification_missing",
          "Capability specification is not registered."
        ))
      }
      capability <- get(
        capability_id,
        envir = private$specifications,
        inherits = FALSE
      )
      if (!identical(capability@state, "active")) {
        return(denied(
          "capability_inactive",
          "Capability specification is not active.",
          capability
        ))
      }
      if (
        !exists(
          capability_id,
          envir = private$implementations,
          inherits = FALSE
        )
      ) {
        return(denied(
          "implementation_missing",
          "Runtime capability implementation is not registered.",
          capability
        ))
      }
      implementation <- get(
        capability_id,
        envir = private$implementations,
        inherits = FALSE
      )
      if (
        length(capability@model_roles) > 0L &&
          (is.null(model_role) || !model_role %in% capability@model_roles)
      ) {
        return(denied(
          "model_role_denied",
          "The current model role is not allowed to use this capability.",
          capability,
          implementation
        ))
      }
      denied_connections <- setdiff(
        capability@connection_ref_ids,
        allowed_connection_ref_ids
      )
      if (length(denied_connections) > 0L) {
        return(denied(
          "connection_not_allowed",
          paste0(
            "Connection ",
            denied_connections[[1]],
            " is not allowed in this context."
          ),
          capability,
          implementation
        ))
      }
      if (
        length(capability@connection_ref_ids) > 0L &&
          is.null(private$connection_provider)
      ) {
        return(denied(
          "connection_provider_missing",
          "No runtime connection provider is configured.",
          capability,
          implementation
        ))
      }
      if (length(capability@connection_ref_ids) > 0L) {
        connection_check <- tryCatch(
          {
            private$connection_provider$preflight(
              capability@connection_ref_ids,
              allowed_ref_ids = allowed_connection_ref_ids
            )
            NULL
          },
          error = function(error) error
        )
        if (inherits(connection_check, "error")) {
          return(denied(
            "connection_unavailable",
            conditionMessage(connection_check),
            capability,
            implementation
          ))
        }
      }
      authorization <- tryCatch(
        tempest_capability_authorization(
          implementation,
          capability,
          context
        ),
        error = function(error) error
      )
      if (inherits(authorization, "error")) {
        return(denied(
          "authorization_failed",
          conditionMessage(authorization),
          capability,
          implementation
        ))
      }
      if (!authorization$granted) {
        return(denied(
          "authorization_denied",
          authorization$reason %||% "Authorization denied.",
          capability,
          implementation
        ))
      }
      list(
        capability_id = capability_id,
        required = required,
        granted = TRUE,
        reason_code = NULL,
        reason = NULL,
        capability = capability,
        implementation = implementation,
        connection_ref_ids = capability@connection_ref_ids
      )
    }
  )
)

#' Create a Tempest capability resolver
#'
#' `r lifecycle::badge("experimental")`
#'
#' This experimental API is frozen and scheduled for removal in Tempest 0.2.0.
#' No compatibility shim is planned; see
#' [tempest-generic-kernel-retirement].
#'
#' The resolver keeps serializable capability specifications separate from
#' runtime factories. It preflights every requested capability before invoking
#' factories, treats required failures as errors, and records optional failures
#' as serializable denied grants.
#'
#' @param specifications Optional list of [tempest_capability_spec()] objects.
#' @param implementations Named runtime factories or descriptor lists with
#'   `factory` and optional `authorize` functions. A factory receives
#'   `capability_spec`, named `connections`, and runtime `context`, and returns
#'   a list containing `tools`, `registrars`, and optional serializable
#'   `metadata`.
#' @param connection_provider Optional [tempest_connection_provider()] used to
#'   resolve explicitly allowed opaque connection references.
#' @return A mutable resolver with registration, inspection, and `resolve()`
#'   methods. Resolution returns runtime tools and registrars plus serializable
#'   grant records.
#' @examples
#' specification <- tempest_capability_spec(
#'   "documents.search",
#'   purpose = "Search approved documents",
#'   instructions = "Search only the granted index.",
#'   operation_id = "capability.documents.search"
#' )
#' resolver <- tempest_capability_resolver(
#'   list(specification),
#'   implementations = list(
#'     "documents.search" = function(capability_spec, connections, context) {
#'       list(tools = list(), registrars = list())
#'     }
#'   )
#' )
#' resolver$resolve(required_capability_ids = "documents.search")
#' @export
tempest_capability_resolver <- function(
  specifications = list(),
  implementations = list(),
  connection_provider = NULL
) {
  TempestCapabilityResolver$new(
    specifications = specifications,
    implementations = implementations,
    connection_provider = connection_provider
  )
}

tempest_register_capabilities <- function(
  chat,
  resolution,
  context = list()
) {
  if (
    !inherits(resolution, "tempest_capability_resolution") ||
      !is.list(resolution$tools) ||
      !is.list(resolution$registrars)
  ) {
    tempest_capability_resolution_abort(
      "{.arg resolution} is not a capability resolution."
    )
  }
  context <- tempest_runtime_context(context)
  if (length(resolution$tools) > 0L) {
    register_tools <- tryCatch(chat$register_tools, error = \(error) NULL)
    if (!is.function(register_tools)) {
      tempest_capability_resolution_abort(
        "The chat does not provide a {.fn register_tools} method.",
        class = "tempest_capability_registration_error"
      )
    }
    tryCatch(
      register_tools(resolution$tools),
      error = function(error) {
        tempest_capability_resolution_abort(
          "Resolved capability tools could not be registered.",
          parent = error,
          class = "tempest_capability_registration_error"
        )
      }
    )
  }
  for (registrar in resolution$registrars) {
    tryCatch(
      registrar(chat = chat, context = context),
      error = function(error) {
        tempest_capability_resolution_abort(
          "A resolved capability registrar failed.",
          parent = error,
          class = "tempest_capability_registration_error"
        )
      }
    )
  }
  invisible(chat)
}
