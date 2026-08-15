# Runtime operation registry

tempest_operation_kinds <- function() {
  c(
    "step",
    "generator",
    "validator",
    "renderer",
    "exporter",
    "skill",
    "capability"
  )
}

tempest_operation_abort <- function(message, ..., parent = NULL) {
  tempest_abort(
    message,
    ...,
    class = c("tempest_operation_registry_error", "tempest_error"),
    parent = parent,
    .envir = rlang::caller_env()
  )
}

tempest_operation_descriptor <- function(
  id,
  implementation,
  version = "1",
  kind = "step",
  metadata = list()
) {
  id <- tempest_workflow_scalar(id, "id")
  version <- tempest_workflow_version(version)
  kind <- tempest_workflow_scalar(kind, "kind")
  if (!kind %in% tempest_operation_kinds()) {
    tempest_operation_abort(
      "{.arg kind} must be one of {.val {tempest_operation_kinds()}}."
    )
  }
  if (!is.function(implementation)) {
    tempest_operation_abort(
      "{.arg implementation} must be a function."
    )
  }
  metadata <- tempest_workflow_serializable_list(metadata, "metadata")
  list(
    id = id,
    version = version,
    kind = kind,
    implementation = implementation,
    metadata = metadata
  )
}

TempestOperationRegistry <- R6::R6Class(
  "TempestOperationRegistry",
  public = list(
    initialize = function(operations = list()) {
      private$operations <- new.env(parent = emptyenv())
      self$register_many(operations)
      invisible(self)
    },

    register = function(
      id,
      implementation,
      version = "1",
      kind = "step",
      metadata = list(),
      replace = FALSE
    ) {
      descriptor <- tempest_operation_descriptor(
        id = id,
        implementation = implementation,
        version = version,
        kind = kind,
        metadata = metadata
      )
      replace <- tempest_workflow_flag(replace, "replace")
      if (
        exists(
          descriptor$id,
          envir = private$operations,
          inherits = FALSE
        ) &&
          !replace
      ) {
        tempest_operation_abort(
          c(
            "Operation {.val {descriptor$id}} is already registered.",
            i = "Set {.arg replace} to `TRUE` to replace it explicitly."
          )
        )
      }
      assign(descriptor$id, descriptor, envir = private$operations)
      invisible(descriptor$id)
    },

    register_many = function(operations) {
      operations <- operations %||% list()
      if (!is.list(operations) || is.data.frame(operations)) {
        tempest_operation_abort(
          "{.arg operations} must be a list of functions or descriptors."
        )
      }
      if (length(operations) == 0L) {
        return(invisible(self))
      }

      operation_names <- names(operations)
      for (i in seq_along(operations)) {
        operation <- operations[[i]]
        entry_name <- if (
          !is.null(operation_names) &&
            nzchar(operation_names[[i]] %||% "")
        ) {
          operation_names[[i]]
        } else {
          NULL
        }
        if (is.function(operation)) {
          if (is.null(entry_name)) {
            tempest_operation_abort(
              "Function shorthand entries in {.arg operations} must be named."
            )
          }
          self$register(entry_name, operation)
          next
        }
        if (!is.list(operation) || is.data.frame(operation)) {
          tempest_operation_abort(
            "Each operation must be a function or descriptor list."
          )
        }
        id <- operation$id %||% entry_name
        if (is.null(id)) {
          tempest_operation_abort(
            "Operation descriptors must include an {.field id} or a list name."
          )
        }
        implementation <- operation$implementation %||% NULL
        self$register(
          id = id,
          implementation = implementation,
          version = operation$version %||% "1",
          kind = operation$kind %||% "step",
          metadata = operation$metadata %||% list(),
          replace = operation$replace %||% FALSE
        )
      }
      invisible(self)
    },

    resolve = function(id, version = NULL, kind = NULL) {
      descriptor <- private$resolve_descriptor(
        id,
        version = version,
        kind = kind,
        error = TRUE
      )
      descriptor$implementation
    },

    describe = function(id, version = NULL, kind = NULL) {
      descriptor <- private$resolve_descriptor(
        id,
        version = version,
        kind = kind,
        error = TRUE
      )
      descriptor[c("id", "version", "kind", "metadata")]
    },

    has = function(id, version = NULL, kind = NULL) {
      !is.null(private$resolve_descriptor(
        id,
        version = version,
        kind = kind,
        error = FALSE
      ))
    },

    list = function() {
      ids <- sort(ls(private$operations, all.names = TRUE))
      stats::setNames(
        lapply(ids, function(id) {
          descriptor <- get(
            id,
            envir = private$operations,
            inherits = FALSE
          )
          descriptor[c("id", "version", "kind", "metadata")]
        }),
        ids
      )
    }
  ),
  private = list(
    operations = NULL,

    resolve_descriptor = function(
      id,
      version = NULL,
      kind = NULL,
      error = TRUE
    ) {
      id <- tryCatch(
        tempest_workflow_scalar(id, "id"),
        error = function(condition) {
          if (error) {
            stop(condition)
          }
          NULL
        }
      )
      if (is.null(id)) {
        return(NULL)
      }
      if (!exists(id, envir = private$operations, inherits = FALSE)) {
        if (error) {
          tempest_operation_abort(
            "Operation {.val {id}} is not registered."
          )
        }
        return(NULL)
      }
      descriptor <- get(id, envir = private$operations, inherits = FALSE)

      if (!is.null(version)) {
        version <- tryCatch(
          tempest_workflow_version(version),
          error = function(condition) {
            if (error) {
              stop(condition)
            }
            NULL
          }
        )
        if (
          is.null(version) ||
            !identical(descriptor$version, version)
        ) {
          if (error) {
            tempest_operation_abort(c(
              "Operation {.val {id}} has an incompatible version.",
              x = "Requested {.val {version}}, registered {.val {descriptor$version}}."
            ))
          }
          return(NULL)
        }
      }

      if (!is.null(kind)) {
        kind <- tryCatch(
          tempest_workflow_scalar(kind, "kind"),
          error = function(condition) {
            if (error) {
              stop(condition)
            }
            NULL
          }
        )
        if (
          is.null(kind) ||
            !kind %in% tempest_operation_kinds() ||
            !identical(descriptor$kind, kind)
        ) {
          if (error) {
            tempest_operation_abort(c(
              "Operation {.val {id}} has an incompatible kind.",
              x = "Requested {.val {kind}}, registered {.val {descriptor$kind}}."
            ))
          }
          return(NULL)
        }
      }
      descriptor
    }
  )
)

#' Create a Tempest runtime operation registry
#'
#' `r lifecycle::badge("experimental")`
#'
#' This experimental API is frozen and scheduled for removal in Tempest 0.2.0.
#' No compatibility shim is planned; see
#' [tempest-generic-kernel-retirement].
#'
#' The registry keeps executable functions outside serializable workflow and
#' deliverable specifications. Operations are resolved by stable id, version,
#' and kind when a run is rehydrated.
#'
#' @param operations Optional list of named function shorthand entries or
#'   descriptor lists. A descriptor contains `id`, `implementation`, `version`,
#'   `kind`, and optional serializable `metadata`.
#' @return A mutable runtime registry with `register()`, `resolve()`,
#'   `describe()`, `has()`, and `list()` methods.
#' @examples
#' registry <- tempest_operation_registry(list(
#'   summarize = list(
#'     kind = "generator",
#'     implementation = function(context) context
#'   )
#' ))
#' registry$has("summarize", kind = "generator")
#' @export
tempest_operation_registry <- function(operations = list()) {
  TempestOperationRegistry$new(operations = operations)
}
