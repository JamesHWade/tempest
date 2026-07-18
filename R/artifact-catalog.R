# Typed artifact catalog

tempest_artifact_catalog_abort <- function(message, ..., parent = NULL) {
  tempest_abort(
    message,
    ...,
    class = c("tempest_artifact_catalog_error", "tempest_error"),
    parent = parent,
    .envir = rlang::caller_env()
  )
}

tempest_validation_result_data <- function(result) {
  if (!S7::S7_inherits(result, TempestValidationResult)) {
    tempest_artifact_catalog_abort(
      "{.arg result} must be created by {.fn tempest_validation_result}."
    )
  }
  stats::setNames(
    lapply(S7::prop_names(result), function(name) S7::prop(result, name)),
    S7::prop_names(result)
  )
}

tempest_artifact_data <- function(artifact, include_content = TRUE) {
  if (!S7::S7_inherits(artifact, TempestArtifact)) {
    tempest_artifact_catalog_abort(
      "{.arg artifact} must be created by {.fn tempest_artifact}."
    )
  }
  include_content <- tempest_workflow_flag(
    include_content,
    "include_content"
  )
  property_names <- S7::prop_names(artifact)
  if (!include_content) {
    property_names <- setdiff(property_names, "content")
  }
  data <- stats::setNames(
    lapply(
      property_names,
      function(name) S7::prop(artifact, name)
    ),
    property_names
  )
  data$validation_results <- lapply(
    artifact@validation_results,
    tempest_validation_result_data
  )
  data
}

tempest_artifact_catalog_artifacts <- function(artifacts) {
  artifacts <- artifacts %||% list()
  if (
    !is.list(artifacts) ||
      any(
        !vapply(
          artifacts,
          function(artifact) S7::S7_inherits(artifact, TempestArtifact),
          logical(1)
        )
      )
  ) {
    tempest_artifact_catalog_abort(
      "{.arg artifacts} must contain only objects from {.fn tempest_artifact}."
    )
  }
  artifacts
}

TempestArtifactCatalog <- R6::R6Class(
  "TempestArtifactCatalog",
  public = list(
    initialize = function(store = NULL, artifacts = list()) {
      if (
        !is.null(store) &&
          !inherits(store, "tempest_artifact_store")
      ) {
        tempest_artifact_catalog_abort(
          "{.arg store} must be created by {.fn tempest_artifact_store}."
        )
      }
      private$artifacts <- new.env(parent = emptyenv())
      private$store <- store
      self$add_many(artifacts, persist = FALSE)
      invisible(self)
    },

    add = function(artifact, replace = FALSE, persist = TRUE) {
      if (!S7::S7_inherits(artifact, TempestArtifact)) {
        tempest_artifact_catalog_abort(
          "{.arg artifact} must be created by {.fn tempest_artifact}."
        )
      }
      replace <- tempest_workflow_flag(replace, "replace")
      persist <- tempest_workflow_flag(persist, "persist")
      artifact_id <- artifact@artifact_id
      if (
        exists(
          artifact_id,
          envir = private$artifacts,
          inherits = FALSE
        ) &&
          !replace
      ) {
        tempest_artifact_catalog_abort(
          c(
            "Artifact {.val {artifact_id}} already exists.",
            i = "Set {.arg replace} to `TRUE` to replace it explicitly."
          )
        )
      }

      if (persist && !is.null(private$store)) {
        tryCatch(
          tempest_artifact_write(
            private$store,
            artifact_id,
            artifact@content,
            metadata = tempest_artifact_data(
              artifact,
              include_content = FALSE
            )
          ),
          error = function(error) {
            tempest_artifact_catalog_abort(
              "Could not persist artifact {.val {artifact_id}}.",
              parent = error
            )
          }
        )
      }
      assign(artifact_id, artifact, envir = private$artifacts)
      invisible(artifact_id)
    },

    add_many = function(
      artifacts,
      replace = FALSE,
      persist = TRUE
    ) {
      artifacts <- tempest_artifact_catalog_artifacts(artifacts)
      replace <- tempest_workflow_flag(replace, "replace")
      persist <- tempest_workflow_flag(persist, "persist")
      for (artifact in artifacts) {
        self$add(
          artifact,
          replace = replace,
          persist = persist
        )
      }
      invisible(self)
    },

    get = function(artifact_id, error = TRUE) {
      artifact_id <- tempest_workflow_scalar(
        artifact_id,
        "artifact_id"
      )
      error <- tempest_workflow_flag(error, "error")
      if (
        !exists(
          artifact_id,
          envir = private$artifacts,
          inherits = FALSE
        )
      ) {
        if (error) {
          tempest_artifact_catalog_abort(
            "Unknown artifact id {.val {artifact_id}}."
          )
        }
        return(NULL)
      }
      get(
        artifact_id,
        envir = private$artifacts,
        inherits = FALSE
      )
    },

    has = function(artifact_id, version = NULL) {
      artifact <- self$get(artifact_id, error = FALSE)
      if (is.null(artifact)) {
        return(FALSE)
      }
      if (is.null(version)) {
        return(TRUE)
      }
      version <- tryCatch(
        tempest_workflow_version(version),
        error = function(error) NULL
      )
      !is.null(version) &&
        identical(artifact@deliverable_version, version)
    },

    version = function(artifact_id) {
      self$get(artifact_id)@deliverable_version
    },

    list = function(
      deliverable_id = NULL,
      status = NULL,
      include_content = FALSE
    ) {
      ids <- sort(ls(private$artifacts, all.names = TRUE))
      artifacts <- lapply(
        ids,
        function(id) {
          get(id, envir = private$artifacts, inherits = FALSE)
        }
      )
      if (!is.null(deliverable_id)) {
        deliverable_id <- tempest_workflow_scalar(
          deliverable_id,
          "deliverable_id"
        )
        keep <- vapply(
          artifacts,
          function(artifact) {
            identical(artifact@deliverable_id, deliverable_id)
          },
          logical(1)
        )
        ids <- ids[keep]
        artifacts <- artifacts[keep]
      }
      if (!is.null(status)) {
        status <- tempest_workflow_character(status, "status")
        keep <- vapply(
          artifacts,
          function(artifact) artifact@status %in% status,
          logical(1)
        )
        ids <- ids[keep]
        artifacts <- artifacts[keep]
      }
      stats::setNames(
        lapply(
          artifacts,
          tempest_artifact_data,
          include_content = include_content
        ),
        ids
      )
    },

    snapshot = function(include_content = TRUE) {
      self$list(include_content = include_content)
    }
  ),
  private = list(
    artifacts = NULL,
    store = NULL
  ),
  cloneable = FALSE
)

#' Create a typed Tempest artifact catalog
#'
#' `r lifecycle::badge("experimental")`
#'
#' The catalog owns typed artifacts for one workflow run. It supports
#' metadata-only listing so host applications do not need to load large
#' artifact content to render an output index.
#'
#' @param store Optional host artifact-store adapter. Catalog writes are
#'   persisted through the adapter before becoming visible in memory.
#' @param artifacts Optional initial list of typed artifacts.
#' @return A `TempestArtifactCatalog` with `add()`, `get()`, `has()`,
#'   `version()`, `list()`, and `snapshot()` methods.
#' @examples
#' spec <- tempest_deliverable_spec(
#'   "brief",
#'   title = "Brief",
#'   purpose = "Summarize the result",
#'   instructions = "Be concise.",
#'   generator_id = "tempest.generator.markdown_report",
#'   renderer_ids = "tempest.renderer.markdown"
#' )
#' catalog <- tempest_artifact_catalog()
#' artifact <- tempest_artifact(spec, content = "# Brief")
#' catalog$add(artifact)
#' catalog$list()
#' @export
tempest_artifact_catalog <- function(store = NULL, artifacts = list()) {
  TempestArtifactCatalog$new(store = store, artifacts = artifacts)
}
