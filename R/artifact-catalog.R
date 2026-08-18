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

tempest_artifact_catalog_deliverables <- function(deliverables) {
  deliverables <- deliverables %||% list()
  if (
    !is.list(deliverables) ||
      any(
        !vapply(
          deliverables,
          function(deliverable) {
            S7::S7_inherits(deliverable, TempestDeliverableSpec)
          },
          logical(1)
        )
      )
  ) {
    tempest_artifact_catalog_abort(
      "{.arg deliverables} must contain only objects from {.fn tempest_deliverable_spec}."
    )
  }
  deliverables
}

tempest_deliverable_catalog_key <- function(deliverable_id, version) {
  paste0(deliverable_id, "@", version)
}

TempestArtifactCatalog <- R6::R6Class(
  "TempestArtifactCatalog",
  public = list(
    initialize = function(
      store = NULL,
      artifacts = list(),
      deliverables = list()
    ) {
      if (
        !is.null(store) &&
          !inherits(store, "tempest_artifact_store")
      ) {
        tempest_artifact_catalog_abort(
          "{.arg store} must be created by {.fn tempest_artifact_store}."
        )
      }
      private$artifacts <- new.env(parent = emptyenv())
      private$deliverables <- new.env(parent = emptyenv())
      private$store <- store
      self$register_many(deliverables)
      self$add_many(artifacts, persist = FALSE)
      invisible(self)
    },

    register = function(deliverable, replace = FALSE) {
      if (!S7::S7_inherits(deliverable, TempestDeliverableSpec)) {
        tempest_artifact_catalog_abort(
          "{.arg deliverable} must be created by {.fn tempest_deliverable_spec}."
        )
      }
      replace <- tempest_workflow_flag(replace, "replace")
      key <- tempest_deliverable_catalog_key(
        deliverable@deliverable_id,
        deliverable@version
      )
      if (exists(key, envir = private$deliverables, inherits = FALSE)) {
        current <- get(key, envir = private$deliverables, inherits = FALSE)
        if (
          identical(
            tempest_deliverable_fingerprint(current),
            tempest_deliverable_fingerprint(deliverable)
          )
        ) {
          return(invisible(key))
        }
        if (!replace) {
          tempest_artifact_catalog_abort(
            c(
              "Deliverable specification {.val {key}} is already registered with different content.",
              i = "Use a new version or set {.arg replace} to `TRUE` explicitly."
            )
          )
        }
      }
      assign(key, deliverable, envir = private$deliverables)
      invisible(key)
    },

    register_many = function(deliverables, replace = FALSE) {
      deliverables <- tempest_artifact_catalog_deliverables(deliverables)
      replace <- tempest_workflow_flag(replace, "replace")
      for (deliverable in deliverables) {
        self$register(deliverable, replace = replace)
      }
      invisible(self)
    },

    get_deliverable = function(deliverable_id, version = NULL) {
      deliverable_id <- tempest_workflow_scalar(
        deliverable_id,
        "deliverable_id"
      )
      if (!is.null(version)) {
        version <- tempest_workflow_version(version)
        key <- tempest_deliverable_catalog_key(deliverable_id, version)
        if (!exists(key, envir = private$deliverables, inherits = FALSE)) {
          tempest_artifact_catalog_abort(
            "Unknown deliverable specification {.val {key}}."
          )
        }
        return(get(key, envir = private$deliverables, inherits = FALSE))
      }
      prefix <- paste0(deliverable_id, "@")
      keys <- ls(private$deliverables, all.names = TRUE)
      keys <- keys[startsWith(keys, prefix)]
      if (length(keys) == 0L) {
        tempest_artifact_catalog_abort(
          "Unknown deliverable id {.val {deliverable_id}}."
        )
      }
      if (length(keys) > 1L) {
        tempest_artifact_catalog_abort(
          c(
            "Deliverable id {.val {deliverable_id}} has multiple versions.",
            i = "Supply {.arg version} explicitly."
          )
        )
      }
      get(keys[[1]], envir = private$deliverables, inherits = FALSE)
    },

    list_deliverables = function() {
      keys <- sort(ls(private$deliverables, all.names = TRUE))
      stats::setNames(
        lapply(keys, function(key) {
          tempest_deliverable_spec_record(
            get(key, envir = private$deliverables, inherits = FALSE)
          )
        }),
        keys
      )
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
      deliverable_key <- tempest_deliverable_catalog_key(
        artifact@deliverable_id,
        artifact@deliverable_version
      )
      if (
        !exists(
          deliverable_key,
          envir = private$deliverables,
          inherits = FALSE
        )
      ) {
        tempest_artifact_catalog_abort(
          c(
            "Artifact {.val {artifact_id}} references an unregistered deliverable.",
            i = "Register {.val {deliverable_key}} before adding its artifacts."
          )
        )
      }
      deliverable <- get(
        deliverable_key,
        envir = private$deliverables,
        inherits = FALSE
      )
      if (
        !identical(
          artifact@spec_fingerprint,
          tempest_deliverable_fingerprint(deliverable)
        )
      ) {
        tempest_artifact_catalog_abort(
          "Artifact {.val {artifact_id}} has a mismatched deliverable fingerprint."
        )
      }
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
          tempest_artifact_store_write(private$store, artifact),
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
      list(
        schema_version = 1L,
        deliverables = self$list_deliverables(),
        artifacts = self$list(include_content = include_content)
      )
    }
  ),
  private = list(
    artifacts = NULL,
    deliverables = NULL,
    store = NULL
  ),
  cloneable = FALSE
)

#' Create a typed Tempest artifact catalog
#'
#' `r lifecycle::badge("experimental")`
#'
#' This experimental API is frozen and scheduled for removal in Tempest 0.2.0.
#' No compatibility shim is planned; see
#' [tempest-generic-kernel-retirement].
#'
#' The catalog owns typed artifacts for one workflow run. It supports
#' metadata-only listing so host applications do not need to load large
#' artifact content to render an output index.
#'
#' @param store Optional host artifact-store adapter. Catalog writes are
#'   persisted through the adapter before becoming visible in memory.
#' @param artifacts Optional initial list of typed artifacts.
#' @param deliverables Deliverable specifications referenced by `artifacts`.
#' @return A `TempestArtifactCatalog` with `add()`, `get()`, `has()`,
#'   `version()`, `list()`, deliverable-registration, and `snapshot()` methods.
#' @examples
#' spec <- tempest_deliverable_spec(
#'   "brief",
#'   title = "Brief",
#'   purpose = "Summarize the result",
#'   instructions = "Be concise.",
#'   generator_id = "tempest.generator.markdown_report",
#'   renderer_ids = "tempest.renderer.markdown"
#' )
#' catalog <- tempest_artifact_catalog(deliverables = list(spec))
#' artifact <- tempest_artifact(spec, content = "# Brief")
#' catalog$add(artifact)
#' catalog$list()
#' @export
#' @noRd
tempest_artifact_catalog <- function(
  store = NULL,
  artifacts = list(),
  deliverables = list()
) {
  TempestArtifactCatalog$new(
    store = store,
    artifacts = artifacts,
    deliverables = deliverables
  )
}

tempest_artifact_catalog_validate_lineage <- function(
  catalog,
  evidence_store = NULL
) {
  records <- catalog$list()
  artifact_ids <- names(records)
  for (record in records) {
    missing_parents <- setdiff(
      tempest_codec_character(record$parent_artifact_ids),
      artifact_ids
    )
    if (length(missing_parents) > 0L) {
      tempest_artifact_catalog_abort(
        "Artifact {.val {record$artifact_id}} references unknown parent artifact ids: {.val {missing_parents}}."
      )
    }
  }
  if (is.null(evidence_store)) {
    return(invisible(catalog))
  }
  if (!inherits(evidence_store, "ResearchWorkspace")) {
    tempest_artifact_catalog_abort(
      "{.arg evidence_store} must be a ResearchWorkspace."
    )
  }
  source_ids <- purrr::map_chr(
    evidence_store$list_retrieved_resources(),
    tempest_resource_identity
  )
  claim_ids <- purrr::map_chr(
    evidence_store$list_proposed_claims(),
    function(claim) claim@claim_id
  )
  evidence_span_ids <- purrr::map_chr(
    evidence_store$list_evidence_spans(),
    function(span) span@evidence_span_id
  )
  for (record in records) {
    missing_resources <- setdiff(
      tempest_codec_character(record$resource_ids),
      source_ids
    )
    missing_claims <- setdiff(
      tempest_codec_character(record$claim_ids),
      claim_ids
    )
    missing_spans <- setdiff(
      tempest_codec_character(record$evidence_span_ids),
      evidence_span_ids
    )
    if (
      length(missing_resources) > 0L ||
        length(missing_claims) > 0L ||
        length(missing_spans) > 0L
    ) {
      tempest_artifact_catalog_abort(
        "Artifact {.val {record$artifact_id}} contains unresolved evidence lineage."
      )
    }
  }
  invisible(catalog)
}

#' Restore a typed Tempest artifact catalog
#'
#' `r lifecycle::badge("experimental")`
#'
#' This experimental API is frozen and scheduled for removal in Tempest 0.2.0.
#' No compatibility shim is planned; see
#' [tempest-generic-kernel-retirement].
#'
#' Restoration reconstructs every specification and artifact through its
#' validated constructor, verifies fingerprints and content checksums, and
#' optionally verifies evidence lineage against a [ResearchWorkspace]. Runtime
#' store adapters are reattached but are not written during restoration.
#'
#' @param snapshot A catalog snapshot from
#'   `TempestArtifactCatalog$snapshot()`.
#' @param store Optional runtime artifact-store adapter.
#' @param evidence_store Optional [ResearchWorkspace] used to validate
#'   resource, claim, and evidence-span identifiers.
#' @return A restored `TempestArtifactCatalog`.
#' @export
#' @noRd
tempest_artifact_catalog_restore <- function(
  snapshot,
  store = NULL,
  evidence_store = NULL
) {
  if (
    !is.list(snapshot) ||
      !identical(as.integer(snapshot$schema_version %||% NA_integer_), 1L)
  ) {
    tempest_artifact_catalog_abort(
      "Unsupported or malformed artifact catalog snapshot."
    )
  }
  deliverables <- lapply(
    snapshot$deliverables %||% list(),
    tempest_deliverable_spec_from_data
  )
  catalog <- tempest_artifact_catalog(
    store = store,
    deliverables = deliverables
  )
  for (record in snapshot$artifacts %||% list()) {
    deliverable <- catalog$get_deliverable(
      record$deliverable_id,
      record$deliverable_version
    )
    artifact <- tempest_artifact_from_data(record, deliverable)
    catalog$add(artifact, persist = FALSE)
  }
  tempest_artifact_catalog_validate_lineage(catalog, evidence_store)
  catalog
}

# Frozen generic artifact-store adapters

tempest_artifact_store_abort <- function(message, ..., parent = NULL) {
  tempest_abort(
    message,
    ...,
    class = c("tempest_artifact_store_error", "tempest_error"),
    parent = parent,
    .envir = rlang::caller_env()
  )
}

tempest_artifact_store_call <- function(operation, callback) {
  tryCatch(
    callback(),
    error = function(error) {
      if (inherits(error, "tempest_artifact_store_error")) {
        stop(error)
      }
      tempest_artifact_store_abort(
        "Artifact store operation {.val {operation}} failed.",
        parent = error
      )
    }
  )
}

tempest_artifact_store_runtime_value <- function(value) {
  if (
    is.function(value) ||
      is.environment(value) ||
      typeof(value) %in% c("externalptr", "weakref") ||
      inherits(value, "S7_object")
  ) {
    return(TRUE)
  }
  is.list(value) &&
    any(vapply(value, tempest_artifact_store_runtime_value, logical(1)))
}

tempest_artifact_store_validate_listing <- function(value) {
  if (!is.list(value) || is.data.frame(value)) {
    tempest_artifact_store_abort(
      "Artifact store metadata listings must be a named list."
    )
  }
  ids <- names(value)
  if (
    length(value) > 0L &&
      (is.null(ids) ||
        anyNA(ids) ||
        any(!nzchar(ids)) ||
        anyDuplicated(ids))
  ) {
    tempest_artifact_store_abort(
      "Artifact store metadata listings require unique artifact-id names."
    )
  }
  for (artifact_id in ids %||% character()) {
    record <- value[[artifact_id]]
    if (
      !is.list(record) ||
        is.data.frame(record) ||
        !is.null(record$content) ||
        tempest_artifact_store_runtime_value(record)
    ) {
      tempest_artifact_store_abort(
        "Artifact metadata for {.val {artifact_id}} is not a durable content-free record."
      )
    }
    if (
      !is.null(record$artifact_id) &&
        !identical(record$artifact_id, artifact_id)
    ) {
      tempest_artifact_store_abort(
        "Artifact metadata key does not match its artifact id."
      )
    }
  }
  value
}

tempest_artifact_store_validate_read <- function(
  value,
  artifact_id,
  default
) {
  if (is.null(value) || identical(value, default)) {
    return(value)
  }
  if (!S7::S7_inherits(value, TempestArtifact)) {
    tempest_artifact_store_abort(
      "Artifact store reads must return a typed artifact or the supplied default."
    )
  }
  if (!identical(value@artifact_id, artifact_id)) {
    tempest_artifact_store_abort(
      "Artifact store read returned an artifact with a mismatched id."
    )
  }
  value
}

#' Create a Tempest artifact store adapter
#'
#' `r lifecycle::badge("experimental")`
#'
#' This experimental API is frozen and scheduled for removal in Tempest 0.2.0.
#' No compatibility shim is planned; see
#' [tempest-generic-kernel-retirement].
#'
#' Artifact stores let host applications observe or persist typed Tempest
#' outputs without replacing the live in-memory artifact catalog. The default
#' store is a no-op adapter.
#'
#' @param write Function with signature `function(artifact)` used to persist a
#'   typed artifact.
#' @param read Function with signature `function(artifact_id, default)` that
#'   returns a typed artifact.
#' @param list_metadata Function with no arguments that returns a named list of
#'   artifact metadata records without inline content.
#' @param exists Function with signature `function(artifact_id, version)` used
#'   to test artifact identity and optional deliverable version.
#' @param version Function with signature `function(artifact_id, default)` that
#'   returns the persisted deliverable version.
#' @return A typed artifact-store adapter.
#' @examples
#' store <- tempest_memory_artifact_store()
#' spec <- tempest_deliverable_spec(
#'   "report",
#'   title = "Report",
#'   purpose = "Explain the result",
#'   instructions = "Be concise.",
#'   generator_id = "tempest.generator.provided_content",
#'   renderer_ids = "tempest.renderer.markdown"
#' )
#' artifact <- tempest_artifact(spec, content = "# Report")
#' store$write(artifact)
#' store$read(artifact@artifact_id)
#' @export
#' @noRd
tempest_artifact_store <- function(
  write = NULL,
  read = NULL,
  list_metadata = NULL,
  exists = NULL,
  version = NULL
) {
  write_impl <- write %||%
    function(artifact) {
      invisible(artifact@artifact_id)
    }
  read_impl <- read %||%
    function(artifact_id, default = NULL) {
      default
    }
  list_impl <- list_metadata %||%
    function() {
      list()
    }
  exists_impl <- exists %||%
    function(artifact_id, version = NULL) {
      FALSE
    }
  version_impl <- version %||%
    function(artifact_id, default = NULL) {
      artifact <- tempest_artifact_store_validate_read(
        read_impl(artifact_id, default = NULL),
        artifact_id,
        NULL
      )
      if (S7::S7_inherits(artifact, TempestArtifact)) {
        artifact@deliverable_version
      } else {
        default
      }
    }
  for (fn in list(
    write = write_impl,
    read = read_impl,
    list = list_impl,
    exists = exists_impl,
    version = version_impl
  )) {
    if (!is.function(fn)) {
      tempest_abort(
        c(
          "Artifact store entries must be functions.",
          i = "Use {.fn tempest_artifact_store} with function values for its adapter arguments."
        ),
        class = c(
          "tempest_artifact_store_error",
          "tempest_config_error",
          "tempest_error"
        )
      )
    }
  }
  write_fn <- function(artifact) {
    if (!S7::S7_inherits(artifact, TempestArtifact)) {
      tempest_artifact_store_abort(
        "{.arg artifact} must be created by {.fn tempest_artifact}."
      )
    }
    tempest_artifact_store_call("write", function() {
      write_impl(artifact)
    })
    invisible(artifact@artifact_id)
  }
  read_fn <- function(artifact_id, default = NULL) {
    artifact_id <- tempest_workflow_scalar(artifact_id, "artifact_id")
    value <- tempest_artifact_store_call("read", function() {
      read_impl(artifact_id, default = default)
    })
    tempest_artifact_store_validate_read(value, artifact_id, default)
  }
  list_fn <- function() {
    value <- tempest_artifact_store_call("list", list_impl)
    tempest_artifact_store_validate_listing(value)
  }
  exists_fn <- function(artifact_id, version = NULL) {
    artifact_id <- tempest_workflow_scalar(artifact_id, "artifact_id")
    if (!is.null(version)) {
      version <- tempest_workflow_version(version)
    }
    value <- tempest_artifact_store_call("exists", function() {
      exists_impl(artifact_id, version = version)
    })
    if (!is.logical(value) || length(value) != 1L || is.na(value)) {
      tempest_artifact_store_abort(
        "Artifact store exists checks must return `TRUE` or `FALSE`."
      )
    }
    value
  }
  version_fn <- function(artifact_id, default = NULL) {
    artifact_id <- tempest_workflow_scalar(artifact_id, "artifact_id")
    value <- tempest_artifact_store_call("version", function() {
      version_impl(artifact_id, default = default)
    })
    if (is.null(value) || identical(value, default)) {
      return(default)
    }
    tryCatch(
      tempest_workflow_version(value),
      error = function(error) {
        tempest_artifact_store_abort(
          "Artifact store versions must be stable non-empty version strings.",
          parent = error
        )
      }
    )
  }
  structure(
    list(
      write = write_fn,
      read = read_fn,
      list = list_fn,
      exists = exists_fn,
      version = version_fn
    ),
    class = "tempest_artifact_store"
  )
}

#' Create an in-memory Tempest artifact store
#'
#' `r lifecycle::badge("experimental")`
#'
#' This experimental API is frozen and scheduled for removal in Tempest 0.2.0.
#' No compatibility shim is planned; see
#' [tempest-generic-kernel-retirement].
#'
#' This is useful for tests and host apps that want to capture artifacts before
#' deciding where to persist them.
#'
#' @return A `tempest_artifact_store`.
#' @examples
#' store <- tempest_memory_artifact_store()
#' # Stores accept typed artifacts produced by a deliverable lifecycle.
#' store$list()
#' @export
#' @noRd
tempest_memory_artifact_store <- function() {
  artifacts <- new.env(parent = emptyenv())
  tempest_artifact_store(
    write = function(artifact) {
      if (!S7::S7_inherits(artifact, TempestArtifact)) {
        tempest_abort(
          "{.arg artifact} must be created by {.fn tempest_artifact}.",
          class = c("tempest_artifact_store_error", "tempest_error")
        )
      }
      artifacts[[artifact@artifact_id]] <- artifact
      invisible(artifact@artifact_id)
    },
    read = function(artifact_id, default = NULL) {
      artifact <- artifacts[[artifact_id]]
      if (is.null(artifact)) default else artifact
    },
    list_metadata = function() {
      ids <- sort(ls(artifacts, all.names = TRUE))
      stats::setNames(
        lapply(
          ids,
          function(id) {
            tempest_artifact_data(
              artifacts[[id]],
              include_content = FALSE
            )
          }
        ),
        ids
      )
    },
    exists = function(artifact_id, version = NULL) {
      artifact <- artifacts[[artifact_id]]
      if (is.null(artifact)) {
        return(FALSE)
      }
      is.null(version) || identical(artifact@deliverable_version, version)
    },
    version = function(artifact_id, default = NULL) {
      artifact <- artifacts[[artifact_id]]
      if (is.null(artifact)) {
        default
      } else {
        artifact@deliverable_version
      }
    }
  )
}

#' @keywords internal
tempest_artifact_store_write <- function(store, artifact) {
  if (is.null(store)) {
    return(invisible(artifact@artifact_id))
  }
  if (!inherits(store, "tempest_artifact_store")) {
    tempest_abort(
      c(
        "{.arg artifact_store} must be created by {.fn tempest_artifact_store}.",
        i = "Use {.fn tempest_memory_artifact_store} for a simple in-memory adapter."
      ),
      class = c(
        "tempest_artifact_store_error",
        "tempest_config_error",
        "tempest_error"
      )
    )
  }
  if (!S7::S7_inherits(artifact, TempestArtifact)) {
    tempest_abort(
      "{.arg artifact} must be created by {.fn tempest_artifact}.",
      class = c("tempest_artifact_store_error", "tempest_error")
    )
  }
  tryCatch(
    store$write(artifact),
    error = function(error) {
      tempest_abort(
        "Could not persist artifact {.val {artifact@artifact_id}}.",
        class = c("tempest_artifact_store_error", "tempest_error"),
        parent = error
      )
    }
  )
  invisible(artifact@artifact_id)
}

#' @keywords internal
tempest_artifact_store_read <- function(
  store,
  artifact_id,
  default = NULL
) {
  if (!inherits(store, "tempest_artifact_store")) {
    tempest_artifact_store_abort(
      "{.arg store} must be a Tempest artifact store."
    )
  }
  store$read(artifact_id, default = default)
}

#' @keywords internal
tempest_artifact_store_list <- function(store) {
  if (!inherits(store, "tempest_artifact_store")) {
    tempest_artifact_store_abort(
      "{.arg store} must be a Tempest artifact store."
    )
  }
  store$list()
}

#' @keywords internal
tempest_artifact_store_exists <- function(
  store,
  artifact_id,
  version = NULL
) {
  if (!inherits(store, "tempest_artifact_store")) {
    tempest_artifact_store_abort(
      "{.arg store} must be a Tempest artifact store."
    )
  }
  store$exists(artifact_id, version = version)
}

#' @keywords internal
tempest_artifact_store_version <- function(
  store,
  artifact_id,
  default = NULL
) {
  if (!inherits(store, "tempest_artifact_store")) {
    tempest_artifact_store_abort(
      "{.arg store} must be a Tempest artifact store."
    )
  }
  store$version(artifact_id, default = default)
}
