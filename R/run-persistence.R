# STORM run persistence

#' @keywords internal
tempest_topic_slug <- function(topic, max_chars = 80) {
  slug <- tolower(tempest_trim(topic))
  slug <- gsub("[^a-z0-9]+", "-", slug)
  slug <- gsub("^-+|-+$", "", slug)
  if (is.na(slug) || !nzchar(slug)) {
    slug <- "run"
  }
  substr(slug, 1, max_chars)
}

#' @keywords internal
tempest_prepare_run_dir <- function(output_dir, topic, run_id = NULL) {
  if (is.null(output_dir)) {
    return(NULL)
  }
  if (!rlang::is_string(output_dir)) {
    tempest_abort(
      "{.arg output_dir} must be NULL or a single path string."
    )
  }
  tempest_require("jsonlite", "Persisted STORM runs require jsonlite.")

  dir <- path.expand(output_dir)
  run_name <- tempest_topic_slug(run_id %||% topic)
  run_dir <- file.path(dir, run_name)
  dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)
  normalizePath(run_dir, winslash = "/", mustWork = TRUE)
}

#' @keywords internal
tempest_run_artifact_paths <- function(run_dir) {
  list(
    run_config = file.path(run_dir, "run_config.json"),
    workspace = file.path(run_dir, "workspace.json"),
    perspectives = file.path(run_dir, "perspectives.json"),
    experts = file.path(run_dir, "experts.json"),
    sources = file.path(run_dir, "sources.json"),
    claims = file.path(run_dir, "claims.json"),
    citation_audit = file.path(run_dir, "citation_audit.json"),
    draft_outline = file.path(run_dir, "direct_gen_outline.json"),
    outline = file.path(run_dir, "storm_gen_outline.json"),
    lead_section = file.path(run_dir, "lead_section.md"),
    draft_md = file.path(run_dir, "storm_gen_article.md"),
    report_md = file.path(run_dir, "storm_gen_article_polished.md"),
    references = file.path(run_dir, "references.json")
  )
}

#' @keywords internal
tempest_write_json <- function(path, x) {
  tempest_require("jsonlite", "STORM run persistence requires jsonlite.")
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  json <- jsonlite::toJSON(
    x,
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null",
    na = "null"
  )
  tempest_atomic_write_lines(json, path)
  invisible(path)
}

#' @keywords internal
tempest_read_json <- function(path) {
  tempest_require("jsonlite", "STORM run persistence requires jsonlite.")
  tryCatch(
    jsonlite::fromJSON(path, simplifyVector = FALSE),
    error = function(e) {
      tempest_warn(
        "Could not read run artifact {.path {path}} ({conditionMessage(e)}); ignoring it."
      )
      NULL
    }
  )
}

# Condition-class hierarchy for Tempest persistence errors. Every persistence
# failure carries the `tempest_persistence_error` and `tempest_error` base
# classes so callers can catch any save/load failure with a single handler.
# Session persistence errors additionally carry `tempest_session_error`, which
# is shared with non-persistence session errors raised by `TempestSession`.
#' @keywords internal
tempest_persistence_error_class <- function(specific = character()) {
  c(specific, "tempest_persistence_error", "tempest_error")
}

#' @keywords internal
tempest_session_persistence_error_class <- function(specific = character()) {
  c(
    specific,
    "tempest_session_error",
    "tempest_persistence_error",
    "tempest_error"
  )
}

tempest_persistence_schema_version <- function(value, what, class) {
  valid <- is.numeric(value) &&
    length(value) == 1L &&
    !is.na(value) &&
    is.finite(value) &&
    value >= 0 &&
    value == floor(value) &&
    value <= .Machine$integer.max
  if (!valid) {
    tempest_abort(
      "{what} must be one finite scalar whole number.",
      class = class
    )
  }
  as.integer(value)
}

#' @keywords internal
tempest_read_json_strict <- function(
  path,
  what = "JSON file",
  class = tempest_persistence_error_class()
) {
  tempest_require("jsonlite", "Tempest persistence requires jsonlite.")
  if (!file.exists(path)) {
    tempest_abort(
      c("Cannot read {what}.", x = "File does not exist: {.path {path}}."),
      class = class
    )
  }
  tryCatch(
    jsonlite::fromJSON(path, simplifyVector = FALSE),
    error = function(e) {
      tempest_abort(
        c(
          "Cannot read {what}.",
          x = "File is not valid JSON: {.path {path}}."
        ),
        class = class,
        parent = e
      )
    }
  )
}

#' @keywords internal
tempest_env_values <- function(env) {
  ids <- sort(ls(env, all.names = TRUE))
  unname(lapply(ids, function(id) env[[id]]))
}

#' @keywords internal
tempest_env_snapshot <- function(env) {
  ids <- sort(ls(env, all.names = TRUE))
  stats::setNames(lapply(ids, function(id) env[[id]]), ids)
}

#' @keywords internal
tempest_research_workspace_snapshot_abort <- function(message) {
  tempest_abort(
    c("Cannot snapshot ResearchWorkspace.", x = message),
    class = tempest_persistence_error_class(
      "tempest_research_workspace_snapshot_error"
    )
  )
}

#' @keywords internal
tempest_research_workspace_max_sources_data <- function(max_sources) {
  if (is.infinite(max_sources)) {
    return("unbounded")
  }
  as.integer(max_sources)
}

#' @keywords internal
tempest_research_workspace_citation_audit_data <- function(citation_audit) {
  if (is.null(citation_audit)) {
    return(NULL)
  }
  rows <- seq_len(nrow(citation_audit))
  unname(lapply(rows, function(row) {
    stats::setNames(
      lapply(citation_audit, function(column) column[[row]]),
      names(citation_audit)
    )
  }))
}

#' @keywords internal
tempest_research_workspace_snapshot <- function(workspace) {
  if (!inherits(workspace, "ResearchWorkspace")) {
    tempest_research_workspace_snapshot_abort(
      "{.arg workspace} must be a ResearchWorkspace."
    )
  }
  entries <- workspace$retrieved_resources
  typed <- vapply(
    entries,
    \(entry) S7::S7_inherits(entry, TempestResource),
    logical(1)
  )
  resources <- unname(lapply(
    entries[typed],
    tempest_resource_record,
    include_content = TRUE
  ))
  sources <- unname(lapply(entries[!typed], function(source) {
    tryCatch(
      tempest_validate_source(source),
      error = function(error) {
        tempest_research_workspace_snapshot_abort(
          paste0(
            "Raw source entry cannot be represented durably: ",
            conditionMessage(error)
          )
        )
      }
    )
  }))

  list(
    schema_version = 4L,
    base_snapshot_id = workspace$base_snapshot_id,
    max_sources = tempest_research_workspace_max_sources_data(
      workspace$max_sources
    ),
    accepted_graft_references = workspace$list_accepted_graft_references(),
    resources = resources,
    sources = sources,
    proposed_claims = lapply(
      workspace$list_proposed_claims(),
      tempest_claim_to_list
    ),
    evidence_spans = lapply(
      workspace$list_evidence_spans(),
      tempest_evidence_span_to_list
    ),
    disputes = lapply(
      workspace$list_disputes(),
      tempest_dispute_to_list
    ),
    citation_audit = tempest_research_workspace_citation_audit_data(
      workspace$citation_audit
    )
  )
}

#' @keywords internal
tempest_research_workspace_restore_abort <- function(message, parent = NULL) {
  tempest_abort(
    c("Cannot restore ResearchWorkspace snapshot.", x = message),
    class = tempest_persistence_error_class(
      "tempest_research_workspace_restore_error"
    ),
    parent = parent
  )
}

#' @keywords internal
tempest_research_workspace_restore_schema <- function(snapshot) {
  value <- snapshot$schema_version %||% 1L
  if (
    !is.numeric(value) ||
      length(value) != 1L ||
      is.na(value) ||
      !is.finite(value) ||
      value != floor(value) ||
      value > .Machine$integer.max
  ) {
    tempest_research_workspace_restore_abort(
      "The workspace schema version must be one whole number."
    )
  }
  value <- as.integer(value)
  if (!value %in% c(1L, 2L, 3L, 4L)) {
    tempest_research_workspace_restore_abort(
      paste0("Unsupported workspace schema version: ", value, ".")
    )
  }
  value
}

#' @keywords internal
tempest_research_workspace_require_current_schema <- function(
  snapshot,
  what,
  class
) {
  if (!is.list(snapshot) || is.data.frame(snapshot)) {
    tempest_abort(
      "{what} must be a ResearchWorkspace snapshot record.",
      class = class
    )
  }
  schema_version <- tempest_persistence_schema_version(
    snapshot$schema_version %||% NA_integer_,
    paste0(what, " schema version"),
    class
  )
  if (!identical(schema_version, 4L)) {
    tempest_abort(
      "{what} must use ResearchWorkspace snapshot schema version 4.",
      class = class
    )
  }
  invisible(schema_version)
}

#' @keywords internal
tempest_research_workspace_restore_max_sources <- function(value) {
  if (identical(value, "unbounded")) {
    return(Inf)
  }
  if (
    !is.numeric(value) ||
      length(value) != 1L ||
      is.na(value) ||
      !is.finite(value) ||
      value <= 0 ||
      value != floor(value) ||
      value > .Machine$integer.max
  ) {
    tempest_research_workspace_restore_abort(
      "{.field max_sources} must be a positive whole number or {.val unbounded}."
    )
  }
  as.integer(value)
}

#' @keywords internal
tempest_research_workspace_restore_records <- function(
  snapshot,
  field,
  required = FALSE
) {
  if (required && !field %in% names(snapshot)) {
    tempest_research_workspace_restore_abort(
      "Workspace snapshot is missing required field {.field {field}}."
    )
  }
  records <- snapshot[[field]] %||% list()
  if (!is.list(records) || is.data.frame(records)) {
    tempest_research_workspace_restore_abort(
      "Workspace field {.field {field}} must be a list of records."
    )
  }
  records
}

#' @keywords internal
tempest_research_workspace_restore_metadata <- function(
  snapshot,
  schema_version,
  workspace = NULL
) {
  require_fields <- schema_version %in% c(3L, 4L)
  if (require_fields && "artifacts" %in% names(snapshot)) {
    tempest_research_workspace_restore_abort(
      "Current workspaces cannot contain an arbitrary artifacts field."
    )
  }
  if (require_fields && !"base_snapshot_id" %in% names(snapshot)) {
    tempest_research_workspace_restore_abort(
      "Workspace snapshot is missing required field {.field base_snapshot_id}."
    )
  }
  base_snapshot_id <- if (
    !"base_snapshot_id" %in% names(snapshot) && !is.null(workspace)
  ) {
    workspace$base_snapshot_id
  } else {
    tryCatch(
      tempest_research_workspace_snapshot_id(snapshot$base_snapshot_id),
      error = function(error) {
        tempest_research_workspace_restore_abort(
          "{.field base_snapshot_id} is invalid.",
          parent = error
        )
      }
    )
  }

  if (require_fields && !"accepted_graft_references" %in% names(snapshot)) {
    tempest_research_workspace_restore_abort(
      paste0(
        "Workspace snapshot is missing required field ",
        "{.field accepted_graft_references}."
      )
    )
  }
  references <- if (
    !"accepted_graft_references" %in% names(snapshot) && !is.null(workspace)
  ) {
    workspace$list_accepted_graft_references()
  } else {
    tryCatch(
      tempest_research_workspace_references(
        snapshot$accepted_graft_references %||% list()
      ),
      error = function(error) {
        tempest_research_workspace_restore_abort(
          "{.field accepted_graft_references} is invalid.",
          parent = error
        )
      }
    )
  }

  if (require_fields && !"max_sources" %in% names(snapshot)) {
    tempest_research_workspace_restore_abort(
      "Workspace snapshot is missing required field {.field max_sources}."
    )
  }
  max_sources <- if (
    !"max_sources" %in% names(snapshot) && !is.null(workspace)
  ) {
    workspace$max_sources
  } else {
    tempest_research_workspace_restore_max_sources(
      snapshot$max_sources %||% "unbounded"
    )
  }

  list(
    base_snapshot_id = base_snapshot_id,
    max_sources = max_sources,
    accepted_graft_references = references
  )
}

#' @keywords internal
tempest_research_workspace_restore <- function(snapshot, workspace = NULL) {
  if (!is.list(snapshot)) {
    tempest_abort(
      "{.arg snapshot} must be a list.",
      class = tempest_persistence_error_class(
        "tempest_research_workspace_restore_error"
      )
    )
  }
  if (!is.null(workspace) && !inherits(workspace, "ResearchWorkspace")) {
    tempest_abort(
      "{.arg workspace} must be a ResearchWorkspace or `NULL`.",
      class = tempest_persistence_error_class(
        "tempest_research_workspace_restore_error"
      )
    )
  }
  schema_version <- tempest_research_workspace_restore_schema(snapshot)
  if (identical(schema_version, 4L)) {
    expected_fields <- c(
      "schema_version",
      "base_snapshot_id",
      "max_sources",
      "accepted_graft_references",
      "resources",
      "sources",
      "proposed_claims",
      "evidence_spans",
      "disputes",
      "citation_audit"
    )
    snapshot_fields <- names(snapshot)
    if (
      is.null(snapshot_fields) ||
        anyNA(snapshot_fields) ||
        anyDuplicated(snapshot_fields) ||
        !setequal(snapshot_fields, expected_fields)
    ) {
      tempest_research_workspace_restore_abort(
        "Schema 4 workspaces must contain exactly the product workspace fields."
      )
    }
  }
  metadata <- tempest_research_workspace_restore_metadata(
    snapshot,
    schema_version,
    workspace = workspace
  )
  if (is.null(workspace)) {
    workspace <- tempest_research_workspace(
      base_snapshot_id = metadata$base_snapshot_id,
      max_sources = metadata$max_sources,
      accepted_graft_references = metadata$accepted_graft_references
    )
  } else {
    if (!identical(workspace$base_snapshot_id, metadata$base_snapshot_id)) {
      tempest_research_workspace_restore_abort(
        "{.field base_snapshot_id} does not match the pinned workspace."
      )
    }
    if (
      !identical(
        workspace$list_accepted_graft_references(),
        metadata$accepted_graft_references
      )
    ) {
      tempest_research_workspace_restore_abort(
        paste0(
          "{.field accepted_graft_references} do not match the pinned ",
          "workspace."
        )
      )
    }
    tryCatch(
      workspace$set_max_sources(metadata$max_sources),
      error = function(error) {
        tempest_research_workspace_restore_abort(
          "{.field max_sources} cannot be restored into the workspace.",
          parent = error
        )
      }
    )
  }

  if (schema_version %in% c(2L, 3L, 4L)) {
    records <- tempest_research_workspace_restore_records(
      snapshot,
      "resources",
      required = schema_version %in% c(3L, 4L)
    )
    for (i in seq_along(records)) {
      resource <- tryCatch(
        tempest_resource_from_data(records[[i]]),
        error = function(error) {
          tempest_research_workspace_restore_abort(
            paste0("Resource entry ", i, " is invalid."),
            parent = error
          )
        }
      )
      tryCatch(
        workspace$upsert_resource(resource),
        error = function(error) {
          tempest_research_workspace_restore_abort(
            paste0("Resource entry ", i, " cannot be restored."),
            parent = error
          )
        }
      )
    }
  } else {
    snapshot_sources <- tempest_research_workspace_restore_records(
      snapshot,
      "sources"
    )
    # Schema 1 restoration is retained only for existing development bundles.
    for (i in seq_along(snapshot_sources)) {
      source <- snapshot_sources[[i]]
      if (!is.list(source) || is.null(source$url)) {
        tempest_research_workspace_restore_abort(
          paste0("Source entry ", i, " is missing a {.field url}.")
        )
      }
      source$id <- source$id %||% tempest_source_id(source$url)
      source$meta <- source$meta %||% list()
      tryCatch(
        workspace$upsert_source(source),
        error = function(error) {
          tempest_research_workspace_restore_abort(
            paste0("Source entry ", i, " is invalid."),
            parent = error
          )
        }
      )
    }
  }

  if (schema_version %in% c(2L, 3L, 4L)) {
    source_records <- tempest_research_workspace_restore_records(
      snapshot,
      "sources",
      required = schema_version %in% c(3L, 4L)
    )
    resource_ids <- purrr::map_chr(
      workspace$list_resources(),
      tempest_resource_identity
    )
    for (i in seq_along(source_records)) {
      source <- tryCatch(
        tempest_validate_source(source_records[[i]]),
        error = function(error) {
          tempest_research_workspace_restore_abort(
            paste0("Web-source entry ", i, " is invalid."),
            parent = error
          )
        }
      )
      if (
        identical(schema_version, 4L) &&
          source$id %in% resource_ids
      ) {
        tempest_research_workspace_restore_abort(
          paste0(
            "Web-source entry ",
            i,
            " duplicates a typed resource id."
          )
        )
      }
      # Schemas 2 and 3 represented web sources as projections of the typed
      # resource ledger. The resource record remains authoritative when those
      # legacy mirrors overlap; non-overlapping entries are restored so early
      # development bundles using a partitioned representation remain readable.
      if (source$id %in% resource_ids) {
        next
      }
      tryCatch(
        workspace$upsert_source(source),
        error = function(error) {
          tempest_research_workspace_restore_abort(
            paste0("Web-source entry ", i, " cannot be restored."),
            parent = error
          )
        }
      )
    }
  }

  source_ids <- purrr::map_chr(
    workspace$list_resources(),
    tempest_resource_identity
  )
  span_records <- tempest_research_workspace_restore_records(
    snapshot,
    "evidence_spans",
    required = schema_version %in% c(3L, 4L)
  )
  for (i in seq_along(span_records)) {
    span <- tryCatch(
      tempest_evidence_span_from_list(span_records[[i]]),
      error = function(error) {
        tempest_research_workspace_restore_abort(
          paste0("Evidence-span entry ", i, " is invalid."),
          parent = error
        )
      }
    )
    if (!(span@source_id %in% source_ids)) {
      tempest_research_workspace_restore_abort(
        paste0(
          "Evidence span ",
          span@evidence_span_id,
          " cites unknown source id: ",
          span@source_id,
          "."
        )
      )
    }
    workspace$add_evidence_span(span)
  }

  claim_field <- if (schema_version %in% c(3L, 4L)) {
    "proposed_claims"
  } else {
    "claims"
  }
  claim_records <- tempest_research_workspace_restore_records(
    snapshot,
    claim_field,
    required = schema_version %in% c(3L, 4L)
  )
  for (i in seq_along(claim_records)) {
    claim <- tryCatch(
      tempest_claim_from_list(claim_records[[i]]),
      error = function(error) {
        tempest_research_workspace_restore_abort(
          paste0("Proposed-claim entry ", i, " is invalid."),
          parent = error
        )
      }
    )
    missing_source_ids <- setdiff(claim@source_ids, source_ids)
    if (length(missing_source_ids) > 0) {
      tempest_research_workspace_restore_abort(
        paste0(
          "Claim ",
          claim@claim_id,
          " cites unknown source id(s): ",
          paste(missing_source_ids, collapse = ", "),
          "."
        )
      )
    }
    tryCatch(
      workspace$add_proposed_claim(claim),
      error = function(error) {
        tempest_research_workspace_restore_abort(
          paste0("Proposed claim ", claim@claim_id, " cannot be restored."),
          parent = error
        )
      }
    )
  }

  claim_ids <- purrr::map_chr(
    workspace$list_proposed_claims(),
    ~ .x@claim_id
  )
  dispute_records <- tempest_research_workspace_restore_records(
    snapshot,
    "disputes",
    required = schema_version %in% c(3L, 4L)
  )
  for (i in seq_along(dispute_records)) {
    dispute <- tryCatch(
      tempest_dispute_from_list(dispute_records[[i]]),
      error = function(error) {
        tempest_research_workspace_restore_abort(
          paste0("Dispute entry ", i, " is invalid."),
          parent = error
        )
      }
    )
    missing_claim_ids <- setdiff(dispute@claim_ids, claim_ids)
    if (length(missing_claim_ids) > 0) {
      tempest_research_workspace_restore_abort(
        paste0(
          "Dispute ",
          dispute@dispute_id,
          " cites unknown claim id(s): ",
          paste(missing_claim_ids, collapse = ", "),
          "."
        )
      )
    }
    workspace$add_dispute(dispute)
  }

  citation_audit <- if (schema_version %in% c(3L, 4L)) {
    snapshot$citation_audit
  } else if (identical(schema_version, 2L)) {
    if (!is.null(snapshot$artifacts) && !is.list(snapshot$artifacts)) {
      tempest_research_workspace_restore_abort(
        "Legacy {.field artifacts} must be a list."
      )
    }
    snapshot$citation_audit %||%
      (snapshot$artifacts %||% list())$citation_audit
  } else {
    NULL
  }
  if (!is.null(citation_audit)) {
    audit <- tryCatch(
      tempest_restore_citation_audit(citation_audit),
      error = function(error) {
        tempest_research_workspace_restore_abort(
          "{.field citation_audit} is invalid.",
          parent = error
        )
      }
    )
    tryCatch(
      workspace$set_citation_audit(audit),
      error = function(error) {
        tempest_research_workspace_restore_abort(
          "{.field citation_audit} cannot be restored.",
          parent = error
        )
      }
    )
  }

  workspace
}

#' @keywords internal
tempest_source_store_snapshot <- function(store, artifacts = NULL) {
  stopifnot(inherits(store, "SourceStore"))
  if (!is.null(artifacts) && !is.character(artifacts)) {
    tempest_abort("{.arg artifacts} must be NULL or a character vector.")
  }

  workspace <- tempest_research_workspace_snapshot(store)
  artifact_names <- artifacts %||% sort(ls(store$artifacts, all.names = TRUE))
  artifact_names <- artifact_names[vapply(
    artifact_names,
    exists,
    logical(1),
    envir = store$artifacts,
    inherits = FALSE
  )]

  list(
    schema_version = 2L,
    base_snapshot_id = workspace$base_snapshot_id,
    max_sources = workspace$max_sources,
    accepted_graft_references = workspace$accepted_graft_references,
    resources = workspace$resources,
    sources = workspace$sources,
    claims = workspace$proposed_claims,
    evidence_spans = workspace$evidence_spans,
    disputes = workspace$disputes,
    artifacts = stats::setNames(
      lapply(artifact_names, function(name) store$get_artifact(name)),
      artifact_names
    )
  )
}

#' @keywords internal
tempest_source_store_restore_abort <- function(message, parent = NULL) {
  tempest_abort(
    c("Cannot restore SourceStore snapshot.", x = message),
    class = tempest_persistence_error_class(
      "tempest_source_store_restore_error"
    ),
    parent = parent
  )
}

#' @keywords internal
tempest_source_store_restore <- function(snapshot, store = NULL) {
  if (!is.list(snapshot)) {
    tempest_abort(
      "{.arg snapshot} must be a list.",
      class = tempest_persistence_error_class(
        "tempest_source_store_restore_error"
      )
    )
  }
  if (!is.null(store) && !inherits(store, "SourceStore")) {
    tempest_abort(
      "{.arg store} must be a SourceStore or `NULL`.",
      class = tempest_persistence_error_class(
        "tempest_source_store_restore_error"
      )
    )
  }
  store_schema <- snapshot$schema_version %||% 1L
  if (
    !is.numeric(store_schema) ||
      length(store_schema) != 1L ||
      is.na(store_schema) ||
      !is.finite(store_schema) ||
      store_schema != floor(store_schema) ||
      store_schema > .Machine$integer.max ||
      !as.integer(store_schema) %in% c(1L, 2L)
  ) {
    tempest_source_store_restore_abort(
      paste0("Unsupported store schema version: ", store_schema, ".")
    )
  }

  if (is.null(store)) {
    metadata <- tryCatch(
      tempest_research_workspace_restore_metadata(
        snapshot,
        as.integer(store_schema)
      ),
      error = function(error) {
        tempest_source_store_restore_abort(
          conditionMessage(error),
          parent = error
        )
      }
    )
    store <- suppressWarnings(SourceStore$new(
      max_sources = metadata$max_sources,
      base_snapshot_id = metadata$base_snapshot_id,
      accepted_graft_references = metadata$accepted_graft_references
    ))
  }
  store <- tryCatch(
    tempest_research_workspace_restore(snapshot, workspace = store),
    error = function(error) {
      tempest_source_store_restore_abort(
        conditionMessage(error),
        parent = error
      )
    }
  )

  artifacts <- snapshot$artifacts %||% list()
  if (!is.list(artifacts) || is.data.frame(artifacts)) {
    tempest_source_store_restore_abort(
      "Legacy {.field artifacts} must be a named list."
    )
  }
  for (name in names(artifacts)) {
    store$set_artifact(name, artifacts[[name]])
  }

  store
}

#' @keywords internal
tempest_session_restore_abort <- function(message) {
  tempest_abort(
    c("Cannot restore TempestSession snapshot.", x = message),
    class = tempest_session_persistence_error_class(
      "tempest_session_restore_error"
    )
  )
}

#' @keywords internal
tempest_expert_records <- function(experts) {
  experts <- tempest_validate_experts(experts, active_only = FALSE)
  unname(lapply(experts, tempest_expert_profile_record))
}

#' @keywords internal
tempest_experts_from_records <- function(
  records,
  what = "expert profiles",
  class = tempest_persistence_error_class()
) {
  if (!is.list(records) || is.data.frame(records)) {
    tempest_abort(
      "Cannot restore {what}; expected a list of expert-profile records.",
      class = class
    )
  }
  tryCatch(
    {
      experts <- lapply(records, tempest_expert_profile_from_data)
      tempest_validate_experts(experts, active_only = FALSE)
    },
    error = function(error) {
      tempest_abort(
        "Cannot restore {what}; an expert-profile record is invalid.",
        class = class,
        parent = error
      )
    }
  )
}

#' @keywords internal
tempest_session_runtime_records <- function(runtime) {
  if (!inherits(runtime, "TempestRuntime")) {
    tempest_abort(
      "{.arg runtime} must be created by {.fn tempest_runtime}.",
      class = tempest_session_persistence_error_class(
        "tempest_session_snapshot_error"
      )
    )
  }
  list(
    skills = unname(runtime$skills$list()),
    connection_refs = unname(runtime$connections$list())
  )
}

#' @keywords internal
tempest_session_verify_runtime_records <- function(
  records,
  runtime_records,
  decoder,
  id_property,
  type
) {
  if (!is.list(records) || is.data.frame(records)) {
    tempest_session_restore_abort(
      paste0("Snapshot ", type, " records must be a list.")
    )
  }
  restored <- tryCatch(
    lapply(records, decoder),
    error = function(error) {
      tempest_session_restore_abort(
        paste0("Snapshot contains an invalid ", type, " record.")
      )
    }
  )
  ids <- vapply(restored, \(value) S7::prop(value, id_property), character(1))
  if (anyDuplicated(ids)) {
    tempest_session_restore_abort(
      paste0("Snapshot contains duplicate ", type, " ids.")
    )
  }
  runtime_records <- runtime_records %||% list()
  for (i in seq_along(records)) {
    id <- ids[[i]]
    runtime_record <- runtime_records[[id]] %||% NULL
    if (is.null(runtime_record)) {
      tempest_session_restore_abort(
        paste0(
          "Runtime does not provide the saved ",
          type,
          " ",
          id,
          "."
        )
      )
    }
    if (
      !identical(records[[i]]$version, runtime_record$version) ||
        !identical(records[[i]]$fingerprint, runtime_record$fingerprint)
    ) {
      tempest_session_restore_abort(
        paste0(
          "Runtime ",
          type,
          " ",
          id,
          " does not match the saved version and fingerprint."
        )
      )
    }
  }
  invisible(restored)
}

#' @keywords internal
tempest_session_connection_permissions <- function(
  connection_permissions,
  runtime,
  action = c("snapshot", "restore")
) {
  action <- match.arg(action)
  abort_permissions <- function(message, parent = NULL) {
    if (identical(action, "restore")) {
      tempest_session_restore_abort(message)
    }
    tempest_abort(
      message,
      class = tempest_session_persistence_error_class(
        "tempest_session_snapshot_error"
      ),
      parent = parent
    )
  }
  connection_permissions <- connection_permissions %||% list()
  if (
    !is.list(connection_permissions) ||
      is.data.frame(connection_permissions) ||
      (length(connection_permissions) > 0L &&
        (is.null(names(connection_permissions)) ||
          any(!nzchar(names(connection_permissions))) ||
          anyDuplicated(names(connection_permissions))))
  ) {
    abort_permissions(
      "Connection permissions must be a uniquely named list."
    )
  }
  permissions <- tryCatch(
    stats::setNames(
      lapply(connection_permissions, function(ids) {
        tempest_contract_ids(
          unname(unlist(ids, use.names = FALSE)),
          "connection_permissions"
        )
      }),
      names(connection_permissions)
    ),
    error = function(error) {
      abort_permissions(
        "Connection permissions contain an invalid connection id.",
        parent = error
      )
    }
  )
  runtime_connection_ids <- names(runtime$connections$list())
  requested <- unique(unlist(permissions, use.names = FALSE))
  missing <- setdiff(requested, runtime_connection_ids)
  if (length(missing) > 0L) {
    abort_permissions(
      paste0(
        "Runtime does not provide permitted connection ",
        missing[[1]],
        "."
      )
    )
  }
  permissions
}

#' @keywords internal
tempest_session_restore_connection_permissions <- function(
  saved_permissions,
  connection_permissions,
  runtime
) {
  saved_permissions <- tempest_session_connection_permissions(
    saved_permissions,
    runtime,
    action = "restore"
  )
  if (is.null(connection_permissions)) {
    return(saved_permissions)
  }

  connection_permissions <- tempest_session_connection_permissions(
    connection_permissions,
    runtime,
    action = "restore"
  )
  added_contexts <- setdiff(
    names(connection_permissions),
    names(saved_permissions)
  )
  if (length(added_contexts) > 0L) {
    tempest_session_restore_abort(
      paste0(
        "Connection-permission override adds unsaved context ",
        added_contexts[[1]],
        "."
      )
    )
  }
  for (context_id in names(connection_permissions)) {
    added_connections <- setdiff(
      connection_permissions[[context_id]],
      saved_permissions[[context_id]]
    )
    if (length(added_connections) > 0L) {
      tempest_session_restore_abort(
        paste0(
          "Connection-permission override adds unsaved connection ",
          added_connections[[1]],
          " for context ",
          context_id,
          "."
        )
      )
    }
  }

  connection_permissions
}

#' @keywords internal
tempest_capability_grants_record <- function(grants, action = "snapshot") {
  tryCatch(
    tempest_contract_serializable_list(
      grants %||% list(),
      "capability_grants"
    ),
    error = function(error) {
      if (identical(action, "restore")) {
        tempest_session_restore_abort(
          "Saved capability grants are not serializable grant records."
        )
      }
      tempest_abort(
        "Cannot snapshot non-serializable capability grants.",
        class = tempest_session_persistence_error_class(
          "tempest_session_snapshot_error"
        ),
        parent = error
      )
    }
  )
}

#' @keywords internal
tempest_expert_sessions_snapshot <- function(session) {
  manager <- session$expert_session_manager
  if (is.null(manager)) {
    return(list())
  }

  session_ids <- sort(manager$list_sessions())
  lapply(session_ids, function(session_id) {
    binding <- manager$session_profile(session_id)
    tempest_contract_serializable_list(
      list(
        session_id = binding$session_id,
        expert_id = binding$expert_id,
        expert_version = binding$expert_version,
        expert_fingerprint = binding$expert_fingerprint,
        model_role = binding$model_role,
        allowed_connection_ref_ids = binding$allowed_connection_ref_ids %||%
          character(),
        grants = binding$grants %||% list(),
        created_at = binding$created_at
      ),
      "expert_session"
    )
  })
}

#' @keywords internal
tempest_config_snapshot <- function(config) {
  list(
    models = config@models,
    search_provider = config@search_provider,
    cache_dir = config@cache_dir,
    cache_enabled = config@cache_enabled,
    cache_ttl = config@cache_ttl,
    max_search_results = config@max_search_results,
    max_search_queries_per_turn = config@max_search_queries_per_turn,
    retrieve_top_k = config@retrieve_top_k,
    max_sources = config@max_sources,
    user_agent = config@user_agent,
    node_expansion_trigger_count = config@node_expansion_trigger_count,
    enable_discourse_manager = config@enable_discourse_manager,
    max_active_experts = config@max_active_experts,
    enable_unseen_surfacing = config@enable_unseen_surfacing,
    citation_policy = config@citation_policy,
    min_support_score = config@min_support_score,
    on_unsupported_claim = config@on_unsupported_claim
  )
}

#' @keywords internal
tempest_session_snapshot_record <- function(value, field) {
  tryCatch(
    tempest_contract_serializable_list(value %||% list(), field),
    error = function(error) {
      tempest_abort(
        "Cannot snapshot non-serializable {.field {field}} state.",
        class = tempest_session_persistence_error_class(
          "tempest_session_snapshot_error"
        ),
        parent = error
      )
    }
  )
}

#' @keywords internal
tempest_session_suggested_questions <- function(value, action = "snapshot") {
  value <- value %||% character()
  if (is.list(value) && is.null(names(value))) {
    valid <- vapply(
      value,
      \(question) {
        is.character(question) &&
          length(question) == 1L &&
          !is.na(question)
      },
      logical(1)
    )
    if (all(valid)) {
      value <- unlist(value, use.names = FALSE)
    }
  }
  if (!is.character(value) || anyNA(value)) {
    message <- "Suggested questions must be a character vector without missing values."
    if (identical(action, "restore")) {
      tempest_session_restore_abort(message)
    }
    tempest_abort(
      message,
      class = tempest_session_persistence_error_class(
        "tempest_session_snapshot_error"
      )
    )
  }
  unname(value)
}

#' Snapshot a Co-STORM session
#'
#' `r lifecycle::badge("experimental")`
#'
#' `tempest_session_snapshot()` returns a structured, in-memory representation
#' of the durable state in a [TempestSession]. It includes the research
#' manifest; fixed session and configuration identity; the authoritative
#' [ResearchWorkspace]; expert profiles; transcript and mind map; typed
#' deliverables; progress-event and expert-session metadata; serializable
#' runtime records; and any attached generic workflow run. The deprecated
#' `store` alias is not duplicated; restoration binds it to the restored
#' workspace. Live chat handles, runtime clients, tools, closures, Shiny
#' reactive state, credentials, and provider request bodies are not included.
#'
#' Use [tempest_session_restore()] to rebuild a session from the returned list,
#' or [tempest_session_save()] to write the same durable state to a directory
#' bundle.
#'
#' @param session A [TempestSession] object.
#' @return A list containing a schema-versioned session snapshot.
#' @export
tempest_session_snapshot <- function(session) {
  if (!inherits(session, "TempestSession")) {
    tempest_abort(
      "{.arg session} must be a {.cls TempestSession} object.",
      class = tempest_session_persistence_error_class(
        "tempest_session_snapshot_error"
      )
    )
  }
  research_manifest <- tryCatch(
    tempest_costorm_manifest_validate(
      session$manifest,
      session$session_id,
      session$config,
      session$workspace
    ),
    error = function(error) {
      tempest_abort(
        "Cannot snapshot an inconsistent Co-STORM research manifest.",
        class = tempest_session_persistence_error_class(
          "tempest_session_snapshot_error"
        ),
        parent = error
      )
    }
  )
  workspace <- tempest_research_workspace_snapshot(session$workspace)
  suggested_questions <- tempest_session_suggested_questions(
    session$artifacts[["suggested_questions"]] %||% character()
  )
  runtime_records <- tempest_session_runtime_records(session$runtime)

  list(
    schema_version = 5L,
    package_version = tryCatch(
      as.character(utils::packageVersion("tempest")),
      error = function(e) NA_character_
    ),
    research_manifest = tempest_research_manifest_record(research_manifest),
    topic = session$topic,
    title = session$title,
    session_id = session$session_id,
    config = tempest_config_snapshot(session$config),
    experts = tempest_expert_records(session$experts),
    skills = runtime_records$skills,
    connection_refs = runtime_records$connection_refs,
    connection_permissions = tempest_session_connection_permissions(
      session$connection_permissions,
      session$runtime
    ),
    capability_grants = tempest_capability_grants_record(
      session$capability_grants
    ),
    transcript = tempest_session_snapshot_record(
      session$transcript,
      "transcript"
    ),
    mindmap = tempest_session_snapshot_record(session$mindmap, "mindmap"),
    artifact_catalog = session$artifact_catalog$snapshot(
      include_content = TRUE
    ),
    suggested_questions = suggested_questions,
    progress_events = tempest_execution_events(session),
    workspace = workspace,
    expert_sessions = tempest_expert_sessions_snapshot(session),
    workflow_run = if (inherits(session$workflow_run, "TempestRun")) {
      tempest_run_snapshot(session$workflow_run)
    } else {
      NULL
    }
  )
}

#' @keywords internal
tempest_session_snapshot_translate_v4 <- function(snapshot, config) {
  schema_version <- tempest_persistence_schema_version(
    snapshot$schema_version %||% NA_integer_,
    "Legacy session snapshot schema version",
    tempest_session_persistence_error_class(
      "tempest_session_restore_error"
    )
  )
  if (!identical(schema_version, 4L)) {
    tempest_session_restore_abort(
      "The legacy session translator accepts only schema version 4."
    )
  }
  session_id <- snapshot$session_id %||% NULL
  if (!rlang::is_string(session_id) || !nzchar(tempest_trim(session_id))) {
    tempest_session_restore_abort(
      "Legacy schema 4 snapshot must include a non-empty session id."
    )
  }
  legacy_store <- snapshot$store %||% NULL
  if (!is.list(legacy_store) || is.data.frame(legacy_store)) {
    tempest_session_restore_abort(
      "Legacy schema 4 snapshot must include a SourceStore record."
    )
  }
  legacy_artifacts <- legacy_store$artifacts %||% list()
  if (!is.list(legacy_artifacts) || is.data.frame(legacy_artifacts)) {
    tempest_session_restore_abort(
      "Legacy schema 4 SourceStore artifacts must be a list."
    )
  }
  legacy_store$citation_audit <- legacy_store$citation_audit %||%
    legacy_artifacts$citation_audit
  legacy_store$artifacts <- NULL
  workspace <- tryCatch(
    tempest_research_workspace_restore(legacy_store),
    error = function(error) {
      tempest_session_restore_abort(
        paste0(
          "Legacy schema 4 SourceStore state could not be translated: ",
          conditionMessage(error)
        )
      )
    }
  )
  workspace_snapshot <- tempest_research_workspace_snapshot(workspace)
  legacy_session_artifacts <- snapshot$artifacts %||% list()
  if (
    !is.list(legacy_session_artifacts) ||
      is.data.frame(legacy_session_artifacts)
  ) {
    tempest_session_restore_abort(
      "Legacy schema 4 session artifacts must be a list."
    )
  }
  manifest <- tryCatch(
    tempest_research_manifest(
      research_run_id = tempest_trim(session_id),
      mode = "costorm",
      config = config,
      programs = list(),
      knowledge_snapshot = tempest_costorm_manifest_snapshot_reference(
        workspace
      ),
      runtime = list(),
      traces = list(),
      deliverables = list(),
      status = "running"
    ),
    error = function(error) {
      tempest_session_restore_abort(
        paste0(
          "Legacy schema 4 research identity could not be translated: ",
          conditionMessage(error)
        )
      )
    }
  )

  list(
    schema_version = 5L,
    package_version = snapshot$package_version %||% NA_character_,
    research_manifest = tempest_research_manifest_record(manifest),
    topic = snapshot$topic,
    title = snapshot$title,
    session_id = tempest_trim(session_id),
    config = snapshot$config %||% list(),
    experts = snapshot$experts %||% list(),
    skills = snapshot$skills %||% list(),
    connection_refs = snapshot$connection_refs %||% list(),
    connection_permissions = snapshot$connection_permissions %||% list(),
    capability_grants = snapshot$capability_grants %||% list(),
    transcript = snapshot$transcript %||% list(),
    mindmap = snapshot$mindmap,
    artifact_catalog = snapshot$artifact_catalog,
    suggested_questions = snapshot$suggested_questions %||%
      legacy_session_artifacts$suggested_questions %||%
      character(),
    progress_events = snapshot$progress_events %||%
      legacy_session_artifacts$progress_events %||%
      list(),
    workspace = workspace_snapshot,
    expert_sessions = snapshot$expert_sessions %||% list(),
    workflow_run = snapshot$workflow_run %||% NULL
  )
}

#' @keywords internal
tempest_session_restore_expert_sessions <- function(session, expert_sessions) {
  for (expert_session in expert_sessions %||% list()) {
    required <- c(
      "session_id",
      "expert_id",
      "expert_version",
      "expert_fingerprint",
      "grants"
    )
    if (
      !is.list(expert_session) ||
        is.data.frame(expert_session) ||
        length(setdiff(required, names(expert_session))) > 0L
    ) {
      tempest_session_restore_abort(
        "Snapshot contains malformed expert-session metadata."
      )
    }
    expert_ids <- vapply(
      session$experts,
      \(expert) expert@expert_id,
      character(1)
    )
    idx <- match(expert_session$expert_id, expert_ids)
    if (is.na(idx)) {
      tempest_session_restore_abort(
        paste0(
          "Expert session ",
          expert_session$session_id,
          " does not match a restored expert."
        )
      )
    }
    expert <- session$experts[[idx]]
    if (
      !identical(expert@version, expert_session$expert_version) ||
        !identical(
          tempest_expert_profile_fingerprint(expert),
          expert_session$expert_fingerprint
        )
    ) {
      tempest_session_restore_abort(
        paste0(
          "Expert session ",
          expert_session$session_id,
          " does not match the restored expert version and fingerprint."
        )
      )
    }
    tempest_capability_grants_record(
      expert_session$grants,
      action = "restore"
    )
    tryCatch(
      session$expert_session_manager$restore_session(expert_session),
      error = function(error) {
        tempest_session_restore_abort(
          paste0(
            "Expert session ",
            expert_session$session_id,
            " could not be rehydrated through the supplied runtime."
          )
        )
      }
    )
    restored <- session$expert_session_manager$session_profile(
      expert_session$session_id
    )
    for (field in c(
      "session_id",
      "expert_id",
      "expert_version",
      "expert_fingerprint"
    )) {
      if (!identical(restored[[field]], expert_session[[field]])) {
        tempest_session_restore_abort(
          paste0(
            "Rehydrated expert session ",
            expert_session$session_id,
            " changed its pinned profile binding."
          )
        )
      }
    }
  }
  invisible(session)
}

#' @keywords internal
tempest_session_restore_workflow_run <- function(session, snapshot) {
  workflow_snapshot <- snapshot$workflow_run %||% NULL
  if (is.null(workflow_snapshot)) {
    return(invisible(session))
  }
  workflow_snapshot <- tempest_generic_run_snapshot_from_json(
    workflow_snapshot
  )
  workflow_id <- workflow_snapshot$workflow$workflow_id %||% NULL
  if (!identical(workflow_id, "tempest.costorm")) {
    tempest_session_restore_abort(
      "Attached workflow state must use the built-in Co-STORM specification."
    )
  }
  options <- workflow_snapshot$objective$metadata$costorm_options %||% list()
  adapter <- tryCatch(
    tempest_costorm_workflow_adapter(
      session = session,
      style = options$style %||% "technical",
      include_references = options$include_references %||% TRUE,
      reorganize = options$reorganize %||% TRUE,
      verbose = options$verbose %||% TRUE
    ),
    error = function(error) {
      tempest_session_restore_abort(
        paste0(
          "Attached Co-STORM workflow options are invalid: ",
          conditionMessage(error)
        )
      )
    }
  )
  registry <- tempest_builtin_workflow_operation_registry(
    costorm_adapter = adapter
  )
  workflow_runtime <- tempest_builtin_workflow_runtime(
    session$runtime,
    registry
  )
  run <- tryCatch(
    tempest_run_restore(
      workflow_snapshot,
      runtime = workflow_runtime,
      artifact_catalog = session$artifact_catalog,
      source_store = session$store,
      runtime_context = list(
        config = session$config,
        retriever = session$retriever,
        expert_session_manager = session$expert_session_manager,
        topic = session$topic
      ),
      connection_permissions = session$connection_permissions
    ),
    error = function(error) {
      tempest_session_restore_abort(
        paste0(
          "Attached Co-STORM workflow state could not be restored: ",
          conditionMessage(error)
        )
      )
    }
  )
  session$workflow_run <- run
  invisible(session)
}

#' Restore a Co-STORM session from a snapshot
#'
#' `r lifecycle::badge("experimental")`
#'
#' `tempest_session_restore()` rebuilds a [TempestSession] from a structured
#' snapshot created by [tempest_session_snapshot()] or read by
#' [tempest_session_resume()]. It restores the research manifest and
#' authoritative workspace, reestablishes the `store` compatibility alias, and
#' creates fresh chat/tool handles using the supplied runtime and `config`.
#'
#' Historical progress events are restored as session artifact data and can be
#' reduced with [tempest_progress_state()]. They are not replayed into the new
#' `progress` callback; future calls on the restored session use that callback.
#'
#' @param snapshot A list from [tempest_session_snapshot()].
#' @param config Runtime [TempestConfig] used to recreate chats, retrievers, and
#'   tools. Functions, credentials, and host-specific stores should be supplied
#'   here rather than read from the snapshot.
#' @param runtime A fresh [tempest_runtime()] supplying process-local skill,
#'   capability, and connection implementations.
#' @param connection_permissions Optional named connection-permission override
#'   that may only remove saved contexts or connection ids. When `NULL`, the
#'   saved opaque connection ids are reused.
#' @param progress Optional callback for future `tempest_progress_event`
#'   objects.
#' @return A restored [TempestSession].
#' @export
tempest_session_restore <- function(
  snapshot,
  config = tempest_config(),
  runtime = tempest_runtime(),
  connection_permissions = NULL,
  progress = NULL
) {
  if (!is.list(snapshot)) {
    tempest_session_restore_abort("{.arg snapshot} must be a list.")
  }
  schema_version <- tempest_persistence_schema_version(
    snapshot$schema_version %||% NA_integer_,
    "Session snapshot schema version",
    tempest_session_persistence_error_class(
      "tempest_session_restore_error"
    )
  )
  if (identical(schema_version, 4L)) {
    snapshot <- tempest_session_snapshot_translate_v4(snapshot, config)
    schema_version <- tempest_persistence_schema_version(
      snapshot$schema_version %||% NA_integer_,
      "Translated session snapshot schema version",
      tempest_session_persistence_error_class(
        "tempest_session_restore_error"
      )
    )
  }
  if (!identical(schema_version, 5L)) {
    tempest_session_restore_abort(
      paste0("Unsupported snapshot schema version: ", schema_version, ".")
    )
  }
  if (any(c("artifacts", "store") %in% names(snapshot))) {
    tempest_session_restore_abort(
      "Schema 5 session snapshots cannot contain legacy arbitrary artifact or store fields."
    )
  }
  tempest_research_workspace_require_current_schema(
    snapshot$workspace,
    "Schema 5 session workspace",
    tempest_session_persistence_error_class(
      "tempest_session_restore_error"
    )
  )
  if (
    !rlang::is_string(snapshot$topic) || !nzchar(tempest_trim(snapshot$topic))
  ) {
    tempest_session_restore_abort("Snapshot must include a non-empty topic.")
  }
  if (
    !rlang::is_string(snapshot$session_id) ||
      !nzchar(tempest_trim(snapshot$session_id))
  ) {
    tempest_session_restore_abort(
      "Snapshot must include a non-empty session id."
    )
  }
  research_manifest <- tryCatch(
    tempest_research_manifest_from_record(snapshot$research_manifest),
    error = function(error) {
      tempest_session_restore_abort(
        paste0(
          "Snapshot contains an invalid research manifest: ",
          conditionMessage(error)
        )
      )
    }
  )
  if (!identical(research_manifest@research_run_id, snapshot$session_id)) {
    tempest_session_restore_abort(
      "Snapshot session id does not match its research manifest run id."
    )
  }
  workspace <- tryCatch(
    tempest_research_workspace_restore(snapshot$workspace),
    error = function(error) {
      tempest_session_restore_abort(
        paste0(
          "Snapshot contains an invalid ResearchWorkspace: ",
          conditionMessage(error)
        )
      )
    }
  )
  tryCatch(
    tempest_costorm_manifest_validate(
      research_manifest,
      snapshot$session_id,
      config,
      workspace
    ),
    error = function(error) {
      tempest_session_restore_abort(
        paste0(
          "Snapshot research identity does not match the restore inputs: ",
          conditionMessage(error)
        )
      )
    }
  )

  experts <- tempest_experts_from_records(
    snapshot$experts %||% list(),
    what = "session expert profiles",
    class = tempest_session_persistence_error_class(
      "tempest_session_restore_error"
    )
  )
  if (!inherits(runtime, "TempestRuntime")) {
    tempest_session_restore_abort(
      "{.arg runtime} must be created by {.fn tempest_runtime}."
    )
  }
  tempest_session_verify_runtime_records(
    snapshot$skills %||% list(),
    runtime$skills$list(),
    tempest_skill_from_data,
    "skill_id",
    "skill"
  )
  tempest_session_verify_runtime_records(
    snapshot$connection_refs %||% list(),
    runtime$connections$list(),
    tempest_connection_ref_from_data,
    "connection_id",
    "connection reference"
  )
  connection_permissions <- tempest_session_restore_connection_permissions(
    snapshot$connection_permissions %||% list(),
    connection_permissions,
    runtime
  )
  capability_grants <- tempest_capability_grants_record(
    snapshot$capability_grants %||% list(),
    action = "restore"
  )
  retriever <- tempest_retriever(config = config, store = workspace)
  session <- tempest_session_restore_new(
    topic = snapshot$topic,
    config = config,
    runtime = runtime,
    experts = experts,
    connection_permissions = connection_permissions,
    retriever = retriever,
    progress = NULL,
    session_id = snapshot$session_id,
    manifest = research_manifest
  )

  session$title <- snapshot$title %||% session$topic
  session$capability_grants <- capability_grants
  session$progress <- tempest_progress_callback(progress)
  session$expert_session_manager$run_id <- session$session_id
  session$expert_session_manager$progress <- function(event) {
    session$record_progress_event(event)
  }
  session$transcript <- snapshot$transcript %||% list()
  session$mindmap <- snapshot$mindmap %||% tempest_mindmap_init(session$topic)
  session$artifacts <- new.env(parent = emptyenv())
  session$artifact_catalog <- tempest_artifact_catalog_restore(
    snapshot$artifact_catalog %||%
      list(
        schema_version = 1L,
        deliverables = list(),
        artifacts = list()
      ),
    store = config@artifact_store,
    evidence_store = workspace
  )

  session$artifacts[["suggested_questions"]] <-
    tempest_session_suggested_questions(
      snapshot$suggested_questions,
      action = "restore"
    )
  session$events <- tempest_session_restore_progress_events(
    snapshot$progress_events
  )

  tempest_session_restore_expert_sessions(
    session,
    snapshot$expert_sessions %||% list()
  )
  tempest_session_restore_workflow_run(session, snapshot)

  session
}

#' @keywords internal
tempest_session_bundle_path <- function(path, ...) {
  file.path(path, ...)
}

#' @keywords internal
tempest_session_bundle_write_json <- function(path, rel_path, value) {
  tryCatch(
    tempest_write_json(tempest_session_bundle_path(path, rel_path), value),
    error = function(error) {
      tempest_abort(
        "Could not write session bundle file {.path {rel_path}}.",
        class = tempest_session_persistence_error_class(
          "tempest_session_save_error"
        ),
        parent = error
      )
    }
  )
  hook <- getOption("tempest.session_write_hook")
  if (is.function(hook)) {
    tryCatch(
      hook(rel_path),
      error = function(error) {
        tempest_abort(
          "Session bundle write was interrupted after {.path {rel_path}}.",
          class = tempest_session_persistence_error_class(
            "tempest_session_save_error"
          ),
          parent = error
        )
      }
    )
  }
  rel_path
}

#' @keywords internal
tempest_session_bundle_write_text <- function(path, rel_path, value) {
  if (!rlang::is_string(value)) {
    return(character())
  }
  tryCatch(
    tempest_write_text(tempest_session_bundle_path(path, rel_path), value),
    error = function(error) {
      tempest_abort(
        "Could not write session bundle file {.path {rel_path}}.",
        class = tempest_session_persistence_error_class(
          "tempest_session_save_error"
        ),
        parent = error
      )
    }
  )
  hook <- getOption("tempest.session_write_hook")
  if (is.function(hook)) {
    tryCatch(
      hook(rel_path),
      error = function(error) {
        tempest_abort(
          "Session bundle write was interrupted after {.path {rel_path}}.",
          class = tempest_session_persistence_error_class(
            "tempest_session_save_error"
          ),
          parent = error
        )
      }
    )
  }
  rel_path
}

#' @keywords internal
tempest_session_prepare_bundle_dir <- function(path, overwrite = FALSE) {
  if (!rlang::is_string(path) || !nzchar(tempest_trim(path))) {
    tempest_abort(
      "{.arg path} must be a single non-empty path string.",
      class = tempest_session_persistence_error_class(
        "tempest_session_save_error"
      )
    )
  }

  bundle_dir <- normalizePath(
    path.expand(path),
    winslash = "/",
    mustWork = FALSE
  )
  if (file.exists(bundle_dir)) {
    if (!dir.exists(bundle_dir)) {
      tempest_abort(
        "{.arg path} must point to a directory.",
        class = tempest_session_persistence_error_class(
          "tempest_session_save_error"
        )
      )
    }
    entries <- list.files(bundle_dir, all.files = TRUE, no.. = TRUE)
    if (length(entries) > 0) {
      if (!isTRUE(overwrite)) {
        tempest_abort(
          c(
            "Session bundle directory already exists.",
            i = "Use {.code overwrite = TRUE} to replace it.",
            x = "Path: {.path {bundle_dir}}."
          ),
          class = tempest_session_persistence_error_class(
            "tempest_session_save_error"
          )
        )
      }
      # Only overwrite a non-empty directory that already looks like a Tempest
      # session bundle. This keeps a mistyped or misconfigured path from
      # recursively deleting unrelated files (for example, when autosave writes
      # with `overwrite = TRUE` to a user-supplied location).
      if (
        !file.exists(tempest_session_bundle_path(bundle_dir, "session.json"))
      ) {
        tempest_abort(
          c(
            "Refusing to overwrite a non-empty directory that is not a Tempest session bundle.",
            i = "Expected a {.path session.json} manifest in the directory.",
            x = "Path: {.path {bundle_dir}}."
          ),
          class = tempest_session_persistence_error_class(
            "tempest_session_save_error"
          )
        )
      }
    }
  }

  dir.create(dirname(bundle_dir), recursive = TRUE, showWarnings = FALSE)
  normalizePath(bundle_dir, winslash = "/", mustWork = FALSE)
}

#' @keywords internal
tempest_session_bundle_checksum <- function(bundle_dir, rel_path) {
  digest::digest(
    file.path(bundle_dir, rel_path),
    algo = "sha256",
    file = TRUE,
    serialize = FALSE
  )
}

#' @keywords internal
tempest_session_commit_bundle <- function(staging_dir, bundle_dir) {
  backup_dir <- NULL
  if (file.exists(bundle_dir)) {
    backup_dir <- tempfile(
      pattern = paste0(".", basename(bundle_dir), "-backup-"),
      tmpdir = dirname(bundle_dir)
    )
    if (!file.rename(bundle_dir, backup_dir)) {
      tempest_abort(
        "Could not stage the previous session bundle for replacement.",
        class = tempest_session_persistence_error_class(
          "tempest_session_save_error"
        )
      )
    }
  }

  if (!file.rename(staging_dir, bundle_dir)) {
    if (!is.null(backup_dir)) {
      file.rename(backup_dir, bundle_dir)
    }
    tempest_abort(
      "Could not atomically install the completed session bundle.",
      class = tempest_session_persistence_error_class(
        "tempest_session_save_error"
      )
    )
  }
  if (!is.null(backup_dir)) {
    unlink(backup_dir, recursive = TRUE, force = TRUE)
  }
  normalizePath(bundle_dir, winslash = "/", mustWork = TRUE)
}

#' Save a Co-STORM session bundle
#'
#' `r lifecycle::badge("experimental")`
#'
#' `tempest_session_save()` writes a schema-versioned directory bundle for a
#' [TempestSession]. The bundle stores the research manifest, authoritative
#' workspace, and typed artifact catalog. The deprecated `store` alias is not
#' serialized separately. Inline artifacts use explicit UTF-8 or canonical
#' JSON codecs, every declared file is checksummed, and the `session.json`
#' manifest is written last. Live chat handles, registered tool closures,
#' Shiny reactive state, credentials, and raw provider request bodies are not
#' serialized.
#'
#' Use [tempest_session_resume()] to load the bundle with a fresh runtime
#' [TempestConfig] and [tempest_runtime()].
#'
#' @param session A [TempestSession] object.
#' @param path Directory where the session bundle should be written.
#' @param overwrite If `TRUE`, replace an existing bundle directory.
#' @param codec_registry Optional [tempest_artifact_codec_registry()] containing
#'   host-defined codecs needed to encode typed artifact content.
#' @return Invisibly returns the normalized bundle directory.
#' @export
tempest_session_save <- function(
  session,
  path,
  overwrite = FALSE,
  codec_registry = NULL
) {
  if (!inherits(session, "TempestSession")) {
    tempest_abort(
      "{.arg session} must be a {.cls TempestSession} object.",
      class = tempest_session_persistence_error_class(
        "tempest_session_save_error"
      )
    )
  }
  tempest_require("jsonlite", "Tempest session persistence requires jsonlite.")

  bundle_dir <- tempest_session_prepare_bundle_dir(path, overwrite = overwrite)
  staging_dir <- tempfile(
    pattern = paste0(".", basename(bundle_dir), "-staging-"),
    tmpdir = dirname(bundle_dir)
  )
  dir.create(staging_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(staging_dir, recursive = TRUE, force = TRUE), add = TRUE)
  snapshot <- tempest_session_snapshot(session)
  files <- character()

  files <- c(
    files,
    tempest_session_bundle_write_json(
      staging_dir,
      "config.json",
      snapshot$config
    ),
    tempest_session_bundle_write_json(
      staging_dir,
      "experts.json",
      snapshot$experts
    ),
    tempest_session_bundle_write_json(
      staging_dir,
      "skills.json",
      snapshot$skills
    ),
    tempest_session_bundle_write_json(
      staging_dir,
      "connection_refs.json",
      snapshot$connection_refs
    ),
    tempest_session_bundle_write_json(
      staging_dir,
      "connection_permissions.json",
      snapshot$connection_permissions
    ),
    tempest_session_bundle_write_json(
      staging_dir,
      "capability_grants.json",
      snapshot$capability_grants
    ),
    tempest_session_bundle_write_json(
      staging_dir,
      "expert_sessions.json",
      snapshot$expert_sessions
    ),
    tempest_session_bundle_write_json(
      staging_dir,
      "transcript.json",
      snapshot$transcript
    ),
    tempest_session_bundle_write_json(
      staging_dir,
      "mindmap.json",
      snapshot$mindmap
    ),
    tempest_session_bundle_write_json(
      staging_dir,
      "progress_events.json",
      snapshot$progress_events
    ),
    tempest_session_bundle_write_json(
      staging_dir,
      "workspace/resources.json",
      snapshot$workspace$resources
    ),
    tempest_session_bundle_write_json(
      staging_dir,
      "workspace/sources.json",
      snapshot$workspace$sources
    ),
    tempest_session_bundle_write_json(
      staging_dir,
      "workspace/proposed_claims.json",
      snapshot$workspace$proposed_claims
    ),
    tempest_session_bundle_write_json(
      staging_dir,
      "workspace/evidence_spans.json",
      snapshot$workspace$evidence_spans
    ),
    tempest_session_bundle_write_json(
      staging_dir,
      "workspace/disputes.json",
      snapshot$workspace$disputes
    ),
    tempest_session_bundle_write_json(
      staging_dir,
      "workspace/citation_audit.json",
      snapshot$workspace$citation_audit
    )
  )
  if (!is.null(snapshot$workflow_run)) {
    files <- c(
      files,
      tempest_session_bundle_write_json(
        staging_dir,
        "workflow_run.json",
        snapshot$workflow_run
      )
    )
  }

  typed_bundle <- tempest_artifact_bundle_write(
    session$artifact_catalog,
    staging_dir,
    codec_registry = codec_registry
  )
  files <- c(files, typed_bundle$files)

  if (length(snapshot$suggested_questions) > 0L) {
    files <- c(
      files,
      tempest_session_bundle_write_json(
        staging_dir,
        "artifacts/suggested_questions.json",
        snapshot$suggested_questions
      )
    )
  }

  files <- sort(unique(files))
  checksums <- stats::setNames(
    lapply(files, function(file) {
      tempest_session_bundle_checksum(staging_dir, file)
    }),
    files
  )
  manifest <- list(
    schema_version = snapshot$schema_version,
    bundle_type = "costorm",
    bundle_status = "complete",
    package_version = snapshot$package_version,
    session_id = snapshot$session_id,
    topic = snapshot$topic,
    title = snapshot$title,
    research_manifest = snapshot$research_manifest,
    workspace = snapshot$workspace[c(
      "schema_version",
      "base_snapshot_id",
      "max_sources",
      "accepted_graft_references"
    )],
    saved_at = tempest_now_utc(),
    files = files,
    checksums = checksums,
    artifact_files = typed_bundle$content_files,
    artifact_index = typed_bundle$index_path,
    deliverable_index = typed_bundle$deliverables_path
  )
  tempest_session_bundle_write_json(staging_dir, "session.json", manifest)

  invisible(tempest_session_commit_bundle(staging_dir, bundle_dir))
}

#' @keywords internal
tempest_session_bundle_optional_json <- function(path, default = NULL, what) {
  if (!file.exists(path)) {
    return(default)
  }
  tryCatch(
    tempest_read_json_strict(
      path,
      what = what,
      class = tempest_session_persistence_error_class(
        "tempest_session_restore_error"
      )
    ),
    error = function(error) {
      if (!isTRUE(getOption("tempest.session_partial_recovery", FALSE))) {
        stop(error)
      }
      tempest_warn("Skipping malformed {what} during partial recovery.")
      default
    }
  )
}

#' @keywords internal
tempest_session_restore_progress_events <- function(events) {
  records <- lapply(events %||% list(), function(event) {
    defaults <- list(
      stage = NA_character_,
      step = NA_character_,
      message = NA_character_,
      payload = list(),
      parent_event_id = NA_character_,
      correlation_id = NA_character_
    )
    for (field in names(defaults)) {
      if (is.null(event[[field]])) {
        event[[field]] <- defaults[[field]]
      }
    }
    tempest_progress_event_record(event)
  })
  if (length(records) == 0L) {
    return(list())
  }
  sequences <- vapply(
    records,
    function(event) {
      value <- event$sequence %||% NA_integer_
      if (
        !is.numeric(value) ||
          length(value) != 1L ||
          is.na(value) ||
          !is.finite(value) ||
          value < 1L ||
          value != as.integer(value)
      ) {
        return(NA_integer_)
      }
      as.integer(value)
    },
    integer(1)
  )
  if (all(is.na(sequences))) {
    sequences <- seq_along(records)
  } else if (
    anyNA(sequences) ||
      !identical(sequences, seq_along(records))
  ) {
    tempest_session_restore_abort(
      "Session progress-event sequences are not contiguous."
    )
  }
  for (i in seq_along(records)) {
    records[[i]]$sequence <- sequences[[i]]
  }
  records
}

#' @keywords internal
tempest_session_bundle_require_files <- function(bundle_dir, files) {
  missing <- files[!file.exists(file.path(bundle_dir, files))]
  if (length(missing) > 0) {
    tempest_abort(
      c(
        "Cannot resume Tempest session bundle.",
        x = "Missing required file{?s}: {.path {missing}}."
      ),
      class = tempest_session_persistence_error_class(
        "tempest_session_restore_error"
      )
    )
  }
}

#' @keywords internal
tempest_session_bundle_optional_presentation_files <- function() {
  "artifacts/suggested_questions.json"
}

#' @keywords internal
tempest_session_bundle_validate_manifest <- function(
  bundle_dir,
  manifest,
  partial_recovery = FALSE
) {
  schema_version <- tempest_persistence_schema_version(
    manifest$schema_version %||% NA_integer_,
    "Session bundle schema version",
    tempest_session_persistence_error_class(
      "tempest_session_restore_error"
    )
  )
  if (!schema_version %in% c(4L, 5L)) {
    tempest_session_restore_abort(
      paste0(
        "Unsupported session bundle schema version: ",
        manifest$schema_version %||% "missing",
        "."
      )
    )
  }
  if (identical(schema_version, 5L)) {
    if (
      !identical(manifest$bundle_type %||% "", "costorm") ||
        !identical(manifest$bundle_status %||% "", "complete")
    ) {
      tempest_session_restore_abort(
        "Schema 5 Co-STORM bundle envelope is not complete."
      )
    }
    if (
      !is.list(manifest$research_manifest) ||
        !is.list(manifest$workspace)
    ) {
      tempest_session_restore_abort(
        "Schema 5 Co-STORM bundle is missing research identity metadata."
      )
    }
    workspace_fields <- c(
      "schema_version",
      "base_snapshot_id",
      "max_sources",
      "accepted_graft_references"
    )
    if (
      is.null(names(manifest$workspace)) ||
        anyDuplicated(names(manifest$workspace)) ||
        !setequal(names(manifest$workspace), workspace_fields)
    ) {
      tempest_session_restore_abort(
        "Schema 5 Co-STORM bundle has invalid workspace identity metadata."
      )
    }
    tempest_research_workspace_require_current_schema(
      manifest$workspace,
      "Schema 5 Co-STORM workspace identity",
      tempest_session_persistence_error_class(
        "tempest_session_restore_error"
      )
    )
  } else if (!identical(manifest$status %||% "", "complete")) {
    tempest_session_restore_abort(
      "Legacy schema 4 session bundle manifest is not complete."
    )
  }
  files <- as.character(unlist(manifest$files %||% character()))
  evidence_required <- if (identical(schema_version, 5L)) {
    c(
      "workspace/resources.json",
      "workspace/sources.json",
      "workspace/proposed_claims.json",
      "workspace/evidence_spans.json",
      "workspace/disputes.json",
      "workspace/citation_audit.json"
    )
  } else {
    c(
      "store/resources.json",
      "store/sources.json",
      "store/claims.json",
      "store/evidence_spans.json",
      "store/disputes.json"
    )
  }
  core_required <- c(
    "config.json",
    "experts.json",
    "skills.json",
    "connection_refs.json",
    "connection_permissions.json",
    "capability_grants.json",
    "expert_sessions.json",
    "transcript.json",
    "mindmap.json",
    "progress_events.json",
    evidence_required,
    "artifacts/typed/deliverables.json",
    "artifacts/typed/index.json"
  )
  optional_presentation <- tempest_session_bundle_optional_presentation_files()
  normalized_files <- gsub("\\\\", "/", files)
  parts <- strsplit(normalized_files, "/", fixed = TRUE)
  unsafe <- grepl("^(/|~|[A-Za-z]:)", normalized_files) |
    vapply(
      parts,
      function(parts) {
        any(!nzchar(parts)) || any(parts %in% c(".", ".."))
      },
      logical(1)
    )
  missing <- files[!file.exists(file.path(bundle_dir, files))]
  checksums <- unlist(manifest$checksums %||% list(), use.names = TRUE)
  missing_checksums <- setdiff(files, names(checksums))
  extra_checksums <- setdiff(names(checksums), files)
  available <- setdiff(files[!unsafe], missing)
  bundle_root <- paste0(
    normalizePath(bundle_dir, winslash = "/", mustWork = TRUE),
    "/"
  )
  escaping <- available[vapply(
    available,
    function(file) {
      resolved <- normalizePath(
        file.path(bundle_dir, file),
        winslash = "/",
        mustWork = TRUE
      )
      !startsWith(resolved, bundle_root)
    },
    logical(1)
  )]
  checksum_candidates <- setdiff(available, escaping)
  mismatched <- checksum_candidates[vapply(
    checksum_candidates,
    function(file) {
      expected <- if (file %in% names(checksums)) {
        checksums[[file]]
      } else {
        NA_character_
      }
      is.na(expected) ||
        !identical(tempest_session_bundle_checksum(bundle_dir, file), expected)
    },
    logical(1)
  )]
  artifact_files <- as.character(unlist(
    manifest$artifact_files %||% character()
  ))
  artifact_index <- manifest$artifact_index %||% NA_character_
  deliverable_index <- manifest$deliverable_index %||% NA_character_
  required <- unique(c(core_required, artifact_files))
  undeclared_required <- setdiff(required, files)
  invalid_artifact_files <- artifact_files[
    !startsWith(artifact_files, "artifacts/typed/content/") |
      !vapply(
        artifact_files,
        tempest_artifact_bundle_path_is_safe,
        logical(1)
      )
  ]

  structural_problems <- c(
    if (length(files) == 0L) "Manifest declares no files.",
    if (anyDuplicated(normalized_files)) {
      "Manifest declares duplicate file paths."
    },
    if (any(unsafe)) "Manifest contains unsafe file paths.",
    if (length(undeclared_required) > 0L) {
      paste0(
        "Manifest omits required files: ",
        paste(undeclared_required, collapse = ", "),
        "."
      )
    },
    if (length(extra_checksums) > 0L) {
      paste0(
        "Manifest contains checksums for undeclared files: ",
        paste(extra_checksums, collapse = ", "),
        "."
      )
    },
    if (
      !identical(artifact_index, "artifacts/typed/index.json") ||
        !identical(
          deliverable_index,
          "artifacts/typed/deliverables.json"
        )
    ) {
      "Manifest contains invalid typed-artifact index paths."
    },
    if (
      anyDuplicated(artifact_files) ||
        length(invalid_artifact_files) > 0L ||
        length(setdiff(artifact_files, files)) > 0L
    ) {
      "Manifest contains invalid typed-artifact content paths."
    }
  )

  invalid_files <- unique(c(
    missing,
    missing_checksums,
    escaping,
    mismatched
  ))
  invalid_optional <- intersect(invalid_files, optional_presentation)
  invalid_required <- setdiff(invalid_files, optional_presentation)
  required_problems <- c(
    if (length(intersect(missing, invalid_required)) > 0L) {
      paste0(
        "Required files are missing: ",
        paste(intersect(missing, invalid_required), collapse = ", "),
        "."
      )
    },
    if (length(intersect(missing_checksums, invalid_required)) > 0L) {
      paste0(
        "Required files have no checksum: ",
        paste(
          intersect(missing_checksums, invalid_required),
          collapse = ", "
        ),
        "."
      )
    },
    if (length(intersect(escaping, invalid_required)) > 0L) {
      paste0(
        "Required files resolve outside the bundle: ",
        paste(intersect(escaping, invalid_required), collapse = ", "),
        "."
      )
    },
    if (length(intersect(mismatched, invalid_required)) > 0L) {
      paste0(
        "Required files failed checksum validation: ",
        paste(intersect(mismatched, invalid_required), collapse = ", "),
        "."
      )
    }
  )
  fatal_problems <- c(structural_problems, required_problems)
  if (length(fatal_problems) > 0L) {
    tempest_session_restore_abort(paste(fatal_problems, collapse = " "))
  }
  if (length(invalid_optional) > 0L && !isTRUE(partial_recovery)) {
    tempest_session_restore_abort(paste0(
      "Optional presentation files failed integrity validation: ",
      paste(invalid_optional, collapse = ", "),
      "."
    ))
  }
  if (length(invalid_optional) > 0L) {
    tempest_warn(c(
      "Recovering an incomplete Tempest session bundle.",
      i = paste0(
        "Skipping optional presentation files: ",
        paste(invalid_optional, collapse = ", "),
        "."
      )
    ))
  }
  invisible(setdiff(files, invalid_optional))
}

#' Resume a saved Co-STORM session bundle
#'
#' `tempest_session_resume()` reads a directory bundle written by
#' [tempest_session_save()] and rebuilds a [TempestSession] with a fresh runtime
#' [TempestConfig] and [tempest_runtime()]. Historical progress events are
#' loaded for display and reduction, but they are not replayed into `progress`.
#'
#' @param path Directory containing a session bundle.
#' @param config Runtime [TempestConfig] used to recreate chats, retrievers, and
#'   tools.
#' @param runtime A fresh [tempest_runtime()] supplying process-local skill,
#'   capability, and connection implementations.
#' @param connection_permissions Optional named connection-permission override
#'   that may only remove saved contexts or connection ids. When `NULL`, the
#'   saved opaque connection ids are reused.
#' @param progress Optional callback for future `tempest_progress_event`
#'   objects.
#' @param partial_recovery Whether to allow explicitly requested recovery when
#'   allowlisted presentation files are missing or fail integrity checks. All
#'   other declared files, including workflow, permission, grant, expert,
#'   runtime, workspace, and typed-artifact state, must pass integrity checks.
#' @param codec_registry Optional [tempest_artifact_codec_registry()] containing
#'   host-defined codecs needed to decode typed artifact content.
#' @return A restored [TempestSession].
#' @export
tempest_session_resume <- function(
  path,
  config = tempest_config(),
  runtime = tempest_runtime(),
  connection_permissions = NULL,
  progress = NULL,
  partial_recovery = FALSE,
  codec_registry = NULL
) {
  previous_partial <- getOption("tempest.session_partial_recovery")
  options(tempest.session_partial_recovery = isTRUE(partial_recovery))
  on.exit(
    options(tempest.session_partial_recovery = previous_partial),
    add = TRUE
  )
  if (!rlang::is_string(path) || !nzchar(tempest_trim(path))) {
    tempest_abort(
      "{.arg path} must be a single non-empty path string.",
      class = tempest_session_persistence_error_class(
        "tempest_session_restore_error"
      )
    )
  }
  bundle_dir <- normalizePath(
    path.expand(path),
    winslash = "/",
    mustWork = FALSE
  )
  tempest_session_bundle_require_files(bundle_dir, "session.json")

  manifest <- tempest_read_json_strict(
    file.path(bundle_dir, "session.json"),
    what = "session bundle manifest",
    class = tempest_session_persistence_error_class(
      "tempest_session_restore_error"
    )
  )
  schema_version <- tempest_persistence_schema_version(
    manifest$schema_version %||% NA_integer_,
    "Session bundle schema version",
    tempest_session_persistence_error_class(
      "tempest_session_restore_error"
    )
  )
  if (!schema_version %in% c(4L, 5L)) {
    tempest_session_restore_abort(
      paste0(
        "Unsupported session bundle schema version: ",
        manifest$schema_version,
        "."
      )
    )
  }
  declared_files <- tempest_session_bundle_validate_manifest(
    bundle_dir,
    manifest,
    partial_recovery = partial_recovery
  )
  strict_json <- function(rel_path, what) {
    tempest_read_json_strict(
      file.path(bundle_dir, rel_path),
      what = what,
      class = tempest_session_persistence_error_class(
        "tempest_session_restore_error"
      )
    )
  }
  declared_json <- function(rel_path, default = NULL, what) {
    if (!rel_path %in% declared_files) {
      return(default)
    }
    strict_json(rel_path, what)
  }
  optional_json <- function(rel_path, default = NULL, what) {
    if (!rel_path %in% declared_files) {
      return(default)
    }
    tempest_session_bundle_optional_json(
      file.path(bundle_dir, rel_path),
      default = default,
      what = what
    )
  }
  typed_catalog <- tempest_artifact_bundle_read(
    bundle_dir,
    declared_files = declared_files,
    codec_registry = codec_registry
  )

  snapshot <- list(
    schema_version = schema_version,
    package_version = manifest$package_version %||% NA_character_,
    research_manifest = manifest$research_manifest,
    topic = manifest$topic,
    title = manifest$title,
    session_id = manifest$session_id,
    config = strict_json(
      "config.json",
      what = "session config summary"
    ),
    experts = strict_json(
      "experts.json",
      what = "session expert profiles"
    ),
    skills = strict_json(
      "skills.json",
      what = "session skill records"
    ),
    connection_refs = strict_json(
      "connection_refs.json",
      what = "session connection-reference records"
    ),
    connection_permissions = strict_json(
      "connection_permissions.json",
      what = "session connection permissions"
    ),
    capability_grants = strict_json(
      "capability_grants.json",
      what = "session capability grants"
    ),
    transcript = strict_json(
      "transcript.json",
      what = "session transcript"
    ),
    mindmap = strict_json(
      "mindmap.json",
      what = "session mind map"
    ),
    artifact_catalog = typed_catalog$snapshot(include_content = TRUE),
    suggested_questions = optional_json(
      "artifacts/suggested_questions.json",
      default = character(),
      what = "suggested questions artifact"
    ),
    progress_events = strict_json(
      "progress_events.json",
      what = "progress-event history"
    ),
    expert_sessions = strict_json(
      "expert_sessions.json",
      what = "expert-session metadata"
    ),
    workflow_run = declared_json(
      "workflow_run.json",
      default = NULL,
      what = "generic Co-STORM workflow state"
    )
  )
  if (identical(schema_version, 5L)) {
    workspace <- manifest$workspace
    workspace$resources <- strict_json(
      "workspace/resources.json",
      what = "session typed resource ledger"
    )
    workspace$sources <- strict_json(
      "workspace/sources.json",
      what = "session legacy web-source projection"
    )
    workspace$proposed_claims <- strict_json(
      "workspace/proposed_claims.json",
      what = "session proposed-claim ledger"
    )
    workspace$evidence_spans <- strict_json(
      "workspace/evidence_spans.json",
      what = "session evidence-span ledger"
    )
    workspace$disputes <- strict_json(
      "workspace/disputes.json",
      what = "session dispute ledger"
    )
    workspace["citation_audit"] <- list(
      strict_json(
        "workspace/citation_audit.json",
        what = "session citation audit"
      )
    )
    snapshot$workspace <- workspace
  } else {
    citation_audit <- declared_json(
      "artifacts/citation_audit.json",
      what = "legacy citation audit artifact"
    )
    store_artifacts <- list(citation_audit = citation_audit)
    store_artifacts <- store_artifacts[
      !vapply(
        store_artifacts,
        is.null,
        logical(1)
      )
    ]
    snapshot$research_manifest <- NULL
    snapshot$artifacts <- list(
      suggested_questions = snapshot$suggested_questions
    )
    snapshot$store <- list(
      schema_version = 2L,
      resources = strict_json(
        "store/resources.json",
        what = "session typed resource ledger"
      ),
      sources = strict_json(
        "store/sources.json",
        what = "session source ledger"
      ),
      claims = strict_json(
        "store/claims.json",
        what = "session claim ledger"
      ),
      evidence_spans = strict_json(
        "store/evidence_spans.json",
        what = "session evidence-span ledger"
      ),
      disputes = strict_json(
        "store/disputes.json",
        what = "session dispute ledger"
      ),
      artifacts = store_artifacts
    )
  }

  tempest_session_restore(
    snapshot,
    config = config,
    runtime = runtime,
    connection_permissions = connection_permissions,
    progress = progress
  )
}

#' @keywords internal
tempest_restore_sources <- function(store, sources) {
  if (is.null(sources) || length(sources) == 0) {
    return(invisible(NULL))
  }
  for (source in sources) {
    if (!is.list(source) || is.null(source$url)) {
      next
    }
    source$id <- source$id %||% tempest_source_id(source$url)
    source$meta <- source$meta %||% list()
    store$upsert_source(source)
  }
  invisible(NULL)
}

#' @keywords internal
tempest_restore_claims <- function(store, claims) {
  if (is.null(claims) || length(claims) == 0) {
    return(invisible(NULL))
  }
  existing_ids <- purrr::map_chr(
    store$list_claims(),
    ~ .x@claim_id
  )
  for (x in claims) {
    if (!is.list(x) || is.null(x$claim_text)) {
      next
    }
    claim <- tempest_claim_from_list(x)
    if (claim@claim_id %in% existing_ids) {
      next
    }
    store$add_claim(claim)
    existing_ids <- c(existing_ids, claim@claim_id)
  }
  invisible(NULL)
}

#' @keywords internal
tempest_restore_citation_audit <- function(citation_audit) {
  if (is.null(citation_audit) || length(citation_audit) == 0) {
    return(tibble::tibble(
      claim_id = character(),
      claim_text = character(),
      verification_status = character(),
      support_score = numeric(),
      rationale = character()
    ))
  }
  if (is.data.frame(citation_audit)) {
    return(tibble::as_tibble(citation_audit))
  }

  tibble::tibble(
    claim_id = purrr::map_chr(citation_audit, ~ .x$claim_id %||% NA_character_),
    claim_text = purrr::map_chr(
      citation_audit,
      ~ .x$claim_text %||% NA_character_
    ),
    verification_status = purrr::map_chr(
      citation_audit,
      ~ .x$verification_status %||% NA_character_
    ),
    support_score = purrr::map_dbl(citation_audit, function(x) {
      suppressWarnings(as.numeric(x$support_score %||% NA_real_))
    }),
    rationale = purrr::map_chr(
      citation_audit,
      ~ .x$rationale %||% NA_character_
    )
  )
}

tempest_storm_stage_required_files <- function(completed_stages) {
  files_by_stage <- list(
    perspectives = c("perspectives.json", "experts.json"),
    research = "workspace.json",
    outline = c("direct_gen_outline.json", "storm_gen_outline.json"),
    write = c("storm_gen_outline.json", "storm_gen_article.md"),
    polish = c(
      "storm_gen_article.md",
      "storm_gen_article_polished.md",
      "references.json"
    )
  )
  unique(unlist(
    files_by_stage[intersect(names(files_by_stage), completed_stages)],
    use.names = FALSE
  ))
}

#' @keywords internal
tempest_run_bundle_validate_manifest <- function(run_dir, manifest) {
  schema_version <- tempest_persistence_schema_version(
    manifest$schema_version %||% NA_integer_,
    "STORM run schema version",
    tempest_persistence_error_class("tempest_run_restore_error")
  )
  valid_header <- if (identical(schema_version, 3L)) {
    identical(manifest$status %||% "", "complete")
  } else if (identical(schema_version, 4L)) {
    identical(manifest$bundle_type %||% "", "storm") &&
      identical(manifest$bundle_status %||% "", "complete") &&
      is.list(manifest$research_manifest)
  } else {
    FALSE
  }
  if (!isTRUE(valid_header)) {
    tempest_abort(
      "STORM run manifest is incomplete or uses an unsupported schema.",
      class = tempest_persistence_error_class(
        "tempest_run_restore_error"
      )
    )
  }
  files <- as.character(unlist(manifest$files %||% character()))
  normalized <- gsub("\\\\", "/", files)
  required <- c(
    if (identical(schema_version, 4L)) "workspace.json",
    "artifacts/typed/deliverables.json",
    "artifacts/typed/index.json"
  )
  stage_required <- if (identical(schema_version, 4L)) {
    tempest_storm_stage_required_files(
      tempest_as_character_vector(manifest$completed_stages)
    )
  } else {
    character()
  }
  unsafe <- !vapply(
    normalized,
    tempest_artifact_bundle_path_is_safe,
    logical(1)
  )
  missing <- files[!file.exists(file.path(run_dir, files))]
  checksums <- unlist(manifest$checksums %||% list(), use.names = TRUE)
  missing_checksums <- setdiff(files, names(checksums))
  extra_checksums <- setdiff(names(checksums), files)
  available <- setdiff(files, missing)
  run_root <- paste0(
    normalizePath(run_dir, winslash = "/", mustWork = TRUE),
    "/"
  )
  escaping <- available[vapply(
    available,
    function(file) {
      resolved <- normalizePath(
        file.path(run_dir, file),
        winslash = "/",
        mustWork = TRUE
      )
      !startsWith(resolved, run_root)
    },
    logical(1)
  )]
  checksum_candidates <- setdiff(available, escaping)
  mismatched <- checksum_candidates[vapply(
    checksum_candidates,
    function(file) {
      expected <- if (file %in% names(checksums)) {
        checksums[[file]]
      } else {
        NA_character_
      }
      is.na(expected) ||
        !identical(
          tempest_session_bundle_checksum(run_dir, file),
          expected
        )
    },
    logical(1)
  )]
  problems <- c(
    if (length(files) == 0L) "Manifest declares no files.",
    if (anyDuplicated(normalized)) "Manifest declares duplicate files.",
    if (any(unsafe)) "Manifest contains unsafe paths.",
    if (length(setdiff(required, files)) > 0L) {
      "Manifest omits required bundle files."
    },
    if (length(setdiff(stage_required, files)) > 0L) {
      paste0(
        "Manifest omits product files required by completed stages: ",
        paste(setdiff(stage_required, files), collapse = ", "),
        "."
      )
    },
    if (length(missing) > 0L) "Manifest declares missing files.",
    if (
      length(missing_checksums) > 0L ||
        length(extra_checksums) > 0L
    ) {
      "Manifest checksum inventory does not match its file inventory."
    },
    if (length(escaping) > 0L) {
      "Manifest declares files outside the run directory."
    },
    if (length(mismatched) > 0L) "Manifest checksum validation failed."
  )
  if (length(problems) > 0L) {
    tempest_abort(
      paste(problems, collapse = " "),
      class = tempest_persistence_error_class(
        "tempest_run_restore_error"
      )
    )
  }
  invisible(files)
}

#' @keywords internal
tempest_storm_workspace_identity_record <- function(workspace) {
  list(
    base_snapshot_id = workspace$base_snapshot_id,
    max_sources = tempest_research_workspace_max_sources_data(
      workspace$max_sources
    ),
    accepted_graft_references = workspace$list_accepted_graft_references()
  )
}

#' @keywords internal
tempest_storm_snapshot_reference <- function(workspace) {
  snapshot_id <- workspace$base_snapshot_id
  if (is.null(snapshot_id)) {
    return(list())
  }
  list(snapshot_id = snapshot_id)
}

#' @keywords internal
tempest_storm_run_restore_abort <- function(message, parent = NULL) {
  tempest_abort(
    message,
    class = tempest_persistence_error_class("tempest_run_restore_error"),
    parent = parent,
    .envir = rlang::caller_env()
  )
}

#' @keywords internal
tempest_storm_restore_workspace <- function(metadata, config) {
  schema_version <- tempest_persistence_schema_version(
    metadata$schema_version %||% NA_integer_,
    "STORM run schema version",
    tempest_persistence_error_class("tempest_run_restore_error")
  )
  if (identical(schema_version, 3L)) {
    return(tempest_research_workspace(max_sources = config@max_sources))
  }

  identity <- metadata$workspace
  if (!is.list(identity) || is.data.frame(identity)) {
    tempest_storm_run_restore_abort(
      "Schema 4 STORM bundles must contain a workspace identity record."
    )
  }
  required <- c(
    "base_snapshot_id",
    "max_sources",
    "accepted_graft_references"
  )
  if (!setequal(names(identity), required)) {
    tempest_storm_run_restore_abort(
      "The STORM workspace identity record has unexpected fields."
    )
  }
  base_snapshot_id <- tryCatch(
    tempest_research_workspace_snapshot_id(identity$base_snapshot_id),
    error = function(error) {
      tempest_storm_run_restore_abort(
        "The persisted workspace snapshot identity is invalid.",
        parent = error
      )
    }
  )
  max_sources <- tryCatch(
    tempest_research_workspace_restore_max_sources(identity$max_sources),
    error = function(error) {
      tempest_storm_run_restore_abort(
        "The persisted workspace source limit is invalid.",
        parent = error
      )
    }
  )
  accepted_references <- tryCatch(
    tempest_research_workspace_references(
      identity$accepted_graft_references %||% list()
    ),
    error = function(error) {
      tempest_storm_run_restore_abort(
        "The persisted accepted graft references are invalid.",
        parent = error
      )
    }
  )

  tempest_research_workspace(
    base_snapshot_id = base_snapshot_id,
    max_sources = max_sources,
    accepted_graft_references = accepted_references
  )
}

tempest_storm_workspace_equivalence_record <- function(workspace) {
  tempest_research_workspace_snapshot(workspace)
}

tempest_storm_workspace_is_empty <- function(workspace) {
  snapshot <- tempest_storm_workspace_equivalence_record(workspace)
  evidence_fields <- c(
    "resources",
    "sources",
    "proposed_claims",
    "evidence_spans",
    "disputes"
  )
  all(vapply(snapshot[evidence_fields], length, integer(1)) == 0L) &&
    is.null(snapshot$citation_audit)
}

tempest_storm_workspace_has_legacy_artifacts <- function(workspace) {
  inherits(workspace, "SourceStore") &&
    length(ls(workspace$artifacts, all.names = TRUE)) > 0L
}

tempest_storm_clear_legacy_artifacts <- function(workspace) {
  if (!inherits(workspace, "SourceStore")) {
    return(invisible(NULL))
  }
  artifacts <- ls(workspace$artifacts, all.names = TRUE)
  if (length(artifacts) > 0L) {
    rm(list = artifacts, envir = workspace$artifacts)
  }
  invisible(NULL)
}

tempest_storm_assert_workspace_equivalent <- function(supplied, persisted) {
  if (is.null(supplied)) {
    return(persisted)
  }
  if (tempest_storm_workspace_has_legacy_artifacts(supplied)) {
    tempest_storm_run_restore_abort(
      "The supplied workspace contains legacy arbitrary artifacts."
    )
  }
  supplied_record <- tryCatch(
    tempest_storm_workspace_equivalence_record(supplied),
    error = function(error) {
      tempest_storm_run_restore_abort(
        "The supplied workspace cannot be compared with persisted STORM state.",
        parent = error
      )
    }
  )
  persisted_record <- tempest_storm_workspace_equivalence_record(persisted)
  if (identical(supplied_record, persisted_record)) {
    return(supplied)
  }
  if (!tempest_storm_workspace_is_empty(supplied)) {
    tempest_storm_run_restore_abort(
      "The supplied workspace diverges from the persisted STORM workspace."
    )
  }
  if (!identical(supplied$base_snapshot_id, persisted$base_snapshot_id)) {
    tempest_storm_run_restore_abort(
      "The supplied workspace does not match the persisted base snapshot."
    )
  }
  supplied_references <- supplied$list_accepted_graft_references()
  persisted_references <- persisted$list_accepted_graft_references()
  supplied_keys <- vapply(
    supplied_references,
    tempest_research_workspace_reference_json,
    character(1)
  )
  persisted_keys <- vapply(
    persisted_references,
    tempest_research_workspace_reference_json,
    character(1)
  )
  if (length(setdiff(supplied_keys, persisted_keys)) > 0L) {
    tempest_storm_run_restore_abort(
      "The supplied workspace contains accepted graft references outside the persisted run."
    )
  }
  for (reference in persisted_references) {
    supplied$record_accepted_graft_reference(reference)
  }
  supplied <- tryCatch(
    tempest_research_workspace_restore(
      tempest_research_workspace_snapshot(persisted),
      workspace = supplied
    ),
    error = function(error) {
      tempest_storm_run_restore_abort(
        "The persisted STORM workspace cannot be restored into the supplied workspace.",
        parent = error
      )
    }
  )
  tempest_storm_clear_legacy_artifacts(supplied)
  if (
    !identical(
      tempest_storm_workspace_equivalence_record(supplied),
      persisted_record
    )
  ) {
    tempest_storm_run_restore_abort(
      "The supplied workspace cannot represent the persisted STORM workspace."
    )
  }
  supplied
}

#' @keywords internal
tempest_storm_restore_manifest <- function(
  metadata,
  workspace,
  state,
  config,
  run_dir,
  run_id = NULL
) {
  schema_version <- tempest_persistence_schema_version(
    metadata$schema_version %||% NA_integer_,
    "STORM run schema version",
    tempest_persistence_error_class("tempest_run_restore_error")
  )
  if (identical(schema_version, 3L)) {
    status <- if (tempest_storm_state_is_complete(state)) {
      "succeeded"
    } else {
      "running"
    }
    return(tempest_research_manifest(
      research_run_id = run_id %||% basename(run_dir),
      mode = "storm",
      config = config,
      programs = list(),
      knowledge_snapshot = tempest_storm_snapshot_reference(workspace),
      status = status
    ))
  }

  manifest <- tryCatch(
    tempest_research_manifest_from_record(metadata$research_manifest),
    error = function(error) {
      tempest_storm_run_restore_abort(
        "The persisted research manifest is invalid.",
        parent = error
      )
    }
  )
  if (!identical(manifest@mode, "storm")) {
    tempest_storm_run_restore_abort(
      "The persisted research manifest is not a STORM run."
    )
  }
  if (!is.null(run_id) && !identical(manifest@research_run_id, run_id)) {
    tempest_storm_run_restore_abort(
      "The supplied run id does not match the persisted research run id."
    )
  }
  config_digest <- tempest_research_config_digest(config)
  if (!identical(manifest@config_digest, config_digest)) {
    tempest_storm_run_restore_abort(
      "The current configuration does not match the persisted research run."
    )
  }
  snapshot_id <- manifest@knowledge_snapshot$snapshot_id %||% NULL
  if (!identical(snapshot_id, workspace$base_snapshot_id)) {
    tempest_storm_run_restore_abort(
      "The persisted research manifest and workspace use different knowledge snapshots."
    )
  }
  manifest
}

#' @keywords internal
tempest_storm_read_state <- function(
  run_dir,
  paths,
  metadata,
  path_is_declared
) {
  read_json_artifact <- function(name, default = NULL) {
    path <- paths[[name]]
    if (!path_is_declared(path) || !file.exists(path)) {
      return(default)
    }
    tempest_read_json(path) %||% default
  }
  read_text_artifact <- function(name) {
    path <- paths[[name]]
    if (!path_is_declared(path) || !file.exists(path)) {
      return(NULL)
    }
    tempest_read_text(path)
  }
  experts <- list()
  if (path_is_declared(paths$experts) && file.exists(paths$experts)) {
    expert_records <- tempest_read_json_strict(
      paths$experts,
      what = "STORM expert profiles",
      class = tempest_persistence_error_class("tempest_run_restore_error")
    )
    experts <- tempest_experts_from_records(
      expert_records,
      what = "STORM expert profiles",
      class = tempest_persistence_error_class("tempest_run_restore_error")
    )
  }
  schema_version <- tempest_persistence_schema_version(
    metadata$schema_version %||% NA_integer_,
    "STORM run schema version",
    tempest_persistence_error_class("tempest_run_restore_error")
  )
  completed_stages <- tempest_as_character_vector(metadata$completed_stages)
  inferred_stages <- tempest_infer_completed_stages(
    paths,
    path_is_declared = path_is_declared
  )
  if (identical(schema_version, 3L)) {
    completed_stages <- if (length(completed_stages) == 0L) {
      inferred_stages
    } else {
      intersect(completed_stages, inferred_stages)
    }
  } else if (length(completed_stages) == 0L) {
    completed_stages <- inferred_stages
  }
  topic <- metadata$topic %||% metadata$title %||% basename(run_dir)
  title <- metadata$title %||% topic
  tryCatch(
    tempest_storm_state(
      topic = topic,
      title = title,
      perspectives = read_json_artifact("perspectives", list()),
      experts = experts,
      draft_outline = read_json_artifact("draft_outline"),
      outline = read_json_artifact("outline"),
      lead_section = read_text_artifact("lead_section"),
      draft_md = read_text_artifact("draft_md"),
      report_md = read_text_artifact("report_md"),
      references = read_json_artifact("references", list()),
      completed_stages = completed_stages
    ),
    error = function(error) {
      tempest_storm_run_restore_abort(
        "The persisted STORM product state is invalid.",
        parent = error
      )
    }
  )
}

tempest_artifact_catalog_import <- function(target, source) {
  for (record in source$list_deliverables()) {
    target$register(tempest_deliverable_spec_from_data(record))
  }
  for (artifact_id in names(source$list())) {
    target$add(source$get(artifact_id), persist = FALSE)
  }
  invisible(target)
}

#' @keywords internal
tempest_load_run_artifacts <- function(
  run_dir,
  workspace = NULL,
  config = tempest_config(),
  run_id = NULL,
  artifact_catalog = tempest_artifact_catalog()
) {
  supplied_workspace <- workspace
  if (!is.null(workspace) && !inherits(workspace, "ResearchWorkspace")) {
    tempest_storm_run_restore_abort(
      "{.arg workspace} must be a ResearchWorkspace or `NULL`."
    )
  }
  if (!S7::S7_inherits(config, TempestConfig)) {
    tempest_storm_run_restore_abort(
      "{.arg config} must be created by {.fn tempest_config}."
    )
  }
  if (!inherits(artifact_catalog, "TempestArtifactCatalog")) {
    tempest_abort(
      "{.arg artifact_catalog} must be a TempestArtifactCatalog."
    )
  }
  paths <- tempest_run_artifact_paths(run_dir)
  metadata <- if (file.exists(paths$run_config)) {
    tempest_read_json_strict(
      paths$run_config,
      what = "STORM run manifest",
      class = tempest_persistence_error_class(
        "tempest_run_restore_error"
      )
    )
  } else {
    tempest_abort(
      "STORM run manifest is missing.",
      class = tempest_persistence_error_class(
        "tempest_run_restore_error"
      )
    )
  }
  declared_files <- tempest_run_bundle_validate_manifest(run_dir, metadata)
  schema_version <- tempest_persistence_schema_version(
    metadata$schema_version %||% NA_integer_,
    "STORM run schema version",
    tempest_persistence_error_class("tempest_run_restore_error")
  )
  path_is_declared <- function(path) {
    rel_path <- gsub(
      "\\\\",
      "/",
      as.character(fs::path_rel(path, start = run_dir))
    )
    rel_path %in% declared_files
  }

  workspace <- tempest_storm_restore_workspace(metadata, config)
  if (identical(schema_version, 4L)) {
    workspace_snapshot <- tempest_read_json_strict(
      paths$workspace,
      what = "STORM research workspace",
      class = tempest_persistence_error_class("tempest_run_restore_error")
    )
    tempest_research_workspace_require_current_schema(
      workspace_snapshot,
      "Schema 4 STORM research workspace",
      tempest_persistence_error_class("tempest_run_restore_error")
    )
    workspace <- tryCatch(
      tempest_research_workspace_restore(
        workspace_snapshot,
        workspace = workspace
      ),
      error = function(error) {
        tempest_storm_run_restore_abort(
          "The persisted STORM research workspace is invalid.",
          parent = error
        )
      }
    )
  } else {
    if (path_is_declared(paths$sources) && file.exists(paths$sources)) {
      tempest_restore_sources(workspace, tempest_read_json(paths$sources))
    }
    if (path_is_declared(paths$claims) && file.exists(paths$claims)) {
      tempest_restore_claims(workspace, tempest_read_json(paths$claims))
    }
    if (
      path_is_declared(paths$citation_audit) &&
        file.exists(paths$citation_audit)
    ) {
      citation_audit <- tempest_read_json(paths$citation_audit)
      if (!is.null(citation_audit)) {
        workspace$set_citation_audit(
          tempest_restore_citation_audit(citation_audit)
        )
      }
    }
  }
  state <- tempest_storm_read_state(
    run_dir,
    paths,
    metadata,
    path_is_declared
  )
  research_manifest <- tempest_storm_restore_manifest(
    metadata,
    workspace,
    state,
    config,
    run_dir,
    run_id = run_id
  )

  workspace <- tempest_storm_assert_workspace_equivalent(
    supplied_workspace,
    workspace
  )
  restored_catalog <- tempest_artifact_bundle_read(
    run_dir,
    evidence_store = workspace,
    declared_files = declared_files
  )
  tempest_artifact_catalog_import(artifact_catalog, restored_catalog)

  list(
    metadata = metadata,
    completed_stages = state$completed_stages,
    research_manifest = research_manifest,
    state = state,
    workspace = workspace,
    artifact_catalog = artifact_catalog
  )
}

#' @keywords internal
tempest_infer_completed_stages <- function(paths, path_is_declared = NULL) {
  artifact_exists <- function(name) {
    path <- paths[[name]] %||% NULL
    if (is.null(path)) {
      return(FALSE)
    }
    (is.null(path_is_declared) || isTRUE(path_is_declared(path))) &&
      file.exists(path)
  }
  stages <- character()
  if (artifact_exists("perspectives") && artifact_exists("experts")) {
    stages <- c(stages, "perspectives")
  }
  if (artifact_exists("sources") && artifact_exists("claims")) {
    stages <- c(stages, "research")
  }
  if (artifact_exists("outline")) {
    stages <- c(stages, "outline")
  }
  if (artifact_exists("draft_md")) {
    stages <- c(stages, "write")
  }
  if (artifact_exists("report_md")) {
    stages <- c(stages, "polish")
  }
  stages
}

#' @keywords internal
tempest_save_run_artifacts <- function(
  run_dir,
  workspace,
  state,
  research_manifest,
  config,
  steps,
  research_strategy,
  parallel_writing = FALSE,
  remove_duplicate = FALSE,
  artifact_catalog = tempest_artifact_catalog()
) {
  if (is.null(run_dir)) {
    return(invisible(NULL))
  }
  if (!inherits(workspace, "ResearchWorkspace")) {
    tempest_abort("{.arg workspace} must be a ResearchWorkspace.")
  }
  state <- tempest_storm_state_validate(state)
  if (!S7::S7_inherits(research_manifest, TempestResearchManifest)) {
    tempest_abort(
      "{.arg research_manifest} must be a TempestResearchManifest."
    )
  }
  if (!identical(research_manifest@mode, "storm")) {
    tempest_abort("{.arg research_manifest} must describe a STORM run.")
  }
  if (
    identical(research_manifest@status, "succeeded") &&
      !tempest_storm_state_is_complete(state)
  ) {
    tempest_abort(
      paste0(
        "A succeeded {.arg research_manifest} requires a completed polish ",
        "stage and a non-empty {.field report_md}."
      ),
      class = tempest_persistence_error_class(
        "tempest_run_persistence_error"
      )
    )
  }
  if (
    !identical(
      research_manifest@config_digest,
      tempest_research_config_digest(config)
    )
  ) {
    tempest_abort(
      "{.arg research_manifest} does not match the current configuration."
    )
  }
  snapshot_id <- research_manifest@knowledge_snapshot$snapshot_id %||% NULL
  if (!identical(snapshot_id, workspace$base_snapshot_id)) {
    tempest_abort(
      "{.arg research_manifest} does not match the workspace snapshot."
    )
  }
  if (!inherits(artifact_catalog, "TempestArtifactCatalog")) {
    tempest_abort(
      "{.arg artifact_catalog} must be a TempestArtifactCatalog."
    )
  }
  paths <- tempest_run_artifact_paths(run_dir)
  completed_stages <- state$completed_stages

  metadata <- list(
    topic = state$topic,
    title = state$title,
    completed_stages = completed_stages,
    requested_steps = steps,
    research_strategy = research_strategy,
    parallel_writing = isTRUE(parallel_writing),
    remove_duplicate = isTRUE(remove_duplicate),
    updated_at = tempest_now_utc(),
    models = config@models,
    search_provider = config@search_provider,
    max_search_results = config@max_search_results,
    max_search_queries_per_turn = config@max_search_queries_per_turn,
    retrieve_top_k = config@retrieve_top_k,
    max_sources = config@max_sources
  )

  tempest_write_json(
    paths$workspace,
    tempest_research_workspace_snapshot(workspace)
  )
  for (legacy_path in paths[c("sources", "claims", "citation_audit")]) {
    if (file.exists(legacy_path)) {
      unlink(legacy_path)
    }
  }

  # References are the sources actually cited in the report/draft, not a copy
  # of every collected source.
  cited_md <- state$report_md %||%
    state$draft_md %||%
    ""
  cited_ids <- tempest_extract_citation_ids(cited_md)
  references <- Filter(
    Negate(is.null),
    lapply(cited_ids, function(id) workspace$get_source(id))
  )
  state$references <- references
  state <- tempest_storm_state_validate(state)
  tempest_write_json(paths$references, references)

  for (path_name in c("perspectives", "draft_outline", "outline")) {
    value <- state[[path_name]]
    if (!is.null(value)) {
      tempest_write_json(paths[[path_name]], value)
    } else if (file.exists(paths[[path_name]])) {
      unlink(paths[[path_name]])
    }
  }
  tempest_write_json(paths$experts, tempest_expert_records(state$experts))

  for (path_name in c("lead_section", "draft_md", "report_md")) {
    value <- state[[path_name]]
    if (rlang::is_string(value)) {
      tempest_write_text(paths[[path_name]], value)
    } else if (file.exists(paths[[path_name]])) {
      unlink(paths[[path_name]])
    }
  }

  typed_dir <- file.path(run_dir, "artifacts", "typed")
  if (dir.exists(typed_dir)) {
    unlink(typed_dir, recursive = TRUE, force = TRUE)
  }
  typed_bundle <- tempest_artifact_bundle_write(
    artifact_catalog,
    run_dir
  )
  files <- list.files(
    run_dir,
    recursive = TRUE,
    all.files = TRUE,
    no.. = TRUE
  )
  files <- sort(setdiff(files, "run_config.json"))
  checksums <- stats::setNames(
    lapply(
      files,
      function(file) tempest_session_bundle_checksum(run_dir, file)
    ),
    files
  )
  metadata$schema_version <- 4L
  metadata$bundle_type <- "storm"
  metadata$bundle_status <- "complete"
  metadata$research_manifest <- tempest_research_manifest_record(
    research_manifest
  )
  metadata$workspace <- tempest_storm_workspace_identity_record(workspace)
  metadata$files <- files
  metadata$checksums <- checksums
  metadata$artifact_files <- typed_bundle$content_files
  metadata$artifact_index <- typed_bundle$index_path
  metadata$deliverable_index <- typed_bundle$deliverables_path

  # Write the run manifest last, so a crash mid-save never leaves
  # `completed_stages` asserting a stage whose artifacts are not yet on disk.
  tempest_write_json(paths$run_config, metadata)

  invisible(run_dir)
}

#' @keywords internal
tempest_stage_complete <- function(completed_stages, stage) {
  stage %in% completed_stages
}

#' @keywords internal
tempest_mark_stage_complete <- function(completed_stages, stage) {
  allowed <- c("perspectives", "research", "outline", "write", "polish")
  completed_stages <- unique(c(completed_stages, stage))
  completed_stages[order(match(completed_stages, allowed))]
}
