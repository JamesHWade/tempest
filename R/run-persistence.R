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
    graft_snapshot = file.path(
      run_dir,
      "knowledge",
      "graft-snapshot.rds"
    ),
    perspectives = file.path(run_dir, "perspectives.json"),
    experts = file.path(run_dir, "experts.json"),
    draft_outline = file.path(run_dir, "direct_gen_outline.json"),
    outline = file.path(run_dir, "storm_gen_outline.json"),
    lead_section = file.path(run_dir, "lead_section.md"),
    draft_md = file.path(run_dir, "storm_gen_article.md"),
    report_md = file.path(run_dir, "storm_gen_article_polished.md"),
    references = file.path(run_dir, "references.json")
  )
}

#' @keywords internal
tempest_graft_snapshot_relative_path <- function() {
  "knowledge/graft-snapshot.rds"
}

#' @keywords internal
tempest_graft_snapshot_field_names <- function() {
  c(
    "schema_version",
    "snapshot_id",
    "store_id",
    "store_format_version",
    "schema_build_digest",
    "commit_order",
    "batch_id",
    "committed_at",
    "history_complete"
  )
}

#' @keywords internal
tempest_graft_snapshot_abort <- function(message, class, parent = NULL) {
  tempest_abort(message, class = class, parent = parent)
}

#' @keywords internal
tempest_graft_snapshot_validate <- function(
  snapshot,
  class,
  what = "Graft snapshot"
) {
  if (is.null(snapshot)) {
    return(NULL)
  }
  snapshot <- tryCatch(
    tempest_research_workspace_graft_snapshot(snapshot),
    error = function(error) {
      tempest_graft_snapshot_abort(
        paste0(what, " is not a real valid graft::GraftSnapshot."),
        class,
        parent = error
      )
    }
  )
  fields <- tempest_graft_snapshot_field_names()
  if (!identical(S7::prop_names(snapshot), fields)) {
    tempest_graft_snapshot_abort(
      paste0(what, " does not expose the complete public Graft boundary."),
      class
    )
  }
  values <- stats::setNames(
    lapply(fields, \(field) S7::prop(snapshot, field)),
    fields
  )
  reference <- tryCatch(
    tempest_snapshot_reference(snapshot),
    error = function(error) {
      tempest_graft_snapshot_abort(
        paste0(what, " cannot be represented by the Tempest manifest."),
        class,
        parent = error
      )
    }
  )
  list(snapshot = snapshot, values = values, reference = reference)
}

#' @keywords internal
tempest_graft_snapshot_assert_binding <- function(
  snapshot,
  manifest_reference,
  workspace,
  class,
  what = "Persisted Graft snapshot"
) {
  manifest_reference <- manifest_reference %||% list()
  if (!is.list(manifest_reference) || is.data.frame(manifest_reference)) {
    tempest_graft_snapshot_abort(
      "The research manifest knowledge snapshot must be a reference record.",
      class
    )
  }
  if (!inherits(workspace, "ResearchWorkspace")) {
    tempest_graft_snapshot_abort(
      "The Graft snapshot must be bound to a ResearchWorkspace.",
      class
    )
  }
  if (is.null(snapshot)) {
    if (
      length(manifest_reference) > 0L ||
        !is.null(workspace$base_snapshot_id) ||
        !is.null(workspace$graft_snapshot)
    ) {
      tempest_graft_snapshot_abort(
        paste0(
          "Pinned accepted knowledge requires an actual path-free ",
          "graft::GraftSnapshot."
        ),
        class
      )
    }
    return(invisible(NULL))
  }
  validated <- tempest_graft_snapshot_validate(snapshot, class, what)
  workspace_validated <- tempest_graft_snapshot_validate(
    workspace$graft_snapshot,
    class,
    "ResearchWorkspace Graft snapshot"
  )
  if (is.null(workspace_validated)) {
    tempest_graft_snapshot_abort(
      "The ResearchWorkspace does not retain the persisted Graft snapshot.",
      class
    )
  }
  if (!identical(validated$values, workspace_validated$values)) {
    tempest_graft_snapshot_abort(
      "The persisted Graft snapshot does not match the ResearchWorkspace.",
      class
    )
  }
  manifest_reference <- tryCatch(
    tempest_research_manifest_knowledge_snapshot(manifest_reference),
    error = function(error) {
      tempest_graft_snapshot_abort(
        "The research manifest knowledge snapshot is invalid.",
        class,
        parent = error
      )
    }
  )
  if (!identical(validated$reference, manifest_reference)) {
    tempest_graft_snapshot_abort(
      paste0(
        "The research manifest does not match all immutable Graft snapshot ",
        "fields."
      ),
      class
    )
  }
  if (
    !identical(
      workspace$base_snapshot_id,
      validated$reference$snapshot_id
    )
  ) {
    tempest_graft_snapshot_abort(
      "The ResearchWorkspace base snapshot does not match the Graft snapshot.",
      class
    )
  }
  matching_references <- Filter(
    \(reference) {
      identical(
        reference$snapshot_id %||% NULL,
        validated$reference$snapshot_id
      )
    },
    workspace$list_accepted_graft_references()
  )
  complete_references <- Filter(
    \(reference) identical(reference, validated$reference),
    matching_references
  )
  if (length(complete_references) != 1L) {
    tempest_graft_snapshot_abort(
      paste0(
        "The ResearchWorkspace does not retain the complete immutable ",
        "Graft snapshot reference."
      ),
      class
    )
  }
  invisible(validated$reference)
}

#' @keywords internal
tempest_graft_snapshot_write <- function(root, snapshot, class) {
  if (is.null(snapshot)) {
    return(character())
  }
  validated <- tempest_graft_snapshot_validate(
    snapshot,
    class,
    "Graft snapshot sidecar"
  )
  relative_path <- tempest_graft_snapshot_relative_path()
  path <- file.path(root, relative_path)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  staging <- tempfile(
    pattern = ".graft-snapshot-staging-",
    tmpdir = dirname(path),
    fileext = ".rds"
  )
  installed <- FALSE
  on.exit(
    if (!installed && file.exists(staging)) {
      unlink(staging)
    },
    add = TRUE
  )
  tryCatch(
    saveRDS(validated$snapshot, staging, version = 3L),
    error = function(error) {
      tempest_graft_snapshot_abort(
        "Could not serialize the Graft snapshot sidecar.",
        class,
        parent = error
      )
    }
  )
  tryCatch(
    tempest_graft_snapshot_validate(
      readRDS(staging),
      class,
      "Serialized Graft snapshot sidecar"
    ),
    error = function(error) {
      tempest_graft_snapshot_abort(
        "Could not verify the serialized Graft snapshot sidecar.",
        class,
        parent = error
      )
    }
  )
  if (!file.rename(staging, path)) {
    if (!file.copy(staging, path, overwrite = TRUE)) {
      tempest_graft_snapshot_abort(
        "Could not install the Graft snapshot sidecar.",
        class
      )
    }
    unlink(staging)
  }
  installed <- TRUE
  relative_path
}

#' @keywords internal
tempest_graft_snapshot_read <- function(
  root,
  declared_files,
  manifest_reference,
  class
) {
  relative_path <- tempest_graft_snapshot_relative_path()
  declared <- relative_path %in% declared_files
  pinned <- length(manifest_reference %||% list()) > 0L
  if (!identical(declared, pinned)) {
    tempest_graft_snapshot_abort(
      paste0(
        "The Graft snapshot sidecar and research manifest must either both ",
        "be present or both be absent."
      ),
      class
    )
  }
  if (!pinned) {
    return(NULL)
  }
  if (!requireNamespace("graft", quietly = TRUE)) {
    tempest_graft_snapshot_abort(
      paste0(
        "Restoring pinned accepted knowledge requires the optional ",
        "graft package."
      ),
      class
    )
  }
  snapshot <- tryCatch(
    readRDS(file.path(root, relative_path)),
    error = function(error) {
      tempest_graft_snapshot_abort(
        "Could not read the Graft snapshot sidecar.",
        class,
        parent = error
      )
    }
  )
  tempest_graft_snapshot_validate(
    snapshot,
    class,
    "Persisted Graft snapshot sidecar"
  )$snapshot
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

#' @keywords internal
tempest_unsupported_format_abort <- function(what, version = NULL, class) {
  suffix <- if (is.null(version)) "" else paste0(": ", version)
  tempest_abort(
    paste0("Unsupported ", what, suffix, "."),
    class = unique(c("tempest_unsupported_format_error", class))
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
tempest_persistence_manifest_files <- function(value, what, class) {
  valid <- is.list(value) &&
    !is.data.frame(value) &&
    is.null(names(value)) &&
    length(value) > 0L &&
    all(vapply(
      value,
      function(file) {
        rlang::is_string(file) &&
          !is.na(file) &&
          nzchar(tempest_trim(file))
      },
      logical(1)
    ))
  if (!isTRUE(valid)) {
    tempest_abort(
      "{what} must be an unnamed list of non-empty file paths.",
      class = class
    )
  }
  unname(vapply(value, identity, character(1)))
}

#' @keywords internal
tempest_persistence_manifest_checksums <- function(
  value,
  files,
  what,
  class
) {
  value_names <- names(value)
  valid <- is.list(value) &&
    !is.data.frame(value) &&
    !is.null(value_names) &&
    length(value) > 0L &&
    !anyNA(value_names) &&
    !anyDuplicated(value_names) &&
    all(nzchar(value_names)) &&
    setequal(value_names, files) &&
    all(vapply(
      value,
      function(checksum) {
        rlang::is_string(checksum) &&
          !is.na(checksum) &&
          grepl("^[a-f0-9]{64}$", checksum)
      },
      logical(1)
    ))
  if (!isTRUE(valid)) {
    tempest_abort(
      "{what} must map every declared file to one SHA-256 checksum.",
      class = class
    )
  }
  unlist(value, use.names = TRUE)
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
  entries <- workspace$list_retrieved_resources()
  retrieved_resources <- unname(lapply(
    entries,
    tempest_resource_record,
    include_content = TRUE
  ))

  snapshot <- list(
    schema_version = 4L,
    base_snapshot_id = workspace$base_snapshot_id,
    max_sources = tempest_research_workspace_max_sources_data(
      workspace$max_sources
    ),
    accepted_graft_references = workspace$list_accepted_graft_references(),
    retrieved_resources = retrieved_resources,
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
  tryCatch(
    tempest_storm_state_record_value(snapshot, "workspace"),
    error = function(error) {
      tempest_research_workspace_snapshot_abort(
        "Workspace records must contain only canonical portable values."
      )
    }
  )
}

#' @keywords internal
tempest_research_workspace_restore_abort <- function(message, parent = NULL) {
  tempest_abort(
    c("Cannot restore ResearchWorkspace snapshot.", x = message),
    class = tempest_persistence_error_class(
      "tempest_research_workspace_restore_error"
    ),
    parent = parent,
    .envir = rlang::caller_env()
  )
}

#' @keywords internal
tempest_research_workspace_restore_schema <- function(snapshot) {
  value <- snapshot$schema_version %||% NA_integer_
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
  if (!identical(value, 4L)) {
    tempest_unsupported_format_abort(
      "ResearchWorkspace snapshot format",
      value,
      tempest_persistence_error_class(
        "tempest_research_workspace_restore_error"
      )
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
    tempest_unsupported_format_abort(
      paste0(what, " format"),
      schema_version,
      class
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
tempest_research_workspace_record_fields <- function(field) {
  switch(
    field,
    retrieved_resources = c(
      "resource_id",
      "resource_kind",
      "locator",
      "title",
      "media_type",
      "content",
      "storage_ref",
      "origin_connection_id",
      "scope_metadata",
      "content_hash",
      "retrieved_at",
      "redaction",
      "retention",
      "metadata",
      "schema_version",
      "fingerprint"
    ),
    proposed_claims = c(
      "claim_id",
      "claim_text",
      "claim_type",
      "source_ids",
      "evidence_span_ids",
      "supporting_quotes",
      "contradicting_source_ids",
      "contradiction_note",
      "confidence",
      "support_score",
      "contradiction_score",
      "source_quality_score",
      "retrieval_query",
      "retrieval_step_id",
      "perspective_id",
      "expert_id",
      "session_id",
      "section_id",
      "created_at",
      "verified_at",
      "verifier_model",
      "verification_status"
    ),
    evidence_spans = c(
      "evidence_span_id",
      "source_id",
      "chunk_id",
      "quote",
      "start_offset",
      "end_offset",
      "page",
      "section_heading",
      "relevance_score",
      "extracted_by",
      "created_at"
    ),
    disputes = c(
      "dispute_id",
      "topic",
      "claim_ids",
      "axis_of_disagreement",
      "likely_explanation",
      "evidence_balance",
      "unresolved_questions",
      "created_at"
    ),
    tempest_research_workspace_restore_abort(
      "Unknown workspace record field {.field {field}}."
    )
  )
}

#' @keywords internal
tempest_research_workspace_exact_records <- function(records, field) {
  tempest_persistence_exact_records(
    records,
    tempest_research_workspace_record_fields(field),
    paste0("workspace ", gsub("_", "-", field), " records"),
    tempest_persistence_error_class(
      "tempest_research_workspace_restore_error"
    )
  )
}

#' @keywords internal
tempest_persistence_record_string <- function(
  value,
  field,
  nullable = FALSE,
  non_empty = FALSE
) {
  if (is.null(value) && isTRUE(nullable)) {
    return(NULL)
  }
  if (
    !rlang::is_string(value) ||
      is.na(value) ||
      (isTRUE(non_empty) && !nzchar(tempest_trim(value)))
  ) {
    tempest_research_workspace_restore_abort(paste0(
      "Workspace record field ",
      field,
      " must be ",
      if (nullable) "NULL or " else "",
      "one ",
      if (non_empty) "non-empty " else "",
      "string."
    ))
  }
  value
}

#' @keywords internal
tempest_persistence_record_string_array <- function(
  value,
  field,
  ids = FALSE
) {
  values <- if (is.character(value) && is.null(names(value))) {
    value
  } else if (is.list(value) && !is.data.frame(value) && is.null(names(value))) {
    if (length(value) == 0L) {
      character()
    } else {
      valid <- vapply(
        value,
        \(item) rlang::is_string(item) && !is.na(item),
        logical(1)
      )
      if (!all(valid)) {
        tempest_research_workspace_restore_abort(paste0(
          "Workspace record field ",
          field,
          " must be a flat string array."
        ))
      }
      unlist(value, use.names = FALSE)
    }
  } else {
    tempest_research_workspace_restore_abort(paste0(
      "Workspace record field ",
      field,
      " must be a flat string array."
    ))
  }
  if (
    anyNA(values) ||
      (isTRUE(ids) &&
        (any(!nzchar(values)) ||
          !identical(values, tempest_trim(values)) ||
          anyDuplicated(values)))
  ) {
    tempest_research_workspace_restore_abort(paste0(
      "Workspace record field ",
      field,
      " contains invalid or duplicate identifiers."
    ))
  }
  unname(values)
}

#' @keywords internal
tempest_persistence_record_number <- function(
  value,
  field,
  nullable = FALSE,
  integer = FALSE,
  minimum = -Inf,
  maximum = Inf
) {
  if (is.null(value) && isTRUE(nullable)) {
    return(NULL)
  }
  valid <- is.numeric(value) &&
    length(value) == 1L &&
    !is.na(value) &&
    is.finite(value) &&
    value >= minimum &&
    value <= maximum &&
    (!isTRUE(integer) ||
      (value == floor(value) &&
        value >= -.Machine$integer.max - 1 &&
        value <= .Machine$integer.max))
  if (!isTRUE(valid)) {
    tempest_research_workspace_restore_abort(paste0(
      "Workspace record field ",
      field,
      " must be ",
      if (nullable) "NULL or " else "",
      if (integer) "one exact integer." else "one finite number."
    ))
  }
  if (isTRUE(integer)) as.integer(value) else value
}

#' @keywords internal
tempest_persistence_record_timestamp <- function(
  value,
  field,
  nullable = FALSE
) {
  value <- tempest_persistence_record_string(
    value,
    field,
    nullable = nullable,
    non_empty = TRUE
  )
  if (is.null(value)) {
    return(NULL)
  }
  parsed <- suppressWarnings(as.POSIXct(value, tz = "UTC"))
  if (is.na(parsed)) {
    tempest_research_workspace_restore_abort(paste0(
      "Workspace record field ",
      field,
      " must be a valid timestamp."
    ))
  }
  value
}

#' @keywords internal
tempest_research_workspace_validate_claim_record <- function(record) {
  for (field in c(
    "claim_id",
    "claim_text",
    "claim_type",
    "confidence",
    "verification_status"
  )) {
    record[[field]] <- tempest_persistence_record_string(
      record[[field]],
      field,
      non_empty = TRUE
    )
  }
  for (field in c(
    "contradiction_note",
    "retrieval_query",
    "retrieval_step_id",
    "perspective_id",
    "expert_id",
    "session_id",
    "section_id",
    "verifier_model"
  )) {
    record[field] <- list(tempest_persistence_record_string(
      record[[field]],
      field,
      nullable = TRUE
    ))
  }
  record$created_at <- tempest_persistence_record_timestamp(
    record$created_at,
    "created_at"
  )
  record["verified_at"] <- list(tempest_persistence_record_timestamp(
    record$verified_at,
    "verified_at",
    nullable = TRUE
  ))
  for (field in c(
    "source_ids",
    "evidence_span_ids",
    "contradicting_source_ids"
  )) {
    record[[field]] <- tempest_persistence_record_string_array(
      record[[field]],
      field,
      ids = TRUE
    )
  }
  quotes <- record$supporting_quotes
  if (!is.list(quotes) || is.data.frame(quotes) || !is.null(names(quotes))) {
    tempest_research_workspace_restore_abort(
      "Workspace record field supporting_quotes must be a flat string array."
    )
  }
  tempest_persistence_record_string_array(quotes, "supporting_quotes")
  for (field in c(
    "support_score",
    "contradiction_score",
    "source_quality_score"
  )) {
    record[field] <- list(tempest_persistence_record_number(
      record[[field]],
      field,
      nullable = TRUE,
      minimum = 0,
      maximum = 1
    ))
  }
  record
}

#' @keywords internal
tempest_research_workspace_validate_span_record <- function(record) {
  for (field in c("evidence_span_id", "source_id", "extracted_by")) {
    record[[field]] <- tempest_persistence_record_string(
      record[[field]],
      field,
      non_empty = TRUE
    )
  }
  for (field in c("chunk_id", "quote", "section_heading")) {
    record[field] <- list(tempest_persistence_record_string(
      record[[field]],
      field,
      nullable = TRUE
    ))
  }
  record$created_at <- tempest_persistence_record_timestamp(
    record$created_at,
    "created_at"
  )
  for (field in c("start_offset", "end_offset", "page")) {
    record[field] <- list(tempest_persistence_record_number(
      record[[field]],
      field,
      nullable = TRUE,
      integer = TRUE
    ))
  }
  record["relevance_score"] <- list(tempest_persistence_record_number(
    record$relevance_score,
    "relevance_score",
    nullable = TRUE,
    minimum = 0,
    maximum = 1
  ))
  record
}

#' @keywords internal
tempest_research_workspace_validate_dispute_record <- function(record) {
  for (field in c("dispute_id", "topic", "evidence_balance")) {
    record[[field]] <- tempest_persistence_record_string(
      record[[field]],
      field,
      non_empty = TRUE
    )
  }
  for (field in c("axis_of_disagreement", "likely_explanation")) {
    record[field] <- list(tempest_persistence_record_string(
      record[[field]],
      field,
      nullable = TRUE
    ))
  }
  record$created_at <- tempest_persistence_record_timestamp(
    record$created_at,
    "created_at"
  )
  record$claim_ids <- tempest_persistence_record_string_array(
    record$claim_ids,
    "claim_ids",
    ids = TRUE
  )
  record$unresolved_questions <- tempest_persistence_record_string_array(
    record$unresolved_questions,
    "unresolved_questions"
  )
  record
}

#' @keywords internal
tempest_research_workspace_unique_record_ids <- function(
  records,
  id_field,
  what
) {
  ids <- vapply(
    records,
    function(record) {
      id <- record[[id_field]]
      if (!rlang::is_string(id) || is.na(id) || !nzchar(tempest_trim(id))) {
        tempest_research_workspace_restore_abort(paste0(
          what,
          " contains an invalid ",
          id_field,
          "."
        ))
      }
      id
    },
    character(1)
  )
  if (anyDuplicated(ids)) {
    tempest_research_workspace_restore_abort(paste0(
      what,
      " contains duplicate ",
      id_field,
      " values."
    ))
  }
  invisible(ids)
}

#' @keywords internal
tempest_research_workspace_restore_metadata <- function(
  snapshot,
  schema_version,
  workspace = NULL
) {
  require_fields <- identical(schema_version, 4L)
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
tempest_research_workspace_restore <- function(
  snapshot,
  workspace = NULL,
  graft_snapshot = NULL
) {
  graft_snapshot_missing <- missing(graft_snapshot)
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
  if (graft_snapshot_missing && !is.null(workspace)) {
    graft_snapshot <- workspace$graft_snapshot
  }
  schema_version <- tempest_research_workspace_restore_schema(snapshot)
  if (identical(schema_version, 4L)) {
    expected_fields <- c(
      "schema_version",
      "base_snapshot_id",
      "max_sources",
      "accepted_graft_references",
      "retrieved_resources",
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
      tempest_unsupported_format_abort(
        "ResearchWorkspace snapshot format",
        schema_version,
        tempest_persistence_error_class(
          "tempest_research_workspace_restore_error"
        )
      )
    }
  }
  metadata <- tempest_research_workspace_restore_metadata(
    snapshot,
    schema_version,
    workspace = workspace
  )
  graft_snapshot <- tryCatch(
    tempest_research_workspace_graft_snapshot(
      graft_snapshot,
      metadata$base_snapshot_id
    ),
    error = function(error) {
      tempest_research_workspace_restore_abort(
        "The retained Graft snapshot is invalid.",
        parent = error
      )
    }
  )
  candidate <- tempest_research_workspace(
    base_snapshot_id = metadata$base_snapshot_id,
    graft_snapshot = graft_snapshot,
    max_sources = metadata$max_sources,
    accepted_graft_references = metadata$accepted_graft_references
  )
  if (!is.null(workspace)) {
    if (!identical(workspace$base_snapshot_id, metadata$base_snapshot_id)) {
      tempest_research_workspace_restore_abort(
        "{.field base_snapshot_id} does not match the pinned workspace."
      )
    }
    workspace_references <- workspace$list_accepted_graft_references()
    workspace_reference_keys <- vapply(
      workspace_references,
      tempest_research_workspace_reference_json,
      character(1)
    )
    metadata_reference_keys <- vapply(
      metadata$accepted_graft_references,
      tempest_research_workspace_reference_json,
      character(1)
    )
    if (
      length(
        setdiff(workspace_reference_keys, metadata_reference_keys)
      ) >
        0L
    ) {
      tempest_research_workspace_restore_abort(
        paste0(
          "{.field accepted_graft_references} contain identities outside ",
          "the pinned workspace snapshot."
        )
      )
    }
    workspace_graft <- tryCatch(
      tempest_research_workspace_graft_snapshot(
        workspace$graft_snapshot,
        metadata$base_snapshot_id
      ),
      error = function(error) {
        tempest_research_workspace_restore_abort(
          "The supplied workspace Graft snapshot is invalid.",
          parent = error
        )
      }
    )
    if (
      !identical(
        lapply(
          tempest_graft_snapshot_field_names(),
          \(field) {
            if (is.null(workspace_graft)) {
              NULL
            } else {
              S7::prop(workspace_graft, field)
            }
          }
        ),
        lapply(
          tempest_graft_snapshot_field_names(),
          \(field) {
            if (is.null(graft_snapshot)) {
              NULL
            } else {
              S7::prop(graft_snapshot, field)
            }
          }
        )
      )
    ) {
      tempest_research_workspace_restore_abort(
        "The retained Graft snapshot does not match the supplied workspace."
      )
    }
  }

  records <- tempest_research_workspace_restore_records(
    snapshot,
    "retrieved_resources",
    required = TRUE
  )
  records <- tempest_research_workspace_exact_records(
    records,
    "retrieved_resources"
  )
  tempest_research_workspace_unique_record_ids(
    records,
    "resource_id",
    "Retrieved-resource records"
  )
  for (i in seq_along(records)) {
    resource <- tryCatch(
      tempest_resource_from_data(records[[i]]),
      error = function(error) {
        tempest_research_workspace_restore_abort(
          paste0("Retrieved-resource entry ", i, " is invalid."),
          parent = error
        )
      }
    )
    tryCatch(
      candidate$upsert_retrieved_resource(resource),
      error = function(error) {
        tempest_research_workspace_restore_abort(
          paste0("Retrieved-resource entry ", i, " cannot be restored."),
          parent = error
        )
      }
    )
  }

  source_ids <- purrr::map_chr(
    candidate$list_retrieved_resources(),
    tempest_resource_identity
  )
  span_records <- tempest_research_workspace_restore_records(
    snapshot,
    "evidence_spans",
    required = TRUE
  )
  span_records <- tempest_research_workspace_exact_records(
    span_records,
    "evidence_spans"
  )
  span_records <- lapply(
    span_records,
    tempest_research_workspace_validate_span_record
  )
  tempest_research_workspace_unique_record_ids(
    span_records,
    "evidence_span_id",
    "Evidence-span records"
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
    candidate$add_evidence_span(span)
  }

  claim_records <- tempest_research_workspace_restore_records(
    snapshot,
    "proposed_claims",
    required = TRUE
  )
  claim_records <- tempest_research_workspace_exact_records(
    claim_records,
    "proposed_claims"
  )
  claim_records <- lapply(
    claim_records,
    tempest_research_workspace_validate_claim_record
  )
  tempest_research_workspace_unique_record_ids(
    claim_records,
    "claim_id",
    "Proposed-claim records"
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
      candidate$add_proposed_claim(claim),
      error = function(error) {
        tempest_research_workspace_restore_abort(
          paste0("Proposed claim ", claim@claim_id, " cannot be restored."),
          parent = error
        )
      }
    )
  }

  claim_ids <- purrr::map_chr(
    candidate$list_proposed_claims(),
    ~ .x@claim_id
  )
  dispute_records <- tempest_research_workspace_restore_records(
    snapshot,
    "disputes",
    required = TRUE
  )
  dispute_records <- tempest_research_workspace_exact_records(
    dispute_records,
    "disputes"
  )
  dispute_records <- lapply(
    dispute_records,
    tempest_research_workspace_validate_dispute_record
  )
  tempest_research_workspace_unique_record_ids(
    dispute_records,
    "dispute_id",
    "Dispute records"
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
    candidate$add_dispute(dispute)
  }

  citation_audit <- snapshot$citation_audit
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
      candidate$set_citation_audit(audit),
      error = function(error) {
        tempest_research_workspace_restore_abort(
          "{.field citation_audit} cannot be restored.",
          parent = error
        )
      }
    )
  }

  if (is.null(workspace)) {
    return(candidate)
  }
  target_private <- workspace$.__enclos_env__$private
  source_private <- candidate$.__enclos_env__$private
  fields <- c(
    "resources_value",
    "claims_value",
    "evidence_spans_value",
    "disputes_value",
    "max_sources_value",
    "claims_by_source",
    "base_snapshot_id_value",
    "graft_snapshot_value",
    "accepted_graft_references_value",
    "citation_audit_value"
  )
  for (field in fields) {
    target_private[[field]] <- source_private[[field]]
  }
  workspace
}

#' @keywords internal
tempest_session_restore_abort <- function(message, parent = NULL) {
  tempest_abort(
    c("Cannot restore TempestSession snapshot.", x = message),
    class = tempest_session_persistence_error_class(
      "tempest_session_restore_error"
    ),
    parent = parent,
    .envir = rlang::caller_env()
  )
}

#' @keywords internal
tempest_expert_records <- function(experts) {
  experts <- tempest_validate_experts(experts, active_only = FALSE)
  unname(lapply(experts, tempest_expert_profile_record))
}

#' @keywords internal
tempest_expert_record_fields <- function() {
  c(
    "expert_id",
    "version",
    "name",
    "title",
    "description",
    "instructions",
    "focus_areas",
    "skill_ids",
    "skill_versions",
    "required_capability_ids",
    "optional_capability_ids",
    "model_role",
    "model_policy_ref",
    "selection_metadata",
    "initial_work_items",
    "initial_questions",
    "state",
    "metadata",
    "schema_version",
    "fingerprint"
  )
}

#' @keywords internal
tempest_skill_record_fields <- function() {
  c(
    "skill_id",
    "version",
    "title",
    "purpose",
    "instructions",
    "input_schema",
    "output_schema",
    "required_capability_ids",
    "operation_ids",
    "operation_versions",
    "state",
    "metadata",
    "schema_version",
    "fingerprint"
  )
}

#' @keywords internal
tempest_connection_ref_record_fields <- function() {
  c(
    "connection_id",
    "version",
    "provider_id",
    "connection_type",
    "title",
    "description",
    "scopes",
    "state",
    "metadata",
    "schema_version",
    "fingerprint"
  )
}

#' @keywords internal
tempest_persistence_exact_records <- function(
  records,
  fields,
  what,
  class
) {
  valid <- is.list(records) &&
    !is.data.frame(records) &&
    is.null(names(records)) &&
    all(vapply(
      records,
      function(record) {
        is.list(record) &&
          !is.data.frame(record) &&
          !is.null(names(record)) &&
          !anyNA(names(record)) &&
          !anyDuplicated(names(record)) &&
          setequal(names(record), fields)
      },
      logical(1)
    ))
  if (!isTRUE(valid)) {
    tempest_abort(
      "Cannot restore {what}; records do not match the current schema.",
      class = class
    )
  }
  tryCatch(
    tempest_workflow_serializable_list(records, what),
    error = function(error) {
      tempest_abort(
        "Cannot restore {what}; records contain non-portable values.",
        class = class,
        parent = error
      )
    }
  )
  records
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
  records <- tempest_persistence_exact_records(
    records,
    tempest_expert_record_fields(),
    what,
    class
  )
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
tempest_expert_session_record_fields <- function() {
  c(
    "session_id",
    "expert_id",
    "expert_version",
    "expert_fingerprint",
    "model_role",
    "allowed_connection_ref_ids",
    "grants",
    "created_at"
  )
}

#' @keywords internal
tempest_session_snapshot_value_abort <- function(
  message,
  action,
  parent = NULL
) {
  if (identical(action, "restore")) {
    tempest_session_restore_abort(message, parent = parent)
  }
  tempest_abort(
    message,
    class = tempest_session_persistence_error_class(
      "tempest_session_snapshot_error"
    ),
    parent = parent
  )
}

#' @keywords internal
tempest_session_snapshot_record <- function(
  value,
  field,
  action = "snapshot"
) {
  tryCatch(
    tempest_contract_serializable_list(value %||% list(), field),
    error = function(error) {
      tempest_session_snapshot_value_abort(
        paste0("Cannot snapshot non-serializable ", field, " state."),
        action,
        parent = error
      )
    }
  )
}

#' @keywords internal
tempest_session_transcript_record <- function(value, action = "snapshot") {
  value <- tempest_session_snapshot_record(
    value,
    "transcript",
    action = action
  )
  valid <- is.null(names(value)) &&
    all(vapply(
      value,
      function(turn) {
        is.list(turn) &&
          !is.data.frame(turn) &&
          identical(names(turn), c("speaker", "role", "text", "at")) &&
          all(vapply(
            turn[c("speaker", "text", "at")],
            function(field) {
              rlang::is_string(field) && !is.na(field) && nzchar(field)
            },
            logical(1)
          )) &&
          rlang::is_string(turn$role) &&
          !is.na(turn$role) &&
          turn$role %in% c("user", "assistant")
      },
      logical(1)
    ))
  if (!isTRUE(valid)) {
    tempest_session_snapshot_value_abort(
      "Co-STORM transcript must contain only canonical turn records.",
      action
    )
  }
  value
}

#' @keywords internal
tempest_session_mindmap_record <- function(value, action = "snapshot") {
  value <- tempest_session_snapshot_record(value, "mindmap", action = action)
  if (
    !identical(sort(names(value)), c("edges", "nodes")) ||
      !is.list(value$nodes) ||
      !is.null(names(value$nodes)) ||
      !is.list(value$edges) ||
      !is.null(names(value$edges))
  ) {
    tempest_session_snapshot_value_abort(
      "Co-STORM mind map must contain unnamed node and edge record lists.",
      action
    )
  }
  valid_string <- function(value, nullable = FALSE) {
    (isTRUE(nullable) && is.null(value)) ||
      (rlang::is_string(value) && !is.na(value) && nzchar(value))
  }
  valid_ids <- function(value) {
    if (is.character(value)) {
      return(!anyNA(value) && all(nzchar(value)))
    }
    is.list(value) &&
      is.null(names(value)) &&
      all(vapply(value, valid_string, logical(1)))
  }
  nodes_valid <- vapply(
    value$nodes,
    function(node) {
      fields <- names(node)
      is.list(node) &&
        !is.data.frame(node) &&
        !is.null(fields) &&
        !anyNA(fields) &&
        !anyDuplicated(fields) &&
        all(c("id", "label") %in% fields) &&
        all(fields %in% c("id", "label", "parent", "notes", "source_ids")) &&
        valid_string(node$id) &&
        valid_string(node$label) &&
        valid_string(node$parent %||% NULL, nullable = TRUE) &&
        (is.null(node$notes) || rlang::is_string(node$notes)) &&
        valid_ids(node$source_ids %||% character())
    },
    logical(1)
  )
  edges_valid <- vapply(
    value$edges,
    function(edge) {
      fields <- names(edge)
      is.list(edge) &&
        !is.data.frame(edge) &&
        !is.null(fields) &&
        !anyNA(fields) &&
        !anyDuplicated(fields) &&
        all(c("from", "to") %in% fields) &&
        all(fields %in% c("from", "to", "relation")) &&
        valid_string(edge$from) &&
        valid_string(edge$to) &&
        (is.null(edge$relation) || rlang::is_string(edge$relation))
    },
    logical(1)
  )
  node_ids <- vapply(value$nodes, function(node) node$id %||% "", character(1))
  if (
    !all(nodes_valid) ||
      !all(edges_valid) ||
      anyDuplicated(node_ids)
  ) {
    tempest_session_snapshot_value_abort(
      "Co-STORM mind map contains malformed node or edge records.",
      action
    )
  }
  value
}

#' @keywords internal
tempest_session_mindmap_assert_binding <- function(mindmap, workspace) {
  node_ids <- vapply(mindmap$nodes, `[[`, character(1), "id")
  if (!"root" %in% node_ids) {
    tempest_session_restore_abort(
      "The restored mind map must contain the canonical root node."
    )
  }
  invalid_parent <- vapply(
    mindmap$nodes,
    function(node) {
      parent <- node$parent %||% NULL
      if (identical(node$id, "root")) {
        return(!is.null(parent))
      }
      is.null(parent) || !parent %in% node_ids || identical(parent, node$id)
    },
    logical(1)
  )
  invalid_edge <- vapply(
    mindmap$edges,
    function(edge) {
      !edge$from %in% node_ids || !edge$to %in% node_ids
    },
    logical(1)
  )
  resource_ids <- vapply(
    workspace$list_retrieved_resources(),
    tempest_resource_identity,
    character(1)
  )
  invalid_sources <- vapply(
    mindmap$nodes,
    function(node) {
      source_ids <- tempest_codec_character(node$source_ids %||% character())
      anyDuplicated(source_ids) ||
        length(setdiff(source_ids, resource_ids)) > 0L
    },
    logical(1)
  )
  parents <- stats::setNames(
    lapply(mindmap$nodes, \(node) node$parent %||% NULL),
    node_ids
  )
  reaches_root <- vapply(
    node_ids,
    function(node_id) {
      visited <- character()
      current <- node_id
      while (!identical(current, "root")) {
        if (current %in% visited || is.null(parents[[current]])) {
          return(FALSE)
        }
        visited <- c(visited, current)
        current <- parents[[current]]
      }
      TRUE
    },
    logical(1)
  )
  if (
    any(invalid_parent) ||
      any(invalid_edge) ||
      any(invalid_sources) ||
      !all(reaches_root)
  ) {
    tempest_session_restore_abort(
      paste0(
        "The restored mind map contains an unknown or cyclic parent, ",
        "edge endpoint, or evidence source id."
      )
    )
  }
  invisible(mindmap)
}

#' @keywords internal
tempest_session_portable_snapshot <- function(snapshot, action = "snapshot") {
  portable <- snapshot[c("transcript", "mindmap", "progress_events")]
  for (field in names(portable)) {
    tryCatch(
      tempest_workflow_serializable_list(
        list(value = portable[[field]]),
        field
      ),
      error = function(error) {
        tempest_session_snapshot_value_abort(
          paste0(
            "Co-STORM snapshot contains non-portable ",
            field,
            " state."
          ),
          action,
          parent = error
        )
      }
    )
  }
  for (field in c("topic", "title", "session_id")) {
    value <- snapshot[[field]]
    if (
      !rlang::is_string(value) ||
        is.na(value) ||
        !nzchar(tempest_trim(value))
    ) {
      tempest_session_snapshot_value_abort(
        paste0("Co-STORM snapshot requires non-empty scalar ", field, "."),
        action
      )
    }
  }
  package_version <- snapshot$package_version
  if (
    !is.null(package_version) &&
      (!rlang::is_string(package_version) || is.na(package_version))
  ) {
    tempest_session_snapshot_value_abort(
      "Co-STORM snapshot package version must be one string or null.",
      action
    )
  }
  invisible(snapshot)
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

#' @keywords internal
tempest_session_snapshot_fields <- function() {
  c(
    "schema_version",
    "package_version",
    "research_manifest",
    "topic",
    "title",
    "session_id",
    "experts",
    "transcript",
    "mindmap",
    "report_md",
    "suggested_questions",
    "progress_events",
    "workspace",
    "expert_sessions",
    "graft_snapshot"
  )
}

#' @keywords internal
tempest_session_report_record <- function(value, action = "snapshot") {
  if (is.null(value)) {
    return(NULL)
  }
  if (!rlang::is_string(value)) {
    message <- "The Co-STORM report must be one Markdown string or NULL."
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
  enc2utf8(value)
}

#' Snapshot a Co-STORM session
#'
#' `r lifecycle::badge("experimental")`
#'
#' `tempest_session_snapshot()` returns a structured, in-memory representation
#' of the durable state in a [TempestSession]. It includes the research
#' manifest; fixed session and configuration identity; the authoritative
#' [ResearchWorkspace]; expert profiles; transcript and mind map; the latest
#' report Markdown; progress-event and expert-session metadata; and the optional
#' immutable Graft snapshot. Live chat
#' handles, runtime clients, tools, closures, generic workflows, generic
#' artifact catalogs, Shiny reactive state, credentials, and provider request
#' bodies are not included.
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
  tryCatch(
    {
      live_programs <- tempest_program_set_manifest_programs(
        tempest_session_program_set(session)
      )
      if (
        !tempest_program_set_identity_equal(
          live_programs,
          research_manifest@programs
        )
      ) {
        tempest_ecosystem_contract_abort(
          "The live Co-STORM ProgramSet does not match its research manifest."
        )
      }
    },
    error = function(error) {
      tempest_abort(
        "Cannot snapshot an inconsistent Co-STORM ProgramSet.",
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
  graft_snapshot <- session$workspace$graft_snapshot
  tryCatch(
    tempest_graft_snapshot_assert_binding(
      graft_snapshot,
      research_manifest@knowledge_snapshot,
      session$workspace,
      tempest_session_persistence_error_class(
        "tempest_session_snapshot_error"
      ),
      "Co-STORM Graft snapshot"
    ),
    error = function(error) {
      tempest_abort(
        "Cannot snapshot inconsistent accepted-knowledge state.",
        class = tempest_session_persistence_error_class(
          "tempest_session_snapshot_error"
        ),
        parent = error
      )
    }
  )

  snapshot <- list(
    schema_version = 5L,
    package_version = tryCatch(
      as.character(utils::packageVersion("tempest")),
      error = function(e) NA_character_
    ),
    research_manifest = tempest_research_manifest_record(research_manifest),
    topic = session$topic,
    title = session$title,
    session_id = session$session_id,
    experts = tempest_expert_records(session$experts),
    transcript = tempest_session_transcript_record(
      session$transcript,
      action = "snapshot"
    ),
    mindmap = tempest_session_mindmap_record(
      session$mindmap,
      action = "snapshot"
    ),
    report_md = tempest_session_report_record(
      tempest_session_report_value(session)
    ),
    suggested_questions = suggested_questions,
    progress_events = tempest_session_restore_progress_events(
      tempest_execution_events(session),
      session_id = session$session_id,
      action = "snapshot"
    ),
    workspace = workspace,
    expert_sessions = tempest_expert_sessions_snapshot(session),
    graft_snapshot = graft_snapshot
  )
  tempest_session_portable_snapshot(snapshot)
  snapshot
}

#' @keywords internal
tempest_session_restore_expert_sessions <- function(session, expert_sessions) {
  expert_sessions <- expert_sessions %||% list()
  expert_sessions <- tempest_persistence_exact_records(
    expert_sessions,
    tempest_expert_session_record_fields(),
    "session expert-session records",
    tempest_session_persistence_error_class(
      "tempest_session_restore_error"
    )
  )
  if (length(expert_sessions) > 0L) {
    session_ids <- vapply(
      expert_sessions,
      function(binding) {
        value <- binding$session_id
        if (!rlang::is_string(value) || is.na(value)) {
          return("")
        }
        value
      },
      character(1)
    )
    expert_ids <- vapply(
      expert_sessions,
      function(binding) {
        value <- binding$expert_id
        if (!rlang::is_string(value) || is.na(value)) {
          return("")
        }
        value
      },
      character(1)
    )
    if (
      any(!nzchar(session_ids)) ||
        any(!nzchar(expert_ids)) ||
        anyDuplicated(session_ids) ||
        anyDuplicated(expert_ids)
    ) {
      tempest_session_restore_abort(
        paste0(
          "Expert-session records require unique session ids and one ",
          "session per expert."
        )
      )
    }
  }
  for (expert_session in expert_sessions) {
    expert_session$allowed_connection_ref_ids <- tryCatch(
      tempest_contract_ids(
        tempest_expert_session_connection_ids(
          expert_session$allowed_connection_ref_ids,
          "allowed_connection_ref_ids"
        ),
        "allowed_connection_ref_ids"
      ),
      error = function(error) {
        tempest_session_restore_abort(
          "Snapshot contains invalid expert-session connection ids.",
          parent = error
        )
      }
    )
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
    tryCatch(
      tempest_expert_session_grants(expert_session$grants),
      error = function(error) {
        tempest_session_restore_abort(
          "Snapshot contains invalid expert-session grant records.",
          parent = error
        )
      }
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
      "expert_fingerprint",
      "model_role",
      "allowed_connection_ref_ids",
      "created_at"
    )) {
      restored_value <- restored[[field]]
      saved_value <- expert_session[[field]]
      if (identical(field, "allowed_connection_ref_ids")) {
        restored_value <- unname(restored_value)
        saved_value <- unname(saved_value)
      }
      if (!identical(restored_value, saved_value)) {
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

#' Restore a Co-STORM session from a snapshot
#'
#' `r lifecycle::badge("experimental")`
#'
#' `tempest_session_restore()` rebuilds a [TempestSession] from a structured
#' snapshot created by [tempest_session_snapshot()] or read by
#' [tempest_session_resume()]. It restores the research manifest and
#' authoritative workspace, and creates fresh chat/tool handles using `config`.
#'
#' Historical progress events are restored as session artifact data and can be
#' reduced with [tempest_progress_state()]. They are not replayed into the new
#' `progress` callback; future calls on the restored session use that callback.
#'
#' @param snapshot A list from [tempest_session_snapshot()].
#' @param config Runtime [TempestConfig] used to recreate chats, retrievers, and
#'   tools.
#' @param progress Optional callback for future `tempest_progress_event`
#'   objects.
#' @param program_set A [TempestProgramSet] carrying the same program
#'   identities recorded in the snapshot. If `NULL`, the builtin set is used.
#' @return A restored [TempestSession].
#' @export
tempest_session_restore <- function(
  snapshot,
  config = tempest_config(),
  progress = NULL,
  program_set = NULL
) {
  tempest_session_restore_internal(
    snapshot = snapshot,
    config = config,
    runtime = tempest_runtime(),
    connection_permissions = NULL,
    progress = progress,
    program_set = program_set
  )
}

#' @keywords internal
tempest_session_restore_internal <- function(
  snapshot,
  config = tempest_config(),
  runtime = tempest_runtime(),
  connection_permissions = NULL,
  progress = NULL,
  program_set = NULL
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
  if (!identical(schema_version, 5L)) {
    tempest_unsupported_format_abort(
      "TempestSession snapshot format",
      schema_version,
      tempest_session_persistence_error_class(
        "tempest_session_restore_error"
      )
    )
  }
  snapshot_fields <- names(snapshot)
  if (
    is.null(snapshot_fields) ||
      anyNA(snapshot_fields) ||
      anyDuplicated(snapshot_fields) ||
      !setequal(snapshot_fields, tempest_session_snapshot_fields())
  ) {
    tempest_unsupported_format_abort(
      "TempestSession snapshot format",
      schema_version,
      tempest_session_persistence_error_class(
        "tempest_session_restore_error"
      )
    )
  }
  tempest_session_portable_snapshot(snapshot, action = "restore")
  snapshot$transcript <- tempest_session_transcript_record(
    snapshot$transcript,
    action = "restore"
  )
  snapshot$mindmap <- tempest_session_mindmap_record(
    snapshot$mindmap,
    action = "restore"
  )
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
    !rlang::is_string(snapshot$title) || !nzchar(tempest_trim(snapshot$title))
  ) {
    tempest_session_restore_abort("Snapshot must include a non-empty title.")
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
    tempest_research_workspace_restore(
      snapshot$workspace,
      graft_snapshot = snapshot$graft_snapshot
    ),
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
    tempest_graft_snapshot_assert_binding(
      snapshot$graft_snapshot,
      research_manifest@knowledge_snapshot,
      workspace,
      tempest_session_persistence_error_class(
        "tempest_session_restore_error"
      ),
      "Restored Co-STORM Graft snapshot"
    ),
    error = function(error) {
      tempest_session_restore_abort(
        paste0(
          "Snapshot accepted-knowledge identity is invalid: ",
          conditionMessage(error)
        )
      )
    }
  )
  program_set <- program_set %||% tempest_program_set()
  tryCatch(
    {
      tempest_costorm_manifest_validate(
        research_manifest,
        snapshot$session_id,
        config,
        workspace
      )
      declared_programs <- tempest_program_set_manifest_programs(program_set)
      if (
        !tempest_program_set_identity_equal(
          declared_programs,
          research_manifest@programs
        )
      ) {
        tempest_ecosystem_contract_abort(
          "The supplied ProgramSet does not match the Co-STORM manifest."
        )
      }
    },
    error = function(error) {
      tempest_session_restore_abort(
        paste0(
          "Snapshot research identity does not match the restore inputs: ",
          conditionMessage(error)
        )
      )
    }
  )
  tempest_session_mindmap_assert_binding(snapshot$mindmap, workspace)

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
  connection_permissions <- tempest_session_connection_permissions(
    connection_permissions %||% list(),
    runtime,
    action = "restore"
  )
  retriever <- tempest_retriever(config = config, workspace = workspace)
  session <- tempest_session_restore_new(
    topic = snapshot$topic,
    config = config,
    runtime = runtime,
    experts = experts,
    connection_permissions = connection_permissions,
    retriever = retriever,
    progress = NULL,
    session_id = snapshot$session_id,
    program_set = program_set,
    manifest = research_manifest
  )

  session$title <- snapshot$title
  session$progress <- tempest_progress_callback(progress)
  session$expert_session_manager$run_id <- session$session_id
  session$expert_session_manager$progress <- function(event) {
    session$record_progress_event(event)
  }
  session$transcript <- snapshot$transcript
  session$mindmap <- snapshot$mindmap
  session$artifacts <- new.env(parent = emptyenv())
  tempest_session_set_report_value(
    session,
    tempest_session_report_record(snapshot$report_md, action = "restore")
  )

  session$artifacts[["suggested_questions"]] <-
    tempest_session_suggested_questions(
      snapshot$suggested_questions,
      action = "restore"
    )
  session$events <- tempest_session_restore_progress_events(
    snapshot$progress_events,
    session_id = snapshot$session_id,
    action = "restore"
  )

  tempest_session_restore_expert_sessions(
    session,
    snapshot$expert_sessions %||% list()
  )
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
#' workspace, optional immutable Graft snapshot, and narrow report product.
#' Every declared file is checksummed, and the `session.json` manifest is
#' written last. Generic workflow and artifact-catalog state, live chat handles,
#' registered tool closures, Shiny reactive state, credentials, and raw provider
#' request bodies are not serialized.
#'
#' Use [tempest_session_resume()] to load the bundle with a fresh runtime
#' [TempestConfig].
#'
#' @param session A [TempestSession] object.
#' @param path Directory where the session bundle should be written.
#' @param overwrite If `TRUE`, replace an existing bundle directory.
#' @return Invisibly returns the normalized bundle directory.
#' @export
tempest_session_save <- function(
  session,
  path,
  overwrite = FALSE
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
      "experts.json",
      snapshot$experts
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
      "workspace/retrieved_resources.json",
      snapshot$workspace$retrieved_resources
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
  if (!is.null(snapshot$report_md)) {
    files <- c(
      files,
      tempest_session_bundle_write_text(
        staging_dir,
        "report.md",
        snapshot$report_md
      )
    )
  }
  files <- c(
    files,
    tempest_graft_snapshot_write(
      staging_dir,
      snapshot$graft_snapshot,
      tempest_session_persistence_error_class(
        "tempest_session_save_error"
      )
    )
  )

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
    checksums = checksums
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
tempest_session_restore_progress_events <- function(
  events,
  session_id,
  action = "restore"
) {
  if (
    !is.list(events) ||
      is.data.frame(events) ||
      !is.null(names(events))
  ) {
    tempest_session_snapshot_value_abort(
      "Session progress events must be an unnamed list.",
      action
    )
  }
  event_fields <- c(
    "event_id",
    "run_id",
    "workflow",
    "event_type",
    "stage",
    "step",
    "status",
    "timestamp",
    "message",
    "payload",
    "parent_event_id",
    "correlation_id",
    "sequence"
  )
  event_props <- setdiff(event_fields, "sequence")
  optional <- c(
    "stage",
    "step",
    "message",
    "parent_event_id",
    "correlation_id"
  )
  events <- lapply(events, function(event) {
    if (
      !is.list(event) ||
        is.data.frame(event) ||
        is.null(names(event)) ||
        anyNA(names(event)) ||
        anyDuplicated(names(event)) ||
        !setequal(names(event), event_fields)
    ) {
      tempest_session_snapshot_value_abort(
        "Session progress events must contain exactly the current fields.",
        action
      )
    }
    for (field in optional) {
      if (
        is.character(event[[field]]) &&
          length(event[[field]]) == 1L &&
          is.na(event[[field]])
      ) {
        event[field] <- list(NULL)
      }
    }
    event
  })
  events <- tempest_session_snapshot_record(
    events,
    "progress_events",
    action = action
  )
  records <- lapply(events, function(event) {
    for (field in optional) {
      if (is.null(event[[field]])) {
        event[[field]] <- NA_character_
      }
    }
    sequence <- event$sequence
    if (
      !is.numeric(sequence) ||
        length(sequence) != 1L ||
        is.na(sequence) ||
        !is.finite(sequence) ||
        sequence < 1L ||
        sequence != as.integer(sequence)
    ) {
      tempest_session_snapshot_value_abort(
        "Session progress-event sequences must be explicit positive integers.",
        action
      )
    }
    record <- tryCatch(
      tempest_progress_event_data(do.call(
        tempest_progress_event,
        event[event_props]
      )),
      error = function(error) {
        tempest_session_snapshot_value_abort(
          "Session progress events contain invalid values.",
          action,
          parent = error
        )
      }
    )
    record$sequence <- as.integer(sequence)
    if (identical(action, "snapshot")) {
      for (field in optional) {
        if (is.na(record[[field]])) {
          record[field] <- list(NULL)
        }
      }
    }
    record
  })
  if (length(records) == 0L) {
    return(list())
  }
  sequences <- vapply(records, `[[`, integer(1), "sequence")
  event_ids <- vapply(records, `[[`, character(1), "event_id")
  run_ids <- vapply(records, `[[`, character(1), "run_id")
  workflows <- vapply(records, `[[`, character(1), "workflow")
  if (
    !identical(sequences, seq_along(records)) ||
      anyDuplicated(event_ids) ||
      any(run_ids != session_id) ||
      any(workflows != "costorm")
  ) {
    tempest_session_snapshot_value_abort(
      paste0(
        "Session progress events require contiguous sequences, unique ids, ",
        "and exact Co-STORM session binding."
      ),
      action
    )
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
tempest_session_bundle_manifest_fields <- function() {
  c(
    "schema_version",
    "bundle_type",
    "bundle_status",
    "package_version",
    "session_id",
    "topic",
    "title",
    "research_manifest",
    "workspace",
    "saved_at",
    "files",
    "checksums"
  )
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
  if (!identical(schema_version, 5L)) {
    tempest_unsupported_format_abort(
      "Co-STORM bundle format",
      schema_version,
      tempest_session_persistence_error_class(
        "tempest_session_restore_error"
      )
    )
  }
  manifest_fields <- names(manifest)
  if (
    is.null(manifest_fields) ||
      anyNA(manifest_fields) ||
      anyDuplicated(manifest_fields) ||
      !setequal(manifest_fields, tempest_session_bundle_manifest_fields())
  ) {
    tempest_unsupported_format_abort(
      "Co-STORM bundle format",
      schema_version,
      tempest_session_persistence_error_class(
        "tempest_session_restore_error"
      )
    )
  }
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
  files <- tempest_persistence_manifest_files(
    manifest$files,
    "Schema 5 Co-STORM file inventory",
    tempest_session_persistence_error_class(
      "tempest_session_restore_error"
    )
  )
  evidence_required <- c(
    "workspace/retrieved_resources.json",
    "workspace/proposed_claims.json",
    "workspace/evidence_spans.json",
    "workspace/disputes.json",
    "workspace/citation_audit.json"
  )
  core_required <- c(
    "experts.json",
    "expert_sessions.json",
    "transcript.json",
    "mindmap.json",
    "progress_events.json",
    evidence_required
  )
  optional_presentation <- tempest_session_bundle_optional_presentation_files()
  snapshot_path <- tempest_graft_snapshot_relative_path()
  knowledge_reference <- manifest$research_manifest$knowledge_snapshot %||%
    list()
  pinned <- length(knowledge_reference) > 0L
  required <- c(core_required, if (pinned) snapshot_path else character())
  allowed <- c(
    core_required,
    optional_presentation,
    "report.md",
    if (pinned) snapshot_path else character()
  )
  normalized_files <- gsub("\\\\", "/", files)
  physical_files <- setdiff(
    gsub(
      "\\\\",
      "/",
      list.files(
        bundle_dir,
        recursive = TRUE,
        all.files = TRUE,
        no.. = TRUE
      )
    ),
    "session.json"
  )
  undeclared_on_disk <- setdiff(physical_files, normalized_files)
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
  checksums <- tempest_persistence_manifest_checksums(
    manifest$checksums,
    files,
    "Schema 5 Co-STORM checksum inventory",
    tempest_session_persistence_error_class(
      "tempest_session_restore_error"
    )
  )
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
  undeclared_required <- setdiff(required, files)
  unexpected_files <- setdiff(files, allowed)
  snapshot_exists <- file.exists(file.path(bundle_dir, snapshot_path))

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
    if (length(unexpected_files) > 0L) {
      paste0(
        "Manifest declares unsupported files: ",
        paste(unexpected_files, collapse = ", "),
        "."
      )
    },
    if (length(undeclared_on_disk) > 0L) {
      paste0(
        "Bundle contains undeclared files: ",
        paste(undeclared_on_disk, collapse = ", "),
        "."
      )
    },
    if (!identical(pinned, snapshot_path %in% files)) {
      paste0(
        "The Graft snapshot sidecar and research manifest must either both ",
        "be declared or both be absent."
      )
    },
    if (!pinned && snapshot_exists) {
      "An unpinned bundle cannot contain a Graft snapshot sidecar."
    },
    if (length(extra_checksums) > 0L) {
      paste0(
        "Manifest contains checksums for undeclared files: ",
        paste(extra_checksums, collapse = ", "),
        "."
      )
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
#' [TempestConfig]. Historical progress events are
#' loaded for display and reduction, but they are not replayed into `progress`.
#'
#' @param path Directory containing a session bundle.
#' @param config Runtime [TempestConfig] used to recreate chats, retrievers, and
#'   tools.
#' @param progress Optional callback for future `tempest_progress_event`
#'   objects.
#' @param partial_recovery Whether to allow explicitly requested recovery when
#'   allowlisted presentation files are missing or fail integrity checks. All
#'   other declared files, including expert, workspace, report, and Graft
#'   snapshot state, must pass integrity checks.
#' @param program_set A [TempestProgramSet] carrying the same program
#'   identities recorded in the bundle. If `NULL`, the builtin set is used.
#' @return A restored [TempestSession].
#' @export
tempest_session_resume <- function(
  path,
  config = tempest_config(),
  progress = NULL,
  partial_recovery = FALSE,
  program_set = NULL
) {
  tempest_session_resume_internal(
    path = path,
    config = config,
    runtime = tempest_runtime(),
    connection_permissions = NULL,
    progress = progress,
    partial_recovery = partial_recovery,
    program_set = program_set
  )
}

#' @keywords internal
tempest_session_resume_internal <- function(
  path,
  config = tempest_config(),
  runtime = tempest_runtime(),
  connection_permissions = NULL,
  progress = NULL,
  partial_recovery = FALSE,
  program_set = NULL
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
  if (!identical(schema_version, 5L)) {
    tempest_unsupported_format_abort(
      "Co-STORM bundle format",
      schema_version,
      tempest_session_persistence_error_class(
        "tempest_session_restore_error"
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
  report_md <- if ("report.md" %in% declared_files) {
    tryCatch(
      tempest_read_text(file.path(bundle_dir, "report.md")),
      error = function(error) {
        tempest_session_restore_abort(
          paste0(
            "Could not read the persisted Co-STORM report: ",
            conditionMessage(error)
          )
        )
      }
    )
  } else {
    NULL
  }
  graft_snapshot <- tempest_graft_snapshot_read(
    bundle_dir,
    declared_files = declared_files,
    manifest_reference = manifest$research_manifest$knowledge_snapshot %||%
      list(),
    class = tempest_session_persistence_error_class(
      "tempest_session_restore_error"
    )
  )

  snapshot <- list(
    schema_version = schema_version,
    package_version = manifest$package_version %||% NA_character_,
    research_manifest = manifest$research_manifest,
    topic = manifest$topic,
    title = manifest$title,
    session_id = manifest$session_id,
    experts = strict_json(
      "experts.json",
      what = "session expert profiles"
    ),
    transcript = strict_json(
      "transcript.json",
      what = "session transcript"
    ),
    mindmap = strict_json(
      "mindmap.json",
      what = "session mind map"
    ),
    report_md = report_md,
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
    graft_snapshot = graft_snapshot
  )
  workspace <- manifest$workspace
  workspace$retrieved_resources <- strict_json(
    "workspace/retrieved_resources.json",
    what = "session retrieved-resource ledger"
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

  tempest_session_restore_internal(
    snapshot,
    config = config,
    runtime = runtime,
    connection_permissions = connection_permissions,
    progress = progress,
    program_set = program_set
  )
}

#' @keywords internal
tempest_restore_citation_audit <- function(citation_audit) {
  if (is.null(citation_audit)) {
    return(NULL)
  }
  if (
    !is.list(citation_audit) ||
      is.data.frame(citation_audit) ||
      !is.null(names(citation_audit))
  ) {
    tempest_research_workspace_restore_abort(
      "Citation audit must be an unnamed list of current-schema rows."
    )
  }
  if (length(citation_audit) == 0L) {
    return(tibble::tibble(
      claim_id = character(),
      claim_text = character(),
      verification_status = character(),
      support_score = numeric(),
      rationale = character()
    ))
  }
  fields <- c(
    "claim_id",
    "claim_text",
    "verification_status",
    "support_score",
    "rationale"
  )
  rows <- lapply(citation_audit, function(row) {
    row_fields <- names(row)
    if (
      !is.list(row) ||
        is.data.frame(row) ||
        is.null(row_fields) ||
        anyNA(row_fields) ||
        anyDuplicated(row_fields) ||
        !setequal(row_fields, fields)
    ) {
      tempest_research_workspace_restore_abort(
        "Citation-audit rows do not match the current schema."
      )
    }
    for (field in c("claim_id", "claim_text", "verification_status")) {
      if (
        !rlang::is_string(row[[field]]) ||
          is.na(row[[field]]) ||
          !nzchar(tempest_trim(row[[field]]))
      ) {
        tempest_research_workspace_restore_abort(
          "Citation-audit rows contain an invalid {.field {field}}."
        )
      }
    }
    if (
      !is.null(row$rationale) &&
        (!rlang::is_string(row$rationale) || is.na(row$rationale))
    ) {
      tempest_research_workspace_restore_abort(
        "Citation-audit rationale must be one string or null."
      )
    }
    if (
      !is.null(row$support_score) &&
        (!is.numeric(row$support_score) ||
          length(row$support_score) != 1L ||
          is.na(row$support_score) ||
          !is.finite(row$support_score) ||
          row$support_score < 0 ||
          row$support_score > 1)
    ) {
      tempest_research_workspace_restore_abort(
        "Citation-audit support scores must be null or finite values in [0, 1]."
      )
    }
    row
  })
  tibble::tibble(
    claim_id = vapply(rows, `[[`, character(1), "claim_id"),
    claim_text = vapply(rows, `[[`, character(1), "claim_text"),
    verification_status = vapply(
      rows,
      `[[`,
      character(1),
      "verification_status"
    ),
    support_score = vapply(
      rows,
      \(row) row$support_score %||% NA_real_,
      numeric(1)
    ),
    rationale = vapply(
      rows,
      \(row) row$rationale %||% NA_character_,
      character(1)
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
tempest_storm_persistence_abort <- function(message, action, parent = NULL) {
  suffix <- if (identical(action, "restore")) {
    "tempest_run_restore_error"
  } else {
    "tempest_run_persistence_error"
  }
  tempest_abort(
    message,
    class = tempest_persistence_error_class(suffix),
    parent = parent,
    .envir = rlang::caller_env()
  )
}

#' @keywords internal
tempest_storm_record_strings <- function(value, field, action) {
  is_character_array <- is.character(value) && is.null(names(value))
  is_scalar_string_list <- is.list(value) &&
    !is.data.frame(value) &&
    is.null(names(value)) &&
    all(vapply(value, rlang::is_string, logical(1)))
  if (!is_character_array && !is_scalar_string_list) {
    tempest_storm_persistence_abort(
      "STORM {.field {field}} must be a flat array of strings.",
      action
    )
  }
  if (is_scalar_string_list) {
    value <- vapply(value, `[[`, character(1), 1L, USE.NAMES = FALSE)
  }
  if (
    anyNA(value) ||
      any(!nzchar(tempest_trim(value))) ||
      anyDuplicated(value)
  ) {
    tempest_storm_persistence_abort(
      "STORM {.field {field}} must contain unique non-empty strings.",
      action
    )
  }
  unname(value)
}

#' @keywords internal
tempest_storm_validate_perspectives <- function(perspectives, action) {
  valid <- is.list(perspectives) &&
    !is.data.frame(perspectives) &&
    is.null(names(perspectives))
  if (!isTRUE(valid)) {
    tempest_storm_persistence_abort(
      "STORM perspectives must be an unnamed list of records.",
      action
    )
  }
  for (perspective in perspectives) {
    fields <- names(perspective)
    if (
      !is.list(perspective) ||
        is.data.frame(perspective) ||
        is.null(fields) ||
        anyNA(fields) ||
        anyDuplicated(fields) ||
        !setequal(fields, c("name", "description", "key_questions")) ||
        !rlang::is_string(perspective$name) ||
        is.na(perspective$name) ||
        !nzchar(tempest_trim(perspective$name)) ||
        !rlang::is_string(perspective$description) ||
        is.na(perspective$description) ||
        !nzchar(tempest_trim(perspective$description))
    ) {
      tempest_storm_persistence_abort(
        "STORM perspective records do not match the current schema.",
        action
      )
    }
    questions <- tempest_storm_record_strings(
      perspective$key_questions,
      "key_questions",
      action
    )
    if (length(questions) == 0L) {
      tempest_storm_persistence_abort(
        "Each STORM perspective requires at least one research question.",
        action
      )
    }
  }
  invisible(perspectives)
}

#' @keywords internal
tempest_storm_validate_outline <- function(outline, field, action) {
  fields <- names(outline)
  if (
    !is.list(outline) ||
      is.data.frame(outline) ||
      is.null(fields) ||
      anyNA(fields) ||
      anyDuplicated(fields) ||
      !setequal(fields, c("title", "sections")) ||
      !rlang::is_string(outline$title) ||
      is.na(outline$title) ||
      !nzchar(tempest_trim(outline$title)) ||
      !is.list(outline$sections) ||
      is.data.frame(outline$sections) ||
      !is.null(names(outline$sections)) ||
      length(outline$sections) == 0L
  ) {
    tempest_storm_persistence_abort(
      "STORM {.field {field}} does not match the current outline schema.",
      action
    )
  }
  for (section in outline$sections) {
    section_fields <- names(section)
    if (
      !is.list(section) ||
        is.data.frame(section) ||
        is.null(section_fields) ||
        anyNA(section_fields) ||
        anyDuplicated(section_fields) ||
        !setequal(
          section_fields,
          c("title", "summary", "subsections")
        ) ||
        !rlang::is_string(section$title) ||
        is.na(section$title) ||
        !nzchar(tempest_trim(section$title)) ||
        !rlang::is_string(section$summary) ||
        is.na(section$summary) ||
        !is.list(section$subsections) ||
        is.data.frame(section$subsections) ||
        !is.null(names(section$subsections))
    ) {
      tempest_storm_persistence_abort(
        "STORM outline sections do not match the current schema.",
        action
      )
    }
    for (subsection in section$subsections) {
      subsection_fields <- names(subsection)
      if (
        !is.list(subsection) ||
          is.data.frame(subsection) ||
          is.null(subsection_fields) ||
          anyNA(subsection_fields) ||
          anyDuplicated(subsection_fields) ||
          !setequal(
            subsection_fields,
            c("title", "bullets", "needed")
          ) ||
          !rlang::is_string(subsection$title) ||
          is.na(subsection$title) ||
          !nzchar(tempest_trim(subsection$title))
      ) {
        tempest_storm_persistence_abort(
          "STORM outline subsections do not match the current schema.",
          action
        )
      }
      tempest_storm_record_strings(
        subsection$bullets,
        "outline bullets",
        action
      )
      tempest_storm_record_strings(
        subsection$needed,
        "outline needed questions",
        action
      )
    }
  }
  invisible(outline)
}

#' @keywords internal
tempest_storm_reference_fields <- function() {
  c(
    "id",
    "url",
    "title",
    "snippet",
    "content_text",
    "context_text",
    "fetched_at",
    "content_hash",
    "meta"
  )
}

#' @keywords internal
tempest_storm_validate_references <- function(state, workspace, action) {
  references <- state$references
  valid <- is.list(references) &&
    !is.data.frame(references) &&
    is.null(names(references))
  if (!isTRUE(valid)) {
    tempest_storm_persistence_abort(
      "STORM references must be an unnamed list of source records.",
      action
    )
  }
  reference_ids <- character()
  for (reference in references) {
    fields <- names(reference)
    if (
      !is.list(reference) ||
        is.data.frame(reference) ||
        is.null(fields) ||
        anyNA(fields) ||
        anyDuplicated(fields) ||
        !setequal(fields, tempest_storm_reference_fields()) ||
        !rlang::is_string(reference$id) ||
        is.na(reference$id) ||
        !nzchar(tempest_trim(reference$id)) ||
        !is.list(reference$meta) ||
        is.data.frame(reference$meta)
    ) {
      tempest_storm_persistence_abort(
        "STORM reference records do not match the current source schema.",
        action
      )
    }
    reference_ids <- c(reference_ids, reference$id)
    expected <- workspace$get_retrieved_source(reference$id)
    matches_workspace <- !is.null(expected) &&
      tryCatch(
        identical(
          tempest_storm_state_record_value(reference, "reference"),
          tempest_storm_state_record_value(expected, "reference")
        ),
        error = function(error) FALSE
      )
    if (!matches_workspace) {
      tempest_storm_persistence_abort(
        "A STORM reference does not match its authoritative workspace source.",
        action
      )
    }
  }
  if (anyDuplicated(reference_ids)) {
    tempest_storm_persistence_abort(
      "STORM references cannot contain duplicate source ids.",
      action
    )
  }
  cited_md <- state$report_md %||% state$draft_md %||% ""
  cited_ids <- unique(tempest_extract_citation_ids(cited_md))
  if (!identical(reference_ids, cited_ids)) {
    tempest_storm_persistence_abort(
      paste0(
        "STORM references must exactly match citations in the durable ",
        "report product."
      ),
      action
    )
  }
  invisible(references)
}

#' @keywords internal
tempest_storm_validate_persisted_state <- function(
  state,
  workspace,
  action = c("save", "restore")
) {
  action <- match.arg(action)
  perspectives_complete <- "perspectives" %in% state$completed_stages
  if (length(state$perspectives) > 0L) {
    tempest_storm_validate_perspectives(state$perspectives, action)
  }
  if (
    perspectives_complete &&
      (length(state$perspectives) == 0L ||
        length(state$experts) == 0L ||
        length(state$perspectives) != length(state$experts))
  ) {
    tempest_storm_persistence_abort(
      paste0(
        "A completed STORM perspectives stage requires a non-empty ",
        "one-to-one perspective and expert pairing."
      ),
      action
    )
  }
  if (
    length(state$perspectives) > 0L &&
      length(state$experts) > 0L &&
      length(state$perspectives) != length(state$experts)
  ) {
    tempest_storm_persistence_abort(
      "STORM perspective and expert records must remain one-to-one.",
      action
    )
  }
  if ("outline" %in% state$completed_stages) {
    tempest_storm_validate_outline(
      state$draft_outline,
      "draft_outline",
      action
    )
    tempest_storm_validate_outline(state$outline, "outline", action)
  }
  if (
    "write" %in%
      state$completed_stages &&
      (!rlang::is_string(state$draft_md) ||
        is.na(state$draft_md) ||
        !nzchar(tempest_trim(state$draft_md)))
  ) {
    tempest_storm_persistence_abort(
      "A completed STORM write stage requires a non-empty draft.",
      action
    )
  }
  if (
    "polish" %in%
      state$completed_stages &&
      (!rlang::is_string(state$report_md) ||
        is.na(state$report_md) ||
        !nzchar(tempest_trim(state$report_md)))
  ) {
    tempest_storm_persistence_abort(
      "A completed STORM polish stage requires a non-empty report.",
      action
    )
  }
  tempest_storm_validate_references(state, workspace, action)
  invisible(state)
}

#' @keywords internal
tempest_run_bundle_manifest_fields <- function() {
  c(
    "topic",
    "title",
    "completed_stages",
    "schema_version",
    "bundle_type",
    "bundle_status",
    "research_manifest",
    "workspace",
    "files",
    "checksums"
  )
}

#' @keywords internal
tempest_run_bundle_owned_files <- function(include_manifest = FALSE) {
  files <- c(
    "workspace.json",
    "perspectives.json",
    "experts.json",
    "direct_gen_outline.json",
    "storm_gen_outline.json",
    "lead_section.md",
    "storm_gen_article.md",
    "storm_gen_article_polished.md",
    "references.json",
    tempest_graft_snapshot_relative_path()
  )
  if (isTRUE(include_manifest)) c("run_config.json", files) else files
}

#' @keywords internal
tempest_run_bundle_validate_manifest <- function(run_dir, manifest) {
  schema_version <- tempest_persistence_schema_version(
    manifest$schema_version %||% NA_integer_,
    "STORM run schema version",
    tempest_persistence_error_class("tempest_run_restore_error")
  )
  if (!identical(schema_version, 4L)) {
    tempest_unsupported_format_abort(
      "STORM bundle format",
      schema_version,
      tempest_persistence_error_class("tempest_run_restore_error")
    )
  }
  manifest_fields <- names(manifest)
  if (
    is.null(manifest_fields) ||
      anyNA(manifest_fields) ||
      anyDuplicated(manifest_fields) ||
      !setequal(manifest_fields, tempest_run_bundle_manifest_fields())
  ) {
    tempest_unsupported_format_abort(
      "STORM bundle format",
      schema_version,
      tempest_persistence_error_class("tempest_run_restore_error")
    )
  }
  valid_header <- identical(manifest$bundle_type %||% "", "storm") &&
    identical(manifest$bundle_status %||% "", "complete") &&
    is.list(manifest$research_manifest) &&
    rlang::is_string(manifest$topic) &&
    !is.na(manifest$topic) &&
    nzchar(tempest_trim(manifest$topic)) &&
    rlang::is_string(manifest$title) &&
    !is.na(manifest$title) &&
    nzchar(tempest_trim(manifest$title))
  if (!isTRUE(valid_header)) {
    tempest_abort(
      "STORM run manifest is incomplete or uses an unsupported schema.",
      class = tempest_persistence_error_class(
        "tempest_run_restore_error"
      )
    )
  }
  completed_stages <- tryCatch(
    tempest_storm_state_completed_stages(
      manifest$completed_stages,
      from_record = TRUE
    ),
    error = function(error) {
      tempest_abort(
        "STORM run manifest has invalid completed-stage metadata.",
        class = tempest_persistence_error_class(
          "tempest_run_restore_error"
        ),
        parent = error
      )
    }
  )
  files <- tempest_persistence_manifest_files(
    manifest$files,
    "Schema 4 STORM file inventory",
    tempest_persistence_error_class("tempest_run_restore_error")
  )
  normalized <- gsub("\\\\", "/", files)
  physical_files <- setdiff(
    gsub(
      "\\\\",
      "/",
      list.files(
        run_dir,
        recursive = TRUE,
        all.files = TRUE,
        no.. = TRUE
      )
    ),
    "run_config.json"
  )
  undeclared_on_disk <- setdiff(physical_files, normalized)
  snapshot_path <- tempest_graft_snapshot_relative_path()
  knowledge_reference <- manifest$research_manifest$knowledge_snapshot %||%
    list()
  pinned <- length(knowledge_reference) > 0L
  required <- c(
    "workspace.json",
    "experts.json",
    "references.json",
    if (pinned) snapshot_path else character()
  )
  allowed <- setdiff(
    tempest_run_bundle_owned_files(),
    if (pinned) character() else snapshot_path
  )
  stage_required <- tempest_storm_stage_required_files(
    completed_stages
  )
  unsafe <- !vapply(
    normalized,
    tempest_artifact_bundle_path_is_safe,
    logical(1)
  )
  missing <- files[!file.exists(file.path(run_dir, files))]
  checksums <- tempest_persistence_manifest_checksums(
    manifest$checksums,
    files,
    "Schema 4 STORM checksum inventory",
    tempest_persistence_error_class("tempest_run_restore_error")
  )
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
  snapshot_exists <- file.exists(file.path(run_dir, snapshot_path))
  problems <- c(
    if (length(files) == 0L) "Manifest declares no files.",
    if (anyDuplicated(normalized)) "Manifest declares duplicate files.",
    if (any(unsafe)) "Manifest contains unsafe paths.",
    if (length(setdiff(required, files)) > 0L) {
      "Manifest omits required bundle files."
    },
    if (length(setdiff(files, allowed)) > 0L) {
      paste0(
        "Manifest declares unsupported files: ",
        paste(setdiff(files, allowed), collapse = ", "),
        "."
      )
    },
    if (length(undeclared_on_disk) > 0L) {
      paste0(
        "Bundle contains undeclared files: ",
        paste(undeclared_on_disk, collapse = ", "),
        "."
      )
    },
    if (!identical(pinned, snapshot_path %in% files)) {
      paste0(
        "The Graft snapshot sidecar and research manifest must either both ",
        "be declared or both be absent."
      )
    },
    if (!pinned && snapshot_exists) {
      "An unpinned STORM bundle cannot contain a Graft snapshot sidecar."
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
  snapshot <- workspace$graft_snapshot
  if (is.null(snapshot) && is.null(workspace$base_snapshot_id)) {
    return(list())
  }
  if (is.null(snapshot)) {
    tempest_abort(
      paste0(
        "Pinned STORM execution requires an actual path-free ",
        "graft::GraftSnapshot; a scalar snapshot id is insufficient."
      ),
      class = tempest_persistence_error_class(
        "tempest_run_persistence_error"
      )
    )
  }
  validated <- tempest_graft_snapshot_validate(
    snapshot,
    tempest_persistence_error_class("tempest_run_persistence_error"),
    "STORM Graft snapshot"
  )
  if (!identical(validated$reference$snapshot_id, workspace$base_snapshot_id)) {
    tempest_abort(
      "The STORM Graft snapshot does not match the workspace base snapshot.",
      class = tempest_persistence_error_class(
        "tempest_run_persistence_error"
      )
    )
  }
  validated$reference
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
tempest_storm_program_set_abort <- function(message, action, parent = NULL) {
  class <- if (identical(action, "restore")) {
    tempest_persistence_error_class("tempest_run_restore_error")
  } else {
    tempest_persistence_error_class("tempest_run_persistence_error")
  }
  tempest_abort(
    message,
    class = class,
    parent = parent,
    .envir = rlang::caller_env()
  )
}

#' @keywords internal
tempest_storm_program_set_validate <- function(
  program_set,
  research_manifest,
  action = c("save", "restore")
) {
  action <- match.arg(action)
  if (!S7::S7_inherits(research_manifest, TempestResearchManifest)) {
    tempest_storm_program_set_abort(
      "The STORM research manifest is invalid.",
      action
    )
  }
  if (
    is.null(program_set) ||
      !S7::S7_inherits(program_set, TempestProgramSet)
  ) {
    tempest_storm_program_set_abort(
      paste0(
        "Current STORM bundles require an explicit complete ",
        "TempestProgramSet."
      ),
      action
    )
  }
  declared <- tryCatch(
    tempest_research_manifest_programs(
      tempest_program_set_entries(program_set)
    ),
    error = function(error) {
      tempest_storm_program_set_abort(
        "The supplied STORM ProgramSet is invalid.",
        action,
        parent = error
      )
    }
  )
  required_stages <- tempest_program_set_stages()
  if (
    length(declared) == 0L ||
      !setequal(names(declared), required_stages) ||
      length(research_manifest@programs) == 0L ||
      !setequal(names(research_manifest@programs), required_stages)
  ) {
    tempest_storm_program_set_abort(
      "Current STORM bundles require every exact ProgramSet stage.",
      action
    )
  }
  programs <- tryCatch(
    tempest_program_set_programs(program_set),
    error = function(error) {
      tempest_storm_program_set_abort(
        "The supplied STORM ProgramSet cannot resolve its programs.",
        action,
        parent = error
      )
    }
  )
  same_identity <- tryCatch(
    tempest_program_set_identity_equal(declared, research_manifest@programs),
    error = function(error) {
      tempest_storm_program_set_abort(
        "The STORM ProgramSet references are malformed.",
        action,
        parent = error
      )
    }
  )
  if (!isTRUE(same_identity)) {
    tempest_storm_program_set_abort(
      paste0(
        "The supplied STORM ProgramSet identity does not match the ",
        "persisted research manifest."
      ),
      action
    )
  }
  for (stage in required_stages) {
    actual_id <- tryCatch(
      dsprrr::program_artifact_id(programs[[stage]]),
      error = function(error) {
        tempest_storm_program_set_abort(
          "The STORM program for stage {.val {stage}} is corrupt.",
          action,
          parent = error
        )
      }
    )
    if (
      !identical(actual_id, declared[[stage]]$program_artifact_id) ||
        !identical(
          actual_id,
          research_manifest@programs[[stage]]$program_artifact_id
        )
    ) {
      tempest_storm_program_set_abort(
        paste0(
          "The recomputed dsprrr identity for STORM stage ",
          "{.val {stage}} does not match its declared program artifact."
        ),
        action
      )
    }
  }
  program_set
}

#' @keywords internal
tempest_storm_restore_workspace <- function(
  metadata,
  config,
  graft_snapshot = NULL
) {
  schema_version <- tempest_persistence_schema_version(
    metadata$schema_version %||% NA_integer_,
    "STORM run schema version",
    tempest_persistence_error_class("tempest_run_restore_error")
  )
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
  identity_fields <- names(identity)
  if (
    is.null(identity_fields) ||
      anyNA(identity_fields) ||
      anyDuplicated(identity_fields) ||
      !setequal(identity_fields, required)
  ) {
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
    graft_snapshot = graft_snapshot,
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
    "retrieved_resources",
    "proposed_claims",
    "evidence_spans",
    "disputes"
  )
  all(vapply(snapshot[evidence_fields], length, integer(1)) == 0L) &&
    is.null(snapshot$citation_audit)
}

tempest_storm_assert_workspace_equivalent <- function(supplied, persisted) {
  if (is.null(supplied)) {
    return(persisted)
  }
  snapshot_values <- function(workspace, label) {
    snapshot <- workspace$graft_snapshot
    if (is.null(snapshot)) {
      return(NULL)
    }
    tempest_graft_snapshot_validate(
      snapshot,
      tempest_persistence_error_class("tempest_run_restore_error"),
      label
    )$values
  }
  if (
    !identical(
      snapshot_values(supplied, "Supplied workspace Graft snapshot"),
      snapshot_values(persisted, "Persisted workspace Graft snapshot")
    )
  ) {
    tempest_storm_run_restore_abort(
      "The supplied workspace does not retain the persisted Graft snapshot."
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
  supplied <- tryCatch(
    tempest_research_workspace_restore(
      tempest_research_workspace_snapshot(persisted),
      workspace = supplied,
      graft_snapshot = persisted$graft_snapshot
    ),
    error = function(error) {
      tempest_storm_run_restore_abort(
        "The persisted STORM workspace cannot be restored into the supplied workspace.",
        parent = error
      )
    }
  )
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
  program_set,
  run_dir,
  run_id = NULL
) {
  schema_version <- tempest_persistence_schema_version(
    metadata$schema_version %||% NA_integer_,
    "STORM run schema version",
    tempest_persistence_error_class("tempest_run_restore_error")
  )
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
  if (
    identical(manifest@status, "succeeded") &&
      !tempest_storm_state_is_complete(state)
  ) {
    tempest_storm_run_restore_abort(
      paste0(
        "A succeeded STORM research manifest requires a completed polish ",
        "stage and a non-empty report."
      )
    )
  }
  tryCatch(
    tempest_graft_snapshot_assert_binding(
      workspace$graft_snapshot,
      manifest@knowledge_snapshot,
      workspace,
      tempest_persistence_error_class("tempest_run_restore_error"),
      "Restored STORM Graft snapshot"
    ),
    error = function(error) {
      tempest_storm_run_restore_abort(
        "The persisted STORM accepted-knowledge identity is invalid.",
        parent = error
      )
    }
  )
  tempest_storm_program_set_validate(
    program_set,
    manifest,
    action = "restore"
  )
  manifest
}

#' @keywords internal
tempest_storm_read_state <- function(
  run_dir,
  paths,
  metadata,
  path_is_declared,
  workspace
) {
  read_json_artifact <- function(name, default = NULL) {
    path <- paths[[name]]
    if (!path_is_declared(path) || !file.exists(path)) {
      return(default)
    }
    tempest_read_json_strict(
      path,
      what = paste("STORM", gsub("_", " ", name)),
      class = tempest_persistence_error_class(
        "tempest_run_restore_error"
      )
    )
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
  completed_stages <- tempest_storm_state_completed_stages(
    metadata$completed_stages,
    from_record = TRUE
  )
  topic <- metadata$topic
  title <- metadata$title
  state <- tryCatch(
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
  tempest_storm_validate_persisted_state(
    state,
    workspace,
    action = "restore"
  )
  state
}

#' @keywords internal
tempest_load_run_artifacts <- function(
  run_dir,
  workspace = NULL,
  config = tempest_config(),
  program_set = NULL,
  run_id = NULL
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
  graft_snapshot <- tempest_graft_snapshot_read(
    run_dir,
    declared_files = declared_files,
    manifest_reference = metadata$research_manifest$knowledge_snapshot %||%
      list(),
    class = tempest_persistence_error_class("tempest_run_restore_error")
  )
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

  workspace <- tempest_storm_restore_workspace(
    metadata,
    config,
    graft_snapshot = graft_snapshot
  )
  manifest_workspace_identity <- tempest_storm_workspace_identity_record(
    workspace
  )
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
      workspace = workspace,
      graft_snapshot = graft_snapshot
    ),
    error = function(error) {
      tempest_storm_run_restore_abort(
        "The persisted STORM research workspace is invalid.",
        parent = error
      )
    }
  )
  restored_workspace_identity <- tempest_storm_workspace_identity_record(
    workspace
  )
  if (!identical(restored_workspace_identity, manifest_workspace_identity)) {
    tempest_storm_run_restore_abort(
      paste0(
        "The persisted STORM workspace does not exactly match its manifest ",
        "identity."
      )
    )
  }
  state <- tempest_storm_read_state(
    run_dir,
    paths,
    metadata,
    path_is_declared,
    workspace
  )
  research_manifest <- tempest_storm_restore_manifest(
    metadata,
    workspace,
    state,
    config,
    program_set,
    run_dir,
    run_id = run_id
  )

  workspace <- tempest_storm_assert_workspace_equivalent(
    supplied_workspace,
    workspace
  )

  list(
    metadata = metadata,
    completed_stages = state$completed_stages,
    research_manifest = research_manifest,
    program_set = program_set,
    state = state,
    workspace = workspace
  )
}

#' @keywords internal
tempest_save_run_artifacts <- function(
  run_dir,
  workspace,
  state,
  research_manifest,
  program_set = NULL,
  config,
  steps,
  research_strategy,
  parallel_writing = FALSE,
  remove_duplicate = FALSE
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
  tempest_storm_program_set_validate(
    program_set,
    research_manifest,
    action = "save"
  )
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
  tryCatch(
    tempest_graft_snapshot_assert_binding(
      workspace$graft_snapshot,
      research_manifest@knowledge_snapshot,
      workspace,
      tempest_persistence_error_class(
        "tempest_run_persistence_error"
      ),
      "STORM Graft snapshot"
    ),
    error = function(error) {
      tempest_abort(
        "{.arg research_manifest} does not match the workspace snapshot.",
        class = tempest_persistence_error_class(
          "tempest_run_persistence_error"
        ),
        parent = error
      )
    }
  )
  paths <- tempest_run_artifact_paths(run_dir)
  completed_stages <- state$completed_stages

  existing_files <- list.files(
    run_dir,
    recursive = TRUE,
    all.files = TRUE,
    no.. = TRUE
  )
  unowned_files <- setdiff(
    gsub("\\\\", "/", existing_files),
    tempest_run_bundle_owned_files(include_manifest = TRUE)
  )
  if (length(unowned_files) > 0L) {
    tempest_abort(
      paste0(
        "Cannot save a STORM bundle over unsupported or unowned files: ",
        paste(unowned_files, collapse = ", "),
        "."
      ),
      class = tempest_persistence_error_class(
        "tempest_run_persistence_error"
      )
    )
  }

  metadata <- list(
    topic = state$topic,
    title = state$title,
    completed_stages = completed_stages
  )

  tempest_write_json(
    paths$workspace,
    tempest_research_workspace_snapshot(workspace)
  )
  # References are the sources actually cited in the report/draft, not a copy
  # of every collected source.
  cited_md <- state$report_md %||%
    state$draft_md %||%
    ""
  cited_ids <- tempest_extract_citation_ids(cited_md)
  references <- Filter(
    Negate(is.null),
    lapply(cited_ids, function(id) workspace$get_retrieved_source(id))
  )
  state$references <- references
  state <- tempest_storm_state_validate(state)
  tempest_storm_validate_persisted_state(
    state,
    workspace,
    action = "save"
  )
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

  if (is.null(workspace$graft_snapshot)) {
    if (file.exists(paths$graft_snapshot)) {
      unlink(paths$graft_snapshot)
    }
  } else {
    tempest_graft_snapshot_write(
      run_dir,
      workspace$graft_snapshot,
      tempest_persistence_error_class(
        "tempest_run_persistence_error"
      )
    )
  }
  files <- sort(Filter(
    function(file) file.exists(file.path(run_dir, file)),
    tempest_run_bundle_owned_files()
  ))
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
