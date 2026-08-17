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
    references = file.path(run_dir, "references.json"),
    stage_records = file.path(run_dir, "stage_records.json")
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
  tempest_exact_integer_scalar(
    value,
    what,
    class = class,
    minimum = 0L
  )
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
tempest_persistence_leaf_path_is_symlink <- function(path) {
  expanded <- path.expand(path)
  without_trailing_separator <- sub("[/\\\\]+$", "", expanded)
  if (nzchar(without_trailing_separator)) {
    expanded <- without_trailing_separator
  }
  link_target <- Sys.readlink(expanded)
  !is.na(link_target) && nzchar(link_target)
}

#' @keywords internal
tempest_persistence_bundle_path_has_symlink <- function(bundle_dir, rel_path) {
  parts <- strsplit(gsub("\\\\", "/", rel_path), "/", fixed = TRUE)[[1]]
  current <- bundle_dir
  for (part in parts) {
    current <- file.path(current, part)
    if (nzchar(Sys.readlink(current))) {
      return(TRUE)
    }
  }
  FALSE
}

#' @keywords internal
tempest_persistence_require_regular_bundle_files <- function(
  bundle_dir,
  files,
  what,
  class
) {
  if (!dir.exists(bundle_dir)) {
    tempest_abort("{what} directory does not exist.", class = class)
  }
  files <- unname(as.character(files))
  safe <- length(files) > 0L &&
    !anyNA(files) &&
    all(nzchar(files)) &&
    !anyDuplicated(files) &&
    all(vapply(files, tempest_artifact_bundle_path_is_safe, logical(1)))
  if (!isTRUE(safe)) {
    tempest_abort(
      "{what} contains missing, duplicate, or unsafe file paths.",
      class = class
    )
  }
  root <- normalizePath(bundle_dir, winslash = "/", mustWork = TRUE)
  root_prefix <- paste0(root, "/")
  problems <- vapply(
    files,
    function(file) {
      path <- file.path(bundle_dir, file)
      if (!file.exists(path)) {
        return("missing")
      }
      if (
        !utils::file_test("-f", path) ||
          tempest_persistence_bundle_path_has_symlink(bundle_dir, file)
      ) {
        return("not a regular non-symlink file")
      }
      resolved <- tryCatch(
        normalizePath(path, winslash = "/", mustWork = TRUE),
        error = function(error) ""
      )
      if (!nzchar(resolved) || !startsWith(resolved, root_prefix)) {
        return("resolves outside the bundle")
      }
      ""
    },
    character(1)
  )
  invalid <- names(problems)[nzchar(problems)]
  if (length(invalid) > 0L) {
    details <- paste0(
      invalid,
      " (",
      unname(problems[invalid]),
      ")",
      collapse = ", "
    )
    tempest_abort(
      "{what} failed regular-file validation: {details}.",
      class = class
    )
  }
  invisible(files)
}

#' @keywords internal
tempest_persistence_credential_audit <- function(value, what, class) {
  evidence_path <- function(path) {
    grepl(
      paste0(
        "\\$workspace\\$retrieved_resources\\[\\[[0-9]+\\]\\]",
        "\\$content($|\\$|\\[\\[)"
      ),
      path,
      perl = TRUE
    ) ||
      grepl(
        paste0(
          "\\$workspace\\$retrieved_resources\\[\\[[0-9]+\\]\\]",
          "\\$metadata\\$(snippet|content_text|context_text|",
          "citation_context|answer_context)($|\\$|\\[\\[)"
        ),
        path,
        perl = TRUE
      ) ||
      grepl(
        paste0(
          "\\$workspace\\$proposed_claims\\[\\[[0-9]+\\]\\]",
          "\\$(claim_text|supporting_quotes)($|\\$|\\[\\[)"
        ),
        path,
        perl = TRUE
      ) ||
      grepl(
        paste0(
          "\\$workspace\\$evidence_spans\\[\\[[0-9]+\\]\\]",
          "\\$quote($|\\$|\\[\\[)"
        ),
        path,
        perl = TRUE
      ) ||
      grepl(
        paste0(
          "\\$state\\$references\\[\\[[0-9]+\\]\\]",
          "\\$(snippet|content_text|context_text)($|\\$|\\[\\[)"
        ),
        path,
        perl = TRUE
      ) ||
      grepl(
        paste0(
          "\\$state\\$references\\[\\[[0-9]+\\]\\]\\$meta",
          "\\$(snippet|content_text|context_text|citation_context|",
          "answer_context)($|\\$|\\[\\[)"
        ),
        path,
        perl = TRUE
      )
  }
  scan <- function(item, path) {
    if (evidence_path(path)) {
      return(character())
    }
    if (is.list(item)) {
      item_names <- names(item)
      sensitive_names <- if (is.null(item_names)) {
        character()
      } else {
        item_names[vapply(
          item_names,
          tempest_research_sensitive_name,
          logical(1)
        )]
      }
      found <- if (length(sensitive_names) == 0L) {
        character()
      } else {
        paste0(path, "$", sensitive_names)
      }
      child_paths <- if (is.null(item_names)) {
        paste0(path, "[[", seq_along(item), "]]")
      } else {
        paste0(path, "$", item_names)
      }
      return(c(
        found,
        unlist(Map(scan, item, child_paths), use.names = FALSE)
      ))
    }
    if (tempest_contract_sensitive_scalar(item)) path else character()
  }
  sensitive <- unique(scan(value, "snapshot"))
  if (length(sensitive) > 0L) {
    tempest_abort(
      paste0(
        "Cannot persist ",
        what,
        "; credential-like data appears outside authoritative evidence at ",
        sensitive[[1]],
        "."
      ),
      class = class
    )
  }
  invisible(value)
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
tempest_research_workspace_snapshot_abort <- function(message, parent = NULL) {
  tempest_abort(
    c("Cannot snapshot ResearchWorkspace.", x = message),
    class = tempest_persistence_error_class(
      "tempest_research_workspace_snapshot_error"
    ),
    parent = parent
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
tempest_research_workspace_array_record <- function(record, fields) {
  for (field in fields) {
    value <- record[[field]] %||% character()
    record[[field]] <- unname(as.list(value))
  }
  record
}

#' @keywords internal
tempest_research_workspace_claim_record <- function(claim) {
  tempest_research_workspace_array_record(
    tempest_claim_to_list(claim),
    c(
      "source_ids",
      "evidence_span_ids",
      "supporting_quotes",
      "contradicting_source_ids"
    )
  )
}

#' @keywords internal
tempest_research_workspace_dispute_record <- function(dispute) {
  tempest_research_workspace_array_record(
    tempest_dispute_to_list(dispute),
    c("claim_ids", "unresolved_questions")
  )
}

#' @keywords internal
tempest_research_workspace_validate_noncontent <- function(
  workspace,
  action = c("snapshot", "restore")
) {
  action <- match.arg(action)
  abort <- if (identical(action, "snapshot")) {
    tempest_research_workspace_snapshot_abort
  } else {
    tempest_research_workspace_restore_abort
  }
  validate_ids <- function(values, field) {
    values <- unname(unlist(values, use.names = FALSE))
    values <- values[!is.na(values)]
    if (
      any(!nzchar(values)) ||
        any(
          !vapply(
            values,
            tempest_research_workspace_reference_id_valid,
            logical(1)
          )
        )
    ) {
      abort(
        paste0(
          "Workspace ",
          field,
          " must contain bounded credential-free identifiers."
        )
      )
    }
  }

  claims <- workspace$list_proposed_claims()
  for (claim in claims) {
    validate_ids(
      c(
        claim@claim_id,
        claim@source_ids,
        claim@evidence_span_ids,
        claim@contradicting_source_ids,
        claim@retrieval_step_id,
        claim@perspective_id,
        claim@expert_id,
        claim@session_id,
        claim@section_id,
        claim@verifier_model
      ),
      "claim provenance"
    )
    if (tempest_contract_sensitive_scalar(claim@verifier_model)) {
      abort("Workspace claim verifier metadata contains a credential value.")
    }
  }
  for (span in workspace$list_evidence_spans()) {
    validate_ids(
      c(span@evidence_span_id, span@source_id, span@extracted_by),
      "evidence-span provenance"
    )
  }
  for (support in workspace$list_claim_supports()) {
    validate_ids(
      c(
        support@claim_support_id,
        support@claim_id,
        support@evidence_span_id,
        support@source_id
      ),
      "claim-support provenance"
    )
    if (tempest_contract_sensitive_scalar(support@rationale)) {
      abort("Workspace claim-support rationale contains a credential value.")
    }
  }
  for (dispute in workspace$list_disputes()) {
    validate_ids(
      c(dispute@dispute_id, dispute@claim_ids),
      "dispute provenance"
    )
  }
  invisible(workspace)
}

#' @keywords internal
tempest_research_workspace_snapshot_fields <- function() {
  c(
    "schema_version",
    "base_snapshot_id",
    "max_sources",
    "accepted_graft_references",
    "retrieved_resources",
    "proposed_claims",
    "evidence_spans",
    "claim_supports",
    "disputes"
  )
}

#' @keywords internal
tempest_research_workspace_snapshot <- function(workspace) {
  if (!inherits(workspace, "ResearchWorkspace")) {
    tempest_research_workspace_snapshot_abort(
      "{.arg workspace} must be a ResearchWorkspace."
    )
  }
  tryCatch(
    workspace$validate_integrity(),
    error = function(error) {
      tempest_research_workspace_snapshot_abort(
        "Workspace cross-record integrity validation failed.",
        parent = error
      )
    }
  )
  tempest_research_workspace_validate_noncontent(workspace, "snapshot")
  entries <- workspace$list_retrieved_resources()
  retrieved_resources <- unname(lapply(
    entries,
    tempest_resource_record,
    include_content = TRUE
  ))

  snapshot <- list(
    schema_version = 5L,
    base_snapshot_id = workspace$base_snapshot_id,
    max_sources = tempest_research_workspace_max_sources_data(
      workspace$max_sources
    ),
    accepted_graft_references = workspace$list_accepted_graft_references(),
    retrieved_resources = retrieved_resources,
    proposed_claims = lapply(
      workspace$list_proposed_claims(),
      tempest_research_workspace_claim_record
    ),
    evidence_spans = lapply(
      workspace$list_evidence_spans(),
      tempest_evidence_span_to_list
    ),
    claim_supports = lapply(
      workspace$list_claim_supports(),
      tempest_claim_support_to_list
    ),
    disputes = lapply(
      workspace$list_disputes(),
      tempest_research_workspace_dispute_record
    )
  )
  snapshot <- tryCatch(
    tempest_storm_state_record_value(snapshot, "workspace"),
    error = function(error) {
      tempest_research_workspace_snapshot_abort(
        "Workspace records must contain only canonical portable values."
      )
    }
  )
  snapshot <- snapshot[tempest_research_workspace_snapshot_fields()]
  tempest_persistence_credential_audit(
    list(workspace = snapshot),
    "ResearchWorkspace snapshot",
    tempest_persistence_error_class(
      "tempest_research_workspace_snapshot_error"
    )
  )
  snapshot
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
  value <- tempest_exact_integer_scalar(
    value,
    "The workspace schema version",
    class = tempest_persistence_error_class(
      "tempest_research_workspace_restore_error"
    ),
    minimum = 0L
  )
  if (!identical(value, 5L)) {
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
tempest_stage_records_verification_projection <- function(records, workspace) {
  records <- tempest_stage_records_validate(records)
  if (!inherits(workspace, "ResearchWorkspace")) {
    tempest_stage_record_abort(
      "{.arg workspace} must be a ResearchWorkspace."
    )
  }
  supports <- workspace$list_claim_supports()
  verification <- Filter(
    function(record) {
      identical(record@stage, "verify_claim_support") &&
        identical(record@status, "succeeded")
    },
    records
  )
  if (length(supports) == 0L) {
    if (length(verification) > 0L) {
      tempest_stage_record_abort(
        paste0(
          "Succeeded verification records cannot exist without the exact ",
          "durable claim-support set."
        )
      )
    }
    return(invisible(list(
      verified_at = NA_character_,
      verifier_model = NA_character_
    )))
  }

  support_ids <- vapply(
    supports,
    \(support) support@claim_support_id,
    character(1)
  )
  record_ids <- vapply(
    verification,
    function(record) {
      ids <- unlist(record@output_reference$ids, use.names = FALSE)
      if (
        !identical(record@output_reference$kind, "claim_supports") ||
          length(ids) != 1L
      ) {
        tempest_stage_record_abort(
          paste0(
            "Each succeeded verification record must bind exactly one ",
            "claim-support assessment."
          )
        )
      }
      ids[[1]]
    },
    character(1)
  )
  if (
    length(record_ids) != length(support_ids) ||
      anyDuplicated(record_ids) ||
      !setequal(record_ids, support_ids)
  ) {
    tempest_stage_record_abort(
      paste0(
        "Succeeded verification records must bind every durable ",
        "claim-support assessment exactly once."
      )
    )
  }
  verification <- verification[match(support_ids, record_ids)]
  verified_at <- vapply(
    verification,
    \(record) record@trace_references$verified_at,
    character(1)
  )
  verifier_model <- vapply(
    verification,
    function(record) {
      record@trace_references$verifier_model %||% NA_character_
    },
    character(1)
  )
  if (
    !all(vapply(
      verified_at,
      tempest_ledger_timestamp_valid,
      logical(1)
    )) ||
      !all(vapply(
        verifier_model,
        tempest_ledger_identifier_valid,
        logical(1),
        optional = TRUE
      )) ||
      !all(vapply(verified_at, identical, logical(1), verified_at[[1]])) ||
      !all(vapply(
        verifier_model,
        identical,
        logical(1),
        verifier_model[[1]]
      ))
  ) {
    tempest_stage_record_abort(
      paste0(
        "A verification batch must bind one exact canonical timestamp and ",
        "optional verifier identity across every claim-support record."
      )
    )
  }
  batch_time <- tempest_stage_time_parse(verified_at[[1]])
  starts <- vapply(
    verification,
    function(record) as.numeric(tempest_stage_time_parse(record@started_at)),
    numeric(1)
  )
  if (is.na(batch_time) || any(as.numeric(batch_time) > starts)) {
    tempest_stage_record_abort(
      paste0(
        "Verification batch time must be at or before every bound stage ",
        "attempt start."
      )
    )
  }
  for (index in seq_along(supports)) {
    support <- supports[[index]]
    claim <- workspace$get_proposed_claim(support@claim_id)
    if (
      is.null(claim) ||
        !identical(claim@verified_at, verified_at[[index]]) ||
        !identical(claim@verifier_model, verifier_model[[index]])
    ) {
      tempest_stage_record_abort(
        paste0(
          "Claim verifier metadata must be the exact projection of its ",
          "authoritative verification-stage proof."
        )
      )
    }
  }
  invisible(list(
    verified_at = verified_at[[1]],
    verifier_model = verifier_model[[1]]
  ))
}

#' @keywords internal
tempest_stage_records_validate_workspace <- function(
  records,
  workspace,
  min_support_score = 0.7
) {
  records <- tempest_stage_records_validate(records)
  if (!inherits(workspace, "ResearchWorkspace")) {
    tempest_stage_record_abort(
      "{.arg workspace} must be a ResearchWorkspace."
    )
  }
  min_support_score <- tempest_normalize_min_support_score(min_support_score)
  claim_ids <- vapply(
    workspace$list_proposed_claims(),
    \(claim) claim@claim_id,
    character(1)
  )
  claim_supports <- workspace$list_claim_supports()
  support_ids <- vapply(
    claim_supports,
    \(support) support@claim_support_id,
    character(1)
  )
  verification_reference_ids <- list()
  for (record in records) {
    if (identical(record@stage, "verify_claim_support")) {
      expected_threshold <- tempest_stage_support_threshold_string(
        min_support_score
      )
      actual_threshold <-
        record@trace_references$min_support_score %||% NULL
      if (!identical(actual_threshold, expected_threshold)) {
        tempest_stage_record_abort(
          paste0(
            "Verification-stage threshold trace does not match the exact ",
            "configured min_support_score."
          )
        )
      }
    }
    reference <- record@output_reference
    if (length(reference) == 0L) {
      next
    }
    ids <- unlist(reference$ids, use.names = FALSE)
    mismatched <- switch(
      reference$kind,
      workspace_claims = length(setdiff(ids, claim_ids)) > 0L,
      claim_supports = {
        verification_reference_ids <- c(
          verification_reference_ids,
          list(ids)
        )
        length(ids) == 0L || length(setdiff(ids, support_ids)) > 0L
      },
      FALSE
    )
    if (isTRUE(mismatched)) {
      tempest_stage_record_abort(
        paste0(
          "Stage-record output references do not match the durable ",
          "ResearchWorkspace."
        )
      )
    }
    if (identical(reference$kind, "workspace_claims")) {
      referenced_claims <- lapply(ids, workspace$get_proposed_claim)
      referenced_span_ids <- unname(unlist(
        lapply(referenced_claims, \(claim) claim@evidence_span_ids),
        use.names = FALSE
      ))
      referenced_spans <- lapply(
        referenced_span_ids,
        workspace$get_evidence_span
      )
      expected_digest <- tempest_stage_claims_output_digest(
        referenced_claims,
        record,
        referenced_spans
      )
      if (!identical(reference$content_digest, expected_digest)) {
        tempest_stage_record_abort(
          paste0(
            "Extraction-stage output digest does not match the exact ",
            "durable claim records."
          )
        )
      }
    }
    if (identical(reference$kind, "claim_supports")) {
      support <- if (length(ids) == 1L) {
        workspace$get_claim_support(ids[[1]])
      } else {
        NULL
      }
      claim <- if (is.null(support)) {
        NULL
      } else {
        workspace$get_proposed_claim(support@claim_id)
      }
      evidence_span <- if (is.null(support)) {
        NULL
      } else {
        workspace$get_evidence_span(support@evidence_span_id)
      }
      expected_digest <- tempest_stage_verification_output_digest(
        support,
        record,
        claim,
        evidence_span,
        workspace
      )
      if (!identical(reference$content_digest, expected_digest)) {
        tempest_stage_record_abort(
          paste0(
            "Verification-stage output digest does not match the exact ",
            "durable claim-span support and authoritative source evidence."
          )
        )
      }
    }
    if (identical(record@stage, "verify_claim_support")) {
      if (length(ids) != 1L) {
        tempest_stage_record_abort(
          paste0(
            "Each verification-stage record must reference exactly one ",
            "claim-support assessment."
          )
        )
      }
      support <- workspace$get_claim_support(ids[[1]])
      normalized_status <- tempest_apply_min_support_score(
        support@verification_status,
        support@support_score,
        min_support_score = min_support_score
      )
      if (!identical(support@verification_status, normalized_status)) {
        tempest_stage_record_abort(
          paste0(
            "Persisted claim-span status is not the exact normalized ",
            "verification result at the configured threshold."
          )
        )
      }
      expected_support <- tempest_stage_verification_support_status(
        support@verification_status,
        support@support_score,
        min_support_score
      )
      if (!identical(record@support_status, expected_support)) {
        tempest_stage_record_abort(
          paste0(
            "Verification-stage trust does not match the durable claim-span ",
            "support at the configured threshold."
          )
        )
      }
    }
    if (
      record@stage %in%
        c(
          "refined_outline",
          "section_writing",
          "lead_section"
        )
    ) {
      evidence_field <- if (isTRUE(record@fallback_taken)) {
        "verified_evidence_claim_ids"
      } else {
        "evidence_claim_ids"
      }
      evidence_ids <- unlist(
        record@trace_references[[evidence_field]] %||% list(),
        use.names = FALSE
      )
      unknown_evidence <- setdiff(evidence_ids, claim_ids)
      if (length(unknown_evidence) > 0L) {
        tempest_stage_record_abort(
          "Grounded stage trace references unknown evidence claims."
        )
      }
      evidence <- lapply(
        evidence_ids,
        workspace$get_proposed_claim
      )
      expected_support <- tempest_stage_evidence_support(
        evidence,
        list(min_support_score = min_support_score)
      )
      if (
        isTRUE(record@fallback_taken) &&
          (length(evidence) == 0L ||
            !identical(expected_support, "verified"))
      ) {
        tempest_stage_record_abort(
          "Grounded fallback trace must bind exact threshold-supported claims."
        )
      }
      persisted_support <- if (identical(record@stage, "refined_outline")) {
        "unknown"
      } else {
        expected_support
      }
      if (
        !identical(record@support_status, persisted_support) ||
          (identical(record@stage, "refined_outline") &&
            isTRUE(record@publication_allowed))
      ) {
        tempest_stage_record_abort(
          paste0(
            "Grounded stage support does not match its durable evidence ",
            "claims at the configured threshold."
          )
        )
      }
    }
  }
  if (length(verification_reference_ids) > 0L) {
    covered_ids <- unique(unlist(
      verification_reference_ids,
      use.names = FALSE
    ))
    if (!setequal(covered_ids, support_ids)) {
      tempest_stage_record_abort(
        paste0(
          "Successful verification-stage references must cover the durable ",
          "claim-support ledger."
        )
      )
    }
  }
  tempest_stage_records_verification_projection(records, workspace)
  invisible(records)
}

#' @keywords internal
tempest_stage_records_validate_workspace_coverage <- function(
  records,
  workspace,
  require_extraction = FALSE,
  require_verification = FALSE
) {
  records <- tempest_stage_records_validate(records)
  claims <- workspace$list_proposed_claims()
  claim_ids <- vapply(claims, \(claim) claim@claim_id, character(1))
  succeeded <- Filter(\(record) identical(record@status, "succeeded"), records)
  extraction <- Filter(
    \(record) identical(record@stage, "extract_claims"),
    succeeded
  )
  verification <- Filter(
    \(record) identical(record@stage, "verify_claim_support"),
    succeeded
  )
  extracted_ids <- unname(unlist(lapply(
    extraction,
    \(record) record@output_reference$ids
  )))
  if (
    isTRUE(require_extraction) &&
      (length(extraction) == 0L || !setequal(extracted_ids, claim_ids))
  ) {
    tempest_stage_record_abort(
      paste0(
        "Succeeded extraction-stage records must exactly cover the ",
        "durable workspace claim ledger."
      )
    )
  }

  support_ids <- vapply(
    workspace$list_claim_supports(),
    \(support) support@claim_support_id,
    character(1)
  )
  verified_ids <- unname(unlist(lapply(
    verification,
    \(record) record@output_reference$ids
  )))
  if (
    isTRUE(require_verification) &&
      length(claim_ids) > 0L &&
      (length(support_ids) == 0L ||
        length(verification) == 0L ||
        !setequal(verified_ids, support_ids))
  ) {
    tempest_stage_record_abort(
      paste0(
        "Succeeded verification-stage records must exactly cover the ",
        "durable claim-support ledger."
      )
    )
  }
  invisible(records)
}

#' @keywords internal
tempest_stage_records_validate_generated_experts <- function(records, experts) {
  records <- tempest_stage_records_validate(records)
  experts <- tempest_validate_experts(experts, active_only = FALSE)
  expert_ids <- vapply(experts, \(expert) expert@expert_id, character(1))
  generated <- which(startsWith(expert_ids, "expert.generated-"))
  persona_records <- tempest_storm_succeeded_stage_records(records, "personas")
  record_digests <- vapply(
    persona_records,
    \(record) record@output_reference$content_digest,
    character(1)
  )
  candidate_digests <- character()
  candidate_coverage <- list()
  covered <- integer()
  for (index in seq_along(experts)) {
    singleton <- tempest_stage_state_output_digest(
      "personas",
      experts[index]
    )
    candidate_digests <- c(candidate_digests, singleton)
    candidate_coverage <- c(candidate_coverage, list(index))
    prefix <- tempest_stage_state_output_digest(
      "personas",
      experts[seq_len(index)]
    )
    candidate_digests <- c(candidate_digests, prefix)
    candidate_coverage <- c(candidate_coverage, list(seq_len(index)))
  }
  matches <- match(record_digests, candidate_digests)
  if (anyNA(matches)) {
    tempest_stage_record_abort(
      paste0(
        "Every succeeded persona-stage record must bind an exact canonical ",
        "durable expert-profile set."
      )
    )
  }
  if (length(matches) > 0L) {
    covered <- unique(unlist(candidate_coverage[matches], use.names = FALSE))
  }
  if (length(setdiff(generated, unique(covered))) > 0L) {
    tempest_stage_record_abort(
      paste0(
        "Every automatically generated expert requires an exact succeeded ",
        "persona-stage content binding."
      )
    )
  }
  invisible(records)
}

#' @keywords internal
tempest_stage_records_validate_claim_provenance <- function(
  records,
  workspace,
  research_run_id,
  experts = list(),
  builtin_expert_ids = "moderator"
) {
  records <- tempest_stage_records_validate(records)
  if (
    !rlang::is_string(research_run_id) ||
      is.na(research_run_id) ||
      !tempest_research_workspace_reference_id_valid(research_run_id)
  ) {
    tempest_stage_record_abort(
      "Authoritative claim provenance requires a credential-free run id."
    )
  }
  experts <- tempest_validate_experts(experts, active_only = FALSE)
  expert_ids <- c(
    vapply(experts, \(expert) expert@expert_id, character(1)),
    builtin_expert_ids
  )
  succeeded <- Filter(\(record) identical(record@status, "succeeded"), records)
  extraction <- Filter(
    \(record) identical(record@stage, "extract_claims"),
    succeeded
  )
  present <- function(value) {
    is.character(value) &&
      length(value) == 1L &&
      !is.na(value) &&
      nzchar(value)
  }

  claims <- workspace$list_proposed_claims()
  for (claim in claims) {
    if (
      present(claim@session_id) &&
        !identical(claim@session_id, research_run_id)
    ) {
      tempest_stage_record_abort(
        "Claim provenance session id does not match the authoritative run."
      )
    }
    if (
      present(claim@expert_id) &&
        !claim@expert_id %in% expert_ids
    ) {
      tempest_stage_record_abort(
        "Claim provenance references an expert outside the durable roster."
      )
    }
    matching <- Filter(
      function(record) {
        claim@claim_id %in%
          unlist(
            record@output_reference$ids,
            use.names = FALSE
          )
      },
      extraction
    )
    spans <- lapply(claim@evidence_span_ids, workspace$get_evidence_span)
    program_spans <- Filter(
      \(span) startsWith(span@extracted_by, "sha256:"),
      spans
    )
    requires_extraction <- present(claim@session_id) ||
      present(claim@expert_id) ||
      present(claim@retrieval_step_id) ||
      length(program_spans) > 0L
    if (requires_extraction && length(matching) != 1L) {
      tempest_stage_record_abort(
        paste0(
          "Generated claim provenance requires one exact succeeded ",
          "extraction-stage record."
        )
      )
    }
    if (length(matching) == 1L) {
      record <- matching[[1]]
      if (
        length(program_spans) > 0L &&
          any(vapply(
            program_spans,
            \(span) {
              !identical(
                span@extracted_by,
                record@program_artifact_id
              )
            },
            logical(1)
          ))
      ) {
        tempest_stage_record_abort(
          "Evidence-span extraction provenance does not match its stage."
        )
      }
      trace_expert <- record@trace_references$expert_id %||% NULL
      if (
        present(claim@expert_id) &&
          !identical(claim@expert_id, trace_expert)
      ) {
        tempest_stage_record_abort(
          "Claim expert provenance does not match its extraction trace."
        )
      }
      correlation <- record@trace_references$correlation_id %||% NULL
      if (
        present(claim@retrieval_step_id) &&
          !identical(claim@retrieval_step_id, correlation)
      ) {
        tempest_stage_record_abort(
          "Claim retrieval provenance does not match its extraction trace."
        )
      }
    }
  }

  tempest_stage_records_verification_projection(records, workspace)
  invisible(records)
}

#' @keywords internal
tempest_stage_records_validate_persisted_trust <- function(
  records,
  workspace,
  min_support_score = 0.7
) {
  records <- tempest_stage_records_validate(records)
  min_support_score <- tempest_normalize_min_support_score(min_support_score)
  succeeded <- Filter(\(record) identical(record@status, "succeeded"), records)
  fixed_unknown <- c(
    "perspectives",
    "personas",
    "query_decomposition",
    "extract_claims",
    "next_question",
    "draft_outline",
    "refined_outline"
  )
  for (record in succeeded) {
    if (
      record@stage %in%
        fixed_unknown &&
        (!identical(record@support_status, "unknown") ||
          isTRUE(record@publication_allowed))
    ) {
      tempest_stage_record_abort(
        paste0(
          "Persisted exploratory, extraction, and planning stages must ",
          "remain unknown and non-publishable."
        )
      )
    }
  }

  verification <- Filter(
    \(record) identical(record@stage, "verify_claim_support"),
    succeeded
  )
  proof_ids <- character()
  grounded <- Filter(
    \(record) record@stage %in% c("section_writing", "lead_section"),
    succeeded
  )
  for (record in grounded) {
    verified_ids <- unlist(
      record@trace_references$verified_evidence_claim_ids %||% list(),
      use.names = FALSE
    )
    proof_ids <- c(proof_ids, verified_ids)
    if (
      identical(record@support_status, "verified") ||
        isTRUE(record@publication_allowed)
    ) {
      proof_ids <- c(
        proof_ids,
        unlist(
          record@trace_references$evidence_claim_ids %||% list(),
          use.names = FALSE
        )
      )
    }
  }
  proof_ids <- unique(proof_ids)
  for (claim_id in proof_ids) {
    supports <- Filter(
      \(support) identical(support@claim_id, claim_id),
      workspace$list_claim_supports()
    )
    if (length(supports) == 0L) {
      tempest_stage_record_abort(
        paste0(
          "Verified publication evidence requires the complete bound ",
          "claim-by-span support set."
        )
      )
    }
    for (support in supports) {
      matching <- Filter(
        \(record) {
          identical(
            unlist(record@output_reference$ids, use.names = FALSE),
            support@claim_support_id
          )
        },
        verification
      )
      expected_pair <- tempest_stage_verification_support_status(
        support@verification_status,
        support@support_score,
        min_support_score
      )
      if (length(matching) != 1L || !identical(expected_pair, "verified")) {
        tempest_stage_record_abort(
          paste0(
            "Verified publication evidence requires one exact succeeded ",
            "verification record for every threshold-supported span."
          )
        )
      }
    }
    claim <- workspace$get_proposed_claim(claim_id)
    expected <- tempest_stage_verification_support_status(
      claim@verification_status,
      claim@support_score,
      min_support_score
    )
    if (!identical(expected, "verified")) {
      tempest_stage_record_abort(
        "Publication proof is below the persisted support threshold."
      )
    }
  }
  invisible(records)
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
  if (!identical(schema_version, 5L)) {
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
  tempest_exact_integer_scalar(
    value,
    "Workspace field max_sources",
    class = tempest_persistence_error_class(
      "tempest_research_workspace_restore_error"
    ),
    minimum = 1L
  )
}

#' @keywords internal
tempest_research_workspace_restore_records <- function(
  snapshot,
  field
) {
  if (!field %in% names(snapshot)) {
    tempest_research_workspace_restore_abort(
      "Workspace snapshot is missing required field {.field {field}}."
    )
  }
  records <- snapshot[[field]]
  if (is.null(records) || !is.list(records) || is.data.frame(records)) {
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
    retrieved_resources = tempest_resource_record_fields(),
    proposed_claims = tempest_claim_record_fields(),
    evidence_spans = tempest_evidence_span_record_fields(),
    claim_supports = c(
      "claim_support_id",
      "claim_id",
      "evidence_span_id",
      "source_id",
      "verification_status",
      "support_score",
      "rationale"
    ),
    disputes = tempest_dispute_record_fields(),
    tempest_research_workspace_restore_abort(
      "Unknown workspace record field {.field {field}}."
    )
  )
}

#' @keywords internal
tempest_research_workspace_exact_records <- function(records, field) {
  fields <- tempest_research_workspace_record_fields(field)
  ordered <- is.list(records) &&
    !is.data.frame(records) &&
    is.null(names(records)) &&
    all(vapply(
      records,
      \(record) identical(names(record), fields),
      logical(1)
    ))
  if (!isTRUE(ordered)) {
    tempest_research_workspace_restore_abort(paste0(
      "Workspace ",
      gsub("_", "-", field),
      " records must use the exact current field order."
    ))
  }
  tempest_persistence_exact_records(
    records,
    fields,
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
  values <- if (
    is.list(value) && !is.data.frame(value) && is.null(names(value))
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
      tempest_exact_integer_scalar_valid(
        value,
        minimum = max(minimum, -.Machine$integer.max),
        maximum = min(maximum, .Machine$integer.max)
      ))
  if (!isTRUE(valid)) {
    tempest_research_workspace_restore_abort(paste0(
      "Workspace record field ",
      field,
      " must be ",
      if (nullable) "NULL or " else "",
      if (integer) "one exact integer." else "one finite number."
    ))
  }
  value
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
tempest_research_workspace_restore_metadata <- function(snapshot) {
  if ("artifacts" %in% names(snapshot)) {
    tempest_research_workspace_restore_abort(
      "Current workspaces cannot contain an arbitrary artifacts field."
    )
  }
  base_snapshot_id <- tryCatch(
    tempest_research_workspace_snapshot_id(snapshot$base_snapshot_id),
    error = function(error) {
      tempest_research_workspace_restore_abort(
        "{.field base_snapshot_id} is invalid.",
        parent = error
      )
    }
  )
  if (is.null(snapshot$accepted_graft_references)) {
    tempest_research_workspace_restore_abort(
      "{.field accepted_graft_references} cannot be literal null."
    )
  }
  references <- tryCatch(
    tempest_research_workspace_references(
      snapshot$accepted_graft_references
    ),
    error = function(error) {
      tempest_research_workspace_restore_abort(
        "{.field accepted_graft_references} is invalid.",
        parent = error
      )
    }
  )
  max_sources <- tempest_research_workspace_restore_max_sources(
    snapshot$max_sources
  )

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
  expected_fields <- tempest_research_workspace_snapshot_fields()
  snapshot_fields <- names(snapshot)
  if (!identical(snapshot_fields, expected_fields)) {
    tempest_unsupported_format_abort(
      "ResearchWorkspace snapshot format",
      schema_version,
      tempest_persistence_error_class(
        "tempest_research_workspace_restore_error"
      )
    )
  }
  tempest_persistence_credential_audit(
    list(workspace = snapshot),
    "ResearchWorkspace snapshot",
    tempest_persistence_error_class(
      "tempest_research_workspace_restore_error"
    )
  )
  metadata <- tempest_research_workspace_restore_metadata(snapshot)
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
    "retrieved_resources"
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
    "evidence_spans"
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
    "proposed_claims"
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
  restored_claims <- list()
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
    restored_claims[[length(restored_claims) + 1L]] <- claim
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
    "disputes"
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

  support_records <- tempest_research_workspace_restore_records(
    snapshot,
    "claim_supports"
  )
  support_records <- tempest_research_workspace_exact_records(
    support_records,
    "claim_supports"
  )
  tempest_research_workspace_unique_record_ids(
    support_records,
    "claim_support_id",
    "Claim-support records"
  )
  supports <- lapply(seq_along(support_records), function(i) {
    tryCatch(
      tempest_claim_support_from_list(support_records[[i]]),
      error = function(error) {
        tempest_research_workspace_restore_abort(
          paste0("Claim-support entry ", i, " is invalid."),
          parent = error
        )
      }
    )
  })
  if (length(supports) == 0L) {
    exact_unverified <- all(vapply(
      restored_claims,
      function(claim) {
        identical(candidate$get_proposed_claim(claim@claim_id), claim)
      },
      logical(1)
    ))
    if (!isTRUE(exact_unverified)) {
      tempest_research_workspace_restore_abort(
        paste0(
          "Verified claim summaries require the complete authoritative ",
          "claim-support set."
        )
      )
    }
  } else {
    verifier_models <- unique(vapply(
      restored_claims,
      \(claim) claim@verifier_model,
      character(1)
    ))
    verified_times <- unique(vapply(
      restored_claims,
      \(claim) claim@verified_at,
      character(1)
    ))
    if (
      length(verifier_models) != 1L ||
        !tempest_ledger_identifier_valid(
          verifier_models[[1]],
          optional = TRUE
        ) ||
        length(verified_times) != 1L ||
        !tempest_ledger_timestamp_valid(verified_times[[1]])
    ) {
      tempest_research_workspace_restore_abort(
        paste0(
          "A complete claim-support set requires one exact verifier and ",
          "verification timestamp across all claim summaries."
        )
      )
    }
    tryCatch(
      candidate$verify_proposed_claims_batch(
        supports,
        verified_at = verified_times[[1]],
        min_support_score = 0,
        verifier = verifier_models[[1]]
      ),
      error = function(error) {
        tempest_research_workspace_restore_abort(
          "{.field claim_supports} cannot be restored.",
          parent = error
        )
      }
    )
    restored_claims_value <- new.env(parent = emptyenv())
    for (claim in restored_claims) {
      derived <- candidate$get_proposed_claim(claim@claim_id)
      if (!identical(derived, claim)) {
        tempest_research_workspace_restore_abort(
          paste0(
            "Claim summary {.val {claim@claim_id}} is not exactly derived ",
            "from the authoritative claim-support set."
          )
        )
      }
      restored_claims_value[[claim@claim_id]] <- claim
    }
    candidate$.__enclos_env__$private$claims_value <- restored_claims_value
  }

  tryCatch(
    candidate$validate_integrity(),
    error = function(error) {
      tempest_research_workspace_restore_abort(
        "Restored claim-support integrity validation failed.",
        parent = error
      )
    }
  )
  tempest_research_workspace_validate_noncontent(candidate, "restore")

  if (is.null(workspace)) {
    return(candidate)
  }
  target_private <- workspace$.__enclos_env__$private
  source_private <- candidate$.__enclos_env__$private
  fields <- c(
    "resources_value",
    "claims_value",
    "evidence_spans_value",
    "claim_supports_value",
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
  unname(lapply(experts, function(expert) {
    record <- tempest_expert_profile_record(expert)
    record$focus_areas <- unname(as.list(record$focus_areas))
    if (length(record$metadata) == 0L) {
      names(record$metadata) <- character()
    }
    record
  }))
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
  valid_writer_fields <- vapply(
    records,
    function(record) {
      rlang::is_string(record$version) &&
        !is.na(record$version) &&
        rlang::is_string(record$state) &&
        !is.na(record$state) &&
        identical(record$schema_version, 1L) &&
        is.list(record$focus_areas) &&
        !is.data.frame(record$focus_areas) &&
        is.null(names(record$focus_areas)) &&
        all(vapply(
          record$focus_areas,
          \(value) rlang::is_string(value) && !is.na(value),
          logical(1)
        )) &&
        is.list(record$metadata) &&
        !is.data.frame(record$metadata)
    },
    logical(1)
  )
  if (!all(valid_writer_fields)) {
    tempest_abort(
      paste0(
        "Cannot restore {what}; expert-profile records must retain exact ",
        "non-null writer fields."
      ),
      class = class
    )
  }
  tryCatch(
    {
      experts <- lapply(records, function(record) {
        if (
          length(record$metadata) == 0L &&
            !is.null(names(record$metadata))
        ) {
          record$metadata <- list()
        }
        tempest_expert_profile_from_data(record)
      })
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
tempest_session_credential_free_value <- function(value, field, action) {
  sensitive <- tempest_contract_sensitive_values(value, field)
  if (length(sensitive) > 0L) {
    tempest_session_snapshot_value_abort(
      paste0(
        "Co-STORM ",
        field,
        " cannot contain credential or secret values."
      ),
      action
    )
  }
  invisible(value)
}

#' @keywords internal
tempest_session_transcript_record <- function(value, action = "snapshot") {
  if (is.null(value)) {
    tempest_session_snapshot_value_abort(
      "Co-STORM transcript cannot be literal null.",
      action
    )
  }
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
    if (identical(action, "restore")) {
      return(
        !is.null(value) &&
          is.list(value) &&
          !is.data.frame(value) &&
          is.null(names(value)) &&
          all(vapply(value, valid_string, logical(1)))
      )
    }
    if (is.null(value)) {
      return(TRUE)
    }
    if (
      is.character(value) &&
        !is.object(value) &&
        is.null(names(value))
    ) {
      return(!anyNA(value) && all(nzchar(value)))
    }
    is.list(value) &&
      !is.data.frame(value) &&
      is.null(names(value)) &&
      all(vapply(value, valid_string, logical(1)))
  }
  nodes_valid <- vapply(
    value$nodes,
    function(node) {
      fields <- names(node)
      source_ids_valid <- FALSE
      if (is.list(node) && !is.data.frame(node)) {
        source_ids_valid <- if (identical(action, "restore")) {
          "source_ids" %in% fields && valid_ids(node$source_ids)
        } else {
          valid_ids(node$source_ids %||% character())
        }
      }
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
        source_ids_valid
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
  value$nodes <- lapply(value$nodes, function(node) {
    node$source_ids <- tempest_codec_character(
      node$source_ids %||% character()
    )
    node
  })
  value
}

#' @keywords internal
tempest_session_mindmap_binding_abort <- function(message, action) {
  if (identical(action, "restore")) {
    tempest_session_restore_abort(message)
  }
  if (identical(action, "snapshot")) {
    tempest_session_snapshot_value_abort(message, action = "snapshot")
  }
  tempest_abort(
    message,
    class = c(
      "tempest_session_mindmap_error",
      "tempest_session_error",
      "tempest_error"
    )
  )
}

#' @keywords internal
tempest_session_mindmap_assert_binding <- function(
  mindmap,
  workspace,
  action = c("restore", "snapshot", "update")
) {
  action <- match.arg(action)
  node_ids <- vapply(mindmap$nodes, `[[`, character(1), "id")
  if (!"root" %in% node_ids) {
    tempest_session_mindmap_binding_abort(
      "The Co-STORM mind map must contain the canonical root node.",
      action
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
    tempest_session_mindmap_binding_abort(
      paste0(
        "The Co-STORM mind map contains an unknown or cyclic parent, ",
        "edge endpoint, or evidence source id."
      ),
      action
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
  for (field in c("topic", "title")) {
    tempest_session_credential_free_value(snapshot[[field]], field, action)
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
  if (identical(action, "restore")) {
    if (
      is.null(value) ||
        !is.list(value) ||
        is.data.frame(value) ||
        !is.null(names(value))
    ) {
      tempest_session_restore_abort(
        "Stored suggested questions must be one unnamed JSON array."
      )
    }
    valid <- vapply(
      value,
      \(question) {
        is.character(question) &&
          length(question) == 1L &&
          !is.na(question)
      },
      logical(1)
    )
    if (!all(valid)) {
      tempest_session_restore_abort(
        "Stored suggested questions must contain only exact strings."
      )
    }
    value <- if (length(value) == 0L) {
      character()
    } else {
      unlist(value, use.names = FALSE)
    }
  } else {
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
  tempest_session_credential_free_value(
    value,
    "suggested_questions",
    action
  )
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
    "report_reference",
    "suggested_questions",
    "progress_events",
    "stage_records",
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
  value <- enc2utf8(value)
  tempest_session_credential_free_value(value, "report_md", action)
  value
}

#' @keywords internal
tempest_persistence_execution_review_candidates <- function(...) {
  histories <- list(...)
  candidates <- unlist(
    lapply(histories, function(records) {
      records <- tempest_stage_records_validate(records)
      vapply(
        seq.int(0L, length(records)),
        function(size) {
          tempest_stage_records_execution_review(records[seq_len(size)])
        },
        character(1)
      )
    }),
    use.names = FALSE
  )
  candidates <- unique(candidates[nzchar(candidates)])
  candidates[order(nchar(candidates), decreasing = TRUE)]
}

#' @keywords internal
tempest_persistence_report_without_execution_review <- function(
  value,
  records,
  prior_records = records,
  trusted_title = NULL
) {
  if (is.null(value)) {
    return(NULL)
  }
  value <- enc2utf8(value)
  body <- tempest_markdown_without_trusted_title(value, trusted_title)
  if (!tempest_markdown_has_heading(body, "Execution review")) {
    return(value)
  }
  candidates <- tempest_persistence_execution_review_candidates(
    prior_records,
    records
  )
  matches <- candidates[vapply(
    candidates,
    \(review) endsWith(value, paste0("\n\n", review, "\n")),
    logical(1)
  )]
  if (length(matches) == 0L) {
    tempest_stage_record_abort(
      paste0(
        "A durable report can remove only an exact package-owned terminal ",
        "Execution review."
      )
    )
  }
  suffix <- paste0("\n", matches[[1]], "\n")
  prefix_length <- nchar(value) - nchar(suffix)
  if (prefix_length == 0L) {
    return("")
  }
  substr(value, 1L, prefix_length)
}

#' @keywords internal
tempest_persistence_report_for_records <- function(
  value,
  records,
  prior_records = records,
  trusted_title = NULL
) {
  if (is.null(value)) {
    return(NULL)
  }
  base <- tempest_persistence_report_without_execution_review(
    value,
    records,
    prior_records = prior_records,
    trusted_title = trusted_title
  )
  review <- tempest_stage_records_execution_review(records)
  tempest_markdown_append_execution_review(
    base,
    review,
    trusted_title = trusted_title
  )
}

#' @keywords internal
tempest_persistence_report_reference <- function(value) {
  if (is.null(value)) {
    return(NULL)
  }
  list(
    report_id = "report_md",
    sha256 = tempest_stage_content_digest_id(value)
  )
}

#' @keywords internal
tempest_persistence_validate_report_reference <- function(reference, value) {
  expected <- tempest_persistence_report_reference(value)
  if (is.null(expected)) {
    if (!is.null(reference)) {
      tempest_stage_record_abort(
        "A missing durable report cannot carry a report reference."
      )
    }
    return(invisible(NULL))
  }
  if (
    !is.list(reference) ||
      is.data.frame(reference) ||
      !identical(names(reference), names(expected)) ||
      !identical(reference, expected)
  ) {
    tempest_stage_record_abort(
      "The durable report reference does not match its exact content digest."
    )
  }
  invisible(reference)
}

#' @keywords internal
tempest_persistence_stage_manifest_traces <- function(records) {
  records <- tempest_stage_records_validate(records)
  traces <- lapply(records, function(record) {
    references <- record@trace_references
    if (!identical(references$trace_id %||% NULL, record@attempt_id)) {
      tempest_stage_record_abort(
        paste0(
          "Every durable stage attempt must use its attempt id as the exact ",
          "canonical trace id."
        )
      )
    }
    trace_fields <- c(
      "deputy_run_id",
      "deputy_session_id",
      "parent_run_id",
      "delegation_id",
      "tool_call_id",
      "trace_id",
      "expert_id",
      "correlation_id",
      "role"
    )
    trace <- references[intersect(trace_fields, names(references))]
    trace$program_artifact_id <- record@program_artifact_id
    trace$stage <- record@stage
    trace$status <- record@status
    trace$trace_type <- "stage_attempt"
    trace
  })
  tempest_research_manifest_traces(traces)
}

#' @keywords internal
tempest_persistence_manifest_bind_stage_records <- function(manifest, records) {
  traces <- tempest_persistence_stage_manifest_traces(records)
  runtime <- manifest@runtime
  runtime_fields <- c(
    deputy_run_id = "deputy_run_ids",
    deputy_session_id = "deputy_session_ids"
  )
  for (field in names(runtime_fields)) {
    values <- vapply(
      records,
      function(record) record@trace_references[[field]] %||% NA_character_,
      character(1)
    )
    values <- values[!is.na(values)]
    target <- runtime_fields[[field]]
    existing <- unlist(runtime[[target]] %||% list(), use.names = FALSE)
    combined <- sort(unique(c(existing, values)))
    if (length(combined) > 0L) {
      runtime[[target]] <- as.list(combined)
    }
  }
  tempest_research_manifest_update(
    manifest,
    runtime = runtime,
    traces = traces
  )
}

#' @keywords internal
tempest_persistence_manifest_validate_stage_records <- function(
  manifest,
  records
) {
  expected <- tempest_persistence_stage_manifest_traces(records)
  if (!identical(manifest@traces, expected)) {
    tempest_stage_record_abort(
      paste0(
        "The research manifest trace declarations do not exactly match its ",
        "durable stage attempts."
      )
    )
  }
  tempest_stage_records_validate_manifest(records, manifest)
}

#' @keywords internal
tempest_persistence_manifest_bind_report <- function(manifest, report_md) {
  reference <- tempest_persistence_report_reference(report_md)
  deliverables <- manifest@deliverables
  deliverables$report_md <- if (is.null(reference)) {
    NULL
  } else {
    c(reference, list(status = "durable"))
  }
  tempest_research_manifest_update(
    manifest,
    deliverables = deliverables
  )
}

#' @keywords internal
tempest_persistence_manifest_validate_report <- function(
  manifest,
  reference,
  report_md
) {
  tempest_persistence_validate_report_reference(reference, report_md)
  expected <- if (is.null(reference)) {
    NULL
  } else {
    c(reference, list(status = "durable"))
  }
  if (!identical(manifest@deliverables$report_md %||% NULL, expected)) {
    tempest_stage_record_abort(
      "The research manifest does not bind the exact durable report."
    )
  }
  invisible(reference)
}

#' @keywords internal
tempest_persistence_report_inline_citations <- function(value) {
  gsub(
    "\\[\\^(S[0-9a-f]{12})\\]",
    "[\\1]",
    value,
    perl = TRUE
  )
}

#' @keywords internal
tempest_persistence_validate_report_policy <- function(
  report_md,
  title,
  workspace,
  config,
  records
) {
  if (is.null(report_md)) {
    return(invisible(NULL))
  }
  tempest_final_report_validate(
    report_md = report_md,
    workspace = workspace,
    title = title,
    citation_policy = config@citation_policy,
    on_unsupported_claim = config@on_unsupported_claim,
    min_support_score = config@min_support_score,
    stage_records = records
  )
}

#' Snapshot a Co-STORM session
#'
#' `r lifecycle::badge("experimental")`
#'
#' `tempest_session_snapshot()` returns a structured, in-memory representation
#' of the durable state in a [TempestSession]. It includes the research
#' manifest; fixed session and configuration identity; the authoritative
#' [ResearchWorkspace]; expert profiles; transcript and mind map; the latest
#' report Markdown; stage-record, progress-event, and expert-session metadata;
#' and the optional immutable Graft snapshot. Live chat
#' handles, runtime clients, tools, closures, generic workflows, generic
#' artifact catalogs, Shiny reactive state, credentials, and provider request
#' bodies are not included.
#'
#' Use [tempest_session_restore()] to rebuild a session from the returned list,
#' or [tempest_session_save()] to write the same durable state to a directory
#' bundle. A stage attempt that is still running is projected to a cancelled
#' durable record without changing the live session.
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
  durable_records <- tryCatch(
    {
      live_records <- tempest_session_stage_records(session)
      durable_records <- tempest_stage_records_interrupt(
        live_records,
        completed_at = tempest_now_utc()
      )
      research_manifest <- tempest_persistence_manifest_bind_stage_records(
        research_manifest,
        durable_records
      )
      tempest_persistence_manifest_validate_stage_records(
        research_manifest,
        durable_records
      )
      tempest_stage_records_validate_workspace(
        durable_records,
        session$workspace,
        min_support_score = session$config@min_support_score
      )
      tempest_stage_records_validate_persisted_trust(
        durable_records,
        session$workspace,
        min_support_score = session$config@min_support_score
      )
      tempest_stage_records_validate_workspace_coverage(
        durable_records,
        session$workspace,
        require_verification = session$config@citation_policy %in%
          c("claim_verified", "strict")
      )
      tempest_stage_records_validate_product_outputs(
        durable_records,
        list(experts = session$experts)
      )
      tempest_stage_records_validate_generated_experts(
        durable_records,
        session$experts
      )
      tempest_stage_records_validate_claim_provenance(
        durable_records,
        session$workspace,
        research_manifest@research_run_id,
        session$experts
      )
      durable_records
    },
    error = function(error) {
      tempest_abort(
        "Cannot snapshot inconsistent Co-STORM stage records.",
        class = tempest_session_persistence_error_class(
          "tempest_session_snapshot_error"
        ),
        parent = error
      )
    }
  )
  report_md <- tryCatch(
    {
      value <- tempest_session_report_record(
        tempest_session_report_value(session)
      )
      value <- tempest_persistence_report_for_records(
        value,
        durable_records,
        prior_records = live_records,
        trusted_title = session$title
      )
      tempest_persistence_validate_report_policy(
        value,
        session$title,
        session$workspace,
        session$config,
        durable_records
      )
      value
    },
    error = function(error) {
      tempest_abort(
        "Cannot snapshot an invalid Co-STORM report product.",
        class = tempest_session_persistence_error_class(
          "tempest_session_snapshot_error"
        ),
        parent = error
      )
    }
  )
  report_reference <- tempest_persistence_report_reference(report_md)
  research_manifest <- tryCatch(
    tempest_persistence_manifest_bind_report(research_manifest, report_md),
    error = function(error) {
      tempest_abort(
        "Cannot bind the Co-STORM report to its research manifest.",
        class = tempest_session_persistence_error_class(
          "tempest_session_snapshot_error"
        ),
        parent = error
      )
    }
  )
  workspace <- tryCatch(
    tempest_research_workspace_snapshot(session$workspace),
    error = function(error) {
      tempest_abort(
        "Cannot snapshot an invalid Co-STORM research workspace.",
        class = tempest_session_persistence_error_class(
          "tempest_session_snapshot_error"
        ),
        parent = error
      )
    }
  )
  mindmap <- tempest_session_mindmap_record(
    session$mindmap,
    action = "snapshot"
  )
  tempest_session_mindmap_assert_binding(
    mindmap,
    session$workspace,
    action = "snapshot"
  )
  mindmap$nodes <- lapply(mindmap$nodes, function(node) {
    node$source_ids <- as.list(unname(node$source_ids))
    node
  })
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
    schema_version = 8L,
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
    mindmap = mindmap,
    report_md = report_md,
    report_reference = report_reference,
    suggested_questions = as.list(suggested_questions),
    progress_events = tempest_session_restore_progress_events(
      tempest_execution_events(session),
      session_id = session$session_id,
      action = "snapshot"
    ),
    stage_records = tempest_stage_records_data(durable_records),
    workspace = workspace,
    expert_sessions = tempest_expert_sessions_snapshot(session),
    graft_snapshot = graft_snapshot
  )
  tempest_persistence_credential_audit(
    snapshot,
    "Co-STORM session snapshot",
    tempest_session_persistence_error_class(
      "tempest_session_snapshot_error"
    )
  )
  tempest_session_portable_snapshot(snapshot)
  snapshot
}

#' @keywords internal
tempest_session_restore_expert_sessions <- function(session, expert_sessions) {
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
    if (
      is.null(expert_session$grants) ||
        !is.list(expert_session$grants) ||
        is.data.frame(expert_session$grants)
    ) {
      tempest_session_restore_abort(
        "Expert-session grants must be a non-null list."
      )
    }
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
#' Stage-record history is restored for audit, but running attempts are rejected
#' rather than resumed.
#'
#' @param snapshot A list from [tempest_session_snapshot()].
#' @param config Runtime [TempestConfig] used to recreate chats, retrievers, and
#'   tools.
#' @param progress Optional callback for future `tempest_progress_event`
#'   objects.
#' @param program_set A [TempestProgramSet] carrying the same program
#'   identities recorded in the snapshot. If `NULL`, the builtin set is used.
#' @param knowledge_view Optional transient immutable Graft view required by
#'   future execution when `program_set` contains governed procedures. It is
#'   never reconstructed from or written to persistence.
#' @return A restored [TempestSession].
#' @export
tempest_session_restore <- function(
  snapshot,
  config = tempest_config(),
  progress = NULL,
  program_set = NULL,
  knowledge_view = NULL
) {
  tempest_session_restore_internal(
    snapshot = snapshot,
    config = config,
    runtime = tempest_runtime(),
    connection_permissions = NULL,
    progress = progress,
    program_set = program_set,
    knowledge_view = knowledge_view
  )
}

#' @keywords internal
tempest_session_restore_internal <- function(
  snapshot,
  config = tempest_config(),
  runtime = tempest_runtime(),
  connection_permissions = NULL,
  progress = NULL,
  program_set = NULL,
  knowledge_view = NULL
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
  if (!identical(schema_version, 8L)) {
    tempest_unsupported_format_abort(
      "TempestSession snapshot format",
      schema_version,
      tempest_session_persistence_error_class(
        "tempest_session_restore_error"
      )
    )
  }
  snapshot_fields <- names(snapshot)
  if (!identical(snapshot_fields, tempest_session_snapshot_fields())) {
    tempest_unsupported_format_abort(
      "TempestSession snapshot format",
      schema_version,
      tempest_session_persistence_error_class(
        "tempest_session_restore_error"
      )
    )
  }
  tempest_persistence_credential_audit(
    snapshot,
    "Co-STORM session snapshot",
    tempest_session_persistence_error_class(
      "tempest_session_restore_error"
    )
  )
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
    "Schema 8 session workspace",
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
  stage_records <- tryCatch(
    {
      records <- tempest_stage_records_from_data(
        snapshot$stage_records,
        allow_running = FALSE
      )
      tempest_persistence_manifest_validate_stage_records(
        research_manifest,
        records
      )
      records
    },
    error = function(error) {
      tempest_session_restore_abort(
        "Snapshot contains invalid or mismatched stage-record history.",
        parent = error
      )
    }
  )
  tryCatch(
    tempest_stage_records_validate_execution_review(
      snapshot$report_md,
      stage_records,
      trusted_title = snapshot$title
    ),
    error = function(error) {
      tempest_session_restore_abort(
        "Snapshot report is missing its exact execution disclosure.",
        parent = error
      )
    }
  )
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
    tempest_stage_records_validate_workspace(
      stage_records,
      workspace,
      min_support_score = config@min_support_score
    ),
    error = function(error) {
      tempest_session_restore_abort(
        "Snapshot stage-record history does not match its ResearchWorkspace.",
        parent = error
      )
    }
  )
  tryCatch(
    {
      tempest_stage_records_validate_persisted_trust(
        stage_records,
        workspace,
        min_support_score = config@min_support_score
      )
      tempest_stage_records_validate_workspace_coverage(
        stage_records,
        workspace,
        require_verification = config@citation_policy %in%
          c("claim_verified", "strict")
      )
      tempest_stage_records_validate_generated_experts(
        stage_records,
        tempest_experts_from_records(
          snapshot$experts %||% list(),
          what = "session expert profiles",
          class = tempest_session_persistence_error_class(
            "tempest_session_restore_error"
          )
        )
      )
      tempest_stage_records_validate_claim_provenance(
        stage_records,
        workspace,
        research_manifest@research_run_id,
        tempest_experts_from_records(
          snapshot$experts %||% list(),
          what = "session expert profiles",
          class = tempest_session_persistence_error_class(
            "tempest_session_restore_error"
          )
        )
      )
      tempest_persistence_manifest_validate_report(
        research_manifest,
        snapshot$report_reference,
        snapshot$report_md
      )
      tempest_persistence_validate_report_policy(
        snapshot$report_md,
        snapshot$title,
        workspace,
        config,
        stage_records
      )
    },
    error = function(error) {
      tempest_session_restore_abort(
        "Snapshot product history or report binding is invalid.",
        parent = error
      )
    }
  )
  tryCatch(
    tempest_stage_records_validate_product_outputs(
      stage_records,
      list(experts = snapshot$experts)
    ),
    error = function(error) {
      tempest_session_restore_abort(
        "Snapshot stage-record history does not match Co-STORM product state.",
        parent = error
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
    knowledge_view = knowledge_view,
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
    snapshot$expert_sessions
  )
  tempest_session_set_stage_records(session, stage_records)
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

  expanded_path <- path.expand(path)
  if (tempest_persistence_leaf_path_is_symlink(expanded_path)) {
    tempest_abort(
      "{.arg path} cannot be a symbolic link.",
      class = tempest_session_persistence_error_class(
        "tempest_session_save_error"
      )
    )
  }
  bundle_dir <- normalizePath(
    expanded_path,
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
      tryCatch(
        {
          tempest_persistence_require_regular_bundle_files(
            bundle_dir,
            "session.json",
            "Existing Co-STORM root manifest",
            tempest_session_persistence_error_class(
              "tempest_session_save_error"
            )
          )
          existing_manifest <- tempest_read_json_strict(
            file.path(bundle_dir, "session.json"),
            what = "existing Co-STORM session manifest",
            class = tempest_session_persistence_error_class(
              "tempest_session_save_error"
            )
          )
          tempest_session_bundle_validate_manifest(
            bundle_dir,
            existing_manifest
          )
        },
        error = function(error) {
          tempest_abort(
            "Refusing to overwrite an invalid existing Co-STORM bundle.",
            class = tempest_session_persistence_error_class(
              "tempest_session_save_error"
            ),
            parent = error
          )
        }
      )
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
tempest_atomic_commit_bundle <- function(
  staging_dir,
  bundle_dir,
  class,
  what = "bundle"
) {
  backup_dir <- NULL
  if (file.exists(bundle_dir)) {
    backup_dir <- tempfile(
      pattern = paste0(".", basename(bundle_dir), "-backup-"),
      tmpdir = dirname(bundle_dir)
    )
    if (!file.rename(bundle_dir, backup_dir)) {
      tempest_abort(
        "Could not stage the previous {what} for replacement.",
        class = class
      )
    }
  }

  if (!file.rename(staging_dir, bundle_dir)) {
    restored <- is.null(backup_dir) || file.rename(backup_dir, bundle_dir)
    if (!isTRUE(restored)) {
      tempest_abort(
        paste0(
          "Could not install the completed {what}, and rollback of the ",
          "previous bundle also failed."
        ),
        class = class
      )
    }
    tempest_abort(
      "Could not atomically install the completed {what}.",
      class = class
    )
  }
  if (!is.null(backup_dir)) {
    unlink(backup_dir, recursive = TRUE, force = TRUE)
  }
  normalizePath(bundle_dir, winslash = "/", mustWork = TRUE)
}

#' @keywords internal
tempest_session_commit_bundle <- function(staging_dir, bundle_dir) {
  tempest_atomic_commit_bundle(
    staging_dir,
    bundle_dir,
    class = tempest_session_persistence_error_class(
      "tempest_session_save_error"
    ),
    what = "session bundle"
  )
}

#' @keywords internal
tempest_run_bundle_write_json <- function(path, rel_path, value) {
  tryCatch(
    tempest_write_json(file.path(path, rel_path), value),
    error = function(error) {
      tempest_abort(
        "Could not write STORM bundle file {.path {rel_path}}.",
        class = tempest_persistence_error_class(
          "tempest_run_persistence_error"
        ),
        parent = error
      )
    }
  )
  hook <- getOption("tempest.run_write_hook")
  if (is.function(hook)) {
    tryCatch(
      hook(rel_path),
      error = function(error) {
        tempest_abort(
          "STORM bundle write was interrupted after {.path {rel_path}}.",
          class = tempest_persistence_error_class(
            "tempest_run_persistence_error"
          ),
          parent = error
        )
      }
    )
  }
  rel_path
}

#' @keywords internal
tempest_run_bundle_write_text <- function(path, rel_path, value) {
  if (!rlang::is_string(value)) {
    return(character())
  }
  tryCatch(
    tempest_write_text(file.path(path, rel_path), value),
    error = function(error) {
      tempest_abort(
        "Could not write STORM bundle file {.path {rel_path}}.",
        class = tempest_persistence_error_class(
          "tempest_run_persistence_error"
        ),
        parent = error
      )
    }
  )
  hook <- getOption("tempest.run_write_hook")
  if (is.function(hook)) {
    tryCatch(
      hook(rel_path),
      error = function(error) {
        tempest_abort(
          "STORM bundle write was interrupted after {.path {rel_path}}.",
          class = tempest_persistence_error_class(
            "tempest_run_persistence_error"
          ),
          parent = error
        )
      }
    )
  }
  rel_path
}

#' Save a Co-STORM session bundle
#'
#' `r lifecycle::badge("experimental")`
#'
#' `tempest_session_save()` writes a schema-versioned directory bundle for a
#' [TempestSession]. The bundle stores the research manifest, authoritative
#' workspace, explicit stage-record history, optional immutable Graft snapshot,
#' and narrow report product. Every declared file is checksummed, and the
#' `session.json` manifest is written last. Generic workflow and
#' artifact-catalog state, live chat handles, registered tool closures, Shiny
#' reactive state, credentials, and raw provider request bodies are not
#' serialized. A stage attempt that is still running is written as cancelled
#' without changing the live session.
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
      "stage_records.json",
      snapshot$stage_records
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
      "workspace/claim_supports.json",
      snapshot$workspace$claim_supports
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
    report_reference = snapshot$report_reference,
    workspace = snapshot$workspace[
      tempest_session_bundle_workspace_fields()
    ],
    saved_at = tempest_now_utc(),
    files = as.list(unname(files)),
    checksums = checksums
  )
  tempest_session_bundle_write_json(staging_dir, "session.json", manifest)
  tempest_persistence_require_regular_bundle_files(
    staging_dir,
    "session.json",
    "Co-STORM staged root manifest",
    tempest_session_persistence_error_class("tempest_session_save_error")
  )
  tryCatch(
    tempest_session_bundle_validate_manifest(staging_dir, manifest),
    error = function(error) {
      tempest_abort(
        "The completed staged Co-STORM bundle failed validation.",
        class = tempest_session_persistence_error_class(
          "tempest_session_save_error"
        ),
        parent = error
      )
    }
  )

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
tempest_session_progress_event_fields <- function() {
  c(
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
  event_fields <- tempest_session_progress_event_fields()
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
        !identical(names(event), event_fields)
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
    if (!tempest_exact_integer_scalar_valid(sequence, minimum = 1L)) {
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
    record$sequence <- sequence
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
    "report_reference",
    "workspace",
    "saved_at",
    "files",
    "checksums"
  )
}

#' @keywords internal
tempest_session_bundle_workspace_fields <- function() {
  c(
    "schema_version",
    "base_snapshot_id",
    "max_sources",
    "accepted_graft_references"
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
  if (!identical(schema_version, 8L)) {
    tempest_unsupported_format_abort(
      "Co-STORM bundle format",
      schema_version,
      tempest_session_persistence_error_class(
        "tempest_session_restore_error"
      )
    )
  }
  manifest_fields <- names(manifest)
  if (!identical(manifest_fields, tempest_session_bundle_manifest_fields())) {
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
      "Schema 8 Co-STORM bundle envelope is not complete."
    )
  }
  if (
    !is.list(manifest$research_manifest) ||
      !is.list(manifest$workspace)
  ) {
    tempest_session_restore_abort(
      "Schema 8 Co-STORM bundle is missing research identity metadata."
    )
  }
  workspace_fields <- tempest_session_bundle_workspace_fields()
  if (!identical(names(manifest$workspace), workspace_fields)) {
    tempest_session_restore_abort(
      "Schema 8 Co-STORM bundle has invalid workspace identity metadata."
    )
  }
  tempest_research_workspace_require_current_schema(
    manifest$workspace,
    "Schema 8 Co-STORM workspace identity",
    tempest_session_persistence_error_class(
      "tempest_session_restore_error"
    )
  )
  files <- tempest_persistence_manifest_files(
    manifest$files,
    "Schema 8 Co-STORM file inventory",
    tempest_session_persistence_error_class(
      "tempest_session_restore_error"
    )
  )
  tempest_persistence_require_regular_bundle_files(
    bundle_dir,
    files,
    "Co-STORM bundle",
    tempest_session_persistence_error_class(
      "tempest_session_restore_error"
    )
  )
  evidence_required <- c(
    "workspace/retrieved_resources.json",
    "workspace/proposed_claims.json",
    "workspace/evidence_spans.json",
    "workspace/disputes.json",
    "workspace/claim_supports.json"
  )
  core_required <- c(
    "experts.json",
    "expert_sessions.json",
    "transcript.json",
    "mindmap.json",
    "progress_events.json",
    "stage_records.json",
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
    "Schema 8 Co-STORM checksum inventory",
    tempest_session_persistence_error_class(
      "tempest_session_restore_error"
    )
  )
  missing_checksums <- setdiff(files, names(checksums))
  extra_checksums <- setdiff(names(checksums), files)
  available <- setdiff(files[!unsafe], missing)
  stage_record_path <- file.path(bundle_dir, "stage_records.json")
  non_regular_stage_records <- if (
    file.exists(stage_record_path) &&
      (!utils::file_test("-f", stage_record_path) ||
        nzchar(Sys.readlink(stage_record_path)))
  ) {
    "stage_records.json"
  } else {
    character()
  }
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
  checksum_candidates <- setdiff(
    available,
    c(escaping, non_regular_stage_records)
  )
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
    if (length(non_regular_stage_records) > 0L) {
      "The stage-record sidecar must be a regular non-symlink file."
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
#' Stage-record history is restored for audit, but running attempts are rejected
#' rather than resumed.
#'
#' @param path Directory containing a session bundle.
#' @param config Runtime [TempestConfig] used to recreate chats, retrievers, and
#'   tools.
#' @param progress Optional callback for future `tempest_progress_event`
#'   objects.
#' @param partial_recovery Whether to allow explicitly requested recovery when
#'   allowlisted presentation files are missing or fail integrity checks. All
#'   other declared files, including stage-record, expert, workspace, report,
#'   and Graft snapshot state, must pass integrity checks.
#' @param program_set A [TempestProgramSet] carrying the same program
#'   identities recorded in the bundle. If `NULL`, the builtin set is used.
#' @param knowledge_view Optional transient immutable Graft view required by
#'   future execution when `program_set` contains governed procedures. It is
#'   never reconstructed from or written to persistence.
#' @return A restored [TempestSession].
#' @export
tempest_session_resume <- function(
  path,
  config = tempest_config(),
  progress = NULL,
  partial_recovery = FALSE,
  program_set = NULL,
  knowledge_view = NULL
) {
  tempest_session_resume_internal(
    path = path,
    config = config,
    runtime = tempest_runtime(),
    connection_permissions = NULL,
    progress = progress,
    partial_recovery = partial_recovery,
    program_set = program_set,
    knowledge_view = knowledge_view
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
  program_set = NULL,
  knowledge_view = NULL
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
  tempest_persistence_require_regular_bundle_files(
    bundle_dir,
    "session.json",
    "Co-STORM root manifest",
    tempest_session_persistence_error_class(
      "tempest_session_restore_error"
    )
  )

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
  if (!identical(schema_version, 8L)) {
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
    report_reference = manifest$report_reference,
    suggested_questions = optional_json(
      "artifacts/suggested_questions.json",
      default = list(),
      what = "suggested questions artifact"
    ),
    progress_events = strict_json(
      "progress_events.json",
      what = "progress-event history"
    ),
    stage_records = strict_json(
      "stage_records.json",
      what = "stage-record history"
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
  workspace["claim_supports"] <- list(
    strict_json(
      "workspace/claim_supports.json",
      what = "session claim-support ledger"
    )
  )
  workspace <- workspace[tempest_research_workspace_snapshot_fields()]
  snapshot$workspace <- workspace
  snapshot <- snapshot[tempest_session_snapshot_fields()]

  tempest_session_restore_internal(
    snapshot,
    config = config,
    runtime = runtime,
    connection_permissions = connection_permissions,
    progress = progress,
    program_set = program_set,
    knowledge_view = knowledge_view
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
tempest_storm_perspective_fields <- function() {
  c("name", "description", "key_questions")
}

#' @keywords internal
tempest_storm_outline_fields <- function() {
  c("title", "sections")
}

#' @keywords internal
tempest_storm_outline_section_fields <- function() {
  c("title", "summary", "subsections")
}

#' @keywords internal
tempest_storm_outline_subsection_fields <- function() {
  c("title", "bullets", "needed")
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
        !identical(fields, tempest_storm_perspective_fields()) ||
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
      !identical(fields, tempest_storm_outline_fields()) ||
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
        !identical(
          section_fields,
          tempest_storm_outline_section_fields()
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
          !identical(
            subsection_fields,
            tempest_storm_outline_subsection_fields()
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
  cited_ids <- unique(tempest_extract_citation_ids(
    tempest_persistence_report_inline_citations(cited_md)
  ))
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
    "requested_steps",
    "completed_stages",
    "schema_version",
    "bundle_type",
    "bundle_status",
    "research_manifest",
    "report_reference",
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
    "stage_records.json",
    tempest_graft_snapshot_relative_path()
  )
  if (isTRUE(include_manifest)) c("run_config.json", files) else files
}

#' @keywords internal
tempest_storm_require_current_schema <- function(metadata) {
  schema_version <- tempest_persistence_schema_version(
    metadata$schema_version %||% NA_integer_,
    "STORM run schema version",
    tempest_persistence_error_class("tempest_run_restore_error")
  )
  if (!identical(schema_version, 7L)) {
    tempest_unsupported_format_abort(
      "STORM bundle format",
      schema_version,
      tempest_persistence_error_class("tempest_run_restore_error")
    )
  }
  schema_version
}

#' @keywords internal
tempest_run_bundle_validate_manifest <- function(run_dir, manifest) {
  schema_version <- tempest_storm_require_current_schema(manifest)
  manifest_fields <- names(manifest)
  if (!identical(manifest_fields, tempest_run_bundle_manifest_fields())) {
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
  requested_steps <- tryCatch(
    tempest_storm_requested_steps(
      manifest$requested_steps,
      from_record = TRUE
    ),
    error = function(error) {
      tempest_abort(
        "STORM run manifest has invalid requested-step metadata.",
        class = tempest_persistence_error_class(
          "tempest_run_restore_error"
        ),
        parent = error
      )
    }
  )
  if (length(setdiff(completed_stages, requested_steps)) > 0L) {
    tempest_abort(
      "STORM completed stages are outside the immutable requested steps.",
      class = tempest_persistence_error_class(
        "tempest_run_restore_error"
      )
    )
  }
  files <- tempest_persistence_manifest_files(
    manifest$files,
    "Schema 7 STORM file inventory",
    tempest_persistence_error_class("tempest_run_restore_error")
  )
  tempest_persistence_require_regular_bundle_files(
    run_dir,
    files,
    "STORM bundle",
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
    "stage_records.json",
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
    "Schema 7 STORM checksum inventory",
    tempest_persistence_error_class("tempest_run_restore_error")
  )
  missing_checksums <- setdiff(files, names(checksums))
  extra_checksums <- setdiff(names(checksums), files)
  available <- setdiff(files, missing)
  stage_record_path <- file.path(run_dir, "stage_records.json")
  non_regular_stage_records <- if (
    file.exists(stage_record_path) &&
      (!utils::file_test("-f", stage_record_path) ||
        nzchar(Sys.readlink(stage_record_path)))
  ) {
    "stage_records.json"
  } else {
    character()
  }
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
  checksum_candidates <- setdiff(
    available,
    c(escaping, non_regular_stage_records)
  )
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
    if (length(non_regular_stage_records) > 0L) {
      "The stage-record sidecar must be a regular non-symlink file."
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
  tempest_storm_require_current_schema(metadata)
  identity <- metadata$workspace
  if (!is.list(identity) || is.data.frame(identity)) {
    tempest_storm_run_restore_abort(
      "Schema 7 STORM bundles must contain a workspace identity record."
    )
  }
  required <- c(
    "base_snapshot_id",
    "max_sources",
    "accepted_graft_references"
  )
  identity_fields <- names(identity)
  if (!identical(identity_fields, required)) {
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
  if (is.null(identity$accepted_graft_references)) {
    tempest_storm_run_restore_abort(
      "Persisted accepted graft references cannot be literal null."
    )
  }
  accepted_references <- tryCatch(
    tempest_research_workspace_references(
      identity$accepted_graft_references
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
    "claim_supports",
    "disputes"
  )
  all(vapply(snapshot[evidence_fields], length, integer(1)) == 0L)
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
  tempest_storm_require_current_schema(metadata)
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
  tryCatch(
    tempest_persistence_manifest_validate_stage_records(
      manifest,
      state$stage_records
    ),
    error = function(error) {
      tempest_storm_run_restore_abort(
        "The persisted STORM stage records do not match the research manifest.",
        parent = error
      )
    }
  )
  tryCatch(
    {
      tempest_persistence_manifest_validate_report(
        manifest,
        metadata$report_reference,
        state$report_md
      )
      tempest_persistence_validate_report_policy(
        state$report_md,
        state$title,
        workspace,
        config,
        state$stage_records
      )
    },
    error = function(error) {
      tempest_storm_run_restore_abort(
        "The persisted STORM report binding or citation policy is invalid.",
        parent = error
      )
    }
  )
  manifest
}

#' @keywords internal
tempest_storm_read_state <- function(
  run_dir,
  paths,
  metadata,
  path_is_declared,
  workspace,
  config
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
  expert_records <- list()
  if (path_is_declared(paths$experts) && file.exists(paths$experts)) {
    expert_records <- tempest_read_json_strict(
      paths$experts,
      what = "STORM expert profiles",
      class = tempest_persistence_error_class("tempest_run_restore_error")
    )
  }
  tempest_storm_require_current_schema(metadata)
  completed_stages <- tempest_storm_state_completed_stages(
    metadata$completed_stages,
    from_record = TRUE
  )
  topic <- metadata$topic
  title <- metadata$title
  raw_state <- list(
    topic = topic,
    title = title,
    requested_steps = metadata$requested_steps,
    perspectives = read_json_artifact("perspectives", list()),
    experts = expert_records,
    draft_outline = read_json_artifact("draft_outline"),
    outline = read_json_artifact("outline"),
    lead_section = read_text_artifact("lead_section"),
    draft_md = read_text_artifact("draft_md"),
    report_md = read_text_artifact("report_md"),
    references = read_json_artifact("references", list()),
    stage_records = read_json_artifact("stage_records", list()),
    completed_stages = metadata$completed_stages
  )
  tempest_persistence_credential_audit(
    list(state = raw_state),
    "STORM product state",
    tempest_persistence_error_class("tempest_run_restore_error")
  )
  experts <- tempest_experts_from_records(
    expert_records,
    what = "STORM expert profiles",
    class = tempest_persistence_error_class("tempest_run_restore_error")
  )
  stage_records <- tryCatch(
    tempest_stage_records_from_data(
      raw_state$stage_records,
      allow_running = FALSE
    ),
    error = function(error) {
      tempest_storm_run_restore_abort(
        "The persisted STORM stage-record history is invalid.",
        parent = error
      )
    }
  )
  state <- tryCatch(
    tempest_storm_state(
      topic = topic,
      title = title,
      requested_steps = tempest_storm_requested_steps(
        raw_state$requested_steps,
        from_record = TRUE
      ),
      perspectives = raw_state$perspectives,
      experts = experts,
      draft_outline = raw_state$draft_outline,
      outline = raw_state$outline,
      lead_section = raw_state$lead_section,
      draft_md = raw_state$draft_md,
      report_md = raw_state$report_md,
      references = raw_state$references,
      stage_records = stage_records,
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
  tryCatch(
    {
      tempest_stage_records_validate_storm_coverage(
        state$stage_records,
        state
      )
      tempest_stage_records_validate_workspace_coverage(
        state$stage_records,
        workspace,
        require_extraction = "research" %in% state$completed_stages,
        require_verification = "polish" %in%
          state$completed_stages &&
          config@citation_policy %in% c("claim_verified", "strict")
      )
      tempest_stage_records_validate_claim_provenance(
        state$stage_records,
        workspace,
        metadata$research_manifest$research_run_id,
        state$experts
      )
    },
    error = function(error) {
      tempest_storm_run_restore_abort(
        "Persisted STORM product stages lack exact terminal record coverage.",
        parent = error
      )
    }
  )
  tryCatch(
    tempest_stage_records_validate_workspace(
      state$stage_records,
      workspace,
      min_support_score = config@min_support_score
    ),
    error = function(error) {
      tempest_storm_run_restore_abort(
        "The persisted STORM stage records do not match the workspace.",
        parent = error
      )
    }
  )
  tryCatch(
    tempest_stage_records_validate_persisted_trust(
      state$stage_records,
      workspace,
      min_support_score = config@min_support_score
    ),
    error = function(error) {
      tempest_storm_run_restore_abort(
        "The persisted STORM stage trust proof is invalid.",
        parent = error
      )
    }
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
  tempest_persistence_require_regular_bundle_files(
    run_dir,
    "run_config.json",
    "STORM root manifest",
    tempest_persistence_error_class("tempest_run_restore_error")
  )
  metadata <- tempest_read_json_strict(
    paths$run_config,
    what = "STORM run manifest",
    class = tempest_persistence_error_class(
      "tempest_run_restore_error"
    )
  )
  tempest_persistence_credential_audit(
    metadata,
    "STORM run manifest",
    tempest_persistence_error_class("tempest_run_restore_error")
  )
  declared_files <- tempest_run_bundle_validate_manifest(run_dir, metadata)
  graft_snapshot <- tempest_graft_snapshot_read(
    run_dir,
    declared_files = declared_files,
    manifest_reference = metadata$research_manifest$knowledge_snapshot %||%
      list(),
    class = tempest_persistence_error_class("tempest_run_restore_error")
  )
  tempest_storm_require_current_schema(metadata)
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
    "Schema 7 STORM research workspace",
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
    workspace,
    config
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
  if (!rlang::is_string(run_dir) || !nzchar(tempest_trim(run_dir))) {
    tempest_abort(
      "{.arg run_dir} must be one non-empty directory path.",
      class = tempest_persistence_error_class(
        "tempest_run_persistence_error"
      )
    )
  }
  expanded_run_dir <- path.expand(run_dir)
  if (tempest_persistence_leaf_path_is_symlink(expanded_run_dir)) {
    tempest_abort(
      "{.arg run_dir} cannot be a symbolic link.",
      class = tempest_persistence_error_class(
        "tempest_run_persistence_error"
      )
    )
  }
  run_dir <- normalizePath(
    expanded_run_dir,
    winslash = "/",
    mustWork = FALSE
  )
  if (file.exists(run_dir) && !dir.exists(run_dir)) {
    tempest_abort(
      "{.arg run_dir} must point to a directory.",
      class = tempest_persistence_error_class(
        "tempest_run_persistence_error"
      )
    )
  }
  dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)
  if (!inherits(workspace, "ResearchWorkspace")) {
    tempest_abort(
      "{.arg workspace} must be a ResearchWorkspace.",
      class = tempest_persistence_error_class(
        "tempest_run_persistence_error"
      )
    )
  }
  if (!S7::S7_inherits(config, TempestConfig)) {
    tempest_abort(
      "{.arg config} must be created by {.fn tempest_config}.",
      class = tempest_persistence_error_class(
        "tempest_run_persistence_error"
      )
    )
  }
  requested_steps <- tempest_storm_requested_steps(steps)
  state <- tempest_storm_state_validate(state)
  live_records <- state$stage_records
  durable_records <- tryCatch(
    tempest_stage_records_interrupt(
      live_records,
      completed_at = tempest_now_utc()
    ),
    error = function(error) {
      tempest_abort(
        "Could not project terminal STORM stage-record history.",
        class = tempest_persistence_error_class(
          "tempest_run_persistence_error"
        ),
        parent = error
      )
    }
  )
  state <- rlang::duplicate(state, shallow = TRUE)
  state$requested_steps <- requested_steps
  state$stage_records <- durable_records
  state["report_md"] <- list(tryCatch(
    tempest_persistence_report_for_records(
      state$report_md,
      durable_records,
      prior_records = live_records,
      trusted_title = state$title
    ),
    error = function(error) {
      tempest_abort(
        "Could not canonicalize the durable STORM execution review.",
        class = tempest_persistence_error_class(
          "tempest_run_persistence_error"
        ),
        parent = error
      )
    }
  ))
  cited_md <- tempest_persistence_report_inline_citations(
    state$report_md %||% state$draft_md %||% ""
  )
  cited_ids <- tempest_extract_citation_ids(cited_md)
  state$references <- Filter(
    Negate(is.null),
    lapply(cited_ids, \(id) workspace$get_retrieved_source(id))
  )
  state <- tempest_storm_state_validate(state)
  if (!S7::S7_inherits(research_manifest, TempestResearchManifest)) {
    tempest_abort(
      "{.arg research_manifest} must be a TempestResearchManifest.",
      class = tempest_persistence_error_class(
        "tempest_run_persistence_error"
      )
    )
  }
  if (!identical(research_manifest@mode, "storm")) {
    tempest_abort(
      "{.arg research_manifest} must describe a STORM run.",
      class = tempest_persistence_error_class(
        "tempest_run_persistence_error"
      )
    )
  }
  research_manifest <- tryCatch(
    tempest_persistence_manifest_bind_stage_records(
      research_manifest,
      durable_records
    ),
    error = function(error) {
      tempest_abort(
        "Could not bind durable STORM stage traces into the manifest.",
        class = tempest_persistence_error_class(
          "tempest_run_persistence_error"
        ),
        parent = error
      )
    }
  )
  tempest_storm_program_set_validate(
    program_set,
    research_manifest,
    action = "save"
  )
  tryCatch(
    tempest_persistence_manifest_validate_stage_records(
      research_manifest,
      state$stage_records
    ),
    error = function(error) {
      tempest_abort(
        "{.arg state} stage records do not match the research manifest.",
        class = tempest_persistence_error_class(
          "tempest_run_persistence_error"
        ),
        parent = error
      )
    }
  )
  tryCatch(
    {
      tempest_stage_records_validate_workspace(
        durable_records,
        workspace,
        min_support_score = config@min_support_score
      )
      tempest_stage_records_validate_persisted_trust(
        durable_records,
        workspace,
        min_support_score = config@min_support_score
      )
      tempest_stage_records_validate_workspace_coverage(
        durable_records,
        workspace,
        require_extraction = "research" %in% state$completed_stages,
        require_verification = "polish" %in%
          state$completed_stages &&
          config@citation_policy %in% c("claim_verified", "strict")
      )
      tempest_stage_records_validate_claim_provenance(
        durable_records,
        workspace,
        research_manifest@research_run_id,
        state$experts
      )
      tempest_stage_records_validate_storm_coverage(durable_records, state)
    },
    error = function(error) {
      tempest_abort(
        "{.arg state} stage records do not match the research workspace.",
        class = tempest_persistence_error_class(
          "tempest_run_persistence_error"
        ),
        parent = error
      )
    }
  )
  tryCatch(
    tempest_persistence_validate_report_policy(
      state$report_md,
      state$title,
      workspace,
      config,
      durable_records
    ),
    error = function(error) {
      tempest_abort(
        "The STORM report does not match its authoritative evidence policy.",
        class = tempest_persistence_error_class(
          "tempest_run_persistence_error"
        ),
        parent = error
      )
    }
  )
  research_manifest <- tryCatch(
    tempest_persistence_manifest_bind_report(
      research_manifest,
      state$report_md
    ),
    error = function(error) {
      tempest_abort(
        "Could not bind the STORM report to its research manifest.",
        class = tempest_persistence_error_class(
          "tempest_run_persistence_error"
        ),
        parent = error
      )
    }
  )
  if (
    identical(research_manifest@status, "succeeded") &&
      !tempest_storm_state_is_complete(state)
  ) {
    tempest_abort(
      paste0(
        "A succeeded {.arg research_manifest} requires the full requested ",
        "dependency chain, exact stage history, and a non-empty report."
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
      "{.arg research_manifest} does not match the current configuration.",
      class = tempest_persistence_error_class(
        "tempest_run_persistence_error"
      )
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
  tempest_storm_validate_persisted_state(
    state,
    workspace,
    action = "save"
  )
  if (length(existing_files) > 0L) {
    tempest_persistence_require_regular_bundle_files(
      run_dir,
      "run_config.json",
      "Existing STORM root manifest",
      tempest_persistence_error_class("tempest_run_persistence_error")
    )
    existing_manifest <- tempest_read_json_strict(
      file.path(run_dir, "run_config.json"),
      what = "existing STORM run manifest",
      class = tempest_persistence_error_class(
        "tempest_run_persistence_error"
      )
    )
    tryCatch(
      tempest_run_bundle_validate_manifest(run_dir, existing_manifest),
      error = function(error) {
        tempest_abort(
          "Refusing to replace an invalid existing STORM bundle.",
          class = tempest_persistence_error_class(
            "tempest_run_persistence_error"
          ),
          parent = error
        )
      }
    )
    existing_requested <- tempest_storm_requested_steps(
      existing_manifest$requested_steps,
      from_record = TRUE
    )
    existing_run_id <- existing_manifest$research_manifest$research_run_id %||%
      NULL
    if (
      !identical(existing_requested, requested_steps) ||
        !identical(existing_run_id, research_manifest@research_run_id)
    ) {
      tempest_abort(
        paste0(
          "An existing STORM bundle cannot change its immutable requested ",
          "steps or research run identity."
        ),
        class = tempest_persistence_error_class(
          "tempest_run_persistence_error"
        )
      )
    }
  }

  workspace_record <- tempest_research_workspace_snapshot(workspace)
  tempest_persistence_credential_audit(
    list(
      research_manifest = tempest_research_manifest_record(
        research_manifest
      ),
      state = tempest_storm_state_record(state),
      workspace = workspace_record
    ),
    "STORM run snapshot",
    tempest_persistence_error_class("tempest_run_persistence_error")
  )

  staging_dir <- tempfile(
    pattern = paste0(".", basename(run_dir), "-staging-"),
    tmpdir = dirname(run_dir)
  )
  dir.create(staging_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(staging_dir, recursive = TRUE, force = TRUE), add = TRUE)
  paths <- tempest_run_artifact_paths(staging_dir)
  files <- character()
  files <- c(
    files,
    tempest_run_bundle_write_json(
      staging_dir,
      "workspace.json",
      workspace_record
    ),
    tempest_run_bundle_write_json(
      staging_dir,
      "references.json",
      state$references
    ),
    tempest_run_bundle_write_json(
      staging_dir,
      "stage_records.json",
      tempest_stage_records_data(durable_records)
    ),
    tempest_run_bundle_write_json(
      staging_dir,
      "experts.json",
      tempest_expert_records(state$experts)
    )
  )
  json_fields <- c(
    perspectives = "perspectives.json",
    draft_outline = "direct_gen_outline.json",
    outline = "storm_gen_outline.json"
  )
  for (field in names(json_fields)) {
    value <- state[[field]]
    if (!is.null(value)) {
      files <- c(
        files,
        tempest_run_bundle_write_json(
          staging_dir,
          json_fields[[field]],
          value
        )
      )
    }
  }
  text_fields <- c(
    lead_section = "lead_section.md",
    draft_md = "storm_gen_article.md",
    report_md = "storm_gen_article_polished.md"
  )
  for (field in names(text_fields)) {
    value <- state[[field]]
    if (rlang::is_string(value)) {
      files <- c(
        files,
        tempest_run_bundle_write_text(
          staging_dir,
          text_fields[[field]],
          value
        )
      )
    }
  }
  files <- c(
    files,
    tempest_graft_snapshot_write(
      staging_dir,
      workspace$graft_snapshot,
      tempest_persistence_error_class(
        "tempest_run_persistence_error"
      )
    )
  )
  files <- sort(unique(files))
  checksums <- stats::setNames(
    lapply(
      files,
      \(file) tempest_session_bundle_checksum(staging_dir, file)
    ),
    files
  )
  metadata <- list(
    topic = state$topic,
    title = state$title,
    requested_steps = requested_steps,
    completed_stages = state$completed_stages,
    schema_version = 7L,
    bundle_type = "storm",
    bundle_status = "complete",
    research_manifest = tempest_research_manifest_record(research_manifest),
    report_reference = tempest_persistence_report_reference(state$report_md),
    workspace = tempest_storm_workspace_identity_record(workspace),
    files = as.list(unname(files)),
    checksums = checksums
  )
  tempest_run_bundle_write_json(
    staging_dir,
    "run_config.json",
    metadata
  )
  tryCatch(
    {
      tempest_persistence_require_regular_bundle_files(
        staging_dir,
        "run_config.json",
        "Staged STORM root manifest",
        tempest_persistence_error_class("tempest_run_persistence_error")
      )
      tempest_run_bundle_validate_manifest(staging_dir, metadata)
      tempest_load_run_artifacts(
        staging_dir,
        config = config,
        program_set = program_set,
        run_id = research_manifest@research_run_id
      )
    },
    error = function(error) {
      tempest_abort(
        "The completed staged STORM bundle failed validation.",
        class = tempest_persistence_error_class(
          "tempest_run_persistence_error"
        ),
        parent = error
      )
    }
  )
  invisible(tempest_atomic_commit_bundle(
    staging_dir,
    run_dir,
    class = tempest_persistence_error_class(
      "tempest_run_persistence_error"
    ),
    what = "STORM bundle"
  ))
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
