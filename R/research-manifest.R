# Product-specific research manifest -----------------------------------------

tempest_research_manifest_abort <- function(message, ..., parent = NULL) {
  tempest_abort(
    message,
    ...,
    class = c("tempest_research_manifest_error", "tempest_error"),
    parent = parent,
    .envir = rlang::caller_env()
  )
}

tempest_opaque_identifier_valid <- function(value) {
  if (
    !rlang::is_string(value) ||
      is.na(value) ||
      nchar(value, type = "bytes") > 128L
  ) {
    return(FALSE)
  }
  stages <- c(
    "perspectives",
    "personas",
    "query_decomposition",
    "extract_claims",
    "verify_claim_support",
    "next_question",
    "draft_outline",
    "refined_outline",
    "section_writing",
    "lead_section"
  )
  builtin_ids <- c(
    paste0("tempest::", stages),
    paste0("tempest::evaluator/", stages)
  )
  safe_builtin <- value %in% builtin_ids
  safe_sha256 <- grepl("^sha256:[a-f0-9]{64}$", value)
  safe_namespaced <- grepl(
    "^[A-Za-z][A-Za-z0-9._-]*::[A-Za-z0-9][A-Za-z0-9._/@:+-]{0,120}$",
    value
  )
  safe_opaque <- grepl("^[A-Za-z0-9][A-Za-z0-9._/@:+-]{0,127}$", value)
  credential_marker <- grepl(
    paste0(
      "(^|[:/@._+-])(?:bearer|authorization|api[-_]?key|",
      "access[-_]?token|refresh[-_]?token|token|secret|password|",
      "client[-_]?secret|private[-_]?key)(?:$|[:/@._+-])"
    ),
    value,
    ignore.case = TRUE,
    perl = TRUE
  )
  (safe_builtin || safe_sha256 || safe_namespaced || safe_opaque) &&
    !credential_marker &&
    !tempest_contract_sensitive_scalar(value)
}

tempest_research_manifest_string <- function(value, arg) {
  if (!tempest_opaque_identifier_valid(value)) {
    tempest_research_manifest_abort(
      paste0(
        "{.arg {arg}} must be a bounded opaque identifier, not prose or ",
        "credentials."
      )
    )
  }
  value
}

tempest_research_manifest_choice <- function(value, arg, choices) {
  value <- tempest_research_manifest_string(value, arg)
  if (!value %in% choices) {
    tempest_research_manifest_abort(
      "{.arg {arg}} must be one of {.val {choices}}."
    )
  }
  value
}

tempest_execution_paths <- function() {
  c("governed", "grounded", "exploratory")
}

tempest_support_statuses <- function() {
  c(
    "verified",
    "partially_supported",
    "unsupported",
    "conflicted",
    "unknown"
  )
}

tempest_execution_path <- function(value) {
  tempest_research_manifest_choice(
    value,
    "execution_path",
    tempest_execution_paths()
  )
}

tempest_support_status <- function(value) {
  tempest_research_manifest_choice(
    value,
    "support_status",
    tempest_support_statuses()
  )
}

tempest_research_provenance_record <- function(
  execution_path,
  support_status
) {
  # These dimensions are recorded independently. In particular, callers must
  # establish the governed-path conditions; executing dsprrr is not enough.
  list(
    execution_path = tempest_execution_path(execution_path),
    support_status = tempest_support_status(support_status)
  )
}

tempest_research_manifest_schema_version <- function(value) {
  if (
    !is.numeric(value) ||
      length(value) != 1L ||
      is.na(value) ||
      !is.finite(value) ||
      value != 1L
  ) {
    tempest_research_manifest_abort(
      "{.arg schema_version} must be the supported version `1`."
    )
  }
  1L
}

tempest_research_manifest_digest <- function(value) {
  value <- tempest_research_manifest_string(value, "config_digest")
  if (!grepl("^sha256:[a-f0-9]{64}$", value)) {
    tempest_research_manifest_abort(
      paste0(
        "{.arg config_digest} must use the form ",
        "{.code sha256:<64 lowercase hexadecimal characters>}."
      )
    )
  }
  value
}

tempest_research_manifest_sensitive_name <- function(value) {
  tempest_research_sensitive_name(value)
}

tempest_research_manifest_list_names <- function(value, path) {
  tempest_research_reference_list_names(
    value,
    path,
    abort = tempest_research_manifest_abort
  )
}

tempest_research_manifest_canonical_value <- function(
  value,
  path = "value",
  reject_sensitive = TRUE
) {
  tempest_research_reference_value(
    value,
    path,
    abort = tempest_research_manifest_abort,
    noun = "Manifest references",
    reject_sensitive = reject_sensitive
  )
}

tempest_research_manifest_validate_ids <- function(value, path) {
  if (!is.list(value) || length(value) == 0L) {
    return(value)
  }
  value_names <- names(value)
  if (is.null(value_names)) {
    return(lapply(
      seq_along(value),
      function(index) {
        tempest_research_manifest_validate_ids(
          value[[index]],
          paste0(path, "[[", index, "]]")
        )
      }
    ))
  }
  for (name in value_names) {
    child_path <- paste0(path, "$", name)
    child <- value[[name]]
    normalized_name <- tolower(gsub("[^A-Za-z0-9]+", "_", name))
    if (grepl("(^|_)ids$", normalized_name)) {
      if (is.null(child)) {
        next
      }
      if (is.character(child) && length(child) == 1L) {
        child <- list(child)
      }
      valid <- is.list(child) &&
        is.null(names(child)) &&
        all(vapply(
          child,
          function(id) {
            is.character(id) &&
              length(id) == 1L &&
              !is.na(id) &&
              nzchar(tempest_trim(id))
          },
          logical(1)
        ))
      if (!valid) {
        tempest_research_manifest_abort(
          "ID collections must contain only non-empty strings at {.field {child_path}}."
        )
      }
      value[[name]] <- lapply(child, tempest_trim)
      next
    }
    if (grepl("(^|_)id$", normalized_name)) {
      if (
        !is.null(child) &&
          (!is.character(child) ||
            length(child) != 1L ||
            is.na(child) ||
            !nzchar(tempest_trim(child)))
      ) {
        tempest_research_manifest_abort(
          "IDs must be `NULL` or a non-empty string at {.field {child_path}}."
        )
      }
      if (!is.null(child)) {
        value[[name]] <- tempest_trim(child)
      }
      next
    }
    value[[name]] <- tempest_research_manifest_validate_ids(child, child_path)
  }
  value
}

tempest_research_manifest_reference_list <- function(value, arg) {
  value <- value %||% list()
  if (!is.list(value) || is.data.frame(value)) {
    tempest_research_manifest_abort(
      "{.arg {arg}} must be a canonical JSON-compatible list."
    )
  }
  value <- tempest_research_manifest_canonical_value(value, arg)
  tempest_research_manifest_validate_ids(value, arg)
}

tempest_research_manifest_unknown_fields <- function(
  value,
  allowed,
  path
) {
  unknown <- setdiff(names(value) %||% character(), allowed)
  if (length(unknown) > 0L) {
    tempest_research_manifest_abort(
      "Unknown reference fields at {.field {path}}: {.field {unknown}}."
    )
  }
  invisible(value)
}

tempest_research_manifest_named_record <- function(value, path) {
  if (!is.list(value) || is.data.frame(value) || length(value) == 0L) {
    tempest_research_manifest_abort(
      "{.field {path}} must be a non-empty named reference record."
    )
  }
  value_names <- names(value)
  if (
    is.null(value_names) ||
      anyNA(value_names) ||
      any(!nzchar(value_names)) ||
      anyDuplicated(value_names)
  ) {
    tempest_research_manifest_abort(
      "{.field {path}} must be a non-empty named reference record."
    )
  }
  value
}

tempest_research_manifest_id <- function(value, path, nullable = FALSE) {
  if (is.null(value) && isTRUE(nullable)) {
    return(NULL)
  }
  if (!tempest_opaque_identifier_valid(value)) {
    tempest_research_manifest_abort(
      paste0(
        "{.field {path}} must be a bounded opaque identifier, not prose or ",
        "credentials."
      )
    )
  }
  value
}

tempest_research_manifest_ids <- function(value, path) {
  if (is.character(value)) {
    value <- as.list(value)
  }
  if (!is.list(value) || !is.null(names(value))) {
    tempest_research_manifest_abort(
      "{.field {path}} must be an unnamed list of identifiers."
    )
  }
  ids <- lapply(
    seq_along(value),
    \(index) {
      tempest_research_manifest_id(
        value[[index]],
        paste0(path, "[[", index, "]]")
      )
    }
  )
  as.list(sort(unique(unlist(ids, use.names = FALSE))))
}

tempest_research_manifest_program_contract_version <- function(value, path) {
  if (
    !is.numeric(value) ||
      length(value) != 1L ||
      is.na(value) ||
      !is.finite(value) ||
      value != 1L
  ) {
    tempest_research_manifest_abort(
      "{.field {path}} must be the supported contract version `1`."
    )
  }
  1L
}

tempest_research_manifest_program_artifact_id <- function(value, path) {
  if (
    !rlang::is_string(value) ||
      is.na(value) ||
      !grepl("^sha256:[a-f0-9]{64}$", value)
  ) {
    tempest_research_manifest_abort(
      paste0(
        "{.field {path}} must use the form ",
        "{.code sha256:<64 lowercase hexadecimal characters>}."
      )
    )
  }
  value
}

tempest_research_manifest_program_artifact_reference <- function(
  value,
  stage,
  path
) {
  value <- tempest_research_manifest_canonical_value(value, path)
  value <- tempest_research_manifest_named_record(value, path)
  reference_type <- tempest_research_manifest_id(
    value$type,
    paste0(path, "$type")
  )
  if (!reference_type %in% c("builtin", "file")) {
    tempest_research_manifest_abort(
      "{.field {path}$type} must be {.val builtin} or {.val file}."
    )
  }
  if (identical(reference_type, "builtin")) {
    fields <- c("type", "id")
    if (!setequal(names(value), fields)) {
      tempest_research_manifest_abort(
        paste0(
          "{.field {path}} must contain exactly {.field type} ",
          "and {.field id} for a builtin reference."
        )
      )
    }
    id <- tempest_research_manifest_id(
      value$id,
      paste0(path, "$id")
    )
    expected_id <- paste0("tempest::", stage)
    if (!identical(id, expected_id)) {
      tempest_research_manifest_abort(
        "{.field {path}$id} must be the builtin stage id {.val {expected_id}}."
      )
    }
    return(list(type = reference_type, id = id))
  }
  fields <- c("type", "path")
  if (!setequal(names(value), fields)) {
    tempest_research_manifest_abort(
      paste0(
        "{.field {path}} must contain exactly {.field type} and ",
        "{.field path} for a file reference."
      )
    )
  }
  artifact_path <- tempest_research_manifest_id(
    value$path,
    paste0(path, "$path")
  )
  expected_path <- paste0("programs/", stage, ".rds")
  if (!identical(artifact_path, expected_path)) {
    tempest_research_manifest_abort(
      "{.field {path}$path} must be the portable stage locator {.path {expected_path}}."
    )
  }
  list(type = reference_type, path = artifact_path)
}

tempest_research_manifest_programs <- function(value) {
  value <- value %||% list()
  if (!is.list(value) || is.data.frame(value)) {
    tempest_research_manifest_abort(
      "{.arg programs} must be a named list of program references."
    )
  }
  if (length(value) == 0L) {
    return(list())
  }
  stages <- names(value)
  if (
    is.null(stages) ||
      anyNA(stages) ||
      any(!nzchar(stages)) ||
      anyDuplicated(stages)
  ) {
    tempest_research_manifest_abort(
      "{.arg programs} must use unique non-empty stage names."
    )
  }
  value <- value[order(stages)]
  allowed <- c(
    "stage",
    "contract_version",
    "program_artifact_id",
    "artifact_reference",
    "governed_procedure_revision_id",
    "evaluator_id",
    "evaluator_version"
  )
  stats::setNames(
    lapply(names(value), function(stage) {
      path <- paste0("programs$", stage)
      reference <- tempest_research_manifest_canonical_value(
        value[[stage]],
        path
      )
      reference <- tempest_research_manifest_named_record(reference, path)
      tempest_research_manifest_unknown_fields(reference, allowed, path)
      if (!setequal(names(reference), allowed)) {
        tempest_research_manifest_abort(
          paste0(
            "{.field {path}} must contain exactly the current ProgramSet ",
            "reference fields."
          )
        )
      }
      reference$stage <- tempest_research_manifest_id(
        reference$stage,
        paste0(path, "$stage")
      )
      if (!identical(reference$stage, stage)) {
        tempest_research_manifest_abort(
          "{.field {path}$stage} must match its named manifest stage."
        )
      }
      reference$contract_version <-
        tempest_research_manifest_program_contract_version(
          reference$contract_version,
          paste0(path, "$contract_version")
        )
      reference$program_artifact_id <-
        tempest_research_manifest_program_artifact_id(
          reference$program_artifact_id,
          paste0(path, "$program_artifact_id")
        )
      reference$artifact_reference <-
        tempest_research_manifest_program_artifact_reference(
          reference$artifact_reference,
          stage,
          paste0(path, "$artifact_reference")
        )
      reference["governed_procedure_revision_id"] <- list(
        tempest_research_manifest_id(
          reference$governed_procedure_revision_id,
          paste0(path, "$governed_procedure_revision_id"),
          nullable = TRUE
        )
      )
      reference$evaluator_id <- tempest_research_manifest_id(
        reference$evaluator_id,
        paste0(path, "$evaluator_id")
      )
      reference$evaluator_version <- tempest_research_manifest_id(
        reference$evaluator_version,
        paste0(path, "$evaluator_version")
      )
      reference[allowed]
    }),
    names(value)
  )
}

tempest_research_manifest_program_identity_records <- function(value) {
  value <- tempest_research_manifest_programs(value)
  identity_fields <- c(
    "stage",
    "contract_version",
    "program_artifact_id",
    "governed_procedure_revision_id",
    "evaluator_id",
    "evaluator_version"
  )
  lapply(value, \(reference) reference[identity_fields])
}

tempest_research_manifest_programs_same_identity <- function(x, y) {
  identical(
    tempest_research_manifest_program_identity_records(x),
    tempest_research_manifest_program_identity_records(y)
  )
}

tempest_research_manifest_knowledge_snapshot <- function(value) {
  value <- value %||% list()
  if (!is.list(value) || is.data.frame(value)) {
    tempest_research_manifest_abort(
      "{.arg knowledge_snapshot} must be a snapshot reference record."
    )
  }
  if (length(value) == 0L) {
    return(list())
  }
  value <- tempest_research_manifest_canonical_value(
    value,
    "knowledge_snapshot"
  )
  value <- tempest_research_manifest_named_record(
    value,
    "knowledge_snapshot"
  )
  allowed <- c(
    "batch_id",
    "commit_order",
    "committed_at",
    "history_complete",
    "schema_build_digest",
    "schema_version",
    "snapshot_id",
    "store_format_version",
    "store_id"
  )
  tempest_research_manifest_unknown_fields(
    value,
    allowed,
    "knowledge_snapshot"
  )
  if (is.null(value$snapshot_id)) {
    tempest_research_manifest_abort(
      "{.field knowledge_snapshot$snapshot_id} is required."
    )
  }
  value$snapshot_id <- tempest_research_manifest_id(
    value$snapshot_id,
    "knowledge_snapshot$snapshot_id"
  )
  if (!is.null(value$store_id)) {
    value$store_id <- tempest_research_manifest_id(
      value$store_id,
      "knowledge_snapshot$store_id"
    )
  }
  for (field in intersect(
    c(
      "batch_id",
      "committed_at",
      "schema_build_digest",
      "store_format_version"
    ),
    names(value)
  )) {
    if (is.null(value[[field]])) {
      next
    }
    value[[field]] <- tempest_research_manifest_id(
      value[[field]],
      paste0("knowledge_snapshot$", field)
    )
  }
  if (!is.null(value$schema_version)) {
    schema_version <- value$schema_version
    if (
      !is.numeric(schema_version) ||
        length(schema_version) != 1L ||
        is.na(schema_version) ||
        !is.finite(schema_version) ||
        schema_version < 1 ||
        schema_version != trunc(schema_version) ||
        schema_version > .Machine$integer.max
    ) {
      tempest_research_manifest_abort(
        "{.field knowledge_snapshot$schema_version} must be a positive whole number."
      )
    }
    value$schema_version <- as.integer(schema_version)
  }
  if (!is.null(value$history_complete)) {
    history_complete <- value$history_complete
    if (
      !is.logical(history_complete) ||
        length(history_complete) != 1L ||
        is.na(history_complete)
    ) {
      tempest_research_manifest_abort(
        "{.field knowledge_snapshot$history_complete} must be TRUE or FALSE."
      )
    }
  }
  if (!is.null(value$commit_order)) {
    commit_order <- value$commit_order
    if (
      !is.numeric(commit_order) ||
        length(commit_order) != 1L ||
        is.na(commit_order) ||
        !is.finite(commit_order) ||
        commit_order < 0 ||
        commit_order != trunc(commit_order) ||
        commit_order >= 2^53
    ) {
      tempest_research_manifest_abort(
        "{.field knowledge_snapshot$commit_order} must be a non-negative whole number."
      )
    }
    value$commit_order <- as.double(commit_order)
  }
  value[order(names(value))]
}

tempest_research_manifest_runtime <- function(value) {
  value <- value %||% list()
  if (!is.list(value) || is.data.frame(value)) {
    tempest_research_manifest_abort(
      "{.arg runtime} must contain only Deputy session and run identifiers."
    )
  }
  if (length(value) == 0L) {
    return(list())
  }
  value <- tempest_research_manifest_canonical_value(value, "runtime")
  value <- tempest_research_manifest_named_record(value, "runtime")
  allowed <- c("deputy_run_ids", "deputy_session_ids")
  tempest_research_manifest_unknown_fields(value, allowed, "runtime")
  for (field in names(value)) {
    value[[field]] <- tempest_research_manifest_ids(
      value[[field]],
      paste0("runtime$", field)
    )
  }
  value[order(names(value))]
}

tempest_research_manifest_reference_tree <- function(
  value,
  path,
  allowed,
  required_any,
  id_fields,
  scalar_fields,
  allow_empty = FALSE
) {
  if (!is.list(value) || is.data.frame(value)) {
    tempest_research_manifest_abort(
      "{.field {path}} must be a reference record or collection."
    )
  }
  if (length(value) == 0L) {
    if (isTRUE(allow_empty)) {
      return(list())
    }
    tempest_research_manifest_abort(
      "{.field {path}} must contain a non-empty reference record."
    )
  }
  value <- tempest_research_manifest_canonical_value(value, path)
  value_names <- names(value)
  if (is.null(value_names)) {
    return(lapply(seq_along(value), function(index) {
      tempest_research_manifest_reference_tree(
        value[[index]],
        paste0(path, "[[", index, "]]"),
        allowed,
        required_any,
        id_fields,
        scalar_fields,
        allow_empty = FALSE
      )
    }))
  }
  is_record <- any(value_names %in% allowed) ||
    any(!vapply(value, is.list, logical(1)))
  if (!is_record) {
    return(stats::setNames(
      lapply(value_names, function(name) {
        tempest_research_manifest_reference_tree(
          value[[name]],
          paste0(path, "$", name),
          allowed,
          required_any,
          id_fields,
          scalar_fields,
          allow_empty = FALSE
        )
      }),
      value_names
    ))
  }
  tempest_research_manifest_unknown_fields(value, allowed, path)
  if (!any(required_any %in% value_names)) {
    tempest_research_manifest_abort(
      "{.field {path}} is missing a required identity field."
    )
  }
  for (field in intersect(value_names, c(id_fields, scalar_fields))) {
    value[[field]] <- tempest_research_manifest_id(
      value[[field]],
      paste0(path, "$", field)
    )
  }
  value[order(names(value))]
}

tempest_research_manifest_traces <- function(value) {
  tempest_research_manifest_reference_tree(
    value %||% list(),
    "traces",
    allowed = c(
      "agent_id",
      "correlation_id",
      "delegation_id",
      "deputy_run_id",
      "deputy_session_id",
      "expert_id",
      "parent_agent_id",
      "parent_run_id",
      "program_artifact_id",
      "role",
      "stage",
      "status",
      "tool_call_id",
      "trace_id",
      "trace_type"
    ),
    required_any = "trace_id",
    id_fields = c(
      "agent_id",
      "correlation_id",
      "delegation_id",
      "deputy_run_id",
      "deputy_session_id",
      "expert_id",
      "parent_agent_id",
      "parent_run_id",
      "program_artifact_id",
      "tool_call_id",
      "trace_id"
    ),
    scalar_fields = c("role", "stage", "status", "trace_type"),
    allow_empty = TRUE
  )
}

tempest_research_manifest_deliverables <- function(value) {
  tempest_research_manifest_reference_tree(
    value %||% list(),
    "deliverables",
    allowed = c(
      "artifact_id",
      "checksum",
      "deliverable_id",
      "report_id",
      "sha256",
      "status"
    ),
    required_any = c("artifact_id", "deliverable_id", "report_id"),
    id_fields = c("artifact_id", "deliverable_id", "report_id"),
    scalar_fields = c("checksum", "sha256", "status"),
    allow_empty = TRUE
  )
}

tempest_research_manifest_field <- function(value, field) {
  switch(
    field,
    programs = tempest_research_manifest_programs(value),
    knowledge_snapshot = tempest_research_manifest_knowledge_snapshot(value),
    runtime = tempest_research_manifest_runtime(value),
    traces = tempest_research_manifest_traces(value),
    deliverables = tempest_research_manifest_deliverables(value),
    tempest_research_manifest_abort(
      "Unknown research manifest reference field: {.field {field}}."
    )
  )
}

tempest_research_manifest_canonical_json <- function(value) {
  value <- tempest_research_manifest_canonical_value(value)
  tryCatch(
    as.character(jsonlite::toJSON(
      value,
      auto_unbox = TRUE,
      null = "null",
      digits = NA,
      pretty = FALSE,
      force = TRUE
    )),
    error = function(error) {
      if (inherits(error, "tempest_research_manifest_error")) {
        stop(error)
      }
      tempest_research_manifest_abort(
        "Could not encode a canonical research manifest record.",
        parent = error
      )
    }
  )
}

tempest_research_config_without_credentials <- function(
  value,
  path = "config$params"
) {
  if (!is.list(value)) {
    return(value)
  }
  value_names <- tempest_research_manifest_list_names(value, path)
  if (is.null(value_names)) {
    names(value) <- NULL
    return(lapply(
      seq_along(value),
      function(index) {
        tempest_research_config_without_credentials(
          value[[index]],
          paste0(path, "[[", index, "]]")
        )
      }
    ))
  }
  keep <- !vapply(
    value_names,
    tempest_research_manifest_sensitive_name,
    logical(1)
  )
  value <- value[keep]
  value_names <- names(value)
  stats::setNames(
    lapply(
      value_names,
      function(name) {
        tempest_research_config_without_credentials(
          value[[name]],
          paste0(path, "$", name)
        )
      }
    ),
    value_names
  )
}

tempest_research_config_projection <- function(config) {
  if (!S7::S7_inherits(config, TempestConfig)) {
    tempest_research_manifest_abort(
      "{.arg config} must be created by {.fn tempest_config}."
    )
  }
  cache_ttl <- if (is.infinite(config@cache_ttl)) {
    "unbounded"
  } else {
    config@cache_ttl
  }
  projection <- list(
    schema_version = 1L,
    models = config@models,
    params = tempest_research_config_without_credentials(config@params),
    search_provider = config@search_provider,
    cache_enabled = config@cache_enabled,
    cache_ttl = cache_ttl,
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
  projection <- tempest_research_config_without_credentials(
    projection,
    path = "config"
  )
  tempest_research_manifest_canonical_value(
    projection,
    path = "config",
    reject_sensitive = FALSE
  )
}

tempest_research_config_digest <- function(config) {
  projection <- tempest_research_config_projection(config)
  paste0(
    "sha256:",
    digest::digest(
      tempest_research_manifest_canonical_json(projection),
      algo = "sha256",
      serialize = FALSE
    )
  )
}

tempest_research_manifest_prop_string <- function() {
  S7::new_property(
    S7::class_character,
    validator = function(value) {
      if (
        length(value) != 1L ||
          is.na(value) ||
          !nzchar(tempest_trim(value))
      ) {
        "must be a single non-empty string"
      }
    }
  )
}

tempest_research_manifest_prop_enum <- function(choices) {
  S7::new_property(
    S7::class_character,
    validator = function(value) {
      if (length(value) != 1L || is.na(value) || !value %in% choices) {
        sprintf("must be one of: %s", paste(choices, collapse = ", "))
      }
    }
  )
}

tempest_research_manifest_s7_validator <- function(self) {
  if (!identical(self@schema_version, 1L)) {
    return("schema_version must be the supported version 1")
  }
  if (!grepl("^sha256:[a-f0-9]{64}$", self@config_digest)) {
    return("config_digest must be a SHA-256 identifier")
  }
  for (field in c(
    "programs",
    "knowledge_snapshot",
    "runtime",
    "traces",
    "deliverables"
  )) {
    value <- S7::prop(self, field)
    valid <- tryCatch(
      identical(
        value,
        tempest_research_manifest_field(value, field)
      ),
      error = function(error) conditionMessage(error)
    )
    if (!identical(valid, TRUE)) {
      if (is.character(valid)) {
        return(valid)
      }
      return(sprintf("%s must use canonical JSON-compatible values", field))
    }
  }
}

#' TempestResearchManifest (S7)
#'
#' A small immutable value contract describing the identities and references
#' used by one STORM or Co-STORM research run. Runtime clients, credentials,
#' stores, tools, and executable objects are deliberately excluded.
#'
#' @keywords internal
TempestResearchManifest <- S7::new_class(
  "TempestResearchManifest",
  properties = list(
    schema_version = S7::new_property(S7::class_integer, default = 1L),
    research_run_id = tempest_research_manifest_prop_string(),
    mode = tempest_research_manifest_prop_enum(c("storm", "costorm")),
    config_digest = tempest_research_manifest_prop_string(),
    programs = S7::new_property(S7::class_list, default = list()),
    knowledge_snapshot = S7::new_property(S7::class_list, default = list()),
    runtime = S7::new_property(S7::class_list, default = list()),
    traces = S7::new_property(S7::class_list, default = list()),
    deliverables = S7::new_property(S7::class_list, default = list()),
    status = tempest_research_manifest_prop_enum(
      c("running", "succeeded", "failed", "cancelled")
    )
  ),
  validator = tempest_research_manifest_s7_validator
)

#' Create a Tempest research manifest
#'
#' `tempest_research_manifest()` records only durable identities and references
#' for one STORM or Co-STORM run. Supply a [tempest_config()] object to compute
#' its behavior-relevant digest. `config_digest` is intended for restoration of
#' an existing record.
#'
#' Reference fields accept only canonical JSON-compatible plain values. They
#' cannot contain credentials, chats, functions, environments, connections,
#' S7 or R6 objects, external pointers, or missing and non-finite values.
#'
#' @param research_run_id Stable research-run identifier.
#' @param mode Product mode, either `"storm"` or `"costorm"`.
#' @param config A `TempestConfig` used to compute `config_digest`, or `NULL`
#'   when restoring an existing digest.
#' @param config_digest Existing SHA-256 configuration identity. Supply this
#'   only when `config` is unavailable during restoration.
#' @param programs Named references to exact scientific programs.
#' @param knowledge_snapshot Reference to a pinned accepted-knowledge snapshot.
#' @param runtime Opaque Deputy session and run references.
#' @param traces References to Deputy or dsprrr traces.
#' @param deliverables References to product deliverables.
#' @param status Run status: `"running"`, `"succeeded"`, `"failed"`, or
#'   `"cancelled"`.
#' @param schema_version Manifest record schema. Only version 1 is supported.
#' @return A `TempestResearchManifest` S7 object.
#' @examples
#' manifest <- tempest_research_manifest(
#'   research_run_id = "research-123",
#'   mode = "storm",
#'   config = tempest_config()
#' )
#' manifest@status
#' @export
tempest_research_manifest <- function(
  research_run_id,
  mode = c("storm", "costorm"),
  config = NULL,
  config_digest = NULL,
  programs = list(),
  knowledge_snapshot = list(),
  runtime = list(
    deputy_session_ids = character(),
    deputy_run_ids = character()
  ),
  traces = list(),
  deliverables = list(),
  status = "running",
  schema_version = 1L
) {
  if (missing(mode)) {
    mode <- "storm"
  }
  research_run_id <- tempest_research_manifest_string(
    research_run_id,
    "research_run_id"
  )
  mode <- tempest_research_manifest_choice(
    mode,
    "mode",
    c("storm", "costorm")
  )
  status <- tempest_research_manifest_choice(
    status,
    "status",
    c("running", "succeeded", "failed", "cancelled")
  )
  schema_version <- tempest_research_manifest_schema_version(schema_version)
  if (is.null(config) && is.null(config_digest)) {
    tempest_research_manifest_abort(
      "Supply {.arg config} or an existing {.arg config_digest}."
    )
  }
  computed_digest <- if (is.null(config)) {
    NULL
  } else {
    tempest_research_config_digest(config)
  }
  if (!is.null(config_digest)) {
    config_digest <- tempest_research_manifest_digest(config_digest)
  }
  if (
    !is.null(computed_digest) &&
      !is.null(config_digest) &&
      !identical(computed_digest, config_digest)
  ) {
    tempest_research_manifest_abort(
      "{.arg config_digest} does not match the supplied {.arg config}."
    )
  }
  config_digest <- computed_digest %||% config_digest

  TempestResearchManifest(
    schema_version = schema_version,
    research_run_id = research_run_id,
    mode = mode,
    config_digest = config_digest,
    programs = tempest_research_manifest_programs(programs),
    knowledge_snapshot = tempest_research_manifest_knowledge_snapshot(
      knowledge_snapshot
    ),
    runtime = tempest_research_manifest_runtime(runtime),
    traces = tempest_research_manifest_traces(traces),
    deliverables = tempest_research_manifest_deliverables(deliverables),
    status = status
  )
}

tempest_research_manifest_record <- function(manifest) {
  if (!S7::S7_inherits(manifest, TempestResearchManifest)) {
    tempest_research_manifest_abort(
      "{.arg manifest} must be created by {.fn tempest_research_manifest}."
    )
  }
  list(
    schema_version = manifest@schema_version,
    research_run_id = manifest@research_run_id,
    mode = manifest@mode,
    config_digest = manifest@config_digest,
    programs = manifest@programs,
    knowledge_snapshot = manifest@knowledge_snapshot,
    runtime = manifest@runtime,
    traces = manifest@traces,
    deliverables = manifest@deliverables,
    status = manifest@status
  )
}

tempest_research_manifest_from_record <- function(record) {
  if (!is.list(record) || is.data.frame(record)) {
    tempest_research_manifest_abort(
      "{.arg record} must be a research manifest record."
    )
  }
  fields <- c(
    "schema_version",
    "research_run_id",
    "mode",
    "config_digest",
    "programs",
    "knowledge_snapshot",
    "runtime",
    "traces",
    "deliverables",
    "status"
  )
  record_names <- names(record)
  if (
    is.null(record_names) ||
      anyNA(record_names) ||
      anyDuplicated(record_names) ||
      !setequal(record_names, fields)
  ) {
    tempest_research_manifest_abort(
      "{.arg record} must contain exactly the schema version 1 manifest fields."
    )
  }
  record <- tempest_research_manifest_canonical_value(record, "record")
  tempest_research_manifest(
    research_run_id = record$research_run_id,
    mode = record$mode,
    config_digest = record$config_digest,
    programs = record$programs,
    knowledge_snapshot = record$knowledge_snapshot,
    runtime = record$runtime,
    traces = record$traces,
    deliverables = record$deliverables,
    status = record$status,
    schema_version = record$schema_version
  )
}

tempest_research_manifest_update <- function(
  manifest,
  status = NULL,
  runtime = NULL,
  traces = NULL,
  deliverables = NULL
) {
  if (!S7::S7_inherits(manifest, TempestResearchManifest)) {
    tempest_research_manifest_abort(
      "{.arg manifest} must be created by {.fn tempest_research_manifest}."
    )
  }
  if (
    !is.null(status) &&
      !identical(manifest@status, "running") &&
      !identical(status, manifest@status)
  ) {
    tempest_research_manifest_abort(
      paste0(
        "A terminal research manifest cannot transition from ",
        "{.val {manifest@status}} to {.val {status}}."
      )
    )
  }
  tempest_research_manifest(
    research_run_id = manifest@research_run_id,
    mode = manifest@mode,
    config_digest = manifest@config_digest,
    programs = manifest@programs,
    knowledge_snapshot = manifest@knowledge_snapshot,
    runtime = runtime %||% manifest@runtime,
    traces = traces %||% manifest@traces,
    deliverables = deliverables %||% manifest@deliverables,
    status = status %||% manifest@status,
    schema_version = manifest@schema_version
  )
}
