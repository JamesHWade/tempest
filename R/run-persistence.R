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
tempest_source_store_snapshot <- function(store, artifacts = NULL) {
  stopifnot(inherits(store, "SourceStore"))
  if (!is.null(artifacts) && !is.character(artifacts)) {
    tempest_abort("{.arg artifacts} must be NULL or a character vector.")
  }

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
    resources = lapply(
      store$list_resources(),
      tempest_resource_record,
      include_content = TRUE
    ),
    sources = Filter(
      function(source) !is.na(source$url),
      store$list_sources()
    ),
    claims = lapply(store$list_claims(), tempest_claim_to_list),
    evidence_spans = lapply(
      tempest_env_values(store$evidence_spans),
      tempest_evidence_span_to_list
    ),
    disputes = lapply(store$list_disputes(), tempest_dispute_to_list),
    artifacts = stats::setNames(
      lapply(artifact_names, function(name) store$get_artifact(name)),
      artifact_names
    )
  )
}

#' @keywords internal
tempest_source_store_restore_abort <- function(message) {
  tempest_abort(
    c("Cannot restore SourceStore snapshot.", x = message),
    class = tempest_persistence_error_class(
      "tempest_source_store_restore_error"
    )
  )
}

#' @keywords internal
tempest_source_store_restore <- function(snapshot, store = SourceStore$new()) {
  if (!is.list(snapshot)) {
    tempest_abort(
      "{.arg snapshot} must be a list.",
      class = tempest_persistence_error_class(
        "tempest_source_store_restore_error"
      )
    )
  }
  if (!inherits(store, "SourceStore")) {
    tempest_abort(
      "{.arg store} must be a SourceStore.",
      class = tempest_persistence_error_class(
        "tempest_source_store_restore_error"
      )
    )
  }
  store_schema <- snapshot$schema_version %||% 1L
  if (!as.integer(store_schema) %in% c(1L, 2L)) {
    tempest_source_store_restore_abort(
      paste0("Unsupported store schema version: ", store_schema, ".")
    )
  }

  if (identical(as.integer(store_schema), 2L)) {
    records <- snapshot$resources %||% list()
    if (!is.list(records) || is.data.frame(records)) {
      tempest_source_store_restore_abort(
        "Typed evidence resources must be a list of resource records."
      )
    }
    for (i in seq_along(records)) {
      resource <- tryCatch(
        tempest_resource_from_data(records[[i]]),
        error = function(error) {
          tempest_source_store_restore_abort(
            paste0("Resource entry ", i, " is invalid.")
          )
        }
      )
      store$upsert_resource(resource)
    }
  } else {
    snapshot_sources <- snapshot$sources %||% list()
    # Schema 1 restoration is retained only for existing development bundles.
    for (i in seq_along(snapshot_sources)) {
      source <- snapshot_sources[[i]]
      if (!is.list(source) || is.null(source$url)) {
        tempest_source_store_restore_abort(
          paste0("Source entry ", i, " is missing a {.field url}.")
        )
      }
    }
    tempest_restore_sources(store, snapshot_sources)
  }

  source_ids <- purrr::map_chr(
    store$list_resources(),
    tempest_resource_identity
  )
  spans <- lapply(
    snapshot$evidence_spans %||% list(),
    tempest_evidence_span_from_list
  )
  for (span in spans) {
    if (!(span@source_id %in% source_ids)) {
      tempest_source_store_restore_abort(
        paste0(
          "Evidence span ",
          span@evidence_span_id,
          " cites unknown source id: ",
          span@source_id,
          "."
        )
      )
    }
    store$add_evidence_span(span)
  }

  claims <- lapply(snapshot$claims %||% list(), tempest_claim_from_list)
  for (claim in claims) {
    missing_source_ids <- setdiff(claim@source_ids, source_ids)
    if (length(missing_source_ids) > 0) {
      tempest_source_store_restore_abort(
        paste0(
          "Claim ",
          claim@claim_id,
          " cites unknown source id(s): ",
          paste(missing_source_ids, collapse = ", "),
          "."
        )
      )
    }
    store$add_claim(claim)
  }

  claim_ids <- purrr::map_chr(store$list_claims(), ~ .x@claim_id)
  disputes <- lapply(snapshot$disputes %||% list(), tempest_dispute_from_list)
  for (dispute in disputes) {
    missing_claim_ids <- setdiff(dispute@claim_ids, claim_ids)
    if (length(missing_claim_ids) > 0) {
      tempest_source_store_restore_abort(
        paste0(
          "Dispute ",
          dispute@dispute_id,
          " cites unknown claim id(s): ",
          paste(missing_claim_ids, collapse = ", "),
          "."
        )
      )
    }
    store$add_dispute(dispute)
  }

  artifacts <- snapshot$artifacts %||% list()
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

#' Snapshot a Co-STORM session
#'
#' `r lifecycle::badge("experimental")`
#'
#' `tempest_session_snapshot()` returns a structured, in-memory representation
#' of the durable state in a [TempestSession]. It includes the session identity,
#' expert profiles, transcript, mind map, typed deliverable specifications and
#' artifacts, auxiliary session state, progress-event history, expert-session
#' metadata, serializable skill and connection-reference records, capability
#' decisions, an attached generic workflow run, and the underlying
#' `SourceStore` ledger. Live chat handles, runtime clients, tools, closures,
#' Shiny reactive state, credentials, and provider request bodies are not
#' included.
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
  artifacts <- tempest_env_snapshot(session$artifacts)
  artifacts[
    c("report", "report_md", "mindmap_md", "progress_events")
  ] <- NULL
  runtime_records <- tempest_session_runtime_records(session$runtime)

  list(
    schema_version = 4L,
    package_version = tryCatch(
      as.character(utils::packageVersion("tempest")),
      error = function(e) NA_character_
    ),
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
    transcript = session$transcript,
    mindmap = session$mindmap,
    artifacts = artifacts,
    artifact_catalog = session$artifact_catalog$snapshot(
      include_content = TRUE
    ),
    suggested_questions = artifacts$suggested_questions %||% character(),
    progress_events = tempest_execution_events(session),
    store = tempest_source_store_snapshot(session$store),
    expert_sessions = tempest_expert_sessions_snapshot(session),
    workflow_run = if (inherits(session$workflow_run, "TempestRun")) {
      tempest_run_snapshot(session$workflow_run)
    } else {
      NULL
    }
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
#' [tempest_session_resume()]. It restores durable research state and creates
#' fresh chat/tool handles using the supplied runtime and `config`.
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
  schema_version <- snapshot$schema_version %||% NA_integer_
  if (!identical(as.integer(schema_version), 4L)) {
    tempest_session_restore_abort(
      paste0("Unsupported snapshot schema version: ", schema_version, ".")
    )
  }
  if (
    !rlang::is_string(snapshot$topic) || !nzchar(tempest_trim(snapshot$topic))
  ) {
    tempest_session_restore_abort("Snapshot must include a non-empty topic.")
  }

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
  store <- tempest_source_store_restore(snapshot$store %||% list())
  retriever <- tempest_retriever(config = config, store = store)
  session <- tempest_session(
    topic = snapshot$topic,
    config = config,
    runtime = runtime,
    experts = experts,
    connection_permissions = connection_permissions,
    retriever = retriever,
    progress = NULL,
    session_id = snapshot$session_id
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
    evidence_store = store
  )

  artifacts <- snapshot$artifacts %||% list()
  legacy_progress_events <- artifacts$progress_events %||% list()
  artifacts[
    c("report", "report_md", "mindmap_md", "progress_events")
  ] <- NULL
  for (name in names(artifacts)) {
    session$artifacts[[name]] <- artifacts[[name]]
  }
  if (
    !is.null(snapshot$suggested_questions) &&
      is.null(session$artifacts[["suggested_questions"]])
  ) {
    session$artifacts[["suggested_questions"]] <- snapshot$suggested_questions
  }
  # JSON read with `simplifyVector = FALSE` returns a multi-element question list
  # as an R list; coerce back to the character vector the in-memory artifact uses
  # so the type is stable across save and resume.
  if (!is.null(session$artifacts[["suggested_questions"]])) {
    session$artifacts[["suggested_questions"]] <- as.character(unlist(
      session$artifacts[["suggested_questions"]]
    ))
  }
  progress_events <- snapshot$progress_events
  if (is.null(progress_events)) {
    progress_events <- legacy_progress_events
  }
  session$events <- tempest_session_restore_progress_events(progress_events)

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
#' [TempestSession]. The bundle stores durable research state plus a typed
#' artifact catalog. Inline artifacts use explicit UTF-8 or canonical JSON
#' codecs, every declared file is checksummed, and the `session.json` manifest
#' is written last. Live chat handles, registered tool closures, Shiny reactive
#' state, credentials, and raw provider request bodies are not serialized.
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
      "store/resources.json",
      snapshot$store$resources
    ),
    tempest_session_bundle_write_json(
      staging_dir,
      "store/sources.json",
      snapshot$store$sources
    ),
    tempest_session_bundle_write_json(
      staging_dir,
      "store/claims.json",
      snapshot$store$claims
    ),
    tempest_session_bundle_write_json(
      staging_dir,
      "store/evidence_spans.json",
      snapshot$store$evidence_spans
    ),
    tempest_session_bundle_write_json(
      staging_dir,
      "store/disputes.json",
      snapshot$store$disputes
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

  if (!is.null(snapshot$artifacts$suggested_questions)) {
    files <- c(
      files,
      tempest_session_bundle_write_json(
        staging_dir,
        "artifacts/suggested_questions.json",
        snapshot$artifacts$suggested_questions
      )
    )
  }
  if (!is.null(snapshot$store$artifacts$citation_audit)) {
    files <- c(
      files,
      tempest_session_bundle_write_json(
        staging_dir,
        "artifacts/citation_audit.json",
        snapshot$store$artifacts$citation_audit
      )
    )
  }
  if (!is.null(snapshot$store$artifacts$references)) {
    files <- c(
      files,
      tempest_session_bundle_write_json(
        staging_dir,
        "artifacts/references.json",
        snapshot$store$artifacts$references
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
    package_version = snapshot$package_version,
    session_id = snapshot$session_id,
    topic = snapshot$topic,
    title = snapshot$title,
    saved_at = tempest_now_utc(),
    status = "complete",
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
  if (!identical(manifest$status %||% "", "complete")) {
    tempest_session_restore_abort("Session bundle manifest is not complete.")
  }
  files <- as.character(unlist(manifest$files %||% character()))
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
    "store/resources.json",
    "store/sources.json",
    "store/claims.json",
    "store/evidence_spans.json",
    "store/disputes.json",
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
#'   runtime, store, and typed-artifact state, must pass integrity checks.
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
  tempest_session_bundle_require_files(
    bundle_dir,
    c(
      "session.json",
      "experts.json",
      "skills.json",
      "connection_refs.json",
      "connection_permissions.json",
      "capability_grants.json",
      "store/resources.json",
      "store/sources.json"
    )
  )

  manifest <- tempest_read_json_strict(
    file.path(bundle_dir, "session.json"),
    what = "session bundle manifest",
    class = tempest_session_persistence_error_class(
      "tempest_session_restore_error"
    )
  )
  if (!identical(as.integer(manifest$schema_version %||% NA_integer_), 4L)) {
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

  citation_audit <- declared_json(
    "artifacts/citation_audit.json",
    what = "citation audit artifact"
  )
  store_artifacts <- list(
    citation_audit = if (is.null(citation_audit)) {
      NULL
    } else {
      tempest_restore_citation_audit(citation_audit)
    },
    references = declared_json(
      "artifacts/references.json",
      what = "references artifact"
    )
  )
  store_artifacts <- store_artifacts[
    !vapply(
      store_artifacts,
      is.null,
      logical(1)
    )
  ]

  snapshot <- list(
    schema_version = manifest$schema_version,
    package_version = manifest$package_version %||% NA_character_,
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
    artifacts = list(
      suggested_questions = optional_json(
        "artifacts/suggested_questions.json",
        what = "suggested questions artifact"
      )
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
    store = list(
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
  snapshot$artifacts <- snapshot$artifacts[
    !vapply(
      snapshot$artifacts,
      is.null,
      logical(1)
    )
  ]

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

#' @keywords internal
tempest_run_bundle_validate_manifest <- function(run_dir, manifest) {
  if (
    !identical(as.integer(manifest$schema_version %||% NA_integer_), 3L) ||
      !identical(manifest$status %||% "", "complete")
  ) {
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
    "artifacts/typed/deliverables.json",
    "artifacts/typed/index.json"
  )
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
      "Manifest omits typed artifact indexes."
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
  store,
  artifact_catalog = tempest_artifact_catalog()
) {
  stopifnot(inherits(store, "SourceStore"))
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
  path_is_declared <- function(path) {
    rel_path <- gsub(
      "\\\\",
      "/",
      as.character(fs::path_rel(path, start = run_dir))
    )
    rel_path %in% declared_files
  }

  if (path_is_declared(paths$sources) && file.exists(paths$sources)) {
    tempest_restore_sources(store, tempest_read_json(paths$sources))
  }
  if (path_is_declared(paths$claims) && file.exists(paths$claims)) {
    tempest_restore_claims(store, tempest_read_json(paths$claims))
    store$set_artifact("claims", store$list_claims())
  }
  if (path_is_declared(paths$references) && file.exists(paths$references)) {
    references <- tempest_read_json(paths$references)
    if (!is.null(references)) {
      store$set_artifact("references", references)
    }
  }
  if (
    path_is_declared(paths$citation_audit) &&
      file.exists(paths$citation_audit)
  ) {
    citation_audit <- tempest_read_json(paths$citation_audit)
    if (!is.null(citation_audit)) {
      store$set_artifact(
        "citation_audit",
        tempest_restore_citation_audit(citation_audit)
      )
    }
  }

  json_artifacts <- c(
    perspectives = "perspectives",
    draft_outline = "draft_outline",
    outline = "outline"
  )
  for (artifact_name in names(json_artifacts)) {
    path <- paths[[artifact_name]]
    if (path_is_declared(path) && file.exists(path)) {
      value <- tempest_read_json(path)
      if (!is.null(value)) {
        store$set_artifact(json_artifacts[[artifact_name]], value)
      }
    }
  }
  if (path_is_declared(paths$experts) && file.exists(paths$experts)) {
    expert_records <- tempest_read_json_strict(
      paths$experts,
      what = "STORM expert profiles",
      class = tempest_persistence_error_class(
        "tempest_run_restore_error"
      )
    )
    experts <- tempest_experts_from_records(
      expert_records,
      what = "STORM expert profiles",
      class = tempest_persistence_error_class(
        "tempest_run_restore_error"
      )
    )
    store$set_artifact("experts", experts)
  }

  text_artifacts <- c(
    lead_section = "lead_section",
    draft_md = "draft_md",
    report_md = "report_md"
  )
  for (artifact_name in names(text_artifacts)) {
    path <- paths[[artifact_name]]
    if (path_is_declared(path) && file.exists(path)) {
      store$set_artifact(
        text_artifacts[[artifact_name]],
        tempest_read_text(path)
      )
    }
  }

  if (!is.null(metadata$title)) {
    store$set_artifact("title", metadata$title)
  }

  completed_stages <- tempest_as_character_vector(metadata$completed_stages)
  restored_catalog <- tempest_artifact_bundle_read(
    run_dir,
    evidence_store = store,
    declared_files = declared_files
  )
  tempest_artifact_catalog_import(artifact_catalog, restored_catalog)

  list(
    metadata = metadata,
    completed_stages = completed_stages,
    artifact_catalog = artifact_catalog
  )
}

#' @keywords internal
tempest_infer_completed_stages <- function(paths) {
  stages <- character()
  if (file.exists(paths$perspectives) && file.exists(paths$experts)) {
    stages <- c(stages, "perspectives")
  }
  if (file.exists(paths$sources) && file.exists(paths$claims)) {
    stages <- c(stages, "research")
  }
  if (file.exists(paths$outline)) {
    stages <- c(stages, "outline")
  }
  if (file.exists(paths$draft_md)) {
    stages <- c(stages, "write")
  }
  if (file.exists(paths$report_md)) {
    stages <- c(stages, "polish")
  }
  stages
}

#' @keywords internal
tempest_save_run_artifacts <- function(
  run_dir,
  store,
  topic,
  title,
  config,
  completed_stages,
  steps,
  research_strategy,
  parallel_writing = FALSE,
  remove_duplicate = FALSE,
  artifact_catalog = tempest_artifact_catalog()
) {
  if (is.null(run_dir)) {
    return(invisible(NULL))
  }
  stopifnot(inherits(store, "SourceStore"))
  if (!inherits(artifact_catalog, "TempestArtifactCatalog")) {
    tempest_abort(
      "{.arg artifact_catalog} must be a TempestArtifactCatalog."
    )
  }
  paths <- tempest_run_artifact_paths(run_dir)
  completed_stages <- unique(tempest_as_character_vector(completed_stages))

  metadata <- list(
    topic = topic,
    title = title,
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

  tempest_write_json(paths$sources, store$list_sources())
  tempest_write_json(
    paths$claims,
    lapply(store$list_claims(), tempest_claim_to_list)
  )
  audit <- store$get_artifact("citation_audit")
  if (!is.null(audit)) {
    tempest_write_json(paths$citation_audit, audit)
  }

  # References are the sources actually cited in the report/draft, not a copy
  # of every collected source.
  cited_md <- store$get_artifact("report_md") %||%
    store$get_artifact("draft_md") %||%
    ""
  cited_ids <- tempest_extract_citation_ids(cited_md)
  references <- Filter(
    Negate(is.null),
    lapply(cited_ids, function(id) store$get_source(id))
  )
  tempest_write_json(paths$references, references)

  json_artifacts <- c(
    perspectives = "perspectives",
    draft_outline = "draft_outline",
    outline = "outline"
  )
  for (path_name in names(json_artifacts)) {
    value <- store$get_artifact(json_artifacts[[path_name]])
    if (!is.null(value)) {
      tempest_write_json(paths[[path_name]], value)
    }
  }
  experts <- store$get_artifact("experts")
  if (!is.null(experts)) {
    tempest_write_json(paths$experts, tempest_expert_records(experts))
  }

  text_artifacts <- c(
    lead_section = "lead_section",
    draft_md = "draft_md",
    report_md = "report_md"
  )
  for (path_name in names(text_artifacts)) {
    value <- store$get_artifact(text_artifacts[[path_name]])
    if (rlang::is_string(value)) {
      tempest_write_text(paths[[path_name]], value)
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
  metadata$schema_version <- 3L
  metadata$status <- "complete"
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
  unique(c(completed_stages, stage))
}
