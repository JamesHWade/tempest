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
    personas = file.path(run_dir, "personas.json"),
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

#' @keywords internal
tempest_read_json_strict <- function(
  path,
  what = "JSON file",
  class = "tempest_persistence_error"
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
    schema_version = 1L,
    sources = store$list_sources(),
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
    class = "tempest_source_store_restore_error"
  )
}

#' @keywords internal
tempest_source_store_restore <- function(snapshot, store = SourceStore$new()) {
  if (!is.list(snapshot)) {
    tempest_abort(
      "{.arg snapshot} must be a list.",
      class = "tempest_source_store_restore_error"
    )
  }
  if (!inherits(store, "SourceStore")) {
    tempest_abort(
      "{.arg store} must be a SourceStore.",
      class = "tempest_source_store_restore_error"
    )
  }

  tempest_restore_sources(store, snapshot$sources %||% list())

  source_ids <- purrr::map_chr(store$list_sources(), "id")
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
    class = "tempest_session_restore_error"
  )
}

#' @keywords internal
tempest_expert_sessions_snapshot <- function(session) {
  manager <- session$expert_session_manager
  if (is.null(manager)) {
    return(list())
  }

  session_ids <- sort(manager$list_sessions())
  personas <- session$personas %||% list()
  persona_ids <- purrr::map_chr(
    personas,
    ~ as.character(.x$id %||% NA_integer_)
  )
  persona_names <- purrr::map_chr(personas, ~ .x$name %||% NA_character_)

  lapply(session_ids, function(session_id) {
    provenance <- if (
      exists(session_id, envir = manager$session_provenance, inherits = FALSE)
    ) {
      get(session_id, envir = manager$session_provenance, inherits = FALSE)
    } else {
      NULL
    }
    base <- if (is.environment(provenance)) {
      provenance$base %||% list()
    } else {
      list()
    }
    persona_id <- as.character(base$persona_id %||% NA_character_)
    persona_name <- NA_character_
    persona_idx <- match(persona_id, persona_ids)
    if (!is.na(persona_idx)) {
      persona_name <- persona_names[[persona_idx]]
    }
    list(
      session_id = session_id,
      persona_id = persona_id,
      persona_name = persona_name
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
#' personas, transcript, mind map, report artifacts, progress-event history,
#' expert-session metadata, and the underlying `SourceStore` ledger. Live chat
#' handles, tools, Shiny reactive state, credentials, and provider request
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
      class = c(
        "tempest_session_snapshot_error",
        "tempest_session_error",
        "tempest_error"
      )
    )
  }
  artifacts <- tempest_env_snapshot(session$artifacts)

  list(
    schema_version = 1L,
    package_version = tryCatch(
      as.character(utils::packageVersion("tempest")),
      error = function(e) NA_character_
    ),
    topic = session$topic,
    title = session$title,
    session_id = session$session_id,
    config = tempest_config_snapshot(session$config),
    personas = session$personas,
    transcript = session$transcript,
    mindmap = session$mindmap,
    artifacts = artifacts,
    suggested_questions = artifacts$suggested_questions %||% character(),
    progress_events = artifacts$progress_events %||% list(),
    store = tempest_source_store_snapshot(session$store),
    expert_sessions = tempest_expert_sessions_snapshot(session)
  )
}

#' @keywords internal
tempest_session_persona_for_expert_session <- function(
  session,
  expert_session
) {
  personas <- session$personas %||% list()
  persona_id <- expert_session$persona_id %||% NA_character_
  persona_id <- as.character(persona_id)
  persona_id <- if (length(persona_id) == 0) NA_character_ else persona_id[[1]]
  if (!is.na(persona_id) && nzchar(persona_id)) {
    ids <- purrr::map_chr(personas, ~ as.character(.x$id %||% NA_integer_))
    idx <- match(persona_id, ids)
    if (!is.na(idx)) {
      return(personas[[idx]])
    }
  }

  persona_name <- expert_session$persona_name %||% NA_character_
  if (rlang::is_string(persona_name) && !is.na(persona_name)) {
    idx <- session$find_expert_index(persona_name)
    if (!is.null(idx)) {
      return(personas[[idx]])
    }
  }

  NULL
}

#' @keywords internal
tempest_session_restore_expert_sessions <- function(session, expert_sessions) {
  for (expert_session in expert_sessions %||% list()) {
    if (!is.list(expert_session) || is.null(expert_session$session_id)) {
      next
    }
    persona <- tempest_session_persona_for_expert_session(
      session,
      expert_session
    )
    if (is.null(persona)) {
      tempest_session_restore_abort(
        paste0(
          "Expert session ",
          expert_session$session_id,
          " does not match a restored persona."
        )
      )
    }
    session$expert_session_manager$get_or_create(
      persona,
      session_id = expert_session$session_id
    )
  }
  invisible(session)
}

#' Restore a Co-STORM session from a snapshot
#'
#' `r lifecycle::badge("experimental")`
#'
#' `tempest_session_restore()` rebuilds a [TempestSession] from a structured
#' snapshot created by [tempest_session_snapshot()] or read by
#' [tempest_session_resume()]. It restores durable research state and creates
#' fresh chat/tool handles using the supplied runtime `config`.
#'
#' Historical progress events are restored as session artifact data and can be
#' reduced with [tempest_progress_state()]. They are not replayed into the new
#' `progress` callback; future calls on the restored session use that callback.
#'
#' @param snapshot A list from [tempest_session_snapshot()].
#' @param config Runtime [TempestConfig] used to recreate chats, retrievers, and
#'   tools. Functions, credentials, and host-specific stores should be supplied
#'   here rather than read from the snapshot.
#' @param progress Optional callback for future `tempest_progress_event`
#'   objects.
#' @return A restored [TempestSession].
#' @export
tempest_session_restore <- function(
  snapshot,
  config = tempest_config(),
  progress = NULL
) {
  if (!is.list(snapshot)) {
    tempest_session_restore_abort("{.arg snapshot} must be a list.")
  }
  schema_version <- snapshot$schema_version %||% NA_integer_
  if (!identical(as.integer(schema_version), 1L)) {
    tempest_session_restore_abort(
      paste0("Unsupported snapshot schema version: ", schema_version, ".")
    )
  }
  if (
    !rlang::is_string(snapshot$topic) || !nzchar(tempest_trim(snapshot$topic))
  ) {
    tempest_session_restore_abort("Snapshot must include a non-empty topic.")
  }

  store <- tempest_source_store_restore(snapshot$store %||% list())
  retriever <- tempest_retriever(config = config, store = store)
  session <- tempest_session(
    topic = snapshot$topic,
    config = config,
    personas = snapshot$personas %||% list(),
    retriever = retriever,
    progress = NULL
  )

  session$title <- snapshot$title %||% session$topic
  session$session_id <- snapshot$session_id %||% session$session_id
  session$progress <- tempest_progress_callback(progress)
  session$expert_session_manager$run_id <- session$session_id
  session$expert_session_manager$progress <- session$progress
  session$transcript <- snapshot$transcript %||% list()
  session$mindmap <- snapshot$mindmap %||% tempest_mindmap_init(session$topic)
  session$artifacts <- new.env(parent = emptyenv())

  artifacts <- snapshot$artifacts %||% list()
  for (name in names(artifacts)) {
    session$artifacts[[name]] <- artifacts[[name]]
  }
  if (
    !is.null(snapshot$suggested_questions) &&
      is.null(session$artifacts[["suggested_questions"]])
  ) {
    session$artifacts[["suggested_questions"]] <- snapshot$suggested_questions
  }
  if (
    !is.null(snapshot$progress_events) &&
      is.null(session$artifacts[["progress_events"]])
  ) {
    session$artifacts[["progress_events"]] <- snapshot$progress_events
  }

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
  tempest_write_json(tempest_session_bundle_path(path, rel_path), value)
  rel_path
}

#' @keywords internal
tempest_session_bundle_write_text <- function(path, rel_path, value) {
  if (!rlang::is_string(value)) {
    return(character())
  }
  tempest_write_text(tempest_session_bundle_path(path, rel_path), value)
  rel_path
}

#' @keywords internal
tempest_session_prepare_bundle_dir <- function(path, overwrite = FALSE) {
  if (!rlang::is_string(path) || !nzchar(tempest_trim(path))) {
    tempest_abort(
      "{.arg path} must be a single non-empty path string.",
      class = "tempest_session_save_error"
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
        class = "tempest_session_save_error"
      )
    }
    entries <- list.files(bundle_dir, all.files = TRUE, no.. = TRUE)
    if (length(entries) > 0 && !isTRUE(overwrite)) {
      tempest_abort(
        c(
          "Session bundle directory already exists.",
          i = "Use {.code overwrite = TRUE} to replace it.",
          x = "Path: {.path {bundle_dir}}."
        ),
        class = "tempest_session_save_error"
      )
    }
    if (isTRUE(overwrite)) {
      unlink(bundle_dir, recursive = TRUE, force = TRUE)
    }
  }

  dir.create(bundle_dir, recursive = TRUE, showWarnings = FALSE)
  normalizePath(bundle_dir, winslash = "/", mustWork = TRUE)
}

#' Save a Co-STORM session bundle
#'
#' `r lifecycle::badge("experimental")`
#'
#' `tempest_session_save()` writes a schema-versioned directory bundle for a
#' [TempestSession]. The bundle stores durable research state as JSON and
#' Markdown files and writes the `session.json` manifest last. Live chat
#' handles, registered tool closures, Shiny reactive state, credentials, and
#' raw provider request bodies are not serialized.
#'
#' Use [tempest_session_resume()] to load the bundle with a fresh runtime
#' [TempestConfig].
#'
#' @param session A [TempestSession] object.
#' @param path Directory where the session bundle should be written.
#' @param overwrite If `TRUE`, replace an existing bundle directory.
#' @return Invisibly returns the normalized bundle directory.
#' @export
tempest_session_save <- function(session, path, overwrite = FALSE) {
  if (!inherits(session, "TempestSession")) {
    tempest_abort(
      "{.arg session} must be a {.cls TempestSession} object.",
      class = c(
        "tempest_session_save_error",
        "tempest_session_error",
        "tempest_error"
      )
    )
  }
  tempest_require("jsonlite", "Tempest session persistence requires jsonlite.")

  bundle_dir <- tempest_session_prepare_bundle_dir(path, overwrite = overwrite)
  snapshot <- tempest_session_snapshot(session)
  files <- character()

  files <- c(
    files,
    tempest_session_bundle_write_json(
      bundle_dir,
      "config.json",
      snapshot$config
    ),
    tempest_session_bundle_write_json(
      bundle_dir,
      "personas.json",
      snapshot$personas
    ),
    tempest_session_bundle_write_json(
      bundle_dir,
      "expert_sessions.json",
      snapshot$expert_sessions
    ),
    tempest_session_bundle_write_json(
      bundle_dir,
      "transcript.json",
      snapshot$transcript
    ),
    tempest_session_bundle_write_json(
      bundle_dir,
      "mindmap.json",
      snapshot$mindmap
    ),
    tempest_session_bundle_write_json(
      bundle_dir,
      "progress_events.json",
      snapshot$progress_events
    ),
    tempest_session_bundle_write_json(
      bundle_dir,
      "store/sources.json",
      snapshot$store$sources
    ),
    tempest_session_bundle_write_json(
      bundle_dir,
      "store/claims.json",
      snapshot$store$claims
    ),
    tempest_session_bundle_write_json(
      bundle_dir,
      "store/evidence_spans.json",
      snapshot$store$evidence_spans
    ),
    tempest_session_bundle_write_json(
      bundle_dir,
      "store/disputes.json",
      snapshot$store$disputes
    )
  )

  files <- c(
    files,
    tempest_session_bundle_write_text(
      bundle_dir,
      "artifacts/report.md",
      snapshot$artifacts$report_md
    ),
    tempest_session_bundle_write_text(
      bundle_dir,
      "artifacts/report_body.md",
      snapshot$artifacts$report
    ),
    tempest_session_bundle_write_text(
      bundle_dir,
      "artifacts/mindmap.md",
      snapshot$artifacts$mindmap_md
    )
  )

  if (!is.null(snapshot$artifacts$suggested_questions)) {
    files <- c(
      files,
      tempest_session_bundle_write_json(
        bundle_dir,
        "artifacts/suggested_questions.json",
        snapshot$artifacts$suggested_questions
      )
    )
  }
  if (!is.null(snapshot$store$artifacts$citation_audit)) {
    files <- c(
      files,
      tempest_session_bundle_write_json(
        bundle_dir,
        "artifacts/citation_audit.json",
        snapshot$store$artifacts$citation_audit
      )
    )
  }
  if (!is.null(snapshot$store$artifacts$references)) {
    files <- c(
      files,
      tempest_session_bundle_write_json(
        bundle_dir,
        "artifacts/references.json",
        snapshot$store$artifacts$references
      )
    )
  }

  manifest <- list(
    schema_version = snapshot$schema_version,
    package_version = snapshot$package_version,
    session_id = snapshot$session_id,
    topic = snapshot$topic,
    title = snapshot$title,
    saved_at = tempest_now_utc(),
    status = "complete",
    files = sort(unique(files))
  )
  tempest_session_bundle_write_json(bundle_dir, "session.json", manifest)

  invisible(bundle_dir)
}

#' @keywords internal
tempest_session_bundle_optional_json <- function(path, default = NULL, what) {
  if (!file.exists(path)) {
    return(default)
  }
  tempest_read_json_strict(
    path,
    what = what,
    class = "tempest_session_restore_error"
  )
}

#' @keywords internal
tempest_session_bundle_optional_text <- function(path, default = NULL, what) {
  if (!file.exists(path)) {
    return(default)
  }
  tryCatch(
    tempest_read_text(path),
    error = function(e) {
      tempest_abort(
        c("Cannot read {what}.", x = "Path: {.path {path}}."),
        class = "tempest_session_restore_error",
        parent = e
      )
    }
  )
}

#' @keywords internal
tempest_session_restore_progress_events <- function(events) {
  lapply(events %||% list(), function(event) {
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
      class = "tempest_session_restore_error"
    )
  }
}

#' Resume a saved Co-STORM session bundle
#'
#' `tempest_session_resume()` reads a directory bundle written by
#' [tempest_session_save()] and rebuilds a [TempestSession] with a fresh runtime
#' [TempestConfig]. Historical progress events are loaded for display and
#' reduction, but they are not replayed into `progress`.
#'
#' @param path Directory containing a session bundle.
#' @param config Runtime [TempestConfig] used to recreate chats, retrievers, and
#'   tools.
#' @param progress Optional callback for future `tempest_progress_event`
#'   objects.
#' @return A restored [TempestSession].
#' @export
tempest_session_resume <- function(
  path,
  config = tempest_config(),
  progress = NULL
) {
  if (!rlang::is_string(path) || !nzchar(tempest_trim(path))) {
    tempest_abort(
      "{.arg path} must be a single non-empty path string.",
      class = "tempest_session_restore_error"
    )
  }
  bundle_dir <- normalizePath(
    path.expand(path),
    winslash = "/",
    mustWork = FALSE
  )
  tempest_session_bundle_require_files(
    bundle_dir,
    c("session.json", "personas.json", "store/sources.json")
  )

  manifest <- tempest_read_json_strict(
    file.path(bundle_dir, "session.json"),
    what = "session bundle manifest",
    class = "tempest_session_restore_error"
  )
  if (!identical(as.integer(manifest$schema_version %||% NA_integer_), 1L)) {
    tempest_session_restore_abort(
      paste0(
        "Unsupported session bundle schema version: ",
        manifest$schema_version,
        "."
      )
    )
  }

  citation_audit <- tempest_session_bundle_optional_json(
    file.path(bundle_dir, "artifacts/citation_audit.json"),
    what = "citation audit artifact"
  )
  store_artifacts <- list(
    citation_audit = if (is.null(citation_audit)) {
      NULL
    } else {
      tempest_restore_citation_audit(citation_audit)
    },
    references = tempest_session_bundle_optional_json(
      file.path(bundle_dir, "artifacts/references.json"),
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
    config = tempest_session_bundle_optional_json(
      file.path(bundle_dir, "config.json"),
      default = list(),
      what = "session config summary"
    ),
    personas = tempest_read_json_strict(
      file.path(bundle_dir, "personas.json"),
      what = "session personas",
      class = "tempest_session_restore_error"
    ),
    transcript = tempest_session_bundle_optional_json(
      file.path(bundle_dir, "transcript.json"),
      default = list(),
      what = "session transcript"
    ),
    mindmap = tempest_session_bundle_optional_json(
      file.path(bundle_dir, "mindmap.json"),
      default = NULL,
      what = "session mind map"
    ),
    artifacts = list(
      report_md = tempest_session_bundle_optional_text(
        file.path(bundle_dir, "artifacts/report.md"),
        what = "report artifact"
      ),
      report = tempest_session_bundle_optional_text(
        file.path(bundle_dir, "artifacts/report_body.md"),
        what = "report body artifact"
      ),
      mindmap_md = tempest_session_bundle_optional_text(
        file.path(bundle_dir, "artifacts/mindmap.md"),
        what = "mind map artifact"
      ),
      suggested_questions = tempest_session_bundle_optional_json(
        file.path(bundle_dir, "artifacts/suggested_questions.json"),
        what = "suggested questions artifact"
      )
    ),
    suggested_questions = tempest_session_bundle_optional_json(
      file.path(bundle_dir, "artifacts/suggested_questions.json"),
      default = character(),
      what = "suggested questions artifact"
    ),
    progress_events = tempest_session_restore_progress_events(
      tempest_session_bundle_optional_json(
        file.path(bundle_dir, "progress_events.json"),
        default = list(),
        what = "progress-event history"
      )
    ),
    store = list(
      schema_version = 1L,
      sources = tempest_read_json_strict(
        file.path(bundle_dir, "store/sources.json"),
        what = "session source ledger",
        class = "tempest_session_restore_error"
      ),
      claims = tempest_session_bundle_optional_json(
        file.path(bundle_dir, "store/claims.json"),
        default = list(),
        what = "session claim ledger"
      ),
      evidence_spans = tempest_session_bundle_optional_json(
        file.path(bundle_dir, "store/evidence_spans.json"),
        default = list(),
        what = "session evidence-span ledger"
      ),
      disputes = tempest_session_bundle_optional_json(
        file.path(bundle_dir, "store/disputes.json"),
        default = list(),
        what = "session dispute ledger"
      ),
      artifacts = store_artifacts
    ),
    expert_sessions = tempest_session_bundle_optional_json(
      file.path(bundle_dir, "expert_sessions.json"),
      default = list(),
      what = "expert-session metadata"
    )
  )
  snapshot$artifacts <- snapshot$artifacts[
    !vapply(
      snapshot$artifacts,
      is.null,
      logical(1)
    )
  ]

  tempest_session_restore(snapshot, config = config, progress = progress)
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
tempest_load_run_artifacts <- function(run_dir, store) {
  stopifnot(inherits(store, "SourceStore"))
  paths <- tempest_run_artifact_paths(run_dir)
  metadata <- if (file.exists(paths$run_config)) {
    tempest_read_json(paths$run_config) %||% list()
  } else {
    list()
  }

  if (file.exists(paths$sources)) {
    tempest_restore_sources(store, tempest_read_json(paths$sources))
  }
  if (file.exists(paths$claims)) {
    tempest_restore_claims(store, tempest_read_json(paths$claims))
    store$set_artifact("claims", store$list_claims())
  }
  if (file.exists(paths$references)) {
    references <- tempest_read_json(paths$references)
    if (!is.null(references)) {
      store$set_artifact("references", references)
    }
  }
  if (file.exists(paths$citation_audit)) {
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
    personas = "personas",
    draft_outline = "draft_outline",
    outline = "outline"
  )
  for (artifact_name in names(json_artifacts)) {
    path <- paths[[artifact_name]]
    if (file.exists(path)) {
      value <- tempest_read_json(path)
      if (!is.null(value)) {
        store$set_artifact(json_artifacts[[artifact_name]], value)
      }
    }
  }

  text_artifacts <- c(
    lead_section = "lead_section",
    draft_md = "draft_md",
    report_md = "report_md"
  )
  for (artifact_name in names(text_artifacts)) {
    path <- paths[[artifact_name]]
    if (file.exists(path)) {
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
  if (length(completed_stages) == 0) {
    completed_stages <- tempest_infer_completed_stages(paths)
  }

  list(
    metadata = metadata,
    completed_stages = completed_stages
  )
}

#' @keywords internal
tempest_infer_completed_stages <- function(paths) {
  stages <- character()
  if (file.exists(paths$perspectives) && file.exists(paths$personas)) {
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
  remove_duplicate = FALSE
) {
  if (is.null(run_dir)) {
    return(invisible(NULL))
  }
  stopifnot(inherits(store, "SourceStore"))
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
    personas = "personas",
    draft_outline = "draft_outline",
    outline = "outline"
  )
  for (path_name in names(json_artifacts)) {
    value <- store$get_artifact(json_artifacts[[path_name]])
    if (!is.null(value)) {
      tempest_write_json(paths[[path_name]], value)
    }
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
