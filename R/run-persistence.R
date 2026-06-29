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
tempest_session_snapshot <- function(session) {
  stopifnot(inherits(session, "TempestSession"))
  artifacts <- tempest_env_snapshot(session$artifacts)

  list(
    schema_version = 1L,
    topic = session$topic,
    title = session$title,
    session_id = session$session_id,
    personas = session$personas,
    transcript = session$transcript,
    mindmap = session$mindmap,
    artifacts = artifacts,
    suggested_questions = artifacts$suggested_questions %||% character(),
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

#' @keywords internal
tempest_session_restore <- function(
  snapshot,
  config = tempest_config(),
  progress = NULL
) {
  if (!is.list(snapshot)) {
    tempest_session_restore_abort("{.arg snapshot} must be a list.")
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

  tempest_session_restore_expert_sessions(
    session,
    snapshot$expert_sessions %||% list()
  )

  session
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
