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
    models = config$models,
    search_provider = config$search_provider,
    max_search_results = config$max_search_results,
    max_search_queries_per_turn = config$max_search_queries_per_turn,
    retrieve_top_k = config$retrieve_top_k,
    max_sources = config$max_sources
  )

  tempest_write_json(paths$sources, store$list_sources())
  tempest_write_json(
    paths$claims,
    lapply(store$list_claims(), tempest_claim_to_list)
  )

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
