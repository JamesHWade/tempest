# STORM run persistence

#' @keywords internal
tempest_storm_topic_slug <- function(topic, max_chars = 80) {
  slug <- tolower(tempest_trim(topic))
  slug <- gsub("[^a-z0-9]+", "-", slug)
  slug <- gsub("^-+|-+$", "", slug)
  if (is.na(slug) || !nzchar(slug)) {
    slug <- "run"
  }
  substr(slug, 1, max_chars)
}

#' @keywords internal
tempest_storm_prepare_run_dir <- function(output_dir, topic, run_id = NULL) {
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
  run_name <- tempest_storm_topic_slug(run_id %||% topic)
  run_dir <- file.path(dir, run_name)
  dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)
  normalizePath(run_dir, winslash = "/", mustWork = TRUE)
}

#' @keywords internal
tempest_storm_artifact_paths <- function(run_dir) {
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
tempest_storm_bundle_write_json <- function(path, rel_path, value) {
  tryCatch(
    tempest_product_write_json(file.path(path, rel_path), value),
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
  rel_path
}

#' @keywords internal
tempest_storm_bundle_write_text <- function(path, rel_path, value) {
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
  rel_path
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
    tempest_product_report_inline_citations(cited_md)
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
tempest_storm_bundle_manifest_fields <- function() {
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
tempest_storm_bundle_owned_files <- function(include_manifest = FALSE) {
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
    tempest_product_unsupported_format_abort(
      "STORM bundle format",
      schema_version,
      tempest_persistence_error_class("tempest_run_restore_error")
    )
  }
  schema_version
}

#' @keywords internal
tempest_storm_bundle_validate_manifest <- function(run_dir, manifest) {
  schema_version <- tempest_storm_require_current_schema(manifest)
  manifest_fields <- names(manifest)
  if (!identical(manifest_fields, tempest_storm_bundle_manifest_fields())) {
    tempest_product_unsupported_format_abort(
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
    tempest_storm_bundle_owned_files(),
    if (pinned) character() else snapshot_path
  )
  stage_required <- tempest_storm_stage_required_files(
    completed_stages
  )
  unsafe <- !vapply(
    normalized,
    tempest_product_path_is_safe,
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
          tempest_product_bundle_checksum(run_dir, file),
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
    tempest_product_authority_validate_stage_records(
      manifest,
      state$stage_records,
      expert_ids = vapply(
        state$experts,
        \(expert) expert@expert_id,
        character(1)
      )
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
      tempest_product_authority_validate_report(
        manifest,
        metadata$report_reference,
        state$report_md
      )
      tempest_product_report_validate_policy(
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
  tryCatch(
    tempest_product_authority_validate(
      manifest,
      state$stage_records,
      workspace,
      report_md = state$report_md,
      report_reference = metadata$report_reference,
      config = config,
      experts = state$experts,
      product_state = state,
      require_publishable = identical(manifest@status, "succeeded")
    ),
    error = function(error) {
      tempest_storm_run_restore_abort(
        "The persisted STORM product lacks exact durable execution authority.",
        parent = error
      )
    }
  )
  manifest
}

#' @keywords internal
tempest_storm_read_state <- function(
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
    tempest_product_read_json(
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
    expert_records <- tempest_product_read_json(
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
tempest_storm_load_artifacts <- function(
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
  paths <- tempest_storm_artifact_paths(run_dir)
  tempest_persistence_require_regular_bundle_files(
    run_dir,
    "run_config.json",
    "STORM root manifest",
    tempest_persistence_error_class("tempest_run_restore_error")
  )
  metadata <- tempest_product_read_json(
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
  declared_files <- tempest_storm_bundle_validate_manifest(run_dir, metadata)
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
    graft_snapshot = graft_snapshot
  )
  manifest_workspace_identity <- tempest_storm_workspace_identity_record(
    workspace
  )
  workspace_snapshot <- tempest_product_read_json(
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
    run_id = run_id
  )

  workspace <- tempest_storm_assert_workspace_equivalent(
    supplied_workspace,
    workspace
  )
  workspace_state <- tempest_research_workspace_mutation_state(workspace)
  if (identical(research_manifest@status, "succeeded")) {
    if (identical(workspace_state, "open")) {
      tryCatch(
        tempest_research_workspace_seal(workspace),
        error = function(error) {
          tempest_storm_run_restore_abort(
            "The succeeded STORM workspace could not be sealed.",
            parent = error
          )
        }
      )
    } else if (!identical(workspace_state, "sealed")) {
      tempest_storm_run_restore_abort(
        "A succeeded STORM workspace must restore into sealed state."
      )
    }
  } else if (!identical(workspace_state, "open")) {
    tempest_storm_run_restore_abort(
      "A partial STORM workspace must restore into mutable open state."
    )
  }

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
tempest_storm_save_artifacts <- function(
  run_dir,
  workspace,
  state,
  research_manifest,
  program_set = NULL,
  config,
  steps
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
    tempest_product_report_for_stage_records(
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
  cited_md <- tempest_product_report_inline_citations(
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
  live_trace_types <- vapply(
    research_manifest@traces,
    \(trace) trace$trace_type %||% NA_character_,
    character(1)
  )
  if (anyNA(live_trace_types)) {
    tempest_abort(
      "{.arg research_manifest} contains an untyped execution trace.",
      class = tempest_persistence_error_class(
        "tempest_run_persistence_error"
      )
    )
  }
  deputy_traces <- research_manifest@traces[
    live_trace_types %in% c("deputy_run", "deputy_delegation")
  ]
  expert_ids <- vapply(
    state$experts,
    \(expert) expert@expert_id,
    character(1)
  )
  research_manifest <- tryCatch(
    tempest_product_authority_bind_stage_records(
      research_manifest,
      durable_records,
      deputy_traces = deputy_traces,
      expert_ids = expert_ids
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
    tempest_product_authority_validate_stage_records(
      research_manifest,
      state$stage_records,
      deputy_traces = deputy_traces,
      expert_ids = expert_ids
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
    tempest_product_report_validate_policy(
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
    tempest_product_authority_bind_report(
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
    tempest_product_authority_validate(
      research_manifest,
      durable_records,
      workspace,
      report_md = state$report_md,
      report_reference = tempest_product_report_reference(state$report_md),
      config = config,
      experts = state$experts,
      product_state = state,
      require_publishable = identical(
        research_manifest@status,
        "succeeded"
      )
    ),
    error = function(error) {
      tempest_abort(
        "The STORM product does not have exact durable execution authority.",
        class = tempest_persistence_error_class(
          "tempest_run_persistence_error"
        ),
        parent = error
      )
    }
  )
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
    tempest_storm_bundle_owned_files(include_manifest = TRUE)
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
    existing_manifest <- tempest_product_read_json(
      file.path(run_dir, "run_config.json"),
      what = "existing STORM run manifest",
      class = tempest_persistence_error_class(
        "tempest_run_persistence_error"
      )
    )
    tryCatch(
      tempest_storm_bundle_validate_manifest(run_dir, existing_manifest),
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
  paths <- tempest_storm_artifact_paths(staging_dir)
  files <- character()
  files <- c(
    files,
    tempest_storm_bundle_write_json(
      staging_dir,
      "workspace.json",
      workspace_record
    ),
    tempest_storm_bundle_write_json(
      staging_dir,
      "references.json",
      state$references
    ),
    tempest_storm_bundle_write_json(
      staging_dir,
      "stage_records.json",
      tempest_stage_records_data(durable_records)
    ),
    tempest_storm_bundle_write_json(
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
        tempest_storm_bundle_write_json(
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
        tempest_storm_bundle_write_text(
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
      \(file) tempest_product_bundle_checksum(staging_dir, file)
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
    report_reference = tempest_product_report_reference(state$report_md),
    workspace = tempest_storm_workspace_identity_record(workspace),
    files = as.list(unname(files)),
    checksums = checksums
  )
  tempest_storm_bundle_write_json(
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
      tempest_storm_bundle_validate_manifest(staging_dir, metadata)
      tempest_storm_load_artifacts(
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
  tempest_product_atomic_commit_bundle(
    staging_dir,
    run_dir,
    class = tempest_persistence_error_class(
      "tempest_run_persistence_error"
    ),
    what = "STORM bundle"
  )
  invisible(research_manifest)
}
