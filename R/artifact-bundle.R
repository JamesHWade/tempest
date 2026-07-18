# Durable bundle storage for typed artifact catalogs

tempest_atomic_write_raw <- function(bytes, path) {
  if (!is.raw(bytes)) {
    tempest_artifact_codec_abort("{.arg bytes} must be a raw vector.")
  }
  dir <- dirname(path)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile(tmpdir = dir, fileext = ".tmp")
  done <- FALSE
  on.exit(if (!done && file.exists(tmp)) unlink(tmp), add = TRUE)
  connection <- file(tmp, open = "wb")
  tryCatch(
    writeBin(bytes, connection),
    finally = close(connection)
  )
  if (!file.rename(tmp, path)) {
    if (!file.copy(tmp, path, overwrite = TRUE)) {
      tempest_artifact_codec_abort(
        "Could not write artifact content {.path {path}}."
      )
    }
    unlink(tmp)
  }
  done <- TRUE
  invisible(path)
}

tempest_artifact_bundle_path_is_safe <- function(path) {
  if (!rlang::is_string(path) || !nzchar(path)) {
    return(FALSE)
  }
  path <- gsub("\\\\", "/", path)
  parts <- strsplit(path, "/", fixed = TRUE)[[1]]
  !grepl("^(/|~|[A-Za-z]:)", path) &&
    length(parts) > 0L &&
    all(nzchar(parts)) &&
    !any(parts %in% c(".", ".."))
}

tempest_artifact_bundle_assert_safe_path <- function(
  bundle_dir,
  rel_path
) {
  if (!tempest_artifact_bundle_path_is_safe(rel_path)) {
    tempest_artifact_codec_abort(
      "Artifact bundle contains an unsafe relative path."
    )
  }
  path <- file.path(bundle_dir, rel_path)
  if (file.exists(path)) {
    root <- paste0(
      normalizePath(bundle_dir, winslash = "/", mustWork = TRUE),
      "/"
    )
    resolved <- normalizePath(path, winslash = "/", mustWork = TRUE)
    if (!startsWith(resolved, root)) {
      tempest_artifact_codec_abort(
        "Artifact content resolves outside its bundle."
      )
    }
  }
  path
}

tempest_artifact_bundle_record_map <- function(value, what) {
  value <- value %||% list()
  if (!is.list(value) || is.data.frame(value)) {
    tempest_artifact_codec_abort(
      "{what} must be a JSON object of named records."
    )
  }
  if (
    length(value) > 0L &&
      (is.null(names(value)) ||
        anyNA(names(value)) ||
        any(!nzchar(names(value))) ||
        anyDuplicated(names(value)))
  ) {
    tempest_artifact_codec_abort(
      "{what} must use unique, non-empty record keys."
    )
  }
  value
}

tempest_artifact_bundle_write <- function(
  catalog,
  bundle_dir,
  prefix = "artifacts/typed"
) {
  if (!inherits(catalog, "TempestArtifactCatalog")) {
    tempest_artifact_codec_abort(
      "{.arg catalog} must be created by {.fn tempest_artifact_catalog}."
    )
  }
  if (!rlang::is_string(bundle_dir)) {
    tempest_artifact_codec_abort(
      "{.arg bundle_dir} must be a single path string."
    )
  }
  prefix <- sub("/+$", "", gsub("\\\\", "/", prefix))
  if (!tempest_artifact_bundle_path_is_safe(prefix)) {
    tempest_artifact_codec_abort(
      "{.arg prefix} must be a safe relative path."
    )
  }
  dir.create(bundle_dir, recursive = TRUE, showWarnings = FALSE)
  deliverables_path <- paste0(prefix, "/deliverables.json")
  index_path <- paste0(prefix, "/index.json")
  files <- character()
  content_files <- character()

  deliverables <- catalog$list_deliverables()
  tempest_write_json(
    file.path(bundle_dir, deliverables_path),
    list(schema_version = 1L, deliverables = deliverables)
  )
  files <- c(files, deliverables_path)

  metadata <- catalog$list()
  artifact_records <- stats::setNames(
    lapply(names(metadata), function(artifact_id) {
      artifact <- catalog$get(artifact_id)
      record <- tempest_artifact_record(
        artifact,
        include_content = FALSE
      )
      if (is.null(artifact@content)) {
        record$codec_id <- "tempest.external.reference"
        record$codec_version <- "1"
        record$content_path <- NULL
        record$byte_size <- 0L
        record$content_sha256 <- artifact@checksum
        return(record)
      }
      encoded <- tempest_artifact_codec_encode(
        artifact@content,
        artifact@media_type
      )
      if (!identical(encoded$sha256, artifact@checksum)) {
        tempest_artifact_codec_abort(
          "Artifact {.val {artifact_id}} checksum does not match its encoded content."
        )
      }
      content_name <- paste0(
        digest::digest(
          enc2utf8(artifact_id),
          algo = "sha256",
          serialize = FALSE
        ),
        ".",
        encoded$extension
      )
      content_path <- paste0(prefix, "/content/", content_name)
      tempest_atomic_write_raw(
        encoded$bytes,
        file.path(bundle_dir, content_path)
      )
      content_files <<- c(content_files, content_path)
      record$codec_id <- encoded$codec_id
      record$codec_version <- encoded$codec_version
      record$content_path <- content_path
      record$byte_size <- encoded$byte_size
      record$content_sha256 <- encoded$sha256
      record
    }),
    names(metadata)
  )
  tempest_write_json(
    file.path(bundle_dir, index_path),
    list(schema_version = 1L, artifacts = artifact_records)
  )
  files <- c(files, index_path, content_files)
  list(
    files = sort(unique(files)),
    content_files = sort(unique(content_files)),
    deliverables_path = deliverables_path,
    index_path = index_path
  )
}

tempest_artifact_bundle_read <- function(
  bundle_dir,
  prefix = "artifacts/typed",
  store = NULL,
  evidence_store = NULL,
  declared_files = NULL
) {
  prefix <- sub("/+$", "", gsub("\\\\", "/", prefix))
  deliverables_path <- paste0(prefix, "/deliverables.json")
  index_path <- paste0(prefix, "/index.json")
  for (rel_path in c(deliverables_path, index_path)) {
    path <- tempest_artifact_bundle_assert_safe_path(bundle_dir, rel_path)
    if (!file.exists(path)) {
      tempest_artifact_codec_abort(
        "Artifact bundle is missing required file {.path {rel_path}}."
      )
    }
  }
  deliverable_index <- tempest_read_json_strict(
    file.path(bundle_dir, deliverables_path),
    what = "artifact deliverable index",
    class = c(
      "tempest_artifact_codec_error",
      "tempest_persistence_error",
      "tempest_error"
    )
  )
  artifact_index <- tempest_read_json_strict(
    file.path(bundle_dir, index_path),
    what = "typed artifact index",
    class = c(
      "tempest_artifact_codec_error",
      "tempest_persistence_error",
      "tempest_error"
    )
  )
  if (
    !identical(
      as.integer(deliverable_index$schema_version %||% NA_integer_),
      1L
    ) ||
      !identical(
        as.integer(artifact_index$schema_version %||% NA_integer_),
        1L
      )
  ) {
    tempest_artifact_codec_abort(
      "Unsupported typed artifact bundle schema."
    )
  }
  deliverable_records <- tempest_artifact_bundle_record_map(
    deliverable_index$deliverables,
    "The deliverable index"
  )
  deliverables <- lapply(
    deliverable_records,
    tempest_deliverable_spec_from_data
  )
  for (i in seq_along(deliverables)) {
    expected_key <- tempest_deliverable_catalog_key(
      deliverables[[i]]@deliverable_id,
      deliverables[[i]]@version
    )
    if (!identical(names(deliverables)[[i]], expected_key)) {
      tempest_artifact_codec_abort(
        "Deliverable index key does not match specification identity."
      )
    }
  }
  catalog <- tempest_artifact_catalog(
    store = store,
    deliverables = deliverables
  )
  records <- tempest_artifact_bundle_record_map(
    artifact_index$artifacts,
    "The artifact index"
  )
  content_paths <- vapply(
    records,
    function(record) {
      record$content_path %||% NA_character_
    },
    character(1)
  )
  declared_content_paths <- content_paths[!is.na(content_paths)]
  if (
    anyDuplicated(declared_content_paths) ||
      any(
        !vapply(
          declared_content_paths,
          tempest_artifact_bundle_path_is_safe,
          logical(1)
        )
      )
  ) {
    tempest_artifact_codec_abort(
      "Typed artifact index contains duplicate or unsafe content paths."
    )
  }
  if (
    !is.null(declared_files) &&
      length(
        setdiff(
          c(deliverables_path, index_path, declared_content_paths),
          declared_files
        )
      ) >
        0L
  ) {
    tempest_artifact_codec_abort(
      "Typed artifact index references undeclared bundle files."
    )
  }

  for (artifact_id in names(records)) {
    record <- records[[artifact_id]]
    if (!identical(record$artifact_id, artifact_id)) {
      tempest_artifact_codec_abort(
        "Typed artifact index key does not match artifact identity."
      )
    }
    deliverable <- catalog$get_deliverable(
      record$deliverable_id,
      record$deliverable_version
    )
    content <- NULL
    if (identical(record$codec_id, "tempest.external.reference")) {
      if (
        !identical(record$codec_version, "1") ||
          !is.null(record$content_path)
      ) {
        tempest_artifact_codec_abort(
          "Malformed external artifact reference record."
        )
      }
    } else {
      content_path <- record$content_path %||% NULL
      path <- tempest_artifact_bundle_assert_safe_path(
        bundle_dir,
        content_path
      )
      if (!file.exists(path)) {
        tempest_artifact_codec_abort(
          "Artifact content file is missing: {.path {content_path}}."
        )
      }
      size <- file.info(path)$size
      connection <- file(path, open = "rb")
      bytes <- tryCatch(
        readBin(connection, what = "raw", n = size),
        finally = close(connection)
      )
      content <- tempest_artifact_codec_decode(
        list(
          codec_id = record$codec_id,
          codec_version = record$codec_version,
          byte_size = record$byte_size,
          sha256 = record$content_sha256
        ),
        bytes
      )
    }
    artifact <- tempest_artifact_from_data(
      record,
      deliverable,
      content = content
    )
    catalog$add(artifact, persist = FALSE)
  }
  tempest_artifact_catalog_validate_lineage(catalog, evidence_store)
  catalog
}
