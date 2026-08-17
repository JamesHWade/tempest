# Hard-cut persistence for current Tempest promotion bundles

tempest_promotion_persistence_abort <- function(
  message,
  ...,
  .envir = rlang::caller_env()
) {
  tempest_promotion_abort(
    message,
    ...,
    class = c(
      "tempest_promotion_persistence_error",
      "tempest_persistence_error"
    ),
    .envir = .envir
  )
}

tempest_promotion_bundle_files <- function() {
  c("bundle.json", "manifest.json")
}

tempest_promotion_file_checksum <- function(path) {
  digest::digest(
    path,
    algo = "sha256",
    file = TRUE,
    serialize = FALSE
  )
}

tempest_promotion_write_json <- function(path, value) {
  json <- tryCatch(
    jsonlite::toJSON(
      value,
      auto_unbox = TRUE,
      null = "null",
      na = "null",
      digits = NA,
      pretty = TRUE,
      force = TRUE
    ),
    error = function(error) {
      tempest_promotion_persistence_abort(
        "Could not encode a promotion bundle file."
      )
    }
  )
  tempest_atomic_write_lines(json, path)
  invisible(path)
}

tempest_promotion_read_json <- function(path, what) {
  tryCatch(
    jsonlite::fromJSON(path, simplifyVector = FALSE),
    error = function(error) {
      tempest_promotion_persistence_abort(
        "{what} is not valid current-schema JSON."
      )
    }
  )
}

tempest_promotion_symlink <- function(path) {
  target <- Sys.readlink(path)
  !is.na(target) && nzchar(target)
}

tempest_promotion_assert_no_symlink_path <- function(path, what) {
  raw_parts <- strsplit(
    gsub("\\\\", "/", path.expand(path)),
    "/",
    fixed = TRUE
  )[[1L]]
  if (any(raw_parts %in% c(".", ".."))) {
    tempest_promotion_persistence_abort(
      "{what} must not contain dot or parent traversal components."
    )
  }
  cursor <- as.character(fs::path_abs(path))
  platform_aliases <- c("/var" = "/private/var", "/tmp" = "/private/tmp")
  for (alias in names(platform_aliases)) {
    target <- Sys.readlink(alias)
    expected <- sub("^/", "", platform_aliases[[alias]])
    if (
      identical(target, expected) &&
        (identical(cursor, alias) || startsWith(cursor, paste0(alias, "/")))
    ) {
      cursor <- paste0(
        platform_aliases[[alias]],
        substring(cursor, nchar(alias) + 1L)
      )
      break
    }
  }
  paths <- character()
  repeat {
    paths <- c(paths, cursor)
    parent <- as.character(fs::path_dir(cursor))
    if (identical(parent, cursor)) {
      break
    }
    cursor <- parent
  }
  links <- unname(fs::is_link(paths))
  links[is.na(links)] <- FALSE
  if (any(links)) {
    tempest_promotion_persistence_abort(
      "{what} and all of its ancestors must not be symbolic links."
    )
  }
  invisible(path)
}

tempest_promotion_assert_regular_file <- function(path, max_bytes, what) {
  info <- file.info(path)
  if (
    !file.exists(path) ||
      !utils::file_test("-f", path) ||
      tempest_promotion_symlink(path) ||
      is.na(info$size) ||
      info$size < 1 ||
      info$size > max_bytes
  ) {
    tempest_promotion_persistence_abort(
      "{what} is missing, unsafe, empty, or too large."
    )
  }
  invisible(path)
}

tempest_promotion_manifest_core <- function(bundle_id, checksum) {
  list(
    schema_version = tempest_promotion_schema_version,
    bundle_type = "tempest_promotion",
    status = "complete",
    bundle_id = bundle_id,
    files = list("bundle.json"),
    checksums = list(bundle.json = checksum)
  )
}

tempest_promotion_install <- function(staging, path) {
  file.rename(staging, path)
}

tempest_promotion_validate_manifest <- function(manifest) {
  fields <- c(
    "schema_version",
    "bundle_type",
    "status",
    "bundle_id",
    "files",
    "checksums",
    "manifest_digest"
  )
  if (
    !is.list(manifest) ||
      is.data.frame(manifest) ||
      is.null(names(manifest)) ||
      anyNA(names(manifest)) ||
      anyDuplicated(names(manifest)) ||
      !identical(names(manifest), fields)
  ) {
    tempest_promotion_persistence_abort(
      "Promotion manifest fields are malformed or unsupported."
    )
  }
  if (
    !is.integer(manifest$schema_version) ||
      is.object(manifest$schema_version) ||
      !is.null(names(manifest$schema_version)) ||
      length(manifest$schema_version) != 1L ||
      is.na(manifest$schema_version) ||
      !is.finite(manifest$schema_version) ||
      manifest$schema_version != tempest_promotion_schema_version ||
      !rlang::is_string(manifest$bundle_type) ||
      is.na(manifest$bundle_type) ||
      !identical(manifest$bundle_type, "tempest_promotion") ||
      !rlang::is_string(manifest$status) ||
      is.na(manifest$status) ||
      !identical(manifest$status, "complete") ||
      !identical(manifest$files, list("bundle.json")) ||
      !is.list(manifest$checksums) ||
      !identical(names(manifest$checksums), "bundle.json") ||
      !rlang::is_string(manifest$checksums$bundle.json) ||
      is.na(manifest$checksums$bundle.json) ||
      !grepl("^[a-f0-9]{64}$", manifest$checksums$bundle.json) ||
      !rlang::is_string(manifest$bundle_id) ||
      is.na(manifest$bundle_id) ||
      !grepl("^sha256:[a-f0-9]{64}$", manifest$bundle_id) ||
      !rlang::is_string(manifest$manifest_digest) ||
      is.na(manifest$manifest_digest) ||
      !grepl("^sha256:[a-f0-9]{64}$", manifest$manifest_digest)
  ) {
    tempest_promotion_persistence_abort(
      "Promotion manifest values are malformed or unsupported."
    )
  }
  core <- manifest[setdiff(fields, "manifest_digest")]
  if (!identical(manifest$manifest_digest, tempest_promotion_digest(core))) {
    tempest_promotion_persistence_abort(
      "Promotion manifest digest does not match its exact contents."
    )
  }
  manifest
}

#' Save a Tempest promotion bundle atomically
#'
#' The destination must not exist. The current format is a closed two-file
#' directory with an exact inventory, SHA-256 checksums, and a self-bound
#' manifest. No prior promotion format is read or written.
#'
#' @param bundle A [tempest_promotion_bundle()] value.
#' @param path New destination directory.
#' @return The normalized bundle directory, invisibly.
#' @export
tempest_save_promotion_bundle <- function(bundle, path) {
  data <- tempest_promotion_bundle_data(bundle)
  if (!rlang::is_string(path) || is.na(path) || !nzchar(path)) {
    tempest_promotion_persistence_abort(
      "{.arg path} must be one non-empty destination directory."
    )
  }
  path <- path.expand(path)
  tempest_promotion_assert_no_symlink_path(path, "Promotion destination")
  if (file.exists(path) || tempest_promotion_symlink(path)) {
    tempest_promotion_persistence_abort(
      "Promotion destination already exists; overwrite is not supported."
    )
  }
  parent <- dirname(path)
  tempest_promotion_assert_no_symlink_path(
    parent,
    "Promotion destination parent"
  )
  if (
    !dir.exists(parent) ||
      tempest_promotion_symlink(parent) ||
      !utils::file_test("-d", parent)
  ) {
    tempest_promotion_persistence_abort(
      "Promotion destination parent must be an existing regular directory."
    )
  }
  parent <- normalizePath(parent, winslash = "/", mustWork = TRUE)
  path <- file.path(parent, basename(path))
  staging <- tempfile(
    pattern = paste0(".", basename(path), "-stage-"),
    tmpdir = parent
  )
  if (!dir.create(staging, mode = "0700")) {
    tempest_promotion_persistence_abort(
      "Could not create a promotion-bundle staging directory."
    )
  }
  installed <- FALSE
  on.exit(
    {
      if (!installed && dir.exists(staging)) {
        unlink(staging, recursive = TRUE, force = TRUE)
      }
    },
    add = TRUE
  )

  bundle_path <- file.path(staging, "bundle.json")
  tempest_promotion_write_json(bundle_path, data)
  checksum <- tempest_promotion_file_checksum(bundle_path)
  manifest <- tempest_promotion_manifest_core(bundle@bundle_id, checksum)
  manifest$manifest_digest <- tempest_promotion_digest(manifest)
  tempest_promotion_write_json(file.path(staging, "manifest.json"), manifest)
  Sys.chmod(file.path(staging, tempest_promotion_bundle_files()), mode = "0600")
  if (!tempest_promotion_install(staging, path)) {
    tempest_promotion_persistence_abort(
      "Could not atomically install the completed promotion bundle."
    )
  }
  installed <- TRUE
  invisible(normalizePath(path, winslash = "/", mustWork = TRUE))
}

#' Read and validate a current Tempest promotion bundle
#'
#' Bundle-local digests establish internal consistency, not authenticity. The
#' caller must retain the original bundle id through a trusted channel and
#' supply it when reading the persisted directory.
#'
#' @param path Promotion-bundle directory created by
#'   [tempest_save_promotion_bundle()].
#' @param expected_bundle_id Exact SHA-256 bundle id retained independently
#'   from the directory, for example `bundle@bundle_id` from the value passed
#'   to [tempest_save_promotion_bundle()].
#' @return A validated `TempestPromotionBundle`.
#' @export
tempest_read_promotion_bundle <- function(path, expected_bundle_id) {
  if (missing(expected_bundle_id)) {
    tempest_promotion_persistence_abort(
      "{.arg expected_bundle_id} is required as an out-of-band trust pin."
    )
  }
  if (
    !is.character(expected_bundle_id) ||
      length(expected_bundle_id) != 1L ||
      is.na(expected_bundle_id) ||
      !is.null(attributes(expected_bundle_id)) ||
      !grepl("^sha256:[a-f0-9]{64}$", expected_bundle_id)
  ) {
    tempest_promotion_persistence_abort(
      paste0(
        "{.arg expected_bundle_id} must be one exact plain ",
        "{.code sha256:<64 lowercase hexadecimal characters>} string."
      )
    )
  }
  if (!rlang::is_string(path) || is.na(path) || !nzchar(path)) {
    tempest_promotion_persistence_abort(
      "{.arg path} must be one non-empty promotion-bundle directory."
    )
  }
  path <- path.expand(path)
  tempest_promotion_assert_no_symlink_path(path, "Promotion bundle root")
  if (
    !dir.exists(path) ||
      tempest_promotion_symlink(path) ||
      !utils::file_test("-d", path)
  ) {
    tempest_promotion_persistence_abort(
      "Promotion bundle root is missing or unsafe."
    )
  }
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  inventory <- sort(
    list.files(path, all.files = TRUE, no.. = TRUE),
    method = "radix"
  )
  if (!identical(inventory, sort(tempest_promotion_bundle_files()))) {
    tempest_promotion_persistence_abort(
      "Promotion bundle contents do not match the closed current inventory."
    )
  }
  manifest_path <- file.path(path, "manifest.json")
  bundle_path <- file.path(path, "bundle.json")
  tempest_promotion_assert_regular_file(
    manifest_path,
    1024L * 1024L,
    "Promotion manifest"
  )
  tempest_promotion_assert_regular_file(
    bundle_path,
    32L * 1024L * 1024L,
    "Promotion data file"
  )
  manifest <- tempest_promotion_validate_manifest(
    tempest_promotion_read_json(manifest_path, "Promotion manifest")
  )
  if (!identical(manifest$bundle_id, expected_bundle_id)) {
    tempest_promotion_persistence_abort(
      "Promotion manifest does not match the trusted bundle id."
    )
  }
  checksum <- tempest_promotion_file_checksum(bundle_path)
  if (!identical(checksum, manifest$checksums$bundle.json)) {
    tempest_promotion_persistence_abort(
      "Promotion data checksum does not match the manifest."
    )
  }
  data <- tempest_promotion_read_json(bundle_path, "Promotion data file")
  bundle <- tryCatch(
    tempest_promotion_bundle_from_data(data),
    error = function(error) {
      tempest_promotion_persistence_abort(
        "Promotion data failed current-schema validation."
      )
    }
  )
  if (
    !identical(bundle@bundle_id, expected_bundle_id) ||
      !identical(bundle@bundle_id, manifest$bundle_id)
  ) {
    tempest_promotion_persistence_abort(
      "Promotion manifest and data do not match the trusted bundle id."
    )
  }
  bundle
}
