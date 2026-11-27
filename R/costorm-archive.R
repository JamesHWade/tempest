# Bounded ZIP archive packaging, security checks, and extraction for Co-STORM sessions

tempest_session_archive_storage_root <- function(token = "session") {
  token <- gsub("[^A-Za-z0-9_-]", "-", token %||% "session")
  root <- file.path(tempdir(), "tempest-shiny-sessions", token)
  dir.create(root, recursive = TRUE, showWarnings = FALSE, mode = "0700")
  Sys.chmod(root, mode = "0700", use_umask = FALSE)
  normalizePath(root, winslash = "/", mustWork = TRUE)
}

tempest_session_archive_secure_permissions <- function(path) {
  if (!dir.exists(path)) {
    return(invisible(path))
  }
  directories <- unique(c(
    path,
    list.dirs(path, recursive = TRUE, full.names = TRUE)
  ))
  files <- list.files(
    path,
    recursive = TRUE,
    full.names = TRUE,
    all.files = TRUE,
    no.. = TRUE
  )
  Sys.chmod(directories, mode = "0700", use_umask = FALSE)
  if (length(files) > 0L) {
    Sys.chmod(files, mode = "0600", use_umask = FALSE)
  }
  invisible(path)
}

tempest_session_archive_enforce_quota <- function(
  path,
  max_bytes = getOption("tempest.shiny.session_quota_bytes", 100 * 1024^2)
) {
  max_bytes <- tempest_fetch_positive(max_bytes, "max_bytes")
  files <- list.files(
    path,
    recursive = TRUE,
    full.names = TRUE,
    all.files = TRUE,
    no.. = TRUE
  )
  size <- sum(file.info(files)$size, na.rm = TRUE)
  if (size > max_bytes) {
    unlink(path, recursive = TRUE, force = TRUE)
    stop("Session bundle exceeds the configured storage quota.", call. = FALSE)
  }
  invisible(size)
}

tempest_session_archive_listing_is_safe <- function(entries, sizes) {
  entries <- gsub("\\\\", "/", entries)
  valid_sizes <- is.numeric(sizes) &&
    length(sizes) == length(entries) &&
    all(is.finite(sizes)) &&
    all(sizes >= 0)
  unsafe <- grepl("^(/|~|[A-Za-z]:)", entries) |
    vapply(
      strsplit(entries, "/", fixed = TRUE),
      function(parts) {
        any(!nzchar(parts)) || any(parts %in% c(".", ".."))
      },
      logical(1)
    )
  too_large <- !valid_sizes ||
    sum(sizes) > 100 * 1024^2 ||
    any(sizes > 50 * 1024^2)
  length(entries) > 0L &&
    !anyDuplicated(entries) &&
    !any(unsafe) &&
    !too_large
}

tempest_session_archive_write <- function(save_fn, file) {
  bundle_dir <- tempfile("tempest-session-download-")
  on.exit(unlink(bundle_dir, recursive = TRUE, force = TRUE), add = TRUE)
  save_fn(bundle_dir)
  tempest_session_archive_enforce_quota(bundle_dir)
  tempest_session_archive_secure_permissions(bundle_dir)
  files <- list.files(
    bundle_dir,
    all.files = TRUE,
    no.. = TRUE,
    recursive = TRUE,
    include.dirs = FALSE
  )
  zip::zip(
    file,
    files = files,
    include_directories = FALSE,
    root = bundle_dir,
    mode = "mirror"
  )
  invisible(file)
}

tempest_session_archive_extract <- function(
  archive,
  root,
  reader = tempest_costorm_archive_read
) {
  listing <- utils::unzip(archive, list = TRUE)
  entries <- gsub("\\\\", "/", listing$Name)
  if (!tempest_session_archive_listing_is_safe(entries, listing$Length)) {
    stop("Session archive contains unsafe files.", call. = FALSE)
  }
  complete <- FALSE
  on.exit(
    if (!complete && dir.exists(root)) {
      unlink(root, recursive = TRUE, force = TRUE)
    },
    add = TRUE
  )
  dir.create(root, recursive = TRUE, showWarnings = FALSE, mode = "0700")
  utils::unzip(archive, exdir = root)
  bundle <- reader(root)
  tempest_session_archive_secure_permissions(root)
  complete <- TRUE
  bundle
}
