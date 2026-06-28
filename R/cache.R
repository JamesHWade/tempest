# File-based caching (RDS)
# Provides disk caching for search results and fetched content

#' Get or create cache directory
#'
#' Returns the cache directory path, creating it if necessary.
#' Uses TEMPEST_CACHE_DIR environment variable, or a temp directory.
#'
#' @param cache_dir Optional explicit cache directory path.
#' @return Path to cache directory.
#' @keywords internal
tempest_cache_dir <- function(cache_dir = NULL) {
  cache_dir <- cache_dir %||% Sys.getenv("TEMPEST_CACHE_DIR", unset = "")

  if (identical(cache_dir, "")) {
    cache_dir <- fs::path(tempdir(), "tempest-cache")
  }

  fs::dir_create(cache_dir, recurse = TRUE)
  cache_dir
}

#' Generate cache key from arguments
#'
#' Creates a deterministic hash key from the provided arguments.
#'
#' @param ... Arguments to hash.
#' @param algo Hash algorithm (default: xxhash64).
#' @return Character string cache key.
#' @keywords internal
tempest_cache_key <- function(..., algo = "xxhash64") {
  digest::digest(list(...), algo = algo)
}

#' Get path to cache file
#'
#' @param cache_dir Cache directory path.
#' @param key Cache key.
#' @return Full path to cache file.
#' @keywords internal
tempest_cache_path <- function(cache_dir, key) {
  fs::path(cache_dir, paste0(key, ".rds"))
}

# Normalize a cache age value.
tempest_cache_max_age <- function(max_age = Inf) {
  max_age <- suppressWarnings(as.numeric(max_age))
  if (
    length(max_age) != 1L ||
      is.na(max_age) ||
      max_age < 0
  ) {
    tempest_abort(
      "Cache age (`cache_ttl`/`max_age`) must be a non-negative number or Inf."
    )
  }
  max_age
}

# Check whether a cache file is expired.
tempest_cache_expired <- function(path, max_age = Inf, now = Sys.time()) {
  max_age <- tempest_cache_max_age(max_age)
  if (is.infinite(max_age)) {
    return(FALSE)
  }
  if (max_age <= 0) {
    return(TRUE)
  }
  mtime <- file.info(path)$mtime
  if (is.na(mtime)) {
    return(TRUE)
  }
  age <- as.numeric(difftime(now, mtime, units = "secs"))
  is.na(age) || age > max_age
}

# Look up a cached value and report why it missed.
tempest_cache_lookup <- function(cache_dir, key, max_age = Inf) {
  p <- tempest_cache_path(cache_dir, key)

  if (!fs::file_exists(p)) {
    return(list(value = NULL, status = "miss"))
  }
  if (tempest_cache_expired(p, max_age = max_age)) {
    return(list(value = NULL, status = "expired"))
  }

  tryCatch(
    list(value = readRDS(p), status = "hit"),
    error = function(e) {
      tempest_warn(c(
        "Failed to read from cache.",
        x = "Key: {.val {key}}",
        x = "Error: {conditionMessage(e)}",
        i = "Returning NULL and continuing without cached value."
      ))
      list(value = NULL, status = "read_error")
    }
  )
}

#' Retrieve value from cache
#'
#' Attempts to read a cached value. Returns NULL if the entry is missing, has
#' expired (older than `max_age`), or could not be read. Warns on read errors
#' to help diagnose cache corruption.
#'
#' @param cache_dir Cache directory path.
#' @param key Cache key.
#' @param max_age Maximum cache age in seconds. Entries older than this are
#'   treated as missing. Defaults to `Inf` (no expiry).
#' @return Cached value or NULL.
#' @keywords internal
tempest_cache_get <- function(cache_dir, key, max_age = Inf) {
  tempest_cache_lookup(cache_dir, key, max_age = max_age)$value
}

#' Store value in cache
#'
#' Saves a value to the cache. Creates parent directories if needed.
#' Warns on write errors but doesn't abort.
#'
#' @param cache_dir Cache directory path.
#' @param key Cache key.
#' @param value Value to cache.
#' @return Invisibly returns `TRUE` if the value was written, `FALSE` otherwise.
#' @keywords internal
tempest_cache_set <- function(cache_dir, key, value) {
  p <- tempest_cache_path(cache_dir, key)

  ok <- tryCatch(
    {
      fs::dir_create(fs::path_dir(p), recurse = TRUE)
      saveRDS(value, p)
      TRUE
    },
    error = function(e) {
      tempest_warn(c(
        "Failed to write to cache.",
        x = "Key: {.val {key}}",
        x = "Error: {conditionMessage(e)}",
        i = "Continuing without caching this value."
      ))
      FALSE
    }
  )

  invisible(ok)
}

#' Clear the Tempest cache
#'
#' Removes cached search and fetch results from the specified cache directory.
#' This is useful when a host app wants to force fresh web retrieval without
#' changing the configured cache path.
#'
#' @param cache_dir Cache directory path. If `NULL`, uses
#'   `TEMPEST_CACHE_DIR` or the default Tempest cache under `tempdir()`.
#' @param max_age Optional maximum cache age in seconds. When supplied, only
#'   files older than `max_age` are removed.
#' @return Invisibly returns TRUE.
#' @export
tempest_cache_clear <- function(cache_dir = NULL, max_age = NULL) {
  cache_dir <- tempest_cache_dir(cache_dir)
  if (dir.exists(cache_dir)) {
    files <- list.files(cache_dir, pattern = "\\.rds$", full.names = TRUE)
    if (!is.null(max_age)) {
      files <- files[vapply(
        files,
        tempest_cache_expired,
        logical(1),
        max_age = max_age
      )]
    }
    if (length(files) > 0L) {
      file.remove(files)
      tempest_inform(
        "Cleared {length(files)} cached file{?s} from {.path {cache_dir}}"
      )
    }
  }
  invisible(TRUE)
}
