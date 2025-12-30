# File-based caching (RDS)
# Provides disk caching for search results and fetched content

#' Get or create cache directory
#'
#' Returns the cache directory path, creating it if necessary.
#' Uses STORMR_CACHE_DIR environment variable, or a temp directory.
#'
#' @param cache_dir Optional explicit cache directory path.
#' @return Path to cache directory.
#' @keywords internal
stormr_cache_dir <- function(cache_dir = NULL) {
  cache_dir <- cache_dir %||% Sys.getenv("STORMR_CACHE_DIR", unset = "")

  if (identical(cache_dir, "")) {
    cache_dir <- fs::path(tempdir(), "stormr-cache")
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
stormr_cache_key <- function(..., algo = "xxhash64") {
  digest::digest(list(...), algo = algo)
}

#' Get path to cache file
#'
#' @param cache_dir Cache directory path.
#' @param key Cache key.
#' @return Full path to cache file.
#' @keywords internal
stormr_cache_path <- function(cache_dir, key) {
  fs::path(cache_dir, paste0(key, ".rds"))
}

#' Retrieve value from cache
#'
#' Attempts to read a cached value. Returns NULL if not found or on error.
#' Warns on read errors to help diagnose cache corruption.
#'
#' @param cache_dir Cache directory path.
#' @param key Cache key.
#' @return Cached value or NULL.
#' @keywords internal
stormr_cache_get <- function(cache_dir, key) {
  p <- stormr_cache_path(cache_dir, key)

  if (!fs::file_exists(p)) {
    return(NULL)
  }

  tryCatch(
    readRDS(p),
    error = function(e) {
      stormr_warn(c(
        "Failed to read from cache.",
        x = "Key: {.val {key}}",
        x = "Error: {conditionMessage(e)}",
        i = "Returning NULL and continuing without cached value."
      ))
      NULL
    }
  )
}

#' Store value in cache
#'
#' Saves a value to the cache. Creates parent directories if needed.
#' Warns on write errors but doesn't abort.
#'
#' @param cache_dir Cache directory path.
#' @param key Cache key.
#' @param value Value to cache.
#' @return Invisibly returns the value.
#' @keywords internal
stormr_cache_set <- function(cache_dir, key, value) {
  p <- stormr_cache_path(cache_dir, key)

  tryCatch(
    {
      fs::dir_create(fs::path_dir(p), recurse = TRUE)
      saveRDS(value, p)
    },
    error = function(e) {
      stormr_warn(c(
        "Failed to write to cache.",
        x = "Key: {.val {key}}",
        x = "Error: {conditionMessage(e)}",
        i = "Continuing without caching this value."
      ))
    }
  )

  invisible(value)
}

#' Clear cache directory
#'
#' Removes all cached files from the specified directory.
#'
#' @param cache_dir Cache directory path.
#' @return Invisibly returns TRUE.
#' @keywords internal
stormr_cache_clear <- function(cache_dir) {
  if (dir.exists(cache_dir)) {
    files <- list.files(cache_dir, pattern = "\\.rds$", full.names = TRUE)
    if (length(files) > 0L) {
      file.remove(files)
      stormr_inform("Cleared {length(files)} cached file{?s} from {.path {cache_dir}}")
    }
  }
  invisible(TRUE)
}
