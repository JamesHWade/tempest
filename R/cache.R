# File-based caching (RDS)

#' @keywords internal
stormr_cache_dir <- function(cache_dir = NULL) {
  cache_dir <- cache_dir %||% Sys.getenv("STORMR_CACHE_DIR", unset = "")
  if (identical(cache_dir, "")) {
    cache_dir <- fs::path(tempdir(), "stormr-cache")
  }
  fs::dir_create(cache_dir, recurse = TRUE)
  cache_dir
}

#' @keywords internal
stormr_cache_key <- function(..., algo = "xxhash64") {
  digest::digest(list(...), algo = algo)
}

#' @keywords internal
stormr_cache_path <- function(cache_dir, key) {
  fs::path(cache_dir, paste0(key, ".rds"))
}

#' @keywords internal
stormr_cache_get <- function(cache_dir, key) {
  p <- stormr_cache_path(cache_dir, key)
  if (!fs::file_exists(p)) return(NULL)
  tryCatch(readRDS(p), error = function(e) NULL)
}

#' @keywords internal
stormr_cache_set <- function(cache_dir, key, value) {
  p <- stormr_cache_path(cache_dir, key)
  fs::dir_create(fs::path_dir(p), recurse = TRUE)
  saveRDS(value, p)
  invisible(value)
}
