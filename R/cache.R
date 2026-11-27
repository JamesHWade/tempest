# File-based caching (RDS)
# Provides disk caching for search results and fetched content

tempest_cache_dir <- function(cache_dir = NULL) {
  cache_dir <- cache_dir %||% Sys.getenv("TEMPEST_CACHE_DIR", unset = "")

  if (identical(cache_dir, "")) {
    cache_dir <- fs::path(tempdir(), "tempest-cache")
  }

  fs::dir_create(cache_dir, recurse = TRUE)
  cache_dir
}

tempest_cache_key <- function(..., algo = "xxhash64") {
  digest::digest(list(...), algo = algo)
}

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
      "Cache age (`cache_ttl`/`max_age`) must be a non-negative number or Inf.",
      class = c("tempest_config_error", "tempest_error")
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
      quarantine <- paste0(
        p,
        ".corrupt-",
        format(Sys.time(), "%Y%m%d%H%M%S", tz = "UTC")
      )
      if (!file.rename(p, quarantine)) {
        unlink(p)
      }
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

tempest_cache_set <- function(cache_dir, key, value, writer = saveRDS) {
  p <- tempest_cache_path(cache_dir, key)

  ok <- tryCatch(
    {
      fs::dir_create(fs::path_dir(p), recurse = TRUE)
      staged <- tempfile(
        pattern = paste0(".", basename(p), "-"),
        tmpdir = fs::path_dir(p)
      )
      on.exit(unlink(staged), add = TRUE)
      writer(value, staged)
      if (!file.rename(staged, p)) {
        tempest_abort(
          "Could not atomically replace cache entry {.path {p}}."
        )
      }
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
