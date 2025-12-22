# stormr utility helpers

#' @keywords internal
stormr_has <- function(pkg) {
  requireNamespace(pkg, quietly = TRUE)
}

#' @keywords internal
stormr_abort <- function(message, ..., .envir = parent.frame()) {
  cli::cli_abort(message, ..., .envir = .envir)
}

#' @keywords internal
stormr_inform <- function(message, ..., .envir = parent.frame()) {
  cli::cli_inform(message, ..., .envir = .envir)
}

#' @keywords internal
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

#' @keywords internal
stormr_now_utc <- function() {
  format(Sys.time(), tz = "UTC", usetz = TRUE)
}

#' @keywords internal
stormr_trim <- function(x) {
  if (length(x) == 0) return(x)
  stringi::stri_trim_both(x)
}

#' @keywords internal
stormr_uuid <- function(prefix = "id") {
  # Cheap, good-enough identifier for local state; not cryptographically secure.
  raw <- paste0(prefix, "-", sprintf("%.0f", runif(1, 1e8, 1e9)), "-", sprintf("%.0f", Sys.time()))
  paste0(prefix, "_", digest::digest(raw, algo = "xxhash64"))
}

#' @keywords internal
stormr_require <- function(pkg, why = NULL) {
  if (!stormr_has(pkg)) {
    msg <- if (is.null(why)) {
      glue::glue("Package {.pkg {pkg}} is required for this feature but is not installed.")
    } else {
      glue::glue("Package {.pkg {pkg}} is required: {why}")
    }
    stormr_abort(msg)
  }
}

#' @keywords internal
stormr_pkg_file <- function(...) {
  system.file(..., package = "stormr")
}

#' @keywords internal
stormr_read_text <- function(path) {
  if (!file.exists(path)) stormr_abort("File does not exist: {.path {path}}")
  paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

#' @keywords internal
stormr_write_text <- function(path, text) {
  dir <- dirname(path)
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  writeLines(text, con = path, useBytes = TRUE)
  invisible(path)
}
