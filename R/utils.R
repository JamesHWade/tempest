# tempest utility helpers
# Uses explicit namespacing for clarity and traceability

#' Check if a package is available
#'
#' @param pkg Package name as string.
#' @return Logical indicating if package is installed.
#' @keywords internal
tempest_has <- function(pkg) {
  if (!rlang::is_string(pkg)) {
    tempest_abort(
      "{.arg pkg} must be a single string, not {.obj_type_friendly {pkg}}."
    )
  }
  rlang::is_installed(pkg)
}

#' Abort with a formatted error message
#'
#' Wrapper around [cli::cli_abort()] for consistent error formatting.
#'
#' @param message Error message with cli formatting.
#' @param ... Additional arguments passed to [cli::cli_abort()].
#' @param .envir Environment for glue interpolation.
#' @return Never returns; always throws an error.
#' @keywords internal
tempest_abort <- function(message, ..., .envir = rlang::caller_env()) {
  cli::cli_abort(message, ..., .envir = .envir, call = rlang::caller_env())
}

#' Inform user with a formatted message
#'
#' Wrapper around [cli::cli_inform()] for consistent messaging.
#'
#' @param message Message with cli formatting.
#' @param ... Additional arguments passed to [cli::cli_inform()].
#' @param .envir Environment for glue interpolation.
#' @keywords internal
tempest_inform <- function(message, ..., .envir = rlang::caller_env()) {
  cli::cli_inform(message, ..., .envir = .envir)
}

#' Warn user with a formatted message
#'
#' Wrapper around [cli::cli_warn()] for consistent warning formatting.
#'
#' @param message Warning message with cli formatting.
#' @param ... Additional arguments passed to [cli::cli_warn()].
#' @param .envir Environment for glue interpolation.
#' @keywords internal
tempest_warn <- function(message, ..., .envir = rlang::caller_env()) {
  cli::cli_warn(message, ..., .envir = .envir)
}

#' Get current UTC timestamp
#'
#' @return Character string with UTC timestamp.
#' @keywords internal
tempest_now_utc <- function() {
  format(Sys.time(), tz = "UTC", usetz = TRUE)
}

#' Trim whitespace from strings
#'
#' @param x Character vector.
#' @return Trimmed character vector.
#' @keywords internal
tempest_trim <- function(x) {
  if (length(x) == 0L) {
    return(x)
  }
  stringi::stri_trim_both(x)
}

#' Generate a unique identifier
#'
#' Creates a prefixed identifier using xxhash64. Not cryptographically secure,
#' but sufficient for local session state tracking.
#'
#' @param prefix Prefix string for the identifier.
#' @return Character string identifier.
#' @keywords internal
tempest_uuid <- function(prefix = "id") {
  raw <- paste0(
    prefix,
    "-",
    sprintf("%.0f", runif(1, 1e8, 1e9)),
    "-",
    sprintf("%.0f", Sys.time())
  )
  paste0(prefix, "_", digest::digest(raw, algo = "xxhash64"))
}

#' Require a package or abort
#'
#' Checks if a package is installed and aborts with a helpful message if not.
#'
#' @param pkg Package name.
#' @param why Optional reason explaining why the package is needed.
#' @keywords internal
tempest_require <- function(pkg, why = NULL) {
  if (!rlang::is_string(pkg)) {
    tempest_abort(
      "{.arg pkg} must be a single string, not {.obj_type_friendly {pkg}}."
    )
  }

  if (!tempest_has(pkg)) {
    msg <- if (is.null(why)) {
      c(
        "Package {.pkg {pkg}} is required but not installed.",
        i = "Install it with: {.run install.packages(\"{pkg}\")}"
      )
    } else {
      c(
        "Package {.pkg {pkg}} is required: {why}",
        i = "Install it with: {.run install.packages(\"{pkg}\")}"
      )
    }
    tempest_abort(
      msg,
      class = c("tempest_missing_package_error", "tempest_error"),
      package = pkg,
      reason = why
    )
  }
  invisible(TRUE)
}

#' Get path to package file
#'
#' @param ... Path components relative to package root.
#' @return Full path to file.
#' @keywords internal
tempest_pkg_file <- function(...) {
  system.file(..., package = "tempest")
}

#' Read text file contents
#'
#' @param path Path to file.
#' @return File contents as single string.
#' @keywords internal
tempest_read_text <- function(path) {
  if (!rlang::is_string(path)) {
    tempest_abort(
      "{.arg path} must be a single string, not {.obj_type_friendly {path}}."
    )
  }

  if (!file.exists(path)) {
    tempest_abort("File does not exist: {.path {path}}")
  }
  paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

#' Atomically write lines to a file
#'
#' Writes to a temporary file in the same directory and then renames it into
#' place. An interrupted or failed write therefore cannot truncate or corrupt
#' an existing file: the destination is only replaced once the new content is
#' fully on disk.
#'
#' @param lines Character vector of lines to write.
#' @param path Destination path.
#' @return Invisibly returns the path.
#' @keywords internal
tempest_atomic_write_lines <- function(lines, path) {
  dir <- dirname(path)
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }
  tmp <- tempfile(tmpdir = dir, fileext = ".tmp")
  done <- FALSE
  on.exit(if (!done && file.exists(tmp)) unlink(tmp), add = TRUE)
  writeLines(lines, con = tmp, useBytes = TRUE)
  if (!file.rename(tmp, path)) {
    # Rename can fail across devices or on locked files; fall back to copy.
    if (!file.copy(tmp, path, overwrite = TRUE)) {
      tempest_abort("Could not write file: {.path {path}}")
    }
    unlink(tmp)
  }
  done <- TRUE
  invisible(path)
}

#' Write text to file
#'
#' Creates parent directories if needed. The write is atomic: content is
#' written to a temporary file and renamed into place.
#'
#' @param path Path to file.
#' @param text Text content to write.
#' @return Invisibly returns the path.
#' @keywords internal
tempest_write_text <- function(path, text) {
  if (!rlang::is_string(path)) {
    tempest_abort(
      "{.arg path} must be a single string, not {.obj_type_friendly {path}}."
    )
  }
  if (!rlang::is_string(text)) {
    tempest_abort(
      "{.arg text} must be a single string, not {.obj_type_friendly {text}}."
    )
  }

  tempest_atomic_write_lines(text, path)
  invisible(path)
}
