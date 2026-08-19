#' @keywords internal
tempest_product_write_json <- function(path, x) {
  tempest_require("jsonlite", "STORM run persistence requires jsonlite.")
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  json <- jsonlite::toJSON(
    x,
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null",
    na = "null"
  )
  tempest_atomic_write_lines(json, path)
  invisible(path)
}

# Condition-class hierarchy for Tempest persistence errors. Every persistence
# failure carries the `tempest_persistence_error` and `tempest_error` base
# classes so callers can catch any save/load failure with a single handler.
# Session persistence errors additionally carry `tempest_session_error`, which
# is shared with non-persistence session errors raised by `TempestSession`.
#' @keywords internal
tempest_persistence_error_class <- function(specific = character()) {
  c(specific, "tempest_persistence_error", "tempest_error")
}

#' @keywords internal
tempest_session_persistence_error_class <- function(specific = character()) {
  c(
    specific,
    "tempest_session_error",
    "tempest_persistence_error",
    "tempest_error"
  )
}

#' @keywords internal
tempest_product_unsupported_format_abort <- function(
  what,
  version = NULL,
  class
) {
  suffix <- if (is.null(version)) "" else paste0(": ", version)
  tempest_abort(
    paste0("Unsupported ", what, suffix, "."),
    class = unique(c("tempest_unsupported_format_error", class))
  )
}

tempest_persistence_schema_version <- function(value, what, class) {
  tempest_exact_integer_scalar(
    value,
    what,
    class = class,
    minimum = 0L
  )
}

#' @keywords internal
tempest_persistence_manifest_files <- function(value, what, class) {
  valid <- is.list(value) &&
    !is.data.frame(value) &&
    is.null(names(value)) &&
    length(value) > 0L &&
    all(vapply(
      value,
      function(file) {
        rlang::is_string(file) &&
          !is.na(file) &&
          nzchar(tempest_trim(file))
      },
      logical(1)
    ))
  if (!isTRUE(valid)) {
    tempest_abort(
      "{what} must be an unnamed list of non-empty file paths.",
      class = class
    )
  }
  unname(vapply(value, identity, character(1)))
}

#' @keywords internal
tempest_persistence_manifest_checksums <- function(
  value,
  files,
  what,
  class
) {
  value_names <- names(value)
  valid <- is.list(value) &&
    !is.data.frame(value) &&
    !is.null(value_names) &&
    length(value) > 0L &&
    !anyNA(value_names) &&
    !anyDuplicated(value_names) &&
    all(nzchar(value_names)) &&
    setequal(value_names, files) &&
    all(vapply(
      value,
      function(checksum) {
        rlang::is_string(checksum) &&
          !is.na(checksum) &&
          grepl("^[a-f0-9]{64}$", checksum)
      },
      logical(1)
    ))
  if (!isTRUE(valid)) {
    tempest_abort(
      "{what} must map every declared file to one SHA-256 checksum.",
      class = class
    )
  }
  unlist(value, use.names = TRUE)
}

#' @keywords internal
tempest_persistence_leaf_path_is_symlink <- function(path) {
  expanded <- path.expand(path)
  without_trailing_separator <- sub("[/\\\\]+$", "", expanded)
  if (nzchar(without_trailing_separator)) {
    expanded <- without_trailing_separator
  }
  link_target <- Sys.readlink(expanded)
  !is.na(link_target) && nzchar(link_target)
}

#' @keywords internal
tempest_persistence_bundle_path_has_symlink <- function(bundle_dir, rel_path) {
  parts <- strsplit(gsub("\\\\", "/", rel_path), "/", fixed = TRUE)[[1]]
  current <- bundle_dir
  for (part in parts) {
    current <- file.path(current, part)
    if (nzchar(Sys.readlink(current))) {
      return(TRUE)
    }
  }
  FALSE
}

#' @keywords internal
tempest_persistence_require_regular_bundle_files <- function(
  bundle_dir,
  files,
  what,
  class
) {
  if (!dir.exists(bundle_dir)) {
    tempest_abort("{what} directory does not exist.", class = class)
  }
  files <- unname(as.character(files))
  safe <- length(files) > 0L &&
    !anyNA(files) &&
    all(nzchar(files)) &&
    !anyDuplicated(files) &&
    all(vapply(files, tempest_product_path_is_safe, logical(1)))
  if (!isTRUE(safe)) {
    tempest_abort(
      "{what} contains missing, duplicate, or unsafe file paths.",
      class = class
    )
  }
  root <- normalizePath(bundle_dir, winslash = "/", mustWork = TRUE)
  root_prefix <- paste0(root, "/")
  problems <- vapply(
    files,
    function(file) {
      path <- file.path(bundle_dir, file)
      if (!file.exists(path)) {
        return("missing")
      }
      if (
        !utils::file_test("-f", path) ||
          tempest_persistence_bundle_path_has_symlink(bundle_dir, file)
      ) {
        return("not a regular non-symlink file")
      }
      resolved <- tryCatch(
        normalizePath(path, winslash = "/", mustWork = TRUE),
        error = function(error) ""
      )
      if (!nzchar(resolved) || !startsWith(resolved, root_prefix)) {
        return("resolves outside the bundle")
      }
      ""
    },
    character(1)
  )
  invalid <- names(problems)[nzchar(problems)]
  if (length(invalid) > 0L) {
    details <- paste0(
      invalid,
      " (",
      unname(problems[invalid]),
      ")",
      collapse = ", "
    )
    tempest_abort(
      "{what} failed regular-file validation: {details}.",
      class = class
    )
  }
  invisible(files)
}

#' @keywords internal
tempest_persistence_credential_audit <- function(value, what, class) {
  evidence_path <- function(path) {
    grepl(
      paste0(
        "\\$workspace\\$retrieved_resources\\[\\[[0-9]+\\]\\]",
        "\\$content($|\\$|\\[\\[)"
      ),
      path,
      perl = TRUE
    ) ||
      grepl(
        paste0(
          "\\$workspace\\$retrieved_resources\\[\\[[0-9]+\\]\\]",
          "\\$metadata\\$(snippet|content_text|context_text|",
          "citation_context|answer_context)($|\\$|\\[\\[)"
        ),
        path,
        perl = TRUE
      ) ||
      grepl(
        paste0(
          "\\$workspace\\$proposed_claims\\[\\[[0-9]+\\]\\]",
          "\\$(claim_text|supporting_quotes)($|\\$|\\[\\[)"
        ),
        path,
        perl = TRUE
      ) ||
      grepl(
        paste0(
          "\\$workspace\\$evidence_spans\\[\\[[0-9]+\\]\\]",
          "\\$quote($|\\$|\\[\\[)"
        ),
        path,
        perl = TRUE
      ) ||
      grepl(
        paste0(
          "\\$state\\$references\\[\\[[0-9]+\\]\\]",
          "\\$(snippet|content_text|context_text)($|\\$|\\[\\[)"
        ),
        path,
        perl = TRUE
      ) ||
      grepl(
        paste0(
          "\\$state\\$references\\[\\[[0-9]+\\]\\]\\$meta",
          "\\$(snippet|content_text|context_text|citation_context|",
          "answer_context)($|\\$|\\[\\[)"
        ),
        path,
        perl = TRUE
      )
  }
  scan <- function(item, path) {
    if (evidence_path(path)) {
      return(character())
    }
    if (is.list(item)) {
      item_names <- names(item)
      sensitive_names <- if (is.null(item_names)) {
        character()
      } else {
        item_names[vapply(
          item_names,
          tempest_research_sensitive_name,
          logical(1)
        )]
      }
      found <- if (length(sensitive_names) == 0L) {
        character()
      } else {
        paste0(path, "$", sensitive_names)
      }
      child_paths <- if (is.null(item_names)) {
        paste0(path, "[[", seq_along(item), "]]")
      } else {
        paste0(path, "$", item_names)
      }
      return(c(
        found,
        unlist(Map(scan, item, child_paths), use.names = FALSE)
      ))
    }
    if (tempest_contract_sensitive_scalar(item)) path else character()
  }
  sensitive <- unique(scan(value, "snapshot"))
  if (length(sensitive) > 0L) {
    tempest_abort(
      paste0(
        "Cannot persist ",
        what,
        "; credential-like data appears outside authoritative evidence at ",
        sensitive[[1]],
        "."
      ),
      class = class
    )
  }
  invisible(value)
}

#' @keywords internal
tempest_product_read_json <- function(
  path,
  what = "JSON file",
  class = tempest_persistence_error_class()
) {
  tempest_require("jsonlite", "Tempest persistence requires jsonlite.")
  if (!file.exists(path)) {
    tempest_abort(
      c("Cannot read {what}.", x = "File does not exist: {.path {path}}."),
      class = class
    )
  }
  tryCatch(
    jsonlite::fromJSON(path, simplifyVector = FALSE),
    error = function(e) {
      tempest_abort(
        c(
          "Cannot read {what}.",
          x = "File is not valid JSON: {.path {path}}."
        ),
        class = class,
        parent = e
      )
    }
  )
}

#' @keywords internal
tempest_persistence_exact_records <- function(
  records,
  fields,
  what,
  class
) {
  valid <- is.list(records) &&
    !is.data.frame(records) &&
    is.null(names(records)) &&
    all(vapply(
      records,
      function(record) {
        is.list(record) &&
          !is.data.frame(record) &&
          !is.null(names(record)) &&
          !anyNA(names(record)) &&
          !anyDuplicated(names(record)) &&
          identical(names(record), fields)
      },
      logical(1)
    ))
  if (!isTRUE(valid)) {
    tempest_abort(
      "Cannot restore {what}; records do not match the current schema.",
      class = class
    )
  }
  tryCatch(
    tempest_product_canonical_list(records, what),
    error = function(error) {
      tempest_abort(
        "Cannot restore {what}; records contain non-portable values.",
        class = class,
        parent = error
      )
    }
  )
  records
}

#' @keywords internal
tempest_product_bundle_checksum <- function(bundle_dir, rel_path) {
  digest::digest(
    file.path(bundle_dir, rel_path),
    algo = "sha256",
    file = TRUE,
    serialize = FALSE
  )
}

#' @keywords internal
tempest_product_atomic_commit_bundle <- function(
  staging_dir,
  bundle_dir,
  class,
  what = "bundle"
) {
  backup_dir <- NULL
  if (file.exists(bundle_dir)) {
    backup_dir <- tempfile(
      pattern = paste0(".", basename(bundle_dir), "-backup-"),
      tmpdir = dirname(bundle_dir)
    )
    if (!file.rename(bundle_dir, backup_dir)) {
      tempest_abort(
        "Could not stage the previous {what} for replacement.",
        class = class
      )
    }
  }

  if (!file.rename(staging_dir, bundle_dir)) {
    restored <- is.null(backup_dir) || file.rename(backup_dir, bundle_dir)
    if (!isTRUE(restored)) {
      tempest_abort(
        paste0(
          "Could not install the completed {what}, and rollback of the ",
          "previous bundle also failed."
        ),
        class = class
      )
    }
    tempest_abort(
      "Could not atomically install the completed {what}.",
      class = class
    )
  }
  if (!is.null(backup_dir)) {
    unlink(backup_dir, recursive = TRUE, force = TRUE)
  }
  normalizePath(bundle_dir, winslash = "/", mustWork = TRUE)
}
