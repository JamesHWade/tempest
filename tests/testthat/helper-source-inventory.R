test_source_inventory_normalize <- function(path) {
  if (
    !is.character(path) ||
      length(path) != 1L ||
      is.na(path) ||
      !nzchar(trimws(path))
  ) {
    return(NULL)
  }
  normalizePath(path.expand(path), winslash = "/", mustWork = FALSE)
}

test_source_inventory_source_root_valid <- function(path) {
  root <- test_source_inventory_normalize(path)
  if (is.null(root) || !dir.exists(root)) {
    return(FALSE)
  }
  description <- file.path(root, "DESCRIPTION")
  r_dir <- file.path(root, "R")
  if (!file.exists(description) || !dir.exists(r_dir)) {
    return(FALSE)
  }
  package <- tryCatch(
    suppressWarnings(read.dcf(description, fields = "Package")[[1L]]),
    error = function(error) NA_character_
  )
  identical(package, "tempest") &&
    length(list.files(r_dir, pattern = "[.]R$")) > 0L
}

test_source_inventory_ancestors <- function(path) {
  current <- test_source_inventory_normalize(path)
  if (is.null(current)) {
    return(character())
  }
  ancestors <- character()
  repeat {
    ancestors <- c(ancestors, current)
    parent <- dirname(current)
    if (identical(parent, current)) {
      break
    }
    current <- parent
  }
  unique(ancestors)
}

test_source_inventory_context <- function() {
  explicit <- Sys.getenv("TEMPEST_SOURCE_ROOT", unset = "")
  if (nzchar(trimws(explicit))) {
    if (!test_source_inventory_source_root_valid(explicit)) {
      stop(
        "TEMPEST_SOURCE_ROOT must identify a tempest source checkout.",
        call. = FALSE
      )
    }
    return(list(
      mode = "source",
      origin = "TEMPEST_SOURCE_ROOT",
      root = test_source_inventory_normalize(explicit)
    ))
  }

  workspace <- Sys.getenv("GITHUB_WORKSPACE", unset = "")
  if (test_source_inventory_source_root_valid(workspace)) {
    return(list(
      mode = "source",
      origin = "GITHUB_WORKSPACE",
      root = test_source_inventory_normalize(workspace)
    ))
  }

  test_root <- tryCatch(
    testthat::test_path("..", ".."),
    error = function(error) NULL
  )
  ancestors <- unique(c(
    test_source_inventory_ancestors(getwd()),
    test_source_inventory_ancestors(test_root)
  ))
  candidates <- unique(c(
    file.path(ancestors, "00_pkg_src", "tempest"),
    ancestors
  ))
  valid <- candidates[vapply(
    candidates,
    test_source_inventory_source_root_valid,
    logical(1)
  )]
  if (length(valid) > 0L) {
    return(list(
      mode = "source",
      origin = "local",
      root = test_source_inventory_normalize(valid[[1L]])
    ))
  }

  installed_root <- system.file(package = "tempest")
  description <- system.file("DESCRIPTION", package = "tempest")
  if (
    !nzchar(installed_root) ||
      !dir.exists(installed_root) ||
      !nzchar(description) ||
      !file.exists(description)
  ) {
    stop(
      "Source inventory requires a checkout or installed tempest package.",
      call. = FALSE
    )
  }
  package <- tryCatch(
    suppressWarnings(read.dcf(description, fields = "Package")[[1L]]),
    error = function(error) NA_character_
  )
  if (!identical(package, "tempest")) {
    stop(
      "Source inventory requires a checkout or installed tempest package.",
      call. = FALSE
    )
  }
  list(
    mode = "installed",
    origin = "installed",
    root = normalizePath(installed_root, winslash = "/", mustWork = TRUE)
  )
}

test_source_inventory_definitions <- function(context) {
  if (!identical(context$mode, "source")) {
    stop(
      "Source definitions require a source inventory context.",
      call. = FALSE
    )
  }
  r_dir <- file.path(context$root, "R")
  r_files <- list.files(r_dir, pattern = "[.]R$", full.names = TRUE)
  definitions <- do.call(
    rbind,
    lapply(r_files, function(path) {
      expressions <- parse(path)
      definition_names <- vapply(
        expressions,
        function(expression) {
          if (
            is.call(expression) &&
              identical(expression[[1L]], as.name("<-")) &&
              is.name(expression[[2L]]) &&
              is.call(expression[[3L]]) &&
              identical(expression[[3L]][[1L]], as.name("function"))
          ) {
            return(as.character(expression[[2L]]))
          }
          NA_character_
        },
        character(1)
      )
      count <- sum(!is.na(definition_names))
      data.frame(
        name = definition_names[!is.na(definition_names)],
        owner = rep(basename(path), count),
        stringsAsFactors = FALSE
      )
    })
  )
  if (!is.data.frame(definitions) || nrow(definitions) == 0L) {
    stop("The tempest source definition inventory is empty.", call. = FALSE)
  }
  definitions
}

test_source_inventory_description <- function(context) {
  path <- if (identical(context$mode, "source")) {
    file.path(context$root, "DESCRIPTION")
  } else {
    system.file("DESCRIPTION", package = "tempest")
  }
  if (!nzchar(path) || !file.exists(path)) {
    stop("The tempest DESCRIPTION file is unavailable.", call. = FALSE)
  }
  path
}

test_source_inventory_namespace_functions <- function(pattern) {
  namespace <- asNamespace("tempest")
  bindings <- grep(pattern, ls(namespace, all.names = TRUE), value = TRUE)
  bindings[vapply(
    bindings,
    function(name) is.function(get(name, envir = namespace, inherits = FALSE)),
    logical(1)
  )]
}
