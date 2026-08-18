test_that("the loaded namespace hard-cuts the complete generic kernel", {
  frozen <- readLines(
    test_path("fixtures", "generic-kernel-exports-0.1.0.txt"),
    warn = FALSE
  )
  internal_symbols <- c("tempest_run_restore", "tempest_run_resume")
  public_symbols <- setdiff(frozen, internal_symbols)
  exports <- getNamespaceExports("tempest")
  namespace <- asNamespace("tempest")
  expected_class <- "tempest_generic_kernel_cutover_error"
  expected_message <- paste0(
    "Tempest 0.2 supports only the STORM and Co-STORM product APIs; ",
    "the experimental generic kernel is unavailable."
  )
  capture_error <- function(fn) {
    tryCatch(
      {
        do.call(fn, list())
        NULL
      },
      error = identity
    )
  }
  expect_cutover <- function(fn, symbol) {
    error <- capture_error(fn)
    actual_class <- class(error)[[1]]

    expect_identical(
      actual_class,
      expected_class,
      info = paste("cutover class for", symbol)
    )
    if (!identical(actual_class, expected_class)) {
      return(invisible(NULL))
    }
    expect_identical(
      conditionMessage(error),
      expected_message,
      info = paste("cutover message for", symbol)
    )
    expect_identical(
      error$symbol,
      symbol,
      info = paste("cutover symbol metadata for", symbol)
    )
  }

  expect_length(frozen, 43L)
  expect_length(public_symbols, 41L)
  expect_identical(
    tempest:::tempest_generic_kernel_exports,
    public_symbols
  )
  expect_length(exports, 103L)
  expect_setequal(intersect(exports, public_symbols), public_symbols)
  expect_identical(intersect(exports, internal_symbols), character())

  for (symbol in public_symbols) {
    expect_cutover(namespace[[symbol]], symbol)
  }
  for (symbol in internal_symbols) {
    expect_cutover(namespace[[symbol]], symbol)
  }
})

test_that("retired Co-STORM generic helpers are hard-cut and uncalled", {
  namespace <- asNamespace("tempest")
  retired <- c(
    "tempest_costorm_artifact_catalog",
    "tempest_costorm_report_plan",
    "tempest_create_expert_delegation_tool"
  )
  for (symbol in retired) {
    error <- tryCatch(
      {
        do.call(namespace[[symbol]], list())
        NULL
      },
      error = identity
    )
    expect_s3_class(error, "tempest_generic_kernel_cutover_error")
    expect_identical(error$symbol, symbol)
  }

  source_file <- tryCatch(
    getSrcFilename(namespace[["tempest_runtime"]], full.names = TRUE),
    error = function(error) ""
  )
  if (!rlang::is_string(source_file) || is.na(source_file)) {
    source_file <- ""
  }
  candidates <- unique(c(
    test_path("..", ".."),
    test_path("..", "..", "00_pkg_src", "tempest"),
    if (nzchar(source_file)) dirname(dirname(source_file)) else character(),
    Sys.getenv("GITHUB_WORKSPACE")
  ))
  candidates <- candidates[nzchar(candidates)]
  has_sources <- vapply(
    candidates,
    \(candidate) file.exists(file.path(candidate, "R", "runtime.R")),
    logical(1)
  )
  root <- if (any(has_sources)) {
    normalizePath(candidates[has_sources][[1L]], winslash = "/")
  } else {
    NULL
  }

  if (!is.null(root)) {
    runtime <- paste(
      readLines(file.path(root, "R", "runtime.R"), warn = FALSE),
      collapse = "\n"
    )
    deliverables <- paste(
      readLines(file.path(root, "R", "deliverables.R"), warn = FALSE),
      collapse = "\n"
    )
    product_sources <- paste(
      unlist(lapply(
        list.files(file.path(root, "R"), pattern = "[.]R$", full.names = TRUE),
        readLines,
        warn = FALSE
      )),
      collapse = "\n"
    )
    expect_no_match(runtime, "tempest.expert.delegate", fixed = TRUE)
    expect_no_match(
      runtime,
      "tempest_costorm_last_deputy_execution",
      fixed = TRUE
    )
    expect_identical(
      stringi::stri_count_fixed(
        deliverables,
        "tempest_costorm_artifact_catalog <- function"
      ),
      1L
    )
    expect_identical(
      stringi::stri_count_fixed(
        deliverables,
        "tempest_costorm_report_plan <- function"
      ),
      1L
    )
    expect_identical(
      stringi::stri_count_fixed(
        product_sources,
        "tempest_costorm_artifact_catalog("
      ),
      0L
    )
    expect_identical(
      stringi::stri_count_fixed(
        product_sources,
        "tempest_costorm_report_plan("
      ),
      0L
    )
    expect_identical(
      stringi::stri_count_fixed(
        product_sources,
        "tempest_create_expert_delegation_tool("
      ),
      0L
    )
  } else {
    expression_strings <- function(node) {
      if (is.character(node)) {
        return(node)
      }
      if (is.call(node) || is.pairlist(node) || is.expression(node)) {
        return(unlist(
          lapply(as.list(node), expression_strings),
          use.names = FALSE
        ))
      }
      character()
    }
    bindings <- mget(
      ls(namespace, all.names = TRUE),
      namespace,
      inherits = FALSE
    )
    functions <- Filter(is.function, bindings)
    calls <- unique(unlist(
      lapply(functions, function(fn) {
        c(
          all.names(formals(fn), functions = TRUE, unique = TRUE),
          all.names(body(fn), functions = TRUE, unique = TRUE)
        )
      }),
      use.names = FALSE
    ))
    strings <- unique(unlist(
      lapply(functions, function(fn) {
        c(expression_strings(formals(fn)), expression_strings(body(fn)))
      }),
      use.names = FALSE
    ))

    expect_disjoint(strings, "tempest.expert.delegate")
    expect_disjoint(
      unique(c(calls, strings)),
      "tempest_costorm_last_deputy_execution"
    )
    expect_all_true(vapply(
      retired[1:2],
      \(symbol) is.function(namespace[[symbol]]),
      logical(1)
    ))
    expect_disjoint(calls, retired)
  }
})

test_that("retained product code has no generic-kernel symbols", {
  namespace <- asNamespace("tempest")
  deletion_owned <- c(
    "R/artifact-bundle.R",
    "R/artifact-catalog.R",
    "R/artifact-codecs.R",
    "R/builtin-workflows.R",
    "R/capabilities.R",
    "R/deliverables.R",
    "R/expert-types.R",
    "R/generic-kernel-cutover.R",
    "R/operation-registry.R",
    "R/package-lifecycle.R",
    "R/run-accessors.R",
    "R/runtime.R",
    "R/tempest-run.R",
    "R/workflow-spec.R",
    "R/workflow-types.R"
  )
  frozen_definitions <- readLines(
    test_path("fixtures", "generic-kernel-definitions-0.1.0.txt"),
    warn = FALSE
  )
  expect_length(frozen_definitions, 280L)
  expect_identical(
    frozen_definitions,
    sort(unique(frozen_definitions), method = "radix")
  )
  top_level_definitions <- function(expression) {
    if (!is.call(expression)) {
      return(character())
    }
    operator <- if (is.symbol(expression[[1L]])) {
      as.character(expression[[1L]])
    } else {
      ""
    }
    if (operator %in% c("<-", "=", "<<-")) {
      lhs <- expression[[2L]]
      nested <- expression[[3L]]
      return(c(
        if (is.symbol(lhs)) as.character(lhs) else character(),
        if (
          is.call(nested) &&
            is.symbol(nested[[1L]]) &&
            as.character(nested[[1L]]) %in% c("<-", "=", "<<-")
        ) {
          top_level_definitions(nested)
        } else {
          character()
        }
      ))
    }
    if (operator %in% c("{", "if")) {
      return(unlist(
        lapply(as.list(expression)[-1L], top_level_definitions),
        use.names = FALSE
      ))
    }
    character()
  }
  expression_strings <- function(node) {
    if (is.character(node)) {
      return(node)
    }
    if (is.call(node) || is.pairlist(node) || is.expression(node)) {
      return(unlist(
        lapply(as.list(node), expression_strings),
        use.names = FALSE
      ))
    }
    character()
  }
  source_file <- tryCatch(
    getSrcFilename(namespace[["tempest_runtime"]], full.names = TRUE),
    error = function(error) ""
  )
  if (!rlang::is_string(source_file) || is.na(source_file)) {
    source_file <- ""
  }
  candidates <- unique(c(
    test_path("..", ".."),
    test_path("..", "..", "00_pkg_src", "tempest"),
    if (nzchar(source_file)) dirname(dirname(source_file)) else character(),
    Sys.getenv("GITHUB_WORKSPACE")
  ))
  candidates <- candidates[nzchar(candidates)]
  has_sources <- vapply(
    candidates,
    \(candidate) file.exists(file.path(candidate, "R", "runtime.R")),
    logical(1)
  )
  root <- if (any(has_sources)) {
    normalizePath(candidates[has_sources][[1L]], winslash = "/")
  } else {
    NULL
  }
  legacy_tokens <- c(
    "TempestRun",
    "TempestRuntime",
    "TempestArtifact",
    "ExpertSessionManager",
    "artifact_store",
    "workflow_run",
    "connection_permissions"
  )

  if (!is.null(root)) {
    files <- c(
      list.files(
        file.path(root, "R"),
        pattern = "[.]R$",
        full.names = TRUE
      ),
      list.files(
        file.path(root, "inst", "shiny"),
        pattern = "[.]R$",
        recursive = TRUE,
        full.names = TRUE
      ),
      list.files(
        file.path(root, "inst", "examples"),
        pattern = "[.]R$",
        recursive = TRUE,
        full.names = TRUE
      )
    )
    relative <- substring(
      normalizePath(files, winslash = "/"),
      nchar(root) + 2L
    )
    retained <- files[!relative %in% deletion_owned]
    retained_paths <- relative[!relative %in% deletion_owned]
    deleted_symbols <- sort(
      unique(unlist(
        lapply(
          file.path(root, deletion_owned),
          function(file) {
            unlist(
              lapply(parse(file), top_level_definitions),
              use.names = FALSE
            )
          }
        ),
        use.names = FALSE
      )),
      method = "radix"
    )
    expect_identical(deleted_symbols, frozen_definitions)
    offenders <- unlist(
      Map(
        function(file, path) {
          symbols <- all.names(
            parse(file),
            functions = TRUE,
            unique = TRUE
          )
          symbols <- sort(intersect(symbols, deleted_symbols))
          if (length(symbols) == 0L) {
            return(character())
          }
          paste(path, symbols, sep = "::")
        },
        retained,
        retained_paths
      ),
      use.names = FALSE
    )
    legacy_offenders <- unlist(
      Map(
        function(file, path) {
          contents <- paste(readLines(file, warn = FALSE), collapse = "\n")
          matched <- legacy_tokens[vapply(
            legacy_tokens,
            \(token) grepl(token, contents, fixed = TRUE),
            logical(1)
          )]
          if (length(matched) == 0L) {
            return(character())
          }
          paste(path, matched, sep = "::")
        },
        retained,
        retained_paths
      ),
      use.names = FALSE
    )
    legacy_allowlist <- "R/execution-events.R::TempestRun"
  } else {
    deleted_symbols <- frozen_definitions
    retained_names <- setdiff(
      ls(namespace, all.names = TRUE),
      deleted_symbols
    )
    retained_bindings <- mget(
      retained_names,
      namespace,
      inherits = FALSE
    )
    retained_functions <- Filter(is.function, retained_bindings)
    function_symbols <- lapply(retained_functions, function(fn) {
      unique(c(
        all.names(formals(fn), functions = TRUE, unique = TRUE),
        all.names(body(fn), functions = TRUE, unique = TRUE)
      ))
    })
    function_tokens <- Map(
      function(symbols, fn) {
        unique(c(
          symbols,
          expression_strings(formals(fn)),
          expression_strings(body(fn))
        ))
      },
      function_symbols,
      retained_functions
    )
    installed_roots <- c(
      "inst/shiny" = system.file("shiny", package = "tempest"),
      "inst/examples" = system.file("examples", package = "tempest")
    )
    installed_roots <- installed_roots[nzchar(installed_roots)]
    installed_files <- unlist(
      lapply(
        installed_roots,
        \(path) {
          list.files(
            path,
            pattern = "[.]R$",
            recursive = TRUE,
            full.names = TRUE
          )
        }
      ),
      use.names = FALSE
    )
    installed_paths <- unlist(
      Map(
        function(label, path) {
          files <- list.files(
            path,
            pattern = "[.]R$",
            recursive = TRUE,
            full.names = TRUE
          )
          file.path(label, substring(files, nchar(path) + 2L))
        },
        names(installed_roots),
        installed_roots
      ),
      use.names = FALSE
    )
    installed_expressions <- lapply(installed_files, parse)
    installed_symbols <- lapply(
      installed_expressions,
      \(expressions) {
        all.names(
          expressions,
          functions = TRUE,
          unique = TRUE
        )
      }
    )
    installed_tokens <- Map(
      \(symbols, expressions) {
        unique(c(
          symbols,
          expression_strings(expressions)
        ))
      },
      installed_symbols,
      installed_expressions
    )
    names(installed_symbols) <- installed_paths
    names(installed_tokens) <- installed_paths
    all_symbols <- c(function_symbols, installed_symbols)
    all_tokens <- c(function_tokens, installed_tokens)
    offenders <- unlist(
      Map(
        function(tokens, path) {
          symbols <- sort(intersect(tokens, deleted_symbols))
          if (length(symbols) == 0L) {
            return(character())
          }
          paste(path, symbols, sep = "::")
        },
        all_symbols,
        names(all_symbols)
      ),
      use.names = FALSE
    )
    legacy_offenders <- unlist(
      Map(
        function(tokens, path) {
          matched <- intersect(legacy_tokens, tokens)
          if (length(matched) == 0L) {
            return(character())
          }
          paste(path, matched, sep = "::")
        },
        all_tokens,
        names(all_tokens)
      ),
      use.names = FALSE
    )
    legacy_allowlist <- "tempest_execution_event_history::TempestRun"
  }

  expect_identical(sort(offenders), character())
  legacy_offenders <- setdiff(
    sort(unique(legacy_offenders)),
    legacy_allowlist
  )

  expect_identical(legacy_offenders, character())
})

test_that("tests do not execute retired generic-kernel symbols", {
  frozen <- readLines(
    test_path("fixtures", "generic-kernel-exports-0.1.0.txt"),
    warn = FALSE
  )
  called_symbols <- function(expression) {
    calls <- character()
    walk <- function(node) {
      if (!is.call(node)) {
        return(invisible(NULL))
      }
      head <- node[[1L]]
      if (is.symbol(head)) {
        calls <<- c(calls, as.character(head))
      } else if (
        is.call(head) &&
          is.symbol(head[[1L]]) &&
          as.character(head[[1L]]) %in% c("::", ":::") &&
          length(head) >= 3L
      ) {
        calls <<- c(calls, as.character(head[[3L]]))
      }
      if (length(node) > 1L) {
        for (index in 2:length(node)) {
          walk(node[[index]])
        }
      }
      invisible(NULL)
    }
    walk(expression)
    unique(calls)
  }
  files <- list.files(
    test_path(),
    pattern = "^test-.*[.]R$",
    full.names = TRUE
  )
  offenders <- unlist(
    lapply(files, function(file) {
      expressions <- parse(file)
      calls <- unique(unlist(
        lapply(
          as.list(expressions),
          called_symbols
        ),
        use.names = FALSE
      ))
      symbols <- sort(intersect(calls, frozen))
      if (length(symbols) == 0L) {
        return(character())
      }
      paste(basename(file), symbols, sep = "::")
    }),
    use.names = FALSE
  )

  expect_identical(sort(offenders), character())
})
