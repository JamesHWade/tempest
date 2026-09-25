# The unit tier is deliberately an explicit allowlist. Files that are not in
# this vector are always assigned to the integration/full tier.
test_tier_unit_files <- c(
  "test-app.R",
  "test-briefing-items.R",
  "test-cache.R",
  "test-claim-extraction.R",
  "test-claim-support.R",
  "test-config-s7.R",
  "test-config.R",
  "test-dsprrr-fallback.R",
  "test-dsprrr-integration.R",
  "test-dsprrr-ledger-modules.R",
  "test-execution-events.R",
  "test-expert-types.R",
  "test-governed-procedure.R",
  "test-helper-mocks.R",
  "test-lead-section.R",
  "test-ledger-store.R",
  "test-ledger-types.R",
  "test-platform-contracts.R",
  "test-product-persistence-inventory.R",
  "test-product-persistence-schema-dispatch.R",
  "test-product-persistence.R",
  "test-product-result.R",
  "test-product-surface-inventory.R",
  "test-product-validation.R",
  "test-public-api.R",
  "test-query-decomposition.R",
  "test-research-expert.R",
  "test-research-record-identity.R",
  "test-research-tools.R",
  "test-research-workspace-persistence-claim-support.R",
  "test-research-workspace-persistence-records.R",
  "test-research-workspace-persistence.R",
  "test-research-workspace.R",
  "test-resources.R",
  "test-run-verification.R",
  "test-s7-setup.R",
  "test-shiny-host-example.R",
  "test-shinychat-adapter.R",
  "test-stage-atomicity.R",
  "test-suggestions.R",
  "test-toc-extraction.R",
  "test-tools.R",
  "test-utils.R",
  "test-workspace-claim-batches.R",
  "test-writing.R"
)

test_tier_abort <- function(message, ...) {
  cli::cli_abort(
    message,
    class = c("tempest_test_tier_error", "tempest_error"),
    ...
  )
}

test_tier_root <- function(root = getwd()) {
  if (!is.character(root) || length(root) != 1L || is.na(root)) {
    test_tier_abort("The repository root must be one non-missing path.")
  }
  original <- normalizePath(root, winslash = "/", mustWork = FALSE)
  candidates <- original
  repeat {
    parent <- dirname(tail(candidates, 1L))
    if (identical(parent, tail(candidates, 1L))) {
      break
    }
    candidates <- c(candidates, parent)
  }
  valid <- vapply(
    candidates,
    function(candidate) {
      description <- file.path(candidate, "DESCRIPTION")
      testthat_dir <- file.path(candidate, "tests", "testthat")
      package <- if (file.exists(description)) {
        tryCatch(
          suppressWarnings(read.dcf(description, fields = "Package")[[1L]]),
          error = function(error) NA_character_
        )
      } else {
        NA_character_
      }
      identical(package, "tempest") && dir.exists(testthat_dir)
    },
    logical(1)
  )
  if (!any(valid)) {
    test_tier_abort(paste0(
      "Run the test-tier script from a Tempest repository root (found `",
      original,
      "`)."
    ))
  }
  candidates[[which(valid)[[1L]]]]
}

test_tier_inventory <- function(root = getwd()) {
  root <- test_tier_root(root)
  files <- list.files(
    file.path(root, "tests", "testthat"),
    pattern = "^test.*[.][rR]$",
    full.names = FALSE
  )
  sort(files, method = "radix")
}

test_tier_validate <- function(
  inventory,
  unit_files = test_tier_unit_files
) {
  if (!is.character(inventory) || anyNA(inventory)) {
    test_tier_abort("The test-file inventory must be a character vector.")
  }
  if (anyDuplicated(inventory)) {
    duplicates <- unique(inventory[duplicated(inventory)])
    test_tier_abort(paste0(
      "The test-file inventory contains duplicates: ",
      paste(duplicates, collapse = ", "),
      "."
    ))
  }
  if (length(inventory) == 0L) {
    test_tier_abort("The test-file inventory is empty.")
  }
  if (!is.character(unit_files) || anyNA(unit_files)) {
    test_tier_abort("The unit-tier allowlist must be a character vector.")
  }
  if (length(unit_files) == 0L) {
    test_tier_abort("The unit-tier allowlist is empty.")
  }

  duplicate_units <- unique(unit_files[duplicated(unit_files)])
  if (length(duplicate_units) > 0L) {
    test_tier_abort(paste0(
      "The unit-tier allowlist contains duplicates: ",
      paste(duplicate_units, collapse = ", "),
      "."
    ))
  }
  unknown_units <- setdiff(unit_files, inventory)
  if (length(unknown_units) > 0L) {
    test_tier_abort(paste0(
      "The unit-tier allowlist names unknown test files: ",
      paste(unknown_units, collapse = ", "),
      "."
    ))
  }

  unit_files <- unit_files[order(unit_files, method = "radix")]
  full_files <- setdiff(inventory, unit_files)
  accounted <- c(unit_files, full_files)
  if (
    length(accounted) != length(inventory) ||
      !setequal(accounted, inventory)
  ) {
    test_tier_abort(
      "The test-tier manifest does not account for every test file."
    )
  }

  list(
    inventory = inventory,
    unit = unit_files,
    full = full_files
  )
}

test_tier_manifest <- function(
  root = getwd(),
  unit_files = test_tier_unit_files
) {
  inventory <- test_tier_inventory(root)
  test_tier_validate(inventory, unit_files = unit_files)
}

test_tier_filter <- function(files) {
  if (!is.character(files) || length(files) == 0L || anyNA(files)) {
    test_tier_abort("An exact, non-empty test-file selection is required.")
  }
  contexts <- sub("[.][rR]$", "", sub("^test[-_]", "", files))
  paste(utils::glob2rx(contexts), collapse = "|")
}

test_tier_report <- function(manifest, mode = c("list", "validate")) {
  mode <- match.arg(mode)
  if (identical(mode, "validate")) {
    cat("Test-tier manifest is valid.\n")
  }
  cat(sprintf("Inventory: %d test files\n", length(manifest$inventory)))
  cat(sprintf("Unit tier: %d test files\n", length(manifest$unit)))
  writeLines(paste0("  ", manifest$unit))
  cat(sprintf("Integration/full tier: %d test files\n", length(manifest$full)))
  if (identical(mode, "list")) {
    writeLines(paste0("  ", manifest$full))
  }
  invisible(manifest)
}

test_tier_run_unit <- function(
  root = getwd(),
  manifest = test_tier_manifest(root)
) {
  root <- test_tier_root(root)
  started <- proc.time()[["elapsed"]]
  cli::cli_inform(
    "Running unit tier ({length(manifest$unit)} test files) with an exact file filter."
  )
  results <- testthat::test_local(
    path = root,
    filter = test_tier_filter(manifest$unit),
    reporter = testthat::ProgressReporter$new(),
    stop_on_failure = FALSE
  )
  timings <- as.data.frame(results)
  elapsed <- proc.time()[["elapsed"]] - started
  observed_files <- if ("file" %in% names(timings)) {
    sort(unique(basename(timings$file)), method = "radix")
  } else {
    character()
  }
  if (!setequal(observed_files, manifest$unit)) {
    test_tier_abort(
      paste0(
        "The unit-tier filter did not run the exact allowlist. Expected: ",
        paste(manifest$unit, collapse = ", "),
        "; observed: ",
        paste(observed_files, collapse = ", "),
        "."
      )
    )
  }
  failed <- if ("failed" %in% names(timings)) {
    sum(timings$failed > 0L, na.rm = TRUE)
  } else {
    0L
  }
  errors <- if ("error" %in% names(timings)) {
    sum(as.logical(timings$error), na.rm = TRUE)
  } else {
    0L
  }
  if (failed > 0L || errors > 0L) {
    test_tier_abort(paste0(
      "Unit tier failed (",
      failed,
      " failed tests, ",
      errors,
      " errors)."
    ))
  }
  cli::cli_inform("Unit tier passed in {round(elapsed, 2)} seconds.")
  invisible(list(manifest = manifest, timings = timings, elapsed = elapsed))
}

test_tier_main <- function(args = commandArgs(trailingOnly = TRUE)) {
  if (length(args) != 1L || !args[[1L]] %in% c("unit", "list", "validate")) {
    test_tier_abort(
      "Usage: Rscript tools/test-tier.R <unit|list|validate>"
    )
  }
  mode <- args[[1L]]
  manifest <- test_tier_manifest()
  if (mode %in% c("list", "validate")) {
    test_tier_report(manifest, mode = mode)
  } else {
    test_tier_run_unit(manifest = manifest)
  }
  invisible(manifest)
}

if (identical(environment(), globalenv())) {
  test_tier_main()
}
