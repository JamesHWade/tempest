run_test_shard <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) != 2L || !all(grepl("^[1-9][0-9]*$", args))) {
    cli::cli_abort("Usage: Rscript tools/test-shard.R <shard> <count>")
  }
  shard <- as.integer(args[[1L]])
  count <- as.integer(args[[2L]])
  if (anyNA(c(shard, count)) || shard > count) {
    cli::cli_abort("The shard must be between 1 and the shard count.")
  }
  if (
    !file.exists("DESCRIPTION") ||
      !identical(read.dcf("DESCRIPTION", fields = "Package")[[1L]], "tempest")
  ) {
    cli::cli_abort("Run this script from the Tempest repository root.")
  }
  files <- sort(
    list.files("tests/testthat", pattern = "^test.*[.][rR]$"),
    method = "radix"
  )
  selected <- files[(seq_along(files) - 1L) %% count == shard - 1L]
  if (length(selected) == 0L) {
    cli::cli_abort("This shard contains no test files.")
  }
  contexts <- sub("[.][Rr]$", "", sub("^test[-_]", "", selected))
  filter <- paste(utils::glob2rx(contexts), collapse = "|")
  cli::cli_inform("Shard {shard}/{count}: {length(selected)} test files.")
  results <- testthat::test_local(
    filter = filter,
    reporter = "summary",
    stop_on_failure = FALSE
  )
  timings <- as.data.frame(results)
  utils::write.csv(
    timings[c("file", "test", "real", "failed", "error", "skipped")],
    file.path(
      Sys.getenv("RUNNER_TEMP", unset = tempdir()),
      paste0("test-times-", shard, ".csv")
    ),
    row.names = FALSE
  )
  if (any(timings$failed > 0L | timings$error)) {
    cli::cli_abort("Shard {shard}/{count} failed.")
  }
}

run_test_shard()
