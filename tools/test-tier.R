script <- NULL
if (sys.nframe() > 0L) {
  for (index in rev(seq_len(sys.nframe()))) {
    frame <- sys.frame(index)
    call <- sys.call(index)
    call_name <- if (is.call(call)) as.character(call[[1L]]) else ""
    candidate <- if ("ofile" %in% names(frame)) {
      frame$ofile
    } else if (identical(call_name, "sys.source") && "file" %in% names(frame)) {
      frame$file
    } else {
      NULL
    }
    if (
      is.character(candidate) &&
        length(candidate) == 1L &&
        !is.na(candidate) &&
        nzchar(candidate) &&
        file.exists(candidate)
    ) {
      script <- candidate
      break
    }
  }
}
if (is.null(script)) {
  script_arg <- grep(
    "^--file=",
    commandArgs(trailingOnly = FALSE),
    value = TRUE
  )
  script <- if (length(script_arg) > 0L) {
    sub("^--file=", "", script_arg[[1L]])
  } else {
    ""
  }
}
if (!nzchar(script) || identical(script, "-") || !file.exists(script)) {
  script <- file.path(getwd(), "tools", "test-tier.R")
}
script <- normalizePath(script, winslash = "/", mustWork = TRUE)
root <- dirname(dirname(script))
fixture <- file.path(root, "tests", "testthat", "fixtures", "test-tier.R")
if (!file.exists(fixture)) {
  stop(
    "The test-tier fixture is missing from the Tempest test bundle.",
    call. = FALSE
  )
}
sys.source(fixture, envir = environment())
