await_tempest_promise <- function(
  promise,
  timeout_s = if (identical(Sys.getenv("R_COVR"), "true")) 60 else 10
) {
  resolved <- FALSE
  value <- NULL
  error <- NULL
  promises::then(
    promise,
    onFulfilled = function(result) {
      value <<- result
      resolved <<- TRUE
    },
    onRejected = function(condition) {
      error <<- condition
      resolved <<- TRUE
    }
  )
  deadline <- Sys.time() + timeout_s
  while (!resolved && Sys.time() < deadline) {
    later::run_now(0.02)
    Sys.sleep(0.01)
  }
  if (!resolved) {
    stop("Promise did not settle before the test timeout.", call. = FALSE)
  }
  list(value = value, error = error)
}

local_mirai_coverage_dir <- function(.local_envir = parent.frame()) {
  if (identical(Sys.getenv("R_COVR"), "true")) {
    withr::local_envvar(
      COVERAGE_DIR = tempdir(),
      .local_envir = .local_envir
    )
  }
  invisible(NULL)
}
