await_tempest_promise <- function(promise, timeout_s = 10) {
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
