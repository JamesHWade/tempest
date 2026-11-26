s7_validation_error_class <- function() {
  if (utils::packageVersion("S7") >= "1.0.0") {
    "S7_error_validation_failed"
  } else {
    "simpleError"
  }
}
