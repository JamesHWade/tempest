# Product execution-event history

tempest_execution_events_abort <- function(message, ..., parent = NULL) {
  tempest_abort(
    message,
    ...,
    class = c("tempest_execution_events_error", "tempest_error"),
    parent = parent
  )
}

tempest_execution_events_count <- function(value) {
  if (
    !is.numeric(value) ||
      length(value) != 1L ||
      is.na(value) ||
      !is.finite(value) ||
      value < 0L ||
      value != as.integer(value)
  ) {
    tempest_execution_events_abort(
      "{.arg after_sequence} must be one non-negative whole number."
    )
  }
  as.integer(value)
}

tempest_execution_event_history <- function(x) {
  if (inherits(x, "TempestRun")) {
    tempest_execution_events_abort(
      paste0(
        "{.fn tempest_execution_events} accepts only product execution ",
        "histories, not a generic {.cls TempestRun}."
      )
    )
  }
  if (!inherits(x, "TempestSession")) {
    tempest_execution_events_abort(
      "{.arg x} must be a TempestSession."
    )
  }
  events <- x$events
  if (!is.list(events) || is.data.frame(events)) {
    tempest_execution_events_abort(
      "The execution contains an invalid event history."
    )
  }
  tryCatch(
    lapply(events, tempest_progress_event_record),
    error = function(error) {
      tempest_execution_events_abort(
        "TempestSession contains a malformed progress event.",
        parent = error
      )
    }
  )
}

tempest_execution_event_sequences <- function(events) {
  sequences <- vapply(
    events,
    function(event) {
      if (!is.list(event)) {
        return(NA_integer_)
      }
      value <- event$sequence %||% NA_integer_
      if (
        !is.numeric(value) ||
          length(value) != 1L ||
          is.na(value) ||
          !is.finite(value) ||
          value < 1L ||
          value != as.integer(value)
      ) {
        return(NA_integer_)
      }
      as.integer(value)
    },
    integer(1)
  )
  if (anyNA(sequences) || any(diff(sequences) <= 0L)) {
    tempest_execution_events_abort(
      "Execution event sequences are not strictly increasing."
    )
  }
  sequences
}

#' Query events from a Tempest product execution
#'
#' `r lifecycle::badge("experimental")`
#'
#' `tempest_execution_events()` gives host adapters one cursor-based query for
#' immutable Co-STORM progress records. Generic `TempestRun` histories are
#' outside the supported product boundary and reject immediately.
#'
#' @param x A [TempestSession].
#' @param after_sequence Return only events whose sequence is greater than this
#'   non-negative execution-local cursor.
#' @return An ordered list of normalized event records.
#' @export
tempest_execution_events <- function(x, after_sequence = 0L) {
  after_sequence <- tempest_execution_events_count(after_sequence)
  events <- tempest_execution_event_history(x)
  if (length(events) == 0L) {
    return(list())
  }
  sequences <- tempest_execution_event_sequences(events)
  events[sequences > after_sequence]
}
