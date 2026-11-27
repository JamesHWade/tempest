# Test-only progress event collector, filter, and replay helpers

tempest_progress_without_payload <- function(event) {
  data <- tempest:::tempest_progress_event_data(event)
  data$payload <- list()
  do.call(tempest:::tempest_progress_event, data)
}

tempest_progress_collector <- function(include_payload = FALSE) {
  include_payload <- isTRUE(include_payload)
  events <- list()

  record <- function(event) {
    if (!S7::S7_inherits(event, tempest:::tempest_progress_event)) {
      event <- tempest:::tempest_progress_event_from_record(event)
    }
    if (!include_payload) {
      event <- tempest_progress_without_payload(event)
    }
    events[[length(events) + 1L]] <<- event
    invisible(event)
  }

  collector <- list(
    record = record,
    events = function(
      run_id = NULL,
      workflow = NULL,
      event_type = NULL,
      stage = NULL,
      status = NULL,
      correlation_id = NULL
    ) {
      tempest_progress_filter(
        events,
        run_id = run_id,
        workflow = workflow,
        event_type = event_type,
        stage = stage,
        status = status,
        correlation_id = correlation_id
      )
    },
    data = function(
      run_id = NULL,
      workflow = NULL,
      event_type = NULL,
      stage = NULL,
      status = NULL,
      correlation_id = NULL
    ) {
      filtered <- tempest_progress_filter(
        events,
        run_id = run_id,
        workflow = workflow,
        event_type = event_type,
        stage = stage,
        status = status,
        correlation_id = correlation_id
      )
      lapply(filtered, tempest:::tempest_progress_event_data)
    },
    replay = function(
      progress,
      run_id = NULL,
      workflow = NULL,
      event_type = NULL,
      stage = NULL,
      status = NULL,
      correlation_id = NULL
    ) {
      filtered <- tempest_progress_filter(
        events,
        run_id = run_id,
        workflow = workflow,
        event_type = event_type,
        stage = stage,
        status = status,
        correlation_id = correlation_id
      )
      tempest_progress_replay(filtered, progress)
    },
    clear = function() {
      events <<- list()
      invisible(NULL)
    }
  )
  structure(collector, class = "tempest_progress_collector")
}

tempest_progress_filter <- function(
  events,
  run_id = NULL,
  workflow = NULL,
  event_type = NULL,
  stage = NULL,
  status = NULL,
  correlation_id = NULL
) {
  if (inherits(events, "tempest_progress_collector")) {
    events <- events$events()
  }
  if (!is.list(events)) {
    tempest:::tempest_abort(
      "{.arg events} must be a list or progress collector."
    )
  }
  filters <- list(
    run_id = run_id,
    workflow = workflow,
    event_type = event_type,
    stage = stage,
    status = status,
    correlation_id = correlation_id
  )
  filters <- filters[!vapply(filters, is.null, logical(1))]
  for (field in names(filters)) {
    value <- filters[[field]]
    if (!rlang::is_string(value)) {
      tempest:::tempest_abort(
        "{.arg {field}} must be NULL or a single string.",
        field = field
      )
    }
  }

  Filter(
    function(event) {
      if (!S7::S7_inherits(event, tempest:::tempest_progress_event)) {
        tempest:::tempest_abort(
          "{.arg events} must contain only progress event objects."
        )
      }
      data <- tempest:::tempest_progress_event_data(event)
      for (field in names(filters)) {
        value <- data[[field]]
        if (is.na(value) || !identical(value, filters[[field]])) {
          return(FALSE)
        }
      }
      TRUE
    },
    events
  )
}

tempest_progress_replay <- function(events, progress) {
  if (inherits(events, "tempest_progress_collector")) {
    events <- events$events()
  }
  if (is.null(progress)) {
    tempest:::tempest_abort("{.arg progress} must be a function.")
  }
  progress <- tempest:::tempest_progress_callback(progress)
  for (event in events) {
    if (!S7::S7_inherits(event, tempest:::tempest_progress_event)) {
      tempest:::tempest_abort(
        "{.arg events} must contain only progress event objects."
      )
    }
    progress(event)
  }
  invisible(events)
}
