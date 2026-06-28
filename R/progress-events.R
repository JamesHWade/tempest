# Progress event contract ------------------------------------------------------

tempest_progress_workflows <- function() {
  c("storm", "costorm")
}

tempest_progress_event_types <- function() {
  c(
    "workflow",
    "stage",
    "step",
    "expert",
    "tool",
    "artifact",
    "cancellation"
  )
}

tempest_progress_statuses <- function() {
  c(
    "pending",
    "started",
    "running",
    "succeeded",
    "failed",
    "cancelled",
    "skipped",
    "available"
  )
}

tempest_progress_chr <- function(required = FALSE, default = NA_character_) {
  S7::new_property(
    S7::class_character,
    default = default,
    validator = function(value) {
      if (length(value) != 1) {
        return("must be length 1")
      }
      if (required && (is.na(value) || !nzchar(tempest_trim(value)))) {
        return("must be a single non-empty string")
      }
    }
  )
}

#' Create a Tempest progress event
#'
#' `tempest_progress_event()` creates a small host-neutral event record for
#' STORM and Co-STORM workflow progress. Events are designed for package code,
#' host apps, tests, and future telemetry adapters that need lifecycle,
#' stage, step, expert/tool, cancellation, and artifact status without
#' importing Shiny.
#'
#' Core fields are validated at construction. Payloads should stay small and
#' privacy-aware: use them for progress metadata and references, not full raw
#' expert answers or source bodies by default.
#'
#' @include ledger-types.R
#' @param run_id Stable run or session identifier.
#' @param workflow Workflow kind, one of `"storm"` or `"costorm"`.
#' @param event_type Event family, one of `"workflow"`, `"stage"`, `"step"`,
#'   `"expert"`, `"tool"`, `"artifact"`, or `"cancellation"`.
#' @param status Event status, one of `"pending"`, `"started"`, `"running"`,
#'   `"succeeded"`, `"failed"`, `"cancelled"`, `"skipped"`, or `"available"`.
#' @param stage Optional workflow stage name.
#' @param step Optional stage-local step name.
#' @param message Optional short human-readable progress message.
#' @param payload Optional list of structured progress metadata.
#' @param parent_event_id Optional parent event identifier.
#' @param correlation_id Optional identifier shared by related events.
#' @param event_id Optional stable event identifier. Defaults to a generated id.
#' @param timestamp Optional event timestamp. Defaults to the current UTC time.
#' @return A `tempest_progress_event` S7 object.
#' @examples
#' event <- tempest_progress_event(
#'   run_id = "run-1",
#'   workflow = "storm",
#'   event_type = "stage",
#'   status = "started",
#'   stage = "research"
#' )
#' event@status
#' @export
tempest_progress_event <- S7::new_class(
  "tempest_progress_event",
  properties = list(
    event_id = tempest_progress_chr(required = TRUE),
    run_id = tempest_progress_chr(required = TRUE),
    workflow = prop_enum(tempest_progress_workflows()),
    event_type = prop_enum(tempest_progress_event_types()),
    stage = tempest_progress_chr(),
    step = tempest_progress_chr(),
    status = prop_enum(tempest_progress_statuses()),
    timestamp = tempest_progress_chr(required = TRUE),
    message = tempest_progress_chr(),
    payload = S7::new_property(S7::class_list, default = list()),
    parent_event_id = tempest_progress_chr(),
    correlation_id = tempest_progress_chr()
  ),
  constructor = function(
    run_id,
    workflow,
    event_type,
    status,
    stage = NA_character_,
    step = NA_character_,
    message = NA_character_,
    payload = list(),
    parent_event_id = NA_character_,
    correlation_id = NA_character_,
    event_id = NULL,
    timestamp = NULL
  ) {
    S7::new_object(
      S7::S7_object(),
      event_id = event_id %||% tempest_uuid("P"),
      run_id = run_id,
      workflow = workflow,
      event_type = event_type,
      stage = stage,
      step = step,
      status = status,
      timestamp = timestamp %||% tempest_now_utc(),
      message = message,
      payload = payload,
      parent_event_id = parent_event_id,
      correlation_id = correlation_id
    )
  },
  validator = function(self) {
    if (
      identical(self@event_type, "stage") &&
        (is.na(self@stage) || !nzchar(tempest_trim(self@stage)))
    ) {
      return("stage events must include stage")
    }
    if (
      identical(self@event_type, "step") &&
        (is.na(self@step) || !nzchar(tempest_trim(self@step)))
    ) {
      return("step events must include step")
    }
  }
)

#' Convert a Tempest progress event to a list
#'
#' @param event A `tempest_progress_event` object.
#' @return A named list with the event contract fields.
#' @examples
#' event <- tempest_progress_event(
#'   run_id = "session-1",
#'   workflow = "costorm",
#'   event_type = "expert",
#'   status = "started",
#'   stage = "warmup"
#' )
#' tempest_progress_event_data(event)
#' @export
tempest_progress_event_data <- function(event) {
  if (!S7::S7_inherits(event, tempest_progress_event)) {
    tempest_abort("{.arg event} must be a tempest_progress_event object.")
  }

  props <- S7::prop_names(event)
  stats::setNames(lapply(props, function(prop) S7::prop(event, prop)), props)
}

tempest_progress_callback <- function(progress) {
  if (is.null(progress)) {
    return(NULL)
  }
  if (!is.function(progress)) {
    tempest_abort("{.arg progress} must be NULL or a function.")
  }
  progress
}

tempest_progress_error_payload <- function(error) {
  list(
    error_class = class(error)[[1]],
    error_message = conditionMessage(error)
  )
}

tempest_emit_progress <- function(progress, ...) {
  event <- tempest_progress_event(...)
  if (is.null(progress)) {
    return(invisible(event))
  }
  tryCatch(
    progress(event),
    error = function(e) {
      rlang::abort(
        "Progress callback failed.",
        class = "tempest_progress_callback_error",
        parent = e
      )
    }
  )
  invisible(event)
}

tempest_progress_without_payload <- function(event) {
  data <- tempest_progress_event_data(event)
  data$payload <- list()
  do.call(tempest_progress_event, data)
}

#' Create an in-memory Tempest progress event collector
#'
#' `tempest_progress_collector()` returns a small host-neutral sink for Tempest
#' progress events. Use `collector$record` as a `progress` callback, then read
#' events back with filtering or replay them to another callback.
#'
#' By default, the collector strips event payloads before storing them. Set
#' `include_payload = TRUE` when tests or trusted host adapters need to assert
#' or replay structured payload metadata.
#'
#' @param include_payload If `TRUE`, retain event payloads. The default `FALSE`
#'   stores event metadata only.
#' @return A `tempest_progress_collector` list with `record()`, `events()`,
#'   `data()`, `replay()`, and `clear()` methods.
#' @examples
#' collector <- tempest_progress_collector()
#' collector$record(tempest_progress_event(
#'   run_id = "run-1",
#'   workflow = "storm",
#'   event_type = "stage",
#'   status = "started",
#'   stage = "research"
#' ))
#' collector$data(stage = "research")
#' @export
tempest_progress_collector <- function(include_payload = FALSE) {
  include_payload <- isTRUE(include_payload)
  events <- list()

  record <- function(event) {
    if (!S7::S7_inherits(event, tempest_progress_event)) {
      tempest_abort("{.arg event} must be a tempest_progress_event object.")
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
      lapply(filtered, tempest_progress_event_data)
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

#' Filter Tempest progress events
#'
#' @param events A list of `tempest_progress_event` objects, or a
#'   `tempest_progress_collector`.
#' @param run_id,workflow,event_type,stage,status,correlation_id Optional
#'   scalar filters. `NULL` leaves that field unfiltered.
#' @return A list of matching `tempest_progress_event` objects.
#' @examples
#' events <- list(tempest_progress_event(
#'   run_id = "run-1",
#'   workflow = "storm",
#'   event_type = "stage",
#'   status = "started",
#'   stage = "research"
#' ))
#' tempest_progress_filter(events, stage = "research")
#' @export
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
    tempest_abort("{.arg events} must be a list or progress collector.")
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
      tempest_abort(
        "{.arg {field}} must be NULL or a single string.",
        field = field
      )
    }
  }

  Filter(
    function(event) {
      if (!S7::S7_inherits(event, tempest_progress_event)) {
        tempest_abort("{.arg events} must contain only progress event objects.")
      }
      data <- tempest_progress_event_data(event)
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

#' Replay Tempest progress events to a callback
#'
#' @param events A list of `tempest_progress_event` objects, or a
#'   `tempest_progress_collector`.
#' @param progress Function called once for each event.
#' @return The input events, invisibly.
#' @examples
#' events <- list(tempest_progress_event(
#'   run_id = "run-1",
#'   workflow = "storm",
#'   event_type = "workflow",
#'   status = "started"
#' ))
#' tempest_progress_replay(events, function(event) event@status)
#' @export
tempest_progress_replay <- function(events, progress) {
  if (inherits(events, "tempest_progress_collector")) {
    events <- events$events()
  }
  if (is.null(progress)) {
    tempest_abort("{.arg progress} must be a function.")
  }
  progress <- tempest_progress_callback(progress)
  for (event in events) {
    if (!S7::S7_inherits(event, tempest_progress_event)) {
      tempest_abort("{.arg events} must contain only progress event objects.")
    }
    progress(event)
  }
  invisible(events)
}

#' Reduce Tempest progress events to workflow state
#'
#' `tempest_progress_state()` consumes recorded Tempest progress events and
#' returns a compact, serializable state object that host apps can poll or
#' render. The reducer is host-neutral: it does not import Shiny and it works
#' with raw event lists, collector objects, or stored event data lists.
#'
#' @param events A list of `tempest_progress_event` objects, a
#'   `tempest_progress_collector`, or a list of event data lists from
#'   `tempest_progress_event_data()`.
#' @return A named list with run/workflow ids, current active work, completed
#'   stages, failures, cancellation details, artifact references, and terminal
#'   status.
#' @examples
#' events <- list(
#'   tempest_progress_event(
#'     run_id = "run-1",
#'     workflow = "storm",
#'     event_type = "workflow",
#'     status = "started"
#'   ),
#'   tempest_progress_event(
#'     run_id = "run-1",
#'     workflow = "storm",
#'     event_type = "stage",
#'     status = "started",
#'     stage = "research"
#'   )
#' )
#' tempest_progress_state(events)
#' @export
tempest_progress_state <- function(events) {
  events <- tempest_progress_normalize_events(events)
  state <- tempest_progress_empty_state()
  for (event in events) {
    state <- tempest_progress_state_step(state, event)
  }
  state
}

tempest_progress_empty_state <- function() {
  list(
    run_id = NA_character_,
    workflow = NA_character_,
    status = "pending",
    terminal = FALSE,
    current_stage = NA_character_,
    current_step = NA_character_,
    completed_stages = character(),
    skipped_stages = character(),
    active = list(
      stages = list(),
      steps = list(),
      experts = list(),
      tools = list()
    ),
    failures = list(),
    cancellation = NULL,
    artifacts = list(),
    event_count = 0L,
    latest_event_id = NA_character_,
    updated_at = NA_character_
  )
}

tempest_progress_normalize_events <- function(events) {
  if (inherits(events, "tempest_progress_collector")) {
    events <- events$events()
  } else if (
    S7::S7_inherits(events, tempest_progress_event) ||
      tempest_progress_is_event_data(events)
  ) {
    events <- list(events)
  }
  if (!is.list(events)) {
    tempest_abort("{.arg events} must be a list or progress collector.")
  }
  lapply(events, tempest_progress_event_record)
}

tempest_progress_is_event_data <- function(event) {
  is.list(event) &&
    all(
      c(
        "event_id",
        "run_id",
        "workflow",
        "event_type",
        "status",
        "timestamp"
      ) %in%
        names(event)
    )
}

tempest_progress_event_record <- function(event) {
  if (S7::S7_inherits(event, tempest_progress_event)) {
    return(tempest_progress_event_data(event))
  }
  if (!tempest_progress_is_event_data(event)) {
    tempest_abort(
      "{.arg events} must contain progress event objects or event data lists."
    )
  }

  defaults <- list(
    stage = NA_character_,
    step = NA_character_,
    message = NA_character_,
    payload = list(),
    parent_event_id = NA_character_,
    correlation_id = NA_character_
  )
  missing <- setdiff(names(defaults), names(event))
  event[missing] <- defaults[missing]

  required <- c(
    "event_id",
    "run_id",
    "workflow",
    "event_type",
    "status",
    "timestamp"
  )
  for (field in required) {
    if (
      !rlang::is_string(event[[field]]) ||
        is.na(event[[field]]) ||
        !nzchar(tempest_trim(event[[field]]))
    ) {
      tempest_abort("{.arg events} contains an invalid {.field {field}}.")
    }
  }
  for (field in c(
    "stage",
    "step",
    "message",
    "parent_event_id",
    "correlation_id"
  )) {
    if (!is.character(event[[field]]) || length(event[[field]]) != 1L) {
      tempest_abort("{.arg events} contains an invalid {.field {field}}.")
    }
  }
  if (!is.list(event$payload)) {
    tempest_abort("{.arg events} contains an invalid {.field payload}.")
  }

  event
}

#' Progress labels for Tempest workflows
#'
#' `tempest_progress_labels()` returns compact, host-neutral labels for progress
#' event stages and steps. Host apps can use these labels with
#' [tempest_progress_state()] instead of maintaining their own workflow-specific
#' remapping tables.
#'
#' @param workflow Workflow kind, one of `"storm"` or `"costorm"`.
#' @param kind Label kind, either `"stage"` or `"step"`.
#' @return A named character vector.
#' @examples
#' tempest_progress_labels("costorm")
#' tempest_progress_labels("costorm", kind = "step")
#' @export
tempest_progress_labels <- function(
  workflow = c("storm", "costorm"),
  kind = c("stage", "step")
) {
  workflow <- match.arg(workflow)
  kind <- match.arg(kind)

  if (identical(workflow, "storm") && identical(kind, "stage")) {
    return(c(
      perspectives = "Perspectives",
      research = "Research",
      outline = "Outline",
      write = "Write",
      polish = "Polish",
      verification = "Verify"
    ))
  }
  if (identical(workflow, "storm") && identical(kind, "step")) {
    return(c(
      created = "Ready",
      persist = "Save",
      report_md = "Report"
    ))
  }
  if (identical(workflow, "costorm") && identical(kind, "stage")) {
    return(c(
      session = "Setup",
      warmup = "Warmup",
      dialogue = "Answer",
      evidence = "Evidence",
      mindmap = "Map",
      suggestions = "Next",
      report = "Report"
    ))
  }
  c(
    created = "Ready",
    expert_fanout = "Experts",
    expert_question = "Question",
    turn = "Turn",
    user_turn = "User",
    moderator_response = "Moderator",
    fact_extraction = "Capture facts",
    update = "Update map",
    question_generation = "Questions",
    generate = "Generate",
    report_md = "Report"
  )
}

tempest_progress_state_step <- function(state, event) {
  if (is.na(state$run_id)) {
    state$run_id <- event$run_id
    state$workflow <- event$workflow
  } else if (!identical(state$run_id, event$run_id)) {
    tempest_abort("{.arg events} must belong to one run_id.")
  } else if (!identical(state$workflow, event$workflow)) {
    tempest_abort("{.arg events} must belong to one workflow.")
  }

  state$event_count <- state$event_count + 1L
  state$latest_event_id <- event$event_id
  state$updated_at <- event$timestamp

  bucket <- tempest_progress_active_bucket(event$event_type)
  if (
    !is.null(bucket) && event$status %in% c("pending", "started", "running")
  ) {
    key <- tempest_progress_active_key(event)
    state$active[[bucket]][[key]] <- tempest_progress_active_summary(event)
  }
  if (
    !is.null(bucket) &&
      event$status %in%
        c(
          "succeeded",
          "failed",
          "cancelled",
          "skipped"
        )
  ) {
    state$active[[bucket]] <- tempest_progress_remove_active(
      state$active[[bucket]],
      event
    )
  }

  if (
    identical(event$event_type, "stage") &&
      identical(event$status, "succeeded")
  ) {
    state$completed_stages <- unique(c(state$completed_stages, event$stage))
  }
  if (
    identical(event$event_type, "stage") &&
      identical(event$status, "skipped")
  ) {
    state$skipped_stages <- unique(c(state$skipped_stages, event$stage))
  }
  if (identical(event$status, "failed")) {
    state$failures[[length(state$failures) + 1L]] <-
      tempest_progress_failure_summary(event)
    if (event$event_type %in% c("workflow", "stage")) {
      state$status <- "failed"
    }
  }
  if (
    identical(event$event_type, "artifact") &&
      identical(event$status, "available")
  ) {
    artifact <- tempest_progress_artifact_name(event)
    state$artifacts[[artifact]] <- tempest_progress_artifact_summary(
      event,
      artifact
    )
  }
  if (
    identical(event$event_type, "cancellation") ||
      identical(event$status, "cancelled")
  ) {
    state$status <- "cancelled"
    state$terminal <- TRUE
    state$cancellation <- tempest_progress_cancellation_summary(event)
    state$active <- tempest_progress_clear_active(state$active)
  } else if (
    identical(event$event_type, "workflow") &&
      identical(event$status, "succeeded")
  ) {
    state$status <- "succeeded"
    state$terminal <- TRUE
    state$active <- tempest_progress_clear_active(state$active)
  } else if (
    identical(event$event_type, "workflow") &&
      identical(event$status, "failed")
  ) {
    state$status <- "failed"
    state$terminal <- TRUE
    state$active <- tempest_progress_clear_active(state$active)
  } else if (
    !isTRUE(state$terminal) &&
      event$status %in% c("pending", "started", "running")
  ) {
    state$status <- "running"
  }

  tempest_progress_update_current(state)
}

tempest_progress_active_bucket <- function(event_type) {
  switch(
    event_type,
    stage = "stages",
    step = "steps",
    expert = "experts",
    tool = "tools",
    NULL
  )
}

tempest_progress_active_key <- function(event) {
  tempest_progress_first_chr(
    event$correlation_id,
    event$parent_event_id,
    event$event_id
  )
}

tempest_progress_active_summary <- function(event) {
  payload <- event$payload %||% list()
  list(
    event_id = event$event_id,
    event_type = event$event_type,
    stage = event$stage,
    step = event$step,
    status = event$status,
    message = event$message,
    started_at = event$timestamp,
    updated_at = event$timestamp,
    parent_event_id = event$parent_event_id,
    correlation_id = event$correlation_id,
    expert_id = tempest_progress_payload_chr(payload, "expert_id"),
    expert_name = tempest_progress_payload_chr(payload, "expert_name"),
    session_id = tempest_progress_payload_chr(payload, "session_id"),
    question_index = tempest_progress_payload_chr(payload, "question_index")
  )
}

tempest_progress_remove_active <- function(active, event) {
  keys <- unique(c(
    tempest_progress_valid_key(event$parent_event_id),
    tempest_progress_valid_key(event$correlation_id),
    tempest_progress_valid_key(event$event_id)
  ))
  active[keys] <- NULL
  Filter(
    function(item) !tempest_progress_same_activity(item, event),
    active
  )
}

tempest_progress_same_activity <- function(item, event) {
  if (!identical(item$event_type, event$event_type)) {
    return(FALSE)
  }
  if (!tempest_progress_same_chr(item$stage, event$stage)) {
    return(FALSE)
  }
  if (!tempest_progress_same_chr(item$step, event$step)) {
    return(FALSE)
  }
  payload <- event$payload %||% list()
  for (field in c("expert_id", "expert_name", "session_id", "question_index")) {
    value <- tempest_progress_payload_chr(payload, field)
    if (
      !is.na(value) &&
        !tempest_progress_same_chr(item[[field]] %||% NA_character_, value)
    ) {
      return(FALSE)
    }
  }
  TRUE
}

tempest_progress_update_current <- function(state) {
  active <- c(
    state$active$stages,
    state$active$steps,
    state$active$experts,
    state$active$tools
  )
  state$current_stage <- tempest_progress_last_active_chr(active, "stage")
  state$current_step <- tempest_progress_last_active_chr(active, "step")
  state
}

tempest_progress_last_active_chr <- function(active, field) {
  if (length(active) == 0L) {
    return(NA_character_)
  }
  values <- vapply(
    active,
    function(item) item[[field]] %||% NA_character_,
    character(1)
  )
  values <- values[!is.na(values) & nzchar(values)]
  if (length(values) == 0L) {
    NA_character_
  } else {
    values[[length(values)]]
  }
}

tempest_progress_clear_active <- function(active) {
  active$stages <- list()
  active$steps <- list()
  active$experts <- list()
  active$tools <- list()
  active
}

tempest_progress_failure_summary <- function(event) {
  payload <- event$payload %||% list()
  list(
    event_id = event$event_id,
    event_type = event$event_type,
    stage = event$stage,
    step = event$step,
    message = event$message,
    error_class = tempest_progress_payload_chr(payload, "error_class"),
    error_message = tempest_progress_payload_chr(payload, "error_message"),
    expert_id = tempest_progress_payload_chr(payload, "expert_id"),
    expert_name = tempest_progress_payload_chr(payload, "expert_name"),
    timestamp = event$timestamp
  )
}

tempest_progress_artifact_name <- function(event) {
  payload <- event$payload %||% list()
  tempest_progress_first_chr(
    tempest_progress_payload_chr(payload, "artifact"),
    event$step,
    event$stage,
    event$event_id
  )
}

tempest_progress_artifact_summary <- function(event, artifact) {
  payload <- event$payload %||% list()
  list(
    artifact = artifact,
    event_id = event$event_id,
    stage = event$stage,
    step = event$step,
    status = event$status,
    available_at = event$timestamp,
    persisted = payload$persisted %||% NA
  )
}

tempest_progress_cancellation_summary <- function(event) {
  list(
    event_id = event$event_id,
    stage = event$stage,
    step = event$step,
    message = event$message,
    timestamp = event$timestamp
  )
}

tempest_progress_first_chr <- function(...) {
  values <- list(...)
  for (value in values) {
    if (
      is.character(value) &&
        length(value) == 1L &&
        !is.na(value) &&
        nzchar(value)
    ) {
      return(value)
    }
  }
  NA_character_
}

tempest_progress_payload_chr <- function(payload, field) {
  value <- payload[[field]]
  if (is.null(value) || length(value) == 0L) {
    return(NA_character_)
  }
  value <- value[[1]]
  if (is.na(value)) {
    return(NA_character_)
  }
  as.character(value)
}

tempest_progress_valid_key <- function(value) {
  if (
    is.character(value) && length(value) == 1L && !is.na(value) && nzchar(value)
  ) {
    value
  } else {
    character()
  }
}

tempest_progress_same_chr <- function(x, y) {
  if (is.na(x) && is.na(y)) {
    return(TRUE)
  }
  identical(x, y)
}
