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

tempest_progress_event_data <- function(event) {
  if (!S7::S7_inherits(event, tempest_progress_event)) {
    tempest_abort("{.arg event} must be a tempest_progress_event object.")
  }

  props <- S7::prop_names(event)
  stats::setNames(lapply(props, function(prop) S7::prop(event, prop)), props)
}

# The exact canonical field order delivered to a host progress callback.
tempest_progress_record_fields <- function() {
  c(
    "event_id",
    "run_id",
    "workflow",
    "event_type",
    "stage",
    "step",
    "status",
    "timestamp",
    "message",
    "payload",
    "parent_event_id",
    "correlation_id"
  )
}

# Project one progress event into the canonical plain named record. Hosts store
# and reduce this with ordinary R code; they never need event constructors,
# decoders, collectors, replay helpers, or label tables.
tempest_progress_record <- function(event) {
  data <- tempest_progress_event_record(event)
  fields <- tempest_progress_record_fields()
  stats::setNames(lapply(fields, function(field) data[[field]]), fields)
}

# Rebuild a progress event from its canonical plain record.
tempest_progress_event_from_record <- function(record) {
  if (!tempest_progress_is_event_data(record)) {
    tempest_abort("{.arg event} must be a tempest_progress_event object.")
  }
  data <- tempest_progress_event_record(record)
  tempest_progress_event(
    run_id = data$run_id,
    workflow = data$workflow,
    event_type = data$event_type,
    status = data$status,
    stage = data$stage,
    step = data$step,
    message = data$message,
    payload = data$payload,
    parent_event_id = data$parent_event_id,
    correlation_id = data$correlation_id,
    event_id = data$event_id,
    timestamp = data$timestamp
  )
}

tempest_progress_callback <- function(progress) {
  if (is.null(progress)) {
    return(NULL)
  }
  if (!is.function(progress)) {
    tempest_abort("{.arg progress} must be NULL or a function.")
  }
  function(event) {
    progress(tempest_progress_record(event))
  }
}

tempest_progress_error_payload <- function(error) {
  known_classes <- intersect(class(error), tempest_stage_failure_classes())
  list(
    error_class = if (length(known_classes) > 0L) {
      known_classes[[1]]
    } else {
      "tempest_operation_error"
    },
    error_message = "The operation failed."
  )
}

tempest_emit_progress <- function(progress, ...) {
  event <- tempest_progress_event(...)
  tempest_otel_progress_event(event)
  if (is.null(progress)) {
    return(invisible(event))
  }
  tryCatch(
    progress(event),
    error = function(e) {
      tempest_rethrow_operation(e, class = "tempest_progress_callback_error")
    }
  )
  invisible(event)
}

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
  if (!inherits(x, "TempestSession")) {
    tempest_execution_events_abort(
      "{.arg x} must be a TempestSession."
    )
  }
  events <- tempest_session_events(x)
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

tempest_execution_events <- function(x, after_sequence = 0L) {
  after_sequence <- tempest_execution_events_count(after_sequence)
  events <- tempest_execution_event_history(x)
  if (length(events) == 0L) {
    return(list())
  }
  sequences <- tempest_execution_event_sequences(events)
  events[sequences > after_sequence]
}
