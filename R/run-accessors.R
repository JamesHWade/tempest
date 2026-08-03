# Public accessors and controls for generic Tempest runs

tempest_run_accessor_abort <- function(message, ..., parent = NULL) {
  tempest_run_abort(
    message,
    ...,
    class = "tempest_run_accessor_error",
    parent = parent
  )
}

tempest_run_accessor_validate <- function(run) {
  if (!inherits(run, "TempestRun")) {
    tempest_run_accessor_abort(
      "{.arg run} must be created by {.fn tempest_run_workflow}."
    )
  }
  run
}

tempest_run_accessor_count <- function(value, arg) {
  if (
    !is.numeric(value) ||
      length(value) != 1L ||
      is.na(value) ||
      !is.finite(value) ||
      value < 0L ||
      value != as.integer(value)
  ) {
    tempest_run_accessor_abort(
      paste0("`", arg, "` must be one non-negative whole number.")
    )
  }
  as.integer(value)
}

tempest_run_accessor_flag <- function(value, arg) {
  if (!is.logical(value) || length(value) != 1L || is.na(value)) {
    tempest_run_accessor_abort(
      paste0("`", arg, "` must be `TRUE` or `FALSE`.")
    )
  }
  value
}

tempest_run_accessor_string <- function(value, arg) {
  if (
    !is.character(value) ||
      length(value) != 1L ||
      is.na(value) ||
      !nzchar(tempest_trim(value))
  ) {
    tempest_run_accessor_abort(
      paste0("`", arg, "` must be one non-empty string.")
    )
  }
  tempest_trim(value)
}

tempest_run_accessor_call <- function(operation, callback) {
  tryCatch(
    callback(),
    error = function(error) {
      if (inherits(error, "tempest_run_error")) {
        stop(error)
      }
      tempest_run_accessor_abort(
        paste0("Tempest run operation `", operation, "` failed."),
        parent = error
      )
    }
  )
}

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
  if (!inherits(x, "TempestRun") && !inherits(x, "TempestSession")) {
    tempest_execution_events_abort(
      "{.arg x} must be a TempestRun or TempestSession."
    )
  }
  events <- x$events
  if (!is.list(events) || is.data.frame(events)) {
    tempest_execution_events_abort(
      "The execution contains an invalid event history."
    )
  }
  if (inherits(x, "TempestSession")) {
    events <- tryCatch(
      lapply(events, tempest_progress_event_record),
      error = function(error) {
        tempest_execution_events_abort(
          "TempestSession contains a malformed progress event.",
          parent = error
        )
      }
    )
  }
  events
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

#' Query events from a Tempest execution
#'
#' `r lifecycle::badge("experimental")`
#'
#' `tempest_execution_events()` gives host adapters one cursor-based event
#' query for generic `TempestRun` workflows and interactive [TempestSession]
#' sessions. It returns immutable list records rather than requiring callers to
#' reach into mutable R6 fields.
#'
#' Every record contains `event_id`, a positive execution-local `sequence`,
#' `run_id`, `event_type`, `status`, `timestamp`, and a serializable `payload`.
#' Generic-run records also contain `workflow_id` and step, attempt, expert,
#' artifact, and approval context. Co-STORM records contain `workflow`, `stage`,
#' `step`, parent, and correlation context.
#'
#' @param x A `TempestRun` or [TempestSession].
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

#' Inspect and control a generic Tempest run
#'
#' `r lifecycle::badge("experimental")`
#'
#' These functions provide host applications with stable access to mutable
#' `TempestRun` state without reaching into R6 fields or methods directly.
#' Event filtering preserves run-local sequence order. Approval controls remain
#' nonblocking: automatic resume stops again if another approval is required.
#'
#' **Host record contracts.**
#'
#' Run status is one of `pending`, `running`, `awaiting_approval`, `succeeded`,
#' `failed`, `cancel_requested`, `cancelled`, or `partially_recovered`.
#'
#' Each event record contains `event_id`, a positive run-local `sequence`,
#' `run_id`, `workflow_id`, `event_type`, `status`, `timestamp`, and a
#' serializable `payload`. Context fields `step_id`, `attempt`, `expert_id`,
#' `artifact_id`, `approval_id`, and `message` are `NULL` when they do not
#' apply.
#'
#' Approval records are named by `approval_id` and contain `approval_kind`
#' (`step` or `artifact`), `step_id`, `artifact_ids`, `status`, `reason`,
#' `policy_decision_id`, `requested_at`, `decided_at`, `note`, and serializable
#' `metadata`.
#'
#' Capability grants are named by step id. Each step record contains the
#' latest `attempt`, per-expert `experts` grants, step-level `step` grants,
#' `recorded_at`, and a named `attempts` history. Individual grants contain
#' capability and operation ids and versions, required and status flags,
#' connection reference ids, denial reason fields, and serializable metadata.
#'
#' @name tempest_run_accessors
#' @param run A `TempestRun` created by [tempest_run_workflow()] or restored by
#'   [tempest_run_restore()].
#' @return `tempest_run_status()` returns one run-status string.
NULL

#' @rdname tempest_run_accessors
#' @export
tempest_run_status <- function(run) {
  run <- tempest_run_accessor_validate(run)
  status <- run$status
  if (
    !is.character(status) ||
      length(status) != 1L ||
      is.na(status) ||
      !status %in% tempest_run_statuses()
  ) {
    tempest_run_accessor_abort(
      "TempestRun contains an invalid status."
    )
  }
  status
}

#' @rdname tempest_run_accessors
#' @param after_sequence Return only events whose sequence is greater than this
#'   non-negative run-local cursor.
#' @return `tempest_run_events()` returns an ordered list of generic event
#'   records.
#' @export
tempest_run_events <- function(run, after_sequence = 0L) {
  run <- tempest_run_accessor_validate(run)
  after_sequence <- tempest_run_accessor_count(
    after_sequence,
    "after_sequence"
  )
  tempest_execution_events(run, after_sequence = after_sequence)
}

#' @rdname tempest_run_accessors
#' @param status Optional approval status filter: `"pending"`, `"approved"`,
#'   `"rejected"`, or `"cancelled"`.
#' @return `tempest_run_approvals()` returns a named list of approval records.
#' @export
tempest_run_approvals <- function(run, status = NULL) {
  run <- tempest_run_accessor_validate(run)
  approvals <- run$approvals
  if (!is.list(approvals) || is.data.frame(approvals)) {
    tempest_run_accessor_abort(
      "TempestRun contains invalid approval state."
    )
  }
  if (is.null(status)) {
    return(approvals)
  }
  if (
    !is.character(status) ||
      length(status) == 0L ||
      anyNA(status) ||
      any(!status %in% c("pending", "approved", "rejected", "cancelled"))
  ) {
    tempest_run_accessor_abort(
      "{.arg status} must contain pending, approved, rejected, or cancelled."
    )
  }
  approval_statuses <- vapply(
    approvals,
    function(approval) {
      if (
        !is.list(approval) ||
          !is.character(approval$status) ||
          length(approval$status) != 1L ||
          is.na(approval$status) ||
          !approval$status %in%
            c("pending", "approved", "rejected", "cancelled")
      ) {
        return(NA_character_)
      }
      approval$status
    },
    character(1)
  )
  if (anyNA(approval_statuses)) {
    tempest_run_accessor_abort(
      "TempestRun contains a malformed approval record."
    )
  }
  approvals[approval_statuses %in% status]
}

#' @rdname tempest_run_accessors
#' @param ... Filters forwarded to the run's typed artifact catalog.
#' @return `tempest_run_artifacts()` returns typed artifact metadata records.
#' @export
tempest_run_artifacts <- function(run, ...) {
  run <- tempest_run_accessor_validate(run)
  tempest_run_accessor_call("artifacts", function() {
    run$artifacts(...)
  })
}

#' @rdname tempest_run_accessors
#' @return `tempest_run_capability_grants()` returns the latest grant records
#'   grouped by workflow step and expert, plus per-attempt grant history for
#'   retried steps.
#' @export
tempest_run_capability_grants <- function(run) {
  run <- tempest_run_accessor_validate(run)
  tryCatch(
    tempest_contract_serializable_list(
      run$capability_grants,
      "capability_grants"
    ),
    error = function(error) {
      tempest_run_accessor_abort(
        "TempestRun contains invalid capability grant records.",
        parent = error
      )
    }
  )
}

#' @rdname tempest_run_accessors
#' @param artifact_id Stable artifact identifier.
#' @return `tempest_run_artifact()` returns one typed artifact, including its
#'   inline content or external storage reference.
#' @export
tempest_run_artifact <- function(run, artifact_id) {
  run <- tempest_run_accessor_validate(run)
  artifact_id <- tempest_run_accessor_string(artifact_id, "artifact_id")
  tempest_run_accessor_call("artifact", function() {
    run$artifact(artifact_id)
  })
}

#' @rdname tempest_run_accessors
#' @param approval_id Stable approval-request identifier.
#' @param decision Either `"approved"` or `"rejected"`.
#' @param note Optional human-readable decision note.
#' @param metadata Canonical JSON-compatible decision metadata.
#' @param resume Whether to resume execution immediately after recording the
#'   decision.
#' @return Control functions return `run` invisibly.
#' @export
tempest_run_record_approval <- function(
  run,
  approval_id,
  decision = c("approved", "rejected"),
  note = NULL,
  metadata = list(),
  resume = TRUE
) {
  run <- tempest_run_accessor_validate(run)
  approval_id <- tempest_run_accessor_string(
    approval_id,
    "approval_id"
  )
  decision <- tryCatch(
    match.arg(decision, c("approved", "rejected")),
    error = function(error) {
      tempest_run_accessor_abort(
        "{.arg decision} must be approved or rejected.",
        parent = error
      )
    }
  )
  if (!is.null(note)) {
    note <- tempest_run_accessor_string(note, "note")
  }
  metadata <- tryCatch(
    tempest_contract_serializable_list(metadata, "metadata"),
    error = function(error) {
      tempest_run_accessor_abort(
        "{.arg metadata} must be canonical JSON-compatible.",
        parent = error
      )
    }
  )
  resume <- tempest_run_accessor_flag(resume, "resume")
  tempest_run_accessor_call("record_approval", function() {
    run$record_approval(
      approval_id = approval_id,
      decision = decision,
      note = note,
      metadata = metadata
    )
  })
  if (resume) {
    tempest_run_accessor_call("resume", run$resume)
  }
  invisible(run)
}

#' @rdname tempest_run_accessors
#' @param reason Human-readable cancellation reason.
#' @export
tempest_run_request_cancel <- function(
  run,
  reason = "Cancellation requested."
) {
  run <- tempest_run_accessor_validate(run)
  reason <- tempest_run_accessor_string(reason, "reason")
  tempest_run_accessor_call("cancel", function() {
    run$cancel(reason)
  })
  invisible(run)
}
