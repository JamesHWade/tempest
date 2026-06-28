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
