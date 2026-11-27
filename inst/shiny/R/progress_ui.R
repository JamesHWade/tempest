# Live workflow progress status UI helpers.

workflow_progress_ui <- function(state, stage_labels = NULL) {
  if (is.null(state) || is.na(state$workflow)) {
    return(NULL)
  }
  status <- workflow_display_status(state)
  shiny::div(
    class = "tempest-progress py-2",
    shiny::div(
      class = "d-flex flex-wrap align-items-center gap-2 mb-2",
      shiny::span(
        class = paste(
          "badge rounded-pill",
          workflow_status_class(status)
        ),
        workflow_status_label(status)
      ),
      shiny::strong(workflow_title(state$workflow)),
      workflow_current_label(state)
    ),
    workflow_stage_list(state, stage_labels),
    workflow_activity_list(state),
    workflow_failure_list(state),
    workflow_artifact_list(state)
  )
}

workflow_display_status <- function(state) {
  if (
    identical(state$workflow, "costorm") &&
      identical(state$status, "running") &&
      !workflow_state_has_active(state)
  ) {
    return("ready")
  }
  state$status
}

workflow_state_has_active <- function(state) {
  active <- c(
    state$active$stages,
    state$active$steps,
    state$active$experts,
    state$active$tools
  )
  length(active) > 0L
}

workflow_status_label <- function(status) {
  switch(
    status %||% "pending",
    pending = "Queued",
    ready = "Ready",
    running = "Running",
    succeeded = "Done",
    failed = "Failed",
    cancelled = "Cancelled",
    stringi::stri_trans_totitle(status %||% "Pending")
  )
}

workflow_status_class <- function(status) {
  switch(
    status %||% "pending",
    pending = "text-bg-secondary",
    ready = "text-bg-secondary",
    running = "text-bg-primary",
    succeeded = "text-bg-success",
    failed = "text-bg-danger",
    cancelled = "text-bg-warning",
    "text-bg-secondary"
  )
}

workflow_title <- function(workflow) {
  switch(
    workflow %||% "",
    storm = "STORM",
    costorm = "Co-STORM",
    workflow %||% "Workflow"
  )
}

workflow_current_label <- function(state) {
  stage_labels <- workflow_labels(state$workflow, "stage")
  step_labels <- workflow_labels(state$workflow, "step")
  current <- workflow_stage_label(state$current_stage, stage_labels)
  step <- workflow_stage_label(state$current_step, step_labels)
  if (!is.na(step) && nzchar(step)) {
    current <- if (!is.na(current) && nzchar(current)) {
      paste(current, step, sep = " / ")
    } else {
      step
    }
  }
  if (is.na(current) || !nzchar(current)) {
    return(NULL)
  }
  shiny::span(class = "small text-muted", paste("Current:", current))
}

workflow_stage_list <- function(state, stage_labels = NULL) {
  if (is.null(stage_labels) || length(stage_labels) == 0L) {
    return(NULL)
  }
  items <- lapply(names(stage_labels), function(stage) {
    status <- workflow_stage_status(state, stage)
    shiny::span(
      class = paste("tempest-progress-stage", workflow_stage_class(status)),
      shiny::span(class = "me-1", workflow_stage_icon(status)),
      stage_labels[[stage]]
    )
  })
  shiny::div(class = "d-flex flex-wrap gap-2 small", items)
}

workflow_stage_status <- function(state, stage) {
  if (stage %in% state$completed_stages) {
    return("succeeded")
  }
  if (stage %in% state$skipped_stages) {
    return("skipped")
  }
  if (identical(state$current_stage, stage)) {
    return("running")
  }
  if (length(state$failures) > 0L) {
    failed <- vapply(
      state$failures,
      function(failure) identical(failure$stage, stage),
      logical(1)
    )
    if (any(failed)) {
      return("failed")
    }
  }
  "pending"
}

workflow_stage_class <- function(status) {
  switch(
    status,
    succeeded = "text-success",
    skipped = "text-secondary",
    running = "text-primary fw-semibold",
    failed = "text-danger fw-semibold",
    "text-muted"
  )
}

workflow_stage_icon <- function(status) {
  switch(
    status,
    succeeded = shiny::icon("circle-check"),
    skipped = shiny::icon("circle-minus"),
    running = shiny::icon("spinner", class = "fa-spin"),
    failed = shiny::icon("triangle-exclamation"),
    shiny::icon("circle")
  )
}

workflow_activity_list <- function(state) {
  active <- c(state$active$experts, state$active$tools, state$active$steps)
  if (length(active) == 0L) {
    return(NULL)
  }
  items <- lapply(
    active,
    function(item) workflow_activity_item(item, state$workflow)
  )
  items <- Filter(Negate(is.null), items)
  if (length(items) == 0L) {
    return(NULL)
  }
  shiny::div(
    class = "small text-muted mt-2 tempest-progress-activity",
    shiny::icon("arrows-rotate", class = "me-1"),
    shiny::span(class = "visually-hidden", "Active work:"),
    shiny::div(
      class = "d-inline-flex flex-wrap align-items-center gap-2",
      items
    )
  )
}

workflow_activity_item <- function(item, workflow = NULL) {
  label <- workflow_activity_label(item, workflow)
  if (is.na(label) || !nzchar(label)) {
    return(NULL)
  }
  expert_name <- item$expert_name %||% NA_character_
  expert_id <- item$expert_id %||% NA_character_
  has_expert <- workflow_has_text(expert_name) || workflow_has_text(expert_id)
  shiny::span(
    class = "tempest-activity-item",
    if (has_expert) {
      persona_icon(expert_name, expert_id, size = "sm")
    },
    shiny::span(class = "tempest-activity-label", label)
  )
}

workflow_activity_label <- function(item, workflow = NULL) {
  step_labels <- workflow_labels(workflow, "step")
  expert_name <- item$expert_name %||% NA_character_
  label <- if (!is.na(expert_name) && nzchar(expert_name)) {
    expert_name
  } else {
    workflow_first_text(item$step, item$stage)
  }
  label <- workflow_stage_label(label, step_labels)
  if (is.na(label) || !nzchar(label)) {
    return("")
  }
  if (identical(item$event_type, "tool") && !is.na(item$step)) {
    paste0(label, " running")
  } else {
    label
  }
}

workflow_has_text <- function(value) {
  !is.na(workflow_first_text(value)) && nzchar(workflow_first_text(value))
}

workflow_failure_list <- function(state) {
  if (length(state$failures) == 0L) {
    return(NULL)
  }
  # Don't surface recorded transient failures once the workflow has no active
  # work; the reducer keeps tool/expert failures in `state$failures`, which
  # would otherwise make a ready session look failed or stuck.
  if (
    identical(state$status, "succeeded") ||
      (!identical(state$status, "failed") && !workflow_state_has_active(state))
  ) {
    return(NULL)
  }
  latest <- state$failures[[length(state$failures)]]
  msg <- workflow_first_text(
    latest$error_message,
    latest$message,
    "Workflow failed."
  )
  shiny::div(
    class = "small text-danger mt-2",
    shiny::icon("triangle-exclamation", class = "me-1"),
    msg
  )
}

workflow_artifact_list <- function(state) {
  if (length(state$artifacts) == 0L) {
    return(NULL)
  }
  labels <- vapply(
    state$artifacts,
    function(artifact) workflow_stage_label(artifact$artifact),
    character(1)
  )
  shiny::div(
    class = "small text-success mt-2",
    shiny::icon("file-lines", class = "me-1"),
    paste("Available:", paste(labels, collapse = ", "))
  )
}

workflow_stage_label <- function(value, labels = NULL) {
  if (is.null(value) || length(value) == 0L || is.na(value)) {
    return(NA_character_)
  }
  value <- as.character(value[[1]])
  if (!is.null(labels) && value %in% names(labels)) {
    return(labels[[value]])
  }
  value <- gsub("_", " ", value)
  stringi::stri_trans_totitle(value)
}

workflow_labels <- function(workflow, kind = c("stage", "step")) {
  kind <- match.arg(kind)
  if (is.null(workflow) || length(workflow) == 0L) {
    return(NULL)
  }
  workflow <- workflow[[1]]
  if (is.na(workflow) || !workflow %in% c("storm", "costorm")) {
    return(NULL)
  }
  tryCatch(
    tempest:::tempest_progress_labels(workflow, kind = kind),
    error = function(e) NULL
  )
}

workflow_first_text <- function(...) {
  values <- list(...)
  for (value in values) {
    if (is.null(value) || length(value) == 0L) {
      next
    }
    value <- as.character(value[[1]])
    if (!is.na(value) && nzchar(value)) {
      return(value)
    }
  }
  ""
}
