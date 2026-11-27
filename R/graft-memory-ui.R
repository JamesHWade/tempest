# Shared Graft project-memory review UI and server action primitives
# Used by both tempest_researcher_app() and the bundled tempest_app().

tempest_graft_memory_status_badge <- function(status, stream = NULL) {
  label <- switch(
    status %||% "provisional",
    accepted = if (nzchar(stream %||% "")) {
      paste("Accepted \u00b7", stream)
    } else {
      "Accepted"
    },
    superseded = if (nzchar(stream %||% "")) {
      paste("Superseded \u00b7", stream)
    } else {
      "Superseded"
    },
    withdrawn = if (nzchar(stream %||% "")) {
      paste("Withdrawn \u00b7", stream)
    } else {
      "Withdrawn"
    },
    provisional = "Provisional draft",
    unknown = "Memory status unavailable",
    "Not published to Graft"
  )
  tone <- switch(
    status %||% "provisional",
    accepted = "bg-success-subtle text-success-emphasis",
    superseded = "bg-warning-subtle text-warning-emphasis",
    withdrawn = "bg-secondary-subtle text-secondary-emphasis",
    provisional = "bg-info-subtle text-info-emphasis",
    unknown = "bg-warning-subtle text-warning-emphasis",
    "bg-light text-muted"
  )
  shiny::tags$span(
    class = paste("badge rounded-pill", tone),
    label
  )
}

tempest_graft_memory_expected_head <- function(streams_df, state, stream) {
  match_idx <- which(streams_df$stream == stream)
  if (length(match_idx) > 0L) {
    streams_df$decision_id[[match_idx[[1L]]]]
  } else if (
    !is.null(state) &&
      identical(state$stream, stream) &&
      rlang::is_string(state$decision_id)
  ) {
    state$decision_id
  } else {
    ""
  }
}

tempest_graft_memory_resolve_stream <- function(input) {
  existing <- trimws(input$existing_stream %||% "")
  if (nzchar(existing)) {
    existing
  } else {
    trimws(input$stream %||% "")
  }
}

tempest_graft_memory_handle_accept <- function(
  input,
  store,
  purpose,
  default_actor,
  streams_df,
  state,
  resolve_selection,
  feedback,
  feedback_is_error,
  memory_revision,
  on_accepted = NULL
) {
  if (!isTRUE(input$reviewed)) {
    feedback("Confirm that you reviewed the report and its cited evidence.")
    feedback_is_error(TRUE)
    return(invisible(FALSE))
  }
  stream <- tempest_graft_memory_resolve_stream(input)
  expected_head <- tempest_graft_memory_expected_head(streams_df, state, stream)
  tryCatch(
    {
      selection_id <- resolve_selection()
      decision <- tempest_graft_accept_selection(
        store = store,
        selection_id = selection_id,
        stream = stream,
        actor = input$actor %||% default_actor,
        reason = input$reason %||% "",
        purpose = purpose,
        expected = expected_head
      )
      if (is.function(on_accepted)) {
        on_accepted(decision)
      }
      feedback(sprintf(
        "Accepted into stream '%s' (decision %s).",
        decision@stream,
        substr(decision@id, 1L, 12L)
      ))
      feedback_is_error(FALSE)
      memory_revision(shiny::isolate(memory_revision()) + 1L)
      invisible(TRUE)
    },
    error = function(error) {
      feedback(conditionMessage(error))
      feedback_is_error(TRUE)
      invisible(FALSE)
    }
  )
}

tempest_graft_memory_handle_withdraw <- function(
  input,
  store,
  default_actor,
  state,
  feedback,
  feedback_is_error,
  memory_revision,
  success_format = "Withdrew stream '%s' from active memory (decision %s)."
) {
  if (is.null(state) || !identical(state$status, "accepted")) {
    feedback("Only a currently accepted stream can be withdrawn.")
    feedback_is_error(TRUE)
    return(invisible(FALSE))
  }
  tryCatch(
    {
      decision <- tempest_graft_withdraw_stream(
        store = store,
        stream = state$stream,
        actor = input$actor %||% default_actor,
        reason = input$reason %||% "Withdrawn after review",
        expected = state$decision_id
      )
      feedback(sprintf(
        success_format,
        decision@stream,
        substr(decision@id, 1L, 12L)
      ))
      feedback_is_error(FALSE)
      memory_revision(shiny::isolate(memory_revision()) + 1L)
      invisible(TRUE)
    },
    error = function(error) {
      feedback(conditionMessage(error))
      feedback_is_error(TRUE)
      invisible(FALSE)
    }
  )
}

tempest_graft_memory_history_ui <- function(state) {
  if (is.null(state) || nrow(state$history) == 0L) {
    return(NULL)
  }
  shiny::tags$details(
    class = "mt-2",
    shiny::tags$summary(sprintf(
      "Decision history (%d)",
      nrow(state$history)
    )),
    shiny::tags$table(
      class = "table table-sm mt-2 mb-0",
      shiny::tags$thead(shiny::tags$tr(
        shiny::tags$th("#"),
        shiny::tags$th("Action"),
        shiny::tags$th("Reviewer"),
        shiny::tags$th("Reason"),
        shiny::tags$th("Decision")
      )),
      shiny::tags$tbody(lapply(
        seq_len(nrow(state$history)),
        function(i) {
          row <- state$history[i, , drop = FALSE]
          shiny::tags$tr(
            shiny::tags$td(row$sequence),
            shiny::tags$td(row$action),
            shiny::tags$td(row$actor),
            shiny::tags$td(row$reason),
            shiny::tags$td(shiny::tags$code(substr(
              row$decision_id,
              1L,
              12L
            )))
          )
        }
      ))
    )
  )
}

tempest_graft_memory_notice_ui <- function(feedback_text, is_error) {
  if (is.null(feedback_text)) {
    return(NULL)
  }
  shiny::div(
    class = if (isTRUE(is_error)) {
      "alert alert-warning py-2 px-3 mt-2 mb-0"
    } else {
      "alert alert-success py-2 px-3 mt-2 mb-0"
    },
    role = if (isTRUE(is_error)) "alert" else "status",
    feedback_text
  )
}

tempest_graft_memory_review_card_ui <- function(
  ns,
  heading,
  status_code,
  stream,
  existing_streams,
  default_stream,
  default_actor,
  feedback_text,
  feedback_is_error,
  state,
  card_class,
  card_tag = c("div", "section"),
  aria_label = NULL,
  intro_text = NULL,
  summary_new = "Review and accept into project memory",
  accept_label = "Accept to project memory",
  open_when_unaccepted = FALSE
) {
  card_tag <- match.arg(card_tag)
  existing_choices <- c(
    "Create new stream below" = "",
    stats::setNames(existing_streams, existing_streams)
  )
  selected_existing <- if (default_stream %in% existing_streams) {
    default_stream
  } else {
    ""
  }
  is_accepted <- identical(status_code, "accepted")
  body <- shiny::div(
    class = "card-body",
    shiny::div(
      class = "d-flex flex-wrap justify-content-between align-items-center gap-2 mb-2",
      shiny::tags$strong(heading),
      tempest_graft_memory_status_badge(status_code, stream)
    ),
    if (!is.null(intro_text)) {
      shiny::p(class = "small text-body-secondary mb-2", intro_text)
    },
    shiny::tags$details(
      open = if (isTRUE(open_when_unaccepted) && !is_accepted) {
        "open"
      } else {
        NULL
      },
      shiny::tags$summary(
        if (is_accepted) {
          "Update or withdraw accepted memory"
        } else {
          summary_new
        }
      ),
      shiny::div(
        class = "mt-2 d-grid gap-2",
        if (length(existing_streams) > 0L) {
          shiny::selectInput(
            ns("existing_stream"),
            "Update an existing stream (optional)",
            choices = existing_choices,
            selected = selected_existing
          )
        },
        shiny::textInput(
          ns("stream"),
          "New stream name",
          value = default_stream
        ),
        shiny::textInput(
          ns("actor"),
          "Reviewer",
          value = default_actor
        ),
        shiny::textAreaInput(
          ns("reason"),
          "Review reason",
          placeholder = "Why is this finding ready to remember (or withdraw)?",
          rows = 2
        ),
        shiny::checkboxInput(
          ns("reviewed"),
          "I reviewed this report and its cited evidence",
          value = FALSE
        ),
        shiny::div(
          class = "d-flex flex-wrap gap-2",
          shiny::actionButton(
            ns("accept"),
            accept_label,
            class = "btn-primary btn-sm"
          ),
          if (is_accepted) {
            shiny::actionButton(
              ns("withdraw"),
              "Withdraw from active memory",
              class = "btn-outline-secondary btn-sm"
            )
          },
          shiny::actionButton(
            ns("refresh"),
            "Refresh status",
            class = "btn-outline-secondary btn-sm"
          )
        )
      )
    ),
    tempest_graft_memory_notice_ui(feedback_text, feedback_is_error),
    tempest_graft_memory_history_ui(state)
  )
  if (identical(card_tag, "section")) {
    shiny::tags$section(class = card_class, `aria-label` = aria_label, body)
  } else {
    shiny::div(class = card_class, body)
  }
}
