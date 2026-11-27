# Run review view helpers, HTML tables, and trajectory display wrappers.

run_review_empty_state_ui <- function(icon_name, heading, message) {
  shiny::div(
    class = "tempest-scripted-empty-state tempest-review-empty-state",
    shiny::div(
      class = "tempest-scripted-empty-icon",
      shiny::icon(icon_name, `aria-hidden` = "true")
    ),
    shiny::h2(class = "h5 mb-2", heading),
    shiny::p(class = "mb-0", message)
  )
}

run_review_inline_empty_state_ui <- function(message) {
  shiny::div(
    class = "tempest-review-inline-empty text-body-secondary",
    shiny::icon("inbox", class = "me-2", `aria-hidden` = "true"),
    shiny::span(message)
  )
}

run_review_prop <- function(value, field) {
  if (is.null(value)) {
    return(NULL)
  }
  tryCatch(
    S7::prop(value, field),
    error = function(error) {
      if (is.list(value)) value[[field]] else NULL
    }
  )
}

run_review_field <- function(value, field) {
  candidate <- run_review_prop(value, field)
  if (
    is.null(candidate) ||
      length(candidate) != 1L ||
      !(is.character(candidate) ||
        is.numeric(candidate) ||
        is.logical(candidate)) ||
      is.na(candidate)
  ) {
    return("")
  }
  text <- as.character(candidate)
  if (!nzchar(text) || run_review_sensitive_text(text)) {
    return("")
  }
  substr(enc2utf8(text), 1L, 512L)
}

run_review_sensitive_text <- function(value) {
  credential_like <- grepl(
    paste0(
      "(?i)(authorization\\s*:|bearer\\s+|api[_ -]?key|",
      "access[_ -]?token|password|credential|client[_ -]?secret|sk-[a-z0-9])"
    ),
    value,
    perl = TRUE
  )
  path_like <- grepl(
    "^(?:/|~[/\\\\]|[A-Za-z]:[/\\\\]|file://|\\\\\\\\|\\.\\.?[/\\\\])",
    value,
    perl = TRUE
  )
  isTRUE(credential_like || path_like)
}

run_review_empty_lane <- function() {
  list(total = 0L, retained = 0L, omitted = 0L, digest = "", items = list())
}

run_review_lane <- function(value) {
  if (!is.list(value)) {
    return(run_review_empty_lane())
  }
  items <- value$items
  if (!is.list(items) || is.data.frame(items)) {
    items <- list()
  }
  visible <- utils::head(items, 250L)
  count <- function(field, fallback) {
    candidate <- value[[field]]
    if (
      !is.numeric(candidate) ||
        length(candidate) != 1L ||
        is.na(candidate) ||
        !is.finite(candidate) ||
        candidate < 0L
    ) {
      return(as.integer(fallback))
    }
    as.integer(candidate)
  }
  total <- count("total", length(items))
  retained <- count("retained", min(length(items), 250L))
  omitted <- count("omitted", max(0L, total - length(visible)))
  list(
    total = max(total, length(visible)),
    retained = min(retained, 250L),
    omitted = max(omitted, total - length(visible)),
    digest = run_review_field(value, "digest"),
    items = visible
  )
}

run_review_label <- function(value) {
  tools::toTitleCase(gsub("_", " ", value, fixed = TRUE))
}

run_review_mode_label <- function(value) {
  switch(
    value,
    storm = "STORM",
    costorm = "Co-STORM",
    run_review_label(value)
  )
}

run_review_short_id <- function(value) {
  if (!nzchar(value) || nchar(value) <= 18L) {
    return(value)
  }
  paste0(
    substr(value, 1L, 9L),
    "…",
    substr(value, nchar(value) - 6L, nchar(value))
  )
}

run_review_stage_choice_label <- function(stage) {
  paste0(
    run_review_label(run_review_field(stage, "stage")),
    " — ",
    run_review_short_id(run_review_field(stage, "attempt_id"))
  )
}

run_review_finding_reference_ids <- function(findings) {
  unique(vapply(findings, run_review_field, character(1), field = "ref_id"))
}

run_review_stage_has_attention <- function(stage, attention_ids) {
  any(
    c(
      run_review_field(stage, "attempt_id"),
      run_review_field(stage, "trace_id"),
      run_review_field(stage, "stage")
    ) %in%
      attention_ids
  )
}

run_review_join_matched_fields <- function(join) {
  tempest:::tempest_trajectory_join_matched_fields(join)
}

run_review_authoritative_agent_indices <- function(stage, joins, agents) {
  tempest:::tempest_trajectory_authoritative_agent_indices(stage, joins, agents)
}

run_review_status_icon <- function(status) {
  switch(
    status,
    succeeded = "circle-check",
    failed = "circle-xmark",
    cancelled = "ban",
    running = "spinner",
    "circle-info"
  )
}

run_review_summary_ui <- function(
  product,
  review_id,
  stage_lane,
  finding_lane,
  errors,
  warnings,
  infos
) {
  status <- run_review_field(product, "status")
  mode <- run_review_field(product, "mode")
  run_id <- run_review_field(product, "research_run_id")
  report_reference <- run_review_prop(product, "report_reference")
  shiny::div(
    class = "tempest-review-summary d-flex flex-column gap-3",
    shiny::div(
      class = "d-flex flex-wrap gap-3 align-items-start justify-content-between",
      shiny::div(
        shiny::h2(
          class = "h4 mb-1",
          paste(run_review_mode_label(mode), "run review")
        ),
        shiny::tags$code(class = "text-break small", run_id)
      ),
      shiny::span(
        class = paste(
          "badge rounded-pill",
          if (status %in% c("succeeded", "completed")) {
            "text-bg-success"
          } else if (status %in% c("failed", "cancelled")) {
            "text-bg-danger"
          } else {
            "text-bg-secondary"
          }
        ),
        shiny::icon(run_review_status_icon(status), class = "me-1"),
        run_review_label(status)
      )
    ),
    shiny::tags$details(
      class = "tempest-review-identifiers",
      shiny::tags$summary("Record identifiers"),
      shiny::div(
        class = "tempest-review-summary-meta d-flex flex-wrap gap-2",
        shiny::div(
          class = "tempest-review-meta-item",
          shiny::span(class = "text-body-secondary", "Review:"),
          shiny::tags$code(class = "text-break", review_id)
        ),
        shiny::div(
          class = "tempest-review-meta-item",
          shiny::span(class = "text-body-secondary", "Config:"),
          shiny::tags$code(
            class = "text-break",
            run_review_field(product, "config_digest")
          )
        ),
        shiny::div(
          class = "tempest-review-meta-item",
          shiny::span(class = "text-body-secondary", "Report:"),
          shiny::tags$code(
            class = "text-break",
            run_review_field(report_reference, "sha256")
          )
        )
      ),
    ),
    shiny::div(
      class = "tempest-review-summary-copy text-body-secondary",
      sprintf(
        paste0(
          "%d authoritative stages retained; %d omitted. ",
          "%d structural findings retained; %d omitted. ",
          "Among retained findings, %d errors and %d warnings need ",
          "attention; %d are informational."
        ),
        stage_lane$retained,
        stage_lane$omitted,
        finding_lane$retained,
        finding_lane$omitted,
        errors,
        warnings,
        infos
      )
    )
  )
}

run_review_stage_table_ui <- function(stages, findings, lane) {
  if (length(stages) == 0L) {
    return(run_review_inline_empty_state_ui(
      "No StageRecord rows match the current filters."
    ))
  }
  attention_ids <- run_review_finding_reference_ids(findings)
  rows <- lapply(stages, function(stage) {
    status <- run_review_field(stage, "status")
    shiny::tags$tr(
      shiny::tags$td(run_review_label(run_review_field(stage, "stage"))),
      shiny::tags$td(
        shiny::icon(run_review_status_icon(status)),
        " ",
        run_review_label(status)
      ),
      shiny::tags$td(run_review_label(run_review_field(
        stage,
        "execution_path"
      ))),
      shiny::tags$td(run_review_label(run_review_field(
        stage,
        "support_status"
      ))),
      shiny::tags$td(
        if (
          identical(
            run_review_field(stage, "publication_allowed"),
            "TRUE"
          )
        ) {
          "Allowed"
        } else {
          "Blocked"
        }
      ),
      shiny::tags$td(
        if (
          run_review_stage_has_attention(
            stage,
            attention_ids
          )
        ) {
          "Needs attention"
        } else {
          "No retained finding"
        }
      ),
      shiny::tags$td(shiny::tags$code(
        run_review_field(stage, "attempt_id")
      ))
    )
  })
  shiny::tagList(
    shiny::div(
      class = "table-responsive tempest-review-table-wrap",
      shiny::tags$table(
        class = "table table-sm table-hover align-middle mb-1 tempest-review-table",
        `aria-label` = "Authoritative StageRecord trajectory",
        shiny::tags$caption(
          class = "visually-hidden",
          "Authoritative StageRecord trajectory; use the Stage details control to inspect references."
        ),
        shiny::tags$thead(shiny::tags$tr(
          shiny::tags$th(scope = "col", "Stage"),
          shiny::tags$th(scope = "col", "Status"),
          shiny::tags$th(scope = "col", "Execution"),
          shiny::tags$th(scope = "col", "Support"),
          shiny::tags$th(scope = "col", "Publication"),
          shiny::tags$th(scope = "col", "Attention"),
          shiny::tags$th(scope = "col", "Attempt reference")
        )),
        shiny::tags$tbody(rows)
      )
    ),
    shiny::p(
      class = "small text-body-secondary mb-0",
      sprintf(
        "Showing %d filtered rows from %d retained records; %d complete-projection rows omitted.",
        length(stages),
        lane$retained,
        lane$omitted
      )
    )
  )
}

run_review_definition_list <- function(values) {
  entries <- unlist(
    lapply(names(values), function(label) {
      value <- values[[label]]
      if (!nzchar(value)) {
        value <- "Not recorded"
      }
      list(
        shiny::tags$dt(class = "col-sm-4", label),
        shiny::tags$dd(
          class = "col-sm-8",
          shiny::tags$code(class = "text-break", value)
        )
      )
    }),
    recursive = FALSE
  )
  do.call(
    shiny::tags$dl,
    c(list(class = "row mb-0 tempest-review-definitions"), entries)
  )
}

run_review_stage_detail_ui <- function(stage, linked_agents, agent_lane) {
  output <- run_review_prop(stage, "output")
  shiny::tagList(
    run_review_definition_list(c(
      Stage = run_review_label(run_review_field(stage, "stage")),
      Status = run_review_label(run_review_field(stage, "status")),
      `Attempt id` = run_review_field(stage, "attempt_id"),
      `Trace id` = run_review_field(stage, "trace_id"),
      `Program artifact id` = run_review_field(stage, "program_artifact_id"),
      `Governed procedure revision` = run_review_field(
        stage,
        "governed_procedure_revision_id"
      ),
      `Started at` = run_review_field(stage, "started_at"),
      `Completed at` = run_review_field(stage, "completed_at"),
      `Output kind` = run_review_field(output, "kind"),
      `Output count` = run_review_field(output, "count"),
      `Output digest` = run_review_field(output, "digest")
    )),
    shiny::tags$hr(),
    shiny::tags$h3(
      class = "h6 mb-2",
      shiny::icon("link", class = "me-2"),
      "Exactly matched untimed Deputy references"
    ),
    shiny::p(
      class = "small text-body-secondary",
      paste(
        "These terminal references have an authority-validated execution join",
        "from this StageRecord's attempt to one exact Deputy run and session.",
        "They are not placed in chronological order."
      )
    ),
    run_review_agent_table_ui(
      linked_agents,
      paste(
        "Untimed Deputy references authority-validated against the selected",
        "StageRecord"
      ),
      agent_lane
    )
  )
}

run_review_agent_table_ui <- function(agents, label, lane) {
  table <- if (length(agents) == 0L) {
    run_review_inline_empty_state_ui(
      "No untimed Deputy references are present in this section."
    )
  } else {
    rows <- lapply(agents, function(agent) {
      shiny::tags$tr(
        shiny::tags$td(run_review_label(run_review_field(agent, "status"))),
        shiny::tags$td(shiny::tags$code(run_review_field(agent, "trace_id"))),
        shiny::tags$td(shiny::tags$code(
          run_review_field(agent, "deputy_run_id")
        )),
        shiny::tags$td(shiny::tags$code(
          run_review_field(agent, "deputy_session_id")
        )),
        shiny::tags$td(shiny::tags$code(
          run_review_field(agent, "parent_run_id")
        )),
        shiny::tags$td(shiny::tags$code(
          run_review_field(agent, "delegation_id")
        )),
        shiny::tags$td(shiny::tags$code(
          run_review_field(agent, "tool_call_id")
        ))
      )
    })
    shiny::div(
      class = "table-responsive tempest-review-table-wrap",
      shiny::tags$table(
        class = "table table-sm table-hover align-middle mb-0 tempest-review-table",
        `aria-label` = label,
        shiny::tags$caption(class = "visually-hidden", label),
        shiny::tags$thead(shiny::tags$tr(
          shiny::tags$th(scope = "col", "Terminal status"),
          shiny::tags$th(scope = "col", "Trace reference"),
          shiny::tags$th(scope = "col", "Deputy run"),
          shiny::tags$th(scope = "col", "Deputy session"),
          shiny::tags$th(scope = "col", "Parent run"),
          shiny::tags$th(scope = "col", "Delegation"),
          shiny::tags$th(scope = "col", "Tool call")
        )),
        shiny::tags$tbody(rows)
      )
    )
  }
  shiny::tagList(
    table,
    shiny::p(
      class = "small text-body-secondary mb-0",
      sprintf(
        paste0(
          "Showing %d rows in this section from %d retained Deputy ",
          "references; %d complete-projection rows omitted."
        ),
        length(agents),
        lane$retained,
        lane$omitted
      )
    )
  )
}

run_review_progress_record <- function(event) {
  tempest:::tempest_trajectory_progress_record(event)
}

run_review_event_records <- function(events) {
  tempest:::tempest_trajectory_progress_records(events, max_rows = 250L)
}

run_review_event_key <- function(event) {
  event_id <- run_review_field(event, "event_id")
  if (nzchar(event_id)) {
    return(event_id)
  }
  paste(
    run_review_field(event, "run_id"),
    run_review_field(event, "event_type"),
    run_review_field(event, "status"),
    run_review_field(event, "timestamp"),
    sep = "|"
  )
}

run_review_progress_ui <- function(events, product_id) {
  if (length(events) == 0L) {
    return(run_review_inline_empty_state_ui(
      "No live progress observations are currently available."
    ))
  }
  rows <- lapply(events, function(event) {
    run_id <- run_review_field(event, "run_id")
    status <- run_review_field(event, "status")
    shiny::tags$tr(
      shiny::tags$td(run_review_label(run_review_field(event, "event_type"))),
      shiny::tags$td(run_review_label(run_review_field(event, "stage"))),
      shiny::tags$td(run_review_label(run_review_field(event, "step"))),
      shiny::tags$td(
        shiny::icon(run_review_status_icon(status)),
        " ",
        run_review_label(status)
      ),
      shiny::tags$td(run_review_field(event, "timestamp")),
      shiny::tags$td(
        if (
          nzchar(product_id) &&
            identical(
              run_id,
              product_id
            )
        ) {
          "Same run id"
        } else {
          "Not authority-linked"
        }
      )
    )
  })
  shiny::tagList(
    shiny::div(
      class = "tempest-review-live-boundary d-flex align-items-start gap-2",
      shiny::icon("eye", class = "mt-1", `aria-hidden` = "true"),
      shiny::p(
        class = "small text-body-secondary mb-0",
        paste(
          "Mutable progress is shown only as an untrusted observation.",
          "It cannot change the review id, findings, joins, or persistence."
        )
      )
    ),
    shiny::div(
      class = "table-responsive tempest-review-table-wrap",
      shiny::tags$table(
        class = "table table-sm table-hover align-middle mb-1 tempest-review-table",
        `aria-label` = "Untrusted live progress observations",
        shiny::tags$caption(
          class = "visually-hidden",
          "Untrusted live progress observations outside the authoritative trajectory review."
        ),
        shiny::tags$thead(shiny::tags$tr(
          shiny::tags$th(scope = "col", "Type"),
          shiny::tags$th(scope = "col", "Stage"),
          shiny::tags$th(scope = "col", "Step"),
          shiny::tags$th(scope = "col", "Status"),
          shiny::tags$th(scope = "col", "Observed at"),
          shiny::tags$th(scope = "col", "Relationship")
        )),
        shiny::tags$tbody(rows)
      )
    ),
    shiny::p(
      class = "small text-body-secondary mb-0",
      sprintf(
        "Showing %d of %d live rows; %d omitted above the 250-row UI cap.",
        length(events),
        attr(events, "total") %||% length(events),
        attr(events, "omitted") %||% 0L
      )
    )
  )
}
