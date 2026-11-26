# Run review tab: project a completed product through the bounded trajectory
# review contract. Mutable progress stays beside, but outside, that projection.

mod_run_review_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::nav_panel(
    title = shiny::tagList(shiny::icon("route"), "Run review"),
    value = "Run review",
    bslib::layout_sidebar(
      class = "tempest-scripted-layout tempest-review-layout",
      sidebar = bslib::sidebar(
        title = "Trajectory review",
        width = 320,
        class = "tempest-scripted-sidebar",
        bslib::card(
          class = "tempest-review-filter-card",
          bslib::card_header(
            shiny::span(
              shiny::icon("filter", class = "me-2"),
              "Source and filters"
            )
          ),
          bslib::card_body(
            shiny::selectInput(
              ns("product_source"),
              "Research product",
              choices = character()
            ),
            shiny::helpText(
              "Choose a completed STORM run or Co-STORM session."
            ),
            shiny::selectInput(
              ns("stage_filter"),
              "Stage",
              choices = c("All stages" = "")
            ),
            shiny::selectInput(
              ns("status_filter"),
              "Status",
              choices = c("All statuses" = "")
            ),
            shiny::checkboxInput(
              ns("attention_only"),
              "Only stages with retained findings",
              value = FALSE
            ),
            shiny::selectInput(
              ns("stage_detail"),
              "Stage details",
              choices = character()
            ),
            shiny::helpText(
              "Select an attempt to inspect its fixed artifact and authority references."
            )
          )
        ),
        bslib::card(
          class = "tempest-review-boundary-card",
          bslib::card_header(
            shiny::span(
              shiny::icon("shield-halved", class = "me-2"),
              "Evidence boundary"
            )
          ),
          bslib::card_body(
            shiny::p(
              class = "small mb-2",
              "Recorded StageRecords and their exact joins are authoritative."
            ),
            shiny::p(
              class = "small text-body-secondary mb-0",
              "Live progress is retained separately as an untrusted observation."
            )
          )
        )
      ),
      shiny::div(
        class = "tempest-review-content d-flex flex-column gap-3",
        shiny::uiOutput(ns("new_failure_alert")),
        shiny::div(
          class = "tempest-review-summary-region",
          role = "status",
          `aria-live` = "polite",
          `aria-atomic` = "true",
          shiny::uiOutput(ns("summary"))
        ),
        bslib::layout_columns(
          col_widths = c(7, 5),
          gap = "1rem",
          class = "tempest-review-primary-grid",
          bslib::card(
            class = "tempest-review-card tempest-review-stage-card",
            bslib::card_header(
              class = "tempest-scripted-card-header",
              shiny::div(
                shiny::h2(class = "h5 mb-1", "Authoritative stage records"),
                shiny::p(
                  class = "mb-0 small text-body-secondary",
                  "The fixed trajectory used for findings, joins, and publication decisions."
                )
              )
            ),
            bslib::card_body(
              class = "tempest-scripted-card-body",
              shiny::uiOutput(ns("stage_table"))
            )
          ),
          bslib::card(
            id = ns("stage_detail_card"),
            class = "tempest-review-card tempest-review-detail-card",
            `aria-labelledby` = ns("stage_detail_heading"),
            bslib::card_header(
              class = "tempest-scripted-card-header",
              shiny::tags$h2(
                id = ns("stage_detail_heading"),
                class = "h5 mb-0",
                "Selected stage details"
              )
            ),
            bslib::card_body(
              class = "tempest-scripted-card-body",
              shiny::uiOutput(ns("stage_detail_body"))
            )
          )
        ),
        bslib::layout_columns(
          col_widths = c(6, 6),
          gap = "1rem",
          class = "tempest-review-secondary-grid",
          bslib::card(
            class = "tempest-review-card tempest-review-agent-card",
            bslib::card_header(
              class = "tempest-scripted-card-header",
              shiny::div(
                shiny::h2(
                  class = "h5 mb-1",
                  "Unlinked untimed Deputy references"
                ),
                shiny::p(
                  class = "mb-0 small text-body-secondary",
                  "References that do not have an authority-validated StageRecord execution join."
                )
              )
            ),
            bslib::card_body(
              class = "tempest-scripted-card-body",
              shiny::uiOutput(ns("unlinked_agents"))
            )
          ),
          bslib::card(
            class = "tempest-review-card tempest-review-live-card",
            bslib::card_header(
              class = "tempest-scripted-card-header",
              shiny::div(
                shiny::h2(
                  class = "h5 mb-1",
                  "Live progress observations (untrusted)"
                ),
                shiny::p(
                  class = "mb-0 small text-body-secondary",
                  "Useful for orientation, but never used to rewrite the recorded review."
                )
              )
            ),
            bslib::card_body(
              class = "tempest-scripted-card-body",
              shiny::uiOutput(ns("live_progress"))
            )
          )
        ),
        bslib::card(
          class = "tempest-review-card tempest-review-memory-card",
          bslib::card_header(
            class = "tempest-scripted-card-header",
            shiny::div(
              shiny::h2(
                class = "h5 mb-1",
                "Reviewed memory consultations (Graft)"
              ),
              shiny::p(
                class = "mb-0 small text-body-secondary",
                "Pinned input selections and read-only @extra$graft recall records."
              )
            )
          ),
          bslib::card_body(
            class = "tempest-scripted-card-body",
            shiny::uiOutput(ns("graft_memory"))
          )
        )
      )
    )
  )
}

mod_run_review_server <- function(
  id,
  costorm_product,
  storm_product,
  costorm_events = shiny::reactive(list()),
  storm_events = shiny::reactive(list()),
  review_builder = tempest:::tempest_trajectory_review
) {
  shiny::moduleServer(id, function(input, output, session) {
    source_choices <- shiny::reactive({
      choices <- character()
      if (!is.null(costorm_product())) {
        choices["Co-STORM session"] <- "costorm"
      }
      if (!is.null(storm_product())) {
        choices["Last successful STORM run"] <- "storm"
      }
      choices
    })

    prior_source_choices <- shiny::reactiveVal(NULL)
    shiny::observeEvent(
      source_choices(),
      {
        choices <- source_choices()
        if (identical(choices, shiny::isolate(prior_source_choices()))) {
          return()
        }
        prior_source_choices(choices)
        selected <- shiny::isolate(input$product_source %||% "")
        if (!selected %in% unname(choices)) {
          selected <- if (length(choices) > 0L) unname(choices)[[1L]] else ""
        }
        shiny::updateSelectInput(
          session,
          "product_source",
          choices = choices,
          selected = selected
        )
      },
      ignoreInit = FALSE
    )

    selected_source <- shiny::reactive({
      choices <- unname(source_choices())
      selected <- input$product_source %||% ""
      if (selected %in% choices) selected else ""
    })

    selected_product <- shiny::reactive({
      switch(
        selected_source(),
        costorm = costorm_product(),
        storm = storm_product(),
        NULL
      )
    })

    review_state <- shiny::reactive({
      product <- selected_product()
      if (is.null(product)) {
        return(list(status = "blank", review = NULL))
      }
      review <- tryCatch(
        review_builder(product),
        error = function(error) NULL
      )
      if (is.null(review)) {
        return(list(status = "unavailable", review = NULL))
      }
      list(status = "ready", review = review)
    })

    stage_lane <- shiny::reactive({
      state <- review_state()
      if (!identical(state$status, "ready")) {
        return(run_review_empty_lane())
      }
      run_review_lane(run_review_prop(state$review, "stages"))
    })

    agent_lane <- shiny::reactive({
      state <- review_state()
      if (!identical(state$status, "ready")) {
        return(run_review_empty_lane())
      }
      run_review_lane(run_review_prop(state$review, "agent_runs"))
    })

    join_lane <- shiny::reactive({
      state <- review_state()
      if (!identical(state$status, "ready")) {
        return(run_review_empty_lane())
      }
      run_review_lane(run_review_prop(state$review, "joins"))
    })

    finding_lane <- shiny::reactive({
      state <- review_state()
      if (!identical(state$status, "ready")) {
        return(run_review_empty_lane())
      }
      run_review_lane(run_review_prop(state$review, "findings"))
    })

    prior_filter_choices <- shiny::reactiveVal(NULL)
    shiny::observeEvent(
      stage_lane(),
      {
        stages <- stage_lane()$items
        stage_values <- unique(vapply(
          stages,
          run_review_field,
          character(1),
          field = "stage"
        ))
        stage_values <- stage_values[nzchar(stage_values)]
        status_values <- unique(vapply(
          stages,
          run_review_field,
          character(1),
          field = "status"
        ))
        status_values <- status_values[nzchar(status_values)]
        choices <- list(stages = stage_values, statuses = status_values)
        if (identical(choices, shiny::isolate(prior_filter_choices()))) {
          return()
        }
        prior_filter_choices(choices)

        selected_stage <- shiny::isolate(input$stage_filter %||% "")
        if (!selected_stage %in% c("", stage_values)) {
          selected_stage <- ""
        }
        selected_status <- shiny::isolate(input$status_filter %||% "")
        if (!selected_status %in% c("", status_values)) {
          selected_status <- ""
        }
        shiny::updateSelectInput(
          session,
          "stage_filter",
          choices = c(
            "All stages" = "",
            stats::setNames(
              stage_values,
              run_review_label(stage_values)
            )
          ),
          selected = selected_stage
        )
        shiny::updateSelectInput(
          session,
          "status_filter",
          choices = c(
            "All statuses" = "",
            stats::setNames(
              status_values,
              run_review_label(status_values)
            )
          ),
          selected = selected_status
        )
      },
      ignoreInit = FALSE
    )

    filtered_stages <- shiny::reactive({
      stages <- stage_lane()$items
      if (nzchar(input$stage_filter %||% "")) {
        stages <- Filter(
          \(item) {
            identical(
              run_review_field(item, "stage"),
              input$stage_filter
            )
          },
          stages
        )
      }
      if (nzchar(input$status_filter %||% "")) {
        stages <- Filter(
          \(item) {
            identical(
              run_review_field(item, "status"),
              input$status_filter
            )
          },
          stages
        )
      }
      if (isTRUE(input$attention_only)) {
        attention_ids <- run_review_finding_reference_ids(
          finding_lane()$items
        )
        stages <- Filter(
          \(item) run_review_stage_has_attention(item, attention_ids),
          stages
        )
      }
      utils::head(stages, 250L)
    })

    prior_detail_choices <- shiny::reactiveVal(NULL)
    shiny::observeEvent(
      filtered_stages(),
      {
        stages <- filtered_stages()
        attempts <- vapply(
          stages,
          run_review_field,
          character(1),
          field = "attempt_id"
        )
        keep <- nzchar(attempts) & !duplicated(attempts)
        stages <- stages[keep]
        attempts <- attempts[keep]
        labels <- vapply(stages, run_review_stage_choice_label, character(1))
        choices <- list(attempts = attempts, labels = labels)
        if (identical(choices, shiny::isolate(prior_detail_choices()))) {
          return()
        }
        prior_detail_choices(choices)
        selected <- shiny::isolate(input$stage_detail %||% "")
        if (!selected %in% attempts) {
          selected <- if (length(attempts) > 0L) attempts[[1L]] else ""
        }
        shiny::updateSelectInput(
          session,
          "stage_detail",
          choices = stats::setNames(attempts, labels),
          selected = selected
        )
      },
      ignoreInit = FALSE
    )

    selected_stage <- shiny::reactive({
      attempt_id <- input$stage_detail %||% ""
      matched <- Filter(
        \(item) {
          identical(
            run_review_field(item, "attempt_id"),
            attempt_id
          )
        },
        filtered_stages()
      )
      if (length(matched) == 0L) NULL else matched[[1L]]
    })

    selected_events <- shiny::reactive({
      events <- switch(
        selected_source(),
        costorm = costorm_events(),
        storm = storm_events(),
        list()
      )
      run_review_event_records(events)
    })

    observed_event_ids <- shiny::reactiveVal(character())
    observed_event_source <- shiny::reactiveVal(NULL)
    new_failure_alert <- shiny::reactiveVal(NULL)
    shiny::observeEvent(
      selected_events(),
      {
        events <- selected_events()
        ids <- vapply(events, run_review_event_key, character(1))
        source <- selected_source()
        if (!identical(source, shiny::isolate(observed_event_source()))) {
          observed_event_source(source)
          observed_event_ids(utils::tail(unique(ids), 250L))
          new_failure_alert(NULL)
          return()
        }
        previous <- shiny::isolate(observed_event_ids())
        new <- events[!ids %in% previous]
        observed_event_ids(utils::tail(unique(c(previous, ids)), 250L))
        statuses <- vapply(
          new,
          run_review_field,
          character(1),
          field = "status"
        )
        if (any(statuses %in% c("failed", "cancelled"))) {
          new_failure_alert(
            if (any(statuses == "failed")) {
              "A newly observed live progress event reported a failure."
            } else {
              "A newly observed live progress event reported cancellation."
            }
          )
        } else {
          new_failure_alert(NULL)
        }
      },
      ignoreInit = FALSE
    )

    output$new_failure_alert <- shiny::renderUI({
      message <- new_failure_alert()
      if (is.null(message)) {
        return(NULL)
      }
      shiny::div(
        class = "tempest-review-alert alert alert-warning mb-0",
        role = "alert",
        shiny::icon("triangle-exclamation", class = "me-2"),
        shiny::span(message)
      )
    })

    output$summary <- shiny::renderUI({
      state <- review_state()
      if (identical(state$status, "blank")) {
        return(run_review_empty_state_ui(
          "route",
          "No run selected",
          "Complete a STORM run or Co-STORM session to review its trajectory."
        ))
      }
      if (identical(state$status, "unavailable")) {
        return(run_review_empty_state_ui(
          "shield-halved",
          "Review unavailable",
          paste(
            "No authoritative review is available for this product.",
            "Running, failed, cancelled, stale, or invalid products are",
            "rejected without replacing a valid completed review."
          )
        ))
      }

      review <- state$review
      product <- run_review_prop(review, "product")
      findings <- finding_lane()
      severity <- vapply(
        findings$items,
        run_review_field,
        character(1),
        field = "severity"
      )
      run_review_summary_ui(
        product = product,
        review_id = run_review_field(review, "review_id"),
        stage_lane = stage_lane(),
        finding_lane = findings,
        errors = sum(severity == "error"),
        warnings = sum(severity == "warning"),
        infos = sum(severity == "info")
      )
    })

    output$stage_table <- shiny::renderUI({
      if (!identical(review_state()$status, "ready")) {
        return(run_review_inline_empty_state_ui(
          "No authoritative StageRecord rows are available."
        ))
      }
      run_review_stage_table_ui(
        filtered_stages(),
        finding_lane()$items,
        stage_lane()
      )
    })

    output$stage_detail_body <- shiny::renderUI({
      stage <- selected_stage()
      if (is.null(stage)) {
        return(run_review_inline_empty_state_ui(
          "Choose an authoritative StageRecord to inspect its fixed references."
        ))
      }
      agents <- agent_lane()$items
      linked_indices <- run_review_authoritative_agent_indices(
        stage,
        joins = join_lane()$items,
        agents = agents
      )
      run_review_stage_detail_ui(
        stage,
        agents[linked_indices],
        agent_lane()
      )
    })

    output$unlinked_agents <- shiny::renderUI({
      stages <- stage_lane()$items
      agents <- agent_lane()$items
      linked_indices <- unique(unlist(lapply(
        stages,
        run_review_authoritative_agent_indices,
        joins = join_lane()$items,
        agents = agents
      )))
      unlinked <- agents[setdiff(seq_along(agents), linked_indices)]
      run_review_agent_table_ui(
        unlinked,
        paste(
          "Untimed Deputy references without an authority-validated",
          "StageRecord execution join"
        ),
        agent_lane()
      )
    })

    output$live_progress <- shiny::renderUI({
      state <- review_state()
      product_id <- if (identical(state$status, "ready")) {
        run_review_field(
          run_review_prop(state$review, "product"),
          "research_run_id"
        )
      } else {
        ""
      }
      run_review_progress_ui(selected_events(), product_id)
    })

    output$graft_memory <- shiny::renderUI({
      state <- review_state()
      input_sel <- if (identical(state$status, "ready")) {
        knowledge <- run_review_prop(state$review, "knowledge")
        if (is.list(knowledge)) knowledge$input_selection else NULL
      } else {
        NULL
      }
      consultations <- tryCatch(
        shiny_product_graft_consultations(selected_product()),
        error = function(error) tempest:::tempest_graft_consultations(NULL)
      )
      if (is.null(input_sel) && nrow(consultations) == 0L) {
        return(run_review_inline_empty_state_ui(
          "No Graft memory selections or recall consultations recorded for this run."
        ))
      }
      blocks <- list()
      if (!is.null(input_sel)) {
        blocks[[length(blocks) + 1L]] <- shiny::p(
          class = "small mb-2",
          shiny::strong("Pinned input selection: "),
          shiny::tags$code(substr(input_sel$selection_id %||% "", 1L, 12L)),
          " (",
          input_sel$purpose %||% "research",
          ", ",
          input_sel$records$total %||% 0L,
          " records)"
        )
      }
      if (nrow(consultations) > 0L) {
        blocks[[length(blocks) + 1L]] <- shiny::tags$table(
          class = "table table-sm mb-0",
          shiny::tags$thead(shiny::tags$tr(
            shiny::tags$th("Stream"),
            shiny::tags$th("Status"),
            shiny::tags$th("Purpose"),
            shiny::tags$th("Decision"),
            shiny::tags$th("Artifacts")
          )),
          shiny::tags$tbody(lapply(
            seq_len(nrow(consultations)),
            function(i) {
              row <- consultations[i, , drop = FALSE]
              shiny::tags$tr(
                shiny::tags$td(row$stream),
                shiny::tags$td(row$status),
                shiny::tags$td(row$purpose),
                shiny::tags$td(shiny::tags$code(substr(
                  row$decision_id %||% "",
                  1L,
                  12L
                ))),
                shiny::tags$td(row$artifact_count)
              )
            }
          ))
        )
      }
      shiny::tagList(blocks)
    })

    invisible(NULL)
  })
}

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
  proof <- run_review_prop(join, "proof")
  fields <- run_review_prop(proof, "matched_fields")
  if (
    !is.list(fields) ||
      is.data.frame(fields) ||
      length(fields) == 0L ||
      !all(vapply(
        fields,
        \(field) is.character(field) && length(field) == 1L && !is.na(field),
        logical(1)
      ))
  ) {
    return(character())
  }
  unname(unlist(fields, use.names = FALSE))
}

run_review_authoritative_agent_indices <- function(stage, joins, agents) {
  attempt_id <- run_review_field(stage, "attempt_id")
  if (!nzchar(attempt_id) || length(joins) == 0L || length(agents) == 0L) {
    return(integer())
  }
  exact_joins <- Filter(
    function(join) {
      proof <- run_review_prop(join, "proof")
      identical(run_review_field(join, "from_type"), "stage_attempt") &&
        identical(run_review_field(join, "from_id"), attempt_id) &&
        identical(run_review_field(join, "relation"), "executed_as") &&
        identical(run_review_field(join, "to_type"), "deputy_run") &&
        identical(run_review_field(proof, "kind"), "authority_validated") &&
        identical(
          run_review_join_matched_fields(join),
          c("deputy_run_id", "deputy_session_id")
        )
    },
    joins
  )
  run_ids <- unique(vapply(
    exact_joins,
    run_review_field,
    character(1),
    field = "to_id"
  ))
  run_ids <- run_ids[nzchar(run_ids)]
  linked <- unlist(lapply(run_ids, function(run_id) {
    matches <- which(vapply(
      agents,
      function(agent) {
        identical(run_review_field(agent, "deputy_run_id"), run_id) &&
          nzchar(run_review_field(agent, "deputy_session_id"))
      },
      logical(1)
    ))
    if (length(matches) == 1L) matches else integer()
  }))
  unique(as.integer(linked))
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

run_review_progress_closed_value <- function(value, allowed) {
  if (!nzchar(value)) {
    return("")
  }
  if (value %in% allowed) value else "redacted"
}

run_review_progress_stage_value <- function(value, workflow, kind) {
  if (!nzchar(value)) {
    return("")
  }
  if (!workflow %in% c("storm", "costorm")) {
    return("redacted")
  }
  allowed <- names(tempest:::tempest_progress_labels(workflow, kind = kind))
  run_review_progress_closed_value(value, allowed)
}

run_review_progress_record <- function(event) {
  workflow <- run_review_progress_closed_value(
    run_review_field(event, "workflow"),
    c("storm", "costorm")
  )
  timestamp <- run_review_field(event, "timestamp")
  if (
    nzchar(timestamp) &&
      !isTRUE(tempest:::tempest_ledger_timestamp_valid(timestamp))
  ) {
    timestamp <- ""
  }
  list(
    event_id = run_review_field(event, "event_id"),
    run_id = run_review_field(event, "run_id"),
    workflow = workflow,
    event_type = run_review_progress_closed_value(
      run_review_field(event, "event_type"),
      tempest:::tempest_progress_event_types()
    ),
    stage = run_review_progress_stage_value(
      run_review_field(event, "stage"),
      workflow,
      "stage"
    ),
    step = run_review_progress_stage_value(
      run_review_field(event, "step"),
      workflow,
      "step"
    ),
    status = run_review_progress_closed_value(
      run_review_field(event, "status"),
      tempest:::tempest_progress_statuses()
    ),
    timestamp = timestamp,
    sequence = run_review_field(event, "sequence"),
    parent_event_id = run_review_field(event, "parent_event_id"),
    correlation_id = run_review_field(event, "correlation_id")
  )
}

run_review_event_records <- function(events) {
  if (!is.list(events) || is.data.frame(events)) {
    return(list())
  }
  total <- length(events)
  retained_events <- utils::tail(events, 250L)
  records <- lapply(retained_events, run_review_progress_record)
  attr(records, "total") <- total
  attr(records, "omitted") <- max(0L, total - length(records))
  records
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
