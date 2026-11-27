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
      knowledge <- if (identical(state$status, "ready")) {
        run_review_prop(state$review, "knowledge")
      } else {
        NULL
      }
      input_sel <- if (is.list(knowledge)) knowledge$input_selection else NULL
      consultations <- if (
        is.list(knowledge) &&
          is.list(knowledge$consultations) &&
          is.list(knowledge$consultations$items)
      ) {
        items <- knowledge$consultations$items
        if (length(items) == 0L) {
          tempest:::tempest_graft_consultations(NULL)
        } else {
          do.call(
            rbind,
            lapply(items, function(item) {
              data.frame(
                status = item$status %||% "",
                stream = item$stream %||% "",
                purpose = item$purpose %||% NA_character_,
                decision_id = item$decision_id %||% NA_character_,
                selection_id = item$selection_id %||% NA_character_,
                artifact_count = as.integer(item$artifact_count %||% 0L),
                text_bytes = as.integer(item$text_bytes %||% 0L),
                truncated = isTRUE(item$truncated),
                omitted_count = as.integer(item$omitted_count %||% 0L),
                stringsAsFactors = FALSE
              )
            })
          )
        }
      } else {
        tryCatch(
          shiny_product_graft_consultations(selected_product()),
          error = function(error) tempest:::tempest_graft_consultations(NULL)
        )
      }
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
