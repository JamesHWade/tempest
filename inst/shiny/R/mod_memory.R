# Graft project-memory integration for the bundled tempest_app() workspace.

shiny_memory_context <- function() {
  ctx <- getOption("tempest.shiny.memory", NULL)
  if (!is.list(ctx) || is.null(ctx$store)) {
    return(NULL)
  }
  ctx
}

shiny_memory_wrap_config <- function(cfg, memory_ctx, use_tools = FALSE) {
  if (is.null(memory_ctx) || !isTRUE(use_tools)) {
    return(cfg)
  }
  memory_tools <- tryCatch(
    tempest:::tempest_graft_memory_tools(
      store = memory_ctx$store,
      purpose = memory_ctx$purpose,
      eligible = memory_ctx$eligible
    ),
    error = function(error) list()
  )
  if (!length(memory_tools)) {
    return(cfg)
  }
  memory_tool_names <- vapply(memory_tools, \(t) t@name, character(1))
  original_config <- cfg
  original_chat_fn <- cfg@chat_fn
  wrapped_chat_fn <- function(role, model, system_prompt, echo) {
    chat <- if (!is.null(original_chat_fn)) {
      original_chat_fn(
        role = role,
        model = model,
        system_prompt = system_prompt,
        echo = echo
      )
    } else {
      tempest:::tempest_make_chat(
        original_config,
        role = role,
        system_prompt = system_prompt,
        echo = echo
      )
    }
    if (
      !role %in% c("moderator", "expert") ||
        is.null(chat) ||
        !is.function(chat$register_tools)
    ) {
      return(chat)
    }
    existing <- tryCatch(
      vapply(chat$get_tools(), \(t) t@name, character(1)),
      error = function(error) character()
    )
    to_add <- memory_tools[!memory_tool_names %in% existing]
    if (length(to_add) > 0L) {
      chat$register_tools(to_add)
    }
    chat
  }
  cfg@chat_fn <- wrapped_chat_fn
  cfg
}

mod_memory_config_ui <- function(id, streams = character()) {
  ns <- shiny::NS(id)
  choices <- c(
    "None (fresh session)" = "",
    stats::setNames(streams, streams)
  )
  bslib::accordion_panel(
    title = "Project memory (Graft)",
    icon = shiny::icon("book-bookmark"),
    shiny::p(
      class = "small text-body-secondary tempest-config-intro",
      paste(
        "Let the moderator and experts consult team-reviewed findings from",
        "Graft, or pin an accepted stream for new sessions."
      )
    ),
    shiny::checkboxInput(
      ns("use_memory_tools"),
      "Consult reviewed memory in chat",
      value = TRUE
    ),
    shiny::selectInput(
      ns("pinned_stream"),
      "Pin accepted stream for new sessions",
      choices = choices,
      selected = "",
      width = "100%"
    ),
    shiny::div(
      class = "small text-body-secondary",
      role = "status",
      `aria-live` = "polite",
      shiny::uiOutput(ns("stream_status"))
    )
  )
}

mod_memory_config_server <- function(
  id,
  memory_ctx,
  memory_revision = shiny::reactiveVal(0L)
) {
  shiny::moduleServer(id, function(input, output, session) {
    accepted_streams <- shiny::reactive({
      memory_revision()
      if (is.null(memory_ctx)) {
        return(character())
      }
      tryCatch(
        tempest:::tempest_graft_memory_accepted_streams(
          memory_ctx$store,
          purpose = memory_ctx$purpose,
          eligible = memory_ctx$eligible
        )$stream,
        error = function(error) character()
      )
    })

    shiny::observeEvent(
      accepted_streams(),
      {
        streams <- accepted_streams()
        selected <- shiny::isolate(input$pinned_stream %||% "")
        if (!selected %in% c("", streams)) {
          selected <- ""
        }
        shiny::updateSelectInput(
          session,
          "pinned_stream",
          choices = c(
            "None (fresh session)" = "",
            stats::setNames(streams, streams)
          ),
          selected = selected
        )
      },
      ignoreInit = FALSE
    )

    output$stream_status <- shiny::renderUI({
      n <- length(accepted_streams())
      if (n == 0L) {
        "No accepted streams yet. Publish and accept a report on the Report tab."
      } else {
        sprintf(
          "%d accepted stream%s ready for recall.",
          n,
          if (n == 1L) "" else "s"
        )
      }
    })

    use_tools <- shiny::reactive(isTRUE(input$use_memory_tools))
    pinned_knowledge <- shiny::reactive({
      memory_revision()
      stream <- trimws(input$pinned_stream %||% "")
      if (is.null(memory_ctx) || !nzchar(stream)) {
        return(NULL)
      }
      tryCatch(
        tempest:::tempest_graft_reuse_stream(
          store = memory_ctx$store,
          stream = stream,
          purpose = memory_ctx$purpose,
          eligible = memory_ctx$eligible
        ),
        error = function(error) NULL
      )
    })

    list(
      use_tools = use_tools,
      knowledge = pinned_knowledge,
      accepted_streams = accepted_streams
    )
  })
}

mod_memory_report_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("panel"))
}

shiny_memory_product_key <- function(product) {
  if (is.null(product)) {
    return(NULL)
  }
  if (inherits(product, "TempestSession")) {
    manifest <- tryCatch(
      tempest:::tempest_session_manifest(product),
      error = function(error) NULL
    )
    if (is.null(manifest) || !identical(manifest@status, "succeeded")) {
      return(NULL)
    }
    report_sha <- manifest@deliverables$report_md$sha256 %||% ""
    return(paste(manifest@research_run_id, report_sha, sep = ":"))
  }
  if (tempest:::tempest_is_product_result(product)) {
    manifest <- product@manifest
    if (!identical(manifest@status, "succeeded")) {
      return(NULL)
    }
    report_sha <- manifest@deliverables$report_md$sha256 %||% ""
    return(paste(manifest@research_run_id, report_sha, sep = ":"))
  }
  NULL
}

shiny_memory_product_topic <- function(product) {
  if (inherits(product, "TempestSession")) {
    product$topic %||% ""
  } else if (tempest:::tempest_is_product_result(product)) {
    product@title %||% product@topic %||% ""
  } else {
    ""
  }
}

mod_memory_report_server <- function(
  id,
  memory_ctx,
  product,
  memory_revision = shiny::reactiveVal(0L)
) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    published_cache <- new.env(parent = emptyenv())
    current_selection <- shiny::reactiveVal(NULL)
    feedback <- shiny::reactiveVal(NULL)
    feedback_is_error <- shiny::reactiveVal(FALSE)

    product_key <- shiny::reactive(shiny_memory_product_key(product()))

    shiny::observeEvent(
      product_key(),
      {
        key <- product_key()
        feedback(NULL)
        feedback_is_error(FALSE)
        if (is.null(key) || is.null(memory_ctx)) {
          current_selection(NULL)
          return()
        }
        if (exists(key, envir = published_cache, inherits = FALSE)) {
          current_selection(get(key, envir = published_cache, inherits = FALSE))
        } else {
          current_selection(NULL)
        }
      },
      ignoreNULL = FALSE
    )

    ensure_published <- function() {
      sel <- shiny::isolate(current_selection())
      if (!is.null(sel)) {
        return(sel)
      }
      prod <- shiny::isolate(product())
      key <- shiny::isolate(product_key())
      if (is.null(prod) || is.null(key)) {
        tempest:::tempest_graft_memory_abort(
          "Publish a report before saving it to Graft.",
          class = "tempest_input_error"
        )
      }
      sel <- tempest::tempest_publish_artifact_research(prod, memory_ctx$store)
      assign(key, sel, envir = published_cache)
      current_selection(sel)
      sel
    }

    selection_state <- shiny::reactive({
      memory_revision()
      sel <- current_selection()
      if (is.null(sel) || is.null(memory_ctx)) {
        return(NULL)
      }
      tempest:::tempest_graft_selection_status(
        store = memory_ctx$store,
        selection_id = sel,
        purpose = memory_ctx$purpose,
        eligible = memory_ctx$eligible
      )
    })

    shiny::observeEvent(input$accept, {
      if (is.null(memory_ctx)) {
        return()
      }
      if (!isTRUE(input$reviewed)) {
        feedback("Confirm that you reviewed the report and its cited evidence.")
        feedback_is_error(TRUE)
        return()
      }
      existing <- trimws(input$existing_stream %||% "")
      stream <- if (nzchar(existing)) {
        existing
      } else {
        trimws(input$stream %||% "")
      }
      tryCatch(
        {
          sel <- ensure_published()
          decision <- tempest:::tempest_graft_accept_selection(
            store = memory_ctx$store,
            selection_id = sel,
            stream = stream,
            actor = input$actor %||% memory_ctx$actor,
            reason = input$reason %||% "",
            purpose = memory_ctx$purpose
          )
          feedback(sprintf(
            "Accepted into stream '%s' (decision %s).",
            decision@stream,
            substr(decision@id, 1L, 12L)
          ))
          feedback_is_error(FALSE)
          memory_revision(shiny::isolate(memory_revision()) + 1L)
        },
        error = function(error) {
          feedback(conditionMessage(error))
          feedback_is_error(TRUE)
        }
      )
    })

    shiny::observeEvent(input$withdraw, {
      state <- selection_state()
      if (
        is.null(memory_ctx) ||
          is.null(state) ||
          !identical(state$status, "accepted")
      ) {
        feedback("Only a currently accepted stream can be withdrawn.")
        feedback_is_error(TRUE)
        return()
      }
      tryCatch(
        {
          decision <- tempest:::tempest_graft_withdraw_stream(
            store = memory_ctx$store,
            stream = state$stream,
            actor = input$actor %||% memory_ctx$actor,
            reason = input$reason %||% "Withdrawn after review"
          )
          feedback(sprintf(
            "Withdrew stream '%s' from active memory (decision %s).",
            decision@stream,
            substr(decision@id, 1L, 12L)
          ))
          feedback_is_error(FALSE)
          memory_revision(shiny::isolate(memory_revision()) + 1L)
        },
        error = function(error) {
          feedback(conditionMessage(error))
          feedback_is_error(TRUE)
        }
      )
    })

    output$panel <- shiny::renderUI({
      if (is.null(memory_ctx) || is.null(product_key())) {
        return(NULL)
      }
      state <- selection_state()
      status_code <- if (is.null(state)) "provisional" else state$status
      existing_streams <- tryCatch(
        tempest:::tempest_graft_memory_streams(
          memory_ctx$store,
          purpose = memory_ctx$purpose,
          eligible = memory_ctx$eligible
        )$stream,
        error = function(error) character()
      )
      default_stream <- (if (!is.null(state)) state$stream else NULL) %||%
        tempest:::tempest_graft_stream_slug(shiny_memory_product_topic(
          product()
        ))
      existing_choices <- c(
        "Create new stream below" = "",
        stats::setNames(existing_streams, existing_streams)
      )
      selected_existing <- if (default_stream %in% existing_streams) {
        default_stream
      } else {
        ""
      }

      history_ui <- if (!is.null(state) && nrow(state$history) > 0L) {
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
      } else {
        NULL
      }

      notice_ui <- if (!is.null(feedback())) {
        shiny::div(
          class = if (isTRUE(feedback_is_error())) {
            "alert alert-warning py-2 px-3 mt-2 mb-0"
          } else {
            "alert alert-success py-2 px-3 mt-2 mb-0"
          },
          role = if (isTRUE(feedback_is_error())) "alert" else "status",
          feedback()
        )
      } else {
        NULL
      }

      shiny::div(
        class = "tempest-memory-report-panel card mb-3",
        shiny::div(
          class = "card-body",
          shiny::div(
            class = "d-flex flex-wrap justify-content-between align-items-center gap-2 mb-2",
            shiny::tags$strong("Retain in Graft project memory"),
            tempest:::tempest_researcher_memory_status_badge(
              status_code,
              if (!is.null(state)) state$stream else NULL
            )
          ),
          shiny::tags$details(
            shiny::tags$summary(
              if (identical(status_code, "accepted")) {
                "Update or withdraw accepted memory"
              } else {
                "Review and accept this report into project memory"
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
                value = memory_ctx$actor
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
                  "Accept to project memory",
                  class = "btn-primary btn-sm"
                ),
                if (identical(status_code, "accepted")) {
                  shiny::actionButton(
                    ns("withdraw"),
                    "Withdraw from active memory",
                    class = "btn-outline-secondary btn-sm"
                  )
                }
              )
            )
          ),
          notice_ui,
          history_ui
        )
      )
    })

    list(
      selection = current_selection,
      status = selection_state
    )
  })
}

shiny_product_graft_consultations <- function(product) {
  empty <- tempest:::tempest_graft_consultations(NULL)
  if (is.null(product) || !inherits(product, "TempestSession")) {
    return(empty)
  }
  priv <- tryCatch(product$.__enclos_env__$private, error = function(e) NULL)
  if (is.null(priv)) {
    return(empty)
  }
  chats <- list()
  if (!is.null(priv$moderator_chat)) {
    chats[[length(chats) + 1L]] <- priv$moderator_chat
  }
  if (is.list(priv$expert_chats)) {
    chats <- c(chats, unname(priv$expert_chats))
  }
  if (!length(chats)) {
    return(empty)
  }
  frames <- lapply(chats, function(chat) {
    tryCatch(
      tempest:::tempest_graft_consultations(chat),
      error = function(error) empty
    )
  })
  frames <- Filter(\(df) is.data.frame(df) && nrow(df) > 0L, frames)
  if (!length(frames)) {
    return(empty)
  }
  do.call(rbind, frames)
}
