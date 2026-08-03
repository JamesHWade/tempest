# Chat tab: the interactive Co-STORM session.
#
# Session creation, the warmup phase, and per-turn fact/mind-map extraction all
# operate on a live `TempestSession` (which holds ellmer chats and is not
# serialisable), so they run in the main process rather than as ExtendedTasks.
# The warmup streams compact progress into the chat while expert calls run as
# bounded async work; full expert answers are recorded in the session state.

mod_chat_ui <- function(id, config_ui) {
  ns <- shiny::NS(id)
  bslib::nav_panel(
    title = shiny::tagList(shiny::icon("comments"), "Chat"),
    value = "Chat",
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        title = "Session settings",
        width = 300,
        bslib::accordion(
          id = ns("session_settings"),
          open = "Research",
          multiple = TRUE,
          bslib::accordion_panel(
            title = "Research",
            icon = shiny::icon("magnifying-glass"),
            shiny::textInput(
              ns("topic"),
              "Research topic",
              placeholder = "Enter a research topic..."
            ),
            shiny::sliderInput(
              ns("n_experts"),
              "Number of experts",
              1,
              5,
              3
            ),
            bslib::input_switch(
              ns("warmup"),
              "Orient the expert panel",
              FALSE
            ),
            bslib::input_switch(
              ns("suggest"),
              "Suggest follow-up questions",
              TRUE
            ),
            bslib::input_task_button(
              ns("start"),
              "Start session",
              icon = shiny::icon("play"),
              label_busy = "Starting...",
              auto_reset = FALSE,
              class = "w-100"
            )
          ),
          bslib::accordion_panel(
            title = "Session and report",
            icon = shiny::icon("folder-open"),
            shiny::downloadButton(
              ns("save_session"),
              "Download session",
              icon = shiny::icon("floppy-disk"),
              class = "btn-outline-primary btn-sm w-100"
            ),
            shiny::fileInput(
              ns("load_session"),
              "Restore session bundle",
              accept = ".zip",
              buttonLabel = "Choose bundle",
              placeholder = "No bundle selected"
            ),
            bslib::input_switch(
              ns("autosave_session"),
              "Autosave after changes",
              FALSE
            ),
            shiny::uiOutput(ns("session_persistence")),
            shiny::selectInput(
              ns("report_style"),
              "Report style",
              choices = c("technical", "executive"),
              selected = "technical"
            ),
            bslib::input_task_button(
              ns("generate_report"),
              "Generate report",
              icon = shiny::icon("file-export"),
              label_busy = "Generating...",
              type = "secondary",
              class = "w-100"
            )
          ),
          bslib::accordion_panel(
            title = "Expert panel",
            icon = shiny::icon("users"),
            shiny::uiOutput(ns("expert_panel"))
          )
        ),
        config_ui
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header(shiny::uiOutput(ns("progress"))),
        bslib::card_body(
          class = "p-0",
          shinychat::chat_ui(
            ns("chat"),
            height = "100%",
            greeting = shinychat::chat_greeting(welcome_message()),
            icon_assistant = tempest_chat_icon(),
            allow_attachments = tempest_chat_attachment_types(),
            footer = chat_footer_ui(ns)
          ),
          chat_citation_sanitizer_script(ns)
        )
      )
    )
  )
}

mod_chat_server <- function(
  id,
  config,
  store,
  experts = NULL,
  runtime = tempest::tempest_runtime(),
  connection_permissions = list(),
  session_id = NULL
) {
  shiny::moduleServer(id, function(input, output, session) {
    report_ready <- shiny::reactiveVal(0L)
    progress_events <- shiny::reactiveVal(list())
    warmup_run_id <- 0L
    active_session_id <- 0L
    session_ended <- FALSE
    work_queue <- costorm_async_queue()

    next_warmup_guard <- function() {
      warmup_run_id <<- warmup_run_id + 1L
      run_id <- warmup_run_id
      function() {
        !isTRUE(session_ended) && identical(run_id, warmup_run_id)
      }
    }

    session$onSessionEnded(function() {
      session_ended <<- TRUE
      warmup_run_id <<- warmup_run_id + 1L
      active_session_id <<- active_session_id + 1L
      work_queue$cancel()
    })

    # --- shinychat adapter ---------------------------------------------------
    initial_chat <- tempest_make_chat(
      shiny::isolate(config()),
      "coordinator",
      system_prompt = paste(
        "You are the tempest chat shell.",
        "Ask the user to start a Co-STORM session from the sidebar before research chat.",
        sep = "\n"
      ),
      echo = "none"
    )
    # A Tempest conversation includes experts, evidence, map, report, and
    # progress state that shinychat's turn-only history cannot yet restore.
    chat <- shinychat::chat_server("chat", initial_chat, history = FALSE)
    current_source_store <- function() {
      ses <- tryCatch(shiny::isolate(store$get()), error = function(e) NULL)
      if (is.null(ses)) {
        return(NULL)
      }
      citation_source_store(ses$store %||% NULL)
    }
    append_chat <- function(text) {
      chat$append(
        citation_markdown(text, store = current_source_store()),
        role = "assistant"
      )
    }
    append_suggestion_cards <- function(cards) {
      chat_append_suggestion_cards(chat, cards)
    }
    append_chat_if_active <- function(text, session_id = active_session_id) {
      if (!isTRUE(session_ended) && identical(session_id, active_session_id)) {
        append_chat(text)
      }
      invisible(NULL)
    }
    append_suggestion_cards_if_active <- function(
      cards,
      session_id = active_session_id
    ) {
      if (!isTRUE(session_ended) && identical(session_id, active_session_id)) {
        append_suggestion_cards(cards)
      }
      invisible(NULL)
    }
    record_progress <- function(event) {
      record_costorm_progress_event(progress_events, event, session)
    }
    session_root <- session_storage_root(session)
    autosave_path <- file.path(session_root, "autosave")
    session$onSessionEnded(function() {
      unlink(session_root, recursive = TRUE, force = TRUE)
    })

    restore_progress_history <- function(ses) {
      progress_events(ses$artifacts[["progress_events"]] %||% list())
      invisible(NULL)
    }

    restore_session_bundle <- function(path) {
      warmup_run_id <<- warmup_run_id + 1L
      active_session_id <<- active_session_id + 1L
      work_queue$cancel()
      ses <- store$restore(
        path,
        config = config(),
        runtime = reactive_or_value(runtime),
        connection_permissions = reactive_or_value(connection_permissions),
        progress = record_progress
      )
      shiny::updateTextInput(session, "topic", value = ses$topic %||% "")
      shiny::updateSliderInput(
        session,
        "n_experts",
        value = max(1L, min(5L, length(ses$experts %||% list())))
      )
      restore_progress_history(ses)
      replace_chat_with_session(chat, ses)
      ses
    }

    session_autosave_server(
      store = store,
      path = shiny::reactive(autosave_path),
      enabled = shiny::reactive(input$autosave_session),
      on_error = function(error) {
        store$set_persistence(
          "error",
          path = NULL,
          message = "Autosave failed."
        )
        shiny::showNotification(
          "Autosave failed.",
          type = "error",
          duration = 10
        )
      }
    )

    output$session_persistence <- shiny::renderUI({
      session_persistence_status_ui(store$persistence())
    })

    output$progress <- shiny::renderUI({
      state <- costorm_progress_state(progress_events())
      workflow_progress_ui(state, costorm_stage_labels())
    })

    # --- Session lifecycle ---------------------------------------------------
    output$save_session <- shiny::downloadHandler(
      filename = function() {
        ses <- store$peek()
        topic <- if (is.null(ses)) "session" else ses$topic %||% "session"
        paste0("tempest-", topic_slug(topic), ".zip")
      },
      content = function(file) {
        session_archive_write(store, file)
      },
      contentType = "application/zip"
    )

    shiny::observeEvent(input$load_session, {
      upload <- input$load_session
      shiny::req(upload$datapath)
      extract_root <- file.path(session_root, tempest:::tempest_uuid("upload"))
      on.exit(unlink(extract_root, recursive = TRUE, force = TRUE), add = TRUE)
      tryCatch(
        {
          bundle_path <- session_archive_extract(upload$datapath, extract_root)
          restored <- restore_session_bundle(bundle_path)
          store$set_persistence(
            "restored",
            path = NULL,
            message = "Loaded uploaded session bundle."
          )
          shiny::showNotification(
            paste0("Loaded session: ", restored$topic),
            type = "message",
            duration = 5
          )
        },
        error = function(e) {
          store$set_persistence(
            "error",
            path = NULL,
            message = "Could not load the session bundle."
          )
          shiny::showNotification(
            "Could not load the session bundle.",
            type = "error",
            duration = 10
          )
        }
      )
    })

    create_session <- function(
      topic,
      n_experts,
      config_value,
      runtime_value,
      session_experts,
      session_connection_permissions,
      session_id_value,
      on_error = NULL
    ) {
      ses <- tryCatch(
        tempest::tempest_session(
          topic,
          config = config_value,
          runtime = runtime_value,
          n_experts = n_experts,
          experts = session_experts,
          connection_permissions = session_connection_permissions,
          session_id = session_id_value,
          progress = record_progress
        ),
        error = function(e) {
          costorm_log("session setup failed: %s", conditionMessage(e))
          if (is.function(on_error)) {
            on_error(e)
          }
          shiny::showNotification(
            paste0("Failed to create session: ", conditionMessage(e)),
            type = "error",
            duration = 10
          )
          NULL
        }
      )
      if (is.null(ses)) {
        return(NULL)
      }
      active_session_id <<- active_session_id + 1L
      store$set(ses)
      shiny::isolate(chat$set_client(ses$chats$moderator, sync = FALSE))
      # Clear the landing greeting; the session intro below replaces it.
      chat$clear(greeting = FALSE)
      append_chat(paste0(
        "Co-STORM session started for: **",
        topic,
        "**",
        expert_intro(ses),
        "\n\nAsk questions, request sources, or ask for a report.\n"
      ))
      costorm_log(
        "session ready: %s with %d experts",
        ses$session_id,
        length(ses$experts)
      )
      ses
    }

    clear_session <- function() {
      warmup_run_id <<- warmup_run_id + 1L
      active_session_id <<- active_session_id + 1L
      work_queue$cancel()
      progress_events(list())
      store$set(NULL)
      store$set_report(NULL)
      chat$set_client(initial_chat, sync = FALSE)
      chat$clear(
        messages = list(list(
          role = "assistant",
          content = paste(
            "Session cleared.",
            "Enter a topic in the sidebar and start a new Co-STORM session."
          )
        )),
        greeting = FALSE,
        client_history = "clear"
      )
      invisible(NULL)
    }

    run_report_generation <- function() {
      ses <- store$get()
      report_session_id <- active_session_id
      generate_report_for_chat_async(
        ses = ses,
        store = store,
        append_chat = append_chat,
        report_ready = report_ready,
        style = input$report_style %||% "technical",
        queue = work_queue,
        is_current = function() {
          !isTRUE(session_ended) &&
            identical(report_session_id, active_session_id)
        }
      )
    }

    run_chat_command <- function(command) {
      command <- chat_command_normalize(command)
      if (identical(command, "new")) {
        clear_session()
        return(invisible(NULL))
      }
      if (identical(command, "report")) {
        run_report_generation()
        return(invisible(NULL))
      }
      append_chat(chat_command_message(command, store$get(), config = config()))
      invisible(NULL)
    }

    register_chat_commands <- function() {
      chat$slash_command(
        "new",
        "Clear the current Co-STORM session.",
        function() run_chat_command("new")
      )
      chat$slash_command(
        "new-session",
        "Clear the current Co-STORM session.",
        function() run_chat_command("new")
      )
      chat$slash_command(
        "experts",
        "Show the current expert panel.",
        function() run_chat_command("experts")
      )
      chat$slash_command(
        "sources",
        "Summarize collected sources.",
        function() run_chat_command("sources")
      )
      chat$slash_command(
        "facts",
        "Summarize collected facts.",
        function() run_chat_command("facts")
      )
      chat$slash_command(
        "claims",
        "Summarize collected claims.",
        function() run_chat_command("facts")
      )
      chat$slash_command(
        "report",
        "Generate a report from collected evidence.",
        function() run_chat_command("report")
      )
      chat$slash_command(
        "system",
        "Show the moderator system prompt.",
        function() run_chat_command("system")
      )
      chat$slash_command(
        "tools",
        "Show runtime tools and command status.",
        function() run_chat_command("tools")
      )
    }
    register_chat_commands()

    output$runtime_footer <- shiny::renderUI({
      chat_runtime_footer_ui(
        ses = store$get(),
        progress_state = costorm_progress_state(progress_events()),
        chat_status = chat$status()
      )
    })

    shiny::observeEvent(input$footer_new, {
      run_chat_command("new")
    })
    shiny::observeEvent(input$footer_report, {
      run_chat_command("report")
    })

    shiny::observeEvent(input$start, {
      work_queue$cancel()
      warmup_is_current <- next_warmup_guard()
      progress_events(list())
      topic <- stringi::stri_trim_both(input$topic %||% "")
      if (!nzchar(topic)) {
        if (warmup_is_current()) {
          bslib::update_task_button("start", state = "ready", session = session)
        }
        return()
      }
      config_value <- shiny::isolate(config())
      runtime_value <- shiny::isolate(reactive_or_value(runtime))
      session_experts <- shiny::isolate(reactive_or_value(experts))
      session_connection_permissions <- shiny::isolate(
        reactive_or_value(connection_permissions)
      )
      session_id_value <- shiny::isolate(reactive_or_value(session_id))
      session_id_value <- session_id_value %||%
        tempest:::tempest_uuid("session")
      n_experts <- input$n_experts %||% 3
      suggest_enabled <- isTRUE(input$suggest)
      warmup_enabled <- isTRUE(input$warmup)

      record_progress(costorm_starting_event(session_id_value))
      costorm_log("start requested: %s", topic)
      experts_ready <- if (is.null(session_experts)) {
        tempest:::tempest_generate_experts_async(
          topic,
          n = n_experts,
          config = config_value
        )
      } else {
        promises::promise_resolve(session_experts)
      }

      later::later(
        function() {
          shiny::withReactiveDomain(session, {
            if (!warmup_is_current()) {
              return()
            }

            reset_start_button <- function() {
              if (warmup_is_current()) {
                bslib::update_task_button(
                  "start",
                  state = "ready",
                  session = session
                )
              }
            }

            promises::then(
              experts_ready,
              onFulfilled = function(generated_experts) {
                if (!warmup_is_current()) {
                  return(NULL)
                }
                session_experts <- generated_experts
                tryCatch(
                  {
                    schedule_start_suggestions <- function(
                      ses,
                      start_session_id
                    ) {
                      if (!warmup_is_current() || !isTRUE(suggest_enabled)) {
                        return(invisible(NULL))
                      }
                      delay_s <- getOption(
                        "tempest.shiny.suggestion_delay_s",
                        0.05
                      )
                      later::later(
                        function() {
                          shiny::withReactiveDomain(session, {
                            if (!warmup_is_current()) {
                              return()
                            }
                            costorm_log(
                              "suggestions started: %s",
                              ses$session_id %||% session_id_value
                            )
                            work_queue$enqueue(function(queue_current) {
                              append_suggestions_async(
                                ses,
                                suggest_enabled,
                                function(cards) {
                                  append_suggestion_cards_if_active(
                                    cards,
                                    session_id = start_session_id
                                  )
                                },
                                n = 4,
                                is_current = function() {
                                  queue_current() && warmup_is_current()
                                },
                                on_error = function(error) {
                                  costorm_log(
                                    "suggestions failed: %s",
                                    conditionMessage(error)
                                  )
                                }
                              )
                            })
                            costorm_log(
                              "suggestions finished: %s",
                              ses$session_id %||% session_id_value
                            )
                          })
                        },
                        delay = delay_s
                      )
                      invisible(NULL)
                    }

                    session_error <- NULL
                    ses <- create_session(
                      topic,
                      n_experts,
                      config_value = config_value,
                      runtime_value = runtime_value,
                      session_experts = session_experts,
                      session_connection_permissions = session_connection_permissions,
                      session_id_value = session_id_value,
                      on_error = function(error) {
                        session_error <<- error
                      }
                    )
                    if (is.null(ses)) {
                      record_progress(costorm_session_failed_event(
                        session_id_value,
                        session_error
                      ))
                      reset_start_button()
                      return()
                    }
                    record_progress(costorm_session_ready_event(
                      session_id_value,
                      ses
                    ))

                    start_session_id <- active_session_id
                    delay_suggestions <- should_delay_start_suggestions(
                      warmup_enabled,
                      ses$experts
                    )
                    if (!warmup_enabled || length(ses$experts) == 0) {
                      reset_start_button()
                      schedule_start_suggestions(ses, start_session_id)
                      return()
                    }

                    costorm_log("warmup started: %s", ses$session_id)
                    warmup_done <- promises::then(
                      run_warmup(
                        ses,
                        store,
                        append_chat,
                        is_current = warmup_is_current,
                        queue = work_queue
                      ),
                      onFulfilled = function(...) {
                        costorm_log("warmup finished: %s", ses$session_id)
                        NULL
                      },
                      onRejected = function(e) {
                        costorm_log("warmup failed: %s", conditionMessage(e))
                        if (warmup_is_current()) {
                          append_chat_if_active(
                            paste0("Warmup failed: ", conditionMessage(e)),
                            session_id = start_session_id
                          )
                        }
                        NULL
                      }
                    )
                    promises::finally(
                      warmup_done,
                      function() {
                        shiny::withReactiveDomain(session, {
                          if (warmup_is_current()) {
                            reset_start_button()
                            if (delay_suggestions) {
                              schedule_start_suggestions(ses, start_session_id)
                            }
                          }
                        })
                      }
                    )
                  },
                  error = function(error) {
                    costorm_log(
                      "start flow failed: %s",
                      conditionMessage(error)
                    )
                    record_progress(costorm_session_failed_event(
                      session_id_value,
                      error
                    ))
                    reset_start_button()
                  }
                )
              }
            ) |>
              promises::catch(function(error) {
                if (warmup_is_current()) {
                  costorm_log(
                    "expert generation failed: %s",
                    conditionMessage(error)
                  )
                  record_progress(costorm_session_failed_event(
                    session_id_value,
                    error
                  ))
                  reset_start_button()
                }
              })
          })
        },
        delay = getOption("tempest.shiny.start_delay_s", 0.05)
      )
    })

    # --- Post-process each completed chat turn -------------------------------
    shiny::observeEvent(chat$last_turn(), {
      ses <- store$get()
      if (is.null(ses)) {
        return()
      }
      msg <- chat_input_text(chat$last_input())
      turn <- chat$last_turn()
      ans <- tryCatch(
        ellmer::contents_markdown(turn),
        error = function(e) {
          if (is.character(turn)) paste(turn, collapse = "\n") else ""
        }
      )
      ans <- sanitize_external_citation_markers(ans)
      if (nzchar(msg)) {
        ses$add_turn("user", "user", msg)
      }
      if (nzchar(ans)) {
        ses$add_turn("Moderator", "assistant", ans)
      }
      turn_id <- paste0(
        "chat-turn-",
        active_session_id,
        "-",
        length(ses$transcript)
      )
      turn_event <- session_emit_progress(
        ses,
        "stage",
        "started",
        stage = "dialogue",
        step = "turn",
        correlation_id = turn_id
      )
      if (nzchar(msg)) {
        session_emit_progress(
          ses,
          "step",
          "succeeded",
          stage = "dialogue",
          step = "user_turn",
          parent_event_id = progress_event_id(turn_event),
          correlation_id = turn_id
        )
      }
      if (nzchar(ans)) {
        session_emit_progress(
          ses,
          "step",
          "succeeded",
          stage = "dialogue",
          step = "moderator_response",
          parent_event_id = progress_event_id(turn_event),
          correlation_id = turn_id
        )
      }
      turn_session_id <- active_session_id
      suggest_enabled <- isTRUE(input$suggest)
      task <- work_queue$enqueue(function(queue_current) {
        is_current <- function() {
          queue_current() &&
            !isTRUE(session_ended) &&
            identical(turn_session_id, active_session_id)
        }
        evidence <- tempest:::tempest_session_commit_evidence_async(
          ses,
          ans,
          turn = turn,
          session_id = ses$session_id,
          expert_id = "moderator",
          correlation_id = turn_id,
          is_current = is_current
        )
        evidence <- promises::then(evidence, function(result) {
          if (
            is_current() &&
              nzchar(msg) &&
              nzchar(ans) &&
              length(result$source_ids %||% character()) == 0L
          ) {
            append_chat_if_active(
              turn_evidence_gap_message(),
              session_id = turn_session_id
            )
          }
          result
        })
        evidence <- costorm_async_continue(
          evidence,
          function(error) {
            warning("Fact extraction failed: ", conditionMessage(error))
            append_chat_if_active(
              paste0(
                "**Evidence processing failed:** This answer was not added to ",
                "the evidence ledger. ",
                conditionMessage(error)
              ),
              session_id = turn_session_id
            )
          }
        )
        mindmap <- promises::then(evidence, function(evidence_result) {
          tempest:::tempest_session_update_mindmap_async(
            ses,
            last_exchange = turn_mindmap_exchange(
              msg,
              ans,
              evidence_result
            ),
            is_current = is_current
          )
        })
        mindmap <- costorm_async_continue(
          mindmap,
          function(error) {
            warning("Mind map update failed: ", conditionMessage(error))
          }
        )
        promises::then(mindmap, function(...) {
          if (!nzchar(msg) || !isTRUE(suggest_enabled)) {
            return(NULL)
          }
          append_suggestions_async(
            ses,
            enabled = TRUE,
            append_fn = function(cards) {
              append_suggestion_cards_if_active(
                cards,
                session_id = turn_session_id
              )
            },
            n = 4,
            is_current = is_current
          )
        })
      })
      promises::then(
        task,
        onFulfilled = function(...) {
          if (
            isTRUE(session_ended) ||
              !identical(turn_session_id, active_session_id)
          ) {
            return(NULL)
          }
          session_emit_progress(
            ses,
            "stage",
            "succeeded",
            stage = "dialogue",
            step = "turn",
            parent_event_id = progress_event_id(turn_event),
            correlation_id = turn_id
          )
          store$touch()
        },
        onRejected = function(error) {
          if (!isTRUE(session_ended)) {
            warning("Turn enrichment failed: ", conditionMessage(error))
          }
        }
      )
    })

    # --- Report generation ---------------------------------------------------
    shiny::observeEvent(input$generate_report, {
      report <- run_report_generation()
      promises::finally(report, function() {
        shiny::withReactiveDomain(session, {
          bslib::update_task_button(
            "generate_report",
            state = "ready",
            session = session
          )
        })
      })
    })

    # --- Expert panel --------------------------------------------------------
    output$expert_panel <- shiny::renderUI({
      ses <- store$get()
      if (is.null(ses) || length(ses$experts) == 0) {
        return(shiny::p(
          class = "text-muted small",
          "Start a session to see experts."
        ))
      }
      shiny::tagList(lapply(ses$experts, expert_card))
    })

    shiny::reactive(report_ready())
  })
}

# --- Chat module helpers -----------------------------------------------------

# The landing-page greeting shown in the empty chat.
welcome_message <- function() {
  paste(
    "**Welcome to tempest** — research reports written by a panel of AI experts.",
    "",
    paste(
      "Give it a topic and the experts search the web, gather cited evidence,",
      "and write it up. To start, enter a topic in the sidebar and click",
      "**Start Session**. The panel assembles and you can ask questions right",
      "here; answers stream in and you can stop them mid-response. When you've",
      "gathered enough, click **Generate Report**."
    ),
    "",
    "For a one-shot run without the back-and-forth, use the **STORM** tab.",
    sep = "\n"
  )
}

tempest_chat_attachment_types <- function() {
  c(
    "image/png",
    "image/jpeg",
    "image/webp",
    "application/pdf",
    "text/plain",
    "text/markdown",
    "text/csv",
    "application/json"
  )
}

reactive_or_value <- function(x) {
  if (shiny::is.reactive(x)) {
    x()
  } else if (is.function(x)) {
    x()
  } else {
    x
  }
}

chat_footer_ui <- function(ns) {
  button <- function(id, icon, label) {
    chat_footer_tooltip(
      shiny::actionButton(
        ns(id),
        label = shiny::span(label, class = "visually-hidden"),
        icon = shiny::icon(icon),
        title = label,
        `aria-label` = label,
        class = "btn-outline-secondary btn-sm px-2"
      ),
      label
    )
  }
  shiny::div(
    class = paste(
      "d-flex flex-wrap align-items-center justify-content-between",
      "gap-2 w-100"
    ),
    shiny::uiOutput(
      ns("runtime_footer"),
      class = "flex-grow-1 text-start"
    ),
    shiny::div(
      class = "d-flex flex-wrap align-items-center gap-1",
      button("footer_new", "plus", "New session"),
      button("footer_report", "file-lines", "Generate report")
    )
  )
}

chat_footer_tooltip <- function(trigger, label) {
  bslib::tooltip(
    trigger,
    label,
    placement = "top"
  )
}

# shinychat 0.4.0.9000 does not expose a transformation hook for streamed
# ContentText chunks. Keep this narrowly scoped provider-marker cleanup at the
# chat root until the same transformation can happen before React rendering.
chat_citation_sanitizer_script <- function(ns) {
  root_id <- ns("chat")
  root_id <- gsub("\\", "\\\\", root_id, fixed = TRUE)
  root_id <- gsub("'", "\\'", root_id, fixed = TRUE)
  shiny::tags$script(shiny::HTML(sprintf(
    "
(function(rootId) {
  function clean(value) {
    if (!value || value.indexOf('cite') === -1) {
      return value;
    }
    return value
      .replace(/\\uE200cite\\uE202[^\\uE201\\n]*(?:\\uE201)?/g, '')
      .replace(/[\\uE000-\\uF8FF]*cite[\\uE000-\\uF8FF]*turn\\d+(?:search|view|fetch|image|news|source)\\d+(?:[\\uE000-\\uF8FF]*turn\\d+(?:search|view|fetch|image|news|source)\\d+)*[\\uE000-\\uF8FF]*/g, '')
      .replace(/[ \\t]+([.,;:!?])/g, '$1');
  }
  function cleanTree(root) {
    if (!root) {
      return;
    }
    if (root.nodeType === Node.TEXT_NODE) {
      var cleaned = clean(root.nodeValue);
      if (cleaned !== root.nodeValue) {
        root.nodeValue = cleaned;
      }
      return;
    }
    if (
      root.nodeType !== Node.ELEMENT_NODE &&
      root.nodeType !== Node.DOCUMENT_FRAGMENT_NODE
    ) {
      return;
    }
    var walker = document.createTreeWalker(root, NodeFilter.SHOW_TEXT);
    var node;
    while ((node = walker.nextNode())) {
      cleanTree(node);
    }
  }
  function observe(root) {
    if (!root || root.dataset.tempestCitationSanitizer === 'true') {
      return;
    }
    root.dataset.tempestCitationSanitizer = 'true';
    cleanTree(root);
    var observer = new MutationObserver(function(mutations) {
      mutations.forEach(function(mutation) {
        if (mutation.type === 'characterData') {
          cleanTree(mutation.target);
        }
        mutation.addedNodes.forEach(cleanTree);
      });
    });
    observer.observe(root, {
      childList: true,
      characterData: true,
      subtree: true
    });
    if (root.shadowRoot) {
      cleanTree(root.shadowRoot);
      observer.observe(root.shadowRoot, {
        childList: true,
        characterData: true,
        subtree: true
      });
    }
  }
  function start() {
    observe(document.getElementById(rootId));
  }
  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', start, { once: true });
  } else {
    start();
  }
  document.addEventListener('shiny:connected', start);
})('%s');
",
    root_id
  )))
}

chat_runtime_counts <- function(ses) {
  if (is.null(ses)) {
    return(list(experts = 0L, sources = 0L, facts = 0L, report = FALSE))
  }
  sources <- tryCatch(ses$store$list_sources(), error = function(e) list())
  claims <- tryCatch(ses$store$list_claims(), error = function(e) list())
  report <- tryCatch(
    nzchar(ses$artifacts[["report_md"]] %||% ""),
    error = function(e) FALSE
  )
  list(
    experts = length(ses$experts %||% list()),
    sources = length(sources),
    facts = length(claims),
    report = isTRUE(report)
  )
}

chat_runtime_footer_ui <- function(
  ses,
  progress_state = NULL,
  chat_status = "idle"
) {
  counts <- chat_runtime_counts(ses)
  session_label <- if (is.null(ses)) {
    "No session"
  } else if (identical(chat_status, "streaming")) {
    "Answering"
  } else if (
    !is.null(progress_state) &&
      !is.na(progress_state$current_stage) &&
      nzchar(progress_state$current_stage)
  ) {
    paste("Running", progress_state$current_stage)
  } else {
    "Ready"
  }
  report_label <- if (isTRUE(counts$report)) "report ready" else "no report"
  shiny::div(
    class = "d-flex flex-wrap align-items-center gap-1 text-body-secondary",
    chat_footer_tooltip(
      shiny::span(
        class = "badge rounded-pill text-bg-light border text-body",
        title = paste("Session status:", session_label),
        `aria-label` = paste("Session status:", session_label),
        session_label
      ),
      paste("Session status:", session_label)
    ),
    chat_footer_tooltip(
      shiny::span(
        class = "d-inline-flex align-items-center gap-1 text-nowrap",
        title = paste("Experts:", counts$experts),
        `aria-label` = paste("Experts:", counts$experts),
        shiny::icon("users"),
        counts$experts
      ),
      paste("Experts:", counts$experts)
    ),
    chat_footer_tooltip(
      shiny::span(
        class = "d-inline-flex align-items-center gap-1 text-nowrap",
        title = paste("Sources:", counts$sources),
        `aria-label` = paste("Sources:", counts$sources),
        shiny::icon("link"),
        counts$sources
      ),
      paste("Sources:", counts$sources)
    ),
    chat_footer_tooltip(
      shiny::span(
        class = "d-inline-flex align-items-center gap-1 text-nowrap",
        title = paste("Facts:", counts$facts),
        `aria-label` = paste("Facts:", counts$facts),
        shiny::icon("clipboard-check"),
        counts$facts
      ),
      paste("Facts:", counts$facts)
    ),
    chat_footer_tooltip(
      shiny::span(
        class = "d-inline-flex align-items-center text-nowrap",
        title = paste("Report status:", report_label),
        `aria-label` = paste("Report status:", report_label),
        report_label
      ),
      paste("Report status:", report_label)
    )
  )
}

chat_command_normalize <- function(command) {
  command <- tolower(trimws(command %||% ""))
  switch(
    command,
    "new-session" = "new",
    claims = "facts",
    command
  )
}

chat_command_message <- function(command, ses = NULL, config = NULL) {
  command <- chat_command_normalize(command)
  switch(
    command,
    experts = chat_command_experts(ses),
    sources = chat_command_sources(ses),
    facts = chat_command_facts(ses),
    system = chat_command_system_prompt(),
    tools = chat_command_tools(ses, config),
    paste0(
      "Unknown command `/",
      command,
      "`.\n\nTry `/experts`, `/sources`, `/facts`, `/report`, `/system`, or `/tools`."
    )
  )
}

chat_command_experts <- function(ses) {
  if (is.null(ses) || length(ses$experts %||% list()) == 0L) {
    return("No expert panel is active. Start a session first.")
  }
  lines <- vapply(
    ses$experts,
    function(expert) {
      paste0(
        "- **",
        expert@name,
        "** [",
        expert@expert_id,
        "]: ",
        expert@title,
        if (nzchar(expert@description)) {
          paste0(" - ", expert@description)
        } else {
          ""
        }
      )
    },
    character(1)
  )
  paste(c("**Expert panel**", "", lines), collapse = "\n")
}

chat_command_sources <- function(ses, n = 5L) {
  sources <- if (is.null(ses)) {
    list()
  } else {
    tryCatch(ses$store$list_sources(), error = function(e) list())
  }
  if (length(sources) == 0L) {
    return("No sources collected yet. Ask a research question first.")
  }
  shown <- head(sources, n)
  lines <- vapply(
    shown,
    function(source) {
      title <- source$title %||%
        source$url %||%
        source$id %||%
        "Untitled source"
      if (length(title) == 0L || is.na(title) || !nzchar(title)) {
        title <- "Untitled source"
      }
      paste0("- **", title, "** (", source$id %||% "source", ")")
    },
    character(1)
  )
  more <- length(sources) - length(shown)
  if (more > 0L) {
    lines <- c(lines, paste0("- ...and ", more, " more."))
  }
  paste(c("**Collected sources**", "", lines), collapse = "\n")
}

chat_command_facts <- function(ses, n = 5L) {
  claims <- if (is.null(ses)) {
    list()
  } else {
    tryCatch(ses$store$list_claims(), error = function(e) list())
  }
  if (length(claims) == 0L) {
    return("No facts collected yet. Ask a research question first.")
  }
  shown <- head(claims, n)
  lines <- vapply(
    shown,
    function(claim) {
      score <- if (!is.na(claim@support_score)) {
        paste0(", support ", round(claim@support_score, 2))
      } else {
        ""
      }
      paste0(
        "- ",
        claim@claim_text,
        " (",
        claim@verification_status,
        score,
        ")"
      )
    },
    character(1)
  )
  more <- length(claims) - length(shown)
  if (more > 0L) {
    lines <- c(lines, paste0("- ...and ", more, " more."))
  }
  paste(c("**Collected facts**", "", lines), collapse = "\n")
}

chat_command_system_prompt <- function() {
  prompt <- tryCatch(tempest_prompt("moderator_system"), error = function(e) "")
  if (!nzchar(prompt)) {
    return("The moderator system prompt is not available.")
  }
  paste0("**Moderator system prompt**\n\n```text\n", prompt, "\n```")
}

chat_command_tools <- function(ses, config = NULL) {
  config <- if (is.null(ses)) config else ses$config %||% config
  provider <- tryCatch(config@search_provider, error = function(e) "unknown")
  grants <- if (is.null(ses)) list() else ses$capability_grants %||% list()
  grant_lines <- unlist(
    lapply(names(grants), function(context_id) {
      records <- grants[[context_id]] %||% list()
      vapply(
        records,
        function(grant) {
          paste0(
            "- `",
            context_id,
            "` / `",
            grant$capability_id,
            "`: ",
            grant$status
          )
        },
        character(1)
      )
    }),
    use.names = FALSE
  )
  if (length(grant_lines) == 0L) {
    grant_lines <- "- No session capability grants are active."
  }
  paste(
    "**Tools and commands**",
    "",
    paste0("- Search provider: `", provider, "`."),
    "**Scoped capability grants**",
    paste(grant_lines, collapse = "\n"),
    "- Commands: `/new`, `/experts`, `/sources`, `/facts`, `/report`, `/system`, `/tools`.",
    sep = "\n"
  )
}

costorm_async_queue <- function() {
  generation <- 0L
  tail <- promises::promise_resolve(NULL)
  pending <- 0L
  list(
    enqueue = function(task) {
      stopifnot(is.function(task))
      ticket <- generation
      pending <<- pending + 1L
      is_current <- function() identical(ticket, generation)
      result <- promises::then(tail, function(...) {
        if (!is_current()) {
          return(NULL)
        }
        task(is_current)
      })
      tail <<- promises::then(
        result,
        onFulfilled = function(value) {
          pending <<- max(0L, pending - 1L)
          value
        },
        onRejected = function(error) {
          pending <<- max(0L, pending - 1L)
          NULL
        }
      )
      result
    },
    cancel = function() {
      generation <<- generation + 1L
      pending <<- 0L
      tail <<- promises::promise_resolve(NULL)
      invisible(NULL)
    },
    pending = function() pending
  )
}

costorm_async_continue <- function(promise, on_error = NULL) {
  promises::then(
    promise,
    onFulfilled = identity,
    onRejected = function(error) {
      if (is.function(on_error)) {
        on_error(error)
      }
      NULL
    }
  )
}

append_suggestions_async <- function(
  ses,
  enabled,
  append_fn,
  n = 4,
  is_current = function() TRUE,
  on_error = NULL
) {
  if (is.null(ses) || !isTRUE(enabled)) {
    return(promises::promise_resolve(NULL))
  }
  request <- tempest:::tempest_session_suggest_questions_async(
    ses,
    n = n,
    is_current = is_current
  )
  promises::then(
    request,
    onFulfilled = function(questions) {
      if (!warmup_is_current(is_current)) {
        return(NULL)
      }
      cards <- suggestion_cards(questions)
      if (!is.null(cards)) {
        append_fn(cards)
      }
      invisible(NULL)
    },
    onRejected = function(error) {
      if (is.function(on_error)) {
        on_error(error)
      }
      stop(error)
    }
  )
}

generate_report_for_chat_async <- function(
  ses,
  store,
  append_chat,
  report_ready,
  queue,
  style = "technical",
  is_current = function() TRUE
) {
  if (is.null(ses)) {
    append_chat("No session active. Start a session first.")
    return(promises::promise_resolve(FALSE))
  }
  n_evidence <- length(ses$store$list_claims()) +
    length(ses$store$list_sources())
  if (n_evidence == 0L) {
    append_chat(
      "No facts or sources collected yet. Ask some questions first to gather research."
    )
    return(promises::promise_resolve(FALSE))
  }
  append_chat("Generating report...")
  task <- queue$enqueue(function(queue_current) {
    current <- function() queue_current() && warmup_is_current(is_current)
    tempest:::tempest_session_report_async(
      ses,
      style = style,
      is_current = current
    )
  })
  promises::then(
    task,
    onFulfilled = function(markdown) {
      if (
        !warmup_is_current(is_current) || is.null(markdown) || !nzchar(markdown)
      ) {
        return(FALSE)
      }
      store$set_report(markdown, ses$topic, source_store = ses$store)
      store$touch()
      append_chat(sprintf(
        "Report generated (%d chars). See the **Report** tab.",
        nchar(markdown)
      ))
      report_ready(report_ready() + 1L)
      TRUE
    },
    onRejected = function(error) {
      if (warmup_is_current(is_current)) {
        append_chat(paste0(
          "Report generation failed: ",
          conditionMessage(error)
        ))
      }
      FALSE
    }
  )
}

generate_report_for_chat <- function(
  ses,
  store,
  append_chat,
  report_ready,
  style = "technical"
) {
  if (is.null(ses)) {
    append_chat("No session active. Start a session first.")
    return(invisible(FALSE))
  }
  n_evidence <- length(ses$store$list_claims()) +
    length(ses$store$list_sources())
  if (n_evidence == 0) {
    append_chat(
      "No facts or sources collected yet. Ask some questions first to gather research."
    )
    return(invisible(FALSE))
  }
  append_chat("Generating report...")
  md <- tryCatch(
    ses$report(style = style),
    error = function(e) {
      append_chat(paste0("Report generation failed: ", conditionMessage(e)))
      NULL
    }
  )
  if (is.null(md) || !nzchar(md)) {
    append_chat(
      "Report generation returned empty. Try asking more questions first."
    )
    return(invisible(FALSE))
  }
  ses$artifacts[["report_md"]] <- md
  store$set_report(md, ses$topic, source_store = ses$store)
  store$touch()
  append_chat(sprintf(
    "Report generated (%d chars). See the **Report** tab.",
    nchar(md)
  ))
  report_ready(report_ready() + 1L)
  invisible(TRUE)
}

session_autosave_server <- function(
  store,
  path,
  enabled,
  delay_ms = 1000,
  on_saved = NULL,
  on_error = NULL
) {
  trigger <- shiny::debounce(
    shiny::reactive(store$autosave_trigger()),
    delay_ms
  )
  shiny::observeEvent(
    trigger(),
    {
      if (!isTRUE(enabled()) || is.null(store$peek())) {
        return()
      }
      bundle_path <- path() %||% ""
      if (!rlang::is_string(bundle_path) || !nzchar(trimws(bundle_path))) {
        return()
      }
      tryCatch(
        {
          saved <- store$save(
            bundle_path,
            overwrite = TRUE,
            status = "autosaved"
          )
          session_bundle_enforce_quota(saved)
          session_secure_permissions(saved)
          if (is.function(on_saved)) {
            on_saved(saved)
          }
        },
        error = function(e) {
          if (is.function(on_error)) {
            on_error(e)
          }
        }
      )
    },
    ignoreInit = TRUE
  )
}

session_storage_root <- function(session) {
  token <- gsub("[^A-Za-z0-9_-]", "-", session$token %||% "session")
  root <- file.path(tempdir(), "tempest-shiny-sessions", token)
  dir.create(root, recursive = TRUE, showWarnings = FALSE, mode = "0700")
  Sys.chmod(root, mode = "0700", use_umask = FALSE)
  normalizePath(root, winslash = "/", mustWork = TRUE)
}

session_secure_permissions <- function(path) {
  if (!dir.exists(path)) {
    return(invisible(path))
  }
  directories <- unique(c(
    path,
    list.dirs(path, recursive = TRUE, full.names = TRUE)
  ))
  files <- list.files(
    path,
    recursive = TRUE,
    full.names = TRUE,
    all.files = TRUE,
    no.. = TRUE
  )
  Sys.chmod(directories, mode = "0700", use_umask = FALSE)
  if (length(files) > 0L) {
    Sys.chmod(files, mode = "0600", use_umask = FALSE)
  }
  invisible(path)
}

session_bundle_enforce_quota <- function(
  path,
  max_bytes = getOption("tempest.shiny.session_quota_bytes", 100 * 1024^2)
) {
  max_bytes <- tempest:::tempest_fetch_positive(max_bytes, "max_bytes")
  files <- list.files(
    path,
    recursive = TRUE,
    full.names = TRUE,
    all.files = TRUE,
    no.. = TRUE
  )
  size <- sum(file.info(files)$size, na.rm = TRUE)
  if (size > max_bytes) {
    unlink(path, recursive = TRUE, force = TRUE)
    stop("Session bundle exceeds the configured storage quota.", call. = FALSE)
  }
  invisible(size)
}

session_archive_write <- function(store, file) {
  if (is.null(store$peek())) {
    stop("No Co-STORM session is active.", call. = FALSE)
  }
  bundle_dir <- tempfile("tempest-session-download-")
  on.exit(unlink(bundle_dir, recursive = TRUE, force = TRUE), add = TRUE)
  store$save(bundle_dir, overwrite = FALSE, status = "saved")
  session_bundle_enforce_quota(bundle_dir)
  session_secure_permissions(bundle_dir)
  files <- list.files(
    bundle_dir,
    all.files = TRUE,
    no.. = TRUE,
    recursive = TRUE,
    include.dirs = FALSE
  )
  zip::zip(
    file,
    files = files,
    include_directories = FALSE,
    root = bundle_dir,
    mode = "mirror"
  )
  store$set_persistence(
    "saved",
    path = NULL,
    message = "Downloaded session bundle."
  )
  invisible(file)
}

session_archive_extract <- function(archive, root) {
  listing <- utils::unzip(archive, list = TRUE)
  entries <- gsub("\\\\", "/", listing$Name)
  if (!session_archive_listing_is_safe(entries, listing$Length)) {
    stop("Session archive contains unsafe files.", call. = FALSE)
  }
  manifest <- session_archive_read_manifest(archive, listing)
  declared_files <- session_archive_manifest_files(manifest)
  if (
    !session_archive_listing_is_safe(
      entries,
      listing$Length,
      declared_files = declared_files
    )
  ) {
    stop(
      "Session archive contents do not match its manifest.",
      call. = FALSE
    )
  }
  complete <- FALSE
  on.exit(
    if (!complete && dir.exists(root)) {
      unlink(root, recursive = TRUE, force = TRUE)
    },
    add = TRUE
  )
  dir.create(root, recursive = TRUE, showWarnings = FALSE, mode = "0700")
  utils::unzip(archive, exdir = root)
  tempest:::tempest_session_bundle_validate_manifest(
    root,
    manifest,
    partial_recovery = FALSE
  )
  session_secure_permissions(root)
  complete <- TRUE
  normalizePath(root, winslash = "/", mustWork = TRUE)
}

session_archive_listing_is_safe <- function(
  entries,
  sizes,
  declared_files = NULL
) {
  entries <- gsub("\\\\", "/", entries)
  valid_sizes <- is.numeric(sizes) &&
    length(sizes) == length(entries) &&
    all(is.finite(sizes)) &&
    all(sizes >= 0)
  unsafe <- grepl("^(/|~|[A-Za-z]:)", entries) |
    vapply(
      strsplit(entries, "/", fixed = TRUE),
      function(parts) {
        any(!nzchar(parts)) || any(parts %in% c(".", ".."))
      },
      logical(1)
    )
  too_large <- !valid_sizes ||
    sum(sizes) > 100 * 1024^2 ||
    any(sizes > 50 * 1024^2)
  safe <- length(entries) > 0L &&
    !anyDuplicated(entries) &&
    !any(unsafe) &&
    !too_large
  if (!safe || is.null(declared_files)) {
    return(safe)
  }
  declared_files <- gsub("\\\\", "/", declared_files)
  declared_safe <- length(declared_files) > 0L &&
    !anyDuplicated(declared_files) &&
    all(vapply(
      declared_files,
      tempest:::tempest_artifact_bundle_path_is_safe,
      logical(1)
    ))
  declared_safe &&
    setequal(entries, c("session.json", declared_files)) &&
    length(entries) == length(declared_files) + 1L
}

session_archive_read_manifest <- function(archive, listing) {
  names <- gsub("\\\\", "/", listing$Name)
  index <- which(names == "session.json")
  if (
    length(index) != 1L ||
      !is.finite(listing$Length[[index]]) ||
      listing$Length[[index]] > 50 * 1024^2
  ) {
    stop(
      "Session archive must contain exactly one bounded session.json.",
      call. = FALSE
    )
  }
  connection <- unz(archive, "session.json", open = "rb")
  bytes <- tryCatch(
    readBin(
      connection,
      what = "raw",
      n = as.integer(listing$Length[[index]])
    ),
    finally = close(connection)
  )
  tryCatch(
    jsonlite::fromJSON(rawToChar(bytes), simplifyVector = FALSE),
    error = function(error) {
      stop("Session archive manifest is not valid JSON.", call. = FALSE)
    }
  )
}

session_archive_manifest_files <- function(manifest) {
  if (
    !is.list(manifest) ||
      !identical(as.integer(manifest$schema_version %||% NA_integer_), 4L) ||
      !identical(manifest$status %||% "", "complete")
  ) {
    stop(
      "Session archive manifest uses an unsupported schema or status.",
      call. = FALSE
    )
  }
  files <- as.character(unlist(manifest$files %||% character()))
  checksums <- unlist(manifest$checksums %||% list(), use.names = TRUE)
  artifact_files <- as.character(unlist(
    manifest$artifact_files %||% character()
  ))
  indexes <- c(
    manifest$artifact_index %||% NA_character_,
    manifest$deliverable_index %||% NA_character_
  )
  if (
    length(files) == 0L ||
      anyDuplicated(files) ||
      !setequal(names(checksums), files) ||
      length(names(checksums)) != length(files) ||
      any(
        !vapply(
          files,
          tempest:::tempest_artifact_bundle_path_is_safe,
          logical(1)
        )
      ) ||
      anyDuplicated(artifact_files) ||
      length(setdiff(artifact_files, files)) > 0L ||
      any(
        !startsWith(
          artifact_files,
          "artifacts/typed/content/"
        )
      ) ||
      !identical(
        indexes,
        c(
          "artifacts/typed/index.json",
          "artifacts/typed/deliverables.json"
        )
      )
  ) {
    stop("Session archive manifest is internally inconsistent.", call. = FALSE)
  }
  files
}

session_persistence_status_ui <- function(state) {
  if (is.null(state) || identical(state$status, "idle")) {
    return(NULL)
  }
  status_class <- switch(
    state$status,
    saved = "text-success",
    autosaved = "text-success",
    restored = "text-info",
    "text-danger"
  )
  status_icon <- switch(
    state$status,
    saved = "circle-check",
    autosaved = "clock-rotate-left",
    restored = "folder-open",
    "triangle-exclamation"
  )
  shiny::div(
    class = paste("small mt-2", status_class),
    shiny::span(shiny::icon(status_icon), class = "me-1"),
    shiny::span(state$message %||% ""),
  )
}

replace_chat_with_session <- function(chat, session) {
  chat$set_client(session$chats$moderator, sync = FALSE)
  chat$clear(greeting = FALSE, client_history = "keep")
  append_restored_session_chat(
    chat = chat,
    session = session,
    source_store = citation_source_store(session$store %||% NULL)
  )
  invisible(chat)
}

append_restored_session_chat <- function(chat, session, source_store = NULL) {
  topic <- session$topic %||% "Untitled topic"
  chat$append(
    citation_markdown(
      paste0("Resumed Co-STORM session for: **", topic, "**"),
      store = source_store
    ),
    role = "assistant"
  )

  for (turn in session$transcript %||% list()) {
    text <- turn$text %||% ""
    if (!nzchar(text)) {
      next
    }
    role <- tolower(turn$role %||% "")
    speaker <- turn$speaker %||%
      if (identical(role, "user")) "User" else "Moderator"
    display_role <- if (identical(role, "user")) "user" else "assistant"
    markdown <- paste0("**", speaker, ":**\n\n", text)
    chat$append(
      citation_markdown(markdown, store = source_store),
      role = display_role
    )
  }

  report_md <- if (
    !is.null(session$artifact_catalog) &&
      session$artifact_catalog$has("report_md")
  ) {
    session$artifact_catalog$get("report_md")@content
  } else {
    session$artifacts[["report_md"]] %||% ""
  }
  if (nzchar(report_md)) {
    chat$append(
      "Restored report artifact. See the **Report** tab.",
      role = "assistant"
    )
  }
  invisible(chat)
}

expert_intro <- function(ses) {
  if (length(ses$experts) == 0) {
    return("")
  }
  lines <- vapply(
    ses$experts,
    function(expert) {
      paste0(
        "- **",
        expert@name,
        "** (",
        expert@title,
        ")"
      )
    },
    character(1)
  )
  paste0("\n\n**Expert Panel:**\n", paste(lines, collapse = "\n"))
}

expert_card <- function(expert) {
  shiny::div(
    class = "mb-2 p-2 border rounded small",
    shiny::div(
      class = "d-flex align-items-center gap-2",
      persona_icon(expert@name, expert@expert_id),
      shiny::div(
        class = "min-w-0",
        shiny::strong(expert@name),
        shiny::br(),
        shiny::span(class = "text-muted", expert@title)
      )
    ),
    if (nzchar(expert@description)) {
      shiny::div(class = "mt-1 fst-italic", expert@description)
    }
  )
}

# Flatten shinychat's last_input (string or list) into a single string.
chat_input_text <- function(x) {
  if (is.null(x)) {
    return("")
  }
  if (is.character(x)) {
    return(paste(x, collapse = "\n"))
  }
  if (is.list(x)) {
    parts <- vapply(
      x,
      chat_input_part_text,
      character(1)
    )
    parts <- parts[nzchar(parts)]
    return(paste(parts, collapse = "\n"))
  }
  chat_input_part_text(x)
}

chat_input_part_text <- function(x) {
  if (is.character(x)) {
    return(paste(x, collapse = "\n"))
  }
  text <- tryCatch(ellmer::contents_text(x), error = function(e) NULL)
  usable_text <- !is.null(text) &&
    length(text) > 0L &&
    any(!is.na(text) & nzchar(text))
  if (isTRUE(usable_text)) {
    text <- text[!is.na(text) & nzchar(text)]
    return(paste(text, collapse = "\n"))
  }
  if (inherits(x, "ellmer::ContentImage")) {
    return("[Image attachment]")
  }
  if (inherits(x, "ellmer::ContentPDF")) {
    filename <- tryCatch(x@filename, error = function(e) "")
    if (
      !is.null(filename) &&
        length(filename) == 1L &&
        !is.na(filename) &&
        nzchar(filename)
    ) {
      return(paste0("[PDF attachment: ", filename, "]"))
    }
    return("[PDF attachment]")
  }
  "[Attachment]"
}

suggestion_card_titles <- function(n) {
  if (n <= 0L) {
    return(character())
  }
  rep_len(
    c(
      "Evidence gap",
      "Key uncertainty",
      "Another perspective",
      "How to verify"
    ),
    n
  )
}

# Build a shinychat suggestion-card block from a vector of questions. shinychat
# renders a markdown list whose items are all `<span class="suggestion submit">`
# as a grid of cards and submits the selected question through the native chat
# input.
# Returns NULL when there are no usable questions.
suggestion_cards <- function(questions, lead = "**Research next:**") {
  titles <- names(questions)
  questions <- sanitize_external_citation_markers(questions)
  questions <- trimws(questions)
  keep <- !is.na(questions) & nzchar(questions)
  questions <- questions[keep]
  if (is.null(titles)) {
    titles <- rep("", length(keep))
  }
  titles <- trimws(titles[keep])
  if (length(questions) == 0) {
    return(NULL)
  }
  fallback_titles <- suggestion_card_titles(length(questions))
  missing_titles <- is.na(titles) | !nzchar(titles)
  titles[missing_titles] <- fallback_titles[missing_titles]
  items <- paste0(
    "- <span class=\"suggestion submit\" title=\"",
    htmltools::htmlEscape(titles, attribute = TRUE),
    "\">",
    htmltools::htmlEscape(questions),
    "</span>"
  )
  structure(
    paste0(lead, "\n\n", paste(items, collapse = "\n")),
    class = c("tempest_shinychat_suggestions", "character")
  )
}

chat_append_suggestion_cards <- function(chat, cards) {
  if (!inherits(cards, "tempest_shinychat_suggestions")) {
    stop("Suggestion cards must be created by suggestion_cards().")
  }
  chat$append(as.character(cards), role = "assistant")
  invisible(cards)
}

# Generate suggestion cards and append them, gated on `enabled`. Quiet on
# failure: a stalled or erroring generator simply shows no cards. Pure of Shiny
# reactives (deps injected) so it can be unit-tested directly.
append_suggestions <- function(
  ses,
  enabled,
  append_fn,
  n = 4,
  on_error = NULL
) {
  if (is.null(ses) || !isTRUE(enabled)) {
    return(invisible(NULL))
  }
  questions <- tryCatch(ses$suggest_questions(n), error = function(e) {
    if (is.function(on_error)) {
      on_error(e)
    }
    character()
  })
  cards <- suggestion_cards(questions)
  if (!is.null(cards)) {
    append_fn(cards)
  }
  invisible(NULL)
}

should_delay_start_suggestions <- function(warmup_enabled, experts) {
  isTRUE(warmup_enabled) && length(experts) > 0
}

costorm_log <- function(format, ...) {
  if (!isTRUE(getOption("tempest.shiny.log", TRUE))) {
    return(invisible(NULL))
  }
  args <- list(...)
  text <- if (length(args) == 0L) {
    format
  } else {
    sprintf(format, ...)
  }
  message("[tempest:chat] ", text)
  invisible(NULL)
}

costorm_starting_event <- function(session_id) {
  tempest::tempest_progress_event(
    run_id = session_id,
    workflow = "costorm",
    event_type = "stage",
    status = "started",
    stage = "session",
    step = "created",
    message = "Starting Co-STORM session."
  )
}

costorm_session_ready_event <- function(session_id, ses = NULL) {
  experts <- if (!is.null(ses)) ses$experts else NULL
  tempest::tempest_progress_event(
    run_id = session_id,
    workflow = "costorm",
    event_type = "stage",
    status = "succeeded",
    stage = "session",
    step = "created",
    message = "Co-STORM session ready.",
    payload = list(expert_count = length(experts %||% list()))
  )
}

costorm_session_failed_event <- function(session_id, error = NULL) {
  message <- "Session setup failed."
  payload <- list(error_message = message)
  if (!is.null(error)) {
    message <- paste0(message, " ", conditionMessage(error))
    payload <- progress_error_payload(error)
  }
  tempest::tempest_progress_event(
    run_id = session_id,
    workflow = "costorm",
    event_type = "stage",
    status = "failed",
    stage = "session",
    step = "created",
    message = message,
    payload = payload
  )
}

costorm_progress_state <- function(events) {
  if (length(events) == 0L) {
    return(NULL)
  }
  tryCatch(
    tempest::tempest_progress_state(events),
    error = function(e) {
      costorm_log("progress reducer failed: %s", conditionMessage(e))
      NULL
    }
  )
}

record_costorm_progress_event <- function(
  progress_events,
  event,
  session = NULL
) {
  update <- function() {
    progress_events(c(shiny::isolate(progress_events()), list(event)))
  }
  if (is.null(session)) {
    update()
  } else {
    shiny::withReactiveDomain(session, update())
  }
  invisible(event)
}

costorm_stage_labels <- function() {
  tempest::tempest_progress_labels("costorm", kind = "stage")
}

session_emit_progress <- function(ses, ...) {
  if (is.null(ses) || is.null(ses$emit_progress)) {
    return(NULL)
  }
  tryCatch(ses$emit_progress(...), error = function(e) NULL)
}

progress_event_id <- function(event) {
  if (S7::S7_inherits(event, tempest::tempest_progress_event)) {
    event@event_id
  } else {
    NA_character_
  }
}

progress_error_payload <- function(error) {
  list(
    error_class = class(error)[[1]],
    error_message = conditionMessage(error)
  )
}

warmup_prompt <- function(topic, expert) {
  seed_questions <- unique(c(
    expert@initial_questions,
    expert@initial_work_items
  ))
  seed_questions <- stringi::stri_trim_both(seed_questions)
  seed_questions <- seed_questions[
    !is.na(seed_questions) & nzchar(seed_questions)
  ]
  seeds <- if (length(seed_questions) > 0L) {
    paste0(
      "\n\nUse these seed questions as planning context:\n- ",
      paste(seed_questions, collapse = "\n- ")
    )
  } else {
    ""
  }
  paste0(
    "Topic: ",
    topic,
    "\n\nGive the panel a concise, evidence-backed orientation from your ",
    "professional perspective. This bounded pass must seed the shared evidence ",
    "ledger, not merely brainstorm. First inspect relevant evidence already in ",
    "the session. If none is available, make exactly one web search, inspect no ",
    "more than two results, and set k = 2 when the search tool accepts k. Do ",
    "not make more than two retrieval or fetch calls. Ground at least one ",
    "orientation claim in an inspected source and preserve its citation. The ",
    "app commits evidence after your response, so do not call add_claim or ",
    "add_fact yourself. Label anything not supported by inspected evidence as ",
    "uncertain.",
    seeds,
    "\n\nIn no more than 250 words, cover:\n",
    "- the lens you bring to this topic;\n",
    "- two or three high-value research questions; and\n",
    "- the main uncertainty, tradeoff, or risk the panel should investigate."
  )
}

record_warmup_orientations_async <- function(
  ses,
  orientations,
  queue = costorm_async_queue(),
  is_current = function() TRUE
) {
  if (length(orientations) == 0L) {
    return(promises::promise_resolve(NULL))
  }
  queue$enqueue(function(queue_current) {
    current <- function() queue_current() && warmup_is_current(is_current)
    if (!current()) {
      return(NULL)
    }
    evidence_results <- vector("list", length(orientations))
    for (orientation in orientations) {
      if (!current()) {
        return(NULL)
      }
      ses$add_turn(
        orientation$name,
        "assistant",
        orientation$response
      )
    }
    commit_one <- function(previous, index) {
      promises::then(previous, function(...) {
        if (!current()) {
          return(NULL)
        }
        orientation <- orientations[[index]]
        has_async_extractor <- is.function(
          ses$chats$extractor$chat_structured_async %||% NULL
        )
        request <- if (has_async_extractor) {
          tempest:::tempest_session_commit_evidence_async(
            ses,
            orientation$response,
            turn = orientation$turn %||% NULL,
            session_id = orientation$session_id,
            expert_id = orientation$expert_id,
            correlation_id = orientation$correlation_id,
            is_current = current
          )
        } else {
          tryCatch(
            {
              source_ids <- harvest_session_sources(
                ses,
                turn = orientation$turn %||% NULL
              )
              ses$expert_session_manager$extract_facts(
                orientation$response,
                turn = orientation$turn %||% NULL,
                source_ids = source_ids,
                session_id = orientation$session_id,
                expert_id = orientation$expert_id,
                correlation_id = orientation$correlation_id
              )
              promises::promise_resolve(list(
                source_ids = source_ids,
                extraction_skipped = NA_character_
              ))
            },
            error = function(error) promises::promise_reject(error)
          )
        }
        promises::then(
          request,
          onFulfilled = function(result) {
            evidence_results[[index]] <<- result
            result
          },
          onRejected = function(error) {
            evidence_results[[index]] <<- list(error = error)
            NULL
          }
        )
      })
    }
    evidence <- Reduce(
      commit_one,
      seq_along(orientations),
      promises::promise_resolve(NULL)
    )
    promises::then(evidence, function(...) {
      if (!current()) {
        return(NULL)
      }
      exchange <- paste(
        Map(
          warmup_orientation_exchange,
          orientations,
          evidence_results
        ),
        collapse = "\n\n---\n\n"
      )
      map_request <- if (is.null(ses$chats$mindmap)) {
        tryCatch(
          {
            ses$update_mindmap(last_exchange = exchange)
            promises::promise_resolve(TRUE)
          },
          error = function(error) promises::promise_reject(error)
        )
      } else {
        tempest:::tempest_session_update_mindmap_async(
          ses,
          last_exchange = exchange,
          is_current = current
        )
      }
      promises::then(
        costorm_async_continue(map_request),
        function(...) {
          list(
            evidence_results = evidence_results,
            evidence_failure_count = sum(vapply(
              evidence_results,
              \(result) !is.null(result$error),
              logical(1)
            ))
          )
        }
      )
    })
  })
}

harvest_session_sources <- function(ses, turn = NULL, chat = NULL) {
  if (is.null(ses) || is.null(ses$harvest_native_sources)) {
    return(character())
  }
  ids <- tryCatch(
    ses$harvest_native_sources(turn = turn, chat = chat),
    error = function(e) character()
  )
  unique(ids[!is.na(ids) & nzchar(ids)])
}

extract_chat_turn_facts <- function(
  ses,
  answer_text,
  turn = NULL,
  source_ids = NULL,
  session_id = NULL,
  expert_id = NA_character_,
  correlation_id = NA_character_
) {
  # Harvest only when the caller has not already done so for this turn; the
  # session's extract_facts also avoids re-harvesting when source_ids are passed.
  if (is.null(source_ids)) {
    source_ids <- harvest_session_sources(ses, turn = turn)
  }
  ses$extract_facts(
    answer_text,
    turn = turn,
    source_ids = source_ids,
    session_id = session_id %||% ses$session_id,
    expert_id = expert_id,
    correlation_id = correlation_id
  )
  invisible(source_ids)
}

warmup_summary <- function(
  ses,
  orientation_count,
  expert_count,
  evidence_failure_count = 0L
) {
  source_count <- length(ses$store$list_sources())
  claim_count <- length(ses$store$list_claims())
  evidence_text <- if (source_count > 0L && claim_count > 0L) {
    claim_label <- if (claim_count == 1L) "fact" else "facts"
    source_label <- if (source_count == 1L) "source" else "sources"
    paste0(
      " Collected ",
      claim_count,
      " source-backed ",
      claim_label,
      " from ",
      source_count,
      " ",
      source_label,
      "."
    )
  } else {
    paste0(
      " No citable evidence was collected; the orientations are scoping ",
      "context only, not research findings."
    )
  }
  failure_text <- if (evidence_failure_count > 0L) {
    paste0(
      " Evidence processing failed for ",
      evidence_failure_count,
      " expert orientation(s)."
    )
  } else {
    ""
  }
  paste0(
    "**Warmup complete.** Oriented ",
    orientation_count,
    " of ",
    expert_count,
    " experts around the topic.",
    evidence_text,
    failure_text
  )
}

turn_evidence_gap_message <- function() {
  paste(
    "**Evidence gap:** This answer did not add or cite an inspected source.",
    "Treat its factual statements as unverified."
  )
}

evidence_result_has_sources <- function(result) {
  length(result$source_ids %||% character()) > 0L
}

turn_mindmap_exchange <- function(user_text, answer_text, evidence_result) {
  tempest:::tempest_costorm_mindmap_exchange(
    user_text,
    answer_text,
    evidence_result$source_ids %||% character()
  )
}

warmup_orientation_exchange <- function(orientation, evidence_result) {
  status <- if (evidence_result_has_sources(evidence_result)) {
    "Evidence-backed orientation"
  } else {
    paste(
      "Scoping-only orientation with no cited source.",
      "Do not add its factual claims to the mind map"
    )
  }
  paste0(
    status,
    " from ",
    orientation$name,
    ":\n",
    orientation$response
  )
}

warmup_is_current <- function(is_current) {
  tryCatch(
    isTRUE(is_current()),
    error = function(e) FALSE
  )
}

warmup_timeout_condition <- function(label, timeout_s) {
  structure(
    list(
      message = sprintf("%s timed out after %.0f seconds", label, timeout_s),
      call = NULL,
      label = label,
      timeout_s = timeout_s
    ),
    class = c("tempest_warmup_timeout", "error", "condition")
  )
}

warmup_with_timeout <- function(promise, timeout_s, label = "Warmup step") {
  if (is.null(timeout_s) || !is.finite(timeout_s) || timeout_s <= 0) {
    return(promise)
  }

  promises::promise(function(resolve, reject) {
    settled <- FALSE

    later::later(
      function() {
        if (!settled) {
          settled <<- TRUE
          reject(warmup_timeout_condition(label, timeout_s))
        }
      },
      delay = timeout_s
    )

    promises::then(
      promise,
      onFulfilled = function(value) {
        if (!settled) {
          settled <<- TRUE
          resolve(value)
        }
      },
      onRejected = function(err) {
        if (!settled) {
          settled <<- TRUE
          reject(err)
        }
      }
    )
  })
}

warmup_chat_response <- function(expert_chat, prompt) {
  response <- tryCatch(
    expert_chat$chat_async(prompt),
    error = function(e) promises::promise_reject(e)
  )
  promises::then(
    promises::promise_resolve(response),
    function(value) warmup_response_text(expert_chat, value)
  )
}

warmup_response_text <- function(expert_chat, response) {
  turn <- tryCatch(expert_chat$last_turn(), error = function(e) NULL)
  text <- tryCatch(
    if (is.null(turn)) "" else ellmer::contents_markdown(turn),
    error = function(e) ""
  )
  if (nzchar(text)) {
    return(text)
  }
  if (is.character(response) && length(response) > 0) {
    return(paste(response, collapse = ""))
  }
  ""
}

warmup_safe_append <- function(text, append_chat, is_current) {
  if (warmup_is_current(is_current)) {
    append_chat(text)
  }
  invisible(NULL)
}

warmup_safe_touch <- function(store, is_current) {
  if (warmup_is_current(is_current)) {
    store$touch()
  }
  invisible(NULL)
}

# Ask each fully equipped expert for one bounded orientation, then update the
# shared mind map once from the complete panel. Returns a promise that resolves
# when warmup finishes.
run_warmup <- function(
  ses,
  store,
  append_chat,
  is_current = function() TRUE,
  timeout_s = getOption("tempest.shiny.warmup_timeout_s", 120),
  max_parallel_experts = getOption(
    "tempest.shiny.warmup_max_parallel_experts",
    3L
  ),
  queue = costorm_async_queue()
) {
  if (!warmup_is_current(is_current)) {
    return(promises::promise_resolve(NULL))
  }

  safe_append <- function(text) {
    warmup_safe_append(text, append_chat, is_current)
  }
  safe_touch <- function() {
    warmup_safe_touch(store, is_current)
  }
  emit_progress <- function(...) {
    session_emit_progress(ses, ...)
  }

  warmup_event <- emit_progress(
    "stage",
    "started",
    stage = "warmup",
    step = "expert_fanout",
    payload = list(expert_count = length(ses$experts))
  )

  if (is.null(max_parallel_experts) || length(max_parallel_experts) == 0L) {
    max_parallel_experts <- 3L
  } else {
    max_parallel_experts <- suppressWarnings(
      as.integer(max_parallel_experts[[1]])
    )
  }
  if (is.na(max_parallel_experts) || max_parallel_experts < 1L) {
    max_parallel_experts <- 1L
  }

  state <- new.env(parent = emptyenv())
  state$orientations <- vector("list", length(ses$experts))
  state$failure_count <- 0L

  orient_expert <- function(idx) {
    expert <- ses$experts[[idx]]
    name <- expert@name
    expert_id <- expert@expert_id
    expert_correlation_id <- paste("warmup-expert", expert_id, sep = "-")
    expert_event <- NULL
    expert_chat <- NULL
    expert_session_id <- NULL
    expert_provenance <- NULL
    old_provenance <- list()
    capability_count <- 0L
    orientation_active <- FALSE
    retirement <- list(retired = FALSE, cancellation_supported = FALSE)
    restore_provenance <- function() {
      if (!is.null(expert_provenance)) {
        expert_provenance$current <- old_provenance
      }
      invisible(NULL)
    }
    retire_expert_session <- function() {
      if (isTRUE(retirement$retired) || is.null(expert_session_id)) {
        return(retirement)
      }
      retirement <<- tryCatch(
        ses$expert_session_manager$retire_session(expert_session_id),
        error = function(retire_error) {
          list(retired = FALSE, cancellation_supported = FALSE)
        }
      )
      retirement
    }
    promises::then(promises::promise_resolve(NULL), function(...) {
      if (!warmup_is_current(is_current)) {
        return(NULL)
      }
      session_result <- ses$expert_session_manager$get_or_create(expert_id)
      expert_chat <<- session_result$chat
      expert_session_id <<- session_result$session_id
      expert_provenance <<- session_result$provenance
      old_provenance <<- expert_provenance$current %||% list()
      expert_provenance$current <- list(
        session_id = expert_session_id,
        expert_id = expert_id,
        retrieval_step_id = expert_correlation_id
      )
      capability_count <<- sum(vapply(
        session_result$grants %||% list(),
        \(grant) identical(grant$status, "granted"),
        logical(1)
      ))
      expert_event <<- emit_progress(
        "expert",
        "started",
        stage = "warmup",
        step = "expert_fanout",
        parent_event_id = progress_event_id(warmup_event),
        correlation_id = expert_correlation_id,
        payload = list(
          expert_id = expert_id,
          expert_name = name,
          mode = "bounded_research",
          session_id = expert_session_id,
          tools_available = capability_count > 0L,
          capability_count = capability_count
        )
      )
      orientation_active <<- TRUE
      request <- warmup_chat_response(
        expert_chat,
        warmup_prompt(ses$topic, expert)
      )
      work <- promises::then(request, function(response) {
        if (!orientation_active) {
          return(NULL)
        }
        if (!warmup_is_current(is_current)) {
          orientation_active <<- FALSE
          restore_provenance()
          retire_expert_session()
          return(NULL)
        }
        if (!nzchar(trimws(response))) {
          stop("The expert returned an empty orientation.")
        }
        turn <- tryCatch(
          expert_chat$last_turn(),
          error = function(error) NULL
        )
        state$orientations[[idx]] <- list(
          expert_id = expert_id,
          name = name,
          response = response,
          turn = turn,
          session_id = expert_session_id,
          correlation_id = expert_correlation_id
        )
        orientation_active <<- FALSE
        restore_provenance()
        emit_progress(
          "expert",
          "succeeded",
          stage = "warmup",
          step = "expert_fanout",
          parent_event_id = progress_event_id(expert_event),
          correlation_id = expert_correlation_id,
          payload = list(
            expert_id = expert_id,
            expert_name = name,
            mode = "bounded_research",
            session_id = expert_session_id,
            tools_available = capability_count > 0L,
            capability_count = capability_count
          )
        )
        NULL
      })
      warmup_with_timeout(work, timeout_s, paste(name, "orientation")) |>
        promises::catch(function(error) {
          orientation_active <<- FALSE
          restore_provenance()
          timed_out <- inherits(error, "tempest_warmup_timeout")
          if (timed_out) {
            retire_expert_session()
          }
          if (!warmup_is_current(is_current)) {
            return(NULL)
          }
          state$failure_count <- state$failure_count + 1L
          failure_kind <- if (timed_out) "timeout" else "provider_error"
          emit_progress(
            "expert",
            "failed",
            stage = "warmup",
            step = "expert_fanout",
            parent_event_id = progress_event_id(expert_event),
            correlation_id = expert_correlation_id,
            payload = c(
              list(
                expert_id = expert_id,
                expert_name = name,
                mode = "bounded_research",
                session_id = expert_session_id,
                tools_available = capability_count > 0L,
                capability_count = capability_count,
                failure_kind = failure_kind,
                session_retired = retirement$retired,
                cancellation_supported = retirement$cancellation_supported
              ),
              progress_error_payload(error)
            )
          )
          if (timed_out) {
            safe_append(sprintf(
              "**%s** orientation timed out after %.0f seconds; continuing with the rest of the panel.",
              name,
              timeout_s
            ))
          } else {
            safe_append(sprintf(
              "**%s** orientation was unavailable: %s",
              name,
              conditionMessage(error)
            ))
          }
          NULL
        })
    }) |>
      promises::catch(function(error) {
        restore_provenance()
        if (warmup_is_current(is_current)) {
          state$failure_count <- state$failure_count + 1L
          emit_progress(
            "expert",
            "failed",
            stage = "warmup",
            step = "expert_fanout",
            parent_event_id = progress_event_id(expert_event),
            correlation_id = expert_correlation_id,
            payload = c(
              list(
                expert_id = expert_id,
                expert_name = name,
                mode = "bounded_research",
                session_id = expert_session_id,
                tools_available = capability_count > 0L,
                capability_count = capability_count
              ),
              progress_error_payload(error)
            )
          )
          safe_append(sprintf(
            "**%s** orientation was unavailable: %s",
            name,
            conditionMessage(error)
          ))
        }
        NULL
      })
  }

  expert_indices <- seq_along(ses$experts)
  batches <- split(
    expert_indices,
    ceiling(seq_along(expert_indices) / max_parallel_experts)
  )
  run_batch <- function(prev, batch) {
    promises::then(prev, function(...) {
      if (!warmup_is_current(is_current)) {
        return(NULL)
      }
      promises::promise_all(.list = lapply(batch, orient_expert)) |>
        promises::then(function(...) NULL)
    })
  }

  chain <- Reduce(run_batch, batches, promises::promise_resolve(NULL))
  promises::then(chain, function(...) {
    if (!warmup_is_current(is_current)) {
      return(NULL)
    }
    orientations <- Filter(Negate(is.null), state$orientations)
    recorded <- record_warmup_orientations_async(
      ses,
      orientations,
      queue = queue,
      is_current = is_current
    )
    promises::then(costorm_async_continue(recorded), function(record_result) {
      if (!warmup_is_current(is_current)) {
        return(NULL)
      }
      safe_append(warmup_summary(
        ses = ses,
        orientation_count = length(orientations),
        expert_count = length(ses$experts),
        evidence_failure_count = record_result$evidence_failure_count %||% 0L
      ))
      emit_progress(
        "stage",
        "succeeded",
        stage = "warmup",
        step = "expert_fanout",
        parent_event_id = progress_event_id(warmup_event),
        payload = list(
          expert_count = length(ses$experts),
          orientation_count = length(orientations),
          failure_count = state$failure_count,
          evidence_failure_count = record_result$evidence_failure_count %||% 0L,
          source_count = length(ses$store$list_sources()),
          claim_count = length(ses$store$list_claims()),
          bounded_research = TRUE
        )
      )
      safe_touch()
    })
  })
}
