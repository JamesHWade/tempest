# Chat tab: the interactive Co-STORM session.
#
# Package lifecycle functions own warmup and post-turn processing for the live
# `TempestSession`. This module owns only Shiny reactivity and presentation,
# while the shinychat adapter owns the provider widget lifecycle.

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
          tempest:::tempest_shinychat_ui(
            ns("chat"),
            height = "100%",
            greeting = welcome_message(),
            icon_assistant = tempest_chat_icon(),
            allow_attachments = tempest_chat_attachment_types(),
            footer = chat_footer_ui(ns)
          ),
          tempest:::tempest_shinychat_citation_sanitizer(ns("chat"))
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
    current_source_store <- function() {
      ses <- tryCatch(shiny::isolate(store$get()), error = function(e) NULL)
      if (is.null(ses)) {
        return(NULL)
      }
      citation_source_store(ses$store %||% NULL)
    }
    chat <- NULL
    append_chat <- function(text) {
      chat$append(text, role = "assistant")
    }
    append_suggestion_cards <- function(cards) {
      chat$append_suggestions(cards)
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
    process_completed_turn <- function(
      user_text,
      assistant_text,
      assistant_turn,
      is_current
    ) {
      ses <- tryCatch(shiny::isolate(store$get()), error = function(error) NULL)
      if (is.null(ses)) {
        return(invisible(NULL))
      }
      turn_session_id <- active_session_id
      suggest_enabled <- shiny::isolate(isTRUE(input$suggest))
      turn_id <- tempest:::tempest_uuid("chat-turn")
      task <- work_queue$enqueue(function(queue_current) {
        current <- function() {
          queue_current() &&
            is_current() &&
            !isTRUE(session_ended) &&
            identical(turn_session_id, active_session_id)
        }
        tempest::tempest_session_process_turn_async(
          ses,
          user_text = user_text,
          assistant_text = assistant_text,
          provider_turn = assistant_turn,
          suggest = suggest_enabled,
          n_suggestions = 4L,
          turn_id = turn_id,
          is_current = current
        )
      })
      promises::then(
        task,
        onFulfilled = function(result) {
          shiny::withReactiveDomain(session, {
            current <- !isTRUE(session_ended) &&
              identical(turn_session_id, active_session_id) &&
              is_current() &&
              !identical(result@status, "cancelled")
            if (!current) {
              return(NULL)
            }
            for (notice in result@notices) {
              message <- turn_notice_message(notice)
              if (nzchar(message)) {
                append_chat_if_active(message, turn_session_id)
              }
            }
            if (identical(result@suggestion_status, "generated")) {
              cards <- tempest:::tempest_shinychat_suggestion_cards(
                result@suggestions
              )
              if (!is.null(cards)) {
                append_suggestion_cards_if_active(cards, turn_session_id)
              }
            }
            store$touch()
            result
          })
        },
        onRejected = function(error) {
          if (
            !isTRUE(session_ended) &&
              identical(turn_session_id, active_session_id) &&
              is_current()
          ) {
            warning("Turn processing failed: ", conditionMessage(error))
          }
          NULL
        }
      )
      invisible(task)
    }
    # Tempest owns complete session restoration. shinychat history remains off
    # because it cannot restore evidence, experts, mind-map, or artifact state.
    chat <- tempest:::tempest_shinychat_adapter(
      "chat",
      initial_client = initial_chat,
      session = session,
      on_turn = process_completed_turn,
      source_store = current_source_store,
      render_message = function(text, role, source_store) {
        citation_markdown(text, store = source_store)
      }
    )
    record_progress <- function(event) {
      record_costorm_progress_event(progress_events, event, session)
    }
    session_root <- session_storage_root(session)
    autosave_path <- file.path(session_root, "autosave")
    session$onSessionEnded(function() {
      unlink(session_root, recursive = TRUE, force = TRUE)
    })

    restore_progress_history <- function(ses) {
      progress_events(tempest::tempest_execution_events(ses))
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
      report_available <- !is.null(ses$artifact_catalog) &&
        ses$artifact_catalog$has("report_md")
      messages <- tempest:::tempest_shinychat_restore_messages(
        ses$transcript,
        topic = ses$topic,
        report_available = report_available
      )
      chat$bind(
        ses$chats$moderator,
        messages = messages,
        client_history = "keep"
      )
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
      chat$bind(
        ses$chats$moderator,
        messages = list(list(
          role = "assistant",
          content = paste0(
            "Co-STORM session started for: **",
            topic,
            "**",
            expert_intro(ses),
            "\n\nAsk questions, request sources, or ask for a report.\n"
          )
        )),
        client_history = "clear"
      )
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
      chat$reset(paste(
        "Session cleared.",
        "Enter a topic in the sidebar and start a new Co-STORM session."
      ))
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
      chat$register_commands(list(
        new = list(
          description = "Clear the current Co-STORM session.",
          handler = function() run_chat_command("new")
        ),
        `new-session` = list(
          description = "Clear the current Co-STORM session.",
          handler = function() run_chat_command("new")
        ),
        experts = list(
          description = "Show the current expert panel.",
          handler = function() run_chat_command("experts")
        ),
        sources = list(
          description = "Summarize collected sources.",
          handler = function() run_chat_command("sources")
        ),
        facts = list(
          description = "Summarize collected facts.",
          handler = function() run_chat_command("facts")
        ),
        claims = list(
          description = "Summarize collected claims.",
          handler = function() run_chat_command("facts")
        ),
        report = list(
          description = "Generate a report from collected evidence.",
          handler = function() run_chat_command("report")
        ),
        system = list(
          description = "Show the moderator system prompt.",
          handler = function() run_chat_command("system")
        ),
        tools = list(
          description = "Show runtime tools and command status.",
          handler = function() run_chat_command("tools")
        )
      ))
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
                              current <- function() {
                                queue_current() && warmup_is_current()
                              }
                              request <-
                                tempest:::tempest_session_suggest_questions_async(
                                  ses,
                                  n = 4L,
                                  is_current = current
                                )
                              promises::then(
                                request,
                                onFulfilled = function(questions) {
                                  if (!current()) {
                                    return(NULL)
                                  }
                                  cards <-
                                    tempest:::tempest_shinychat_suggestion_cards(
                                      questions
                                    )
                                  if (!is.null(cards)) {
                                    append_suggestion_cards_if_active(
                                      cards,
                                      session_id = start_session_id
                                    )
                                  }
                                  NULL
                                },
                                onRejected = function(error) {
                                  if (current()) {
                                    costorm_log(
                                      "suggestions failed: %s",
                                      conditionMessage(error)
                                    )
                                  }
                                  NULL
                                }
                              )
                            })
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
                    warmup_request <- work_queue$enqueue(
                      function(queue_current) {
                        tempest::tempest_session_warmup_async(
                          ses,
                          is_current = function() {
                            queue_current() && warmup_is_current()
                          }
                        )
                      }
                    )
                    warmup_done <- promises::then(
                      warmup_request,
                      onFulfilled = function(result) {
                        shiny::withReactiveDomain(session, {
                          if (
                            !warmup_is_current() ||
                              identical(result@status, "cancelled")
                          ) {
                            return(NULL)
                          }
                          for (message in warmup_result_messages(result)) {
                            append_chat_if_active(message, start_session_id)
                          }
                          store$touch()
                          costorm_log(
                            "warmup finished: %s (%s)",
                            ses$session_id,
                            result@status
                          )
                          result
                        })
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

chat_runtime_counts <- function(ses) {
  if (is.null(ses)) {
    return(list(experts = 0L, sources = 0L, facts = 0L, report = FALSE))
  }
  sources <- tryCatch(ses$store$list_sources(), error = function(e) list())
  claims <- tryCatch(ses$store$list_claims(), error = function(e) list())
  report <- tryCatch(
    !is.null(ses$artifact_catalog) &&
      ses$artifact_catalog$has("report_md") &&
      nzchar(ses$artifact_catalog$get("report_md")@content),
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

progress_error_payload <- function(error) {
  list(
    error_class = class(error)[[1]],
    error_message = conditionMessage(error)
  )
}

warmup_summary <- function(result) {
  evidence_text <- if (result@source_count > 0L && result@claim_count > 0L) {
    claim_label <- if (result@claim_count == 1L) "fact" else "facts"
    source_label <- if (result@source_count == 1L) "source" else "sources"
    paste0(
      " Collected ",
      result@claim_count,
      " source-backed ",
      claim_label,
      " from ",
      result@source_count,
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
  failure_text <- if (result@evidence_failure_count > 0L) {
    paste0(
      " Evidence processing failed for ",
      result@evidence_failure_count,
      " expert orientation(s)."
    )
  } else {
    ""
  }
  paste0(
    "**Warmup complete.** Oriented ",
    result@orientation_count,
    " of ",
    result@expert_count,
    " experts around the topic.",
    evidence_text,
    failure_text
  )
}

warmup_result_messages <- function(result) {
  failures <- vapply(
    result@orientations,
    function(orientation) {
      if (identical(orientation$status, "succeeded")) {
        return(NA_character_)
      }
      if (identical(orientation$status, "timeout")) {
        return(paste0(
          "**",
          orientation$expert_name,
          "** orientation timed out; continuing with the rest of the panel."
        ))
      }
      detail <- orientation$error_message
      if (is.na(detail) || !nzchar(detail)) {
        detail <- "No error details were returned."
      }
      paste0(
        "**",
        orientation$expert_name,
        "** orientation was unavailable: ",
        detail
      )
    },
    character(1)
  )
  c(failures[!is.na(failures)], warmup_summary(result))
}

turn_evidence_gap_message <- function() {
  paste(
    "**Evidence gap:** This answer did not add or cite an inspected source.",
    "Treat its factual statements as unverified."
  )
}

turn_notice_message <- function(notice) {
  detail <- notice@details$error_message %||% ""
  switch(
    notice@code,
    evidence_gap = turn_evidence_gap_message(),
    evidence_failed = paste0(
      "**Evidence processing failed:** This answer was not added to the ",
      "evidence ledger. ",
      detail
    ),
    mindmap_failed = paste0("**Mind map update failed:** ", detail),
    suggestions_failed = paste0("**Suggestion generation failed:** ", detail),
    ""
  )
}

warmup_is_current <- function(is_current) {
  tryCatch(
    isTRUE(is_current()),
    error = function(e) FALSE
  )
}
