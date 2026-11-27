# Chat tab: the interactive Co-STORM session.
#
# Package lifecycle functions own warmup and post-turn processing for the live
# `TempestSession`. This module owns only Shiny reactivity and presentation,
# while the shinychat adapter owns the provider widget lifecycle.

mod_chat_ui <- function(id, config_ui, allow_user_experts = FALSE) {
  ns <- shiny::NS(id)
  bslib::nav_panel(
    title = shiny::tagList(
      shiny::icon("comments"),
      shiny::span("Chat", class = "tempest-nav-label")
    ),
    value = "Chat",
    bslib::layout_sidebar(
      class = "tempest-chat-layout",
      fillable = TRUE,
      fill = TRUE,
      height = "100%",
      gap = "1rem",
      sidebar = chat_settings_sidebar_ui(ns, config_ui),
      bslib::card(
        class = "tempest-chat-card border-0",
        full_screen = TRUE,
        bslib::card_body(
          class = "p-0 tempest-chat-card-body",
          fill = TRUE,
          tempest:::tempest_shinychat_ui(
            ns("chat"),
            height = "100%",
            greeting = chat_session_greeting_ui(
              ns,
              allow_user_experts = allow_user_experts
            ),
            icon_assistant = tempest_chat_icon(),
            allow_attachments = tempest_chat_attachment_types(),
            footer = chat_footer_ui(ns)
          ),
          tempest:::tempest_shinychat_citation_sanitizer(ns("chat")),
          chat_settings_drawer_controller_ui(
            drawer_id = ns("settings"),
            title_id = ns("settings_title"),
            trigger_ids = c(
              ns("setup_settings_toggle"),
              ns("footer_settings_toggle")
            ),
            footer_id = ns("runtime_footer")
          ),
          shiny::uiOutput(ns("report_error"))
        )
      )
    )
  )
}

chat_session_greeting_ui <- function(ns, allow_user_experts = FALSE) {
  research_options <- shiny::tags$details(
    id = ns("research_options_popover"),
    class = "tempest-chat-details tempest-chat-research-options-details",
    shiny::tags$summary(
      id = ns("research_options"),
      class = "bslib-toolbar-input-button tempest-chat-details-summary",
      `aria-controls` = ns("research_options_panel"),
      shiny::icon("sliders"),
      shiny::span(
        class = "bslib-toolbar-label visually-hidden",
        "Research options"
      ),
      shiny::div(style = "display:contents;", "Research options")
    ),
    shiny::div(
      id = ns("research_options_panel"),
      class = "tempest-chat-details-panel tempest-chat-research-options",
      shiny::checkboxInput(
        ns("warmup"),
        "Orient experts before chat",
        FALSE
      ),
      shiny::checkboxInput(
        ns("suggest"),
        "Suggest follow-up questions",
        TRUE
      ),
      shiny::p(
        class = "small text-body-secondary mb-0",
        "An optional evidence-gathering pass runs before you start asking questions."
      )
    )
  )

  shiny::div(
    class = "tempest-chat-welcome",
    shiny::div(
      class = "tempest-chat-welcome-kicker",
      shiny::icon("compass"),
      shiny::span("Co-STORM discussion")
    ),
    shiny::div(
      class = "tempest-chat-welcome-copy",
      shiny::h2(
        class = "tempest-display-heading h3 mb-2",
        "Start a research discussion"
      ),
      shiny::p(
        class = "lead mb-2",
        "Choose a focused topic and an expert panel, then follow cited evidence as the discussion develops."
      )
    ),
    shiny::div(
      class = "tempest-chat-welcome-form",
      shiny::div(
        class = "tempest-chat-welcome-topic",
        shiny::textInput(
          ns("topic"),
          "Research topic",
          placeholder = "e.g., How do grid-scale batteries affect peak demand?"
        )
      ),
      shiny::div(
        class = "tempest-chat-welcome-actions",
        shiny::div(
          class = "tempest-chat-welcome-experts",
          expert_setup_control_ui(ns, allow_user_experts)
        ),
        bslib::toolbar(
          class = "tempest-chat-welcome-tools",
          align = "left",
          gap = ".35rem",
          width = "auto",
          research_options,
          bslib::toolbar_input_button(
            ns("setup_settings_toggle"),
            "Workspace settings",
            icon = shiny::icon("gear"),
            `aria-controls` = ns("settings"),
            `aria-expanded` = "false",
            `data-tempest-settings-trigger` = "true",
            title = "Open workspace settings",
            border = TRUE
          )
        ),
        bslib::input_task_button(
          ns("start"),
          "Start session",
          icon = shiny::icon("play"),
          label_busy = "Starting...",
          auto_reset = FALSE,
          class = "tempest-chat-start btn-sm",
          title = "Start the Co-STORM discussion"
        )
      ),
      shiny::uiOutput(
        ns("start_validation"),
        class = "tempest-chat-start-feedback"
      )
    )
  )
}

chat_settings_sidebar_ui <- function(ns, config_ui) {
  bslib::sidebar(
    id = ns("settings"),
    title = shiny::h2(
      id = ns("settings_title"),
      class = "sidebar-title h5 d-flex align-items-center gap-2",
      tabindex = "-1",
      shiny::span(`aria-hidden` = "true", shiny::icon("sliders")),
      shiny::span("Workspace settings")
    ),
    position = "right",
    open = FALSE,
    width = 340,
    resizable = FALSE,
    class = "tempest-chat-settings",
    shiny::div(
      class = "tempest-chat-settings-intro",
      shiny::p(
        class = "small text-body-secondary mb-0",
        "Session controls and defaults for the next discussion."
      )
    ),
    bslib::accordion(
      id = ns("session_settings"),
      open = FALSE,
      multiple = TRUE,
      bslib::accordion_panel(
        title = "Expert panel",
        icon = shiny::icon("users"),
        shiny::uiOutput(ns("expert_panel"))
      ),
      bslib::accordion_panel(
        title = "Session storage",
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
          placeholder = "No bundle selected",
          width = "100%"
        ),
        shiny::uiOutput(ns("session_persistence"))
      )
    ),
    if (!is.null(config_ui)) {
      shiny::div(
        class = "tempest-chat-config",
        config_ui
      )
    }
  )
}

mod_chat_server <- function(
  id,
  config,
  store,
  experts = NULL,
  session_id = NULL,
  program_set = NULL,
  knowledge = NULL,
  allow_user_experts = FALSE
) {
  shiny::moduleServer(id, function(input, output, session) {
    report_navigation_event <- shiny::reactiveVal(0L)
    report_error_message <- shiny::reactiveVal(NULL)
    start_validation_message <- shiny::reactiveVal(NULL)
    persistence_error_message <- shiny::reactiveVal(NULL)
    suggestions_enabled <- shiny::reactiveVal(TRUE)
    progress_events <- shiny::reactiveVal(list())
    warmup_run_id <- 0L
    active_session_id <- 0L
    session_ended <- FALSE
    work_queue <- costorm_async_queue()
    expert_setup_mode <- shiny::reactiveVal("generated")
    generated_expert_count <- shiny::reactiveVal(3L)
    user_experts <- shiny::reactiveVal(list())

    output$start_validation <- shiny::renderUI({
      message <- start_validation_message()
      if (is.null(message)) {
        return(NULL)
      }
      shiny::div(
        class = "alert alert-danger mt-2 mb-0",
        role = "alert",
        `aria-live` = "assertive",
        `aria-atomic` = "true",
        message
      )
    })
    output$report_error <- shiny::renderUI({
      message <- report_error_message()
      if (is.null(message)) {
        return(NULL)
      }
      shiny::div(
        class = "alert alert-danger m-2",
        role = "alert",
        message
      )
    })

    resolve_program_set <- function() {
      value <- shiny::isolate(reactive_or_value(program_set)) %||%
        tempest:::tempest_program_set()
      tempest:::tempest_program_set_manifest_programs(value)
      value
    }

    resolve_program_binding <- function() {
      program_set_value <- resolve_program_set()
      knowledge_value <- shiny::isolate(
        reactive_or_value(knowledge)
      )
      tempest:::tempest_knowledge_argument(knowledge_value, admit = FALSE)
      list(program_set = program_set_value, knowledge = knowledge_value)
    }

    update_expert_setup_button <- function() {
      if (!isTRUE(allow_user_experts)) {
        return(invisible(NULL))
      }
      mode <- shiny::isolate(expert_setup_mode())
      count <- if (identical(mode, "custom")) {
        length(shiny::isolate(user_experts()))
      } else {
        shiny::isolate(generated_expert_count())
      }
      shiny::updateActionButton(
        session,
        "expert_setup",
        label = expert_setup_button_label(mode, count),
        icon = shiny::icon("users")
      )
      invisible(NULL)
    }

    if (isTRUE(allow_user_experts)) {
      output$custom_expert_fields <- shiny::renderUI({
        count <- as.integer(input$custom_expert_count %||% 3L)
        drafts <- custom_expert_drafts(user_experts())
        for (index in seq_len(count)) {
          current <- if (length(drafts) >= index) {
            drafts[[index]]
          } else {
            list()
          }
          for (field in c("name", "title", "perspective")) {
            value <- shiny::isolate(
              input[[custom_expert_input_id(field, index)]]
            )
            if (!is.null(value)) {
              current[[field]] <- value
            }
          }
          drafts[[index]] <- current
        }
        custom_expert_fields_ui(session$ns, count, drafts)
      })

      shiny::observeEvent(input$expert_setup, {
        shiny::showModal(custom_expert_setup_modal(
          session$ns,
          mode = expert_setup_mode(),
          generated_count = generated_expert_count(),
          custom_experts = user_experts()
        ))
      })

      shiny::observeEvent(input$apply_expert_setup, {
        mode <- input$expert_setup_mode %||% "generated"
        if (identical(mode, "generated")) {
          generated_expert_count(expert_setup_count(
            input$generated_expert_count %||% 3L
          ))
        } else {
          profiles <- tryCatch(
            custom_expert_profiles(
              custom_expert_specs_from_input(
                input,
                input$custom_expert_count %||% 3L
              )
            ),
            tempest_custom_expert_input_error = function(error) {
              shiny::showNotification(
                conditionMessage(error),
                type = "error",
                duration = 8
              )
              NULL
            }
          )
          if (is.null(profiles)) {
            return()
          }
          user_experts(profiles)
        }
        expert_setup_mode(mode)
        update_expert_setup_button()
        shiny::removeModal()
      })
    }

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
        paste(
          "Ask the user to start a Co-STORM session from the welcome panel",
          "before research chat."
        ),
        sep = "\n"
      ),
      echo = "none"
    )
    current_workspace <- function() {
      ses <- tryCatch(
        shiny::isolate(store$costorm_session()),
        error = function(e) NULL
      )
      if (is.null(ses)) {
        return(NULL)
      }
      citation_workspace(tempest:::tempest_session_workspace(ses) %||% NULL)
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
      ses,
      turn_session_id,
      completion_id,
      is_current
    ) {
      turn_suggestions_enabled <- shiny::isolate(suggestions_enabled())
      cancel_completion <- function() {
        tryCatch(
          {
            status <- tempest:::tempest_session_agent_completion_status(
              ses,
              completion_id
            )
            if (identical(status, "issued")) {
              tempest:::tempest_session_agent_completion_cancel(
                ses,
                completion_id
              )
            }
          },
          error = function(error) NULL
        )
        invisible(NULL)
      }
      task <- tryCatch(
        work_queue$enqueue(
          function(queue_current) {
            current <- function() {
              queue_current() &&
                is_current() &&
                !isTRUE(session_ended) &&
                identical(turn_session_id, active_session_id)
            }
            tempest:::tempest_session_process_turn_async(
              ses,
              completion_id = completion_id,
              suggest = turn_suggestions_enabled,
              n_suggestions = 4L,
              is_current = current
            )
          },
          on_cancel = cancel_completion
        ),
        error = function(error) {
          cancel_completion()
          NULL
        }
      )
      if (is.null(task)) {
        return(invisible(NULL))
      }
      promises::then(
        task,
        onFulfilled = function(result) {
          shiny::withReactiveDomain(session, {
            if (is.null(result)) {
              return(NULL)
            }
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
            store$touch_costorm_session()
            result
          })
        },
        onRejected = function(error) {
          if (
            !isTRUE(session_ended) &&
              identical(turn_session_id, active_session_id) &&
              is_current()
          ) {
            warning("Turn processing failed.")
          }
          NULL
        }
      )
      invisible(task)
    }
    session_turn_callback <- function(ses, turn_session_id) {
      force(ses)
      force(turn_session_id)
      function(completion_id, is_current) {
        process_completed_turn(
          ses = ses,
          turn_session_id = turn_session_id,
          completion_id = completion_id,
          is_current = is_current
        )
      }
    }
    # Tempest owns complete session restoration. shinychat history remains off
    # because it cannot restore evidence, experts, mind-map, or artifact state.
    chat <- tempest:::tempest_shinychat_adapter(
      "chat",
      initial_client = initial_chat,
      session = session,
      on_turn = function(...) invisible(NULL),
      workspace = current_workspace,
      render_message = function(text, role, workspace) {
        citation_markdown(text, workspace = workspace)
      }
    )
    record_progress <- function(event) {
      record_costorm_progress_event(progress_events, event, session)
    }
    session_root <- session_storage_root(session)
    session$onSessionEnded(function() {
      unlink(session_root, recursive = TRUE, force = TRUE)
    })

    restore_progress_history <- function(ses) {
      progress_events(tempest:::tempest_execution_events(ses))
      invisible(NULL)
    }

    restore_session_bundle <- function(path) {
      warmup_run_id <<- warmup_run_id + 1L
      active_session_id <<- active_session_id + 1L
      work_queue$cancel()
      binding <- resolve_program_binding()
      ses <- store$resume_costorm_session(
        path,
        config = config(),
        progress = record_progress,
        program_set = binding$program_set,
        knowledge = binding$knowledge
      )
      shiny::updateTextInput(session, "topic", value = ses$topic %||% "")
      restored_count <- max(1L, min(5L, length(ses$experts %||% list())))
      if (isTRUE(allow_user_experts)) {
        generated_expert_count(restored_count)
        if (identical(expert_setup_mode(), "generated")) {
          update_expert_setup_button()
        }
      } else {
        shiny::updateSelectInput(
          session,
          "n_experts",
          selected = restored_count
        )
      }
      restore_progress_history(ses)
      report_available <- nzchar(
        shiny::isolate(store$report_md()) %||% ""
      )
      messages <- tempest:::tempest_shinychat_restore_messages(
        ses$transcript,
        topic = ses$topic,
        report_available = report_available
      )
      chat$bind(
        tempest:::tempest_session_chat(ses, "moderator"),
        messages = messages,
        client_history = "keep",
        on_turn = session_turn_callback(ses, active_session_id)
      )
      ses
    }

    output$session_persistence <- shiny::renderUI({
      shiny::tagList(
        session_persistence_status_ui(store$costorm_persistence_status()),
        session_persistence_error_ui(persistence_error_message())
      )
    })

    # --- Session lifecycle ---------------------------------------------------
    output$save_session <- shiny::downloadHandler(
      filename = function() {
        ses <- store$peek_costorm_session()
        topic <- if (is.null(ses)) "session" else ses$topic %||% "session"
        paste0("tempest-", topic_slug(topic), ".zip")
      },
      content = function(file) {
        persistence_error_message(NULL)
        tryCatch(
          session_archive_write(store, file),
          error = function(error) {
            persistence_error_message(
              "Could not download the session bundle."
            )
            stop(error)
          }
        )
      },
      contentType = "application/zip"
    )

    shiny::observeEvent(input$load_session, {
      upload <- input$load_session
      shiny::req(upload$datapath)
      persistence_error_message(NULL)
      extract_root <- file.path(session_root, tempest:::tempest_uuid("upload"))
      on.exit(unlink(extract_root, recursive = TRUE, force = TRUE), add = TRUE)
      tryCatch(
        {
          bundle_path <- session_archive_extract(upload$datapath, extract_root)
          restored <- restore_session_bundle(bundle_path)
          shiny::showNotification(
            paste0("Loaded session: ", restored$topic),
            type = "message",
            duration = 5
          )
        },
        error = function(e) {
          persistence_error_message(
            "Could not load the session bundle."
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
      session_experts,
      session_id_value,
      program_set_value,
      knowledge_value,
      stage_records = list(),
      on_error = NULL
    ) {
      ses <- tryCatch(
        {
          value <- tempest:::tempest_session_with_knowledge(
            topic,
            config = config_value,
            n_experts = n_experts,
            experts = session_experts,
            session_id = session_id_value,
            progress = record_progress,
            program_set = program_set_value,
            knowledge = knowledge_value
          )
          tempest:::tempest_session_set_stage_records(value, stage_records)
          value
        },
        error = function(e) {
          costorm_log("session setup failed")
          if (is.function(on_error)) {
            on_error(e)
          }
          shiny::showNotification(
            "Failed to create session.",
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
      store$set_costorm_session(ses)
      chat$bind(
        tempest:::tempest_session_chat(ses, "moderator"),
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
        client_history = "clear",
        on_turn = session_turn_callback(ses, active_session_id)
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
      store$set_costorm_session(NULL)
      chat$reset()
      if (isTRUE(allow_user_experts)) {
        later::later(
          function() {
            shiny::withReactiveDomain(session, {
              if (!isTRUE(session_ended)) {
                update_expert_setup_button()
              }
            })
          },
          delay = 0.2
        )
      }
      invisible(NULL)
    }

    run_report_generation <- function() {
      ses <- store$costorm_session()
      report_session_id <- active_session_id
      report_error_message(NULL)
      generate_report_for_chat_async(
        ses = ses,
        store = store,
        append_chat = append_chat,
        report_navigation_event = report_navigation_event,
        on_error = report_error_message,
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
      append_chat(chat_command_message(
        command,
        store$costorm_session(),
        config = config()
      ))
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
        ses = store$costorm_session(),
        progress_state = costorm_progress_state(progress_events()),
        chat_status = chat$status(),
        ns = session$ns
      )
    })

    shiny::observeEvent(
      input$suggest,
      {
        suggestions_enabled(isTRUE(input$suggest))
      },
      ignoreNULL = TRUE
    )

    shiny::observeEvent(input$footer_new, {
      run_chat_command("new")
    })

    toggle_workspace_settings <- function() {
      bslib::toggle_sidebar("settings", session = session)
      invisible(NULL)
    }
    shiny::observeEvent(input$setup_settings_toggle, {
      toggle_workspace_settings()
    })
    shiny::observeEvent(input$footer_settings_toggle, {
      toggle_workspace_settings()
    })

    shiny::observeEvent(input$start, {
      work_queue$cancel()
      warmup_is_current <- next_warmup_guard()
      progress_events(list())
      topic <- stringi::stri_trim_both(input$topic %||% "")
      if (!nzchar(topic)) {
        start_validation_message(
          "Enter a research topic before starting a Co-STORM session."
        )
        if (warmup_is_current()) {
          bslib::update_task_button("start", state = "ready", session = session)
        }
        return()
      }
      start_validation_message(NULL)
      config_value <- shiny::isolate(config())
      session_experts <- shiny::isolate(reactive_or_value(experts))
      session_experts <- costorm_session_experts(
        host_experts = session_experts,
        custom_experts = shiny::isolate(user_experts()),
        allow_user_experts = allow_user_experts,
        mode = shiny::isolate(expert_setup_mode())
      )
      session_id_value <- shiny::isolate(reactive_or_value(session_id))
      session_id_value <- session_id_value %||%
        tempest:::tempest_uuid("session")
      program_binding <- resolve_program_binding()
      program_set_value <- program_binding$program_set
      knowledge_value <- program_binding$knowledge
      n_experts <- if (isTRUE(allow_user_experts)) {
        shiny::isolate(generated_expert_count())
      } else {
        as.integer(input$n_experts %||% 3)
      }
      suggest_enabled <- isTRUE(input$suggest %||% suggestions_enabled())
      suggestions_enabled(suggest_enabled)
      warmup_enabled <- isTRUE(input$warmup)

      record_progress(costorm_starting_event(session_id_value))
      costorm_log("start requested: %s", topic)
      experts_ready <- if (is.null(session_experts)) {
        personas_program <- tempest:::tempest_costorm_program_execution(
          program_set_value,
          "personas",
          session_id_value
        )
        tempest:::tempest_generate_experts_async(
          topic,
          n = n_experts,
          config = config_value,
          program = personas_program
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
                persona_record <- NULL
                if (
                  inherits(generated_experts, "tempest_persona_stage_result")
                ) {
                  persona_record <- generated_experts$record
                  session_experts <- generated_experts$experts
                } else {
                  session_experts <- generated_experts
                }
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
                                    costorm_log("suggestions failed")
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
                      session_experts = session_experts,
                      session_id_value = session_id_value,
                      program_set_value = program_set_value,
                      knowledge_value = knowledge_value,
                      stage_records = if (is.null(persona_record)) {
                        list()
                      } else {
                        list(persona_record)
                      },
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
                        tempest:::tempest_session_warmup_async(
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
                          store$touch_costorm_session()
                          costorm_log(
                            "warmup finished: %s (%s)",
                            ses$session_id,
                            result@status
                          )
                          result
                        })
                      },
                      onRejected = function(e) {
                        costorm_log("warmup failed")
                        if (warmup_is_current()) {
                          append_chat_if_active(
                            "Warmup failed.",
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
                    costorm_log("start flow failed")
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
                  costorm_log("expert generation failed")
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
      ses <- store$costorm_session()
      if (is.null(ses) || length(ses$experts) == 0) {
        return(shiny::p(
          class = "text-muted small",
          "Start a session to see experts."
        ))
      }
      shiny::tagList(lapply(ses$experts, expert_card))
    })

    list(
      report_navigation_event = shiny::reactive(
        report_navigation_event()
      )
    )
  })
}
