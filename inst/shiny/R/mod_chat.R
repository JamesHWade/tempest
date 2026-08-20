# Chat tab: the interactive Co-STORM session.
#
# Package lifecycle functions own warmup and post-turn processing for the live
# `TempestSession`. This module owns only Shiny reactivity and presentation,
# while the shinychat adapter owns the provider widget lifecycle.

mod_chat_ui <- function(id, config_ui, allow_user_experts = FALSE) {
  ns <- shiny::NS(id)
  bslib::nav_panel(
    title = shiny::tagList(shiny::icon("comments"), "Chat"),
    value = "Chat",
    bslib::layout_sidebar(
      class = "tempest-chat-layout",
      sidebar = chat_settings_sidebar_ui(ns, config_ui),
      bslib::card(
        class = "tempest-chat-card",
        full_screen = TRUE,
        bslib::card_body(
          class = "p-0",
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
  research_options <- bslib::popover(
    bslib::toolbar_input_button(
      ns("research_options"),
      "Research options",
      icon = shiny::icon("sliders"),
      show_label = TRUE,
      tooltip = FALSE,
      border = TRUE
    ),
    id = ns("research_options_popover"),
    title = "Research options",
    placement = "bottom",
    shiny::div(
      class = "tempest-chat-research-options",
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
      shiny::p(
        class = "small text-body-secondary mb-0",
        "Orientation gives experts a short evidence-gathering pass before chat."
      )
    )
  )

  shiny::div(
    class = "tempest-chat-welcome",
    shiny::div(
      class = "tempest-chat-welcome-copy",
      shiny::h2(class = "h4 mb-2", "Welcome to tempest"),
      shiny::p(
        paste(
          "Investigate a topic with a panel of AI experts.",
          "Ask follow-up questions as they gather cited evidence,",
          "then generate a report."
        )
      ),
      shiny::p(
        class = "mb-0",
        "For a one-shot report, use ",
        shiny::strong("STORM"),
        "."
      )
    ),
    shiny::div(
      class = "tempest-chat-welcome-form",
      shiny::div(
        class = "tempest-chat-welcome-topic",
        shiny::textInput(
          ns("topic"),
          "Research topic",
          placeholder = "What should the expert panel investigate?"
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
            border = TRUE
          )
        ),
        bslib::input_task_button(
          ns("start"),
          "Start session",
          icon = shiny::icon("play"),
          label_busy = "Starting...",
          auto_reset = FALSE,
          class = "tempest-chat-start btn-sm"
        )
      ),
      shiny::uiOutput(ns("start_validation"))
    )
  )
}

expert_count_choices <- function() {
  stats::setNames(
    1:5,
    paste(1:5, ifelse(1:5 == 1L, "expert", "experts"))
  )
}

expert_setup_control_ui <- function(ns, allow_user_experts = FALSE) {
  if (!isTRUE(allow_user_experts)) {
    return(shiny::selectInput(
      ns("n_experts"),
      "Experts",
      choices = expert_count_choices(),
      selected = 3,
      selectize = FALSE
    ))
  }

  shiny::div(
    class = "form-group shiny-input-container",
    shiny::tags$label(
      class = "control-label",
      `for` = ns("expert_setup"),
      "Expert panel"
    ),
    shiny::actionButton(
      ns("expert_setup"),
      "3 generated",
      icon = shiny::icon("users"),
      class = paste(
        "btn-outline-secondary btn-sm",
        "tempest-chat-expert-setup"
      ),
      title = "Configure the expert panel"
    )
  )
}

custom_expert_input_id <- function(field, index) {
  paste0("custom_expert_", field, "_", index)
}

expert_setup_count <- function(value) {
  count <- suppressWarnings(as.integer(value))
  if (
    length(count) != 1L ||
      is.na(count) ||
      count < 1L ||
      count > 5L
  ) {
    custom_expert_input_abort("Choose between one and five experts.")
  }
  count
}

custom_expert_drafts <- function(experts) {
  lapply(experts %||% list(), function(expert) {
    list(
      name = expert@name,
      title = expert@title,
      perspective = expert@description
    )
  })
}

custom_expert_fields_ui <- function(ns, count, drafts = list()) {
  count <- expert_setup_count(count %||% 3L)
  shiny::div(
    class = "tempest-custom-expert-grid",
    lapply(seq_len(count), function(index) {
      draft <- if (length(drafts) >= index) drafts[[index]] else list()
      shiny::div(
        class = "tempest-custom-expert-card",
        shiny::div(
          class = "tempest-custom-expert-heading",
          persona_icon(draft$name, paste0("expert.user.", index)),
          shiny::strong(paste("Expert", index))
        ),
        shiny::div(
          class = "tempest-custom-expert-identity",
          shiny::textInput(
            ns(custom_expert_input_id("name", index)),
            "Name",
            value = draft$name %||% "",
            placeholder = "e.g., Maya Chen"
          ),
          shiny::textInput(
            ns(custom_expert_input_id("title", index)),
            "Role or expertise",
            value = draft$title %||% "",
            placeholder = "e.g., Battery policy analyst"
          )
        ),
        shiny::textAreaInput(
          ns(custom_expert_input_id("perspective", index)),
          "Perspective and priorities",
          value = draft$perspective %||% "",
          placeholder = paste(
            "Describe what this expert should focus on, question,",
            "or contribute."
          ),
          rows = 3,
          width = "100%"
        )
      )
    })
  )
}

custom_expert_setup_modal <- function(
  ns,
  mode = "generated",
  generated_count = 3L,
  custom_experts = list()
) {
  custom_count <- if (length(custom_experts) > 0L) {
    length(custom_experts)
  } else {
    expert_setup_count(generated_count)
  }
  shiny::modalDialog(
    title = "Configure expert panel",
    size = "l",
    easyClose = TRUE,
    shiny::p(
      class = "text-body-secondary",
      paste(
        "Choose a generated panel or define the perspectives you want",
        "represented in this session."
      )
    ),
    shiny::radioButtons(
      ns("expert_setup_mode"),
      "Panel source",
      choices = c(
        "Generate a balanced panel" = "generated",
        "Choose my own experts" = "custom"
      ),
      selected = mode,
      inline = TRUE
    ),
    shiny::conditionalPanel(
      condition = "input.expert_setup_mode === 'generated'",
      ns = ns,
      shiny::selectInput(
        ns("generated_expert_count"),
        "Number of experts",
        choices = expert_count_choices(),
        selected = generated_count,
        selectize = FALSE,
        width = "14rem"
      )
    ),
    shiny::conditionalPanel(
      condition = "input.expert_setup_mode === 'custom'",
      ns = ns,
      shiny::div(
        class = "tempest-custom-expert-builder",
        shiny::selectInput(
          ns("custom_expert_count"),
          "Number of experts",
          choices = expert_count_choices(),
          selected = custom_count,
          selectize = FALSE,
          width = "14rem"
        ),
        shiny::uiOutput(ns("custom_expert_fields"))
      )
    ),
    footer = shiny::tagList(
      shiny::modalButton("Cancel"),
      shiny::actionButton(
        ns("apply_expert_setup"),
        "Use this panel",
        class = "btn-primary"
      )
    )
  )
}

custom_expert_specs_from_input <- function(input, count) {
  count <- expert_setup_count(count)
  lapply(seq_len(count), function(index) {
    list(
      name = input[[custom_expert_input_id("name", index)]] %||% "",
      title = input[[custom_expert_input_id("title", index)]] %||% "",
      perspective = input[[
        custom_expert_input_id("perspective", index)
      ]] %||%
        ""
    )
  })
}

custom_expert_input_abort <- function(message) {
  rlang::abort(
    message,
    class = c("tempest_custom_expert_input_error", "tempest_error")
  )
}

custom_expert_profiles <- function(specs) {
  if (!is.list(specs) || length(specs) < 1L || length(specs) > 5L) {
    custom_expert_input_abort("Choose between one and five custom experts.")
  }

  lapply(seq_along(specs), function(index) {
    spec <- specs[[index]]
    if (!is.list(spec)) {
      custom_expert_input_abort(paste("Expert", index, "is malformed."))
    }
    values <- vapply(
      c("name", "title", "perspective"),
      function(field) {
        value <- spec[[field]] %||% ""
        if (!is.character(value) || length(value) != 1L || is.na(value)) {
          custom_expert_input_abort(paste(
            "Expert",
            index,
            "has an invalid",
            field,
            "value."
          ))
        }
        stringi::stri_trim_both(value)
      },
      character(1)
    )
    missing <- names(values)[!nzchar(values)]
    if (length(missing) > 0L) {
      labels <- c(
        name = "name",
        title = "role or expertise",
        perspective = "perspective and priorities"
      )
      custom_expert_input_abort(paste0(
        "Expert ",
        index,
        " needs a ",
        labels[[missing[[1]]]],
        "."
      ))
    }

    tempest::tempest_expert(
      expert_id = sprintf("expert.user.%02d", index),
      name = values[["name"]],
      title = values[["title"]],
      description = values[["perspective"]],
      instructions = paste(
        "Represent this user-defined expert perspective:",
        values[["perspective"]],
        paste(
          "Gather and cite relevant evidence, distinguish evidence from",
          "interpretation, and state material uncertainty."
        )
      ),
      selection_metadata = list(source = "user")
    )
  })
}

expert_setup_button_label <- function(mode, count) {
  paste(count, if (identical(mode, "custom")) "custom" else "generated")
}

costorm_session_experts <- function(
  host_experts,
  custom_experts,
  allow_user_experts,
  mode
) {
  if (
    isTRUE(allow_user_experts) &&
      identical(mode, "custom") &&
      length(custom_experts) > 0L
  ) {
    return(custom_experts)
  }
  host_experts
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
          placeholder = "No bundle selected"
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
  knowledge_view = NULL,
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
        tempest::tempest_program_set()
      tempest:::tempest_program_set_manifest_programs(value)
      value
    }

    resolve_program_binding <- function() {
      program_set_value <- resolve_program_set()
      knowledge_view_value <- shiny::isolate(
        reactive_or_value(knowledge_view)
      )
      knowledge <- tempest:::tempest_product_knowledge_view(
        program_set_value,
        knowledge_view_value
      )
      list(
        program_set = program_set_value,
        knowledge_view = knowledge$view
      )
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
      citation_workspace(ses$workspace %||% NULL)
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
            tempest::tempest_session_process_turn_async(
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
      progress_events(tempest::tempest_execution_events(ses))
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
        knowledge_view = binding$knowledge_view
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
      knowledge_view_value,
      stage_records = list(),
      on_error = NULL
    ) {
      ses <- tryCatch(
        {
          value <- tempest:::tempest_session_new(
            topic,
            config = config_value,
            n_experts = n_experts,
            experts = session_experts,
            session_id = session_id_value,
            progress = record_progress,
            program_set = program_set_value,
            knowledge_view = knowledge_view_value
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
      bslib::toggle_popover(
        "research_options_popover",
        show = FALSE,
        session = session
      )
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
      bslib::toggle_popover(
        "research_options_popover",
        show = FALSE,
        session = session
      )
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
      knowledge_view_value <- program_binding$knowledge_view
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
        personas_program$knowledge_view <- knowledge_view_value
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
                      knowledge_view_value = knowledge_view_value,
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

# --- Chat module helpers -----------------------------------------------------

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
  shiny::div(
    class = "tempest-chat-footer w-100",
    shiny::uiOutput(
      ns("runtime_footer"),
      class = "tempest-chat-footer-output w-100"
    )
  )
}

chat_report_options_ui <- function(ns) {
  bslib::popover(
    bslib::toolbar_input_button(
      ns("report_options"),
      "Report options",
      icon = shiny::icon("sliders"),
      show_label = TRUE,
      tooltip = FALSE,
      border = TRUE
    ),
    id = ns("report_options_popover"),
    title = "Report options",
    placement = "top",
    shiny::selectInput(
      ns("report_style"),
      "Report style",
      choices = c(
        "Technical" = "technical",
        "Executive" = "executive"
      ),
      selected = "technical",
      selectize = FALSE
    ),
    shiny::p(
      class = "small text-body-secondary mb-0",
      "Reports use the evidence collected in this session."
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
  sources <- tryCatch(
    ses$workspace$list_retrieved_sources(),
    error = function(e) list()
  )
  claims <- tryCatch(
    ses$workspace$list_proposed_claims(),
    error = function(e) list()
  )
  report <- tryCatch(
    {
      report_md <- tempest::tempest_session_report_md(ses)
      is.character(report_md) &&
        length(report_md) == 1L &&
        !is.na(report_md) &&
        nzchar(report_md)
    },
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
  chat_status = "idle",
  ns = identity
) {
  if (is.null(ses)) {
    return(shiny::span(
      class = "tempest-chat-footer-idle",
      `aria-hidden` = "true"
    ))
  }
  counts <- chat_runtime_counts(ses)
  session_label <- if (identical(chat_status, "streaming")) {
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

  report_status <- if (isTRUE(counts$report)) {
    chat_footer_tooltip(
      shiny::span(
        class = "badge rounded-pill text-bg-light border text-body",
        title = "Report status: Report ready",
        `aria-label` = "Report status: Report ready",
        "Report ready"
      ),
      "Report status: Report ready"
    )
  }

  shiny::div(
    class = paste(
      "tempest-chat-footer-active",
      "d-flex flex-wrap align-items-center justify-content-between gap-2"
    ),
    shiny::div(
      class = paste(
        "tempest-chat-footer-status",
        "d-flex flex-wrap align-items-center gap-1 text-body-secondary"
      ),
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
      report_status
    ),
    shiny::div(
      class = "tempest-chat-footer-actions d-flex align-items-center gap-2",
      bslib::toolbar(
        align = "left",
        width = "auto",
        bslib::toolbar_input_button(
          ns("footer_settings_toggle"),
          "Workspace settings",
          icon = shiny::icon("gear"),
          `aria-controls` = ns("settings"),
          `aria-expanded` = "false",
          `data-tempest-settings-trigger` = "true",
          border = TRUE
        )
      ),
      shiny::actionButton(
        ns("footer_new"),
        "New session",
        icon = shiny::icon("plus"),
        class = "btn-outline-secondary btn-sm text-nowrap"
      ),
      bslib::toolbar(
        align = "left",
        width = "auto",
        chat_report_options_ui(ns)
      ),
      bslib::input_task_button(
        ns("generate_report"),
        "Generate report",
        icon = shiny::icon("file-lines"),
        label_busy = "Generating...",
        type = "secondary",
        class = "btn-sm text-nowrap"
      )
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
    tryCatch(
      ses$workspace$list_retrieved_sources(),
      error = function(e) list()
    )
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
    tryCatch(
      ses$workspace$list_proposed_claims(),
      error = function(e) list()
    )
  }
  execution_review <- if (is.null(ses)) {
    character()
  } else {
    tempest:::tempest_costorm_execution_review_lines(ses)
  }
  if (length(claims) == 0L) {
    if (length(execution_review) > 0L) {
      return(paste(
        c(
          "**Evidence review**",
          "",
          "Verified evidence: 0 of 0 claims.",
          "",
          "No claims were collected.",
          "",
          "**Execution review**",
          "",
          execution_review
        ),
        collapse = "\n"
      ))
    }
    return("No facts collected yet. Ask a research question first.")
  }
  min_support_score <- tryCatch(
    ses$config@min_support_score,
    error = function(e) 0.7
  )
  verified <- vapply(
    claims,
    function(claim) {
      identical(claim@verification_status, "supported") &&
        !is.na(claim@support_score) &&
        claim@support_score >= min_support_score
    },
    logical(1)
  )
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
  execution_section <- if (length(execution_review) > 0L) {
    c("", "**Execution review**", "", execution_review)
  } else {
    character()
  }
  paste(
    c(
      "**Evidence review**",
      "",
      paste0(
        "Verified evidence: ",
        sum(verified),
        " of ",
        length(claims),
        " claims."
      ),
      "",
      lines,
      execution_section
    ),
    collapse = "\n"
  )
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
  paste(
    "**Tools and commands**",
    "",
    paste0("- Search provider: `", provider, "`."),
    "- Expert research executes through Deputy-owned session adapters.",
    "- Commands: `/new`, `/experts`, `/sources`, `/facts`, `/report`, `/system`, `/tools`.",
    sep = "\n"
  )
}

costorm_async_queue <- function() {
  generation <- 0L
  tail <- promises::promise_resolve(NULL)
  pending <- 0L
  list(
    enqueue = function(task, on_cancel = function() invisible(NULL)) {
      stopifnot(is.function(task))
      stopifnot(is.function(on_cancel))
      ticket <- generation
      pending <<- pending + 1L
      is_current <- function() identical(ticket, generation)
      result <- promises::then(tail, function(...) {
        if (!is_current()) {
          on_cancel()
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
  report_navigation_event,
  queue,
  on_error = function(message) invisible(message),
  style = "technical",
  is_current = function() TRUE
) {
  if (is.null(ses)) {
    append_chat("No session active. Start a session first.")
    return(promises::promise_resolve(FALSE))
  }
  n_evidence <- length(ses$workspace$list_proposed_claims()) +
    length(ses$workspace$list_retrieved_sources())
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
  published <- promises::then(
    task,
    onFulfilled = function(markdown) {
      if (
        !warmup_is_current(is_current) || is.null(markdown) || !nzchar(markdown)
      ) {
        return(FALSE)
      }
      authority_report <- tempest::tempest_session_report_md(ses)
      if (!identical(authority_report, markdown)) {
        stop("Generated report content does not match product authority.")
      }
      report_md <- store$publish_costorm_report(ses)
      if (!identical(report_md, markdown)) {
        stop("Published report content does not match the generated report.")
      }
      store$touch_costorm_session()
      append_chat(sprintf(
        "Report generated (%d chars). See the **Report** tab.",
        nchar(markdown)
      ))
      report_navigation_event(report_navigation_event() + 1L)
      TRUE
    }
  )
  promises::catch(published, function(error) {
    if (warmup_is_current(is_current)) {
      message <- "Report generation or publication failed."
      on_error(message)
      append_chat(message)
    }
    FALSE
  })
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
  if (is.null(store$peek_costorm_session())) {
    stop("No Co-STORM session is active.", call. = FALSE)
  }
  bundle_dir <- tempfile("tempest-session-download-")
  on.exit(unlink(bundle_dir, recursive = TRUE, force = TRUE), add = TRUE)
  store$save_costorm_session(bundle_dir, overwrite = FALSE)
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
  invisible(file)
}

session_archive_extract <- function(archive, root) {
  listing <- utils::unzip(archive, list = TRUE)
  entries <- gsub("\\\\", "/", listing$Name)
  if (!session_archive_listing_is_safe(entries, listing$Length)) {
    stop("Session archive contains unsafe files.", call. = FALSE)
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
  bundle <- tempest:::tempest_costorm_archive_read(root)
  session_secure_permissions(root)
  complete <- TRUE
  bundle
}

session_archive_listing_is_safe <- function(entries, sizes) {
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
  safe
}

session_persistence_status_ui <- function(state) {
  if (is.null(state) || identical(state$status, "idle")) {
    return(NULL)
  }
  status_class <- switch(
    state$status,
    saved = "text-success",
    restored = "text-info",
    "text-danger"
  )
  status_icon <- switch(
    state$status,
    saved = "circle-check",
    restored = "folder-open",
    "triangle-exclamation"
  )
  role <- if (identical(state$status, "error")) "alert" else "status"
  shiny::div(
    class = paste("small mt-2", status_class),
    role = role,
    `aria-live` = if (identical(role, "status")) "polite" else NULL,
    `aria-atomic` = "true",
    shiny::span(shiny::icon(status_icon), class = "me-1"),
    shiny::span(state$message %||% ""),
  )
}

session_persistence_error_ui <- function(message) {
  if (is.null(message) || !nzchar(message)) {
    return(NULL)
  }
  shiny::div(
    class = "small mt-2 text-danger",
    role = "alert",
    `aria-atomic` = "true",
    shiny::span(shiny::icon("triangle-exclamation"), class = "me-1"),
    shiny::span(message)
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

costorm_session_ready_event <- function(session_id, ses) {
  tempest::tempest_progress_event(
    run_id = session_id,
    workflow = "costorm",
    event_type = "stage",
    status = "succeeded",
    stage = "session",
    step = "created",
    message = "Co-STORM session ready.",
    payload = list(expert_count = length(ses$experts))
  )
}

costorm_session_failed_event <- function(session_id, error = NULL) {
  message <- "Session setup failed."
  payload <- list(error_message = message)
  if (!is.null(error)) {
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
      costorm_log("progress reducer failed")
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

progress_error_payload <- function(error) {
  tempest:::tempest_progress_error_payload(error)
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
