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
        title = "Session Settings",
        width = 300,
        shiny::textInput(
          ns("topic"),
          "Research Topic",
          placeholder = "Enter a research topic..."
        ),
        shiny::sliderInput(ns("n_experts"), "Number of Experts", 1, 5, 3),
        shiny::checkboxInput(
          ns("warmup"),
          "Run warmup (experts research initial questions)",
          FALSE
        ),
        shiny::checkboxInput(
          ns("suggest"),
          "Suggest follow-up questions",
          TRUE
        ),
        bslib::input_task_button(
          ns("start"),
          "Start Session",
          icon = shiny::icon("play"),
          label_busy = "Starting...",
          auto_reset = FALSE,
          class = "w-100"
        ),
        shiny::hr(),
        shiny::h6("Session"),
        shiny::textInput(
          ns("session_bundle"),
          "Bundle directory",
          value = session_bundle_default_path()
        ),
        shiny::div(
          class = "d-flex gap-2",
          shiny::actionButton(
            ns("save_session"),
            "Save",
            icon = shiny::icon("floppy-disk"),
            class = "btn-outline-primary btn-sm flex-fill"
          ),
          shiny::actionButton(
            ns("load_session"),
            "Load",
            icon = shiny::icon("folder-open"),
            class = "btn-outline-secondary btn-sm flex-fill"
          )
        ),
        shiny::checkboxInput(
          ns("autosave_session"),
          "Autosave after changes",
          FALSE
        ),
        shiny::uiOutput(ns("session_persistence")),
        shiny::hr(),
        shiny::selectInput(
          ns("report_style"),
          "Report Style",
          choices = c("technical", "executive"),
          selected = "technical"
        ),
        bslib::input_task_button(
          ns("generate_report"),
          "Generate Report",
          icon = shiny::icon("file-export"),
          label_busy = "Generating...",
          type = "secondary",
          class = "w-100"
        ),
        shiny::hr(),
        shiny::h6("Expert Panel"),
        shiny::uiOutput(ns("expert_panel")),
        shiny::hr(),
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
            enable_cancel = TRUE,
            icon_assistant = tempest_chat_icon()
          )
        )
      )
    )
  )
}

mod_chat_server <- function(id, config, store) {
  shiny::moduleServer(id, function(input, output, session) {
    report_ready <- shiny::reactiveVal(0L)
    progress_events <- shiny::reactiveVal(list())
    warmup_run_id <- 0L
    active_session_id <- 0L
    session_ended <- FALSE

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
    chat <- shinychat::chat_server(
      "chat",
      initial_chat,
      greeting = shinychat::chat_greeting(welcome_message())
    )
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
    append_chat_if_active <- function(text, session_id = active_session_id) {
      if (!isTRUE(session_ended) && identical(session_id, active_session_id)) {
        append_chat(text)
      }
      invisible(NULL)
    }
    record_progress <- function(event) {
      record_costorm_progress_event(progress_events, event, session)
    }

    restore_progress_history <- function(ses) {
      progress_events(ses$artifacts[["progress_events"]] %||% list())
      invisible(NULL)
    }

    replace_chat_with_session <- function(ses) {
      chat$set_client(ses$chats$moderator, sync = FALSE)
      chat$clear(greeting = FALSE)
      append_restored_session_chat(
        chat = chat,
        session = ses,
        source_store = citation_source_store(ses$store %||% NULL)
      )
      invisible(NULL)
    }

    restore_session_bundle <- function(path) {
      warmup_run_id <<- warmup_run_id + 1L
      active_session_id <<- active_session_id + 1L
      ses <- store$restore(path, config = config(), progress = record_progress)
      shiny::updateTextInput(session, "topic", value = ses$topic %||% "")
      shiny::updateSliderInput(
        session,
        "n_experts",
        value = max(1L, min(5L, length(ses$personas %||% list())))
      )
      restore_progress_history(ses)
      replace_chat_with_session(ses)
      ses
    }

    session_autosave_server(
      store = store,
      path = shiny::reactive(input$session_bundle),
      enabled = shiny::reactive(input$autosave_session),
      on_error = function(error) {
        store$set_persistence(
          "error",
          path = input$session_bundle %||% "",
          message = paste0("Autosave failed: ", conditionMessage(error))
        )
        shiny::showNotification(
          paste0("Autosave failed: ", conditionMessage(error)),
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
    shiny::observeEvent(input$save_session, {
      path <- input$session_bundle %||% ""
      tryCatch(
        {
          saved <- store$save(path, overwrite = TRUE)
          shiny::showNotification(
            paste0("Session saved to ", saved),
            type = "message",
            duration = 5
          )
        },
        error = function(e) {
          store$set_persistence(
            "error",
            path = path,
            message = paste0("Save failed: ", conditionMessage(e))
          )
          shiny::showNotification(
            paste0("Save failed: ", conditionMessage(e)),
            type = "error",
            duration = 10
          )
        }
      )
    })

    shiny::observeEvent(input$load_session, {
      path <- input$session_bundle %||% ""
      tryCatch(
        {
          restored <- restore_session_bundle(path)
          shiny::showNotification(
            paste0("Loaded session: ", restored$topic),
            type = "message",
            duration = 5
          )
        },
        error = function(e) {
          store$set_persistence(
            "error",
            path = path,
            message = paste0("Load failed: ", conditionMessage(e))
          )
          shiny::showNotification(
            paste0("Load failed: ", conditionMessage(e)),
            type = "error",
            duration = 10
          )
        }
      )
    })

    create_session <- function(topic, n_experts) {
      ses <- tryCatch(
        tempest::tempest_session(
          topic,
          config = config(),
          n_experts = n_experts,
          progress = record_progress
        ),
        error = function(e) {
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
      chat$set_client(ses$chats$moderator, sync = FALSE)
      # Clear the landing greeting; the session intro below replaces it.
      chat$clear(greeting = FALSE)
      append_chat(paste0(
        "Co-STORM session started for: **",
        topic,
        "**",
        expert_intro(ses),
        "\n\nAsk questions, request sources, or ask for a report.\n"
      ))
      ses
    }

    shiny::observeEvent(input$start, {
      warmup_is_current <- next_warmup_guard()
      progress_events(list())
      topic <- stringi::stri_trim_both(input$topic %||% "")
      if (!nzchar(topic)) {
        if (warmup_is_current()) {
          bslib::update_task_button("start", state = "ready")
        }
        return()
      }
      ses <- create_session(topic, input$n_experts %||% 3)
      if (is.null(ses)) {
        if (warmup_is_current()) {
          bslib::update_task_button("start", state = "ready")
        }
        return()
      }
      suggest_enabled <- isTRUE(input$suggest)
      start_session_id <- active_session_id
      append_start_suggestions <- function() {
        if (warmup_is_current()) {
          append_suggestions(
            ses,
            suggest_enabled,
            function(text) {
              append_chat_if_active(text, session_id = start_session_id)
            },
            n = 4
          )
        }
      }
      delay_suggestions <- should_delay_start_suggestions(
        input$warmup,
        ses$personas
      )
      if (!delay_suggestions) {
        append_start_suggestions()
      }
      if (!isTRUE(input$warmup) || length(ses$personas) == 0) {
        if (warmup_is_current()) {
          bslib::update_task_button("start", state = "ready")
        }
        return()
      }
      warmup_done <- promises::then(
        run_warmup(ses, store, append_chat, is_current = warmup_is_current),
        onFulfilled = function(...) {
          append_start_suggestions()
        },
        onRejected = function(e) {
          if (warmup_is_current()) {
            append_chat_if_active(
              paste0("Warmup failed: ", conditionMessage(e)),
              session_id = start_session_id
            )
            append_start_suggestions()
          }
          NULL
        }
      )
      promises::finally(
        warmup_done,
        function() {
          if (warmup_is_current()) {
            bslib::update_task_button("start", state = "ready")
          }
        }
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
      source_ids <- harvest_session_sources(ses, turn = turn)
      ans <- tryCatch(
        ellmer::contents_markdown(turn),
        error = function(e) {
          if (is.character(turn)) paste(turn, collapse = "\n") else ""
        }
      )
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
      session_id <- active_session_id
      suggest_enabled <- isTRUE(input$suggest)
      later::later(
        function() {
          if (
            isTRUE(session_ended) || !identical(session_id, active_session_id)
          ) {
            return()
          }
          tryCatch(
            extract_chat_turn_facts(
              ses,
              ans,
              turn = turn,
              source_ids = source_ids,
              session_id = ses$session_id,
              persona_id = "moderator",
              correlation_id = turn_id
            ),
            error = function(e) {
              warning("Fact extraction failed: ", conditionMessage(e))
            }
          )
          tryCatch(
            ses$update_mindmap(
              last_exchange = paste0("User: ", msg, "\n\nModerator: ", ans)
            ),
            error = function(e) {
              warning("Mind map update failed: ", conditionMessage(e))
            }
          )
          if (
            isTRUE(session_ended) || !identical(session_id, active_session_id)
          ) {
            return()
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
          if (nzchar(msg)) {
            append_suggestions(
              ses,
              suggest_enabled,
              function(text) {
                append_chat_if_active(text, session_id = session_id)
              },
              n = 4
            )
          }
        },
        delay = 0
      )
    })

    # --- Report generation ---------------------------------------------------
    shiny::observeEvent(input$generate_report, {
      ses <- store$get()
      if (is.null(ses)) {
        append_chat("No session active. Start a session first.")
        return()
      }
      n_evidence <- length(ses$store$list_claims()) +
        length(ses$store$list_sources())
      if (n_evidence == 0) {
        append_chat(
          "No facts or sources collected yet. Ask some questions first to gather research."
        )
        return()
      }
      append_chat("Generating report...")
      md <- tryCatch(
        ses$report(style = input$report_style %||% "technical"),
        error = function(e) {
          append_chat(paste0("Report generation failed: ", conditionMessage(e)))
          NULL
        }
      )
      if (is.null(md) || !nzchar(md)) {
        append_chat(
          "Report generation returned empty. Try asking more questions first."
        )
        return()
      }
      ses$artifacts[["report_md"]] <- md
      store$set_report(md, ses$topic, source_store = ses$store)
      store$touch()
      append_chat(sprintf(
        "Report generated (%d chars). See the **Report** tab.",
        nchar(md)
      ))
      report_ready(report_ready() + 1L)
    })

    # --- Expert panel --------------------------------------------------------
    output$expert_panel <- shiny::renderUI({
      ses <- store$get()
      if (is.null(ses) || length(ses$personas) == 0) {
        return(shiny::p(
          class = "text-muted small",
          "Start a session to see experts."
        ))
      }
      shiny::tagList(lapply(ses$personas, expert_card))
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

session_bundle_default_path <- function() {
  file.path("~", "tempest-session")
}

session_autosave_server <- function(
  store,
  path,
  enabled,
  delay_ms = 1000,
  on_saved = NULL,
  on_error = NULL
) {
  trigger <- shiny::debounce(shiny::reactive(store$version()), delay_ms)
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
    if (!is.null(state$path) && nzchar(state$path)) {
      shiny::span(class = "d-block text-truncate", state$path)
    }
  )
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

  if (nzchar(session$artifacts[["report_md"]] %||% "")) {
    chat$append(
      "Restored report artifact. See the **Report** tab.",
      role = "assistant"
    )
  }
  invisible(chat)
}

expert_intro <- function(ses) {
  if (length(ses$personas) == 0) {
    return("")
  }
  lines <- vapply(
    ses$personas,
    function(p) {
      paste0(
        "- **",
        p$name %||% "Expert",
        "** (",
        p$title %||% "Specialist",
        ")"
      )
    },
    character(1)
  )
  paste0("\n\n**Expert Panel:**\n", paste(lines, collapse = "\n"))
}

expert_card <- function(p) {
  shiny::div(
    class = "mb-2 p-2 border small tempest-expert-card",
    shiny::div(
      class = "d-flex align-items-center gap-2",
      persona_icon(p$name, p$id),
      shiny::div(
        class = "min-w-0",
        shiny::strong(p$name %||% "Expert"),
        shiny::br(),
        shiny::span(class = "text-muted", p$title %||% "")
      )
    ),
    if (!is.null(p$perspective) && nzchar(p$perspective)) {
      shiny::div(class = "mt-1 fst-italic", p$perspective)
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
      function(item) {
        if (is.character(item)) {
          paste(item, collapse = "\n")
        } else {
          as.character(item)
        }
      },
      character(1)
    )
    return(paste(parts, collapse = "\n"))
  }
  as.character(x)
}

# Build a shinychat suggestion-card block from a vector of questions. shinychat
# renders a markdown list whose items are all `<span class="suggestion">` as a
# grid of cards; the `submit` class makes a click send the question immediately.
# Returns NULL when there are no usable questions.
suggestion_cards <- function(questions, lead = "**Research next:**") {
  questions <- trimws(questions)
  questions <- questions[!is.na(questions) & nzchar(questions)]
  if (length(questions) == 0) {
    return(NULL)
  }
  items <- paste0(
    "- <span class=\"suggestion submit\">",
    htmltools::htmlEscape(questions),
    "</span>"
  )
  paste0(lead, "\n\n", paste(items, collapse = "\n"))
}

# Generate suggestion cards and append them, gated on `enabled`. Quiet on
# failure: a stalled or erroring generator simply shows no cards. Pure of Shiny
# reactives (deps injected) so it can be unit-tested directly.
append_suggestions <- function(ses, enabled, append_fn, n = 4) {
  if (is.null(ses) || !isTRUE(enabled)) {
    return(invisible(NULL))
  }
  questions <- tryCatch(ses$suggest_questions(n), error = function(e) {
    character()
  })
  cards <- suggestion_cards(questions)
  if (!is.null(cards)) {
    append_fn(cards)
  }
  invisible(NULL)
}

should_delay_start_suggestions <- function(warmup_enabled, personas) {
  isTRUE(warmup_enabled) && length(personas) > 0
}

costorm_progress_state <- function(events) {
  if (length(events) == 0L) {
    return(NULL)
  }
  tryCatch(
    tempest::tempest_progress_state(events),
    error = function(e) NULL
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

warmup_prompt <- function(topic, question) {
  paste0(
    "Topic: ",
    topic,
    "\n\n",
    "Question: ",
    question,
    "\n\n",
    "Instructions:\n",
    "- Use the available web/source tools to find and cite sources.\n",
    "- If web_search and fetch_url are available, search first and then fetch sources.\n",
    "- Only state factual claims supported by sources you inspected.\n",
    "- For each factual sentence, add source IDs like [Sxxxxxxxxxxxx] when available.\n",
    "- If evidence is weak or unclear, say so.\n\n",
    "Respond now:"
  )
}

record_warmup_turn <- function(
  ses,
  name,
  question,
  response,
  expert_chat = NULL,
  session_id = NA_character_,
  persona_id = NA_character_,
  correlation_id = NA_character_
) {
  turn <- tryCatch(expert_chat$last_turn(), error = function(e) NULL)
  source_ids <- harvest_session_sources(ses, turn = turn)
  tryCatch(
    ses$expert_session_manager$extract_facts(
      response,
      turn = turn,
      source_ids = source_ids,
      session_id = session_id,
      persona_id = persona_id,
      correlation_id = correlation_id
    ),
    error = function(e) NULL
  )
  ses$add_turn(name, "assistant", response)
  tryCatch(
    ses$update_mindmap(
      last_exchange = paste0(
        "Initial research by ",
        name,
        ":\nQ: ",
        question,
        "\n\nA: ",
        response
      )
    ),
    error = function(e) NULL
  )
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
  persona_id = NA_character_,
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
    persona_id = persona_id,
    correlation_id = correlation_id
  )
  invisible(source_ids)
}

warmup_summary <- function(ses) {
  facts <- ses$store$list_claims()
  n_facts <- length(facts)
  n_sources <- length(ses$store$list_sources())

  highlights <- ""
  if (n_facts > 0) {
    high <- Filter(function(f) identical(f@confidence, "high"), facts)
    show <- if (length(high) > 0) {
      utils::head(high, 5)
    } else {
      utils::head(facts, 5)
    }
    bullets <- paste(
      vapply(show, function(f) paste0("- ", f@claim_text), character(1)),
      collapse = "\n"
    )
    highlights <- paste0("\n\n**Key findings so far:**\n", bullets)
  }

  paste0(
    "**Warmup complete!** Collected ",
    n_facts,
    " facts from ",
    n_sources,
    " sources.",
    highlights,
    "\n\nYou can now ask questions."
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

warmup_question_label <- function(question, width = 120) {
  question <- stringi::stri_trim_both(question %||% "")
  if (!nzchar(question)) {
    return("(untitled question)")
  }
  if (nchar(question, type = "width") <= width) {
    return(question)
  }
  paste0(substr(question, 1, width - 3), "...")
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

warmup_selected_questions <- function(
  questions,
  max_questions_per_expert = Inf
) {
  questions <- stringi::stri_trim_both(questions %||% character())
  questions <- questions[!is.na(questions) & nzchar(questions)]

  if (
    is.null(max_questions_per_expert) ||
      !is.finite(max_questions_per_expert)
  ) {
    return(questions)
  }

  limit <- suppressWarnings(as.integer(max_questions_per_expert[[1]]))
  if (is.na(limit) || limit < 1L) {
    return(character())
  }
  utils::head(questions, limit)
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

# Research each expert's initial questions, running independent experts in
# bounded parallel batches. Questions for one expert remain ordered because each
# expert chat is stateful. Returns a promise that resolves when warmup finishes.
run_warmup <- function(
  ses,
  store,
  append_chat,
  is_current = function() TRUE,
  timeout_s = getOption("tempest.shiny.warmup_timeout_s", 120),
  max_questions_per_expert = getOption(
    "tempest.shiny.warmup_max_questions_per_expert",
    Inf
  ),
  max_parallel_experts = getOption(
    "tempest.shiny.warmup_max_parallel_experts",
    3L
  )
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

  safe_append(
    "**Running warmup phase...** Each expert is researching their initial questions."
  )
  warmup_event <- emit_progress(
    "stage",
    "started",
    stage = "warmup",
    step = "expert_fanout",
    payload = list(expert_count = length(ses$personas))
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

  research_expert <- function(idx) {
    persona <- ses$personas[[idx]]
    name <- persona$name %||% paste("Expert", idx)
    persona_id <- as.character(persona$id %||% idx)
    expert_event <- NULL
    answered_count <- 0L
    promises::then(promises::promise_resolve(NULL), function(...) {
      if (!warmup_is_current(is_current)) {
        return(NULL)
      }
      all_questions <- persona$initial_questions %||% character()
      questions <- warmup_selected_questions(
        all_questions,
        max_questions_per_expert
      )
      if (length(questions) == 0) {
        emit_progress(
          "expert",
          "skipped",
          stage = "warmup",
          step = "expert_fanout",
          parent_event_id = progress_event_id(warmup_event),
          payload = list(
            expert_id = persona_id,
            expert_name = name,
            reason = "no_initial_questions"
          )
        )
        return(NULL)
      }
      skipped <- max(length(all_questions) - length(questions), 0L)
      safe_append(sprintf(
        "**%s** researching (%d questions)...",
        name,
        length(questions)
      ))
      if (skipped > 0L) {
        safe_append(sprintf(
          "**%s** skipping %d remaining warmup questions for this run.",
          name,
          skipped
        ))
      }
      expert_event <<- emit_progress(
        "expert",
        "started",
        stage = "warmup",
        step = "expert_fanout",
        parent_event_id = progress_event_id(warmup_event),
        payload = list(
          expert_id = persona_id,
          expert_name = name,
          question_count = length(questions)
        )
      )
      session_result <- ses$expert_session_manager$get_or_create(persona)
      expert_chat <- session_result$chat
      expert_session_id <- session_result$session_id %||% NA_character_
      provenance <- session_result$provenance
      if (is.null(provenance)) {
        provenance <- new.env(parent = emptyenv())
        provenance$current <- list()
      }

      answered <- Reduce(
        function(acc, question_idx) {
          promises::then(acc, function(...) {
            if (!warmup_is_current(is_current)) {
              return(NULL)
            }

            question <- questions[[question_idx]]
            question_correlation_id <- tempest:::tempest_uuid("tool")
            label <- sprintf(
              "%s warmup question %d/%d",
              name,
              question_idx,
              length(questions)
            )
            safe_append(sprintf(
              "**%s** warmup question %d/%d: %s",
              name,
              question_idx,
              length(questions),
              warmup_question_label(question)
            ))
            question_event <- emit_progress(
              "tool",
              "started",
              stage = "warmup",
              step = "expert_question",
              parent_event_id = progress_event_id(expert_event),
              correlation_id = question_correlation_id,
              payload = list(
                expert_id = persona_id,
                expert_name = name,
                session_id = expert_session_id,
                question_index = question_idx
              )
            )

            old_provenance <- provenance$current %||% list()
            provenance$current <- list(
              session_id = expert_session_id,
              persona_id = persona_id,
              retrieval_step_id = question_correlation_id
            )
            chat_promise <- warmup_chat_response(
              expert_chat,
              warmup_prompt(ses$topic, question)
            )

            warmup_with_timeout(chat_promise, timeout_s, label) |>
              promises::then(function(response) {
                if (!warmup_is_current(is_current)) {
                  provenance$current <- old_provenance
                  return(NULL)
                }

                record_warmup_turn(
                  ses,
                  name,
                  question,
                  response,
                  expert_chat = expert_chat,
                  session_id = expert_session_id,
                  persona_id = persona_id,
                  correlation_id = question_correlation_id
                )
                provenance$current <- old_provenance
                answered_count <<- answered_count + 1L
                emit_progress(
                  "tool",
                  "succeeded",
                  stage = "warmup",
                  step = "expert_question",
                  parent_event_id = progress_event_id(question_event),
                  correlation_id = question_correlation_id,
                  payload = list(
                    expert_id = persona_id,
                    expert_name = name,
                    session_id = expert_session_id,
                    question_index = question_idx
                  )
                )
                safe_touch()
              }) |>
              promises::catch(function(e) {
                if (!warmup_is_current(is_current)) {
                  provenance$current <- old_provenance
                  return(NULL)
                }

                if (inherits(e, "tempest_warmup_timeout")) {
                  safe_append(sprintf(
                    "**%s** warmup question %d/%d timed out after %.0f seconds; skipping it.",
                    name,
                    question_idx,
                    length(questions),
                    timeout_s
                  ))
                } else {
                  safe_append(sprintf(
                    "**%s** warmup question %d/%d failed: %s",
                    name,
                    question_idx,
                    length(questions),
                    conditionMessage(e)
                  ))
                }
                emit_progress(
                  "tool",
                  "failed",
                  stage = "warmup",
                  step = "expert_question",
                  parent_event_id = progress_event_id(question_event),
                  correlation_id = question_correlation_id,
                  payload = c(
                    list(
                      expert_id = persona_id,
                      expert_name = name,
                      session_id = expert_session_id,
                      question_index = question_idx
                    ),
                    progress_error_payload(e)
                  )
                )
                provenance$current <- old_provenance
                NULL
              })
          }) |>
            promises::catch(function(e) {
              if (exists("old_provenance", inherits = FALSE)) {
                provenance$current <- old_provenance
              }
              if (warmup_is_current(is_current)) {
                safe_append(sprintf(
                  "**%s** warmup question %d/%d failed: %s",
                  name,
                  question_idx,
                  length(questions),
                  conditionMessage(e)
                ))
              }
              NULL
            })
        },
        seq_along(questions),
        promises::promise_resolve(NULL)
      )

      promises::then(answered, function(...) {
        if (!warmup_is_current(is_current)) {
          return(NULL)
        }
        emit_progress(
          "expert",
          "succeeded",
          stage = "warmup",
          step = "expert_fanout",
          parent_event_id = progress_event_id(expert_event),
          payload = list(
            expert_id = persona_id,
            expert_name = name,
            session_id = expert_session_id,
            questions_answered = answered_count
          )
        )
        safe_append(sprintf(
          "**%s** done (%d facts so far)",
          name,
          length(ses$store$list_claims())
        ))
      })
    }) |>
      promises::catch(function(e) {
        if (warmup_is_current(is_current)) {
          emit_progress(
            "expert",
            "failed",
            stage = "warmup",
            step = "expert_fanout",
            parent_event_id = progress_event_id(expert_event),
            payload = c(
              list(
                expert_id = persona_id,
                expert_name = name,
                session_id = expert_session_id
              ),
              progress_error_payload(e)
            )
          )
          safe_append(paste0("Warmup error: ", conditionMessage(e)))
        }
        NULL
      })
  }

  expert_indices <- seq_along(ses$personas)
  batches <- split(
    expert_indices,
    ceiling(seq_along(expert_indices) / max_parallel_experts)
  )
  run_batch <- function(prev, batch) {
    promises::then(prev, function(...) {
      if (!warmup_is_current(is_current)) {
        return(NULL)
      }
      promises::promise_all(.list = lapply(batch, research_expert)) |>
        promises::then(function(...) NULL)
    })
  }

  chain <- Reduce(run_batch, batches, promises::promise_resolve(NULL))
  promises::then(chain, function(...) {
    if (!warmup_is_current(is_current)) {
      return(NULL)
    }
    safe_append(warmup_summary(ses))
    emit_progress(
      "stage",
      "succeeded",
      stage = "warmup",
      step = "expert_fanout",
      parent_event_id = progress_event_id(warmup_event),
      payload = list(
        expert_count = length(ses$personas),
        claim_count = length(ses$store$list_claims()),
        source_count = length(ses$store$list_sources())
      )
    )
    safe_touch()
  })
}
