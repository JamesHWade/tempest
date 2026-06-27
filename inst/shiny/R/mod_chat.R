# Chat tab: the interactive Co-STORM session.
#
# Session creation, the warmup phase, and per-turn fact/mind-map extraction all
# operate on a live `TempestSession` (which holds ellmer chats and is not
# serialisable), so they run in the main process rather than as ExtendedTasks.
# The warmup still streams progress into the chat between steps; it is a
# functional promise chain (no superassignment).

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
        bslib::card_body(
          class = "p-0",
          shinychat::chat_ui(ns("chat"), height = "100%", enable_cancel = TRUE)
        )
      )
    )
  )
}

mod_chat_server <- function(id, config, store) {
  shiny::moduleServer(id, function(input, output, session) {
    report_ready <- shiny::reactiveVal(0L)

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
    append_chat <- function(text) chat$append(text, role = "assistant")

    # Gated on the sidebar toggle; delegates to the testable append_suggestions().
    maybe_append_suggestions <- function(ses, n = 4) {
      append_suggestions(ses, input$suggest, append_chat, n)
    }

    # --- Session lifecycle ---------------------------------------------------
    create_session <- function(topic, n_experts) {
      ses <- tryCatch(
        tempest::tempest_session(
          topic,
          config = config(),
          n_experts = n_experts
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
      topic <- stringi::stri_trim_both(input$topic %||% "")
      if (!nzchar(topic)) {
        bslib::update_task_button("start", state = "ready")
        return()
      }
      ses <- create_session(topic, input$n_experts %||% 3)
      if (is.null(ses)) {
        bslib::update_task_button("start", state = "ready")
        return()
      }
      maybe_append_suggestions(ses)
      if (!isTRUE(input$warmup) || length(ses$personas) == 0) {
        bslib::update_task_button("start", state = "ready")
        return()
      }
      promises::finally(
        run_warmup(ses, store, append_chat),
        function() bslib::update_task_button("start", state = "ready")
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
      if (nzchar(msg)) {
        ses$add_turn("user", "user", msg)
      }
      if (nzchar(ans)) {
        ses$add_turn("Moderator", "assistant", ans)
      }
      tryCatch(
        ses$extract_facts(ans),
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
      store$touch()
      if (nzchar(msg)) {
        maybe_append_suggestions(ses)
      }
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
      store$set_report(md, ses$topic)
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
    class = "mb-2 p-2 border rounded small",
    shiny::strong(p$name %||% "Expert"),
    shiny::br(),
    shiny::span(class = "text-muted", p$title %||% ""),
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
suggestion_cards <- function(questions, lead = "**You might ask:**") {
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
  questions <- tryCatch(ses$suggest_questions(n), error = function(e) character())
  cards <- suggestion_cards(questions)
  if (!is.null(cards)) {
    append_fn(cards)
  }
  invisible(NULL)
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
    "- Use web_search + fetch_url to find and cite sources.\n",
    "- Only state factual claims supported by sources you fetched.\n",
    "- For each factual sentence, add citations like [Sxxxxxxxxxxxx].\n",
    "- If evidence is weak or unclear, say so.\n\n",
    "Respond now:"
  )
}

record_warmup_turn <- function(ses, name, question, response) {
  tryCatch(
    ses$expert_session_manager$extract_facts(response),
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

# Research each expert's initial questions in turn, streaming progress into the
# chat. Returns a promise that resolves when warmup finishes.
run_warmup <- function(ses, store, append_chat) {
  append_chat(
    "**Running warmup phase...** Each expert is researching their initial questions."
  )

  research_expert <- function(prev, idx) {
    promises::then(prev, function(...) {
      persona <- ses$personas[[idx]]
      name <- persona$name %||% paste("Expert", idx)
      questions <- persona$initial_questions %||% character()
      if (length(questions) == 0) {
        return(NULL)
      }
      append_chat(sprintf(
        "**%s** researching (%d questions)...",
        name,
        length(questions)
      ))
      expert_chat <- ses$expert_session_manager$get_or_create(persona)$chat

      answered <- Reduce(
        function(acc, question) {
          promises::then(acc, function(...) {
            expert_chat$chat_async(warmup_prompt(ses$topic, question))
          }) |>
            promises::then(function(response) {
              record_warmup_turn(ses, name, question, response)
              store$touch()
            })
        },
        questions,
        promises::promise_resolve(NULL)
      )

      promises::then(answered, function(...) {
        append_chat(sprintf(
          "**%s** done (%d facts so far)",
          name,
          length(ses$store$list_claims())
        ))
      })
    }) |>
      promises::catch(function(e) {
        append_chat(paste0("Warmup error: ", conditionMessage(e)))
        NULL
      })
  }

  chain <- Reduce(
    research_expert,
    seq_along(ses$personas),
    promises::promise_resolve(NULL)
  )
  promises::then(chain, function(...) {
    append_chat(warmup_summary(ses))
    store$touch()
  })
}
