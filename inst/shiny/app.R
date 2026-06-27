devtools::load_all()

`%||%` <- rlang::`%||%`

# Feature-detection for optional packages
has_dt <- requireNamespace("DT", quietly = TRUE)
has_visnetwork <- requireNamespace("visNetwork", quietly = TRUE)

# Helper: convert mindmap to visNetwork data.frames
mindmap_to_visnetwork <- function(mindmap) {
  nodes <- mindmap$nodes %||% list()
  edges <- mindmap$edges %||% list()
  if (length(nodes) == 0) {
    return(NULL)
  }

  # Build node levels via parent lookup
  parent_map <- list()
  for (nd in nodes) {
    if (!is.null(nd$parent)) parent_map[[nd$id]] <- nd$parent
  }
  get_level <- function(id) {
    lvl <- 1L
    cur <- id
    visited <- character()
    while (!is.null(parent_map[[cur]]) && !cur %in% visited) {
      visited <- c(visited, cur)
      lvl <- lvl + 1L
      cur <- parent_map[[cur]]
    }
    lvl
  }

  # Color scale by source count (light blue -> primary)
  color_for_count <- function(n) {
    frac <- min(n / 5, 1)
    r <- as.integer(200 - frac * (200 - 74))
    g <- as.integer(220 - frac * (220 - 144))
    b <- as.integer(240 - frac * (240 - 164))
    sprintf("#%02X%02X%02X", r, g, b)
  }

  node_ids <- character(length(nodes))
  node_labels <- character(length(nodes))
  node_levels <- integer(length(nodes))
  node_colors <- character(length(nodes))
  node_titles <- character(length(nodes))

  for (i in seq_along(nodes)) {
    nd <- nodes[[i]]
    nid <- nd$id %||% paste0("node_", i)
    node_ids[i] <- nid
    node_labels[i] <- nd$label %||% nid
    node_levels[i] <- get_level(nid)
    src_count <- length(nd$source_ids %||% character(0))
    node_colors[i] <- color_for_count(src_count)

    # HTML tooltip
    notes_html <- if (!is.null(nd$notes) && nzchar(nd$notes)) {
      paste0("<p>", htmltools::htmlEscape(nd$notes), "</p>")
    } else {
      ""
    }
    src_html <- if (src_count > 0) {
      paste0(
        "<p><em>Sources: ",
        htmltools::htmlEscape(paste(nd$source_ids, collapse = ", ")),
        "</em></p>"
      )
    } else {
      ""
    }
    node_titles[i] <- paste0(
      "<b>",
      htmltools::htmlEscape(nd$label %||% nid),
      "</b>",
      notes_html,
      src_html
    )
  }

  nodes_df <- data.frame(
    id = node_ids,
    label = node_labels,
    level = node_levels,
    color = node_colors,
    title = node_titles,
    stringsAsFactors = FALSE
  )

  if (length(edges) > 0) {
    edges_df <- data.frame(
      from = vapply(edges, function(e) e$from %||% "", character(1)),
      to = vapply(edges, function(e) e$to %||% "", character(1)),
      label = vapply(edges, function(e) e$relation %||% "", character(1)),
      arrows = "to",
      stringsAsFactors = FALSE
    )
  } else {
    edges_df <- data.frame(
      from = character(0),
      to = character(0),
      label = character(0),
      arrows = character(0),
      stringsAsFactors = FALSE
    )
  }

  list(nodes = nodes_df, edges = edges_df)
}

# Helper: centered placeholder for empty tab content
empty_state <- function(icon_name, message) {
  shiny::div(
    class = "text-center text-muted py-5",
    shiny::icon(icon_name, class = "fa-3x mb-3"),
    shiny::p(message)
  )
}

# Helper: create a DT datatable with standard export options
styled_datatable <- function(df) {
  DT::datatable(
    df,
    escape = FALSE,
    extensions = "Buttons",
    options = list(
      dom = "Bfrtip",
      buttons = c("csv", "excel"),
      pageLength = 25,
      scrollX = TRUE
    ),
    rownames = FALSE
  )
}

# Helper: slugify topic for filenames
topic_slug <- function(topic) {
  if (is.null(topic) || is.na(topic) || !nzchar(topic)) {
    return("untitled")
  }
  slug <- tolower(topic)
  slug <- gsub("[^a-z0-9]+", "-", slug)
  slug <- gsub("^-+|-+$", "", slug)
  if (nchar(slug) > 40) {
    slug <- substr(slug, 1, 40)
  }
  slug
}

# Theme configuration with storm brand colors
theme <- bslib::bs_theme(
  version = 5,
  preset = "shiny",
  primary = "#4A90A4",
  secondary = "#6C757D",
  success = "#5CB85C",
  info = "#5BC0DE",
  warning = "#F0AD4E",
  danger = "#D9534F"
)

# ── Config panel (reused in Chat sidebar and read by STORM tab) ──
config_accordion <- bslib::accordion(
  id = "config_accordion",
  open = FALSE,
  bslib::accordion_panel(
    title = "Models",
    icon = shiny::icon("microchip"),
    shiny::textInput(
      "cfg_coordinator",
      "Coordinator",
      value = "openai/gpt-5-mini"
    ),
    shiny::textInput("cfg_expert", "Expert", value = "openai/gpt-5-mini"),
    shiny::textInput("cfg_writer", "Writer", value = "openai/gpt-5-mini"),
    shiny::textInput("cfg_mindmap", "Mind Map", value = "openai/gpt-5-mini"),
    shiny::textInput("cfg_judge", "Judge", value = "openai/gpt-5-mini")
  ),
  bslib::accordion_panel(
    title = "Search",
    icon = shiny::icon("magnifying-glass"),
    shiny::selectInput(
      "cfg_search_provider",
      "Search Provider",
      choices = c(
        "native",
        "wikipedia",
        "you",
        "bing",
        "serper",
        "brave",
        "duckduckgo",
        "tavily",
        "searxng",
        "google",
        "azure_ai_search"
      ),
      selected = "native"
    )
  ),
  bslib::accordion_panel(
    title = "Advanced",
    icon = shiny::icon("sliders"),
    shiny::checkboxInput(
      "cfg_discourse",
      "Enable discourse manager",
      value = FALSE
    ),
    shiny::checkboxInput("cfg_unseen", "Surface unseen sources", value = FALSE),
    shiny::numericInput(
      "cfg_node_trigger",
      "Node expansion trigger",
      value = NA,
      min = 1,
      max = 50,
      step = 1
    )
  )
)

# ── UI ───────────────────────────────────────────────────────────
ui <- bslib::page_navbar(
  title = shiny::tagList(shiny::icon("cloud-bolt"), "tempest"),
  id = "main_nav",
  theme = theme,
  fillable = TRUE,
  bslib::nav_spacer(),
  bslib::nav_item(bslib::input_dark_mode(id = "dark_mode", mode = "light")),

  # ── 1. Chat (Co-STORM) ──
  bslib::nav_panel(
    title = shiny::tagList(shiny::icon("comments"), "Chat"),
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        title = "Session Settings",
        width = 300,
        shiny::textInput(
          "topic",
          "Research Topic",
          placeholder = "Enter a research topic..."
        ),
        shiny::sliderInput(
          "n_experts",
          "Number of Experts",
          min = 1,
          max = 5,
          value = 3
        ),
        shiny::checkboxInput(
          "do_warmup",
          "Run warmup (experts research initial questions)",
          value = FALSE
        ),
        bslib::input_task_button(
          "start",
          "Start Session",
          icon = shiny::icon("play"),
          label_busy = "Starting...",
          auto_reset = FALSE,
          class = "w-100"
        ),
        shiny::hr(),
        shiny::selectInput(
          "report_style",
          "Report Style",
          choices = c("technical", "executive"),
          selected = "technical"
        ),
        bslib::input_task_button(
          "generate_report",
          "Generate Report",
          icon = shiny::icon("file-export"),
          label_busy = "Generating...",
          type = "secondary",
          class = "w-100"
        ),
        shiny::hr(),
        shiny::h6("Expert Panel"),
        shiny::uiOutput("expert_panel"),
        shiny::hr(),
        config_accordion
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_body(
          class = "p-0",
          shinychat::chat_ui(
            "storm_chat",
            height = "100%",
            enable_cancel = TRUE
          )
        )
      )
    )
  ),

  # ── 2. STORM (scripted pipeline) ──
  bslib::nav_panel(
    title = shiny::tagList(shiny::icon("bolt"), "STORM"),
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        title = "STORM Settings",
        width = 300,
        shiny::textInput(
          "storm_topic",
          "Research Topic",
          placeholder = "Enter a research topic..."
        ),
        shiny::numericInput(
          "storm_n_experts",
          "Expert Count",
          value = 3,
          min = 1,
          max = 10
        ),
        shiny::selectInput(
          "storm_strategy",
          "Research Strategy",
          choices = c("key_questions", "conversation"),
          selected = "key_questions"
        ),
        shiny::numericInput(
          "storm_max_rounds",
          "Max Rounds",
          value = 3,
          min = 1,
          max = 10
        ),
        shiny::checkboxInput(
          "storm_parallel",
          "Parallel research",
          value = FALSE
        ),
        bslib::input_task_button(
          "storm_run",
          "Run STORM Pipeline",
          icon = shiny::icon("bolt"),
          label_busy = "Running...",
          auto_reset = FALSE,
          class = "w-100"
        )
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("STORM Pipeline Progress"),
        bslib::card_body(
          shiny::uiOutput("storm_progress"),
          shiny::uiOutput("storm_result")
        )
      )
    )
  ),

  # ── 3. Mind Map ──
  bslib::nav_panel(
    title = shiny::tagList(shiny::icon("sitemap"), "Mind Map"),
    bslib::card(
      full_screen = TRUE,
      bslib::card_header(shiny::uiOutput("mindmap_header")),
      bslib::card_body(
        if (has_visnetwork) {
          visNetwork::visNetworkOutput("mindmap_vis", height = "100%")
        } else {
          shiny::verbatimTextOutput("mindmap")
        }
      )
    )
  ),

  # ── 4. Sources (DT) ──
  bslib::nav_panel(
    title = shiny::tagList(shiny::icon("link"), "Sources"),
    bslib::card(
      full_screen = TRUE,
      bslib::card_header("Collected Sources"),
      bslib::card_body(
        class = "p-2",
        if (has_dt) {
          DT::DTOutput("sources")
        } else {
          shiny::tableOutput("sources_basic")
        }
      )
    )
  ),

  # ── 5. Facts (DT) ──
  bslib::nav_panel(
    title = shiny::tagList(shiny::icon("check-circle"), "Facts"),
    bslib::card(
      full_screen = TRUE,
      bslib::card_header("Extracted Facts"),
      bslib::card_body(
        class = "p-2",
        if (has_dt) DT::DTOutput("facts") else shiny::tableOutput("facts_basic")
      )
    )
  ),

  # ── 6. Transcript ──
  bslib::nav_panel(
    title = shiny::tagList(shiny::icon("clock"), "Transcript"),
    bslib::card(
      full_screen = TRUE,
      bslib::card_header(shiny::uiOutput("transcript_header")),
      bslib::card_body(
        class = "p-2",
        style = "overflow-y: auto;",
        shiny::uiOutput("transcript")
      )
    )
  ),

  # ── 7. Report (with download buttons) ──
  bslib::nav_panel(
    title = shiny::tagList(shiny::icon("file-lines"), "Report"),
    bslib::card(
      full_screen = TRUE,
      bslib::card_header(
        class = "d-flex justify-content-between align-items-center",
        shiny::span("Generated Report"),
        shiny::div(
          shiny::downloadButton(
            "dl_report_md",
            "Markdown",
            class = "btn-sm btn-outline-secondary me-1"
          ),
          shiny::downloadButton(
            "dl_report_html",
            "HTML",
            class = "btn-sm btn-outline-secondary"
          )
        )
      ),
      bslib::card_body(
        shiny::uiOutput("report")
      )
    )
  )
)

# ── Server ───────────────────────────────────────────────────────
server <- function(input, output, session) {
  rlang::check_installed(
    c("promises", "shinychat", "ellmer"),
    reason = "to run the Co-STORM app."
  )
  if (!"chat_server" %in% getNamespaceExports("shinychat")) {
    rlang::abort(
      "The tempest Shiny app requires the development version of shinychat with chat_server()."
    )
  }

  # Session state
  storm_session <- shiny::reactiveVal(NULL)

  # Reactive trigger to force UI updates when R6 object is mutated
  data_version <- shiny::reactiveVal(0)
  bump_version <- function() data_version(data_version() + 1)

  # STORM pipeline state
  storm_pipeline_status <- shiny::reactiveVal("idle") # idle | running | done | error
  storm_pipeline_step <- shiny::reactiveVal(0L)
  storm_pipeline_error <- shiny::reactiveVal(NULL)
  storm_pipeline_result <- shiny::reactiveVal(NULL)

  # ── Build config from inputs ──
  build_config <- function() {
    models <- list(
      coordinator = input$cfg_coordinator %||% "openai/gpt-5-mini",
      expert = input$cfg_expert %||% "openai/gpt-5-mini",
      writer = input$cfg_writer %||% "openai/gpt-5-mini",
      mindmap = input$cfg_mindmap %||% "openai/gpt-5-mini",
      judge = input$cfg_judge %||% "openai/gpt-5-mini"
    )
    trigger <- input$cfg_node_trigger
    if (is.na(trigger)) {
      trigger <- NULL
    }

    tempest::tempest_config(
      models = models,
      search_provider = input$cfg_search_provider %||% "native",
      enable_discourse_manager = input$cfg_discourse %||% FALSE,
      enable_unseen_surfacing = input$cfg_unseen %||% FALSE,
      node_expansion_trigger_count = trigger
    )
  }

  # ── shinychat adapter ──
  initial_chat <- build_config()$make_chat(
    "coordinator",
    system_prompt = paste(
      "You are the tempest chat shell.",
      "Ask the user to start a Co-STORM session from the sidebar before research chat.",
      sep = "\n"
    ),
    echo = "none"
  )
  chat_controls <- shinychat::chat_server(
    "storm_chat",
    initial_chat,
    greeting = shinychat::chat_greeting(
      paste(
        "Start a Co-STORM session from the sidebar.",
        "Then ask research questions here; responses stream with cancellation support."
      )
    )
  )

  chat_input_text <- function(x) {
    if (is.null(x)) {
      return("")
    }
    if (is.character(x)) {
      return(paste(x, collapse = "\n"))
    }
    if (is.list(x)) {
      return(paste(
        vapply(
          x,
          function(item) {
            if (is.character(item)) {
              paste(item, collapse = "\n")
            } else {
              as.character(item)
            }
          },
          character(1)
        ),
        collapse = "\n"
      ))
    }
    as.character(x)
  }

  append_chat <- function(response, role = "assistant") {
    chat_controls$append(response, role = role)
  }

  clear_chat <- function(messages = NULL, client_history = "clear") {
    chat_controls$clear(
      messages = messages,
      greeting = TRUE,
      client_history = client_history
    )
  }

  set_chat_client <- function(client, sync = FALSE) {
    chat_controls$set_client(client, sync = sync)
    invisible(NULL)
  }

  # ── Helper to create Co-STORM session ──
  create_session <- function(topic, n_experts = 3) {
    cfg <- build_config()
    ses <- tryCatch(
      tempest::tempest_session(topic, config = cfg, n_experts = n_experts),
      error = function(e) {
        shiny::showNotification(
          paste0("Failed to create session: ", conditionMessage(e)),
          type = "error",
          duration = 10
        )
        return(NULL)
      }
    )
    if (is.null(ses)) {
      return(NULL)
    }
    storm_session(ses)
    set_chat_client(ses$chats$moderator, sync = FALSE)
    bump_version()
    clear_chat()

    expert_intro <- if (length(ses$personas) > 0) {
      expert_list <- paste(
        vapply(
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
        ),
        collapse = "\n"
      )
      paste0("\n\n**Expert Panel:**\n", expert_list)
    } else {
      ""
    }

    append_chat(
      paste0(
        "Co-STORM session started for: **",
        topic,
        "**",
        expert_intro,
        "\n\nAsk questions, request sources, or ask for a report.\n"
      )
    )
    ses
  }

  # ── Start session button ──
  shiny::observeEvent(input$start, {
    topic <- input$topic %||% ""
    topic <- stringi::stri_trim_both(topic)
    if (identical(topic, "")) {
      bslib::update_task_button("start", state = "ready")
      return()
    }

    do_warmup <- input$do_warmup %||% FALSE
    ses <- create_session(topic, n_experts = input$n_experts)
    if (is.null(ses)) {
      bslib::update_task_button("start", state = "ready")
      return()
    }

    if (!do_warmup || length(ses$personas) == 0) {
      bslib::update_task_button("start", state = "ready")
      return()
    }

    # Streaming warmup: process one expert at a time via chat_async()
    append_chat(
      "**Running warmup phase...** Each expert is researching their initial questions."
    )

    warmup_next <- function(idx) {
      if (idx > length(ses$personas)) {
        return(promises::promise_resolve(TRUE))
      }

      persona <- ses$personas[[idx]]
      name <- persona$name %||% paste("Expert", idx)
      qs <- persona$initial_questions %||% character()

      if (length(qs) == 0) {
        return(warmup_next(idx + 1))
      }

      append_chat(
        paste0("**", name, "** researching (", length(qs), " questions)...")
      )

      session_result <- ses$expert_session_manager$get_or_create(persona)
      chat <- session_result$chat

      # Chain promises for each question
      chain <- promises::promise_resolve(NULL)
      for (q in qs) {
        local({
          question <- q
          prompt <- paste0(
            "Topic: ",
            ses$topic,
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
          chain <<- chain$then(function(...) {
            chat$chat_async(prompt)
          })$then(function(response) {
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
            bump_version()
          })
        })
      }

      # After all questions for this expert, report and move to next
      chain$then(function(...) {
        n_facts <- length(ses$store$list_facts())
        append_chat(
          paste0("**", name, "** done (", n_facts, " facts so far)")
        )
        warmup_next(idx + 1)
      })$catch(function(e) {
        append_chat(
          paste0("Error during **", name, "** warmup: ", conditionMessage(e))
        )
        warmup_next(idx + 1)
      })
    }

    warmup_next(1)$then(function(...) {
      facts <- ses$store$list_facts()
      total_facts <- length(facts)
      total_sources <- length(ses$store$list_sources())

      # Warm start summary — show top facts
      fact_summary <- ""
      if (total_facts > 0) {
        high_facts <- Filter(
          function(f) identical(f$confidence, "high"),
          facts
        )
        show_facts <- if (length(high_facts) > 0) {
          head(high_facts, 5)
        } else {
          head(facts, 5)
        }
        if (length(show_facts) > 0) {
          bullets <- paste(
            vapply(show_facts, function(f) paste0("- ", f$claim), character(1)),
            collapse = "\n"
          )
          fact_summary <- paste0("\n\n**Key findings so far:**\n", bullets)
        }
      }

      append_chat(
        paste0(
          "**Warmup complete!** Collected ",
          total_facts,
          " facts from ",
          total_sources,
          " sources.",
          fact_summary,
          "\n\nYou can now ask questions."
        )
      )
      storm_session(ses)
      bump_version()
      bslib::update_task_button("start", state = "ready")
    })$catch(function(e) {
      append_chat(
        paste0("Warmup error: ", conditionMessage(e))
      )
      bslib::update_task_button("start", state = "ready")
    })
  })

  # ── Post-process completed shinychat responses ──
  shiny::observeEvent(chat_controls$last_turn(), {
    ses <- storm_session()
    if (is.null(ses)) {
      return()
    }

    msg <- chat_input_text(chat_controls$last_input())
    turn <- chat_controls$last_turn()
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

    tryCatch(ses$extract_facts(ans), error = function(e) {
      warning("Fact extraction failed: ", conditionMessage(e))
    })
    tryCatch(
      ses$update_mindmap(
        last_exchange = paste0("User: ", msg, "\n\nModerator: ", ans)
      ),
      error = function(e) {
        warning("Mind map update failed: ", conditionMessage(e))
      }
    )
    storm_session(ses)
    bump_version()
  })

  # ── Generate report button ──
  shiny::observeEvent(input$generate_report, {
    ses <- storm_session()
    if (is.null(ses)) {
      append_chat(
        "No session active. Start a session first."
      )
      return()
    }

    facts <- ses$store$list_facts()
    sources <- ses$store$list_sources()
    if (length(facts) == 0 && length(sources) == 0) {
      append_chat(
        "No facts or sources collected yet. Ask some questions first to gather research."
      )
      return()
    }

    append_chat("Generating report...")

    md <- tryCatch(
      ses$report(style = input$report_style %||% "technical"),
      error = function(e) {
        append_chat(
          paste0("Report generation failed: ", conditionMessage(e))
        )
        return(NULL)
      }
    )
    if (is.null(md) || identical(md, "")) {
      append_chat(
        "Report generation returned empty. Try asking more questions first."
      )
      return()
    }

    ses$artifacts[["report_md"]] <- md
    storm_session(ses)
    bump_version()
    append_chat(
      paste0(
        "Report generated (",
        nchar(md),
        " chars). See the **Report** tab."
      )
    )
    bslib::nav_select("main_nav", "Report")
  })

  # ── STORM pipeline ──
  shiny::observeEvent(input$storm_run, {
    topic <- stringi::stri_trim_both(input$storm_topic %||% "")
    if (identical(topic, "")) {
      bslib::update_task_button("storm_run", state = "ready")
      return()
    }

    storm_pipeline_status("running")
    storm_pipeline_step(1L)
    storm_pipeline_error(NULL)
    storm_pipeline_result(NULL)

    cfg <- build_config()
    n_experts <- input$storm_n_experts %||% 3
    strategy <- input$storm_strategy %||% "key_questions"
    max_rounds <- input$storm_max_rounds %||% 3
    parallel <- input$storm_parallel %||% FALSE

    promises::future_promise({
      tempest::tempest_run(
        topic = topic,
        config = cfg,
        n_experts = n_experts,
        research_strategy = strategy,
        max_rounds = max_rounds,
        parallel_research = parallel,
        verbose = FALSE
      )
    }) |>
      promises::then(
        onFulfilled = function(result) {
          storm_pipeline_step(5L)
          storm_pipeline_status("done")
          storm_pipeline_result(result)

          # Store report in Co-STORM session artifacts for shared access
          ses <- storm_session()
          if (!is.null(ses)) {
            ses$artifacts[["report_md"]] <- result$report_md
            storm_session(ses)
          }
          bump_version()
          bslib::update_task_button("storm_run", state = "ready")
        },
        onRejected = function(e) {
          storm_pipeline_status("error")
          storm_pipeline_error(conditionMessage(e))
          bslib::update_task_button("storm_run", state = "ready")
        }
      )
  })

  # STORM progress UI
  output$storm_progress <- shiny::renderUI({
    status <- storm_pipeline_status()
    if (status == "idle") {
      return(empty_state(
        "bolt",
        "Configure settings and click 'Run STORM Pipeline' to begin."
      ))
    }

    step <- storm_pipeline_step()
    steps <- list(
      list(name = "Perspectives", icon = "users"),
      list(name = "Research", icon = "magnifying-glass"),
      list(name = "Outline", icon = "list-ol"),
      list(name = "Write", icon = "pen-nib"),
      list(name = "Polish", icon = "wand-magic-sparkles")
    )

    step_style <- function(i, step, status) {
      if (status == "error") {
        if (i == step) {
          list(cls = "text-danger", icon = shiny::icon("circle-xmark"))
        } else if (i < step) {
          list(cls = "text-danger", icon = shiny::icon("circle-check"))
        } else {
          list(cls = "text-muted", icon = shiny::icon("circle"))
        }
      } else if (i < step || (i == step && status == "done")) {
        list(cls = "text-success", icon = shiny::icon("circle-check"))
      } else if (i == step && status == "running") {
        list(
          cls = "text-primary",
          icon = shiny::icon("spinner", class = "fa-spin")
        )
      } else {
        list(cls = "text-muted", icon = shiny::icon("circle"))
      }
    }

    step_items <- lapply(seq_along(steps), function(i) {
      s <- steps[[i]]
      style <- step_style(i, step, status)
      icon_cls <- style$cls
      status_icon <- style$icon
      shiny::div(
        class = paste("d-flex align-items-center mb-2", icon_cls),
        shiny::span(class = "me-2", status_icon),
        shiny::icon(s$icon, class = "me-2"),
        shiny::strong(s$name)
      )
    })

    shiny::div(class = "py-3 px-2", step_items)
  })

  # STORM result UI
  output$storm_result <- shiny::renderUI({
    status <- storm_pipeline_status()
    if (status == "error") {
      err <- storm_pipeline_error()
      return(shiny::div(
        class = "alert alert-danger mt-3",
        shiny::icon("triangle-exclamation"),
        " Pipeline error: ",
        err
      ))
    }
    if (status != "done") {
      return(NULL)
    }

    result <- storm_pipeline_result()
    if (is.null(result)) {
      return(NULL)
    }
    char_count <- nchar(result$report_md %||% "")

    shiny::div(
      class = "alert alert-success mt-3",
      shiny::icon("circle-check"),
      paste0(" Pipeline complete! Report: ", char_count, " characters. "),
      shiny::actionLink(
        "storm_view_report",
        "View Report",
        class = "alert-link"
      )
    )
  })

  # Navigate to report tab when clicking "View Report"
  shiny::observeEvent(input$storm_view_report, {
    result <- storm_pipeline_result()
    if (!is.null(result)) {
      # Ensure report is stored in session artifacts
      ses <- storm_session()
      if (is.null(ses)) {
        # Create a lightweight session to hold the report
        ses <- create_session(
          input$storm_topic %||% "STORM Report",
          n_experts = 0
        )
      }
      ses$artifacts[["report_md"]] <- result$report_md
      storm_session(ses)
      bump_version()
    }
    bslib::nav_select("main_nav", "Report")
  })

  # ── Outputs ────────────────────────────────────────────────────

  # Mind Map header with stats
  output$mindmap_header <- shiny::renderUI({
    data_version()
    ses <- storm_session()
    if (is.null(ses) || is.null(ses$mindmap)) {
      return(shiny::span("Knowledge Mind Map"))
    }
    n_nodes <- length(ses$mindmap$nodes %||% list())
    n_edges <- length(ses$mindmap$edges %||% list())
    shiny::tagList(
      shiny::span("Knowledge Mind Map"),
      shiny::span(
        class = "badge bg-secondary ms-2",
        paste0(n_nodes, " nodes, ", n_edges, " edges")
      )
    )
  })

  # Mind Map (visNetwork or text fallback)
  if (has_visnetwork) {
    output$mindmap_vis <- visNetwork::renderVisNetwork({
      data_version()
      ses <- storm_session()
      if (is.null(ses) || is.null(ses$mindmap)) {
        return(
          visNetwork::visNetwork(
            data.frame(
              id = 1,
              label = "Start a session to see the mind map",
              stringsAsFactors = FALSE
            ),
            data.frame(
              from = integer(0),
              to = integer(0),
              stringsAsFactors = FALSE
            )
          ) |>
            visNetwork::visOptions(highlightNearest = FALSE) |>
            visNetwork::visInteraction(dragNodes = FALSE)
        )
      }

      vn <- mindmap_to_visnetwork(ses$mindmap)
      if (is.null(vn)) {
        return(NULL)
      }

      visNetwork::visNetwork(vn$nodes, vn$edges) |>
        visNetwork::visHierarchicalLayout(
          direction = "UD",
          sortMethod = "directed"
        ) |>
        visNetwork::visNodes(
          shape = "box",
          font = list(size = 14),
          widthConstraint = list(maximum = 200)
        ) |>
        visNetwork::visEdges(
          arrows = "to",
          color = list(color = "#6C757D"),
          smooth = list(type = "cubicBezier")
        ) |>
        visNetwork::visOptions(
          highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE),
          nodesIdSelection = FALSE
        ) |>
        visNetwork::visEvents(
          click = "function(params) {
            if (params.nodes.length > 0) {
              Shiny.setInputValue('mindmap_selected_node', params.nodes[0], {priority: 'event'});
            }
          }"
        ) |>
        visNetwork::visInteraction(
          navigationButtons = TRUE,
          zoomView = TRUE
        )
    })

    # Click-to-inspect modal
    shiny::observeEvent(input$mindmap_selected_node, {
      ses <- storm_session()
      if (is.null(ses) || is.null(ses$mindmap)) {
        return()
      }

      node_id <- input$mindmap_selected_node
      node <- NULL
      for (nd in ses$mindmap$nodes) {
        if (identical(nd$id, node_id)) {
          node <- nd
          break
        }
      }
      if (is.null(node)) {
        return()
      }

      notes_ui <- if (!is.null(node$notes) && nzchar(node$notes)) {
        shiny::p(node$notes)
      } else {
        shiny::p(class = "text-muted", "No notes available.")
      }
      src_ui <- if (length(node$source_ids %||% character(0)) > 0) {
        shiny::p(
          shiny::strong("Sources: "),
          paste(node$source_ids, collapse = ", ")
        )
      } else {
        shiny::p(class = "text-muted", "No sources linked.")
      }

      shiny::showModal(shiny::modalDialog(
        title = node$label %||% node_id,
        notes_ui,
        src_ui,
        easyClose = TRUE,
        footer = shiny::modalButton("Close")
      ))
    })
  }

  # Text fallback for mind map
  output$mindmap <- shiny::renderText({
    data_version()
    ses <- storm_session()
    if (is.null(ses)) {
      return("Start a session to see the mind map.")
    }
    ses$artifacts[["mindmap_md"]] %||% ses$mindmap_markdown()
  })

  # ── Sources ──
  if (has_dt) {
    output$sources <- DT::renderDT({
      data_version()
      ses <- storm_session()
      if (is.null(ses)) {
        return(NULL)
      }
      df <- tempest::tempest_sources(ses$store)
      if (is.null(df) || nrow(df) == 0) {
        return(NULL)
      }

      # Make URLs clickable
      if ("url" %in% names(df)) {
        df$url <- vapply(
          df$url,
          function(u) {
            if (is.na(u) || !nzchar(u)) {
              return("")
            }
            paste0(
              '<a href="',
              htmltools::htmlEscape(u),
              '" target="_blank">',
              htmltools::htmlEscape(u),
              '</a>'
            )
          },
          character(1)
        )
      }

      styled_datatable(df)
    })
  }

  output$sources_basic <- shiny::renderTable(
    {
      data_version()
      ses <- storm_session()
      if (is.null(ses)) {
        return(NULL)
      }
      tempest::tempest_sources(ses$store)
    },
    striped = TRUE,
    hover = TRUE,
    bordered = TRUE,
    spacing = "s"
  )

  # ── Facts ──
  if (has_dt) {
    output$facts <- DT::renderDT({
      data_version()
      ses <- storm_session()
      if (is.null(ses)) {
        return(NULL)
      }
      df <- tempest::tempest_facts(ses$store)
      if (is.null(df) || nrow(df) == 0) {
        return(NULL)
      }

      # Confidence badges
      if ("confidence" %in% names(df)) {
        df$confidence <- vapply(
          df$confidence,
          function(c) {
            color <- switch(
              tolower(c %||% ""),
              high = "bg-success",
              medium = "bg-warning text-dark",
              low = "bg-danger",
              "bg-secondary"
            )
            paste0(
              '<span class="badge ',
              color,
              '">',
              htmltools::htmlEscape(c %||% "N/A"),
              '</span>'
            )
          },
          character(1)
        )
      }

      # Collapse source_ids
      if ("source_ids" %in% names(df)) {
        df$source_ids <- vapply(
          df$source_ids,
          function(ids) {
            if (is.list(ids)) {
              ids <- unlist(ids)
            }
            paste(ids, collapse = ", ")
          },
          character(1)
        )
      }

      styled_datatable(df)
    })
  }

  output$facts_basic <- shiny::renderTable(
    {
      data_version()
      ses <- storm_session()
      if (is.null(ses)) {
        return(NULL)
      }
      tempest::tempest_facts(ses$store)
    },
    striped = TRUE,
    hover = TRUE,
    bordered = TRUE,
    spacing = "s"
  )

  # ── Transcript ──
  output$transcript_header <- shiny::renderUI({
    data_version()
    ses <- storm_session()
    n_turns <- if (is.null(ses)) 0L else length(ses$transcript %||% list())
    shiny::tagList(
      shiny::span("Conversation Transcript"),
      if (n_turns > 0) {
        shiny::span(
          class = "badge bg-secondary ms-2",
          paste0(n_turns, " turns")
        )
      }
    )
  })

  output$transcript <- shiny::renderUI({
    data_version()
    ses <- storm_session()
    if (is.null(ses) || length(ses$transcript %||% list()) == 0) {
      return(empty_state(
        "clock",
        "Start a conversation to see the transcript."
      ))
    }

    turns <- ses$transcript
    # Limit to most recent 100 turns
    if (length(turns) > 100) {
      turns <- turns[(length(turns) - 99):length(turns)]
    }

    has_commonmark <- requireNamespace("commonmark", quietly = TRUE)

    cards <- lapply(turns, function(turn) {
      role <- turn$role %||% "assistant"
      speaker <- turn$speaker %||% role
      text <- turn$text %||% ""
      at <- turn$at %||% ""

      is_user <- identical(role, "user")
      bg_class <- if (is_user) "bg-light" else ""
      icon_name <- if (is_user) "user" else "robot"

      text_html <- if (has_commonmark && nzchar(text)) {
        shiny::HTML(commonmark::markdown_html(text))
      } else {
        shiny::p(text)
      }

      shiny::div(
        class = paste("card mb-2", bg_class),
        shiny::div(
          class = "card-body py-2 px-3",
          shiny::div(
            class = "d-flex justify-content-between align-items-center mb-1",
            shiny::span(
              shiny::icon(icon_name, class = "me-1"),
              shiny::strong(speaker)
            ),
            if (nzchar(at)) shiny::small(class = "text-muted", at)
          ),
          text_html
        )
      )
    })

    shiny::tagList(cards)
  })

  # ── Report ──
  output$report <- shiny::renderUI({
    data_version()
    ses <- storm_session()
    md <- if (!is.null(ses)) ses$artifacts[["report_md"]] %||% "" else ""
    if (identical(md, "")) {
      return(empty_state("file-lines", "Generate a report to view it here."))
    }
    if (requireNamespace("commonmark", quietly = TRUE)) {
      shiny::HTML(commonmark::markdown_html(md))
    } else {
      shiny::pre(md)
    }
  })

  # ── Report download handlers ──
  report_filename <- function(ext) {
    ses <- storm_session()
    slug <- if (!is.null(ses)) topic_slug(ses$topic) else "report"
    paste0("tempest-", slug, ext)
  }

  report_md <- function() {
    ses <- storm_session()
    if (!is.null(ses)) ses$artifacts[["report_md"]] %||% "" else ""
  }

  output$dl_report_md <- shiny::downloadHandler(
    filename = function() report_filename(".md"),
    content = function(file) {
      md <- report_md()
      shiny::req(nzchar(md))
      writeLines(md, file)
    },
    contentType = "text/markdown"
  )

  output$dl_report_html <- shiny::downloadHandler(
    filename = function() report_filename(".html"),
    content = function(file) {
      md <- report_md()
      shiny::req(nzchar(md))
      body_html <- if (requireNamespace("commonmark", quietly = TRUE)) {
        commonmark::markdown_html(md)
      } else {
        paste0("<pre>", htmltools::htmlEscape(md), "</pre>")
      }
      html_doc <- paste0(
        '<!DOCTYPE html>\n<html lang="en">\n<head>\n',
        '<meta charset="UTF-8">\n',
        '<meta name="viewport" content="width=device-width, initial-scale=1.0">\n',
        '<title>tempest Report</title>\n',
        '<style>\n',
        'body { max-width: 800px; margin: 2rem auto; padding: 0 1rem; ',
        'font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif; ',
        'line-height: 1.6; color: #333; }\n',
        'h1, h2, h3 { color: #4A90A4; }\n',
        'a { color: #4A90A4; }\n',
        'pre { background: #f5f5f5; padding: 1rem; border-radius: 4px; overflow-x: auto; }\n',
        'code { background: #f5f5f5; padding: 0.15em 0.3em; border-radius: 3px; }\n',
        'blockquote { border-left: 3px solid #4A90A4; margin-left: 0; padding-left: 1rem; color: #666; }\n',
        'table { border-collapse: collapse; width: 100%; margin: 1rem 0; }\n',
        'th, td { border: 1px solid #ddd; padding: 0.5rem; text-align: left; }\n',
        'th { background: #f5f5f5; }\n',
        '</style>\n</head>\n<body>\n',
        body_html,
        '\n</body>\n</html>'
      )
      writeLines(html_doc, file)
    },
    contentType = "text/html"
  )

  # ── Expert panel ──
  output$expert_panel <- shiny::renderUI({
    data_version()
    ses <- storm_session()
    if (is.null(ses) || length(ses$personas) == 0) {
      return(shiny::p(
        class = "text-muted small",
        "Start a session to see experts."
      ))
    }

    expert_cards <- lapply(ses$personas, function(p) {
      shiny::div(
        class = "mb-2 p-2 border rounded small",
        shiny::strong(p$name %||% "Expert"),
        shiny::br(),
        shiny::span(class = "text-muted", p$title %||% ""),
        if (!is.null(p$perspective) && nzchar(p$perspective)) {
          shiny::div(class = "mt-1 fst-italic", p$perspective)
        }
      )
    })

    shiny::tagList(expert_cards)
  })
}

shiny::shinyApp(ui, server)
