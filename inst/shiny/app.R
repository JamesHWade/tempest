devtools::load_all()
# tempest demo app (Co-STORM)
# Modern bslib/shiny implementation

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

ui <- bslib::page_navbar(
  title = shiny::tagList(shiny::icon("cloud-bolt"), "tempest"),
  id = "main_nav",
  theme = theme,
  fillable = TRUE,
  bslib::nav_spacer(),
  bslib::nav_item(bslib::input_dark_mode(id = "dark_mode", mode = "light")),
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
        shiny::actionButton(
          "start",
          "Start Session",
          class = "btn-primary w-100",
          icon = shiny::icon("play")
        ),
        shiny::hr(),
        shiny::selectInput(
          "report_style",
          "Report Style",
          choices = c("technical", "executive"),
          selected = "technical"
        ),
        shiny::actionButton(
          "generate_report",
          "Generate Report",
          class = "btn-secondary w-100",
          icon = shiny::icon("file-export")
        ),
        shiny::hr(),
        shiny::h6("Expert Panel"),
        shiny::uiOutput("expert_panel")
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_body(
          class = "p-0",
          shinychat::chat_ui("storm_chat", height = "100%")
        )
      )
    )
  ),
  bslib::nav_panel(
    title = shiny::tagList(shiny::icon("sitemap"), "Mind Map"),
    bslib::card(
      full_screen = TRUE,
      bslib::card_header("Knowledge Mind Map"),
      bslib::card_body(
        shiny::verbatimTextOutput("mindmap")
      )
    )
  ),
  bslib::nav_panel(
    title = shiny::tagList(shiny::icon("link"), "Sources"),
    bslib::card(
      full_screen = TRUE,
      bslib::card_header("Collected Sources"),
      bslib::card_body(
        class = "p-2",
        shiny::tableOutput("sources")
      )
    )
  ),
  bslib::nav_panel(
    title = shiny::tagList(shiny::icon("check-circle"), "Facts"),
    bslib::card(
      full_screen = TRUE,
      bslib::card_header("Extracted Facts"),
      bslib::card_body(
        class = "p-2",
        shiny::tableOutput("facts")
      )
    )
  ),
  bslib::nav_panel(
    title = shiny::tagList(shiny::icon("file-lines"), "Report"),
    bslib::card(
      full_screen = TRUE,
      bslib::card_header("Generated Report"),
      bslib::card_body(
        shiny::uiOutput("report")
      )
    )
  )
)

server <- function(input, output, session) {
  rlang::check_installed(
    c("promises", "shinychat", "ellmer"),
    reason = "to run the Co-STORM app."
  )

  `%||%` <- rlang::`%||%`

  # Session state
  storm_session <- shiny::reactiveVal(NULL)


  # Reactive trigger to force UI updates when R6 object is mutated
  # Increment this whenever the session's store changes
  data_version <- shiny::reactiveVal(0)
  bump_version <- function() data_version(data_version() + 1)

  # Helper to create session
  create_session <- function(topic, n_experts = 3) {
    cfg <- tempest::storm_config()
    ses <- tempest::costorm_session(topic, config = cfg, n_experts = n_experts)
    storm_session(ses)
    bump_version()
    shinychat::chat_clear("storm_chat")

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

    shinychat::chat_append(
      "storm_chat",
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

  # Start session button
  shiny::observeEvent(input$start, {
    topic <- input$topic %||% ""
    topic <- stringi::stri_trim_both(topic)
    if (identical(topic, "")) {
      return()
    }

    do_warmup <- input$do_warmup %||% FALSE
    ses <- create_session(topic, n_experts = input$n_experts)

    # Run warmup if requested
    if (do_warmup && length(ses$personas) > 0) {
      shinychat::chat_append(
        "storm_chat",
        "**Running warmup phase...** Each expert is researching their initial questions."
      )

      promises::future_promise({
        ses$warmup(verbose = FALSE)
      }) |>
        promises::then(
          onFulfilled = function(results) {
            total_facts <- length(ses$store$list_facts())
            total_sources <- length(ses$store$list_sources())
            shinychat::chat_append(
              "storm_chat",
              paste0(
                "**Warmup complete!** Collected ",
                total_facts,
                " facts from ",
                total_sources,
                " sources.\n\nYou can now ask questions."
              )
            )
            storm_session(ses)
            bump_version()
          },
          onRejected = function(e) {
            shinychat::chat_append(
              "storm_chat",
              paste0("Warmup error: ", conditionMessage(e))
            )
          }
        )
    }

    shiny::invalidateLater(10, session)
  })

  # Handle user chat input
  shiny::observeEvent(input$storm_chat_user_input, {
    msg <- input$storm_chat_user_input

    ses <- storm_session()
    if (is.null(ses)) {
      ses <- create_session(
        input$topic %||% "Untitled topic",
        n_experts = input$n_experts
      )
    }

    # Get the moderator chat - it maintains conversation history and has the system prompt
    chat <- ses$chats$moderator

    # Stream response with tool calls visible
    stream <- chat$stream_async(msg, stream = "content")

    shinychat::chat_append("storm_chat", stream)$then(
      onFulfilled = function(response) {
        ans <- if (is.character(response)) response else as.character(response)

        # Update session state
        ses$extract_facts(ans)
        ses$update_mindmap(
          last_exchange = paste0("User: ", msg, "\n\nModerator: ", ans)
        )
        storm_session(ses)
        bump_version()
      },
      onRejected = function(e) {
        shinychat::chat_append(
          "storm_chat",
          paste0("Error: ", conditionMessage(e))
        )
      }
    )
  })

  # Generate report button
  shiny::observeEvent(input$generate_report, {
    ses <- storm_session()
    if (is.null(ses)) {
      shinychat::chat_append("storm_chat", "No session active. Start a session first.")
      return()
    }

    # Check if we have any facts to report on
    facts <- ses$store$list_facts()
    sources <- ses$store$list_sources()
    if (length(facts) == 0 && length(sources) == 0) {
      shinychat::chat_append(
        "storm_chat",
        "No facts or sources collected yet. Ask some questions first to gather research."
      )
      return()
    }

    shinychat::chat_append("storm_chat", "Generating report...")

    md <- ses$report(style = input$report_style)
    if (is.null(md) || identical(md, "")) {
      shinychat::chat_append("storm_chat", "Report generation returned empty. Try asking more questions first.")
      return()
    }

    ses$artifacts[["report_md"]] <- md
    storm_session(ses)
    bump_version()
    shinychat::chat_append(
      "storm_chat",
      paste0("Report generated (", nchar(md), " chars). See the **Report** tab.")
    )
    bslib::nav_select("main_nav", "Report")
  })

  # Outputs - all depend on data_version() to trigger re-render when R6 object mutates
  output$mindmap <- shiny::renderText({
    data_version()  # Reactive dependency
    ses <- storm_session()
    if (is.null(ses)) {
      return("Start a session to see the mind map.")
    }
    ses$artifacts[["mindmap_md"]] %||% ses$mindmap_markdown()
  })

  output$sources <- shiny::renderTable(
    {
      data_version()  # Reactive dependency
      ses <- storm_session()
      if (is.null(ses)) {
        return(NULL)
      }
      tempest::storm_sources(ses$store)
    },
    striped = TRUE,
    hover = TRUE,
    bordered = TRUE,
    spacing = "s"
  )

  output$facts <- shiny::renderTable(
    {
      data_version()  # Reactive dependency
      ses <- storm_session()
      if (is.null(ses)) {
        return(NULL)
      }
      tempest::storm_facts(ses$store)
    },
    striped = TRUE,
    hover = TRUE,
    bordered = TRUE,
    spacing = "s"
  )

  output$report <- shiny::renderUI({
    data_version()  # Reactive dependency
    ses <- storm_session()
    if (is.null(ses)) {
      return(
        shiny::div(
          class = "text-center text-muted py-5",
          shiny::icon("file-lines", class = "fa-3x mb-3"),
          shiny::p("Generate a report to view it here.")
        )
      )
    }
    md <- ses$artifacts[["report_md"]] %||% ""
    if (identical(md, "")) {
      return(
        shiny::div(
          class = "text-center text-muted py-5",
          shiny::icon("file-lines", class = "fa-3x mb-3"),
          shiny::p("Generate a report to view it here.")
        )
      )
    }
    if (requireNamespace("commonmark", quietly = TRUE)) {
      shiny::HTML(commonmark::markdown_html(md))
    } else {
      shiny::pre(md)
    }
  })

  output$expert_panel <- shiny::renderUI({
    data_version()  # Reactive dependency
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
