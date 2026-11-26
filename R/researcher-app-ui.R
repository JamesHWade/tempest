tempest_researcher_ui <- function(
  tool_catalog = data.frame(id = character(), label = character()),
  mcp_servers = character(),
  model = "openai/gpt-6.1-sol",
  max_experts = 5L,
  library_enabled = FALSE,
  memory_enabled = FALSE,
  memory_streams = character()
) {
  rlang::check_bool(library_enabled)
  rlang::check_bool(memory_enabled)
  max_experts <- min(10L, max_experts)
  if (
    !is.data.frame(tool_catalog) ||
      !all(c("id", "label") %in% names(tool_catalog))
  ) {
    stop("`tool_catalog` must contain `id` and `label` columns.", call. = FALSE)
  }

  tool_choices <- stats::setNames(
    as.character(tool_catalog$id),
    as.character(tool_catalog$label)
  )

  css <- "
    :root {
      --researcher-ink: #202833;
      --researcher-muted: #657386;
      --researcher-blue: #315fae;
      --researcher-line: #e3e8ef;
      --researcher-paper: #ffffff;
      --researcher-sans: system-ui, -apple-system, BlinkMacSystemFont,
        'Segoe UI', sans-serif;
      --researcher-serif: ui-serif, 'Iowan Old Style',
        'Palatino Linotype', 'Book Antiqua', Georgia, serif;
    }

    body {
      color: var(--researcher-ink);
      background: #f6f8fb;
      font-family: var(--researcher-sans);
    }

    .navbar {
      min-height: 4.25rem;
      background: var(--researcher-paper);
      border-bottom: 1px solid var(--researcher-line);
    }

    .navbar-brand {
      color: var(--researcher-ink);
      font-size: 1.05rem;
      font-weight: 700;
      letter-spacing: -0.02em;
    }

    .bslib-sidebar-layout > .sidebar {
      background: var(--researcher-paper);
      border-right: 1px solid var(--researcher-line);
    }

    .researcher-sidebar {
      display: flex;
      flex-direction: column;
      gap: 1.05rem;
      padding-bottom: 1rem;
      font-family: var(--researcher-sans);
    }

    .researcher-sidebar h2,
    .researcher-sidebar h3 {
      margin: 0;
      color: var(--researcher-ink);
    }

    .researcher-sidebar h2 {
      font-family: var(--researcher-serif);
      font-size: 1.45rem;
      letter-spacing: -0.025em;
    }

    .researcher-sidebar-intro,
    .researcher-help,
    .researcher-tools-note {
      color: var(--researcher-muted);
      font-size: 0.875rem;
      line-height: 1.55;
    }

    .researcher-sidebar-intro {
      margin: 0.25rem 0 0;
    }

    .researcher-sidebar .form-label,
    .researcher-sidebar .control-label {
      color: var(--researcher-ink);
      font-size: 0.9rem;
      font-weight: 600;
    }

    .researcher-sidebar .form-control,
    .researcher-sidebar .selectize-input {
      border-color: #d8dfe8;
      border-radius: 0.55rem;
    }

    .researcher-sidebar .form-control:focus,
    .researcher-sidebar .selectize-input.focus {
      border-color: var(--researcher-blue);
      box-shadow: 0 0 0 0.2rem rgb(49 95 174 / 15%);
    }

    .researcher-accordion {
      --bs-accordion-border-color: var(--researcher-line);
      --bs-accordion-bg: transparent;
      --bs-accordion-btn-focus-box-shadow: 0 0 0 0.15rem
        rgb(49 95 174 / 16%);
    }

    .researcher-accordion .accordion-button {
      padding: 0.85rem 0.2rem;
      color: var(--researcher-ink);
      font-size: 0.92rem;
      font-weight: 650;
    }

    .researcher-accordion .accordion-body {
      padding: 0.5rem 0 1rem;
    }

    .researcher-number-grid {
      display: grid;
      grid-template-columns: repeat(2, minmax(0, 1fr));
      gap: 0.25rem 0.75rem;
    }

    .researcher-number-grid .form-group,
    .researcher-number-grid .shiny-input-container {
      min-width: 0;
    }

    .researcher-check-stack {
      display: grid;
      gap: 0.75rem;
      margin-top: 0.65rem;
    }

    .researcher-check-stack .checkbox {
      margin: 0;
    }

    .researcher-help {
      margin: 0.25rem 0 0;
    }

    .researcher-tools-note {
      margin: 0 0 0.75rem;
    }

    .researcher-briefs {
      margin-top: -0.25rem;
    }

    .researcher-brief-disclosure {
      padding: 0.7rem 0.8rem;
      background: #fbfcfe;
      border: 1px solid var(--researcher-line);
      border-radius: 0.65rem;
    }

    .researcher-brief-disclosure summary {
      color: var(--researcher-blue);
      cursor: pointer;
      font-size: 0.86rem;
      font-weight: 650;
    }

    .researcher-brief-disclosure > .researcher-help {
      margin: 0.65rem 0 0.75rem;
    }

    .researcher-brief-actions {
      display: grid;
      gap: 0.6rem;
      margin: 0;
      padding: 0;
      list-style: none;
    }

    .researcher-brief-action {
      display: grid;
      grid-template-columns: minmax(0, 1fr) auto;
      align-items: center;
      gap: 0.7rem;
      padding-top: 0.6rem;
      border-top: 1px solid var(--researcher-line);
    }

    .researcher-brief-action-copy {
      display: grid;
      gap: 0.15rem;
      min-width: 0;
      color: var(--researcher-muted);
      font-size: 0.78rem;
      line-height: 1.45;
    }

    .researcher-brief-action-copy strong {
      color: var(--researcher-ink);
      font-size: 0.82rem;
    }

    .researcher-brief-select {
      white-space: nowrap;
      border-color: #b9c9e2;
      font-size: 0.78rem;
    }

    .researcher-brief-disclosure summary:focus-visible,
    .researcher-brief-select:focus-visible,
    .tempest-researcher-library a:focus-visible,
    .tempest-researcher-library button:focus-visible {
      outline: 0.2rem solid rgb(49 95 174 / 38%);
      outline-offset: 0.12rem;
    }

    .researcher-brief-select:hover,
    .researcher-brief-select:focus {
      color: #234b8d;
      background: #edf3fc;
      border-color: var(--researcher-blue);
    }

    .researcher-brief-status {
      min-height: 1.2rem;
      margin: 0;
      color: var(--researcher-muted);
      font-size: 0.78rem;
      line-height: 1.45;
    }

    .tempest-researcher-library {
      margin-top: 1.5rem;
      padding: clamp(1rem, 3vw, 1.5rem);
      background: var(--researcher-paper);
      border: 1px solid var(--researcher-line);
      border-radius: 0.8rem;
      box-shadow: 0 12px 34px rgb(25 40 60 / 4%);
    }

    .tempest-researcher-library summary,
    .researcher-evidence-navigator summary {
      cursor: pointer;
      font: 600 .95rem var(--researcher-sans);
      color: var(--researcher-ink);
    }

    .tempest-researcher-library[open] summary {margin-bottom: 1rem;}
    .tempest-researcher-library [role=status] {margin: .5rem 0;}
    .tempest-researcher-library .shiny-input-container {width: 100%;}
    .researcher-evidence-navigator details {
      padding: .65rem 0;
      border-top: 1px solid var(--researcher-line);
    }
    .researcher-evidence-navigator {margin-bottom: 1.25rem;}
    .researcher-evidence-navigator li {margin-block: .45rem;}

    .researcher-sidebar .btn {
      border-radius: 0.55rem;
      font-weight: 600;
    }

    .researcher-run-controls {
      display: grid;
      grid-template-columns: minmax(0, 1fr) auto;
      gap: 0.65rem;
      margin-top: 0.15rem;
    }

    .researcher-run-controls .btn-primary {
      min-height: 2.75rem;
      background: var(--researcher-blue);
      border-color: var(--researcher-blue);
    }

    .researcher-main {
      width: min(100%, 66rem);
      margin: 0 auto;
      padding: clamp(1.25rem, 3.5vw, 3rem);
    }

    .researcher-report-header {
      display: flex;
      align-items: flex-start;
      justify-content: space-between;
      gap: 1.5rem;
      padding-bottom: 1.35rem;
      border-bottom: 1px solid var(--researcher-line);
    }

    .researcher-eyebrow {
      margin: 0 0 0.4rem;
      color: var(--researcher-blue);
      font-size: 0.73rem;
      font-weight: 700;
      letter-spacing: 0.12em;
      text-transform: uppercase;
    }

    .researcher-report-header h1 {
      margin: 0;
      color: var(--researcher-ink);
      font-family: var(--researcher-serif);
      font-size: clamp(2rem, 4vw, 2.7rem);
      letter-spacing: -0.035em;
      line-height: 1.08;
    }

    .researcher-report-deck {
      max-width: 47rem;
      margin: 0.7rem 0 0;
      color: var(--researcher-muted);
      line-height: 1.55;
    }

    .researcher-download {
      flex: 0 0 auto;
      margin-top: 0.2rem;
      border-radius: 0.55rem;
      font-weight: 600;
    }

    .researcher-live-status,
    .researcher-tools-status {
      color: var(--researcher-muted);
      font-size: 0.9rem;
      line-height: 1.5;
    }

    .researcher-live-status:empty,
    .researcher-tools-status:empty {
      display: none;
    }

    .researcher-progress {
      margin: 0.75rem 0 1.25rem;
    }

    .researcher-report-paper {
      min-height: 28rem;
      margin-top: 1.25rem;
      padding: clamp(1.35rem, 4vw, 3rem);
      overflow-wrap: anywhere;
      background: var(--researcher-paper);
      border: 1px solid var(--researcher-line);
      border-radius: 0.8rem;
      box-shadow: 0 12px 34px rgb(25 40 60 / 4%);
    }

    .researcher-report-copy {
      width: 100%;
      color: #303b49;
      font-family: var(--researcher-serif);
      font-size: 1.04rem;
      line-height: 1.75;
    }

    .researcher-report-copy > :not(iframe) {
      max-width: 74ch;
    }

    .researcher-report-copy h1,
    .researcher-report-copy h2,
    .researcher-report-copy h3,
    .researcher-report-copy h4 {
      margin-top: 1.8em;
      color: var(--researcher-ink);
      font-family: var(--researcher-serif);
      line-height: 1.2;
    }

    .researcher-report-copy h1:first-child,
    .researcher-report-copy h2:first-child {
      margin-top: 0;
    }

    .researcher-report-copy a {
      color: var(--researcher-blue);
      text-underline-offset: 0.16em;
    }

    .researcher-report-copy table {
      display: block;
      max-width: 100%;
      overflow-x: auto;
      font-family: var(--researcher-sans);
      font-size: 0.9rem;
    }

    .researcher-report-copy img {
      max-width: 100%;
      height: auto;
    }

    .researcher-report-copy iframe {
      width: 100%;
      max-width: none;
    }

    @media (max-width: 767.98px) {
      body.bslib-page-sidebar .bslib-sidebar-layout {
        display: flex !important;
        flex-direction: column;
        flex: none !important;
        height: auto !important;
        overflow: visible;
      }

      body.bslib-page-sidebar .bslib-sidebar-layout > .main {
        flex: none !important;
        height: auto !important;
        overflow: visible;
      }

      .bslib-sidebar-layout > .sidebar {
        order: -1;
        flex: 0 0 auto;
        width: 100%;
        overflow: visible;
        border-right: 0;
        border-bottom: 1px solid var(--researcher-line);
      }

      .researcher-main {
        padding: 1.25rem 1rem 2rem;
      }

      .researcher-report-header {
        flex-direction: column;
        gap: 1rem;
      }

      .researcher-download {
        width: 100%;
        margin-top: 0;
      }

      .researcher-number-grid {
        grid-template-columns: minmax(0, 1fr);
      }

      .researcher-brief-action {
        grid-template-columns: minmax(0, 1fr);
      }

      .researcher-brief-select {
        justify-self: start;
      }

      .researcher-report-paper {
        min-height: 18rem;
        padding: 1.25rem;
      }
    }
  "

  theme <- bslib::bs_theme(
    version = 5,
    bg = "#f6f8fb",
    fg = "#202833",
    primary = "#315fae",
    base_font = "system-ui"
  )

  bslib::page_sidebar(
    title = "Tempest Researcher",
    window_title = "Tempest Researcher",
    theme = theme,
    fillable = FALSE,
    sidebar = bslib::sidebar(
      width = 390,
      shiny::div(
        class = "researcher-sidebar",
        shiny::div(
          shiny::h2("Research brief"),
          shiny::p(
            class = "researcher-sidebar-intro",
            "Run research with the sources and tools you choose."
          )
        ),
        shiny::textAreaInput(
          "topic",
          "Research question",
          rows = 4,
          placeholder = "What question should this research answer?"
        ),
        tempest_researcher_briefs_ui("briefs", topic_id = "topic"),
        shiny::textInput(
          "model",
          "Model",
          value = model
        ),
        bslib::accordion(
          id = "researcher_options",
          multiple = TRUE,
          open = FALSE,
          class = "researcher-accordion",
          bslib::accordion_panel(
            "Research limits",
            value = "limits",
            shiny::selectInput(
              "research_strategy",
              "Research approach",
              choices = c(
                "Key questions" = "key_questions",
                "Expert conversations" = "conversation"
              ),
              selected = "key_questions"
            ),
            shiny::div(
              class = "researcher-number-grid",
              shiny::numericInput(
                "n_experts",
                "Experts",
                min = 1,
                max = max_experts,
                value = min(3L, max_experts),
                step = 1
              ),
              shiny::conditionalPanel(
                "input.research_strategy === 'conversation'",
                shiny::numericInput(
                  "max_rounds",
                  "Rounds",
                  min = 1,
                  max = 10,
                  value = 3,
                  step = 1
                )
              ),
              shiny::conditionalPanel(
                "input.research_strategy === 'key_questions'",
                shiny::numericInput(
                  "max_questions",
                  "Questions per expert",
                  min = 1,
                  max = 10,
                  value = 3,
                  step = 1
                )
              ),
              shiny::numericInput(
                "max_analyses",
                "Analysis steps",
                min = 0,
                max = 10,
                value = 3,
                step = 1
              ),
              shiny::numericInput(
                "max_illustrations",
                "Illustrations",
                min = 0,
                max = 10,
                value = 2,
                step = 1
              )
            ),
            shiny::div(
              class = "researcher-check-stack",
              shiny::checkboxInput(
                "allow_code",
                "Run generated R code locally",
                value = FALSE
              ),
              shiny::p(
                class = "researcher-help",
                "Code runs with your local account's permissions and access. It is not isolated in a sandbox."
              ),
              shiny::checkboxInput(
                "allow_images",
                "Allow generated illustrations",
                value = FALSE
              )
            )
          ),
          bslib::accordion_panel(
            "Tool access",
            value = "tools",
            shiny::p(
              class = "researcher-tools-note",
              paste(
                "Use host-provided R tools and MCP JSON configurations.",
                "Discovery only lists tools; select each tool to make it",
                "available under the research permissions."
              )
            ),
            shiny::checkboxGroupInput(
              "local_tools",
              "Host-provided R tools",
              choices = tool_choices
            ),
            shiny::fileInput(
              "mcp_file",
              "MCP configuration JSON",
              accept = c(".json", "application/json"),
              buttonLabel = "Choose JSON",
              placeholder = "No file selected"
            ),
            shiny::selectizeInput(
              "mcp_servers",
              "Configured MCP servers",
              choices = as.character(mcp_servers),
              multiple = TRUE,
              options = list(placeholder = "Choose configured servers")
            ),
            shiny::actionButton(
              "discover",
              "Discover tools",
              class = "btn-outline-primary"
            ),
            shiny::div(
              class = "researcher-tools-status",
              role = "status",
              `aria-live` = "polite",
              `aria-atomic` = "true",
              shiny::uiOutput("tools_status")
            ),
            shiny::uiOutput("mcp_tools")
          ),
          if (isTRUE(memory_enabled)) {
            tempest_researcher_memory_sidebar_ui(memory_streams)
          }
        ),
        shiny::div(
          class = "researcher-run-controls",
          shiny::actionButton(
            "run",
            "Start research",
            class = "btn-primary"
          ),
          shiny::actionButton(
            "cancel",
            "Cancel",
            class = "btn-outline-secondary"
          )
        )
      )
    ),
    shiny::tags$head(
      shiny::tags$style(htmltools::HTML(css))
    ),
    shiny::tags$main(
      class = "researcher-main",
      shiny::tags$header(
        class = "researcher-report-header",
        shiny::div(
          shiny::p(class = "researcher-eyebrow", "Research workspace"),
          shiny::h1("Research report"),
          shiny::p(
            class = "researcher-report-deck",
            "Review the evidence, computations, and methods in one report."
          )
        ),
        shiny::conditionalPanel(
          "output.report_ready === true",
          shiny::downloadButton(
            "download",
            "Download report bundle",
            class = "btn-outline-primary researcher-download",
            icon = NULL
          )
        )
      ),
      shiny::div(
        class = "researcher-live-status",
        role = "status",
        `aria-live` = "polite",
        `aria-atomic` = "true",
        shiny::uiOutput("status")
      ),
      shiny::div(
        class = "researcher-progress",
        role = "region",
        `aria-label` = "Research progress",
        shiny::uiOutput("progress")
      ),
      if (isTRUE(library_enabled)) {
        tempest_researcher_library_ui(
          "library",
          memory_enabled = memory_enabled
        )
      },
      if (isTRUE(memory_enabled)) {
        tempest_researcher_memory_review_ui("memory_review")
      },
      shiny::tags$section(
        class = "researcher-report-paper",
        `aria-label` = "Live research report",
        shiny::uiOutput("report", class = "researcher-report-copy")
      )
    )
  )
}
