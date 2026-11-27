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

  css <- tempest_read_text(tempest_pkg_file("researcher", "researcher.css"))

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
