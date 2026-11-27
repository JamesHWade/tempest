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

tempest_researcher_starter_briefs <- function() {
  list(
    evidence_comparison = list(
      title = "Evidence comparison",
      description = "Compare published findings on heat exposure in Detroit.",
      question = "What does the best available evidence say about the health effects of urban heat in Detroit, and where do sources agree or disagree?",
      outputs = c(
        "A comparison matrix with 4-6 claims, source links, and evidence strength.",
        "A synthesis of agreement, disagreement, and the reasons for each.",
        "A decision-relevant conclusion with open questions and uncertainty."
      )
    ),
    public_data_analysis = list(
      title = "Public-data analysis",
      description = "Explore ozone measurements in New York with R.",
      question = "In R's airquality dataset, how did measured ozone vary by month in New York from May through September 1973?",
      outputs = c(
        "Trace the measurement units and sampling locations to the dataset documentation.",
        "Use reproducible R code to plot monthly distributions and tabulate the observed and missing days.",
        "Explain uncertainty and distinguish descriptive patterns from causal claims."
      )
    ),
    visual_explainer = list(
      title = "Visual explainer",
      description = "Illustrate how a solar eclipse casts its shadow.",
      question = "Why can one town see a total solar eclipse while a nearby town sees only a partial eclipse?",
      outputs = c(
        "Explain the umbra and penumbra in language a first-time eclipse observer can follow.",
        "Diagram the Sun, Moon, Earth, and two observers. Label distances and sizes that are not to scale.",
        "Include alt text, reading order, source notes, and a boundary between fact and illustration."
      )
    )
  )
}

tempest_researcher_briefs_ui <- function(id = "briefs", topic_id = "topic") {
  ns <- shiny::NS(id)
  briefs <- tempest_researcher_starter_briefs()
  topic_id <- as.character(topic_id)[[1L]]

  actions <- lapply(briefs, function(brief) {
    draft <- paste(
      brief$question,
      "\n\nRequested outputs\n",
      paste0("- ", brief$outputs, collapse = "\n"),
      sep = ""
    )
    shiny::tags$li(
      class = "researcher-brief-action",
      shiny::tags$div(
        class = "researcher-brief-action-copy",
        shiny::tags$strong(brief$title),
        shiny::tags$span(brief$description)
      ),
      shiny::tags$button(
        type = "button",
        class = "btn btn-outline-primary researcher-brief-select",
        `data-researcher-brief-trigger` = "true",
        `data-brief-draft` = draft,
        `data-brief-title` = brief$title,
        `aria-label` = paste("Use the", brief$title, "starter"),
        "Use this brief"
      )
    )
  })

  script <- paste0(
    "(function() {\n",
    "  function wireResearcherBriefs() {\n",
    "    var panel = document.getElementById('",
    ns("panel"),
    "');\n",
    "    if (!panel || panel.dataset.researcherBriefsWired === 'true') {\n",
    "      return;\n",
    "    }\n",
    "    panel.dataset.researcherBriefsWired = 'true';\n",
    "    var topic = document.getElementById('",
    topic_id,
    "');\n",
    "    var status = document.getElementById('",
    ns("status"),
    "');\n",
    "    panel.addEventListener('click', function(event) {\n",
    "      var button = event.target.closest('[data-researcher-brief-trigger]');\n",
    "      if (!button || !panel.contains(button) || !topic) {\n",
    "        return;\n",
    "      }\n",
    "      var draft = button.getAttribute('data-brief-draft') || '';\n",
    "      var title = button.getAttribute('data-brief-title') || 'starter brief';\n",
    "      if (topic.value.trim() && !window.confirm(\n",
    "        'Replace the current research question with this starter brief?'\n",
    "      )) {\n",
    "        return;\n",
    "      }\n",
    "      topic.value = draft;\n",
    "      topic.dispatchEvent(new Event('input', { bubbles: true }));\n",
    "      topic.dispatchEvent(new Event('change', { bubbles: true }));\n",
    "      if (status) {\n",
    "        status.textContent = 'Drafted the ' + title + ' starter. Edit it, then choose Start research when ready.';\n",
    "      }\n",
    "      topic.focus();\n",
    "    });\n",
    "  }\n",
    "  if (document.readyState === 'loading') {\n",
    "    document.addEventListener('DOMContentLoaded', wireResearcherBriefs);\n",
    "  } else {\n",
    "    wireResearcherBriefs();\n",
    "  }\n",
    "})();"
  )

  shiny::tags$section(
    id = ns("panel"),
    class = "researcher-briefs",
    `aria-labelledby` = ns("heading"),
    shiny::tags$details(
      class = "researcher-brief-disclosure",
      shiny::tags$summary(
        id = ns("heading"),
        "Choose a starter brief"
      ),
      shiny::tags$p(
        class = "researcher-help",
        "Each starter drafts a concrete question and the outputs it should produce. Edit the draft before anything runs."
      ),
      shiny::tags$ul(
        class = "researcher-brief-actions",
        actions
      )
    ),
    shiny::tags$p(
      id = ns("status"),
      class = "researcher-brief-status",
      role = "status",
      `aria-live` = "polite",
      `aria-atomic` = "true"
    ),
    shiny::tags$script(htmltools::HTML(script))
  )
}

tempest_researcher_evidence_ui <- function(report) {
  bundle <- jsonlite::read_json(file.path(report@path, "report.json"))
  tags <- htmltools::tags
  sources <- tempest_enriched_render_items(bundle$sources)
  source_item <- function(source) {
    title <- tempest_enriched_render_text(source$title)
    if (!nzchar(title)) {
      title <- tempest_enriched_render_text(source$id, "Source")
    }
    url <- tempest_enriched_render_safe_url(source$url) %||%
      tempest_enriched_render_safe_url(source$locator)
    tags$li(
      if (is.null(url)) {
        title
      } else {
        tags$a(
          title,
          href = url,
          target = "_blank",
          rel = "noopener noreferrer"
        )
      }
    )
  }
  outcomes <- function(tasks, label, empty) {
    tasks <- tempest_enriched_render_items(tasks)
    tags$details(
      tags$summary(sprintf("%s (%d)", label, length(tasks))),
      if (!length(tasks)) {
        tags$p(empty)
      } else {
        tags$ul(lapply(tasks, function(task) {
          status <- switch(
            tempest_enriched_render_text(task$status),
            succeeded = "Completed",
            skipped = "Skipped",
            failed = "Failed",
            "Status unavailable"
          )
          tags$li(tags$strong(status), " \u00b7 ", task$title %||% task$id)
        }))
      }
    )
  }
  tags$section(
    class = "researcher-evidence-navigator",
    `aria-label` = "Evidence navigator",
    tags$h2(tempest_enriched_render_text(bundle$title, "Saved report")),
    tags$p(
      if (identical(report@status, "partial")) {
        "Partial report"
      } else {
        "Complete report"
      },
      " \u00b7 ",
      length(sources),
      if (length(sources) == 1L) " recorded source" else " recorded sources"
    ),
    tags$details(
      tags$summary(sprintf("Recorded sources (%d)", length(sources))),
      if (length(sources)) {
        tags$ol(lapply(sources, source_item))
      } else {
        tags$p("No sources recorded.")
      }
    ),
    outcomes(bundle$analyses, "Analysis outcomes", "No analyses recorded."),
    outcomes(
      bundle$illustrations,
      "Illustration outcomes",
      "No illustrations recorded."
    )
  )
}

tempest_researcher_example_ui <- function() {
  # Plant-level summaries from the published example's output-1.csv.
  bars <- function(region, means) {
    shiny::div(
      class = "tempest-example-group",
      shiny::tags$strong(region),
      lapply(seq_along(means), function(i) {
        shiny::div(
          class = "tempest-example-row",
          shiny::span(c("Nonchilled", "Chilled")[[i]]),
          shiny::div(
            class = "tempest-example-track",
            shiny::span(
              class = c("control", "chilled")[[i]],
              style = paste0("width:", 100 * means[[i]] / 40, "%")
            )
          ),
          shiny::span(sprintf("%.2f", means[[i]]))
        )
      })
    )
  }

  shiny::tags$article(
    class = "tempest-example",
    shiny::tags$style(htmltools::HTML(
      "
      .tempest-example {max-width:48rem;margin:auto;}
      .tempest-example .sample-label {font:600 .84rem var(--researcher-sans);color:#376b62;}
      .tempest-example figure {margin:1.5rem 0;background:#f3f7f4;padding:clamp(.85rem,3vw,1.6rem);border-radius:.6rem;}
      .tempest-example figcaption,.tempest-example-group {font-family:var(--researcher-sans);}
      .tempest-example figcaption {font-size:.88rem;}
      .tempest-example-group {margin-top:1rem;font-size:.85rem;}
      .tempest-example-row {display:grid;grid-template-columns:5.2rem minmax(0,1fr) 3rem;align-items:center;gap:.5rem;font-variant-numeric:tabular-nums;}
      .tempest-example-row span:last-child {text-align:right;}
      .tempest-example-track span {display:block;height:.65rem;background:#347c71;border-radius:0 .18rem .18rem 0;}
      .tempest-example-track .chilled {background:#556998;}
    "
    )),
    shiny::p(class = "sample-label", "Example report"),
    shiny::h2("Chilling and the uptake gap"),
    shiny::p(paste(
      "After chilling and recovery, the difference in average carbon uptake",
      "was larger among the Mississippi plants than among the Quebec plants."
    )),
    shiny::tags$figure(
      shiny::tags$figcaption(
        "Mean carbon uptake across seven concentrations (\u00b5mol CO\u2082/m\u00b2/s)"
      ),
      bars("Mississippi", c(25.95, 15.81)),
      bars("Quebec", c(35.33, 31.75))
    ),
    shiny::p(
      "Each group contained three plants. The full report shows the individual measurements, R analysis, and limits of the comparison."
    ),
    shiny::tags$a(
      href = "https://jameshwade.github.io/tempest/live-research/index.html",
      target = "_blank",
      rel = "noopener noreferrer",
      "Read the worked example"
    )
  )
}
