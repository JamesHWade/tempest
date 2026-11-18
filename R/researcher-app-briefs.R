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
