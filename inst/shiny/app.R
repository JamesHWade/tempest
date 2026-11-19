# tempest — STORM / Co-STORM research assistant.
#
# This entry point wires together the modules defined in R/ (auto-sourced by
# Shiny). The tempest namespace is loaded by tempest_app() (or devtools::load_all()
# in development), so tempest functions are reached with the tempest:: prefix.

library(shiny)
library(bslib)

panels <- tempest:::tempest_shiny_panel_choices()
tempest:::tempest_shiny_require_ui(panels)
tempest:::tempest_shiny_require_server(panels)

# Colors, typography, and logo are defined in _brand.yml (auto-discovered by
# bslib from the app directory).
theme <- bs_theme(version = 5, preset = "shiny", brand = TRUE)

ui <- page_navbar(
  title = span(
    class = "tempest-wordmark",
    tags$img(src = tempest_logo_src(), alt = "", width = 36, height = 36),
    "tempest"
  ),
  window_title = "Tempest research workspace",
  selected = "Chat",
  lang = "en",
  navbar_options = navbar_options(theme = "dark"),
  id = "nav",
  theme = theme,
  fillable = TRUE,
  header = tags$head(
    tags$link(rel = "icon", type = "image/svg+xml", href = tempest_logo_src()),
    tags$link(rel = "stylesheet", href = "app.css"),
    tags$link(rel = "stylesheet", href = "discussion.css"),
    tags$link(rel = "stylesheet", href = "evidence.css"),
    tags$link(rel = "stylesheet", href = "scripted.css")
  ),
  mod_chat_ui(
    "chat",
    config_ui = mod_config_ui("config"),
    allow_user_experts = TRUE
  ),
  mod_storm_ui("storm"),
  nav_menu(
    "Evidence",
    mod_sources_ui("sources"),
    mod_facts_ui("facts"),
    mod_mindmap_ui("mindmap")
  ),
  nav_menu(
    "Session record",
    mod_transcript_ui("transcript"),
    mod_run_review_ui("review")
  ),
  mod_report_ui("report"),
  nav_spacer(),
  about_nav_item(),
  nav_item(input_dark_mode(id = "dark_mode", mode = "light"))
)

server <- function(input, output, session) {
  store <- new_session_store()
  config <- mod_config_server("config")

  chat_handle <- mod_chat_server(
    "chat",
    config = config,
    store = store,
    allow_user_experts = TRUE
  )
  storm_handle <- mod_storm_server(
    "storm",
    config = config,
    store = store
  )
  costorm_events <- reactive({
    active <- store$costorm_session()
    if (is.null(active)) list() else tempest:::tempest_execution_events(active)
  })
  mod_run_review_server(
    "review",
    costorm_product = store$costorm_session,
    storm_product = storm_handle$last_successful_product,
    costorm_events = costorm_events,
    storm_events = storm_handle$storm_events
  )
  mod_mindmap_server("mindmap", store = store)
  mod_sources_server("sources", store = store)
  mod_facts_server("facts", store = store)
  mod_transcript_server("transcript", store = store)
  mod_report_server("report", store = store)

  # Modules emit monotonic navigation events only after report publication.
  show_report <- function() nav_select("nav", "Report")
  observeEvent(
    chat_handle$report_navigation_event(),
    show_report(),
    ignoreInit = TRUE
  )
  observeEvent(
    storm_handle$report_navigation_event(),
    show_report(),
    ignoreInit = TRUE
  )
}

shinyApp(ui, server)
