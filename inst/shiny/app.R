# tempest — STORM / Co-STORM research assistant.
library(shiny)
library(bslib)

modules <- tempest:::tempest_shiny_module_env()
panels <- tempest:::tempest_shiny_panel_choices()
tempest:::tempest_shiny_require_ui(panels)
tempest:::tempest_shiny_require_server(panels)

# Colors, typography, and logo are defined in _brand.yml (auto-discovered by
# bslib from the app directory).
theme <- bs_theme(version = 5, preset = "shiny", brand = TRUE)

ui <- page_navbar(
  title = span(
    class = "tempest-wordmark",
    tags$img(
      src = modules$tempest_logo_src(),
      alt = "",
      width = 36,
      height = 36
    ),
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
    tags$link(
      rel = "icon",
      type = "image/svg+xml",
      href = modules$tempest_logo_src()
    ),
    tags$link(rel = "stylesheet", href = "app.css"),
    tags$link(rel = "stylesheet", href = "discussion.css"),
    tags$link(rel = "stylesheet", href = "evidence.css"),
    tags$link(rel = "stylesheet", href = "scripted.css")
  ),
  modules$mod_chat_ui(
    "chat",
    config_ui = modules$mod_config_ui("config"),
    allow_user_experts = TRUE
  ),
  modules$mod_storm_ui("storm"),
  nav_menu(
    "Evidence",
    modules$mod_sources_ui("sources"),
    modules$mod_facts_ui("facts"),
    modules$mod_mindmap_ui("mindmap")
  ),
  nav_menu(
    "Session record",
    modules$mod_transcript_ui("transcript"),
    modules$mod_run_review_ui("review")
  ),
  modules$mod_report_ui("report"),
  nav_spacer(),
  modules$about_nav_item(),
  nav_item(input_dark_mode(id = "dark_mode", mode = "light"))
)

server <- function(input, output, session) {
  store <- modules$new_session_store()
  config <- modules$mod_config_server("config")

  chat_handle <- modules$mod_chat_server(
    "chat",
    config = config,
    store = store,
    allow_user_experts = TRUE
  )
  storm_handle <- modules$mod_storm_server(
    "storm",
    config = config,
    store = store
  )
  costorm_events <- reactive({
    active <- store$costorm_session()
    if (is.null(active)) list() else tempest:::tempest_execution_events(active)
  })
  modules$mod_run_review_server(
    "review",
    costorm_product = store$costorm_session,
    storm_product = storm_handle$last_successful_product,
    costorm_events = costorm_events,
    storm_events = storm_handle$storm_events
  )
  modules$mod_mindmap_server("mindmap", store = store)
  modules$mod_sources_server("sources", store = store)
  modules$mod_facts_server("facts", store = store)
  modules$mod_transcript_server("transcript", store = store)
  modules$mod_report_server("report", store = store)

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
