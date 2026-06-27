# tempest — STORM / Co-STORM research assistant.
#
# This entry point wires together the modules defined in R/ (auto-sourced by
# Shiny). The tempest namespace is loaded by run_app() (or devtools::load_all()
# in development), so tempest functions are reached with the tempest:: prefix.

library(shiny)
library(bslib)

theme <- bs_theme(
  version = 5,
  preset = "shiny",
  primary = "#4A90A4",
  secondary = "#6C757D",
  success = "#5CB85C",
  info = "#5BC0DE",
  warning = "#F0AD4E",
  danger = "#D9534F"
)

ui <- page_navbar(
  title = tagList(icon("cloud-bolt"), "tempest"),
  id = "nav",
  theme = theme,
  fillable = TRUE,
  nav_spacer(),
  about_nav_item(),
  nav_item(input_dark_mode(id = "dark_mode", mode = "light")),
  mod_chat_ui("chat", config_ui = mod_config_ui("config")),
  mod_storm_ui("storm"),
  mod_mindmap_ui("mindmap"),
  mod_sources_ui("sources"),
  mod_facts_ui("facts"),
  mod_transcript_ui("transcript"),
  mod_report_ui("report")
)

server <- function(input, output, session) {
  rlang::check_installed(
    c("promises", "shinychat", "ellmer", "mirai"),
    reason = "to run the tempest app."
  )
  if (!"chat_server" %in% getNamespaceExports("shinychat")) {
    rlang::abort(
      "The tempest Shiny app requires the development version of shinychat with chat_server()."
    )
  }

  store <- new_session_store()
  config <- mod_config_server("config")

  chat_report_ready <- mod_chat_server("chat", config = config, store = store)
  storm_report_ready <- mod_storm_server(
    "storm",
    config = config,
    store = store
  )
  mod_mindmap_server("mindmap", store = store)
  mod_sources_server("sources", store = store)
  mod_facts_server("facts", store = store)
  mod_transcript_server("transcript", store = store)
  mod_report_server("report", store = store)

  # Modules signal when a report is ready; navigation lives here, in the parent.
  show_report <- function() nav_select("nav", "Report")
  observeEvent(chat_report_ready(), show_report(), ignoreInit = TRUE)
  observeEvent(storm_report_ready(), show_report(), ignoreInit = TRUE)
}

shinyApp(ui, server)
