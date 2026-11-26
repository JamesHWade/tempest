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
memory_ctx <- modules$shiny_memory_context()
memory_streams <- if (is.null(memory_ctx)) {
  character()
} else {
  tryCatch(
    tempest:::tempest_graft_memory_accepted_streams(
      memory_ctx$store,
      purpose = memory_ctx$purpose,
      eligible = memory_ctx$eligible
    )$stream,
    error = function(error) character()
  )
}
memory_config_ui <- if (is.null(memory_ctx)) {
  NULL
} else {
  modules$mod_memory_config_ui("memory_config", streams = memory_streams)
}
memory_report_ui <- if (is.null(memory_ctx)) {
  NULL
} else {
  modules$mod_memory_report_ui("memory_report")
}

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
    config_ui = modules$mod_config_ui("config", memory_ui = memory_config_ui),
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
  modules$mod_report_ui("report", memory_ui = memory_report_ui),
  nav_spacer(),
  modules$about_nav_item(),
  nav_item(input_dark_mode(id = "dark_mode", mode = "light"))
)

server <- function(input, output, session) {
  store <- modules$new_session_store()
  base_config <- modules$mod_config_server("config")
  memory_revision <- reactiveVal(0L)
  memory_config <- if (is.null(memory_ctx)) {
    NULL
  } else {
    modules$mod_memory_config_server(
      "memory_config",
      memory_ctx = memory_ctx,
      memory_revision = memory_revision
    )
  }
  config <- reactive({
    cfg <- base_config()
    if (is.null(memory_config)) {
      return(cfg)
    }
    modules$shiny_memory_wrap_config(
      cfg,
      memory_ctx = memory_ctx,
      use_tools = memory_config$use_tools()
    )
  })

  chat_handle <- modules$mod_chat_server(
    "chat",
    config = config,
    store = store,
    knowledge = if (is.null(memory_config)) NULL else memory_config$knowledge,
    allow_user_experts = TRUE
  )
  storm_handle <- modules$mod_storm_server(
    "storm",
    config = config,
    store = store
  )
  active_report_product <- reactive({
    modules$shiny_active_report_product(
      report_md = store$report_md(),
      report_workspace = store$report_workspace(),
      costorm_session = store$costorm_session(),
      storm_product = storm_handle$last_successful_product()
    )
  })
  if (!is.null(memory_ctx)) {
    modules$mod_memory_report_server(
      "memory_report",
      memory_ctx = memory_ctx,
      product = active_report_product,
      memory_revision = memory_revision
    )
  }
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
