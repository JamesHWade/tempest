test_that("the discussion landing surface keeps its research-first flow", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("shinychat")

  app <- tempest:::tempest_shiny_module_env()
  html <- paste(
    as.character(app$mod_chat_ui("chat", app$mod_config_ui("config"))),
    collapse = ""
  )

  expect_match(html, "tempest-chat-welcome-kicker", fixed = TRUE)
  expect_match(html, "Co-STORM discussion", fixed = TRUE)
  expect_match(html, "Start a research discussion", fixed = TRUE)
  expect_match(html, "cited evidence", fixed = TRUE)
  expect_match(html, "chat-topic", fixed = TRUE)
  expect_match(html, "chat-start", fixed = TRUE)
  expect_match(html, "chat-research_options", fixed = TRUE)
  expect_match(html, "tempest-chat-details", fixed = TRUE)
  expect_match(html, "chat-warmup", fixed = TRUE)
  expect_match(html, "chat-suggest", fixed = TRUE)
  expect_match(html, "checked=\\&quot;checked\\&quot;", fixed = TRUE)
  expect_match(html, "chat-setup_settings_toggle", fixed = TRUE)
  expect_match(html, "tempest-chat-start-feedback", fixed = TRUE)
  expect_match(html, "Start the Co-STORM discussion", fixed = TRUE)
  expect_no_match(html, "For a one-shot report, use", fixed = TRUE)
})

test_that("discussion settings stay namespaced and readable", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  app <- tempest:::tempest_shiny_module_env()
  html <- paste(as.character(app$mod_config_ui("config")), collapse = "")
  sidebar <- paste(
    as.character(app$chat_settings_sidebar_ui(
      shiny::NS("chat"),
      app$mod_config_ui("config")
    )),
    collapse = ""
  )

  expect_match(html, "tempest-config-accordion", fixed = TRUE)
  expect_match(html, "config-coordinator", fixed = TRUE)
  expect_match(html, "config-search_provider", fixed = TRUE)
  expect_match(html, "config-research_review", fixed = TRUE)
  expect_match(html, "Route each research role", fixed = TRUE)
  expect_match(
    html,
    "Choose where the expert panel looks for sources",
    fixed = TRUE
  )
  expect_match(html, "Findings are advisory", fixed = TRUE)
  expect_match(sidebar, "chat-session_settings", fixed = TRUE)
  expect_match(sidebar, "chat-save_session", fixed = TRUE)
  expect_match(sidebar, "chat-load_session", fixed = TRUE)
  expect_match(sidebar, "Session controls and defaults", fixed = TRUE)
})

test_that("active discussion status is announced without changing controls", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  app <- tempest:::tempest_shiny_module_env()
  workspace <- new.env(parent = emptyenv())
  workspace$list_retrieved_sources <- function() list()
  workspace$list_proposed_claims <- function() list()
  ses <- list2env(
    list(experts = list(list(name = "Dr. Discussion"))),
    parent = emptyenv()
  )
  class(ses) <- "TempestSession"
  test_session_private(ses, workspace = workspace)

  footer <- paste(
    as.character(app$chat_runtime_footer_ui(
      ses,
      chat_status = "streaming",
      ns = shiny::NS("chat")
    )),
    collapse = ""
  )

  expect_match(footer, "role=\"status\"", fixed = TRUE)
  expect_match(footer, "aria-live=\"polite\"", fixed = TRUE)
  expect_match(footer, "Answering", fixed = TRUE)
  expect_match(footer, "chat-footer_settings_toggle", fixed = TRUE)
  expect_match(footer, "chat-footer_new", fixed = TRUE)
  expect_match(footer, "chat-generate_report", fixed = TRUE)
  expect_match(footer, "chat-report_options", fixed = TRUE)
  expect_match(footer, "chat-report_style", fixed = TRUE)
  expect_match(footer, "tempest-chat-report-options-details", fixed = TRUE)
  expect_no_match(footer, "bslib-popover", fixed = TRUE)
  expect_match(footer, "Generate a report from the evidence", fixed = TRUE)
})

test_that("discussion stylesheet supports editorial contrast and keyboard paths", {
  css_path <- system.file("shiny", "www", "discussion.css", package = "tempest")
  if (!nzchar(css_path)) {
    css_path <- testthat::test_path(
      "../../inst/shiny/www/discussion.css"
    )
  }
  skip_if_not(file.exists(css_path), "discussion stylesheet not found")
  css <- paste(readLines(css_path, warn = FALSE), collapse = "\n")

  expect_match(css, "--tempest-ink", fixed = TRUE)
  expect_match(css, "--tempest-teal", fixed = TRUE)
  expect_match(css, "prefers-color-scheme: dark", fixed = TRUE)
  expect_match(css, "data-bs-theme=\"dark\"", fixed = TRUE)
  expect_match(css, "focus-visible", fixed = TRUE)
  expect_match(css, "prefers-reduced-motion: reduce", fixed = TRUE)
  expect_match(css, "max-width: 767.98px", fixed = TRUE)
})
