# Tests for the bundled Shiny app's modules. The app files live in
# inst/shiny/R and are sourced on demand.

source_shiny_modules <- function() {
  dir <- system.file("shiny", "R", package = "tempest")
  skip_if(identical(dir, ""), "Shiny app files not found")
  env <- new.env(parent = globalenv())
  for (f in sort(list.files(dir, pattern = "[.][Rr]$", full.names = TRUE))) {
    sys.source(f, envir = env)
  }
  env
}

test_that("every module UI builds a Shiny tag", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("shinychat")
  app <- source_shiny_modules()

  expect_s3_class(app$mod_config_ui("config"), "shiny.tag")
  expect_s3_class(
    app$mod_chat_ui("chat", app$mod_config_ui("config")),
    "shiny.tag"
  )
  expect_s3_class(app$mod_storm_ui("storm"), "shiny.tag")
  expect_s3_class(app$mod_mindmap_ui("mindmap"), "shiny.tag")
  expect_s3_class(app$mod_sources_ui("sources"), "shiny.tag")
  expect_s3_class(app$mod_facts_ui("facts"), "shiny.tag")
  expect_s3_class(app$mod_transcript_ui("transcript"), "shiny.tag")
  expect_s3_class(app$mod_report_ui("report"), "shiny.tag")
})

test_that("module UIs namespace their input ids", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  app <- source_shiny_modules()
  html <- as.character(app$mod_storm_ui("storm"))
  expect_match(paste(html, collapse = ""), "storm-run")
  expect_match(paste(html, collapse = ""), "storm-topic")
})

test_that("the About popover links the papers and upstream repos", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  app <- source_shiny_modules()
  html <- paste(as.character(app$about_nav_item()), collapse = "")
  expect_match(html, "arxiv.org/abs/2402.14207")
  expect_match(html, "arxiv.org/abs/2408.15232")
  expect_match(html, "arxiv.org/abs/2310.03714")
  expect_match(html, "github.com/stanford-oval/storm")
  expect_match(html, "github.com/stanfordnlp/dspy")
})

test_that("the chat module provides a landing welcome message", {
  app <- source_shiny_modules()
  msg <- app$welcome_message()
  expect_type(msg, "character")
  expect_length(msg, 1)
  expect_match(msg, "Welcome to tempest")
  expect_match(msg, "Start Session")
})

test_that("nav panels carry explicit string values for nav_select()", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  app <- source_shiny_modules()
  report_html <- paste(as.character(app$mod_report_ui("report")), collapse = "")
  expect_match(report_html, 'data-value="Report"')
})

test_that("the report module renders markdown and an empty state", {
  skip_if_not_installed("shiny")
  app <- source_shiny_modules()
  store <- app$new_session_store()

  shiny::testServer(app$mod_report_server, args = list(store = store), {
    expect_match(as.character(output$body$html), "Generate a report")
    store$set_report("# Findings\n\nSome text.", topic = "My Topic")
    session$flushReact()
    expect_match(as.character(output$body$html), "Findings")
  })
})

test_that("the transcript module shows recent turns from the store", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ellmer")
  app <- source_shiny_modules()
  store <- app$new_session_store()
  ses <- tempest_session(
    "Test topic",
    config = tempest_config(),
    personas = list(list(
      id = 1,
      name = "Dr. A",
      title = "Sci",
      perspective = "X"
    ))
  )

  shiny::testServer(app$mod_transcript_server, args = list(store = store), {
    expect_match(as.character(output$body$html), "Start a conversation")
    ses$add_turn("user", "user", "hello world")
    store$set(ses)
    session$flushReact()
    expect_match(as.character(output$body$html), "hello world")
  })
})

test_that("the mind map module counts nodes, sources, facts, and turns", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ellmer")
  app <- source_shiny_modules()
  store <- app$new_session_store()
  ses <- tempest_session(
    "Test topic",
    config = tempest_config(),
    personas = list(list(
      id = 1,
      name = "Dr. A",
      title = "Sci",
      perspective = "X"
    ))
  )
  ses$add_turn("user", "user", "q1")

  shiny::testServer(app$mod_mindmap_server, args = list(store = store), {
    store$set(ses)
    session$flushReact()
    expect_equal(output$n_turns, "1")
    expect_match(output$n_sources, "^[0-9]+$")
    expect_match(output$n_facts, "^[0-9]+$")
  })
})

test_that("suggestion_cards builds a shinychat submit-card list", {
  app <- source_shiny_modules()
  md <- app$suggestion_cards(c("What is X?", "How does Y work?"))
  expect_type(md, "character")
  expect_match(md, "You might ask")
  expect_match(md, '- <span class="suggestion submit">What is X\\?</span>')
  expect_match(md, '- <span class="suggestion submit">How does Y work\\?</span>')
})

test_that("suggestion_cards returns NULL for no questions", {
  app <- source_shiny_modules()
  expect_null(app$suggestion_cards(character()))
  expect_null(app$suggestion_cards(c("", NA_character_)))
})

test_that("suggestion_cards keeps only valid questions, in order", {
  app <- source_shiny_modules()
  md <- app$suggestion_cards(c("Keep this?", "", NA_character_, "And this"))
  n_items <- length(gregexpr('class="suggestion submit"', md, fixed = TRUE)[[1]])
  expect_equal(n_items, 2)
  expect_lt(
    regexpr("Keep this", md, fixed = TRUE),
    regexpr("And this", md, fixed = TRUE)
  )
})

test_that("suggestion_cards honors a custom lead", {
  app <- source_shiny_modules()
  md <- app$suggestion_cards("Q1", lead = "**Try asking:**")
  expect_match(md, "Try asking", fixed = TRUE)
})
