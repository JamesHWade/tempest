test_that("Co-STORM evidence panels keep a readable native layout", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  app <- tempest:::tempest_shiny_module_env()

  sources <- paste(as.character(app$mod_sources_ui("sources")), collapse = "")
  facts <- paste(as.character(app$mod_facts_ui("facts")), collapse = "")
  transcript <- paste(
    as.character(app$mod_transcript_ui("transcript")),
    collapse = ""
  )
  report <- paste(as.character(app$mod_report_ui("report")), collapse = "")

  expect_match(sources, 'data-value="Sources"', fixed = TRUE)
  expect_match(sources, "tempest-source-browser", fixed = TRUE)
  expect_match(facts, 'data-value="Facts"', fixed = TRUE)
  expect_match(facts, "tempest-facts-browser", fixed = TRUE)
  expect_match(transcript, "tempest-transcript-card", fixed = TRUE)
  expect_match(report, 'data-value="Report"', fixed = TRUE)
  expect_match(report, "tempest-report-card", fixed = TRUE)
  expect_match(report, 'id="report-report_title"', fixed = TRUE)
  expect_match(
    report,
    'data-display-if="output.report_available"',
    fixed = TRUE
  )
  expect_match(report, 'data-ns-prefix="report-"', fixed = TRUE)
  expect_match(report, "Download report as Markdown", fixed = TRUE)
  expect_match(report, "Download report as HTML", fixed = TRUE)
  expect_equal(
    lengths(regmatches(report, gregexpr("fa-download", report, fixed = TRUE))),
    2L
  )
})

test_that("Co-STORM evidence panels explain their empty states", {
  skip_if_not_installed("shiny")

  app <- tempest:::tempest_shiny_module_env()

  empty_store <- list(
    costorm_workspace = shiny::reactiveVal(NULL),
    costorm_session = shiny::reactiveVal(NULL),
    report_md = shiny::reactiveVal(""),
    report_workspace = shiny::reactiveVal(NULL),
    report_topic = shiny::reactiveVal("")
  )

  shiny::testServer(
    app$mod_sources_server,
    args = list(store = empty_store),
    {
      session$flushReact()
      html <- as.character(output$body$html)
      expect_match(html, 'role="status"', fixed = TRUE)
      expect_match(html, "No active session", fixed = TRUE)
      expect_match(html, "Start a session to collect sources.", fixed = TRUE)
    }
  )

  shiny::testServer(
    app$mod_facts_server,
    args = list(store = empty_store),
    {
      session$flushReact()
      html <- as.character(output$body$html)
      expect_match(html, 'role="status"', fixed = TRUE)
      expect_match(html, "No active session", fixed = TRUE)
      expect_match(html, "Start a session to collect facts.", fixed = TRUE)
    }
  )

  shiny::testServer(
    app$mod_transcript_server,
    args = list(store = empty_store),
    {
      session$flushReact()
      html <- as.character(output$body$html)
      expect_match(html, "Transcript is waiting", fixed = TRUE)
      expect_match(
        html,
        "Start a conversation to see the transcript.",
        fixed = TRUE
      )
    }
  )

  shiny::testServer(
    app$mod_report_server,
    args = list(store = empty_store),
    {
      session$flushReact()
      html <- as.character(output$body$html)
      expect_match(html, "Your report will appear here", fixed = TRUE)
      expect_match(html, "Generate a report to view it here.", fixed = TRUE)
      expect_identical(
        shiny::isolate(session$returned$report_available()),
        FALSE
      )
    }
  )
})

test_that("Co-STORM evidence panels foreground populated content", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ellmer")

  app <- tempest:::tempest_shiny_module_env()

  workspace <- fake_store_with_sources(1)
  source <- workspace$list_retrieved_sources()[[1]]
  workspace$add_proposed_claim(tempest_claim(
    claim_text = "The source supports the working finding.",
    source_ids = source$id,
    confidence = "high",
    verification_status = "supported",
    support_score = 0.9
  ))
  source_id <- source$id
  session <- list(
    transcript = list(
      list(
        role = "user",
        speaker = "Researcher",
        text = "What does the evidence show?",
        at = "2026-01-01T00:00:00Z"
      ),
      list(
        role = "assistant",
        speaker = "Moderator",
        text = paste0("The evidence supports the finding [", source_id, "]."),
        at = "2026-01-01T00:01:00Z"
      )
    )
  )
  report_md <- paste0(
    "# Evidence brief\n\n",
    "The source supports the working finding [",
    source_id,
    "]."
  )
  store <- list(
    costorm_workspace = shiny::reactiveVal(workspace),
    costorm_session = shiny::reactiveVal(session),
    report_md = shiny::reactiveVal(report_md),
    report_workspace = shiny::reactiveVal(workspace),
    report_topic = shiny::reactiveVal("Evidence topic")
  )

  shiny::testServer(
    app$mod_sources_server,
    args = list(store = store),
    {
      session$flushReact()
      html <- as.character(output$body$html)
      expect_match(html, 'aria-label="Collected sources"', fixed = TRUE)
      if (requireNamespace("DT", quietly = TRUE)) {
        expect_match(html, "Search the table to filter", fixed = TRUE)
        expect_match(
          as.character(output$table),
          "Search sources:",
          fixed = TRUE
        )
      }
    }
  )

  shiny::testServer(
    app$mod_facts_server,
    args = list(store = store),
    {
      session$flushReact()
      html <- as.character(output$body$html)
      expect_match(html, 'aria-label="Extracted evidence claims"', fixed = TRUE)
      if (requireNamespace("DT", quietly = TRUE)) {
        expect_match(html, "Search the table to filter", fixed = TRUE)
        expect_match(as.character(output$table), "Search facts:", fixed = TRUE)
      }
    }
  )

  shiny::testServer(
    app$mod_transcript_server,
    args = list(store = store),
    {
      session$flushReact()
      html <- as.character(output$body$html)
      expect_match(html, "What does the evidence show?", fixed = TRUE)
      expect_match(html, "The evidence supports the finding", fixed = TRUE)
      expect_match(html, "tempest-transcript-turn", fixed = TRUE)
    }
  )

  shiny::testServer(
    app$mod_report_server,
    args = list(store = store),
    {
      session$flushReact()
      html <- as.character(output$body$html)
      expect_match(html, "tempest-report-reader", fixed = TRUE)
      expect_match(html, "tempest-report-prose", fixed = TRUE)
      expect_match(html, "Evidence brief", fixed = TRUE)
      expect_match(html, "1 source · 1 fact retained", fixed = TRUE)
      expect_match(html, "tempest-reference-panel", fixed = TRUE)
      expect_identical(
        shiny::isolate(session$returned$report_available()),
        TRUE
      )

      bad_workspace <- list(
        to_tibbles = function() stop("evidence read failed", call. = FALSE)
      )
      store$report_workspace(bad_workspace)
      session$flushReact()
      html <- as.character(output$body$html)
      expect_match(html, "sources unavailable", fixed = TRUE)
      expect_match(html, "facts unavailable", fixed = TRUE)
      expect_no_match(html, "0 sources", fixed = TRUE)
      expect_no_match(html, "0 facts", fixed = TRUE)
    }
  )
})

test_that("citation cleanup preserves execution review prose", {
  app <- tempest:::tempest_shiny_module_env()
  workspace <- fake_store_with_sources(1)
  source_id <- workspace$list_retrieved_sources()[[1]]$id
  markdown <- paste0(
    "# Report\n\nFinding [^",
    source_id,
    "].\n\n",
    "## References\n\n",
    "Keep this note from the report.\n\n",
    "[^",
    source_id,
    "]: Old footnote.\n\n",
    "## Execution review\n\n",
    "The execution review remains visible.\n"
  )
  rendered <- app$citation_markdown(
    markdown,
    workspace = workspace,
    include_references = TRUE
  )

  expect_equal(
    lengths(regmatches(
      rendered,
      gregexpr("tempest-reference-heading", rendered, fixed = TRUE)
    )),
    1L
  )
  expect_match(rendered, "Keep this note from the report.", fixed = TRUE)
  expect_match(rendered, "## Execution review", fixed = TRUE)
  expect_match(rendered, "The execution review remains visible.", fixed = TRUE)
  example <- paste("~~~markdown", "## References", "~~~", sep = "\n")
  with_code <- paste(example, markdown, sep = "\n")
  expect_match(
    app$citation_strip_tempest_footnotes(with_code),
    example,
    fixed = TRUE
  )
  expect_no_match(rendered, "Old footnote.", fixed = TRUE)
})

test_that("the evidence stylesheet uses theme variables and responsive reading rules", {
  css_path <- system.file("shiny", "www", "evidence.css", package = "tempest")
  skip_if(identical(css_path, ""), "Evidence stylesheet not installed")
  css <- paste(readLines(css_path, warn = FALSE), collapse = "\n")

  expect_match(css, "--bs-body-color", fixed = TRUE)
  expect_match(css, "--bs-primary", fixed = TRUE)
  expect_match(css, "tempest-report-prose", fixed = TRUE)
  expect_match(css, "prefers-reduced-motion", fixed = TRUE)
  expect_match(css, "max-width: 767.98px", fixed = TRUE)
})
