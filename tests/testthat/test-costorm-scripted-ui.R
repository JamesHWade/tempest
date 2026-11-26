test_that("scripted STORM UI makes startup controls and progress legible", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  env <- tempest:::tempest_shiny_module_env()

  html <- paste(
    as.character(env$mod_storm_ui("storm")),
    collapse = ""
  )

  expect_match(html, "tempest-scripted-layout", fixed = TRUE)
  expect_match(html, "tempest-scripted-settings-card", fixed = TRUE)
  expect_match(html, "Run configuration", fixed = TRUE)
  expect_match(html, "storm-topic", fixed = TRUE)
  expect_match(html, "storm-n_experts", fixed = TRUE)
  expect_match(html, "storm-strategy", fixed = TRUE)
  expect_match(html, "storm-max_rounds", fixed = TRUE)
  expect_match(html, "storm-run", fixed = TRUE)
  expect_match(html, "STORM pipeline progress", fixed = TRUE)
  expect_no_match(html, "Background run", fixed = TRUE)
  expect_match(html, "storm-cancel_control", fixed = TRUE)
  expect_match(html, 'role="status"', fixed = TRUE)
  expect_match(html, 'aria-live="polite"', fixed = TRUE)
  expect_match(html, 'aria-atomic="true"', fixed = TRUE)
  expect_no_match(html, "parallel", fixed = TRUE)
})

test_that("run review UI distinguishes recorded and live evidence", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  env <- tempest:::tempest_shiny_module_env()

  html <- paste(
    as.character(env$mod_run_review_ui("review")),
    collapse = ""
  )

  expect_match(html, "tempest-review-layout", fixed = TRUE)
  expect_match(html, "review-product_source", fixed = TRUE)
  expect_match(html, "review-stage_filter", fixed = TRUE)
  expect_match(html, "review-status_filter", fixed = TRUE)
  expect_match(html, "review-attention_only", fixed = TRUE)
  expect_match(html, "review-stage_detail", fixed = TRUE)
  expect_match(html, "review-stage_detail_card", fixed = TRUE)
  expect_match(html, "Authoritative stage records", fixed = TRUE)
  expect_match(html, "Unlinked untimed Deputy references", fixed = TRUE)
  expect_match(html, "Live progress observations (untrusted)", fixed = TRUE)
  expect_match(html, "Evidence boundary", fixed = TRUE)
  expect_match(
    html,
    'aria-labelledby="review-stage_detail_heading"',
    fixed = TRUE
  )
  expect_match(html, 'aria-live="polite"', fixed = TRUE)
})

test_that("scripted empty states and live observations keep their hierarchy", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  env <- tempest:::tempest_shiny_module_env()

  storm_empty <- paste(
    as.character(env$storm_progress_empty_state_ui()),
    collapse = ""
  )
  review_empty <- paste(
    as.character(env$run_review_empty_state_ui(
      "route",
      "No run selected",
      "Complete a STORM run or Co-STORM session to review its trajectory."
    )),
    collapse = ""
  )
  live <- paste(
    as.character(env$run_review_progress_ui(
      list(list(
        event_id = "event-1",
        run_id = "run-safe",
        workflow = "storm",
        event_type = "stage",
        stage = "outline",
        step = "report_md",
        status = "running",
        timestamp = "2026-08-19T12:03:00Z"
      )),
      "run-safe"
    )),
    collapse = ""
  )

  expect_match(storm_empty, "tempest-scripted-empty-state", fixed = TRUE)
  expect_match(storm_empty, "Ready to run", fixed = TRUE)
  expect_match(
    storm_empty,
    "Enter a research topic, then select Run STORM.",
    fixed = TRUE
  )
  expect_match(review_empty, "No run selected", fixed = TRUE)
  expect_match(
    review_empty,
    "Complete a STORM run or Co-STORM session to review its trajectory.",
    fixed = TRUE
  )
  expect_match(live, "tempest-review-live-boundary", fixed = TRUE)
  expect_match(live, "untrusted observation", fixed = TRUE)
  expect_match(live, "Same run id", fixed = TRUE)
})

test_that("scripted stylesheet uses the app theme and responsive breakpoints", {
  css_path <- system.file("shiny", "www", "scripted.css", package = "tempest")
  expect_identical(file.exists(css_path), TRUE)
  css <- paste(readLines(css_path, warn = FALSE), collapse = "\n")

  expect_match(css, "--bs-body-bg", fixed = TRUE)
  expect_match(css, "--bs-border-color", fixed = TRUE)
  expect_match(css, "color-mix", fixed = TRUE)
  expect_match(css, "@media (max-width: 575.98px)", fixed = TRUE)
})
