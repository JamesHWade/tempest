test_that("selecting a starter supplies an editable question and its deliverables", {
  skip_if_not_installed("shiny")
  ui <- htmltools::renderTags(tempest_researcher_briefs_ui())
  page <- xml2::read_html(ui$html)
  choices <- xml2::xml_find_all(page, "//button[@data-brief-draft]")
  expect_length(choices, 3L)
  drafts <- xml2::xml_attr(choices, "data-brief-draft")
  briefs <- tempest_researcher_starter_briefs()
  for (i in seq_along(drafts)) {
    expect_match(drafts[i], briefs[[i]]$question, fixed = TRUE)
    for (requirement in briefs[[i]]$outputs) {
      expect_match(drafts[i], requirement, fixed = TRUE)
    }
  }
})

test_that("the saved report library is opt-in and appears before the report", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  withr::local_options(sass.cache = FALSE)

  default_html <- htmltools::renderTags(tempest:::tempest_researcher_ui())
  default_html <- paste(
    default_html$head,
    default_html$html,
    collapse = ""
  )
  expect_no_match(default_html, "Saved research reports", fixed = TRUE)

  enabled <- htmltools::renderTags(
    tempest:::tempest_researcher_ui(library_enabled = TRUE)
  )
  enabled_html <- paste(enabled$head, enabled$html, collapse = "")
  expect_match(enabled_html, 'id="library-search"', fixed = TRUE)
  expect_match(enabled_html, 'id="library-refresh"', fixed = TRUE)
  expect_match(enabled_html, 'id="library-status"', fixed = TRUE)
  expect_lt(
    regexpr('id="library-search"', enabled_html, fixed = TRUE)[[1L]],
    regexpr('id="report"', enabled_html, fixed = TRUE)[[1L]]
  )
})
