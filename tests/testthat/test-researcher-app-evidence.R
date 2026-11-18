test_that("the navigator summarizes a partial enriched report", {
  skip_if_not_installed("shiny")

  fixture <- test_promotion_fixture("storm")
  plan <- test_enriched_plan(fixture$resource@resource_id)
  plan$illustrations <- list(list(
    id = "overview",
    title = "Conceptual overview",
    prompt = "Draw a conceptual overview.",
    alt = "A conceptual overview.",
    caption = "Conceptual sequence."
  ))
  report <- tempest_enrich_report(
    fixture$research,
    file.path(withr::local_tempdir(), "partial"),
    plan = plan,
    allow_code = TRUE,
    executor = test_enriched_executor
  )

  html <- paste(
    htmltools::renderTags(tempest:::tempest_researcher_evidence_ui(
      report
    ))$html,
    collapse = ""
  )

  expect_match(html, "Evidence navigator", fixed = TRUE)
  expect_match(html, "Partial", fixed = TRUE)
  expect_match(html, "Analysis outcomes (1)", fixed = TRUE)
  expect_match(html, "Illustration outcomes (1)", fixed = TRUE)
  expect_match(html, "Completed", fixed = TRUE)
  expect_match(html, "Skipped", fixed = TRUE)
  expect_match(html, fixture$resource@title, fixed = TRUE)
  expect_match(html, fixture$resource@locator, fixed = TRUE)
  expect_length(
    xml2::xml_find_all(
      xml2::read_html(html),
      "//a[@target='_blank'][@rel='noopener noreferrer']"
    ),
    1L
  )
})

test_that("the navigator preserves failed outcomes and empty task labels", {
  skip_if_not_installed("shiny")

  fixture <- test_promotion_fixture("storm")
  plan <- test_enriched_plan(fixture$resource@resource_id)
  failed <- tempest_enrich_report(
    fixture$research,
    file.path(withr::local_tempdir(), "failed"),
    plan = plan,
    allow_code = TRUE,
    executor = function(...) stop("offline failure")
  )
  failed_html <- paste(
    htmltools::renderTags(
      tempest:::tempest_researcher_evidence_ui(failed)
    )$html,
    collapse = ""
  )
  expect_match(failed_html, "Failed", fixed = TRUE)

  empty_plan <- test_enriched_plan(
    fixture$resource@resource_id,
    analyses = FALSE
  )
  complete <- tempest_enrich_report(
    fixture$research,
    file.path(withr::local_tempdir(), "complete"),
    plan = empty_plan
  )
  complete_html <- paste(
    htmltools::renderTags(
      tempest:::tempest_researcher_evidence_ui(complete)
    )$html,
    collapse = ""
  )
  expect_match(complete_html, "Complete", fixed = TRUE)
  expect_match(
    complete_html,
    "No analyses recorded",
    fixed = TRUE
  )
  expect_match(
    complete_html,
    "No illustrations recorded",
    fixed = TRUE
  )
})
