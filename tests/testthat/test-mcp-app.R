test_that("research cards expose exact evidence through read-only model and page tools", {
  skip_if_not_installed("shinymcp")
  skip_if_not_installed("shiny")
  research <- ecosystem_demo_product()
  card <- tempest_mcp_app(research)
  expect_r6_class(card, "McpApp")
  model_tools <- card$tools("model")
  expect_setequal(
    names(model_tools),
    c("review_tempest_research", "inspect_tempest_claim", "read_tempest_report")
  )
  expect_identical(
    vapply(
      model_tools,
      function(tool) tool$annotations$readOnlyHint,
      logical(1)
    ),
    stats::setNames(rep(TRUE, 3L), names(model_tools))
  )
  expect_identical(
    vapply(
      model_tools,
      function(tool) tool$annotations$openWorldHint,
      logical(1)
    ),
    stats::setNames(rep(FALSE, 3L), names(model_tools))
  )

  overview <- card$run_tool("review_tempest_research")
  expect_identical(
    overview$structuredContent$review_id,
    tempest_trajectory_review_data(tempest_trajectory_review(
      research
    ))$review_id
  )
  expect_identical(
    overview$structuredContent$claims,
    nrow(tempest_claims(research))
  )
  expect_null(overview$structuredContent$report_md)
  expect_null(overview$structuredContent[["report"]])
  expect_match(
    as.character(jsonlite::toJSON(overview$`_meta`)),
    "Synthetic polymer recovery pilot",
    fixed = TRUE
  )
  expect_identical(
    card$run_tool("read_tempest_report")$structuredContent$report_md,
    tempest_report(research)
  )

  claim_id <- tempest_claims(research)$claim_id[[1L]]
  model <- card$run_tool("inspect_tempest_claim", list(claim_id = claim_id))
  page <- card$run_tool(
    "inspect_tempest_claim",
    list(claim_id = claim_id),
    context = list(caller = "app")
  )
  expect_identical(model$structuredContent, page$structuredContent)
  expected <- tempest_claim_supports(research)
  evidence <- model$structuredContent$evidence
  expect_identical(evidence$quote, expected$quote)
  expect_identical(evidence$claim_support_id, expected$claim_support_id)
  expect_identical(evidence$source_locator, tempest_sources(research)$locator)
  expect_match(
    as.character(jsonlite::toJSON(model$`_meta`)),
    expected$quote[[1L]],
    fixed = TRUE
  )
  expect_identical(card$run_tool("review_tempest_research"), overview)
})

test_that("unknown claim IDs fail on both entry paths", {
  skip_if_not_installed("shinymcp")
  skip_if_not_installed("shiny")
  card <- tempest_mcp_app(ecosystem_demo_product())
  for (caller in c("model", "app")) {
    expect_error(
      card$call_tool(
        "inspect_tempest_claim",
        list(claim_id = "outside-card"),
        context = list(caller = caller)
      ),
      class = "tempest_mcp_claim_error"
    )
    error <- card$run_tool(
      "inspect_tempest_claim",
      list(claim_id = "outside-card"),
      context = list(caller = caller)
    )
    expect_identical(error$isError, TRUE)
  }
})

test_that("research card HTML suppresses active content and unsafe links", {
  html <- tempest_mcp_report_html(paste(
    "<script>alert('source')</script>",
    "[bad](javascript:alert) [data](data:text/html,script)",
    "[source](https://example.org/source) ![image](https://example.org/tracker.png)",
    sep = "\n\n"
  ))
  document <- xml2::read_html(html)
  expect_length(xml2::xml_find_all(document, "//script|//img"), 0L)
  expect_identical(
    xml2::xml_attr(xml2::xml_find_all(document, "//a[@href]"), "href"),
    "https://example.org/source"
  )
  expect_match(html, "&lt;script&gt;", fixed = TRUE)
  expect_identical(
    grepl(
      "tempest-briefing-item:",
      tempest_mcp_report_html(
        "A statement. <!-- tempest-briefing-item:0123456789abcdef -->"
      ),
      fixed = TRUE
    ),
    FALSE
  )
})
