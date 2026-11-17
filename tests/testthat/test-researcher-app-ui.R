test_that("the researcher UI exposes its controls and live report outputs", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  old_options <- options(sass.cache = FALSE)
  on.exit(options(old_options), add = TRUE)

  ui <- tempest:::tempest_researcher_ui()
  rendered <- htmltools::renderTags(ui)
  html <- paste(rendered$head, rendered$html, collapse = "")

  expect_s3_class(ui, "bslib_page")
  for (id in c(
    "topic",
    "model",
    "research_strategy",
    "n_experts",
    "max_rounds",
    "max_questions",
    "allow_code",
    "allow_images",
    "max_analyses",
    "max_illustrations",
    "local_tools",
    "mcp_file",
    "mcp_servers",
    "discover",
    "mcp_tools",
    "run",
    "cancel",
    "status",
    "progress",
    "report",
    "download",
    "tools_status"
  )) {
    expect_match(html, paste0('id="', id, '"'), fixed = TRUE)
  }

  expect_match(html, 'id="n_experts"[^>]*value="3"', perl = TRUE)
  expect_match(html, 'id="max_rounds"[^>]*value="3"', perl = TRUE)
  expect_match(html, 'id="max_questions"[^>]*value="3"', perl = TRUE)
  expect_match(html, 'id="max_analyses"[^>]*value="3"', perl = TRUE)
  expect_match(html, 'id="max_illustrations"[^>]*value="2"', perl = TRUE)
  expect_match(html, 'id="n_experts"[^>]*min="1"[^>]*max="5"', perl = TRUE)
  for (id in c("max_rounds", "max_questions")) {
    expect_match(
      html,
      paste0('id="', id, '"[^>]*min="1"[^>]*max="10"'),
      perl = TRUE
    )
  }
  for (id in c("max_analyses", "max_illustrations")) {
    expect_match(
      html,
      paste0('id="', id, '"[^>]*min="0"[^>]*max="10"'),
      perl = TRUE
    )
  }
  expect_no_match(html, 'id="allow_code"[^>]*checked', perl = TRUE)
  expect_no_match(html, 'id="allow_images"[^>]*checked', perl = TRUE)
  expect_match(html, "Run generated R code locally", fixed = TRUE)
  expect_match(html, "local account's permissions", fixed = TRUE)
  expect_match(html, "not isolated in a sandbox", fixed = TRUE)
  expect_match(html, 'accept=".json,application/json"', fixed = TRUE)
  expect_match(html, 'multiple="multiple"', fixed = TRUE)
  expect_match(html, 'role="status"', fixed = TRUE)
  expect_match(html, 'aria-live="polite"', fixed = TRUE)
  expect_match(html, 'for="topic"', fixed = TRUE)
  expect_match(html, "Download report bundle", fixed = TRUE)
  expect_no_match(html, "fas fa-download", fixed = TRUE)
})

test_that("the expert control respects small and large configured budgets", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  withr::local_options(sass.cache = FALSE)

  for (budget in c(1L, 2L, 30L)) {
    rendered <- htmltools::renderTags(tempest_researcher_ui(
      max_experts = budget
    ))
    input <- xml2::xml_find_first(
      xml2::read_html(rendered$html),
      '//*[@id="n_experts"]'
    )
    expect_identical(
      xml2::xml_attr(input, "max"),
      as.character(min(10L, budget))
    )
    expect_identical(
      xml2::xml_attr(input, "value"),
      as.character(min(3L, budget))
    )
  }
})

test_that("tool catalog and server labels render as text", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  old_options <- options(sass.cache = FALSE)
  on.exit(options(old_options), add = TRUE)

  ui <- tempest:::tempest_researcher_ui(
    tool_catalog = data.frame(
      id = "host-tool",
      label = "<script>alert(1)</script>"
    ),
    mcp_servers = "<img src=x onerror=alert(2)>"
  )
  rendered <- htmltools::renderTags(ui)
  html <- paste(rendered$head, rendered$html, collapse = "")

  expect_match(html, "&lt;script&gt;alert(1)&lt;/script&gt;", fixed = TRUE)
  expect_no_match(html, "<script>alert(1)</script>", fixed = TRUE)
  expect_match(html, "&lt;img src=x onerror=alert(2)&gt;", fixed = TRUE)
  expect_no_match(html, "<img src=x onerror=alert(2)>", fixed = TRUE)
})

test_that("the researcher page includes responsive layout styling without web fonts", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  old_options <- options(sass.cache = FALSE)
  on.exit(options(old_options), add = TRUE)

  rendered <- htmltools::renderTags(tempest:::tempest_researcher_ui())
  html <- paste(rendered$head, rendered$html, collapse = "")

  expect_match(html, "@media (max-width: 767.98px)", fixed = TRUE)
  expect_match(html, "grid-template-columns: minmax(0, 1fr)", fixed = TRUE)
  expect_no_match(html, "fonts.googleapis.com", fixed = TRUE)
  expect_no_match(html, "use.typekit.net", fixed = TRUE)
})
