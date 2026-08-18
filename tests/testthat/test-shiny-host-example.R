test_that("example host uses only the scripted STORM product API", {
  app_path <- test_path(
    "..",
    "..",
    "inst",
    "examples",
    "shiny-host",
    "app.R"
  )
  symbols <- all.names(
    parse(app_path),
    functions = TRUE,
    unique = TRUE
  )
  generic <- grep(
    paste0(
      "^tempest_",
      "(runtime|artifact|workflow|deliverable|capability|connection|skill)",
      "($|_)"
    ),
    symbols,
    value = TRUE
  )

  expect_contains(symbols, "tempest_run")
  expect_identical(sort(generic), character())
})
