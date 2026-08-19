test_that("example host uses the exported asynchronous Shiny adapter", {
  app_path <- system.file(
    "examples",
    "shiny-host",
    "app.R",
    package = "tempest"
  )
  expect_identical(file.exists(app_path), TRUE)
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

  expect_contains(symbols, "tempest_shiny_ui")
  expect_contains(symbols, "tempest_shiny_server")
  expect_contains(symbols, "tempest_config")
  expect_identical("tempest_run" %in% symbols, FALSE)
  expect_identical(sort(generic), character())
})
