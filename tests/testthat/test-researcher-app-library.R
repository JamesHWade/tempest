test_that("the library UI is namespaced and does not expose file inputs", {
  skip_if_not_installed("shiny")

  html <- paste(
    as.character(tempest_researcher_library_ui("library")),
    collapse = ""
  )

  expect_match(html, "library-search", fixed = TRUE)
  expect_match(html, "library-refresh", fixed = TRUE)
  expect_match(html, "library-status", fixed = TRUE)
  expect_match(html, "Search saved reports", fixed = TRUE)
  expect_match(html, "Refresh saved reports", fixed = TRUE)
  expect_no_match(html, "fileInput", fixed = TRUE)
  expect_no_match(html, "datapath", fixed = TRUE)
  expect_match(html, 'aria-labelledby="library-title"', fixed = TRUE)
})

test_that("the catalog accepts only app-published report names", {
  root <- withr::local_tempdir()
  fixture <- test_promotion_fixture("storm")
  publish <- function(name) {
    output <- file.path(root, name)
    tempest_enrich_report(
      fixture$research,
      output,
      plan = test_enriched_plan(fixture$resource@resource_id, analyses = FALSE),
      allow_code = FALSE
    )@path
  }
  valid <- publish("research-run-a1b2")
  publish("research-not-app")
  dir.create(file.path(root, "research-run-deadbeef"))
  writeLines(
    "incomplete",
    file.path(root, "research-run-deadbeef", "notes.txt")
  )

  catalog <- tempest:::tempest_researcher_library_catalog(root)

  expect_identical(catalog$status, "ok")
  expect_identical(catalog$reports$id, basename(valid))
  expect_identical(catalog$reports$title, "A reproducible research brief")
  expect_identical(catalog$skipped, 1L)
})

test_that("opening revalidates the direct child and forwards a verified report", {
  skip_if_not_installed("shiny")
  root <- withr::local_tempdir()
  fixture <- test_promotion_fixture("storm")
  valid <- tempest_enrich_report(
    fixture$research,
    file.path(root, "research-run-a1b2"),
    plan = test_enriched_plan(fixture$resource@resource_id, analyses = FALSE),
    allow_code = FALSE
  )
  opened <- new.env(parent = emptyenv())
  opened$report <- NULL

  shiny::testServer(
    function(id) {
      tempest_researcher_library_server(
        id,
        directory = root,
        on_open = function(report) opened$report <- report,
        refresh_token = shiny::reactive(0L)
      )
    },
    {
      session$flushReact()
      session$setInputs(selected = "research-run-a1b2", open = 1)
      session$flushReact()
      expect_s7_class(opened$report, TempestEnrichedReport)
      expect_identical(opened$report@path, valid@path)

      opened$report <- NULL
      session$setInputs(
        selected = file.path(root, "research-run-a1b2"),
        open = 2
      )
      session$flushReact()
      expect_null(opened$report)
      expect_match(
        paste(as.character(output$status$html), collapse = ""),
        "Choose a saved report",
        fixed = TRUE
      )
    }
  )
})

test_that("reactive and manual refresh discover reports and search filters choices", {
  skip_if_not_installed("shiny")
  root <- withr::local_tempdir()
  fixture <- test_promotion_fixture("storm")
  publish <- function(name, title) {
    plan <- test_enriched_plan(fixture$resource@resource_id, analyses = FALSE)
    plan$title <- title
    tempest_enrich_report(
      fixture$research,
      file.path(root, name),
      plan = plan,
      allow_code = FALSE
    )
  }
  token <- shiny::reactiveVal(0L)

  shiny::testServer(
    function(id) {
      tempest_researcher_library_server(
        id,
        directory = root,
        on_open = function(report) invisible(report),
        refresh_token = token
      )
    },
    {
      session$flushReact()
      expect_match(
        paste(as.character(output$status$html), collapse = ""),
        "No saved reports",
        fixed = TRUE
      )
      publish("research-run-a1b2", "First report")
      token(1L)
      session$flushReact()
      expect_match(
        paste(as.character(output$status$html), collapse = ""),
        "1 saved report",
        fixed = TRUE
      )
      publish("research-run-c3d4", "Second report")
      session$setInputs(refresh = 1)
      session$flushReact()
      expect_match(
        paste(as.character(output$status$html), collapse = ""),
        "2 saved reports",
        fixed = TRUE
      )
      session$setInputs(search = "First")
      session$flushReact()
      expect_match(
        paste(as.character(output$reports$html), collapse = ""),
        "First report",
        fixed = TRUE
      )
      expect_no_match(
        paste(as.character(output$reports$html), collapse = ""),
        "Second report",
        fixed = TRUE
      )
    }
  )
})

test_that("symlinked report trees cannot be opened", {
  skip_on_os("windows")
  skip_if_not_installed("shiny")
  root <- withr::local_tempdir()
  outside <- withr::local_tempdir()
  fixture <- test_promotion_fixture("storm")
  linked <- tempest_enrich_report(
    fixture$research,
    file.path(root, "research-run-deadbeef"),
    plan = test_enriched_plan(fixture$resource@resource_id, analyses = FALSE),
    allow_code = FALSE
  )@path
  unlink(file.path(linked, "research.md"))
  writeLines("outside", file.path(outside, "research.md"))
  expect_true(file.symlink(
    file.path(outside, "research.md"),
    file.path(linked, "research.md")
  ))

  opened <- new.env(parent = emptyenv())
  opened$report <- NULL
  shiny::testServer(
    function(id) {
      tempest_researcher_library_server(
        id,
        directory = root,
        on_open = function(report) opened$report <- report,
        refresh_token = shiny::reactive(0L)
      )
    },
    {
      session$flushReact()
      session$setInputs(selected = "research-run-deadbeef", open = 1)
      session$flushReact()
      expect_null(opened$report)
    }
  )
})
