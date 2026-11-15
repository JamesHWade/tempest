test_that("scientific review settings follow their controls", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  env <- tempest:::tempest_shiny_module_env()
  server <- function(input, output, session) {
    session$userData$config <- env$mod_config_server("settings")
  }

  shiny::testServer(server, {
    session$flushReact()
    config <- shiny::isolate(session$userData$config())
    expect_identical(config@research_review, FALSE)
    expect_identical(config@repair_evidence, FALSE)
    expect_identical(config@max_review_claims, 5L)
    expect_identical(config@review_concurrency, 2L)

    session$setInputs(
      `settings-research_review` = TRUE,
      `settings-repair_evidence` = TRUE,
      `settings-max_review_claims` = 12
    )
    session$flushReact()
    config <- shiny::isolate(session$userData$config())
    expect_identical(config@research_review, TRUE)
    expect_identical(config@repair_evidence, TRUE)
    expect_identical(config@max_review_claims, 12L)

    session$setInputs(`settings-research_review` = FALSE)
    session$flushReact()
    config <- shiny::isolate(session$userData$config())
    expect_identical(config@research_review, FALSE)
    expect_identical(config@repair_evidence, FALSE)
    expect_identical(config@max_review_claims, 12L)
  })
})

test_that("scientific review claim count stays within configured bounds", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  env <- tempest:::tempest_shiny_module_env()
  server <- function(input, output, session) {
    session$userData$config <- env$mod_config_server("settings")
  }

  shiny::testServer(server, {
    session$setInputs(
      `settings-research_review` = TRUE,
      `settings-max_review_claims` = 1
    )
    session$flushReact()
    expect_identical(
      shiny::isolate(session$userData$config())@max_review_claims,
      1L
    )

    session$setInputs(`settings-max_review_claims` = 20)
    session$flushReact()
    expect_identical(
      shiny::isolate(session$userData$config())@max_review_claims,
      20L
    )

    session$setInputs(`settings-max_review_claims` = 0)
    session$flushReact()
    expect_error(
      shiny::isolate(session$userData$config()),
      class = "tempest_config_error"
    )

    session$setInputs(`settings-max_review_claims` = 21)
    session$flushReact()
    expect_error(
      shiny::isolate(session$userData$config()),
      class = "tempest_config_error"
    )
  })
})
