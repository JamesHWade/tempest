test_that("the dedicated app launches without constructing host tools", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("callr")
  skip_if_not_installed("zip")
  withr::local_options(tempest.app_runner = function(app, ...) {
    list(app = app, args = list(...))
  })
  result <- tempest_researcher_app(
    port = 9876L,
    launch.browser = FALSE,
    tools = list(literature = function() stop("must be lazy"))
  )
  expect_s3_class(result$app, "shiny.appobj")
  expect_identical(result$args, list(port = 9876L, launch.browser = FALSE))
})

test_that("the launcher passes the configured expert budget to the UI", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  withr::local_options(tempest.app_runner = function(app, ...) app)
  observed <- new.env(parent = emptyenv())
  local_mocked_bindings(tempest_researcher_ui = function(..., max_experts) {
    observed$budget <- max_experts
    shiny::div()
  })

  tempest_researcher_app(config = tempest_config(max_active_experts = 1L))

  expect_identical(observed$budget, 1L)
})

test_that("the server selects only discovered tools and their owning servers", {
  skip_if_not_installed("shiny")
  events <- new.env(parent = emptyenv())
  events$value <- list(
    status = "idle",
    kind = NULL,
    result = NULL,
    error = NULL,
    progress = list()
  )
  events$closed <- FALSE
  catalog <- data.frame(
    id = c("a:first", "b:second"),
    name = c("first", "second"),
    description = c("First tool", "Second tool"),
    server = c("a", "b")
  )
  local_mocked_bindings(tempest_researcher_app_controller = function(...) {
    list(
      snapshot = function() events$value,
      discover = function(servers) {
        events$servers <- servers
        events$value <- list(
          status = "running",
          kind = "discover",
          result = NULL,
          error = NULL,
          progress = list()
        )
      },
      poll = function() {
        if (identical(events$value$kind, "discover")) {
          events$value$status <- "complete"
          events$value$result <- catalog
        }
        events$value
      },
      start = function(request, retriever = NULL) {
        events$request <- request
        events$retriever <- retriever
        events$value$status <- "running"
        events$value$kind <- "research"
      },
      cancel = function() events$value$status <- "cancelled",
      close = function() events$closed <- TRUE
    )
  })
  shiny::testServer(
    tempest_researcher_app_server(tempest_config(), list(), NULL, tempdir()),
    {
      session$setInputs(mcp_servers = c("a", "b"), discover = 1)
      session$elapse(250)
      session$flushReact()
      expect_equal(events$servers, c("a", "b"))
      expect_match(output$mcp_tools$html, "First tool", fixed = TRUE)
      session$setInputs(
        selected_mcp = "b:second",
        topic = "A research question",
        model = "openai/gpt-6.1-sol",
        n_experts = 2,
        max_rounds = 2,
        max_questions = 2,
        max_analyses = 0,
        max_illustrations = 0,
        allow_code = FALSE,
        allow_images = FALSE,
        run = 1
      )
      expect_identical(events$request$selected_mcp, "b:second")
      expect_null(events$retriever)
      expect_identical(events$request$mcp_servers, "b")
      expect_identical(events$request$allow_code, FALSE)
      session$setInputs(cancel = 1)
      expect_match(output$status$html, "cancelled", fixed = TRUE)
      events$request <- NULL
      session$setInputs(selected_mcp = "forged", run = 2)
      expect_null(events$request)
      expect_match(output$status$html, "rediscover", fixed = TRUE)
    }
  )
  expect_identical(events$closed, TRUE)
})

test_that("server errors do not expose provider or configuration secrets", {
  skip_if_not_installed("shiny")
  local_mocked_bindings(tempest_researcher_app_controller = function(...) {
    list(
      snapshot = function() list(status = "idle", progress = list()),
      start = function(request, retriever = NULL) {
        stop("Authorization: Bearer secret-value")
      },
      close = function() invisible(NULL)
    )
  })
  shiny::testServer(
    tempest_researcher_app_server(tempest_config(), list(), NULL, tempdir()),
    {
      session$setInputs(topic = "question", run = 1)
      expect_match(output$status$html, "Research could not start", fixed = TRUE)
      expect_no_match(output$status$html, "secret-value", fixed = TRUE)
      expect_match(output$status$html, 'role="alert"', fixed = TRUE)
    }
  )
})

test_that("an invalid completed report cannot be announced as ready", {
  skip_if_not_installed("shiny")
  events <- new.env(parent = emptyenv())
  events$value <- list(status = "idle", kind = "research", progress = list())
  local_mocked_bindings(tempest_researcher_app_controller = function(...) {
    list(
      snapshot = function() events$value,
      start = function(request, retriever = NULL) {
        events$value$status <- "running"
      },
      poll = function() {
        list(
          status = "complete",
          kind = "research",
          progress = list(),
          result = list(path = file.path(tempdir(), "missing-report"))
        )
      },
      close = function() invisible(NULL)
    )
  })
  shiny::testServer(
    tempest_researcher_app_server(tempest_config(), list(), NULL, tempdir()),
    {
      session$setInputs(topic = "question", run = 1)
      session$elapse(250)
      session$flushReact()
      expect_match(output$status$html, "could not be verified", fixed = TRUE)
      expect_no_match(output$status$html, "Report ready", fixed = TRUE)
      expect_no_match(output$report$html, "iframe", fixed = TRUE)
    }
  )
})

test_that("saved report browsing requires the host to opt in", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  withr::local_options(tempest.app_runner = function(app, ...) app)
  observed <- new.env(parent = emptyenv())
  local_mocked_bindings(
    tempest_researcher_ui = function(..., library_enabled) {
      observed$ui <- library_enabled
      shiny::div()
    },
    tempest_researcher_app_server = function(..., report_library) {
      observed$server <- report_library
      function(input, output, session) {}
    }
  )
  tempest_researcher_app()
  expect_identical(observed$ui, FALSE)
  expect_identical(observed$server, FALSE)
  tempest_researcher_app(report_library = TRUE)
  expect_identical(observed$ui, TRUE)
  expect_identical(observed$server, TRUE)
})

test_that("the server builds a host retriever for each research run", {
  skip_if_not_installed("shiny")
  events <- new.env(parent = emptyenv())
  idle <- list(
    status = "idle",
    kind = NULL,
    result = NULL,
    error = NULL,
    progress = list()
  )
  local_mocked_bindings(tempest_researcher_app_controller = function(...) {
    list(
      snapshot = function() idle,
      start = function(request, retriever = NULL) {
        events$retriever <- retriever
      },
      close = function() NULL
    )
  })
  withr::local_options(
    tempest.shiny.retriever = function(config, session, default) {
      events$session <- session
      list(
        workspace = default$workspace,
        search = function(query, k = 5L) data.frame(),
        fetch = function(url) NULL
      )
    }
  )
  shiny::testServer(
    tempest_researcher_app_server(tempest_config(), list(), NULL, tempdir()),
    {
      session$setInputs(
        topic = "A research question",
        model = "openai/gpt-6.1-sol",
        n_experts = 1,
        max_rounds = 1,
        max_questions = 1,
        max_analyses = 0,
        max_illustrations = 0,
        run = 1
      )
      expect_identical(events$session, session)
      expect_s3_class(events$retriever$workspace, "ResearchWorkspace")
    }
  )
})
