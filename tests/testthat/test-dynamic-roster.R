test_that("TempestSession retire_expert marks persona retired", {
  skip_if_not_installed("ellmer")
  skip_if(
    Sys.getenv("OPENAI_API_KEY") == "" && Sys.getenv("ANTHROPIC_API_KEY") == "",
    "No API key available"
  )

  mock_personas <- list(
    list(
      id = 1,
      name = "Dr. Alice Smith",
      title = "Scientist",
      perspective = "Technical"
    ),
    list(
      id = 2,
      name = "Prof. Bob Jones",
      title = "Ethicist",
      perspective = "Ethical"
    )
  )

  cfg <- tempest_config()
  session <- tempest_session(
    "Test topic",
    config = cfg,
    personas = mock_personas
  )

  # Retire first expert
  result <- session$retire_expert("Dr. Alice Smith")
  expect_true(result)
  expect_true(isTRUE(session$personas[[1]]$retired))
  expect_false(isTRUE(session$personas[[2]]$retired))
})

test_that("TempestSession retire_expert returns FALSE for unknown", {
  skip_if_not_installed("ellmer")
  skip_if(
    Sys.getenv("OPENAI_API_KEY") == "" && Sys.getenv("ANTHROPIC_API_KEY") == "",
    "No API key available"
  )

  mock_personas <- list(
    list(
      id = 1,
      name = "Dr. Alice Smith",
      title = "Scientist",
      perspective = "Technical"
    )
  )

  cfg <- tempest_config()
  session <- tempest_session(
    "Test topic",
    config = cfg,
    personas = mock_personas
  )

  result <- session$retire_expert("Unknown Expert")
  expect_false(result)
})

test_that("TempestSession get_active_personas filters retired", {
  skip_if_not_installed("ellmer")
  skip_if(
    Sys.getenv("OPENAI_API_KEY") == "" && Sys.getenv("ANTHROPIC_API_KEY") == "",
    "No API key available"
  )

  mock_personas <- list(
    list(
      id = 1,
      name = "Dr. Alice Smith",
      title = "Scientist",
      perspective = "Technical"
    ),
    list(
      id = 2,
      name = "Prof. Bob Jones",
      title = "Ethicist",
      perspective = "Ethical"
    ),
    list(
      id = 3,
      name = "Dr. Carol Lee",
      title = "Engineer",
      perspective = "Applied"
    )
  )

  cfg <- tempest_config()
  session <- tempest_session(
    "Test topic",
    config = cfg,
    personas = mock_personas
  )

  # Initially all active
  active <- session$get_active_personas()
  expect_equal(length(active), 3)

  # Retire one
  session$retire_expert("Prof. Bob Jones")
  active <- session$get_active_personas()
  expect_equal(length(active), 2)

  # Verify the right ones remain
  active_names <- vapply(active, function(p) p$name, character(1))
  expect_true("Dr. Alice Smith" %in% active_names)
  expect_true("Dr. Carol Lee" %in% active_names)
  expect_false("Prof. Bob Jones" %in% active_names)
})

test_that("tempest_register_single_expert_tool works", {
  skip_if_not_installed("ellmer")
  skip_if(
    Sys.getenv("OPENAI_API_KEY") == "" && Sys.getenv("ANTHROPIC_API_KEY") == "",
    "No API key available"
  )

  persona <- list(
    id = 1,
    name = "Dr. New Expert",
    title = "Specialist",
    perspective = "Unique perspective"
  )

  cfg <- tempest_config()
  store <- SourceStore$new()
  retriever <- tempest_retriever(config = cfg, store = store)
  mgr <- tempest:::ExpertSessionManager$new(cfg, retriever)

  chat <- cfg$make_chat("coordinator")
  # This should not error
  tempest:::tempest_register_single_expert_tool(
    chat,
    persona,
    mgr,
    "Test topic"
  )
  expect_true(TRUE) # If we got here, it worked
})

test_that("tempest_generate_single_persona returns persona structure", {
  skip_if_not_installed("ellmer")
  skip_if(
    Sys.getenv("OPENAI_API_KEY") == "" && Sys.getenv("ANTHROPIC_API_KEY") == "",
    "No API key available"
  )

  cfg <- tempest_config()
  existing <- list(
    list(name = "Dr. Alice", title = "Scientist", perspective = "Technical")
  )

  persona <- tempest:::tempest_generate_single_persona(
    "Climate change",
    "Policy analysis",
    existing,
    cfg
  )

  expect_type(persona, "list")
  expect_true(!is.null(persona$name))
  expect_true(!is.null(persona$title))
})
