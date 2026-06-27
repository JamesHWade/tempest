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
  expect_length(chat$get_tools(), 0)
  tempest:::tempest_register_single_expert_tool(
    chat,
    persona,
    mgr,
    "Test topic"
  )
  expect_length(chat$get_tools(), 1)
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

test_that("transcript_markdown returns the most recent turns", {
  skip_if_not_installed("ellmer")
  ses <- tempest_session(
    "Test topic",
    config = tempest_config(),
    personas = list(
      list(id = 1, name = "Dr. Alice", title = "Scientist", perspective = "Tech")
    )
  )
  for (i in seq_len(5)) {
    ses$add_turn(paste0("S", i), "assistant", paste("turn", i))
  }

  md <- ses$transcript_markdown(max_turns = 2)

  expect_match(md, "turn 4")
  expect_match(md, "turn 5")
  expect_no_match(md, "turn 1")
})

test_that("retire_expert handles out-of-range legacy ids gracefully", {
  skip_if_not_installed("ellmer")
  ses <- tempest_session(
    "Test topic",
    config = tempest_config(),
    personas = list(
      list(id = 1, name = "Dr. Alice", title = "Scientist", perspective = "Tech")
    )
  )

  expect_false(ses$retire_expert("expert_9"))
  expect_true(ses$retire_expert("expert_1"))
})

test_that("add_expert returns NULL at the active-expert cap", {
  skip_if_not_installed("ellmer")
  ses <- tempest_session(
    "Test topic",
    config = tempest_config(max_active_experts = 1),
    personas = list(
      list(id = 1, name = "Dr. Alice", title = "Scientist", perspective = "Tech")
    )
  )

  result <- suppressWarnings(ses$add_expert(area = "New area"))
  expect_null(result)
})
