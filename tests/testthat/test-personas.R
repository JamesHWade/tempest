test_that("tempest_type_personas returns valid ellmer type", {
  skip_if_not_installed("ellmer")

  type <- tempest:::tempest_type_personas()
  expect_true(!is.null(type))
})

test_that("tempest_format_persona_details formats correctly", {
  persona <- list(
    name = "Dr. Sarah Chen",
    title = "Climate Scientist",
    affiliation = "Arctic Research Institute",
    background = "20 years studying polar ice dynamics.",
    focus_areas = c("Ice sheet modeling", "Sea level rise"),
    perspective = "Physical science perspective on climate change"
  )

  details <- tempest:::tempest_format_persona_details(persona)

  expect_true(grepl("Arctic Research Institute", details))
  expect_true(grepl("20 years", details))
  expect_true(grepl("Ice sheet modeling", details))
  expect_true(grepl("Physical science", details))
})

test_that("tempest_format_persona_details handles missing fields", {
  persona <- list(
    name = "Dr. Sarah Chen",
    title = "Climate Scientist"
  )

  details <- tempest:::tempest_format_persona_details(persona)
  expect_true(is.character(details))
  expect_equal(details, "") # No fields to format
})

test_that("tempest_render_expert_prompt creates prompt with persona", {
  persona <- list(
    name = "Dr. Sarah Chen",
    title = "Climate Scientist",
    affiliation = "Arctic Research Institute",
    background = "20 years studying polar ice dynamics.",
    focus_areas = c("Ice sheet modeling", "Sea level rise"),
    perspective = "Physical science perspective on climate change"
  )

  prompt <- tempest:::tempest_render_expert_prompt(
    persona = persona,
    expert_id = 1
  )

  expect_true(grepl("Dr. Sarah Chen", prompt))
  expect_true(grepl("Climate Scientist", prompt))
  expect_true(grepl("Arctic Research Institute", prompt))
})

test_that("tempest_render_expert_prompt creates fallback for NULL persona", {
  prompt <- tempest:::tempest_render_expert_prompt(
    persona = NULL,
    expert_id = 3
  )

  expect_true(grepl("Expert 3", prompt))
  expect_true(grepl("Research Specialist", prompt))
})

test_that("TempestSession stores personas", {
  skip_if_not_installed("ellmer")
  skip_if(
    Sys.getenv("OPENAI_API_KEY") == "" && Sys.getenv("ANTHROPIC_API_KEY") == "",
    "No API key available"
  )

  # Create session with mock personas to avoid API call
  mock_personas <- list(
    list(
      id = 1,
      name = "Dr. Alice Smith",
      title = "Computer Scientist",
      affiliation = "Tech University",
      background = "Expert in algorithms",
      focus_areas = c("Machine learning"),
      perspective = "Technical perspective"
    ),
    list(
      id = 2,
      name = "Prof. Bob Jones",
      title = "Ethicist",
      affiliation = "Philosophy Department",
      background = "Expert in AI ethics",
      focus_areas = c("AI ethics"),
      perspective = "Ethical perspective"
    )
  )

  cfg <- tempest_config()
  session <- tempest_session(
    topic = "AI in healthcare",
    config = cfg,
    n_experts = 2,
    personas = mock_personas
  )

  expect_equal(length(session$personas), 2)
  expect_equal(session$personas[[1]]$name, "Dr. Alice Smith")
  expect_equal(session$personas[[2]]$name, "Prof. Bob Jones")
})

test_that("TempestSession get_persona_names returns names", {
  skip_if_not_installed("ellmer")
  skip_if(
    Sys.getenv("OPENAI_API_KEY") == "" && Sys.getenv("ANTHROPIC_API_KEY") == "",
    "No API key available"
  )

  mock_personas <- list(
    list(id = 1, name = "Dr. Alice Smith", title = "Scientist"),
    list(id = 2, name = "Prof. Bob Jones", title = "Ethicist")
  )

  cfg <- tempest_config()
  session <- tempest_session(
    topic = "Test topic",
    config = cfg,
    personas = mock_personas
  )

  names <- session$get_persona_names()
  expect_equal(names, c("Dr. Alice Smith", "Prof. Bob Jones"))
})

test_that("TempestSession find_expert_index matches names", {
  skip_if_not_installed("ellmer")
  skip_if(
    Sys.getenv("OPENAI_API_KEY") == "" && Sys.getenv("ANTHROPIC_API_KEY") == "",
    "No API key available"
  )

  mock_personas <- list(
    list(id = 1, name = "Dr. Alice Smith", title = "Scientist"),
    list(id = 2, name = "Prof. Bob Jones", title = "Ethicist")
  )

  cfg <- tempest_config()
  session <- tempest_session(
    topic = "Test topic",
    config = cfg,
    personas = mock_personas
  )

  # Exact match
  expect_equal(session$find_expert_index("Dr. Alice Smith"), 1)
  expect_equal(session$find_expert_index("Prof. Bob Jones"), 2)

  # Case-insensitive
  expect_equal(session$find_expert_index("dr. alice smith"), 1)

  # First word match (handles titles like Dr., Prof.)
  expect_equal(session$find_expert_index("Dr."), 1)
  expect_equal(session$find_expert_index("Prof."), 2)

  # Legacy format
  expect_equal(session$find_expert_index("expert_1"), 1)
  expect_equal(session$find_expert_index("expert_2"), 2)

  # Not found
  expect_null(session$find_expert_index("Unknown Person"))
})

test_that("TempestSession has expert_session_manager", {
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
    topic = "Test topic",
    config = cfg,
    personas = mock_personas
  )

  # Session has an expert_session_manager
  expect_true(inherits(session$expert_session_manager, "ExpertSessionManager"))
})

test_that("ExpertSessionManager generates session IDs", {
  skip_if_not_installed("ellmer")
  skip_if(
    Sys.getenv("OPENAI_API_KEY") == "" && Sys.getenv("ANTHROPIC_API_KEY") == "",
    "No API key available"
  )

  cfg <- tempest_config()
  store <- tempest:::SourceStore$new()
  retriever <- tempest:::tempest_retriever(config = cfg, store = store)
  mgr <- tempest:::ExpertSessionManager$new(cfg, retriever)

  # Generate a session ID
  sid <- mgr$generate_session_id("Dr. Sarah Chen")
  expect_true(is.character(sid))
  expect_true(grepl("^dr-sarah-chen-", sid))
})

test_that("tempest_create_expert_tool creates valid ellmer tool", {
  skip_if_not_installed("ellmer")
  skip_if(
    Sys.getenv("OPENAI_API_KEY") == "" && Sys.getenv("ANTHROPIC_API_KEY") == "",
    "No API key available"
  )

  persona <- list(
    id = 1,
    name = "Dr. Sarah Chen",
    title = "Climate Scientist",
    perspective = "Physical science perspective"
  )

  cfg <- tempest_config()
  store <- tempest:::SourceStore$new()
  retriever <- tempest:::tempest_retriever(config = cfg, store = store)
  mgr <- tempest:::ExpertSessionManager$new(cfg, retriever)

  tool <- tempest:::tempest_create_expert_tool(persona, mgr, "Climate change")

  # Tool should have the expected name (ellmer tools are S7 objects, use @)
  expect_equal(tool@name, "ask_dr_sarah_chen")

  # Tool should have a description mentioning the persona
  expect_true(grepl("Dr. Sarah Chen", tool@description))
  expect_true(grepl("Climate Scientist", tool@description))
})

test_that("expert tools fall back to returned text", {
  skip_if_not_installed("ellmer")

  persona <- list(
    id = 1,
    name = "Dr. Sarah Chen",
    title = "Climate Scientist",
    perspective = "Physical science perspective"
  )
  fake <- fake_chat(text = list("Expert answer [S123456789abc]."))
  cfg <- tempest_config(chat_fn = function(role, model, system_prompt, echo) {
    fake
  })
  store <- tempest:::SourceStore$new()
  retriever <- tempest:::tempest_retriever(config = cfg, store = store)
  mgr <- tempest:::ExpertSessionManager$new(cfg, retriever)

  tool <- tempest:::tempest_create_expert_tool(persona, mgr, "Climate change")
  result <- tool(question = "What should we know?")

  expect_equal(result$expert, "Dr. Sarah Chen")
  expect_equal(result$response, "Expert answer [S123456789abc].")
  prompts <- vapply(fake$.calls(), function(call) call$prompt, character(1))
  expect_match(prompts[[1]], "available web/source tools", fixed = TRUE)
})

test_that("expert tools harvest native search sources before fact extraction", {
  skip_if_not_installed("ellmer")

  url <- "https://example.org/native-source"
  source_id <- tempest:::tempest_source_id(url)
  SearchResponse <- getFromNamespace("ContentToolResponseSearch", "ellmer")
  turn <- ellmer::AssistantTurn(
    contents = list(
      SearchResponse(
        urls = url,
        json = list(
          results = list(list(
            title = "Native Source",
            url = url,
            snippet = "Native source snippet."
          ))
        )
      ),
      ellmer::ContentText(paste("Native-backed claim", url))
    )
  )
  expert_chat <- list(
    chat = function(prompt, ...) paste("Native-backed claim", url),
    last_turn = function() turn,
    register_tools = function(...) invisible(NULL)
  )
  extractor <- fake_chat(
    structured = list(list(
      facts = list(list(
        claim = "Native-backed claim",
        sources = list(list(url = url)),
        confidence = "high"
      ))
    ))
  )
  cfg <- tempest_config(chat_fn = function(role, model, system_prompt, echo) {
    if (identical(role, "expert")) expert_chat else fake_chat()
  })
  store <- tempest:::SourceStore$new()
  retriever <- tempest:::tempest_retriever(config = cfg, store = store)
  mgr <- tempest:::ExpertSessionManager$new(
    cfg,
    retriever,
    extractor = extractor,
    store = store
  )
  persona <- list(
    id = 1,
    name = "Dr. Sarah Chen",
    title = "Climate Scientist",
    perspective = "Physical science perspective"
  )

  tool <- tempest:::tempest_create_expert_tool(persona, mgr, "Climate change")
  result <- tool(question = "What should we know?")

  expect_equal(result$response, paste("Native-backed claim", url))
  expect_equal(store$get_source(source_id)$title, "Native Source")
  claims <- store$list_claims()
  expect_length(claims, 1)
  expect_equal(claims[[1]]@source_ids, source_id)
})

test_that("merging source records tolerates empty and missing fields", {
  old <- list(
    title = "Old title",
    snippet = "Old snippet",
    content_text = "Old body",
    fetched_at = "2026-01-01 00:00:00 UTC",
    meta = list(kind = "old")
  )
  new <- list(
    title = character(),
    snippet = NA_character_,
    content_text = "",
    fetched_at = "2027-01-01 00:00:00 UTC",
    meta = list(provider_tool = "native")
  )

  merged <- tempest:::tempest_merge_source_record(old, new)

  expect_equal(merged$title, "Old title")
  expect_equal(merged$snippet, "Old snippet")
  expect_equal(merged$content_text, "Old body")
  expect_equal(merged$fetched_at, "2027-01-01 00:00:00 UTC")
  expect_equal(merged$meta, list(kind = "old", provider_tool = "native"))
})
