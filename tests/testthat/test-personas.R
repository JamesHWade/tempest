test_that("tempest_type_personas returns the provider expert schema", {
  skip_if_not_installed("ellmer")

  type <- tempest:::tempest_type_personas()
  expect_s7_class(type, getFromNamespace("TypeObject", "ellmer"))
})

test_that("tempest_format_persona_details formats provider records", {
  record <- list(
    name = "Dr. Sarah Chen",
    title = "Climate Scientist",
    affiliation = "Arctic Research Institute",
    background = "20 years studying polar ice dynamics.",
    focus_areas = c("Ice sheet modeling", "Sea level rise"),
    perspective = "Physical science perspective on climate change"
  )

  details <- tempest:::tempest_format_persona_details(record)

  expect_match(details, "Arctic Research Institute", fixed = TRUE)
  expect_match(details, "20 years", fixed = TRUE)
  expect_match(details, "Ice sheet modeling", fixed = TRUE)
  expect_match(details, "Physical science", fixed = TRUE)
})

test_that("tempest_render_expert_prompt accepts an expert profile", {
  expert <- test_expert(
    expert_id = "expert.climate",
    name = "Dr. Sarah Chen",
    title = "Climate Scientist",
    description = "Physical science perspective on climate change",
    metadata = list(
      affiliation = "Arctic Research Institute",
      background = "20 years studying polar ice dynamics."
    )
  )

  prompt <- tempest:::tempest_render_expert_prompt(expert)

  expect_match(prompt, "Dr. Sarah Chen", fixed = TRUE)
  expect_match(prompt, "Climate Scientist", fixed = TRUE)
  expect_match(prompt, "Arctic Research Institute", fixed = TRUE)
})

test_that("tempest_render_expert_prompt rejects a missing profile", {
  expect_error(
    tempest:::tempest_render_expert_prompt(
      persona = NULL,
      expert_id = "expert.3"
    ),
    class = "tempest_config_error"
  )
})

test_that("TempestSession stores selected expert profiles", {
  skip_if_not_installed("ellmer")
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  experts <- list(
    test_expert(
      expert_id = "expert.alice",
      name = "Dr. Alice Smith",
      title = "Computer Scientist"
    ),
    test_expert(
      expert_id = "expert.bob",
      name = "Prof. Bob Jones",
      title = "Ethicist"
    )
  )

  session <- tempest_session(
    topic = "AI in healthcare",
    config = cfg,
    experts = experts
  )

  expect_length(session$experts, 2)
  expect_equal(
    session$get_expert_names(),
    c(
      "Dr. Alice Smith",
      "Prof. Bob Jones"
    )
  )
  expect_equal(session$find_expert("expert.alice"), 1)
  expect_null(session$find_expert("Dr. Alice Smith"))
  expect_r6_class(session$expert_session_manager, "ExpertSessionManager")
})

test_that("expert delegation tool uses stable ids and reuses sessions", {
  skip_if_not_installed("ellmer")
  expert_chat <- fake_chat(text = list("First answer.", "Second answer."))
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) {
      if (identical(role, "expert")) expert_chat else fake_chat()
    }
  )
  store <- test_research_workspace()
  retriever <- tempest:::tempest_retriever(config = cfg, workspace = store)
  expert <- test_expert(
    expert_id = "expert.climate",
    name = "Dr. Sarah Chen",
    title = "Climate Scientist"
  )
  manager <- tempest:::ExpertSessionManager$new(
    experts = list(expert),
    runtime = tempest_runtime(),
    config = cfg,
    retriever = retriever
  )
  tool <- tempest:::tempest_create_expert_delegation_tool(
    manager,
    "Climate change",
    experts = list(expert)
  )

  first <- tool(
    expert_id = "expert.climate",
    question = "What should we know?"
  )
  second <- tool(
    expert_id = "expert.climate",
    question = "What else?"
  )

  expect_equal(tool@name, "delegate_to_expert")
  expect_equal(first$expert_id, "expert.climate")
  expect_equal(first$expert, "Dr. Sarah Chen")
  expect_equal(second$session_id, first$session_id)
  expect_length(manager$list_sessions(), 1)
  expect_error(
    tool(expert_id = "Dr. Sarah Chen", question = "Use a display name"),
    "valid stable expert id"
  )
})

test_that("expert delegation harvests native sources before extraction", {
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
  expert_chat <- fake_chat(text = list(paste("Native-backed claim", url)))
  expert_chat$last_turn <- function(role = "assistant") turn
  extractor <- fake_chat(
    structured = list(list(
      facts = list(list(
        claim = "Native-backed claim",
        sources = list(list(source_id = source_id)),
        confidence = "high"
      ))
    ))
  )
  cfg <- tempest_config(chat_fn = function(role, model, system_prompt, echo) {
    if (identical(role, "expert")) expert_chat else fake_chat()
  })
  store <- test_research_workspace()
  retriever <- tempest:::tempest_retriever(config = cfg, workspace = store)
  expert <- test_expert(
    expert_id = "expert.climate",
    name = "Dr. Sarah Chen",
    title = "Climate Scientist"
  )
  manager <- tempest:::ExpertSessionManager$new(
    experts = list(expert),
    runtime = tempest_runtime(),
    config = cfg,
    retriever = retriever,
    extractor = extractor,
    extract_claims_program = tempest:::tempest_costorm_program_execution(
      tempest_program_set(),
      "extract_claims",
      "session-native-source"
    ),
    workspace = store,
    run_id = "session-native-source"
  )
  tool <- tempest:::tempest_create_expert_delegation_tool(
    manager,
    "Climate change"
  )

  result <- tool(
    expert_id = "expert.climate",
    question = "What should we know?"
  )

  expect_equal(result$response, paste("Native-backed claim", url))
  expect_equal(store$get_retrieved_source(source_id)$title, "Native Source")
  claims <- store$list_proposed_claims()
  expect_length(claims, 1)
  expect_equal(claims[[1]]@source_ids, source_id)
  expect_equal(claims[[1]]@expert_id, "expert.climate")
  expect_identical(claims[[1]]@session_id, manager$run_id)
})

test_that("expert delegation harvests OpenAI native annotations", {
  skip_if_not_installed("ellmer")
  url <- "https://example.org/openai-native-source"
  source_id <- tempest:::tempest_source_id(url)
  turn <- native_openai_json_turn(
    claim_text = "OpenAI native-backed claim.",
    url = url,
    title = "OpenAI Native Source"
  )
  expert_chat <- fake_chat(text = list("OpenAI native-backed claim."))
  expert_chat$last_turn <- function(role = "assistant") turn
  extractor <- fake_chat(
    structured = list(list(
      facts = list(list(
        claim = "OpenAI native-backed claim",
        sources = list(list(source_id = source_id)),
        confidence = "high"
      ))
    ))
  )
  cfg <- tempest_config(chat_fn = function(role, model, system_prompt, echo) {
    if (identical(role, "expert")) expert_chat else fake_chat()
  })
  store <- test_research_workspace()
  retriever <- tempest:::tempest_retriever(config = cfg, workspace = store)
  expert <- test_expert(
    expert_id = "expert.climate",
    name = "Dr. Sarah Chen",
    title = "Climate Scientist"
  )
  manager <- tempest:::ExpertSessionManager$new(
    experts = list(expert),
    runtime = tempest_runtime(),
    config = cfg,
    retriever = retriever,
    extractor = extractor,
    extract_claims_program = tempest:::tempest_costorm_program_execution(
      tempest_program_set(),
      "extract_claims",
      "session-openai-source"
    ),
    workspace = store,
    run_id = "session-openai-source"
  )
  tool <- tempest:::tempest_create_expert_delegation_tool(
    manager,
    "Climate change"
  )

  result <- tool(
    expert_id = "expert.climate",
    question = "What should we know?"
  )

  expect_equal(result$response, "OpenAI native-backed claim.")
  expect_equal(
    store$get_retrieved_source(source_id)$title,
    "OpenAI Native Source"
  )
  expect_equal(store$list_proposed_claims()[[1]]@source_ids, source_id)
})

test_that("merging source records tolerates empty and missing fields", {
  old <- list(
    title = "Old title",
    snippet = "Old snippet",
    content_text = "Old body",
    fetched_at = "2026-01-01T00:00:00Z",
    meta = list(kind = "old")
  )
  new <- list(
    title = character(),
    snippet = NA_character_,
    content_text = "",
    fetched_at = "2027-01-01T00:00:00Z",
    meta = list(provider_tool = "native")
  )

  merged <- tempest:::tempest_merge_source_record(old, new)

  expect_equal(merged$title, "Old title")
  expect_equal(merged$snippet, "Old snippet")
  expect_equal(merged$content_text, "Old body")
  expect_equal(merged$fetched_at, "2027-01-01T00:00:00Z")
  expect_equal(merged$meta, list(kind = "old", provider_tool = "native"))
})
