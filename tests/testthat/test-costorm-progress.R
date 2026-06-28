test_that("TempestSession emits Co-STORM progress events", {
  skip_if_not_installed("ellmer")
  source <- fake_source(
    url = "https://example.org/costorm-progress",
    title = "Co-STORM source",
    content_text = "Co-STORM progress uses compact event metadata."
  )
  source_id <- source$id
  store <- SourceStore$new()
  store$upsert_source(source)
  collector <- tempest_progress_collector(include_payload = TRUE)
  mindmap <- list(
    nodes = list(list(
      id = "root",
      label = "Co-STORM progress",
      summary = "Progress metadata",
      source_ids = source_id
    )),
    edges = list()
  )
  claim_result <- function(claim) {
    list(
      facts = list(list(
        claim = claim,
        sources = list(list(source_id = source_id)),
        confidence = "high"
      ))
    )
  }
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) {
      if (identical(role, "expert")) {
        return(fake_chat(
          text = list(paste0(
            "Expert warmup cites progress [",
            source_id,
            "]."
          ))
        ))
      }
      if (identical(role, "mindmap")) {
        return(fake_chat(structured = list(mindmap, mindmap)))
      }
      if (
        identical(role, "judge") &&
          identical(system_prompt, tempest_prompt("fact_extractor_system"))
      ) {
        return(fake_chat(
          structured = list(
            claim_result("Warmup progress is observable."),
            claim_result("Dialogue progress is observable.")
          )
        ))
      }
      if (
        identical(role, "coordinator") &&
          identical(system_prompt, tempest_prompt("question_suggester_system"))
      ) {
        return(fake_chat(structured = list(list(questions = "What next?"))))
      }
      if (identical(role, "coordinator")) {
        return(fake_chat(
          text = list(paste0(
            "Moderator answer cites progress [",
            source_id,
            "]."
          ))
        ))
      }
      if (identical(role, "writer")) {
        return(fake_chat(
          text = list(paste0(
            "Report body cites progress [",
            source_id,
            "]."
          ))
        ))
      }
      fake_chat()
    }
  )
  session <- tempest_session(
    "Co-STORM progress",
    config = cfg,
    retriever = tempest_retriever(config = cfg, store = store),
    personas = list(list(
      id = 1,
      name = "Dr. Flow",
      title = "Workflow analyst",
      perspective = "Progress metadata",
      initial_questions = "How should progress be exposed?"
    )),
    progress = collector$record
  )

  session$warmup(verbose = FALSE)
  session$step("What should we inspect?")
  questions <- session$suggest_questions(n = 1)
  report <- session$report(include_references = FALSE, reorganize = FALSE)

  event_data <- collector$data()
  labels <- vapply(
    event_data,
    function(event) {
      paste(
        event$event_type,
        event$stage,
        event$step,
        event$status,
        sep = ":"
      )
    },
    character(1)
  )

  expect_contains(
    labels,
    c(
      "workflow:session:created:started",
      "stage:warmup:expert_fanout:started",
      "expert:warmup:expert_fanout:started",
      "tool:warmup:expert_question:started",
      "tool:warmup:expert_question:succeeded",
      "expert:warmup:expert_fanout:succeeded",
      "stage:warmup:expert_fanout:succeeded",
      "stage:dialogue:turn:started",
      "step:dialogue:user_turn:succeeded",
      "step:dialogue:moderator_response:started",
      "step:dialogue:moderator_response:succeeded",
      "step:evidence:fact_extraction:succeeded",
      "step:mindmap:update:succeeded",
      "step:suggestions:question_generation:succeeded",
      "stage:report:generate:succeeded",
      "artifact:report:report_md:available"
    )
  )
  expect_equal(questions, "What next?")
  expect_match(report, "Report body")
  expect_equal(
    unique(vapply(event_data, `[[`, character(1), "workflow")),
    "costorm"
  )
  expert_payload <- collector$data(event_type = "expert", status = "started")[[
    1
  ]]$payload
  expect_equal(expert_payload$expert_name, "Dr. Flow")
  expect_null(expert_payload$response)
})

test_that("warmup failure emits failed expert and tool events", {
  skip_if_not_installed("ellmer")
  collector <- tempest_progress_collector(include_payload = TRUE)
  failing_chat <- list(
    chat = function(...) stop("warmup unavailable"),
    register_tools = function(...) invisible(NULL)
  )
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) {
      if (identical(role, "expert")) failing_chat else fake_chat()
    }
  )
  session <- tempest_session(
    "Co-STORM progress",
    config = cfg,
    personas = list(list(
      id = 2,
      name = "Dr. Warmup",
      title = "Warmup specialist",
      perspective = "Warmup progress",
      initial_questions = "Will warmup report failures?"
    )),
    progress = collector$record
  )

  expect_error(session$warmup(verbose = FALSE), "warmup unavailable")

  tool_events <- collector$data(event_type = "tool", stage = "warmup")
  expect_equal(
    vapply(tool_events, `[[`, character(1), "status"),
    c("started", "failed")
  )
  expect_equal(tool_events[[2]]$parent_event_id, tool_events[[1]]$event_id)
  expect_equal(tool_events[[2]]$payload$error_class, "simpleError")

  expert_events <- collector$data(event_type = "expert", stage = "warmup")
  expect_equal(
    vapply(expert_events, `[[`, character(1), "status"),
    c("started", "failed")
  )
  expect_equal(expert_events[[2]]$parent_event_id, expert_events[[1]]$event_id)

  stage_failed <- collector$data(
    event_type = "stage",
    status = "failed",
    stage = "warmup"
  )[[1]]
  expect_equal(stage_failed$payload$error_class, "simpleError")
})

test_that("expert tools emit correlated progress events", {
  skip_if_not_installed("ellmer")
  source <- fake_source(url = "https://example.org/expert-tool")
  store <- SourceStore$new()
  store$upsert_source(source)
  collector <- tempest_progress_collector(include_payload = TRUE)
  expert_chat <- fake_chat(
    text = list(paste0(
      "Expert answer cites source [",
      source$id,
      "]."
    ))
  )
  extractor <- fake_chat(
    structured = list(list(
      facts = list(list(
        claim = "Expert tool progress is observable.",
        sources = list(list(source_id = source$id)),
        confidence = "high"
      ))
    ))
  )
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) {
      if (identical(role, "expert")) expert_chat else fake_chat()
    }
  )
  retriever <- tempest_retriever(config = cfg, store = store)
  mgr <- tempest:::ExpertSessionManager$new(
    cfg,
    retriever,
    extractor = extractor,
    store = store,
    progress = collector$record,
    run_id = "session-1"
  )
  persona <- list(
    id = 7,
    name = "Dr. Tool",
    title = "Tool specialist",
    perspective = "Expert tools"
  )

  tool <- tempest:::tempest_create_expert_tool(persona, mgr, "Progress")
  result <- tool(question = "What should we know?")

  tool_events <- collector$data(event_type = "tool", stage = "dialogue")
  expect_equal(
    vapply(tool_events, `[[`, character(1), "status"),
    c("started", "succeeded")
  )
  expect_equal(tool_events[[1]]$correlation_id, tool_events[[2]]$correlation_id)
  expect_equal(tool_events[[2]]$parent_event_id, tool_events[[1]]$event_id)
  expect_equal(tool_events[[1]]$payload$expert_name, "Dr. Tool")
  expect_null(tool_events[[2]]$payload$response)
  expect_equal(result$expert, "Dr. Tool")

  claims <- store$list_claims()
  expect_length(claims, 1)
  expect_equal(claims[[1]]@session_id, result$session_id)
  expect_equal(claims[[1]]@persona_id, "7")
  expect_equal(claims[[1]]@retrieval_step_id, tool_events[[1]]$correlation_id)
  fact_events <- collector$data(event_type = "step", stage = "evidence")
  fact_events <- Filter(
    function(event) identical(event$step, "fact_extraction"),
    fact_events
  )
  expect_equal(
    unique(vapply(fact_events, `[[`, character(1), "correlation_id")),
    tool_events[[1]]$correlation_id
  )
})

test_that("expert tools reuse sessions and provenance", {
  skip_if_not_installed("ellmer")
  source <- fake_source(url = "https://example.org/expert-reuse")
  store <- SourceStore$new()
  store$upsert_source(source)
  expert_chat <- fake_chat(
    text = list(
      paste0("First expert answer cites [", source$id, "]."),
      paste0("Second expert answer cites [", source$id, "].")
    )
  )
  extractor <- fake_chat(
    structured = list(
      list(
        facts = list(list(
          claim = "First expert claim is recorded.",
          sources = list(list(source_id = source$id)),
          confidence = "high"
        ))
      ),
      list(
        facts = list(list(
          claim = "Second expert claim is recorded.",
          sources = list(list(source_id = source$id)),
          confidence = "high"
        ))
      )
    )
  )
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) {
      if (identical(role, "expert")) expert_chat else fake_chat()
    }
  )
  retriever <- tempest_retriever(config = cfg, store = store)
  mgr <- tempest:::ExpertSessionManager$new(
    cfg,
    retriever,
    extractor = extractor,
    store = store,
    run_id = "session-1"
  )
  persona <- list(id = 7, name = "Dr. Reuse", title = "Tool specialist")
  tool <- tempest:::tempest_create_expert_tool(persona, mgr, "Progress")

  first <- tool(question = "First?")
  second <- tool(question = "Second?", session_id = first$session_id)

  expect_equal(second$session_id, first$session_id)
  expect_length(mgr$list_sessions(), 1)
  expect_equal(length(expert_chat$.calls()), 2)
  claims <- store$list_claims()
  expect_length(claims, 2)
  expect_equal(
    vapply(claims, function(claim) claim@session_id, character(1)),
    rep(first$session_id, 2)
  )
  expect_equal(
    vapply(claims, function(claim) claim@persona_id, character(1)),
    rep("7", 2)
  )
})

test_that("expert tools emit failed progress events", {
  skip_if_not_installed("ellmer")
  store <- SourceStore$new()
  collector <- tempest_progress_collector(include_payload = TRUE)
  failing_chat <- list(
    chat = function(...) stop("expert unavailable"),
    register_tools = function(...) invisible(NULL)
  )
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) {
      if (identical(role, "expert")) failing_chat else fake_chat()
    }
  )
  retriever <- tempest_retriever(config = cfg, store = store)
  mgr <- tempest:::ExpertSessionManager$new(
    cfg,
    retriever,
    progress = collector$record,
    run_id = "session-1"
  )
  persona <- list(id = 1, name = "Dr. Failure", title = "Specialist")
  tool <- tempest:::tempest_create_expert_tool(persona, mgr, "Progress")

  expect_error(tool(question = "Will this fail?"), "expert unavailable")

  failed <- collector$data(event_type = "tool", status = "failed")[[1]]
  expect_equal(failed$payload$expert_name, "Dr. Failure")
  expect_equal(failed$payload$error_class, "simpleError")
})
