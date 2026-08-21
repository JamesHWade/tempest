test_that("TempestSession emits Co-STORM progress events", {
  skip_if_not_installed("ellmer")
  source <- fake_source(
    url = "https://example.org/costorm-progress",
    title = "Co-STORM source",
    content_text = "Co-STORM progress uses compact event metadata."
  )
  source_id <- source@resource_id
  store <- test_research_workspace()
  store$upsert_retrieved_resource(source)
  collector <- tempest_progress_collector(include_payload = TRUE)
  mindmap <- list(
    nodes = list(list(
      id = "root",
      label = "Co-STORM progress",
      notes = "Progress metadata",
      source_ids = source_id
    )),
    edges = list()
  )
  claim_result <- function(claim) {
    list(
      facts = list(list(
        claim = claim,
        sources = list(list(
          source_id = source_id,
          quote = "Co-STORM progress uses compact event metadata."
        )),
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
            claim_result("Dialogue progress is observable."),
            list(
              status = "supported",
              score = 0.95,
              rationale = "The captured excerpt supports the warmup claim."
            ),
            list(
              status = "supported",
              score = 0.95,
              rationale = "The captured excerpt supports the dialogue claim."
            )
          )
        ))
      }
      if (
        identical(role, "coordinator") &&
          identical(system_prompt, tempest_prompt("question_suggester_system"))
      ) {
        return(fake_chat(
          structured = list(list(
            question = "What next?",
            done = FALSE
          ))
        ))
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
    retriever = tempest_retriever(config = cfg, workspace = store),
    experts = list(test_expert(
      expert_id = "expert.flow",
      name = "Dr. Flow",
      title = "Workflow analyst",
      description = "Progress metadata",
      initial_questions = "How should progress be exposed?"
    )),
    progress = collector$record
  )

  withCallingHandlers(
    session$warmup(verbose = FALSE),
    dsprrr_cache_security_warning = function(condition) {
      invokeRestart("muffleWarning")
    }
  )
  session$step("What should we inspect?")
  questions <- session$suggest_questions(n = 1)
  report <- session$report(include_references = FALSE)
  claims <- session$workspace$list_proposed_claims()
  expect_gt(length(claims), 0L)
  expect_identical(
    unname(vapply(claims, \(claim) claim@session_id, character(1))),
    rep(session$session_id, length(claims))
  )

  event_data <- collector$data()
  session_events <- tempest_execution_events(session)
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
      "expert:warmup:expert_fanout:succeeded",
      "stage:warmup:expert_fanout:succeeded",
      "stage:dialogue:turn:started",
      "step:dialogue:user_turn:succeeded",
      "step:dialogue:moderator_response:succeeded",
      "step:evidence:fact_extraction:succeeded",
      "step:mindmap:update:succeeded",
      "step:suggestions:question_generation:succeeded",
      "stage:report:generate:succeeded",
      "artifact:report:report_md:available"
    )
  )
  expect_equal(questions, "What next?")
  expect_match(report, "Warmup progress is observable")
  expect_identical(tempest_session_report_md(session), report)
  expect_identical(session$manifest@status, "succeeded")
  expect_equal(
    session$mindmap_markdown(),
    tempest:::tempest_mindmap_to_markdown(session$mindmap)
  )
  expect_equal(
    unique(vapply(event_data, `[[`, character(1), "workflow")),
    "costorm"
  )
  expect_length(session_events, length(event_data))
  expect_identical(
    vapply(session_events, `[[`, integer(1), "sequence"),
    seq_along(session_events)
  )
  cursor <- session_events[[2]]$sequence
  expect_identical(
    tempest_execution_events(session, after_sequence = cursor),
    session_events[-seq_len(cursor)]
  )
  expect_null(session$artifacts[["progress_events"]])
  expert_payload <- collector$data(event_type = "expert", status = "started")[[
    1
  ]]$payload
  expect_equal(expert_payload$expert_name, "Dr. Flow")
  expect_null(expert_payload$response)
})

test_that("warmup failure emits a safe failed expert event", {
  skip_if_not_installed("ellmer")
  collector <- tempest_progress_collector(include_payload = TRUE)
  failing_chat <- fake_chat()
  failing_chat$stream <- function(...) stop("warmup unavailable")
  failing_chat$chat <- function(...) stop("warmup unavailable")
  failing_chat$stream_async <- function(...) {
    promises::promise_reject(simpleError("warmup unavailable"))
  }
  failing_chat$chat_async <- function(...) {
    promises::promise_reject(simpleError("warmup unavailable"))
  }
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) {
      if (identical(role, "expert")) failing_chat else fake_chat()
    }
  )
  session <- tempest_session(
    "Co-STORM progress",
    config = cfg,
    experts = list(test_expert(
      expert_id = "expert.warmup",
      name = "Dr. Warmup",
      title = "Warmup specialist",
      description = "Warmup progress",
      initial_questions = "Will warmup report failures?"
    )),
    progress = collector$record
  )

  result <- session$warmup(verbose = FALSE)
  expect_identical(result@status, "succeeded")
  expect_identical(result@failure_count, 1L)
  expect_identical(result@orientations[[1L]]$status, "failed")
  expect_no_match(
    result@orientations[[1L]]$error_message,
    "warmup unavailable",
    fixed = TRUE
  )

  expert_events <- collector$data(event_type = "expert", stage = "warmup")
  expect_equal(
    vapply(expert_events, `[[`, character(1), "status"),
    c("started", "failed")
  )
  expect_equal(expert_events[[2]]$parent_event_id, expert_events[[1]]$event_id)

  stage_complete <- collector$data(
    event_type = "stage",
    status = "succeeded",
    stage = "warmup"
  )[[1]]
  expect_identical(stage_complete$payload$failure_count, 1L)
})
