# Async Co-STORM operations keep provider work off the Shiny event loop and
# commit only while the originating session generation is still current.

tempest_session_extract_facts_async <- function(
  session,
  text,
  turn = NULL,
  source_ids = NULL,
  session_id = session$session_id,
  persona_id = NA_character_,
  correlation_id = NA_character_,
  is_current = function() TRUE
) {
  event <- session$emit_progress(
    "step",
    "started",
    stage = "evidence",
    step = "fact_extraction",
    correlation_id = correlation_id
  )
  harvested <- if (is.null(source_ids)) {
    tempest_harvest_native_sources_from_turn(turn, session$store)
  } else {
    character()
  }
  request <- tempest_extract_facts_from_answer_async(
    session$chats$extractor,
    text,
    session$store,
    source_ids = unique(c(source_ids, harvested)),
    session_id = session_id,
    persona_id = persona_id,
    retrieval_step_id = correlation_id,
    commit_if = is_current
  )
  promises::then(
    request,
    onFulfilled = function(value) {
      if (!tempest_async_is_current(is_current)) {
        session$emit_progress(
          "step",
          "cancelled",
          stage = "evidence",
          step = "fact_extraction",
          parent_event_id = event@event_id,
          correlation_id = event@correlation_id,
          payload = list(reason = "stale_session")
        )
        return(NULL)
      }
      session$emit_progress(
        "step",
        "succeeded",
        stage = "evidence",
        step = "fact_extraction",
        parent_event_id = event@event_id,
        correlation_id = event@correlation_id,
        payload = list(claim_count = length(session$store$list_claims()))
      )
      value
    },
    onRejected = function(error) {
      session$emit_progress(
        "step",
        "failed",
        stage = "evidence",
        step = "fact_extraction",
        parent_event_id = event@event_id,
        correlation_id = event@correlation_id,
        payload = tempest_progress_error_payload(error)
      )
      stop(error)
    }
  )
}

tempest_session_update_mindmap_async <- function(
  session,
  last_exchange,
  is_current = function() TRUE
) {
  event <- session$emit_progress(
    "step",
    "started",
    stage = "mindmap",
    step = "update"
  )
  prompt <- paste0(
    "Update the research mind map based on the latest exchange.\n\n",
    "Topic: ",
    session$topic,
    "\n\nCurrent mind map:\n",
    tempest_mindmap_to_markdown(session$mindmap),
    "\n\nLatest exchange:\n",
    last_exchange,
    "\n\nRules:\n",
    "- Keep node ids stable where possible.\n",
    "- Add nodes for new subtopics, hypotheses, and open questions.\n",
    "- Add source_ids to nodes when the exchange included citations like [Sxxxxxxxxxxxx].\n",
    "- Do not fabricate sources.\n\n",
    "Return an updated mind map as structured data."
  )
  request <- session$chats$mindmap$chat_structured_async(
    prompt,
    type = tempest_type_mindmap(),
    echo = "none",
    convert = FALSE
  )
  promises::then(
    request,
    onFulfilled = function(mindmap) {
      if (!tempest_async_is_current(is_current)) {
        session$emit_progress(
          "step",
          "cancelled",
          stage = "mindmap",
          step = "update",
          parent_event_id = event@event_id,
          correlation_id = event@correlation_id,
          payload = list(reason = "stale_session")
        )
        return(NULL)
      }
      if (!is.null(mindmap$nodes) && length(mindmap$nodes) > 0L) {
        session$mindmap <- mindmap
        session$artifacts[["mindmap_md"]] <- tempest_mindmap_to_markdown(
          mindmap
        )
      }
      session$emit_progress(
        "step",
        "succeeded",
        stage = "mindmap",
        step = "update",
        parent_event_id = event@event_id,
        correlation_id = event@correlation_id,
        payload = list(node_count = length(session$mindmap$nodes %||% list()))
      )
      invisible(TRUE)
    },
    onRejected = function(error) {
      session$emit_progress(
        "step",
        "failed",
        stage = "mindmap",
        step = "update",
        parent_event_id = event@event_id,
        correlation_id = event@correlation_id,
        payload = tempest_progress_error_payload(error)
      )
      stop(error)
    }
  )
}

tempest_session_suggest_questions_async <- function(
  session,
  n = 4,
  is_current = function() TRUE
) {
  event <- session$emit_progress(
    "step",
    "started",
    stage = "suggestions",
    step = "question_generation"
  )
  context <- if (length(session$transcript) > 0L) {
    session$transcript_markdown(max_turns = 12)
  } else {
    NULL
  }
  request <- tempest_suggest_questions_async(
    topic = session$topic,
    context = context,
    n = n,
    config = session$config
  )
  promises::then(
    request,
    onFulfilled = function(questions) {
      if (!tempest_async_is_current(is_current)) {
        session$emit_progress(
          "step",
          "cancelled",
          stage = "suggestions",
          step = "question_generation",
          parent_event_id = event@event_id,
          correlation_id = event@correlation_id,
          payload = list(reason = "stale_session")
        )
        return(character())
      }
      session$emit_progress(
        "step",
        "succeeded",
        stage = "suggestions",
        step = "question_generation",
        parent_event_id = event@event_id,
        correlation_id = event@correlation_id,
        payload = list(question_count = length(questions))
      )
      questions
    },
    onRejected = function(error) {
      session$emit_progress(
        "step",
        "failed",
        stage = "suggestions",
        step = "question_generation",
        parent_event_id = event@event_id,
        correlation_id = event@correlation_id,
        payload = tempest_progress_error_payload(error)
      )
      stop(error)
    }
  )
}

tempest_session_report_async <- function(
  session,
  style = c("technical", "executive"),
  include_references = TRUE,
  is_current = function() TRUE
) {
  style <- match.arg(style)
  event <- session$emit_progress(
    "stage",
    "started",
    stage = "report",
    step = "generate",
    payload = list(style = style, include_references = include_references)
  )
  plan <- tempest_costorm_report_plan(
    session,
    style,
    include_references,
    generate_text = function(prompt) {
      session$chats$reporter$chat_async(prompt, echo = "none")
    }
  )
  request <- tempest_deliverable_generate(plan)
  completed <- promises::then(
    request,
    onFulfilled = function(body) {
      if (!tempest_async_is_current(is_current)) {
        session$emit_progress(
          "stage",
          "cancelled",
          stage = "report",
          step = "generate",
          parent_event_id = event@event_id,
          correlation_id = event@correlation_id,
          payload = list(reason = "stale_session")
        )
        return(NULL)
      }
      result <- tempest_deliverable_finalize(plan, body)
      artifact <- tempest_deliverable_primary_artifact(result)
      markdown <- artifact@content
      session$artifacts[["report"]] <- body
      session$artifacts[["report_md"]] <- markdown
      session$emit_progress(
        "artifact",
        "available",
        stage = "report",
        step = "report_md",
        parent_event_id = event@event_id,
        correlation_id = event@correlation_id,
        payload = list(artifact = "report_md")
      )
      session$emit_progress(
        "stage",
        "succeeded",
        stage = "report",
        step = "generate",
        parent_event_id = event@event_id,
        correlation_id = event@correlation_id
      )
      markdown
    }
  )
  promises::catch(
    completed,
    onRejected = function(error) {
      session$emit_progress(
        "stage",
        "failed",
        stage = "report",
        step = "generate",
        parent_event_id = event@event_id,
        correlation_id = event@correlation_id,
        payload = tempest_progress_error_payload(error)
      )
      stop(error)
    }
  )
}
