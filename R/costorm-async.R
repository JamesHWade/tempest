# Async Co-STORM operations keep provider work off the Shiny event loop and
# commit only while the originating session generation is still current.

tempest_async_promise_try <- function(callback) {
  tryCatch(
    promises::promise_resolve(callback()),
    error = function(error) promises::promise_reject(error)
  )
}

tempest_session_extract_facts_async <- function(
  session,
  text,
  turn = NULL,
  source_ids = NULL,
  session_id = session$session_id,
  expert_id = NA_character_,
  correlation_id = NA_character_,
  is_current = function() TRUE,
  emit_stale_progress = TRUE
) {
  workspace <- session$workspace %||% session$store %||% NULL
  if (!inherits(workspace, "ResearchWorkspace")) {
    tempest_research_workspace_abort(
      "The Co-STORM session must expose a ResearchWorkspace."
    )
  }
  event <- session$emit_progress(
    "step",
    "started",
    stage = "evidence",
    step = "fact_extraction",
    correlation_id = correlation_id
  )
  harvested <- if (is.null(source_ids)) {
    tempest_harvest_native_sources_from_turn(turn, workspace)
  } else {
    character()
  }
  request <- tempest_async_promise_try(function() {
    tempest_extract_facts_from_answer_async(
      session$chats$extractor,
      text,
      workspace,
      source_ids = unique(c(source_ids, harvested)),
      session_id = session_id,
      expert_id = expert_id,
      retrieval_step_id = correlation_id,
      commit_if = is_current
    )
  })
  promises::then(
    request,
    onFulfilled = function(value) {
      if (!tempest_async_is_current(is_current)) {
        if (isTRUE(emit_stale_progress)) {
          session$emit_progress(
            "step",
            "cancelled",
            stage = "evidence",
            step = "fact_extraction",
            parent_event_id = event@event_id,
            correlation_id = event@correlation_id,
            payload = list(reason = "stale_session")
          )
        }
        return(NULL)
      }
      session$emit_progress(
        "step",
        "succeeded",
        stage = "evidence",
        step = "fact_extraction",
        parent_event_id = event@event_id,
        correlation_id = event@correlation_id,
        payload = list(claim_count = length(workspace$list_claims()))
      )
      value
    },
    onRejected = function(error) {
      if (!tempest_async_is_current(is_current)) {
        if (isTRUE(emit_stale_progress)) {
          session$emit_progress(
            "step",
            "cancelled",
            stage = "evidence",
            step = "fact_extraction",
            parent_event_id = event@event_id,
            correlation_id = event@correlation_id,
            payload = list(reason = "stale_session")
          )
        }
        return(NULL)
      }
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

tempest_session_evidence_counts <- function(session) {
  workspace <- session$workspace %||% session$store %||% NULL
  if (!inherits(workspace, "ResearchWorkspace")) {
    return(list(source_count = 0L, claim_count = 0L))
  }
  list(
    source_count = length(workspace$list_sources()),
    claim_count = length(workspace$list_claims())
  )
}

tempest_session_commit_evidence_async <- function(
  session,
  text,
  turn = NULL,
  source_ids = NULL,
  session_id = session$session_id,
  expert_id = NA_character_,
  correlation_id = NA_character_,
  is_current = function() TRUE,
  emit_stale_progress = TRUE
) {
  tempest_require("promises", "Async evidence commitment requires promises.")
  workspace <- session$workspace %||% session$store %||% NULL
  if (!inherits(workspace, "ResearchWorkspace")) {
    tempest_research_workspace_abort(
      "The Co-STORM session must expose a ResearchWorkspace."
    )
  }
  before <- tempest_session_evidence_counts(session)
  if (!tempest_async_is_current(is_current)) {
    return(promises::promise_resolve(c(
      before,
      list(
        source_ids = character(),
        sources_added = 0L,
        claims_added = 0L,
        cancelled = TRUE
      )
    )))
  }
  if (is.null(source_ids)) {
    source_ids <- if (!is.null(session$harvest_native_sources)) {
      session$harvest_native_sources(turn = turn)
    } else {
      tempest_harvest_native_sources_from_turn(turn, workspace)
    }
  }
  source_ids <- unique(source_ids[!is.na(source_ids) & nzchar(source_ids)])
  source_ids <- tempest_session_answer_source_ids(session, text, source_ids)

  summarize <- function(
    extraction_skipped = NA_character_,
    cancelled = FALSE
  ) {
    after <- tempest_session_evidence_counts(session)
    list(
      source_count = after$source_count,
      claim_count = after$claim_count,
      source_ids = source_ids,
      sources_added = max(0L, after$source_count - before$source_count),
      claims_added = max(0L, after$claim_count - before$claim_count),
      extraction_skipped = extraction_skipped,
      cancelled = cancelled
    )
  }

  if (length(source_ids) == 0L) {
    session$emit_progress(
      "step",
      "skipped",
      stage = "evidence",
      step = "fact_extraction",
      correlation_id = correlation_id,
      payload = list(reason = "no_cited_sources")
    )
    return(promises::promise_resolve(summarize("no_cited_sources")))
  }

  request <- tempest_async_promise_try(function() {
    tempest_session_extract_facts_async(
      session,
      text,
      turn = turn,
      source_ids = source_ids,
      session_id = session_id,
      expert_id = expert_id,
      correlation_id = correlation_id,
      is_current = is_current,
      emit_stale_progress = emit_stale_progress
    )
  })
  promises::then(request, function(...) {
    if (!tempest_async_is_current(is_current)) {
      return(summarize(cancelled = TRUE))
    }
    summarize()
  })
}

tempest_session_update_mindmap_async <- function(
  session,
  last_exchange,
  is_current = function() TRUE,
  emit_stale_progress = TRUE
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
    "- Keep the map concise with no more than 24 nodes.\n",
    "- Add nodes for new subtopics, hypotheses, and open questions.\n",
    "- Add source_ids to nodes when the exchange included citations like [Sxxxxxxxxxxxx].\n",
    "- When the exchange marks content as scoping-only or an evidence gap, add only open-question or gap nodes; do not turn unsupported statements into findings.\n",
    "- Do not fabricate sources.\n\n",
    "Return an updated mind map as structured data."
  )
  request <- tempest_async_promise_try(function() {
    session$chats$mindmap$chat_structured_async(
      prompt,
      type = tempest_type_mindmap(),
      echo = "none",
      convert = FALSE
    )
  })
  promises::then(
    request,
    onFulfilled = function(mindmap) {
      if (!tempest_async_is_current(is_current)) {
        if (isTRUE(emit_stale_progress)) {
          session$emit_progress(
            "step",
            "cancelled",
            stage = "mindmap",
            step = "update",
            parent_event_id = event@event_id,
            correlation_id = event@correlation_id,
            payload = list(reason = "stale_session")
          )
        }
        return(NULL)
      }
      if (!is.null(mindmap$nodes) && length(mindmap$nodes) > 0L) {
        session$mindmap <- mindmap
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
      if (!tempest_async_is_current(is_current)) {
        if (isTRUE(emit_stale_progress)) {
          session$emit_progress(
            "step",
            "cancelled",
            stage = "mindmap",
            step = "update",
            parent_event_id = event@event_id,
            correlation_id = event@correlation_id,
            payload = list(reason = "stale_session")
          )
        }
        return(NULL)
      }
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
  request <- tempest_async_promise_try(function() {
    tempest_suggest_questions_async(
      topic = session$topic,
      context = context,
      n = n,
      config = session$config
    )
  })
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

tempest_session_turn_text <- function(value, arg) {
  if (!is.character(value) || length(value) != 1L || is.na(value)) {
    tempest_abort(
      "{.arg {arg}} must be a single string.",
      class = c("tempest_session_turn_error", "tempest_error")
    )
  }
  value
}

tempest_session_turn_append_notice <- function(state, notice) {
  state$notices <- c(state$notices, list(notice))
  invisible(state)
}

tempest_session_turn_cancel <- function(state) {
  state$cancelled <- TRUE
  state$suggestion_status <- "cancelled"
  state$suggestions <- character()
  invisible(state)
}

#' Process a completed Co-STORM turn asynchronously
#'
#' `r lifecycle::badge("experimental")`
#'
#' Records a completed user and moderator exchange, then asynchronously commits
#' cited evidence, updates the session mind map, and optionally generates
#' follow-up questions. Enrichment failures are returned as typed notices so
#' host applications can choose their own presentation. Stale work is cancelled
#' before it can commit later pipeline stages.
#'
#' @param session A [TempestSession] object.
#' @param user_text Completed user input as a single string.
#' @param assistant_text Completed moderator response as a single string.
#' @param provider_turn Optional process-local provider turn used to harvest
#'   native sources. It is never retained in the result.
#' @param suggest Whether to generate follow-up questions.
#' @param n_suggestions Maximum number of follow-up questions.
#' @param turn_id Optional stable correlation identifier.
#' @param is_current Process-local predicate returning `TRUE` while this work is
#'   allowed to commit. It is never retained in the result.
#' @return A promise resolving to a typed, serializable
#'   `tempest_session_turn_result` object.
#' @export
tempest_session_process_turn_async <- function(
  session,
  user_text,
  assistant_text,
  provider_turn = NULL,
  suggest = TRUE,
  n_suggestions = 4L,
  turn_id = NULL,
  is_current = function() TRUE
) {
  tempest_require("promises", "Async turn processing requires promises.")
  if (!inherits(session, "TempestSession")) {
    tempest_abort(
      "{.arg session} must be a TempestSession.",
      class = c("tempest_session_turn_error", "tempest_error")
    )
  }
  user_text <- tempest_session_turn_text(user_text, "user_text")
  assistant_text <- tempest_session_turn_text(
    assistant_text,
    "assistant_text"
  )
  if (!nzchar(user_text) && !nzchar(assistant_text)) {
    tempest_abort(
      "At least one of {.arg user_text} or {.arg assistant_text} must be non-empty.",
      class = c("tempest_session_turn_error", "tempest_error")
    )
  }
  suggest <- tempest_workflow_flag(suggest, "suggest")
  n_suggestions <- tempest_config_count(n_suggestions, "n_suggestions")
  turn_id <- turn_id %||% tempest_uuid("turn")
  if (!rlang::is_string(turn_id) || !nzchar(tempest_trim(turn_id))) {
    tempest_abort(
      "{.arg turn_id} must be a single non-empty string or {.code NULL}.",
      class = c("tempest_session_turn_error", "tempest_error")
    )
  }
  turn_id <- tempest_trim(turn_id)
  if (!is.function(is_current)) {
    tempest_abort(
      "{.arg is_current} must be a function.",
      class = c("tempest_session_turn_error", "tempest_error")
    )
  }

  state <- new.env(parent = emptyenv())
  state$cancelled <- !tempest_async_is_current(is_current)
  state$evidence_status <- "cancelled"
  state$source_ids <- character()
  state$source_count <- 0L
  state$claim_count <- 0L
  state$sources_added <- 0L
  state$claims_added <- 0L
  state$mindmap_status <- "cancelled"
  state$suggestion_status <- "cancelled"
  state$suggestions <- character()
  state$notices <- list()
  turn_event <- NULL

  finish <- function() {
    counts <- tempest_session_evidence_counts(session)
    state$source_count <- counts$source_count
    state$claim_count <- counts$claim_count
    node_count <- length(session$mindmap$nodes %||% list())
    warning_count <- sum(vapply(
      state$notices,
      \(notice) identical(notice@severity, "warning"),
      logical(1)
    ))
    status <- if (state$cancelled) {
      "cancelled"
    } else if (warning_count > 0L) {
      "partial"
    } else {
      "succeeded"
    }
    if (!is.null(turn_event)) {
      session$emit_progress(
        "stage",
        if (state$cancelled) "cancelled" else "succeeded",
        stage = "dialogue",
        step = "turn",
        parent_event_id = turn_event@event_id,
        correlation_id = turn_id,
        payload = list(
          result_status = status,
          notice_count = length(state$notices)
        )
      )
    }
    tempest_session_turn_result(
      session_id = session$session_id,
      turn_id = turn_id,
      status = status,
      evidence_status = state$evidence_status,
      source_ids = state$source_ids,
      source_count = state$source_count,
      claim_count = state$claim_count,
      sources_added = state$sources_added,
      claims_added = state$claims_added,
      mindmap_status = state$mindmap_status,
      mindmap_node_count = node_count,
      suggestion_status = state$suggestion_status,
      suggestions = state$suggestions,
      notices = state$notices
    )
  }

  if (state$cancelled) {
    return(promises::promise_resolve(finish()))
  }

  turn_event <- session$emit_progress(
    "stage",
    "started",
    stage = "dialogue",
    step = "turn",
    correlation_id = turn_id
  )
  if (nzchar(user_text)) {
    session$add_turn("user", "user", user_text)
    session$emit_progress(
      "step",
      "succeeded",
      stage = "dialogue",
      step = "user_turn",
      parent_event_id = turn_event@event_id,
      correlation_id = turn_id
    )
  }
  if (nzchar(assistant_text)) {
    session$add_turn("Moderator", "assistant", assistant_text)
    session$emit_progress(
      "step",
      "succeeded",
      stage = "dialogue",
      step = "moderator_response",
      parent_event_id = turn_event@event_id,
      correlation_id = turn_id
    )
  }

  evidence <- tempest_async_promise_try(function() {
    tempest_session_commit_evidence_async(
      session,
      assistant_text,
      turn = provider_turn,
      session_id = session$session_id,
      expert_id = "moderator",
      correlation_id = turn_id,
      is_current = is_current
    )
  })
  evidence <- promises::then(
    evidence,
    onFulfilled = function(result) {
      if (
        !tempest_async_is_current(is_current) ||
          isTRUE(result$cancelled %||% FALSE)
      ) {
        tempest_session_turn_cancel(state)
        return(NULL)
      }
      state$source_ids <- result$source_ids %||% character()
      state$source_count <- result$source_count %||% 0L
      state$claim_count <- result$claim_count %||% 0L
      state$sources_added <- result$sources_added %||% 0L
      state$claims_added <- result$claims_added %||% 0L
      if (length(state$source_ids) == 0L) {
        state$evidence_status <- "gap"
        tempest_session_turn_append_notice(
          state,
          tempest_session_turn_notice(
            code = "evidence_gap",
            stage = "evidence",
            message = "The assistant answer cited no inspected source."
          )
        )
      } else {
        state$evidence_status <- "committed"
      }
      result
    },
    onRejected = function(error) {
      if (!tempest_async_is_current(is_current)) {
        tempest_session_turn_cancel(state)
        return(NULL)
      }
      state$evidence_status <- "failed"
      tempest_session_turn_append_notice(
        state,
        tempest_session_turn_error_notice(
          code = "evidence_failed",
          stage = "evidence",
          message = "Evidence processing failed.",
          error = error
        )
      )
      NULL
    }
  )

  mindmap <- promises::then(evidence, function(evidence_result) {
    if (state$cancelled || !tempest_async_is_current(is_current)) {
      tempest_session_turn_cancel(state)
      return(NULL)
    }
    source_ids <- if (identical(state$evidence_status, "committed")) {
      evidence_result$source_ids %||% character()
    } else {
      character()
    }
    request <- tempest_async_promise_try(function() {
      tempest_session_update_mindmap_async(
        session,
        last_exchange = tempest_costorm_mindmap_exchange(
          user_text,
          assistant_text,
          source_ids
        ),
        is_current = is_current
      )
    })
    promises::then(
      request,
      onFulfilled = function(value) {
        if (!tempest_async_is_current(is_current)) {
          tempest_session_turn_cancel(state)
          return(NULL)
        }
        state$mindmap_status <- if (is.null(value)) "unchanged" else "updated"
        value
      },
      onRejected = function(error) {
        if (!tempest_async_is_current(is_current)) {
          tempest_session_turn_cancel(state)
          return(NULL)
        }
        state$mindmap_status <- "failed"
        tempest_session_turn_append_notice(
          state,
          tempest_session_turn_error_notice(
            code = "mindmap_failed",
            stage = "mindmap",
            message = "Mind-map update failed.",
            error = error
          )
        )
        NULL
      }
    )
  })

  suggestions <- promises::then(mindmap, function(...) {
    if (state$cancelled || !tempest_async_is_current(is_current)) {
      tempest_session_turn_cancel(state)
      return(NULL)
    }
    if (!suggest || !nzchar(user_text)) {
      state$suggestion_status <- "skipped"
      return(NULL)
    }
    request <- tempest_async_promise_try(function() {
      tempest_session_suggest_questions_async(
        session,
        n = n_suggestions,
        is_current = is_current
      )
    })
    promises::then(
      request,
      onFulfilled = function(questions) {
        if (!tempest_async_is_current(is_current)) {
          tempest_session_turn_cancel(state)
          return(NULL)
        }
        questions <- tempest_as_character_vector(questions)
        questions <- tempest_trim(questions)
        state$suggestions <- unique(
          questions[!is.na(questions) & nzchar(questions)]
        )
        state$suggestion_status <- if (length(state$suggestions) > 0L) {
          "generated"
        } else {
          "skipped"
        }
        state$suggestions
      },
      onRejected = function(error) {
        if (!tempest_async_is_current(is_current)) {
          tempest_session_turn_cancel(state)
          return(NULL)
        }
        state$suggestion_status <- "failed"
        tempest_session_turn_append_notice(
          state,
          tempest_session_turn_error_notice(
            code = "suggestions_failed",
            stage = "suggestions",
            message = "Suggestion generation failed.",
            error = error
          )
        )
        NULL
      }
    )
  })

  promises::then(suggestions, function(...) {
    if (!tempest_async_is_current(is_current)) {
      tempest_session_turn_cancel(state)
    }
    finish()
  })
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
      session$chats$reporter$chat_async(prompt)
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
