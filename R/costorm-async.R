# Async Co-STORM operations keep provider work off the Shiny event loop and
# commit only while the originating session generation is still current.

tempest_async_promise_try <- function(callback) {
  tryCatch(
    promises::promise_resolve(callback()),
    error = function(error) promises::promise_reject(error)
  )
}

tempest_session_completion_evidence_binding <- function(session, claim) {
  claim <- tempest_session_agent_completion_assert_claim(
    session,
    claim,
    state = "consumed"
  )
  tempest_session_async_work_assert_completion(
    session,
    claim$completion_id
  )
  trace <- tempest_session_turn_deputy_execution(
    session,
    claim$deputy_execution
  )
  workspace <- session$workspace %||% NULL
  if (!inherits(workspace, "ResearchWorkspace")) {
    tempest_research_workspace_abort(
      "The Co-STORM session must expose a ResearchWorkspace."
    )
  }
  response <- tempest_session_turn_text(claim$response, "response")
  provider_turn <- tempest_agent_completion_provider_turn(claim$provider_turn)
  source_ids <- tempest_harvest_native_sources_from_turn(
    provider_turn,
    workspace
  )
  source_ids <- tempest_session_answer_source_ids(
    session,
    response,
    source_ids
  )
  list(
    completion_id = claim$completion_id,
    response = response,
    provider_turn = provider_turn,
    source_ids = source_ids,
    correlation_id = trace$correlation_id,
    deputy_execution = trace
  )
}

tempest_session_extract_facts_async <- function(
  session,
  claim,
  is_current = function() TRUE,
  emit_stale_progress = TRUE
) {
  binding <- tempest_session_completion_evidence_binding(session, claim)
  workspace <- session$workspace
  event <- session$emit_progress(
    "step",
    "started",
    stage = "evidence",
    step = "fact_extraction",
    correlation_id = binding$correlation_id
  )
  record_stage <- tempest_session_stage_recorder(session)
  request <- tempest_async_promise_try(function() {
    tempest_extract_facts_from_answer_async(
      tempest_session_chat(session, "extractor"),
      binding$response,
      workspace,
      module = tempest_session_programs(session)$extract_claims,
      source_ids = binding$source_ids,
      session_id = session$session_id,
      expert_id = "moderator",
      retrieval_step_id = binding$correlation_id,
      deputy_run_id = binding$deputy_execution$deputy_run_id,
      deputy_session_id = binding$deputy_execution$deputy_session_id,
      parent_run_id = binding$deputy_execution$parent_run_id %||%
        NA_character_,
      delegation_id = binding$deputy_execution$delegation_id %||%
        NA_character_,
      tool_call_id = binding$deputy_execution$tool_call_id %||% NA_character_,
      commit_if = is_current,
      record_stage = record_stage
    )
  })
  tempest_otel_then(
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
        payload = list(claim_count = length(workspace$list_proposed_claims()))
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
      tempest_rethrow_operation(error, class = "tempest_session_error")
    }
  )
}

tempest_session_evidence_counts <- function(session) {
  workspace <- session$workspace %||% NULL
  if (!inherits(workspace, "ResearchWorkspace")) {
    return(list(source_count = 0L, claim_count = 0L))
  }
  list(
    source_count = length(workspace$list_retrieved_sources()),
    claim_count = length(workspace$list_proposed_claims())
  )
}

tempest_session_commit_evidence_async <- function(
  session,
  claim,
  is_current = function() TRUE,
  emit_stale_progress = TRUE
) {
  tempest_require("promises", "Async evidence commitment requires promises.")
  binding <- tempest_session_completion_evidence_binding(session, claim)
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
  source_ids <- binding$source_ids

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
      correlation_id = binding$correlation_id,
      payload = list(reason = "no_cited_sources")
    )
    return(promises::promise_resolve(summarize("no_cited_sources")))
  }

  request <- tempest_async_promise_try(function() {
    tempest_session_extract_facts_async(
      session,
      claim,
      is_current = is_current,
      emit_stale_progress = emit_stale_progress
    )
  })
  tempest_otel_then(request, function(...) {
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
  tempest_agent_completion_text(last_exchange)
  request <- promises::promise_resolve(
    tempest_costorm_mindmap_projection(session)
  )
  tempest_otel_then(
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
        mindmap <- tryCatch(
          tempest_session_mindmap_validate_update(
            mindmap,
            session$workspace
          ),
          error = function(error) {
            session$emit_progress(
              "step",
              "failed",
              stage = "mindmap",
              step = "update",
              parent_event_id = event@event_id,
              correlation_id = event@correlation_id,
              payload = tempest_progress_error_payload(error)
            )
            tempest_rethrow_operation(error, class = "tempest_session_error")
          }
        )
        tempest_session_commit_mindmap(session, mindmap)
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
      tempest_rethrow_operation(error, class = "tempest_session_error")
    }
  )
}

tempest_session_suggest_questions_async <- function(
  session,
  n = 4,
  is_current = function() TRUE
) {
  n <- tempest_config_count(n, "n")
  event <- session$emit_progress(
    "step",
    "started",
    stage = "suggestions",
    step = "question_generation"
  )
  answered <- if (length(session$transcript) > 0L) {
    session$transcript_markdown(max_turns = 12)
  } else {
    "(none yet)"
  }
  facts <- tempest_summarize_facts_for_prompt(
    session$workspace,
    max_items = 60L,
    verified_only = TRUE,
    min_support_score = session$config@min_support_score
  )
  module <- tempest_session_programs(session)$next_question
  chat <- tempest_session_chat(session, "next_question")
  questions <- character()
  run_one <- function(previous, index) {
    tempest_otel_then(previous, function(...) {
      if (!tempest_async_is_current(is_current)) {
        return(NULL)
      }
      request <- tempest_execute_stage_async(
        module,
        chat,
        inputs = list(
          topic = session$topic,
          perspective = paste0(
            "Follow-up research suggestion ",
            index,
            " of ",
            n
          ),
          answered = answered,
          facts = facts
        ),
        context = tempest_stage_context_knowledge_view(
          list(),
          module,
          tempest_session_knowledge_view(session)
        ),
        record_stage = function(record, output = NULL) {
          tempest_session_record_stage(session, record, output)
        },
        is_current = is_current
      )
      tempest_otel_then(request, function(stage_result) {
        output <- tempest_normalize_next_question(stage_result$output)
        if (!isTRUE(output$done) && nzchar(tempest_trim(output$question))) {
          questions <<- unique(c(questions, tempest_trim(output$question)))
        }
        output
      })
    })
  }
  request <- Reduce(
    run_one,
    seq_len(n),
    init = promises::promise_resolve(NULL)
  )
  tempest_otel_then(
    request,
    onFulfilled = function(...) {
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
      questions <- utils::head(questions, n)
      tempest_session_set_suggestions(session, questions)
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
      tempest_rethrow_operation(error, class = "tempest_session_error")
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

tempest_session_turn_deputy_execution <- function(
  session,
  deputy_execution
) {
  trace <- tryCatch(
    tempest_costorm_deputy_trace(
      rlang::duplicate(deputy_execution, shallow = FALSE)
    ),
    error = function(error) {
      tempest_abort(
        paste0(
          "{.arg deputy_execution} must be one exact canonical ",
          "Co-STORM Deputy trace."
        ),
        class = c("tempest_session_turn_error", "tempest_error")
      )
    }
  )
  if (
    !identical(trace$stage, "dialogue") ||
      !identical(trace$role, "moderator") ||
      !identical(trace$status, "complete") ||
      !identical(trace$completion_disposition, "issued")
  ) {
    tempest_abort(
      paste0(
        "{.arg deputy_execution} must be one completed dialogue moderator ",
        "trace."
      ),
      class = c("tempest_session_turn_error", "tempest_error")
    )
  }
  recorded <- tempest_session_deputy_traces(session)
  matches <- vapply(recorded, identical, logical(1), y = trace)
  if (sum(matches) != 1L) {
    tempest_abort(
      paste0(
        "{.arg deputy_execution} must be the exact session-recorded ",
        "terminal trace."
      ),
      class = c("tempest_session_turn_error", "tempest_error")
    )
  }
  trace
}

tempest_session_turn_append_notice <- function(state, notice) {
  state$notices <- c(state$notices, list(notice))
  invisible(state)
}

tempest_session_turn_append_progress_notice <- function(state, error) {
  codes <- vapply(
    state$notices,
    \(notice) notice@code,
    character(1)
  )
  if (!"progress_failed" %in% codes) {
    tempest_session_turn_append_notice(
      state,
      tempest_session_turn_error_notice(
        code = "progress_failed",
        stage = "dialogue",
        message = "The host progress callback failed.",
        error = error
      )
    )
  }
  invisible(state)
}

tempest_session_turn_progress_event <- function(session, state, ...) {
  before <- length(session$events)
  tryCatch(
    session$emit_progress(...),
    error = function(error) {
      if (!inherits(error, "tempest_progress_callback_error")) {
        stop(error)
      }
      tempest_session_turn_append_progress_notice(state, error)
      if (!identical(length(session$events), before + 1L)) {
        tempest_abort(
          "A Co-STORM progress event was not recorded atomically.",
          class = c("tempest_session_turn_error", "tempest_error")
        )
      }
      event <- session$events[[before + 1L]]
      event$sequence <- NULL
      do.call(tempest_progress_event, event)
    }
  )
}

tempest_session_completion_transcript <- function(user_text, assistant_text) {
  turns <- list()
  if (nzchar(user_text)) {
    turns[[length(turns) + 1L]] <- list(
      speaker = "user",
      role = "user",
      text = user_text,
      at = tempest_now_utc()
    )
  }
  if (nzchar(assistant_text)) {
    turns[[length(turns) + 1L]] <- list(
      speaker = "Moderator",
      role = "assistant",
      text = assistant_text,
      at = tempest_now_utc()
    )
  }
  turns
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
#' @param completion_id Opaque, process-local completion identifier returned by
#'   `session$request_completion_async()`.
#' @param suggest Whether to generate follow-up questions.
#' @param n_suggestions Maximum number of follow-up questions.
#' @param is_current Process-local predicate returning `TRUE` while this work is
#'   allowed to commit. It is never retained in the result.
#' @return A promise resolving to a typed, serializable
#'   `tempest_session_turn_result` object.
#' @export
tempest_session_process_turn_async <- function(
  session,
  completion_id,
  suggest = TRUE,
  n_suggestions = 4L,
  is_current = function() TRUE
) {
  tempest_require("promises", "Async turn processing requires promises.")
  if (!inherits(session, "TempestSession")) {
    tempest_abort(
      "{.arg session} must be a TempestSession.",
      class = c("tempest_session_turn_error", "tempest_error")
    )
  }
  tempest_session_assert_mutable(session, "process a dialogue turn")
  suggest <- tempest_product_flag(suggest, "suggest")
  n_suggestions <- tempest_config_count(n_suggestions, "n_suggestions")
  if (!is.function(is_current)) {
    tempest_abort(
      "{.arg is_current} must be a function.",
      class = c("tempest_session_turn_error", "tempest_error")
    )
  }
  otel_context <- tempest_otel_provider_call(
    tempest_otel_context_start("costorm.turn.commit")
  )
  otel_callback_context <- otel_context %||%
    tempest_otel_no_context_sentinel
  execute <- function() {
    .tempest_otel_context <- otel_callback_context
    claim <- tempest_session_agent_completion_claim(session, completion_id)
    prepared <- tryCatch(
      {
        user_text <- tempest_session_turn_text(claim$prompt, "prompt")
        assistant_text <- tempest_session_turn_text(
          claim$response,
          "response"
        )
        if (!nzchar(user_text) && !nzchar(assistant_text)) {
          tempest_abort(
            "An agent completion must contain a prompt or response.",
            class = c("tempest_session_turn_error", "tempest_error")
          )
        }
        deputy_execution <- tempest_session_turn_deputy_execution(
          session,
          claim$deputy_execution
        )
        list(
          user_text = user_text,
          assistant_text = assistant_text,
          provider_turn = claim$provider_turn,
          deputy_execution = deputy_execution,
          turn_id = deputy_execution$correlation_id,
          transcript = tempest_session_completion_transcript(
            user_text,
            assistant_text
          )
        )
      },
      error = function(error) {
        tempest_session_agent_completion_release(session, claim)
        stop(error)
      }
    )
    user_text <- prepared$user_text
    assistant_text <- prepared$assistant_text
    provider_turn <- prepared$provider_turn
    deputy_execution <- prepared$deputy_execution
    turn_id <- prepared$turn_id
    transcript <- prepared$transcript
    work_id <- tempest_session_async_work_start(
      session,
      "dialogue",
      work_id = paste0("turn-", completion_id)
    )
    promise_owns_work <- FALSE
    on.exit(
      {
        if (!promise_owns_work) {
          tempest_session_async_work_finish(session, work_id)
        }
      },
      add = TRUE
    )
    finalize_work <- function(promise) {
      promise_owns_work <<- TRUE
      tempest_otel_finally(
        promise,
        function() tempest_session_async_work_finish(session, work_id)
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
        tempest_session_turn_progress_event(
          session,
          state,
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
        warning_count <- sum(vapply(
          state$notices,
          \(notice) identical(notice@severity, "warning"),
          logical(1)
        ))
        if (!state$cancelled && warning_count > 0L) {
          status <- "partial"
        }
      }
      tempest_session_turn_result(
        session_id = session$session_id,
        turn_id = turn_id,
        deputy_run_id = deputy_execution$deputy_run_id,
        deputy_session_id = deputy_execution$deputy_session_id,
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
      tempest_session_agent_completion_cancel(session, completion_id)
      return(finalize_work(promises::promise_resolve(finish())))
    }

    tryCatch(
      tempest_session_agent_completion_consume(session, claim),
      error = function(error) {
        try(
          tempest_session_agent_completion_release(session, claim),
          silent = TRUE
        )
        stop(error)
      }
    )

    tempest_session_commit_transcript(
      session,
      c(session$transcript, transcript)
    )
    turn_event <- tempest_session_turn_progress_event(
      session,
      state,
      "stage",
      "started",
      stage = "dialogue",
      step = "turn",
      correlation_id = turn_id
    )
    if (nzchar(user_text)) {
      tempest_session_turn_progress_event(
        session,
        state,
        "step",
        "succeeded",
        stage = "dialogue",
        step = "user_turn",
        parent_event_id = turn_event@event_id,
        correlation_id = turn_id
      )
    }
    if (nzchar(assistant_text)) {
      tempest_session_turn_progress_event(
        session,
        state,
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
        claim,
        is_current = is_current
      )
    })
    evidence <- tempest_otel_then(
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
        tempest_rethrow_dsprrr_contract(error)
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

    mindmap <- tempest_otel_then(evidence, function(evidence_result) {
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
      tempest_otel_then(
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

    suggestions <- tempest_otel_then(mindmap, function(...) {
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
      tempest_otel_then(
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

    completed <- tempest_otel_then(suggestions, function(...) {
      if (!tempest_async_is_current(is_current)) {
        tempest_session_turn_cancel(state)
      }
      finish()
    })
    finalize_work(completed)
  }
  tempest_otel_trace_promise(
    "costorm.turn.commit",
    execute(),
    context = otel_context
  )
}

tempest_session_report_progress_event <- function(session, ...) {
  before <- length(session$events)
  tryCatch(
    session$emit_progress(...),
    error = function(error) {
      if (!inherits(error, "tempest_progress_callback_error")) {
        stop(error)
      }
      if (!identical(length(session$events), before + 1L)) {
        tempest_costorm_session_abort(
          "A Co-STORM report progress event was not recorded atomically."
        )
      }
      event <- session$events[[before + 1L]]
      event$sequence <- NULL
      do.call(tempest_progress_event, event)
    }
  )
}

tempest_session_report_async <- function(
  session,
  style = c("technical", "executive"),
  include_references = TRUE,
  is_current = function() TRUE
) {
  tempest_require("promises", "Async report generation requires promises.")
  if (!inherits(session, "TempestSession")) {
    tempest_costorm_session_abort(
      "{.arg session} must be a TempestSession."
    )
  }
  if (!is.function(is_current)) {
    tempest_costorm_session_abort("{.arg is_current} must be a function.")
  }
  style <- match.arg(style)
  include_references <- tempest_product_flag(
    include_references,
    "include_references"
  )
  tempest_session_assert_mutable(session, "generate a report")
  tempest_costorm_report_assert_quiescent(session)
  workspace <- session$workspace
  publication_owner <- tempest_session_verification_owner_token(session)
  otel_context <- tempest_otel_provider_call(
    tempest_otel_context_start("costorm.report")
  )
  otel_callback_context <- otel_context %||%
    tempest_otel_no_context_sentinel
  execute <- function() {
    .tempest_otel_context <- otel_callback_context
    tempest_research_workspace_publication_lock(
      workspace,
      publication_owner
    )
    lock_owned <- TRUE
    release_publication_lock <- function() {
      if (
        lock_owned &&
          identical(
            tempest_research_workspace_mutation_state(workspace),
            "publication_locked"
          )
      ) {
        tempest_research_workspace_publication_release(
          workspace,
          publication_owner
        )
      }
      lock_owned <<- FALSE
      invisible(NULL)
    }
    work_id <- tryCatch(
      tempest_session_async_work_start(session, "report"),
      error = function(error) {
        release_publication_lock()
        stop(error)
      }
    )
    promise_owns_work <- FALSE
    on.exit(
      {
        if (!promise_owns_work) {
          tempest_session_async_work_finish(session, work_id)
          release_publication_lock()
        }
      },
      add = TRUE
    )
    event <- tempest_session_report_progress_event(
      session,
      "stage",
      "started",
      stage = "report",
      step = "generate",
      payload = list(style = style, include_references = include_references)
    )
    finish_work <- function(promise) {
      promise_owns_work <<- TRUE
      tempest_otel_finally(
        promise,
        function() {
          tempest_session_async_work_finish(session, work_id)
          release_publication_lock()
        }
      )
    }
    observe_progress <- function(...) {
      tempest_session_report_progress_event(session, ...)
    }
    if (!tempest_async_is_current(is_current)) {
      observe_progress(
        "stage",
        "cancelled",
        stage = "report",
        step = "generate",
        parent_event_id = event@event_id,
        correlation_id = event@correlation_id,
        payload = list(reason = "stale_session")
      )
      return(finish_work(promises::promise_resolve(NULL)))
    }
    request <- tempest_async_promise_try(function() {
      tempest_costorm_report_verify_async(session, is_current)
    })
    request <- tempest_otel_then(request, function(...) {
      tempest_costorm_report_finalize(
        session,
        style,
        include_references,
        is_current,
        work_id
      )
    })
    completed <- tempest_otel_then(
      request,
      onFulfilled = function(markdown) {
        if (is.null(markdown)) {
          observe_progress(
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
        observe_progress(
          "artifact",
          "available",
          stage = "report",
          step = "report_md",
          parent_event_id = event@event_id,
          correlation_id = event@correlation_id,
          payload = list(artifact = "report_md", published = TRUE)
        )
        observe_progress(
          "stage",
          "succeeded",
          stage = "report",
          step = "generate",
          parent_event_id = event@event_id,
          correlation_id = event@correlation_id,
          payload = list(published = TRUE)
        )
        markdown
      }
    )
    completed <- tempest_otel_catch(
      completed,
      onRejected = function(error) {
        if (!tempest_async_is_current(is_current)) {
          observe_progress(
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
        observe_progress(
          "stage",
          "failed",
          stage = "report",
          step = "generate",
          parent_event_id = event@event_id,
          correlation_id = event@correlation_id,
          payload = tempest_progress_error_payload(error)
        )
        tempest_rethrow_operation(error, class = "tempest_session_error")
      }
    )
    finish_work(completed)
  }
  tempest_otel_trace_promise(
    "costorm.report",
    execute(),
    context = otel_context
  )
}
