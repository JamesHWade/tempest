# Co-STORM session state accessors, async work, and completion operations

tempest_session_private <- function(session) {
  if (!inherits(session, "TempestSession")) {
    tempest_costorm_session_abort(
      "{.arg session} must be a TempestSession."
    )
  }
  session$.__enclos_env__$private
}

tempest_session_assert_mutable <- function(
  session,
  action = "mutate",
  allow_report_work = FALSE
) {
  if (!inherits(session, "TempestSession")) {
    tempest_costorm_session_abort(
      "{.arg session} must be a TempestSession."
    )
  }
  report_md <- tempest_session_private(session)$report_md_value
  if (
    !identical(tempest_session_manifest(session)@status, "running") ||
      !is.null(report_md)
  ) {
    tempest_costorm_session_abort(
      paste0(
        "Cannot ",
        action,
        " after the Co-STORM product has been finalized."
      )
    )
  }
  allow_report_work <- tempest_product_flag(
    allow_report_work,
    "allow_report_work"
  )
  active <- tempest_session_async_work_active(session)
  report_active <- any(vapply(
    active,
    \(work) identical(work$kind, "report"),
    logical(1)
  ))
  if (report_active && !allow_report_work) {
    tempest_session_async_work_abort(
      paste0(
        "Cannot ",
        action,
        " while Co-STORM report publication owns the session."
      )
    )
  }
  invisible(session)
}

tempest_session_append_transcript <- function(session, speaker, role, text) {
  tempest_session_assert_mutable(session, "append transcript state")
  speaker <- tempest_product_scalar(speaker, "speaker")
  role <- tempest_research_manifest_choice(
    role,
    "role",
    c("user", "assistant")
  )
  text <- tempest_agent_completion_text(text)
  transcript <- c(
    tempest_session_private(session)$transcript_value,
    list(list(
      speaker = speaker,
      role = role,
      text = text,
      at = tempest_now_utc()
    ))
  )
  private <- tempest_session_private(session)
  private$transcript_value <-
    tempest_session_transcript_record(transcript, action = "snapshot")
  invisible(session)
}

tempest_session_commit_transcript <- function(session, transcript) {
  tempest_session_assert_mutable(session, "commit transcript state")
  transcript <- tempest_session_transcript_record(
    transcript,
    action = "snapshot"
  )
  private <- tempest_session_private(session)
  private$transcript_value <-
    rlang::duplicate(transcript, shallow = FALSE)
  invisible(session)
}

tempest_session_commit_mindmap <- function(session, mindmap) {
  tempest_session_assert_mutable(session, "commit the mind map projection")
  mindmap <- tempest_session_mindmap_validate_update(
    mindmap,
    tempest_session_workspace(session)
  )
  private <- tempest_session_private(session)
  private$mindmap_value <-
    rlang::duplicate(mindmap, shallow = FALSE)
  invisible(session)
}

tempest_session_restore_product_state <- function(
  session,
  title,
  transcript,
  mindmap,
  events,
  progress
) {
  if (!inherits(session, "TempestSession")) {
    tempest_costorm_session_abort(
      "Product state restoration requires a TempestSession."
    )
  }
  title <- tempest_report_title_validate(title)
  transcript <- tempest_session_transcript_record(
    transcript,
    action = "restore"
  )
  mindmap <- tryCatch(
    tempest_session_mindmap_validate_update(
      mindmap,
      tempest_session_workspace(session)
    ),
    error = function(error) {
      tempest_session_restore_abort(
        "Cannot restore the validated Co-STORM mind map.",
        parent = error
      )
    }
  )
  events <- tempest_session_restore_progress_events(
    events,
    session_id = session$session_id,
    action = "restore"
  )
  progress <- tempest_progress_callback(progress)
  private <- tempest_session_private(session)
  private$title_value <- title
  private$transcript_value <- rlang::duplicate(transcript, shallow = FALSE)
  private$mindmap_value <- rlang::duplicate(mindmap, shallow = FALSE)
  private$events_value <- rlang::duplicate(events, shallow = FALSE)
  private$progress_value <- progress
  if (identical(tempest_session_manifest(session)@status, "succeeded")) {
    tempest_research_workspace_seal(
      tempest_session_workspace(session),
      tempest_session_verification_owner_token(session)
    )
  }
  invisible(session)
}

tempest_session_set_progress <- function(session, progress) {
  tempest_session_assert_mutable(session, "replace the progress callback")
  private <- tempest_session_private(session)
  private$progress_value <-
    tempest_progress_callback(progress)
  invisible(session)
}

tempest_session_chat <- function(session, role) {
  if (!inherits(session, "TempestSession")) {
    tempest_costorm_session_abort(
      "{.arg session} must be a TempestSession."
    )
  }
  role <- tempest_research_manifest_choice(
    role,
    "role",
    c("moderator", "extractor", "next_question")
  )
  tempest_session_private(session)$chats_value[[role]]
}

tempest_session_expert_manager <- function(session) {
  if (!inherits(session, "TempestSession")) {
    tempest_costorm_session_abort(
      "{.arg session} must be a TempestSession."
    )
  }
  manager <- tempest_session_private(session)$expert_manager_value
  if (!inherits(manager, "TempestDeputyExpertManager")) {
    tempest_costorm_session_abort(
      "The Co-STORM Deputy expert manager is unavailable."
    )
  }
  manager
}

tempest_session_retired_expert_ids <- function(session) {
  tempest_session_expert_manager(session)$list_retired_expert_ids()
}

tempest_session_suggestions <- function(session) {
  if (!inherits(session, "TempestSession")) {
    tempest_costorm_session_abort(
      "{.arg session} must be a TempestSession."
    )
  }
  rlang::duplicate(
    tempest_session_private(session)$suggestions_value,
    shallow = FALSE
  )
}

tempest_session_set_suggestions <- function(session, suggestions) {
  if (!inherits(session, "TempestSession")) {
    tempest_costorm_session_abort(
      "{.arg session} must be a TempestSession."
    )
  }
  suggestions <- tempest_suggested_questions_validate(suggestions)
  private <- tempest_session_private(session)
  private$suggestions_value <- suggestions
  invisible(session)
}

tempest_session_async_work_abort <- function(message) {
  tempest_abort(
    message,
    class = c(
      "tempest_session_async_work_error",
      "tempest_session_error",
      "tempest_error"
    )
  )
}

tempest_session_async_work_registry <- function(session) {
  if (!inherits(session, "TempestSession")) {
    tempest_session_async_work_abort(
      "Async product work requires a TempestSession."
    )
  }
  registry <- tempest_session_private(session)$async_work_value
  if (!is.environment(registry)) {
    tempest_session_async_work_abort(
      "The session async-work registry is unavailable."
    )
  }
  registry
}

tempest_session_async_work_start <- function(
  session,
  kind,
  work_id = tempest_uuid("session-work"),
  state = "active"
) {
  kind <- tempest_research_manifest_choice(
    kind,
    "kind",
    c("dialogue", "warmup", "report")
  )
  state <- tempest_research_manifest_choice(
    state,
    "state",
    c("queued", "active")
  )
  work_id <- tempest_research_manifest_id(work_id, "work_id")
  tempest_session_async_work_assert_startable(session, kind)
  registry <- tempest_session_async_work_registry(session)
  if (exists(work_id, registry, inherits = FALSE)) {
    tempest_session_async_work_abort(
      "A session async-work identifier cannot be reused."
    )
  }
  assign(
    work_id,
    list(kind = kind, state = state),
    envir = registry
  )
  work_id
}

tempest_session_async_work_finish <- function(session, work_id) {
  work_id <- tempest_research_manifest_id(work_id, "work_id")
  registry <- tempest_session_async_work_registry(session)
  if (exists(work_id, registry, inherits = FALSE)) {
    rm(list = work_id, envir = registry)
  }
  invisible(work_id)
}

tempest_session_async_work_active <- function(session) {
  registry <- tempest_session_async_work_registry(session)
  ids <- sort(ls(registry, all.names = TRUE), method = "radix")
  unname(lapply(ids, function(work_id) {
    c(list(work_id = work_id), get(work_id, registry, inherits = FALSE))
  }))
}

tempest_session_async_work_assert_startable <- function(session, kind) {
  kind <- tempest_research_manifest_choice(
    kind,
    "kind",
    c("dialogue", "warmup", "report")
  )
  active <- tempest_session_async_work_active(session)
  active_kinds <- vapply(active, `[[`, character(1), "kind")
  if (
    (identical(kind, "report") && length(active) > 0L) ||
      (length(active) > 0L && "report" %in% active_kinds)
  ) {
    tempest_session_async_work_abort(
      "Co-STORM report publication requires exclusive session work."
    )
  }
  invisible(NULL)
}

tempest_session_async_work_assert_exclusive <- function(
  session,
  work_id,
  kind
) {
  work_id <- tempest_research_manifest_id(work_id, "work_id")
  kind <- tempest_research_manifest_choice(
    kind,
    "kind",
    c("dialogue", "warmup", "report")
  )
  active <- tempest_session_async_work_active(session)
  valid <- length(active) == 1L &&
    identical(active[[1L]]$work_id, work_id) &&
    identical(active[[1L]]$kind, kind) &&
    identical(active[[1L]]$state, "active")
  if (!valid) {
    tempest_session_async_work_abort(
      "Co-STORM terminal publication lost exclusive session ownership."
    )
  }
  invisible(NULL)
}

tempest_session_async_work_assert_completion <- function(
  session,
  completion_id
) {
  context <- tempest_session_agent_completion_context(session)
  completion_id <- tempest_agent_completion_assert_id(
    context$registry,
    completion_id
  )
  work_id <- paste0("turn-", completion_id)
  active <- tempest_session_async_work_active(session)
  matches <- vapply(
    active,
    function(work) {
      identical(work$work_id, work_id) &&
        identical(work$kind, "dialogue") &&
        identical(work$state, "active")
    },
    logical(1)
  )
  if (sum(matches) != 1L) {
    tempest_session_async_work_abort(
      "Agent-derived evidence requires its active dialogue work owner."
    )
  }
  invisible(work_id)
}

tempest_session_async_work_assert_quiescent <- function(session) {
  if (length(tempest_session_async_work_active(session)) > 0L) {
    tempest_session_async_work_abort(
      "Co-STORM product work remains queued or active."
    )
  }
  invisible(NULL)
}

#' @keywords internal
tempest_session_verification_owner_token <- function(session) {
  if (!inherits(session, "TempestSession")) {
    tempest_costorm_session_abort(
      "Verification ownership requires a TempestSession."
    )
  }
  token <- tempest_session_private(session)$verification_owner_token_value
  if (!is.environment(token)) {
    tempest_costorm_session_abort(
      "TempestSession verification ownership is not bound."
    )
  }
  token
}

#' @keywords internal
tempest_session_emit_progress <- function(session, ...) {
  if (!inherits(session, "TempestSession")) {
    tempest_costorm_session_abort("{.arg session} must be a TempestSession.")
  }
  tempest_session_private(session)$emit_progress(...)
}

tempest_session_request_completion_async <- function(session, ...) {
  if (!inherits(session, "TempestSession")) {
    tempest_costorm_session_abort("{.arg session} must be a TempestSession.")
  }
  tempest_session_private(session)$request_completion_async(...)
}

tempest_session_mindmap_markdown <- function(session, ...) {
  if (!inherits(session, "TempestSession")) {
    tempest_costorm_session_abort("{.arg session} must be a TempestSession.")
  }
  tempest_session_private(session)$mindmap_markdown(...)
}

tempest_session_transcript_markdown <- function(session, ...) {
  if (!inherits(session, "TempestSession")) {
    tempest_costorm_session_abort("{.arg session} must be a TempestSession.")
  }
  tempest_session_private(session)$transcript_markdown(...)
}

tempest_session_active_experts <- function(session) {
  if (!inherits(session, "TempestSession")) {
    tempest_costorm_session_abort("{.arg session} must be a TempestSession.")
  }
  tempest_session_private(session)$get_active_experts()
}

tempest_session_workspace <- function(session) {
  if (!inherits(session, "TempestSession")) {
    tempest_costorm_session_abort(
      "{.arg session} must be a TempestSession."
    )
  }
  tempest_session_private(session)$workspace_value
}

tempest_session_config <- function(session) {
  if (!inherits(session, "TempestSession")) {
    tempest_costorm_session_abort(
      "{.arg session} must be a TempestSession."
    )
  }
  tempest_session_private(session)$config_value
}

tempest_session_manifest <- function(session) {
  if (!inherits(session, "TempestSession")) {
    tempest_costorm_session_abort(
      "{.arg session} must be a TempestSession."
    )
  }
  tempest_session_private(session)$manifest_value
}

tempest_session_retriever <- function(session) {
  if (!inherits(session, "TempestSession")) {
    tempest_costorm_session_abort(
      "{.arg session} must be a TempestSession."
    )
  }
  tempest_session_private(session)$retriever_value
}

tempest_session_title <- function(session) {
  if (!inherits(session, "TempestSession")) {
    tempest_costorm_session_abort(
      "{.arg session} must be a TempestSession."
    )
  }
  tempest_session_private(session)$title_value
}

tempest_session_progress <- function(session) {
  if (!inherits(session, "TempestSession")) {
    tempest_costorm_session_abort(
      "{.arg session} must be a TempestSession."
    )
  }
  tempest_session_private(session)$progress_value
}

tempest_session_events <- function(session) {
  if (!inherits(session, "TempestSession")) {
    tempest_costorm_session_abort(
      "{.arg session} must be a TempestSession."
    )
  }
  tempest_session_private(session)$events_value
}

tempest_session_program_set <- function(session) {
  if (!inherits(session, "TempestSession")) {
    tempest_costorm_session_abort(
      "{.arg session} must be a TempestSession."
    )
  }
  tempest_session_private(session)$program_set_value
}

#' @keywords internal
tempest_session_programs <- function(session) {
  if (!inherits(session, "TempestSession")) {
    tempest_costorm_session_abort(
      "{.arg session} must be a TempestSession."
    )
  }
  tempest_session_private(session)$programs_value
}


tempest_session_stage_records <- function(session) {
  if (!inherits(session, "TempestSession")) {
    tempest_costorm_session_abort(
      "{.arg session} must be a TempestSession."
    )
  }
  records <- tempest_stage_records_validate(
    tempest_session_private(session)$stage_records_value
  )
  rlang::duplicate(records, shallow = FALSE)
}

tempest_session_deputy_traces <- function(session) {
  if (!inherits(session, "TempestSession")) {
    tempest_costorm_session_abort(
      "{.arg session} must be a TempestSession."
    )
  }
  traces <- tempest_costorm_deputy_traces(
    tempest_session_private(session)$deputy_traces_value
  )
  rlang::duplicate(traces, shallow = FALSE)
}

tempest_session_pending_deputy_runs <- function(session) {
  if (!inherits(session, "TempestSession")) {
    tempest_costorm_session_abort(
      "{.arg session} must be a TempestSession."
    )
  }
  pending_runs <- tempest_costorm_pending_deputy_runs(
    tempest_session_private(session)$pending_deputy_runs_value
  )
  rlang::duplicate(pending_runs, shallow = FALSE)
}

tempest_session_agent_completion_context <- function(session) {
  if (!inherits(session, "TempestSession")) {
    tempest_agent_completion_binding_abort()
  }
  private <- tempest_session_private(session)
  owner <- private$agent_completion_owner_value
  registry <- private$agent_completion_registry_value
  if (!is.environment(owner)) {
    tempest_agent_completion_binding_abort()
  }
  tempest_agent_completion_assert_owner(registry, owner)
  list(registry = registry, owner = owner)
}

tempest_session_agent_completion_status <- function(
  session,
  completion_id
) {
  context <- tempest_session_agent_completion_context(session)
  tempest_agent_completion_status(
    context$registry,
    completion_id,
    context$owner
  )
}

tempest_session_agent_completion_claim <- function(session, completion_id) {
  context <- tempest_session_agent_completion_context(session)
  tempest_agent_completion_claim(
    context$registry,
    completion_id,
    context$owner
  )
}

tempest_session_agent_completion_assert_claim <- function(
  session,
  claim,
  state = "processing"
) {
  context <- tempest_session_agent_completion_context(session)
  tempest_agent_completion_assert_claim(
    context$registry,
    claim,
    context$owner,
    state = state
  )$claim
}

tempest_session_agent_completion_release <- function(session, claim) {
  context <- tempest_session_agent_completion_context(session)
  tempest_agent_completion_release(
    context$registry,
    claim,
    context$owner
  )
}

tempest_session_agent_completion_consume <- function(session, claim) {
  context <- tempest_session_agent_completion_context(session)
  tempest_agent_completion_consume(
    context$registry,
    claim,
    context$owner
  )
}

tempest_session_agent_completion_cancel <- function(
  session,
  completion_id
) {
  context <- tempest_session_agent_completion_context(session)
  tempest_agent_completion_cancel(
    context$registry,
    completion_id,
    context$owner
  )
}

tempest_session_agent_completion_active <- function(session) {
  context <- tempest_session_agent_completion_context(session)
  tempest_agent_completion_active(context$registry)
}

tempest_session_agent_completion_assert_quiescent <- function(session) {
  context <- tempest_session_agent_completion_context(session)
  tempest_agent_completion_assert_quiescent(context$registry)
}

tempest_session_match_pending_deputy_run <- function(
  session,
  record,
  expected_fields
) {
  if (
    !inherits(session, "TempestSession") ||
      !is.list(record) ||
      is.data.frame(record) ||
      !identical(names(record), expected_fields)
  ) {
    tempest_agent_completion_record_abort()
  }
  context <- tempest_session_agent_completion_context(session)
  pending_runs <- tempest_session_pending_deputy_runs(session)
  completion_id <- record$completion_id
  ids <- vapply(pending_runs, `[[`, character(1), "completion_id")
  index <- which(ids == completion_id)
  trace <- tryCatch(
    tempest_costorm_deputy_trace(record$deputy_execution),
    error = function(error) NULL
  )
  if (length(index) != 1L || is.null(trace)) {
    tempest_agent_completion_record_abort()
  }
  pending_run <- pending_runs[[index]]
  identity_fields <- setdiff(names(pending_run), "completion_id")
  if (!identical(trace[identity_fields], pending_run[identity_fields])) {
    tempest_agent_completion_record_abort()
  }
  list(
    context = context,
    pending_runs = pending_runs,
    index = index,
    completion_id = completion_id,
    trace = trace
  )
}

tempest_session_drop_pending_deputy_run <- function(
  session,
  pending_runs,
  index
) {
  private <- tempest_session_private(session)
  private$pending_deputy_runs_value <-
    tempest_costorm_pending_deputy_runs(pending_runs[-index])
  invisible(NULL)
}

tempest_session_settle_agent_completion <- function(session, completion) {
  expected <- c(
    "completion_id",
    "prompt",
    "response",
    "provider_turn",
    "deputy_execution"
  )
  matched <- tempest_session_match_pending_deputy_run(
    session,
    completion,
    expected
  )
  trace <- matched$trace
  if (
    !identical(trace$status, "complete") ||
      !identical(trace$completion_disposition, "issued")
  ) {
    tempest_agent_completion_record_abort()
  }

  recorded <- tryCatch(
    {
      tempest_agent_completion_issue(
        matched$context$registry,
        completion_id = matched$completion_id,
        prompt = completion$prompt,
        response = completion$response,
        provider_turn = completion$provider_turn,
        deputy_execution = trace
      )
      tempest_session_record_deputy_trace(session, trace)
      TRUE
    },
    error = function(error) FALSE
  )
  if (!recorded) {
    tempest_agent_completion_rollback_issue(
      matched$context$registry,
      matched$completion_id,
      matched$context$owner
    )
    tempest_agent_completion_record_abort()
  }
  tempest_session_drop_pending_deputy_run(
    session,
    matched$pending_runs,
    matched$index
  )
  invisible(matched$completion_id)
}

tempest_session_settle_agent_terminal <- function(session, terminal) {
  expected <- c("completion_id", "deputy_execution", "disposition")
  matched <- tempest_session_match_pending_deputy_run(
    session,
    terminal,
    expected
  )
  trace <- matched$trace
  disposition_valid <- rlang::is_string(terminal$disposition) &&
    identical(trace$completion_disposition %||% NULL, terminal$disposition) &&
    ((!identical(trace$status %||% NULL, "complete") &&
      identical(terminal$disposition, "terminal")) ||
      (identical(trace$status %||% NULL, "complete") &&
        identical(terminal$disposition, "discarded")))
  id_available <- tryCatch(
    {
      tempest_agent_completion_assert_id(
        matched$context$registry,
        matched$completion_id,
        must_exist = FALSE
      )
      TRUE
    },
    error = function(error) FALSE
  )
  if (!disposition_valid || !id_available) {
    tempest_agent_completion_record_abort()
  }
  recorded <- tryCatch(
    {
      tempest_session_record_deputy_trace(session, trace)
      TRUE
    },
    error = function(error) FALSE
  )
  if (!recorded) {
    tempest_agent_completion_record_abort()
  }
  tempest_session_drop_pending_deputy_run(
    session,
    matched$pending_runs,
    matched$index
  )
  invisible(matched$completion_id)
}

tempest_session_start_deputy_run <- function(session, pending_run) {
  if (!inherits(session, "TempestSession")) {
    tempest_costorm_session_abort(
      "{.arg session} must be a TempestSession."
    )
  }
  pending_run <- tempest_costorm_pending_deputy_run(
    rlang::duplicate(pending_run, shallow = FALSE)
  )
  pending_runs <- tempest_session_pending_deputy_runs(session)
  ids <- vapply(pending_runs, `[[`, character(1), "deputy_run_id")
  existing <- match(pending_run$deputy_run_id, ids)
  if (!is.na(existing)) {
    if (!identical(pending_runs[[existing]], pending_run)) {
      tempest_costorm_session_abort(
        "A pending Deputy run cannot change after it starts."
      )
    }
    return(invisible(pending_run))
  }
  terminal_ids <- vapply(
    tempest_session_deputy_traces(session),
    `[[`,
    character(1),
    "deputy_run_id"
  )
  if (pending_run$deputy_run_id %in% terminal_ids) {
    tempest_costorm_session_abort(
      "A completed Deputy run cannot become pending."
    )
  }
  pending_runs[[length(pending_runs) + 1L]] <- pending_run
  private <- tempest_session_private(session)
  private$pending_deputy_runs_value <-
    tempest_costorm_pending_deputy_runs(pending_runs)
  invisible(pending_run)
}

tempest_session_record_deputy_trace <- function(session, trace) {
  if (!inherits(session, "TempestSession")) {
    tempest_costorm_session_abort(
      "{.arg session} must be a TempestSession."
    )
  }
  trace <- tempest_costorm_deputy_trace(
    rlang::duplicate(trace, shallow = FALSE)
  )
  traces <- tempest_session_deputy_traces(session)
  trace_ids <- vapply(traces, `[[`, character(1), "trace_id")
  existing <- match(trace$trace_id, trace_ids)
  if (!is.na(existing)) {
    if (!identical(traces[[existing]], trace)) {
      tempest_costorm_session_abort(
        "A Deputy run trace cannot change after it is recorded."
      )
    }
    return(invisible(trace))
  }
  traces[[length(traces) + 1L]] <- trace
  private <- tempest_session_private(session)
  private$deputy_traces_value <-
    tempest_costorm_deputy_traces(traces)
  invisible(trace)
}

tempest_session_stage_recorder <- function(session) {
  if (!inherits(session, "TempestSession")) {
    return(tempest_stage_record_discard)
  }
  function(record, output = NULL) {
    tempest_session_record_stage(session, record, output)
  }
}

tempest_session_stage_batch_recorder <- function(session) {
  if (!inherits(session, "TempestSession")) {
    return(function(records, outputs = NULL) invisible(records))
  }
  function(records, outputs = NULL) {
    tempest_session_record_stages(session, records, outputs)
  }
}

tempest_session_record_stage <- function(
  session,
  record,
  output = NULL,
  commit = NULL
) {
  if (!inherits(session, "TempestSession")) {
    tempest_costorm_session_abort(
      "{.arg session} must be a TempestSession."
    )
  }
  record <- rlang::duplicate(record, shallow = FALSE)
  records <- tempest_stage_records_upsert(
    tempest_session_private(session)$stage_records_value,
    record
  )
  if (!is.null(commit)) {
    if (!is.function(commit)) {
      tempest_costorm_session_abort(
        "{.arg commit} must be a function or {.code NULL}."
      )
    }
    commit()
  }
  private <- tempest_session_private(session)
  private$stage_records_value <- records
  invisible(record)
}

tempest_session_record_stages <- function(session, records, outputs = NULL) {
  if (!inherits(session, "TempestSession")) {
    tempest_costorm_session_abort(
      "{.arg session} must be a TempestSession."
    )
  }
  records <- rlang::duplicate(records, shallow = FALSE)
  updated <- tempest_stage_records_upsert_many(
    tempest_session_private(session)$stage_records_value,
    records
  )
  private <- tempest_session_private(session)
  private$stage_records_value <- updated
  invisible(records)
}

tempest_session_set_stage_records <- function(session, records) {
  if (!inherits(session, "TempestSession")) {
    tempest_costorm_session_abort(
      "{.arg session} must be a TempestSession."
    )
  }
  records <- tempest_stage_records_validate(records)
  private <- tempest_session_private(session)
  private$stage_records_value <- rlang::duplicate(
    records,
    shallow = FALSE
  )
  invisible(session)
}

#' @keywords internal
tempest_session_report_value <- function(session) {
  if (!inherits(session, "TempestSession")) {
    tempest_costorm_session_abort(
      "{.arg session} must be a TempestSession."
    )
  }
  tempest_session_private(session)$report_md_value
}

#' @keywords internal
tempest_session_set_report_value <- function(session, report_md) {
  if (!inherits(session, "TempestSession")) {
    tempest_costorm_session_abort(
      "{.arg session} must be a TempestSession."
    )
  }
  if (
    !is.null(report_md) &&
      (!is.character(report_md) ||
        length(report_md) != 1L ||
        is.na(report_md))
  ) {
    tempest_costorm_session_abort(
      "{.arg report_md} must be one string or {.code NULL}."
    )
  }
  reference <- tempest_session_manifest(session)@deliverables$report_md %||%
    NULL
  if (identical(tempest_session_manifest(session)@status, "running")) {
    if (!is.null(report_md) || !is.null(reference)) {
      tempest_costorm_session_abort(
        "A running Co-STORM session must remain report-free."
      )
    }
  } else if (identical(tempest_session_manifest(session)@status, "succeeded")) {
    if (is.null(report_md)) {
      tempest_costorm_session_abort(
        "A succeeded Co-STORM session requires its canonical report."
      )
    }
    tempest_product_report_reference_validate(
      reference[c("report_id", "sha256")],
      report_md
    )
  } else {
    tempest_costorm_session_abort(
      "Only running or succeeded Co-STORM report state can be restored."
    )
  }
  private <- tempest_session_private(session)
  private$report_md_value <- report_md
  invisible(session)
}
