fake_costorm_warmup_session <- function(
  chat_async = NULL,
  experts = NULL,
  progress = NULL,
  extractor_async = NULL,
  mindmap_async = NULL
) {
  local_mocked_bindings(
    tempest_session_assert_mutable = function(session, action) {
      invisible(session)
    },
    tempest_session_expert_manager = function(session) session$fake_manager,
    tempest_session_chat = function(session, role) session$fake_chats[[role]],
    tempest_session_append_transcript = function(session, speaker, role, text) {
      if (!identical(role, "assistant")) {
        stop("The fake warmup manager commits only expert completions.")
      }
      session$state$turns[[length(session$state$turns) + 1L]] <- list(
        speaker = speaker,
        role = role,
        text = text
      )
      invisible(session)
    },
    tempest_session_async_work_start = function(...) "fake-warmup-work",
    tempest_session_async_work_finish = function(...) invisible(NULL),
    tempest_session_deputy_traces = function(session) session$state$traces,
    tempest_session_programs = function(session) session$programs,
    tempest_session_stage_recorder = function(session) {
      tempest:::tempest_stage_record_discard
    },
    tempest_costorm_mindmap_projection = function(session) {
      session$state$map_updates <- session$state$map_updates + 1L
      mindmap_async()
    },
    .env = parent.frame()
  )
  store <- fake_store_with_sources(1)
  source_id <- store$list_retrieved_sources()[[1]]$id
  if (is.null(experts)) {
    experts <- list(test_expert(
      expert_id = "expert.a",
      name = "Dr. A",
      title = "Expert",
      initial_questions = "What matters?"
    ))
  }
  if (is.null(chat_async)) {
    chat_async <- function(prompt, expert, generation) {
      promises::promise_resolve(paste0(
        expert@name,
        " orientation [",
        source_id,
        "]."
      ))
    }
  }
  if (is.null(extractor_async)) {
    extractor_async <- function(...) {
      promises::promise_resolve(list(
        facts = list(list(
          claim = "Warmup finding",
          sources = list(list(source_id = source_id)),
          confidence = "high",
          support_score = 0.9
        ))
      ))
    }
  }
  if (is.null(mindmap_async)) {
    mindmap_async <- function(...) {
      promises::promise_resolve(list(
        nodes = list(
          list(
            id = "root",
            label = "Test topic",
            parent = NULL,
            notes = "",
            source_ids = character()
          ),
          list(
            id = "orientation",
            label = "Orientation",
            parent = "root",
            notes = "",
            source_ids = source_id
          )
        ),
        edges = list()
      ))
    }
  }

  state <- new.env(parent = emptyenv())
  state$turns <- list()
  state$map_updates <- 0L
  state$retired <- 0L
  state$generations <- new.env(parent = emptyenv())
  state$chats <- new.env(parent = emptyenv())
  state$session_keys <- new.env(parent = emptyenv())
  state$run_count <- 0L
  state$traces <- list()
  state$completions <- new.env(parent = emptyenv())

  call_chat <- function(prompt, expert, generation) {
    args <- names(formals(chat_async))
    if ("..." %in% args || length(args) >= 3L) {
      return(chat_async(prompt, expert, generation))
    }
    if (length(args) >= 2L) {
      return(chat_async(prompt, expert))
    }
    chat_async(prompt)
  }

  manager <- list()
  manager$get_or_create <- function(expert_id) {
    ids <- vapply(experts, \(expert) expert@expert_id, character(1))
    index <- match(expert_id, ids)
    if (is.na(index)) {
      stop("Unknown expert: ", expert_id)
    }
    expert <- experts[[index]]
    generation <- state$generations[[expert_id]] %||% 0L
    chat <- state$chats[[expert_id]]
    if (is.null(chat)) {
      generation <- generation + 1L
      state$generations[[expert_id]] <- generation
      execution <- new.env(parent = emptyenv())
      execution$value <- NULL
      chat <- list(
        get_tools = function() list(research = TRUE),
        chat_async = function(prompt, run_context = list()) {
          state$run_count <- state$run_count + 1L
          deputy_run_id <- paste0("fake-deputy-run-", state$run_count)
          execution$value <- list(
            agent_id = paste0("fake-agent-", expert_id),
            correlation_id = run_context$correlation_id,
            deputy_run_id = deputy_run_id,
            deputy_session_id = paste0(
              "fake-",
              expert_id,
              "-",
              generation
            ),
            expert_id = expert_id,
            completion_disposition = "issued",
            role = run_context$role %||% "expert",
            stage = run_context$stage %||% "warmup",
            status = "complete",
            trace_id = deputy_run_id,
            trace_type = "deputy_run"
          )
          state$traces[[length(state$traces) + 1L]] <- execution$value
          call_chat(prompt, expert, generation)
        },
        .trace = function() execution$value
      )
      state$chats[[expert_id]] <- chat
    }
    session_id <- paste0("fake-", expert_id, "-", generation)
    state$session_keys[[session_id]] <- expert_id
    provenance <- new.env(parent = emptyenv())
    provenance$current <- list()
    list(
      chat = chat,
      session_id = session_id,
      provenance = provenance,
      grants = list(research = list(status = "granted"))
    )
  }
  manager$retire_session <- function(session_id) {
    expert_id <- state$session_keys[[session_id]]
    if (
      !is.null(expert_id) && exists(expert_id, state$chats, inherits = FALSE)
    ) {
      rm(list = expert_id, envir = state$chats)
    }
    state$retired <- state$retired + 1L
    list(retired = TRUE, cancellation_supported = FALSE)
  }
  manager$request_completion_async <- function(
    expert_id,
    prompt,
    stage,
    correlation_id
  ) {
    session_result <- manager$get_or_create(expert_id)
    request <- session_result$chat$chat_async(
      prompt,
      run_context = list(
        correlation_id = correlation_id,
        role = "expert",
        stage = stage
      )
    )
    trace <- session_result$chat$.trace()
    completion_id <- paste0("fake-completion-", trace$deputy_run_id)
    promises::then(
      request,
      function(response) {
        assign(
          completion_id,
          list(
            response = response,
            trace = trace,
            session_id = session_result$session_id,
            expert_id = expert_id
          ),
          state$completions
        )
        completion_id
      }
    )
  }
  manager$commit_completion <- function(
    completion_id,
    expert_id,
    stage,
    is_current
  ) {
    completion <- get(completion_id, state$completions, inherits = FALSE)
    rm(list = completion_id, envir = state$completions)
    if (!isTRUE(is_current())) {
      return(list(cancelled = TRUE))
    }
    before_sources <- length(tempest:::tempest_session_workspace(
      session
    )$list_retrieved_sources())
    before_claims <- length(tempest:::tempest_session_workspace(
      session
    )$list_proposed_claims())
    source_ids <- tempest:::tempest_answer_source_ids(
      tempest:::tempest_session_workspace(session),
      completion$response,
      character()
    )
    request <- tempest:::tempest_extract_facts_from_answer_async(
      session$fake_chats$extractor,
      completion$response,
      tempest:::tempest_session_workspace(session),
      module = session$programs$extract_claims,
      source_ids = source_ids,
      session_id = session$session_id,
      expert_id = expert_id,
      retrieval_step_id = completion$trace$correlation_id,
      deputy_run_id = completion$trace$deputy_run_id,
      deputy_session_id = completion$trace$deputy_session_id,
      commit_if = is_current,
      record_stage = tempest:::tempest_stage_record_discard
    )
    request <- promises::then(request, function(...) {
      after_sources <- length(tempest:::tempest_session_workspace(
        session
      )$list_retrieved_sources())
      after_claims <- length(tempest:::tempest_session_workspace(
        session
      )$list_proposed_claims())
      list(
        source_ids = source_ids,
        sources_added = max(0L, after_sources - before_sources),
        claims_added = max(0L, after_claims - before_claims),
        extraction_skipped = NA_character_
      )
    })
    promises::then(
      request,
      onFulfilled = function(evidence) {
        list(
          cancelled = FALSE,
          response = completion$response,
          deputy_execution = completion$trace,
          session_id = completion$session_id,
          source_ids = evidence$source_ids,
          claim_ids = if (evidence$claims_added > 0L) {
            tail(
              vapply(
                tempest:::tempest_session_workspace(
                  session
                )$list_proposed_claims(),
                \(claim) claim@claim_id,
                character(1)
              ),
              evidence$claims_added
            )
          } else {
            character()
          },
          sources_added = evidence$sources_added,
          claims_added = evidence$claims_added,
          evidence_committed = is.na(
            evidence$extraction_skipped %||% NA_character_
          ),
          evidence_error = NULL
        )
      },
      onRejected = function(error) {
        list(
          cancelled = FALSE,
          response = completion$response,
          deputy_execution = completion$trace,
          session_id = completion$session_id,
          source_ids = character(),
          claim_ids = character(),
          evidence_committed = FALSE,
          evidence_error = tempest:::tempest_progress_error_payload(error)
        )
      }
    )
  }
  manager$cancel_completion <- function(completion_id) {
    if (exists(completion_id, state$completions, inherits = FALSE)) {
      rm(list = completion_id, envir = state$completions)
    }
    invisible(completion_id)
  }

  session <- list2env(
    list(
      experts = experts,
      mindmap = tempest:::tempest_mindmap_init("Test topic")
    ),
    parent = emptyenv()
  )
  class(session) <- "TempestSession"
  session$session_id <- "warmup-session"
  session$programs <- test_program_executions(run_id = session$session_id)
  session$topic <- "Test topic"
  session$fake_manager <- manager
  session$state <- state
  # Mirror the R6 private layout the internal session accessors read.
  test_session_private(session, workspace = store)
  session$fake_chats <- list(
    extractor = fake_chat_r6(list(chat_structured_async = extractor_async)),
    mindmap = fake_chat_r6(list())
  )
  session$emit_progress <- function(
    event_type,
    status,
    stage = NA_character_,
    step = NA_character_,
    message = NA_character_,
    payload = list(),
    parent_event_id = NA_character_,
    correlation_id = NA_character_
  ) {
    event <- tempest_progress_event(
      run_id = session$session_id,
      workflow = "costorm",
      event_type = event_type,
      status = status,
      stage = stage,
      step = step,
      message = message,
      payload = payload,
      parent_event_id = parent_event_id,
      correlation_id = correlation_id
    )
    if (is.function(progress)) {
      progress(event)
    }
    event
  }
  test_session_private(
    session,
    .methods = list(emit_progress = session$emit_progress)
  )
  session
}

test_costorm_deputy_trace <- function(
  run_id = "deputy-run-test",
  session_id = "deputy-session-test",
  stage = "dialogue",
  role = "moderator",
  correlation_id = paste0(run_id, "-correlation"),
  expert_id = NULL
) {
  trace <- list(
    agent_id = paste0("test-agent-", role),
    completion_disposition = "issued",
    deputy_run_id = run_id,
    deputy_session_id = session_id,
    role = role,
    stage = stage,
    status = "complete",
    trace_id = run_id,
    trace_type = "deputy_run"
  )
  trace$correlation_id <- correlation_id
  if (!is.null(expert_id)) {
    trace$expert_id <- expert_id
  }
  trace
}

test_record_costorm_deputy_trace <- function(session, ...) {
  trace <- test_costorm_deputy_trace(...)
  tempest:::tempest_session_record_deputy_trace(session, trace)
  tempest:::tempest_costorm_deputy_trace(trace)
}

# Give a fake TempestSession the R6 private layout the internal session
# accessors read, so stubs exercise the same code path as a real session.
test_session_private <- function(session, ..., .methods = list()) {
  values <- list(...)
  private <- session$.__enclos_env__$private %||% new.env(parent = emptyenv())
  for (name in names(values)) {
    assign(paste0(name, "_value"), values[[name]], envir = private)
  }
  for (name in names(.methods)) {
    assign(name, .methods[[name]], envir = private)
  }
  session$.__enclos_env__ <- list2env(
    list(private = private),
    parent = emptyenv()
  )
  invisible(session)
}
