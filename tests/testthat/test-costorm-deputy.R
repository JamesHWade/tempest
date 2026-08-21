test_that("moderator delegates through persistent Deputy expert execution", {
  skip_if_not_installed("deputy")
  skip_if_not_installed("ellmer")
  expert <- test_expert(name = "Deputy Expert")

  moderator_chat <- local({
    state <- new.env(parent = emptyenv())
    state$turns <- list()
    state$tools <- list()
    state$system_prompt <- NULL
    state$on_tool_request <- function(request) invisible(request)
    state$on_tool_result <- function(result) invisible(result)
    state$request_number <- 0L
    state$raw_chat_calls <- 0L
    state$delegations <- list()

    add_turn <- function(contents) {
      state$turns <- c(
        state$turns,
        list(ellmer::AssistantTurn(contents, tokens = c(6, 3, 0), cost = 0))
      )
    }
    chat <- NULL
    chat <- structure(
      list(
        chat = function(...) {
          state$raw_chat_calls <- state$raw_chat_calls + 1L
          stop("raw moderator chat path")
        },
        stream = function(
          prompt = NULL,
          stream = c("text", "content"),
          controller = NULL
        ) {
          state$request_number <- state$request_number + 1L
          request_number <- state$request_number
          if (request_number %% 2L == 0L) {
            step <- 0L
            return(function() {
              step <<- step + 1L
              if (step == 1L) {
                response <- ellmer::ContentText(paste(
                  "Moderator synthesis",
                  request_number %/% 2L
                ))
                add_turn(list(response))
                return(response)
              }
              coro::exhausted()
            })
          }
          delegation_number <- (request_number + 1L) %/% 2L
          tool <- state$tools[["delegate_to_expert"]]
          request <- ellmer::ContentToolRequest(
            id = paste0("moderator-delegation-", delegation_number),
            name = "delegate_to_expert",
            arguments = list(
              expert_id = expert@expert_id,
              question = paste("Evidence question", delegation_number)
            ),
            tool = tool
          )
          step <- 0L
          function() {
            step <<- step + 1L
            if (step == 1L) {
              add_turn(list(request))
              return(request)
            }
            if (step == 2L) {
              state$on_tool_request(request)
              value <- tool(
                expert_id = request@arguments$expert_id,
                question = request@arguments$question
              )
              state$delegations[[delegation_number]] <- value
              result <- ellmer::ContentToolResult(
                value = value,
                request = request
              )
              state$on_tool_result(result)
              return(result)
            }
            coro::exhausted()
          }
        },
        stream_async = function(...) stop("unexpected moderator async path"),
        get_turns = function() state$turns,
        set_turns = function(turns) {
          state$turns <- turns
          invisible(NULL)
        },
        get_system_prompt = function() state$system_prompt,
        set_system_prompt = function(prompt) {
          state$system_prompt <- prompt
          invisible(NULL)
        },
        get_tools = function() state$tools,
        register_tool = function(tool) {
          state$tools[[tool@name]] <- tool
          invisible(NULL)
        },
        register_tools = function(tools) {
          for (tool in tools) {
            state$tools[[tool@name]] <- tool
          }
          invisible(NULL)
        },
        get_tokens = function() {
          data.frame(input = 10, output = 5, cached_input = 0, cost = 0)
        },
        get_provider = function() list(name = "mock", model = "moderator"),
        get_model = function() "moderator",
        last_turn = function(role = "assistant") {
          if (length(state$turns) == 0L) {
            return(NULL)
          }
          tail(state$turns, 1L)[[1L]]
        },
        on_tool_request = function(callback) {
          state$on_tool_request <- callback
          invisible(NULL)
        },
        on_tool_result = function(callback) {
          state$on_tool_result <- callback
          invisible(NULL)
        },
        clone = function() chat,
        .state = function() state
      ),
      class = c("Chat", "list")
    )
    chat
  })
  expert_chat <- fake_chat(text = list("Expert answer 1", "Expert answer 2"))
  mindmap <- list(
    nodes = list(list(
      id = "root",
      label = "Deputy integration",
      parent = NULL,
      notes = "",
      source_ids = character()
    )),
    edges = list()
  )
  mindmap_chat <- fake_chat(structured = list(mindmap, mindmap))
  config <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) {
      if (identical(role, "expert")) {
        return(expert_chat)
      }
      if (identical(role, "mindmap")) {
        return(mindmap_chat)
      }
      if (identical(role, "coordinator")) {
        return(moderator_chat)
      }
      fake_chat()
    }
  )
  session <- tempest_session(
    "Deputy integration",
    config = config,
    experts = list(expert)
  )

  first <- session$step("First user question")
  second <- session$step("Second user question")
  moderator_state <- moderator_chat$.state()
  delegations <- moderator_state$delegations
  expert_session <- tempest:::tempest_session_expert_manager(
    session
  )$get_or_create(
    expert@expert_id
  )
  expert_identity <- tempest:::tempest_deputy_chat_identity(
    expert_session$chat
  )
  traces <- tempest:::tempest_session_deputy_traces(session)
  moderator_traces <- Filter(
    \(trace) identical(trace$role, "moderator"),
    traces
  )
  expert_traces <- Filter(
    \(trace) identical(trace$role, "expert"),
    traces
  )

  expect_length(delegations, 2L)
  expect_identical(
    delegations[[2L]]$session_id,
    delegations[[1L]]$session_id
  )
  expect_identical(
    delegations[[2L]]$deputy_session_id,
    delegations[[1L]]$deputy_session_id
  )
  expect_identical(expert_session$session_id, delegations[[1L]]$session_id)
  expect_identical(
    expert_identity$deputy_session_id,
    delegations[[1L]]$deputy_session_id
  )
  expect_identical(
    length(unique(c(
      first$deputy_run_id,
      second$deputy_run_id,
      delegations[[1L]]$deputy_run_id,
      delegations[[2L]]$deputy_run_id
    ))),
    4L
  )
  expect_length(moderator_traces, 2L)
  expect_length(expert_traces, 2L)
  expect_identical(
    unique(vapply(expert_traces, `[[`, character(1), "agent_id")),
    expert_identity$agent_id
  )
  expect_identical(
    unique(vapply(
      expert_traces,
      `[[`,
      character(1),
      "deputy_session_id"
    )),
    expert_identity$deputy_session_id
  )
  expect_identical(moderator_state$raw_chat_calls, 0L)
  expect_identical(
    vapply(expert_chat$.calls(), `[[`, character(1), "transport"),
    rep("stream", 2L)
  )

  snapshot <- tempest_session_snapshot(session)
  expect_identical(snapshot$research_manifest$traces, traces)
  expect_identical(
    unlist(
      snapshot$research_manifest$runtime$deputy_run_ids,
      use.names = FALSE
    ),
    sort(vapply(traces, `[[`, character(1), "deputy_run_id"))
  )
  expect_identical(
    unlist(
      snapshot$research_manifest$runtime$deputy_session_ids,
      use.names = FALSE
    ),
    sort(unique(vapply(
      traces,
      `[[`,
      character(1),
      "deputy_session_id"
    )))
  )
})

test_that("unseen-source questions expose their moderator Deputy execution", {
  skip_if_not_installed("deputy")
  skip_if_not_installed("ellmer")

  moderator_chat <- fake_chat(
    text = list(
      "What does the unseen source establish?\nWhat remains uncertain?",
      "Which unseen finding changes the current map?",
      "The unseen finding should be investigated."
    )
  )
  mindmap <- list(
    nodes = list(list(
      id = "root",
      label = "Unseen evidence",
      parent = NULL,
      notes = "",
      source_ids = character()
    )),
    edges = list()
  )
  config <- tempest_config(chat_fn = function(
    role,
    model,
    system_prompt,
    echo
  ) {
    if (identical(role, "coordinator")) {
      return(moderator_chat)
    }
    if (identical(role, "mindmap")) {
      return(fake_chat(structured = list(mindmap)))
    }
    fake_chat()
  })
  session <- tempest_session(
    "Unseen evidence",
    config = config,
    experts = list(test_expert(
      name = "Unseen Evidence Expert"
    ))
  )

  expect_null(session$surface_unseen_information())
  expect_length(tempest:::tempest_session_deputy_traces(session), 0L)
  expect_length(moderator_chat$.calls(), 0L)

  session$workspace$upsert_retrieved_resource(tempest_resource(
    resource_kind = "web",
    locator = "https://example.org/unseen-deputy",
    title = "Unseen Deputy source",
    media_type = "text/html",
    content = "An unseen finding changes the evidence map.",
    metadata = list(
      snippet = "An unseen finding changes the evidence map."
    )
  ))
  surfaced <- session$surface_unseen_information(max_questions = 1L)
  expect_named(
    surfaced,
    c(
      "questions",
      "correlation_id",
      "deputy_run_id",
      "deputy_session_id"
    )
  )
  expect_identical(
    surfaced$questions,
    "What does the unseen source establish?"
  )
  expect_match(surfaced$deputy_run_id, "^run[_-]")
  expect_match(surfaced$deputy_session_id, "^tempest-moderator-")
  execution <- tail(tempest:::tempest_session_deputy_traces(session), 1L)[[1L]]
  expect_identical(surfaced$correlation_id, execution$correlation_id)
  expect_identical(execution$stage, "dialogue")
  expect_identical(execution$role, "moderator")
  expect_identical(is.null(execution$correlation_id), FALSE)

  expect_identical(
    vapply(moderator_chat$.calls(), `[[`, character(1), "transport"),
    "stream"
  )
  expect_length(tempest:::tempest_session_deputy_traces(session), 1L)
})

test_that("synchronous warmup binds one correlation across Deputy and stage", {
  skip_if_not_installed("deputy")
  skip_if_not_installed("ellmer")

  source_id <- NULL
  expert_chat <- fake_chat(
    text = list(function(prompt) {
      paste0("A source-backed warmup answer [", source_id, "].")
    })
  )
  extractor_chat <- fake_chat(
    structured = list(function(prompt) {
      list(
        facts = list(list(
          claim = "A source-backed warmup answer",
          sources = list(list(source_id = source_id)),
          confidence = "high",
          support_score = 0.9
        ))
      )
    })
  )
  mindmap <- list(
    nodes = list(list(
      id = "root",
      label = "Synchronous Deputy warmup",
      parent = NULL,
      notes = "",
      source_ids = character()
    )),
    edges = list()
  )
  cfg <- tempest_config(chat_fn = function(
    role,
    model,
    system_prompt,
    echo
  ) {
    if (identical(role, "expert")) {
      return(expert_chat)
    }
    if (identical(role, "judge")) {
      return(extractor_chat)
    }
    if (identical(role, "mindmap")) {
      return(fake_chat(structured = list(mindmap)))
    }
    fake_chat()
  })
  expert <- test_expert(
    name = "Dr. Sync Warmup",
    initial_questions = "What does the source establish?"
  )
  session <- tempest_session(
    "Synchronous Deputy warmup",
    config = cfg,
    experts = list(expert)
  )
  source <- tempest_resource(
    resource_kind = "web",
    locator = "https://example.org/sync-warmup",
    title = "Synchronous warmup source",
    media_type = "text/html",
    content = "A source-backed warmup answer.",
    metadata = list(snippet = "A source-backed warmup answer.")
  )
  session$workspace$upsert_retrieved_resource(source)
  source_id <- session$workspace$list_retrieved_sources()[[1L]]$id

  result <- session$warmup(verbose = FALSE)
  traces <- tempest:::tempest_session_deputy_traces(session)
  records <- tempest:::tempest_session_stage_records(session)
  trace <- traces[[1L]]
  record <- records[[1L]]

  expect_length(traces, 1L)
  expect_length(records, 1L)
  expect_length(
    tempest:::tempest_session_pending_deputy_runs(session),
    0L
  )
  expect_identical(trace$stage, "warmup")
  expect_identical(trace$role, "expert")
  expect_identical(trace$expert_id, expert@expert_id)
  expect_identical(
    tempest:::tempest_opaque_identifier_valid(trace$correlation_id),
    TRUE
  )
  expect_identical(
    record@trace_references$correlation_id,
    trace$correlation_id
  )
  expect_identical(
    record@trace_references$deputy_run_id,
    trace$deputy_run_id
  )
  expect_identical(
    record@trace_references$deputy_session_id,
    trace$deputy_session_id
  )
  expect_identical(record@trace_references$expert_id, expert@expert_id)
  orientation <- result@orientations[[1L]]
  expect_identical(orientation$deputy_run_id, trace$deputy_run_id)
  expect_identical(orientation$deputy_session_id, trace$deputy_session_id)
})

test_that("Deputy expert restores require the exact current binding wire shape", {
  skip_if_not_installed("deputy")
  skip_if_not_installed("ellmer")

  expert <- test_expert(
    name = "Exact Restore Expert"
  )
  config <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  source_session <- tempest_session(
    "Exact expert restore source",
    config = config,
    experts = list(expert)
  )
  source_manager <- tempest:::tempest_session_expert_manager(source_session)
  source <- source_manager$get_or_create(expert@expert_id)
  binding <- source$profile

  restored_session <- tempest_session(
    "Exact expert restore target",
    config = config,
    experts = list(expert)
  )
  manager <- tempest:::tempest_session_expert_manager(restored_session)
  malformed <- list(
    reordered = binding[rev(names(binding))],
    padded_scalar = within(binding, session_id <- paste0(" ", session_id)),
    scalar_list = within(binding, session_id <- list(session_id)),
    version_list = within(binding, expert_version <- list(expert_version)),
    fingerprint_null = within(binding, expert_fingerprint <- NULL)
  )

  for (candidate in malformed) {
    error <- tryCatch(manager$restore_session(candidate), error = identity)
    expect_s3_class(error, "tempest_deputy_expert_error")
  }
  expect_length(manager$list_sessions(), 0L)

  restored <- manager$restore_session(binding)
  expect_identical(restored$profile, binding)
  expect_identical(identical(restored$chat, source$chat), FALSE)
})
