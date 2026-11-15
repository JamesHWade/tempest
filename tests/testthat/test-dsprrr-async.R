test_that("async dsprrr returns simple output with producer metadata", {
  skip_if_not_installed("promises")
  skip_if_not_installed("later")

  execution <- tempest:::tempest_program_set_execution(
    tempest_program_set(),
    "query_decomposition",
    trace_context = list(
      product = "tempest",
      research_run_id = "async-structured-result",
      stage = "query_decomposition"
    )
  )
  state <- new.env(parent = emptyenv())
  state$run <- list(
    run_id = "run-before",
    agent_id = "agent-1",
    session_id = "session-1",
    stop_reason = "completed"
  )
  state$run_context <- NULL
  chat <- fake_chat_r6(list(
    chat_structured_async = function(..., type = NULL, run_context = NULL) {
      state$run_context <- run_context
      state$run <- list(
        run_id = "run-after",
        agent_id = "agent-1",
        session_id = "session-1",
        stop_reason = "completed"
      )
      promises::promise_resolve(list(
        queries = list("targeted query")
      ))
    },
    last_run = function() state$run
  ))

  request <- tempest:::tempest_run_dsprrr_module_async(
    execution,
    chat,
    inputs = list(question = "Question", topic = "Topic"),
    step = "query_decomposition"
  )
  correlation <- list(
    program_artifact_id = execution$program_artifact_id,
    trace_context = execution$trace_context
  )
  settled <- await_tempest_promise(request)

  expect_null(settled$error)
  expect_identical(settled$value, list(queries = list("targeted query")))
  expect_identical(class(settled$value), "list")
  expect_identical(
    attr(request, "dsprrr_trace_context", exact = TRUE),
    correlation
  )
  expect_identical(state$run_context, list(dsprrr = correlation))

  trace <- execution$program$state$traces[[1L]]
  expect_identical(
    trace$metadata$program_artifact_id,
    correlation$program_artifact_id
  )
  expect_identical(trace$metadata$trace_context, correlation$trace_context)
  expect_identical(
    trace$metadata$agent_run,
    list(
      run_id = "run-after",
      agent_id = "agent-1",
      session_id = "session-1",
      stop_reason = "completed"
    )
  )
})

test_that("async dsprrr rejects mismatched final structured identity", {
  skip_if_not_installed("promises")
  skip_if_not_installed("later")

  execution <- tempest:::tempest_program_set_execution(
    tempest_program_set(),
    "query_decomposition",
    trace_context = list(
      product = "tempest",
      research_run_id = "async-metadata-mismatch",
      stage = "query_decomposition"
    )
  )
  correlation <- list(
    program_artifact_id = execution$program_artifact_id,
    trace_context = execution$trace_context
  )
  tamper <- NULL
  local_mocked_bindings(
    tempest_dsprrr_run_async = function(
      module,
      ...,
      .llm = NULL,
      .return_format = "simple",
      .trace_context = list()
    ) {
      expect_identical(.return_format, "structured")
      expect_identical(.trace_context, execution$trace_context)
      result_metadata <- correlation
      if (identical(tamper, "program_artifact_id")) {
        result_metadata$program_artifact_id <- paste0(
          "sha256:",
          strrep("0", 64L)
        )
      } else {
        result_metadata$trace_context$stage <- "personas"
      }
      request <- promises::promise_resolve(structure(
        list(
          output = list(queries = list("targeted query")),
          metadata = result_metadata
        ),
        class = "dsprrr_result"
      ))
      attr(request, "dsprrr_trace_context") <- correlation
      request
    }
  )

  for (field in c("program_artifact_id", "trace_context")) {
    tamper <- field
    settled <- await_tempest_promise(
      tempest:::tempest_run_dsprrr_module_async(
        execution,
        chat = NULL,
        inputs = list(question = "Question", topic = "Topic"),
        step = "query_decomposition"
      )
    )
    expect_s3_class(settled$error, "tempest_ecosystem_contract_error")
    expect_match(
      conditionMessage(settled$error),
      if (identical(field, "program_artifact_id")) {
        "bound program artifact"
      } else {
        "bound Tempest trace context"
      }
    )
  }
})

test_that("async dsprrr preserves producer rejection and failure trace", {
  skip_if_not_installed("promises")
  skip_if_not_installed("later")

  execution <- tempest:::tempest_program_set_execution(
    tempest_program_set(),
    "query_decomposition",
    trace_context = list(
      product = "tempest",
      research_run_id = "async-provider-rejection",
      stage = "query_decomposition"
    )
  )
  chat <- fake_chat_r6(list(
    chat_structured_async = function(..., type = NULL, run_context = NULL) {
      promises::promise(function(resolve, reject) {
        reject(rlang::error_cnd(
          "test_async_provider_error",
          message = "fake provider rejected"
        ))
      })
    }
  ))

  request <- tempest:::tempest_run_dsprrr_module_async(
    execution,
    chat,
    inputs = list(question = "Question", topic = "Topic"),
    step = "query_decomposition"
  )
  settled <- await_tempest_promise(request)

  expect_s3_class(settled$error, "test_async_provider_error")
  expect_match(conditionMessage(settled$error), "fake provider rejected")
  expect_identical(
    attr(request, "dsprrr_trace_context", exact = TRUE),
    list(
      program_artifact_id = execution$program_artifact_id,
      trace_context = execution$trace_context
    )
  )
  expect_length(execution$program$state$traces, 1L)
  trace <- execution$program$state$traces[[1L]]
  expect_identical(trace$metadata$error_stage, "provider")
  expect_match(trace$metadata$error, "fake provider rejected")
  expect_identical(
    trace$metadata$program_artifact_id,
    execution$program_artifact_id
  )
  expect_identical(trace$metadata$trace_context, execution$trace_context)
})
