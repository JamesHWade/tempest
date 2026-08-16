test_that("tempest_run_dsprrr_module propagates provider errors", {
  skip_if_not_installed("dsprrr")
  forward <- function(text, ...) list(answer = text)
  bad_module <- dsprrr::module_fn("text -> answer", forward)
  program_artifact_id <- dsprrr::program_artifact_id(
    bad_module,
    registry = list(forward = forward)
  )
  execution <- tempest:::tempest_dsprrr_execution(
    bad_module,
    program_artifact_id,
    trace_context = list(
      product = "tempest",
      research_run_id = "fallback-runtime",
      stage = "extract_claims"
    ),
    stage = "extract_claims",
    evaluator_id = "tempest::evaluator/extract_claims",
    evaluator_version = "1"
  )
  local_mocked_bindings(
    tempest_dsprrr_run = function(...) stop("boom")
  )
  expect_error(
    tempest:::tempest_run_dsprrr_module(
      execution,
      fake_chat(),
      list(text = "test"),
      "extract_claims"
    ),
    class = "simpleError"
  )
})

test_that("dsprrr execution fails closed without a bound program", {
  expect_error(
    tempest:::tempest_run_dsprrr_module(
      NULL,
      chat = NULL,
      inputs = list(),
      step = "extract_claims"
    ),
    class = "tempest_ecosystem_contract_error"
  )
  expect_error(
    tempest:::tempest_run_dsprrr_module_async(
      NULL,
      chat = NULL,
      inputs = list(),
      step = "extract_claims"
    ),
    class = "tempest_ecosystem_contract_error"
  )
})

test_that("async dsprrr execution propagates synchronous provider errors", {
  forward <- function(text, ...) list(answer = text)
  program <- dsprrr::module_fn("text -> answer", forward)
  program_artifact_id <- dsprrr::program_artifact_id(
    program,
    registry = list(forward = forward)
  )
  execution <- tempest:::tempest_dsprrr_execution(
    program,
    program_artifact_id,
    trace_context = list(
      product = "tempest",
      research_run_id = "async-provider-error",
      stage = "extract_claims"
    ),
    stage = "extract_claims",
    evaluator_id = "tempest::evaluator/extract_claims",
    evaluator_version = "1"
  )
  local_mocked_bindings(
    tempest_dsprrr_run_async = function(...) {
      rlang::abort("provider failed", class = "test_provider_error")
    }
  )

  expect_error(
    tempest:::tempest_run_dsprrr_module_async(
      execution,
      chat = NULL,
      inputs = list(text = "test"),
      step = "extract_claims"
    ),
    class = "test_provider_error"
  )
})

test_that("tempest_run_dsprrr_module rethrows correlation contract errors", {
  forward <- function(text, ...) list(answer = text)
  program <- dsprrr::module_fn("text -> answer", forward)
  program_artifact_id <- dsprrr::program_artifact_id(
    program,
    registry = list(forward = forward)
  )
  execution <- tempest:::tempest_dsprrr_execution(
    program,
    program_artifact_id,
    trace_context = list(
      product = "tempest",
      research_run_id = "fallback-contract",
      stage = "extract_claims"
    ),
    stage = "extract_claims",
    evaluator_id = "tempest::evaluator/extract_claims",
    evaluator_version = "1"
  )
  current_class <- NULL
  local_mocked_bindings(
    tempest_dsprrr_run = function(...) {
      rlang::abort("contract failure", class = current_class)
    }
  )

  for (condition_class in c(
    "dsprrr_trace_contract_error",
    "dsprrr_program_trace_contract_error",
    "dsprrr_artifact_integrity_error"
  )) {
    current_class <- condition_class
    expect_error(
      tempest:::tempest_run_dsprrr_module(
        execution,
        chat = NULL,
        inputs = list(text = "test"),
        step = "extract_claims"
      ),
      class = condition_class
    )
  }
})

test_that("tempest_run_dsprrr_module rejects anonymous programs", {
  forward <- function(text, ...) list(answer = text)
  program <- dsprrr::module_fn("text -> answer", forward)

  expect_error(
    tempest:::tempest_run_dsprrr_module(
      program,
      chat = NULL,
      inputs = list(text = "test"),
      step = "extract_claims"
    ),
    class = "tempest_ecosystem_contract_error"
  )
})

test_that("sync dsprrr execution rejects post-bind program mutation", {
  program_set <- tempest_program_set()
  program <- tempest:::tempest_program_set_execution(
    program_set,
    "extract_claims",
    trace_context = list(stage = "extract_claims")
  )
  program$program$signature@instructions <- paste(
    program$program$signature@instructions,
    "Mutated after binding."
  )
  calls <- 0L
  local_mocked_bindings(
    tempest_dsprrr_run = function(...) {
      calls <<- calls + 1L
      NULL
    }
  )

  expect_error(
    tempest:::tempest_run_dsprrr_module_structured(
      program,
      chat = NULL,
      inputs = list(),
      step = "extract_claims"
    ),
    class = "tempest_program_set_verification_error"
  )
  expect_equal(calls, 0L)
})

test_that("async dsprrr execution rejects post-bind program mutation", {
  program_set <- tempest_program_set()
  program <- tempest:::tempest_program_set_execution(
    program_set,
    "extract_claims",
    trace_context = list(stage = "extract_claims")
  )
  program$program$signature@instructions <- paste(
    program$program$signature@instructions,
    "Mutated after binding."
  )
  calls <- 0L
  local_mocked_bindings(
    tempest_dsprrr_run_async = function(...) {
      calls <<- calls + 1L
      NULL
    }
  )

  expect_error(
    tempest:::tempest_run_dsprrr_module_async(
      program,
      chat = NULL,
      inputs = list(),
      step = "extract_claims"
    ),
    class = "tempest_program_set_verification_error"
  )
  expect_equal(calls, 0L)
})
