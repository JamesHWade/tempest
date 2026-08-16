test_that("tempest_run_dsprrr_module signals fallback on error", {
  skip_if_not_installed("dsprrr")
  forward <- function(text, ...) list(answer = text)
  bad_module <- dsprrr::module_fn("text -> answer", forward)
  dsprrr::program_artifact_id(
    bad_module,
    registry = list(forward = forward)
  )
  local_mocked_bindings(
    tempest_dsprrr_run = function(...) stop("boom")
  )
  expect_warning(
    out <- tempest:::tempest_run_dsprrr_module(
      bad_module,
      fake_chat(),
      list(text = "test"),
      "test step"
    ),
    "falling back"
  )
  expect_null(out)
})

test_that("tempest_run_dsprrr_module rethrows correlation contract errors", {
  forward <- function(text, ...) list(answer = text)
  program <- dsprrr::module_fn("text -> answer", forward)
  dsprrr::program_artifact_id(program, registry = list(forward = forward))
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
        program,
        chat = NULL,
        inputs = list(text = "test"),
        step = "test step"
      ),
      class = condition_class
    )
  }
})
