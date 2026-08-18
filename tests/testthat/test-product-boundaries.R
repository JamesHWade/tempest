test_that("scripted STORM does not reach the generic product kernel", {
  skip_if_not_installed("ellmer")
  fixture <- storm_progress_fixture()
  raw_polisher_calls <- 0L
  original_chat_fn <- fixture$config@chat_fn
  fixture$config@chat_fn <- function(role, model, system_prompt, echo) {
    if (
      identical(role, "writer") &&
        identical(system_prompt, tempest_prompt("polisher_system"))
    ) {
      raw_polisher_calls <<- raw_polisher_calls + 1L
    }
    original_chat_fn(role, model, system_prompt, echo)
  }
  generic_functions <- c(
    "tempest_runtime",
    "tempest_artifact_catalog",
    "tempest_storm_report_plan",
    "tempest_deliverable_generate",
    "tempest_deliverable_finalize",
    "tempest_deliverable_primary_artifact"
  )
  calls <- stats::setNames(
    integer(length(generic_functions)),
    generic_functions
  )
  reject_generic_call <- function(name) {
    calls[[name]] <<- calls[[name]] + 1L
    rlang::abort(
      paste0("Scripted STORM reached generic kernel function `", name, "`."),
      class = "test_generic_kernel_reached"
    )
  }
  local_mocked_bindings(
    tempest_runtime = function(...) {
      reject_generic_call("tempest_runtime")
    },
    tempest_artifact_catalog = function(...) {
      reject_generic_call("tempest_artifact_catalog")
    },
    tempest_storm_report_plan = function(...) {
      reject_generic_call("tempest_storm_report_plan")
    },
    tempest_deliverable_generate = function(...) {
      reject_generic_call("tempest_deliverable_generate")
    },
    tempest_deliverable_finalize = function(...) {
      reject_generic_call("tempest_deliverable_finalize")
    },
    tempest_deliverable_primary_artifact = function(...) {
      reject_generic_call("tempest_deliverable_primary_artifact")
    }
  )

  tempest_run(
    "T7 STORM product boundary",
    config = fixture$config,
    retriever = fixture$retriever,
    experts = list(tempest_expert(
      expert_id = "expert.product-boundary",
      name = "Product Boundary Expert",
      title = "Researcher",
      description = "Exercises the authoritative STORM product path.",
      instructions = "Use the supplied evidence."
    )),
    max_questions_per_perspective = 1,
    program_set = tempest_program_set(),
    verbose = FALSE
  )

  expect_identical(
    calls,
    stats::setNames(integer(length(generic_functions)), generic_functions)
  )
  expect_identical(raw_polisher_calls, 0L)
})
