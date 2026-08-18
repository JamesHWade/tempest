test_that("fresh governed workflows require a live pinned view", {
  program_set <- test_governed_program_set()

  expect_error(
    tempest_run(
      "Governed STORM",
      program_set = program_set,
      steps = "perspectives",
      verbose = FALSE
    ),
    class = "tempest_governed_procedure_error",
    regexp = "requires its exact pinned"
  )
  expect_error(
    tempest_session(
      "Governed Co-STORM",
      experts = list(test_expert()),
      program_set = program_set
    ),
    class = "tempest_governed_procedure_error",
    regexp = "requires its exact pinned"
  )
  expect_error(
    tempest_run_async(
      "Governed async STORM",
      program_set = program_set
    ),
    class = "tempest_governed_procedure_error",
    regexp = "never serializes a live pinned"
  )
  restored <- tempest:::tempest_product_knowledge_view(
    program_set,
    knowledge_view = NULL,
    restoring = TRUE
  )
  expect_null(restored$view)
  expect_identical(restored$required, TRUE)
})

test_that("a supplied view defines new workspace snapshot authority", {
  skip_if_not_installed("graft")
  fixture <- test_knowledge_view()
  config <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  session <- tempest_session(
    "Pinned Co-STORM",
    config = config,
    experts = list(test_expert()),
    knowledge_view = fixture$view
  )

  expect_identical(
    tempest:::tempest_session_knowledge_view(session),
    fixture$view
  )
  expect_identical(
    tempest:::tempest_snapshot_reference(session$workspace$graft_snapshot),
    tempest:::tempest_snapshot_reference(fixture$snapshot)
  )
  expect_identical(
    test_contains_runtime_value(
      tempest_research_manifest_record(session$manifest)
    ),
    FALSE
  )
  contains_exact_view <- function(value) {
    if (identical(value, fixture$view)) {
      return(TRUE)
    }
    if (!is.list(value)) {
      return(FALSE)
    }
    any(vapply(value, contains_exact_view, logical(1)))
  }
  expect_identical(
    contains_exact_view(tempest_session_snapshot(session)),
    FALSE
  )
})

test_that("supplied retriever workspaces must match the exact view", {
  skip_if_not_installed("graft")
  fixture <- test_knowledge_view()
  config <- tempest_config()
  retriever <- tempest_retriever(
    config = config,
    workspace = tempest_research_workspace()
  )

  expect_error(
    tempest_session(
      "Mismatched Co-STORM",
      config = config,
      retriever = retriever,
      experts = list(test_expert()),
      knowledge_view = fixture$view
    ),
    class = "tempest_governed_procedure_error",
    regexp = "does not use the supplied pinned"
  )
})

test_that("governed references must belong to the supplied view", {
  skip_if_not_installed("graft")
  fixture <- test_knowledge_view()
  program_set <- test_governed_program_set()

  expect_error(
    tempest:::tempest_product_knowledge_view(
      program_set,
      fixture$view
    ),
    class = "tempest_governed_procedure_error",
    regexp = "do not belong to the supplied pinned"
  )
})

test_that("structured contexts retain the view only at runtime", {
  view <- new.env(parent = emptyenv())
  module <- structure(
    list(knowledge_view = view),
    class = c("tempest_dsprrr_execution", "list")
  )
  context <- tempest:::tempest_stage_context_knowledge_view(
    list(topic = "runtime only"),
    module
  )

  expect_identical(context$knowledge_view, view)
  expect_identical(context$topic, "runtime only")
})

test_that("STORM never serializes a live knowledge view to workers", {
  view <- new.env(parent = emptyenv())
  programs <- list(
    section_writing = structure(
      list(knowledge_view = view),
      class = c("tempest_dsprrr_execution", "list")
    )
  )

  expect_error(
    tempest_run_async("Governed async STORM", knowledge_view = view),
    class = "tempest_governed_procedure_error",
    regexp = "never serializes a live pinned"
  )
  expect_null(tempest:::tempest_write_sections_parallel(
    jobs = list(list()),
    config = tempest_config(),
    programs = programs
  ))
})

test_that("restored governed sessions stay inspectable without a live view", {
  skip_if_not_installed("graft")
  fixture <- test_knowledge_view()
  program_set <- test_governed_program_set(
    snapshot_reference = tempest:::tempest_snapshot_reference(
      fixture$snapshot
    )
  )
  config <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  session <- tempest_session(
    "Restored governed Co-STORM",
    config = config,
    experts = list(test_expert()),
    program_set = program_set,
    knowledge_view = fixture$view
  )
  snapshot <- tempest_session_snapshot(session)
  restored <- tempest_session_restore(
    snapshot,
    config = session$config,
    program_set = program_set,
    knowledge_view = NULL
  )

  expect_null(tempest:::tempest_session_knowledge_view(restored))
  expect_identical(restored$topic, session$topic)
  expect_identical(
    identical(
      tempest:::tempest_session_verification_owner_token(restored),
      tempest:::tempest_session_verification_owner_token(session)
    ),
    FALSE
  )
  expect_false("verification_owner_token" %in% names(snapshot$workspace))
  module <- tempest:::tempest_session_programs(restored)$personas
  expect_error(
    tempest:::tempest_execute_stage(
      module,
      fake_chat(),
      inputs = list(
        topic = restored$topic,
        n_experts = 1L,
        requirements = tempest:::tempest_persona_requirements(NULL)
      ),
      context = list(n_experts = 1L)
    ),
    class = "tempest_governed_procedure_error",
    regexp = "requires its exact pinned"
  )
})
