test_that("a supplied view defines new workspace snapshot authority", {
  skip_if_not_installed("graft")
  fixture <- test_knowledge_view()
  config <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  session <- tempest:::tempest_session_new(
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
    tempest:::tempest_snapshot_reference(
      tempest:::tempest_session_workspace(session)$graft_snapshot
    ),
    tempest:::tempest_snapshot_reference(fixture$snapshot)
  )
  expect_identical(
    test_contains_runtime_value(
      tempest_research_manifest_record(tempest:::tempest_session_manifest(
        session
      ))
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
    tempest:::tempest_session_new(
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

test_that("restored sessions keep host programs independent of a live view", {
  skip_if_not_installed("graft")
  fixture <- test_knowledge_view()
  program_set <- tempest_program_set()
  config <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  session <- tempest:::tempest_session_new(
    "Restored governed Co-STORM",
    config = config,
    experts = list(test_expert()),
    program_set = program_set,
    knowledge_view = fixture$view
  )
  snapshot <- tempest_session_snapshot(session)
  restored <- tempest_session_restore(
    snapshot,
    config = tempest:::tempest_session_config(session),
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
  expect_identical(
    tempest:::tempest_dsprrr_execution_verify(module, "personas"),
    module
  )
  expect_identical(
    intersect(names(module), c("knowledge_view", "governed_procedure_ref")),
    character()
  )
})
