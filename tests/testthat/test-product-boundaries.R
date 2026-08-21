test_that("scripted STORM stays on its product-owned execution path", {
  skip_if_not_installed("ellmer")
  fixture <- storm_progress_fixture()

  result <- tempest_run(
    "T8 STORM product boundary",
    config = fixture$config,
    retriever = fixture$retriever,
    experts = list(tempest_expert(
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
    intersect(
      names(result),
      c("runtime", "artifact_catalog", "workflow_run", "deliverables")
    ),
    character()
  )
  expect_r6_class(result$workspace, "ResearchWorkspace")
  expect_s7_class(result$manifest, TempestResearchManifest)
  expect_identical(result$manifest@status, "succeeded")
})

test_that("Co-STORM exposes only the explicit product turn seam", {
  session <- tempest_session(
    "T8 Co-STORM product boundary",
    config = tempest_config(
      chat_fn = function(role, model, system_prompt, echo) fake_chat()
    ),
    experts = list(test_expert(
      expert_id = "expert.product-surface",
      name = "Product Surface Expert"
    ))
  )

  expect_identical(names(formals(session$step)), "user_input")
  expect_disjoint(
    names(session),
    c("extract_facts", "harvest_native_sources", "execute_turn_decision")
  )
})
