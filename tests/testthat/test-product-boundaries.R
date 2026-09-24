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
    verbose = FALSE
  )

  expect_identical(
    intersect(
      names(tempest:::TempestResult@properties),
      c("runtime", "artifact_catalog", "workflow_run", "deliverables")
    ),
    character()
  )
  expect_r6_class(result@workspace, "ResearchWorkspace")
  expect_s7_class(result@manifest, TempestResearchManifest)
  expect_identical(result@manifest@status, "succeeded")
})

test_that("scripted STORM completes with a host retriever", {
  fixture <- storm_progress_fixture()
  built_in <- fixture$retriever
  retriever <- list(
    workspace = fixture$store,
    search = function(query, k) built_in$search(query, k = k),
    fetch = function(url) built_in$fetch(url)
  )
  expert <- tempest_expert(
    name = "Host Boundary Expert",
    title = "Researcher",
    description = "Exercises a host-owned retriever.",
    instructions = "Use the supplied evidence."
  )
  output_dir <- withr::local_tempdir()

  result <- tempest_run(
    "Host retriever product boundary",
    config = fixture$config,
    retriever = retriever,
    experts = list(expert),
    max_questions_per_perspective = 1,
    output_dir = output_dir,
    run_id = "host-retriever-product",
    verbose = FALSE
  )

  expect_s7_class(result, TempestResult)
  expect_identical(result@retriever, retriever)
  expect_identical(result@config, fixture$config)
  expect_identical(result@manifest@status, "succeeded")

  resumed <- tempest_run(
    "Host retriever product boundary",
    config = fixture$config,
    retriever = retriever,
    experts = list(expert),
    max_questions_per_perspective = 1,
    output_dir = output_dir,
    resume = TRUE,
    run_id = "host-retriever-product",
    verbose = FALSE
  )
  expect_identical(resumed@manifest@status, "succeeded")
  expect_identical(resumed@retriever, retriever)
  expect_identical(resumed@config, fixture$config)
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
