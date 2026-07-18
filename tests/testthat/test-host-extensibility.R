test_that("tempest_expert creates validated host expert definitions", {
  expert <- tempest_expert(
    expert_id = "expert.policy",
    name = "Dr. Rivera",
    title = "Policy analyst",
    description = "Policy incentives",
    instructions = "Compare policy mechanisms.",
    initial_questions = c("What policies matter?", "Who pays?")
  )

  expect_equal(expert@expert_id, "expert.policy")
  expect_equal(expert@name, "Dr. Rivera")
  expect_equal(expert@title, "Policy analyst")
  expect_equal(
    expert@initial_questions,
    c("What policies matter?", "Who pays?")
  )
  expect_error(
    tempest_expert(
      expert_id = "expert.invalid",
      name = "",
      title = "T",
      description = "P",
      instructions = "Test."
    ),
    "name"
  )
})

test_that("tempest_expert validates stable ids", {
  expect_error(
    tempest_expert(
      expert_id = "not allowed",
      name = "A",
      title = "T",
      description = "P",
      instructions = "Test."
    ),
    "expert_id"
  )
  expect_error(
    tempest_expert(
      expert_id = c("expert.a", "expert.b"),
      name = "A",
      title = "T",
      description = "P",
      instructions = "Test."
    ),
    "expert_id"
  )
  expect_equal(
    test_expert(expert_id = "expert.2")@expert_id,
    "expert.2"
  )
})

test_that("tempest_validate_experts preserves ids and rejects duplicates", {
  validated <- tempest:::tempest_validate_experts(list(
    test_expert(expert_id = "expert.a", name = "A"),
    test_expert(expert_id = "expert.b", name = "B")
  ))
  expect_equal(validated[[1]]@expert_id, "expert.a")
  expect_equal(validated[[2]]@expert_id, "expert.b")

  expect_error(
    tempest:::tempest_validate_experts(list(
      test_expert(expert_id = "expert.duplicate", name = "A"),
      test_expert(expert_id = "expert.duplicate", name = "B")
    )),
    "unique"
  )
  expect_error(
    tempest:::tempest_validate_experts(list(list(name = "A"))),
    "tempest_expert"
  )
})

test_that("tempest_session accepts host experts and a shared retriever", {
  skip_if_not_installed("ellmer")
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  store <- SourceStore$new()
  retriever <- tempest_retriever(config = cfg, store = store)
  expert <- tempest_expert(
    expert_id = "expert.host",
    name = "Host Expert",
    title = "Domain specialist",
    description = "Host-provided context",
    instructions = "Use the host-provided context."
  )

  session <- tempest_session(
    "Host topic",
    config = cfg,
    experts = list(expert),
    retriever = retriever
  )

  expect_identical(session$store, store)
  expect_identical(session$retriever, retriever)
  expect_equal(session$experts[[1]]@expert_id, "expert.host")
  expect_equal(session$experts[[1]]@name, "Host Expert")
})

test_that("tempest_session accepts a host session id", {
  skip_if_not_installed("ellmer")
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  session <- tempest_session(
    "Host topic",
    config = cfg,
    experts = list(test_expert(
      expert_id = "expert.host",
      name = "Host Expert"
    )),
    session_id = "project-123"
  )

  expect_equal(session$session_id, "project-123")
  expect_error(
    tempest_session(
      "Host topic",
      config = cfg,
      experts = list(test_expert(
        expert_id = "expert.host",
        name = "Host Expert"
      )),
      session_id = ""
    ),
    class = "tempest_session_error"
  )
})

test_that("artifact stores can capture report artifacts", {
  skip_if_not_installed("ellmer")
  artifacts <- tempest_memory_artifact_store()
  reporter <- fake_chat(text = list("Report body."))
  cfg <- tempest_config(
    artifact_store = artifacts,
    chat_fn = function(role, model, system_prompt, echo) {
      if (identical(role, "writer")) reporter else fake_chat()
    }
  )
  session <- tempest_session(
    "Artifact topic",
    config = cfg,
    experts = list(tempest_expert(
      expert_id = "expert.artifact",
      name = "Artifact Expert",
      title = "Archivist",
      description = "Artifact capture",
      instructions = "Capture the report artifact."
    ))
  )

  report <- session$report(include_references = FALSE, reorganize = FALSE)

  expect_equal(report, "Report body.")
  expect_equal(artifacts$read("report_md")@content, "Report body.")
  expect_named(artifacts$list(), "report_md")
  expect_identical(artifacts$exists("report_md", "1"), TRUE)
  expect_equal(
    session$artifact_catalog$get("report_md")@content,
    "Report body."
  )
  expect_equal(
    reporter$.calls()[[1]]$prompt,
    tempest:::tempest_costorm_report_prompt(session, "technical")
  )
})

test_that("default artifact store is a no-op adapter", {
  store <- tempest_artifact_store()
  spec <- tempest_deliverable_spec(
    "report",
    title = "Report",
    purpose = "Explain",
    instructions = "Be concise.",
    generator_id = "generator",
    renderer_ids = "renderer"
  )
  artifact <- tempest_artifact(
    spec,
    content = "value",
    artifact_id = "x"
  )

  expect_silent(store$write(artifact))
  expect_null(store$read("x"))
  expect_equal(store$list(), list())
  expect_identical(store$exists("x"), FALSE)
  expect_error(
    tempest_config(artifact_store = list()),
    class = "tempest_artifact_store_error"
  )
  expect_error(
    tempest_artifact_store(write = "not a function"),
    class = "tempest_artifact_store_error"
  )
})

test_that("memory artifact store overwrites existing names", {
  store <- tempest_memory_artifact_store()
  spec <- tempest_deliverable_spec(
    "report",
    title = "Report",
    purpose = "Explain",
    instructions = "Be concise.",
    generator_id = "generator",
    renderer_ids = "renderer"
  )
  store$write(tempest_artifact(
    spec,
    content = "first",
    artifact_id = "report_md"
  ))
  store$write(tempest_artifact(
    spec,
    content = "second",
    artifact_id = "report_md"
  ))

  expect_equal(store$read("report_md")@content, "second")
  expect_named(store$list(), "report_md")
})

test_that("tempest_session rejects a retriever without a SourceStore", {
  skip_if_not_installed("ellmer")
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  expect_error(
    tempest_session("Topic", config = cfg, retriever = 1:3),
    "SourceStore"
  )
  expect_error(
    tempest_session("Topic", config = cfg, retriever = list(store = 1)),
    "SourceStore"
  )
})
