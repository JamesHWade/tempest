test_that("tempest_expert creates validated host expert definitions", {
  expert <- tempest_expert(
    name = "Dr. Rivera",
    title = "Policy analyst",
    perspective = "Policy incentives",
    initial_questions = c("What policies matter?", "Who pays?")
  )

  expect_equal(expert$name, "Dr. Rivera")
  expect_equal(expert$title, "Policy analyst")
  expect_equal(
    expert$initial_questions,
    c("What policies matter?", "Who pays?")
  )
  expect_error(
    tempest_expert(name = "", title = "T", perspective = "P"),
    "name"
  )
})

test_that("tempest_expert rejects ids that are not coercible to an integer", {
  expect_error(
    tempest_expert(name = "A", title = "T", perspective = "P", id = "abc"),
    "id"
  )
  expect_error(
    tempest_expert(name = "A", title = "T", perspective = "P", id = c(1, 2)),
    "id"
  )
  expect_equal(
    tempest_expert(name = "A", title = "T", perspective = "P", id = "2")$id,
    2L
  )
})

test_that("tempest_validate_personas assigns ids and rejects duplicates", {
  validated <- tempest_validate_personas(list(
    list(name = "A"),
    list(name = "B", id = 5)
  ))
  expect_equal(validated[[1]]$id, 1L)
  expect_equal(validated[[2]]$id, 5L)

  expect_error(
    tempest_validate_personas(list(
      list(name = "A", id = 1),
      list(name = "B", id = 1)
    )),
    "unique"
  )
  expect_error(
    tempest_validate_personas(list(list(name = "A", id = "abc"))),
    "id"
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
    name = "Host Expert",
    title = "Domain specialist",
    perspective = "Host-provided context"
  )

  session <- tempest_session(
    "Host topic",
    config = cfg,
    personas = list(expert),
    retriever = retriever
  )

  expect_identical(session$store, store)
  expect_identical(session$retriever, retriever)
  expect_equal(session$personas[[1]]$id, 1L)
  expect_equal(session$personas[[1]]$name, "Host Expert")
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
    personas = list(tempest_expert(
      name = "Artifact Expert",
      title = "Archivist",
      perspective = "Artifact capture"
    ))
  )

  report <- session$report(include_references = FALSE, reorganize = FALSE)

  expect_equal(report, "Report body.")
  expect_equal(artifacts$read("report_md"), "Report body.")
  expect_equal(artifacts$list(), "report_md")
})

test_that("default artifact store is a no-op adapter", {
  store <- tempest_artifact_store()

  expect_silent(store$write("x", 1, metadata = list()))
  expect_null(store$read("x"))
  expect_equal(store$list(), character())
  expect_error(
    tempest_config(artifact_store = list()),
    "artifact_store"
  )
})

test_that("memory artifact store overwrites existing names", {
  store <- tempest_memory_artifact_store()
  store$write("report_md", "first")
  store$write("report_md", "second")

  expect_equal(store$read("report_md"), "second")
  expect_equal(store$list(), "report_md")
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
