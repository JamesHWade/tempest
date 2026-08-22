test_that("the cohesive product surface hides raw internals", {
  expect_setequal(
    names(tempest:::TempestResult@properties),
    c(
      "title",
      "topic",
      "run_id",
      "status",
      "report_md",
      "output_dir",
      "perspectives",
      "experts",
      "outline",
      "draft_md",
      "manifest",
      "state",
      "workspace",
      "retriever"
    )
  )
  expect_identical(names(formals(tempest_report)), "x")
  expect_identical(names(formals(tempest_sources)), "x")
  expect_identical(names(formals(tempest_claims)), "x")
  expect_identical(names(formals(tempest_claim_supports)), "x")
})

test_that("product reads reject values that are not a run or session", {
  for (accessor in list(
    tempest_report,
    tempest_sources,
    tempest_claims,
    tempest_claim_supports
  )) {
    expect_error(accessor(list(workspace = "raw")))
  }
  expect_error(
    tempest:::tempest_product_read_workspace(tempest_research_workspace()),
    class = "tempest_product_result_error"
  )
})

test_that("TempestSession exposes only the product interaction contract", {
  generator <- get("TempestSession", envir = asNamespace("tempest"))

  expect_setequal(
    setdiff(names(generator$public_methods), c("initialize", "clone")),
    c(
      "warmup",
      "step",
      "suggest_questions",
      "add_expert",
      "retire_expert",
      "publish"
    )
  )
  expect_setequal(
    names(generator$active),
    c("session_id", "topic", "status", "experts", "transcript", "mindmap")
  )
})

test_that("progress callbacks receive the canonical plain record", {
  event <- tempest:::tempest_progress_event(
    run_id = "progress-record-run",
    workflow = "storm",
    event_type = "stage",
    status = "started",
    stage = "perspectives"
  )
  seen <- NULL
  callback <- tempest:::tempest_progress_callback(function(x) seen <<- x)
  callback(event)

  expect_identical(
    names(seen),
    tempest:::tempest_progress_record_fields()
  )
  expect_identical(seen$run_id, "progress-record-run")
  expect_identical(seen$stage, "perspectives")
  expect_identical(seen$payload, list())
  expect_all_true(vapply(
    seen[setdiff(names(seen), "payload")],
    \(value) is.character(value) && length(value) == 1L,
    logical(1)
  ))
  expect_identical(class(seen), "list")
})

test_that("a progress collector accepts either progress record shape", {
  event <- tempest:::tempest_progress_event(
    run_id = "progress-collector-run",
    workflow = "storm",
    event_type = "stage",
    status = "started",
    stage = "outline"
  )
  collector <- tempest_progress_collector()
  collector$record(event)
  collector$record(tempest:::tempest_progress_record(event))

  recorded <- collector$events()
  expect_length(recorded, 2L)
  expect_identical(
    vapply(recorded, \(x) x@stage, character(1)),
    c("outline", "outline")
  )
})
