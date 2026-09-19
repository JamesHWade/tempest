test_that("product entry points accept only the validated knowledge value", {
  expect_error(
    tempest_run("Topic", knowledge = list(view = "raw")),
    class = "tempest_knowledge_error"
  )
  expect_error(
    tempest_session("Topic", knowledge = "raw-view"),
    class = "tempest_knowledge_error"
  )
  expect_identical(
    "knowledge_view" %in% names(formals(tempest_run)),
    FALSE
  )
  expect_identical(
    "program_set" %in% names(formals(tempest_run)),
    FALSE
  )
  expect_identical(
    "knowledge_view" %in% names(formals(tempest_session)),
    FALSE
  )
  expect_identical(
    "program_set" %in% names(formals(tempest_session)),
    FALSE
  )
})

test_that("an absent knowledge value resolves the builtin program set", {
  resolved <- tempest:::tempest_knowledge_argument(NULL)

  expect_null(resolved$value)
  expect_identical(resolved$records, list())
  expect_s7_class(resolved$program_set, tempest:::TempestProgramSet)
})
