test_that("built-in evaluation dataset has the public task contract", {
  dataset <- tempest:::tempest_eval_dataset("qa")

  expect_s3_class(dataset, "tbl_df")
  expect_named(dataset, c("input", "target"))
  expect_gt(nrow(dataset), 0L)
  expect_all_true(nzchar(dataset$input))
  expect_all_true(nzchar(dataset$target))
})

test_that("cited-answer solver runs with a fake chat and no network", {
  skip_if_not_installed("ellmer")
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) {
      fake_chat(text = list("Fixture answer"))
    }
  )

  solved <- tempest:::tempest_solver_cited_answer(
    c("Question one", "Question two"),
    config = cfg
  )

  expect_equal(solved$result, c("Fixture answer", "Fixture answer"))
  expect_length(solved$solver_chat, 2L)
  expect_length(solved$solver_metadata, 2L)
  expect_named(solved$solver_metadata[[1]], c("sources", "claims"))
})

test_that("evaluation task constructors accept fake solver and scorer contracts", {
  skip_if_not_installed("vitals")
  skip_if_not_installed("ellmer")
  solver <- function(input, ...) {
    list(
      result = rep("fixture", length(input)),
      solver_chat = vector("list", length(input))
    )
  }
  scorer <- function(...) 1

  storm_task <- suppressWarnings(tempest_task(
    solver = solver,
    scorer = scorer
  ))
  costorm_task <- suppressWarnings(tempest_costorm_task(
    solver = solver,
    scorer = scorer,
    max_turns = 2L
  ))

  expect_r6_class(storm_task, "Task")
  expect_r6_class(costorm_task, "Task")
  expect_named(storm_task$get_samples(), c("input", "target", "id"))
  expect_named(costorm_task$get_samples(), c("input", "target", "id"))
  expect_error(
    tempest_costorm_task(solver = solver, scorer = scorer, max_turns = 0),
    class = "tempest_config_error"
  )
})
