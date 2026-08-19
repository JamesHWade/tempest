test_that("built-in evaluation dataset has the public task contract", {
  dataset <- tempest:::tempest_eval_dataset("qa")

  expect_s3_class(dataset, "tbl_df")
  expect_named(dataset, c("input", "target"))
  expect_gt(nrow(dataset), 0L)
  expect_all_true(nzchar(dataset$input))
  expect_all_true(nzchar(dataset$target))
})

test_that("retired report and lightweight evaluation prompts are absent", {
  prompt_roles <- c(
    "polisher",
    "reporter",
    paste0("qa", "_solver")
  )
  prompts <- paste0(prompt_roles, "_system.md")
  paths <- vapply(
    prompts,
    \(prompt) system.file("prompts", prompt, package = "tempest"),
    character(1)
  )

  expect_identical(unname(paths), rep("", length(prompts)))
})

test_that("default STORM solver returns an authoritative product report", {
  skip_if_not_installed("ellmer")
  fixture <- storm_product_baseline_fixture()
  config <- rlang::duplicate(fixture$result$retriever$config, shallow = FALSE)
  config@chat_fn <- function(role, model, system_prompt, echo) fake_chat()
  calls <- list()
  local_mocked_bindings(
    tempest_run = function(...) {
      calls[[length(calls) + 1L]] <<- list(...)
      fixture$result
    }
  )

  solved <- tempest:::tempest_solver_storm(
    "Question one",
    config = config
  )

  expect_length(calls, 1L)
  expect_identical(calls[[1L]]$topic, "Question one")
  expect_identical(calls[[1L]]$n_experts, 1L)
  expect_identical(solved$result, fixture$result$report_md)
  expect_s3_class(solved$solver_chat[[1L]], "Chat")
  expect_named(
    solved$solver_metadata[[1]],
    c("manifest", "workspace", "stage_records")
  )
  expect_identical(
    solved$solver_metadata[[1L]]$manifest$report_reference,
    fixture$result$manifest@deliverables$report_md[
      c("report_id", "sha256")
    ]
  )

  contains_live_capability <- function(value) {
    if (
      is.function(value) ||
        is.environment(value) ||
        identical(typeof(value), "externalptr") ||
        inherits(value, c("Chat", "Agent", "R6"))
    ) {
      return(TRUE)
    }
    if (!is.list(value)) {
      return(FALSE)
    }
    any(vapply(value, contains_live_capability, logical(1)))
  }
  expect_identical(
    contains_live_capability(solved$solver_metadata),
    FALSE
  )

  task <- suppressWarnings(tempest_task(
    config = config,
    scorer = function(...) 1
  ))
  suppressWarnings(task$solve())
  expect_identical(
    task$get_samples()$result,
    rep(fixture$result$report_md, nrow(task$get_samples()))
  )
})

test_that("default Co-STORM solver uses the committed session product", {
  skip_if_not_installed("ellmer")
  fixture <- costorm_product_baseline_fixture()
  calls <- list()
  chat_calls <- character()
  session_chat <- tempest:::tempest_session_chat
  local_mocked_bindings(
    tempest_costorm_evaluation_product = function(
      topic,
      config,
      max_turns
    ) {
      calls[[length(calls) + 1L]] <<- list(
        topic = topic,
        config = config,
        max_turns = max_turns
      )
      list(session = fixture$session, turns = 2L)
    },
    tempest_session_chat = function(session, role) {
      chat_calls <<- c(chat_calls, role)
      session_chat(session, role)
    }
  )

  solved <- tempest:::tempest_solver_costorm(
    "Question one",
    config = fixture$config,
    max_turns = 2L
  )

  expect_length(calls, 1L)
  expect_identical(calls[[1L]]$topic, "Question one")
  expect_identical(calls[[1L]]$max_turns, 2L)
  expect_identical(chat_calls, "moderator")
  expect_identical(solved$result, fixture$report)
  expect_s3_class(solved$solver_chat[[1L]], "Chat")
  expect_named(
    solved$solver_metadata[[1L]],
    c(
      "turns",
      "deputy_traces",
      "manifest",
      "workspace",
      "stage_records"
    )
  )

  contains_live_capability <- function(value) {
    if (
      is.function(value) ||
        is.environment(value) ||
        identical(typeof(value), "externalptr") ||
        inherits(value, c("Chat", "Agent", "R6"))
    ) {
      return(TRUE)
    }
    if (!is.list(value)) {
      return(FALSE)
    }
    any(vapply(value, contains_live_capability, logical(1)))
  }
  expect_identical(
    contains_live_capability(solved$solver_metadata),
    FALSE
  )

  task <- suppressWarnings(tempest_costorm_task(
    config = fixture$config,
    max_turns = 2L,
    scorer = function(...) 1
  ))
  suppressWarnings(task$solve())
  expect_identical(
    task$get_samples()$result,
    rep(fixture$report, nrow(task$get_samples()))
  )
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
  expect_null(formals(tempest_task)$solver)
  expect_null(formals(tempest_costorm_task)$solver)
  expect_named(storm_task$get_samples(), c("input", "target", "id"))
  expect_named(costorm_task$get_samples(), c("input", "target", "id"))
  expect_error(
    tempest_costorm_task(solver = solver, scorer = scorer, max_turns = 0),
    class = "tempest_config_error"
  )
})
