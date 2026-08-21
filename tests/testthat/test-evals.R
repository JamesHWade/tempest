test_that("built-in evaluation dataset has the public task contract", {
  dataset <- tempest:::tempest_eval_dataset("qa")
  normalized <- tempest:::tempest_evaluation_dataset_normalize("qa")

  expect_s3_class(dataset, "tbl_df")
  expect_named(dataset, c("input", "target"))
  expect_gt(nrow(dataset), 0L)
  expect_all_true(nzchar(dataset$input))
  expect_all_true(nzchar(dataset$target))
  expect_named(normalized$data, c("input", "target", "id"))
  expect_identical(normalized$kind, "builtin")
  expect_match(normalized$digest, "^sha256:[a-f0-9]{64}$")
  expect_identical(normalized$metadata$row_count, nrow(dataset))
  expect_match(
    tempest:::tempest_evaluation_task_name("storm", normalized),
    paste0("^tempest-storm-builtin-qa-[a-f0-9]{64}$")
  )
})

test_that("caller evaluation datasets have one canonical identity", {
  reordered <- data.frame(
    target = c("Answer one", "Answer two"),
    id = c("case-1", "case-2"),
    input = c("Question one", "Question two")
  )
  canonical <- reordered[c("input", "target", "id")]

  first <- tempest:::tempest_evaluation_dataset_normalize(reordered)
  second <- tempest:::tempest_evaluation_dataset_normalize(canonical)
  changed <- tempest:::tempest_evaluation_dataset_normalize(
    transform(canonical, target = c("Changed", "Answer two"))
  )

  expect_identical(first$data, tibble::as_tibble(canonical))
  expect_identical(first$kind, "caller")
  expect_identical(first$digest, second$digest)
  expect_identical(
    tempest:::tempest_evaluation_task_name("storm", first),
    tempest:::tempest_evaluation_task_name("storm", second)
  )
  expect_identical(identical(first$digest, changed$digest), FALSE)
})

test_that("caller evaluation datasets reject ambiguous rows", {
  valid <- data.frame(
    input = c("Question one", "Question two"),
    target = c("Answer one", "Answer two"),
    id = c("case-1", "case-2")
  )
  invalid <- list(
    valid["input"],
    transform(valid, extra = "no"),
    transform(valid, id = c("case-1", "case-1")),
    transform(valid, input = c("", "Question two")),
    transform(valid, input = c(" Question one", "Question two")),
    transform(valid, target = factor(target)),
    transform(valid, input = I(list("Question one", "Question two"))),
    valid[FALSE, ]
  )

  for (dataset in invalid) {
    expect_error(
      tempest:::tempest_evaluation_dataset_normalize(dataset),
      class = "tempest_evaluation_error"
    )
  }
  expect_error(
    tempest:::tempest_evaluation_dataset_normalize("benchmark"),
    class = "tempest_evaluation_error"
  )
  expect_error(
    tempest:::tempest_evaluation_dataset_metadata_validate(
      list(api_key = "secret")
    ),
    class = "tempest_evaluation_error"
  )
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
  config <- rlang::duplicate(fixture$result@retriever$config, shallow = FALSE)
  config@chat_fn <- function(role, model, system_prompt, echo) fake_chat()
  calls <- list()
  local_mocked_bindings(
    tempest_eval_dataset = function(...) {
      tibble::tibble(input = "Question one", target = "Answer one")
    },
    tempest_run_internal = function(...) {
      calls[[length(calls) + 1L]] <<- list(...)
      fixture$result
    }
  )
  dataset <- tempest:::tempest_evaluation_dataset_normalize("qa")
  program_set <- new.env(parent = emptyenv())
  knowledge_view <- list(snapshot_id = "snapshot:test")

  solved <- tempest:::tempest_solver_storm(
    "Question one",
    dataset = dataset$metadata,
    config = config,
    program_set = program_set,
    knowledge_view = knowledge_view
  )

  expect_length(calls, 1L)
  expect_identical(calls[[1L]]$topic, "Question one")
  expect_identical(calls[[1L]]$n_experts, 1L)
  expect_identical(calls[[1L]]$program_set, program_set)
  expect_identical(calls[[1L]]$knowledge_view, knowledge_view)
  expect_identical(solved$result, fixture$result@report_md)
  expect_s3_class(solved$solver_chat[[1L]], "Chat")
  expect_named(
    solved$solver_metadata[[1]],
    c(
      "metadata_version",
      "dataset",
      "review",
      "product",
      "programs",
      "stages"
    )
  )
  expect_identical(
    solved$solver_metadata[[1L]]$product$report_reference,
    fixture$result@manifest@deliverables$report_md[
      c("report_id", "sha256")
    ]
  )
  expect_identical(solved$solver_metadata[[1L]]$dataset, dataset$metadata)
  expect_identical(
    names(solved$solver_metadata[[1L]]$programs),
    tempest:::tempest_program_set_stages()
  )
  expect_identical(
    names(solved$solver_metadata[[1L]]$stages$items),
    tempest:::tempest_program_set_stages()
  )
  expect_identical(solved$solver_metadata[[1L]]$metadata_version, 1L)
  expect_match(
    solved$solver_metadata[[1L]]$review$review_id,
    "^sha256:[a-f0-9]{64}$"
  )
  expect_match(
    solved$solver_metadata[[1L]]$stages$digest,
    "^sha256:[a-f0-9]{64}$"
  )
  expect_identical(
    solved$solver_metadata[[1L]]$stages$digest,
    tempest_trajectory_review(fixture$result)@stages$digest
  )
  for (program in solved$solver_metadata[[1L]]$programs) {
    expect_named(
      program,
      c(
        "stage",
        "contract_version",
        "program_artifact_id",
        "evaluator_id",
        "evaluator_version"
      )
    )
  }
  for (stage in solved$solver_metadata[[1L]]$stages$items) {
    expect_named(
      stage,
      c(
        "stage",
        "attempt_count",
        "fallback_count",
        "execution_counts",
        "support_counts",
        "publication_counts",
        "finding_counts"
      )
    )
  }
  metadata_json <- tempest:::tempest_product_canonical_json(
    solved$solver_metadata[[1L]]
  )
  expect_no_match(metadata_json, "attempt_id", fixed = TRUE)
  expect_no_match(metadata_json, "evidence_span_id", fixed = TRUE)
  expect_no_match(metadata_json, "Question one", fixed = TRUE)
  expect_no_match(metadata_json, "Answer one", fixed = TRUE)
  expect_no_match(
    metadata_json,
    "STORM progress emits stage events",
    fixed = TRUE
  )

  contains_live_capability <- function(value) {
    if (
      is.function(value) ||
        is.environment(value) ||
        identical(typeof(value), "externalptr") ||
        inherits(value, c("Chat", "Agent", "R6", "S7_object"))
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
  expect_error(
    tempest:::tempest_evaluation_product_metadata(
      research = fixture$result,
      manifest = fixture$result@manifest,
      report_md = fixture$result@report_md,
      stage_records = fixture$result@state$stage_records[-1L],
      mode = "storm",
      dataset = dataset$metadata
    ),
    class = "tempest_evaluation_error",
    regexp = "do not match"
  )

  task <- suppressWarnings(tempest_task(
    config = config,
    scorer = function(...) 1
  ))
  suppressWarnings(task$solve())
  expect_identical(nrow(task$get_samples()), 1L)
  expect_identical(
    task$get_samples()$result,
    fixture$result@report_md
  )
  expect_contains(names(task$get_samples()), "solver_metadata")
  expect_identical(
    task$get_samples()$solver_metadata[[1L]]$dataset,
    dataset$metadata
  )
})

test_that("default Co-STORM solver uses the committed session product", {
  skip_if_not_installed("ellmer")
  fixture <- costorm_product_baseline_fixture()
  calls <- list()
  chat_calls <- character()
  session_chat <- tempest:::tempest_session_chat
  local_mocked_bindings(
    tempest_eval_dataset = function(...) {
      tibble::tibble(input = "Question one", target = "Answer one")
    },
    tempest_costorm_evaluation_product = function(
      topic,
      config,
      max_turns,
      program_set = NULL,
      knowledge_view = NULL
    ) {
      calls[[length(calls) + 1L]] <<- list(
        topic = topic,
        config = config,
        max_turns = max_turns,
        program_set = program_set,
        knowledge_view = knowledge_view
      )
      list(session = fixture$session, turns = 2L)
    },
    tempest_session_chat = function(session, role) {
      chat_calls <<- c(chat_calls, role)
      session_chat(session, role)
    }
  )
  dataset <- tempest:::tempest_evaluation_dataset_normalize("qa")
  program_set <- new.env(parent = emptyenv())
  knowledge_view <- list(snapshot_id = "snapshot:test")

  solved <- tempest:::tempest_solver_costorm(
    "Question one",
    dataset = dataset$metadata,
    config = fixture$config,
    max_turns = 2L,
    program_set = program_set,
    knowledge_view = knowledge_view
  )

  expect_length(calls, 1L)
  expect_identical(calls[[1L]]$topic, "Question one")
  expect_identical(calls[[1L]]$max_turns, 2L)
  expect_identical(calls[[1L]]$program_set, program_set)
  expect_identical(calls[[1L]]$knowledge_view, knowledge_view)
  expect_identical(chat_calls, "moderator")
  expect_identical(solved$result, fixture$report)
  expect_s3_class(solved$solver_chat[[1L]], "Chat")
  expect_named(
    solved$solver_metadata[[1L]],
    c(
      "metadata_version",
      "dataset",
      "review",
      "product",
      "programs",
      "stages"
    )
  )
  expect_identical(solved$solver_metadata[[1L]]$dataset, dataset$metadata)

  contains_live_capability <- function(value) {
    if (
      is.function(value) ||
        is.environment(value) ||
        identical(typeof(value), "externalptr") ||
        inherits(value, c("Chat", "Agent", "R6", "S7_object"))
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
  expect_identical(nrow(task$get_samples()), 1L)
  expect_identical(
    task$get_samples()$result,
    fixture$report
  )
  expect_contains(names(task$get_samples()), "solver_metadata")
  expect_identical(
    task$get_samples()$solver_metadata[[1L]]$dataset,
    dataset$metadata
  )
})

test_that("evaluation task constructors accept fake solver and scorer contracts", {
  skip_if_not_installed("vitals")
  skip_if_not_installed("ellmer")
  calls <- list()
  solver <- function(input, config, max_turns = NULL, ...) {
    calls[[length(calls) + 1L]] <<- list(
      input = input,
      config = config,
      max_turns = max_turns
    )
    list(
      result = rep("fixture", length(input)),
      solver_chat = rep(list(fake_chat()), length(input))
    )
  }
  scorer <- function(...) 1
  dataset <- data.frame(
    input = "Question one",
    target = "Answer one",
    id = "case-1"
  )

  storm_task <- suppressWarnings(tempest_task(
    dataset = dataset,
    solver = solver,
    scorer = scorer
  ))
  costorm_task <- suppressWarnings(tempest_costorm_task(
    dataset = dataset,
    solver = solver,
    scorer = scorer,
    max_turns = 2L
  ))
  suppressWarnings(storm_task$solve())
  suppressWarnings(costorm_task$solve())

  expect_r6_class(storm_task, "Task")
  expect_r6_class(costorm_task, "Task")
  expect_null(formals(tempest_task)$solver)
  expect_null(formals(tempest_costorm_task)$solver)
  expect_null(formals(tempest_task)$program_set)
  expect_null(formals(tempest_task)$knowledge_view)
  expect_null(formals(tempest_costorm_task)$program_set)
  expect_null(formals(tempest_costorm_task)$knowledge_view)
  expected_fields <- c("input", "target", "id", "result", "solver_chat")
  expect_named(storm_task$get_samples(), expected_fields)
  expect_named(costorm_task$get_samples(), expected_fields)
  expect_length(calls, 2L)
  expect_null(calls[[1L]]$max_turns)
  expect_identical(calls[[2L]]$max_turns, 2L)
  expect_identical(calls[[1L]]$input, "Question one")
  expect_identical(calls[[2L]]$input, "Question one")
  expect_error(
    tempest_costorm_task(solver = solver, scorer = scorer, max_turns = 0),
    class = "tempest_config_error"
  )
})

test_that("built-in tasks route exact inputs without compiling programs", {
  skip_if_not_installed("vitals")
  skip_if_not_installed("ellmer")
  program_set <- new.env(parent = emptyenv())
  knowledge_view <- list(snapshot_id = "snapshot:test")
  calls <- list()
  solved <- function(input) {
    list(
      result = rep("fixture", length(input)),
      solver_chat = rep(list(fake_chat()), length(input))
    )
  }
  local_mocked_bindings(
    tempest_compile_programs = function(...) {
      stop("evaluation must not compile programs")
    },
    tempest_solver_storm = function(
      input,
      dataset,
      config,
      program_set,
      knowledge_view
    ) {
      calls$storm <<- list(
        dataset = dataset,
        program_set = program_set,
        knowledge_view = knowledge_view
      )
      solved(input)
    },
    tempest_solver_costorm = function(
      input,
      dataset,
      config,
      max_turns,
      program_set,
      knowledge_view
    ) {
      calls$costorm <<- list(
        dataset = dataset,
        program_set = program_set,
        knowledge_view = knowledge_view
      )
      solved(input)
    }
  )

  storm_task <- suppressWarnings(tempest_task(
    program_set = program_set,
    knowledge_view = knowledge_view,
    scorer = function(...) 1
  ))
  costorm_task <- suppressWarnings(tempest_costorm_task(
    program_set = program_set,
    knowledge_view = knowledge_view,
    scorer = function(...) 1
  ))
  suppressWarnings(storm_task$solve())
  suppressWarnings(costorm_task$solve())

  expect_identical(calls$storm$program_set, program_set)
  expect_identical(calls$storm$knowledge_view, knowledge_view)
  expect_identical(calls$costorm$program_set, program_set)
  expect_identical(calls$costorm$knowledge_view, knowledge_view)
  expect_identical(calls$storm$dataset$kind, "builtin")
  expect_identical(calls$costorm$dataset, calls$storm$dataset)
})

test_that("custom solvers cannot claim Tempest governed inputs", {
  skip_if_not_installed("vitals")
  skip_if_not_installed("ellmer")
  solver <- function(input, ...) {
    list(
      result = rep("fixture", length(input)),
      solver_chat = rep(list(fake_chat()), length(input))
    )
  }
  scorer <- function(...) 1

  expect_error(
    tempest_task(
      solver = solver,
      scorer = scorer,
      program_set = list()
    ),
    class = "tempest_evaluation_error"
  )
  expect_error(
    tempest_costorm_task(
      solver = solver,
      scorer = scorer,
      knowledge_view = list()
    ),
    class = "tempest_evaluation_error"
  )
})

test_that("baseline and candidate evaluations use separate mutable Tasks", {
  skip_if_not_installed("vitals")
  skip_if_not_installed("ellmer")
  solver <- function(input, ...) {
    list(
      result = rep("fixture", length(input)),
      solver_chat = rep(list(fake_chat()), length(input))
    )
  }
  scorer <- function(...) 1
  dataset <- data.frame(input = "Question", target = "Answer")

  baseline <- suppressWarnings(tempest_task(
    dataset = dataset,
    solver = solver,
    scorer = scorer
  ))
  candidate <- suppressWarnings(tempest_task(
    dataset = dataset,
    solver = solver,
    scorer = scorer
  ))
  suppressWarnings(candidate$solve())

  expect_identical(identical(baseline, candidate), FALSE)
  expect_named(baseline$get_samples(), c("input", "target", "id"))
  expect_named(
    candidate$get_samples(),
    c("input", "target", "id", "result", "solver_chat")
  )
})
