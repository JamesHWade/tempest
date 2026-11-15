test_that("the frozen synthetic corpus has topic-disjoint complete splits", {
  context <- research_eval_test_context()
  skip_if(
    is.null(context),
    "developer harness is excluded from source tarballs"
  )
  eval <- context$eval
  corpus <- eval$research_eval_read_corpus(context$corpus_path)
  rows <- eval$research_eval_case_table(corpus)

  expect_identical(corpus$synthetic, TRUE)
  expect_identical(corpus$human_adjudicated, FALSE)
  expect_match(attr(corpus, "sha256"), "^[a-f0-9]{64}$")
  expect_setequal(
    unique(rows$challenge_type[rows$split == "train"]),
    eval$research_eval_required_challenges()
  )
  expect_setequal(
    unique(rows$challenge_type[rows$split == "validation"]),
    eval$research_eval_required_challenges()
  )
  expect_setequal(
    unique(rows$challenge_type[rows$split == "test"]),
    eval$research_eval_required_challenges()
  )
  expect_identical(length(unique(rows$topic_id[rows$split == "train"])), 8L)
  expect_identical(
    length(unique(rows$topic_id[rows$split == "validation"])),
    8L
  )
  expect_identical(length(unique(rows$topic_id[rows$split == "test"])), 8L)
  expect_length(
    intersect(
      unique(rows$topic_id[rows$split == "train"]),
      unique(rows$topic_id[rows$split == "validation"])
    ),
    0L
  )
  expect_length(
    intersect(
      unique(rows$topic_id[rows$split == "train"]),
      unique(rows$topic_id[rows$split == "test"])
    ),
    0L
  )
  expect_length(
    intersect(
      unique(rows$topic_id[rows$split == "validation"]),
      unique(rows$topic_id[rows$split == "test"])
    ),
    0L
  )
  claim_sets <- lapply(corpus$splits, function(split) {
    unique(rows$claim_text[rows$split == split])
  })
  for (left in seq_len(length(claim_sets) - 1L)) {
    for (right in seq.int(left + 1L, length(claim_sets))) {
      expect_length(intersect(claim_sets[[left]], claim_sets[[right]]), 0L)
    }
  }
})

test_that("the corpus distinguishes source-specific contradiction and duplication", {
  context <- research_eval_test_context()
  skip_if(
    is.null(context),
    "developer harness is excluded from source tarballs"
  )
  eval <- context$eval
  corpus <- eval$research_eval_read_corpus(context$corpus_path)
  rows <- eval$research_eval_case_table(corpus)
  conflict <- rows[
    rows$split == "test" & rows$challenge_type == "contradictory_studies",
    ,
    drop = FALSE
  ]
  copies <- rows[
    rows$split == "test" &
      rows$challenge_type == "misleading_duplicate_sources",
    ,
    drop = FALSE
  ]

  expect_setequal(conflict$expected_status, c("supported", "contradicted"))
  expect_identical(length(unique(conflict$topic_id)), 1L)
  expect_setequal(copies$expected_status, "unsupported")
  expect_identical(length(unique(copies$source_origin_id)), 1L)
  expect_setequal(
    unique(vapply(corpus$cases, `[[`, character(1), "judgement_basis")),
    "synthetic_design_not_human_adjudication"
  )
})

test_that("citation status, false support, false abstention, and coverage stay separate", {
  context <- research_eval_test_context()
  skip_if(
    is.null(context),
    "developer harness is excluded from source tarballs"
  )
  eval <- context$eval
  corpus <- eval$research_eval_read_corpus(context$corpus_path)
  rows <- eval$research_eval_split_data(corpus, "test")
  predictions <- data.frame(
    case_id = rows$case_id,
    predicted_status = rows$expected_status,
    stringsAsFactors = FALSE
  )
  supported_index <- which(rows$expected_status == "supported")[[1L]]
  unsupported_index <- which(rows$expected_status == "unsupported")[[1L]]
  predictions$predicted_status[[supported_index]] <- "unverifiable"
  predictions$predicted_status[[unsupported_index]] <- "supported"
  summary <- eval$research_eval_summarize(predictions, rows)

  expect_identical(
    summary$overall$citation_support_correctness$correct,
    nrow(rows) - 2L
  )
  expect_identical(summary$overall$false_support$count, 1L)
  expect_identical(summary$overall$false_support$denominator, 5L)
  expect_identical(summary$overall$full_support_overcall$count, 1L)
  expect_identical(summary$overall$false_abstention$count, 1L)
  expect_identical(summary$overall$false_abstention$denominator, 9L)
  expect_identical(summary$overall$coverage$covered, 8L)
  expect_equal(summary$overall$coverage$rate, 0.8)
  expect_equal(summary$overall$coverage$answerable_rate, 8 / 9)
})

test_that("provider failures are nonresponses, not correct unverifiable decisions", {
  context <- research_eval_test_context()
  skip_if(
    is.null(context),
    "developer harness is excluded from source tarballs"
  )
  eval <- context$eval
  corpus <- eval$research_eval_read_corpus(context$corpus_path)
  rows <- eval$research_eval_split_data(corpus, "test")
  failed <- eval$research_eval_extract_evaluation_predictions(
    list(
      predictions = rep(list(NULL), nrow(rows)),
      metadata = rep(list(list(error = "provider failed")), nrow(rows))
    ),
    rows
  )
  summary <- eval$research_eval_summarize(failed, rows)

  expect_identical(
    summary$overall$citation_support_correctness$correct,
    0L
  )
  expect_equal(summary$overall$citation_support_correctness$accuracy, 0)
  expect_identical(summary$overall$execution_errors$count, nrow(rows))
  expect_equal(summary$overall$execution_errors$rate, 1)
  expect_identical(summary$overall$coverage$covered, 0L)
  expect_equal(summary$overall$coverage$rate, 0)
  expect_identical(summary$overall$false_abstention$count, 0L)
  expect_identical(summary$overall$correct_unverifiable$count, 0L)
  expect_identical(
    summary$cost_and_trace$trace_event_count$observed_sum,
    NA_real_
  )
  expect_identical(
    summary$cost_and_trace$trace_event_count$observed_rows,
    0L
  )
  expect_identical(
    summary$cost_and_trace$trace_event_count$unknown_rows,
    nrow(rows)
  )
  challenge_failures <- vapply(
    summary$by_challenge,
    function(item) item$execution_errors$count,
    numeric(1)
  )
  expect_equal(sum(challenge_failures), nrow(rows))
  expect_identical(all(challenge_failures > 0), TRUE)

  malformed <- eval$research_eval_extract_evaluation_predictions(
    list(
      predictions = rep(list(list(status = "bogus")), nrow(rows)),
      metadata = rep(list(list()), nrow(rows)),
      traces = rep(list(list()), nrow(rows))
    ),
    rows
  )
  expect_setequal(
    unique(malformed$execution_error),
    "malformed_status_output"
  )
  malformed_summary <- eval$research_eval_summarize(malformed, rows)
  expect_identical(
    malformed_summary$overall$execution_errors$count,
    nrow(rows)
  )
})

test_that("trace event counts distinguish missing telemetry from an empty trace", {
  context <- research_eval_test_context()
  skip_if(
    is.null(context),
    "developer harness is excluded from source tarballs"
  )
  eval <- context$eval
  corpus <- eval$research_eval_read_corpus(context$corpus_path)
  rows <- eval$research_eval_split_data(corpus, "test")
  traces <- rep(list(NULL), nrow(rows))
  traces[[1L]] <- list(events = list())
  traces[[2L]] <- list(metadata = list())
  predictions <- eval$research_eval_extract_evaluation_predictions(
    list(
      predictions = lapply(rows$expected_status, function(status) {
        list(status = status)
      }),
      metadata = rep(list(list()), nrow(rows)),
      traces = traces
    ),
    rows
  )
  summary <- eval$research_eval_summarize(predictions, rows)
  event_counts <- summary$cost_and_trace$trace_event_count

  expect_identical(predictions$trace_event_count[[1L]], 0)
  expect_identical(predictions$trace_event_count[[2L]], NA_real_)
  expect_identical(predictions$trace_event_count[[3L]], NA_real_)
  expect_identical(event_counts$observed_sum, 0)
  expect_identical(event_counts$observed_mean, 0)
  expect_identical(event_counts$observed_rows, 1L)
  expect_identical(event_counts$unknown_rows, nrow(rows) - 1L)
})

test_that("offline selfcheck writes corpus and model provenance without program claims", {
  context <- research_eval_test_context()
  skip_if(
    is.null(context),
    "developer harness is excluded from source tarballs"
  )
  eval <- context$eval
  corpus <- eval$research_eval_read_corpus(context$corpus_path)
  rows <- eval$research_eval_split_data(corpus, "test")
  predictions <- data.frame(
    case_id = rows$case_id,
    predicted_status = rows$expected_status,
    stringsAsFactors = FALSE
  )
  report_path <- tempfile(fileext = ".json")
  report <- eval$research_eval_selfcheck(
    predictions,
    split = "test",
    corpus_path = context$corpus_path,
    report_path = report_path
  )
  saved <- jsonlite::read_json(report_path, simplifyVector = FALSE)

  expect_identical(file.exists(report_path), TRUE)
  expect_identical(report$corpus$sha256, attr(corpus, "sha256"))
  expect_identical(
    report$model_ids$task_model_id,
    "supplied-predictions-offline"
  )
  expect_null(report$program_ids$baseline)
  expect_null(report$optimization_result)
  expect_equal(
    saved$evaluations$test$overall$citation_support_correctness$accuracy,
    1
  )
  expect_identical(report$split$test_used_for_optimization, FALSE)
})

test_that("selfcheck rejects a corpus that labels fixtures as human gold", {
  context <- research_eval_test_context()
  skip_if(
    is.null(context),
    "developer harness is excluded from source tarballs"
  )
  eval <- context$eval
  corpus <- eval$research_eval_read_corpus(context$corpus_path)
  corpus$human_adjudicated <- TRUE

  expect_error(
    eval$research_eval_validate_corpus(corpus),
    class = "tempest_research_eval_error"
  )
})

test_that("the harness refuses to reload a different live Tempest namespace", {
  context <- research_eval_test_context()
  skip_if(
    is.null(context),
    "developer harness is excluded from source tarballs"
  )
  other_checkout <- tempfile("other-tempest-checkout-")
  dir.create(other_checkout)
  withr::defer(unlink(other_checkout, recursive = TRUE), envir = environment())
  namespace <- asNamespace("tempest")

  expect_error(
    context$eval$research_eval_load_tempest_checkout(other_checkout),
    regexp = "Start a fresh R session",
    class = "tempest_research_eval_error"
  )
  expect_identical(asNamespace("tempest"), namespace)
  expect_identical(
    getFromNamespace("tempest_is_exact_expert", "tempest")(
      getExportedValue("tempest", "tempest_expert")(
        name = "Retained expert",
        title = "Research analyst",
        description = "Evaluates cited evidence.",
        instructions = "Preserve uncertainty."
      )
    ),
    TRUE
  )
})

test_that("GEPA metrics expose feedback and a separate trace-aware token objective", {
  context <- research_eval_test_context()
  skip_if(
    is.null(context),
    "developer harness is excluded from source tarballs"
  )
  eval <- context$eval
  metrics <- eval$research_eval_gepa_metrics()

  expect_setequal(
    names(metrics),
    c(
      "status_accuracy",
      "false_support",
      "full_support_overcall",
      "false_abstention",
      "token_efficiency"
    )
  )
  expect_s3_class(metrics$status_accuracy, "dsprrr_feedback_metric")
  expect_s3_class(metrics$false_support, "dsprrr_feedback_metric")
  expect_s3_class(metrics$full_support_overcall, "dsprrr_feedback_metric")
  expect_s3_class(metrics$false_abstention, "dsprrr_feedback_metric")
  expect_s3_class(metrics$token_efficiency, "dsprrr_trace_metric")

  corpus <- eval$research_eval_read_corpus(context$corpus_path)
  row <- eval$research_eval_split_data(corpus, "test")
  row <- row[row$expected_status == "unverifiable", , drop = FALSE][
    1,
    ,
    drop = FALSE
  ]
  invalid <- metrics$status_accuracy(list(status = "invalid"), row)
  expect_equal(invalid$score, 0)
  expect_match(invalid$feedback, "execution error, not an abstention")
})

test_that("opt-in GEPA uses the current verifier and keeps test topics out of compile", {
  context <- research_eval_test_context()
  skip_if(
    is.null(context),
    "developer harness is excluded from source tarballs"
  )
  eval <- context$eval
  expert <- getExportedValue("tempest", "tempest_expert")(
    name = "Retained expert",
    title = "Research analyst",
    description = "Evaluates cited evidence.",
    instructions = "Preserve uncertainty."
  )
  expert_record <- getFromNamespace(
    "tempest_expert_profile_record",
    "tempest"
  )(expert)
  fake_response <- function(prompt, type = NULL, ...) {
    if (grepl("improving system instructions", prompt, ignore.case = TRUE)) {
      list(instructions = "Apply the evidence status criteria precisely.")
    } else {
      list(
        status = "supported",
        score = 0.8,
        rationale = "Deterministic fake response for harness coverage."
      )
    }
  }
  task_chat <- research_eval_fake_chat(fake_response, "fake-task-model")
  reflection_chat <- research_eval_fake_chat(
    fake_response,
    "fake-reflection-model"
  )
  compile_inputs <- new.env(parent = emptyenv())
  compile_gepa <- eval$research_eval_compile_gepa
  eval$research_eval_compile_gepa <- function(
    program,
    teleprompter,
    trainset,
    valset,
    ...
  ) {
    compile_inputs$trainset <- trainset
    compile_inputs$valset <- valset
    compile_gepa(program, teleprompter, trainset, valset, ...)
  }
  withr::defer(
    eval$research_eval_compile_gepa <- compile_gepa,
    envir = environment()
  )
  old_cache_config <- dsprrr::configure_cache(enable_disk = FALSE)
  withr::defer(
    if (is.null(old_cache_config)) {
      dsprrr::configure_cache()
    } else {
      do.call(dsprrr::configure_cache, old_cache_config)
    },
    envir = environment()
  )
  report_path <- tempfile(fileext = ".json")

  report <- eval$research_eval_optimize(
    chat = task_chat,
    model_id = "fake-task-model",
    reflection_chat = reflection_chat,
    reflection_model_id = "fake-reflection-model",
    corpus_path = context$corpus_path,
    package_dir = test_path("..", ".."),
    report_path = report_path,
    seed = 7L,
    population_size = 2L,
    generations = 1L
  )

  expect_setequal(unique(compile_inputs$trainset$split), "train")
  expect_setequal(unique(compile_inputs$valset$split), "validation")
  expect_identical(any(compile_inputs$trainset$split == "test"), FALSE)
  expect_identical(any(compile_inputs$valset$split == "test"), FALSE)
  expect_match(report$program_ids$baseline, "^sha256:[a-f0-9]{64}$")
  expect_match(report$program_ids$best, "^sha256:[a-f0-9]{64}$")
  expect_identical(report$model_ids$task_model_id, "fake-task-model")
  expect_identical(
    report$model_ids$reflection_model_id,
    "fake-reflection-model"
  )
  expect_identical(report$optimization_result$optimizer, "GEPA")
  expect_named(
    report$optimization_result$best_vs_baseline,
    c(
      "baseline_score",
      "best_score",
      "delta",
      "score_source"
    )
  )
  expect_identical(report$split$test_used_for_optimization, FALSE)
  expect_identical(file.exists(report_path), TRUE)
  expect_identical(
    getFromNamespace("tempest_is_exact_expert", "tempest")(expert),
    TRUE
  )
  expect_identical(
    getFromNamespace("tempest_expert_profile_record", "tempest")(expert),
    expert_record
  )
})
