# STORM pipeline (scripted)

tempest_storm_report_with_execution_review <- function(
  report_md,
  stage_records,
  title = NULL
) {
  if (!rlang::is_string(report_md) || !nzchar(tempest_trim(report_md))) {
    tempest_abort(
      "STORM execution review requires a non-empty Markdown report.",
      class = "tempest_stage_commit_error"
    )
  }
  review <- tempest_stage_records_execution_review(stage_records)
  tempest_markdown_append_execution_review(
    report_md,
    review,
    trusted_title = title
  )
}

tempest_run_verification <- function(
  store,
  config,
  verifier = NULL,
  program,
  record_stage = function(record, output = NULL) invisible(record),
  record_stages = function(records, outputs = NULL) invisible(records)
) {
  verifier <- verifier %||% tempest_make_chat(config, "judge")
  tempest_verify_claims_internal(
    workspace = store,
    verifier = verifier,
    policy = "claim_verified",
    verifier_model = config@models[["judge"]] %||% NA_character_,
    program = program,
    min_support_score = config@min_support_score,
    record_stage = record_stage,
    record_stages = record_stages
  )
}

tempest_storm_retriever_workspace <- function(retriever) {
  workspace <- retriever$workspace %||% NULL
  if (!inherits(workspace, "ResearchWorkspace")) {
    tempest_config_abort(
      "{.arg retriever} must expose a ResearchWorkspace at {.code retriever$workspace}."
    )
  }
  workspace
}

#' Run the STORM pipeline
#'
#' This is a scripted workflow that:
#' 1) discovers perspectives and research questions,
#' 2) runs a multi-perspective research loop (search/fetch + expert synthesis),
#' 3) creates an outline,
#' 4) writes a cited report in Markdown.
#'
#' @param topic Research topic or question.
#' @param config A `TempestConfig`.
#' @param retriever Optional `TempestRetriever` or compatible retriever with a
#'   `ResearchWorkspace` at `$workspace` and `search(query, k)` and
#'   `fetch(url)` methods. `search()` returns a data frame with non-empty
#'   `title` and HTTP/HTTPS `url` columns; `snippet` is optional. Tempest
#'   derives or verifies `source_id`. Create the workspace with
#'   [tempest_research_workspace()] and source IDs with [tempest_source_id()].
#'   If `NULL`, a retriever is created from `config`.
#' @param knowledge Optional accepted organizational knowledge from
#'   [tempest_artifact_knowledge()]. It supplies
#'   accepted evidence records without selecting or authorizing executable
#'   programs.
#'   The live Graft view and knowledge object are not serialized;
#'   materialized evidence and artifact selections are retained in the bundle.
#' @param n_experts Number of expert profiles to generate when `experts` is
#'   `NULL` (default 3).
#' @param experts Optional list of active profiles created by
#'   [tempest_expert()]. When supplied, STORM uses this selected team and does
#'   not generate experts.
#' @param research_strategy Either "key_questions" (default, faster) or
#'   "conversation" (more thorough but slower). Key questions uses predefined
#'   questions; conversation dynamically generates follow-up questions.
#' @param max_rounds Maximum rounds per perspective for the "conversation"
#'   strategy (default 3).
#' @param max_questions_per_perspective Maximum questions per perspective for
#'   the "key_questions" strategy (default 3).
#' @param parallel_writing If `TRUE`, write report sections in parallel using
#'   the mirai package. Failed parallel sections are retried sequentially.
#' @param steps Character vector controlling which steps to run. Defaults to
#'   all.
#' @param output_dir Optional directory for persisted STORM run artifacts. When
#'   supplied, a current schema-11 product bundle with schema-5 STORM state is
#'   written under a topic-specific subdirectory.
#' @param resume If `TRUE` and `output_dir` contains a previous run, load saved
#'   current-format artifacts and skip stages recorded as complete. Older,
#'   future, missing, extra, or mismatched product shapes are rejected rather
#'   than migrated.
#' @param run_id Optional run directory name. Defaults to a slug of `topic`.
#' @param progress Optional function called with one plain named progress
#'   record as STORM workflow stages start, finish, fail, persist artifacts, or
#'   make final artifacts available. See the package vignette for the eleven
#'   stable fields.
#' @param verbose If `TRUE`, prints progress messages.
#' @return A validated completed Tempest research product. Read it with
#'   [tempest_report()], [tempest_sources()], [tempest_claims()],
#'   [tempest_claim_supports()], and [tempest_trajectory_review()].
#' @examples
#' \dontrun{
#' cfg <- tempest_config()
#' result <- tempest_run("History of jazz", config = cfg)
#' cat(tempest_report(result))
#' }
#' @export
tempest_run <- function(
  topic,
  config = tempest_config(),
  retriever = NULL,
  knowledge = NULL,
  n_experts = 3,
  experts = NULL,
  research_strategy = c("key_questions", "conversation"),
  max_rounds = 3,
  max_questions_per_perspective = 3,
  parallel_writing = FALSE,
  steps = c("perspectives", "research", "outline", "write", "polish"),
  output_dir = NULL,
  resume = FALSE,
  run_id = NULL,
  progress = NULL,
  verbose = TRUE
) {
  supplied_knowledge <- knowledge
  knowledge <- tempest_knowledge_argument(knowledge, admit = FALSE)
  tempest_otel_trace(
    "storm.run",
    tempest_run_internal(
      topic = topic,
      config = config,
      retriever = retriever,

      knowledge_records = knowledge$records,
      knowledge_selection = knowledge$artifact_selection,
      program_set = knowledge$program_set,
      n_experts = n_experts,
      experts = experts,
      research_strategy = research_strategy,
      max_rounds = max_rounds,
      max_questions_per_perspective = max_questions_per_perspective,
      parallel_writing = parallel_writing,
      steps = steps,
      output_dir = output_dir,
      resume = resume,
      run_id = run_id,
      progress = progress,
      verbose = verbose,
      .admit_knowledge = function() {
        tempest_knowledge_argument(supplied_knowledge)
      }
    )
  )
}

tempest_run_internal <- function(
  topic,
  config = tempest_config(),
  retriever = NULL,
  knowledge_records = list(),
  knowledge_selection = list(),
  n_experts = 3,
  experts = NULL,
  research_strategy = c("key_questions", "conversation"),
  max_rounds = 3,
  max_questions_per_perspective = 3,
  parallel_writing = FALSE,
  program_set = NULL,
  steps = c("perspectives", "research", "outline", "write", "polish"),
  output_dir = NULL,
  resume = FALSE,
  run_id = NULL,
  progress = NULL,
  verbose = TRUE,
  .requested_steps = NULL,
  .admit_knowledge = NULL
) {
  ctx <- tempest_storm_run_context(
    topic = topic,
    config = config,
    retriever = retriever,
    knowledge_records = knowledge_records,
    knowledge_selection = knowledge_selection,
    n_experts = n_experts,
    experts = experts,
    research_strategy = research_strategy,
    max_rounds = max_rounds,
    max_questions_per_perspective = max_questions_per_perspective,
    parallel_writing = parallel_writing,
    program_set = program_set,
    steps = steps,
    output_dir = output_dir,
    resume = resume,
    run_id = run_id,
    progress = progress,
    verbose = verbose,
    .requested_steps = .requested_steps,
    .admit_knowledge = .admit_knowledge
  )

  if (identical(ctx$research_manifest@status, "succeeded")) {
    tempest_storm_emit_progress(
      ctx,
      "workflow",
      "started",
      message = "Resuming completed STORM workflow.",
      payload = list(steps = ctx$steps, resume = TRUE)
    )
    tempest_storm_emit_progress(
      ctx,
      "workflow",
      "succeeded",
      message = "Loaded completed STORM workflow.",
      payload = list(completed_stages = ctx$completed_stages)
    )
    return(tempest_product_result(
      title = ctx$state$title,
      topic = ctx$topic,
      perspectives = ctx$state$perspectives,
      experts = ctx$state$experts,
      outline = ctx$state$outline,
      draft_md = ctx$state$draft_md,
      report_md = ctx$state$report_md,
      manifest = ctx$research_manifest,
      state = ctx$state,
      workspace = ctx$workspace,
      retriever = ctx$retriever,
      output_dir = ctx$run_dir,
      config = ctx$config
    ))
  }

  tempest_storm_emit_progress(
    ctx,
    "workflow",
    "started",
    message = "Starting STORM workflow.",
    payload = list(steps = ctx$steps, resume = isTRUE(ctx$resume))
  )

  withCallingHandlers(
    {
      ctx$coordinator <- tempest_make_chat(
        ctx$config,
        "coordinator",
        echo = if (ctx$verbose) "output" else "none"
      )
      ctx$writer <- tempest_make_chat(
        ctx$config,
        "writer",
        echo = if (ctx$verbose) "output" else "none"
      )
      ctx$extractor <- tempest_make_chat(
        ctx$config,
        "judge",
        system_prompt = tempest_prompt("fact_extractor_system"),
        echo = "none"
      )

      tempest_storm_stage_perspectives(ctx)
      tempest_storm_stage_research(ctx)
      tempest_storm_stage_outline(ctx)
      tempest_storm_stage_write(ctx)
      tempest_storm_stage_polish(ctx)
      tempest_storm_finalize_result(ctx)
    },
    error = function(e) {
      if (identical(ctx$research_manifest@status, "running")) {
        tempest_storm_persist_terminal_state(ctx)
      }
      stage <- ctx$current_progress_stage
      if (!is.na(stage)) {
        tempest_storm_emit_progress_best_effort(
          ctx,
          "stage",
          "failed",
          stage = stage,
          payload = tempest_progress_error_payload(e)
        )
        ctx$current_progress_stage <- NA_character_
      }
      tempest_storm_emit_progress_best_effort(
        ctx,
        "workflow",
        "failed",
        message = "STORM workflow failed.",
        payload = c(list(stage = stage), tempest_progress_error_payload(e))
      )
      tempest_rethrow_operation(e, class = "tempest_run_error")
    },
    interrupt = function(e) {
      if (identical(ctx$research_manifest@status, "running")) {
        tempest_storm_persist_terminal_state(ctx)
      }
      stage <- ctx$current_progress_stage
      tempest_storm_emit_progress_best_effort(
        ctx,
        "cancellation",
        "cancelled",
        stage = stage,
        message = "STORM workflow cancelled.",
        payload = list(stage = stage)
      )
      tempest_abort(
        "STORM workflow was cancelled.",
        class = c(
          "tempest_run_cancelled",
          "tempest_run_error",
          "tempest_error",
          "interrupt"
        )
      )
    }
  )
}

tempest_run_async <- function(...) {
  args <- list(...)
  knowledge <- args$knowledge %||% NULL
  if (!is.null(knowledge)) {
    tempest_governed_procedure_abort(
      paste0(
        "{.fn tempest_run_async} never serializes a live ",
        "{.arg knowledge}. Run the knowledge-backed workflow with ",
        "{.fn tempest_run} in the process that owns the admission callback."
      )
    )
  }
  tempest_require("promises", "tempest_run_async() uses promises.")
  tempest_require("mirai", "tempest_run_async() uses a Mirai worker.")
  runner <- getOption("tempest.async_runner", tempest_run)
  if (!is.function(runner)) {
    tempest_abort(
      "The configured async runner must be a function.",
      class = c("tempest_async_error", "tempest_error")
    )
  }
  otel_enabled <- tempest_otel_worker_intent()
  worker_call <- tempest_otel_worker_call
  job <- mirai::mirai(
    {
      tryCatch(
        list(
          ok = TRUE,
          value = worker_call(runner, args, otel_enabled)
        ),
        error = function(error) list(ok = FALSE, condition = error)
      )
    },
    runner = runner,
    args = args,
    worker_call = worker_call,
    otel_enabled = otel_enabled
  )
  promise <- promises::then(
    promises::as.promise(job),
    function(result) {
      if (inherits(result, "errorValue")) {
        tempest_abort(
          "Asynchronous STORM run was cancelled.",
          class = c(
            "tempest_async_cancelled",
            "tempest_async_error",
            "tempest_error"
          )
        )
      }
      if (!is.list(result) || !isTRUE(result$ok)) {
        condition <- result$condition %||%
          simpleError(
            "Asynchronous STORM worker returned an invalid result."
          )
        tempest_rethrow_operation(condition, class = "tempest_async_error")
      }
      result$value
    },
    onRejected = function(error) {
      if (grepl("Operation canceled", conditionMessage(error), fixed = TRUE)) {
        tempest_abort(
          "Asynchronous STORM run was cancelled.",
          class = c(
            "tempest_async_cancelled",
            "tempest_async_error",
            "tempest_error"
          )
        )
      }
      tempest_rethrow_operation(error, class = "tempest_async_error")
    }
  )
  attr(promise, "tempest_mirai") <- job
  class(promise) <- c("tempest_async_run", class(promise))
  promise
}

tempest_run_cancel <- function(run) {
  if (!inherits(run, "tempest_async_run")) {
    tempest_abort(
      "{.arg run} must be returned by {.fn tempest_run_async}.",
      class = c("tempest_async_error", "tempest_error")
    )
  }
  job <- attr(run, "tempest_mirai", exact = TRUE)
  if (is.null(job) || !mirai::unresolved(job)) {
    return(invisible(FALSE))
  }
  mirai::stop_mirai(job)
  invisible(TRUE)
}
