# vitals evaluation helpers

#' Load an included evaluation dataset
#'
#' @param name Dataset name. Currently supports "qa".
#' @return A tibble with columns `input` and `target`.
#' @keywords internal
tempest_eval_dataset <- function(name = c("qa")) {
  name <- match.arg(name)
  path <- system.file(
    "extdata",
    "evals",
    paste0(name, ".csv"),
    package = "tempest"
  )
  if (identical(path, "")) {
    tempest_abort("Eval dataset not found in installed package.")
  }
  read.csv(path, stringsAsFactors = FALSE) |>
    tibble::as_tibble()
}

tempest_evaluation_manifest_summary <- function(manifest, mode) {
  if (
    !S7::S7_inherits(manifest, TempestResearchManifest) ||
      !identical(manifest@mode, mode) ||
      !identical(manifest@status, "succeeded")
  ) {
    tempest_abort(
      paste0(
        "Evaluation requires a succeeded authoritative ",
        toupper(mode),
        " product."
      )
    )
  }
  tryCatch(
    S7::validate(manifest),
    error = function(error) {
      tempest_abort(
        "Evaluation received an invalid product manifest.",
        parent = error
      )
    }
  )
  list(
    schema_version = manifest@schema_version,
    research_run_id = manifest@research_run_id,
    mode = manifest@mode,
    config_digest = manifest@config_digest,
    status = manifest@status
  )
}

tempest_evaluation_report_reference <- function(manifest, report_md) {
  manifest_reference <- manifest@deliverables$report_md %||% NULL
  reference <- tempest_product_report_reference(report_md)
  expected <- c(reference, list(status = "durable"))
  if (!identical(manifest_reference, expected)) {
    tempest_product_report_abort(
      "Evaluation requires the exact canonical durable report binding."
    )
  }
  reference
}

tempest_evaluation_workspace_summary <- function(workspace) {
  if (!inherits(workspace, "ResearchWorkspace")) {
    tempest_abort("Evaluation requires an authoritative ResearchWorkspace.")
  }
  sources <- workspace$list_retrieved_sources()
  claims <- workspace$list_proposed_claims()
  evidence_spans <- workspace$list_evidence_spans()
  claim_supports <- workspace$list_claim_supports()
  list(
    source_ids = sort(
      vapply(sources, `[[`, character(1), "id"),
      method = "radix"
    ),
    claim_ids = sort(
      vapply(claims, \(claim) claim@claim_id, character(1)),
      method = "radix"
    ),
    evidence_span_ids = sort(
      vapply(
        evidence_spans,
        \(span) span@evidence_span_id,
        character(1)
      ),
      method = "radix"
    ),
    claim_support_ids = sort(
      vapply(
        claim_supports,
        \(support) support@claim_support_id,
        character(1)
      ),
      method = "radix"
    )
  )
}

tempest_evaluation_stage_summaries <- function(stage_records) {
  stage_records <- tempest_stage_records_validate(
    stage_records,
    allow_running = FALSE
  )
  lapply(stage_records, function(record) {
    list(
      stage = record@stage,
      attempt_id = record@attempt_id,
      status = record@status,
      program_artifact_id = record@program_artifact_id,
      execution_path = record@execution_path,
      support_status = record@support_status,
      fallback_taken = record@fallback_taken,
      publication_allowed = record@publication_allowed
    )
  })
}

tempest_evaluation_product_metadata <- function(
  manifest,
  report_md,
  workspace,
  stage_records,
  mode
) {
  manifest_summary <- tempest_evaluation_manifest_summary(manifest, mode)
  manifest_summary$report_reference <-
    tempest_evaluation_report_reference(manifest, report_md)
  list(
    manifest = manifest_summary,
    workspace = tempest_evaluation_workspace_summary(workspace),
    stage_records = tempest_evaluation_stage_summaries(stage_records)
  )
}

tempest_storm_evaluation_product <- function(topic, config) {
  tempest_run(
    topic = topic,
    config = config,
    n_experts = 1L,
    max_rounds = 1L,
    max_questions_per_perspective = 1L,
    verbose = FALSE
  )
}

tempest_solver_storm <- function(input, config = tempest_config()) {
  tempest_require("ellmer", "tempest_task() requires ellmer.")
  n <- length(input)
  results <- character(n)
  chats <- vector("list", n)
  metadata <- vector("list", n)

  for (i in seq_len(n)) {
    product <- tempest_storm_evaluation_product(input[[i]], config)
    report_md <- product$report_md %||% NULL
    if (!rlang::is_string(report_md) || is.na(report_md)) {
      tempest_abort("STORM evaluation did not produce one canonical report.")
    }
    stage_records <- product$state$stage_records %||% NULL
    results[[i]] <- report_md
    chats[[i]] <- tempest_make_chat(config, "writer", echo = "none")
    product_metadata <- tempest_evaluation_product_metadata(
      manifest = product$manifest,
      report_md = report_md,
      workspace = product$workspace,
      stage_records = stage_records,
      mode = "storm"
    )
    tempest_product_authority_validate(
      manifest = product$manifest,
      stage_records = stage_records,
      workspace = product$workspace,
      report_md = report_md,
      report_reference = product_metadata$manifest$report_reference,
      config = config,
      experts = product$experts,
      product_state = product$state,
      require_publishable = TRUE
    )
    metadata[[i]] <- product_metadata
  }

  list(
    result = results,
    solver_chat = chats,
    solver_metadata = metadata
  )
}

tempest_costorm_evaluation_product <- function(topic, config, max_turns) {
  session <- tempest_session(topic, config = config, n_experts = 1L)
  simulated_user <- SimulatedUser$new(
    topic,
    config = config,
    max_turns = max_turns
  )
  simulated_user$run_session(session, warmup = FALSE, verbose = FALSE)
  session$report(style = "technical", include_references = FALSE)
  list(session = session, turns = simulated_user$turn_count)
}

tempest_solver_costorm <- function(
  input,
  config = tempest_config(),
  max_turns = 5L
) {
  tempest_require("ellmer", "tempest_costorm_task() requires ellmer.")
  max_turns <- tempest_config_count(max_turns, "max_turns")
  n <- length(input)
  results <- character(n)
  chats <- vector("list", n)
  metadata <- vector("list", n)

  for (i in seq_len(n)) {
    product <- tempest_costorm_evaluation_product(
      input[[i]],
      config,
      max_turns
    )
    session <- product$session
    if (!inherits(session, "TempestSession")) {
      tempest_abort("Co-STORM evaluation requires a real TempestSession.")
    }
    report_md <- tempest_session_report_md(session)
    results[[i]] <- report_md
    chats[[i]] <- tempest_session_chat(session, "moderator")
    stage_records <- tempest_session_stage_records(session)
    product_metadata <- tempest_evaluation_product_metadata(
      manifest = session$manifest,
      report_md = report_md,
      workspace = session$workspace,
      stage_records = stage_records,
      mode = "costorm"
    )
    tempest_product_authority_validate(
      manifest = session$manifest,
      stage_records = stage_records,
      workspace = session$workspace,
      report_md = report_md,
      report_reference = product_metadata$manifest$report_reference,
      config = config,
      experts = session$experts,
      expert_sessions = tempest_expert_sessions_snapshot(session),
      product_state = list(title = session$title),
      require_publishable = TRUE
    )
    metadata[[i]] <- c(
      list(
        turns = product$turns,
        deputy_traces = tempest_session_deputy_traces(session)
      ),
      product_metadata
    )
  }

  list(
    result = results,
    solver_chat = chats,
    solver_metadata = metadata
  )
}

#' Create a vitals Task for tempest
#'
#' @param dataset Which built-in dataset to use. Currently "qa".
#' @param solver Optional vitals-compatible solver. When `NULL`, evaluates the
#'   authoritative [tempest_run()] product.
#' @param scorer A vitals scorer. If `NULL`, defaults to `vitals::model_graded_qa()`.
#' @param scorer_chat Chat used by the scorer (required for model-graded scoring).
#' @param config A `TempestConfig` passed to the solver.
#' @param ... Passed to `vitals::Task$new()`.
#' @return A `vitals::Task`.
#' @examples
#' \dontrun{
#' scorer_chat <- ellmer::chat("openai/gpt-5.6-luna")
#' task <- tempest_task(scorer_chat = scorer_chat, config = tempest_config())
#' task$eval()
#' }
#' @export
tempest_task <- function(
  dataset = c("qa"),
  solver = NULL,
  scorer = NULL,
  scorer_chat = NULL,
  config = tempest_config(),
  ...
) {
  tempest_require("vitals", "tempest_task() requires vitals.")
  tempest_require("ellmer", "tempest_task() requires ellmer (solver + scorer).")
  dataset <- match.arg(dataset)
  ds <- tempest_eval_dataset(dataset)

  if (is.null(solver)) {
    solver <- function(input, ...) {
      tempest_solver_storm(input = input, config = config)
    }
  } else {
    injected_solver <- solver
    solver <- function(input, ...) {
      injected_solver(input = input, config = config, ...)
    }
  }

  if (is.null(scorer)) {
    if (is.null(scorer_chat)) {
      tempest_abort(
        "Provide scorer_chat (an ellmer Chat) or set scorer explicitly."
      )
    }
    scorer <- vitals::model_graded_qa(
      partial_credit = TRUE,
      scorer_chat = scorer_chat
    )
  }

  vitals::Task$new(
    dataset = ds,
    solver = solver,
    scorer = scorer,
    name = paste0("tempest-", dataset),
    ...
  )
}

#' Create a Co-STORM evaluation task using SimulatedUser
#'
#' Runs automated Co-STORM sessions with a simulated user for evaluation.
#' Moderator and expert turns use the same persistent Deputy agents as normal
#' Co-STORM sessions. The built-in solver includes credential-safe terminal
#' Deputy traces in `solver_metadata`; it never returns Deputy Agent objects.
#'
#' @param dataset Which built-in dataset to use. Currently "qa".
#' @param config A `TempestConfig`.
#' @param max_turns Maximum turns per simulated session.
#' @param solver Optional vitals-compatible solver. When `NULL`, uses the
#'   built-in simulated Co-STORM session solver.
#' @param scorer Optional vitals-compatible scorer. When `NULL`, uses
#'   `vitals::model_graded_qa()`.
#' @param scorer_chat Optional chat for the default model-graded scorer. When
#'   `NULL`, a judge chat is created from `config`.
#' @param ... Passed to `vitals::Task$new()`.
#' @return A `vitals::Task`.
#' @examples
#' \dontrun{
#' task <- tempest_costorm_task(config = tempest_config(), max_turns = 5)
#' task$eval()
#' }
#' @export
tempest_costorm_task <- function(
  dataset = c("qa"),
  config = tempest_config(),
  max_turns = 5L,
  solver = NULL,
  scorer = NULL,
  scorer_chat = NULL,
  ...
) {
  tempest_require("vitals", "tempest_costorm_task() requires vitals.")
  tempest_require("ellmer", "tempest_costorm_task() requires ellmer.")
  dataset <- match.arg(dataset)
  ds <- tempest_eval_dataset(dataset)
  max_turns <- tempest_config_count(max_turns, "max_turns")

  if (is.null(solver)) {
    solver <- function(input, ...) {
      tempest_solver_costorm(
        input = input,
        config = config,
        max_turns = max_turns
      )
    }
  }

  if (is.null(scorer)) {
    scorer_chat <- scorer_chat %||%
      tempest_make_chat(config, "judge", echo = "none")
    scorer <- vitals::model_graded_qa(
      partial_credit = TRUE,
      scorer_chat = scorer_chat
    )
  }

  vitals::Task$new(
    dataset = ds,
    solver = solver,
    scorer = scorer,
    name = paste0("tempest-costorm-", dataset),
    ...
  )
}
