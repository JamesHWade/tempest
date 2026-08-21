# vitals evaluation helpers

tempest_evaluation_metadata_version <- 1L

tempest_evaluation_abort <- function(message, ..., parent = NULL) {
  tempest_abort(
    message,
    ...,
    class = c("tempest_evaluation_error", "tempest_error"),
    parent = parent,
    .envir = rlang::caller_env()
  )
}

# Load an included evaluation dataset.
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

tempest_evaluation_character_column <- function(value, field) {
  if (
    !is.character(value) ||
      is.object(value) ||
      !is.null(attributes(value)) ||
      anyNA(value) ||
      any(!validUTF8(value)) ||
      any(!nzchar(value)) ||
      !identical(value, trimws(value))
  ) {
    tempest_evaluation_abort(
      paste0(
        "{.field ",
        field,
        "} must contain canonical non-empty UTF-8 strings."
      )
    )
  }
  value
}

tempest_evaluation_id_column <- function(value) {
  if (
    is.object(value) ||
      !is.null(attributes(value)) ||
      !(is.character(value) || is.integer(value) || is.double(value)) ||
      anyNA(value) ||
      (is.numeric(value) && any(!is.finite(value)))
  ) {
    tempest_evaluation_abort(
      paste0(
        "{.field id} must contain canonical non-missing character or ",
        "numeric values."
      )
    )
  }
  if (
    is.character(value) &&
      (any(!validUTF8(value)) ||
        any(!nzchar(value)) ||
        !identical(value, trimws(value)))
  ) {
    tempest_evaluation_abort(
      "{.field id} must contain canonical non-empty UTF-8 strings."
    )
  }
  if (anyDuplicated(value)) {
    tempest_evaluation_abort("{.field id} values must be unique.")
  }
  value
}

tempest_evaluation_dataset_validate <- function(dataset) {
  if (!is.data.frame(dataset)) {
    tempest_evaluation_abort(
      "{.arg dataset} must be {.val qa} or one exact data frame."
    )
  }
  fields <- names(dataset)
  required <- c("input", "target")
  allowed <- c(required, "id")
  if (
    is.null(fields) ||
      anyNA(fields) ||
      any(!nzchar(fields)) ||
      anyDuplicated(fields) ||
      !all(required %in% fields) ||
      any(!fields %in% allowed)
  ) {
    tempest_evaluation_abort(
      paste0(
        "{.arg dataset} must contain only {.field input}, ",
        "{.field target}, and optional {.field id} columns."
      )
    )
  }
  if (nrow(dataset) == 0L) {
    tempest_evaluation_abort("{.arg dataset} must contain at least one row.")
  }
  input <- tempest_evaluation_character_column(dataset$input, "input")
  target <- tempest_evaluation_character_column(dataset$target, "target")
  id <- if ("id" %in% fields) {
    tempest_evaluation_id_column(dataset$id)
  } else {
    seq_len(nrow(dataset))
  }
  tibble::tibble(input = input, target = target, id = id)
}

tempest_evaluation_dataset_digest <- function(dataset) {
  rows <- lapply(
    seq_len(nrow(dataset)),
    \(index) {
      list(
        id = dataset$id[[index]],
        input = dataset$input[[index]],
        target = dataset$target[[index]]
      )
    }
  )
  paste0(
    "sha256:",
    tempest_product_record_hash(list(
      schema_version = 1L,
      id_type = typeof(dataset$id),
      rows = rows
    ))
  )
}

tempest_evaluation_dataset_metadata_validate <- function(value) {
  if (
    !is.list(value) ||
      is.data.frame(value) ||
      is.object(value) ||
      !identical(names(value), c("kind", "digest", "row_count")) ||
      !rlang::is_string(value$kind) ||
      is.na(value$kind) ||
      !value$kind %in% c("builtin", "caller") ||
      !rlang::is_string(value$digest) ||
      is.na(value$digest) ||
      !grepl("^sha256:[a-f0-9]{64}$", value$digest) ||
      !is.integer(value$row_count) ||
      length(value$row_count) != 1L ||
      is.na(value$row_count) ||
      value$row_count < 1L
  ) {
    tempest_evaluation_abort(
      "Evaluation dataset metadata must be one exact safe identity record."
    )
  }
  value
}

tempest_evaluation_dataset_normalize <- function(dataset) {
  builtin <- is.character(dataset) &&
    length(dataset) == 1L &&
    !is.na(dataset) &&
    identical(dataset, "qa")
  if (is.character(dataset) && !builtin) {
    tempest_evaluation_abort(
      "{.arg dataset} must be {.val qa} or one exact data frame."
    )
  }
  data <- if (builtin) {
    tempest_evaluation_dataset_validate(tempest_eval_dataset("qa"))
  } else {
    tempest_evaluation_dataset_validate(dataset)
  }
  digest <- tempest_evaluation_dataset_digest(data)
  metadata <- tempest_evaluation_dataset_metadata_validate(list(
    kind = if (builtin) "builtin" else "caller",
    digest = digest,
    row_count = nrow(data)
  ))
  list(
    data = data,
    kind = if (builtin) "builtin" else "caller",
    label = if (builtin) "qa" else "data",
    digest = digest,
    metadata = metadata
  )
}

tempest_evaluation_task_name <- function(mode, dataset) {
  paste0(
    "tempest-",
    mode,
    "-",
    dataset$kind,
    "-",
    dataset$label,
    "-",
    sub("^sha256:", "", dataset$digest)
  )
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

tempest_evaluation_program_summaries <- function(review) {
  stages <- tempest_program_set_stages()
  programs <- review@programs
  if (!is.list(programs) || !identical(names(programs), stages)) {
    tempest_evaluation_abort(
      "Trajectory review programs do not contain the fixed Tempest stages."
    )
  }
  stats::setNames(
    lapply(
      stages,
      \(stage) {
        program <- programs[[stage]]
        list(
          stage = program$stage,
          contract_version = program$contract_version,
          program_artifact_id = program$program_artifact_id,
          evaluator_id = program$evaluator_id,
          evaluator_version = program$evaluator_version
        )
      }
    ),
    stages
  )
}

tempest_evaluation_value_counts <- function(records, property, values) {
  stats::setNames(
    lapply(
      values,
      \(value) {
        as.integer(sum(vapply(
          records,
          \(record) identical(S7::prop(record, property), value),
          logical(1)
        )))
      }
    ),
    values
  )
}

tempest_evaluation_stage_summaries <- function(stage_records) {
  stage_records <- tempest_stage_records_validate(
    stage_records,
    allow_running = FALSE
  )
  stages <- tempest_program_set_stages()
  finding_codes <- c(
    "stage_failed",
    "stage_cancelled",
    "fallback_taken",
    "exploratory_execution",
    "support_unverified",
    "publication_blocked"
  )
  finding_counts <- tempest_trajectory_stage_finding_counts(stage_records)
  if (!is.list(finding_counts) || !identical(names(finding_counts), stages)) {
    tempest_evaluation_abort(
      "Trajectory finding counts do not contain the fixed Tempest stages."
    )
  }
  stats::setNames(
    lapply(
      stages,
      \(stage) {
        records <- Filter(
          \(record) identical(record@stage, stage),
          stage_records
        )
        stage_findings <- finding_counts[[stage]]
        if (
          !is.list(stage_findings) ||
            is.data.frame(stage_findings) ||
            is.object(stage_findings) ||
            !identical(names(stage_findings), finding_codes) ||
            any(
              !vapply(
                stage_findings,
                \(count) {
                  is.integer(count) &&
                    length(count) == 1L &&
                    !is.na(count) &&
                    count >= 0L
                },
                logical(1)
              )
            )
        ) {
          tempest_evaluation_abort(
            "Trajectory finding counts are invalid for stage {.val {stage}}."
          )
        }
        list(
          stage = stage,
          attempt_count = length(records),
          fallback_count = as.integer(sum(vapply(
            records,
            \(record) isTRUE(record@fallback_taken),
            logical(1)
          ))),
          execution_counts = tempest_evaluation_value_counts(
            records,
            "execution_path",
            tempest_execution_paths()
          ),
          support_counts = tempest_evaluation_value_counts(
            records,
            "support_status",
            tempest_support_statuses()
          ),
          publication_counts = list(
            allowed = as.integer(sum(vapply(
              records,
              \(record) isTRUE(record@publication_allowed),
              logical(1)
            ))),
            blocked = as.integer(sum(vapply(
              records,
              \(record) !isTRUE(record@publication_allowed),
              logical(1)
            )))
          ),
          finding_counts = stats::setNames(
            lapply(finding_codes, \(code) stage_findings[[code]]),
            finding_codes
          )
        )
      }
    ),
    stages
  )
}

tempest_evaluation_product_metadata <- function(
  research,
  manifest,
  report_md,
  stage_records,
  mode,
  dataset
) {
  manifest_summary <- tempest_evaluation_manifest_summary(manifest, mode)
  report_reference <- tempest_evaluation_report_reference(manifest, report_md)
  review <- tempest_trajectory_review(research)
  if (!S7::S7_inherits(review, TempestTrajectoryReview)) {
    tempest_evaluation_abort(
      "Evaluation requires one exact Tempest trajectory review."
    )
  }
  tryCatch(
    S7::validate(review),
    error = function(error) {
      tempest_evaluation_abort(
        "Evaluation received an invalid trajectory review.",
        parent = error
      )
    }
  )
  stage_records <- tempest_stage_records_validate(
    stage_records,
    allow_running = FALSE
  )
  stage_collection <- tempest_trajectory_collection(unname(lapply(
    stage_records,
    tempest_trajectory_stage_item
  )))
  if (
    !identical(stage_collection$total, review@stages$total) ||
      !identical(stage_collection$digest, review@stages$digest)
  ) {
    tempest_evaluation_abort(
      "Evaluation stage records do not match the exact trajectory review."
    )
  }
  metadata <- list(
    metadata_version = tempest_evaluation_metadata_version,
    dataset = dataset,
    review = list(
      schema_version = review@schema_version,
      review_id = review@review_id
    ),
    product = list(
      research_run_id = manifest_summary$research_run_id,
      mode = manifest_summary$mode,
      config_digest = manifest_summary$config_digest,
      report_reference = report_reference,
      knowledge_snapshot_id = manifest@knowledge_snapshot$snapshot_id %||%
        NULL
    ),
    programs = tempest_evaluation_program_summaries(review),
    stages = list(
      digest = stage_collection$digest,
      items = tempest_evaluation_stage_summaries(stage_records)
    )
  )
  tryCatch(
    tempest_product_canonical_value(metadata),
    error = function(error) {
      tempest_evaluation_abort(
        "Evaluation metadata is not a credential-safe plain record.",
        parent = error
      )
    }
  )
  metadata
}

tempest_storm_evaluation_product <- function(
  topic,
  config,
  program_set = NULL,
  knowledge_view = NULL
) {
  tempest_run_internal(
    topic = topic,
    config = config,
    knowledge_view = knowledge_view,
    n_experts = 1L,
    max_rounds = 1L,
    max_questions_per_perspective = 1L,
    program_set = program_set,
    verbose = FALSE
  )
}

tempest_solver_storm <- function(
  input,
  dataset,
  config = tempest_config(),
  program_set = NULL,
  knowledge_view = NULL
) {
  tempest_require("ellmer", "tempest_task() requires ellmer.")
  dataset <- tempest_evaluation_dataset_metadata_validate(dataset)
  n <- length(input)
  results <- character(n)
  chats <- vector("list", n)
  metadata <- vector("list", n)

  for (i in seq_len(n)) {
    product <- tempest_storm_evaluation_product(
      input[[i]],
      config,
      program_set = program_set,
      knowledge_view = knowledge_view
    )
    report_md <- product@report_md %||% NULL
    if (!rlang::is_string(report_md) || is.na(report_md)) {
      tempest_abort("STORM evaluation did not produce one canonical report.")
    }
    stage_records <- product@state$stage_records %||% NULL
    results[[i]] <- report_md
    chats[[i]] <- tempest_make_chat(config, "writer", echo = "none")
    metadata[[i]] <- tempest_evaluation_product_metadata(
      research = product,
      manifest = product@manifest,
      report_md = report_md,
      stage_records = stage_records,
      mode = "storm",
      dataset = dataset
    )
  }

  list(
    result = results,
    solver_chat = chats,
    solver_metadata = metadata
  )
}

tempest_costorm_evaluation_product <- function(
  topic,
  config,
  max_turns,
  program_set = NULL,
  knowledge_view = NULL
) {
  session <- tempest_session_new(
    topic,
    config = config,
    n_experts = 1L,
    program_set = program_set,
    knowledge_view = knowledge_view
  )
  simulated_user <- SimulatedUser$new(
    topic,
    config = config,
    max_turns = max_turns
  )
  simulated_user$run_session(session, warmup = FALSE, verbose = FALSE)
  session$publish(style = "technical", include_references = FALSE)
  list(session = session, turns = simulated_user$turn_count)
}

tempest_solver_costorm <- function(
  input,
  dataset,
  config = tempest_config(),
  max_turns = 5L,
  program_set = NULL,
  knowledge_view = NULL
) {
  tempest_require("ellmer", "tempest_costorm_task() requires ellmer.")
  dataset <- tempest_evaluation_dataset_metadata_validate(dataset)
  max_turns <- tempest_config_count(max_turns, "max_turns")
  n <- length(input)
  results <- character(n)
  chats <- vector("list", n)
  metadata <- vector("list", n)

  for (i in seq_len(n)) {
    product <- tempest_costorm_evaluation_product(
      input[[i]],
      config,
      max_turns,
      program_set = program_set,
      knowledge_view = knowledge_view
    )
    session <- product$session
    if (!inherits(session, "TempestSession")) {
      tempest_abort("Co-STORM evaluation requires a real TempestSession.")
    }
    report_md <- tempest_session_report_read(session)
    results[[i]] <- report_md
    chats[[i]] <- tempest_session_chat(session, "moderator")
    stage_records <- tempest_session_stage_records(session)
    metadata[[i]] <- tempest_evaluation_product_metadata(
      research = session,
      manifest = tempest_session_manifest(session),
      report_md = report_md,
      stage_records = stage_records,
      mode = "costorm",
      dataset = dataset
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
#' The built-in solver runs a real [tempest_run()] product for each input and
#' returns its authoritative report. `solver_metadata` contains only a
#' versioned, credential-safe trajectory summary; live chats, clients, tools,
#' evidence identifiers, source content, and credentials are excluded.
#' Caller datasets are validated and bound to the Task name and metadata by a
#' canonical digest. Custom solvers cannot claim `program_set` or
#' `knowledge_view` inputs on Tempest's behalf.
#'
#' @param dataset The built-in `"qa"` smoke dataset or an exact data frame with
#'   `input`, `target`, and optional unique `id` columns.
#' @param solver Optional vitals-compatible solver. When `NULL`, evaluates the
#'   authoritative [tempest_run()] product.
#' @param scorer A vitals scorer. If `NULL`, defaults to
#'   `vitals::model_graded_qa()`.
#' @param scorer_chat Chat used by the scorer. Required for model-graded
#'   scoring.
#' @param config A `TempestConfig` passed to the solver.
#' @param program_set Optional [TempestProgramSet] evaluated by the built-in
#'   solver.
#' @param knowledge_view Optional immutable Graft view used by governed programs.
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
  dataset = "qa",
  solver = NULL,
  scorer = NULL,
  scorer_chat = NULL,
  config = tempest_config(),
  program_set = NULL,
  knowledge_view = NULL,
  ...
) {
  tempest_require("vitals", "tempest_task() requires vitals.")
  tempest_require("ellmer", "tempest_task() requires ellmer (solver + scorer).")
  dataset <- tempest_evaluation_dataset_normalize(dataset)

  if (is.null(solver)) {
    solver <- function(input, ...) {
      tempest_solver_storm(
        input = input,
        dataset = dataset$metadata,
        config = config,
        program_set = program_set,
        knowledge_view = knowledge_view
      )
    }
  } else {
    if (!is.null(program_set) || !is.null(knowledge_view)) {
      tempest_evaluation_abort(
        paste0(
          "{.arg program_set} and {.arg knowledge_view} require the ",
          "built-in Tempest solver."
        )
      )
    }
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
    dataset = dataset$data,
    solver = solver,
    scorer = scorer,
    name = tempest_evaluation_task_name("storm", dataset),
    ...
  )
}

#' Create a Co-STORM evaluation task using SimulatedUser
#'
#' Runs automated Co-STORM sessions with a simulated user for evaluation.
#' Moderator and expert turns use the same persistent Deputy agents as normal
#' Co-STORM sessions. The built-in solver completes a real [tempest_session()],
#' reads its exact committed report, and includes a versioned, credential-safe
#' trajectory summary in `solver_metadata`; it never returns complete review
#' lanes, Deputy Agent objects, chats, clients, tools, or credentials as
#' metadata.
#' Caller datasets are validated and bound to the Task name and metadata by a
#' canonical digest. Custom solvers cannot claim `program_set` or
#' `knowledge_view` inputs on Tempest's behalf.
#'
#' @param dataset The built-in `"qa"` smoke dataset or an exact data frame with
#'   `input`, `target`, and optional unique `id` columns.
#' @param config A `TempestConfig`.
#' @param max_turns Maximum turns per simulated session.
#' @param solver Optional vitals-compatible solver. When `NULL`, uses the
#'   built-in simulated Co-STORM session solver.
#' @param scorer Optional vitals-compatible scorer. When `NULL`, uses
#'   `vitals::model_graded_qa()`.
#' @param scorer_chat Optional chat for the default model-graded scorer. When
#'   `NULL`, a judge chat is created from `config`.
#' @param program_set Optional [TempestProgramSet] evaluated by the built-in
#'   solver.
#' @param knowledge_view Optional immutable Graft view used by governed programs.
#' @param ... Passed to `vitals::Task$new()`.
#' @return A `vitals::Task`.
#' @examples
#' \dontrun{
#' task <- tempest_costorm_task(config = tempest_config(), max_turns = 5)
#' task$eval()
#' }
#' @export
tempest_costorm_task <- function(
  dataset = "qa",
  config = tempest_config(),
  max_turns = 5L,
  solver = NULL,
  scorer = NULL,
  scorer_chat = NULL,
  program_set = NULL,
  knowledge_view = NULL,
  ...
) {
  tempest_require("vitals", "tempest_costorm_task() requires vitals.")
  tempest_require("ellmer", "tempest_costorm_task() requires ellmer.")
  dataset <- tempest_evaluation_dataset_normalize(dataset)
  max_turns <- tempest_config_count(max_turns, "max_turns")

  if (is.null(solver)) {
    solver <- function(input, ...) {
      tempest_solver_costorm(
        input = input,
        dataset = dataset$metadata,
        config = config,
        max_turns = max_turns,
        program_set = program_set,
        knowledge_view = knowledge_view
      )
    }
  } else {
    if (!is.null(program_set) || !is.null(knowledge_view)) {
      tempest_evaluation_abort(
        paste0(
          "{.arg program_set} and {.arg knowledge_view} require the ",
          "built-in Tempest solver."
        )
      )
    }
    injected_solver <- solver
    solver <- function(input, ...) {
      injected_solver(
        input = input,
        config = config,
        max_turns = max_turns,
        ...
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
    dataset = dataset$data,
    solver = solver,
    scorer = scorer,
    name = tempest_evaluation_task_name("costorm", dataset),
    ...
  )
}
