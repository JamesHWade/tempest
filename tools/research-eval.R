# Developer-only synthetic research evaluation harness.
# Source this file to define the helpers; it performs no work on source().

research_eval_statuses <- function() {
  c(
    "supported",
    "partially_supported",
    "unsupported",
    "contradicted",
    "unverifiable"
  )
}

research_eval_required_challenges <- function() {
  c(
    "valid_support",
    "partial_support",
    "omitted_caveat",
    "scope_inflation",
    "direction_reversal",
    "contradictory_studies",
    "misleading_duplicate_sources",
    "missing_evidence"
  )
}

research_eval_abort <- function(message) {
  stop(structure(
    list(message = message, call = NULL),
    class = c("tempest_research_eval_error", "error", "condition")
  ))
}

research_eval_or_null <- function(value, default) {
  if (is.null(value)) default else value
}

research_eval_find_root <- function(path = getwd()) {
  current <- normalizePath(path, mustWork = TRUE)
  repeat {
    if (
      file.exists(file.path(current, "DESCRIPTION")) &&
        file.exists(file.path(current, "tools", "research-eval.R"))
    ) {
      return(current)
    }
    parent <- dirname(current)
    if (identical(parent, current)) {
      research_eval_abort(
        "Run the research evaluation harness from within the Tempest checkout."
      )
    }
    current <- parent
  }
}

research_eval_default_corpus_path <- function(
  root = research_eval_find_root()
) {
  file.path(root, "inst", "extdata", "research-eval", "corpus.json")
}

research_eval_validate_corpus <- function(corpus) {
  if (!is.list(corpus) || !is.list(corpus$cases)) {
    research_eval_abort(
      "The research evaluation corpus must be a JSON object with cases."
    )
  }
  required_metadata <- c(
    "corpus_id",
    "schema_version",
    "synthetic",
    "human_adjudicated",
    "split_seed",
    "split_unit",
    "split_method",
    "splits"
  )
  missing_metadata <- setdiff(required_metadata, names(corpus))
  if (length(missing_metadata) > 0L) {
    research_eval_abort(paste(
      "The corpus is missing metadata fields:",
      paste(missing_metadata, collapse = ", ")
    ))
  }
  if (
    !identical(corpus$synthetic, TRUE) ||
      !identical(corpus$human_adjudicated, FALSE) ||
      !identical(corpus$split_unit, "topic_id")
  ) {
    research_eval_abort(
      "The corpus must be synthetic, not human-adjudicated, and split by topic_id."
    )
  }
  if (
    !identical(as.character(corpus$splits), c("train", "validation", "test"))
  ) {
    research_eval_abort(
      "Corpus splits must be train, validation, and test in that order."
    )
  }
  if (
    length(corpus$split_seed) != 1L ||
      is.na(corpus$split_seed) ||
      !is.numeric(corpus$split_seed) ||
      !is.finite(corpus$split_seed)
  ) {
    research_eval_abort("The corpus split_seed must be one finite number.")
  }

  cases <- corpus$cases
  required_case_fields <- c(
    "case_id",
    "citation_id",
    "case_group",
    "topic_id",
    "split",
    "challenge_type",
    "claim_text",
    "source_id",
    "source_title",
    "source_origin_id",
    "source_excerpt",
    "expected_status",
    "synthetic",
    "judgement_basis",
    "gold_rationale"
  )
  for (index in seq_along(cases)) {
    missing_fields <- setdiff(required_case_fields, names(cases[[index]]))
    if (length(missing_fields) > 0L) {
      research_eval_abort(paste0(
        "Corpus case ",
        index,
        " is missing fields: ",
        paste(missing_fields, collapse = ", ")
      ))
    }
  }
  case_fields <- lapply(cases, function(case) {
    as.character(unlist(case[required_case_fields], use.names = FALSE))
  })
  fields <- as.data.frame(
    do.call(rbind, case_fields),
    stringsAsFactors = FALSE
  )
  names(fields) <- required_case_fields

  if (anyDuplicated(fields$case_id) || anyDuplicated(fields$citation_id)) {
    research_eval_abort("Corpus case_id and citation_id values must be unique.")
  }
  if (
    any(!fields$split %in% corpus$splits) ||
      any(!fields$expected_status %in% research_eval_statuses())
  ) {
    research_eval_abort(
      "Corpus cases contain an unknown split or expected_status."
    )
  }
  if (
    any(fields$synthetic != "TRUE") ||
      any(fields$judgement_basis != "synthetic_design_not_human_adjudication")
  ) {
    research_eval_abort(
      "Every corpus case must be labeled synthetic and not human-adjudicated."
    )
  }
  if (any(!nzchar(fields$topic_id)) || any(!nzchar(fields$claim_text))) {
    research_eval_abort("Every corpus case needs a topic_id and claim_text.")
  }
  if (any(!nzchar(fields$source_id)) || any(!nzchar(fields$source_title))) {
    research_eval_abort(
      "Every corpus case needs a synthetic source_id and title."
    )
  }

  topics_by_split <- lapply(corpus$splits, function(split) {
    unique(fields$topic_id[fields$split == split])
  })
  names(topics_by_split) <- corpus$splits
  for (left in seq_len(length(topics_by_split) - 1L)) {
    for (right in seq.int(left + 1L, length(topics_by_split))) {
      if (
        length(intersect(topics_by_split[[left]], topics_by_split[[right]])) >
          0L
      ) {
        research_eval_abort("Corpus topics overlap between splits.")
      }
    }
  }
  for (split in corpus$splits) {
    challenges <- unique(fields$challenge_type[fields$split == split])
    if (!setequal(challenges, research_eval_required_challenges())) {
      research_eval_abort(paste0(
        "Split ",
        split,
        " must include every required challenge type."
      ))
    }
  }
  invisible(fields)
}

research_eval_read_corpus <- function(
  path = research_eval_default_corpus_path()
) {
  if (!file.exists(path)) {
    research_eval_abort(paste0("Research evaluation corpus not found: ", path))
  }
  corpus <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  research_eval_validate_corpus(corpus)
  attr(corpus, "path") <- normalizePath(path, mustWork = TRUE)
  attr(corpus, "sha256") <- digest::digest(
    file = path,
    algo = "sha256",
    serialize = FALSE
  )
  corpus
}

research_eval_case_table <- function(corpus) {
  research_eval_validate_corpus(corpus)
  rows <- lapply(corpus$cases, function(case) {
    excerpt <- case$source_excerpt
    source_excerpts <- if (is.null(excerpt) || !nzchar(excerpt)) {
      ""
    } else {
      paste0(
        "Source ID: ",
        case$source_id,
        "\nSource title: ",
        case$source_title,
        "\nSource origin: ",
        case$source_origin_id,
        "\nEvidence excerpt: ",
        excerpt
      )
    }
    data.frame(
      case_id = case$case_id,
      citation_id = case$citation_id,
      case_group = case$case_group,
      topic_id = case$topic_id,
      split = case$split,
      challenge_type = case$challenge_type,
      claim_text = case$claim_text,
      source_origin_id = case$source_origin_id,
      source_excerpts = source_excerpts,
      expected_status = case$expected_status,
      gold_rationale = case$gold_rationale,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

research_eval_split_data <- function(corpus, split) {
  if (
    !is.character(split) || length(split) != 1L || !split %in% corpus$splits
  ) {
    research_eval_abort("split must be one of train, validation, or test.")
  }
  rows <- research_eval_case_table(corpus)
  rows[rows$split == split, , drop = FALSE]
}

research_eval_number <- function(value) {
  if (length(value) != 1L || is.null(value) || is.na(value)) {
    return(NA_real_)
  }
  value <- suppressWarnings(as.numeric(value))
  if (length(value) != 1L || !is.finite(value)) {
    return(NA_real_)
  }
  value
}

research_eval_normalize_predictions <- function(predictions, rows) {
  if (is.character(predictions) && length(predictions) == 1L) {
    if (!file.exists(predictions)) {
      research_eval_abort(paste0("Prediction file not found: ", predictions))
    }
    predictions <- utils::read.csv(
      predictions,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }
  if (!is.data.frame(predictions)) {
    research_eval_abort("predictions must be a data frame or CSV path.")
  }
  required <- c("case_id", "predicted_status")
  missing <- setdiff(required, names(predictions))
  if (length(missing) > 0L) {
    research_eval_abort(paste(
      "Predictions are missing columns:",
      paste(missing, collapse = ", ")
    ))
  }
  if (anyNA(predictions$case_id) || anyDuplicated(predictions$case_id)) {
    research_eval_abort("Every case_id must appear once in predictions.")
  }
  if (
    nrow(predictions) != nrow(rows) ||
      !setequal(as.character(predictions$case_id), rows$case_id)
  ) {
    research_eval_abort(
      "Predictions must contain exactly the cases for the selected split."
    )
  }
  predictions <- predictions[
    match(rows$case_id, predictions$case_id),
    ,
    drop = FALSE
  ]
  status <- trimws(as.character(predictions$predicted_status))
  status[tolower(status) == "abstain"] <- "unverifiable"
  if (anyNA(status) || any(!status %in% research_eval_statuses())) {
    research_eval_abort(
      "predicted_status values must use a Tempest verification status or abstain."
    )
  }
  predictions$case_id <- as.character(predictions$case_id)
  predictions$predicted_status <- status

  optional_numeric <- c(
    "input_tokens",
    "output_tokens",
    "total_tokens",
    "provider_reported_cost",
    "duration_s",
    "latency_ms",
    "trace_event_count"
  )
  for (field in optional_numeric) {
    if (!field %in% names(predictions)) {
      predictions[[field]] <- NA_real_
    } else {
      predictions[[field]] <- vapply(
        predictions[[field]],
        research_eval_number,
        numeric(1)
      )
    }
    invalid <- !is.na(predictions[[field]]) & predictions[[field]] < 0
    if (any(invalid)) {
      research_eval_abort(paste0(field, " values must be non-negative."))
    }
  }
  if (!"rationale" %in% names(predictions)) {
    predictions$rationale <- NA_character_
  }
  if (!"execution_error" %in% names(predictions)) {
    predictions$execution_error <- NA_character_
  }
  predictions$execution_error <- as.character(predictions$execution_error)
  predictions
}

research_eval_rate <- function(numerator, denominator) {
  if (denominator == 0L) {
    return(NA_real_)
  }
  as.double(numerator) / as.double(denominator)
}

research_eval_observed_summary <- function(values) {
  observed <- values[is.finite(values)]
  list(
    observed_sum = if (length(observed) == 0L) NA_real_ else sum(observed),
    observed_mean = if (length(observed) == 0L) NA_real_ else mean(observed),
    observed_median = if (length(observed) == 0L) {
      NA_real_
    } else {
      stats::median(observed)
    },
    observed_rows = length(observed),
    unknown_rows = length(values) - length(observed)
  )
}

research_eval_summarize <- function(predictions, rows) {
  predictions <- research_eval_normalize_predictions(predictions, rows)
  gold <- rows$expected_status
  predicted <- predictions$predicted_status
  answerable <- gold != "unverifiable"
  execution_failed <- !is.na(predictions$execution_error) &
    nzchar(predictions$execution_error)
  successful <- !execution_failed
  gold_supportable <- gold %in% c("supported", "partially_supported")
  predicted_support <- predicted %in% c("supported", "partially_supported")
  negative_for_support <- !gold_supportable
  false_support <- successful & negative_for_support & predicted_support
  full_support_overcall <- successful &
    gold != "supported" &
    predicted == "supported"
  false_abstention <- successful & answerable & predicted == "unverifiable"
  covered <- successful & predicted != "unverifiable"
  exact <- successful & predicted == gold
  n <- length(gold)

  overall <- list(
    citation_support_correctness = list(
      correct = sum(exact),
      total = n,
      accuracy = research_eval_rate(sum(exact), n)
    ),
    false_support = list(
      count = sum(false_support),
      denominator = sum(negative_for_support & successful),
      rate = research_eval_rate(
        sum(false_support),
        sum(negative_for_support & successful)
      )
    ),
    full_support_overcall = list(
      count = sum(full_support_overcall),
      denominator = sum(gold != "supported" & successful),
      rate = research_eval_rate(
        sum(full_support_overcall),
        sum(gold != "supported" & successful)
      )
    ),
    false_abstention = list(
      count = sum(false_abstention),
      denominator = sum(answerable & successful),
      rate = research_eval_rate(
        sum(false_abstention),
        sum(answerable & successful)
      )
    ),
    coverage = list(
      covered = sum(covered),
      total = n,
      rate = research_eval_rate(sum(covered), n),
      answerable_covered = sum(answerable & covered),
      answerable_total = sum(answerable),
      answerable_rate = research_eval_rate(
        sum(answerable & covered),
        sum(answerable)
      )
    ),
    correct_unverifiable = list(
      count = sum(!answerable & !covered & successful),
      denominator = sum(!answerable),
      rate = research_eval_rate(
        sum(!answerable & !covered & successful),
        sum(!answerable)
      )
    ),
    execution_errors = list(
      count = sum(execution_failed),
      denominator = n,
      rate = research_eval_rate(sum(execution_failed), n)
    )
  )

  by_challenge <- lapply(unique(rows$challenge_type), function(challenge) {
    index <- rows$challenge_type == challenge
    answerable_index <- index & answerable
    false_support_index <- index & negative_for_support & successful
    list(
      challenge_type = challenge,
      citation_support_correctness = list(
        correct = sum(exact[index]),
        total = sum(index),
        accuracy = research_eval_rate(sum(exact[index]), sum(index))
      ),
      false_support = list(
        count = sum(false_support[index]),
        denominator = sum(false_support_index),
        rate = research_eval_rate(
          sum(false_support[index]),
          sum(false_support_index)
        )
      ),
      full_support_overcall = list(
        count = sum(full_support_overcall[index]),
        denominator = sum(index & gold != "supported" & successful),
        rate = research_eval_rate(
          sum(full_support_overcall[index]),
          sum(index & gold != "supported" & successful)
        )
      ),
      false_abstention = list(
        count = sum(false_abstention[index]),
        denominator = sum(answerable_index & successful),
        rate = research_eval_rate(
          sum(false_abstention[index]),
          sum(answerable_index & successful)
        )
      ),
      coverage = list(
        covered = sum(covered[index]),
        total = sum(index),
        rate = research_eval_rate(sum(covered[index]), sum(index)),
        answerable_covered = sum(answerable[index] & covered[index]),
        answerable_total = sum(answerable_index),
        answerable_rate = research_eval_rate(
          sum(answerable[index] & covered[index]),
          sum(answerable_index)
        )
      ),
      execution_errors = list(
        count = sum(execution_failed[index]),
        denominator = sum(index),
        rate = research_eval_rate(sum(execution_failed[index]), sum(index))
      )
    )
  })
  names(by_challenge) <- unique(rows$challenge_type)

  list(
    n_citations = n,
    n_topics = length(unique(rows$topic_id)),
    overall = overall,
    by_challenge = by_challenge,
    cost_and_trace = list(
      input_tokens = research_eval_observed_summary(predictions$input_tokens),
      output_tokens = research_eval_observed_summary(predictions$output_tokens),
      total_tokens = research_eval_observed_summary(predictions$total_tokens),
      provider_reported_cost = research_eval_observed_summary(
        predictions$provider_reported_cost
      ),
      duration_s = research_eval_observed_summary(predictions$duration_s),
      latency_ms = research_eval_observed_summary(predictions$latency_ms),
      trace_event_count = research_eval_observed_summary(
        predictions$trace_event_count
      )
    ),
    case_results = lapply(seq_len(n), function(index) {
      list(
        case_id = rows$case_id[[index]],
        citation_id = rows$citation_id[[index]],
        topic_id = rows$topic_id[[index]],
        challenge_type = rows$challenge_type[[index]],
        expected_status = gold[[index]],
        predicted_status = predicted[[index]],
        prediction_succeeded = successful[[index]],
        exact_match = exact[[index]],
        false_support = false_support[[index]],
        full_support_overcall = full_support_overcall[[index]],
        false_abstention = false_abstention[[index]],
        rationale = predictions$rationale[[index]],
        execution_error = predictions$execution_error[[index]]
      )
    })
  )
}

research_eval_metric_feedback <- function(prediction, expected, metric) {
  predicted <- as.character(research_eval_or_null(prediction$status, ""))
  gold <- as.character(expected$expected_status[[1L]])
  rationale <- as.character(expected$gold_rationale[[1L]])
  case_id <- as.character(expected$case_id[[1L]])
  if (
    length(predicted) != 1L ||
      is.na(predicted) ||
      !predicted %in% research_eval_statuses()
  ) {
    return(list(
      score = 0,
      feedback = paste0(
        case_id,
        ": missing or malformed status output; this is an execution error, not an abstention. ",
        rationale
      )
    ))
  }
  if (identical(metric, "status_accuracy")) {
    score <- as.numeric(identical(predicted, gold))
    feedback <- if (score == 1) {
      paste0(case_id, ": correct citation status.")
    } else {
      paste0(
        case_id,
        ": expected ",
        gold,
        " but got ",
        predicted,
        ". ",
        rationale
      )
    }
  } else if (identical(metric, "false_support")) {
    false_support <- !gold %in% c("supported", "partially_supported") &&
      predicted %in% c("supported", "partially_supported")
    score <- as.numeric(!false_support)
    feedback <- if (false_support) {
      paste0(case_id, ": false support judgment. ", rationale)
    } else {
      paste0(case_id, ": no false support judgment.")
    }
  } else if (identical(metric, "full_support_overcall")) {
    overcall <- !identical(gold, "supported") &&
      identical(predicted, "supported")
    score <- as.numeric(!overcall)
    feedback <- if (overcall) {
      paste0(
        case_id,
        ": full support overstates the expected status. ",
        rationale
      )
    } else {
      paste0(case_id, ": no full-support overcall.")
    }
  } else if (identical(metric, "false_abstention")) {
    false_abstention <- !identical(gold, "unverifiable") &&
      identical(predicted, "unverifiable")
    score <- as.numeric(!false_abstention)
    feedback <- if (false_abstention) {
      paste0(case_id, ": abstained despite available evidence. ", rationale)
    } else {
      paste0(case_id, ": no false abstention.")
    }
  } else {
    research_eval_abort("Unknown evaluation metric.")
  }
  list(score = score, feedback = feedback)
}

research_eval_gepa_metrics <- function(token_budget_per_citation = 4096L) {
  if (
    length(token_budget_per_citation) != 1L ||
      is.na(token_budget_per_citation) ||
      token_budget_per_citation < 1L
  ) {
    research_eval_abort("token_budget_per_citation must be a positive integer.")
  }
  fields <- c(
    "status_accuracy",
    "false_support",
    "full_support_overcall",
    "false_abstention"
  )
  feedback_metrics <- lapply(fields, function(metric_name) {
    dsprrr::metric_with_feedback(
      function(prediction, expected) {
        research_eval_metric_feedback(prediction, expected, metric_name)
      },
      field = "expected_status"
    )
  })
  names(feedback_metrics) <- fields
  feedback_metrics$token_efficiency <- dsprrr::metric_with_trace(
    function(prediction, expected, program_trace) {
      total_tokens <- research_eval_number(
        research_eval_or_null(program_trace$metadata$total_tokens, NA_real_)
      )
      if (is.na(total_tokens)) {
        return(list(
          score = NA_real_,
          feedback = "Token telemetry was unavailable; cost was not inferred."
        ))
      }
      score <- max(0, 1 - total_tokens / token_budget_per_citation)
      list(
        score = score,
        feedback = paste0(
          "Observed ",
          total_tokens,
          " total tokens; target budget is ",
          token_budget_per_citation,
          " per citation. Provider-reported cost and latency are reported separately."
        )
      )
    },
    field = "expected_status"
  )
  feedback_metrics
}

research_eval_extract_evaluation_predictions <- function(evaluation, rows) {
  metadata <- research_eval_or_null(
    evaluation$metadata,
    vector("list", nrow(rows))
  )
  traces <- research_eval_or_null(
    evaluation$traces,
    vector("list", nrow(rows))
  )
  output <- vector("list", nrow(rows))
  for (index in seq_len(nrow(rows))) {
    raw_prediction <- if (length(evaluation$predictions) >= index) {
      evaluation$predictions[[index]]
    } else {
      NULL
    }
    output_error <- NULL
    if (!is.list(raw_prediction) || is.data.frame(raw_prediction)) {
      output_error <- "missing_or_malformed_prediction"
      prediction <- list()
    } else {
      prediction <- raw_prediction
    }
    row_metadata <- if (length(metadata) >= index) {
      research_eval_or_null(metadata[[index]], list())
    } else {
      list()
    }
    trace <- if (length(traces) >= index) {
      research_eval_or_null(traces[[index]], list())
    } else {
      list()
    }
    trace_metadata <- research_eval_or_null(trace$metadata, list())
    get_numeric <- function(field) {
      value <- research_eval_or_null(
        row_metadata[[field]],
        research_eval_or_null(trace_metadata[[field]], NA_real_)
      )
      research_eval_number(value)
    }
    raw_status <- prediction$status
    status <- if (is.null(raw_status)) {
      character()
    } else {
      as.character(raw_status)
    }
    status_valid <- length(status) == 1L &&
      !is.na(status) &&
      status %in% research_eval_statuses()
    if (is.null(output_error) && !status_valid) {
      output_error <- if (length(status) == 0L) {
        "missing_status_output"
      } else {
        "malformed_status_output"
      }
    }
    if (!status_valid) {
      status <- "unverifiable"
    }
    provider_error <- as.character(
      research_eval_or_null(row_metadata$error, "")
    )
    if (
      length(provider_error) != 1L ||
        is.na(provider_error) ||
        !nzchar(provider_error)
    ) {
      provider_error <- NA_character_
    }
    execution_error <- if (!is.na(provider_error)) {
      provider_error
    } else if (!is.null(output_error)) {
      output_error
    } else {
      NA_character_
    }
    output[[index]] <- data.frame(
      case_id = rows$case_id[[index]],
      predicted_status = status,
      rationale = as.character(
        research_eval_or_null(prediction$rationale, NA_character_)
      ),
      input_tokens = get_numeric("input_tokens"),
      output_tokens = get_numeric("output_tokens"),
      total_tokens = get_numeric("total_tokens"),
      provider_reported_cost = get_numeric("cost"),
      duration_s = get_numeric("duration_s"),
      latency_ms = get_numeric("latency_ms"),
      trace_event_count = as.double(length(
        research_eval_or_null(trace$events, list())
      )),
      execution_error = execution_error,
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, output)
}

research_eval_report_evaluation <- function(
  module,
  rows,
  chat,
  split,
  model_label,
  program_id
) {
  executable_program <- module$copy(deep = TRUE)
  metric <- research_eval_gepa_metrics()$status_accuracy
  evaluation <- dsprrr::evaluate(
    executable_program,
    data = rows,
    metric = metric,
    .llm = chat,
    .cache = FALSE,
    .progress = FALSE,
    .return_format = "structured",
    .trace_context = list(
      product = "tempest",
      evaluation = "synthetic-research-corpus",
      split = split,
      model_label = model_label
    )
  )
  predictions <- research_eval_extract_evaluation_predictions(evaluation, rows)
  summary <- research_eval_summarize(predictions, rows)
  list(
    model_label = model_label,
    split = split,
    program_id = program_id,
    dsprrr_mean_score = evaluation$mean_score,
    dsprrr_n_errors = evaluation$n_errors,
    dsprrr_total_cost = evaluation$total_cost,
    metrics = summary
  )
}

research_eval_compile_gepa <- function(
  program,
  teleprompter,
  trainset,
  valset,
  ...
) {
  dsprrr::compile(
    program,
    teleprompter,
    trainset = trainset,
    valset = valset,
    ...
  )
}

research_eval_write_report <- function(report, report_path, overwrite = FALSE) {
  if (
    !is.character(report_path) ||
      length(report_path) != 1L ||
      is.na(report_path) ||
      !nzchar(report_path)
  ) {
    research_eval_abort("report_path must be one non-empty file path.")
  }
  if (file.exists(report_path) && !isTRUE(overwrite)) {
    research_eval_abort(
      paste0(
        "Report already exists; choose a new path or set overwrite = TRUE: ",
        report_path
      )
    )
  }
  dir.create(dirname(report_path), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(
    report,
    path = report_path,
    auto_unbox = TRUE,
    null = "null",
    na = "null",
    dataframe = "rows",
    digits = NA,
    pretty = TRUE
  )
  normalizePath(report_path, mustWork = TRUE)
}

research_eval_base_report <- function(corpus, evaluation_mode) {
  corpus_path <- attr(corpus, "path", exact = TRUE)
  list(
    report_schema = "tempest-research-eval-report/1",
    evaluation_mode = evaluation_mode,
    generated_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    corpus = list(
      corpus_id = corpus$corpus_id,
      schema_version = corpus$schema_version,
      file = basename(corpus_path),
      sha256 = attr(corpus, "sha256", exact = TRUE),
      synthetic = TRUE,
      human_adjudicated = FALSE
    ),
    split = list(
      split_seed = corpus$split_seed,
      split_unit = corpus$split_unit,
      split_method = corpus$split_method,
      topic_ids = lapply(corpus$splits, function(split) {
        rows <- research_eval_case_table(corpus)
        sort(unique(rows$topic_id[rows$split == split]))
      }) |>
        stats::setNames(corpus$splits),
      test_used_for_optimization = FALSE
    ),
    limitations = c(
      "Every case is synthetic and its expected status is a designed fixture, not human gold.",
      "Offline selfcheck scores supplied labels only; it is not a model evaluation.",
      "Provider cost and latency are reported only when the execution trace supplies them."
    )
  )
}

research_eval_selfcheck <- function(
  predictions,
  split = "test",
  corpus_path = research_eval_default_corpus_path(),
  report_path,
  model_id = "supplied-predictions-offline",
  overwrite = FALSE
) {
  corpus <- research_eval_read_corpus(corpus_path)
  rows <- research_eval_split_data(corpus, split)
  supplied <- research_eval_normalize_predictions(predictions, rows)
  report <- research_eval_base_report(corpus, "offline_selfcheck")
  report$model_ids <- list(
    task_model_id = model_id,
    reflection_model_id = NULL,
    provenance = "supplied predictions; no provider was called"
  )
  report$program_ids <- list(
    baseline = NULL,
    best = NULL,
    provenance = "offline predictions have no executable program artifact"
  )
  report$split$evaluated_split <- split
  report$evaluations <- list()
  report$evaluations[[split]] <- research_eval_summarize(supplied, rows)
  report$optimization_result <- NULL
  report$file <- research_eval_write_report(report, report_path, overwrite)
  report
}

research_eval_git_head <- function(package_dir) {
  head <- tryCatch(
    system2(
      "git",
      c("-C", shQuote(package_dir), "rev-parse", "HEAD"),
      stdout = TRUE,
      stderr = FALSE
    ),
    error = function(error) character()
  )
  if (length(head) == 0L || !nzchar(head[[1L]])) {
    return(NA_character_)
  }
  head[[1L]]
}

research_eval_optimization_summary <- function(result) {
  baseline <- research_eval_number(
    research_eval_or_null(result$baseline_score, NA_real_)
  )
  best <- research_eval_number(research_eval_or_null(
    result$best_score,
    NA_real_
  ))
  list(
    optimizer = research_eval_or_null(result$optimizer, NA_character_),
    status = research_eval_or_null(result$status, NA_character_),
    best_vs_baseline = list(
      baseline_score = baseline,
      best_score = best,
      delta = if (is.na(baseline) || is.na(best)) NA_real_ else best - baseline,
      score_source = "dsprrr::optimization_result()"
    ),
    best_trial = research_eval_or_null(result$best_trial, NULL),
    best_params = research_eval_or_null(result$best_params, NULL),
    stop_reason = research_eval_or_null(result$stop_reason, NULL),
    budget = research_eval_or_null(result$budget, NULL)
  )
}

research_eval_program_identity <- function(program) {
  executable_program <- program$copy(deep = TRUE)
  list(
    id = dsprrr::program_artifact_id(executable_program),
    method = paste(
      "dsprrr::program_artifact_id() on Module$copy();",
      "optimization run state reset, executable instructions retained"
    )
  )
}

research_eval_load_tempest_checkout <- function(package_dir) {
  package_dir <- normalizePath(package_dir, mustWork = TRUE)
  if ("tempest" %in% loadedNamespaces()) {
    namespace <- asNamespace("tempest")
    loaded_path <- tryCatch(
      normalizePath(getNamespaceInfo(namespace, "path"), mustWork = TRUE),
      error = function(error) NA_character_
    )
    path_missing <- length(loaded_path) != 1L || is.na(loaded_path)
    if (path_missing || !identical(loaded_path, package_dir)) {
      research_eval_abort(paste0(
        "Tempest is already loaded from ",
        if (path_missing) "an unknown path" else loaded_path,
        ". Start a fresh R session to load the requested current checkout at ",
        package_dir,
        ". The evaluation harness will not reload a live Tempest namespace."
      ))
    }
    return(invisible(namespace))
  }

  devtools::load_all(package_dir, quiet = TRUE)
  namespace <- asNamespace("tempest")
  loaded_path <- normalizePath(
    getNamespaceInfo(namespace, "path"),
    mustWork = TRUE
  )
  if (!identical(loaded_path, package_dir)) {
    research_eval_abort(paste0(
      "Loading Tempest did not produce the requested checkout at ",
      package_dir,
      ". Start a fresh R session and retry."
    ))
  }
  invisible(namespace)
}

research_eval_optimize <- function(
  chat,
  model_id,
  report_path,
  reflection_chat = NULL,
  reflection_model_id = model_id,
  corpus_path = research_eval_default_corpus_path(),
  package_dir = research_eval_find_root(),
  seed = 20261114L,
  population_size = 4L,
  generations = 1L,
  token_budget_per_citation = 4096L,
  verbose = FALSE,
  overwrite = FALSE
) {
  if (is.null(chat) || !inherits(chat, "Chat")) {
    research_eval_abort(
      "Opt-in optimization requires an explicitly supplied ellmer Chat as chat."
    )
  }
  if (
    !is.character(model_id) ||
      length(model_id) != 1L ||
      is.na(model_id) ||
      !nzchar(model_id)
  ) {
    research_eval_abort(
      "model_id must identify the explicitly supplied task model."
    )
  }
  if (is.null(reflection_chat)) {
    reflection_chat <- chat
    reflection_model_id <- model_id
  }
  if (
    !inherits(reflection_chat, "Chat") ||
      !is.character(reflection_model_id) ||
      length(reflection_model_id) != 1L ||
      is.na(reflection_model_id) ||
      !nzchar(reflection_model_id)
  ) {
    research_eval_abort(
      "reflection_chat and reflection_model_id must identify an ellmer Chat."
    )
  }
  if (
    length(population_size) != 1L ||
      is.na(population_size) ||
      population_size < 2L ||
      length(generations) != 1L ||
      is.na(generations) ||
      generations < 1L
  ) {
    research_eval_abort(
      "GEPA population_size and generations must be positive integers."
    )
  }

  corpus <- research_eval_read_corpus(corpus_path)
  rows <- research_eval_case_table(corpus)
  trainset <- rows[rows$split == "train", , drop = FALSE]
  valset <- rows[rows$split == "validation", , drop = FALSE]
  testset <- rows[rows$split == "test", , drop = FALSE]

  research_eval_load_tempest_checkout(package_dir)
  producer <- getFromNamespace("tempest_make_dsprrr_modules", "tempest")
  config <- getExportedValue("tempest", "tempest_config")()
  baseline_module <- producer(config)$verify_claim_support
  baseline_identity <- research_eval_program_identity(baseline_module)
  baseline_program_id <- baseline_identity$id

  baseline_train <- research_eval_report_evaluation(
    baseline_module,
    trainset,
    chat,
    "train",
    "baseline",
    baseline_program_id
  )
  baseline_validation <- research_eval_report_evaluation(
    baseline_module,
    valset,
    chat,
    "validation",
    "baseline",
    baseline_program_id
  )

  metrics <- research_eval_gepa_metrics(token_budget_per_citation)
  optimizer <- dsprrr::GEPA(
    metrics = metrics,
    population_size = as.integer(population_size),
    generations = as.integer(generations),
    seed = as.integer(seed),
    selection = "pareto",
    verbose = isTRUE(verbose)
  )
  optimized_module <- research_eval_compile_gepa(
    baseline_module,
    optimizer,
    trainset = trainset,
    valset = valset,
    .llm = chat,
    .reflection_llm = reflection_chat,
    .trace_context = list(
      product = "tempest",
      evaluation = "synthetic-research-corpus",
      corpus_id = corpus$corpus_id,
      split_seed = corpus$split_seed,
      optimizer_seed = as.integer(seed)
    )
  )
  optimized_identity <- research_eval_program_identity(optimized_module)
  optimized_program_id <- optimized_identity$id
  optimization_result <- dsprrr::optimization_result(optimized_module)
  optimization_summary <- research_eval_optimization_summary(
    optimization_result
  )

  baseline_test <- research_eval_report_evaluation(
    baseline_module,
    testset,
    chat,
    "test",
    "baseline",
    baseline_program_id
  )
  optimized_train <- research_eval_report_evaluation(
    optimized_module,
    trainset,
    chat,
    "train",
    "best",
    optimized_program_id
  )
  optimized_validation <- research_eval_report_evaluation(
    optimized_module,
    valset,
    chat,
    "validation",
    "best",
    optimized_program_id
  )
  optimized_test <- research_eval_report_evaluation(
    optimized_module,
    testset,
    chat,
    "test",
    "best",
    optimized_program_id
  )

  report <- research_eval_base_report(corpus, "dsprrr_evaluate_gepa")
  report$model_ids <- list(
    task_model_id = model_id,
    reflection_model_id = reflection_model_id,
    provenance = "Explicitly supplied ellmer Chats; provider work was opted into by calling research_eval_optimize()."
  )
  report$program_ids <- list(
    producer = "tempest:::tempest_make_dsprrr_modules()$verify_claim_support",
    baseline = baseline_program_id,
    best = optimized_program_id,
    baseline_method = baseline_identity$method,
    best_method = optimized_identity$method,
    package_head = research_eval_git_head(package_dir)
  )
  report$split$optimizer_seed <- as.integer(seed)
  report$split$test_used_for_optimization <- FALSE
  report$optimizer_settings <- list(
    name = "GEPA",
    population_size = as.integer(population_size),
    generations = as.integer(generations),
    selection = "pareto",
    token_budget_per_citation = as.integer(token_budget_per_citation),
    tuning_splits = c("train", "validation"),
    held_out_split = "test"
  )
  report$evaluations <- list(
    baseline = list(
      train = baseline_train,
      validation = baseline_validation,
      test = baseline_test
    ),
    best = list(
      train = optimized_train,
      validation = optimized_validation,
      test = optimized_test
    )
  )
  report$optimization_result <- optimization_summary
  report$file <- research_eval_write_report(report, report_path, overwrite)
  report
}

research_eval_cli <- function(args = commandArgs(trailingOnly = TRUE)) {
  if (length(args) < 1L || !identical(args[[1L]], "selfcheck")) {
    research_eval_abort(paste(
      "Usage:",
      "Rscript tools/research-eval.R selfcheck <predictions.csv> <report.json>",
      "The opt-in GEPA runner is a function so callers can inject explicit ellmer Chats."
    ))
  }
  if (length(args) < 3L || length(args) > 4L) {
    research_eval_abort(paste(
      "Usage:",
      "Rscript tools/research-eval.R selfcheck <predictions.csv> <report.json> [corpus.json]"
    ))
  }
  corpus_path <- if (length(args) == 4L) {
    args[[4L]]
  } else {
    research_eval_default_corpus_path()
  }
  report <- research_eval_selfcheck(
    predictions = args[[2L]],
    corpus_path = corpus_path,
    report_path = args[[3L]]
  )
  print(jsonlite::toJSON(
    report$evaluations,
    auto_unbox = TRUE,
    null = "null",
    na = "null",
    dataframe = "rows",
    digits = NA,
    pretty = TRUE
  ))
  invisible(report)
}

if (sys.nframe() == 0L && !interactive()) {
  arguments <- commandArgs(trailingOnly = TRUE)
  if (length(arguments) > 0L) {
    research_eval_cli(arguments)
  }
}
