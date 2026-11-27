# Stage output evaluators and evidence grounding

tempest_stage_output_abort <- function(message, ..., parent = NULL) {
  tempest_stage_abort(
    message,
    ...,
    class = "tempest_stage_output_validation_error",
    parent = parent,
    .envir = rlang::caller_env()
  )
}

tempest_stage_evaluator_abort <- function(message, ..., parent = NULL) {
  tempest_stage_abort(
    message,
    ...,
    class = "tempest_stage_evaluator_contract_error",
    parent = parent,
    .envir = rlang::caller_env()
  )
}

tempest_stage_governance_abort <- function(message, ..., parent = NULL) {
  tempest_stage_abort(
    message,
    ...,
    class = "tempest_stage_governance_error",
    parent = parent,
    .envir = rlang::caller_env()
  )
}

tempest_stage_scalar_character <- function(value, field) {
  if (
    !rlang::is_string(value) ||
      is.na(value) ||
      !nzchar(tempest_trim(value))
  ) {
    tempest_stage_output_abort(
      "Stage output field {.field {field}} must be a non-empty string."
    )
  }
  tempest_trim(value)
}

tempest_stage_plain_record <- function(value, field) {
  if (
    !is.list(value) ||
      is.data.frame(value) ||
      !is.null(attr(value, "class", exact = TRUE)) ||
      is.null(names(value)) ||
      anyNA(names(value)) ||
      any(!nzchar(names(value))) ||
      anyDuplicated(names(value))
  ) {
    tempest_stage_output_abort(
      "Stage output field {.field {field}} must be a plain named record."
    )
  }
  value
}

tempest_stage_exact_record <- function(
  value,
  field,
  required,
  optional = character()
) {
  value <- tempest_stage_plain_record(value, field)
  fields <- names(value)
  missing <- setdiff(required, fields)
  unexpected <- setdiff(fields, c(required, optional))
  if (length(missing) > 0L || length(unexpected) > 0L) {
    tempest_stage_output_abort(
      paste0(
        "Stage output field {.field {field}} has missing or unsupported ",
        "fields."
      )
    )
  }
  value
}

tempest_stage_character_vector <- function(value, field, allow_empty = FALSE) {
  if (is.list(value)) {
    if (is.data.frame(value) || !is.null(names(value))) {
      tempest_stage_output_abort(
        "Stage output field {.field {field}} must be an unnamed string array."
      )
    }
    if (!all(vapply(value, rlang::is_string, logical(1)))) {
      tempest_stage_output_abort(
        "Stage output field {.field {field}} must contain only scalar strings."
      )
    }
    value <- unlist(value, use.names = FALSE)
  }
  if (
    !is.character(value) ||
      is.object(value) ||
      !is.null(names(value))
  ) {
    tempest_stage_output_abort(
      "Stage output field {.field {field}} must be an unnamed string array."
    )
  }
  value <- tempest_trim(value)
  if (
    anyNA(value) ||
      any(!nzchar(value)) ||
      (!allow_empty && length(value) == 0L)
  ) {
    tempest_stage_output_abort(
      "Stage output field {.field {field}} must contain non-empty strings."
    )
  }
  if (anyDuplicated(value)) {
    tempest_stage_output_abort(
      "Stage output field {.field {field}} must not contain duplicates."
    )
  }
  value
}

tempest_stage_outline_character_vector <- function(
  value,
  field,
  allow_empty = FALSE
) {
  if (is.data.frame(value)) {
    tempest_stage_output_abort(
      "Stage output field {.field {field}} must not be tabular."
    )
  }
  if (is.list(value)) {
    if (!all(vapply(value, rlang::is_string, logical(1)))) {
      tempest_stage_output_abort(
        paste0(
          "Stage output field {.field {field}} contains a non-string list ",
          "element."
        )
      )
    }
    value <- unlist(value, use.names = FALSE)
  } else if (is.character(value) && !is.object(value)) {
    value <- unname(value)
  } else {
    shape <- if (is.object(value)) "typed atomic" else typeof(value)
    tempest_stage_output_abort(
      "Stage output field {.field {field}} has unsupported {shape} shape."
    )
  }
  tempest_stage_character_vector(value, field, allow_empty = allow_empty)
}

tempest_stage_normalize_output <- function(callback) {
  tryCatch(
    callback(),
    tempest_stage_output_validation_error = function(error) stop(error),
    error = function(error) {
      tempest_stage_output_abort(
        "Stage output failed its exact normalization contract."
      )
    }
  )
}

tempest_stage_evaluate_perspectives <- function(output, context) {
  output <- tempest_stage_exact_record(
    output,
    "output",
    c("title", "perspectives")
  )
  perspectives <- output$perspectives
  if (
    !is.list(perspectives) ||
      is.data.frame(perspectives) ||
      !is.null(names(perspectives)) ||
      length(perspectives) == 0L
  ) {
    tempest_stage_output_abort(
      "Perspective output must contain at least one perspective."
    )
  }
  for (perspective in perspectives) {
    perspective <- tempest_stage_exact_record(
      perspective,
      "perspectives",
      c("name", "description", "key_questions")
    )
    tempest_stage_scalar_character(perspective$name, "perspectives$name")
    tempest_stage_scalar_character(
      perspective$description,
      "perspectives$description"
    )
    tempest_stage_character_vector(
      perspective$key_questions,
      "perspectives$key_questions"
    )
  }
  topic <- context$topic %||% output$title %||% "Research report"
  normalized <- tempest_stage_normalize_output(\() {
    tempest_normalize_perspectives(
      output,
      topic = tempest_stage_scalar_character(topic, "topic"),
      n_experts = context$n_experts %||% NULL
    )
  })
  list(output = normalized, support_status = "unknown")
}

tempest_stage_evaluate_personas <- function(output, context) {
  output <- tempest_stage_exact_record(output, "output", "personas")
  values <- output$personas
  if (
    !is.list(values) ||
      is.data.frame(values) ||
      !is.null(names(values)) ||
      length(values) == 0L
  ) {
    tempest_stage_output_abort(
      "Persona output must contain at least one persona."
    )
  }
  for (persona in values) {
    persona <- tempest_stage_exact_record(
      persona,
      "personas",
      c(
        "name",
        "title",
        "affiliation",
        "background",
        "focus_areas",
        "perspective",
        "initial_questions"
      )
    )
    tempest_stage_scalar_character(persona$name, "personas$name")
    tempest_stage_scalar_character(persona$title, "personas$title")
    tempest_stage_scalar_character(persona$affiliation, "personas$affiliation")
    tempest_stage_scalar_character(persona$background, "personas$background")
    tempest_stage_character_vector(persona$focus_areas, "personas$focus_areas")
    tempest_stage_scalar_character(persona$perspective, "personas$perspective")
    tempest_stage_character_vector(
      persona$initial_questions,
      "personas$initial_questions"
    )
  }
  normalized <- tempest_stage_normalize_output(\() {
    tempest_normalize_experts(
      output,
      n = context$n_experts %||% NULL
    )
  })
  if (length(normalized) == 0L) {
    tempest_stage_output_abort("Persona output did not produce expert records.")
  }
  list(output = normalized, support_status = "unknown")
}

tempest_stage_evaluate_queries <- function(output, context) {
  output <- tempest_stage_exact_record(output, "output", "queries")
  queries <- output$queries
  queries <- tempest_stage_character_vector(queries, "queries")
  max_queries <- context$max_queries %||% length(queries)
  if (
    !is.numeric(max_queries) ||
      length(max_queries) != 1L ||
      is.na(max_queries) ||
      max_queries < 1L
  ) {
    tempest_stage_evaluator_abort("Query evaluator context is malformed.")
  }
  list(
    output = list(
      queries = queries[seq_len(min(length(queries), max_queries))]
    ),
    support_status = "unknown"
  )
}

tempest_stage_known_source_ids <- function(context) {
  if (!inherits(context$workspace, "ResearchWorkspace")) {
    tempest_stage_evaluator_abort(
      "Claim evaluator context requires the exact ResearchWorkspace."
    )
  }
  resources <- context$workspace$list_retrieved_resources()
  vapply(resources, \(resource) resource@resource_id, character(1))
}

tempest_stage_fact_sources <- function(value, workspace) {
  value <- tempest_stage_plain_record(value, "facts")
  sources <- value$sources
  if (
    !is.list(sources) ||
      is.data.frame(sources) ||
      !is.null(names(sources)) ||
      length(sources) == 0L
  ) {
    tempest_stage_output_abort("Every extracted claim must cite a source.")
  }
  sources <- lapply(
    sources,
    function(source) {
      source <- tempest_stage_plain_record(source, "facts$sources")
      allowed <- c("source_id", "url", "quote")
      unexpected <- setdiff(names(source), allowed)
      if (!"source_id" %in% names(source) || length(unexpected) > 0L) {
        tempest_stage_output_abort(
          paste0(
            "Claim source records require {.field source_id} and may only ",
            "contain {.field url} and {.field quote}."
          )
        )
      }
      source_id <- tempest_stage_scalar_character(
        source$source_id,
        "facts$sources$source_id"
      )
      resource <- workspace$get_retrieved_source(source_id)
      if (is.null(resource)) {
        tempest_stage_output_abort(
          "Extracted claims cite unknown source ID {.val {source_id}}."
        )
      }
      url <- if ("url" %in% names(source)) {
        tempest_stage_scalar_character(source$url, "facts$sources$url")
      } else {
        NULL
      }
      if (!is.null(url)) {
        canonical_url <- resource$url %||% NA_character_
        normalized <- tryCatch(
          list(
            output = tempest_normalize_url(url),
            canonical = tempest_normalize_url(canonical_url)
          ),
          error = identity
        )
        if (
          inherits(normalized, "condition") ||
            is.na(normalized$canonical) ||
            !identical(normalized$output, normalized$canonical)
        ) {
          tempest_stage_output_abort(
            paste0(
              "Claim source URL does not match canonical workspace source ",
              "{.val {source_id}}."
            ),
            parent = if (inherits(normalized, "condition")) normalized else NULL
          )
        }
      }
      quote <- if ("quote" %in% names(source)) {
        tempest_stage_scalar_character(source$quote, "facts$sources$quote")
      } else {
        NULL
      }
      if (!is.null(quote)) {
        captured_text <- c(
          resource$content_text %||% NA_character_,
          resource$snippet %||% NA_character_,
          resource$context_text %||% NA_character_
        )
        captured_text <- unique(captured_text[
          !is.na(captured_text) & nzchar(captured_text)
        ])
        if (length(captured_text) == 0L) {
          tempest_stage_output_abort(
            paste0(
              "Claim quote cannot be validated because workspace source ",
              "{.val {source_id}} has no captured text."
            )
          )
        }
        matched <- any(vapply(
          captured_text,
          \(text) grepl(quote, text, fixed = TRUE),
          logical(1)
        ))
        if (!matched) {
          tempest_stage_output_abort(
            "Claim quote is not an exact substring of source {.val {source_id}}."
          )
        }
      }
      list(source_id = source_id, url = url, quote = quote)
    }
  )
  keys <- vapply(
    sources,
    function(source) {
      as.character(jsonlite::toJSON(
        list(source_id = source$source_id, quote = source$quote),
        auto_unbox = TRUE,
        null = "null"
      ))
    },
    character(1)
  )
  if (anyDuplicated(keys)) {
    tempest_stage_output_abort(
      "Extracted claims cannot repeat an identical source record."
    )
  }
  sources
}

tempest_stage_claim_context_trace_validate <- function(
  claim_context,
  trace_references
) {
  if (
    !is.list(trace_references) ||
      (length(trace_references) > 0L && is.null(names(trace_references)))
  ) {
    tempest_stage_evaluator_abort(
      "Claim evaluator context requires exact execution trace references."
    )
  }
  identifier_fields <- intersect(
    c(
      "claim_id",
      "retrieval_step_id",
      "perspective_id",
      "expert_id",
      "session_id",
      "section_id"
    ),
    names(claim_context)
  )
  for (field in identifier_fields) {
    value <- claim_context[[field]]
    if (
      !is.character(value) ||
        length(value) != 1L ||
        (!is.na(value) && !tempest_opaque_identifier_valid(value))
    ) {
      tempest_stage_governance_abort(
        "Claim context field {.field {field}} is not a safe opaque identifier."
      )
    }
  }
  bindings <- c(
    session_id = "research_run_id",
    expert_id = "expert_id",
    retrieval_step_id = "correlation_id"
  )
  for (claim_field in names(bindings)) {
    value <- claim_context[[claim_field]] %||% NA_character_
    if (is.na(value)) {
      next
    }
    trace_field <- bindings[[claim_field]]
    if (!identical(trace_references[[trace_field]] %||% NULL, value)) {
      tempest_stage_governance_abort(
        paste0(
          "Claim context field {.field {claim_field}} must exactly match ",
          "the executing trace reference {.field {trace_field}}."
        )
      )
    }
  }
  invisible(claim_context)
}

tempest_stage_evaluate_claims <- function(output, context) {
  output <- tryCatch(
    tempest_stage_exact_record(output, "output", "facts"),
    tempest_stage_output_validation_error = function(error) stop(error)
  )
  if (is.null(output$facts)) {
    tempest_stage_output_abort(
      "Claim extraction output must contain the exact facts field."
    )
  }
  facts <- output$facts
  if (!is.list(facts) || is.data.frame(facts) || !is.null(names(facts))) {
    tempest_stage_output_abort(
      "Claim extraction output must be a list of facts."
    )
  }
  workspace <- context$workspace
  known_source_ids <- tempest_stage_known_source_ids(context)
  claim_context <- context$claim_context %||% NULL
  if (!is.list(claim_context) || is.null(names(claim_context))) {
    tempest_stage_evaluator_abort("Claim evaluator context is malformed.")
  }
  claim_arguments <- c(
    "claim_type",
    "contradicting_source_ids",
    "contradiction_note",
    "contradiction_score",
    "source_quality_score",
    "retrieval_query",
    "retrieval_step_id",
    "perspective_id",
    "expert_id",
    "session_id",
    "section_id",
    "claim_id",
    "created_at"
  )
  reserved <- c(
    "claim_text",
    "source_ids",
    "confidence",
    "support_score",
    "evidence_span_ids",
    "supporting_quotes"
  )
  if (
    anyDuplicated(names(claim_context)) ||
      any(names(claim_context) %in% reserved) ||
      any(!names(claim_context) %in% claim_arguments)
  ) {
    tempest_stage_evaluator_abort(
      "Claim evaluator context contains unsupported constructor fields."
    )
  }
  tempest_stage_claim_context_trace_validate(
    claim_context,
    context$execution_trace_references %||% list()
  )
  evidence_spans <- list()
  claims <- lapply(facts, function(fact) {
    fact <- tempest_stage_plain_record(fact, "facts")
    allowed_fact_fields <- c(
      "claim",
      "sources",
      "confidence",
      "support_score",
      "note"
    )
    unexpected_fact_fields <- setdiff(names(fact), allowed_fact_fields)
    if (
      !all(c("claim", "sources") %in% names(fact)) ||
        length(unexpected_fact_fields) > 0L
    ) {
      tempest_stage_output_abort(
        "Claim facts contain missing or unsupported fields."
      )
    }
    claim_text <- tempest_stage_scalar_character(fact$claim, "facts$claim")
    sources <- tempest_stage_fact_sources(fact, workspace)
    source_ids <- unique(vapply(
      sources,
      `[[`,
      character(1),
      "source_id"
    ))
    unknown <- setdiff(source_ids, known_source_ids)
    if (length(unknown) > 0L) {
      tempest_stage_output_abort(
        "Extracted claims cite unknown source IDs: {.val {unknown}}."
      )
    }
    confidence <- fact$confidence %||% "medium"
    if (
      !rlang::is_string(confidence) ||
        !confidence %in% c("low", "medium", "high")
    ) {
      tempest_stage_output_abort(
        "Claim confidence must be low, medium, or high."
      )
    }
    score <- fact$support_score %||% NA_real_
    if (
      !is.numeric(score) ||
        length(score) != 1L ||
        (!is.na(score) && (!is.finite(score) || score < 0 || score > 1))
    ) {
      tempest_stage_output_abort("Claim support score must be in [0, 1].")
    }
    if (!is.null(fact$note)) {
      tempest_stage_scalar_character(fact$note, "facts$note")
    }
    quoted_sources <- Filter(\(source) !is.null(source$quote), sources)
    fact_spans <- lapply(quoted_sources, function(source) {
      tempest_evidence_span(
        source_id = source$source_id,
        quote = source$quote,
        extracted_by = context$program_artifact_id
      )
    })
    evidence_spans <<- c(evidence_spans, fact_spans)
    do.call(
      tempest_claim,
      c(
        list(
          claim_text = claim_text,
          source_ids = source_ids,
          confidence = confidence,
          support_score = as.numeric(score),
          evidence_span_ids = vapply(
            fact_spans,
            \(span) span@evidence_span_id,
            character(1)
          ),
          supporting_quotes = unname(lapply(
            quoted_sources,
            `[[`,
            "quote"
          ))
        ),
        claim_context
      )
    )
  })
  attr(claims, "tempest_evidence_spans") <- unname(evidence_spans)
  list(output = claims, support_status = "unknown")
}

tempest_stage_verification_support_status <- function(
  status,
  score,
  min_support_score
) {
  status <- tempest_apply_min_support_score(
    status,
    score,
    min_support_score = min_support_score
  )
  switch(
    status,
    supported = "verified",
    partially_supported = "partially_supported",
    unsupported = "unsupported",
    contradicted = "conflicted",
    unverifiable = "unknown",
    tempest_stage_evaluator_abort(
      "Verification status cannot be mapped to support trust."
    )
  )
}

tempest_stage_evaluate_verification <- function(output, context) {
  output <- tempest_stage_exact_record(
    output,
    "output",
    c("status", "rationale"),
    "score"
  )
  claim <- context$claim %||% NULL
  span <- context$evidence_span %||% NULL
  if (!S7::S7_inherits(claim, tempest_claim)) {
    tempest_stage_evaluator_abort(
      "Verification evaluator context requires one exact claim record."
    )
  }
  if (!S7::S7_inherits(span, tempest_evidence_span)) {
    tempest_stage_evaluator_abort(
      "Verification evaluator context requires one exact evidence span."
    )
  }
  if (
    !span@evidence_span_id %in% claim@evidence_span_ids ||
      !span@source_id %in% claim@source_ids
  ) {
    tempest_stage_governance_abort(
      "Verification evaluator span must be bound to its exact claim."
    )
  }
  statuses <- c(
    "supported",
    "partially_supported",
    "unsupported",
    "contradicted",
    "unverifiable"
  )
  status <- output$status %||% NULL
  if (!rlang::is_string(status) || !status %in% statuses) {
    tempest_stage_output_abort("Verification output has an invalid status.")
  }
  score <- output$score %||% NA_real_
  if (
    !is.numeric(score) ||
      is.object(score) ||
      !is.null(names(score)) ||
      length(score) != 1L ||
      (!is.na(score) && (!is.finite(score) || score < 0 || score > 1))
  ) {
    tempest_stage_output_abort("Verification score must be in [0, 1].")
  }
  if (identical(status, "unverifiable") && !is.na(score)) {
    tempest_stage_output_abort(
      "Unverifiable claim-span support must omit its score."
    )
  }
  if (!identical(status, "unverifiable") && is.na(score)) {
    tempest_stage_output_abort(
      "A verifiable claim-span support assessment requires a score."
    )
  }
  rationale <- output$rationale
  if (
    !rlang::is_string(rationale) ||
      is.na(rationale) ||
      !nzchar(tempest_trim(rationale)) ||
      !identical(rationale, tempest_trim(rationale)) ||
      nchar(rationale, type = "bytes") > 2000L ||
      tempest_contract_sensitive_scalar(rationale)
  ) {
    tempest_stage_output_abort(
      paste0(
        "Verification rationale must be canonical, non-empty, bounded, ",
        "and credential-free."
      )
    )
  }
  min_support_score <- tempest_normalize_min_support_score(
    context$min_support_score %||% 0.7
  )
  status <- tempest_apply_min_support_score(
    status,
    score,
    min_support_score = min_support_score
  )
  support_status <- tempest_stage_verification_support_status(
    status,
    score,
    min_support_score
  )
  support <- tempest_claim_support(
    claim_id = claim@claim_id,
    evidence_span_id = span@evidence_span_id,
    source_id = span@source_id,
    verification_status = status,
    support_score = as.double(score),
    rationale = rationale
  )
  list(
    output = support,
    support_status = support_status
  )
}

tempest_stage_evaluate_next_question <- function(output, context) {
  output <- tempest_stage_exact_record(
    output,
    "output",
    "question",
    "done"
  )
  done <- if ("done" %in% names(output)) output$done else FALSE
  if (!rlang::is_bool(done)) {
    tempest_stage_output_abort(
      "Next-question output must contain a question and one logical done flag."
    )
  }
  question <- tempest_stage_scalar_character(output$question, "question")
  list(
    output = list(question = question, done = done),
    support_status = "unknown"
  )
}

tempest_stage_evaluate_outline <- function(output, context) {
  output <- tempest_stage_exact_record(
    output,
    "output",
    c("title", "sections")
  )
  sections <- output$sections
  if (
    !is.list(sections) ||
      is.data.frame(sections) ||
      !is.null(names(sections)) ||
      length(sections) == 0L
  ) {
    tempest_stage_output_abort(
      "Outline output must contain a non-empty unnamed sections array."
    )
  }
  for (section_index in seq_along(sections)) {
    section <- sections[[section_index]]
    section <- tempest_stage_exact_record(
      section,
      "sections",
      c("title", "summary", "subsections")
    )
    tempest_stage_scalar_character(section$title, "sections$title")
    tempest_stage_scalar_character(section$summary, "sections$summary")
    subsections <- section$subsections
    if (
      !is.list(subsections) ||
        is.data.frame(subsections) ||
        !is.null(names(subsections)) ||
        length(subsections) == 0L
    ) {
      tempest_stage_output_abort(
        "Outline sections require a non-empty unnamed subsections array."
      )
    }
    for (subsection_index in seq_along(subsections)) {
      subsection <- subsections[[subsection_index]]
      subsection <- tempest_stage_exact_record(
        subsection,
        "subsections",
        c("title", "bullets"),
        "needed"
      )
      tempest_stage_scalar_character(subsection$title, "subsections$title")
      subsection$bullets <- tempest_stage_outline_character_vector(
        subsection$bullets,
        "subsections$bullets"
      )
      if ("needed" %in% names(subsection)) {
        subsection$needed <- if (is.null(subsection$needed)) {
          character()
        } else {
          tempest_stage_outline_character_vector(
            subsection$needed,
            "subsections$needed",
            allow_empty = TRUE
          )
        }
      }
      subsections[[subsection_index]] <- subsection
    }
    section$subsections <- subsections
    sections[[section_index]] <- section
  }
  output$sections <- sections
  normalized <- tempest_stage_normalize_output(\() {
    tempest_normalize_outline(output)
  })
  if (length(normalized$sections) == 0L) {
    tempest_stage_output_abort(
      "Outline output must contain at least one section."
    )
  }
  list(output = normalized, support_status = "unknown")
}

tempest_stage_verified_evidence <- function(context) {
  evidence <- tempest_stage_authoritative_claims(
    context,
    "verified_evidence"
  )
  if (!is.list(evidence) || length(evidence) == 0L) {
    tempest_stage_governance_abort(
      "Grounded fallback requires explicit verified evidence."
    )
  }
  min_support_score <- tempest_normalize_min_support_score(
    context$min_support_score %||% 0.7
  )
  workspace <- tempest_stage_workspace(context)
  valid <- vapply(
    evidence,
    function(claim) {
      tempest_stage_claim_verified(claim, workspace, min_support_score)
    },
    logical(1)
  )
  if (!all(valid)) {
    tempest_stage_governance_abort(
      "Grounded fallback evidence must contain only supported claims."
    )
  }
  evidence
}

tempest_stage_workspace <- function(context) {
  workspace <- context$workspace %||% NULL
  if (!inherits(workspace, "ResearchWorkspace")) {
    tempest_stage_evaluator_abort(
      "Grounded stage context requires the authoritative ResearchWorkspace."
    )
  }
  tryCatch(
    workspace$validate_integrity(),
    error = function(error) {
      tempest_stage_evaluator_abort(
        "Grounded stage workspace failed authoritative integrity validation."
      )
    }
  )
  workspace
}

tempest_stage_authoritative_claims <- function(context, field) {
  workspace <- tempest_stage_workspace(context)
  claims <- context[[field]] %||% list()
  if (
    !is.list(claims) ||
      is.data.frame(claims) ||
      !is.null(names(claims)) ||
      !all(vapply(
        claims,
        \(claim) S7::S7_inherits(claim, tempest_claim),
        logical(1)
      ))
  ) {
    tempest_stage_evaluator_abort(
      "Stage context {.field {field}} must be an unnamed list of exact claims."
    )
  }
  ids <- vapply(claims, \(claim) claim@claim_id, character(1))
  if (anyDuplicated(ids)) {
    tempest_stage_evaluator_abort(
      "Stage context {.field {field}} cannot repeat claim IDs."
    )
  }
  for (index in seq_along(claims)) {
    authoritative <- workspace$get_proposed_claim(ids[[index]])
    if (is.null(authoritative) || !identical(authoritative, claims[[index]])) {
      tempest_stage_governance_abort(
        paste0(
          "Stage context {.field {field}} contains a missing, replaced, or ",
          "forged claim."
        )
      )
    }
  }
  claims
}

tempest_stage_claim_support_records <- function(
  workspace,
  claim,
  required = TRUE
) {
  supports <- Filter(
    \(support) identical(support@claim_id, claim@claim_id),
    workspace$list_claim_supports()
  )
  support_span_ids <- vapply(
    supports,
    \(support) support@evidence_span_id,
    character(1)
  )
  exact <- length(supports) > 0L &&
    setequal(support_span_ids, claim@evidence_span_ids)
  if (!exact) {
    if (isTRUE(required)) {
      tempest_stage_governance_abort(
        paste0(
          "Grounded evidence claim {.val {claim@claim_id}} requires the ",
          "complete authoritative claim-by-span support set."
        )
      )
    }
    return(NULL)
  }
  supports
}

tempest_stage_source_evidence_projection <- function(
  workspace,
  source_id,
  required = TRUE
) {
  resource <- workspace$get_retrieved_resource(source_id)
  if (is.null(resource)) {
    if (isTRUE(required)) {
      tempest_stage_governance_abort(
        "Verification source {.val {source_id}} is not in the workspace."
      )
    }
    return(NULL)
  }
  source <- tempest_resource_as_source(resource)
  candidates <- list(
    source$content_text %||% NA_character_,
    source$snippet %||% NA_character_,
    source$context_text %||% NA_character_
  )
  captured_text <- NULL
  for (candidate in candidates) {
    if (
      rlang::is_string(candidate) &&
        !is.na(candidate) &&
        nzchar(tempest_trim(candidate))
    ) {
      captured_text <- candidate
      break
    }
  }
  if (is.null(captured_text)) {
    if (isTRUE(required)) {
      tempest_stage_governance_abort(
        paste0(
          "Verification source {.val {source_id}} has no exact captured ",
          "content, snippet, or context."
        )
      )
    }
    return(NULL)
  }
  title <- source$title %||% ""
  if (!rlang::is_string(title) || is.na(title)) {
    title <- ""
  }
  excerpt <- paste0(
    "[",
    source_id,
    "] ",
    title,
    ": ",
    substr(captured_text, 1L, 1500L)
  )
  list(
    source_id = source_id,
    resource_fingerprint = tempest_resource_fingerprint(resource),
    content_hash = if (is.na(resource@content_hash)) {
      NULL
    } else {
      resource@content_hash
    },
    captured_text_digest = tempest_stage_content_digest_id(captured_text),
    excerpt = excerpt,
    excerpt_digest = tempest_stage_content_digest_id(excerpt)
  )
}

tempest_stage_claim_source_evidence <- function(
  claim,
  workspace,
  required = TRUE
) {
  if (!S7::S7_inherits(claim, tempest_claim)) {
    tempest_stage_evaluator_abort(
      "Verification evidence requires one exact claim."
    )
  }
  if (!inherits(workspace, "ResearchWorkspace")) {
    tempest_stage_evaluator_abort(
      "Verification evidence requires one authoritative ResearchWorkspace."
    )
  }
  lapply(
    claim@source_ids,
    \(source_id) {
      tempest_stage_source_evidence_projection(
        workspace,
        source_id,
        required = required
      )
    }
  )
}

tempest_stage_claim_has_captured_evidence <- function(claim, workspace) {
  if (length(claim@source_ids) == 0L) {
    return(FALSE)
  }
  evidence <- tempest_stage_claim_source_evidence(
    claim,
    workspace,
    required = FALSE
  )
  length(evidence) == length(claim@source_ids) &&
    all(!vapply(evidence, is.null, logical(1)))
}

tempest_stage_claim_verified <- function(claim, workspace, min_support_score) {
  if (!identical(claim@verification_status, "supported")) {
    return(FALSE)
  }
  supports <- tempest_stage_claim_support_records(
    workspace,
    claim,
    required = TRUE
  )
  support_scores <- vapply(
    supports,
    \(support) support@support_score,
    numeric(1)
  )
  all(vapply(
    supports,
    \(support) identical(support@verification_status, "supported"),
    logical(1)
  )) &&
    all(!is.na(support_scores) & support_scores >= min_support_score) &&
    !is.na(claim@support_score) &&
    is.finite(claim@support_score) &&
    claim@support_score >= min_support_score &&
    tempest_stage_claim_has_captured_evidence(claim, workspace)
}

tempest_stage_evidence <- function(context) {
  tempest_stage_authoritative_claims(context, "evidence")
}

tempest_stage_evidence_support <- function(evidence, context) {
  if (length(evidence) == 0L) {
    return("unknown")
  }
  statuses <- vapply(
    evidence,
    \(claim) claim@verification_status,
    character(1)
  )
  if (any(statuses == "contradicted")) {
    return("conflicted")
  }
  min_support_score <- tempest_normalize_min_support_score(
    context$min_support_score %||% 0.7
  )
  verified <- vapply(
    evidence,
    function(claim) {
      identical(claim@verification_status, "supported") &&
        !is.na(claim@support_score) &&
        claim@support_score >= min_support_score
    },
    logical(1)
  )
  low_score_supported <- vapply(
    evidence,
    function(claim) {
      identical(claim@verification_status, "supported") &&
        !is.na(claim@support_score) &&
        claim@support_score < min_support_score
    },
    logical(1)
  )
  if (any(statuses == "unsupported") || any(low_score_supported)) {
    return("unsupported")
  }
  unknown <- statuses %in%
    c("unverified", "unverifiable") |
    (statuses == "supported" & !verified)
  if (any(unknown)) {
    return("unknown")
  }
  if (any(statuses == "partially_supported")) {
    return("partially_supported")
  }
  if (all(verified)) {
    return("verified")
  }
  "unknown"
}

tempest_stage_outline_terms <- function(value) {
  terms <- unlist(
    strsplit(tolower(paste(value, collapse = " ")), "[^[:alnum:]]+"),
    use.names = FALSE
  )
  unique(terms[nchar(terms) >= 4L])
}

tempest_stage_outline_evidence_score <- function(section, evidence) {
  subsection_text <- unlist(
    lapply(section$subsections, function(subsection) {
      c(subsection$title, subsection$bullets, subsection$needed)
    }),
    use.names = FALSE
  )
  section_terms <- tempest_stage_outline_terms(c(
    section$title,
    section$summary,
    subsection_text
  ))
  evidence_terms <- tempest_stage_outline_terms(vapply(
    evidence,
    \(claim) claim@claim_text,
    character(1)
  ))
  sum(section_terms %in% evidence_terms)
}

tempest_stage_compact_refined_outline <- function(outline, evidence) {
  sections <- tempest_sections_to_write(outline)
  if (length(sections) == 0L) {
    tempest_stage_governance_abort(
      "A refined outline requires at least one substantive detail section."
    )
  }
  claim_ids <- unique(vapply(
    evidence,
    \(claim) claim@claim_id,
    character(1)
  ))
  limit <- min(length(sections), length(claim_ids))
  if (length(sections) > limit) {
    scores <- vapply(
      sections,
      tempest_stage_outline_evidence_score,
      integer(1),
      evidence = evidence
    )
    keep <- order(-scores, seq_along(sections))[seq_len(limit)]
    sections <- sections[sort(keep)]
  }
  outline$sections <- unname(sections)
  outline
}

tempest_stage_evaluate_evidence_outline <- function(output, context) {
  evaluated <- tempest_stage_evaluate_outline(output, context)
  evidence <- tempest_stage_verified_evidence(context)
  evaluated$output <- tempest_stage_compact_refined_outline(
    evaluated$output,
    evidence
  )
  # An outline is planning metadata rather than a publishable factual product.
  # Its evidence trace remains durable, but it cannot itself assert verification.
  evaluated$support_status <- "unknown"
  evaluated
}

tempest_stage_evaluator_registry <- function() {
  list(
    perspectives = tempest_stage_evaluate_perspectives,
    personas = tempest_stage_evaluate_personas,
    query_decomposition = tempest_stage_evaluate_queries,
    extract_claims = tempest_stage_evaluate_claims,
    verify_claim_support = tempest_stage_evaluate_verification,
    next_question = tempest_stage_evaluate_next_question,
    draft_outline = tempest_stage_evaluate_outline,
    refined_outline = tempest_stage_evaluate_evidence_outline,
    section_writing = \(output, context) {
      tempest_stage_evaluate_briefing_items(
        output,
        context,
        "section_writing"
      )
    },
    lead_section = \(output, context) {
      tempest_stage_evaluate_briefing_items(output, context, "lead_section")
    }
  )
}

tempest_stage_evaluator_resolve <- function(execution) {
  if (!inherits(execution, "tempest_dsprrr_execution")) {
    tempest_stage_evaluator_abort(
      "Stage execution must be resolved from a TempestProgramSet."
    )
  }
  stage <- execution$stage %||% NULL
  if (!rlang::is_string(stage) || !stage %in% tempest_program_set_stages()) {
    tempest_stage_evaluator_abort("Stage execution metadata is incomplete.")
  }
  expected <- tempest_program_set_default_evaluators()[[stage]]
  if (
    !identical(execution$evaluator_id, expected$evaluator_id) ||
      !identical(execution$evaluator_version, expected$evaluator_version)
  ) {
    tempest_stage_evaluator_abort(
      "Stage execution references an unknown builtin evaluator pair."
    )
  }
  evaluator <- tempest_stage_evaluator_registry()[[stage]] %||% NULL
  if (!is.function(evaluator)) {
    tempest_stage_evaluator_abort("Builtin stage evaluator is unavailable.")
  }
  list(
    stage = stage,
    evaluator_id = expected$evaluator_id,
    evaluator_version = expected$evaluator_version,
    evaluate = evaluator
  )
}

tempest_stage_evaluate <- function(execution, output, context = list()) {
  if (!is.list(context)) {
    tempest_stage_evaluator_abort("{.arg context} must be a list.")
  }
  evaluator <- tempest_stage_evaluator_resolve(execution)
  context$program_artifact_id <- execution$program_artifact_id
  context$execution_trace_references <- execution$trace_context
  result <- tryCatch(
    evaluator$evaluate(output, context),
    error = identity
  )
  if (!inherits(result, "condition")) {
    return(result)
  }
  if (
    inherits(result, "tempest_stage_output_validation_error") ||
      inherits(result, "tempest_stage_governance_error")
  ) {
    stop(result)
  }
  tempest_stage_evaluator_abort(
    "Builtin stage evaluator failed its static contract."
  )
}
