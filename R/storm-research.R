# STORM research stage

tempest_storm_deputy_session_id <- function(research_run_id, expert_id) {
  paste0(
    "tempest-storm-expert-",
    substr(
      digest::digest(
        tempest_research_manifest_canonical_json(list(
          product = "tempest",
          research_run_id = research_run_id,
          expert_id = expert_id,
          role = "expert"
        )),
        algo = "sha256",
        serialize = FALSE
      ),
      1L,
      24L
    )
  )
}

tempest_storm_deputy_trace <- function(trace, expert_id = NULL) {
  canonical <- tryCatch(
    tempest_research_manifest_traces(list(trace))[[1L]],
    error = function(error) {
      tempest_stage_governance_abort(
        "A STORM Deputy trace does not match the manifest contract."
      )
    }
  )
  required <- c(
    "agent_id",
    "correlation_id",
    "deputy_run_id",
    "deputy_session_id",
    "expert_id",
    "role",
    "stage",
    "status",
    "trace_id",
    "trace_type"
  )
  if (
    !all(required %in% names(canonical)) ||
      !identical(canonical$trace_type, "deputy_run") ||
      !identical(canonical$trace_id, canonical$deputy_run_id) ||
      !identical(canonical$stage, "research") ||
      !identical(canonical$role, "expert") ||
      (!is.null(expert_id) && !identical(canonical$expert_id, expert_id))
  ) {
    tempest_stage_governance_abort(
      "A STORM expert answer must identify one exact Deputy research run."
    )
  }
  canonical
}

tempest_storm_manifest_add_deputy_trace <- function(manifest, trace) {
  trace <- tempest_storm_deputy_trace(trace)
  existing <- manifest@traces %||% list()
  trace_ids <- vapply(
    existing,
    \(item) item$trace_id %||% NA_character_,
    character(1)
  )
  if (trace$trace_id %in% trace_ids) {
    tempest_stage_governance_abort(
      "A STORM Deputy run cannot be recorded more than once."
    )
  }
  types <- vapply(
    existing,
    \(item) item$trace_type %||% NA_character_,
    character(1)
  )
  if (anyNA(types) || any(!types %in% c("stage_attempt", "deputy_run"))) {
    tempest_stage_governance_abort(
      "The STORM manifest contains an unknown execution trace."
    )
  }
  stages <- existing[types == "stage_attempt"]
  deputy <- c(existing[types == "deputy_run"], list(trace))
  deputy_ids <- vapply(deputy, `[[`, character(1), "trace_id")
  deputy <- deputy[order(deputy_ids, method = "radix")]
  traces <- c(stages, deputy)
  runtime <- list(
    deputy_run_ids = as.list(sort(
      unique(vapply(
        deputy,
        `[[`,
        character(1),
        "deputy_run_id"
      )),
      method = "radix"
    )),
    deputy_session_ids = as.list(sort(
      unique(vapply(
        deputy,
        `[[`,
        character(1),
        "deputy_session_id"
      )),
      method = "radix"
    ))
  )
  tempest_research_manifest_update(
    manifest,
    runtime = runtime,
    traces = traces
  )
}

tempest_storm_completion_answer <- function(completion) {
  turn <- completion$provider_turn
  response <- completion$response
  if (is.null(turn) || !rlang::is_string(response)) {
    tempest_stage_governance_abort(
      "A claimed STORM completion must retain its exact provider turn."
    )
  }
  answer_text <- tryCatch(
    ellmer::contents_markdown(turn),
    error = function(error) {
      tempest_stage_governance_abort(
        "A claimed STORM completion contains an invalid provider turn."
      )
    }
  )
  if (!identical(answer_text, response)) {
    tempest_stage_governance_abort(
      "A claimed STORM response does not match its exact provider turn."
    )
  }
  list(answer_text = response, provider_turn = turn)
}
tempest_generate_next_question <- function(
  writer_chat,
  topic,
  perspective,
  answered_md,
  facts_md,
  module,
  knowledge_view = module$knowledge_view %||% NULL,
  record_stage = function(record, output = NULL) invisible(record)
) {
  stage_result <- tempest_execute_stage(
    module,
    writer_chat,
    inputs = list(
      topic = topic,
      perspective = paste(
        perspective$name %||% "",
        perspective$description %||% "",
        sep = "\n"
      ),
      answered = answered_md,
      facts = facts_md
    ),
    context = tempest_stage_context_knowledge_view(
      list(),
      module,
      knowledge_view
    ),
    record_stage = record_stage
  )
  stage_result$output
}

#' @keywords internal
tempest_normalize_next_question <- function(output) {
  if (!is.list(output) || is.data.frame(output)) {
    tempest_abort(
      "Next-question stage output must be a record.",
      class = "tempest_stage_output_error"
    )
  }
  question <- tempest_stage_string(output$question, "question")
  done <- output$done %||% FALSE
  if (!is.logical(done) || length(done) != 1L || is.na(done)) {
    tempest_abort(
      "Next-question stage output field {.field done} must be `TRUE` or `FALSE`.",
      class = "tempest_stage_output_error"
    )
  }
  list(question = question, done = done)
}

#' Decompose a research question into targeted search queries
#'
#' @param chat An ellmer chat object.
#' @param question The research question to decompose.
#' @param topic The overall research topic.
#' @param module Optional dsprrr module for query decomposition.
#' @param max_queries Maximum number of queries to return.
#' @return A list with a `queries` character vector.
#' @keywords internal
tempest_decompose_query <- function(
  chat,
  question,
  topic,
  module,
  max_queries = 3,
  knowledge_view = module$knowledge_view %||% NULL,
  record_stage = function(record, output = NULL) invisible(record)
) {
  stage_result <- tempest_execute_stage(
    module,
    chat,
    inputs = list(question = question, topic = topic),
    context = tempest_stage_context_knowledge_view(
      list(max_queries = as.integer(max_queries)),
      module,
      knowledge_view
    ),
    record_stage = record_stage
  )
  stage_result$output
}

#' @keywords internal
tempest_normalize_query_decomposition <- function(
  x,
  max_queries = 4
) {
  queries <- if (is.list(x) && !is.null(x$queries)) x$queries else x
  queries <- tempest_stage_string_array(queries, "queries")
  max_queries <- as.integer(max_queries %||% 4L)
  if (is.na(max_queries) || max_queries < 1) {
    tempest_abort(
      "{.arg max_queries} must be a positive whole number.",
      class = "tempest_config_error"
    )
  }
  list(queries = queries[seq_len(min(length(queries), max_queries))])
}

#' @keywords internal
tempest_normalize_optional_score <- function(x) {
  if (is.null(x)) {
    return(NA_real_)
  }
  if (
    !is.numeric(x) ||
      length(x) != 1L ||
      (!is.na(x) && (!is.finite(x) || x < 0 || x > 1))
  ) {
    tempest_abort(
      "Claim-extraction support scores must be `NA` or finite values in [0, 1].",
      class = "tempest_stage_output_error"
    )
  }
  as.double(x)
}

#' @keywords internal
tempest_normalize_fact_output <- function(x) {
  if (!is.list(x) || is.data.frame(x) || is.null(x$facts)) {
    tempest_abort(
      "Claim-extraction stage output must contain a {.field facts} list.",
      class = "tempest_stage_output_error"
    )
  }
  facts <- x$facts
  if (!is.list(facts) || is.data.frame(facts) || !is.null(names(facts))) {
    tempest_abort(
      "Claim-extraction stage field {.field facts} must be a list.",
      class = "tempest_stage_output_error"
    )
  }
  purrr::map(facts, function(f) {
    if (!is.list(f) || is.data.frame(f)) {
      tempest_abort(
        "Claim-extraction fact entries must be records.",
        class = "tempest_stage_output_error"
      )
    }
    claim <- tempest_stage_string(f$claim, "claim")
    sources <- f$sources
    if (
      !is.list(sources) ||
        is.data.frame(sources) ||
        !is.null(names(sources)) ||
        length(sources) == 0L
    ) {
      tempest_abort(
        "Claim-extraction facts require a non-empty {.field sources} list.",
        class = "tempest_stage_output_error"
      )
    }
    confidence <- f$confidence %||% "medium"
    if (
      !rlang::is_string(confidence) ||
        !confidence %in% c("low", "medium", "high")
    ) {
      tempest_abort(
        paste0(
          "Claim-extraction confidence must be one of ",
          "{.val {c('low', 'medium', 'high')}}."
        ),
        class = "tempest_stage_output_error"
      )
    }
    note <- f$note %||% NA_character_
    if (
      !is.character(note) ||
        is.object(note) ||
        !is.null(names(note)) ||
        length(note) != 1L
    ) {
      tempest_abort(
        "Claim-extraction note must be a single string or `NA`.",
        class = "tempest_stage_output_error"
      )
    }
    list(
      claim = claim,
      sources = sources,
      confidence = confidence,
      support_score = tempest_normalize_optional_score(
        f$support_score %||% f$score
      ),
      note = note
    )
  })
}

#' @keywords internal
tempest_answer_source_context <- function(
  answer_text,
  store,
  source_ids = NULL
) {
  sources <- store$list_retrieved_sources()
  if (length(sources) == 0 || is.null(answer_text) || !nzchar(answer_text)) {
    return("")
  }
  source_ids <- unique(source_ids[!is.na(source_ids) & nzchar(source_ids)])
  cited <- purrr::keep(sources, function(source) {
    source_id <- source$id %||% ""
    url <- source$url %||% ""
    source_id %in%
      source_ids ||
      (!is.na(source_id) &&
        nzchar(source_id) &&
        grepl(source_id, answer_text, fixed = TRUE)) ||
      (!is.na(url) && nzchar(url) && grepl(url, answer_text, fixed = TRUE))
  })
  if (length(cited) == 0) {
    return("")
  }
  lines <- purrr::map_chr(cited, function(source) {
    title <- source$title %||% ""
    if (is.na(title)) {
      title <- ""
    }
    paste0(
      "- [",
      source$id,
      "] ",
      title,
      " ",
      source$url %||% ""
    )
  })
  paste(
    "Known source IDs for citations found in the answer:",
    paste(lines, collapse = "\n"),
    sep = "\n"
  )
}

#' @keywords internal
tempest_claim_extraction_citation_mode <- function(
  answer_text,
  source_ids = NULL,
  source_context = ""
) {
  source_ids <- unique(source_ids[!is.na(source_ids) & nzchar(source_ids)])
  has_native <- length(source_ids) > 0
  has_inline <- length(tempest_extract_citation_ids(answer_text)) > 0
  has_url <- nzchar(source_context) &&
    grepl("https?://", answer_text %||% "", ignore.case = TRUE)

  n_modes <- sum(c(has_native, has_inline, has_url))
  if (n_modes > 1) {
    return("mixed")
  }
  if (has_native) {
    return("provider_native")
  }
  if (has_url) {
    return("url")
  }
  "tempest_inline"
}

#' @keywords internal
tempest_claim_extraction_inputs <- function(answer_text, store, source_ids) {
  source_context <- tempest_answer_source_context(
    answer_text,
    store,
    source_ids = source_ids
  )
  list(
    answer_text = answer_text,
    source_context = source_context,
    source_ids = paste(source_ids, collapse = "\n"),
    citation_mode = tempest_claim_extraction_citation_mode(
      answer_text,
      source_ids = source_ids,
      source_context = source_context
    )
  )
}

#' @keywords internal
tempest_extracted_evidence_spans <- function(claims) {
  spans <- attr(claims, "tempest_evidence_spans", exact = TRUE) %||% list()
  if (
    !is.list(spans) ||
      is.data.frame(spans) ||
      !is.null(names(spans)) ||
      !all(vapply(
        spans,
        \(span) S7::S7_inherits(span, tempest_evidence_span),
        logical(1)
      ))
  ) {
    tempest_stage_evaluator_abort(
      "Claim evaluator returned malformed evidence-span lineage."
    )
  }
  spans
}

#' @keywords internal
tempest_extract_claims_execution_bind <- function(
  module,
  session_id,
  expert_id,
  retrieval_step_id,
  perspective_id,
  section_id,
  deputy_run_id = NA_character_,
  deputy_session_id = NA_character_,
  parent_run_id = NA_character_,
  delegation_id = NA_character_,
  tool_call_id = NA_character_
) {
  module <- tempest_dsprrr_execution_verify(module, "extract_claims")
  claim_context <- list(
    claim_type = "finding",
    session_id = session_id,
    expert_id = expert_id,
    retrieval_step_id = retrieval_step_id,
    perspective_id = perspective_id,
    section_id = section_id
  )
  identifier_fields <- setdiff(names(claim_context), "claim_type")
  for (field in identifier_fields) {
    value <- claim_context[[field]]
    if (
      !is.character(value) ||
        length(value) != 1L ||
        is.object(value) ||
        !is.null(names(value)) ||
        (!is.na(value) && !tempest_opaque_identifier_valid(value))
    ) {
      tempest_stage_governance_abort(
        "Claim context field {.field {field}} is not a safe opaque identifier."
      )
    }
  }
  deputy_context <- list(
    deputy_run_id = deputy_run_id,
    deputy_session_id = deputy_session_id
  )
  delegation_context <- list(
    parent_run_id = parent_run_id,
    delegation_id = delegation_id,
    tool_call_id = tool_call_id
  )
  execution_context <- c(deputy_context, delegation_context)
  for (field in names(execution_context)) {
    value <- execution_context[[field]]
    if (
      !is.character(value) ||
        length(value) != 1L ||
        is.object(value) ||
        !is.null(names(value)) ||
        (!is.na(value) && !tempest_opaque_identifier_valid(value))
    ) {
      tempest_stage_governance_abort(
        "Claim execution field {.field {field}} is not a safe opaque identifier."
      )
    }
  }
  if (xor(is.na(deputy_run_id), is.na(deputy_session_id))) {
    tempest_stage_governance_abort(
      "Claim execution must bind Deputy run and session identifiers together."
    )
  }
  delegation_present <- !vapply(
    delegation_context,
    is.na,
    logical(1)
  )
  if (any(delegation_present) && !all(delegation_present)) {
    tempest_stage_governance_abort(
      paste0(
        "Delegated claim execution must bind parent run, delegation, and ",
        "tool-call identifiers together."
      )
    )
  }
  if (all(delegation_present) && is.na(deputy_run_id)) {
    tempest_stage_governance_abort(
      "Delegated claim execution requires a Deputy run and session pair."
    )
  }
  if (
    all(delegation_present) &&
      identical(parent_run_id, deputy_run_id)
  ) {
    tempest_stage_governance_abort(
      "A delegated claim execution cannot be its own parent run."
    )
  }
  trace <- module$trace_context
  if (!is.na(session_id)) {
    if (!identical(trace$research_run_id %||% NULL, session_id)) {
      tempest_stage_governance_abort(
        paste0(
          "Claim session identity must match the ProgramSet research-run ",
          "trace before provider execution."
        )
      )
    }
  }
  dynamic_bindings <- c(
    expert_id = "expert_id",
    retrieval_step_id = "correlation_id"
  )
  for (claim_field in names(dynamic_bindings)) {
    value <- claim_context[[claim_field]]
    if (is.na(value)) {
      next
    }
    trace_field <- dynamic_bindings[[claim_field]]
    existing <- trace[[trace_field]] %||% NULL
    if (!is.null(existing) && !identical(existing, value)) {
      tempest_stage_governance_abort(
        "Claim context identity conflicts with the bound ProgramSet trace."
      )
    }
    trace[[trace_field]] <- value
  }
  module$trace_context <- tempest_research_manifest_canonical_value(
    trace,
    "trace_context"
  )
  deputy_execution <- if (is.na(deputy_run_id)) {
    NULL
  } else if (all(delegation_present)) {
    c(deputy_context, delegation_context)
  } else {
    deputy_context
  }
  list(
    module = module,
    claim_context = claim_context,
    deputy_execution = deputy_execution
  )
}

#' @keywords internal
tempest_extract_facts_from_answer <- function(
  chat,
  answer_text,
  store,
  module,
  source_ids = NULL,
  session_id = NA_character_,
  expert_id = NA_character_,
  retrieval_step_id = NA_character_,
  perspective_id = NA_character_,
  section_id = NA_character_,
  deputy_run_id = NA_character_,
  deputy_session_id = NA_character_,
  parent_run_id = NA_character_,
  delegation_id = NA_character_,
  tool_call_id = NA_character_,
  knowledge_view = module$knowledge_view %||% NULL,
  record_stage = function(record, output = NULL) invisible(record)
) {
  record_stage_callback <- record_stage %||% tempest_stage_record_discard
  binding <- tempest_extract_claims_execution_bind(
    module = module,
    session_id = session_id,
    expert_id = expert_id,
    retrieval_step_id = retrieval_step_id,
    perspective_id = perspective_id,
    section_id = section_id,
    deputy_run_id = deputy_run_id,
    deputy_session_id = deputy_session_id,
    parent_run_id = parent_run_id,
    delegation_id = delegation_id,
    tool_call_id = tool_call_id
  )
  module <- binding$module
  # Use a separate extraction call to minimize hallucinated facts. dsprrr gets
  # the same source context as the fallback prompt so native-provider citations
  # can still use the optimized module path when available.
  source_ids <- unique(source_ids[!is.na(source_ids) & nzchar(source_ids)])
  extraction_inputs <- tempest_claim_extraction_inputs(
    answer_text,
    store,
    source_ids
  )
  stage_result <- tempest_execute_stage(
    module,
    chat,
    inputs = extraction_inputs,
    context = tempest_stage_context_knowledge_view(
      list(
        workspace = store,
        known_source_ids = vapply(
          store$list_retrieved_sources(),
          `[[`,
          character(1),
          "id"
        ),
        claim_context = binding$claim_context,
        deputy_execution = binding$deputy_execution
      ),
      module,
      knowledge_view
    ),
    output_reference = function(output, running_record, context) {
      tempest_stage_output_reference(
        "workspace_claims",
        vapply(output, \(claim) claim@claim_id, character(1)),
        content_digest = tempest_stage_claims_output_digest(
          output,
          running_record
        )
      )
    },
    record_stage = function(record, output = NULL) {
      if (is.null(output)) {
        return(record_stage_callback(record))
      }
      store$add_extracted_claim_batch(
        claims = output,
        evidence_spans = tempest_extracted_evidence_spans(output),
        commit = function() record_stage_callback(record, output)
      )
      invisible(record)
    }
  )
  invisible(stage_result)
}

#' @keywords internal
tempest_extract_facts_from_answer_async <- function(
  chat,
  answer_text,
  store,
  module,
  source_ids = NULL,
  session_id = NA_character_,
  expert_id = NA_character_,
  retrieval_step_id = NA_character_,
  perspective_id = NA_character_,
  section_id = NA_character_,
  deputy_run_id = NA_character_,
  deputy_session_id = NA_character_,
  parent_run_id = NA_character_,
  delegation_id = NA_character_,
  tool_call_id = NA_character_,
  knowledge_view = module$knowledge_view %||% NULL,
  commit_if = function() TRUE,
  record_stage = function(record, output = NULL) invisible(record)
) {
  tempest_require("promises", "Async fact extraction requires promises.")
  record_stage_callback <- record_stage %||% tempest_stage_record_discard
  binding <- tempest_extract_claims_execution_bind(
    module = module,
    session_id = session_id,
    expert_id = expert_id,
    retrieval_step_id = retrieval_step_id,
    perspective_id = perspective_id,
    section_id = section_id,
    deputy_run_id = deputy_run_id,
    deputy_session_id = deputy_session_id,
    parent_run_id = parent_run_id,
    delegation_id = delegation_id,
    tool_call_id = tool_call_id
  )
  module <- binding$module
  source_ids <- unique(source_ids[!is.na(source_ids) & nzchar(source_ids)])
  inputs <- tempest_claim_extraction_inputs(answer_text, store, source_ids)
  request <- tempest_execute_stage_async(
    module,
    chat,
    inputs = inputs,
    context = tempest_stage_context_knowledge_view(
      list(
        workspace = store,
        known_source_ids = vapply(
          store$list_retrieved_sources(),
          `[[`,
          character(1),
          "id"
        ),
        claim_context = binding$claim_context,
        deputy_execution = binding$deputy_execution
      ),
      module,
      knowledge_view
    ),
    output_reference = function(output, running_record, context) {
      tempest_stage_output_reference(
        "workspace_claims",
        vapply(output, \(claim) claim@claim_id, character(1)),
        content_digest = tempest_stage_claims_output_digest(
          output,
          running_record
        )
      )
    },
    is_current = commit_if,
    record_stage = function(record, output = NULL) {
      if (is.null(output)) {
        return(record_stage_callback(record))
      }
      if (!tempest_async_is_current(commit_if)) {
        return(invisible(record))
      }
      store$add_extracted_claim_batch(
        claims = output,
        evidence_spans = tempest_extracted_evidence_spans(output),
        commit = function() record_stage_callback(record, output)
      )
      invisible(record)
    }
  )
  promises::then(request, function(stage_result) invisible(stage_result))
}

#' @keywords internal
