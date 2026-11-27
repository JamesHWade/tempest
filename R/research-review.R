# Bounded scientific claim review and one-shot evidence repair.

tempest_research_review_abort <- function(
  message,
  ...,
  class = character(),
  parent = NULL
) {
  tempest_abort(
    message,
    ...,
    class = unique(c(class, "tempest_research_review_error", "tempest_error")),
    parent = parent,
    .envir = rlang::caller_env()
  )
}

tempest_research_review_current <- function(is_current) {
  is.function(is_current) &&
    tryCatch(isTRUE(is_current()), error = function(error) FALSE)
}

tempest_research_review_require_current <- function(is_current) {
  if (!tempest_research_review_current(is_current)) {
    tempest_research_review_abort(
      "Scientific claim review was cancelled before its workspace commit.",
      class = c("tempest_research_review_cancelled", "tempest_async_cancelled")
    )
  }
  invisible(NULL)
}

tempest_research_review_async_try <- function(callback) {
  tryCatch(
    {
      result <- callback()
      if (inherits(result, "promise")) {
        result
      } else {
        promises::promise_resolve(result)
      }
    },
    error = function(error) promises::promise_reject(error)
  )
}

tempest_research_review_async_call <- function(
  module,
  ...,
  chat,
  trace_context
) {
  tryCatch(
    dsprrr::run_async(
      module,
      ...,
      .llm = chat,
      .return_format = "structured",
      .trace_context = trace_context
    ),
    error = function(error) {
      tempest_research_review_abort(
        "A scientific review model request could not be started.",
        class = "tempest_research_review_model_error"
      )
    }
  )
}

tempest_research_review_type_evidence <- function() {
  ellmer::type_array(
    ellmer::type_object(
      source_id = ellmer::type_string(
        "Exact source_id in the supplied dossier."
      ),
      quote = ellmer::type_string(
        "An exact, contiguous verbatim quote from that source's dossier text."
      )
    )
  )
}

tempest_research_review_reviewer_module <- function() {
  evidence <- tempest_research_review_type_evidence()
  output <- ellmer::type_object(
    status = ellmer::type_enum(
      c("no_issue", "potential_issue", "insufficient_evidence"),
      "Whether the dossier reveals a scientific flaw or lacks evidence."
    ),
    finding = ellmer::type_string(
      "A concrete candidate scientific issue, or an empty string."
    ),
    rationale = ellmer::type_string(
      "Why the dossier supports this assessment."
    ),
    evidence = evidence,
    query = ellmer::type_string(
      "One focused evidence search query, or an empty string."
    )
  )
  dsprrr::module(
    dsprrr::signature(
      inputs = list(
        dsprrr::input("claim_text", "string"),
        dsprrr::input("dossier", "string")
      ),
      output_type = output,
      instructions = paste(
        "Review one scientific claim for concrete methodological, logical,",
        "scope, measurement, causal, statistical, or interpretation flaws.",
        "Use only the supplied captured-source dossier. Cite every factual",
        "finding with exact source_id and verbatim contiguous quote pairs.",
        "Use no_issue only to mean that this review found no concrete issue;",
        "it does not establish that the claim is scientifically true.",
        "Use insufficient_evidence when the supplied sources cannot resolve",
        "the claim. Suggest at most one focused query when it could resolve",
        "a specific gap. Do not invent sources or quotes.",
        sep = "\n"
      )
    )
  )
}

tempest_research_review_adjudicator_module <- function() {
  output <- ellmer::type_object(
    verdict = ellmer::type_enum(
      c("no_issue", "confirmed", "dismissed", "unresolved"),
      paste(
        "Whether the review identified a concrete defect, found no issue,",
        "or could not resolve the concern. no_issue does not mean the claim",
        "is true."
      )
    ),
    rationale = ellmer::type_string(
      "Independent reasoning against the dossier and reviewer evidence."
    ),
    evidence = tempest_research_review_type_evidence()
  )
  dsprrr::module(
    dsprrr::signature(
      inputs = list(
        dsprrr::input("claim_text", "string"),
        dsprrr::input("review_packet", "string")
      ),
      output_type = output,
      instructions = paste(
        "Independently adjudicate the proposed issue using only the complete",
        "captured-source dossier and reviewer output in the packet. Check",
        "whether cited quotes actually bear on the claim and whether the",
        "reasoning identifies a concrete scientific defect. Use confirmed",
        "only for a potential_issue reviewer finding supported by its cited",
        "evidence. Use dismissed when a proposed concern is not established,",
        "unresolved when evidence is insufficient, and no_issue only when",
        "the reviewer found no issue. no_issue is not a conclusion that the",
        "claim is true. Say unresolved if the underlying source is unavailable.",
        "Return exact source quotes for any evidence cited.",
        sep = "\n"
      )
    )
  )
}

tempest_research_review_modules <- function() {
  list(
    reviewer = tempest_research_review_reviewer_module(),
    adjudicator = tempest_research_review_adjudicator_module()
  )
}

tempest_research_review_validate_string <- function(
  value,
  field,
  max_chars = 4000L,
  allow_empty = FALSE
) {
  valid <- is.character(value) &&
    !is.object(value) &&
    length(value) == 1L &&
    is.null(attributes(value)) &&
    !is.na(value) &&
    isTRUE(validUTF8(value)) &&
    (allow_empty || nzchar(tempest_trim(value))) &&
    nchar(value, type = "chars") <= max_chars &&
    nchar(value, type = "bytes") <= max_chars * 4L &&
    identical(value, tempest_trim(value)) &&
    !tempest_contract_sensitive_scalar(value)
  if (!valid) {
    tempest_research_review_abort(
      "Structured scientific review field {.field {field}} is malformed.",
      field = field
    )
  }
  value
}

tempest_research_review_exact_output <- function(value, fields, step) {
  if (
    !is.list(value) ||
      is.data.frame(value) ||
      !identical(sort(names(value)), sort(fields)) ||
      anyDuplicated(names(value))
  ) {
    tempest_research_review_abort(
      "The {.val {step}} model returned malformed structured fields."
    )
  }
  value[fields]
}

tempest_research_review_dossier_json <- function(dossier) {
  as.character(jsonlite::toJSON(
    dossier,
    auto_unbox = TRUE,
    null = "null",
    digits = NA,
    dataframe = "rows",
    pretty = FALSE
  ))
}

tempest_research_review_validate_evidence <- function(evidence, dossier, step) {
  if (
    !is.list(evidence) ||
      is.data.frame(evidence) ||
      !is.null(names(evidence)) ||
      length(evidence) > 24L
  ) {
    tempest_research_review_abort(
      "The {.val {step}} model returned malformed evidence citations."
    )
  }
  source_ids <- vapply(dossier, `[[`, character(1), "source_id")
  for (index in seq_along(evidence)) {
    citation <- evidence[[index]]
    if (
      !is.list(citation) ||
        is.data.frame(citation) ||
        !identical(names(citation), c("source_id", "quote"))
    ) {
      tempest_research_review_abort(
        "The {.val {step}} model returned a malformed evidence citation."
      )
    }
    source_id <- tempest_research_review_validate_string(
      citation$source_id,
      paste0(step, ".evidence.source_id"),
      max_chars = 128L
    )
    if (!source_id %in% source_ids) {
      tempest_research_review_abort(
        "The {.val {step}} model cited unknown source id {.val {source_id}}."
      )
    }
    quote <- tempest_research_review_validate_string(
      citation$quote,
      paste0(step, ".evidence.quote"),
      max_chars = 4000L
    )
    source <- dossier[[match(source_id, source_ids)]]
    if (!grepl(quote, source$text, fixed = TRUE)) {
      tempest_research_review_abort(
        "The {.val {step}} model returned a quote that is not verbatim in its cited source."
      )
    }
    evidence[[index]] <- list(source_id = source_id, quote = quote)
  }
  unname(evidence)
}

tempest_research_review_validate_reviewer <- function(output, dossier) {
  fields <- c("status", "finding", "rationale", "evidence", "query")
  output <- tempest_research_review_exact_output(output, fields, "reviewer")
  output$status <- tempest_research_review_validate_string(
    output$status,
    "reviewer.status",
    max_chars = 32L
  )
  if (
    !output$status %in%
      c("no_issue", "potential_issue", "insufficient_evidence")
  ) {
    tempest_research_review_abort(
      "The reviewer returned an unsupported status."
    )
  }
  output$finding <- tempest_research_review_validate_string(
    output$finding,
    "reviewer.finding",
    allow_empty = TRUE
  )
  if (identical(output$status, "potential_issue") && !nzchar(output$finding)) {
    tempest_research_review_abort(
      "A potential scientific issue must include a concrete finding."
    )
  }
  output$rationale <- tempest_research_review_validate_string(
    output$rationale,
    "reviewer.rationale"
  )
  output$evidence <- tempest_research_review_validate_evidence(
    output$evidence,
    dossier,
    "reviewer"
  )
  output$query <- tempest_research_review_validate_string(
    output$query,
    "reviewer.query",
    allow_empty = TRUE
  )
  output
}

tempest_research_review_validate_adjudication <- function(output, dossier) {
  fields <- c("verdict", "rationale", "evidence")
  output <- tempest_research_review_exact_output(
    output,
    fields,
    "adjudicator"
  )
  output$verdict <- tempest_research_review_validate_string(
    output$verdict,
    "adjudication.verdict",
    max_chars = 32L
  )
  if (
    !output$verdict %in% c("no_issue", "confirmed", "dismissed", "unresolved")
  ) {
    tempest_research_review_abort(
      "The adjudicator returned an unsupported verdict."
    )
  }
  output$rationale <- tempest_research_review_validate_string(
    output$rationale,
    "adjudication.rationale"
  )
  output$evidence <- tempest_research_review_validate_evidence(
    output$evidence,
    dossier,
    "adjudicator"
  )
  output
}

tempest_research_review_result <- function(
  result,
  module,
  trace_context,
  step
) {
  if (!inherits(result, "dsprrr_result") || !is.list(result$metadata)) {
    tempest_research_review_abort(
      "The {.val {step}} dsprrr result omitted structured identity metadata."
    )
  }
  expected_id <- dsprrr::program_artifact_id(module)
  metadata <- result$metadata
  if (
    !identical(metadata$program_artifact_id, expected_id) ||
      !identical(metadata$trace_context, trace_context)
  ) {
    tempest_research_review_abort(
      "The {.val {step}} dsprrr metadata does not match its fixed program identity."
    )
  }
  tempest_dsprrr_structured_output(result$output)
}

tempest_research_review_content_candidates <- function(resource) {
  if (!tempest_is_exact_resource(resource)) {
    return(character())
  }
  content <- resource@content
  if (is.null(content)) {
    return(character())
  }
  text <- if (is.character(content) && length(content) == 1L) {
    content
  } else {
    tryCatch(
      tempest_product_canonical_json(content),
      error = function(error) ""
    )
  }
  if (
    !is.character(text) ||
      length(text) != 1L ||
      is.na(text) ||
      !nzchar(tempest_trim(text))
  ) {
    return(character())
  }
  enc2utf8(text)
}

tempest_research_review_excerpt <- function(text, quotes, source_id) {
  limit <- 20000L
  quotes <- unique(quotes[nzchar(quotes)])
  locations <- if (length(quotes) == 0L) {
    integer()
  } else {
    vapply(
      quotes,
      function(quote) {
        as.integer(regexpr(quote, text, fixed = TRUE)[[1]])
      },
      integer(1)
    )
  }
  if (length(locations) > 0L && any(locations < 1L)) {
    tempest_research_review_abort(
      "A required evidence quote is absent from source {.val {source_id}}."
    )
  }
  text_length <- nchar(text, type = "chars")
  if (text_length <= limit) {
    return(text)
  }
  start <- 1L
  if (length(locations) > 0L) {
    quote_ends <- locations + nchar(quotes, type = "chars") - 1L
    lower <- min(locations)
    upper <- max(quote_ends)
    if (upper - lower + 1L > limit) {
      tempest_research_review_abort(
        paste0(
          "Required evidence quotes for source {.val {source_id}} span more",
          " than the bounded dossier excerpt."
        )
      )
    }
    start <- max(1L, min(lower, text_length - limit + 1L))
    if (start + limit - 1L < upper) {
      start <- upper - limit + 1L
    }
    if (start > lower) {
      tempest_research_review_abort(
        "Required evidence quotes could not fit in source {.val {source_id}}."
      )
    }
  }
  excerpt <- substr(text, start, min(text_length, start + limit - 1L))
  if (
    length(quotes) > 0L &&
      any(
        !vapply(
          quotes,
          grepl,
          logical(1),
          x = excerpt,
          fixed = TRUE
        )
      )
  ) {
    tempest_research_review_abort(
      "A required evidence quote was omitted from source {.val {source_id}}."
    )
  }
  excerpt
}

tempest_research_review_source_record <- function(
  resource,
  required = FALSE,
  quotes = character()
) {
  valid_resource <- tempest_is_exact_resource(resource)
  candidates <- if (valid_resource) {
    tempest_research_review_content_candidates(resource)
  } else {
    character()
  }
  hash <- if (valid_resource) resource@content_hash else NA_character_
  has_hash <- is.character(hash) &&
    length(hash) == 1L &&
    !is.na(hash) &&
    grepl("^[a-f0-9]{64}$", hash)
  if (length(candidates) == 0L || !has_hash) {
    if (isTRUE(required)) {
      source_id <- if (valid_resource) resource@resource_id else "unknown"
      tempest_research_review_abort(
        "Required source {.val {source_id}} has no captured text and full content hash."
      )
    }
    return(NULL)
  }
  usable <- candidates[vapply(
    candidates,
    function(text) {
      all(vapply(quotes, grepl, logical(1), x = text, fixed = TRUE))
    },
    logical(1)
  )]
  if (length(usable) == 0L) {
    if (length(quotes) > 0L) {
      tempest_research_review_abort(
        "Required claim evidence is not present in captured source {.val {resource@resource_id}}."
      )
    }
    usable <- candidates[[1L]]
  } else {
    usable <- usable[[1L]]
  }
  text <- tempest_research_review_excerpt(
    usable,
    quotes,
    resource@resource_id
  )
  if (tempest_contract_sensitive_scalar(text)) {
    tempest_research_review_abort(
      "A captured source contains credential-like content and cannot enter a review dossier."
    )
  }
  list(
    source_id = resource@resource_id,
    locator = resource@locator,
    content_hash = hash,
    text = text
  )
}

tempest_research_review_claim_priority <- function(status) {
  priority <- c(
    contradicted = 1L,
    unsupported = 2L,
    partially_supported = 3L,
    supported = 4L,
    unverifiable = 5L,
    unverified = 6L
  )
  value <- unname(priority[status])
  if (length(value) != 1L || is.na(value)) 7L else value
}

tempest_research_review_claim_dossier <- function(
  workspace,
  claim,
  max_sources = 12L,
  reserve_sources = 0L,
  exclude_optional_source_ids = character()
) {
  resources <- workspace$list_retrieved_resources()
  resource_ids <- vapply(
    resources,
    function(resource) {
      resource@resource_id
    },
    character(1)
  )
  names(resources) <- resource_ids
  spans <- workspace$list_evidence_spans()
  linked_span_ids <- claim@evidence_span_ids
  linked_spans <- Filter(
    function(span) span@evidence_span_id %in% linked_span_ids,
    spans
  )
  direct_ids <- unique(c(
    claim@source_ids,
    claim@contradicting_source_ids,
    vapply(linked_spans, function(span) span@source_id, character(1))
  ))
  claims <- workspace$list_proposed_claims()
  disputes <- workspace$list_disputes()
  disputed_claim_ids <- unique(unlist(
    lapply(disputes, function(dispute) {
      if (claim@claim_id %in% dispute@claim_ids) {
        setdiff(dispute@claim_ids, claim@claim_id)
      } else {
        character()
      }
    }),
    use.names = FALSE
  ))
  conflicting_claims <- Filter(
    function(other) other@claim_id %in% disputed_claim_ids,
    claims
  )
  relevant_claims <- c(list(claim), conflicting_claims)
  relevant_span_ids <- unique(unlist(
    lapply(
      relevant_claims,
      function(relevant) relevant@evidence_span_ids
    ),
    use.names = FALSE
  ))
  relevant_spans <- Filter(
    function(span) span@evidence_span_id %in% relevant_span_ids,
    spans
  )
  quotes_by_source <- split(
    vapply(
      relevant_spans,
      function(span) {
        if (is.na(span@quote)) "" else span@quote
      },
      character(1)
    ),
    vapply(relevant_spans, function(span) span@source_id, character(1))
  )
  conflict_ids <- unique(unlist(
    lapply(conflicting_claims, function(other) {
      c(other@source_ids, other@contradicting_source_ids)
    }),
    use.names = FALSE
  ))
  required_ids <- unique(c(direct_ids, conflict_ids))
  missing <- setdiff(required_ids, names(resources))
  if (length(missing) > 0L) {
    tempest_research_review_abort(
      "Claim dossier references missing source id{?s} {.val {missing}}."
    )
  }
  if (length(required_ids) > max_sources) {
    tempest_research_review_abort(
      "Claim dossier requires more than {.val {max_sources}} sources."
    )
  }
  accepted_ids <- names(resources)[vapply(
    resources,
    tempest_is_accepted_knowledge_resource,
    logical(1)
  )]
  required_accepted_ids <- intersect(required_ids, accepted_ids)
  if (length(required_accepted_ids) > 0L) {
    tempest_research_review_abort(
      paste0(
        "Required accepted Graft knowledge cannot enter a scientific review",
        " dossier."
      )
    )
  }
  optional_ids <- setdiff(sort(names(resources)), c(required_ids, accepted_ids))
  optional_ids <- setdiff(optional_ids, exclude_optional_source_ids)
  total_limit <- max(length(required_ids), max_sources - reserve_sources)
  total_limit <- min(max_sources, total_limit)
  optional_limit <- max(0L, total_limit - length(required_ids))
  candidate_ids <- c(required_ids, utils::head(optional_ids, optional_limit))
  records <- lapply(candidate_ids, function(source_id) {
    tempest_research_review_source_record(
      resources[[source_id]],
      required = source_id %in% required_ids,
      quotes = quotes_by_source[[source_id]] %||% character()
    )
  })
  records <- Filter(Negate(is.null), records)
  if (length(records) < length(required_ids)) {
    tempest_research_review_abort(
      "A required claim source could not be retained in its review dossier."
    )
  }
  unname(records)
}

tempest_research_review_record_base_dossier <- function(record) {
  appended <- record$repair$source_ids
  unname(Filter(
    function(source) !source$source_id %in% appended,
    record$dossier
  ))
}

tempest_research_review_existing_resources_match <- function(
  record,
  workspace
) {
  source_ids <- unique(c(
    vapply(record$dossier, `[[`, character(1), "source_id"),
    record$repair$source_ids
  ))
  all(vapply(
    source_ids,
    function(source_id) {
      source <- workspace$get_retrieved_resource(source_id)
      if (is.null(source)) {
        return(FALSE)
      }
      dossier_index <- match(
        source_id,
        vapply(record$dossier, `[[`, character(1), "source_id")
      )
      !is.na(dossier_index) &&
        identical(source@locator, record$dossier[[dossier_index]]$locator) &&
        identical(
          source@content_hash,
          record$dossier[[dossier_index]]$content_hash
        )
    },
    logical(1)
  ))
}

tempest_research_review_existing_for_claim <- function(
  existing,
  claim,
  dossier,
  programs,
  model,
  workspace
) {
  for (review in existing) {
    record <- tempest_claim_review_record(review)
    if (
      identical(record$claim_id, claim@claim_id) &&
        identical(record$claim_text, claim@claim_text) &&
        identical(
          tempest_research_review_record_base_dossier(record),
          dossier
        ) &&
        identical(record$programs, programs) &&
        identical(record$models, list(reviewer = model, adjudicator = model)) &&
        tempest_research_review_existing_resources_match(record, workspace)
    ) {
      return(review)
    }
  }
  NULL
}

tempest_research_review_model_failure <- function(step, error) {
  tempest_research_review_abort(
    "The {.val {step}} scientific review request failed; no claim reviews were committed.",
    class = "tempest_research_review_model_error"
  )
}

tempest_research_review_run_reviewer <- function(
  module,
  claim,
  dossier,
  chat,
  is_current
) {
  tempest_research_review_require_current(is_current)
  trace_context <- list(
    product = "tempest",
    stage = "research_review",
    role = "reviewer",
    claim_id = claim@claim_id
  )
  request <- tempest_research_review_async_call(
    module,
    claim_text = claim@claim_text,
    dossier = tempest_research_review_dossier_json(dossier),
    chat = chat,
    trace_context = trace_context
  )
  promises::then(
    request,
    onFulfilled = function(result) {
      tempest_research_review_require_current(is_current)
      output <- tempest_research_review_result(
        result,
        module,
        trace_context,
        "reviewer"
      )
      tempest_research_review_validate_reviewer(output, dossier)
    },
    onRejected = function(error) {
      if (inherits(error, "tempest_research_review_error")) {
        stop(error)
      }
      tempest_research_review_model_failure("reviewer", error)
    }
  )
}

tempest_research_review_fetch <- function(retriever, url, is_current) {
  tempest_research_review_require_current(is_current)
  result <- tryCatch(
    {
      if (inherits(retriever, "TempestRetriever")) {
        isolated_workspace <- tempest_research_workspace(
          max_sources = retriever$config@max_sources
        )
        isolated_retriever <- tempest_retriever(
          config = retriever$config,
          workspace = isolated_workspace
        )
        isolated_retriever$fetch(url)
      } else {
        retriever$fetch(url)
      }
    },
    error = function(error) {
      tempest_research_review_abort(
        "A bounded evidence fetch failed; no claim reviews were committed.",
        class = "tempest_research_review_retrieval_error"
      )
    }
  )
  tempest_research_review_require_current(is_current)
  result
}

tempest_research_review_repair <- function(
  retriever,
  config,
  reviewer,
  base_dossier,
  workspace,
  is_current,
  source_budget = 2L
) {
  if (!isTRUE(config@repair_evidence) || !nzchar(reviewer$query)) {
    return(list(
      dossier = base_dossier,
      repair = list(
        status = "not_requested",
        query = "",
        source_ids = character()
      ),
      resources = list()
    ))
  }
  tempest_research_review_require_current(is_current)
  query <- reviewer$query
  requested_k <- min(3L, as.integer(config@max_search_results))
  search <- tryCatch(
    tempest_retriever_search(retriever, query, k = requested_k),
    error = function(error) {
      tempest_research_review_abort(
        "A bounded evidence search failed; no claim reviews were committed.",
        class = "tempest_research_review_retrieval_error"
      )
    }
  )
  tempest_research_review_require_current(is_current)
  if (
    !is.data.frame(search) ||
      !all(c("title", "url", "source_id") %in% names(search)) ||
      !is.character(search$url) ||
      !is.character(search$source_id) ||
      anyNA(search$title) ||
      any(!nzchar(tempest_trim(search$title))) ||
      anyNA(search$url) ||
      any(!nzchar(tempest_trim(search$url))) ||
      anyNA(search$source_id) ||
      nrow(search) > requested_k
  ) {
    tempest_research_review_abort(
      "Evidence search returned malformed results.",
      class = "tempest_research_review_retrieval_error"
    )
  }
  expected_ids <- vapply(search$url, tempest_source_id, character(1))
  if (!identical(unname(search$source_id), unname(expected_ids))) {
    tempest_research_review_abort(
      "Evidence search returned source ids that do not match their URLs.",
      class = "tempest_research_review_retrieval_error"
    )
  }
  if (nrow(search) == 0L) {
    return(list(
      dossier = base_dossier,
      repair = list(
        status = "no_results",
        query = query,
        source_ids = character()
      ),
      resources = list()
    ))
  }
  search <- search[!duplicated(search$source_id), , drop = FALSE]
  search_ids <- search$source_id
  results_exceeded <- nrow(search) > 2L
  max_sources <- workspace$max_sources
  existing_resources <- workspace$list_retrieved_resources()
  existing_ids <- vapply(
    existing_resources,
    function(resource) {
      resource@resource_id
    },
    character(1)
  )
  available_sources <- Inf
  if (is.finite(max_sources)) {
    counted <- sum(
      !vapply(
        existing_resources,
        tempest_is_accepted_knowledge_resource,
        logical(1)
      )
    )
    available_sources <- max(0L, as.integer(max_sources) - counted)
  }
  available_sources <- min(available_sources, as.integer(source_budget))
  selected_count <- min(2L, nrow(search))
  selected <- search[seq_len(selected_count), , drop = FALSE]
  additions <- list()
  dossier <- base_dossier
  base_ids <- vapply(base_dossier, `[[`, character(1), "source_id")
  new_sources <- 0L
  budget_exhausted <- isTRUE(results_exceeded)
  for (index in seq_len(nrow(selected))) {
    tempest_research_review_require_current(is_current)
    source_id <- selected$source_id[[index]]
    if (source_id %in% base_ids) {
      next
    }
    if (length(dossier) >= 12L) {
      budget_exhausted <- TRUE
      next
    }
    is_new_source <- !source_id %in% existing_ids
    if (is_new_source && new_sources >= available_sources) {
      budget_exhausted <- TRUE
      next
    }
    resource <- if (source_id %in% existing_ids) {
      existing_resources[[match(source_id, existing_ids)]]
    } else {
      tempest_research_review_fetch(
        retriever,
        selected$url[[index]],
        is_current
      )
    }
    if (
      !tempest_is_exact_resource(resource) ||
        !identical(resource@resource_id, source_id) ||
        !identical(resource@locator, selected$url[[index]])
    ) {
      tempest_research_review_abort(
        "Evidence fetch returned a resource with a mismatched source identity.",
        class = "tempest_research_review_retrieval_error"
      )
    }
    if (tempest_is_accepted_knowledge_resource(resource)) {
      tempest_research_review_abort(
        "Accepted Graft knowledge cannot enter a scientific review dossier."
      )
    }
    source <- tempest_research_review_source_record(resource, required = TRUE)
    if (is_new_source) {
      additions[[length(additions) + 1L]] <- resource
    }
    dossier[[length(dossier) + 1L]] <- source
    if (is_new_source) {
      new_sources <- new_sources + 1L
    }
  }
  status <- if (budget_exhausted) {
    "budget_exhausted"
  } else {
    "completed"
  }
  repair_ids <- setdiff(
    vapply(dossier, `[[`, character(1), "source_id"),
    base_ids
  )
  list(
    dossier = unname(dossier),
    repair = list(status = status, query = query, source_ids = repair_ids),
    resources = Filter(Negate(is.null), additions)
  )
}

tempest_research_review_run_claim <- function(
  claim,
  dossier,
  retriever,
  config,
  modules,
  program_ids,
  model,
  workspace,
  is_current,
  repair_source_budget
) {
  tempfile <- new.env(parent = emptyenv())
  tempfile$resources <- list()
  chat <- tempest_make_chat(
    config,
    "judge",
    system_prompt = paste(
      "You are an independent scientific claim reviewer. Audit the exact",
      "claim against the bounded captured-source dossier and produce the",
      "structured reviewer fields without deciding that unsupported claims",
      "are true. Cite only exact dossier quotations."
    ),
    echo = "none"
  )
  request <- tempest_research_review_run_reviewer(
    modules$reviewer,
    claim,
    dossier,
    chat,
    is_current
  )
  promises::then(request, function(reviewer) {
    tempest_research_review_require_current(is_current)
    repaired <- tempest_research_review_repair(
      retriever,
      config,
      reviewer,
      dossier,
      workspace,
      is_current,
      source_budget = repair_source_budget
    )
    tempfile$resources <- repaired$resources
    repair <- repaired$repair
    packet <- list(
      dossier = repaired$dossier,
      reviewer = reviewer,
      repair = list(
        status = repair$status,
        query = repair$query,
        source_ids = repair$source_ids
      )
    )
    adjudicator_chat <- tempest_make_chat(
      config,
      "judge",
      system_prompt = paste(
        "You are an independent scientific adjudicator. Examine the exact",
        "claim, reviewer output, and all captured source text. Decide whether",
        "a concrete scientific issue is supported, dismissed, or unresolved.",
        "A no_issue verdict means only that no issue was found; it does not",
        "establish truth. Cite exact source quotations."
      ),
      echo = "none"
    )
    trace_context <- list(
      product = "tempest",
      stage = "research_review",
      role = "adjudicator",
      claim_id = claim@claim_id
    )
    tempest_research_review_require_current(is_current)
    adjudication_request <- tempest_research_review_async_call(
      modules$adjudicator,
      claim_text = claim@claim_text,
      review_packet = tempest_research_review_dossier_json(packet),
      chat = adjudicator_chat,
      trace_context = trace_context
    )
    promises::then(
      adjudication_request,
      onFulfilled = function(result) {
        tempest_research_review_require_current(is_current)
        adjudication <- tempest_research_review_validate_adjudication(
          tempest_research_review_result(
            result,
            modules$adjudicator,
            trace_context,
            "adjudicator"
          ),
          repaired$dossier
        )
        if (
          identical(adjudication$verdict, "confirmed") &&
            (!identical(reviewer$status, "potential_issue") ||
              length(adjudication$evidence) == 0L)
        ) {
          tempest_research_review_abort(
            paste0(
              "A confirmed verdict requires a reviewer potential_issue with",
              " cited evidence and an adjudicator evidence citation."
            )
          )
        }
        if (
          identical(adjudication$verdict, "no_issue") &&
            !identical(reviewer$status, "no_issue")
        ) {
          tempest_research_review_abort(
            "A no_issue verdict requires a reviewer no_issue assessment."
          )
        }
        record <- list(
          schema_version = 1L,
          claim_id = claim@claim_id,
          claim_text = claim@claim_text,
          dossier = unname(repaired$dossier),
          reviewer = reviewer,
          adjudication = adjudication,
          repair = repair,
          programs = list(
            reviewer = program_ids$reviewer,
            adjudicator = program_ids$adjudicator
          ),
          models = list(reviewer = model, adjudicator = model)
        )
        record <- tempest_claim_review(record)
        list(record = record, resources = tempfile$resources)
      },
      onRejected = function(error) {
        if (inherits(error, "tempest_research_review_error")) {
          stop(error)
        }
        tempest_research_review_model_failure("adjudicator", error)
      }
    )
  })
}

tempest_research_review_commit <- function(
  workspace,
  records,
  resources,
  is_current,
  verification_owner_token
) {
  tempest_research_review_require_current(is_current)
  resources_by_id <- list()
  for (resource in resources) {
    resource_id <- resource@resource_id
    previous <- resources_by_id[[resource_id]] %||% NULL
    if (
      !is.null(previous) &&
        !identical(previous@content_hash, resource@content_hash)
    ) {
      tempest_research_review_abort(
        "Concurrent evidence repairs returned conflicting content for one source id."
      )
    }
    resources_by_id[[resource_id]] <- resource
  }
  current_resources <- workspace$list_retrieved_resources()
  current_ids <- vapply(
    current_resources,
    function(resource) {
      resource@resource_id
    },
    character(1)
  )
  current_by_id <- stats::setNames(current_resources, current_ids)
  for (resource_id in intersect(names(resources_by_id), current_ids)) {
    existing <- current_by_id[[resource_id]] %||% NULL
    if (
      !identical(existing@locator, resources_by_id[[resource_id]]@locator) ||
        !identical(
          existing@content_hash,
          resources_by_id[[resource_id]]@content_hash
        )
    ) {
      tempest_research_review_abort(
        "Evidence repair cannot replace a captured source with changed content."
      )
    }
    resources_by_id[[resource_id]] <- NULL
  }
  tempest_research_review_require_current(is_current)
  workspace$replace_claim_reviews(
    records,
    .verification_owner_token = verification_owner_token,
    .resources = unname(resources_by_id[sort(names(resources_by_id))])
  )
  records
}

tempest_research_review_async <- function(
  workspace,
  retriever,
  config,
  is_current = function() TRUE,
  .verification_owner_token = NULL
) {
  tempest_require("promises", "Scientific claim review requires promises.")
  if (!inherits(workspace, "ResearchWorkspace")) {
    tempest_research_review_abort(
      "{.arg workspace} must be a ResearchWorkspace."
    )
  }
  if (!S7::S7_inherits(config, TempestConfig)) {
    tempest_research_review_abort(
      "{.arg config} must be created by {.fn tempest_config}."
    )
  }
  existing <- workspace$list_claim_reviews()
  if (!isTRUE(config@research_review)) {
    return(promises::promise_resolve(existing))
  }
  tempest_research_review_require_current(is_current)
  claims <- workspace$list_proposed_claims()
  if (length(claims) == 0L) {
    if (length(existing) == 0L) {
      return(promises::promise_resolve(existing))
    }
    return(tempest_research_review_async_try(function() {
      tempest_research_review_commit(
        workspace,
        list(),
        list(),
        is_current,
        .verification_owner_token
      )
    }))
  }
  modules <- tempest_research_review_modules()
  program_ids <- lapply(modules, dsprrr::program_artifact_id)
  model <- tempest_research_model(config, "judge")
  programs <- list(
    reviewer = program_ids$reviewer,
    adjudicator = program_ids$adjudicator
  )
  if (config@max_review_claims == 0L) {
    return(promises::promise_resolve(existing))
  }
  priority <- vapply(
    claims,
    function(claim) {
      tempest_research_review_claim_priority(
        claim@verification_status
      )
    },
    integer(1)
  )
  claim_ids <- vapply(claims, function(claim) claim@claim_id, character(1))
  claims <- claims[order(priority, claim_ids)]
  claims <- utils::head(claims, as.integer(config@max_review_claims))
  dossiers <- lapply(claims, function(claim) {
    previous <- Filter(
      function(review) {
        tempest_claim_review_record(review)$claim_id == claim@claim_id
      },
      existing
    )
    prior_repair_ids <- unique(unlist(
      lapply(previous, function(review) {
        tempest_claim_review_record(review)$repair$source_ids
      }),
      use.names = FALSE
    ))
    tempest_research_review_claim_dossier(
      workspace,
      claim,
      max_sources = 12L,
      reserve_sources = if (isTRUE(config@repair_evidence)) 2L else 0L,
      exclude_optional_source_ids = prior_repair_ids
    )
  })
  keep_existing <- lapply(seq_along(claims), function(index) {
    tempest_research_review_existing_for_claim(
      existing,
      claims[[index]],
      dossiers[[index]],
      programs,
      model,
      workspace
    )
  })
  reuse <- !vapply(keep_existing, is.null, logical(1))
  work_indices <- which(!reuse)
  if (length(work_indices) == 0L) {
    selected_records <- unname(keep_existing)
    current_ids <- sort(vapply(
      existing,
      function(review) tempest_claim_review_record(review)$review_id,
      character(1)
    ))
    selected_ids <- sort(vapply(
      selected_records,
      function(review) tempest_claim_review_record(review)$review_id,
      character(1)
    ))
    if (identical(current_ids, selected_ids)) {
      return(promises::promise_resolve(existing))
    }
    return(tempest_research_review_async_try(function() {
      tempest_research_review_commit(
        workspace,
        selected_records,
        list(),
        is_current,
        .verification_owner_token
      )
    }))
  }
  repair_budgets <- rep(2L, length(claims))
  if (isTRUE(config@repair_evidence)) {
    all_resources <- workspace$list_retrieved_resources()
    max_sources <- workspace$max_sources
    remaining_sources <- if (is.finite(max_sources)) {
      counted <- sum(
        !vapply(
          all_resources,
          tempest_is_accepted_knowledge_resource,
          logical(1)
        )
      )
      max(0L, as.integer(max_sources) - counted)
    } else {
      Inf
    }
    for (index in work_indices) {
      dossier_slots <- max(0L, 12L - length(dossiers[[index]]))
      repair_budgets[[index]] <- as.integer(min(
        2L,
        dossier_slots,
        remaining_sources
      ))
      if (is.finite(remaining_sources)) {
        remaining_sources <- remaining_sources - repair_budgets[[index]]
      }
    }
  }
  external_current <- is_current
  lifecycle <- new.env(parent = emptyenv())
  lifecycle$active <- TRUE
  is_current <- function() {
    isTRUE(lifecycle$active) &&
      tempest_research_review_current(external_current)
  }
  if (!tempest_retriever_compatible(retriever)) {
    tempest_research_review_abort(
      "{.arg retriever} must expose bounded search and fetch methods."
    )
  }
  concurrency <- as.integer(config@review_concurrency)
  results <- vector("list", length(work_indices))
  run_wave <- function(offset) {
    tempest_research_review_require_current(is_current)
    indices <- seq.int(
      offset,
      min(length(work_indices), offset + concurrency - 1L)
    )
    requests <- lapply(indices, function(result_index) {
      item_index <- work_indices[[result_index]]
      tempest_research_review_async_try(function() {
        tempest_research_review_run_claim(
          claims[[item_index]],
          dossiers[[item_index]],
          retriever,
          config,
          modules,
          programs,
          model,
          workspace,
          is_current,
          repair_budgets[[item_index]]
        )
      })
    })
    promises::then(promises::promise_all(.list = requests), function(values) {
      tempest_research_review_require_current(is_current)
      for (index in seq_along(indices)) {
        results[[indices[[index]]]] <<- values[[index]]
      }
      if (max(indices) < length(work_indices)) {
        return(run_wave(max(indices) + 1L))
      }
      new_results <- unname(results)
      new_records <- lapply(new_results, `[[`, "record")
      new_resources <- unlist(
        lapply(new_results, `[[`, "resources"),
        recursive = FALSE
      )
      final_records <- lapply(seq_along(claims), function(index) {
        if (reuse[[index]]) {
          keep_existing[[index]]
        } else {
          new_records[[match(index, work_indices)]]
        }
      })
      tempest_research_review_commit(
        workspace,
        final_records,
        new_resources,
        is_current,
        .verification_owner_token
      )
    })
  }
  request <- run_wave(1L)
  promises::then(
    request,
    onFulfilled = function(value) {
      lifecycle$active <- FALSE
      value
    },
    onRejected = function(error) {
      lifecycle$active <- FALSE
      stop(error)
    }
  )
}

tempest_research_review_await <- function(
  promise,
  timeout_s = getOption("tempest.research_review_sync_timeout_s", 120)
) {
  tempest_require("later", "Synchronous scientific review requires later.")
  if (
    !is.numeric(timeout_s) ||
      length(timeout_s) != 1L ||
      is.na(timeout_s) ||
      !is.finite(timeout_s) ||
      timeout_s <= 0
  ) {
    tempest_research_review_abort(
      "The synchronous scientific review timeout must be positive and finite."
    )
  }
  tempest_await_promise(
    promise,
    timeout_s = timeout_s,
    on_timeout = function() {
      tempest_research_review_abort(
        "Scientific claim review did not settle before the synchronous timeout."
      )
    }
  )
}

tempest_research_review <- function(
  workspace,
  retriever,
  config,
  is_current = function() TRUE,
  .verification_owner_token = NULL
) {
  active <- new.env(parent = emptyenv())
  active$value <- TRUE
  is_current_guard <- function() {
    isTRUE(active$value) && tempest_research_review_current(is_current)
  }
  on.exit(active$value <- FALSE, add = TRUE)
  request <- tempest_research_review_async(
    workspace,
    retriever,
    config,
    is_current = is_current_guard,
    .verification_owner_token = .verification_owner_token
  )
  tempest_research_review_await(request)
}
