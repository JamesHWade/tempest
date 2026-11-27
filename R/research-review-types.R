# Internal S7 value records for claim-review evidence.

tempest_claim_review_record_fields <- function() {
  c(
    "schema_version",
    "review_id",
    "claim_id",
    "claim_text",
    "dossier",
    "reviewer",
    "adjudication",
    "repair",
    "programs",
    "models"
  )
}

tempest_claim_review_payload_fields <- function() {
  setdiff(tempest_claim_review_record_fields(), "review_id")
}

tempest_claim_review_abort <- function(message, parent = NULL) {
  tempest_abort(
    message,
    class = c("tempest_claim_review_error", "tempest_error"),
    parent = parent,
    .envir = rlang::caller_env()
  )
}

tempest_claim_review_plain_scalar <- function(
  value,
  max_chars,
  allow_empty = FALSE
) {
  if (
    !is.character(value) ||
      is.object(value) ||
      length(value) != 1L ||
      !is.null(attributes(value)) ||
      is.na(value) ||
      !isTRUE(validUTF8(value)) ||
      (!isTRUE(allow_empty) && !nzchar(tempest_trim(value))) ||
      nchar(value, type = "chars") > max_chars ||
      nchar(value, type = "bytes") > max_chars * 4L
  ) {
    return(FALSE)
  }
  TRUE
}

tempest_claim_review_record_list <- function(value, fields, path) {
  if (
    !is.list(value) ||
      is.data.frame(value) ||
      is.null(names(value)) ||
      anyNA(names(value)) ||
      anyDuplicated(names(value)) ||
      length(value) != length(fields) ||
      !setequal(names(value), fields)
  ) {
    return(paste0(path, " must use the exact current fields"))
  }
  NULL
}

tempest_claim_review_reorder_list <- function(value, fields, path) {
  issue <- tempest_claim_review_record_list(value, fields, path)
  if (!is.null(issue)) {
    tempest_claim_review_abort(issue)
  }
  value[fields]
}

tempest_claim_review_normalize_record <- function(record, include_id = TRUE) {
  fields <- if (isTRUE(include_id)) {
    tempest_claim_review_record_fields()
  } else {
    tempest_claim_review_payload_fields()
  }
  record <- tempest_claim_review_reorder_list(
    record,
    fields,
    "A claim-review record"
  )
  record$dossier <- lapply(record$dossier, function(source) {
    tempest_claim_review_reorder_list(
      source,
      c("source_id", "locator", "content_hash", "text"),
      "dossier sources"
    )
  })
  record$reviewer <- tempest_claim_review_reorder_list(
    record$reviewer,
    c("status", "finding", "rationale", "evidence", "query"),
    "reviewer"
  )
  record$reviewer$evidence <- lapply(record$reviewer$evidence, function(item) {
    tempest_claim_review_reorder_list(
      item,
      c("source_id", "quote"),
      "reviewer evidence entries"
    )
  })
  record$adjudication <- tempest_claim_review_reorder_list(
    record$adjudication,
    c("verdict", "rationale", "evidence"),
    "adjudication"
  )
  record$adjudication$evidence <- lapply(
    record$adjudication$evidence,
    function(item) {
      tempest_claim_review_reorder_list(
        item,
        c("source_id", "quote"),
        "adjudication evidence entries"
      )
    }
  )
  record$repair <- tempest_claim_review_reorder_list(
    record$repair,
    c("status", "query", "source_ids"),
    "repair"
  )
  record$programs <- tempest_claim_review_reorder_list(
    record$programs,
    c("reviewer", "adjudicator"),
    "programs"
  )
  record$models <- tempest_claim_review_reorder_list(
    record$models,
    c("reviewer", "adjudicator"),
    "models"
  )
  record
}

tempest_dossier_validate_evidence <- function(
  evidence,
  source_ids,
  source_texts,
  validate_entry,
  on_list_error,
  on_unknown_source,
  on_missing_quote,
  check_duplicates = FALSE,
  on_duplicate = NULL,
  max_entries = 24L
) {
  if (
    !is.list(evidence) ||
      is.data.frame(evidence) ||
      !is.null(names(evidence)) ||
      length(evidence) > max_entries
  ) {
    return(list(ok = FALSE, issue = on_list_error()))
  }
  seen <- character()
  normalized <- vector("list", length(evidence))
  for (index in seq_along(evidence)) {
    entry <- validate_entry(evidence[[index]])
    if (!isTRUE(entry$ok)) {
      return(list(ok = FALSE, issue = entry$issue))
    }
    source_id <- entry$source_id
    if (!source_id %in% source_ids) {
      return(list(ok = FALSE, issue = on_unknown_source(source_id)))
    }
    quote <- entry$quote
    if (isTRUE(check_duplicates)) {
      pair <- tempest_product_canonical_json(list(source_id, quote))
      if (pair %in% seen) {
        return(list(ok = FALSE, issue = on_duplicate()))
      }
      seen <- c(seen, pair)
    }
    source_text <- source_texts[[source_id]]
    if (is.null(source_text) || !grepl(quote, source_text, fixed = TRUE)) {
      return(list(ok = FALSE, issue = on_missing_quote(source_id)))
    }
    normalized[[index]] <- list(source_id = source_id, quote = quote)
  }
  list(ok = TRUE, evidence = unname(normalized), issue = NULL)
}

tempest_claim_review_validate_evidence <- function(evidence, dossier_ids) {
  source_texts <- attr(dossier_ids, "source_texts", exact = TRUE) %||% list()
  result <- tempest_dossier_validate_evidence(
    evidence = evidence,
    source_ids = dossier_ids,
    source_texts = source_texts,
    validate_entry = function(item) {
      issue <- tempest_claim_review_record_list(
        item,
        c("source_id", "quote"),
        "evidence entries"
      )
      if (!is.null(issue)) {
        return(list(ok = FALSE, issue = issue))
      }
      if (!tempest_opaque_identifier_valid(item$source_id)) {
        return(list(
          ok = FALSE,
          issue = "evidence source_id must be a bounded identifier"
        ))
      }
      if (!tempest_claim_review_plain_scalar(item$quote, 4000L)) {
        return(list(
          ok = FALSE,
          issue = "evidence quote must be one non-empty string of at most 4000 characters"
        ))
      }
      list(ok = TRUE, source_id = item$source_id, quote = item$quote)
    },
    on_list_error = function() {
      "evidence must be an unnamed list of at most 24 source quotes"
    },
    on_unknown_source = function(source_id) {
      "evidence references a source absent from the dossier"
    },
    on_missing_quote = function(source_id) {
      "evidence quote must be an exact substring of its dossier source text"
    },
    check_duplicates = TRUE,
    on_duplicate = function() {
      "evidence cannot repeat an identical source and quote pair"
    }
  )
  result$issue
}

tempest_claim_review_validate_header <- function(record) {
  issue <- tempest_claim_review_record_list(
    record,
    tempest_claim_review_record_fields(),
    "A claim-review record"
  )
  if (!is.null(issue)) {
    return(issue)
  }
  if (!tempest_exact_integer_scalar_valid(record$schema_version, 1L, 1L)) {
    return("schema_version must be exact integer 1")
  }
  if (
    !tempest_claim_review_plain_scalar(record$review_id, 71L) ||
      !grepl("^sha256:[a-f0-9]{64}$", record$review_id)
  ) {
    return("review_id must be a lowercase SHA-256 identifier")
  }
  if (!tempest_opaque_identifier_valid(record$claim_id)) {
    return("claim_id must be a bounded credential-free identifier")
  }
  if (!tempest_claim_review_plain_scalar(record$claim_text, 20000L)) {
    return("claim_text must be one bounded non-empty string")
  }
  NULL
}

tempest_claim_review_validate_dossier <- function(dossier) {
  if (
    !is.list(dossier) ||
      is.data.frame(dossier) ||
      !is.null(names(dossier)) ||
      length(dossier) > 12L
  ) {
    return(list(
      issue = "dossier must be an unnamed list of at most 12 source records"
    ))
  }
  dossier_ids <- character()
  source_texts <- list()
  for (index in seq_along(dossier)) {
    source <- dossier[[index]]
    issue <- tempest_claim_review_record_list(
      source,
      c("source_id", "locator", "content_hash", "text"),
      "dossier sources"
    )
    if (!is.null(issue)) {
      return(list(issue = issue))
    }
    if (!tempest_opaque_identifier_valid(source$source_id)) {
      return(list(
        issue = "dossier source_id must be a bounded credential-free identifier"
      ))
    }
    if (!tempest_claim_review_plain_scalar(source$locator, 4096L)) {
      return(list(
        issue = "dossier locator must be one bounded non-empty string"
      ))
    }
    if (
      !tempest_claim_review_plain_scalar(source$content_hash, 64L) ||
        !grepl("^[a-f0-9]{64}$", source$content_hash)
    ) {
      return(list(
        issue = "dossier content_hash must be a full lowercase SHA-256 hash"
      ))
    }
    if (!tempest_claim_review_plain_scalar(source$text, 20000L, TRUE)) {
      return(list(
        issue = "dossier text must be one string of at most 20000 characters"
      ))
    }
    dossier_ids <- c(dossier_ids, source$source_id)
    source_texts[[source$source_id]] <- source$text
  }
  if (anyDuplicated(dossier_ids)) {
    return(list(issue = "dossier cannot contain duplicate source_id values"))
  }
  attr(dossier_ids, "source_texts") <- source_texts
  list(issue = NULL, dossier_ids = dossier_ids)
}

tempest_claim_review_validate_reviewer <- function(reviewer, dossier_ids) {
  issue <- tempest_claim_review_record_list(
    reviewer,
    c("status", "finding", "rationale", "evidence", "query"),
    "reviewer"
  )
  if (!is.null(issue)) {
    return(issue)
  }
  if (
    !tempest_claim_review_plain_scalar(reviewer$status, 32L) ||
      !reviewer$status %in%
        c("no_issue", "potential_issue", "insufficient_evidence")
  ) {
    return(
      "reviewer status must be no_issue, potential_issue, or insufficient_evidence"
    )
  }
  if (!tempest_claim_review_plain_scalar(reviewer$finding, 4000L, TRUE)) {
    return("reviewer finding must be one bounded string")
  }
  if (
    !tempest_claim_review_plain_scalar(reviewer$rationale, 4000L) ||
      tempest_contract_sensitive_scalar(reviewer$rationale)
  ) {
    return(
      "reviewer rationale must be one bounded credential-free non-empty string"
    )
  }
  if (
    !tempest_claim_review_plain_scalar(reviewer$query, 4000L, TRUE) ||
      tempest_contract_sensitive_scalar(reviewer$query)
  ) {
    return("reviewer query must be one bounded credential-free string")
  }
  issue <- tempest_claim_review_validate_evidence(
    reviewer$evidence,
    dossier_ids
  )
  if (!is.null(issue)) {
    return(paste("reviewer", issue))
  }
  NULL
}

tempest_claim_review_validate_adjudication <- function(
  adjudication,
  reviewer,
  dossier_ids
) {
  issue <- tempest_claim_review_record_list(
    adjudication,
    c("verdict", "rationale", "evidence"),
    "adjudication"
  )
  if (!is.null(issue)) {
    return(issue)
  }
  if (
    !tempest_claim_review_plain_scalar(adjudication$verdict, 32L) ||
      !adjudication$verdict %in%
        c("no_issue", "confirmed", "dismissed", "unresolved")
  ) {
    return(
      "adjudication verdict must be no_issue, confirmed, dismissed, or unresolved"
    )
  }
  if (
    !tempest_claim_review_plain_scalar(adjudication$rationale, 4000L) ||
      tempest_contract_sensitive_scalar(adjudication$rationale)
  ) {
    return(
      "adjudication rationale must be one bounded credential-free non-empty string"
    )
  }
  issue <- tempest_claim_review_validate_evidence(
    adjudication$evidence,
    dossier_ids
  )
  if (!is.null(issue)) {
    return(paste("adjudication", issue))
  }
  reviewer_has_evidence <- length(reviewer$evidence) > 0L
  if (
    identical(adjudication$verdict, "confirmed") &&
      (!identical(reviewer$status, "potential_issue") ||
        !reviewer_has_evidence ||
        length(adjudication$evidence) == 0L ||
        !nzchar(tempest_trim(reviewer$finding)))
  ) {
    return(paste0(
      "a confirmed verdict requires a potential_issue reviewer and adjudicator with cited evidence and a finding"
    ))
  }
  if (
    identical(adjudication$verdict, "no_issue") &&
      !identical(reviewer$status, "no_issue")
  ) {
    return("a no_issue verdict requires a no_issue reviewer")
  }
  NULL
}

tempest_claim_review_validate_repair <- function(
  repair,
  reviewer,
  dossier_ids
) {
  issue <- tempest_claim_review_record_list(
    repair,
    c("status", "query", "source_ids"),
    "repair"
  )
  if (!is.null(issue)) {
    return(issue)
  }
  if (
    !tempest_claim_review_plain_scalar(repair$status, 32L) ||
      !repair$status %in%
        c(
          "not_requested",
          "no_results",
          "completed",
          "budget_exhausted",
          "failed"
        )
  ) {
    return(
      "repair status must be not_requested, no_results, completed, budget_exhausted, or failed"
    )
  }
  if (
    !tempest_claim_review_plain_scalar(repair$query, 4000L, TRUE) ||
      tempest_contract_sensitive_scalar(repair$query)
  ) {
    return("repair query must be one bounded credential-free string")
  }
  repair_ids <- repair$source_ids
  if (
    !is.character(repair_ids) ||
      is.object(repair_ids) ||
      !is.null(attributes(repair_ids)) ||
      length(repair_ids) > 2L ||
      anyNA(repair_ids) ||
      any(
        !vapply(
          repair_ids,
          tempest_opaque_identifier_valid,
          logical(1)
        )
      ) ||
      anyDuplicated(repair_ids)
  ) {
    return(
      "repair source_ids must be a unique array of at most 2 bounded identifiers"
    )
  }
  if (
    identical(repair$status, "not_requested") &&
      (nzchar(repair$query) || length(repair_ids) > 0L)
  ) {
    return("not_requested repair must have an empty query and source_ids array")
  }
  if (
    identical(repair$status, "no_results") &&
      length(repair_ids) > 0L
  ) {
    return("no_results repair cannot list source ids")
  }
  if (length(setdiff(repair_ids, dossier_ids)) > 0L) {
    return("repair source_ids must reference sources in the dossier")
  }
  if (
    !identical(repair$status, "not_requested") &&
      !nzchar(tempest_trim(repair$query))
  ) {
    return("a requested repair must have a non-empty query")
  }
  if (
    !identical(repair$status, "not_requested") &&
      !identical(repair$query, reviewer$query)
  ) {
    return("requested repair query must exactly match reviewer query")
  }
  NULL
}

tempest_claim_review_validate_provenance <- function(record) {
  for (field in c("programs", "models")) {
    value <- record[[field]]
    issue <- tempest_claim_review_record_list(
      value,
      c("reviewer", "adjudicator"),
      field
    )
    if (!is.null(issue)) {
      return(issue)
    }
    valid <- if (identical(field, "programs")) {
      vapply(
        value,
        function(item) {
          tempest_claim_review_plain_scalar(item, 71L) &&
            grepl("^sha256:[a-f0-9]{64}$", item)
        },
        logical(1)
      )
    } else {
      vapply(value, tempest_opaque_identifier_valid, logical(1))
    }
    if (!all(valid)) {
      return(paste(
        field,
        "must contain non-empty bounded reviewer and adjudicator identifiers"
      ))
    }
  }
  payload <- record[setdiff(names(record), "review_id")]
  expected_id <- paste0("sha256:", tempest_product_record_hash(payload))
  if (!identical(record$review_id, expected_id)) {
    return("review_id does not match the canonical review content")
  }
  NULL
}

tempest_claim_review_validate_record <- function(record) {
  issue <- tempest_claim_review_validate_header(record)
  if (!is.null(issue)) {
    return(issue)
  }
  dossier_check <- tempest_claim_review_validate_dossier(record$dossier)
  if (!is.null(dossier_check$issue)) {
    return(dossier_check$issue)
  }
  dossier_ids <- dossier_check$dossier_ids
  issue <- tempest_claim_review_validate_reviewer(record$reviewer, dossier_ids)
  if (!is.null(issue)) {
    return(issue)
  }
  issue <- tempest_claim_review_validate_adjudication(
    record$adjudication,
    record$reviewer,
    dossier_ids
  )
  if (!is.null(issue)) {
    return(issue)
  }
  issue <- tempest_claim_review_validate_repair(
    record$repair,
    record$reviewer,
    dossier_ids
  )
  if (!is.null(issue)) {
    return(issue)
  }
  tempest_claim_review_validate_provenance(record)
}

TempestClaimReview <- S7::new_class(
  "TempestClaimReview",
  properties = list(record = S7::new_property(S7::class_list)),
  validator = function(self) {
    tempest_claim_review_validate_record(self@record)
  }
)

tempest_claim_review <- function(record) {
  if (!is.list(record) || is.data.frame(record)) {
    tempest_claim_review_abort(
      "{.arg record} must be a plain claim-review list."
    )
  }
  fields <- names(record)
  payload_fields <- tempest_claim_review_payload_fields()
  record_fields <- tempest_claim_review_record_fields()
  if (is.list(record) && setequal(fields, payload_fields)) {
    payload <- tempest_claim_review_normalize_record(record, include_id = FALSE)
    review_id <- paste0("sha256:", tempest_product_record_hash(payload))
    record <- append(payload, list(review_id = review_id), after = 1L)
  } else if (is.list(record) && setequal(fields, record_fields)) {
    record <- tempest_claim_review_normalize_record(record)
  } else {
    tempest_claim_review_abort(
      "Claim-review input must use the exact current fields, with review_id omitted or present."
    )
  }
  expected_id <- paste0(
    "sha256:",
    tempest_product_record_hash(record[setdiff(names(record), "review_id")])
  )
  if (!identical(record$review_id, expected_id)) {
    tempest_claim_review_abort(
      "{.field review_id} must match the canonical review content."
    )
  }
  issue <- tempest_claim_review_validate_record(record)
  if (!is.null(issue)) {
    tempest_claim_review_abort(issue)
  }
  record <- rlang::duplicate(record, shallow = FALSE)
  value <- tryCatch(
    TempestClaimReview(record = record),
    error = function(error) {
      tempest_claim_review_abort(
        "Cannot construct an invalid claim-review record.",
        parent = error
      )
    }
  )
  S7::validate(value)
  value
}

tempest_claim_review_record <- function(x) {
  if (!identical(S7::S7_class(x), TempestClaimReview)) {
    tempest_claim_review_abort(
      "{.arg x} must be an exact TempestClaimReview value."
    )
  }
  S7::validate(x)
  rlang::duplicate(x@record, shallow = FALSE)
}

tempest_claim_review_from_data <- function(value) {
  if (
    !is.list(value) ||
      is.data.frame(value) ||
      anyNA(names(value)) ||
      anyDuplicated(names(value)) ||
      length(value) != length(tempest_claim_review_record_fields()) ||
      !setequal(names(value), tempest_claim_review_record_fields())
  ) {
    tempest_claim_review_abort(
      "Persisted claim-review records have an invalid field shape."
    )
  }
  value <- tempest_claim_review_normalize_record(value)
  source_ids <- value$repair$source_ids
  if (
    !is.list(source_ids) ||
      is.data.frame(source_ids) ||
      !is.null(names(source_ids))
  ) {
    tempest_claim_review_abort(
      "Persisted repair source_ids must be a flat string array."
    )
  }
  if (length(source_ids) == 0L) {
    value$repair$source_ids <- character()
  } else {
    valid <- vapply(
      source_ids,
      tempest_claim_review_plain_scalar,
      logical(1),
      max_chars = 128L
    )
    if (!all(valid)) {
      tempest_claim_review_abort(
        "Persisted repair source_ids must be a flat string array."
      )
    }
    value$repair$source_ids <- unname(unlist(source_ids, use.names = FALSE))
  }
  tempest_claim_review(value)
}

tempest_claim_review_resource_texts <- function(resource) {
  content <- resource@content
  if (is.null(content)) {
    return(character())
  }
  if (is.character(content) && length(content) == 1L) {
    return(enc2utf8(content))
  }
  tempest_product_canonical_json(content)
}

tempest_claim_review_source_text_matches <- function(text, resource) {
  captured <- tempest_claim_review_resource_texts(resource)
  if (identical(text, "")) {
    return(any(captured == ""))
  }
  any(vapply(
    captured[nzchar(captured)],
    function(value) grepl(text, value, fixed = TRUE),
    logical(1)
  ))
}

tempest_claim_review_relevant_claims <- function(workspace, claim) {
  disputes <- workspace$list_disputes()
  related_claim_ids <- unique(unlist(
    lapply(disputes, function(dispute) {
      if (claim@claim_id %in% dispute@claim_ids) {
        setdiff(dispute@claim_ids, claim@claim_id)
      } else {
        character()
      }
    }),
    use.names = FALSE
  ))
  related_claims <- list()
  for (related_claim_id in related_claim_ids) {
    related_claim <- workspace$get_proposed_claim(related_claim_id)
    if (is.null(related_claim)) {
      tempest_research_workspace_abort(
        "Claim review cannot establish required source coverage for an unknown disputed claim."
      )
    }
    related_claims[[length(related_claims) + 1L]] <- related_claim
  }
  c(list(claim), related_claims)
}

tempest_claim_review_relevant_spans <- function(workspace, claims) {
  span_ids <- unique(unlist(
    lapply(
      claims,
      \(claim) claim@evidence_span_ids
    ),
    use.names = FALSE
  ))
  spans <- lapply(span_ids, workspace$get_evidence_span)
  if (any(vapply(spans, is.null, logical(1)))) {
    tempest_research_workspace_abort(
      "Claim review cannot establish required source coverage for a missing evidence span."
    )
  }
  unname(spans)
}

tempest_claim_review_required_source_ids <- function(workspace, claim) {
  relevant_claims <- tempest_claim_review_relevant_claims(workspace, claim)
  spans <- tempest_claim_review_relevant_spans(workspace, relevant_claims)
  claim_source_ids <- unlist(
    lapply(relevant_claims, function(value) {
      c(value@source_ids, value@contradicting_source_ids)
    }),
    use.names = FALSE
  )
  unique(c(
    claim_source_ids,
    vapply(spans, \(span) span@source_id, character(1))
  ))
}

tempest_claim_review_validate_linked_quotes <- function(
  workspace,
  claim,
  dossier
) {
  relevant_claims <- tempest_claim_review_relevant_claims(workspace, claim)
  spans <- tempest_claim_review_relevant_spans(workspace, relevant_claims)
  source_ids <- vapply(dossier, `[[`, character(1), "source_id")
  for (span in spans) {
    if (is.na(span@quote)) {
      next
    }
    dossier_index <- match(span@source_id, source_ids)
    if (
      is.na(dossier_index) ||
        !grepl(span@quote, dossier[[dossier_index]]$text, fixed = TRUE)
    ) {
      tempest_research_workspace_abort(
        "Claim review dossier omits linked evidence quote from span {.val {span@evidence_span_id}}."
      )
    }
  }
  invisible(NULL)
}

tempest_claim_review_validate_workspace <- function(review, workspace) {
  record <- tempest_claim_review_record(review)
  claim <- workspace$get_proposed_claim(record$claim_id)
  if (is.null(claim)) {
    tempest_research_workspace_abort(
      "Claim review references unknown claim id: {.val {record$claim_id}}."
    )
  }
  if (!identical(claim@claim_text, record$claim_text)) {
    tempest_research_workspace_abort(
      "Claim review {.val {record$review_id}} captures stale claim text."
    )
  }
  source_ids <- vapply(
    record$dossier,
    `[[`,
    character(1),
    "source_id"
  )
  required_source_ids <- tempest_claim_review_required_source_ids(
    workspace,
    claim
  )
  missing_source_ids <- setdiff(required_source_ids, source_ids)
  if (length(missing_source_ids) > 0L) {
    tempest_research_workspace_abort(
      "Claim review dossier omits required source id{?s} {.val {missing_source_ids}}."
    )
  }
  tempest_claim_review_validate_linked_quotes(
    workspace,
    claim,
    record$dossier
  )
  source_ids <- unique(c(source_ids, record$repair$source_ids))
  for (source_id in source_ids) {
    resource <- workspace$get_retrieved_resource(source_id)
    if (is.null(resource)) {
      tempest_research_workspace_abort(
        "Claim review references unknown source id: {.val {source_id}}."
      )
    }
  }
  for (source in record$dossier) {
    resource <- workspace$get_retrieved_resource(source$source_id)
    if (tempest_is_accepted_knowledge_resource(resource)) {
      tempest_research_workspace_abort(
        "Accepted Graft knowledge cannot enter a claim-review dossier."
      )
    }
    if (
      !identical(resource@locator, source$locator) ||
        is.na(resource@content_hash) ||
        !identical(resource@content_hash, source$content_hash)
    ) {
      tempest_research_workspace_abort(
        "Claim review {.val {record$review_id}} captures a stale source locator or content hash for {.val {source$source_id}}."
      )
    }
    if (!tempest_claim_review_source_text_matches(source$text, resource)) {
      tempest_research_workspace_abort(
        "Claim review {.val {record$review_id}} captures text that is not an exact substring of source {.val {source$source_id}}."
      )
    }
  }
  invisible(review)
}
