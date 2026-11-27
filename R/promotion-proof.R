# Promotion proof builders, credential audit, and restoration

tempest_promotion_support_type <- function(verification_status) {
  switch(
    verification_status,
    supported = "supports",
    partially_supported = "supports",
    contradicted = "contradicts",
    unsupported = "mentions",
    unverifiable = "mentions",
    tempest_promotion_abort(
      "Unsupported pair verification status {.val {verification_status}}."
    )
  )
}

tempest_promotion_span_locator <- function(span) {
  if (!is.na(span@start_offset)) {
    return(list(
      type = "other",
      value = paste0(
        "characters:",
        span@start_offset,
        "-",
        span@end_offset
      )
    ))
  }
  if (!is.na(span@page)) {
    return(list(type = "page", value = as.character(span@page)))
  }
  if (!is.na(span@section_heading)) {
    return(list(type = "section", value = span@section_heading))
  }
  if (!is.na(span@chunk_id)) {
    return(list(type = "other", value = paste0("chunk:", span@chunk_id)))
  }
  list(type = NULL, value = NULL)
}

tempest_promotion_evidence_span_id <- function(tempest_evidence_span_id) {
  if (!tempest_ledger_identifier_valid(tempest_evidence_span_id)) {
    tempest_promotion_abort(
      "A Tempest evidence-span ID cannot map to a canonical research ID."
    )
  }
  mapped <- paste0("tempest:", tempest_evidence_span_id)
  round_trip <- substr(mapped, nchar("tempest:") + 1L, nchar(mapped))
  if (!identical(round_trip, tempest_evidence_span_id)) {
    tempest_promotion_abort(
      "A Tempest evidence-span ID failed its canonical research round trip."
    )
  }
  mapped
}

tempest_promotion_source_row <- function(resource, content_hash) {
  data <- tempest_resource_data(resource, include_content = FALSE)
  stats::setNames(
    list(
      data$resource_id,
      data$resource_kind,
      data$locator,
      data$title,
      data$media_type,
      content_hash,
      data$retrieved_at
    ),
    tempest_promotion_record_fields()$Source
  )
}

tempest_promotion_claim_row <- function(claim, supports) {
  summary <- tempest_promotion_support_summary(supports)
  stats::setNames(
    list(
      claim@claim_id,
      claim@claim_text,
      claim@claim_type,
      claim@confidence,
      summary$status,
      tempest_promotion_nullable(summary$score),
      tempest_promotion_nullable(claim@contradiction_score),
      tempest_promotion_nullable(claim@source_quality_score),
      tempest_promotion_nullable(claim@retrieval_query),
      tempest_promotion_nullable(claim@retrieval_step_id),
      tempest_promotion_nullable(claim@perspective_id),
      tempest_promotion_nullable(claim@expert_id),
      tempest_promotion_nullable(claim@session_id),
      tempest_promotion_nullable(claim@section_id),
      claim@created_at,
      NULL,
      NULL,
      "active"
    ),
    tempest_promotion_record_fields()$Claim
  )
}

tempest_promotion_span_row <- function(span, source_hash) {
  stats::setNames(
    list(
      tempest_promotion_evidence_span_id(span@evidence_span_id),
      span@evidence_span_id,
      span@source_id,
      span@quote,
      tempest_promotion_nullable(span@chunk_id),
      tempest_promotion_nullable(span@start_offset),
      tempest_promotion_nullable(span@end_offset),
      tempest_promotion_nullable(span@page),
      tempest_promotion_nullable(span@page),
      tempest_promotion_nullable(span@section_heading),
      tempest_promotion_nullable(span@relevance_score),
      span@extracted_by,
      "tempest.evidence-span/1",
      source_hash,
      span@created_at
    ),
    tempest_promotion_record_fields()$EvidenceSpan
  )
}

tempest_promotion_support_row <- function(support, span, source_hash) {
  locator <- tempest_promotion_span_locator(span)
  stats::setNames(
    list(
      support@claim_support_id,
      support@claim_id,
      support@claim_id,
      support@source_id,
      tempest_promotion_evidence_span_id(support@evidence_span_id),
      tempest_promotion_support_type(support@verification_status),
      support@verification_status,
      tempest_promotion_nullable(support@support_score),
      support@rationale,
      locator$type,
      locator$value,
      tempest_promotion_nullable(span@page),
      tempest_promotion_nullable(span@page),
      span@quote,
      source_hash,
      span@extracted_by,
      "tempest.evidence-span/1"
    ),
    tempest_promotion_record_fields()$ClaimSupport
  )
}

tempest_promotion_stage_selection <- function(
  stage_records,
  claim_ids,
  claim_support_ids,
  all_claim_ids,
  all_claim_support_ids
) {
  succeeded <- Filter(
    function(record) identical(record@status, "succeeded"),
    stage_records
  )
  relevant <- Filter(
    function(record) {
      ids <- unlist(record@output_reference$ids, use.names = FALSE)
      if (identical(record@stage, "extract_claims")) {
        return(length(intersect(ids, claim_ids)) > 0L)
      }
      if (identical(record@stage, "verify_claim_support")) {
        return(length(intersect(ids, claim_support_ids)) > 0L)
      }
      FALSE
    },
    succeeded
  )
  coverage <- function(stage) {
    records <- Filter(
      function(record) identical(record@stage, stage),
      succeeded
    )
    unname(unlist(lapply(
      records,
      function(record) record@output_reference$ids
    )))
  }
  extracted <- coverage("extract_claims")
  verified <- coverage("verify_claim_support")
  if (
    !identical(sort(extracted, method = "radix"), all_claim_ids) ||
      !identical(
        sort(verified, method = "radix"),
        all_claim_support_ids
      )
  ) {
    tempest_promotion_abort(
      paste0(
        "Promotion requires exactly one succeeded extraction binding for ",
        "every workspace claim and one succeeded verification binding for ",
        "every workspace claim-support pair."
      )
    )
  }
  if (length(relevant) == 0L) {
    tempest_promotion_abort(
      "Promotion has no succeeded extraction or verification records."
    )
  }
  selected_outputs <- list(
    extract_claims = claim_ids,
    verify_claim_support = claim_support_ids
  )
  selection_is_closed <- all(vapply(
    relevant,
    function(record) {
      ids <- unname(unlist(record@output_reference$ids, use.names = FALSE))
      length(setdiff(ids, selected_outputs[[record@stage]])) == 0L
    },
    logical(1)
  ))
  if (!selection_is_closed) {
    tempest_promotion_abort(
      paste0(
        "Promotion selection must include every claim and claim-support ",
        "output bound by each retained StageRecord."
      )
    )
  }
  relevant
}

tempest_promotion_stage_support_threshold <- function(stage_records) {
  verification_records <- Filter(
    function(record) {
      identical(record@stage, "verify_claim_support") &&
        identical(record@status, "succeeded")
    },
    stage_records
  )
  values <- lapply(
    verification_records,
    function(record) record@trace_references$min_support_score %||% NULL
  )
  valid <- length(values) > 0L &&
    all(vapply(
      values,
      function(value) {
        rlang::is_string(value) && !is.na(value) && nzchar(value)
      },
      logical(1)
    ))
  if (
    !valid ||
      !all(vapply(values, identical, logical(1), values[[1L]]))
  ) {
    tempest_promotion_abort(
      paste0(
        "Selected verification records must prove one exact canonical ",
        "min_support_score."
      )
    )
  }
  tryCatch(
    tempest_stage_support_threshold_value(values[[1L]]),
    error = function(error) {
      tempest_promotion_abort(
        "The verified min_support_score trace is not canonical."
      )
    }
  )
}

tempest_promotion_supports <- function(workspace) {
  accessor <- workspace$list_claim_supports
  if (!is.function(accessor)) {
    tempest_promotion_abort(
      paste0(
        "ResearchWorkspace must expose exact pair-level support through ",
        "list_claim_supports()."
      )
    )
  }
  supports <- accessor()
  if (!is.list(supports) || is.data.frame(supports)) {
    tempest_promotion_abort(
      "ResearchWorkspace returned malformed pair-level claim support."
    )
  }
  for (support in supports) {
    if (!S7::S7_inherits(support, TempestClaimSupport)) {
      tempest_promotion_abort(
        "ResearchWorkspace claim support must use TempestClaimSupport values."
      )
    }
    S7::validate(support)
  }
  supports
}

tempest_promotion_stage_output_ids <- function(record) {
  ids <- unlist(record@output_reference$ids, use.names = FALSE)
  if (length(ids) == 0L) {
    return(character())
  }
  as.character(unname(ids))
}

tempest_promotion_resource_proof_row <- function(resource) {
  tempest_promotion_nullable_tree(tempest_resource_record(
    resource,
    include_content = TRUE
  ))
}

tempest_promotion_claim_proof_row <- function(claim) {
  row <- tempest_claim_to_list(claim)
  row[c("support_score", "verified_at", "verifier_model")] <- list(
    NA_real_,
    NA_character_,
    NA_character_
  )
  row["verification_status"] <- list("unverified")
  row <- tempest_promotion_nullable_tree(row)
  for (field in c(
    "source_ids",
    "evidence_span_ids",
    "contradicting_source_ids"
  )) {
    row[[field]] <- unname(as.list(row[[field]]))
  }
  row
}

tempest_promotion_evidence_span_proof_row <- function(span) {
  tempest_promotion_nullable_tree(tempest_evidence_span_to_list(span))
}

tempest_promotion_claim_support_proof_row <- function(support) {
  tempest_promotion_nullable_tree(tempest_claim_support_to_list(support))
}

tempest_promotion_claim_review_proof_row <- function(review) {
  record <- tempest_claim_review_record(review)
  record$repair$source_ids <- unname(as.list(record$repair$source_ids))
  record
}

tempest_promotion_workspace_claim_reviews <- function(workspace) {
  accessor <- workspace$list_claim_reviews
  if (!is.function(accessor)) {
    tempest_promotion_abort(
      "ResearchWorkspace must expose exact claim reviews through list_claim_reviews()."
    )
  }
  reviews <- accessor()
  if (
    !is.list(reviews) ||
      is.data.frame(reviews)
  ) {
    tempest_promotion_abort(
      "ResearchWorkspace returned malformed claim-review records."
    )
  }
  rows <- lapply(reviews, function(review) {
    if (!identical(S7::S7_class(review), TempestClaimReview)) {
      tempest_promotion_abort(
        "ResearchWorkspace claim reviews must use TempestClaimReview values."
      )
    }
    S7::validate(review)
    tempest_promotion_claim_review_proof_row(review)
  })
  ids <- vapply(rows, `[[`, character(1), "review_id")
  if (anyDuplicated(ids)) {
    tempest_promotion_abort(
      "ResearchWorkspace claim reviews must have unique review identifiers."
    )
  }
  rows[order(ids, method = "radix")]
}

tempest_promotion_review_proof_rows <- function(value) {
  if (
    !is.list(value) ||
      is.data.frame(value) ||
      !is.null(names(value))
  ) {
    tempest_promotion_abort(
      "Promotion claim reviews must be an unnamed current-schema row list."
    )
  }
  rows <- lapply(
    value,
    tempest_promotion_assert_exact_row,
    fields = tempest_claim_review_record_fields(),
    noun = "Promotion claim-review proof"
  )
  ids <- vapply(rows, `[[`, character(1), "review_id")
  if (
    anyNA(ids) ||
      anyDuplicated(ids) ||
      !identical(ids, sort(ids, method = "radix"))
  ) {
    tempest_promotion_abort(
      "Promotion claim-review proof must use sorted unique review identifiers."
    )
  }
  rows
}

tempest_promotion_proof_data <- function(workspace, stage_records, supports) {
  support_map <- stats::setNames(
    supports,
    vapply(
      supports,
      function(support) support@claim_support_id,
      character(1)
    )
  )
  extraction_records <- Filter(
    function(record) identical(record@stage, "extract_claims"),
    stage_records
  )
  verification_records <- Filter(
    function(record) identical(record@stage, "verify_claim_support"),
    stage_records
  )
  claim_ids <- unname(unlist(lapply(
    extraction_records,
    tempest_promotion_stage_output_ids
  )))
  support_ids <- unname(unlist(lapply(
    verification_records,
    tempest_promotion_stage_output_ids
  )))
  proof_supports <- unname(support_map[support_ids])
  if (any(vapply(proof_supports, is.null, logical(1)))) {
    tempest_promotion_abort(
      "A retained verification record refers to absent claim support."
    )
  }
  claim_ids <- sort(
    unique(c(
      claim_ids,
      vapply(
        proof_supports,
        function(support) support@claim_id,
        character(1)
      )
    )),
    method = "radix"
  )
  claim_reviews <- tempest_promotion_workspace_claim_reviews(workspace)
  claim_reviews <- Filter(
    function(review) review$claim_id %in% claim_ids,
    claim_reviews
  )
  claims <- stats::setNames(
    lapply(claim_ids, workspace$get_proposed_claim),
    claim_ids
  )
  if (any(vapply(claims, is.null, logical(1)))) {
    tempest_promotion_abort(
      "A retained extraction or verification record refers to an absent claim."
    )
  }
  span_ids <- sort(
    unique(c(
      unname(unlist(lapply(
        claims,
        function(claim) claim@evidence_span_ids
      ))),
      vapply(
        proof_supports,
        function(support) support@evidence_span_id,
        character(1)
      )
    )),
    method = "radix"
  )
  spans <- stats::setNames(
    lapply(span_ids, workspace$get_evidence_span),
    span_ids
  )
  if (any(vapply(spans, is.null, logical(1)))) {
    tempest_promotion_abort(
      "A retained proof record refers to an absent evidence span."
    )
  }
  source_ids <- sort(
    unique(c(
      vapply(spans, function(span) span@source_id, character(1)),
      unlist(
        lapply(claim_reviews, function(review) {
          c(
            vapply(review$dossier, `[[`, character(1), "source_id"),
            unlist(review$repair$source_ids, use.names = FALSE)
          )
        }),
        use.names = FALSE
      )
    )),
    method = "radix"
  )
  resources <- stats::setNames(
    lapply(source_ids, workspace$get_retrieved_resource),
    source_ids
  )
  if (any(vapply(resources, is.null, logical(1)))) {
    tempest_promotion_abort(
      "A retained proof record refers to an absent source."
    )
  }
  proof_supports <- proof_supports[order(
    vapply(
      proof_supports,
      function(support) support@claim_support_id,
      character(1)
    ),
    method = "radix"
  )]
  list(
    resources = unname(lapply(
      resources,
      tempest_promotion_resource_proof_row
    )),
    claims = unname(lapply(claims, tempest_promotion_claim_proof_row)),
    evidence_spans = unname(lapply(
      spans,
      tempest_promotion_evidence_span_proof_row
    )),
    claim_supports = unname(lapply(
      proof_supports,
      tempest_promotion_claim_support_proof_row
    )),
    claim_reviews = unname(claim_reviews)
  )
}

tempest_promotion_proof_rows <- function(value, fields, noun) {
  if (
    !is.list(value) ||
      is.data.frame(value) ||
      !is.null(names(value)) ||
      length(value) == 0L
  ) {
    tempest_promotion_abort(
      "{noun} must be an unnamed non-empty current-schema row list."
    )
  }
  lapply(
    value,
    tempest_promotion_assert_exact_row,
    fields = fields,
    noun = noun
  )
}

tempest_promotion_proof_evidence_metadata_fields <- function() {
  c(
    "snippet",
    "content_text",
    "context_text",
    "citation_context",
    "answer_context"
  )
}

tempest_promotion_proof_scan_value <- function(value, path) {
  sensitive <- c(
    tempest_contract_sensitive_names(value, path),
    tempest_contract_sensitive_values(value, path)
  )
  if (length(sensitive) > 0L) {
    tempest_promotion_abort(
      paste0(
        "Promotion proof contains credential-like content outside an ",
        "authoritative evidence field."
      )
    )
  }
  invisible(value)
}

tempest_promotion_validate_proof_credentials <- function(
  resource_rows,
  claim_rows,
  span_rows,
  support_rows,
  review_rows
) {
  evidence_metadata <- tempest_promotion_proof_evidence_metadata_fields()
  resource_metadata <- c(
    "scope_metadata",
    "redaction",
    "retention",
    "metadata"
  )
  for (index in seq_along(resource_rows)) {
    row <- resource_rows[[index]]
    row$content <- NULL
    for (field in resource_metadata) {
      value <- row[[field]]
      if (is.list(value) && !is.null(names(value))) {
        row[[field]] <- value[!names(value) %in% evidence_metadata]
      }
    }
    tempest_promotion_proof_scan_value(
      row,
      paste0("proof$resources[[", index, "]]")
    )
  }
  for (index in seq_along(claim_rows)) {
    row <- claim_rows[[index]]
    row$claim_text <- NULL
    row$supporting_quotes <- NULL
    tempest_promotion_proof_scan_value(
      row,
      paste0("proof$claims[[", index, "]]")
    )
  }
  for (index in seq_along(span_rows)) {
    row <- span_rows[[index]]
    row$quote <- NULL
    tempest_promotion_proof_scan_value(
      row,
      paste0("proof$evidence_spans[[", index, "]]")
    )
  }
  for (index in seq_along(support_rows)) {
    tempest_promotion_proof_scan_value(
      support_rows[[index]],
      paste0("proof$claim_supports[[", index, "]]")
    )
  }
  for (index in seq_along(review_rows)) {
    row <- review_rows[[index]]
    row$claim_text <- NULL
    for (source_index in seq_along(row$dossier)) {
      row$dossier[[source_index]]$text <- NULL
    }
    for (evidence_index in seq_along(row$reviewer$evidence)) {
      row$reviewer$evidence[[evidence_index]]$quote <- NULL
    }
    for (evidence_index in seq_along(row$adjudication$evidence)) {
      row$adjudication$evidence[[evidence_index]]$quote <- NULL
    }
    tempest_promotion_proof_scan_value(
      row,
      paste0("proof$claim_reviews[[", index, "]]")
    )
  }
  invisible(TRUE)
}

tempest_promotion_restore_proof <- function(proof) {
  tempest_promotion_assert_exact_row(
    proof,
    tempest_promotion_proof_fields(),
    "Promotion proof"
  )
  resource_rows <- tempest_promotion_proof_rows(
    proof$resources,
    tempest_promotion_resource_proof_fields(),
    "Promotion resource proof"
  )
  claim_rows <- tempest_promotion_proof_rows(
    proof$claims,
    tempest_promotion_claim_proof_fields(),
    "Promotion claim proof"
  )
  span_rows <- tempest_promotion_proof_rows(
    proof$evidence_spans,
    tempest_promotion_evidence_span_proof_fields(),
    "Promotion evidence-span proof"
  )
  support_rows <- tempest_promotion_proof_rows(
    proof$claim_supports,
    tempest_promotion_claim_support_proof_fields(),
    "Promotion claim-support proof"
  )
  review_rows <- tempest_promotion_review_proof_rows(proof$claim_reviews)
  tempest_promotion_validate_proof_credentials(
    resource_rows,
    claim_rows,
    span_rows,
    support_rows,
    review_rows
  )
  restored <- tryCatch(
    list(
      resources = lapply(resource_rows, tempest_resource_from_data),
      claims = lapply(claim_rows, tempest_claim_from_list),
      evidence_spans = lapply(
        span_rows,
        tempest_evidence_span_from_list
      ),
      claim_supports = lapply(
        support_rows,
        tempest_claim_support_from_list
      ),
      claim_reviews = lapply(
        review_rows,
        tempest_claim_review_from_data
      )
    ),
    error = function(error) {
      tempest_promotion_abort(
        "Promotion proof failed exact current-schema restoration."
      )
    }
  )
  resources <- restored$resources
  claims <- restored$claims
  spans <- restored$evidence_spans
  supports <- restored$claim_supports
  round_trip <- list(
    resources = unname(lapply(
      resources,
      tempest_promotion_resource_proof_row
    )),
    claims = unname(lapply(claims, tempest_promotion_claim_proof_row)),
    evidence_spans = unname(lapply(
      spans,
      tempest_promotion_evidence_span_proof_row
    )),
    claim_supports = unname(lapply(
      supports,
      tempest_promotion_claim_support_proof_row
    )),
    claim_reviews = unname(lapply(
      restored$claim_reviews,
      tempest_promotion_claim_review_proof_row
    ))
  )
  if (
    !identical(
      round_trip,
      list(
        resources = resource_rows,
        claims = claim_rows,
        evidence_spans = span_rows,
        claim_supports = support_rows,
        claim_reviews = review_rows
      )
    )
  ) {
    tempest_promotion_abort(
      "Promotion proof rows must restore without defaults or coercion."
    )
  }
  values <- list(
    resources = resources,
    claims = claims,
    evidence_spans = spans,
    claim_supports = supports,
    claim_reviews = restored$claim_reviews
  )
  id_accessors <- list(
    resources = function(value) value@resource_id,
    claims = function(value) value@claim_id,
    evidence_spans = function(value) value@evidence_span_id,
    claim_supports = function(value) value@claim_support_id,
    claim_reviews = function(value) {
      tempest_claim_review_record(value)$review_id
    }
  )
  for (field in names(values)) {
    ids <- vapply(values[[field]], id_accessors[[field]], character(1))
    if (
      anyDuplicated(ids) ||
        !identical(ids, sort(ids, method = "radix"))
    ) {
      tempest_promotion_abort(
        "Promotion proof {.field {field}} must use sorted unique identifiers."
      )
    }
    values[[field]] <- stats::setNames(values[[field]], ids)
  }
  workspace <- tempest_research_workspace()
  for (resource in values$resources) {
    workspace$upsert_retrieved_resource(resource)
  }
  workspace$add_extracted_claim_batch(
    unname(values$claims),
    unname(values$evidence_spans)
  )
  workspace$replace_claim_reviews(unname(values$claim_reviews))
  values$workspace <- workspace
  values
}

tempest_promotion_expected_proof_ids <- function(stage_records, proof) {
  extraction_records <- Filter(
    function(record) identical(record@stage, "extract_claims"),
    stage_records
  )
  verification_records <- Filter(
    function(record) identical(record@stage, "verify_claim_support"),
    stage_records
  )
  claim_ids <- unique(unname(unlist(lapply(
    extraction_records,
    tempest_promotion_stage_output_ids
  ))))
  support_ids <- unique(unname(unlist(lapply(
    verification_records,
    tempest_promotion_stage_output_ids
  ))))
  supports <- unname(proof$claim_supports[support_ids])
  if (any(vapply(supports, is.null, logical(1)))) {
    tempest_promotion_abort(
      "Promotion proof omits a retained verification output."
    )
  }
  claim_ids <- sort(
    unique(c(
      claim_ids,
      vapply(supports, function(value) value@claim_id, character(1))
    )),
    method = "radix"
  )
  claims <- unname(proof$claims[claim_ids])
  if (any(vapply(claims, is.null, logical(1)))) {
    tempest_promotion_abort(
      "Promotion proof omits a retained extraction or verification claim."
    )
  }
  span_ids <- sort(
    unique(c(
      unname(unlist(lapply(claims, function(claim) claim@evidence_span_ids))),
      vapply(supports, function(value) value@evidence_span_id, character(1))
    )),
    method = "radix"
  )
  spans <- unname(proof$evidence_spans[span_ids])
  if (any(vapply(spans, is.null, logical(1)))) {
    tempest_promotion_abort(
      "Promotion proof omits a retained evidence span."
    )
  }
  resource_ids <- sort(
    unique(c(
      vapply(spans, function(span) span@source_id, character(1)),
      unlist(
        lapply(proof$claim_reviews, function(review) {
          record <- tempest_claim_review_record(review)
          claim <- proof$claims[[record$claim_id]]
          if (is.null(claim)) {
            tempest_promotion_abort(
              paste0(
                "A claim review references a claim outside the exact ",
                "stage-bound promotion proof."
              )
            )
          }
          if (!identical(claim@claim_text, record$claim_text)) {
            tempest_promotion_abort(
              "A claim review does not bind the exact staged claim text."
            )
          }
          c(
            vapply(record$dossier, `[[`, character(1), "source_id"),
            record$repair$source_ids
          )
        }),
        use.names = FALSE
      )
    )),
    method = "radix"
  )
  list(
    resources = resource_ids,
    claims = claim_ids,
    evidence_spans = span_ids,
    claim_supports = sort(support_ids, method = "radix"),
    claim_reviews = unname(sort(
      vapply(
        proof$claim_reviews,
        function(review) tempest_claim_review_record(review)$review_id,
        character(1)
      ),
      method = "radix"
    ))
  )
}

tempest_promotion_validate_stage_proof <- function(stage_records, proof) {
  expected_ids <- tempest_promotion_expected_proof_ids(stage_records, proof)
  actual_ids <- list(
    resources = names(proof$resources),
    claims = names(proof$claims),
    evidence_spans = names(proof$evidence_spans),
    claim_supports = names(proof$claim_supports),
    claim_reviews = names(proof$claim_reviews)
  )
  if (!identical(expected_ids, actual_ids)) {
    tempest_promotion_abort(
      "Promotion proof is not the exact closed projection for its stage records."
    )
  }
  for (record in stage_records) {
    ids <- tempest_promotion_stage_output_ids(record)
    expected_digest <- if (identical(record@stage, "extract_claims")) {
      claims <- unname(proof$claims[ids])
      spans <- unname(unlist(
        lapply(
          claims,
          function(claim) proof$evidence_spans[claim@evidence_span_ids]
        ),
        recursive = FALSE
      ))
      tempest_stage_claims_output_digest(claims, record, spans)
    } else {
      if (length(ids) != 1L) {
        tempest_promotion_abort(
          "A retained verification record must bind one exact support output."
        )
      }
      support <- proof$claim_supports[[ids[[1L]]]]
      claim <- proof$workspace$get_proposed_claim(support@claim_id)
      span <- proof$workspace$get_evidence_span(support@evidence_span_id)
      tempest_stage_verification_output_digest(
        support,
        record,
        claim,
        span,
        proof$workspace
      )
    }
    if (!identical(record@output_reference$content_digest, expected_digest)) {
      tempest_promotion_abort(
        "A retained stage output digest does not match the exact promotion proof."
      )
    }
  }
  invisible(proof)
}

tempest_promotion_records_from_proof <- function(
  proof,
  stage_records,
  claim_ids,
  min_support_score
) {
  claims <- unname(proof$claims[claim_ids])
  if (any(vapply(claims, is.null, logical(1)))) {
    tempest_promotion_abort("Promotion proof omits a selected claim.")
  }
  supports <- Filter(
    function(support) support@claim_id %in% claim_ids,
    unname(proof$claim_supports)
  )
  expected_pairs <- unname(unlist(lapply(claims, function(claim) {
    paste(claim@claim_id, claim@evidence_span_ids, sep = "\u001f")
  })))
  observed_pairs <- vapply(
    supports,
    function(support) {
      paste(support@claim_id, support@evidence_span_id, sep = "\u001f")
    },
    character(1)
  )
  if (
    anyDuplicated(observed_pairs) ||
      !identical(
        sort(observed_pairs, method = "radix"),
        sort(expected_pairs, method = "radix")
      )
  ) {
    tempest_promotion_abort(
      "Promotion proof does not contain the complete selected claim-pair set."
    )
  }
  support_by_pair <- stats::setNames(supports, observed_pairs)
  claim_supports <- lapply(claims, function(claim) {
    unname(support_by_pair[paste(
      claim@claim_id,
      claim@evidence_span_ids,
      sep = "\u001f"
    )])
  })
  if (
    any(
      !vapply(
        claim_supports,
        tempest_promotion_claim_promotable,
        logical(1),
        min_support_score = min_support_score
      )
    )
  ) {
    tempest_promotion_abort(
      "Promotion proof does not establish pair-derived claim eligibility."
    )
  }
  span_ids <- sort(
    unique(unname(unlist(lapply(
      claims,
      function(claim) claim@evidence_span_ids
    )))),
    method = "radix"
  )
  spans <- proof$evidence_spans[span_ids]
  source_ids <- sort(
    unique(vapply(spans, function(span) span@source_id, character(1))),
    method = "radix"
  )
  resources <- proof$resources[source_ids]
  source_hashes <- vapply(
    resources,
    function(resource) {
      hash <- resource@content_hash
      if (
        !rlang::is_string(hash) ||
          is.na(hash) ||
          !grepl("^[a-f0-9]{64}$", hash)
      ) {
        tempest_promotion_abort(
          "Every promotion source requires an exact SHA-256 content hash."
        )
      }
      paste0("sha256:", hash)
    },
    character(1)
  )
  source_rows <- unname(lapply(source_ids, function(source_id) {
    tempest_promotion_source_row(
      resources[[source_id]],
      source_hashes[[source_id]]
    )
  }))
  claim_rows <- unname(Map(tempest_promotion_claim_row, claims, claim_supports))
  span_rows <- unname(lapply(span_ids, function(span_id) {
    span <- spans[[span_id]]
    tempest_promotion_span_row(span, source_hashes[[span@source_id]])
  }))
  supports <- supports[order(
    vapply(
      supports,
      function(support) support@claim_support_id,
      character(1)
    ),
    method = "radix"
  )]
  support_rows <- unname(lapply(supports, function(support) {
    span <- spans[[support@evidence_span_id]]
    tempest_promotion_support_row(
      support,
      span,
      source_hashes[[span@source_id]]
    )
  }))
  artifact_ids <- sort(
    unique(vapply(
      stage_records,
      function(record) record@program_artifact_id,
      character(1)
    )),
    method = "radix"
  )
  artifact_rows <- unname(lapply(artifact_ids, function(id) {
    stats::setNames(
      list(id, "dsprrr_program"),
      tempest_promotion_record_fields()$ProgramArtifact
    )
  }))
  list(
    Source = source_rows,
    Claim = claim_rows,
    EvidenceSpan = span_rows,
    ClaimSupport = support_rows,
    ProgramArtifact = artifact_rows
  )
}
