# Typed, review-only promotion bundles for provisional scientific evidence

tempest_promotion_schema_version <- 1L
tempest_promotion_schema_build_digest <-
  "sha256:3d907e013022c7a455066d02138183b237e4120d1e763f6705f10573a3ac1034"

tempest_promotion_abort <- function(
  message,
  ...,
  class = character(),
  parent = NULL,
  .envir = rlang::caller_env()
) {
  tempest_abort(
    message,
    ...,
    class = unique(c(class, "tempest_promotion_error", "tempest_error")),
    parent = parent,
    .envir = .envir
  )
}

tempest_promotion_receipt_abort <- function(
  message,
  ...,
  class = character(),
  parent = NULL,
  .envir = rlang::caller_env()
) {
  tempest_promotion_abort(
    message,
    ...,
    class = unique(c(class, "tempest_promotion_receipt_error")),
    parent = parent,
    .envir = .envir
  )
}

tempest_promotion_digest <- function(value) {
  paste0(
    "sha256:",
    digest::digest(
      tempest_product_canonical_json(value),
      algo = "sha256",
      serialize = FALSE
    )
  )
}

tempest_promotion_nullable <- function(value) {
  if (
    is.null(value) ||
      (length(value) == 1L && is.atomic(value) && is.na(value))
  ) {
    return(NULL)
  }
  unname(value)
}

tempest_promotion_nullable_tree <- function(value) {
  if (is.list(value)) {
    return(lapply(value, tempest_promotion_nullable_tree))
  }
  tempest_promotion_nullable(value)
}

tempest_promotion_record_fields <- function() {
  list(
    Source = c(
      "tempest_source_id",
      "resource_kind",
      "locator",
      "title",
      "media_type",
      "content_hash",
      "retrieved_at"
    ),
    Claim = c(
      "tempest_claim_id",
      "statement_text",
      "claim_type",
      "confidence_label",
      "verification_status",
      "support_score",
      "contradiction_score",
      "source_quality_score",
      "retrieval_query",
      "retrieval_step_id",
      "perspective_id",
      "expert_id",
      "session_id",
      "section_id",
      "asserted_at",
      "verified_at",
      "verifier_model",
      "status"
    ),
    EvidenceSpan = c(
      "id",
      "tempest_evidence_span_id",
      "source_id",
      "excerpt",
      "chunk_id",
      "start_offset",
      "end_offset",
      "page_start",
      "page_end",
      "section_heading",
      "relevance_score",
      "extraction_method",
      "extraction_version",
      "source_content_hash",
      "extracted_at"
    ),
    ClaimSupport = c(
      "tempest_claim_support_id",
      "tempest_claim_id",
      "statement_id",
      "source_id",
      "evidence_span_id",
      "support_type",
      "pair_verification_status",
      "support_score",
      "rationale",
      "locator_type",
      "locator_value",
      "page_start",
      "page_end",
      "excerpt",
      "source_content_hash",
      "extraction_method",
      "extraction_version"
    ),
    ProgramArtifact = c("id", "artifact_kind")
  )
}

tempest_promotion_record_id_field <- function(record_class) {
  switch(
    record_class,
    Source = "tempest_source_id",
    Claim = "tempest_claim_id",
    EvidenceSpan = "id",
    ClaimSupport = "tempest_claim_support_id",
    ProgramArtifact = "id",
    tempest_promotion_abort(
      "Unknown promotion record class {.val {record_class}}."
    )
  )
}

tempest_promotion_claim_proof_fields <- function() {
  tempest_claim_record_fields()
}

tempest_promotion_resource_proof_fields <- function() {
  c(
    "resource_id",
    "resource_kind",
    "locator",
    "title",
    "media_type",
    "content",
    "storage_ref",
    "origin_connection_id",
    "scope_metadata",
    "content_hash",
    "retrieved_at",
    "redaction",
    "retention",
    "metadata",
    "schema_version",
    "fingerprint"
  )
}

tempest_promotion_evidence_span_proof_fields <- function() {
  c(
    "evidence_span_id",
    "source_id",
    "chunk_id",
    "quote",
    "start_offset",
    "end_offset",
    "page",
    "section_heading",
    "relevance_score",
    "extracted_by",
    "created_at"
  )
}

tempest_promotion_claim_support_proof_fields <- function() {
  c(
    "claim_support_id",
    "claim_id",
    "evidence_span_id",
    "source_id",
    "verification_status",
    "support_score",
    "rationale"
  )
}

tempest_promotion_proof_fields <- function() {
  c("resources", "claims", "evidence_spans", "claim_supports")
}

tempest_promotion_assert_exact_row <- function(value, fields, noun) {
  if (
    !is.list(value) ||
      is.data.frame(value) ||
      is.null(names(value)) ||
      anyNA(names(value)) ||
      anyDuplicated(names(value)) ||
      !identical(names(value), fields)
  ) {
    tempest_promotion_abort(
      "{noun} must contain exactly the current-schema fields."
    )
  }
  value
}

tempest_promotion_support_summary <- function(supports) {
  tempest_claim_support_aggregate(supports)
}

tempest_promotion_claim_promotable <- function(supports, min_support_score) {
  statuses <- vapply(
    supports,
    function(support) support@verification_status,
    character(1)
  )
  scores <- vapply(
    supports,
    function(support) support@support_score,
    numeric(1)
  )
  summary <- tempest_promotion_support_summary(supports)
  qualifies <- statuses %in%
    c("supported", "partially_supported") &
    !is.na(scores) &
    is.finite(scores) &
    scores >= min_support_score
  summary$status %in% c("supported", "partially_supported") && any(qualifies)
}

tempest_promotion_validate_rows <- function(rows, record_class) {
  fields <- tempest_promotion_record_fields()[[record_class]]
  if (
    !is.list(rows) ||
      is.data.frame(rows) ||
      !is.null(names(rows)) ||
      length(rows) == 0L
  ) {
    tempest_promotion_abort(
      "Promotion class {.val {record_class}} must contain an unnamed non-empty row list."
    )
  }
  for (row in rows) {
    if (
      !is.list(row) ||
        is.data.frame(row) ||
        is.null(names(row)) ||
        anyNA(names(row)) ||
        anyDuplicated(names(row)) ||
        !identical(names(row), fields)
    ) {
      tempest_promotion_abort(
        "Promotion class {.val {record_class}} has a malformed current-schema row."
      )
    }
    tempest_product_canonical_value(row)
    evidence_text_fields <- switch(
      record_class,
      Claim = "statement_text",
      EvidenceSpan = "excerpt",
      ClaimSupport = "excerpt",
      character()
    )
    for (field in names(row)) {
      value <- row[[field]]
      if (
        !is.null(value) &&
          (is.object(value) ||
            !is.null(names(value)) ||
            length(value) != 1L ||
            !typeof(value) %in% c("logical", "integer", "double", "character"))
      ) {
        tempest_promotion_abort(
          "Promotion field {.field {field}} must be one exact JSON scalar or null."
        )
      }
      if (
        is.character(value) &&
          !field %in% evidence_text_fields &&
          tempest_contract_sensitive_scalar(value)
      ) {
        tempest_promotion_abort(
          "Promotion field {.field {field}} contains credential-like content."
        )
      }
    }
  }
  id_field <- tempest_promotion_record_id_field(record_class)
  ids <- vapply(rows, function(row) row[[id_field]], character(1))
  if (
    anyNA(ids) ||
      any(!nzchar(ids)) ||
      anyDuplicated(ids) ||
      !identical(ids, sort(ids, method = "radix"))
  ) {
    tempest_promotion_abort(
      "Promotion class {.val {record_class}} must use sorted unique identifiers."
    )
  }
  invisible(rows)
}

tempest_promotion_claim_support_from_row <- function(row) {
  prefix <- "tempest:"
  if (
    !rlang::is_string(row$evidence_span_id) ||
      is.na(row$evidence_span_id) ||
      !startsWith(row$evidence_span_id, prefix) ||
      nchar(row$evidence_span_id) <= nchar(prefix)
  ) {
    tempest_promotion_abort(
      "A promotion support row has an invalid evidence-span mapping."
    )
  }
  tempest_claim_support_from_list(list(
    claim_support_id = row$tempest_claim_support_id,
    claim_id = row$tempest_claim_id,
    evidence_span_id = substring(
      row$evidence_span_id,
      nchar(prefix) + 1L
    ),
    source_id = row$source_id,
    verification_status = row$pair_verification_status,
    support_score = row$support_score,
    rationale = row$rationale
  ))
}

tempest_promotion_validate_records <- function(records) {
  expected <- names(tempest_promotion_record_fields())
  if (
    !is.list(records) ||
      is.data.frame(records) ||
      !identical(names(records), expected)
  ) {
    tempest_promotion_abort(
      "Promotion records must contain exactly the current research classes."
    )
  }
  for (record_class in expected) {
    tempest_promotion_validate_rows(records[[record_class]], record_class)
  }

  source_ids <- vapply(
    records$Source,
    `[[`,
    character(1),
    "tempest_source_id"
  )
  claim_ids <- vapply(
    records$Claim,
    `[[`,
    character(1),
    "tempest_claim_id"
  )
  span_ids <- vapply(records$EvidenceSpan, `[[`, character(1), "id")
  tempest_span_ids <- vapply(
    records$EvidenceSpan,
    `[[`,
    character(1),
    "tempest_evidence_span_id"
  )
  support_ids <- vapply(
    records$ClaimSupport,
    `[[`,
    character(1),
    "tempest_claim_id"
  )
  support_span_ids <- vapply(
    records$ClaimSupport,
    `[[`,
    character(1),
    "evidence_span_id"
  )
  span_source_ids <- vapply(
    records$EvidenceSpan,
    `[[`,
    character(1),
    "source_id"
  )
  support_source_ids <- vapply(
    records$ClaimSupport,
    `[[`,
    character(1),
    "source_id"
  )
  if (
    length(setdiff(c(span_source_ids, support_source_ids), source_ids)) > 0L ||
      length(setdiff(support_ids, claim_ids)) > 0L ||
      length(setdiff(support_span_ids, span_ids)) > 0L
  ) {
    tempest_promotion_abort(
      "Promotion records contain a claim, source, or evidence reference outside the bundle."
    )
  }
  mapped_span_ids <- unname(vapply(
    tempest_span_ids,
    tempest_promotion_evidence_span_id,
    character(1)
  ))
  if (
    anyDuplicated(tempest_span_ids) ||
      anyDuplicated(mapped_span_ids) ||
      !identical(span_ids, mapped_span_ids)
  ) {
    tempest_promotion_abort(
      "Promotion evidence-span IDs do not retain their exact canonical mapping."
    )
  }
  expected_pairs <- paste(
    vapply(
      records$ClaimSupport,
      `[[`,
      character(1),
      "tempest_claim_id"
    ),
    support_span_ids,
    sep = "\u001f"
  )
  if (anyDuplicated(expected_pairs)) {
    tempest_promotion_abort(
      "Promotion records contain duplicate claim-by-evidence support pairs."
    )
  }
  for (claim in records$Claim) {
    supports <- Filter(
      function(support) {
        identical(support$tempest_claim_id, claim$tempest_claim_id)
      },
      records$ClaimSupport
    )
    summary <- tempest_promotion_support_summary(lapply(
      supports,
      tempest_promotion_claim_support_from_row
    ))
    if (
      !identical(claim$verification_status, summary$status) ||
        !isTRUE(all.equal(
          claim$support_score %||% NA_real_,
          summary$score,
          check.attributes = FALSE
        ))
    ) {
      tempest_promotion_abort(
        "A promoted Claim summary does not match its complete pair set."
      )
    }
  }
  invisible(records)
}

tempest_promotion_bundle_payload <- function(
  schema_build_digest,
  research_run_id,
  min_support_score,
  research_manifest,
  stage_records,
  proof,
  claim_ids,
  records
) {
  list(
    schema_version = tempest_promotion_schema_version,
    schema_build_digest = schema_build_digest,
    research_run_id = research_run_id,
    min_support_score = min_support_score,
    research_manifest = research_manifest,
    stage_records = stage_records,
    proof = proof,
    claim_ids = unname(as.list(claim_ids)),
    records = records
  )
}

tempest_promotion_validate_stage_bindings <- function(
  stage_records,
  claim_ids,
  records,
  min_support_score
) {
  stages <- vapply(stage_records, function(record) record@stage, character(1))
  statuses <- vapply(
    stage_records,
    function(record) record@status,
    character(1)
  )
  if (
    length(stage_records) == 0L ||
      any(!stages %in% c("extract_claims", "verify_claim_support")) ||
      any(statuses != "succeeded")
  ) {
    tempest_promotion_abort(
      "Promotion provenance must contain only succeeded extraction and verification records."
    )
  }
  output_ids <- function(stage) {
    records <- stage_records[stages == stage]
    unname(unlist(lapply(
      records,
      function(record) record@output_reference$ids
    )))
  }
  extracted_ids <- output_ids("extract_claims")
  support_ids <- output_ids("verify_claim_support")
  expected_support_ids <- vapply(
    records$ClaimSupport,
    `[[`,
    character(1),
    "tempest_claim_support_id"
  )
  claim_coverage <- vapply(
    claim_ids,
    function(claim_id) sum(extracted_ids == claim_id),
    integer(1)
  )
  if (
    any(claim_coverage != 1L) ||
      !identical(
        sort(support_ids, method = "radix"),
        sort(expected_support_ids, method = "radix")
      )
  ) {
    tempest_promotion_abort(
      "Promotion provenance does not bind every promoted claim-support record exactly once."
    )
  }
  artifact_ids <- sort(
    unique(vapply(
      stage_records,
      function(record) record@program_artifact_id,
      character(1)
    )),
    method = "radix"
  )
  expected_artifact_ids <- vapply(
    records$ProgramArtifact,
    `[[`,
    character(1),
    "id"
  )
  if (!identical(artifact_ids, expected_artifact_ids)) {
    tempest_promotion_abort(
      "Promotion ProgramArtifact rows do not match their exact stage provenance."
    )
  }
  invalid_supported <- vapply(
    records$ClaimSupport,
    function(support) {
      identical(support$pair_verification_status, "supported") &&
        (is.null(support$support_score) ||
          is.na(support$support_score) ||
          support$support_score < min_support_score)
    },
    logical(1)
  )
  if (any(invalid_supported)) {
    tempest_promotion_abort(
      "A supported promotion pair is below the verified stage threshold."
    )
  }
  threshold <- tempest_promotion_stage_support_threshold(stage_records)
  if (!identical(threshold, min_support_score)) {
    tempest_promotion_abort(
      "Promotion min_support_score is not the exact verified stage threshold."
    )
  }
  invisible(stage_records)
}

tempest_promotion_bundle_validation_message <- function(self) {
  tryCatch(
    {
      if (
        !identical(self@schema_version, tempest_promotion_schema_version) ||
          !identical(
            self@schema_build_digest,
            tempest_promotion_schema_build_digest
          ) ||
          !grepl("^sha256:[a-f0-9]{64}$", self@bundle_id)
      ) {
        stop("bundle identity or schema binding is invalid")
      }
      if (
        !rlang::is_string(self@research_run_id) ||
          is.na(self@research_run_id) ||
          !tempest_ledger_identifier_valid(self@research_run_id)
      ) {
        stop("research_run_id is invalid")
      }
      tempest_normalize_min_support_score(self@min_support_score)
      manifest <- tempest_research_manifest_from_record(self@research_manifest)
      if (
        !identical(manifest@research_run_id, self@research_run_id) ||
          !identical(manifest@status, "succeeded")
      ) {
        stop("research manifest is not the succeeded owning run")
      }
      stage_records <- tempest_stage_records_from_data(
        self@stage_records,
        allow_running = FALSE
      )
      tempest_stage_records_validate_manifest(stage_records, manifest)
      if (
        !is.character(self@claim_ids) ||
          is.object(self@claim_ids) ||
          is.null(names(self@claim_ids)) == FALSE ||
          anyNA(self@claim_ids) ||
          length(self@claim_ids) == 0L ||
          anyDuplicated(self@claim_ids) ||
          !identical(self@claim_ids, sort(self@claim_ids, method = "radix"))
      ) {
        stop("claim_ids must be a sorted unique non-empty character vector")
      }
      proof <- tempest_promotion_restore_proof(self@proof)
      tempest_promotion_validate_stage_proof(stage_records, proof)
      tempest_promotion_validate_records(self@records)
      expected_records <- tempest_promotion_records_from_proof(
        proof,
        stage_records,
        self@claim_ids,
        self@min_support_score
      )
      if (
        !identical(
          tempest_promotion_digest(self@records),
          tempest_promotion_digest(expected_records)
        )
      ) {
        stop("promotion records do not match the exact stage-bound proof")
      }
      record_claim_ids <- vapply(
        self@records$Claim,
        `[[`,
        character(1),
        "tempest_claim_id"
      )
      if (!identical(record_claim_ids, self@claim_ids)) {
        stop("claim_ids do not match the promoted Claim rows")
      }
      tempest_promotion_validate_stage_bindings(
        stage_records,
        self@claim_ids,
        self@records,
        self@min_support_score
      )
      payload <- tempest_promotion_bundle_payload(
        self@schema_build_digest,
        self@research_run_id,
        self@min_support_score,
        self@research_manifest,
        self@stage_records,
        self@proof,
        self@claim_ids,
        self@records
      )
      if (!identical(self@bundle_id, tempest_promotion_digest(payload))) {
        stop("bundle_id does not match the exact bundle payload")
      }
      NULL
    },
    error = conditionMessage
  )
}

#' A deterministic Tempest evidence-promotion proposal
#'
#' @keywords internal
TempestPromotionBundle <- S7::new_class(
  "TempestPromotionBundle",
  properties = list(
    schema_version = S7::new_property(S7::class_integer),
    bundle_id = S7::new_property(S7::class_character),
    schema_build_digest = S7::new_property(S7::class_character),
    research_run_id = S7::new_property(S7::class_character),
    min_support_score = S7::new_property(S7::class_numeric),
    research_manifest = S7::new_property(S7::class_list),
    stage_records = S7::new_property(S7::class_list),
    proof = S7::new_property(S7::class_list),
    claim_ids = S7::new_property(S7::class_character),
    records = S7::new_property(S7::class_list)
  ),
  validator = tempest_promotion_bundle_validation_message
)

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
      "A Tempest evidence-span ID cannot map to a canonical Graft ID."
    )
  }
  mapped <- paste0("tempest:", tempest_evidence_span_id)
  round_trip <- substr(mapped, nchar("tempest:") + 1L, nchar(mapped))
  if (!identical(round_trip, tempest_evidence_span_id)) {
    tempest_promotion_abort(
      "A Tempest evidence-span ID failed its canonical Graft round trip."
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
    unique(vapply(spans, function(span) span@source_id, character(1))),
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
    ))
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
  support_rows
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
  tempest_promotion_validate_proof_credentials(
    resource_rows,
    claim_rows,
    span_rows,
    support_rows
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
    ))
  )
  if (
    !identical(
      round_trip,
      list(
        resources = resource_rows,
        claims = claim_rows,
        evidence_spans = span_rows,
        claim_supports = support_rows
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
    claim_supports = supports
  )
  id_accessors <- list(
    resources = function(value) value@resource_id,
    claims = function(value) value@claim_id,
    evidence_spans = function(value) value@evidence_span_id,
    claim_supports = function(value) value@claim_support_id
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
    unique(vapply(spans, function(span) span@source_id, character(1))),
    method = "radix"
  )
  list(
    resources = resource_ids,
    claims = claim_ids,
    evidence_spans = span_ids,
    claim_supports = sort(support_ids, method = "radix")
  )
}

tempest_promotion_validate_stage_proof <- function(stage_records, proof) {
  expected_ids <- tempest_promotion_expected_proof_ids(stage_records, proof)
  actual_ids <- list(
    resources = names(proof$resources),
    claims = names(proof$claims),
    evidence_spans = names(proof$evidence_spans),
    claim_supports = names(proof$claim_supports)
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

tempest_promotion_storm_result_fields <- function() {
  c(
    "title",
    "perspectives",
    "experts",
    "outline",
    "draft_md",
    "report_md",
    "manifest",
    "state",
    "workspace",
    "retriever",
    "output_dir"
  )
}

tempest_promotion_report_reference <- function(manifest, report_md) {
  reference <- manifest@deliverables$report_md %||% NULL
  expected <- c(
    tempest_product_report_reference(report_md),
    list(status = "durable")
  )
  if (!identical(reference, expected)) {
    tempest_promotion_abort(
      "The research product does not bind its exact durable report."
    )
  }
  tryCatch(
    tempest_product_report_reference_validate(
      reference[c("report_id", "sha256")],
      report_md
    ),
    error = function(error) {
      tempest_promotion_abort(
        "The research product report does not match its exact content reference.",
        parent = error
      )
    }
  )
  reference[c("report_id", "sha256")]
}

tempest_promotion_assert_sealed_workspace <- function(workspace) {
  if (
    !inherits(workspace, "ResearchWorkspace") ||
      !identical(
        tempest_research_workspace_mutation_state(workspace),
        "sealed"
      )
  ) {
    tempest_promotion_abort(
      "Promotion requires the completed product's sealed ResearchWorkspace."
    )
  }
  invisible(workspace)
}

tempest_promotion_storm_context <- function(research) {
  if (
    !is.list(research) ||
      is.object(research) ||
      is.data.frame(research) ||
      !identical(names(research), tempest_promotion_storm_result_fields())
  ) {
    tempest_promotion_abort(
      paste0(
        "{.arg research} must be one exact completed result returned by ",
        "{.fn tempest_run} or a succeeded TempestSession."
      )
    )
  }
  manifest <- research$manifest
  if (
    !S7::S7_inherits(manifest, TempestResearchManifest) ||
      !identical(manifest@mode, "storm") ||
      !identical(manifest@status, "succeeded")
  ) {
    tempest_promotion_abort(
      "A STORM promotion requires its succeeded STORM research Manifest."
    )
  }
  state <- tryCatch(
    tempest_storm_state_validate(research$state),
    error = function(error) {
      tempest_promotion_abort(
        "A STORM promotion requires its exact current product state.",
        parent = error
      )
    }
  )
  if (!tempest_storm_state_is_complete(state)) {
    tempest_promotion_abort(
      "A STORM promotion requires every product stage and its final report."
    )
  }
  projected <- list(
    title = state$title,
    perspectives = state$perspectives,
    experts = state$experts,
    outline = state$outline,
    draft_md = state$draft_md,
    report_md = state$report_md
  )
  if (!identical(research[names(projected)], projected)) {
    tempest_promotion_abort(
      "The STORM result projections do not match its exact product state."
    )
  }
  if (
    !inherits(research$retriever, "TempestRetriever") ||
      !identical(research$retriever$workspace, research$workspace) ||
      !S7::S7_inherits(research$retriever$config, TempestConfig)
  ) {
    tempest_promotion_abort(
      "The STORM result does not retain its exact product retriever identity."
    )
  }
  if (
    !is.null(research$output_dir) &&
      (!rlang::is_string(research$output_dir) ||
        is.na(research$output_dir) ||
        !nzchar(research$output_dir))
  ) {
    tempest_promotion_abort(
      "The STORM result has an invalid product output location."
    )
  }
  tempest_promotion_assert_sealed_workspace(research$workspace)
  report_reference <- tempest_promotion_report_reference(
    manifest,
    research$report_md
  )
  list(
    manifest = manifest,
    stage_records = state$stage_records,
    workspace = research$workspace,
    report_md = research$report_md,
    report_reference = report_reference,
    config = research$retriever$config,
    experts = state$experts,
    expert_sessions = list(),
    product_state = state
  )
}

tempest_promotion_costorm_context <- function(research) {
  if (!inherits(research, "TempestSession") || !inherits(research, "R6")) {
    tempest_promotion_abort(
      "{.arg research} must be a succeeded TempestSession."
    )
  }
  manifest <- research$manifest
  if (
    !S7::S7_inherits(manifest, TempestResearchManifest) ||
      !identical(manifest@mode, "costorm") ||
      !identical(manifest@status, "succeeded")
  ) {
    tempest_promotion_abort(
      "A Co-STORM promotion requires its succeeded Co-STORM research Manifest."
    )
  }
  stage_records <- tryCatch(
    {
      if (length(tempest_session_pending_deputy_runs(research)) > 0L) {
        tempest_promotion_abort(
          "A Co-STORM promotion cannot retain pending Deputy execution."
        )
      }
      tempest_session_agent_completion_assert_quiescent(research)
      tempest_session_async_work_assert_quiescent(research)
      tempest_stage_records_validate(
        tempest_session_stage_records(research),
        allow_running = FALSE
      )
    },
    error = function(error) {
      if (inherits(error, "tempest_promotion_error")) {
        stop(error)
      }
      tempest_promotion_abort(
        "A Co-STORM promotion requires quiescent terminal execution.",
        parent = error
      )
    }
  )
  execution_identity <- tryCatch(
    {
      deputy_traces <- tempest_session_deputy_traces(research)
      experts <- research$experts
      expert_sessions <- tempest_expert_sessions_snapshot(research)
      tempest_product_authority_validate_stage_records(
        manifest,
        stage_records,
        deputy_traces = deputy_traces,
        expert_ids = tempest_product_authority_expert_ids(experts),
        expert_sessions = expert_sessions
      )
      list(
        experts = experts,
        expert_sessions = expert_sessions
      )
    },
    error = function(error) {
      tempest_promotion_abort(
        "A Co-STORM promotion requires its exact live Deputy trace ledger.",
        parent = error
      )
    }
  )
  tempest_promotion_assert_sealed_workspace(research$workspace)
  report_md <- tryCatch(
    tempest_session_report_md(research),
    error = function(error) {
      tempest_promotion_abort(
        "A Co-STORM promotion requires its exact committed report.",
        parent = error
      )
    }
  )
  report_reference <- tempest_promotion_report_reference(manifest, report_md)
  list(
    manifest = manifest,
    stage_records = stage_records,
    workspace = research$workspace,
    report_md = report_md,
    report_reference = report_reference,
    config = research$config,
    experts = execution_identity$experts,
    expert_sessions = execution_identity$expert_sessions,
    product_state = list(title = research$title)
  )
}

tempest_promotion_research_context <- function(research) {
  context <- tryCatch(
    {
      if (inherits(research, "TempestSession")) {
        tempest_promotion_costorm_context(research)
      } else {
        tempest_promotion_storm_context(research)
      }
    },
    error = function(error) {
      if (inherits(error, "tempest_promotion_error")) {
        stop(error)
      }
      tempest_promotion_abort(
        "Could not read the exact completed research product.",
        parent = error
      )
    }
  )
  tryCatch(
    tempest_product_authority_validate(
      manifest = context$manifest,
      stage_records = context$stage_records,
      workspace = context$workspace,
      report_md = context$report_md,
      report_reference = context$report_reference,
      config = context$config,
      experts = context$experts,
      expert_sessions = context$expert_sessions,
      product_state = context$product_state,
      require_publishable = TRUE
    ),
    error = function(error) {
      tempest_promotion_abort(
        "The completed research product lacks exact publication authority.",
        parent = error
      )
    }
  )
  context
}

#' Build a deterministic proposal for reviewed Graft promotion
#'
#' `tempest_promotion_bundle()` validates a completed research product and
#' packages only promotable claims and their exact pair-level evidence. It does
#' not write accepted knowledge. Review [tempest_graft_plan()] and call
#' `graft::graft_commit()` explicitly to exercise acceptance authority.
#'
#' @param research A completed result returned by [tempest_run()] or a succeeded
#'   `TempestSession` returned by [tempest_session()]. Tempest requires the
#'   product's exact sealed Workspace, terminal execution records, committed
#'   report, configuration, and publication authority.
#' @param claim_ids Optional exact claim selection. By default all supported or
#'   partially supported claims are selected. Selection must be closed over
#'   every output bound by each retained extraction and verification record;
#'   Tempest rejects a partial stage-output selection.
#' @return A deterministic `TempestPromotionBundle` proposal.
#' @export
tempest_promotion_bundle <- function(research, claim_ids = NULL) {
  context <- tempest_promotion_research_context(research)
  workspace <- context$workspace
  manifest <- context$manifest
  stage_records <- context$stage_records
  min_support_score <- tempest_promotion_stage_support_threshold(stage_records)
  tryCatch(
    {
      tempest_stage_records_validate_workspace(
        stage_records,
        workspace,
        min_support_score = min_support_score
      )
      tempest_stage_records_validate_workspace_coverage(
        stage_records,
        workspace,
        require_extraction = TRUE,
        require_verification = TRUE
      )
    },
    error = function(error) {
      tempest_promotion_abort(
        "Stage records do not prove the exact completed research product."
      )
    }
  )

  claims <- workspace$list_proposed_claims()
  all_claim_ids <- vapply(claims, function(claim) claim@claim_id, character(1))
  if (length(all_claim_ids) == 0L) {
    tempest_promotion_abort("Promotion requires at least one research claim.")
  }
  supports <- tempest_promotion_supports(workspace)
  if (is.null(claim_ids)) {
    selected <- vapply(
      claims,
      function(claim) {
        claim_supports <- Filter(
          function(support) identical(support@claim_id, claim@claim_id),
          supports
        )
        tempest_promotion_claim_promotable(
          claim_supports,
          min_support_score
        )
      },
      logical(1)
    )
    claim_ids <- all_claim_ids[selected]
  }
  if (
    !is.character(claim_ids) ||
      is.object(claim_ids) ||
      !is.null(names(claim_ids)) ||
      anyNA(claim_ids) ||
      length(claim_ids) == 0L ||
      anyDuplicated(claim_ids) ||
      length(setdiff(claim_ids, all_claim_ids)) > 0L
  ) {
    tempest_promotion_abort(
      "{.arg claim_ids} must select unique claims present in the workspace."
    )
  }
  claim_ids <- sort(claim_ids, method = "radix")
  claims <- lapply(claim_ids, workspace$get_proposed_claim)
  if (
    any(lengths(lapply(claims, function(claim) claim@evidence_span_ids)) == 0L)
  ) {
    tempest_promotion_abort(
      "Every promotion claim requires at least one exact evidence span."
    )
  }

  selected_supports <- Filter(
    function(support) support@claim_id %in% claim_ids,
    supports
  )
  expected_pairs <- unname(unlist(lapply(claims, function(claim) {
    paste(claim@claim_id, claim@evidence_span_ids, sep = "\u001f")
  })))
  observed_pairs <- vapply(
    selected_supports,
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
      paste0(
        "Promotion requires exactly one pair-level assessment for every ",
        "selected claim and linked evidence span, with no extras."
      )
    )
  }

  selected_support_ids <- vapply(
    selected_supports,
    function(support) support@claim_support_id,
    character(1)
  )
  all_support_ids <- vapply(
    supports,
    function(support) support@claim_support_id,
    character(1)
  )
  relevant_stage_records <- tempest_promotion_stage_selection(
    stage_records,
    claim_ids,
    selected_support_ids,
    sort(all_claim_ids, method = "radix"),
    sort(all_support_ids, method = "radix")
  )
  artifact_ids <- sort(
    unique(vapply(
      relevant_stage_records,
      function(record) record@program_artifact_id,
      character(1)
    )),
    method = "radix"
  )

  spans <- unique(unname(unlist(lapply(
    claims,
    function(claim) claim@evidence_span_ids
  ))))
  spans <- sort(spans, method = "radix")
  spans <- stats::setNames(lapply(spans, workspace$get_evidence_span), spans)
  if (any(vapply(spans, is.null, logical(1)))) {
    tempest_promotion_abort("A promotion claim cites an absent evidence span.")
  }
  resources <- stats::setNames(
    lapply(
      sort(
        unique(vapply(spans, function(span) span@source_id, character(1))),
        method = "radix"
      ),
      workspace$get_retrieved_resource
    ),
    sort(
      unique(vapply(spans, function(span) span@source_id, character(1))),
      method = "radix"
    )
  )
  if (any(vapply(resources, is.null, logical(1)))) {
    tempest_promotion_abort("An evidence span cites an absent source.")
  }
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
  for (span in spans) {
    if (
      !rlang::is_string(span@quote) ||
        is.na(span@quote) ||
        !nzchar(span@quote)
    ) {
      tempest_promotion_abort(
        "Every promoted evidence span requires an exact non-empty excerpt."
      )
    }
  }
  for (claim in claims) {
    for (span_id in claim@evidence_span_ids) {
      if (!spans[[span_id]]@source_id %in% claim@source_ids) {
        tempest_promotion_abort(
          paste0(
            "Every promoted evidence span must come from a source cited by ",
            "its claim."
          )
        )
      }
    }
  }

  support_by_pair <- stats::setNames(selected_supports, observed_pairs)
  for (claim in claims) {
    claim_supports <- support_by_pair[paste(
      claim@claim_id,
      claim@evidence_span_ids,
      sep = "\u001f"
    )]
    summary <- tempest_promotion_support_summary(claim_supports)
    if (
      !tempest_promotion_claim_promotable(
        claim_supports,
        min_support_score
      )
    ) {
      tempest_promotion_abort(
        paste0(
          "Every promoted claim must derive supported or partially ",
          "supported status and have a qualifying threshold-supported pair."
        )
      )
    }
    if (
      !identical(claim@verification_status, summary$status) ||
        !isTRUE(all.equal(
          claim@support_score,
          summary$score,
          check.attributes = FALSE
        ))
    ) {
      tempest_promotion_abort(
        "A stored Claim summary is inconsistent with its authoritative pair set."
      )
    }
    for (support in claim_supports) {
      span <- spans[[support@evidence_span_id]]
      if (!identical(support@source_id, span@source_id)) {
        tempest_promotion_abort(
          "A claim-support assessment does not match its evidence-span source."
        )
      }
    }
  }

  source_rows <- unname(lapply(names(resources), function(source_id) {
    tempest_promotion_source_row(
      resources[[source_id]],
      source_hashes[[source_id]]
    )
  }))
  proof <- tempest_promotion_proof_data(
    workspace,
    relevant_stage_records,
    supports
  )
  restored_proof <- tempest_promotion_restore_proof(proof)
  tempest_promotion_validate_stage_proof(
    relevant_stage_records,
    restored_proof
  )
  records <- tempest_promotion_records_from_proof(
    restored_proof,
    relevant_stage_records,
    claim_ids,
    min_support_score
  )
  tempest_promotion_validate_records(records)

  manifest_data <- tempest_research_manifest_record(manifest)
  stage_data <- tempest_stage_records_data(relevant_stage_records)
  payload <- tempest_promotion_bundle_payload(
    tempest_promotion_schema_build_digest,
    manifest@research_run_id,
    min_support_score,
    manifest_data,
    stage_data,
    proof,
    claim_ids,
    records
  )
  TempestPromotionBundle(
    schema_version = tempest_promotion_schema_version,
    bundle_id = tempest_promotion_digest(payload),
    schema_build_digest = tempest_promotion_schema_build_digest,
    research_run_id = manifest@research_run_id,
    min_support_score = min_support_score,
    research_manifest = manifest_data,
    stage_records = stage_data,
    proof = proof,
    claim_ids = claim_ids,
    records = records
  )
}

tempest_promotion_bundle_data <- function(bundle) {
  if (!S7::S7_inherits(bundle, TempestPromotionBundle)) {
    tempest_promotion_abort(
      "{.arg bundle} must be created by tempest_promotion_bundle()."
    )
  }
  S7::validate(bundle)
  c(
    list(bundle_id = bundle@bundle_id),
    tempest_promotion_bundle_payload(
      bundle@schema_build_digest,
      bundle@research_run_id,
      bundle@min_support_score,
      bundle@research_manifest,
      bundle@stage_records,
      bundle@proof,
      bundle@claim_ids,
      bundle@records
    )
  )
}

tempest_promotion_bundle_from_data <- function(data) {
  fields <- c(
    "bundle_id",
    names(tempest_promotion_bundle_payload(
      tempest_promotion_schema_build_digest,
      "run",
      0.7,
      list(),
      list(),
      list(),
      "claim",
      list()
    ))
  )
  if (
    !is.list(data) ||
      is.data.frame(data) ||
      is.null(names(data)) ||
      anyNA(names(data)) ||
      anyDuplicated(names(data)) ||
      !identical(names(data), fields)
  ) {
    tempest_promotion_abort(
      "Promotion data must contain exactly the current bundle fields."
    )
  }
  if (
    !is.integer(data$schema_version) ||
      is.object(data$schema_version) ||
      !is.null(names(data$schema_version)) ||
      length(data$schema_version) != 1L ||
      is.na(data$schema_version) ||
      !is.finite(data$schema_version) ||
      data$schema_version != tempest_promotion_schema_version
  ) {
    tempest_promotion_abort(
      "Promotion schema_version must be the exact current numeric version."
    )
  }
  if (
    !is.numeric(data$min_support_score) ||
      is.object(data$min_support_score) ||
      !is.null(names(data$min_support_score)) ||
      length(data$min_support_score) != 1L ||
      is.na(data$min_support_score) ||
      !is.finite(data$min_support_score)
  ) {
    tempest_promotion_abort(
      "Promotion min_support_score must be one exact JSON number."
    )
  }
  if (
    !is.list(data$claim_ids) ||
      is.data.frame(data$claim_ids) ||
      !is.null(names(data$claim_ids)) ||
      length(data$claim_ids) == 0L ||
      !all(vapply(
        data$claim_ids,
        function(value) {
          rlang::is_string(value) && !is.na(value)
        },
        logical(1)
      ))
  ) {
    tempest_promotion_abort(
      "Promotion claim_ids must be an unnamed JSON array of scalar strings."
    )
  }
  claim_ids <- vapply(data$claim_ids, identity, character(1))
  tryCatch(
    TempestPromotionBundle(
      schema_version = data$schema_version,
      bundle_id = data$bundle_id,
      schema_build_digest = data$schema_build_digest,
      research_run_id = data$research_run_id,
      min_support_score = tempest_normalize_min_support_score(
        data$min_support_score
      ),
      research_manifest = data$research_manifest,
      stage_records = data$stage_records,
      proof = data$proof,
      claim_ids = claim_ids,
      records = data$records
    ),
    error = function(error) {
      tempest_promotion_abort(
        "Promotion data failed exact current-schema validation."
      )
    }
  )
}

tempest_promotion_receipt_payload <- function(
  bundle_id,
  plan_id,
  plan_digest,
  batch_id,
  store_id,
  schema_build_digest,
  snapshot,
  counts,
  record_revisions
) {
  list(
    schema_version = tempest_promotion_schema_version,
    bundle_id = bundle_id,
    plan_id = plan_id,
    plan_digest = plan_digest,
    batch_id = batch_id,
    store_id = store_id,
    schema_build_digest = schema_build_digest,
    snapshot = snapshot,
    counts = counts,
    record_revisions = record_revisions
  )
}

tempest_promotion_receipt_snapshot_fields <- function() {
  c(
    "schema_version",
    "snapshot_id",
    "store_id",
    "store_format_version",
    "schema_build_digest",
    "commit_order",
    "batch_id",
    "committed_at",
    "history_complete"
  )
}

tempest_promotion_receipt_revision_fields <- function() {
  c(
    "class",
    "record_id",
    "revision_id",
    "revision_number",
    "action",
    "batch_id",
    "content_digest",
    "schema_build_digest"
  )
}

tempest_promotion_receipt_classes <- function() {
  sort(names(tempest_promotion_record_fields()), method = "radix")
}

tempest_promotion_receipt_whole_number <- function(
  value,
  minimum = 0
) {
  is.numeric(value) &&
    !is.object(value) &&
    is.null(names(value)) &&
    length(value) == 1L &&
    !is.na(value) &&
    is.finite(value) &&
    value >= minimum &&
    value == trunc(value)
}

tempest_promotion_receipt_store_id_valid <- function(value) {
  rlang::is_string(value) &&
    !is.na(value) &&
    grepl(
      paste0(
        "^graft-store-[0-9]{8}T[0-9]{6}\\.[0-9]{6}-",
        "[a-z0-9]{20}$"
      ),
      value
    )
}

tempest_promotion_receipt_validate_snapshot <- function(self) {
  snapshot <- self@snapshot
  tempest_promotion_assert_exact_row(
    snapshot,
    tempest_promotion_receipt_snapshot_fields(),
    "Promotion receipt snapshot"
  )
  if (
    !identical(snapshot$schema_version, 1L) ||
      !rlang::is_string(snapshot$snapshot_id) ||
      is.na(snapshot$snapshot_id) ||
      !grepl("^sha256:[a-f0-9]{64}$", snapshot$snapshot_id) ||
      !identical(snapshot$store_id, self@store_id) ||
      !identical(snapshot$store_format_version, "3.0.0") ||
      !identical(snapshot$schema_build_digest, self@schema_build_digest) ||
      !tempest_promotion_receipt_whole_number(
        snapshot$commit_order,
        minimum = 1
      ) ||
      !identical(snapshot$batch_id, self@batch_id) ||
      !tempest_ledger_timestamp_valid(snapshot$committed_at) ||
      !identical(snapshot$history_complete, TRUE)
  ) {
    tempest_promotion_abort(
      "Promotion receipt snapshot values are malformed or cross-bound incorrectly."
    )
  }
  invisible(snapshot)
}

tempest_promotion_receipt_validate_counts <- function(counts) {
  actions <- c("inserted", "updated", "matched", "observed")
  classes <- tempest_promotion_receipt_classes()
  if (
    !is.list(counts) ||
      is.data.frame(counts) ||
      !identical(names(counts), actions)
  ) {
    tempest_promotion_abort(
      "Promotion receipt counts must use the exact current action fields."
    )
  }
  for (action in actions) {
    values <- counts[[action]]
    if (
      !is.list(values) ||
        is.data.frame(values) ||
        !identical(names(values), classes) ||
        !all(vapply(
          values,
          tempest_promotion_receipt_whole_number,
          logical(1)
        ))
    ) {
      tempest_promotion_abort(
        "Promotion receipt count rows must be exact nonnegative class counts."
      )
    }
  }
  expected_observed <- Map(
    function(inserted, updated, matched) inserted + updated + matched,
    counts$inserted,
    counts$updated,
    counts$matched
  )
  expected_observed <- lapply(expected_observed, as.integer)
  if (!identical(expected_observed, counts$observed)) {
    tempest_promotion_abort(
      "Promotion receipt observed counts do not reconcile with plan actions."
    )
  }
  invisible(counts)
}

tempest_promotion_receipt_validate_revisions <- function(self) {
  revisions <- self@record_revisions
  classes <- tempest_promotion_receipt_classes()
  if (
    !is.list(revisions) ||
      is.data.frame(revisions) ||
      !is.null(names(revisions)) ||
      length(revisions) == 0L
  ) {
    tempest_promotion_abort(
      "Promotion receipt revisions must be an unnamed non-empty row list."
    )
  }
  keys <- character(length(revisions))
  actions <- character(length(revisions))
  revision_classes <- character(length(revisions))
  for (index in seq_along(revisions)) {
    revision <- tempest_promotion_assert_exact_row(
      revisions[[index]],
      tempest_promotion_receipt_revision_fields(),
      "Promotion receipt revision"
    )
    strings <- c(
      revision$class,
      revision$record_id,
      revision$revision_id,
      revision$action,
      revision$batch_id,
      revision$content_digest,
      revision$schema_build_digest
    )
    if (
      !all(vapply(strings, rlang::is_string, logical(1))) ||
        anyNA(strings) ||
        any(!nzchar(strings)) ||
        !revision$class %in% classes ||
        !revision$action %in% c("insert", "update", "match") ||
        !tempest_ledger_identifier_valid(revision$record_id) ||
        !grepl("^graft:[A-Z0-9]+$", revision$revision_id) ||
        !grepl("^graft:[A-Z0-9]+$", revision$batch_id) ||
        !grepl("^sha256:[a-f0-9]{64}$", revision$content_digest) ||
        !identical(revision$schema_build_digest, self@schema_build_digest) ||
        !tempest_promotion_receipt_whole_number(
          revision$revision_number,
          minimum = 1
        ) ||
        (revision$action %in%
          c("insert", "update") &&
          !identical(revision$batch_id, self@batch_id))
    ) {
      tempest_promotion_abort(
        "Promotion receipt revision values are malformed or cross-bound incorrectly."
      )
    }
    keys[[index]] <- paste(revision$class, revision$record_id, sep = "\u001f")
    actions[[index]] <- revision$action
    revision_classes[[index]] <- revision$class
  }
  if (
    anyDuplicated(keys) ||
      anyDuplicated(vapply(
        revisions,
        `[[`,
        character(1),
        "revision_id"
      )) ||
      !identical(keys, sort(keys, method = "radix"))
  ) {
    tempest_promotion_abort(
      "Promotion receipt revisions must use sorted unique record identities."
    )
  }
  count_for <- function(action, record_class) {
    sum(actions == action & revision_classes == record_class)
  }
  for (record_class in classes) {
    expected <- c(
      inserted = count_for("insert", record_class),
      updated = count_for("update", record_class),
      matched = count_for("match", record_class),
      observed = sum(revision_classes == record_class)
    )
    actual <- vapply(
      c("inserted", "updated", "matched", "observed"),
      function(action) as.integer(self@counts[[action]][[record_class]]),
      integer(1)
    )
    if (!identical(as.integer(expected), unname(actual))) {
      tempest_promotion_abort(
        "Promotion receipt revisions do not reconcile with class action counts."
      )
    }
  }
  invisible(revisions)
}

tempest_promotion_receipt_validation_message <- function(self) {
  tryCatch(
    {
      digests <- c(
        self@receipt_id,
        self@bundle_id,
        self@plan_digest,
        self@schema_build_digest
      )
      if (
        !identical(self@schema_version, tempest_promotion_schema_version) ||
          !all(grepl("^sha256:[a-f0-9]{64}$", digests)) ||
          !identical(
            self@schema_build_digest,
            tempest_promotion_schema_build_digest
          )
      ) {
        stop("receipt identity or schema binding is invalid")
      }
      for (field in c("plan_id", "batch_id", "store_id")) {
        value <- S7::prop(self, field)
        if (!rlang::is_string(value) || is.na(value) || !nzchar(value)) {
          stop(paste(field, "is invalid"))
        }
      }
      if (
        !grepl("^graft:[A-Z0-9]+$", self@plan_id) ||
          !identical(self@batch_id, self@plan_id) ||
          !tempest_promotion_receipt_store_id_valid(self@store_id)
      ) {
        stop("receipt plan, batch, or store identity is malformed")
      }
      tempest_promotion_receipt_validate_snapshot(self)
      tempest_promotion_receipt_validate_counts(self@counts)
      tempest_promotion_receipt_validate_revisions(self)
      payload <- tempest_promotion_receipt_payload(
        self@bundle_id,
        self@plan_id,
        self@plan_digest,
        self@batch_id,
        self@store_id,
        self@schema_build_digest,
        self@snapshot,
        self@counts,
        self@record_revisions
      )
      tempest_product_canonical_value(payload)
      if (!identical(self@receipt_id, tempest_promotion_digest(payload))) {
        stop("receipt_id does not match the exact receipt payload")
      }
      NULL
    },
    error = conditionMessage
  )
}

#' Proof that one reviewed promotion plan was accepted
#'
#' @keywords internal
TempestPromotionReceipt <- S7::new_class(
  "TempestPromotionReceipt",
  properties = list(
    schema_version = S7::new_property(S7::class_integer),
    receipt_id = S7::new_property(S7::class_character),
    bundle_id = S7::new_property(S7::class_character),
    plan_id = S7::new_property(S7::class_character),
    plan_digest = S7::new_property(S7::class_character),
    batch_id = S7::new_property(S7::class_character),
    store_id = S7::new_property(S7::class_character),
    schema_build_digest = S7::new_property(S7::class_character),
    snapshot = S7::new_property(S7::class_list),
    counts = S7::new_property(S7::class_list),
    record_revisions = S7::new_property(S7::class_list)
  ),
  validator = tempest_promotion_receipt_validation_message
)

tempest_promotion_receipt_data <- function(receipt) {
  if (!S7::S7_inherits(receipt, TempestPromotionReceipt)) {
    tempest_promotion_receipt_abort(
      "{.arg receipt} must be a TempestPromotionReceipt."
    )
  }
  S7::validate(receipt)
  c(
    list(receipt_id = receipt@receipt_id),
    tempest_promotion_receipt_payload(
      receipt@bundle_id,
      receipt@plan_id,
      receipt@plan_digest,
      receipt@batch_id,
      receipt@store_id,
      receipt@schema_build_digest,
      receipt@snapshot,
      receipt@counts,
      receipt@record_revisions
    )
  )
}
