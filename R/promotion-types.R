# Typed, review-only promotion bundles for provisional scientific evidence

tempest_promotion_schema_version <- 2L
tempest_promotion_schema_build_digest <-
  "sha256:91f476d515dcd35259bad3fee9c0428dd272724a7ccf6edec3ddf064ebe2b430"

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


tempest_promotion_digest <- function(value) {
  tempest_product_sha256_id(value)
}

tempest_promotion_nullable <- function(value) {
  tempest_product_nullable(value)
}

tempest_promotion_nullable_tree <- function(value) {
  tempest_product_nullable_tree(value)
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
  tempest_resource_record_fields()
}

tempest_promotion_evidence_span_proof_fields <- function() {
  tempest_evidence_span_record_fields()
}

tempest_promotion_claim_support_proof_fields <- function() {
  tempest_claim_support_record_fields()
}

tempest_promotion_proof_fields <- function() {
  c(
    "resources",
    "claims",
    "evidence_spans",
    "claim_supports",
    "claim_reviews"
  )
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

tempest_promotion_research_context <- function(research) {
  tempest_completed_product_context(
    research = research,
    abort = tempest_promotion_abort,
    boundary_class = "tempest_promotion_error",
    purpose = "Promotion"
  )
}

#' Build a deterministic proposal for reviewed Graft promotion
#'
#' `tempest_promotion_bundle()` validates a completed research product and
#' packages promotable claims and their exact pair-level evidence, with
#' advisory claim reviews and their retained source dossiers. Reviews remain
#' separate from scientific claims and claim support; they do not change
#' promotion eligibility. It does not write accepted knowledge. Publish with
#' [tempest_publish_artifact_research()] and call `graft::graft_accept()` after
#' host review. Loose Workspace, Manifest, or StageRecord tuples are not
#' accepted. The schema-2 payload retains complete review context while keeping
#' the terminal report as an eligibility gate rather than copying it.
#'
#' @param research A completed result returned by [tempest_run()] or a succeeded
#'   `TempestSession` returned by [tempest_session()]. Tempest requires the
#'   product's exact sealed Workspace, terminal execution records, committed
#'   report, configuration, and publication authority.
#' @param claim_ids Optional exact claim selection. By default all supported or
#'   partially supported claims are selected. Selection must be closed over
#'   every output bound by each retained extraction and verification record;
#'   Tempest rejects a partial stage-output selection. Advisory reviews for
#'   claims in the selected stage-bound proof are retained; reviews of other
#'   claims remain with the complete research product.
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
