# R/ledger-types.R
# S7 value records for the evidence ledger. Validation happens at construction:
# illegal enum values and out-of-range scores throw immediately.

# --- property helpers ---------------------------------------------------------

prop_enum <- function(choices, default = choices[[1]]) {
  S7::new_property(
    S7::class_character,
    default = default,
    validator = function(value) {
      if (length(value) != 1 || is.na(value) || !value %in% choices) {
        sprintf("must be one of: %s", paste(choices, collapse = ", "))
      }
    }
  )
}

prop_score <- function() {
  S7::new_property(
    S7::class_numeric,
    default = NA_real_,
    validator = function(value) {
      if (length(value) != 1) {
        return("must be length 1")
      }
      if (!is.na(value) && (value < 0 || value > 1)) return("must be in [0, 1]")
    }
  )
}

prop_chr <- function(default = NA_character_) {
  S7::new_property(S7::class_character, default = default)
}

#' @keywords internal
prop_score_default <- function(default) {
  S7::new_property(
    S7::class_numeric,
    default = default,
    validator = function(value) {
      if (length(value) != 1) {
        return("must be length 1")
      }
      if (!is.na(value) && (value < 0 || value > 1)) return("must be in [0, 1]")
    }
  )
}

prop_chr_vec <- function() {
  S7::new_property(S7::class_character, default = character())
}

# --- enums --------------------------------------------------------------------

tempest_claim_types <- function() {
  c(
    "finding",
    "definition",
    "method",
    "limitation",
    "contradiction",
    "open_question"
  )
}

tempest_verification_statuses <- function() {
  c(
    "unverified",
    "supported",
    "partially_supported",
    "unsupported",
    "contradicted",
    "unverifiable"
  )
}

# --- claim --------------------------------------------------------------------

#' Evidence ledger claim record (S7)
#' @keywords internal
tempest_claim <- S7::new_class(
  "tempest_claim",
  properties = list(
    claim_id = prop_chr(),
    claim_text = S7::class_character,
    claim_type = prop_enum(tempest_claim_types(), "finding"),
    source_ids = prop_chr_vec(),
    evidence_span_ids = prop_chr_vec(),
    supporting_quotes = S7::new_property(S7::class_list, default = list()),
    contradicting_source_ids = prop_chr_vec(),
    contradiction_note = prop_chr(),
    confidence = prop_enum(c("low", "medium", "high"), "medium"),
    support_score = prop_score(),
    contradiction_score = prop_score(),
    source_quality_score = prop_score(),
    retrieval_query = prop_chr(),
    retrieval_step_id = prop_chr(),
    perspective_id = prop_chr(),
    persona_id = prop_chr(),
    session_id = prop_chr(),
    section_id = prop_chr(),
    created_at = prop_chr(),
    verified_at = prop_chr(),
    verifier_model = prop_chr(),
    verification_status = prop_enum(
      tempest_verification_statuses(),
      "unverified"
    )
  ),
  constructor = function(
    claim_text,
    source_ids = character(),
    claim_type = "finding",
    confidence = "medium",
    verification_status = "unverified",
    support_score = NA_real_,
    contradiction_score = NA_real_,
    source_quality_score = NA_real_,
    evidence_span_ids = character(),
    supporting_quotes = list(),
    contradicting_source_ids = character(),
    contradiction_note = NA_character_,
    retrieval_query = NA_character_,
    retrieval_step_id = NA_character_,
    perspective_id = NA_character_,
    persona_id = NA_character_,
    session_id = NA_character_,
    section_id = NA_character_,
    verified_at = NA_character_,
    verifier_model = NA_character_,
    claim_id = NULL,
    created_at = NULL
  ) {
    S7::new_object(
      S7::S7_object(),
      claim_id = claim_id %||% tempest_uuid("C"),
      claim_text = claim_text,
      claim_type = claim_type,
      source_ids = unique(source_ids),
      evidence_span_ids = evidence_span_ids,
      supporting_quotes = supporting_quotes,
      contradicting_source_ids = contradicting_source_ids,
      contradiction_note = contradiction_note,
      confidence = confidence,
      support_score = support_score,
      contradiction_score = contradiction_score,
      source_quality_score = source_quality_score,
      retrieval_query = retrieval_query,
      retrieval_step_id = retrieval_step_id,
      perspective_id = perspective_id,
      persona_id = persona_id,
      session_id = session_id,
      section_id = section_id,
      created_at = created_at %||% tempest_now_utc(),
      verified_at = verified_at,
      verifier_model = verifier_model,
      verification_status = verification_status
    )
  },
  validator = function(self) {
    if (
      length(self@claim_text) != 1 ||
        is.na(self@claim_text) ||
        !nzchar(self@claim_text)
    ) {
      return("claim_text must be a single non-empty string")
    }
  }
)

# --- evidence span ------------------------------------------------------------

#' Evidence span record (S7)
#' @keywords internal
tempest_evidence_span <- S7::new_class(
  "tempest_evidence_span",
  properties = list(
    evidence_span_id = prop_chr(),
    source_id = S7::class_character,
    chunk_id = prop_chr(),
    quote = prop_chr(),
    start_offset = S7::new_property(S7::class_integer, default = NA_integer_),
    end_offset = S7::new_property(S7::class_integer, default = NA_integer_),
    page = S7::new_property(S7::class_integer, default = NA_integer_),
    section_heading = prop_chr(),
    relevance_score = prop_score(),
    extracted_by = prop_chr("expert"),
    created_at = prop_chr()
  ),
  constructor = function(
    source_id,
    quote = NA_character_,
    chunk_id = NA_character_,
    start_offset = NA_integer_,
    end_offset = NA_integer_,
    page = NA_integer_,
    section_heading = NA_character_,
    relevance_score = NA_real_,
    extracted_by = "expert",
    evidence_span_id = NULL,
    created_at = NULL
  ) {
    S7::new_object(
      S7::S7_object(),
      evidence_span_id = evidence_span_id %||% tempest_uuid("E"),
      source_id = source_id,
      chunk_id = chunk_id,
      quote = quote,
      start_offset = as.integer(start_offset),
      end_offset = as.integer(end_offset),
      page = as.integer(page),
      section_heading = section_heading,
      relevance_score = relevance_score,
      extracted_by = extracted_by,
      created_at = created_at %||% tempest_now_utc()
    )
  }
)

# --- dispute ------------------------------------------------------------------

#' Dispute record (S7)
#' @keywords internal
tempest_dispute <- S7::new_class(
  "tempest_dispute",
  properties = list(
    dispute_id = prop_chr(),
    topic = S7::class_character,
    claim_ids = prop_chr_vec(),
    axis_of_disagreement = prop_chr(),
    likely_explanation = prop_chr(),
    evidence_balance = prop_enum(c("agreement", "mixed", "conflict"), "mixed"),
    unresolved_questions = prop_chr_vec(),
    created_at = prop_chr()
  ),
  constructor = function(
    topic,
    claim_ids = character(),
    axis_of_disagreement = NA_character_,
    likely_explanation = NA_character_,
    evidence_balance = "mixed",
    unresolved_questions = character(),
    dispute_id = NULL,
    created_at = NULL
  ) {
    S7::new_object(
      S7::S7_object(),
      dispute_id = dispute_id %||% tempest_uuid("D"),
      topic = topic,
      claim_ids = claim_ids,
      axis_of_disagreement = axis_of_disagreement,
      likely_explanation = likely_explanation,
      evidence_balance = evidence_balance,
      unresolved_questions = unresolved_questions,
      created_at = created_at %||% tempest_now_utc()
    )
  }
)

# --- claim converters ---------------------------------------------------------

#' @keywords internal
tempest_claim_to_list <- function(claim) {
  stopifnot(S7::S7_inherits(claim, tempest_claim))
  props <- S7::prop_names(claim)
  stats::setNames(lapply(props, function(p) S7::prop(claim, p)), props)
}

#' @keywords internal
tempest_claim_from_list <- function(x) {
  # JSON round-trip coercions:
  # - NA_character_  -> null in JSON -> NULL in R   => restore to NA_character_
  # - character()    -> []   in JSON -> list() in R => restore to character()
  # - NA_real_       -> null in JSON -> NULL in R   => restore to NA_real_
  # - non-empty character vectors come back as list of scalars => unlist

  chr_scalar_fields <- c(
    "claim_id",
    "claim_text",
    "claim_type",
    "contradiction_note",
    "retrieval_query",
    "retrieval_step_id",
    "perspective_id",
    "persona_id",
    "session_id",
    "section_id",
    "created_at",
    "verified_at",
    "verifier_model",
    "confidence",
    "verification_status"
  )
  for (f in chr_scalar_fields) {
    if (is.null(x[[f]])) x[[f]] <- NA_character_
  }

  chr_vec_fields <- c(
    "source_ids",
    "evidence_span_ids",
    "contradicting_source_ids"
  )
  for (f in chr_vec_fields) {
    v <- x[[f]]
    if (is.null(v) || (is.list(v) && length(v) == 0)) {
      x[[f]] <- character()
    } else if (is.list(v)) {
      x[[f]] <- as.character(unlist(v, use.names = FALSE))
    }
  }

  dbl_scalar_fields <- c(
    "support_score",
    "contradiction_score",
    "source_quality_score"
  )
  for (f in dbl_scalar_fields) {
    if (is.null(x[[f]])) x[[f]] <- NA_real_
  }

  do.call(
    tempest_claim,
    c(
      list(claim_text = x$claim_text %||% ""),
      x[setdiff(names(x), "claim_text")]
    )
  )
}

#' @keywords internal
tempest_evidence_span_to_list <- function(span) {
  stopifnot(S7::S7_inherits(span, tempest_evidence_span))
  props <- S7::prop_names(span)
  stats::setNames(lapply(props, function(p) S7::prop(span, p)), props)
}

#' @keywords internal
tempest_evidence_span_from_list <- function(x) {
  chr_scalar_fields <- c(
    "evidence_span_id",
    "source_id",
    "chunk_id",
    "quote",
    "section_heading",
    "extracted_by",
    "created_at"
  )
  for (f in chr_scalar_fields) {
    if (is.null(x[[f]])) x[[f]] <- NA_character_
  }
  if (is.na(x$extracted_by)) {
    x$extracted_by <- "expert"
  }

  int_scalar_fields <- c("start_offset", "end_offset", "page")
  for (f in int_scalar_fields) {
    if (is.null(x[[f]])) {
      x[[f]] <- NA_integer_
    } else {
      x[[f]] <- as.integer(x[[f]])
    }
  }

  if (is.null(x$relevance_score)) {
    x$relevance_score <- NA_real_
  }

  do.call(
    tempest_evidence_span,
    c(
      list(source_id = x$source_id %||% NA_character_),
      x[setdiff(names(x), "source_id")]
    )
  )
}

#' @keywords internal
tempest_dispute_to_list <- function(dispute) {
  stopifnot(S7::S7_inherits(dispute, tempest_dispute))
  props <- S7::prop_names(dispute)
  stats::setNames(lapply(props, function(p) S7::prop(dispute, p)), props)
}

#' @keywords internal
tempest_dispute_from_list <- function(x) {
  chr_scalar_fields <- c(
    "dispute_id",
    "topic",
    "axis_of_disagreement",
    "likely_explanation",
    "evidence_balance",
    "created_at"
  )
  for (f in chr_scalar_fields) {
    if (is.null(x[[f]])) x[[f]] <- NA_character_
  }
  if (is.na(x$evidence_balance)) {
    x$evidence_balance <- "mixed"
  }

  chr_vec_fields <- c("claim_ids", "unresolved_questions")
  for (f in chr_vec_fields) {
    v <- x[[f]]
    if (is.null(v) || (is.list(v) && length(v) == 0)) {
      x[[f]] <- character()
    } else if (is.list(v)) {
      x[[f]] <- as.character(unlist(v, use.names = FALSE))
    }
  }

  do.call(
    tempest_dispute,
    c(
      list(topic = x$topic %||% NA_character_),
      x[setdiff(names(x), "topic")]
    )
  )
}

#' @keywords internal
tempest_claims_tibble <- function(claims) {
  if (length(claims) == 0) {
    return(tibble::tibble(
      claim_id = character(),
      claim_text = character(),
      claim_type = character(),
      source_ids = list(),
      confidence = character(),
      verification_status = character(),
      support_score = numeric(),
      session_id = character(),
      created_at = character()
    ))
  }
  tibble::tibble(
    claim_id = purrr::map_chr(claims, ~ .x@claim_id),
    claim_text = purrr::map_chr(claims, ~ .x@claim_text),
    claim_type = purrr::map_chr(claims, ~ .x@claim_type),
    source_ids = purrr::map(claims, ~ .x@source_ids),
    confidence = purrr::map_chr(claims, ~ .x@confidence),
    verification_status = purrr::map_chr(claims, ~ .x@verification_status),
    support_score = purrr::map_dbl(claims, ~ .x@support_score),
    session_id = purrr::map_chr(claims, ~ .x@session_id),
    created_at = purrr::map_chr(claims, ~ .x@created_at)
  )
}

# --- source record ------------------------------------------------------------

#' Source record (S7) — typed replacement for the plain source list
#' @keywords internal
tempest_source_record <- S7::new_class(
  "tempest_source_record",
  properties = list(
    id = prop_chr(),
    url = S7::class_character,
    title = prop_chr(),
    snippet = prop_chr(),
    content_text = prop_chr(),
    fetched_at = prop_chr(),
    content_hash = prop_chr(),
    meta = S7::new_property(S7::class_list, default = list())
  ),
  constructor = function(
    url,
    title = NA_character_,
    snippet = NA_character_,
    content_text = NA_character_,
    fetched_at = NA_character_,
    content_hash = NA_character_,
    meta = list(),
    id = NULL
  ) {
    S7::new_object(
      S7::S7_object(),
      id = id %||% tempest_source_id(url),
      url = url,
      title = title,
      snippet = snippet,
      content_text = content_text,
      fetched_at = fetched_at,
      content_hash = content_hash,
      meta = meta
    )
  }
)
