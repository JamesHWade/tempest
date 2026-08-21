# Explicit claim-by-evidence-span support assessments

tempest_claim_support_statuses <- function() {
  setdiff(tempest_verification_statuses(), "unverified")
}

tempest_claim_support_aggregate <- function(supports) {
  if (
    !is.list(supports) ||
      is.data.frame(supports) ||
      length(supports) == 0L ||
      !all(vapply(
        supports,
        \(support) S7::S7_inherits(support, TempestClaimSupport),
        logical(1)
      ))
  ) {
    tempest_abort(
      "Claim-support aggregation requires a non-empty list of exact records.",
      class = c("tempest_claim_support_error", "tempest_error")
    )
  }
  lapply(supports, S7::validate)
  statuses <- vapply(
    supports,
    \(support) support@verification_status,
    character(1)
  )
  status <- if (any(statuses == "contradicted")) {
    "contradicted"
  } else if (all(statuses == "supported")) {
    "supported"
  } else if (any(statuses %in% c("supported", "partially_supported"))) {
    "partially_supported"
  } else if (any(statuses == "unsupported")) {
    "unsupported"
  } else {
    "unverifiable"
  }
  scores <- vapply(
    supports,
    \(support) support@support_score,
    numeric(1)
  )
  score <- if (all(is.na(scores))) NA_real_ else min(scores, na.rm = TRUE)
  list(status = status, score = as.double(score))
}

tempest_claim_support_id <- function(claim_id, evidence_span_id) {
  if (
    !tempest_ledger_identifier_valid(claim_id) ||
      !tempest_ledger_identifier_valid(evidence_span_id)
  ) {
    tempest_abort(
      paste0(
        "Claim-support identity requires bounded credential-free claim and ",
        "evidence-span identifiers."
      ),
      class = c("tempest_claim_support_error", "tempest_error")
    )
  }
  payload <- jsonlite::toJSON(
    list(
      contract = "tempest.claim-support/1",
      claim_id = claim_id,
      evidence_span_id = evidence_span_id
    ),
    auto_unbox = TRUE,
    null = "null",
    digits = NA
  )
  paste0(
    "sha256:",
    digest::digest(payload, algo = "sha256", serialize = FALSE)
  )
}

TempestClaimSupport <- S7::new_class(
  "tempest_claim_support",
  properties = list(
    claim_support_id = prop_chr(),
    claim_id = prop_chr(),
    evidence_span_id = prop_chr(),
    source_id = prop_chr(),
    verification_status = prop_enum(
      tempest_claim_support_statuses(),
      "unverifiable"
    ),
    support_score = prop_score(),
    rationale = prop_chr()
  ),
  constructor = function(
    claim_id,
    evidence_span_id,
    source_id,
    verification_status,
    support_score,
    rationale,
    claim_support_id = NULL
  ) {
    expected_id <- tempest_claim_support_id(claim_id, evidence_span_id)
    S7::new_object(
      S7::S7_object(),
      claim_support_id = claim_support_id %||% expected_id,
      claim_id = claim_id,
      evidence_span_id = evidence_span_id,
      source_id = source_id,
      verification_status = verification_status,
      support_score = support_score,
      rationale = rationale
    )
  },
  validator = function(self) {
    identifiers <- c(
      self@claim_support_id,
      self@claim_id,
      self@evidence_span_id,
      self@source_id
    )
    if (
      !all(vapply(
        identifiers,
        tempest_ledger_identifier_valid,
        logical(1)
      ))
    ) {
      return(
        paste0(
          "claim-support identity fields must be bounded credential-free ",
          "identifiers"
        )
      )
    }
    expected_id <- tryCatch(
      tempest_claim_support_id(self@claim_id, self@evidence_span_id),
      error = \(error) NA_character_
    )
    if (!identical(self@claim_support_id, expected_id)) {
      return(
        "claim_support_id must match the exact claim and evidence-span pair"
      )
    }
    if (!is.null(attributes(self@support_score))) {
      return("support_score must be one plain numeric scalar")
    }
    if (
      identical(self@verification_status, "unverifiable") &&
        !identical(self@support_score, NA_real_)
    ) {
      return("unverifiable support must carry exact numeric NA support_score")
    }
    if (
      !identical(self@verification_status, "unverifiable") &&
        (is.na(self@support_score) || !is.finite(self@support_score))
    ) {
      return("a verifiable support assessment requires a finite support_score")
    }
    if (
      !rlang::is_string(self@rationale) ||
        is.na(self@rationale) ||
        !nzchar(tempest_trim(self@rationale)) ||
        !identical(self@rationale, tempest_trim(self@rationale)) ||
        nchar(self@rationale, type = "bytes") > 2000L ||
        tempest_contract_sensitive_scalar(self@rationale)
    ) {
      return(
        "rationale must be one canonical bounded credential-free non-empty string"
      )
    }
    NULL
  }
)

#' Create an explicit claim-support assessment
#'
#' `tempest_claim_support()` records one verifier judgment for one exact
#' claim-by-evidence-span pair. The source binding is explicit, while claim,
#' span, and source existence are validated by the owning [ResearchWorkspace].
#'
#' @param claim_id Exact provisional claim identifier.
#' @param evidence_span_id Exact evidence-span identifier.
#' @param source_id Exact source identifier owned by the evidence span.
#' @param verification_status One of `"supported"`, `"partially_supported"`,
#'   `"unsupported"`, `"contradicted"`, or `"unverifiable"`.
#' @param support_score Finite support strength in `[0, 1]`, or `NA` only for
#'   an `"unverifiable"` assessment.
#' @param rationale Required bounded credential-free rationale.
#' @return A `tempest_claim_support` S7 value.
#' @keywords internal
tempest_claim_support <- function(
  claim_id,
  evidence_span_id,
  source_id,
  verification_status,
  support_score,
  rationale
) {
  if (
    !is.numeric(support_score) ||
      is.object(support_score) ||
      length(support_score) != 1L ||
      !is.null(attributes(support_score))
  ) {
    tempest_abort(
      "{.arg support_score} must be one exact numeric scalar.",
      class = c("tempest_claim_support_error", "tempest_error")
    )
  }
  tryCatch(
    TempestClaimSupport(
      claim_id = claim_id,
      evidence_span_id = evidence_span_id,
      source_id = source_id,
      verification_status = verification_status,
      support_score = support_score,
      rationale = rationale
    ),
    error = function(error) {
      if (inherits(error, "tempest_claim_support_error")) {
        stop(error)
      }
      tempest_abort(
        "Cannot create an invalid claim-support assessment.",
        class = c("tempest_claim_support_error", "tempest_error"),
        parent = error
      )
    }
  )
}

tempest_claim_support_to_list <- function(support) {
  stopifnot(S7::S7_inherits(support, TempestClaimSupport))
  S7::validate(support)
  fields <- S7::prop_names(support)
  value <- stats::setNames(
    lapply(fields, \(field) S7::prop(support, field)),
    fields
  )
  if (identical(support@verification_status, "unverifiable")) {
    value["support_score"] <- list(NULL)
  }
  value
}

tempest_claim_support_from_list <- function(value) {
  fields <- c(
    "claim_support_id",
    "claim_id",
    "evidence_span_id",
    "source_id",
    "verification_status",
    "support_score",
    "rationale"
  )
  if (
    !is.list(value) ||
      is.data.frame(value) ||
      is.null(names(value)) ||
      anyNA(names(value)) ||
      anyDuplicated(names(value)) ||
      !identical(names(value), fields)
  ) {
    tempest_abort(
      "Claim-support data must contain exactly the current-schema fields.",
      class = c("tempest_claim_support_error", "tempest_error")
    )
  }
  stored_id <- value$claim_support_id
  expected_id <- tempest_claim_support_id(
    value$claim_id,
    value$evidence_span_id
  )
  if (!identical(stored_id, expected_id)) {
    tempest_abort(
      "Stored claim-support identity does not match its exact claim-span pair.",
      class = c("tempest_claim_support_error", "tempest_error")
    )
  }
  score <- value$support_score
  if (identical(value$verification_status, "unverifiable")) {
    if (!is.null(score)) {
      tempest_abort(
        paste0(
          "Unverifiable claim-support data must encode support_score as ",
          "literal JSON null."
        ),
        class = c("tempest_claim_support_error", "tempest_error")
      )
    }
    value["support_score"] <- list(NA_real_)
  } else if (
    !is.numeric(score) ||
      is.object(score) ||
      length(score) != 1L ||
      !is.null(attributes(score)) ||
      is.na(score) ||
      !is.finite(score)
  ) {
    tempest_abort(
      paste0(
        "Verifiable claim-support data must encode support_score as one ",
        "finite JSON number."
      ),
      class = c("tempest_claim_support_error", "tempest_error")
    )
  }
  do.call(
    tempest_claim_support,
    value[setdiff(fields, "claim_support_id")]
  )
}

tempest_claim_supports_tibble <- function(supports) {
  if (length(supports) == 0L) {
    return(tibble::tibble(
      claim_support_id = character(),
      claim_id = character(),
      evidence_span_id = character(),
      source_id = character(),
      verification_status = character(),
      support_score = numeric(),
      rationale = character()
    ))
  }
  records <- lapply(supports, tempest_claim_support_to_list)
  tibble::tibble(
    claim_support_id = vapply(
      records,
      `[[`,
      character(1),
      "claim_support_id"
    ),
    claim_id = vapply(records, `[[`, character(1), "claim_id"),
    evidence_span_id = vapply(
      records,
      `[[`,
      character(1),
      "evidence_span_id"
    ),
    source_id = vapply(records, `[[`, character(1), "source_id"),
    verification_status = vapply(
      records,
      `[[`,
      character(1),
      "verification_status"
    ),
    support_score = vapply(
      records,
      \(record) record$support_score %||% NA_real_,
      numeric(1)
    ),
    rationale = vapply(records, `[[`, character(1), "rationale")
  )
}

#' List explicit claim-support assessments
#'
#' Returns the complete joined proof table rather than foreign keys alone. Each
#' row carries the support identity and judgment, the claim identity and text,
#' the exact evidence-span identity with its quote, offsets, page, and section,
#' and the source identity. Pair it with [tempest_sources()] for the
#' corresponding source metadata and locator.
#'
#' @param x A completed [tempest_run()] product or a `TempestSession`.
#' @return A tibble with one row per exact claim-by-evidence-span judgment.
#' @export
tempest_claim_supports <- function(x) {
  workspace <- tempest_product_read_workspace(x)
  tempest_claim_supports_resolved(workspace)
}

# Join every support row to its durable claim and evidence span.
tempest_claim_supports_resolved <- function(workspace) {
  supports <- tempest_claim_supports_tibble(workspace$list_claim_supports())
  resolve <- function(ids, getter, field, default) {
    vapply(
      ids,
      function(id) {
        value <- tryCatch(getter(id), error = function(error) NULL)
        if (is.null(value)) {
          return(default)
        }
        found <- S7::prop(value, field)
        if (length(found) != 1L || is.na(found)) default else found
      },
      default,
      USE.NAMES = FALSE
    )
  }
  span <- function(field, default) {
    resolve(
      supports$evidence_span_id,
      workspace$get_evidence_span,
      field,
      default
    )
  }
  tibble::tibble(
    claim_support_id = supports$claim_support_id,
    claim_id = supports$claim_id,
    claim_text = resolve(
      supports$claim_id,
      workspace$get_proposed_claim,
      "claim_text",
      NA_character_
    ),
    evidence_span_id = supports$evidence_span_id,
    quote = span("quote", NA_character_),
    start_offset = span("start_offset", NA_integer_),
    end_offset = span("end_offset", NA_integer_),
    page = span("page", NA_integer_),
    section_heading = span("section_heading", NA_character_),
    source_id = supports$source_id,
    verification_status = supports$verification_status,
    support_score = supports$support_score,
    rationale = supports$rationale
  )
}
