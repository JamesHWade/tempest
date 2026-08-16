# Typed Co-STORM post-turn results

tempest_session_turn_count_prop <- function(default = 0L) {
  S7::new_property(
    S7::class_integer,
    default = default,
    validator = function(value) {
      if (length(value) != 1L || is.na(value) || value < 0L) {
        "must be a single non-negative integer"
      }
    }
  )
}

tempest_session_turn_notices_prop <- function() {
  S7::new_property(
    S7::class_list,
    default = list(),
    validator = function(value) {
      valid <- vapply(
        value,
        \(notice) S7::S7_inherits(notice, TempestSessionTurnNotice),
        logical(1)
      )
      if (any(!valid)) {
        "must contain only tempest_session_turn_notice objects"
      }
    }
  )
}

TempestSessionTurnNotice <- S7::new_class(
  "tempest_session_turn_notice",
  properties = list(
    code = prop_enum(c(
      "evidence_gap",
      "evidence_failed",
      "mindmap_failed",
      "suggestions_failed"
    )),
    stage = prop_enum(c("evidence", "mindmap", "suggestions")),
    severity = prop_enum(c("info", "warning")),
    message = prop_chr(),
    details = S7::new_property(S7::class_list, default = list())
  ),
  validator = function(self) {
    if (
      length(self@message) != 1L ||
        is.na(self@message) ||
        !nzchar(tempest_trim(self@message))
    ) {
      return("message must be a single non-empty string")
    }
    if (
      identical(self@code, "evidence_gap") &&
        !identical(self@severity, "info")
    ) {
      return("evidence_gap notices must have info severity")
    }
    if (
      !identical(self@code, "evidence_gap") &&
        !identical(self@severity, "warning")
    ) {
      return("failure notices must have warning severity")
    }
    expected_stage <- switch(
      self@code,
      evidence_gap = "evidence",
      evidence_failed = "evidence",
      mindmap_failed = "mindmap",
      suggestions_failed = "suggestions"
    )
    if (!identical(self@stage, expected_stage)) {
      return("notice code and stage must agree")
    }
    tryCatch(
      {
        tempest_canonical_json(self@details)
        NULL
      },
      error = function(error) "details must contain only serializable values"
    )
  }
)

TempestSessionTurnResult <- S7::new_class(
  "tempest_session_turn_result",
  properties = list(
    session_id = prop_chr(),
    turn_id = prop_chr(),
    status = prop_enum(c("succeeded", "partial", "cancelled")),
    evidence_status = prop_enum(c(
      "committed",
      "gap",
      "failed",
      "cancelled"
    )),
    source_ids = prop_chr_vec(),
    source_count = tempest_session_turn_count_prop(),
    claim_count = tempest_session_turn_count_prop(),
    sources_added = tempest_session_turn_count_prop(),
    claims_added = tempest_session_turn_count_prop(),
    mindmap_status = prop_enum(c(
      "updated",
      "unchanged",
      "failed",
      "cancelled"
    )),
    mindmap_node_count = tempest_session_turn_count_prop(),
    suggestion_status = prop_enum(c(
      "generated",
      "skipped",
      "failed",
      "cancelled"
    )),
    suggestions = prop_chr_vec(),
    notices = tempest_session_turn_notices_prop(),
    completed_at = prop_chr()
  ),
  validator = function(self) {
    for (field in c("session_id", "turn_id", "completed_at")) {
      value <- S7::prop(self, field)
      if (length(value) != 1L || is.na(value) || !nzchar(tempest_trim(value))) {
        return(paste0(field, " must be a single non-empty string"))
      }
    }
    if (any(is.na(self@source_ids)) || any(!nzchar(self@source_ids))) {
      return("source_ids must contain only non-empty strings")
    }
    if (anyDuplicated(self@source_ids)) {
      return("source_ids must be unique")
    }
    if (self@sources_added > self@source_count) {
      return("sources_added cannot exceed source_count")
    }
    if (self@claims_added > self@claim_count) {
      return("claims_added cannot exceed claim_count")
    }
    if (any(is.na(self@suggestions)) || any(!nzchar(self@suggestions))) {
      return("suggestions must contain only non-empty strings")
    }
    if (
      identical(self@evidence_status, "gap") && length(self@source_ids) > 0L
    ) {
      return("gap results cannot contain source_ids")
    }
    if (
      identical(self@evidence_status, "committed") &&
        length(self@source_ids) == 0L
    ) {
      return("committed evidence results must contain source_ids")
    }
    if (
      self@evidence_status %in%
        c("failed", "cancelled") &&
        (length(self@source_ids) > 0L ||
          self@sources_added > 0L ||
          self@claims_added > 0L)
    ) {
      return("failed and cancelled evidence cannot contain committed records")
    }
    if (
      identical(self@suggestion_status, "generated") &&
        length(self@suggestions) == 0L
    ) {
      return("generated suggestion results must contain suggestions")
    }
    if (
      !identical(self@suggestion_status, "generated") &&
        length(self@suggestions) > 0L
    ) {
      return("only generated suggestion results can contain suggestions")
    }
    warning_notices <- vapply(
      self@notices,
      \(notice) identical(notice@severity, "warning"),
      logical(1)
    )
    notice_codes <- vapply(
      self@notices,
      \(notice) notice@code,
      character(1)
    )
    expected_notices <- c(
      evidence = "evidence_failed",
      mindmap = "mindmap_failed",
      suggestions = "suggestions_failed"
    )
    stage_statuses <- c(
      evidence = self@evidence_status,
      mindmap = self@mindmap_status,
      suggestions = self@suggestion_status
    )
    for (stage in names(expected_notices)) {
      failed <- identical(stage_statuses[[stage]], "failed")
      has_notice <- expected_notices[[stage]] %in% notice_codes
      if (failed && !has_notice) {
        return(paste0(stage, " failures require a matching warning notice"))
      }
      if (!failed && has_notice) {
        return(paste0(stage, " failure notices require a failed status"))
      }
    }
    has_gap_notice <- "evidence_gap" %in% notice_codes
    if (identical(self@evidence_status, "gap") && !has_gap_notice) {
      return("evidence gaps require a matching info notice")
    }
    if (!identical(self@evidence_status, "gap") && has_gap_notice) {
      return("evidence gap notices require gap evidence status")
    }
    if (
      identical(self@status, "succeeded") &&
        any(stage_statuses %in% c("failed", "cancelled"))
    ) {
      return("succeeded results cannot contain failed or cancelled stages")
    }
    if (
      !identical(self@status, "cancelled") &&
        any(stage_statuses == "cancelled")
    ) {
      return("only cancelled results can contain cancelled stages")
    }
    if (identical(self@status, "partial") && !any(warning_notices)) {
      return("partial results must contain a warning notice")
    }
    if (identical(self@status, "succeeded") && any(warning_notices)) {
      return("succeeded results cannot contain warning notices")
    }
    if (
      identical(self@status, "cancelled") &&
        !identical(self@suggestion_status, "cancelled")
    ) {
      return("cancelled results must have cancelled suggestion status")
    }
  }
)

tempest_session_turn_notice <- function(
  code,
  stage,
  severity = if (identical(code, "evidence_gap")) "info" else "warning",
  message,
  details = list()
) {
  details <- tempest_workflow_serializable_list(details, "details")
  TempestSessionTurnNotice(
    code = code,
    stage = stage,
    severity = severity,
    message = message,
    details = details
  )
}

tempest_session_turn_error_notice <- function(code, stage, message, error) {
  error_payload <- tempest_progress_error_payload(error)
  tempest_session_turn_notice(
    code = code,
    stage = stage,
    message = message,
    details = error_payload
  )
}

tempest_session_turn_result <- function(
  session_id,
  turn_id,
  status,
  evidence_status,
  source_ids = character(),
  source_count = 0L,
  claim_count = 0L,
  sources_added = 0L,
  claims_added = 0L,
  mindmap_status,
  mindmap_node_count = 0L,
  suggestion_status,
  suggestions = character(),
  notices = list(),
  completed_at = NULL
) {
  TempestSessionTurnResult(
    session_id = session_id,
    turn_id = turn_id,
    status = status,
    evidence_status = evidence_status,
    source_ids = unique(source_ids),
    source_count = as.integer(source_count),
    claim_count = as.integer(claim_count),
    sources_added = as.integer(sources_added),
    claims_added = as.integer(claims_added),
    mindmap_status = mindmap_status,
    mindmap_node_count = as.integer(mindmap_node_count),
    suggestion_status = suggestion_status,
    suggestions = unique(suggestions),
    notices = notices,
    completed_at = completed_at %||% tempest_now_utc()
  )
}

tempest_session_turn_notice_data <- function(notice) {
  if (!S7::S7_inherits(notice, TempestSessionTurnNotice)) {
    tempest_abort("{.arg notice} must be a tempest_session_turn_notice object.")
  }
  properties <- S7::prop_names(notice)
  stats::setNames(
    lapply(properties, \(property) S7::prop(notice, property)),
    properties
  )
}

tempest_session_turn_result_data <- function(result) {
  if (!S7::S7_inherits(result, TempestSessionTurnResult)) {
    tempest_abort("{.arg result} must be a tempest_session_turn_result object.")
  }
  properties <- setdiff(S7::prop_names(result), "notices")
  data <- stats::setNames(
    lapply(properties, \(property) S7::prop(result, property)),
    properties
  )
  data$notices <- lapply(result@notices, tempest_session_turn_notice_data)
  data
}
