# Package-owned asynchronous Co-STORM warmup orchestration.

tempest_warmup_statuses <- function() {
  c("succeeded", "skipped", "cancelled")
}

tempest_warmup_lease_name <- function() {
  "tempest_warmup_lease"
}

tempest_warmup_lease_owned <- function(session, token) {
  identical(
    attr(
      session,
      tempest_warmup_lease_name(),
      exact = TRUE
    ),
    token
  )
}

tempest_warmup_acquire_lease <- function(session) {
  existing <- attr(session, tempest_warmup_lease_name(), exact = TRUE)
  if (!is.null(existing)) {
    tempest_abort(
      "A Co-STORM warmup is already in progress for this session.",
      class = c("tempest_warmup_busy", "tempest_warmup_error", "tempest_error")
    )
  }
  token <- new.env(parent = emptyenv())
  attr(session, tempest_warmup_lease_name()) <- token
  token
}

tempest_warmup_release_lease <- function(session, token) {
  if (tempest_warmup_lease_owned(session, token)) {
    attr(session, tempest_warmup_lease_name()) <- NULL
  }
  invisible(NULL)
}

tempest_warmup_count_prop <- function() {
  S7::new_property(
    S7::class_integer,
    default = 0L,
    validator = function(value) {
      if (length(value) != 1L || is.na(value) || value < 0L) {
        "must be a single non-negative integer"
      }
    }
  )
}

tempest_warmup_flag_prop <- function(default = FALSE) {
  S7::new_property(
    S7::class_logical,
    default = default,
    validator = function(value) {
      if (length(value) != 1L || is.na(value)) {
        "must be TRUE or FALSE"
      }
    }
  )
}

tempest_warmup_chr_prop <- function(required = FALSE) {
  S7::new_property(
    S7::class_character,
    default = NA_character_,
    validator = function(value) {
      if (length(value) != 1L) {
        return("must have length one")
      }
      if (required && (is.na(value) || !nzchar(tempest_trim(value)))) {
        return("must be a single non-empty string")
      }
      if (!is.na(value) && tempest_contract_sensitive_scalar(value)) {
        return("must not contain credential-like content")
      }
    }
  )
}

tempest_warmup_orientation_error <- function(orientation) {
  required <- c(
    "expert_id",
    "expert_name",
    "expert_session_id",
    "deputy_run_id",
    "deputy_session_id",
    "correlation_id",
    "status",
    "evidence_status",
    "source_ids",
    "sources_added",
    "claims_added",
    "failure_kind",
    "error_class",
    "error_message",
    "tools_available",
    "capability_count",
    "session_retired",
    "cancellation_supported"
  )
  if (!is.list(orientation) || is.data.frame(orientation)) {
    return("must be a list")
  }
  if (!identical(names(orientation), required)) {
    return("must contain the exact required fields in canonical order")
  }
  scalar_character <- c(
    "expert_id",
    "expert_name",
    "expert_session_id",
    "deputy_run_id",
    "deputy_session_id",
    "correlation_id",
    "status",
    "evidence_status",
    "failure_kind",
    "error_class",
    "error_message"
  )
  for (field in scalar_character) {
    value <- orientation[[field]]
    if (!is.character(value) || length(value) != 1L) {
      return(paste0("has invalid `", field, "`"))
    }
    if (!is.na(value) && tempest_contract_sensitive_scalar(value)) {
      return(paste0("has credential-like `", field, "`"))
    }
  }
  deputy_ids <- c(
    orientation$deputy_run_id,
    orientation$deputy_session_id
  )
  present_deputy_ids <- !is.na(deputy_ids)
  if (xor(present_deputy_ids[[1L]], present_deputy_ids[[2L]])) {
    return("must bind Deputy run and session identifiers together")
  }
  if (
    any(present_deputy_ids) &&
      !all(vapply(deputy_ids, tempest_opaque_identifier_valid, logical(1)))
  ) {
    return("has invalid Deputy execution identifiers")
  }
  if (!orientation$status %in% c("succeeded", "failed", "timeout")) {
    return("has invalid `status`")
  }
  if (
    !orientation$evidence_status %in%
      c("committed", "skipped", "failed", "not_run")
  ) {
    return("has invalid `evidence_status`")
  }
  if (!is.character(orientation$source_ids) || anyNA(orientation$source_ids)) {
    return("has invalid `source_ids`")
  }
  if (
    any(!nzchar(orientation$source_ids)) ||
      anyDuplicated(orientation$source_ids) ||
      !all(vapply(
        orientation$source_ids,
        tempest_opaque_identifier_valid,
        logical(1)
      ))
  ) {
    return("has invalid `source_ids`")
  }
  for (field in c("sources_added", "claims_added", "capability_count")) {
    value <- orientation[[field]]
    if (
      !is.integer(value) || length(value) != 1L || is.na(value) || value < 0L
    ) {
      return(paste0("has invalid `", field, "`"))
    }
  }
  for (field in c(
    "tools_available",
    "session_retired",
    "cancellation_supported"
  )) {
    value <- orientation[[field]]
    if (!is.logical(value) || length(value) != 1L || is.na(value)) {
      return(paste0("has invalid `", field, "`"))
    }
  }
  failure_fields <- c(
    orientation$failure_kind,
    orientation$error_class,
    orientation$error_message
  )
  if (
    !is.na(orientation$error_class) &&
      !orientation$error_class %in%
        c("tempest_operation_error", tempest_stage_failure_classes())
  ) {
    return("has an invalid `error_class`")
  }
  if (
    !is.na(orientation$error_message) &&
      !identical(orientation$error_message, "The operation failed.")
  ) {
    return("has an invalid `error_message`")
  }
  if (identical(orientation$status, "succeeded")) {
    if (anyNA(deputy_ids)) {
      return("succeeded orientations must identify their Deputy execution")
    }
    if (!is.na(orientation$failure_kind)) {
      return("cannot have `failure_kind` when `status` is succeeded")
    }
    if (!orientation$evidence_status %in% c("committed", "skipped", "failed")) {
      return("has invalid evidence state for a succeeded orientation")
    }
    has_evidence_error <- identical(orientation$evidence_status, "failed")
    if (has_evidence_error && any(is.na(failure_fields[2:3]))) {
      return("must record evidence error details when evidence failed")
    }
    if (!has_evidence_error && any(!is.na(failure_fields[2:3]))) {
      return("cannot record error details without a failure")
    }
  } else {
    expected_kind <- if (identical(orientation$status, "timeout")) {
      "timeout"
    } else {
      "provider_error"
    }
    if (!identical(orientation$failure_kind, expected_kind)) {
      return("has failure kind inconsistent with orientation status")
    }
    if (any(is.na(failure_fields[2:3]))) {
      return("must record error details for a failed orientation")
    }
    if (!identical(orientation$evidence_status, "not_run")) {
      return("cannot record evidence for an unsuccessful orientation")
    }
  }
  if (identical(orientation$evidence_status, "committed")) {
    if (length(orientation$source_ids) == 0L) {
      return("committed evidence must contain source ids")
    }
  } else if (
    length(orientation$source_ids) > 0L ||
      orientation$sources_added > 0L ||
      orientation$claims_added > 0L
  ) {
    return("only committed evidence can contain committed records")
  }
  NULL
}

tempest_warmup_orientations_prop <- function() {
  S7::new_property(
    S7::class_list,
    default = list(),
    validator = function(value) {
      for (orientation in value) {
        error <- tempest_warmup_orientation_error(orientation)
        if (!is.null(error)) {
          return(paste("contains an orientation that", error))
        }
      }
    }
  )
}

TempestWarmupResult <- S7::new_class(
  "tempest_warmup_result",
  properties = list(
    session_id = tempest_warmup_chr_prop(required = TRUE),
    status = prop_enum(tempest_warmup_statuses()),
    expert_count = tempest_warmup_count_prop(),
    orientation_count = tempest_warmup_count_prop(),
    failure_count = tempest_warmup_count_prop(),
    evidence_failure_count = tempest_warmup_count_prop(),
    source_count = tempest_warmup_count_prop(),
    claim_count = tempest_warmup_count_prop(),
    mindmap_updated = tempest_warmup_flag_prop(),
    orientations = tempest_warmup_orientations_prop(),
    completed_at = tempest_warmup_chr_prop(required = TRUE)
  ),
  constructor = function(
    session_id,
    status,
    expert_count,
    orientation_count,
    failure_count,
    evidence_failure_count,
    source_count,
    claim_count,
    mindmap_updated,
    orientations,
    completed_at = NULL
  ) {
    S7::new_object(
      S7::S7_object(),
      session_id = session_id,
      status = status,
      expert_count = as.integer(expert_count),
      orientation_count = as.integer(orientation_count),
      failure_count = as.integer(failure_count),
      evidence_failure_count = as.integer(evidence_failure_count),
      source_count = as.integer(source_count),
      claim_count = as.integer(claim_count),
      mindmap_updated = mindmap_updated,
      orientations = orientations,
      completed_at = completed_at %||% tempest_now_utc()
    )
  },
  validator = function(self) {
    orientation_count <- sum(vapply(
      self@orientations,
      \(record) identical(record$status, "succeeded"),
      logical(1)
    ))
    failure_count <- sum(vapply(
      self@orientations,
      \(record) record$status %in% c("failed", "timeout"),
      logical(1)
    ))
    evidence_failure_count <- sum(vapply(
      self@orientations,
      \(record) identical(record$evidence_status, "failed"),
      logical(1)
    ))
    if (!identical(self@orientation_count, as.integer(orientation_count))) {
      return("orientation_count must match succeeded orientation records")
    }
    if (!identical(self@failure_count, as.integer(failure_count))) {
      return("failure_count must match failed orientation records")
    }
    if (
      !identical(
        self@evidence_failure_count,
        as.integer(evidence_failure_count)
      )
    ) {
      return("evidence_failure_count must match failed evidence records")
    }
    if (
      identical(self@status, "succeeded") &&
        length(self@orientations) != self@expert_count
    ) {
      return("succeeded warmups must contain one record per expert")
    }
    if (
      self@status %in%
        c("skipped", "cancelled") &&
        length(self@orientations) > 0L
    ) {
      return("skipped and cancelled warmups cannot contain orientations")
    }
    if (identical(self@status, "skipped") && self@expert_count != 0L) {
      return("skipped warmups cannot contain experts")
    }
  }
)

tempest_warmup_timeout_condition <- function(label, timeout_s) {
  structure(
    list(
      message = sprintf("%s timed out after %.0f seconds", label, timeout_s),
      call = NULL,
      label = label,
      timeout_s = timeout_s
    ),
    class = c("tempest_warmup_timeout", "error", "condition")
  )
}

tempest_warmup_with_timeout <- function(
  promise,
  timeout_s,
  label = "Warmup step"
) {
  if (is.null(timeout_s) || !is.finite(timeout_s) || timeout_s <= 0) {
    return(promise)
  }
  promises::promise(function(resolve, reject) {
    settled <- FALSE
    later::later(
      function() {
        if (!settled) {
          settled <<- TRUE
          reject(tempest_warmup_timeout_condition(label, timeout_s))
        }
      },
      delay = timeout_s
    )
    promises::then(
      promise,
      onFulfilled = function(value) {
        if (!settled) {
          settled <<- TRUE
          resolve(value)
        }
      },
      onRejected = function(error) {
        if (!settled) {
          settled <<- TRUE
          reject(error)
        }
      }
    )
  })
}

tempest_warmup_prompt <- function(topic, expert) {
  seed_questions <- unique(c(
    expert@initial_questions,
    expert@initial_work_items
  ))
  seed_questions <- stringi::stri_trim_both(seed_questions)
  seed_questions <- seed_questions[
    !is.na(seed_questions) & nzchar(seed_questions)
  ]
  seeds <- if (length(seed_questions) > 0L) {
    paste0(
      "\n\nUse these seed questions as planning context:\n- ",
      paste(seed_questions, collapse = "\n- ")
    )
  } else {
    ""
  }
  paste0(
    "Topic: ",
    topic,
    "\n\nGive the panel a concise, evidence-backed orientation from your ",
    "professional perspective. This bounded pass must seed the shared evidence ",
    "ledger, not merely brainstorm. First inspect relevant evidence already in ",
    "the session. If none is available, make exactly one web search, inspect no ",
    "more than two results, and set k = 2 when the search tool accepts k. Do ",
    "not make more than two retrieval or fetch calls. Ground at least one ",
    "orientation claim in an inspected source and preserve its citation. ",
    paste0(
      "Evidence is committed after your response, so do not call ",
      "add_proposed_claim yourself. Label anything not supported by inspected ",
      "evidence as "
    ),
    "uncertain.",
    seeds,
    "\n\nIn no more than 250 words, cover:\n",
    "- the lens you bring to this topic;\n",
    "- two or three high-value research questions; and\n",
    "- the main uncertainty, tradeoff, or risk the panel should investigate."
  )
}

tempest_warmup_orientation <- function(expert, correlation_id) {
  list(
    expert_id = expert@expert_id,
    expert_name = expert@name,
    expert_session_id = NA_character_,
    deputy_run_id = NA_character_,
    deputy_session_id = NA_character_,
    correlation_id = correlation_id,
    status = "failed",
    evidence_status = "not_run",
    source_ids = character(),
    sources_added = 0L,
    claims_added = 0L,
    failure_kind = NA_character_,
    error_class = NA_character_,
    error_message = NA_character_,
    tools_available = FALSE,
    capability_count = 0L,
    session_retired = FALSE,
    cancellation_supported = FALSE
  )
}

tempest_warmup_error_record <- function(record, error, failure_kind) {
  record$status <- if (identical(failure_kind, "timeout")) {
    "timeout"
  } else {
    "failed"
  }
  record$failure_kind <- failure_kind
  payload <- tempest_progress_error_payload(error)
  record$error_class <- payload$error_class
  record$error_message <- payload$error_message
  record
}

tempest_warmup_progress_id <- function(event) {
  if (S7::S7_inherits(event, tempest_progress_event)) {
    event@event_id
  } else {
    NA_character_
  }
}

tempest_warmup_evidence_exchange <- function(orientation, evidence_result) {
  source_ids <- evidence_result$source_ids %||% character()
  status <- if (length(source_ids) > 0L) {
    "Evidence-backed orientation"
  } else {
    paste(
      "Scoping-only orientation with no cited source.",
      "Do not add its factual claims to the mind map"
    )
  }
  paste0(
    status,
    " from ",
    orientation$expert_name,
    ":\n",
    orientation$response
  )
}

tempest_warmup_store_counts <- function(session) {
  counts <- tempest_session_evidence_counts(session)
  list(
    source_count = as.integer(counts$source_count),
    claim_count = as.integer(counts$claim_count)
  )
}

tempest_warmup_result <- function(
  session,
  status,
  expert_count,
  records = list(),
  evidence_failure_count = 0L,
  mindmap_updated = FALSE
) {
  counts <- tempest_warmup_store_counts(session)
  orientation_count <- sum(vapply(
    records,
    \(record) identical(record$status, "succeeded"),
    logical(1)
  ))
  failure_count <- sum(vapply(
    records,
    \(record) record$status %in% c("failed", "timeout"),
    logical(1)
  ))
  TempestWarmupResult(
    session_id = session$session_id,
    status = status,
    expert_count = expert_count,
    orientation_count = orientation_count,
    failure_count = failure_count,
    evidence_failure_count = evidence_failure_count,
    source_count = counts$source_count,
    claim_count = counts$claim_count,
    mindmap_updated = mindmap_updated,
    orientations = records
  )
}

tempest_warmup_commit_async <- function(session, state, is_current) {
  pending <- which(!vapply(state$pending, is.null, logical(1)))
  if (length(pending) == 0L) {
    return(promises::promise_resolve(list(
      evidence_failure_count = 0L,
      mindmap_updated = FALSE
    )))
  }
  manager <- tempest_session_expert_manager(session)
  committed <- vector("list", length(state$records))
  evidence_failure_count <- 0L
  commit_one <- function(previous, index) {
    promises::then(previous, function(...) {
      if (!tempest_async_is_current(is_current)) {
        state$cancelled <- TRUE
        return(NULL)
      }
      orientation <- state$pending[[index]]
      request <- tempest_async_promise_try(function() {
        manager$commit_completion(
          orientation$completion_id,
          expert_id = orientation$expert_id,
          stage = "warmup",
          is_current = is_current
        )
      })
      promises::then(
        request,
        onFulfilled = function(result) {
          if (
            !tempest_async_is_current(is_current) ||
              isTRUE(result$cancelled %||% FALSE)
          ) {
            state$cancelled <- TRUE
            return(NULL)
          }
          committed[[index]] <<- result
          tempest_session_append_transcript(
            session,
            orientation$expert_name,
            "assistant",
            result$response
          )
          trace <- result$deputy_execution
          state$records[[index]]$status <- "succeeded"
          state$records[[index]]$deputy_run_id <- trace$deputy_run_id
          state$records[[index]]$deputy_session_id <- trace$deputy_session_id
          state$records[[index]]$source_ids <- result$source_ids %||%
            character()
          state$records[[index]]$sources_added <- as.integer(
            result$sources_added %||% 0L
          )
          state$records[[index]]$claims_added <- as.integer(
            result$claims_added %||% 0L
          )
          state$records[[index]]$evidence_status <- if (
            !is.null(result$evidence_error)
          ) {
            evidence_failure_count <<- evidence_failure_count + 1L
            state$records[[index]]$error_class <-
              result$evidence_error$error_class
            state$records[[index]]$error_message <-
              result$evidence_error$error_message
            "failed"
          } else if (!isTRUE(result$evidence_committed)) {
            "skipped"
          } else {
            "committed"
          }
          session$emit_progress(
            "expert",
            "succeeded",
            stage = "warmup",
            step = "expert_fanout",
            parent_event_id = tempest_warmup_progress_id(
              orientation$expert_event
            ),
            correlation_id = orientation$correlation_id,
            payload = list(
              expert_id = orientation$expert_id,
              expert_name = orientation$expert_name,
              mode = "bounded_research",
              session_id = result$session_id,
              deputy_run_id = trace$deputy_run_id,
              deputy_session_id = trace$deputy_session_id,
              tools_available = state$records[[index]]$tools_available,
              capability_count = state$records[[index]]$capability_count,
              evidence_status = state$records[[index]]$evidence_status
            )
          )
          result
        },
        onRejected = function(error) {
          tempest_rethrow_dsprrr_contract(error)
          try(
            manager$cancel_completion(orientation$completion_id),
            silent = TRUE
          )
          if (!tempest_async_is_current(is_current)) {
            state$cancelled <- TRUE
            return(NULL)
          }
          record <- tempest_warmup_error_record(
            state$records[[index]],
            error,
            "provider_error"
          )
          state$records[[index]] <- record
          session$emit_progress(
            "expert",
            "failed",
            stage = "warmup",
            step = "expert_fanout",
            parent_event_id = tempest_warmup_progress_id(
              orientation$expert_event
            ),
            correlation_id = orientation$correlation_id,
            payload = c(
              list(
                expert_id = orientation$expert_id,
                expert_name = orientation$expert_name,
                mode = "bounded_research",
                session_id = orientation$expert_session_id,
                failure_kind = record$failure_kind
              ),
              tempest_progress_error_payload(error)
            )
          )
          NULL
        }
      )
    })
  }
  evidence <- Reduce(
    commit_one,
    pending,
    init = promises::promise_resolve(NULL)
  )
  promises::then(evidence, function(...) {
    if (!tempest_async_is_current(is_current)) {
      state$cancelled <- TRUE
      return(NULL)
    }
    successful <- pending[!vapply(committed[pending], is.null, logical(1))]
    if (length(successful) == 0L) {
      return(list(
        evidence_failure_count = evidence_failure_count,
        mindmap_updated = FALSE
      ))
    }
    exchange <- paste(
      vapply(
        successful,
        function(index) {
          tempest_warmup_evidence_exchange(
            c(
              state$pending[[index]],
              list(response = committed[[index]]$response %||% "")
            ),
            committed[[index]] %||% list()
          )
        },
        character(1)
      ),
      collapse = "\n\n---\n\n"
    )
    map_request <- tempest_async_promise_try(function() {
      tempest_session_update_mindmap_async(
        session,
        last_exchange = exchange,
        is_current = is_current,
        emit_stale_progress = FALSE
      )
    })
    promises::then(
      map_request,
      onFulfilled = function(value) {
        list(
          evidence_failure_count = evidence_failure_count,
          mindmap_updated = !is.null(value) &&
            tempest_async_is_current(is_current)
        )
      },
      onRejected = function(error) {
        list(
          evidence_failure_count = evidence_failure_count,
          mindmap_updated = FALSE
        )
      }
    )
  })
}

#' Warm up a Co-STORM session asynchronously
#'
#' `r lifecycle::badge("experimental")`
#'
#' Runs one bounded, evidence-seeking orientation for each active expert without
#' blocking the caller. Expert requests run in bounded parallel batches, while
#' transcript and evidence commits occur deterministically in expert order. A
#' single mind-map update follows the completed evidence commits.
#'
#' @param session A [TempestSession] created by [tempest_session()].
#' @param timeout_s Maximum seconds allowed for each expert orientation. Use
#'   `NULL`, a non-positive value, or a non-finite value to disable timeouts.
#' @param max_parallel_experts Maximum number of expert requests started at the
#'   same time.
#' @param is_current A zero-argument function that returns `TRUE` while the
#'   originating host session is current. Stale operations resolve without
#'   committing late results.
#' @return A promise that resolves to an internal `tempest_warmup_result` S7
#'   object with aggregate counts and per-expert audit records.
#' @examples
#' \dontrun{
#' session <- tempest_session("History of jazz")
#' tempest_session_warmup_async(session) |>
#'   promises::then(\(result) result@status)
#' }
#' @export
tempest_session_warmup_async <- function(
  session,
  timeout_s = getOption("tempest.costorm.warmup_timeout_s", 120),
  max_parallel_experts = getOption(
    "tempest.costorm.warmup_max_parallel_experts",
    3L
  ),
  is_current = function() TRUE
) {
  tempest_require("promises", "Async Co-STORM warmup requires promises.")
  tempest_require("later", "Async Co-STORM warmup requires later.")
  if (!inherits(session, "TempestSession")) {
    tempest_abort(
      "{.arg session} must be created by {.fn tempest_session}.",
      class = c("tempest_warmup_error", "tempest_error")
    )
  }
  if (!is.function(is_current)) {
    tempest_abort(
      "{.arg is_current} must be a function.",
      class = c("tempest_warmup_error", "tempest_error")
    )
  }
  tempest_session_assert_mutable(session, "warm up experts")
  experts <- tempest_validate_experts(session$experts %||% list())
  expert_count <- length(experts)
  if (is.null(max_parallel_experts) || length(max_parallel_experts) == 0L) {
    max_parallel_experts <- 3L
  } else {
    max_parallel_experts <- suppressWarnings(
      as.integer(max_parallel_experts[[1]])
    )
  }
  if (is.na(max_parallel_experts) || max_parallel_experts < 1L) {
    max_parallel_experts <- 1L
  }
  if (!is.null(timeout_s)) {
    if (!is.numeric(timeout_s) || length(timeout_s) != 1L || is.na(timeout_s)) {
      tempest_abort(
        "{.arg timeout_s} must be a numeric scalar or {.code NULL}.",
        class = c("tempest_warmup_error", "tempest_error")
      )
    }
    timeout_s <- as.numeric(timeout_s)
  }
  manager <- tempest_session_expert_manager(session)

  lease <- tempest_warmup_acquire_lease(session)
  promise_owns_lease <- FALSE
  work_id <- tempest_session_async_work_start(session, "warmup")
  on.exit(
    {
      if (!promise_owns_lease) {
        tempest_warmup_release_lease(session, lease)
        tempest_session_async_work_finish(session, work_id)
      }
    },
    add = TRUE
  )
  if (!tempest_async_is_current(is_current)) {
    return(promises::promise_resolve(tempest_warmup_result(
      session,
      status = "cancelled",
      expert_count = expert_count
    )))
  }

  warmup_event <- session$emit_progress(
    "stage",
    "started",
    stage = "warmup",
    step = "expert_fanout",
    payload = list(expert_count = expert_count)
  )
  if (expert_count == 0L) {
    session$emit_progress(
      "stage",
      "skipped",
      stage = "warmup",
      step = "expert_fanout",
      parent_event_id = tempest_warmup_progress_id(warmup_event),
      correlation_id = warmup_event@correlation_id,
      payload = list(reason = "no_experts")
    )
    return(promises::promise_resolve(tempest_warmup_result(
      session,
      status = "skipped",
      expert_count = 0L
    )))
  }

  state <- new.env(parent = emptyenv())
  state$pending <- vector("list", expert_count)
  state$records <- vector("list", expert_count)
  state$cancelled <- FALSE

  orient_expert <- function(index) {
    expert <- experts[[index]]
    expert_id <- expert@expert_id
    expert_name <- expert@name
    correlation_id <- paste("warmup-expert", expert_id, sep = "-")
    record <- tempest_warmup_orientation(expert, correlation_id)
    expert_event <- NULL
    expert_session_id <- NULL
    orientation_active <- FALSE
    retirement <- list(retired = FALSE, cancellation_supported = FALSE)
    retire_session <- function() {
      if (
        !tempest_warmup_lease_owned(session, lease) ||
          isTRUE(retirement$retired) ||
          is.null(expert_session_id)
      ) {
        return(retirement)
      }
      retirement <<- tryCatch(
        manager$retire_session(expert_session_id),
        error = function(error) {
          list(retired = FALSE, cancellation_supported = FALSE)
        }
      )
      retirement
    }
    promises::then(promises::promise_resolve(NULL), function(...) {
      if (!tempest_async_is_current(is_current)) {
        state$cancelled <- TRUE
        return(NULL)
      }
      session_result <- manager$get_or_create(expert_id)
      expert_session_id <<- session_result$session_id
      capability_count <- length(session_result$chat$get_tools())
      record$expert_session_id <<- expert_session_id
      record$capability_count <<- as.integer(capability_count)
      record$tools_available <<- capability_count > 0L
      expert_event <<- session$emit_progress(
        "expert",
        "started",
        stage = "warmup",
        step = "expert_fanout",
        parent_event_id = tempest_warmup_progress_id(warmup_event),
        correlation_id = correlation_id,
        payload = list(
          expert_id = expert_id,
          expert_name = expert_name,
          mode = "bounded_research",
          session_id = expert_session_id,
          tools_available = record$tools_available,
          capability_count = record$capability_count
        )
      )
      orientation_active <<- TRUE
      request <- manager$request_completion_async(
        expert_id,
        tempest_warmup_prompt(session$topic, expert),
        stage = "warmup",
        correlation_id = correlation_id
      )
      work <- promises::then(request, function(completion_id) {
        if (!orientation_active) {
          try(manager$cancel_completion(completion_id), silent = TRUE)
          return(NULL)
        }
        if (!tempest_async_is_current(is_current)) {
          state$cancelled <- TRUE
          orientation_active <<- FALSE
          try(manager$cancel_completion(completion_id), silent = TRUE)
          retire_session()
          return(NULL)
        }
        state$pending[[index]] <- list(
          expert_id = expert_id,
          expert_name = expert_name,
          expert_session_id = expert_session_id,
          correlation_id = correlation_id,
          completion_id = completion_id,
          expert_event = expert_event
        )
        state$records[[index]] <- record
        orientation_active <<- FALSE
        NULL
      })
      tempest_warmup_with_timeout(
        work,
        timeout_s,
        paste(expert_name, "orientation")
      ) |>
        promises::catch(function(error) {
          orientation_active <<- FALSE
          timed_out <- inherits(error, "tempest_warmup_timeout")
          if (timed_out || !tempest_async_is_current(is_current)) {
            retire_session()
          }
          if (!tempest_async_is_current(is_current)) {
            state$cancelled <- TRUE
            return(NULL)
          }
          record$session_retired <<- isTRUE(retirement$retired)
          record$cancellation_supported <<-
            isTRUE(retirement$cancellation_supported)
          terminal <- Filter(
            function(trace) {
              identical(trace$role, "expert") &&
                identical(trace$stage, "warmup") &&
                identical(trace$expert_id, expert_id) &&
                identical(trace$correlation_id, correlation_id)
            },
            tempest_session_deputy_traces(session)
          )
          if (length(terminal) == 1L) {
            record$deputy_run_id <<- terminal[[1L]]$deputy_run_id
            record$deputy_session_id <<- terminal[[1L]]$deputy_session_id
          }
          record <<- tempest_warmup_error_record(
            record,
            error,
            if (timed_out) "timeout" else "provider_error"
          )
          state$records[[index]] <- record
          session$emit_progress(
            "expert",
            "failed",
            stage = "warmup",
            step = "expert_fanout",
            parent_event_id = tempest_warmup_progress_id(expert_event),
            correlation_id = correlation_id,
            payload = c(
              list(
                expert_id = expert_id,
                expert_name = expert_name,
                mode = "bounded_research",
                session_id = expert_session_id,
                tools_available = record$tools_available,
                capability_count = record$capability_count,
                failure_kind = record$failure_kind,
                session_retired = record$session_retired,
                cancellation_supported = record$cancellation_supported
              ),
              tempest_progress_error_payload(error)
            )
          )
          NULL
        })
    }) |>
      promises::catch(function(error) {
        orientation_active <<- FALSE
        if (!tempest_async_is_current(is_current)) {
          state$cancelled <- TRUE
          retire_session()
          return(NULL)
        }
        record <<- tempest_warmup_error_record(
          record,
          error,
          "provider_error"
        )
        state$records[[index]] <- record
        session$emit_progress(
          "expert",
          "failed",
          stage = "warmup",
          step = "expert_fanout",
          parent_event_id = tempest_warmup_progress_id(expert_event),
          correlation_id = correlation_id,
          payload = c(
            list(
              expert_id = expert_id,
              expert_name = expert_name,
              mode = "bounded_research",
              session_id = expert_session_id,
              tools_available = record$tools_available,
              capability_count = record$capability_count
            ),
            tempest_progress_error_payload(error)
          )
        )
        NULL
      })
  }

  expert_indices <- seq_along(experts)
  batches <- split(
    expert_indices,
    ceiling(seq_along(expert_indices) / max_parallel_experts)
  )
  run_batch <- function(previous, batch) {
    promises::then(previous, function(...) {
      if (!tempest_async_is_current(is_current)) {
        state$cancelled <- TRUE
        return(NULL)
      }
      promises::promise_all(.list = lapply(batch, orient_expert)) |>
        promises::then(function(...) NULL)
    })
  }
  fanout <- Reduce(
    run_batch,
    batches,
    init = promises::promise_resolve(NULL)
  )
  completed <- promises::then(fanout, function(...) {
    if (state$cancelled || !tempest_async_is_current(is_current)) {
      return(tempest_warmup_result(
        session,
        status = "cancelled",
        expert_count = expert_count
      ))
    }
    committed <- tempest_warmup_commit_async(session, state, is_current)
    promises::then(committed, function(commit_result) {
      if (
        state$cancelled ||
          !tempest_async_is_current(is_current) ||
          is.null(commit_result)
      ) {
        return(tempest_warmup_result(
          session,
          status = "cancelled",
          expert_count = expert_count
        ))
      }
      records <- Filter(Negate(is.null), state$records)
      result <- tempest_warmup_result(
        session,
        status = "succeeded",
        expert_count = expert_count,
        records = records,
        evidence_failure_count = commit_result$evidence_failure_count,
        mindmap_updated = commit_result$mindmap_updated
      )
      session$emit_progress(
        "stage",
        "succeeded",
        stage = "warmup",
        step = "expert_fanout",
        parent_event_id = tempest_warmup_progress_id(warmup_event),
        payload = list(
          expert_count = expert_count,
          orientation_count = result@orientation_count,
          failure_count = result@failure_count,
          evidence_failure_count = result@evidence_failure_count,
          source_count = result@source_count,
          claim_count = result@claim_count,
          bounded_research = TRUE
        )
      )
      result
    })
  })
  finalized <- promises::catch(completed, function(error) {
    if (tempest_async_is_current(is_current)) {
      session$emit_progress(
        "stage",
        "failed",
        stage = "warmup",
        step = "expert_fanout",
        parent_event_id = tempest_warmup_progress_id(warmup_event),
        payload = tempest_progress_error_payload(error)
      )
    }
    tempest_rethrow_operation(error, class = "tempest_session_error")
  })
  finalized <- promises::finally(
    finalized,
    function() {
      tempest_warmup_release_lease(session, lease)
      tempest_session_async_work_finish(session, work_id)
    }
  )
  promise_owns_lease <- TRUE
  finalized
}
