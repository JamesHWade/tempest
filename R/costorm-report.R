# Deterministic Co-STORM product reporting.

tempest_costorm_execution_review_lines <- function(session) {
  if (!inherits(session, "TempestSession")) {
    return(character())
  }
  tempest_stage_records_execution_review_lines(
    tempest_session_stage_records(session)
  )
}

tempest_costorm_execution_review <- function(session) {
  if (!inherits(session, "TempestSession")) {
    return("")
  }
  tempest_stage_records_execution_review(
    tempest_session_stage_records(session)
  )
}

tempest_costorm_report_context <- function(
  session,
  style,
  include_references
) {
  if (!inherits(session, "TempestSession")) {
    tempest_product_report_abort(
      "Co-STORM report generation requires a TempestSession."
    )
  }
  if (!rlang::is_string(style) || !style %in% c("technical", "executive")) {
    tempest_product_report_abort(
      "Co-STORM report style must be `technical` or `executive`."
    )
  }
  include_references <- tempest_product_flag(
    include_references,
    "include_references"
  )
  title <- tempest_report_title_validate(session$title %||% session$topic)
  if (!inherits(session$workspace, "ResearchWorkspace")) {
    tempest_product_report_abort(
      "Co-STORM report generation requires a ResearchWorkspace."
    )
  }
  tryCatch(
    S7::validate(session$config),
    error = function(error) {
      tempest_product_report_abort(
        "Co-STORM report configuration failed live validation."
      )
    }
  )
  list(
    title = title,
    workspace = session$workspace,
    include_references = include_references,
    citation_policy = session$config@citation_policy,
    on_unsupported_claim = session$config@on_unsupported_claim,
    min_support_score = tempest_normalize_min_support_score(
      session$config@min_support_score
    ),
    execution_review = tempest_costorm_execution_review(session),
    style = style
  )
}

tempest_costorm_report_supported_claims <- function(session) {
  claims <- tempest_supported_claims(
    session$workspace,
    min_support_score = session$config@min_support_score
  )
  supports <- session$workspace$list_claim_supports()
  context <- paste(
    c(
      vapply(
        session$transcript,
        \(turn) turn$text %||% "",
        character(1)
      ),
      vapply(
        session$mindmap$nodes %||% list(),
        \(node) node$label %||% "",
        character(1)
      )
    ),
    collapse = " "
  )
  tokens <- unique(tolower(unlist(strsplit(context, "[^[:alnum:]]+"))))
  tokens <- tokens[nchar(tokens) >= 4L]
  records <- Filter(
    Negate(is.null),
    lapply(claims, function(claim) {
      bound <- Filter(
        function(support) {
          identical(support@claim_id, claim@claim_id) &&
            identical(support@verification_status, "supported") &&
            is.finite(support@support_score) &&
            support@support_score >= session$config@min_support_score
        },
        supports
      )
      source_ids <- sort(
        unique(vapply(bound, \(support) support@source_id, character(1))),
        method = "radix"
      )
      if (length(source_ids) == 0L) {
        return(NULL)
      }
      text <- tolower(claim@claim_text)
      score <- sum(vapply(
        tokens,
        \(token) grepl(token, text, fixed = TRUE),
        logical(1)
      ))
      list(claim = claim, source_ids = source_ids, context_score = score)
    })
  )
  if (length(records) == 0L) {
    return(list())
  }
  order_ids <- vapply(records, \(record) record$claim@claim_id, character(1))
  records[order(
    -vapply(records, \(record) record$context_score, integer(1)),
    order_ids,
    method = "radix"
  )]
}

tempest_costorm_report_body <- function(session, style) {
  style <- tempest_research_manifest_choice(
    style,
    "style",
    c("technical", "executive")
  )
  records <- tempest_costorm_report_supported_claims(session)
  if (length(records) == 0L) {
    return("")
  }
  heading <- if (identical(style, "technical")) {
    "## Evidence"
  } else {
    "## Summary"
  }
  lines <- vapply(
    records,
    function(record) {
      claim_text <- gsub(
        "[[:space:]]+",
        " ",
        tempest_trim(record$claim@claim_text),
        perl = TRUE
      )
      if (!nzchar(claim_text)) {
        tempest_product_report_abort(
          "A supported Co-STORM claim must have non-empty display text."
        )
      }
      claim_text <- tempest_markdown_escape_plain_text(
        claim_text,
        "claim text"
      )
      citations <- paste0("[", record$source_ids, "]", collapse = " ")
      paste0("- ", claim_text, " ", citations)
    },
    character(1)
  )
  paste(c(heading, "", lines), collapse = "\n")
}

tempest_costorm_report_assert_quiescent <- function(session) {
  if (length(tempest_session_pending_deputy_runs(session)) > 0L) {
    tempest_costorm_session_abort(
      "Cannot finalize a report while Deputy execution remains pending."
    )
  }
  tempest_session_agent_completion_assert_quiescent(session)
  tempest_session_async_work_assert_quiescent(session)
  tempest_stage_records_validate(
    tempest_session_stage_records(session),
    allow_running = FALSE
  )
  invisible(NULL)
}

tempest_costorm_report_verify_async <- function(session, is_current) {
  workspace <- session$workspace
  items <- tempest_verification_work_items(workspace)
  existing <- workspace$list_claim_supports()
  item_keys <- vapply(
    items,
    function(item) {
      paste(
        item$claim@claim_id,
        item$span@evidence_span_id,
        item$span@source_id,
        sep = "\r"
      )
    },
    character(1)
  )
  support_keys <- vapply(
    existing,
    function(support) {
      paste(
        support@claim_id,
        support@evidence_span_id,
        support@source_id,
        sep = "\r"
      )
    },
    character(1)
  )
  if (length(existing) > 0L) {
    if (!identical(sort(item_keys), sort(support_keys))) {
      tempest_costorm_session_abort(
        paste0(
          "Co-STORM publication found partial or stale claim verification; ",
          "current evidence cannot be published."
        )
      )
    }
    return(promises::promise_resolve(existing))
  }
  if (length(items) == 0L) {
    return(promises::promise_resolve(list()))
  }

  program <- tempest_session_programs(session)$verify_claim_support
  chat <- tempest_session_chat(session, "extractor")
  verified_at <- tempest_now_utc()
  verifier_model <- tempest_research_model(session$config, "judge")
  pending <- list()
  running <- new.env(parent = emptyenv())
  results <- list()
  committed <- FALSE
  rollback_recorded <- FALSE
  collect <- function(record, output = NULL) {
    if (identical(record@status, "running")) {
      running[[record@attempt_id]] <- record
    }
    pending <<- tempest_stage_records_upsert(pending, record)
    invisible(record)
  }
  output_reference <- function(output, running_record, context) {
    tempest_stage_output_reference(
      "claim_supports",
      output@claim_support_id,
      content_digest = tempest_stage_verification_output_digest(
        output,
        running_record,
        context$claim,
        context$evidence_span,
        context$workspace
      )
    )
  }
  record_rollback <- function(error) {
    if (committed || rollback_recorded || length(pending) == 0L) {
      return(invisible(NULL))
    }
    rollback_recorded <<- TRUE
    stale <- !tempest_async_is_current(is_current)
    terminal <- lapply(pending, function(record) {
      if (record@status %in% c("failed", "cancelled")) {
        return(record)
      }
      original <- running[[record@attempt_id]]
      if (is.null(original)) {
        tempest_costorm_session_abort(
          "Co-STORM publication lost a running verification attempt."
        )
      }
      if (identical(record@status, "running") && stale) {
        return(tempest_stage_record_cancel(original))
      }
      tempest_stage_record_fail(
        original,
        error = error,
        kind = "commit",
        completed_at = if (identical(record@status, "succeeded")) {
          record@completed_at
        } else {
          tempest_now_utc()
        }
      )
    })
    tempest_session_record_stages(session, terminal)
    invisible(NULL)
  }
  run_one <- function(previous, index) {
    promises::then(previous, function(...) {
      if (!tempest_async_is_current(is_current)) {
        tempest_costorm_session_abort(
          "Co-STORM report verification was cancelled."
        )
      }
      item <- items[[index]]
      request <- tempest_execute_stage_async(
        program,
        chat,
        inputs = list(
          claim_text = item$claim@claim_text,
          source_excerpts = tempest_verification_span_input(
            item$claim,
            item$span,
            workspace
          )
        ),
        context = tempest_stage_context_knowledge_view(
          list(
            workspace = workspace,
            claim = item$claim,
            evidence_span = item$span,
            min_support_score = session$config@min_support_score,
            verified_at = verified_at,
            verifier_model = verifier_model
          ),
          program,
          tempest_session_knowledge_view(session)
        ),
        record_stage = collect,
        output_reference = output_reference,
        is_current = is_current
      )
      promises::then(request, function(stage_result) {
        results[[index]] <<- stage_result$output
        NULL
      })
    })
  }
  request <- Reduce(
    run_one,
    seq_along(items),
    init = promises::promise_resolve(NULL)
  )
  request <- promises::then(request, function(...) {
    if (!tempest_async_is_current(is_current)) {
      tempest_costorm_session_abort(
        "Co-STORM report verification was cancelled."
      )
    }
    records <- tempest_stage_records_upsert_many(
      tempest_session_stage_records(session),
      pending
    )
    workspace$verify_proposed_claims_batch(
      unname(results),
      verified_at = verified_at,
      min_support_score = session$config@min_support_score,
      verifier = verifier_model,
      .verification_owner_token = tempest_session_verification_owner_token(
        session
      ),
      commit = function() {
        session$.__enclos_env__$private$stage_records_value <- records
      }
    )
    committed <<- TRUE
    workspace$list_claim_supports()
  })
  promises::catch(request, onRejected = function(error) {
    record_rollback(error)
    stop(error)
  })
}

tempest_costorm_report_render <- function(
  session,
  style,
  include_references
) {
  context <- tempest_costorm_report_context(
    session,
    style,
    include_references
  )
  body <- tempest_costorm_report_body(session, context$style)
  report_md <- tempest_report_md_render(
    title = context$title,
    body = body,
    workspace = context$workspace,
    citation_policy = context$citation_policy,
    on_unsupported_claim = context$on_unsupported_claim,
    min_support_score = context$min_support_score,
    include_references = context$include_references
  )
  tempest_markdown_append_execution_review(
    report_md,
    context$execution_review,
    trusted_title = context$title
  )
}

tempest_session_commit_terminal_report <- function(
  session,
  manifest,
  report_md
) {
  tempest_session_assert_mutable(
    session,
    "finalize the report",
    allow_report_work = TRUE
  )
  if (
    !S7::S7_inherits(manifest, TempestResearchManifest) ||
      !identical(manifest@mode, "costorm") ||
      !identical(manifest@research_run_id, session$session_id) ||
      !identical(manifest@status, "succeeded") ||
      !rlang::is_string(report_md) ||
      is.na(report_md)
  ) {
    tempest_costorm_session_abort(
      "A terminal Co-STORM report commit is invalid."
    )
  }
  tempest_product_report_reference_validate(
    list(
      report_id = manifest@deliverables$report_md$report_id,
      sha256 = manifest@deliverables$report_md$sha256
    ),
    report_md
  )
  tempest_research_workspace_seal(
    session$workspace,
    tempest_session_verification_owner_token(session)
  )
  private <- session$.__enclos_env__$private
  private$manifest_value <- manifest
  private$report_md_value <- report_md
  invisible(report_md)
}

tempest_costorm_report_finalize <- function(
  session,
  style,
  include_references,
  is_current,
  work_id
) {
  tempest_session_assert_mutable(
    session,
    "generate a report",
    allow_report_work = TRUE
  )
  if (!tempest_async_is_current(is_current)) {
    return(NULL)
  }
  report_md <- tempest_costorm_report_render(
    session,
    style,
    include_references
  )
  tempest_final_report_validate(
    report_md,
    session$workspace,
    title = session$title,
    citation_policy = session$config@citation_policy,
    on_unsupported_claim = session$config@on_unsupported_claim,
    min_support_score = session$config@min_support_score,
    stage_records = tempest_session_stage_records(session)
  )
  if (!tempest_async_is_current(is_current)) {
    return(NULL)
  }
  tempest_session_async_work_assert_exclusive(session, work_id, "report")
  if (length(tempest_session_pending_deputy_runs(session)) > 0L) {
    tempest_costorm_session_abort(
      "Co-STORM publication found a pending Deputy execution."
    )
  }
  tempest_session_agent_completion_assert_quiescent(session)
  candidate <- tempest_product_authority_finalize_manifest(
    session$manifest,
    tempest_session_stage_records(session),
    session$workspace,
    report_md = report_md,
    config = session$config,
    experts = session$experts,
    expert_sessions = tempest_expert_sessions_snapshot(session),
    product_state = list(title = session$title),
    status = "succeeded",
    require_publishable = TRUE,
    deputy_traces = tempest_session_deputy_traces(session)
  )
  if (!tempest_async_is_current(is_current)) {
    return(NULL)
  }
  tempest_session_async_work_assert_exclusive(session, work_id, "report")
  if (length(tempest_session_pending_deputy_runs(session)) > 0L) {
    tempest_costorm_session_abort(
      "Co-STORM publication found a pending Deputy execution."
    )
  }
  tempest_session_agent_completion_assert_quiescent(session)
  tempest_session_commit_terminal_report(session, candidate, report_md)
  report_md
}

#' Read the committed Markdown report from a Co-STORM session
#'
#' This accessor returns the exact bytes already committed during Co-STORM
#' publication. It never generates, repairs, or republishes a report, and it
#' fails unless the session is succeeded, quiescent, and bound to the same
#' report reference as its research Manifest.
#'
#' @param session A `TempestSession`.
#' @return The exact committed Markdown report.
#' @examples
#' \dontrun{
#' session <- tempest_session("History of jazz", config = tempest_config())
#' session$step("Tell me about bebop.")
#' session$report()
#' md <- tempest_session_report_md(session)
#' }
#' @export
tempest_session_report_md <- function(session) {
  if (!inherits(session, "TempestSession")) {
    tempest_product_report_abort(
      "The canonical Co-STORM report requires a TempestSession."
    )
  }
  tryCatch(
    tempest_costorm_manifest_validate(
      session$manifest,
      session$session_id,
      session$config,
      session$workspace
    ),
    error = function(error) {
      tempest_product_report_abort(
        "The Co-STORM report manifest is not bound to this session.",
        parent = error
      )
    }
  )
  if (!identical(session$manifest@status, "succeeded")) {
    tempest_product_report_abort(
      "The canonical Co-STORM report is unavailable before publication."
    )
  }
  pending_runs <- tryCatch(
    tempest_session_pending_deputy_runs(session),
    error = function(error) {
      tempest_product_report_abort(
        "The canonical Co-STORM report has invalid Deputy execution state.",
        parent = error
      )
    }
  )
  if (length(pending_runs) > 0L) {
    tempest_product_report_abort(
      paste0(
        "The canonical Co-STORM report is unavailable while Deputy ",
        "execution remains pending."
      )
    )
  }
  tryCatch(
    tempest_session_agent_completion_assert_quiescent(session),
    error = function(error) {
      tempest_product_report_abort(
        paste0(
          "The canonical Co-STORM report requires quiescent agent ",
          "completion state."
        ),
        parent = error
      )
    }
  )
  tryCatch(
    tempest_session_async_work_assert_quiescent(session),
    error = function(error) {
      tempest_product_report_abort(
        paste0(
          "The canonical Co-STORM report requires quiescent product ",
          "work state."
        ),
        parent = error
      )
    }
  )
  report_md <- tempest_session_report_value(session)
  if (
    !rlang::is_string(report_md) ||
      is.na(report_md) ||
      !nzchar(report_md)
  ) {
    tempest_product_report_abort(
      "The canonical Co-STORM report artifact has no Markdown content."
    )
  }
  reference <- session$manifest@deliverables$report_md %||% NULL
  tempest_product_report_reference_validate(
    reference[c("report_id", "sha256")],
    report_md
  )
  tryCatch(
    tempest_product_authority_validate(
      manifest = session$manifest,
      stage_records = tempest_session_stage_records(session),
      workspace = session$workspace,
      report_md = report_md,
      report_reference = reference[c("report_id", "sha256")],
      config = session$config,
      experts = session$experts,
      expert_sessions = tempest_expert_sessions_snapshot(session),
      product_state = list(title = session$title),
      require_publishable = TRUE
    ),
    error = function(error) {
      tempest_product_report_abort(
        "The canonical Co-STORM report failed product authority validation.",
        parent = error
      )
    }
  )
  report_md
}
