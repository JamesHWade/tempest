#' @keywords internal
tempest_session_restore_abort <- function(message, parent = NULL) {
  tempest_abort(
    c("Cannot restore TempestSession snapshot.", x = message),
    class = tempest_session_persistence_error_class(
      "tempest_session_restore_error"
    ),
    parent = parent,
    .envir = rlang::caller_env()
  )
}

#' @keywords internal
tempest_expert_session_records_from_json <- function(records) {
  records <- tempest_persistence_exact_records(
    records,
    tempest_expert_session_record_fields(),
    "session expert-session records",
    tempest_session_persistence_error_class(
      "tempest_session_restore_error"
    )
  )
  lapply(records, function(record) {
    if (!identical(record$allowed_connection_ref_ids, list())) {
      tempest_session_restore_abort(
        "Stored product expert-session connection IDs must be an empty array."
      )
    }
    record$allowed_connection_ref_ids <- character()
    record
  })
}

#' @keywords internal
tempest_session_snapshot_value_abort <- function(
  message,
  action,
  parent = NULL
) {
  if (identical(action, "restore")) {
    tempest_session_restore_abort(message, parent = parent)
  }
  tempest_abort(
    message,
    class = tempest_session_persistence_error_class(
      "tempest_session_snapshot_error"
    ),
    parent = parent
  )
}

#' @keywords internal
tempest_session_snapshot_record <- function(
  value,
  field,
  action = "snapshot"
) {
  tryCatch(
    tempest_product_serializable_list(value %||% list(), field),
    error = function(error) {
      tempest_session_snapshot_value_abort(
        paste0("Cannot snapshot non-serializable ", field, " state."),
        action,
        parent = error
      )
    }
  )
}

#' @keywords internal
tempest_session_credential_free_value <- function(value, field, action) {
  sensitive <- tempest_contract_sensitive_values(value, field)
  if (length(sensitive) > 0L) {
    tempest_session_snapshot_value_abort(
      paste0(
        "Co-STORM ",
        field,
        " cannot contain credential or secret values."
      ),
      action
    )
  }
  invisible(value)
}

#' @keywords internal
tempest_session_transcript_record <- function(value, action = "snapshot") {
  if (is.null(value)) {
    tempest_session_snapshot_value_abort(
      "Co-STORM transcript cannot be literal null.",
      action
    )
  }
  value <- tempest_session_snapshot_record(
    value,
    "transcript",
    action = action
  )
  valid <- is.null(names(value)) &&
    all(vapply(
      value,
      function(turn) {
        is.list(turn) &&
          !is.data.frame(turn) &&
          identical(names(turn), c("speaker", "role", "text", "at")) &&
          all(vapply(
            turn[c("speaker", "text", "at")],
            function(field) {
              rlang::is_string(field) && !is.na(field) && nzchar(field)
            },
            logical(1)
          )) &&
          rlang::is_string(turn$role) &&
          !is.na(turn$role) &&
          turn$role %in% c("user", "assistant")
      },
      logical(1)
    ))
  if (!isTRUE(valid)) {
    tempest_session_snapshot_value_abort(
      "Co-STORM transcript must contain only canonical turn records.",
      action
    )
  }
  value
}

#' @keywords internal
tempest_session_mindmap_record <- function(value, action = "snapshot") {
  value <- tempest_session_snapshot_record(value, "mindmap", action = action)
  if (
    !identical(sort(names(value)), c("edges", "nodes")) ||
      !is.list(value$nodes) ||
      !is.null(names(value$nodes)) ||
      !is.list(value$edges) ||
      !is.null(names(value$edges))
  ) {
    tempest_session_snapshot_value_abort(
      "Co-STORM mind map must contain unnamed node and edge record lists.",
      action
    )
  }
  valid_string <- function(value, nullable = FALSE) {
    (isTRUE(nullable) && is.null(value)) ||
      (rlang::is_string(value) && !is.na(value) && nzchar(value))
  }
  valid_ids <- function(value) {
    if (identical(action, "restore")) {
      return(
        !is.null(value) &&
          is.list(value) &&
          !is.data.frame(value) &&
          is.null(names(value)) &&
          all(vapply(value, valid_string, logical(1)))
      )
    }
    if (is.null(value)) {
      return(TRUE)
    }
    if (
      is.character(value) &&
        !is.object(value) &&
        is.null(names(value))
    ) {
      return(!anyNA(value) && all(nzchar(value)))
    }
    is.list(value) &&
      !is.data.frame(value) &&
      is.null(names(value)) &&
      all(vapply(value, valid_string, logical(1)))
  }
  nodes_valid <- vapply(
    value$nodes,
    function(node) {
      fields <- names(node)
      source_ids_valid <- FALSE
      if (is.list(node) && !is.data.frame(node)) {
        source_ids_valid <- if (identical(action, "restore")) {
          "source_ids" %in% fields && valid_ids(node$source_ids)
        } else {
          valid_ids(node$source_ids %||% character())
        }
      }
      is.list(node) &&
        !is.data.frame(node) &&
        !is.null(fields) &&
        !anyNA(fields) &&
        !anyDuplicated(fields) &&
        all(c("id", "label") %in% fields) &&
        all(fields %in% c("id", "label", "parent", "notes", "source_ids")) &&
        valid_string(node$id) &&
        valid_string(node$label) &&
        valid_string(node$parent %||% NULL, nullable = TRUE) &&
        (is.null(node$notes) || rlang::is_string(node$notes)) &&
        source_ids_valid
    },
    logical(1)
  )
  edges_valid <- vapply(
    value$edges,
    function(edge) {
      fields <- names(edge)
      is.list(edge) &&
        !is.data.frame(edge) &&
        !is.null(fields) &&
        !anyNA(fields) &&
        !anyDuplicated(fields) &&
        all(c("from", "to") %in% fields) &&
        all(fields %in% c("from", "to", "relation")) &&
        valid_string(edge$from) &&
        valid_string(edge$to) &&
        (is.null(edge$relation) || rlang::is_string(edge$relation))
    },
    logical(1)
  )
  node_ids <- vapply(value$nodes, function(node) node$id %||% "", character(1))
  if (
    !all(nodes_valid) ||
      !all(edges_valid) ||
      anyDuplicated(node_ids)
  ) {
    tempest_session_snapshot_value_abort(
      "Co-STORM mind map contains malformed node or edge records.",
      action
    )
  }
  value$nodes <- lapply(value$nodes, function(node) {
    node$source_ids <- tempest_product_character_array(
      node$source_ids %||% character(),
      "node$source_ids"
    )
    node
  })
  value
}

#' @keywords internal
tempest_session_mindmap_binding_abort <- function(message, action) {
  if (identical(action, "restore")) {
    tempest_session_restore_abort(message)
  }
  if (identical(action, "snapshot")) {
    tempest_session_snapshot_value_abort(message, action = "snapshot")
  }
  tempest_abort(
    message,
    class = c(
      "tempest_session_mindmap_error",
      "tempest_session_error",
      "tempest_error"
    )
  )
}

#' @keywords internal
tempest_session_mindmap_assert_binding <- function(
  mindmap,
  workspace,
  action = c("restore", "snapshot", "update")
) {
  action <- match.arg(action)
  node_ids <- vapply(mindmap$nodes, `[[`, character(1), "id")
  if (!"root" %in% node_ids) {
    tempest_session_mindmap_binding_abort(
      "The Co-STORM mind map must contain the canonical root node.",
      action
    )
  }
  invalid_parent <- vapply(
    mindmap$nodes,
    function(node) {
      parent <- node$parent %||% NULL
      if (identical(node$id, "root")) {
        return(!is.null(parent))
      }
      is.null(parent) || !parent %in% node_ids || identical(parent, node$id)
    },
    logical(1)
  )
  invalid_edge <- vapply(
    mindmap$edges,
    function(edge) {
      !edge$from %in% node_ids || !edge$to %in% node_ids
    },
    logical(1)
  )
  resource_ids <- vapply(
    workspace$list_retrieved_resources(),
    tempest_resource_identity,
    character(1)
  )
  invalid_sources <- vapply(
    mindmap$nodes,
    function(node) {
      source_ids <- tempest_product_character_array(
        node$source_ids %||% character(),
        "node$source_ids"
      )
      anyDuplicated(source_ids) ||
        length(setdiff(source_ids, resource_ids)) > 0L
    },
    logical(1)
  )
  parents <- stats::setNames(
    lapply(mindmap$nodes, \(node) node$parent %||% NULL),
    node_ids
  )
  reaches_root <- vapply(
    node_ids,
    function(node_id) {
      visited <- character()
      current <- node_id
      while (!identical(current, "root")) {
        if (current %in% visited || is.null(parents[[current]])) {
          return(FALSE)
        }
        visited <- c(visited, current)
        current <- parents[[current]]
      }
      TRUE
    },
    logical(1)
  )
  if (
    any(invalid_parent) ||
      any(invalid_edge) ||
      any(invalid_sources) ||
      !all(reaches_root)
  ) {
    tempest_session_mindmap_binding_abort(
      paste0(
        "The Co-STORM mind map contains an unknown or cyclic parent, ",
        "edge endpoint, or evidence source id."
      ),
      action
    )
  }
  invisible(mindmap)
}

#' @keywords internal
tempest_session_portable_snapshot <- function(snapshot, action = "snapshot") {
  portable <- snapshot[c("transcript", "mindmap", "progress_events")]
  for (field in names(portable)) {
    tryCatch(
      tempest_product_canonical_list(
        list(value = portable[[field]]),
        field
      ),
      error = function(error) {
        tempest_session_snapshot_value_abort(
          paste0(
            "Co-STORM snapshot contains non-portable ",
            field,
            " state."
          ),
          action,
          parent = error
        )
      }
    )
  }
  whole_snapshot <- snapshot[setdiff(names(snapshot), "graft_snapshot")]
  tryCatch(
    tempest_product_canonical_list(
      whole_snapshot,
      "Co-STORM session snapshot"
    ),
    error = function(error) {
      tempest_session_snapshot_value_abort(
        paste0(
          "Co-STORM snapshot contains a non-portable Agent, proxy, ",
          "runtime object, or value."
        ),
        action,
        parent = error
      )
    }
  )
  for (field in c("topic", "title", "session_id")) {
    value <- snapshot[[field]]
    if (
      !rlang::is_string(value) ||
        is.na(value) ||
        !nzchar(tempest_trim(value))
    ) {
      tempest_session_snapshot_value_abort(
        paste0("Co-STORM snapshot requires non-empty scalar ", field, "."),
        action
      )
    }
  }
  for (field in c("topic", "title")) {
    tempest_session_credential_free_value(snapshot[[field]], field, action)
  }
  package_version <- snapshot$package_version
  if (
    !is.null(package_version) &&
      (!rlang::is_string(package_version) || is.na(package_version))
  ) {
    tempest_session_snapshot_value_abort(
      "Co-STORM snapshot package version must be one string or null.",
      action
    )
  }
  invisible(snapshot)
}

#' @keywords internal
tempest_session_suggested_questions <- function(value, action = "snapshot") {
  if (identical(action, "restore")) {
    if (
      is.null(value) ||
        !is.list(value) ||
        is.data.frame(value) ||
        !is.null(names(value))
    ) {
      tempest_session_restore_abort(
        "Stored suggested questions must be one unnamed JSON array."
      )
    }
    valid <- vapply(
      value,
      \(question) {
        is.character(question) &&
          length(question) == 1L &&
          !is.na(question)
      },
      logical(1)
    )
    if (!all(valid)) {
      tempest_session_restore_abort(
        "Stored suggested questions must contain only exact strings."
      )
    }
    value <- if (length(value) == 0L) {
      character()
    } else {
      unlist(value, use.names = FALSE)
    }
  } else {
    value <- value %||% character()
    if (is.list(value) && is.null(names(value))) {
      valid <- vapply(
        value,
        \(question) {
          is.character(question) &&
            length(question) == 1L &&
            !is.na(question)
        },
        logical(1)
      )
      if (all(valid)) {
        value <- unlist(value, use.names = FALSE)
      }
    }
  }
  if (!is.character(value) || anyNA(value)) {
    message <- "Suggested questions must be a character vector without missing values."
    if (identical(action, "restore")) {
      tempest_session_restore_abort(message)
    }
    tempest_abort(
      message,
      class = tempest_session_persistence_error_class(
        "tempest_session_snapshot_error"
      )
    )
  }
  tempest_session_credential_free_value(
    value,
    "suggested_questions",
    action
  )
  unname(value)
}

#' @keywords internal
tempest_session_snapshot_fields <- function() {
  c(
    "schema_version",
    "package_version",
    "research_manifest",
    "topic",
    "title",
    "session_id",
    "experts",
    "transcript",
    "mindmap",
    "report_md",
    "report_reference",
    "suggested_questions",
    "progress_events",
    "stage_records",
    "workspace",
    "expert_sessions",
    "graft_snapshot"
  )
}

#' @keywords internal
tempest_session_report_record <- function(value, action = "snapshot") {
  if (is.null(value)) {
    return(NULL)
  }
  if (!rlang::is_string(value)) {
    message <- "The Co-STORM report must be one Markdown string or NULL."
    if (identical(action, "restore")) {
      tempest_session_restore_abort(message)
    }
    tempest_abort(
      message,
      class = tempest_session_persistence_error_class(
        "tempest_session_snapshot_error"
      )
    )
  }
  value <- enc2utf8(value)
  tempest_session_credential_free_value(value, "report_md", action)
  value
}

tempest_session_assert_no_pending_deputy_runs <- function(
  session,
  action = c("snapshot", "save")
) {
  action <- match.arg(action)
  class <- tempest_session_persistence_error_class(
    paste0("tempest_session_", action, "_error")
  )
  pending_runs <- tryCatch(
    tempest_session_pending_deputy_runs(session),
    error = function(error) {
      tempest_abort(
        paste0(
          "Cannot ",
          action,
          " Co-STORM state with an invalid pending Deputy run registry."
        ),
        class = class,
        parent = error
      )
    }
  )
  if (length(pending_runs) > 0L) {
    tempest_abort(
      paste0(
        "Cannot ",
        action,
        " Co-STORM state while Deputy executions await terminal traces."
      ),
      class = class
    )
  }
  invisible(NULL)
}

tempest_session_assert_persistence_quiescent <- function(
  session,
  action = c("snapshot", "save")
) {
  action <- match.arg(action)
  class <- tempest_session_persistence_error_class(
    paste0("tempest_session_", action, "_error")
  )
  tempest_session_assert_no_pending_deputy_runs(session, action = action)
  tryCatch(
    tempest_session_agent_completion_assert_quiescent(session),
    error = function(error) {
      tempest_abort(
        paste0(
          "Cannot ",
          action,
          " Co-STORM state while agent completions remain issued or processing."
        ),
        class = class,
        parent = error
      )
    }
  )
  tryCatch(
    tempest_session_async_work_assert_quiescent(session),
    error = function(error) {
      tempest_abort(
        paste0(
          "Cannot ",
          action,
          " Co-STORM state while product work remains queued or active."
        ),
        class = class,
        parent = error
      )
    }
  )
  records <- tryCatch(
    tempest_session_stage_records(session),
    error = function(error) {
      tempest_abort(
        paste0("Cannot ", action, " invalid Co-STORM stage history."),
        class = class,
        parent = error
      )
    }
  )
  if (
    any(vapply(
      records,
      \(record) identical(record@status, "running"),
      logical(1)
    ))
  ) {
    tempest_abort(
      paste0(
        "Cannot ",
        action,
        " Co-STORM state while a stage attempt is running."
      ),
      class = class
    )
  }
  invisible(NULL)
}

#' Snapshot a Co-STORM session
#'
#' `r lifecycle::badge("experimental")`
#'
#' `tempest_session_snapshot()` returns a structured, in-memory representation
#' of the durable state in a [TempestSession]. The only supported snapshot is
#' the exact current schema-9 product shape; no legacy or migration reader is
#' provided. It includes the research
#' manifest; fixed session and configuration identity; the authoritative
#' [ResearchWorkspace]; expert profiles; transcript and mind map; the latest
#' report Markdown; stage-record, progress-event, and expert-session metadata;
#' and the optional immutable Graft snapshot. Live chat
#' handles, runtime clients, tools, closures, generic workflows, generic
#' artifact catalogs, Shiny reactive state, credentials, and provider request
#' bodies are not included.
#'
#' Use [tempest_session_restore()] to rebuild a session from the returned list,
#' or [tempest_session_save()] to write the same durable state to a directory
#' bundle. A stage attempt that is still running is projected to a cancelled
#' durable record without changing the live session.
#'
#' @param session A [TempestSession] object.
#' @return A list containing an exact schema-9 session snapshot.
#' @export
tempest_session_snapshot <- function(session) {
  if (!inherits(session, "TempestSession")) {
    tempest_abort(
      "{.arg session} must be a {.cls TempestSession} object.",
      class = tempest_session_persistence_error_class(
        "tempest_session_snapshot_error"
      )
    )
  }
  tempest_session_assert_persistence_quiescent(session, action = "snapshot")
  research_manifest <- tryCatch(
    tempest_costorm_manifest_validate(
      session$manifest,
      session$session_id,
      session$config,
      session$workspace
    ),
    error = function(error) {
      tempest_abort(
        "Cannot snapshot an inconsistent Co-STORM research manifest.",
        class = tempest_session_persistence_error_class(
          "tempest_session_snapshot_error"
        ),
        parent = error
      )
    }
  )
  tryCatch(
    {
      live_programs <- tempest_program_set_manifest_programs(
        tempest_session_program_set(session)
      )
      if (
        !tempest_program_set_identity_equal(
          live_programs,
          research_manifest@programs
        )
      ) {
        tempest_ecosystem_contract_abort(
          "The live Co-STORM ProgramSet does not match its research manifest."
        )
      }
    },
    error = function(error) {
      tempest_abort(
        "Cannot snapshot an inconsistent Co-STORM ProgramSet.",
        class = tempest_session_persistence_error_class(
          "tempest_session_snapshot_error"
        ),
        parent = error
      )
    }
  )
  durable_records <- tryCatch(
    {
      live_records <- tempest_session_stage_records(session)
      deputy_traces <- tempest_session_deputy_traces(session)
      expert_sessions <- tempest_expert_sessions_snapshot(session)
      expert_ids <- vapply(
        session$experts,
        \(expert) expert@expert_id,
        character(1)
      )
      durable_records <- tempest_stage_records_interrupt(
        live_records,
        completed_at = tempest_now_utc()
      )
      research_manifest <- tempest_product_authority_bind_stage_records(
        research_manifest,
        durable_records,
        deputy_traces = deputy_traces,
        expert_ids = expert_ids,
        expert_sessions = expert_sessions
      )
      tempest_product_authority_validate_stage_records(
        research_manifest,
        durable_records,
        deputy_traces = deputy_traces,
        expert_ids = expert_ids,
        expert_sessions = expert_sessions
      )
      tempest_stage_records_validate_workspace(
        durable_records,
        session$workspace,
        min_support_score = session$config@min_support_score
      )
      tempest_stage_records_validate_persisted_trust(
        durable_records,
        session$workspace,
        min_support_score = session$config@min_support_score
      )
      tempest_stage_records_validate_workspace_coverage(
        durable_records,
        session$workspace,
        require_verification = session$config@citation_policy %in%
          c("claim_verified", "strict")
      )
      tempest_stage_records_validate_product_outputs(
        durable_records,
        list(experts = session$experts)
      )
      tempest_stage_records_validate_generated_experts(
        durable_records,
        session$experts
      )
      tempest_stage_records_validate_claim_provenance(
        durable_records,
        session$workspace,
        research_manifest@research_run_id,
        session$experts
      )
      durable_records
    },
    error = function(error) {
      tempest_abort(
        "Cannot snapshot inconsistent Co-STORM stage records.",
        class = tempest_session_persistence_error_class(
          "tempest_session_snapshot_error"
        ),
        parent = error
      )
    }
  )
  report_md <- tryCatch(
    {
      value <- tempest_session_report_record(
        tempest_session_report_value(session)
      )
      value <- tempest_product_report_for_stage_records(
        value,
        durable_records,
        prior_records = live_records,
        trusted_title = session$title
      )
      tempest_product_report_validate_policy(
        value,
        session$title,
        session$workspace,
        session$config,
        durable_records
      )
      value
    },
    error = function(error) {
      tempest_abort(
        "Cannot snapshot an invalid Co-STORM report product.",
        class = tempest_session_persistence_error_class(
          "tempest_session_snapshot_error"
        ),
        parent = error
      )
    }
  )
  report_reference <- tempest_product_report_reference(report_md)
  research_manifest <- tryCatch(
    tempest_product_authority_bind_report(research_manifest, report_md),
    error = function(error) {
      tempest_abort(
        "Cannot bind the Co-STORM report to its research manifest.",
        class = tempest_session_persistence_error_class(
          "tempest_session_snapshot_error"
        ),
        parent = error
      )
    }
  )
  workspace <- tryCatch(
    tempest_research_workspace_snapshot(session$workspace),
    error = function(error) {
      tempest_abort(
        "Cannot snapshot an invalid Co-STORM research workspace.",
        class = tempest_session_persistence_error_class(
          "tempest_session_snapshot_error"
        ),
        parent = error
      )
    }
  )
  mindmap <- tempest_session_mindmap_record(
    session$mindmap,
    action = "snapshot"
  )
  tempest_session_mindmap_assert_binding(
    mindmap,
    session$workspace,
    action = "snapshot"
  )
  mindmap$nodes <- lapply(mindmap$nodes, function(node) {
    node$source_ids <- as.list(unname(node$source_ids))
    node
  })
  suggested_questions <- tempest_session_suggested_questions(
    tempest_session_suggestions(session)
  )
  graft_snapshot <- session$workspace$graft_snapshot
  tryCatch(
    tempest_graft_snapshot_assert_binding(
      graft_snapshot,
      research_manifest@knowledge_snapshot,
      session$workspace,
      tempest_session_persistence_error_class(
        "tempest_session_snapshot_error"
      ),
      "Co-STORM Graft snapshot"
    ),
    error = function(error) {
      tempest_abort(
        "Cannot snapshot inconsistent accepted-knowledge state.",
        class = tempest_session_persistence_error_class(
          "tempest_session_snapshot_error"
        ),
        parent = error
      )
    }
  )
  tryCatch(
    tempest_product_authority_validate(
      research_manifest,
      durable_records,
      session$workspace,
      report_md = report_md,
      report_reference = report_reference,
      config = session$config,
      experts = session$experts,
      expert_sessions = expert_sessions,
      product_state = list(title = session$title),
      require_publishable = !is.null(report_md)
    ),
    error = function(error) {
      tempest_abort(
        "Cannot snapshot Co-STORM state without exact product authority.",
        class = tempest_session_persistence_error_class(
          "tempest_session_snapshot_error"
        ),
        parent = error
      )
    }
  )

  snapshot <- list(
    schema_version = 9L,
    package_version = tryCatch(
      as.character(utils::packageVersion("tempest")),
      error = function(e) NA_character_
    ),
    research_manifest = tempest_research_manifest_record(research_manifest),
    topic = session$topic,
    title = session$title,
    session_id = session$session_id,
    experts = tempest_expert_records(session$experts),
    transcript = tempest_session_transcript_record(
      session$transcript,
      action = "snapshot"
    ),
    mindmap = mindmap,
    report_md = report_md,
    report_reference = report_reference,
    suggested_questions = as.list(suggested_questions),
    progress_events = tempest_session_restore_progress_events(
      tempest_execution_events(session),
      session_id = session$session_id,
      action = "snapshot"
    ),
    stage_records = tempest_stage_records_data(durable_records),
    workspace = workspace,
    expert_sessions = expert_sessions,
    graft_snapshot = graft_snapshot
  )
  tempest_persistence_credential_audit(
    snapshot,
    "Co-STORM session snapshot",
    tempest_session_persistence_error_class(
      "tempest_session_snapshot_error"
    )
  )
  tempest_session_portable_snapshot(snapshot)
  snapshot
}

#' @keywords internal
tempest_session_restore_expert_sessions <- function(session, expert_sessions) {
  manager <- tempest_session_expert_manager(session)
  expert_sessions <- tempest_persistence_exact_records(
    expert_sessions,
    tempest_expert_session_record_fields(),
    "session expert-session records",
    tempest_session_persistence_error_class(
      "tempest_session_restore_error"
    )
  )
  if (length(expert_sessions) > 0L) {
    session_ids <- vapply(
      expert_sessions,
      function(binding) {
        value <- binding$session_id
        if (!rlang::is_string(value) || is.na(value)) {
          return("")
        }
        value
      },
      character(1)
    )
    expert_ids <- vapply(
      expert_sessions,
      function(binding) {
        value <- binding$expert_id
        if (!rlang::is_string(value) || is.na(value)) {
          return("")
        }
        value
      },
      character(1)
    )
    if (
      any(!nzchar(session_ids)) ||
        any(!nzchar(expert_ids)) ||
        anyDuplicated(session_ids) ||
        anyDuplicated(expert_ids)
    ) {
      tempest_session_restore_abort(
        paste0(
          "Expert-session records require unique session ids and one ",
          "session per expert."
        )
      )
    }
  }
  for (expert_session in expert_sessions) {
    if (
      !identical(expert_session$allowed_connection_ref_ids, character()) ||
        !identical(expert_session$grants, list()) ||
        !tempest_ledger_timestamp_valid(expert_session$created_at)
    ) {
      tempest_session_restore_abort(
        paste0(
          "Expert sessions require exact empty product capability fields ",
          "and one canonical creation timestamp."
        )
      )
    }
    expert_ids <- vapply(
      session$experts,
      \(expert) expert@expert_id,
      character(1)
    )
    idx <- match(expert_session$expert_id, expert_ids)
    if (is.na(idx)) {
      tempest_session_restore_abort(
        paste0(
          "Expert session ",
          expert_session$session_id,
          " does not match a restored expert."
        )
      )
    }
    expert <- session$experts[[idx]]
    if (
      !identical(expert@version, expert_session$expert_version) ||
        !identical(
          tempest_expert_profile_fingerprint(expert),
          expert_session$expert_fingerprint
        )
    ) {
      tempest_session_restore_abort(
        paste0(
          "Expert session ",
          expert_session$session_id,
          " does not match the restored expert version and fingerprint."
        )
      )
    }
    tryCatch(
      manager$restore_session(expert_session),
      error = function(error) {
        tempest_session_restore_abort(
          paste0(
            "Expert session ",
            expert_session$session_id,
            " could not be restored through the product Deputy manager."
          )
        )
      }
    )
    restored <- manager$session_profile(
      expert_session$session_id
    )
    for (field in c(
      "session_id",
      "expert_id",
      "expert_version",
      "expert_fingerprint",
      "model_role",
      "allowed_connection_ref_ids",
      "grants",
      "created_at"
    )) {
      restored_value <- restored[[field]]
      saved_value <- expert_session[[field]]
      if (!identical(restored_value, saved_value)) {
        tempest_session_restore_abort(
          paste0(
            "Rehydrated expert session ",
            expert_session$session_id,
            " changed its pinned profile binding."
          )
        )
      }
    }
  }
  invisible(session)
}

#' Restore a Co-STORM session from a snapshot
#'
#' `r lifecycle::badge("experimental")`
#'
#' `tempest_session_restore()` rebuilds a [TempestSession] from a structured
#' snapshot created by [tempest_session_snapshot()] or read by
#' [tempest_session_resume()]. It restores the research manifest and
#' authoritative workspace, and creates fresh chat/tool handles using `config`.
#' Only the exact current schema-9 snapshot is accepted; older, future, missing,
#' extra, coerced, or mismatched shapes are rejected rather than migrated.
#'
#' Historical progress events are restored as session artifact data and can be
#' reduced with [tempest_progress_state()]. They are not replayed into the new
#' `progress` callback; future calls on the restored session use that callback.
#' Stage-record history is restored for audit, but running attempts are rejected
#' rather than resumed.
#'
#' @param snapshot A list from [tempest_session_snapshot()].
#' @param config Runtime [TempestConfig] used to recreate chats, retrievers, and
#'   tools.
#' @param progress Optional callback for future `tempest_progress_event`
#'   objects.
#' @param program_set A [TempestProgramSet] carrying the same program
#'   identities recorded in the snapshot. If `NULL`, the builtin set is used.
#' @param knowledge_view Optional transient immutable Graft view required by
#'   future execution when `program_set` contains governed procedures. It is
#'   never reconstructed from or written to persistence.
#' @return A restored [TempestSession].
#' @export
tempest_session_restore <- function(
  snapshot,
  config = tempest_config(),
  progress = NULL,
  program_set = NULL,
  knowledge_view = NULL
) {
  tempest_session_restore_internal(
    snapshot = snapshot,
    config = config,
    progress = progress,
    program_set = program_set,
    knowledge_view = knowledge_view
  )
}

#' @keywords internal
tempest_session_restore_internal <- function(
  snapshot,
  config = tempest_config(),
  progress = NULL,
  program_set = NULL,
  knowledge_view = NULL
) {
  if (!is.list(snapshot)) {
    tempest_session_restore_abort("{.arg snapshot} must be a list.")
  }
  schema_version <- tempest_persistence_schema_version(
    snapshot$schema_version %||% NA_integer_,
    "Session snapshot schema version",
    tempest_session_persistence_error_class(
      "tempest_session_restore_error"
    )
  )
  if (!identical(schema_version, 9L)) {
    tempest_product_unsupported_format_abort(
      "TempestSession snapshot format",
      schema_version,
      tempest_session_persistence_error_class(
        "tempest_session_restore_error"
      )
    )
  }
  snapshot_fields <- names(snapshot)
  if (!identical(snapshot_fields, tempest_session_snapshot_fields())) {
    tempest_product_unsupported_format_abort(
      "TempestSession snapshot format",
      schema_version,
      tempest_session_persistence_error_class(
        "tempest_session_restore_error"
      )
    )
  }
  tempest_persistence_credential_audit(
    snapshot,
    "Co-STORM session snapshot",
    tempest_session_persistence_error_class(
      "tempest_session_restore_error"
    )
  )
  tempest_session_portable_snapshot(snapshot, action = "restore")
  snapshot$transcript <- tempest_session_transcript_record(
    snapshot$transcript,
    action = "restore"
  )
  snapshot$mindmap <- tempest_session_mindmap_record(
    snapshot$mindmap,
    action = "restore"
  )
  tempest_research_workspace_require_current_schema(
    snapshot$workspace,
    "Schema 9 session workspace",
    tempest_session_persistence_error_class(
      "tempest_session_restore_error"
    )
  )
  if (
    !rlang::is_string(snapshot$topic) || !nzchar(tempest_trim(snapshot$topic))
  ) {
    tempest_session_restore_abort("Snapshot must include a non-empty topic.")
  }
  if (
    !rlang::is_string(snapshot$title) || !nzchar(tempest_trim(snapshot$title))
  ) {
    tempest_session_restore_abort("Snapshot must include a non-empty title.")
  }
  if (
    !rlang::is_string(snapshot$session_id) ||
      !nzchar(tempest_trim(snapshot$session_id))
  ) {
    tempest_session_restore_abort(
      "Snapshot must include a non-empty session id."
    )
  }
  experts <- tempest_experts_from_records(
    snapshot$experts %||% list(),
    what = "session expert profiles",
    class = tempest_session_persistence_error_class(
      "tempest_session_restore_error"
    )
  )
  expert_ids <- vapply(experts, \(expert) expert@expert_id, character(1))
  research_manifest <- tryCatch(
    tempest_research_manifest_from_record(snapshot$research_manifest),
    error = function(error) {
      tempest_session_restore_abort(
        paste0(
          "Snapshot contains an invalid research manifest: ",
          conditionMessage(error)
        )
      )
    }
  )
  if (!identical(research_manifest@research_run_id, snapshot$session_id)) {
    tempest_session_restore_abort(
      "Snapshot session id does not match its research manifest run id."
    )
  }
  stage_records <- tryCatch(
    {
      records <- tempest_stage_records_from_data(
        snapshot$stage_records,
        allow_running = FALSE
      )
      tempest_product_authority_validate_stage_records(
        research_manifest,
        records,
        expert_ids = expert_ids,
        expert_sessions = snapshot$expert_sessions
      )
      records
    },
    error = function(error) {
      tempest_session_restore_abort(
        "Snapshot contains invalid or mismatched stage-record history.",
        parent = error
      )
    }
  )
  tryCatch(
    tempest_stage_records_validate_execution_review(
      snapshot$report_md,
      stage_records,
      trusted_title = snapshot$title
    ),
    error = function(error) {
      tempest_session_restore_abort(
        "Snapshot report is missing its exact execution disclosure.",
        parent = error
      )
    }
  )
  workspace <- tryCatch(
    tempest_research_workspace_restore(
      snapshot$workspace,
      graft_snapshot = snapshot$graft_snapshot
    ),
    error = function(error) {
      tempest_session_restore_abort(
        paste0(
          "Snapshot contains an invalid ResearchWorkspace: ",
          conditionMessage(error)
        )
      )
    }
  )
  tryCatch(
    tempest_stage_records_validate_workspace(
      stage_records,
      workspace,
      min_support_score = config@min_support_score
    ),
    error = function(error) {
      tempest_session_restore_abort(
        "Snapshot stage-record history does not match its ResearchWorkspace.",
        parent = error
      )
    }
  )
  tryCatch(
    {
      tempest_stage_records_validate_persisted_trust(
        stage_records,
        workspace,
        min_support_score = config@min_support_score
      )
      tempest_stage_records_validate_workspace_coverage(
        stage_records,
        workspace,
        require_verification = config@citation_policy %in%
          c("claim_verified", "strict")
      )
      tempest_stage_records_validate_generated_experts(
        stage_records,
        experts
      )
      tempest_stage_records_validate_claim_provenance(
        stage_records,
        workspace,
        research_manifest@research_run_id,
        experts
      )
      tempest_product_authority_validate_report(
        research_manifest,
        snapshot$report_reference,
        snapshot$report_md
      )
      tempest_product_report_validate_policy(
        snapshot$report_md,
        snapshot$title,
        workspace,
        config,
        stage_records
      )
    },
    error = function(error) {
      tempest_session_restore_abort(
        "Snapshot product history or report binding is invalid.",
        parent = error
      )
    }
  )
  tryCatch(
    tempest_stage_records_validate_product_outputs(
      stage_records,
      list(experts = snapshot$experts)
    ),
    error = function(error) {
      tempest_session_restore_abort(
        "Snapshot stage-record history does not match Co-STORM product state.",
        parent = error
      )
    }
  )
  tryCatch(
    tempest_graft_snapshot_assert_binding(
      snapshot$graft_snapshot,
      research_manifest@knowledge_snapshot,
      workspace,
      tempest_session_persistence_error_class(
        "tempest_session_restore_error"
      ),
      "Restored Co-STORM Graft snapshot"
    ),
    error = function(error) {
      tempest_session_restore_abort(
        paste0(
          "Snapshot accepted-knowledge identity is invalid: ",
          conditionMessage(error)
        )
      )
    }
  )
  program_set <- program_set %||% tempest_program_set()
  tryCatch(
    {
      tempest_costorm_manifest_validate(
        research_manifest,
        snapshot$session_id,
        config,
        workspace
      )
      declared_programs <- tempest_program_set_manifest_programs(program_set)
      if (
        !tempest_program_set_identity_equal(
          declared_programs,
          research_manifest@programs
        )
      ) {
        tempest_ecosystem_contract_abort(
          "The supplied ProgramSet does not match the Co-STORM manifest."
        )
      }
    },
    error = function(error) {
      tempest_session_restore_abort(
        paste0(
          "Snapshot research identity does not match the restore inputs: ",
          conditionMessage(error)
        )
      )
    }
  )
  tryCatch(
    tempest_product_authority_validate(
      research_manifest,
      stage_records,
      workspace,
      report_md = snapshot$report_md,
      report_reference = snapshot$report_reference,
      config = config,
      experts = experts,
      expert_sessions = snapshot$expert_sessions,
      product_state = list(title = snapshot$title),
      require_publishable = !is.null(snapshot$report_md)
    ),
    error = function(error) {
      tempest_session_restore_abort(
        "Snapshot lacks exact durable product authority.",
        parent = error
      )
    }
  )
  tempest_session_mindmap_assert_binding(snapshot$mindmap, workspace)

  retriever <- tempest_retriever(config = config, workspace = workspace)
  session <- tempest_session_restore_new(
    topic = snapshot$topic,
    config = config,
    experts = experts,
    retriever = retriever,
    progress = NULL,
    session_id = snapshot$session_id,
    program_set = program_set,
    knowledge_view = knowledge_view,
    manifest = research_manifest
  )

  tempest_session_restore_product_state(
    session,
    title = snapshot$title,
    transcript = snapshot$transcript,
    mindmap = snapshot$mindmap,
    events = snapshot$progress_events,
    progress = progress
  )
  tempest_session_set_report_value(
    session,
    tempest_session_report_record(snapshot$report_md, action = "restore")
  )

  tempest_session_set_suggestions(
    session,
    tempest_session_suggested_questions(
      snapshot$suggested_questions,
      action = "restore"
    )
  )
  tempest_session_restore_expert_sessions(
    session,
    snapshot$expert_sessions
  )
  tempest_session_set_stage_records(session, stage_records)
  session
}

#' @keywords internal
tempest_session_bundle_path <- function(path, ...) {
  file.path(path, ...)
}

#' @keywords internal
tempest_session_bundle_write_json <- function(path, rel_path, value) {
  tryCatch(
    tempest_product_write_json(
      tempest_session_bundle_path(path, rel_path),
      value
    ),
    error = function(error) {
      tempest_abort(
        "Could not write session bundle file {.path {rel_path}}.",
        class = tempest_session_persistence_error_class(
          "tempest_session_save_error"
        ),
        parent = error
      )
    }
  )
  rel_path
}

#' @keywords internal
tempest_session_bundle_write_text <- function(path, rel_path, value) {
  if (!rlang::is_string(value)) {
    return(character())
  }
  tryCatch(
    tempest_write_text(tempest_session_bundle_path(path, rel_path), value),
    error = function(error) {
      tempest_abort(
        "Could not write session bundle file {.path {rel_path}}.",
        class = tempest_session_persistence_error_class(
          "tempest_session_save_error"
        ),
        parent = error
      )
    }
  )
  rel_path
}

#' @keywords internal
tempest_session_prepare_bundle_dir <- function(path, overwrite = FALSE) {
  if (!rlang::is_string(path) || !nzchar(tempest_trim(path))) {
    tempest_abort(
      "{.arg path} must be a single non-empty path string.",
      class = tempest_session_persistence_error_class(
        "tempest_session_save_error"
      )
    )
  }

  expanded_path <- path.expand(path)
  if (tempest_persistence_leaf_path_is_symlink(expanded_path)) {
    tempest_abort(
      "{.arg path} cannot be a symbolic link.",
      class = tempest_session_persistence_error_class(
        "tempest_session_save_error"
      )
    )
  }
  bundle_dir <- normalizePath(
    expanded_path,
    winslash = "/",
    mustWork = FALSE
  )
  if (file.exists(bundle_dir)) {
    if (!dir.exists(bundle_dir)) {
      tempest_abort(
        "{.arg path} must point to a directory.",
        class = tempest_session_persistence_error_class(
          "tempest_session_save_error"
        )
      )
    }
    entries <- list.files(bundle_dir, all.files = TRUE, no.. = TRUE)
    if (length(entries) > 0) {
      if (!isTRUE(overwrite)) {
        tempest_abort(
          c(
            "Session bundle directory already exists.",
            i = "Use {.code overwrite = TRUE} to replace it.",
            x = "Path: {.path {bundle_dir}}."
          ),
          class = tempest_session_persistence_error_class(
            "tempest_session_save_error"
          )
        )
      }
      tryCatch(
        {
          tempest_persistence_require_regular_bundle_files(
            bundle_dir,
            "session.json",
            "Existing Co-STORM root manifest",
            tempest_session_persistence_error_class(
              "tempest_session_save_error"
            )
          )
          existing_manifest <- tempest_product_read_json(
            file.path(bundle_dir, "session.json"),
            what = "existing Co-STORM session manifest",
            class = tempest_session_persistence_error_class(
              "tempest_session_save_error"
            )
          )
          tempest_session_bundle_validate_manifest(
            bundle_dir,
            existing_manifest
          )
        },
        error = function(error) {
          tempest_abort(
            "Refusing to overwrite an invalid existing Co-STORM bundle.",
            class = tempest_session_persistence_error_class(
              "tempest_session_save_error"
            ),
            parent = error
          )
        }
      )
    }
  }

  dir.create(dirname(bundle_dir), recursive = TRUE, showWarnings = FALSE)
  normalizePath(bundle_dir, winslash = "/", mustWork = FALSE)
}

#' @keywords internal
tempest_session_commit_bundle <- function(staging_dir, bundle_dir) {
  tempest_product_atomic_commit_bundle(
    staging_dir,
    bundle_dir,
    class = tempest_session_persistence_error_class(
      "tempest_session_save_error"
    ),
    what = "session bundle"
  )
}

#' Save a Co-STORM session bundle
#'
#' `r lifecycle::badge("experimental")`
#'
#' `tempest_session_save()` writes a schema-versioned directory bundle for a
#' [TempestSession]. The exact current format is schema 9, with no legacy or
#' compatibility writer. The bundle stores the research manifest, authoritative
#' workspace, explicit stage-record history, optional immutable Graft snapshot,
#' and narrow report product. Every declared file is checksummed, and the
#' `session.json` manifest is written last. Generic workflow and
#' artifact-catalog state, live chat handles, registered tool closures, Shiny
#' reactive state, credentials, and raw provider request bodies are not
#' serialized. A stage attempt that is still running is written as cancelled
#' without changing the live session.
#'
#' Use [tempest_session_resume()] to load the bundle with a fresh runtime
#' [TempestConfig].
#'
#' @param session A [TempestSession] object.
#' @param path Directory where the session bundle should be written.
#' @param overwrite If `TRUE`, replace an existing bundle directory.
#' @return Invisibly returns the normalized bundle directory.
#' @export
tempest_session_save <- function(
  session,
  path,
  overwrite = FALSE
) {
  if (!inherits(session, "TempestSession")) {
    tempest_abort(
      "{.arg session} must be a {.cls TempestSession} object.",
      class = tempest_session_persistence_error_class(
        "tempest_session_save_error"
      )
    )
  }
  tempest_session_assert_persistence_quiescent(session, action = "save")
  tempest_require("jsonlite", "Tempest session persistence requires jsonlite.")

  bundle_dir <- tempest_session_prepare_bundle_dir(path, overwrite = overwrite)
  staging_dir <- tempfile(
    pattern = paste0(".", basename(bundle_dir), "-staging-"),
    tmpdir = dirname(bundle_dir)
  )
  dir.create(staging_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(staging_dir, recursive = TRUE, force = TRUE), add = TRUE)
  snapshot <- tempest_session_snapshot(session)
  files <- character()

  files <- c(
    files,
    tempest_session_bundle_write_json(
      staging_dir,
      "experts.json",
      snapshot$experts
    ),
    tempest_session_bundle_write_json(
      staging_dir,
      "expert_sessions.json",
      snapshot$expert_sessions
    ),
    tempest_session_bundle_write_json(
      staging_dir,
      "transcript.json",
      snapshot$transcript
    ),
    tempest_session_bundle_write_json(
      staging_dir,
      "mindmap.json",
      snapshot$mindmap
    ),
    tempest_session_bundle_write_json(
      staging_dir,
      "progress_events.json",
      snapshot$progress_events
    ),
    tempest_session_bundle_write_json(
      staging_dir,
      "stage_records.json",
      snapshot$stage_records
    ),
    tempest_session_bundle_write_json(
      staging_dir,
      "workspace/retrieved_resources.json",
      snapshot$workspace$retrieved_resources
    ),
    tempest_session_bundle_write_json(
      staging_dir,
      "workspace/proposed_claims.json",
      snapshot$workspace$proposed_claims
    ),
    tempest_session_bundle_write_json(
      staging_dir,
      "workspace/evidence_spans.json",
      snapshot$workspace$evidence_spans
    ),
    tempest_session_bundle_write_json(
      staging_dir,
      "workspace/disputes.json",
      snapshot$workspace$disputes
    ),
    tempest_session_bundle_write_json(
      staging_dir,
      "workspace/claim_supports.json",
      snapshot$workspace$claim_supports
    )
  )
  if (!is.null(snapshot$report_md)) {
    files <- c(
      files,
      tempest_session_bundle_write_text(
        staging_dir,
        "report.md",
        snapshot$report_md
      )
    )
  }
  files <- c(
    files,
    tempest_graft_snapshot_write(
      staging_dir,
      snapshot$graft_snapshot,
      tempest_session_persistence_error_class(
        "tempest_session_save_error"
      )
    )
  )

  if (length(snapshot$suggested_questions) > 0L) {
    files <- c(
      files,
      tempest_session_bundle_write_json(
        staging_dir,
        "artifacts/suggested_questions.json",
        snapshot$suggested_questions
      )
    )
  }

  files <- sort(unique(files))
  checksums <- stats::setNames(
    lapply(files, function(file) {
      tempest_product_bundle_checksum(staging_dir, file)
    }),
    files
  )
  manifest <- list(
    schema_version = snapshot$schema_version,
    bundle_type = "costorm",
    bundle_status = "complete",
    package_version = snapshot$package_version,
    session_id = snapshot$session_id,
    topic = snapshot$topic,
    title = snapshot$title,
    research_manifest = snapshot$research_manifest,
    report_reference = snapshot$report_reference,
    workspace = snapshot$workspace[
      tempest_session_bundle_workspace_fields()
    ],
    saved_at = tempest_now_utc(),
    files = as.list(unname(files)),
    checksums = checksums
  )
  tempest_session_bundle_write_json(staging_dir, "session.json", manifest)
  tempest_persistence_require_regular_bundle_files(
    staging_dir,
    "session.json",
    "Co-STORM staged root manifest",
    tempest_session_persistence_error_class("tempest_session_save_error")
  )
  tryCatch(
    tempest_session_bundle_validate_manifest(staging_dir, manifest),
    error = function(error) {
      tempest_abort(
        "The completed staged Co-STORM bundle failed validation.",
        class = tempest_session_persistence_error_class(
          "tempest_session_save_error"
        ),
        parent = error
      )
    }
  )

  invisible(tempest_session_commit_bundle(staging_dir, bundle_dir))
}

#' @keywords internal
tempest_session_bundle_optional_json <- function(
  path,
  default = NULL,
  what,
  partial_recovery = FALSE
) {
  if (!file.exists(path)) {
    return(default)
  }
  tryCatch(
    tempest_product_read_json(
      path,
      what = what,
      class = tempest_session_persistence_error_class(
        "tempest_session_restore_error"
      )
    ),
    error = function(error) {
      if (!isTRUE(partial_recovery)) {
        stop(error)
      }
      tempest_warn("Skipping malformed {what} during partial recovery.")
      default
    }
  )
}

#' @keywords internal
tempest_session_progress_event_fields <- function() {
  c(
    "event_id",
    "run_id",
    "workflow",
    "event_type",
    "stage",
    "step",
    "status",
    "timestamp",
    "message",
    "payload",
    "parent_event_id",
    "correlation_id",
    "sequence"
  )
}

#' @keywords internal
tempest_session_restore_progress_events <- function(
  events,
  session_id,
  action = "restore"
) {
  if (
    !is.list(events) ||
      is.data.frame(events) ||
      !is.null(names(events))
  ) {
    tempest_session_snapshot_value_abort(
      "Session progress events must be an unnamed list.",
      action
    )
  }
  event_fields <- tempest_session_progress_event_fields()
  event_props <- setdiff(event_fields, "sequence")
  optional <- c(
    "stage",
    "step",
    "message",
    "parent_event_id",
    "correlation_id"
  )
  events <- lapply(events, function(event) {
    if (
      !is.list(event) ||
        is.data.frame(event) ||
        is.null(names(event)) ||
        anyNA(names(event)) ||
        anyDuplicated(names(event)) ||
        !identical(names(event), event_fields)
    ) {
      tempest_session_snapshot_value_abort(
        "Session progress events must contain exactly the current fields.",
        action
      )
    }
    for (field in optional) {
      if (
        is.character(event[[field]]) &&
          length(event[[field]]) == 1L &&
          is.na(event[[field]])
      ) {
        event[field] <- list(NULL)
      }
    }
    event
  })
  events <- tempest_session_snapshot_record(
    events,
    "progress_events",
    action = action
  )
  records <- lapply(events, function(event) {
    for (field in optional) {
      if (is.null(event[[field]])) {
        event[[field]] <- NA_character_
      }
    }
    sequence <- event$sequence
    if (!tempest_exact_integer_scalar_valid(sequence, minimum = 1L)) {
      tempest_session_snapshot_value_abort(
        "Session progress-event sequences must be explicit positive integers.",
        action
      )
    }
    record <- tryCatch(
      tempest_progress_event_data(do.call(
        tempest_progress_event,
        event[event_props]
      )),
      error = function(error) {
        tempest_session_snapshot_value_abort(
          "Session progress events contain invalid values.",
          action,
          parent = error
        )
      }
    )
    record$sequence <- sequence
    if (identical(action, "snapshot")) {
      for (field in optional) {
        if (is.na(record[[field]])) {
          record[field] <- list(NULL)
        }
      }
    }
    record
  })
  if (length(records) == 0L) {
    return(list())
  }
  sequences <- vapply(records, `[[`, integer(1), "sequence")
  event_ids <- vapply(records, `[[`, character(1), "event_id")
  run_ids <- vapply(records, `[[`, character(1), "run_id")
  workflows <- vapply(records, `[[`, character(1), "workflow")
  if (
    !identical(sequences, seq_along(records)) ||
      anyDuplicated(event_ids) ||
      any(run_ids != session_id) ||
      any(workflows != "costorm")
  ) {
    tempest_session_snapshot_value_abort(
      paste0(
        "Session progress events require contiguous sequences, unique ids, ",
        "and exact Co-STORM session binding."
      ),
      action
    )
  }
  records
}

#' @keywords internal
tempest_session_bundle_optional_presentation_files <- function() {
  "artifacts/suggested_questions.json"
}

#' @keywords internal
tempest_session_bundle_manifest_fields <- function() {
  c(
    "schema_version",
    "bundle_type",
    "bundle_status",
    "package_version",
    "session_id",
    "topic",
    "title",
    "research_manifest",
    "report_reference",
    "workspace",
    "saved_at",
    "files",
    "checksums"
  )
}

#' @keywords internal
tempest_session_bundle_workspace_fields <- function() {
  c(
    "schema_version",
    "base_snapshot_id",
    "max_sources",
    "accepted_graft_references"
  )
}

#' @keywords internal
tempest_session_bundle_validate_manifest <- function(
  bundle_dir,
  manifest,
  partial_recovery = FALSE
) {
  schema_version <- tempest_persistence_schema_version(
    manifest$schema_version %||% NA_integer_,
    "Session bundle schema version",
    tempest_session_persistence_error_class(
      "tempest_session_restore_error"
    )
  )
  if (!identical(schema_version, 9L)) {
    tempest_product_unsupported_format_abort(
      "Co-STORM bundle format",
      schema_version,
      tempest_session_persistence_error_class(
        "tempest_session_restore_error"
      )
    )
  }
  manifest_fields <- names(manifest)
  if (!identical(manifest_fields, tempest_session_bundle_manifest_fields())) {
    tempest_product_unsupported_format_abort(
      "Co-STORM bundle format",
      schema_version,
      tempest_session_persistence_error_class(
        "tempest_session_restore_error"
      )
    )
  }
  if (
    !identical(manifest$bundle_type %||% "", "costorm") ||
      !identical(manifest$bundle_status %||% "", "complete")
  ) {
    tempest_session_restore_abort(
      "Schema 9 Co-STORM bundle envelope is not complete."
    )
  }
  if (
    !is.list(manifest$research_manifest) ||
      !is.list(manifest$workspace)
  ) {
    tempest_session_restore_abort(
      "Schema 9 Co-STORM bundle is missing research identity metadata."
    )
  }
  workspace_fields <- tempest_session_bundle_workspace_fields()
  if (!identical(names(manifest$workspace), workspace_fields)) {
    tempest_session_restore_abort(
      "Schema 9 Co-STORM bundle has invalid workspace identity metadata."
    )
  }
  tempest_research_workspace_require_current_schema(
    manifest$workspace,
    "Schema 9 Co-STORM workspace identity",
    tempest_session_persistence_error_class(
      "tempest_session_restore_error"
    )
  )
  files <- tempest_persistence_manifest_files(
    manifest$files,
    "Schema 9 Co-STORM file inventory",
    tempest_session_persistence_error_class(
      "tempest_session_restore_error"
    )
  )
  tempest_persistence_require_regular_bundle_files(
    bundle_dir,
    files,
    "Co-STORM bundle",
    tempest_session_persistence_error_class(
      "tempest_session_restore_error"
    )
  )
  evidence_required <- c(
    "workspace/retrieved_resources.json",
    "workspace/proposed_claims.json",
    "workspace/evidence_spans.json",
    "workspace/disputes.json",
    "workspace/claim_supports.json"
  )
  core_required <- c(
    "experts.json",
    "expert_sessions.json",
    "transcript.json",
    "mindmap.json",
    "progress_events.json",
    "stage_records.json",
    evidence_required
  )
  optional_presentation <- tempest_session_bundle_optional_presentation_files()
  snapshot_path <- tempest_graft_snapshot_relative_path()
  knowledge_reference <- manifest$research_manifest$knowledge_snapshot %||%
    list()
  pinned <- length(knowledge_reference) > 0L
  required <- c(core_required, if (pinned) snapshot_path else character())
  allowed <- c(
    core_required,
    optional_presentation,
    "report.md",
    if (pinned) snapshot_path else character()
  )
  normalized_files <- gsub("\\\\", "/", files)
  physical_files <- setdiff(
    gsub(
      "\\\\",
      "/",
      list.files(
        bundle_dir,
        recursive = TRUE,
        all.files = TRUE,
        no.. = TRUE
      )
    ),
    "session.json"
  )
  undeclared_on_disk <- setdiff(physical_files, normalized_files)
  parts <- strsplit(normalized_files, "/", fixed = TRUE)
  unsafe <- grepl("^(/|~|[A-Za-z]:)", normalized_files) |
    vapply(
      parts,
      function(parts) {
        any(!nzchar(parts)) || any(parts %in% c(".", ".."))
      },
      logical(1)
    )
  missing <- files[!file.exists(file.path(bundle_dir, files))]
  checksums <- tempest_persistence_manifest_checksums(
    manifest$checksums,
    files,
    "Schema 9 Co-STORM checksum inventory",
    tempest_session_persistence_error_class(
      "tempest_session_restore_error"
    )
  )
  missing_checksums <- setdiff(files, names(checksums))
  extra_checksums <- setdiff(names(checksums), files)
  available <- setdiff(files[!unsafe], missing)
  stage_record_path <- file.path(bundle_dir, "stage_records.json")
  non_regular_stage_records <- if (
    file.exists(stage_record_path) &&
      (!utils::file_test("-f", stage_record_path) ||
        nzchar(Sys.readlink(stage_record_path)))
  ) {
    "stage_records.json"
  } else {
    character()
  }
  bundle_root <- paste0(
    normalizePath(bundle_dir, winslash = "/", mustWork = TRUE),
    "/"
  )
  escaping <- available[vapply(
    available,
    function(file) {
      resolved <- normalizePath(
        file.path(bundle_dir, file),
        winslash = "/",
        mustWork = TRUE
      )
      !startsWith(resolved, bundle_root)
    },
    logical(1)
  )]
  checksum_candidates <- setdiff(
    available,
    c(escaping, non_regular_stage_records)
  )
  mismatched <- checksum_candidates[vapply(
    checksum_candidates,
    function(file) {
      expected <- if (file %in% names(checksums)) {
        checksums[[file]]
      } else {
        NA_character_
      }
      is.na(expected) ||
        !identical(tempest_product_bundle_checksum(bundle_dir, file), expected)
    },
    logical(1)
  )]
  undeclared_required <- setdiff(required, files)
  unexpected_files <- setdiff(files, allowed)
  snapshot_exists <- file.exists(file.path(bundle_dir, snapshot_path))

  structural_problems <- c(
    if (length(files) == 0L) "Manifest declares no files.",
    if (anyDuplicated(normalized_files)) {
      "Manifest declares duplicate file paths."
    },
    if (any(unsafe)) "Manifest contains unsafe file paths.",
    if (length(undeclared_required) > 0L) {
      paste0(
        "Manifest omits required files: ",
        paste(undeclared_required, collapse = ", "),
        "."
      )
    },
    if (length(unexpected_files) > 0L) {
      paste0(
        "Manifest declares unsupported files: ",
        paste(unexpected_files, collapse = ", "),
        "."
      )
    },
    if (length(undeclared_on_disk) > 0L) {
      paste0(
        "Bundle contains undeclared files: ",
        paste(undeclared_on_disk, collapse = ", "),
        "."
      )
    },
    if (length(non_regular_stage_records) > 0L) {
      "The stage-record sidecar must be a regular non-symlink file."
    },
    if (!identical(pinned, snapshot_path %in% files)) {
      paste0(
        "The Graft snapshot sidecar and research manifest must either both ",
        "be declared or both be absent."
      )
    },
    if (!pinned && snapshot_exists) {
      "An unpinned bundle cannot contain a Graft snapshot sidecar."
    },
    if (length(extra_checksums) > 0L) {
      paste0(
        "Manifest contains checksums for undeclared files: ",
        paste(extra_checksums, collapse = ", "),
        "."
      )
    }
  )

  invalid_files <- unique(c(
    missing,
    missing_checksums,
    escaping,
    mismatched
  ))
  invalid_optional <- intersect(invalid_files, optional_presentation)
  invalid_required <- setdiff(invalid_files, optional_presentation)
  required_problems <- c(
    if (length(intersect(missing, invalid_required)) > 0L) {
      paste0(
        "Required files are missing: ",
        paste(intersect(missing, invalid_required), collapse = ", "),
        "."
      )
    },
    if (length(intersect(missing_checksums, invalid_required)) > 0L) {
      paste0(
        "Required files have no checksum: ",
        paste(
          intersect(missing_checksums, invalid_required),
          collapse = ", "
        ),
        "."
      )
    },
    if (length(intersect(escaping, invalid_required)) > 0L) {
      paste0(
        "Required files resolve outside the bundle: ",
        paste(intersect(escaping, invalid_required), collapse = ", "),
        "."
      )
    },
    if (length(intersect(mismatched, invalid_required)) > 0L) {
      paste0(
        "Required files failed checksum validation: ",
        paste(intersect(mismatched, invalid_required), collapse = ", "),
        "."
      )
    }
  )
  fatal_problems <- c(structural_problems, required_problems)
  if (length(fatal_problems) > 0L) {
    tempest_session_restore_abort(paste(fatal_problems, collapse = " "))
  }
  if (length(invalid_optional) > 0L && !isTRUE(partial_recovery)) {
    tempest_session_restore_abort(paste0(
      "Optional presentation files failed integrity validation: ",
      paste(invalid_optional, collapse = ", "),
      "."
    ))
  }
  if (length(invalid_optional) > 0L) {
    tempest_warn(c(
      "Recovering an incomplete Tempest session bundle.",
      i = paste0(
        "Skipping optional presentation files: ",
        paste(invalid_optional, collapse = ", "),
        "."
      )
    ))
  }
  invisible(setdiff(files, invalid_optional))
}

#' @keywords internal
tempest_costorm_bundle_validate_product <- function(snapshot) {
  schema_version <- tempest_persistence_schema_version(
    snapshot$schema_version %||% NA_integer_,
    "Session snapshot schema version",
    tempest_session_persistence_error_class(
      "tempest_session_restore_error"
    )
  )
  if (!identical(schema_version, 9L)) {
    tempest_product_unsupported_format_abort(
      "TempestSession snapshot format",
      schema_version,
      tempest_session_persistence_error_class(
        "tempest_session_restore_error"
      )
    )
  }
  if (!identical(names(snapshot), tempest_session_snapshot_fields())) {
    tempest_product_unsupported_format_abort(
      "TempestSession snapshot format",
      schema_version,
      tempest_session_persistence_error_class(
        "tempest_session_restore_error"
      )
    )
  }
  tempest_persistence_credential_audit(
    snapshot,
    "Co-STORM session snapshot",
    tempest_session_persistence_error_class(
      "tempest_session_restore_error"
    )
  )
  tempest_session_portable_snapshot(snapshot, action = "restore")
  tempest_session_transcript_record(
    snapshot$transcript,
    action = "restore"
  )
  mindmap <- tempest_session_mindmap_record(
    snapshot$mindmap,
    action = "restore"
  )
  tempest_session_suggested_questions(
    snapshot$suggested_questions,
    action = "restore"
  )
  tempest_session_restore_progress_events(
    snapshot$progress_events,
    session_id = snapshot$session_id,
    action = "restore"
  )
  experts <- tempest_experts_from_records(
    snapshot$experts,
    what = "session expert profiles",
    class = tempest_session_persistence_error_class(
      "tempest_session_restore_error"
    )
  )
  expert_ids <- vapply(experts, \(expert) expert@expert_id, character(1))
  research_manifest <- tryCatch(
    tempest_research_manifest_from_record(snapshot$research_manifest),
    error = function(error) {
      tempest_session_restore_abort(
        paste0(
          "Snapshot contains an invalid research manifest: ",
          conditionMessage(error)
        )
      )
    }
  )
  if (!identical(research_manifest@mode, "costorm")) {
    tempest_session_restore_abort(
      "Snapshot research manifest is not a Co-STORM product."
    )
  }
  if (!research_manifest@status %in% c("running", "succeeded")) {
    tempest_session_restore_abort(
      paste0(
        "A Co-STORM session can restore only a running or succeeded research ",
        "manifest."
      )
    )
  }
  if (!identical(research_manifest@research_run_id, snapshot$session_id)) {
    tempest_session_restore_abort(
      "Snapshot session id does not match its research manifest run id."
    )
  }
  if (
    !setequal(names(research_manifest@programs), tempest_program_set_stages())
  ) {
    tempest_session_restore_abort(
      "Snapshot research manifest does not record the complete ProgramSet."
    )
  }
  stage_records <- tryCatch(
    {
      records <- tempest_stage_records_from_data(
        snapshot$stage_records,
        allow_running = FALSE
      )
      tempest_product_authority_validate_stage_records(
        research_manifest,
        records,
        expert_ids = expert_ids,
        expert_sessions = snapshot$expert_sessions
      )
      records
    },
    error = function(error) {
      tempest_session_restore_abort(
        "Snapshot contains invalid or mismatched stage-record history.",
        parent = error
      )
    }
  )
  workspace <- tryCatch(
    tempest_research_workspace_restore(
      snapshot$workspace,
      graft_snapshot = snapshot$graft_snapshot
    ),
    error = function(error) {
      tempest_session_restore_abort(
        paste0(
          "Snapshot contains an invalid ResearchWorkspace: ",
          conditionMessage(error)
        )
      )
    }
  )
  knowledge_snapshot <- research_manifest@knowledge_snapshot
  snapshot_id <- knowledge_snapshot$snapshot_id %||% NULL
  workspace_snapshot <- tryCatch(
    tempest_costorm_manifest_snapshot_reference(workspace),
    error = function(error) {
      tempest_session_restore_abort(
        "Snapshot workspace lacks its exact accepted-knowledge identity.",
        parent = error
      )
    }
  )
  if (
    !identical(snapshot_id, workspace$base_snapshot_id) ||
      !identical(knowledge_snapshot, workspace_snapshot)
  ) {
    tempest_session_restore_abort(
      paste0(
        "Snapshot research manifest does not match the exact ",
        "ResearchWorkspace base snapshot."
      )
    )
  }
  tryCatch(
    {
      tempest_product_authority_validate_report(
        research_manifest,
        snapshot$report_reference,
        snapshot$report_md
      )
      tempest_stage_records_validate_execution_review(
        snapshot$report_md,
        stage_records,
        trusted_title = snapshot$title
      )
      tempest_stage_records_validate_generated_experts(stage_records, experts)
      tempest_stage_records_validate_claim_provenance(
        stage_records,
        workspace,
        research_manifest@research_run_id,
        experts
      )
      tempest_stage_records_validate_product_outputs(
        stage_records,
        list(experts = snapshot$experts)
      )
    },
    error = function(error) {
      tempest_session_restore_abort(
        "Snapshot product history or report binding is invalid.",
        parent = error
      )
    }
  )
  tryCatch(
    tempest_graft_snapshot_assert_binding(
      snapshot$graft_snapshot,
      knowledge_snapshot,
      workspace,
      tempest_session_persistence_error_class(
        "tempest_session_restore_error"
      ),
      "Restored Co-STORM Graft snapshot"
    ),
    error = function(error) {
      tempest_session_restore_abort(
        paste0(
          "Snapshot accepted-knowledge identity is invalid: ",
          conditionMessage(error)
        )
      )
    }
  )
  tempest_session_mindmap_assert_binding(mindmap, workspace)
  invisible(snapshot)
}

#' @keywords internal
tempest_costorm_bundle_read <- function(
  path,
  partial_recovery = FALSE,
  archive = FALSE
) {
  if (!rlang::is_string(path) || !nzchar(tempest_trim(path))) {
    if (isTRUE(archive)) {
      tempest_session_restore_abort(
        "{.arg path} must be a single non-empty extracted bundle directory."
      )
    }
    tempest_abort(
      "{.arg path} must be a single non-empty path string.",
      class = tempest_session_persistence_error_class(
        "tempest_session_restore_error"
      )
    )
  }
  expanded_path <- path.expand(path)
  if (
    isTRUE(archive) &&
      tempest_persistence_leaf_path_is_symlink(expanded_path)
  ) {
    tempest_session_restore_abort(
      "Extracted Co-STORM archive directory cannot be a symbolic link."
    )
  }
  bundle_dir <- normalizePath(
    expanded_path,
    winslash = "/",
    mustWork = FALSE
  )
  root_what <- if (isTRUE(archive)) {
    "Extracted Co-STORM archive root manifest"
  } else {
    "Co-STORM root manifest"
  }
  tempest_persistence_require_regular_bundle_files(
    bundle_dir,
    "session.json",
    root_what,
    tempest_session_persistence_error_class(
      "tempest_session_restore_error"
    )
  )
  manifest_path <- file.path(bundle_dir, "session.json")
  if (isTRUE(archive)) {
    manifest_size <- file.info(manifest_path)$size
    if (
      length(manifest_size) != 1L ||
        is.na(manifest_size) ||
        !is.finite(manifest_size) ||
        manifest_size > 50 * 1024^2
    ) {
      tempest_session_restore_abort(
        "Extracted Co-STORM archive has an unbounded session manifest."
      )
    }
  }
  manifest_what <- if (isTRUE(archive)) {
    "extracted Co-STORM archive manifest"
  } else {
    "session bundle manifest"
  }
  manifest <- tempest_product_read_json(
    manifest_path,
    what = manifest_what,
    class = tempest_session_persistence_error_class(
      "tempest_session_restore_error"
    )
  )
  schema_version <- tempest_persistence_schema_version(
    manifest$schema_version %||% NA_integer_,
    "Session bundle schema version",
    tempest_session_persistence_error_class(
      "tempest_session_restore_error"
    )
  )
  if (!identical(schema_version, 9L)) {
    tempest_product_unsupported_format_abort(
      "Co-STORM bundle format",
      schema_version,
      tempest_session_persistence_error_class(
        "tempest_session_restore_error"
      )
    )
  }
  declared_files <- tempest_session_bundle_validate_manifest(
    bundle_dir,
    manifest,
    partial_recovery = partial_recovery
  )
  strict_json <- function(rel_path, what) {
    tempest_product_read_json(
      file.path(bundle_dir, rel_path),
      what = what,
      class = tempest_session_persistence_error_class(
        "tempest_session_restore_error"
      )
    )
  }
  optional_json <- function(rel_path, default = NULL, what) {
    if (!rel_path %in% declared_files) {
      return(default)
    }
    tempest_session_bundle_optional_json(
      file.path(bundle_dir, rel_path),
      default = default,
      what = what,
      partial_recovery = partial_recovery
    )
  }
  report_md <- if ("report.md" %in% declared_files) {
    tryCatch(
      tempest_read_text(file.path(bundle_dir, "report.md")),
      error = function(error) {
        tempest_session_restore_abort(
          paste0(
            "Could not read the persisted Co-STORM report: ",
            conditionMessage(error)
          )
        )
      }
    )
  } else {
    NULL
  }
  graft_snapshot <- tempest_graft_snapshot_read(
    bundle_dir,
    declared_files = declared_files,
    manifest_reference = manifest$research_manifest$knowledge_snapshot %||%
      list(),
    class = tempest_session_persistence_error_class(
      "tempest_session_restore_error"
    )
  )
  snapshot <- list(
    schema_version = schema_version,
    package_version = manifest$package_version %||% NA_character_,
    research_manifest = manifest$research_manifest,
    topic = manifest$topic,
    title = manifest$title,
    session_id = manifest$session_id,
    experts = strict_json(
      "experts.json",
      what = "session expert profiles"
    ),
    transcript = strict_json(
      "transcript.json",
      what = "session transcript"
    ),
    mindmap = strict_json(
      "mindmap.json",
      what = "session mind map"
    ),
    report_md = report_md,
    report_reference = manifest$report_reference,
    suggested_questions = optional_json(
      "artifacts/suggested_questions.json",
      default = list(),
      what = "suggested questions artifact"
    ),
    progress_events = strict_json(
      "progress_events.json",
      what = "progress-event history"
    ),
    stage_records = strict_json(
      "stage_records.json",
      what = "stage-record history"
    ),
    expert_sessions = tempest_expert_session_records_from_json(
      strict_json(
        "expert_sessions.json",
        what = "expert-session metadata"
      )
    ),
    graft_snapshot = graft_snapshot
  )
  workspace <- manifest$workspace
  workspace$retrieved_resources <- strict_json(
    "workspace/retrieved_resources.json",
    what = "session retrieved-resource ledger"
  )
  workspace$proposed_claims <- strict_json(
    "workspace/proposed_claims.json",
    what = "session proposed-claim ledger"
  )
  workspace$evidence_spans <- strict_json(
    "workspace/evidence_spans.json",
    what = "session evidence-span ledger"
  )
  workspace$disputes <- strict_json(
    "workspace/disputes.json",
    what = "session dispute ledger"
  )
  workspace["claim_supports"] <- list(
    strict_json(
      "workspace/claim_supports.json",
      what = "session claim-support ledger"
    )
  )
  workspace <- workspace[tempest_research_workspace_snapshot_fields()]
  snapshot$workspace <- workspace
  snapshot <- snapshot[tempest_session_snapshot_fields()]
  tempest_costorm_bundle_validate_product(snapshot)
  list(
    path = normalizePath(bundle_dir, winslash = "/", mustWork = TRUE),
    snapshot = snapshot
  )
}

#' @keywords internal
tempest_costorm_archive_read <- function(path) {
  tempest_costorm_bundle_read(path, archive = TRUE)$path
}

#' Resume a saved Co-STORM session bundle
#'
#' `tempest_session_resume()` reads a directory bundle written by
#' [tempest_session_save()] and rebuilds a [TempestSession] with a fresh runtime
#' [TempestConfig]. Only the exact current schema-9 bundle is accepted; no
#' compatibility or migration reader is provided. Historical progress events are
#' loaded for display and reduction, but they are not replayed into `progress`.
#' Stage-record history is restored for audit, but running attempts are rejected
#' rather than resumed.
#'
#' @param path Directory containing a session bundle.
#' @param config Runtime [TempestConfig] used to recreate chats, retrievers, and
#'   tools.
#' @param progress Optional callback for future `tempest_progress_event`
#'   objects.
#' @param partial_recovery Whether to allow explicitly requested recovery of
#'   the optional `artifacts/suggested_questions.json` presentation file when
#'   it fails integrity checks. All durable state, including stage records,
#'   experts, Workspace, report, and Graft snapshot state, remains mandatory
#'   and must pass integrity checks.
#' @param program_set A [TempestProgramSet] carrying the same program
#'   identities recorded in the bundle. If `NULL`, the builtin set is used.
#' @param knowledge_view Optional transient immutable Graft view required by
#'   future execution when `program_set` contains governed procedures. It is
#'   never reconstructed from or written to persistence.
#' @return A restored [TempestSession].
#' @export
tempest_session_resume <- function(
  path,
  config = tempest_config(),
  progress = NULL,
  partial_recovery = FALSE,
  program_set = NULL,
  knowledge_view = NULL
) {
  tempest_session_resume_internal(
    path = path,
    config = config,
    progress = progress,
    partial_recovery = partial_recovery,
    program_set = program_set,
    knowledge_view = knowledge_view
  )
}

#' @keywords internal
tempest_session_resume_internal <- function(
  path,
  config = tempest_config(),
  progress = NULL,
  partial_recovery = FALSE,
  program_set = NULL,
  knowledge_view = NULL
) {
  bundle <- tempest_costorm_bundle_read(
    path,
    partial_recovery = partial_recovery
  )
  tempest_session_restore_internal(
    bundle$snapshot,
    config = config,
    progress = progress,
    program_set = program_set,
    knowledge_view = knowledge_view
  )
}
