# Artifact identities in read-only reviews; no admission or execution occurs here.
tempest_trajectory_artifact_digest <- function(value, noun) {
  tempest_trajectory_scalar_string(value, noun)
  if (!grepl("^[a-f0-9]{64}$", value)) {
    tempest_trajectory_review_abort(
      "{noun} must be an artifact SHA-256 digest."
    )
  }
  invisible(value)
}

tempest_trajectory_artifact_label <- function(value, noun) {
  if (
    !rlang::is_string(value) ||
      is.na(value) ||
      !validUTF8(enc2utf8(value)) ||
      !nzchar(value) ||
      !identical(trimws(value), value) ||
      nchar(enc2utf8(value), type = "bytes") > 1024L ||
      grepl("[[:cntrl:]]", value)
  ) {
    tempest_trajectory_review_abort(
      "{noun} must be unpadded UTF-8 text of at most 1024 bytes without control characters."
    )
  }
  invisible(value)
}

tempest_trajectory_decision <- function(event, selection) {
  if (
    !identical(event$action, "accept") || !identical(event$selection, selection)
  ) {
    tempest_trajectory_review_abort(
      "The recorded decision must accept the exact publication selection."
    )
  }
  tempest_trajectory_exact_whole_number(
    event$sequence,
    "Artifact decision sequence",
    minimum = 1
  )
  if (event$sequence > .Machine$integer.max) {
    tempest_trajectory_review_abort("Artifact decision sequence is too large.")
  }
  result <- list(
    decision_id = event$id,
    stream = event$stream,
    sequence = as.integer(event$sequence),
    selection_id = event$selection,
    purpose = event$purpose
  )
  tempest_trajectory_validate_decision(result)
  result
}

tempest_trajectory_validate_decision <- function(value) {
  tempest_trajectory_exact_record(
    value,
    tempest_trajectory_acceptance_fields(),
    "Artifact decision"
  )
  tempest_trajectory_artifact_digest(
    value$decision_id,
    "Artifact decision identity"
  )
  tempest_trajectory_artifact_digest(
    value$selection_id,
    "Artifact decision selection"
  )
  tempest_trajectory_artifact_label(value$stream, "Artifact decision stream")
  tempest_trajectory_artifact_label(value$purpose, "Artifact decision purpose")
  tempest_trajectory_whole_number(value$sequence, "Artifact decision sequence")
  if (value$sequence < 1L) {
    tempest_trajectory_review_abort(
      "Artifact decision sequence must be positive."
    )
  }
  invisible(value)
}

tempest_trajectory_publication <- function(
  research,
  bundle,
  store,
  selection,
  stream,
  decision
) {
  if (is.null(selection)) {
    if (!is.null(store) || !is.null(stream) || !is.null(decision)) {
      tempest_trajectory_review_abort(
        "Publication review requires an exact {.arg selection} and its {.arg store}."
      )
    }
    return(NULL)
  }
  if (is.null(store) || xor(is.null(stream), is.null(decision))) {
    tempest_trajectory_review_abort(
      "Supply {.arg store} with {.arg selection}, and supply {.arg stream} and {.arg decision} together."
    )
  }
  tryCatch(
    {
      retained <- tempest_read_artifact_research(store, selection)
      bound <- tempest_trajectory_bundle_data(research, retained$bundle)
      if (
        !is.null(bundle) &&
          !identical(
            tempest_promotion_bundle_data(bundle)$bundle_id,
            bound$bundle_id
          )
      ) {
        tempest_trajectory_review_abort(
          "The publication does not contain the supplied promotion bundle."
        )
      }
      acceptance <- if (is.null(decision)) {
        NULL
      } else {
        event <- graft::graft_artifact_read_decision(store, stream, decision)
        tempest_trajectory_decision(event, selection)
      }
      list(selection = selection, bundle = bound, acceptance = acceptance)
    },
    error = function(error) {
      if (inherits(error, "tempest_trajectory_review_error")) {
        stop(error)
      }
      tempest_trajectory_review_abort(
        "Could not verify the exact research publication and decision.",
        parent = error
      )
    }
  )
}

tempest_trajectory_input_record_fields <- function() {
  c("record_id", "revision_id", "class", "sha256")
}

tempest_trajectory_input_selection <- function(selection) {
  if (length(selection) == 0L) {
    return(NULL)
  }
  selection <- tempest_research_manifest_artifact_selection(selection)
  list(
    selection_id = selection$selection_id,
    purpose = selection$purpose,
    digest = tempest_product_record_hash(selection),
    reported_decision = tempest_trajectory_reported_decision(selection),
    records = tempest_trajectory_collection(
      lapply(selection$records, function(record) {
        record[tempest_trajectory_input_record_fields()]
      }),
      preserve_order = FALSE
    )
  )
}

tempest_trajectory_reported_decision <- function(selection) {
  tryCatch(
    {
      event <- selection$provenance$graft_decision
      if (!is.list(event) || !identical(event$purpose, selection$purpose)) {
        return(NULL)
      }
      reported <- c(
        list(verification = "unverified"),
        tempest_trajectory_decision(event, selection$selection_id)
      )
      tempest_trajectory_plain_value(reported)
      reported
    },
    error = \(error) NULL
  )
}

tempest_trajectory_validate_input_selection <- function(value) {
  if (is.null(value)) {
    return(invisible(value))
  }
  tempest_trajectory_exact_record(
    value,
    c("selection_id", "purpose", "digest", "reported_decision", "records"),
    "Input artifact selection"
  )
  tempest_trajectory_validate_opaque_identifier(
    value$selection_id,
    "Input selection identity"
  )
  tempest_trajectory_artifact_label(value$purpose, "Input selection purpose")
  tempest_trajectory_artifact_digest(value$digest, "Input selection digest")
  if (!is.null(value$reported_decision)) {
    reported <- value$reported_decision
    tempest_trajectory_exact_record(
      reported,
      c("verification", tempest_trajectory_acceptance_fields()),
      "Reported input decision"
    )
    if (!identical(reported$verification, "unverified")) {
      tempest_trajectory_review_abort(
        "Reported input decisions must remain unverified."
      )
    }
    tempest_trajectory_validate_decision(
      reported[tempest_trajectory_acceptance_fields()]
    )
    if (
      !identical(reported$selection_id, value$selection_id) ||
        !identical(reported$purpose, value$purpose)
    ) {
      tempest_trajectory_review_abort(
        "Input decision must bind the exact selection and purpose."
      )
    }
  }
  tempest_trajectory_validate_collection(
    value$records,
    tempest_trajectory_input_record_fields(),
    "Input artifact records"
  )
  tempest_trajectory_validate_canonical_set(
    value$records,
    "Input artifact records"
  )
  if (value$records$total < 1L || value$records$total > 1000L) {
    tempest_trajectory_review_abort(
      "Input artifact selection must retain 1 to 1000 records."
    )
  }
  ids <- character()
  for (record in value$records$items) {
    for (field in c("record_id", "revision_id", "class")) {
      tempest_trajectory_validate_opaque_identifier(
        record[[field]],
        paste("Input record", field)
      )
    }
    if (!record$class %in% tempest_knowledge_record_allowlist()) {
      tempest_trajectory_review_abort(
        "Input artifact class is not readable evidence."
      )
    }
    tempest_trajectory_artifact_digest(
      record$sha256,
      "Input record content digest"
    )
    ids <- c(ids, record$record_id)
  }
  if (anyDuplicated(ids)) {
    tempest_trajectory_review_abort(
      "Input artifact record identities must be unique."
    )
  }
  invisible(value)
}

tempest_trajectory_artifact_joins <- function(knowledge, run_id) {
  joins <- list()
  add <- function(join) joins[[length(joins) + 1L]] <<- join
  input <- knowledge$input_selection
  if (!is.null(input)) {
    add(tempest_trajectory_join(
      "product",
      run_id,
      "read_from",
      "artifact_selection",
      input$selection_id,
      "exact_identity",
      c("selection_id", "selection_digest")
    ))
  }
  proposal <- knowledge$proposal
  if (!is.null(proposal$selection_id)) {
    add(tempest_trajectory_join(
      "promotion_bundle",
      proposal$bundle_id,
      "published_as",
      "artifact_selection",
      proposal$selection_id,
      "authority_validated",
      c("bundle_id", "selection_id")
    ))
  }
  acceptance <- knowledge$acceptance
  if (!is.null(acceptance)) {
    add(tempest_trajectory_join(
      "artifact_selection",
      acceptance$selection_id,
      "accepted_as",
      "artifact_decision",
      acceptance$decision_id,
      "exact_identity",
      c("selection_id", "decision_id", "stream")
    ))
  }
  joins
}
