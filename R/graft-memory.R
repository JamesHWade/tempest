# Shared Graft project-memory helpers for Tempest research apps.

tempest_graft_memory_abort <- function(
  message,
  ...,
  class = character(),
  parent = NULL,
  .envir = rlang::caller_env()
) {
  tempest_abort(
    message,
    ...,
    class = unique(c(class, "tempest_graft_memory_error", "tempest_error")),
    parent = parent,
    .envir = .envir
  )
}

tempest_graft_memory_require <- function(reason = "Graft project memory") {
  tempest_require("graft", paste0(reason, " requires the graft package."))
  invisible(TRUE)
}

tempest_graft_memory_eligible <- function(eligible = NULL) {
  if (is.null(eligible)) {
    return(function(stream) TRUE)
  }
  if (!is.function(eligible) || length(formals(eligible)) != 1L) {
    tempest_graft_memory_abort(
      "{.arg memory_eligible} must be `NULL` or a one-argument function taking `stream`.",
      class = "tempest_input_error"
    )
  }
  eligible
}

tempest_graft_memory_check_purpose <- function(purpose) {
  if (
    !rlang::is_string(purpose) ||
      is.na(purpose) ||
      !nzchar(trimws(purpose)) ||
      !identical(purpose, trimws(purpose)) ||
      nchar(enc2utf8(purpose), type = "bytes") > 256L
  ) {
    tempest_graft_memory_abort(
      "{.arg memory_purpose} must be a non-empty bounded string.",
      class = "tempest_input_error"
    )
  }
  purpose
}

tempest_graft_memory_check_actor <- function(actor) {
  if (is.null(actor)) {
    return("local-reviewer")
  }
  if (
    !rlang::is_string(actor) ||
      is.na(actor) ||
      !nzchar(trimws(actor)) ||
      nchar(enc2utf8(actor), type = "bytes") > 256L
  ) {
    tempest_graft_memory_abort(
      "{.arg memory_actor} must be `NULL` or a non-empty string.",
      class = "tempest_input_error"
    )
  }
  trimws(actor)
}

tempest_graft_store_is_handle <- function(x) {
  requireNamespace("graft", quietly = TRUE) &&
    S7::S7_inherits(x, graft::ArtifactStore)
}

tempest_graft_store_open <- function(graft_store, create = TRUE) {
  tempest_graft_memory_require("Project memory")
  if (tempest_graft_store_is_handle(graft_store)) {
    return(graft_store)
  }
  if (
    !rlang::is_string(graft_store) ||
      is.na(graft_store) ||
      !nzchar(trimws(graft_store))
  ) {
    tempest_graft_memory_abort(
      "{.arg graft_store} must be a local directory path or a `graft::ArtifactStore`.",
      class = "tempest_input_error"
    )
  }
  path <- trimws(graft_store)
  exists_dir <- dir.exists(path)
  if (file.exists(path) && !exists_dir) {
    tempest_graft_memory_abort(
      "{.arg graft_store} must name a directory, not an existing file.",
      class = "tempest_input_error"
    )
  }
  graft::graft_store(path, create = isTRUE(create) && !exists_dir)
}

tempest_graft_store_path <- function(graft_store) {
  if (is.null(graft_store)) {
    return(NULL)
  }
  if (rlang::is_string(graft_store) && nzchar(trimws(graft_store))) {
    return(normalizePath(
      path.expand(trimws(graft_store)),
      winslash = "/",
      mustWork = FALSE
    ))
  }
  if (
    requireNamespace("graft", quietly = TRUE) &&
      S7::S7_inherits(graft_store, graft::LocalArtifactStore)
  ) {
    path <- tryCatch(graft_store@path, error = function(error) NULL)
    if (rlang::is_string(path) && nzchar(path)) {
      return(normalizePath(path, winslash = "/", mustWork = FALSE))
    }
  }
  NULL
}

tempest_graft_stream_slug <- function(text, fallback = "research-finding") {
  if (!rlang::is_string(text) || is.na(text) || !nzchar(trimws(text))) {
    return(fallback)
  }
  slug <- tolower(trimws(text))
  slug <- gsub("[^a-z0-9]+", "-", slug)
  slug <- gsub("(^-+|-+$)", "", slug)
  slug <- substr(slug, 1L, 64L)
  slug <- gsub("-+$", "", slug)
  if (!nzchar(slug)) fallback else slug
}

tempest_graft_memory_streams <- function(
  store,
  purpose = "research",
  eligible = NULL,
  max_streams = 100L
) {
  tempest_graft_memory_require("Listing project memory streams")
  eligible_fn <- tempest_graft_memory_eligible(eligible)
  empty <- data.frame(
    stream = character(),
    status = character(),
    decision_id = character(),
    selection_id = character(),
    sequence = integer(),
    actor = character(),
    reason = character(),
    purpose = character(),
    history_count = integer(),
    stringsAsFactors = FALSE
  )
  decisions <- tryCatch(
    graft::graft_streams(store, purpose = purpose, max_streams = max_streams),
    error = function(error) list()
  )
  if (!length(decisions)) {
    return(empty)
  }
  rows <- lapply(decisions, function(decision) {
    stream <- decision@stream
    allowed <- tryCatch(
      isTRUE(eligible_fn(stream)),
      error = function(error) FALSE
    )
    if (!allowed) {
      return(NULL)
    }
    history <- tryCatch(
      graft::graft_history(store, stream),
      error = function(error) list(decision)
    )
    status <- if (identical(decision@action, "accept")) {
      "accepted"
    } else if (identical(decision@action, "withdraw")) {
      "withdrawn"
    } else {
      as.character(decision@action)
    }
    data.frame(
      stream = stream,
      status = status,
      decision_id = decision@id,
      selection_id = decision@selection %||% NA_character_,
      sequence = as.integer(decision@sequence),
      actor = decision@actor %||% "",
      reason = decision@reason %||% "",
      purpose = decision@purpose %||% purpose,
      history_count = as.integer(length(history)),
      stringsAsFactors = FALSE
    )
  })
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) {
    return(empty)
  }
  result <- do.call(rbind, rows)
  result[order(result$status != "accepted", result$stream), , drop = FALSE]
}

tempest_graft_memory_accepted_streams <- function(
  store,
  purpose = "research",
  eligible = NULL
) {
  streams <- tempest_graft_memory_streams(
    store,
    purpose = purpose,
    eligible = eligible
  )
  streams[streams$status == "accepted", , drop = FALSE]
}

tempest_graft_history_table <- function(history) {
  if (!length(history)) {
    return(data.frame(
      sequence = integer(),
      action = character(),
      actor = character(),
      reason = character(),
      decision_id = character(),
      selection_id = character(),
      stringsAsFactors = FALSE
    ))
  }
  data.frame(
    sequence = vapply(history, \(e) as.integer(e@sequence), integer(1)),
    action = vapply(history, \(e) as.character(e@action), character(1)),
    actor = vapply(history, \(e) as.character(e@actor %||% ""), character(1)),
    reason = vapply(history, \(e) as.character(e@reason %||% ""), character(1)),
    decision_id = vapply(history, \(e) as.character(e@id), character(1)),
    selection_id = vapply(
      history,
      \(e) as.character(e@selection %||% NA_character_),
      character(1)
    ),
    stringsAsFactors = FALSE
  )
}

tempest_graft_selection_status <- function(
  store,
  selection_id,
  purpose = "research",
  eligible = NULL
) {
  empty_history <- tempest_graft_history_table(list())
  if (
    is.null(selection_id) ||
      !rlang::is_string(selection_id) ||
      !nzchar(selection_id)
  ) {
    return(list(
      status = "unlinked",
      stream = NULL,
      decision_id = NULL,
      sequence = NA_integer_,
      actor = NULL,
      reason = NULL,
      history = empty_history
    ))
  }
  streams <- tempest_graft_memory_streams(
    store,
    purpose = purpose,
    eligible = eligible
  )
  # First look for a stream whose current accepted head is this selection.
  active_idx <- which(
    streams$status == "accepted" &
      !is.na(streams$selection_id) &
      streams$selection_id == selection_id
  )
  if (length(active_idx) > 0L) {
    row <- streams[active_idx[[1L]], , drop = FALSE]
    history <- tryCatch(
      graft::graft_history(store, row$stream),
      error = function(error) list()
    )
    return(list(
      status = "accepted",
      stream = row$stream,
      decision_id = row$decision_id,
      sequence = row$sequence,
      actor = row$actor,
      reason = row$reason,
      history = tempest_graft_history_table(history)
    ))
  }
  # Next inspect stream histories to distinguish superseded vs. withdrawn.
  for (stream in streams$stream) {
    history <- tryCatch(
      graft::graft_history(store, stream),
      error = function(error) list()
    )
    matched <- Filter(
      function(event) {
        identical(event@action, "accept") &&
          identical(event@selection, selection_id)
      },
      history
    )
    if (length(matched) > 0L) {
      head <- history[[length(history)]]
      status <- if (identical(head@action, "withdraw")) {
        "withdrawn"
      } else {
        "superseded"
      }
      return(list(
        status = status,
        stream = stream,
        decision_id = head@id,
        sequence = as.integer(head@sequence),
        actor = head@actor %||% "",
        reason = head@reason %||% "",
        history = tempest_graft_history_table(history)
      ))
    }
  }
  list(
    status = "provisional",
    stream = NULL,
    decision_id = NULL,
    sequence = NA_integer_,
    actor = NULL,
    reason = NULL,
    history = empty_history
  )
}

tempest_graft_accept_selection <- function(
  store,
  selection_id,
  stream,
  actor,
  reason,
  purpose = "research",
  key = NULL
) {
  tempest_graft_memory_require("Accepting research into project memory")
  stream <- trimws(as.character(stream %||% ""))
  actor <- trimws(as.character(actor %||% ""))
  reason <- trimws(as.character(reason %||% ""))
  if (!nzchar(stream)) {
    tempest_graft_memory_abort(
      "Choose or enter a topic stream name before accepting.",
      class = "tempest_input_error"
    )
  }
  if (!nzchar(actor)) {
    tempest_graft_memory_abort(
      "Enter a reviewer name before accepting.",
      class = "tempest_input_error"
    )
  }
  if (!nzchar(reason)) {
    tempest_graft_memory_abort(
      "Enter a brief review reason before accepting.",
      class = "tempest_input_error"
    )
  }
  # Validate that the selection holds a valid Tempest research publication.
  tempest_read_artifact_research(store, selection_id)
  history <- tryCatch(
    graft::graft_history(store, stream),
    error = function(error) list()
  )
  expected <- if (length(history)) history[[length(history)]] else NULL
  request_key <- key %||% paste0("accept-", tempest_uuid("graft"))
  graft::graft_accept(
    store,
    graft::graft_read_selection(store, selection_id),
    stream = stream,
    expected = expected,
    key = request_key,
    actor = actor,
    reason = reason,
    purpose = purpose
  )
}

tempest_graft_withdraw_stream <- function(
  store,
  stream,
  actor,
  reason,
  expected = NULL,
  key = NULL
) {
  tempest_graft_memory_require("Withdrawing research from project memory")
  stream <- trimws(as.character(stream %||% ""))
  actor <- trimws(as.character(actor %||% ""))
  reason <- trimws(as.character(reason %||% ""))
  if (!nzchar(stream)) {
    tempest_graft_memory_abort(
      "Choose an accepted stream to withdraw.",
      class = "tempest_input_error"
    )
  }
  if (!nzchar(actor)) {
    tempest_graft_memory_abort(
      "Enter a reviewer name before withdrawing.",
      class = "tempest_input_error"
    )
  }
  if (!nzchar(reason)) {
    tempest_graft_memory_abort(
      "Enter a brief reason before withdrawing.",
      class = "tempest_input_error"
    )
  }
  history <- graft::graft_history(store, stream)
  if (!length(history)) {
    tempest_graft_memory_abort(
      "No decisions exist for stream {.val {stream}}."
    )
  }
  head <- history[[length(history)]]
  if (!identical(head@action, "accept")) {
    tempest_graft_memory_abort(
      "Stream {.val {stream}} is not currently accepted."
    )
  }
  expected_decision <- if (is.null(expected)) {
    head
  } else {
    expected_id <- if (S7::S7_inherits(expected, graft::Decision)) {
      expected@id
    } else {
      expected
    }
    if (!identical(head@id, expected_id)) {
      tempest_graft_memory_abort(
        "Stream {.val {stream}} has changed since it was displayed. Refresh and review the latest decision before withdrawing."
      )
    }
    expected
  }
  request_key <- key %||% paste0("withdraw-", tempest_uuid("graft"))
  graft::graft_withdraw(
    store,
    stream = stream,
    expected = expected_decision,
    key = request_key,
    actor = actor,
    reason = reason
  )
}

tempest_graft_reuse_stream <- function(
  store,
  stream,
  purpose = "research",
  eligible = NULL
) {
  tempest_graft_memory_require("Reusing accepted research")
  eligible_fn <- tempest_graft_memory_eligible(eligible)
  history <- tryCatch(
    graft::graft_history(store, stream),
    error = function(error) list()
  )
  if (!length(history)) {
    tempest_graft_memory_abort(
      "Accepted stream {.val {stream}} was not found."
    )
  }
  head <- history[[length(history)]]
  if (!identical(head@action, "accept")) {
    tempest_graft_memory_abort(
      "Stream {.val {stream}} is no longer accepted."
    )
  }
  tempest_reuse_artifact_research(
    store = store,
    stream = stream,
    decision = head@id,
    purpose = purpose,
    eligible = function(event) isTRUE(eligible_fn(event$stream))
  )
}

tempest_graft_has_multi_tools <- function() {
  requireNamespace("graft", quietly = TRUE) &&
    "graft_tools" %in% getNamespaceExports("graft")
}

tempest_graft_memory_tools <- function(
  store,
  purpose = "research",
  eligible = NULL,
  streams = NULL,
  max_streams = 50L,
  max_text_bytes = 64L * 1024L
) {
  tempest_graft_memory_require("Reviewed memory tools")
  eligible_fn <- tempest_graft_memory_eligible(eligible)
  if (tempest_graft_has_multi_tools()) {
    tools <- graft::graft_tools(
      store = store,
      purpose = purpose,
      eligible = eligible_fn,
      streams = streams,
      max_streams = as.integer(max_streams),
      max_text_bytes = as.integer(max_text_bytes)
    )
    return(unname(as.list(tools)))
  }
  # Fallback for Graft releases before graft_tools(): bind one single-stream tool
  # when a single stream is specified or exactly one accepted stream exists.
  target_stream <- if (rlang::is_string(streams) && nzchar(streams)) {
    streams
  } else {
    accepted <- tempest_graft_memory_accepted_streams(
      store,
      purpose = purpose,
      eligible = eligible_fn
    )
    if (nrow(accepted) >= 1L) accepted$stream[[1L]] else NULL
  }
  if (is.null(target_stream)) {
    return(list())
  }
  list(graft::graft_tool(
    store = store,
    stream = target_stream,
    purpose = purpose,
    eligible = function() isTRUE(eligible_fn(target_stream)),
    max_text_bytes = as.integer(max_text_bytes)
  ))
}

# Extract plain-data @extra$graft consultation metadata emitted by graft_tool()
# and graft_tools() (Graft #130/#131) without touching store handles or payloads.
tempest_graft_consultation_record <- function(meta) {
  if (!is.list(meta) || is.data.frame(meta)) {
    return(NULL)
  }
  status <- meta$status
  stream <- meta$stream
  purpose <- meta$purpose
  if (!rlang::is_string(status) || !rlang::is_string(stream)) {
    return(NULL)
  }
  list(
    status = status,
    stream = stream,
    purpose = if (rlang::is_string(purpose)) purpose else NA_character_,
    decision_id = if (rlang::is_string(meta$decision_id)) {
      meta$decision_id
    } else {
      NA_character_
    },
    selection_id = if (rlang::is_string(meta$selection_id)) {
      meta$selection_id
    } else {
      NA_character_
    },
    artifact_count = as.integer(meta$artifact_count %||% 0L),
    text_bytes = as.integer(meta$text_bytes %||% 0L),
    truncated = isTRUE(meta$truncated),
    omitted_count = as.integer(meta$omitted_count %||% 0L)
  )
}

tempest_graft_consultations <- function(x) {
  empty <- data.frame(
    status = character(),
    stream = character(),
    purpose = character(),
    decision_id = character(),
    selection_id = character(),
    artifact_count = integer(),
    text_bytes = integer(),
    truncated = logical(),
    omitted_count = integer(),
    stringsAsFactors = FALSE
  )
  if (is.null(x)) {
    return(empty)
  }
  turns <- if (is.environment(x) && is.function(x$get_turns)) {
    tryCatch(x$get_turns(), error = function(error) list())
  } else if (is.list(x)) {
    x
  } else {
    list(x)
  }
  found <- list()
  collect_item <- function(item) {
    if (is.null(item)) {
      return(invisible(NULL))
    }
    extra <- tryCatch(item@extra, error = function(error) {
      if (is.list(item)) item$extra else NULL
    })
    if (is.list(extra) && is.list(extra$graft)) {
      rec <- tempest_graft_consultation_record(extra$graft)
      if (!is.null(rec)) {
        found[[length(found) + 1L]] <<- rec
      }
    }
    contents <- tryCatch(item@contents, error = function(error) {
      if (is.list(item)) item$contents else NULL
    })
    if (is.list(contents)) {
      for (child in contents) {
        collect_item(child)
      }
    }
  }
  for (turn in turns) {
    collect_item(turn)
  }
  if (!length(found)) {
    return(empty)
  }
  do.call(
    rbind,
    lapply(found, function(rec) {
      as.data.frame(rec, stringsAsFactors = FALSE)
    })
  )
}
