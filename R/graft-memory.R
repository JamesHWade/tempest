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

tempest_graft_memory_redact <- function(text) {
  if (!rlang::is_string(text) || is.na(text) || !nzchar(text)) {
    return("")
  }
  out <- gsub(
    "(?i)(postgres(?:ql)?://[^:@[:space:]]+:)[^@[:space:]]+(@)",
    "\\1[redacted]\\2",
    text,
    perl = TRUE
  )
  out <- gsub(
    "(?i)\\b(api[_-]?key|authorization|bearer|password|passwd|pwd|secret|token)\\b(?:\\s*[:=]\\s*|\\s+)(?:bearer\\s+)?[^[:space:];,]+",
    "\\1=[redacted]",
    out,
    perl = TRUE
  )
  out
}

tempest_graft_store_is_handle <- function(x) {
  requireNamespace("graft", quietly = TRUE) &&
    S7::S7_inherits(x, graft::ArtifactStore)
}

tempest_graft_store_is_postgres <- function(x) {
  requireNamespace("graft", quietly = TRUE) &&
    exists("PostgresArtifactStore", envir = asNamespace("graft")) &&
    S7::S7_inherits(x, graft::PostgresArtifactStore)
}

tempest_graft_store_is_factory <- function(x) {
  if (!is.function(x)) {
    return(FALSE)
  }
  params <- formals(x)
  if (length(params) == 0L) {
    return(TRUE)
  }
  required <- vapply(
    params,
    \(default) identical(default, quote(expr = )),
    logical(1)
  )
  !any(required)
}

tempest_graft_store_close <- function(store) {
  if (!tempest_graft_store_is_postgres(store)) {
    return(invisible(FALSE))
  }
  con <- tryCatch(store@connection, error = function(error) NULL)
  if (
    !is.null(con) &&
      requireNamespace("DBI", quietly = TRUE) &&
      isTRUE(tryCatch(DBI::dbIsValid(con), error = function(error) FALSE))
  ) {
    try(DBI::dbDisconnect(con), silent = TRUE)
    return(invisible(TRUE))
  }
  invisible(FALSE)
}

tempest_graft_store_open <- function(graft_store, create = TRUE) {
  tempest_graft_memory_require("Project memory")
  if (tempest_graft_store_is_handle(graft_store)) {
    return(graft_store)
  }
  if (tempest_graft_store_is_factory(graft_store)) {
    opened <- tryCatch(
      graft_store(),
      error = function(error) {
        tempest_graft_memory_abort(
          "Could not open the Graft store factory: {tempest_graft_memory_redact(conditionMessage(error))}",
          class = "tempest_input_error"
        )
      }
    )
    if (!tempest_graft_store_is_handle(opened)) {
      tempest_graft_memory_abort(
        "{.arg graft_store} factory must return a `graft::ArtifactStore`.",
        class = "tempest_input_error"
      )
    }
    return(opened)
  }
  if (
    !rlang::is_string(graft_store) ||
      is.na(graft_store) ||
      !nzchar(trimws(graft_store))
  ) {
    tempest_graft_memory_abort(
      "{.arg graft_store} must be a local directory path, a `graft::ArtifactStore`, or a zero-argument store factory function.",
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

tempest_graft_store_postgres_info <- function(store) {
  if (!tempest_graft_store_is_postgres(store)) {
    tempest_graft_memory_abort(
      "Expected a `graft::PostgresArtifactStore`.",
      class = "tempest_input_error"
    )
  }
  if (!requireNamespace("DBI", quietly = TRUE)) {
    tempest_graft_memory_abort(
      "PostgreSQL Graft stores require the DBI package.",
      class = "tempest_input_error"
    )
  }
  con <- store@connection
  if (!isTRUE(tryCatch(DBI::dbIsValid(con), error = function(error) FALSE))) {
    tempest_graft_memory_abort(
      "The PostgreSQL Graft store connection is not open.",
      class = "tempest_input_error"
    )
  }
  raw_info <- tryCatch(
    DBI::dbGetInfo(con),
    error = function(error) {
      tempest_graft_memory_abort(
        "Could not inspect the PostgreSQL connection for worker handoff: {tempest_graft_memory_redact(conditionMessage(error))}",
        class = "tempest_input_error"
      )
    }
  )
  pick_scalar <- function(field) {
    value <- raw_info[[field]]
    if (is.null(value) || length(value) != 1L || is.na(value)) {
      return(NULL)
    }
    text <- trimws(as.character(value))
    if (!nzchar(text)) NULL else text
  }
  # Deliberately retain only non-secret libpq connection metadata; credentials
  # stay in PGPASSWORD / .pgpass / PGSERVICE or a user-supplied store factory.
  conn_info <- Filter(
    Negate(is.null),
    list(
      dbname = pick_scalar("dbname"),
      host = pick_scalar("host"),
      port = pick_scalar("port"),
      user = pick_scalar("username") %||% pick_scalar("user")
    )
  )
  list(
    kind = "postgres",
    scope = store@scope,
    max_bytes = store@max_bytes,
    max_revision_bytes = store@max_revision_bytes,
    connection_info = conn_info
  )
}

tempest_graft_store_handoff <- function(graft_store, opened_store = NULL) {
  if (is.null(graft_store) && is.null(opened_store)) {
    return(NULL)
  }
  if (tempest_graft_store_is_factory(graft_store)) {
    return(list(kind = "factory", factory = graft_store))
  }
  local_path <- tempest_graft_store_path(opened_store %||% graft_store)
  if (!is.null(local_path)) {
    return(list(kind = "local", path = local_path))
  }
  candidate <- opened_store %||% graft_store
  if (tempest_graft_store_is_postgres(candidate)) {
    return(tempest_graft_store_postgres_info(candidate))
  }
  tempest_graft_memory_abort(
    "Unsupported Graft store for background worker handoff. Supply a local path, `graft_store()`, `graft_store_postgres()`, or a zero-argument store factory function.",
    class = "tempest_input_error"
  )
}

tempest_graft_store_connect_postgres <- function(handoff) {
  tempest_graft_memory_require("PostgreSQL project memory")
  for (pkg in c("DBI", "RPostgres")) {
    tempest_require(pkg, "PostgreSQL Graft stores require DBI and RPostgres.")
  }
  connect_args <- c(
    list(drv = RPostgres::Postgres()),
    handoff$connection_info %||% list()
  )
  con <- tryCatch(
    do.call(DBI::dbConnect, connect_args),
    error = function(error) {
      tempest_graft_memory_abort(
        paste0(
          "The research worker could not reconnect to the PostgreSQL Graft store (",
          tempest_graft_memory_redact(conditionMessage(error)),
          "). Configure libpq credentials (PGPASSWORD / .pgpass / PGSERVICE) or pass a `graft_store = function() ...` factory."
        ),
        class = "tempest_input_error"
      )
    }
  )
  opened <- FALSE
  on.exit(
    if (!opened) {
      try(DBI::dbDisconnect(con), silent = TRUE)
    },
    add = TRUE
  )
  store <- tryCatch(
    graft::graft_store_postgres(
      connection = con,
      scope = handoff$scope,
      create = FALSE,
      max_bytes = handoff$max_bytes,
      max_revision_bytes = handoff$max_revision_bytes
    ),
    error = function(error) {
      tempest_graft_memory_abort(
        "The research worker could not open the PostgreSQL Graft store: {tempest_graft_memory_redact(conditionMessage(error))}",
        class = "tempest_input_error"
      )
    }
  )
  opened <- TRUE
  store
}

tempest_graft_store_open_handoff <- function(handoff) {
  if (is.null(handoff)) {
    return(list(store = NULL, owns_connection = FALSE))
  }
  if (rlang::is_string(handoff)) {
    return(list(
      store = tempest_graft_store_open(handoff, create = FALSE),
      owns_connection = FALSE
    ))
  }
  if (!is.list(handoff) || !rlang::is_string(handoff$kind)) {
    tempest_graft_memory_abort(
      "Invalid Graft store worker handoff descriptor.",
      class = "tempest_input_error"
    )
  }
  if (identical(handoff$kind, "local")) {
    return(list(
      store = tempest_graft_store_open(handoff$path, create = FALSE),
      owns_connection = FALSE
    ))
  }
  if (identical(handoff$kind, "factory")) {
    store <- tempest_graft_store_open(handoff$factory, create = FALSE)
    return(list(
      store = store,
      owns_connection = tempest_graft_store_is_postgres(store)
    ))
  }
  if (identical(handoff$kind, "postgres")) {
    store <- tempest_graft_store_connect_postgres(handoff)
    return(list(
      store = store,
      owns_connection = TRUE
    ))
  }
  tempest_graft_memory_abort(
    "Unknown Graft store handoff kind {.val {handoff$kind}}.",
    class = "tempest_input_error"
  )
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
  max_streams = 1000L,
  error_on_failure = FALSE
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
    error = function(error) {
      if (isTRUE(error_on_failure)) {
        stop(error)
      }
      list()
    }
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

tempest_graft_selection_status_in_stream <- function(
  store,
  selection_id,
  stream,
  purpose = "research",
  eligible = NULL
) {
  if (!rlang::is_string(stream) || !nzchar(stream)) {
    return(NULL)
  }
  eligible_fn <- tempest_graft_memory_eligible(eligible)
  allowed <- tryCatch(
    isTRUE(eligible_fn(stream)),
    error = function(error) FALSE
  )
  if (!allowed) {
    return(NULL)
  }
  history <- tryCatch(
    graft::graft_history(store, stream),
    error = function(error) list()
  )
  if (!length(history)) {
    return(NULL)
  }
  head <- history[[length(history)]]
  if (
    !is.null(purpose) &&
      rlang::is_string(head@purpose) &&
      !identical(head@purpose, purpose)
  ) {
    return(NULL)
  }
  if (
    identical(head@action, "accept") &&
      identical(head@selection, selection_id)
  ) {
    return(list(
      status = "accepted",
      stream = stream,
      decision_id = head@id,
      sequence = as.integer(head@sequence),
      actor = head@actor %||% "",
      reason = head@reason %||% "",
      history = tempest_graft_history_table(history)
    ))
  }
  matched <- Filter(
    function(event) {
      identical(event@action, "accept") &&
        identical(event@selection, selection_id)
    },
    history
  )
  if (!length(matched)) {
    return(NULL)
  }
  status <- if (identical(head@action, "withdraw")) {
    "withdrawn"
  } else {
    "superseded"
  }
  list(
    status = status,
    stream = stream,
    decision_id = head@id,
    sequence = as.integer(head@sequence),
    actor = head@actor %||% "",
    reason = head@reason %||% "",
    history = tempest_graft_history_table(history)
  )
}

tempest_graft_selection_status <- function(
  store,
  selection_id,
  purpose = "research",
  eligible = NULL,
  stream = NULL
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
  # When the report sidecar remembers the stream, query it directly without
  # scanning the entire store.
  if (rlang::is_string(stream) && nzchar(stream)) {
    direct <- tempest_graft_selection_status_in_stream(
      store = store,
      selection_id = selection_id,
      stream = stream,
      purpose = purpose,
      eligible = eligible
    )
    if (!is.null(direct)) {
      return(direct)
    }
  }
  streams <- tryCatch(
    tempest_graft_memory_streams(
      store,
      purpose = purpose,
      eligible = eligible,
      max_streams = .Machine$integer.max,
      error_on_failure = TRUE
    ),
    error = function(error) NULL
  )
  if (is.null(streams)) {
    return(list(
      status = "unknown",
      stream = if (rlang::is_string(stream) && nzchar(stream)) stream else NULL,
      decision_id = NULL,
      sequence = NA_integer_,
      actor = NULL,
      reason = NULL,
      history = empty_history
    ))
  }
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
  for (stream_name in streams$stream) {
    found <- tempest_graft_selection_status_in_stream(
      store = store,
      selection_id = selection_id,
      stream = stream_name,
      purpose = purpose,
      eligible = eligible
    )
    if (!is.null(found)) {
      return(found)
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

tempest_graft_validate_review_inputs <- function(
  stream,
  actor,
  reason,
  action = c("accept", "withdraw")
) {
  action <- match.arg(action)
  stream <- trimws(as.character(stream %||% ""))
  actor <- trimws(as.character(actor %||% ""))
  reason <- trimws(as.character(reason %||% ""))
  if (!nzchar(stream)) {
    msg <- if (identical(action, "accept")) {
      "Choose or enter a topic stream name before accepting."
    } else {
      "Choose an accepted stream to withdraw."
    }
    tempest_graft_memory_abort(msg, class = "tempest_input_error")
  }
  if (!nzchar(actor)) {
    verb <- if (identical(action, "accept")) "accepting" else "withdrawing"
    tempest_graft_memory_abort(
      "Enter a reviewer name before {verb}.",
      class = "tempest_input_error"
    )
  }
  if (!nzchar(reason)) {
    msg <- if (identical(action, "accept")) {
      "Enter a brief review reason before accepting."
    } else {
      "Enter a brief reason before withdrawing."
    }
    tempest_graft_memory_abort(msg, class = "tempest_input_error")
  }
  list(stream = stream, actor = actor, reason = reason)
}

tempest_graft_resolve_expected_head <- function(
  current_head,
  expected,
  stream,
  action = c("accept", "withdraw")
) {
  action <- match.arg(action)
  verb <- if (identical(action, "accept")) "accepting" else "withdrawing"
  if (is.null(expected)) {
    return(current_head)
  }
  if (identical(action, "accept") && identical(expected, "")) {
    if (!is.null(current_head)) {
      tempest_graft_memory_abort(
        "Stream {.val {stream}} has changed since it was displayed. Refresh and review the latest decision before {verb}."
      )
    }
    return(NULL)
  }
  expected_id <- if (S7::S7_inherits(expected, graft::Decision)) {
    expected@id
  } else {
    expected
  }
  if (is.null(current_head) || !identical(current_head@id, expected_id)) {
    tempest_graft_memory_abort(
      "Stream {.val {stream}} has changed since it was displayed. Refresh and review the latest decision before {verb}."
    )
  }
  current_head
}

tempest_graft_accept_selection <- function(
  store,
  selection_id,
  stream,
  actor,
  reason,
  purpose = "research",
  expected = NULL,
  key = NULL
) {
  tempest_graft_memory_require("Accepting research into project memory")
  inputs <- tempest_graft_validate_review_inputs(
    stream = stream,
    actor = actor,
    reason = reason,
    action = "accept"
  )
  # Validate that the selection holds a valid Tempest research publication.
  tempest_read_artifact_research(store, selection_id)
  history <- tryCatch(
    graft::graft_history(store, inputs$stream),
    error = function(error) list()
  )
  current_head <- if (length(history)) history[[length(history)]] else NULL
  expected_decision <- tempest_graft_resolve_expected_head(
    current_head = current_head,
    expected = expected,
    stream = inputs$stream,
    action = "accept"
  )
  request_key <- key %||% paste0("accept-", tempest_uuid("graft"))
  graft::graft_accept(
    store,
    graft::graft_read_selection(store, selection_id),
    stream = inputs$stream,
    expected = expected_decision,
    key = request_key,
    actor = inputs$actor,
    reason = inputs$reason,
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
  inputs <- tempest_graft_validate_review_inputs(
    stream = stream,
    actor = actor,
    reason = reason,
    action = "withdraw"
  )
  history <- graft::graft_history(store, inputs$stream)
  if (!length(history)) {
    tempest_graft_memory_abort(
      "No decisions exist for stream {.val {inputs$stream}}."
    )
  }
  head <- history[[length(history)]]
  if (!identical(head@action, "accept")) {
    tempest_graft_memory_abort(
      "Stream {.val {inputs$stream}} is not currently accepted."
    )
  }
  expected_decision <- tempest_graft_resolve_expected_head(
    current_head = head,
    expected = expected,
    stream = inputs$stream,
    action = "withdraw"
  )
  request_key <- key %||% paste0("withdraw-", tempest_uuid("graft"))
  graft::graft_withdraw(
    store,
    stream = inputs$stream,
    expected = expected_decision,
    key = request_key,
    actor = inputs$actor,
    reason = inputs$reason
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

tempest_product_graft_consultations <- function(product) {
  empty <- tempest_graft_consultations(NULL)
  if (is.null(product) || !inherits(product, "TempestSession")) {
    return(empty)
  }
  chats <- list()
  moderator_chat <- tryCatch(
    tempest_session_chat(product, "moderator"),
    error = function(error) NULL
  )
  if (!is.null(moderator_chat)) {
    chats[[length(chats) + 1L]] <- moderator_chat
  }
  manager <- tryCatch(
    tempest_session_expert_manager(product),
    error = function(error) NULL
  )
  if (!is.null(manager)) {
    chats <- c(chats, tempest_deputy_expert_session_chats(manager))
  }
  if (!length(chats)) {
    return(empty)
  }
  frames <- lapply(chats, function(chat) {
    tryCatch(
      tempest_graft_consultations(chat),
      error = function(error) empty
    )
  })
  frames <- Filter(\(df) is.data.frame(df) && nrow(df) > 0L, frames)
  if (!length(frames)) {
    return(empty)
  }
  out <- do.call(rbind, frames)
  rownames(out) <- NULL
  out
}
