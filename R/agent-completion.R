# Process-local, one-use agent completion capabilities.

tempest_agent_completion_abort <- function(message, class) {
  tempest_abort(
    message,
    class = c(class, "tempest_agent_completion_error", "tempest_error")
  )
}

tempest_agent_completion_id_abort <- function() {
  tempest_agent_completion_abort(
    "Agent completion identifier is invalid or unknown.",
    "tempest_agent_completion_id_error"
  )
}

tempest_agent_completion_binding_abort <- function() {
  tempest_agent_completion_abort(
    "Agent completion does not match its owning execution.",
    "tempest_agent_completion_binding_error"
  )
}

tempest_agent_completion_state_abort <- function() {
  tempest_agent_completion_abort(
    "Agent completion is not available in the required state.",
    "tempest_agent_completion_state_error"
  )
}

tempest_agent_completion_record_abort <- function() {
  tempest_agent_completion_abort(
    "Agent completion could not be recorded.",
    "tempest_agent_completion_record_error"
  )
}

tempest_agent_completion_registry <- function(owner) {
  if (!is.environment(owner)) {
    tempest_agent_completion_binding_abort()
  }
  registry <- new.env(parent = emptyenv())
  registry$owner <- owner
  registry$entries <- new.env(hash = TRUE, parent = emptyenv())
  registry$counter <- 0L
  seed <- paste(
    format(owner),
    format(registry),
    Sys.getpid(),
    format(Sys.time(), "%Y-%m-%dT%H:%M:%OS6", tz = "UTC"),
    sep = "|"
  )
  registry$registry_id <- substr(
    digest::digest(seed, algo = "sha256", serialize = FALSE),
    1L,
    24L
  )
  class(registry) <- c("TempestAgentCompletionRegistry", "environment")
  registry
}

tempest_agent_completion_registry_validate <- function(registry) {
  valid <- inherits(registry, "TempestAgentCompletionRegistry") &&
    is.environment(registry) &&
    is.environment(registry$owner) &&
    is.environment(registry$entries) &&
    rlang::is_string(registry$registry_id) &&
    is.integer(registry$counter) &&
    length(registry$counter) == 1L &&
    !is.na(registry$counter)
  if (!valid) {
    tempest_agent_completion_binding_abort()
  }
  registry
}

tempest_agent_completion_assert_owner <- function(registry, owner) {
  registry <- tempest_agent_completion_registry_validate(registry)
  if (!is.environment(owner) || !identical(owner, registry$owner)) {
    tempest_agent_completion_binding_abort()
  }
  invisible(registry)
}

tempest_agent_completion_id_prefix <- function(registry) {
  registry <- tempest_agent_completion_registry_validate(registry)
  paste0("agent-completion-", registry$registry_id, "-")
}

tempest_agent_completion_new_id <- function(registry) {
  registry <- tempest_agent_completion_registry_validate(registry)
  repeat {
    registry$counter <- registry$counter + 1L
    nonce <- digest::digest(
      paste(
        registry$registry_id,
        registry$counter,
        format(Sys.time(), "%Y-%m-%dT%H:%M:%OS6", tz = "UTC"),
        sep = "|"
      ),
      algo = "sha256",
      serialize = FALSE
    )
    completion_id <- paste0(
      tempest_agent_completion_id_prefix(registry),
      substr(nonce, 1L, 32L)
    )
    if (!exists(completion_id, registry$entries, inherits = FALSE)) {
      return(completion_id)
    }
  }
}

tempest_agent_completion_assert_id <- function(
  registry,
  completion_id,
  must_exist = TRUE
) {
  registry <- tempest_agent_completion_registry_validate(registry)
  if (
    !rlang::is_string(completion_id) ||
      !tempest_opaque_identifier_valid(completion_id)
  ) {
    tempest_agent_completion_id_abort()
  }
  if (
    !startsWith(completion_id, tempest_agent_completion_id_prefix(registry))
  ) {
    tempest_agent_completion_binding_abort()
  }
  exists <- exists(completion_id, registry$entries, inherits = FALSE)
  if (isTRUE(must_exist) && !exists) {
    tempest_agent_completion_id_abort()
  }
  if (!isTRUE(must_exist) && exists) {
    tempest_agent_completion_state_abort()
  }
  completion_id
}

tempest_agent_completion_text <- function(value) {
  valid_utf8 <- tryCatch(
    !is.na(iconv(value, from = "", to = "UTF-8", sub = NA_character_)),
    error = function(error) FALSE
  )
  if (
    !rlang::is_string(value) ||
      is.na(value) ||
      identical(Encoding(value), "bytes") ||
      !isTRUE(valid_utf8)
  ) {
    tempest_agent_completion_binding_abort()
  }
  value
}

tempest_agent_completion_provider_turn <- function(provider_turn) {
  if (!inherits(provider_turn, "ellmer::AssistantTurn")) {
    tempest_agent_completion_binding_abort()
  }
  provider_turn
}

tempest_agent_completion_response_from_turn <- function(provider_turn) {
  provider_turn <- tempest_agent_completion_provider_turn(provider_turn)
  contents <- tryCatch(
    provider_turn@contents,
    error = function(error) NULL
  )
  if (!is.list(contents)) {
    tempest_agent_completion_binding_abort()
  }
  response <- paste(
    vapply(
      contents,
      function(content) {
        if (inherits(content, "ellmer::ContentText")) {
          return(content@text)
        }
        ""
      },
      character(1)
    ),
    collapse = ""
  )
  tempest_agent_completion_text(response)
}

tempest_agent_completion_response_matches_turn <- function(
  response,
  provider_turn
) {
  response <- tempest_agent_completion_text(response)
  turn_response <- tempest_agent_completion_response_from_turn(provider_turn)
  identical(response, turn_response) ||
    identical(response, paste0(turn_response, "\n"))
}

tempest_agent_completion_trace <- function(deputy_execution) {
  if (!is.list(deputy_execution) || is.data.frame(deputy_execution)) {
    tempest_agent_completion_binding_abort()
  }
  canonical <- tryCatch(
    tempest_research_manifest_traces(list(deputy_execution))[[1L]],
    error = function(error) NULL
  )
  required <- c(
    "agent_id",
    "completion_disposition",
    "correlation_id",
    "deputy_run_id",
    "deputy_session_id",
    "role",
    "stage",
    "status",
    "trace_id",
    "trace_type"
  )
  terminal_statuses <- c(
    "abandoned",
    "complete",
    "cost_limit",
    "error",
    "hook_requested_stop",
    "input_token_limit",
    "interrupted",
    "output_token_limit",
    "provider_error",
    "request_limit",
    "tool_call_limit",
    "total_token_limit"
  )
  delegation_fields <- c(
    "parent_run_id",
    "delegation_id",
    "tool_call_id"
  )
  delegated <- delegation_fields %in% names(canonical %||% list())
  valid <- !is.null(canonical) &&
    all(required %in% names(canonical)) &&
    identical(canonical$trace_type, "deputy_run") &&
    identical(canonical$trace_id, canonical$deputy_run_id) &&
    canonical$status %in% terminal_statuses &&
    canonical$completion_disposition %in%
      c(
        "issued",
        "discarded",
        "terminal"
      ) &&
    identical(
      canonical$status == "complete",
      canonical$completion_disposition %in% c("issued", "discarded")
    ) &&
    (!any(delegated) || all(delegated))
  if (!valid) {
    tempest_agent_completion_binding_abort()
  }
  rlang::duplicate(deputy_execution, shallow = FALSE)
}

tempest_agent_completion_digest <- function(
  prompt,
  response,
  provider_turn,
  deputy_execution
) {
  canonical_value <- function(value) {
    if (is.null(value)) {
      return(NULL)
    }
    if (
      is.function(value) ||
        is.environment(value) ||
        typeof(value) %in% c("externalptr", "weakref")
    ) {
      tempest_agent_completion_binding_abort()
    }
    if (inherits(value, "S7_object")) {
      properties <- tryCatch(
        S7::props(value),
        error = function(error) NULL
      )
      if (!is.list(properties)) {
        tempest_agent_completion_binding_abort()
      }
      return(list(
        class = unname(class(value)),
        properties = lapply(properties, canonical_value)
      ))
    }
    if (is.object(value)) {
      return(list(
        class = unname(class(value)),
        data = canonical_value(unclass(value))
      ))
    }
    if (is.list(value)) {
      return(lapply(value, canonical_value))
    }
    if (!is.atomic(value)) {
      tempest_agent_completion_binding_abort()
    }
    value
  }
  payload <- serialize(
    list(
      prompt = charToRaw(prompt),
      response = charToRaw(response),
      provider_turn = canonical_value(provider_turn),
      deputy_execution = deputy_execution
    ),
    connection = NULL,
    version = 3L
  )
  digest::digest(payload, algo = "sha256", serialize = FALSE)
}

tempest_agent_completion_issue <- function(
  registry,
  completion_id,
  prompt,
  response,
  provider_turn,
  deputy_execution
) {
  registry <- tempest_agent_completion_registry_validate(registry)
  completion_id <- tempest_agent_completion_assert_id(
    registry,
    completion_id,
    must_exist = FALSE
  )
  prompt <- tempest_agent_completion_text(prompt)
  response <- tempest_agent_completion_text(response)
  provider_turn <- tempest_agent_completion_provider_turn(provider_turn)
  deputy_execution <- tempest_agent_completion_trace(deputy_execution)
  entry <- list(
    completion_id = completion_id,
    prompt = prompt,
    response = response,
    provider_turn = provider_turn,
    deputy_execution = deputy_execution,
    digest = tempest_agent_completion_digest(
      prompt,
      response,
      provider_turn,
      deputy_execution
    ),
    state = "issued"
  )
  assign(completion_id, entry, registry$entries)
  completion_id
}

tempest_agent_completion_entry <- function(registry, completion_id, owner) {
  tempest_agent_completion_assert_owner(registry, owner)
  completion_id <- tempest_agent_completion_assert_id(registry, completion_id)
  get(completion_id, registry$entries, inherits = FALSE)
}

tempest_agent_completion_claim_value <- function(entry) {
  list(
    completion_id = entry$completion_id,
    prompt = rlang::duplicate(entry$prompt, shallow = FALSE),
    response = rlang::duplicate(entry$response, shallow = FALSE),
    provider_turn = rlang::duplicate(entry$provider_turn, shallow = FALSE),
    deputy_execution = rlang::duplicate(
      entry$deputy_execution,
      shallow = FALSE
    )
  )
}

tempest_agent_completion_claim <- function(registry, completion_id, owner) {
  entry <- tempest_agent_completion_entry(registry, completion_id, owner)
  if (!identical(entry$state, "issued")) {
    tempest_agent_completion_state_abort()
  }
  entry$state <- "processing"
  assign(completion_id, entry, registry$entries)
  tempest_agent_completion_claim_value(entry)
}

tempest_agent_completion_assert_claim <- function(
  registry,
  claim,
  owner,
  state = "processing"
) {
  if (
    !is.list(claim) ||
      is.data.frame(claim) ||
      !identical(
        names(claim),
        c(
          "completion_id",
          "prompt",
          "response",
          "provider_turn",
          "deputy_execution"
        )
      )
  ) {
    tempest_agent_completion_binding_abort()
  }
  entry <- tempest_agent_completion_entry(
    registry,
    claim$completion_id,
    owner
  )
  if (!identical(entry$state, state)) {
    tempest_agent_completion_state_abort()
  }
  digest <- tryCatch(
    tempest_agent_completion_digest(
      tempest_agent_completion_text(claim$prompt),
      tempest_agent_completion_text(claim$response),
      tempest_agent_completion_provider_turn(claim$provider_turn),
      tempest_agent_completion_trace(claim$deputy_execution)
    ),
    error = function(error) NULL
  )
  if (is.null(digest) || !identical(digest, entry$digest)) {
    tempest_agent_completion_binding_abort()
  }
  list(entry = entry, claim = claim)
}

tempest_agent_completion_release <- function(registry, claim, owner) {
  checked <- tempest_agent_completion_assert_claim(registry, claim, owner)
  checked$entry$state <- "issued"
  assign(claim$completion_id, checked$entry, registry$entries)
  invisible(claim$completion_id)
}

tempest_agent_completion_tombstone <- function(entry, state) {
  list(
    completion_id = entry$completion_id,
    digest = entry$digest,
    state = state
  )
}

tempest_agent_completion_consume <- function(registry, claim, owner) {
  checked <- tempest_agent_completion_assert_claim(registry, claim, owner)
  assign(
    claim$completion_id,
    tempest_agent_completion_tombstone(checked$entry, "consumed"),
    registry$entries
  )
  invisible(claim$completion_id)
}

tempest_agent_completion_cancel <- function(
  registry,
  completion_id,
  owner
) {
  entry <- tempest_agent_completion_entry(registry, completion_id, owner)
  if (!entry$state %in% c("issued", "processing")) {
    tempest_agent_completion_state_abort()
  }
  assign(
    completion_id,
    tempest_agent_completion_tombstone(entry, "cancelled"),
    registry$entries
  )
  invisible(completion_id)
}

tempest_agent_completion_status <- function(
  registry,
  completion_id,
  owner
) {
  tempest_agent_completion_entry(registry, completion_id, owner)$state
}

tempest_agent_completion_active <- function(registry) {
  registry <- tempest_agent_completion_registry_validate(registry)
  ids <- ls(registry$entries, all.names = TRUE)
  active <- vapply(
    ids,
    function(completion_id) {
      entry <- get(completion_id, registry$entries, inherits = FALSE)
      entry$state %in% c("issued", "processing")
    },
    logical(1)
  )
  sort(ids[active], method = "radix")
}

tempest_agent_completion_assert_quiescent <- function(registry) {
  if (length(tempest_agent_completion_active(registry)) > 0L) {
    tempest_agent_completion_state_abort()
  }
  invisible(TRUE)
}

tempest_agent_completion_rollback_issue <- function(
  registry,
  completion_id,
  owner
) {
  entry <- tryCatch(
    tempest_agent_completion_entry(registry, completion_id, owner),
    error = function(error) NULL
  )
  if (!is.null(entry) && identical(entry$state, "issued")) {
    rm(list = completion_id, envir = registry$entries)
  }
  invisible(completion_id)
}

tempest_agent_completion_tag <- function(x, completion_id) {
  if (
    !rlang::is_string(completion_id) ||
      !tempest_opaque_identifier_valid(completion_id)
  ) {
    tempest_agent_completion_id_abort()
  }
  attr(x, "tempest_agent_completion_id") <- completion_id
  x
}

tempest_agent_completion_id <- function(x) {
  completion_id <- attr(x, "tempest_agent_completion_id", exact = TRUE)
  if (
    !rlang::is_string(completion_id) ||
      !tempest_opaque_identifier_valid(completion_id)
  ) {
    tempest_agent_completion_id_abort()
  }
  completion_id
}
