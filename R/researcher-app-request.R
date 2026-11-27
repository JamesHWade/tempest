# Researcher app request validation, model config, and progress log

tempest_researcher_app_abort <- function(
  message,
  class = "tempest_researcher_app_error"
) {
  tempest_abort(
    message,
    class = c(class, "tempest_error"),
    .envir = rlang::caller_env()
  )
}

tempest_researcher_app_string <- function(value, name, max_bytes = 2000L) {
  if (
    !is.character(value) ||
      length(value) != 1L ||
      is.na(value) ||
      !nzchar(trimws(value)) ||
      nchar(value, type = "bytes") > max_bytes
  ) {
    tempest_researcher_app_abort(
      paste0("`", name, "` must be one bounded non-empty string."),
      class = "tempest_researcher_app_input_error"
    )
  }
  trimws(value)
}

tempest_researcher_app_strings <- function(value, name, allow_empty = TRUE) {
  if (
    !is.character(value) ||
      anyNA(value) ||
      any(!nzchar(value)) ||
      any(nchar(value, type = "bytes") > 512L) ||
      anyDuplicated(value) ||
      length(value) > 100L ||
      (!allow_empty && !length(value))
  ) {
    tempest_researcher_app_abort(
      paste0("`", name, "` must be a bounded vector of distinct strings."),
      class = "tempest_researcher_app_input_error"
    )
  }
  value
}

tempest_researcher_app_count <- function(value, name, minimum, maximum) {
  if (
    !is.numeric(value) ||
      length(value) != 1L ||
      is.na(value) ||
      !is.finite(value) ||
      value != trunc(value) ||
      value < minimum ||
      value > maximum
  ) {
    tempest_researcher_app_abort(
      paste0(
        "`",
        name,
        "` must be a whole number from ",
        minimum,
        " to ",
        maximum,
        "."
      ),
      class = "tempest_researcher_app_input_error"
    )
  }
  as.integer(value)
}

tempest_researcher_app_flag <- function(value, name) {
  if (!is.logical(value) || length(value) != 1L || is.na(value)) {
    tempest_researcher_app_abort(
      paste0("`", name, "` must be `TRUE` or `FALSE`."),
      class = "tempest_researcher_app_input_error"
    )
  }
  value
}

tempest_researcher_app_safe_message <- function(value, max_chars = 320L) {
  if (!is.character(value) || length(value) != 1L || is.na(value)) {
    return(NULL)
  }
  value <- substr(value, 1L, max_chars)
  value <- gsub(
    "(?i)(postgres(?:ql)?://[^:@[:space:]]+:)[^@[:space:]]+(@)",
    "\\1[redacted]\\2",
    value,
    perl = TRUE
  )
  gsub(
    "(?i)\\b(api[_-]?key|authorization|bearer|password|passwd|pwd|secret|token)\\b(?:\\s*[:=]\\s*|\\s+)(?:bearer\\s+)?[^[:space:]]+(?:\\s+[^[:space:]]+)?",
    "\\1=[redacted]",
    value,
    perl = TRUE
  )
}

tempest_researcher_app_validate_request <- function(request) {
  fields <- c(
    "topic",
    "model",
    "n_experts",
    "max_rounds",
    "max_questions",
    "allow_code",
    "allow_images",
    "max_analyses",
    "max_illustrations",
    "selected_tools",
    "mcp_servers",
    "selected_mcp"
  )
  allowed_fields <- c(
    fields,
    "research_strategy",
    "use_memory_tools",
    "memory_basis_stream"
  )
  if (
    !is.list(request) ||
      is.data.frame(request) ||
      is.null(names(request)) ||
      anyDuplicated(names(request)) ||
      !all(fields %in% names(request)) ||
      !all(names(request) %in% allowed_fields)
  ) {
    tempest_researcher_app_abort(
      "The research request has missing or unknown fields.",
      class = "tempest_researcher_app_input_error"
    )
  }
  request$topic <- tempest_researcher_app_string(request$topic, "topic")
  request$model <- tempest_researcher_app_string(request$model, "model", 256L)
  if (is.null(request$research_strategy)) {
    request$research_strategy <- "key_questions"
  }
  if (
    !is.character(request$research_strategy) ||
      length(request$research_strategy) != 1L ||
      is.na(request$research_strategy) ||
      !request$research_strategy %in% c("key_questions", "conversation")
  ) {
    tempest_researcher_app_abort(
      "`research_strategy` must be `key_questions` or `conversation`.",
      class = "tempest_researcher_app_input_error"
    )
  }
  for (name in c("n_experts", "max_rounds", "max_questions")) {
    request[[name]] <- tempest_researcher_app_count(
      request[[name]],
      name,
      1L,
      10L
    )
  }
  for (name in c("max_analyses", "max_illustrations")) {
    request[[name]] <- tempest_researcher_app_count(
      request[[name]],
      name,
      0L,
      10L
    )
  }
  request$allow_code <- tempest_researcher_app_flag(
    request$allow_code,
    "allow_code"
  )
  request$allow_images <- tempest_researcher_app_flag(
    request$allow_images,
    "allow_images"
  )
  for (name in c("selected_tools", "mcp_servers", "selected_mcp")) {
    request[[name]] <- tempest_researcher_app_strings(request[[name]], name)
  }
  if (is.null(request$use_memory_tools)) {
    request$use_memory_tools <- FALSE
  } else {
    request$use_memory_tools <- tempest_researcher_app_flag(
      request$use_memory_tools,
      "use_memory_tools"
    )
  }
  if (!is.null(request$memory_basis_stream)) {
    request$memory_basis_stream <- tempest_researcher_app_string(
      request$memory_basis_stream,
      "memory_basis_stream",
      256L
    )
  }
  request
}

tempest_researcher_app_model_config <- function(config, model) {
  roles <- names(config@models)
  if (is.null(roles) || !length(roles)) {
    tempest_researcher_app_abort(
      "The research configuration has no model roles.",
      class = "tempest_researcher_app_config_error"
    )
  }
  if (!is.null(config@chat) && is.null(config@chat_fn)) {
    provider <- tryCatch(config@chat$get_provider(), error = function(error) {
      NULL
    })
    provider_name <- if (is.null(provider)) {
      ""
    } else {
      tryCatch(provider@name, error = function(error) "")
    }
    provider_id <- tolower(gsub("[^a-z0-9]", "", tolower(provider_name)))
    if (provider_id %in% c("gemini", "googlegemini")) {
      provider_id <- "google"
    }
    native_model <- tryCatch(
      config@chat$get_model(),
      error = function(error) NULL
    )
    is_native_model <- identical(model, native_model)
    has_prefix <- grepl("/", model, fixed = TRUE)
    selected_provider <- if (has_prefix) {
      sub("/.*$", "", model)
    } else {
      provider_id
    }
    selected_model <- if (has_prefix) {
      sub("^[^/]*/", "", model)
    } else {
      model
    }
    if (is_native_model) {
      selected_provider <- provider_id
      selected_model <- model
    }
    if (!identical(selected_provider, provider_id) || !nzchar(selected_model)) {
      tempest_researcher_app_abort(
        paste(
          "The selected model uses a different provider from the configured",
          "chat. Choose a model from that provider or configure `chat_fn`."
        ),
        class = "tempest_researcher_app_config_error"
      )
    }
    chat <- config@chat$clone(deep = TRUE)
    chat$set_model(selected_model)
    config@chat <- chat
  }
  config@models <- stats::setNames(
    rep(list(model), length(roles)),
    roles
  )
  config
}

tempest_researcher_app_safe_progress <- function(event) {
  if (S7::S7_inherits(event, tempest_progress_event)) {
    event <- tempest_progress_record(event)
  }
  if (!is.list(event) || is.data.frame(event)) {
    return(NULL)
  }
  fields <- intersect(
    c("event_type", "stage", "step", "status", "timestamp", "message"),
    names(event)
  )
  safe <- lapply(event[fields], function(value) {
    if (!is.character(value) || !length(value) || is.na(value[[1L]])) {
      return(NULL)
    }
    tempest_researcher_app_safe_message(value[[1L]], max_chars = 240L)
  })
  safe <- safe[!vapply(safe, is.null, logical(1))]
  if (!length(safe)) NULL else safe
}

tempest_researcher_app_write_progress <- function(path, event) {
  safe <- tempest_researcher_app_safe_progress(event)
  if (is.null(safe)) {
    return(invisible(NULL))
  }
  encoded <- as.character(jsonlite::toJSON(
    safe,
    auto_unbox = TRUE,
    null = "null",
    na = "null"
  ))
  if (nchar(encoded, type = "bytes") > 4096L) {
    return(invisible(NULL))
  }
  cat(encoded, "\n", file = path, append = TRUE)
  info <- file.info(path)
  if (!is.na(info$size) && info$size > 512 * 1024) {
    lines <- tryCatch(readLines(path, warn = FALSE), error = function(error) {
      character()
    })
    keep <- utils::tail(lines, 100L)
    writeLines(keep, path, useBytes = TRUE)
  }
  invisible(NULL)
}

tempest_researcher_app_read_progress <- function(path) {
  if (!file.exists(path)) {
    return(list())
  }
  lines <- tryCatch(readLines(path, warn = FALSE), error = function(error) {
    character()
  })
  lines <- utils::tail(lines, 100L)
  events <- lapply(lines, function(line) {
    tryCatch(
      jsonlite::fromJSON(line, simplifyVector = FALSE),
      error = function(error) NULL
    )
  })
  Filter(Negate(is.null), events)
}
