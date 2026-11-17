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

tempest_researcher_app_safe_message <- function(value) {
  if (!is.character(value) || length(value) != 1L || is.na(value)) {
    return(NULL)
  }
  value <- substr(value, 1L, 320L)
  gsub(
    "(?i)\\b(api[_-]?key|authorization|bearer|password|token)\\b(?:\\s*[:=]\\s*|\\s+)(?:bearer\\s+)?[^[:space:]]+(?:\\s+[^[:space:]]+)?",
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
  allowed_fields <- c(fields, "research_strategy")
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
    value <- substr(value[[1L]], 1L, 240L)
    gsub(
      "(?i)\\b(api[_-]?key|authorization|bearer|password|token)\\b(?:\\s*[:=]\\s*|\\s+)(?:bearer\\s+)?[^[:space:]]+(?:\\s+[^[:space:]]+)?",
      "\\1=[redacted]",
      value,
      perl = TRUE
    )
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

tempest_researcher_worker <- function(
  kind,
  request,
  config,
  tools,
  mcp_config,
  output_dir,
  progress_path
) {
  if (identical(kind, "discover")) {
    servers <- tempest_researcher_app_strings(
      request$servers,
      "servers"
    )
    return(tempest_researcher_mcp_discover(mcp_config, servers))
  }
  if (!identical(kind, "research")) {
    tempest_researcher_app_abort(
      "Unknown researcher worker task.",
      class = "tempest_researcher_app_input_error"
    )
  }

  request <- tempest_researcher_app_validate_request(request)
  selected <- tempest_researcher_selected_tools(
    tools = tools,
    selected_tools = request$selected_tools,
    mcp_config = mcp_config,
    mcp_servers = request$mcp_servers,
    selected_mcp = request$selected_mcp
  )
  on.exit(
    {
      rm(selected)
      gc()
    },
    add = TRUE
  )
  cfg <- tempest_researcher_app_model_config(config, request$model)
  cfg <- tempest_researcher_tools_config(cfg, selected)
  progress <- function(event) {
    tempest_researcher_app_write_progress(progress_path, event)
  }
  enrichment <- list(
    allow_code = request$allow_code,
    executor = if (request$allow_code) tempest_r_executor() else NULL,
    image_generator = if (request$allow_images) {
      tempest_image_generator()
    } else {
      NULL
    },
    max_analyses = if (request$allow_code) request$max_analyses else 0L,
    max_illustrations = if (request$allow_images) {
      request$max_illustrations
    } else {
      0L
    }
  )
  result <- tempest_researcher_app_run_research(
    topic = request$topic,
    output_dir = output_dir,
    config = cfg,
    n_experts = request$n_experts,
    research_strategy = request$research_strategy,
    max_rounds = request$max_rounds,
    max_questions_per_perspective = request$max_questions,
    progress = progress,
    verbose = FALSE,
    enrichment = enrichment
  )
  list(
    path = result$report@path,
    status = result$report@status
  )
}

tempest_researcher_app_package_path <- function() {
  path <- getNamespaceInfo(asNamespace("tempest"), "path")
  if (file.exists(file.path(path, "R", "researcher-app-worker.R"))) {
    path
  } else {
    NULL
  }
}

tempest_researcher_app_snapshot_ragnar_store <- function(config, job_dir) {
  store <- config@ragnar_store
  worker_config <- config
  worker_config@ragnar_store <- NULL
  worker_config@embed_fn <- NULL

  if (is.null(store)) {
    return(list(config = worker_config, handoff = NULL))
  }

  store_class <- tryCatch(
    get("DuckDBRagnarStore", asNamespace("ragnar")),
    error = function(error) NULL
  )
  if (is.null(store_class) || !S7::S7_inherits(store, store_class)) {
    tempest_researcher_app_abort(
      "The configured Ragnar store cannot be copied to the research worker.",
      class = "tempest_researcher_app_config_error"
    )
  }

  connection <- store@con
  if (!DBI::dbIsValid(connection)) {
    tempest_researcher_app_abort(
      "The configured Ragnar store connection is not available.",
      class = "tempest_researcher_app_config_error"
    )
  }

  handoff <- file.path(job_dir, "ragnar-export")
  if (file.exists(handoff)) {
    tempest_researcher_app_abort(
      "A private Ragnar store snapshot could not be created.",
      class = "tempest_researcher_app_config_error"
    )
  }
  # Export through the live connection instead of copying its locked DB file.
  tryCatch(
    DBI::dbExecute(
      connection,
      paste(
        "EXPORT DATABASE",
        as.character(DBI::dbQuoteString(connection, handoff))
      )
    ),
    error = function(error) {
      tempest_researcher_app_abort(
        "The configured Ragnar store could not be copied to the research worker.",
        class = "tempest_researcher_app_config_error"
      )
    }
  )
  if (!file.exists(file.path(handoff, "schema.sql"))) {
    tempest_researcher_app_abort(
      "The configured Ragnar store could not be copied to the research worker.",
      class = "tempest_researcher_app_config_error"
    )
  }
  Sys.chmod(handoff, mode = "0700")

  list(
    config = worker_config,
    handoff = normalizePath(handoff, mustWork = TRUE)
  )
}

tempest_researcher_app_restore_ragnar_store <- function(config, handoff) {
  if (is.null(handoff)) {
    return(config)
  }
  if (
    !is.character(handoff) ||
      length(handoff) != 1L ||
      is.na(handoff) ||
      !file.exists(file.path(handoff, "schema.sql"))
  ) {
    tempest_researcher_app_abort(
      "The private Ragnar store snapshot is unavailable to the worker.",
      class = "tempest_researcher_app_config_error"
    )
  }

  # Import the snapshot into a fresh file so the child owns its connection.
  store_path <- file.path(dirname(handoff), "ragnar-worker.duckdb")
  connection <- tryCatch(
    DBI::dbConnect(
      duckdb::duckdb(),
      dbdir = store_path,
      array = "matrix"
    ),
    error = function(error) {
      tempest_researcher_app_abort(
        "The worker could not open its private Ragnar store.",
        class = "tempest_researcher_app_config_error"
      )
    }
  )
  connection_open <- TRUE
  on.exit(
    if (connection_open) {
      try(DBI::dbDisconnect(connection, shutdown = TRUE), silent = TRUE)
    },
    add = TRUE
  )

  tryCatch(
    DBI::dbExecute(
      connection,
      paste(
        "INSTALL fts; INSTALL vss; LOAD fts; LOAD vss;",
        "SET hnsw_enable_experimental_persistence = true;"
      )
    ),
    error = function(error) {
      tempest_researcher_app_abort(
        "The worker could not load the Ragnar store extensions.",
        class = "tempest_researcher_app_config_error"
      )
    }
  )
  tryCatch(
    DBI::dbExecute(
      connection,
      paste(
        "IMPORT DATABASE",
        as.character(DBI::dbQuoteString(connection, handoff))
      )
    ),
    error = function(error) {
      tempest_researcher_app_abort(
        "The worker could not restore its private Ragnar store.",
        class = "tempest_researcher_app_config_error"
      )
    }
  )
  DBI::dbDisconnect(connection, shutdown = TRUE)
  connection_open <- FALSE

  store <- tryCatch(
    ragnar::ragnar_store_connect(store_path, read_only = FALSE),
    error = function(error) {
      tempest_researcher_app_abort(
        "The worker could not reconnect its private Ragnar store.",
        class = "tempest_researcher_app_config_error"
      )
    }
  )
  config@ragnar_store <- store
  config@embed_fn <- store@embed
  config
}

tempest_researcher_app_callr_entry <- function(
  package_path,
  libpath,
  work,
  result_path,
  release_path,
  timeout_s = 120
) {
  owned_store <- NULL
  tryCatch(
    {
      options(ellmer_timeout_s = timeout_s)
      .libPaths(unique(c(libpath, .libPaths())))
      if (!is.null(package_path)) {
        if (!requireNamespace("pkgload", quietly = TRUE)) {
          stop("pkgload unavailable")
        }
        pkgload::load_all(package_path, quiet = TRUE)
      } else {
        loadNamespace("tempest")
      }
      restore_store <- get(
        "tempest_researcher_app_restore_ragnar_store",
        asNamespace("tempest")
      )
      if (!is.null(work$ragnar_handoff)) {
        work$config <- restore_store(work$config, work$ragnar_handoff)
        owned_store <- work$config@ragnar_store
      } else if (!is.null(work$config) && !is.null(work$config@ragnar_store)) {
        abort <- get("tempest_researcher_app_abort", asNamespace("tempest"))
        abort(
          "The research worker requires a private Ragnar store snapshot.",
          class = "tempest_researcher_app_config_error"
        )
      }
      work["ragnar_handoff"] <- NULL
      worker <- get("tempest_researcher_worker", asNamespace("tempest"))
      result <- do.call(worker, work)
      list(ok = TRUE, result = result)
    },
    error = function(error) {
      classes <- class(error)
      allowed <- c(
        "tempest_config_error",
        "tempest_execution_error",
        "tempest_persistence_error",
        "tempest_research_tools_error",
        "tempest_error",
        "tempest_researcher_app_error",
        "tempest_researcher_app_config_error",
        "tempest_researcher_app_input_error",
        "tempest_researcher_app_tools_error"
      )
      found <- intersect(classes, allowed)
      error_class <- if (length(found)) found[[1L]] else "tempest_worker_error"
      safe_classes <- c(
        "tempest_researcher_app_error",
        "tempest_researcher_app_config_error",
        "tempest_researcher_app_input_error",
        "tempest_researcher_app_tools_error"
      )
      error_message <- if (any(classes %in% safe_classes)) {
        message <- substr(conditionMessage(error), 1L, 320L)
        gsub(
          "(?i)\\b(api[_-]?key|authorization|bearer|password|token)\\b(?:\\s*[:=]\\s*|\\s+)(?:bearer\\s+)?[^[:space:]]+(?:\\s+[^[:space:]]+)?",
          "\\1=[redacted]",
          message,
          perl = TRUE
        )
      } else {
        NULL
      }
      list(
        ok = FALSE,
        error_class = error_class,
        error_message = error_message
      )
    },
    finally = {
      if (!is.null(owned_store)) {
        try(
          DBI::dbDisconnect(owned_store@con, shutdown = TRUE),
          silent = TRUE
        )
      }
    }
  ) -> payload
  saveRDS(payload, file = result_path, version = 2L)
  while (!file.exists(release_path)) {
    Sys.sleep(0.05)
  }
  invisible(NULL)
}

tempest_researcher_app_spawn <- function(func, args, libpath) {
  tempest_require("callr", "The dedicated researcher app needs callr.")
  environment(func) <- baseenv()
  callr::r_bg(
    func = func,
    args = args,
    libpath = libpath,
    supervise = TRUE,
    cleanup_tree = TRUE,
    stdout = base::nullfile(),
    stderr = base::nullfile()
  )
}

tempest_researcher_app_run_research <- function(...) {
  tempest_researcher(...)
}

tempest_researcher_app_worker_result <- function(path) {
  if (!file.exists(path)) {
    return(NULL)
  }
  tryCatch(readRDS(path), error = function(error) NULL)
}

tempest_researcher_app_process_alive <- function(worker) {
  if (is.null(worker)) {
    FALSE
  } else {
    isTRUE(tryCatch(worker$is_alive(), error = function(error) FALSE))
  }
}

tempest_researcher_app_reap <- function(worker) {
  if (is.null(worker)) {
    return(TRUE)
  }
  tree_kill <- tryCatch(worker$kill_tree(), error = function(error) NULL)
  if (is.null(tree_kill)) {
    try(worker$kill(), silent = TRUE)
    try(worker$wait(timeout = 5000), silent = TRUE)
    return(FALSE)
  }
  try(worker$wait(timeout = 5000), silent = TRUE)
  if (tempest_researcher_app_process_alive(worker)) {
    tree_kill <- tryCatch(worker$kill_tree(), error = function(error) NULL)
    if (is.null(tree_kill)) {
      try(worker$kill(), silent = TRUE)
    }
    try(worker$wait(timeout = 5000), silent = TRUE)
  }
  !tempest_researcher_app_process_alive(worker) && !is.null(tree_kill)
}

TempestResearcherAppSession <- R6::R6Class(
  "TempestResearcherAppSession",
  public = list(
    initialize = function(
      config,
      tools = list(),
      mcp_config = NULL,
      output_dir = tempdir()
    ) {
      if (!S7::S7_inherits(config, TempestConfig)) {
        tempest_researcher_app_abort(
          "`config` must be created by `tempest_config()`. ",
          class = "tempest_researcher_app_config_error"
        )
      }
      if (!is.list(tools) || is.data.frame(tools)) {
        tempest_researcher_app_abort(
          "`tools` must be a named list.",
          class = "tempest_researcher_app_config_error"
        )
      }
      if (
        length(tools) &&
          (is.null(names(tools)) ||
            any(!nzchar(names(tools))) ||
            anyDuplicated(names(tools)))
      ) {
        tempest_researcher_app_abort(
          "`tools` must have distinct non-empty names.",
          class = "tempest_researcher_app_config_error"
        )
      }
      output_dir <- tempest_researcher_app_string(
        output_dir,
        "output_dir",
        4096L
      )
      if (file.exists(output_dir) && !dir.exists(output_dir)) {
        tempest_researcher_app_abort(
          "`output_dir` must name a directory parent.",
          class = "tempest_researcher_app_config_error"
        )
      }
      if (
        !dir.exists(output_dir) && !dir.create(output_dir, recursive = TRUE)
      ) {
        tempest_researcher_app_abort(
          "The report output directory could not be created.",
          class = "tempest_researcher_app_config_error"
        )
      }
      if (!is.null(mcp_config)) {
        tempest_researcher_app_string(mcp_config, "mcp_config", 4096L)
        tempest_researcher_mcp_servers(mcp_config)
      }
      private$config <- config
      private$tools <- tools
      private$mcp_config_path <- mcp_config
      private$mcp_config_owned <- FALSE
      private$output_dir <- normalizePath(output_dir, mustWork = TRUE)
      private$session_dir <- tempfile("tempest-researcher-session-")
      if (!dir.create(private$session_dir, mode = "0700")) {
        tempest_researcher_app_abort(
          "A private researcher session directory could not be created.",
          class = "tempest_researcher_app_config_error"
        )
      }
      mode_set <- Sys.chmod(private$session_dir, mode = "0700")
      if (!identical(.Platform$OS.type, "windows") && !isTRUE(mode_set)) {
        unlink(private$session_dir, recursive = TRUE, force = TRUE)
        tempest_researcher_app_abort(
          "The private researcher session permissions could not be set.",
          class = "tempest_researcher_app_config_error"
        )
      }
      private$status <- "idle"
      private$kind <- NULL
      private$result <- NULL
      private$error <- NULL
      private$progress <- list()
      private$worker <- NULL
      private$result_path <- NULL
      private$release_path <- NULL
      private$progress_path <- NULL
      private$report_candidate <- NULL
      private$job_dir <- NULL
      private$closed <- FALSE
    },
    snapshot = function() {
      private$refresh_progress()
      list(
        status = private$status,
        kind = private$kind,
        result = private$result,
        error = private$error,
        progress = private$progress
      )
    },
    set_mcp_config = function(path) {
      private$assert_open()
      if (
        identical(private$status, "running") ||
          !is.null(private$worker)
      ) {
        tempest_researcher_app_abort(
          "MCP configuration cannot change while a worker is active.",
          class = "tempest_researcher_app_state_error"
        )
      }
      path <- tempest_researcher_app_string(path, "path", 4096L)
      servers <- tempest_researcher_mcp_servers(path)
      copied <- tempfile(
        "mcp-config-",
        tmpdir = private$session_dir,
        fileext = ".json"
      )
      if (!file.copy(path, copied, overwrite = FALSE)) {
        unlink(copied, force = TRUE)
        tempest_researcher_app_abort(
          "The MCP configuration could not be copied into the private session.",
          class = "tempest_researcher_app_config_error"
        )
      }
      mode_set <- Sys.chmod(copied, mode = "0600")
      if (!identical(.Platform$OS.type, "windows") && !isTRUE(mode_set)) {
        unlink(copied, force = TRUE)
        tempest_researcher_app_abort(
          "The private MCP configuration permissions could not be set.",
          class = "tempest_researcher_app_config_error"
        )
      }
      private$replace_mcp_config(copied, owned = TRUE)
      servers
    },
    discover = function(servers) {
      private$assert_open()
      servers <- tempest_researcher_app_strings(servers, "servers")
      private$launch(
        kind = "discover",
        request = list(servers = servers)
      )
    },
    start = function(request) {
      private$assert_open()
      request <- tempest_researcher_app_validate_request(request)
      expert_limit <- min(10L, private$config@max_active_experts)
      if (request$n_experts > expert_limit) {
        tempest_researcher_app_abort(
          paste0(
            "`n_experts` cannot exceed the configured expert limit of ",
            expert_limit,
            "."
          ),
          class = "tempest_researcher_app_input_error"
        )
      }
      private$launch(
        kind = "research",
        request = request
      )
    },
    poll = function() {
      private$assert_open()
      private$refresh_progress()
      if (!identical(private$status, "running")) {
        return(self$snapshot())
      }
      payload <- tempest_researcher_app_worker_result(private$result_path)
      if (is.null(payload)) {
        if (!tempest_researcher_app_process_alive(private$worker)) {
          reaped <- tempest_researcher_app_reap(private$worker)
          private$worker <- NULL
          message <- if (reaped) {
            "The research worker stopped before it returned a result."
          } else {
            "The researcher worker process tree could not be reaped."
          }
          private$fail(message)
        }
        return(self$snapshot())
      }
      if (!tempest_researcher_app_reap(private$worker)) {
        private$worker <- NULL
        private$fail("The researcher worker process tree could not be reaped.")
        return(self$snapshot())
      }
      private$worker <- NULL
      if (
        !is.list(payload) || !is.logical(payload$ok) || length(payload$ok) != 1L
      ) {
        private$fail("The research worker returned an invalid result.")
        return(self$snapshot())
      }
      if (!isTRUE(payload$ok)) {
        class <- payload$error_class
        if (!is.character(class) || length(class) != 1L || is.na(class)) {
          class <- "tempest_worker_error"
        }
        class <- gsub("[^A-Za-z0-9_.]", "", class)
        safe_classes <- c(
          "tempest_researcher_app_error",
          "tempest_researcher_app_config_error",
          "tempest_researcher_app_input_error",
          "tempest_researcher_app_tools_error"
        )
        message <- payload$error_message
        if (
          class %in%
            safe_classes &&
            is.character(message) &&
            length(message) == 1L &&
            !is.na(message) &&
            nzchar(message) &&
            nchar(message, type = "bytes") <= 320L
        ) {
          private$fail(tempest_researcher_app_safe_message(message))
        } else {
          private$fail(paste0("The research worker failed (", class, ")."))
        }
        return(self$snapshot())
      }
      if (identical(private$kind, "discover")) {
        if (!is.data.frame(payload$result)) {
          private$fail("Tool discovery returned an invalid catalog.")
          return(self$snapshot())
        }
        private$result <- payload$result
        private$status <- "complete"
        private$release()
        return(self$snapshot())
      }
      result <- payload$result
      if (
        !is.list(result) ||
          !is.character(result$path) ||
          length(result$path) != 1L ||
          !identical(result$path, private$report_candidate)
      ) {
        private$fail("The research worker returned an invalid report path.")
        return(self$snapshot())
      }
      report <- tryCatch(
        tempest_read_enriched_report(result$path),
        error = function(error) NULL
      )
      if (is.null(report)) {
        private$fail("The completed report failed integrity validation.")
        return(self$snapshot())
      }
      public_path <- private$public_report_path()
      if (
        file.exists(public_path) ||
          dir.exists(public_path) ||
          !file.rename(private$report_candidate, public_path)
      ) {
        private$fail("The validated report could not be published safely.")
        return(self$snapshot())
      }
      published <- tryCatch(
        tempest_read_enriched_report(public_path),
        error = function(error) NULL
      )
      if (is.null(published)) {
        unlink(public_path, recursive = TRUE, force = TRUE)
        private$fail("The published report failed integrity validation.")
        return(self$snapshot())
      }
      private$report_candidate <- NULL
      private$result <- list(path = published@path, status = published@status)
      private$remove_candidate()
      private$status <- "complete"
      private$release()
      self$snapshot()
    },
    cancel = function() {
      if (!identical(private$status, "running")) {
        return(self$snapshot())
      }
      reaped <- tempest_researcher_app_reap(private$worker)
      private$worker <- NULL
      private$release()
      private$remove_candidate()
      private$result <- NULL
      private$error <- if (reaped) {
        NULL
      } else {
        "The researcher worker process tree could not be reaped."
      }
      private$status <- if (reaped) "cancelled" else "error"
      self$snapshot()
    },
    close = function() {
      if (private$closed) {
        return(invisible(NULL))
      }
      if (identical(private$status, "running")) {
        self$cancel()
      }
      private$remove_candidate()
      private$release()
      private$worker <- NULL
      private$closed <- TRUE
      unlink(private$session_dir, recursive = TRUE, force = TRUE)
      invisible(NULL)
    }
  ),
  private = list(
    config = NULL,
    tools = NULL,
    mcp_config_path = NULL,
    mcp_config_owned = FALSE,
    output_dir = NULL,
    session_dir = NULL,
    status = NULL,
    kind = NULL,
    result = NULL,
    error = NULL,
    progress = list(),
    worker = NULL,
    result_path = NULL,
    release_path = NULL,
    progress_path = NULL,
    report_candidate = NULL,
    job_dir = NULL,
    closed = FALSE,
    assert_open = function() {
      if (private$closed) {
        tempest_researcher_app_abort(
          "The researcher session is closed.",
          class = "tempest_researcher_app_state_error"
        )
      }
    },
    replace_mcp_config = function(path, owned) {
      previous <- private$mcp_config_path
      previous_owned <- private$mcp_config_owned
      private$mcp_config_path <- path
      private$mcp_config_owned <- owned
      if (previous_owned && !is.null(previous) && !identical(previous, path)) {
        unlink(previous, force = TRUE)
      }
    },
    launch = function(kind, request) {
      if (identical(private$status, "running")) {
        tempest_researcher_app_abort(
          "This researcher session already has an active worker.",
          class = "tempest_researcher_app_state_error"
        )
      }
      timeout_s <- tempest_shiny_provider_timeout()
      if (!is.null(private$worker)) {
        if (!tempest_researcher_app_reap(private$worker)) {
          private$fail(
            "The researcher worker process tree could not be reaped."
          )
          return(invisible(self$snapshot()))
        }
        private$worker <- NULL
      }
      private$release()
      private$remove_candidate()
      private$status <- "running"
      private$kind <- kind
      private$result <- NULL
      private$error <- NULL
      private$progress <- list()
      token <- basename(tempfile("run-", tmpdir = private$session_dir))
      private$result_path <- file.path(
        private$session_dir,
        paste0(token, ".rds")
      )
      private$release_path <- file.path(
        private$session_dir,
        paste0(token, ".release")
      )
      private$progress_path <- file.path(
        private$session_dir,
        paste0(token, ".jsonl")
      )
      if (identical(kind, "research")) {
        job_dir <- file.path(
          private$output_dir,
          paste0(".tempest-job-", token)
        )
        if (!dir.create(job_dir, mode = "0700")) {
          private$fail(
            "A private report staging directory could not be created."
          )
          return(invisible(self$snapshot()))
        }
        private$job_dir <- job_dir
        Sys.chmod(job_dir, mode = "0700")
        private$report_candidate <- file.path(job_dir, "report")
      }
      work <- list(
        kind = kind,
        request = request,
        config = private$config,
        ragnar_handoff = NULL,
        tools = private$tools,
        mcp_config = private$mcp_config_path,
        output_dir = private$report_candidate,
        progress_path = private$progress_path
      )
      if (identical(kind, "research")) {
        handoff <- tryCatch(
          tempest_researcher_app_snapshot_ragnar_store(
            private$config,
            private$job_dir
          ),
          error = function(error) error
        )
        if (inherits(handoff, "error")) {
          private$fail(
            "The configured Ragnar store could not be prepared for the research worker."
          )
          return(invisible(self$snapshot()))
        }
        work$config <- handoff$config
        work$ragnar_handoff <- handoff$handoff
      }
      if (identical(kind, "discover")) {
        work$config <- NULL
        work$ragnar_handoff <- NULL
        work$tools <- list()
        work$output_dir <- NULL
        work$progress_path <- NULL
      }
      args <- list(
        package_path = tempest_researcher_app_package_path(),
        libpath = .libPaths(),
        work = work,
        result_path = private$result_path,
        release_path = private$release_path,
        timeout_s = timeout_s
      )
      private$worker <- tryCatch(
        tempest_researcher_app_spawn(
          tempest_researcher_app_callr_entry,
          args,
          libpath = .libPaths()
        ),
        error = function(error) NULL
      )
      if (is.null(private$worker)) {
        private$fail("The research worker could not be started.")
      }
      invisible(self$snapshot())
    },
    refresh_progress = function() {
      if (!is.null(private$progress_path)) {
        private$progress <- tempest_researcher_app_read_progress(
          private$progress_path
        )
      }
      invisible(private$progress)
    },
    release = function() {
      paths <- Filter(
        Negate(is.null),
        list(
          private$release_path,
          private$result_path,
          private$progress_path
        )
      )
      if (length(paths)) {
        unlink(unlist(paths, use.names = FALSE), force = TRUE)
      }
      private$release_path <- NULL
      private$result_path <- NULL
      private$progress_path <- NULL
      invisible(NULL)
    },
    remove_candidate = function() {
      if (!is.null(private$job_dir)) {
        unlink(private$job_dir, recursive = TRUE, force = TRUE)
        private$job_dir <- NULL
      }
      private$report_candidate <- NULL
      invisible(NULL)
    },
    fail = function(message) {
      reaped <- tempest_researcher_app_reap(private$worker)
      private$worker <- NULL
      private$release()
      private$remove_candidate()
      private$result <- NULL
      private$error <- if (reaped) {
        message
      } else {
        "The researcher worker process tree could not be reaped."
      }
      private$status <- "error"
      invisible(NULL)
    },
    public_report_path = function() {
      suffix <- sub("^\\.tempest-job-", "", basename(private$job_dir))
      file.path(private$output_dir, paste0("research-", suffix))
    },
    finalize = function() {
      try(self$close(), silent = TRUE)
    }
  ),
  cloneable = FALSE
)
