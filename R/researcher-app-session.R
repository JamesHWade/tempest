# Researcher app session R6 lifecycle

TempestResearcherAppSession <- R6::R6Class(
  "TempestResearcherAppSession",
  public = list(
    initialize = function(
      config,
      tools = list(),
      mcp_config = NULL,
      output_dir = tempdir(),
      graft_store_path = NULL,
      graft_store_handoff = NULL,
      memory_purpose = "research",
      memory_eligible = NULL
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
      private$graft_store_handoff <- graft_store_handoff %||%
        if (!is.null(graft_store_path)) {
          list(kind = "local", path = graft_store_path)
        } else {
          NULL
        }
      private$memory_purpose <- memory_purpose
      private$memory_eligible <- memory_eligible
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
        private$fail(private$format_worker_error(payload))
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
      private$publish_report_result(payload$result)
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
    graft_store_handoff = NULL,
    memory_purpose = "research",
    memory_eligible = NULL,
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
    format_worker_error = function(payload) {
      class <- payload$error_class
      if (!is.character(class) || length(class) != 1L || is.na(class)) {
        class <- "tempest_worker_error"
      }
      class <- gsub("[^A-Za-z0-9_.]", "", class)
      safe_classes <- c(
        "tempest_graft_memory_error",
        "tempest_knowledge_error",
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
        tempest_researcher_app_safe_message(message)
      } else {
        paste0("The research worker failed (", class, ").")
      }
    },
    publish_report_result = function(result) {
      if (
        !is.list(result) ||
          !is.character(result$path) ||
          length(result$path) != 1L ||
          !identical(result$path, private$report_candidate)
      ) {
        private$fail("The research worker returned an invalid report path.")
        return(invisible(NULL))
      }
      report <- tryCatch(
        tempest_read_enriched_report(result$path),
        error = function(error) NULL
      )
      if (is.null(report)) {
        private$fail("The completed report failed integrity validation.")
        return(invisible(NULL))
      }
      public_path <- private$public_report_path()
      if (
        file.exists(public_path) ||
          dir.exists(public_path) ||
          !file.rename(private$report_candidate, public_path)
      ) {
        private$fail("The validated report could not be published safely.")
        return(invisible(NULL))
      }
      published <- tryCatch(
        tempest_read_enriched_report(public_path),
        error = function(error) NULL
      )
      if (is.null(published)) {
        unlink(public_path, recursive = TRUE, force = TRUE)
        private$fail("The published report failed integrity validation.")
        return(invisible(NULL))
      }
      if (
        is.list(result$graft) &&
          (rlang::is_string(result$graft$selection_id) ||
            rlang::is_string(result$graft$publish_error))
      ) {
        tryCatch(
          tempest_researcher_memory_write_record(
            output_dir = private$output_dir,
            report_id = basename(public_path),
            selection_id = result$graft$selection_id,
            research_run_id = result$graft$research_run_id,
            bundle_id = result$graft$bundle_id,
            publish_error = result$graft$publish_error
          ),
          error = function(error) NULL
        )
      }
      private$report_candidate <- NULL
      private$result <- list(path = published@path, status = published@status)
      private$remove_candidate()
      private$status <- "complete"
      private$release()
      invisible(NULL)
    },
    prepare_work_payload = function(kind, request, token) {
      if (identical(kind, "research")) {
        job_dir <- file.path(
          private$output_dir,
          paste0(".tempest-job-", token)
        )
        if (!dir.create(job_dir, mode = "0700")) {
          private$fail(
            "A private report staging directory could not be created."
          )
          return(NULL)
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
          return(NULL)
        }
        work$config <- handoff$config
        work$ragnar_handoff <- handoff$handoff
        if (!is.null(private$graft_store_handoff)) {
          work$graft_store_handoff <- private$graft_store_handoff
          work$memory_purpose <- private$memory_purpose
          work$memory_eligible <- private$memory_eligible
        }
      }
      if (identical(kind, "discover")) {
        work$config <- NULL
        work$ragnar_handoff <- NULL
        work$tools <- list()
        work$output_dir <- NULL
        work$progress_path <- NULL
      }
      work
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
      work <- private$prepare_work_payload(kind, request, token)
      if (is.null(work)) {
        return(invisible(self$snapshot()))
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
