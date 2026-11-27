# Researcher app background worker execution and process management

tempest_researcher_worker <- function(
  kind,
  request,
  config,
  tools,
  mcp_config,
  output_dir,
  progress_path,
  graft_store_path = NULL,
  graft_store_handoff = NULL,
  memory_purpose = "research",
  memory_eligible = NULL
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
  opened_memory <- list(store = NULL, owns_connection = FALSE)
  memory_store <- NULL
  knowledge <- NULL
  memory_descriptor <- graft_store_handoff %||% graft_store_path
  if (!is.null(memory_descriptor)) {
    opened_memory <- tempest_graft_store_open_handoff(memory_descriptor)
    memory_store <- opened_memory$store
    on.exit(
      if (isTRUE(opened_memory$owns_connection)) {
        tempest_graft_store_close(opened_memory$store)
      },
      add = TRUE
    )
    if (isTRUE(request$use_memory_tools)) {
      memory_tools <- tempest_graft_memory_tools(
        memory_store,
        purpose = memory_purpose,
        eligible = memory_eligible
      )
      if (length(memory_tools) > 0L) {
        selected <- c(selected, memory_tools)
        tempest_researcher_checked_tool_names(selected)
      }
    }
    if (!is.null(request$memory_basis_stream)) {
      knowledge <- tempest_graft_reuse_stream(
        memory_store,
        stream = request$memory_basis_stream,
        purpose = memory_purpose,
        eligible = memory_eligible
      )
    }
  }
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
  run_args <- list(
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
  if (!is.null(knowledge)) {
    run_args$knowledge <- knowledge
  }
  result <- do.call(tempest_researcher_app_run_research, run_args)
  output <- list(
    path = result$report@path,
    status = result$report@status
  )
  if (!is.null(memory_store) && !is.null(result$research)) {
    output$graft <- tryCatch(
      {
        bundle <- tempest_promotion_bundle(result$research)
        selection_id <- tempest_publish_artifact_research(
          bundle,
          memory_store,
          report = tempest_report(result$research)
        )
        list(
          selection_id = selection_id,
          research_run_id = bundle@research_run_id,
          bundle_id = bundle@bundle_id,
          publish_error = NULL
        )
      },
      error = function(error) {
        list(
          selection_id = NULL,
          research_run_id = NULL,
          bundle_id = NULL,
          publish_error = conditionMessage(error)
        )
      }
    )
  }
  output
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
        "tempest_graft_memory_error",
        "tempest_knowledge_error",
        "tempest_error",
        "tempest_researcher_app_error",
        "tempest_researcher_app_config_error",
        "tempest_researcher_app_input_error",
        "tempest_researcher_app_tools_error"
      )
      found <- intersect(classes, allowed)
      error_class <- if (length(found)) found[[1L]] else "tempest_worker_error"
      safe_classes <- c(
        "tempest_graft_memory_error",
        "tempest_knowledge_error",
        "tempest_researcher_app_error",
        "tempest_researcher_app_config_error",
        "tempest_researcher_app_input_error",
        "tempest_researcher_app_tools_error"
      )
      error_message <- if (
        any(classes %in% safe_classes) && isNamespaceLoaded("tempest")
      ) {
        safe_message <- get(
          "tempest_researcher_app_safe_message",
          asNamespace("tempest")
        )
        safe_message(conditionMessage(error))
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
