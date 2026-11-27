# Researcher app Ragnar store snapshot and restore handoff

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
