test_that("the session keeps uploaded MCP configuration private and owned", {
  skip_on_os("windows")
  output_dir <- withr::local_tempdir()
  host_config <- tempfile(fileext = ".json")
  upload <- tempfile(fileext = ".json")
  writeLines('{"mcpServers":{"fixture":{}}}', host_config)
  writeLines('{"mcpServers":{"uploaded":{}}}', upload)
  local_mocked_bindings(tempest_researcher_mcp_servers = function(path) {
    if (identical(path, upload) || grepl("mcp-config-", path, fixed = TRUE)) {
      "uploaded"
    } else {
      "fixture"
    }
  })

  session <- TempestResearcherAppSession$new(
    config = tempest_config(),
    mcp_config = host_config,
    output_dir = output_dir
  )
  private <- session$.__enclos_env__$private
  private_dir <- private$session_dir
  expect_identical(private$mcp_config_path, host_config)
  expect_identical(session$set_mcp_config(upload), "uploaded")
  copied <- private$mcp_config_path

  expect_false(identical(copied, upload))
  expect_true(startsWith(copied, private_dir))
  expect_true(file.exists(host_config))
  expect_true(file.exists(upload))
  expect_identical(
    bitwAnd(
      as.integer(file.info(private_dir)$mode),
      as.integer(as.octmode("0777"))
    ),
    as.integer(as.octmode("0700"))
  )
  expect_identical(
    bitwAnd(
      as.integer(file.info(copied)$mode),
      as.integer(as.octmode("0777"))
    ),
    as.integer(as.octmode("0600"))
  )

  session$close()
  expect_false(dir.exists(private_dir))
  expect_true(file.exists(host_config))
  expect_true(file.exists(upload))
})

test_that("the session validates scalar research limits", {
  session <- TempestResearcherAppSession$new(config = tempest_config())
  on.exit(session$close(), add = TRUE)
  request <- function(...) {
    utils::modifyList(
      list(
        topic = "A bounded research question",
        model = "openai/gpt-6.1-sol",
        n_experts = 2L,
        max_rounds = 2L,
        max_questions = 3L,
        allow_code = FALSE,
        allow_images = FALSE,
        max_analyses = 3L,
        max_illustrations = 2L,
        selected_tools = character(),
        mcp_servers = character(),
        selected_mcp = character()
      ),
      list(...)
    )
  }

  expect_error(
    session$start(request(max_questions = 0L)),
    class = "tempest_researcher_app_input_error"
  )
  expect_error(
    session$start(request(model = " ")),
    class = "tempest_researcher_app_input_error"
  )
  expect_error(
    session$start(request(allow_code = NA)),
    class = "tempest_researcher_app_input_error"
  )
  expect_error(
    session$start(request(research_strategy = "unknown")),
    class = "tempest_researcher_app_input_error"
  )
})

test_that("MCP configuration changes only while the worker is inactive", {
  config_path <- tempfile(fileext = ".json")
  writeLines('{"mcpServers":{"fixture":{}}}', config_path)
  local_mocked_bindings(
    tempest_researcher_mcp_servers = function(path) "fixture",
    tempest_researcher_app_spawn = function(func, args, libpath) {
      state <- new.env(parent = emptyenv())
      state$alive <- TRUE
      list(
        is_alive = function() state$alive,
        kill_tree = function() {
          state$alive <- FALSE
          integer()
        },
        wait = function(timeout) invisible(NULL),
        kill = function() {
          state$alive <- FALSE
          invisible(NULL)
        }
      )
    }
  )
  session <- TempestResearcherAppSession$new(config = tempest_config())
  on.exit(session$close(), add = TRUE)

  session$discover("fixture")
  expect_error(
    session$set_mcp_config(config_path),
    class = "tempest_researcher_app_state_error"
  )
  expect_identical(session$cancel()$status, "cancelled")
  expect_identical(session$set_mcp_config(config_path), "fixture")
})

test_that("research workers reuse selected tools and preserve host config", {
  events <- new.env(parent = emptyenv())
  config <- tempest_config(
    models = list(
      coordinator = "openai/old-coordinator",
      writer = "openai/old-writer",
      expert = "openai/old-expert"
    ),
    params = list(temperature = 0.2),
    chat_fn = function(...) NULL
  )
  request <- list(
    topic = "A bounded research question",
    model = "openai/gpt-6.1-sol",
    n_experts = 2L,
    max_rounds = 2L,
    max_questions = 3L,
    allow_code = FALSE,
    allow_images = FALSE,
    max_analyses = 3L,
    max_illustrations = 2L,
    selected_tools = character(),
    mcp_servers = character(),
    selected_mcp = character()
  )
  output_dir <- file.path(tempdir(), paste0("report-", tempfile()))
  local_mocked_bindings(
    tempest_researcher_selected_tools = function(...) {
      events$selected_calls <- (events$selected_calls %||% 0L) + 1L
      events$selection_args <- list(...)
      list(fixture_tool = "tool definition")
    },
    tempest_researcher_tools_config = function(config, tools) {
      events$config <- config
      events$tools <- tools
      config
    },
    tempest_researcher_app_run_research = function(...) {
      events$run <- list(...)
      list(
        report = TempestEnrichedReport(
          path = output_dir,
          status = "complete",
          manifest = list(status = "complete")
        )
      )
    }
  )

  result <- tempest_researcher_worker(
    kind = "research",
    request = request,
    config = config,
    tools = list(fixture_tool = function() "factory"),
    mcp_config = NULL,
    output_dir = output_dir,
    progress_path = tempfile(fileext = ".jsonl")
  )

  expect_identical(events$selected_calls, 1L)
  expect_identical(events$selection_args$selected_tools, character())
  expect_identical(
    events$config@models,
    list(
      coordinator = "openai/gpt-6.1-sol",
      writer = "openai/gpt-6.1-sol",
      expert = "openai/gpt-6.1-sol"
    )
  )
  expect_identical(events$config@params, list(temperature = 0.2))
  expect_identical(events$config@chat_fn, config@chat_fn)
  expect_identical(config@models[["coordinator"]], "openai/old-coordinator")
  expect_identical(events$run$enrichment$allow_code, FALSE)
  expect_identical(events$run$research_strategy, "key_questions")
  expect_identical(events$run$enrichment$max_analyses, 0L)
  expect_identical(events$run$enrichment$max_illustrations, 0L)
  expect_null(events$run$enrichment$executor)
  expect_null(events$run$enrichment$image_generator)
  expect_identical(result, list(path = output_dir, status = "complete"))
})

test_that("a fixed chat clone follows the selected model without mutation", {
  chat <- ellmer::chat_openai(
    model = "gpt-4o-mini",
    credentials = function() "offline-only"
  )
  withr::local_options(tempest.chat = chat)
  config <- tempest_config(params = list(temperature = 0.2))

  changed <- tempest_researcher_app_model_config(
    config,
    "openai/gpt-6.1-sol"
  )

  expect_identical(changed@chat$get_model(), "gpt-6.1-sol")
  expect_identical(config@chat$get_model(), "gpt-4o-mini")
  expect_identical(changed@params, list(temperature = 0.2))
  expect_error(
    tempest_researcher_app_model_config(
      config,
      "anthropic/claude-sonnet-4-20250514"
    ),
    class = "tempest_researcher_app_config_error"
  )
})

test_that("Google Chat accepts its provider-prefixed model selection", {
  chat <- ellmer::chat_google_gemini(
    model = "gemini-2.5-flash",
    credentials = function() "offline-only"
  )
  withr::local_options(tempest.chat = chat)
  config <- tempest_config()

  changed <- tempest_researcher_app_model_config(
    config,
    "google/gemini-2.5-pro"
  )

  expect_identical(changed@chat$get_model(), "gemini-2.5-pro")
  expect_identical(config@chat$get_model(), "gemini-2.5-flash")
})

test_that("progress is capped, structured, and redacts credential-like text", {
  path <- tempfile(fileext = ".jsonl")
  for (index in seq_len(130L)) {
    tempest_researcher_app_write_progress(
      path,
      list(
        event_type = "stage",
        stage = paste0("stage-", index),
        status = "running",
        message = "Authorization: Bearer progress-secret"
      )
    )
  }

  events <- tempest_researcher_app_read_progress(path)
  expect_length(events, 100L)
  expect_identical(events[[100L]]$stage, "stage-130")
  expect_no_match(paste(readLines(path), collapse = "\n"), "progress-secret")
  expect_match(events[[100L]]$message, "redacted", ignore.case = TRUE)
})

test_that("completion and cancellation reap worker descendants", {
  skip_on_os("windows")
  directory <- withr::local_tempdir()

  run_tree_case <- function(outcome) {
    pid_path <- file.path(directory, paste0(outcome, ".pid"))
    local_mocked_bindings(tempest_researcher_app_spawn = function(
      func,
      args,
      libpath
    ) {
      stage_path <- if (!is.null(args$work$output_dir)) {
        file.path(dirname(args$work$output_dir), "orphan-staging")
      } else {
        ""
      }
      fake_worker <- function(result_path, pid_path, outcome, stage_path) {
        child <- processx::process$new(
          command = file.path(R.home("bin"), "Rscript"),
          args = c("-e", "Sys.sleep(60)"),
          cleanup = FALSE,
          stdout = base::nullfile(),
          stderr = base::nullfile()
        )
        writeLines(as.character(child$get_pid()), pid_path)
        if (nzchar(stage_path)) {
          dir.create(stage_path)
          writeLines("orphan", file.path(stage_path, "index.html"))
        }
        if (identical(outcome, "complete")) {
          saveRDS(
            list(
              ok = TRUE,
              result = data.frame(
                id = "fixture",
                name = "sample",
                description = "A local fixture",
                server = "fixture",
                available = TRUE,
                reason = NA_character_
              )
            ),
            result_path
          )
        }
        if (identical(outcome, "error")) {
          saveRDS(
            list(
              ok = FALSE,
              error_class = "tempest_researcher_app_tools_error",
              error_message = "Authorization: Bearer worker-secret"
            ),
            result_path
          )
        }
        repeat {
          Sys.sleep(0.05)
        }
      }
      environment(fake_worker) <- baseenv()
      callr::r_bg(
        fake_worker,
        args = list(
          result_path = args$result_path,
          pid_path = pid_path,
          outcome = outcome,
          stage_path = stage_path
        ),
        libpath = libpath,
        supervise = TRUE,
        cleanup_tree = TRUE,
        stdout = base::nullfile(),
        stderr = base::nullfile()
      )
    })
    session <- TempestResearcherAppSession$new(
      config = tempest_config(),
      output_dir = directory
    )
    on.exit(session$close(), add = TRUE)
    if (identical(outcome, "cancel")) {
      session$start(list(
        topic = "A bounded research question",
        model = "openai/gpt-6.1-sol",
        n_experts = 2L,
        max_rounds = 2L,
        max_questions = 3L,
        allow_code = FALSE,
        allow_images = FALSE,
        max_analyses = 3L,
        max_illustrations = 2L,
        selected_tools = character(),
        mcp_servers = character(),
        selected_mcp = character()
      ))
    } else {
      session$discover("fixture")
    }
    for (attempt in seq_len(100L)) {
      if (file.exists(pid_path)) {
        break
      }
      Sys.sleep(0.05)
    }
    expect_true(file.exists(pid_path))
    child_pid <- readLines(pid_path, warn = FALSE)[[1L]]
    if (identical(outcome, "cancel")) {
      job_dir <- session$.__enclos_env__$private$job_dir
      stage_path <- file.path(job_dir, "orphan-staging")
      expect_true(dir.exists(stage_path))
      value <- session$cancel()
      expect_identical(value$status, "cancelled")
      expect_null(value$result)
      expect_identical(dir.exists(job_dir), FALSE)
      expect_identical(dir.exists(stage_path), FALSE)
    } else {
      value <- NULL
      for (attempt in seq_len(100L)) {
        value <- session$poll()
        if (!identical(value$status, "running")) {
          break
        }
        Sys.sleep(0.05)
      }
      if (identical(outcome, "complete")) {
        expect_identical(value$status, "complete")
        expect_identical(value$result$name, "sample")
      } else {
        expect_identical(value$status, "error")
        expect_match(value$error, "redacted", ignore.case = TRUE)
        expect_no_match(value$error, "worker-secret")
      }
    }
    alive <- TRUE
    for (attempt in seq_len(100L)) {
      status <- suppressWarnings(system2(
        "kill",
        c("-0", child_pid),
        stdout = FALSE,
        stderr = FALSE
      ))
      if (!identical(status, 0L)) {
        alive <- FALSE
        break
      }
      Sys.sleep(0.05)
    }
    expect_identical(alive, FALSE)
  }

  run_tree_case("complete")
  run_tree_case("error")
  run_tree_case("cancel")
})

test_that("research workers receive private Ragnar store copies", {
  skip_if_not_installed("ragnar")
  extension_connection <- DBI::dbConnect(duckdb::duckdb())
  on.exit(
    DBI::dbDisconnect(extension_connection, shutdown = TRUE),
    add = TRUE
  )
  extensions_loaded <- !inherits(
    try(
      DBI::dbExecute(extension_connection, "LOAD fts; LOAD vss;"),
      silent = TRUE
    ),
    "try-error"
  )
  skip_if_not(extensions_loaded, "DuckDB fts/vss extensions are unavailable")

  request <- list(
    topic = "A bounded research question",
    model = "openai/gpt-6.1-sol",
    n_experts = 2L,
    max_rounds = 2L,
    max_questions = 3L,
    allow_code = FALSE,
    allow_images = FALSE,
    max_analyses = 0L,
    max_illustrations = 0L,
    selected_tools = character(),
    mcp_servers = character(),
    selected_mcp = character()
  )
  capture <- new.env(parent = emptyenv())

  run_worker <- function(config, store) {
    ragnar::ragnar_store_build_index(store)
    host_embedding <- DBI::dbGetQuery(
      store@con,
      paste(
        "SELECT array_to_string(embedding, '|') AS value",
        "FROM embeddings ORDER BY chunk_id LIMIT 1"
      )
    )$value[[1L]]

    local_mocked_bindings(tempest_researcher_app_spawn = function(
      func,
      args,
      libpath
    ) {
      capture$args <- args
      state <- new.env(parent = emptyenv())
      state$alive <- FALSE
      list(
        is_alive = function() state$alive,
        kill_tree = function() {
          state$alive <- FALSE
          integer()
        },
        wait = function(timeout) invisible(NULL),
        kill = function() {
          state$alive <- FALSE
          invisible(NULL)
        }
      )
    })

    session <- TempestResearcherAppSession$new(
      config = config,
      output_dir = withr::local_tempdir()
    )
    on.exit(session$close(), add = TRUE)
    started <- session$start(request)
    expect_identical(started$status, "running")

    args <- capture$args
    work <- args$work
    expect_null(work$config@ragnar_store)
    expect_null(work$config@embed_fn)
    expect_identical(
      file.exists(file.path(work$ragnar_handoff, "schema.sql")),
      TRUE
    )
    expect_identical(
      startsWith(
        work$ragnar_handoff,
        session$.__enclos_env__$private$job_dir
      ),
      TRUE
    )

    job_dir <- session$.__enclos_env__$private$job_dir
    child <- callr::r(
      function(package_path, libpath, work, result_path, release_path) {
        .libPaths(unique(c(libpath, .libPaths())))
        if (!is.null(package_path)) {
          pkgload::load_all(package_path, quiet = TRUE)
        } else {
          loadNamespace("tempest")
        }
        namespace <- asNamespace("tempest")
        observed <- new.env(parent = emptyenv())
        worker <- function(
          kind,
          request,
          config,
          tools,
          mcp_config,
          output_dir,
          progress_path
        ) {
          tryCatch(
            {
              observed$store <- config@ragnar_store
              retriever <- get("tempest_retriever", namespace)(
                config = config
              )
              hits <- retriever$retrieve("historic archive traceable", k = 5L)
              before <- DBI::dbGetQuery(
                config@ragnar_store@con,
                "SELECT COUNT(*) AS n FROM documents"
              )$n[[1L]] |>
                as.integer()
              embedding <- DBI::dbGetQuery(
                config@ragnar_store@con,
                paste(
                  "SELECT array_to_string(embedding, '|') AS value",
                  "FROM embeddings ORDER BY chunk_id LIMIT 1"
                )
              )$value[[1L]]
              retriever$ingest_to_ragnar(
                source_id = "Sworkeronly1234",
                url = "https://example.test/worker-only",
                title = "Worker copy only",
                text = "Worker copy only evidence.",
                fetched_at = "2026-11-16T00:00:00Z",
                content_type = "text",
                perspective = "worker"
              )
              after <- DBI::dbGetQuery(
                config@ragnar_store@con,
                "SELECT COUNT(*) AS n FROM documents"
              )$n[[1L]] |>
                as.integer()
              list(
                existing_text = as.character(hits$text),
                rows_before = before,
                rows_after = after,
                embedding = embedding
              )
            },
            error = function(error) {
              rlang::abort(
                conditionMessage(error),
                class = "tempest_researcher_app_config_error"
              )
            }
          )
        }
        unlockBinding("tempest_researcher_worker", namespace)
        assign("tempest_researcher_worker", worker, envir = namespace)
        lockBinding("tempest_researcher_worker", namespace)

        file.create(release_path)
        entry <- get("tempest_researcher_app_callr_entry", namespace)
        environment(entry) <- baseenv()
        entry(
          package_path = NULL,
          libpath = .libPaths(),
          work = work,
          result_path = result_path,
          release_path = release_path
        )
        payload <- readRDS(result_path)
        closed <- !DBI::dbIsValid(observed$store@con)
        reopened <- ragnar::ragnar_store_connect(
          file.path(dirname(work$ragnar_handoff), "ragnar-worker.duckdb"),
          read_only = FALSE
        )
        on.exit(
          DBI::dbDisconnect(reopened@con, shutdown = TRUE),
          add = TRUE
        )
        origins <- DBI::dbGetQuery(
          reopened@con,
          "SELECT origin FROM documents ORDER BY doc_id"
        )$origin
        list(payload = payload, closed = closed, origins = origins)
      },
      args = list(
        package_path = args$package_path,
        libpath = args$libpath,
        work = work,
        result_path = file.path(job_dir, "worker-result.rds"),
        release_path = file.path(job_dir, "worker-release")
      ),
      libpath = args$libpath
    )

    host_rows <- DBI::dbGetQuery(
      store@con,
      "SELECT COUNT(*) AS n FROM documents"
    )$n[[1L]]
    expect_identical(DBI::dbIsValid(store@con), TRUE)
    expect_identical(as.integer(host_rows), 1L)
    expect_identical(
      child$payload$ok,
      TRUE,
      info = paste(
        child$payload$error_class %||% "",
        child$payload$error_message %||% ""
      )
    )
    expect_match(
      paste(child$payload$result$existing_text, collapse = " "),
      "Historic archive",
      ignore.case = TRUE
    )
    expect_identical(child$payload$result$rows_before, 1L)
    expect_identical(child$payload$result$rows_after, 2L)
    expect_identical(child$payload$result$embedding, host_embedding)
    expect_identical(child$closed, TRUE)
    expect_length(child$origins, 2L)
    expect_identical(
      any(grepl("worker-only", child$origins, fixed = TRUE)),
      TRUE
    )
    session$close()
    expect_identical(dir.exists(job_dir), FALSE)
    child
  }

  embed_state <- new.env(parent = emptyenv())
  embed <- local({
    keep_alive <- embed_state
    function(x) {
      matrix(as.numeric(nchar(x)), nrow = length(x), ncol = 1L)
    }
  })

  auto_config <- tempest_config(embed_fn = embed)
  auto_store <- auto_config@ragnar_store
  embed_state$store <- auto_store
  on.exit(DBI::dbDisconnect(auto_store@con, shutdown = TRUE), add = TRUE)
  auto_retriever <- tempest_retriever(config = auto_config)
  auto_retriever$ingest_to_ragnar(
    source_id = "Sauto123456789",
    url = "https://example.test/historic-auto",
    title = "Historic archive",
    text = "Historic archive reports traceable evidence.",
    fetched_at = "2026-11-16T00:00:00Z",
    content_type = "text",
    perspective = "fixture"
  )
  run_worker(auto_config, auto_store)

  supplied_store <- tempest_create_ragnar_store(
    embed,
    cache_dir = withr::local_tempdir()
  )
  on.exit(DBI::dbDisconnect(supplied_store@con, shutdown = TRUE), add = TRUE)
  supplied_retriever <- tempest_retriever(
    config = tempest_config(ragnar_store = supplied_store)
  )
  supplied_retriever$ingest_to_ragnar(
    source_id = "Ssupplied123456",
    url = "https://example.test/historic-supplied",
    title = "Historic archive",
    text = "Historic archive reports traceable evidence.",
    fetched_at = "2026-11-16T00:00:00Z",
    content_type = "text",
    perspective = "fixture"
  )
  run_worker(
    tempest_config(ragnar_store = supplied_store),
    supplied_store
  )
})

test_that("snapshot failures fail launch and clean private job data", {
  capture <- new.env(parent = emptyenv())
  local_mocked_bindings(tempest_researcher_app_snapshot_ragnar_store = function(
    config,
    job_dir
  ) {
    capture$job_dir <- job_dir
    dir.create(file.path(job_dir, "ragnar-export"))
    writeLines("partial", file.path(job_dir, "ragnar-export", "partial.csv"))
    stop("Bearer snapshot-secret")
  })
  session <- TempestResearcherAppSession$new(config = tempest_config())
  on.exit(session$close(), add = TRUE)

  result <- session$start(list(
    topic = "A bounded research question",
    model = "openai/gpt-6.1-sol",
    n_experts = 2L,
    max_rounds = 2L,
    max_questions = 3L,
    allow_code = FALSE,
    allow_images = FALSE,
    max_analyses = 0L,
    max_illustrations = 0L,
    selected_tools = character(),
    mcp_servers = character(),
    selected_mcp = character()
  ))

  expect_identical(result$status, "error")
  expect_identical(
    result$error,
    "The configured Ragnar store could not be prepared for the research worker."
  )
  expect_no_match(result$error, "snapshot-secret")
  expect_null(session$.__enclos_env__$private$job_dir)
  expect_identical(dir.exists(capture$job_dir), FALSE)
})
