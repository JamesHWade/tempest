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
