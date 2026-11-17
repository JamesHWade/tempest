test_that("selected MCP tools execute through an isolated real stdio client", {
  skip_if_not_installed("callr")
  skip_if_not_installed("mcptools")
  skip_if_not_installed("pkgload")
  skip_if_not(
    as.character(utils::packageVersion("mcptools")) %in% c("1.0.2", "1.0.3")
  )
  directory <- withr::local_tempdir()
  config <- file.path(directory, "mcp.json")
  selected_pid <- file.path(directory, "selected.pid")
  excluded_pid <- file.path(directory, "excluded.pid")
  producer <- normalizePath(test_path("fixtures", "researcher-mcp.R"))
  jsonlite::write_json(
    list(
      mcpServers = list(
        chosen = list(
          command = file.path(R.home("bin"), "Rscript"),
          args = c(producer, selected_pid)
        ),
        excluded = list(
          command = file.path(R.home("bin"), "Rscript"),
          args = c(producer, excluded_pid)
        )
      )
    ),
    config,
    auto_unbox = TRUE
  )
  worker <- callr::r_session$new(
    options = callr::r_session_options(
      libpath = .libPaths(),
      user_profile = FALSE
    )
  )
  on.exit(
    {
      worker$kill_tree()
      worker$wait(timeout = 2000)
    },
    add = TRUE
  )
  result <- worker$run(
    function(root, config) {
      if (!is.null(root)) {
        pkgload::load_all(root, quiet = TRUE)
      }
      tools <- tempest:::tempest_researcher_selected_tools(
        list(),
        character(),
        config,
        mcp_servers = "chosen",
        selected_mcp = "mcp:chosen/read_sample"
      )
      list(
        value = tools[[1L]](query = "offline query"),
        source = deputy::tool_metadata(tools[[1L]])$source,
        count = length(tools)
      )
    },
    args = list(root = tempest_researcher_app_package_path(), config = config)
  )
  expect_identical(result$count, 1L)
  expect_identical(result$source$server, "chosen")
  expect_match(
    paste(capture.output(str(result$value)), collapse = " "),
    "Observed fixture: offline query",
    fixed = TRUE
  )
  expect_identical(file.exists(selected_pid), TRUE)
  expect_identical(file.exists(excluded_pid), FALSE)
  worker$kill_tree()
  worker$wait(timeout = 2000)
  expect_identical(worker$is_alive(), FALSE)
})
