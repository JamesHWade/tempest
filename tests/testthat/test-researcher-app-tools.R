test_that("the local tool catalog validates factories without invoking them", {
  make_tool <- function(name) {
    ellmer::tool(
      function() "ok",
      name = name,
      description = paste("Tool", name),
      arguments = list(),
      annotations = ellmer::tool_annotations(
        read_only_hint = TRUE,
        open_world_hint = TRUE
      )
    )
  }
  calls <- 0L
  tools <- list(search = function() {
    calls <<- calls + 1L
    list(make_tool("search_literature"))
  })

  catalog <- tempest_researcher_tool_catalog(tools)

  expect_identical(catalog$id, "search")
  expect_identical(catalog$label, "search")
  expect_identical(calls, 0L)
  expect_error(
    tempest_researcher_tool_catalog(list(bad = function(x) x)),
    class = "tempest_researcher_app_tools_error"
  )
  expect_error(
    tempest_researcher_tool_catalog(list(bad = function(...) list())),
    class = "tempest_researcher_app_tools_error"
  )
})

test_that("MCP configuration parsing returns names and hides contents", {
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)
  writeLines(
    paste0(
      '{"mcpServers":{"lab A":{"command":"fake",',
      '"env":{"TOKEN":"private-value"}},"other":{"url":"https://example.test"}}}'
    ),
    path
  )

  expect_identical(
    tempest_researcher_mcp_servers(path),
    c("lab A", "other")
  )
  expect_identical(tempest_researcher_mcp_servers(NULL), character())

  writeLines('{"mcpServers":[]}', path)
  expect_error(
    tempest_researcher_mcp_servers(path),
    class = "tempest_researcher_app_tools_error"
  )
  writeLines('{"mcpServers":{"lab A":{"env":{"TOKEN":"private-value"}}}', path)
  error <- tryCatch(
    tempest_researcher_mcp_servers(path),
    error = identity
  )
  expect_s3_class(error, "tempest_researcher_app_tools_error")
  expect_no_match(conditionMessage(error), "private-value", fixed = TRUE)
})

test_that("MCP discovery preserves origin and reports policy eligibility", {
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)
  writeLines(
    '{"mcpServers":{"lab A":{"command":"fake"},"other":{"url":"https://example.test"}}}',
    path
  )
  make_tool <- function(
    name,
    annotations = ellmer::tool_annotations(
      read_only_hint = TRUE,
      open_world_hint = TRUE
    )
  ) {
    ellmer::tool(
      function() "ok",
      name = name,
      description = paste("Tool", name),
      arguments = list(),
      annotations = annotations
    )
  }
  make_mcp_tool <- function(
    name,
    server,
    source_name = name,
    annotations = ellmer::tool_annotations(
      read_only_hint = TRUE,
      open_world_hint = TRUE
    )
  ) {
    tool <- make_tool(name, annotations)
    attr(tool, "deputy_tool_source") <- list(
      type = "mcp",
      server = server,
      tool = source_name
    )
    tool
  }
  calls <- new.env(parent = emptyenv())
  calls$count <- 0L
  local_mocked_bindings(
    tempest_researcher_mcptools_version = function() "1.0.3",
    tempest_researcher_call_tools_mcp = function(config, servers) {
      calls$count <- calls$count + 1L
      calls$config <- config
      calls$servers <- servers
      list(
        make_mcp_tool(
          "search_literature",
          "lab A",
          source_name = "library_search"
        ),
        make_mcp_tool(
          "write_record",
          "other",
          annotations = ellmer::tool_annotations(destructive_hint = TRUE)
        ),
        make_mcp_tool("read_file", "lab A")
      )
    }
  )

  catalog <- tempest_researcher_mcp_discover(path, c("lab A", "other"))

  expect_identical(calls$count, 1L)
  expect_identical(calls$config, path)
  expect_identical(calls$servers, c("lab A", "other"))
  expect_identical(
    names(catalog),
    c("id", "name", "description", "server", "available", "reason")
  )
  expect_identical(catalog$id[[1]], "mcp:lab%20A/library_search")
  expect_identical(catalog$name[[1]], "search_literature")
  expect_identical(catalog$server, c("lab A", "other", "lab A"))
  expect_identical(catalog$available, c(TRUE, FALSE, TRUE))
  expect_match(catalog$reason[[2]], "research policy", ignore.case = TRUE)
  expect_false(any(vapply(catalog, is.function, logical(1))))
})

test_that("MCP discovery sanitizes loader warnings and rejects unsupported versions", {
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)
  writeLines('{"mcpServers":{"lab A":{"command":"fake"}}}', path)
  state <- new.env(parent = emptyenv())
  state$version <- "1.0.3"
  state$warn <- TRUE
  state$error <- FALSE
  local_mocked_bindings(
    tempest_researcher_mcptools_version = function() state$version,
    tempest_researcher_call_tools_mcp = function(config, servers) {
      if (state$error) {
        stop("Authorization: Bearer private-value")
      }
      if (state$warn) {
        warning("Authorization: Bearer private-value")
      }
      list()
    }
  )
  warning_error <- tryCatch(
    tempest_researcher_mcp_discover(path, "lab A"),
    error = identity
  )
  expect_s3_class(warning_error, "tempest_researcher_app_tools_error")
  expect_no_match(
    conditionMessage(warning_error),
    "private-value",
    fixed = TRUE
  )

  state$warn <- FALSE
  state$error <- TRUE
  load_error <- tryCatch(
    tempest_researcher_mcp_discover(path, "lab A"),
    error = identity
  )
  expect_s3_class(load_error, "tempest_researcher_app_tools_error")
  expect_no_match(conditionMessage(load_error), "private-value", fixed = TRUE)

  state$error <- FALSE
  state$warn <- FALSE
  state$version <- "1.0.1"
  expect_error(
    tempest_researcher_mcp_discover(path, "lab A"),
    class = "tempest_researcher_app_tools_error"
  )
})

test_that("the worker invokes only selected local and MCP tools once", {
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)
  writeLines('{"mcpServers":{"lab A":{"command":"fake"}}}', path)
  make_tool <- function(name) {
    ellmer::tool(
      function() "ok",
      name = name,
      description = paste("Tool", name),
      arguments = list(),
      annotations = ellmer::tool_annotations(
        read_only_hint = TRUE,
        open_world_hint = TRUE
      )
    )
  }
  make_mcp_tool <- function(name, server) {
    tool <- make_tool(name)
    attr(tool, "deputy_tool_source") <- list(
      type = "mcp",
      server = server,
      tool = name
    )
    tool
  }
  calls <- new.env(parent = emptyenv())
  calls$selected <- 0L
  calls$unused <- 0L
  calls$mcp <- 0L
  tools <- list(
    selected = function() {
      calls$selected <- calls$selected + 1L
      list(make_tool("local_search"), make_tool("local_lookup"))
    },
    unused = function() {
      calls$unused <- calls$unused + 1L
      list(make_tool("unused_tool"))
    }
  )
  local_mocked_bindings(
    tempest_researcher_mcptools_version = function() "1.0.3",
    tempest_researcher_call_tools_mcp = function(config, servers) {
      calls$mcp <- calls$mcp + 1L
      expect_identical(servers, "lab A")
      list(make_mcp_tool("search_literature", "lab A"))
    }
  )
  mcp_id <- "mcp:lab%20A/search_literature"

  selected <- tempest_researcher_selected_tools(
    tools,
    selected_tools = "selected",
    mcp_config = path,
    mcp_servers = "lab A",
    selected_mcp = mcp_id
  )

  expect_identical(calls$selected, 1L)
  expect_identical(calls$unused, 0L)
  expect_identical(calls$mcp, 1L)
  expect_identical(
    vapply(selected, function(tool) tool@name, character(1)),
    c("local_search", "local_lookup", "search_literature")
  )
  expect_error(
    tempest_researcher_selected_tools(
      tools,
      selected_tools = character(),
      mcp_config = path,
      mcp_servers = "lab A",
      selected_mcp = c(mcp_id, "forged-id")
    ),
    class = "tempest_researcher_app_tools_error"
  )
  expect_identical(calls$mcp, 2L)
})

test_that("empty selections never invoke factories or connect to MCP", {
  calls <- new.env(parent = emptyenv())
  calls$factory <- 0L
  calls$mcp <- 0L
  tools <- list(selected = function() {
    calls$factory <- calls$factory + 1L
    list()
  })
  local_mocked_bindings(
    tempest_researcher_mcptools_version = function() stop("must not check"),
    tempest_researcher_call_tools_mcp = function(config, servers) {
      calls$mcp <- calls$mcp + 1L
      stop("must not connect")
    }
  )

  selected <- tempest_researcher_selected_tools(
    tools,
    selected_tools = character(),
    mcp_servers = "unused-server",
    selected_mcp = character()
  )

  expect_length(selected, 0L)
  expect_identical(calls$factory, 0L)
  expect_identical(calls$mcp, 0L)
})

test_that("the worker rejects unavailable, duplicate, native, and reserved tools", {
  make_tool <- function(
    name,
    annotations = ellmer::tool_annotations(
      read_only_hint = TRUE,
      open_world_hint = TRUE
    )
  ) {
    ellmer::tool(
      function() "ok",
      name = name,
      description = paste("Tool", name),
      arguments = list(),
      annotations = annotations
    )
  }
  read_only <- list(read = function() {
    list(make_tool("read_only_tool"))
  })
  expect_length(
    tempest_researcher_selected_tools(read_only, "read"),
    1L
  )
  unsafe <- list(unsafe = function() {
    list(make_tool(
      "unsafe_tool",
      ellmer::tool_annotations(destructive_hint = TRUE)
    ))
  })
  expect_error(
    tempest_researcher_selected_tools(unsafe, "unsafe"),
    class = "tempest_researcher_app_tools_error"
  )

  duplicate <- list(
    first = function() list(make_tool("same_name")),
    second = function() list(make_tool("same_name"))
  )
  expect_error(
    tempest_researcher_selected_tools(duplicate, c("first", "second")),
    class = "tempest_researcher_app_tools_error"
  )

  native <- list(native = function() list(ellmer::openai_tool_web_search()))
  expect_error(
    tempest_researcher_selected_tools(native, "native"),
    class = "tempest_researcher_app_tools_error"
  )

  reserved <- list(reserved = function() {
    list(make_tool("add_proposed_claim"))
  })
  expect_error(
    tempest_researcher_selected_tools(reserved, "reserved"),
    class = "tempest_researcher_app_tools_error"
  )
})

test_that("external tools attach only to expert chats and preserve config", {
  make_tool <- function(name) {
    ellmer::tool(
      function() "ok",
      name = name,
      description = paste("Tool", name),
      arguments = list(),
      annotations = ellmer::tool_annotations(
        read_only_hint = TRUE,
        open_world_hint = TRUE
      )
    )
  }
  make_chat <- function(initial_tools = list()) {
    chat <- new.env(parent = emptyenv())
    chat$registered <- initial_tools
    chat$get_tools <- function() chat$registered
    chat$register_tools <- function(tools) {
      chat$registered <- c(chat$registered, tools)
      invisible(chat)
    }
    chat
  }
  calls <- list()
  chat_fn <- function(role, model, system_prompt, echo) {
    calls[[length(calls) + 1L]] <<- list(
      role = role,
      model = model,
      system_prompt = system_prompt,
      echo = echo
    )
    make_chat()
  }
  config <- tempest_config(
    models = list(expert = "openai/test-model"),
    chat_fn = chat_fn,
    params = list(temperature = 0.2)
  )
  tools <- list(make_tool("external_search"))

  wrapped <- tempest_researcher_tools_config(config, tools)
  expert <- tempest_make_chat(wrapped, "expert", echo = "none")
  coordinator <- tempest_make_chat(wrapped, "coordinator", echo = "none")

  expect_identical(wrapped@params, config@params)
  expect_identical(wrapped@models, config@models)
  expect_identical(config@chat_fn, chat_fn)
  expect_identical(length(expert$registered), 1L)
  expect_identical(length(coordinator$registered), 0L)
  expect_identical(calls[[1]]$role, "expert")
  expect_identical(calls[[1]]$model, "openai/test-model")
  expect_identical(calls[[2]]$role, "coordinator")
})

test_that("tool registration rejects existing names and hides chat errors", {
  make_tool <- function(name) {
    ellmer::tool(
      function() "ok",
      name = name,
      description = paste("Tool", name),
      arguments = list(),
      annotations = ellmer::tool_annotations(
        read_only_hint = TRUE,
        open_world_hint = TRUE
      )
    )
  }
  make_chat <- function(initial_tools = list()) {
    chat <- new.env(parent = emptyenv())
    chat$registered <- initial_tools
    chat$get_tools <- function() chat$registered
    chat$register_tools <- function(tools) {
      chat$registered <- c(chat$registered, tools)
      invisible(chat)
    }
    chat
  }
  duplicated_chat_fn <- function(role, model, system_prompt, echo) {
    make_chat(list(make_tool("external_search")))
  }
  config <- tempest_config(
    models = list(expert = "openai/test-model"),
    chat_fn = duplicated_chat_fn
  )
  wrapped <- tempest_researcher_tools_config(
    config,
    list(make_tool("external_search"))
  )
  expect_error(
    tempest_make_chat(wrapped, "expert"),
    class = "tempest_chat_error"
  )

  failing_config <- tempest_config(
    models = list(expert = "openai/test-model"),
    chat_fn = function(...) stop("Authorization: Bearer private-value")
  )
  failing_wrapped <- tempest_researcher_tools_config(
    failing_config,
    list(make_tool("another_tool"))
  )
  error <- tryCatch(
    tempest_make_chat(failing_wrapped, "expert"),
    error = identity
  )
  expect_s3_class(error, "tempest_chat_error")
  expect_no_match(conditionMessage(error), "private-value", fixed = TRUE)
})
