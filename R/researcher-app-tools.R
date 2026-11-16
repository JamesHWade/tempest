# Optional external tools for the dedicated researcher application.

tempest_researcher_app_tools_abort <- function(message) {
  tempest_abort(
    message,
    class = c("tempest_researcher_app_tools_error", "tempest_error"),
    .envir = rlang::caller_env()
  )
}

tempest_researcher_app_tool_ids <- function(ids, arg) {
  if (is.null(ids)) {
    return(character())
  }
  if (
    !is.character(ids) ||
      anyNA(ids) ||
      any(!nzchar(ids)) ||
      !identical(ids, trimws(ids)) ||
      anyDuplicated(ids)
  ) {
    tempest_researcher_app_tools_abort(
      paste0("{.arg ", arg, "} must contain unique, non-empty exact IDs.")
    )
  }
  ids
}

tempest_researcher_tool_catalog <- function(tools) {
  if (is.null(tools)) {
    tools <- list()
  }
  if (!is.list(tools)) {
    tempest_researcher_app_tools_abort(
      "{.arg tools} must be a named list of trusted tool factories."
    )
  }
  if (length(tools) == 0L) {
    return(data.frame(id = character(), label = character()))
  }
  ids <- names(tools)
  if (
    is.null(ids) ||
      length(ids) != length(tools) ||
      anyNA(ids) ||
      any(!nzchar(ids)) ||
      any(ids != trimws(ids)) ||
      anyDuplicated(ids)
  ) {
    tempest_researcher_app_tools_abort(
      "{.arg tools} must have unique, non-empty names."
    )
  }
  is_factory <- vapply(
    tools,
    function(factory) {
      is.function(factory) &&
        !is.primitive(factory) &&
        length(formals(factory)) == 0L
    },
    logical(1)
  )
  if (!all(is_factory)) {
    tempest_researcher_app_tools_abort(
      "Each tool factory must be a zero-argument R function."
    )
  }
  data.frame(id = ids, label = ids, stringsAsFactors = FALSE)
}

tempest_researcher_mcp_servers <- function(config) {
  if (is.null(config)) {
    return(character())
  }
  if (
    !rlang::is_string(config) ||
      !nzchar(config) ||
      !identical(config, trimws(config)) ||
      !file.exists(config) ||
      dir.exists(config)
  ) {
    tempest_researcher_app_tools_abort(
      "{.arg config} must be a readable MCP JSON configuration file."
    )
  }
  contents <- tryCatch(
    readLines(config, warn = FALSE, encoding = "UTF-8"),
    error = function(error) NULL
  )
  if (is.null(contents)) {
    tempest_researcher_app_tools_abort(
      "The MCP configuration could not be read."
    )
  }
  parsed <- tryCatch(
    jsonlite::fromJSON(
      paste(contents, collapse = "\n"),
      simplifyVector = FALSE
    ),
    error = function(error) NULL
  )
  if (
    !is.list(parsed) ||
      is.null(names(parsed)) ||
      !"mcpServers" %in% names(parsed)
  ) {
    tempest_researcher_app_tools_abort(
      "The MCP configuration must contain an mcpServers object."
    )
  }
  servers <- parsed[["mcpServers"]]
  server_names <- names(servers)
  if (
    !is.list(servers) ||
      is.null(server_names) ||
      anyNA(server_names) ||
      any(!nzchar(server_names)) ||
      any(server_names != trimws(server_names)) ||
      anyDuplicated(server_names) ||
      !all(vapply(
        servers,
        function(server) is.list(server) && !is.null(names(server)),
        logical(1)
      ))
  ) {
    tempest_researcher_app_tools_abort(
      "The MCP configuration must contain a valid mcpServers object."
    )
  }
  server_names
}

tempest_researcher_mcptools_version <- function() {
  if (!requireNamespace("mcptools", quietly = TRUE)) {
    return(NA_character_)
  }
  as.character(utils::packageVersion("mcptools"))
}

tempest_researcher_call_tools_mcp <- function(config, servers) {
  deputy::tools_mcp(config = config, servers = servers)
}

tempest_researcher_tool_metadata <- function(tool) {
  deputy::tool_metadata(tool)
}

tempest_researcher_mcp_empty_catalog <- function() {
  data.frame(
    id = character(),
    name = character(),
    description = character(),
    server = character(),
    available = logical(),
    reason = character(),
    stringsAsFactors = FALSE
  )
}

tempest_researcher_mcp_tool_id <- function(server, name) {
  paste0(
    "mcp:",
    utils::URLencode(server, reserved = TRUE),
    "/",
    utils::URLencode(name, reserved = TRUE)
  )
}

tempest_researcher_reserved_tool_names <- function() {
  c(
    "get_retrieved_source",
    "list_retrieved_sources",
    "list_proposed_claims",
    "get_proposed_claim",
    "get_evidence_for_proposed_claim",
    "list_unsupported_proposed_claims",
    "add_proposed_claim",
    "web_search",
    "web_fetch",
    "fetch_url"
  )
}

tempest_researcher_tool_safety <- function(tool, metadata = NULL) {
  name <- tryCatch(tool@name, error = function(error) NULL)
  if (
    !inherits(tool, "ellmer::ToolDef") ||
      inherits(tool, "ellmer::ToolBuiltIn") ||
      !rlang::is_string(name) ||
      !nzchar(name)
  ) {
    return(list(
      available = FALSE,
      reason = "Only ordinary ellmer tool definitions are supported."
    ))
  }
  if (is.null(metadata)) {
    metadata <- tryCatch(
      tempest_researcher_tool_metadata(tool),
      error = function(error) NULL
    )
  }
  if (
    !is.list(metadata) ||
      !is.list(metadata$annotations) ||
      !is.list(metadata$effective_annotations)
  ) {
    return(list(
      available = FALSE,
      reason = "Deputy could not verify this tool's safety annotations."
    ))
  }
  policy <- tryCatch(
    tempest_deputy_adapter_permissions(name),
    error = function(error) NULL
  )
  decision <- if (is.null(policy)) {
    NULL
  } else {
    tryCatch(
      deputy::permissions_check(
        permissions = policy,
        tool_name = name,
        tool_input = list(),
        context = list(
          tool_annotations = metadata$annotations,
          tool_metadata = metadata
        )
      ),
      error = function(error) NULL
    )
  }
  if (is.null(decision) || !identical(decision@decision, "allow")) {
    annotations <- metadata$effective_annotations
    reason <- if (isTRUE(annotations$destructive_hint)) {
      "The tool may make changes, which the research policy does not allow."
    } else {
      "The tool does not satisfy the research tool policy."
    }
    return(list(available = FALSE, reason = reason))
  }
  list(available = TRUE, reason = NA_character_)
}

tempest_researcher_mcp_catalog <- function(config, servers) {
  if (is.null(servers)) {
    servers <- character()
  }
  if (
    !is.character(servers) ||
      anyNA(servers) ||
      any(!nzchar(servers)) ||
      any(servers != trimws(servers)) ||
      anyDuplicated(servers)
  ) {
    tempest_researcher_app_tools_abort(
      "{.arg servers} must contain unique, non-empty exact server names."
    )
  }
  if (length(servers) == 0L) {
    return(list(
      catalog = tempest_researcher_mcp_empty_catalog(),
      tools = list()
    ))
  }
  configured_servers <- tempest_researcher_mcp_servers(config)
  if (!all(servers %in% configured_servers)) {
    tempest_researcher_app_tools_abort(
      "Only servers in the supplied MCP configuration can be selected."
    )
  }
  version <- tryCatch(
    tempest_researcher_mcptools_version(),
    error = function(error) NA_character_
  )
  if (!version %in% c("1.0.2", "1.0.3")) {
    tempest_researcher_app_tools_abort(
      "MCP tools require mcptools version 1.0.2 or 1.0.3."
    )
  }
  warning_seen <- FALSE
  tools <- tryCatch(
    withCallingHandlers(
      tempest_researcher_call_tools_mcp(config, servers),
      warning = function(warning) {
        warning_seen <<- TRUE
        invokeRestart("muffleWarning")
      }
    ),
    error = function(error) NULL
  )
  if (warning_seen || is.null(tools) || !is.list(tools)) {
    tempest_researcher_app_tools_abort(
      "The selected MCP servers could not be loaded."
    )
  }
  if (length(tools) == 0L) {
    return(list(
      catalog = tempest_researcher_mcp_empty_catalog(),
      tools = list()
    ))
  }
  ids <- character(length(tools))
  names <- character(length(tools))
  descriptions <- character(length(tools))
  origins <- character(length(tools))
  available <- logical(length(tools))
  reasons <- character(length(tools))
  for (index in seq_along(tools)) {
    tool <- tools[[index]]
    if (
      !inherits(tool, "ellmer::ToolDef") ||
        inherits(tool, "ellmer::ToolBuiltIn")
    ) {
      tempest_researcher_app_tools_abort(
        "MCP discovery returned an unsupported tool definition."
      )
    }
    metadata <- tryCatch(
      tempest_researcher_tool_metadata(tool),
      error = function(error) NULL
    )
    source <- metadata$source %||% NULL
    server <- source$server %||% NULL
    source_name <- source$tool %||% NULL
    name <- tryCatch(tool@name, error = function(error) NULL)
    description <- tryCatch(tool@description, error = function(error) NULL)
    if (
      !identical(source$type, "mcp") ||
        !rlang::is_string(server) ||
        !server %in% servers ||
        !rlang::is_string(source_name) ||
        !rlang::is_string(name) ||
        !rlang::is_string(description)
    ) {
      tempest_researcher_app_tools_abort(
        "MCP discovery could not verify a tool's exact server origin."
      )
    }
    ids[[index]] <- tempest_researcher_mcp_tool_id(server, source_name)
    names[[index]] <- name
    descriptions[[index]] <- description
    origins[[index]] <- server
    safety <- tempest_researcher_tool_safety(tool, metadata)
    available[[index]] <- safety$available
    reasons[[index]] <- safety$reason
    if (name %in% tempest_researcher_reserved_tool_names()) {
      available[[index]] <- FALSE
      reasons[[index]] <- "This tool name is reserved for Tempest research."
    }
  }
  if (anyDuplicated(ids)) {
    tempest_researcher_app_tools_abort(
      "MCP discovery returned duplicate server and tool identities."
    )
  }
  names(tools) <- ids
  catalog <- data.frame(
    id = ids,
    name = names,
    description = descriptions,
    server = origins,
    available = available,
    reason = reasons,
    stringsAsFactors = FALSE
  )
  list(catalog = catalog, tools = tools)
}

tempest_researcher_mcp_discover <- function(config, servers) {
  tempest_researcher_mcp_catalog(config, servers)$catalog
}

tempest_researcher_checked_tool_names <- function(tools) {
  if (!is.list(tools)) {
    tempest_researcher_app_tools_abort(
      "Selected tools must be returned as a list."
    )
  }
  if (length(tools) == 0L) {
    return(character())
  }
  names <- vapply(
    tools,
    function(tool) {
      name <- tryCatch(tool@name, error = function(error) NULL)
      if (
        !inherits(tool, "ellmer::ToolDef") ||
          inherits(tool, "ellmer::ToolBuiltIn") ||
          !rlang::is_string(name) ||
          !nzchar(name)
      ) {
        tempest_researcher_app_tools_abort(
          "Selected tools must be ordinary ellmer tool definitions."
        )
      }
      name
    },
    character(1)
  )
  if (anyDuplicated(names)) {
    tempest_researcher_app_tools_abort(
      "Selected tools must have unique tool names."
    )
  }
  if (any(names %in% tempest_researcher_reserved_tool_names())) {
    tempest_researcher_app_tools_abort(
      "A selected tool uses a name reserved for Tempest research."
    )
  }
  for (tool in tools) {
    safety <- tempest_researcher_tool_safety(tool)
    if (!safety$available) {
      tempest_researcher_app_tools_abort(safety$reason)
    }
  }
  names
}

tempest_researcher_selected_tools <- function(
  tools,
  selected_tools,
  mcp_config = NULL,
  mcp_servers = character(),
  selected_mcp = character()
) {
  catalog <- tempest_researcher_tool_catalog(tools)
  selected_tools <- tempest_researcher_app_tool_ids(
    selected_tools,
    "selected_tools"
  )
  selected_mcp <- tempest_researcher_app_tool_ids(selected_mcp, "selected_mcp")
  missing_local <- setdiff(selected_tools, catalog$id)
  if (length(missing_local) > 0L) {
    tempest_researcher_app_tools_abort(
      "A selected local tool is not in the supplied tool catalog."
    )
  }
  mcp_loaded <- list(
    catalog = tempest_researcher_mcp_empty_catalog(),
    tools = list()
  )
  if (length(selected_mcp) > 0L) {
    if (length(mcp_servers) == 0L) {
      tempest_researcher_app_tools_abort(
        "Selected MCP tools require an explicit server allowlist."
      )
    }
    mcp_loaded <- tempest_researcher_mcp_catalog(mcp_config, mcp_servers)
    missing_mcp <- setdiff(selected_mcp, mcp_loaded$catalog$id)
    if (length(missing_mcp) > 0L) {
      tempest_researcher_app_tools_abort(
        "A selected MCP tool is not available from the selected servers."
      )
    }
    selected_catalog <- mcp_loaded$catalog[
      match(selected_mcp, mcp_loaded$catalog$id),
      ,
      drop = FALSE
    ]
    if (any(!selected_catalog$available)) {
      reason <- selected_catalog$reason[which(!selected_catalog$available)[[1]]]
      tempest_researcher_app_tools_abort(reason)
    }
  }
  selected_local_tools <- list()
  if (length(selected_tools) > 0L) {
    factory_results <- lapply(selected_tools, function(id) {
      factory <- tools[[match(id, catalog$id)]]
      warning_seen <- FALSE
      factory_tools <- tryCatch(
        withCallingHandlers(
          factory(),
          warning = function(warning) {
            warning_seen <<- TRUE
            invokeRestart("muffleWarning")
          }
        ),
        error = function(error) NULL
      )
      if (
        warning_seen ||
          !is.list(factory_tools) ||
          length(factory_tools) == 0L
      ) {
        tempest_researcher_app_tools_abort(
          "A selected local tool factory could not create tool definitions."
        )
      }
      factory_tools
    })
    for (factory_tools in factory_results) {
      for (tool in factory_tools) {
        selected_local_tools[[length(selected_local_tools) + 1L]] <- tool
      }
    }
  }
  selected_mcp_tools <- if (length(selected_mcp) == 0L) {
    list()
  } else {
    unname(mcp_loaded$tools[match(selected_mcp, names(mcp_loaded$tools))])
  }
  result <- c(selected_local_tools, selected_mcp_tools)
  if (length(result) > 0L) {
    tempest_researcher_checked_tool_names(result)
  }
  result
}

tempest_researcher_registered_tool_names <- function(chat) {
  if (!is.function(chat$get_tools)) {
    tempest_researcher_app_tools_abort(
      "The configured expert chat cannot verify its registered tools."
    )
  }
  registered <- tryCatch(chat$get_tools(), error = function(error) NULL)
  if (!is.list(registered)) {
    tempest_researcher_app_tools_abort(
      "The configured expert chat cannot verify its registered tools."
    )
  }
  if (length(registered) == 0L) {
    return(character())
  }
  supplied_names <- names(registered) %||% rep("", length(registered))
  names <- vapply(
    seq_along(registered),
    function(index) {
      name <- tryCatch(registered[[index]]@name, error = function(error) NULL)
      if (is.null(name)) {
        name <- supplied_names[[index]]
      }
      if (!rlang::is_string(name) || !nzchar(name)) {
        tempest_researcher_app_tools_abort(
          "The configured expert chat has an invalid registered tool."
        )
      }
      name
    },
    character(1)
  )
  if (anyDuplicated(names)) {
    tempest_researcher_app_tools_abort(
      "The configured expert chat has duplicate registered tool names."
    )
  }
  names
}

tempest_researcher_tools_config <- function(config, tools) {
  if (!S7::S7_inherits(config, TempestConfig)) {
    tempest_researcher_app_tools_abort(
      "{.arg config} must be created by {.fn tempest_config}."
    )
  }
  if (is.null(tools)) {
    tools <- list()
  }
  if (!is.list(tools)) {
    tempest_researcher_app_tools_abort("{.arg tools} must be a list.")
  }
  if (length(tools) == 0L) {
    return(config)
  }
  tool_names <- tempest_researcher_checked_tool_names(tools)
  original_config <- config
  original_chat_fn <- config@chat_fn
  wrapped_chat_fn <- function(role, model, system_prompt, echo) {
    chat <- tryCatch(
      if (!is.null(original_chat_fn)) {
        original_chat_fn(
          role = role,
          model = model,
          system_prompt = system_prompt,
          echo = echo
        )
      } else {
        tempest_make_chat(
          original_config,
          role = role,
          system_prompt = system_prompt,
          echo = echo
        )
      },
      error = function(error) {
        tempest_researcher_app_tools_abort(
          "The configured chat could not be created."
        )
      }
    )
    if (!identical(role, "expert")) {
      return(chat)
    }
    if (is.null(chat) || !is.function(chat$register_tools)) {
      tempest_researcher_app_tools_abort(
        "The configured expert chat cannot register selected tools."
      )
    }
    registered_names <- tempest_researcher_registered_tool_names(chat)
    if (any(tool_names %in% registered_names)) {
      tempest_researcher_app_tools_abort(
        "A selected tool name is already registered on the expert chat."
      )
    }
    tryCatch(
      chat$register_tools(tools),
      error = function(error) {
        tempest_researcher_app_tools_abort(
          "Selected tools could not be registered on the expert chat."
        )
      }
    )
    chat
  }
  wrapped_config <- config
  wrapped_config@chat_fn <- wrapped_chat_fn
  wrapped_config
}
