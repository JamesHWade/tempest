# Deterministic stdio MCP server used by the researcher app integration tests.
args <- commandArgs(trailingOnly = TRUE)
if (length(args)) {
  writeLines(as.character(Sys.getpid()), args[[1L]])
}
input <- file("stdin", open = "r")
empty_object <- structure(list(), names = character())
repeat {
  line <- readLines(input, n = 1L, warn = FALSE)
  if (!length(line)) {
    break
  }
  request <- jsonlite::fromJSON(line, simplifyVector = FALSE)
  if (is.null(request$id)) {
    next
  }
  result <- switch(
    request$method,
    initialize = list(
      protocolVersion = request$params$protocolVersion,
      capabilities = list(tools = empty_object),
      serverInfo = list(name = "tempest-local-fixture", version = "1")
    ),
    `tools/list` = list(
      tools = list(list(
        name = "read_sample",
        description = "Read deterministic research context from the local test server.",
        inputSchema = list(
          type = "object",
          properties = list(query = list(type = "string")),
          required = list("query")
        ),
        annotations = list(
          readOnlyHint = TRUE,
          destructiveHint = FALSE,
          idempotentHint = TRUE,
          openWorldHint = FALSE
        )
      ))
    ),
    `tools/call` = list(
      content = list(list(
        type = "text",
        text = paste0("Observed fixture: ", request$params$arguments$query)
      ))
    ),
    empty_object
  )
  cat(
    jsonlite::toJSON(
      list(jsonrpc = "2.0", id = request$id, result = result),
      auto_unbox = TRUE,
      null = "null"
    ),
    "\n",
    sep = ""
  )
  flush(stdout())
}
