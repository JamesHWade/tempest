tempest_contract_child_chat <- function() {
  state <- new.env(parent = emptyenv())
  state$turns <- list()
  state$tools <- list()
  state$system_prompt <- NULL
  state$on_tool_request <- function(request) invisible(NULL)
  state$on_tool_result <- function(result) invisible(NULL)
  state$provider <- fake_chat_provider("child")

  add_turn <- function(contents) {
    state$turns <- c(
      state$turns,
      list(ellmer::AssistantTurn(contents, tokens = c(4, 2, 0), cost = 0))
    )
  }
  chat <- structure(
    list(
      chat = function(prompt = NULL) "child complete",
      stream = function(
        prompt = NULL,
        stream = c("text", "content"),
        controller = NULL
      ) {
        tool <- state$tools[["inspect_evidence"]]
        request <- ellmer::ContentToolRequest(
          id = "child-tool-call",
          name = "inspect_evidence",
          arguments = list(claim = "claim-1"),
          tool = tool
        )
        step <- 0L
        function() {
          step <<- step + 1L
          if (step == 1L) {
            add_turn(list(request))
            return(request)
          }
          if (step == 2L) {
            state$on_tool_request(request)
            result <- ellmer::ContentToolResult(
              value = tool(claim = request@arguments$claim),
              request = request
            )
            state$on_tool_result(result)
            return(result)
          }
          if (step == 3L) {
            text <- ellmer::ContentText("child complete")
            add_turn(list(text))
            return(text)
          }
          coro::exhausted()
        }
      },
      stream_async = function(...) {
        source <- chat$stream(...)
        coro::async_generator(function() {
          repeat {
            content <- source()
            if (coro::is_exhausted(content)) {
              break
            }
            coro::yield(content)
          }
          coro::exhausted()
        })()
      },
      get_turns = function() state$turns,
      set_turns = function(turns) state$turns <- turns,
      get_system_prompt = function() state$system_prompt,
      set_system_prompt = function(prompt) state$system_prompt <- prompt,
      get_tools = function() state$tools,
      set_tools = function(tools) {
        state$tools <- list()
        for (tool in tools) {
          state$tools[[tool@name]] <- tool
        }
      },
      register_tool = function(tool) state$tools[[tool@name]] <- tool,
      register_tools = function(tools) {
        for (tool in tools) {
          state$tools[[tool@name]] <- tool
        }
      },
      get_tokens = function() {
        data.frame(input = 4, output = 2, cached_input = 0, cost = 0)
      },
      get_provider = function() state$provider,
      get_model = function() "child",
      last_turn = function(role = "assistant") {
        if (length(state$turns) == 0L) {
          return(NULL)
        }
        tail(state$turns, 1L)[[1L]]
      },
      on_tool_request = function(callback) state$on_tool_request <- callback,
      on_tool_result = function(callback) state$on_tool_result <- callback,
      clone = function() chat
    ),
    class = "Chat"
  )
  chat
}

tempest_contract_parent_chat <- function(child_chat) {
  state <- new.env(parent = emptyenv())
  state$turns <- list()
  state$tools <- list()
  state$system_prompt <- NULL
  state$on_tool_request <- function(request) invisible(NULL)
  state$on_tool_result <- function(result) invisible(NULL)
  state$request_number <- 0L
  state$provider <- fake_chat_provider("parent")

  add_turn <- function(contents) {
    state$turns <- c(
      state$turns,
      list(ellmer::AssistantTurn(contents, tokens = c(6, 3, 0), cost = 0))
    )
  }
  chat <- structure(
    list(
      chat = function(prompt = NULL) "lead complete",
      stream = function(
        prompt = NULL,
        stream = c("text", "content"),
        controller = NULL
      ) {
        state$request_number <- state$request_number + 1L
        if (state$request_number > 1L) {
          step <- 0L
          return(function() {
            step <<- step + 1L
            if (step == 1L) {
              text <- ellmer::ContentText("lead complete")
              add_turn(list(text))
              return(text)
            }
            coro::exhausted()
          })
        }
        tool <- state$tools[["delegate_to_agent"]]
        request <- ellmer::ContentToolRequest(
          id = "parent-delegate-call",
          name = "delegate_to_agent",
          arguments = list(
            agent_name = "evidence_reviewer",
            task = "Review claim-1"
          ),
          tool = tool
        )
        step <- 0L
        function() {
          step <<- step + 1L
          if (step == 1L) {
            add_turn(list(request))
            return(request)
          }
          if (step == 2L) {
            state$on_tool_request(request)
            value <- tool(
              agent_name = request@arguments$agent_name,
              task = request@arguments$task
            )
            as_result <- function(value) {
              result <- ellmer::ContentToolResult(
                value = value,
                request = request
              )
              state$on_tool_result(result)
              result
            }
            if (promises::is.promising(value)) {
              return(promises::then(value, as_result))
            }
            return(as_result(value))
          }
          coro::exhausted()
        }
      },
      stream_async = function(...) {
        coro::async_generator(function() {
          complete <- FALSE
          while (!complete) {
            source <- chat$stream(...)
            repeat {
              content <- source()
              if (promises::is.promising(content)) {
                content <- coro::await(content)
              }
              if (coro::is_exhausted(content)) {
                break
              }
              complete <- complete ||
                inherits(
                  content,
                  "ellmer::ContentText"
                )
              coro::yield(content)
            }
          }
          coro::exhausted()
        })()
      },
      get_turns = function() state$turns,
      set_turns = function(turns) state$turns <- turns,
      get_system_prompt = function() state$system_prompt,
      set_system_prompt = function(prompt) state$system_prompt <- prompt,
      get_tools = function() state$tools,
      set_tools = function(tools) {
        state$tools <- list()
        for (tool in tools) {
          state$tools[[tool@name]] <- tool
        }
      },
      register_tool = function(tool) state$tools[[tool@name]] <- tool,
      register_tools = function(tools) {
        for (tool in tools) {
          state$tools[[tool@name]] <- tool
        }
      },
      get_tokens = function() {
        data.frame(input = 10, output = 5, cached_input = 0, cost = 0)
      },
      get_provider = function() state$provider,
      get_model = function() "parent",
      last_turn = function(role = "assistant") {
        if (length(state$turns) == 0L) {
          return(NULL)
        }
        tail(state$turns, 1L)[[1L]]
      },
      on_tool_request = function(callback) state$on_tool_request <- callback,
      on_tool_result = function(callback) state$on_tool_result <- callback,
      clone = function() child_chat
    ),
    class = "Chat"
  )
  chat
}
