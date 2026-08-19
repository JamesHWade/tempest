# Internal shinychat compatibility boundary

tempest_shinychat_backend <- function() {
  tempest_require(
    "shinychat",
    "The Tempest chat interface requires shinychat."
  )
  list(
    version = as.character(utils::packageVersion("shinychat")),
    chat_ui = shinychat::chat_ui,
    chat_greeting = shinychat::chat_greeting,
    chat_server = shinychat::chat_server
  )
}

tempest_shinychat_error <- function(
  message,
  .envir = rlang::caller_env()
) {
  tempest_abort(
    message,
    class = c(
      "tempest_shinychat_error",
      "tempest_shiny_error",
      "tempest_error"
    ),
    .envir = .envir
  )
}

tempest_shinychat_function_supports <- function(fn, arguments) {
  if (!is.function(fn)) {
    return(FALSE)
  }
  available <- names(formals(fn))
  "..." %in% available || all(arguments %in% available)
}

tempest_shinychat_validate_backend <- function(backend) {
  version <- if (is.list(backend)) backend$version else NULL
  version <- as.character(version %||% "unknown")[[1L]]
  required <- c("chat_ui", "chat_greeting", "chat_server")
  missing <- if (is.list(backend)) {
    required[!vapply(backend[required], is.function, logical(1))]
  } else {
    required
  }
  if (length(missing) > 0L) {
    tempest_shinychat_error(c(
      "The installed shinychat does not satisfy the Tempest adapter contract.",
      "x" = "Missing callable interface{?s}: {.field {missing}}.",
      "i" = "Detected shinychat version: {.val {version}}."
    ))
  }

  contracts <- list(
    chat_ui = c("id", "greeting"),
    chat_greeting = "content",
    chat_server = c("id", "client", "history", "session")
  )
  incompatible <- names(Filter(
    Negate(identity),
    Map(
      tempest_shinychat_function_supports,
      backend[names(contracts)],
      contracts
    )
  ))
  if (length(incompatible) > 0L) {
    tempest_shinychat_error(c(
      "The installed shinychat has an incompatible function signature.",
      "x" = "Incompatible interface{?s}: {.field {incompatible}}.",
      "i" = "Detected shinychat version: {.val {version}}."
    ))
  }
  invisible(backend)
}

tempest_shinychat_validate_handle <- function(handle, version = "unknown") {
  if (!is.list(handle) && !is.environment(handle)) {
    tempest_shinychat_error(
      "shinychat::chat_server() returned an incompatible handle."
    )
  }
  required <- c(
    "append",
    "clear",
    "last_input",
    "last_turn",
    "set_client",
    "slash_command",
    "status"
  )
  missing <- required[
    !vapply(
      required,
      \(name) is.function(handle[[name]]),
      logical(1)
    )
  ]
  if (length(missing) > 0L) {
    tempest_shinychat_error(c(
      "shinychat::chat_server() returned an incompatible handle.",
      "x" = "Missing method{?s}: {.field {missing}}.",
      "i" = "Detected shinychat version: {.val {version}}."
    ))
  }

  contracts <- list(
    append = "role",
    clear = c("messages", "greeting", "client_history"),
    set_client = "sync",
    slash_command = c("name", "description", "handler")
  )
  methods <- lapply(names(contracts), \(name) handle[[name]])
  names(methods) <- names(contracts)
  incompatible <- names(Filter(
    Negate(identity),
    Map(
      tempest_shinychat_function_supports,
      methods,
      contracts
    )
  ))
  if (length(incompatible) > 0L) {
    tempest_shinychat_error(c(
      "shinychat::chat_server() returned methods with incompatible signatures.",
      "x" = "Incompatible method{?s}: {.field {incompatible}}.",
      "i" = "Detected shinychat version: {.val {version}}."
    ))
  }
  invisible(handle)
}

tempest_shinychat_require <- function(backend = tempest_shinychat_backend()) {
  tempest_shinychat_validate_backend(backend)
}

tempest_shinychat_ui <- function(
  id,
  ...,
  greeting = NULL,
  backend = tempest_shinychat_backend()
) {
  tempest_shinychat_validate_backend(backend)
  if (!is.null(greeting)) {
    greeting <- backend$chat_greeting(greeting)
  }
  backend$chat_ui(id, ..., greeting = greeting)
}

tempest_shinychat_citation_sanitizer <- function(id) {
  id <- gsub("\\", "\\\\", id, fixed = TRUE)
  id <- gsub("'", "\\'", id, fixed = TRUE)
  shiny::tags$script(shiny::HTML(sprintf(
    "
(function(rootId) {
  function clean(value) {
    if (!value || value.indexOf('cite') === -1) {
      return value;
    }
    return value
      .replace(/\\uE200cite\\uE202[^\\uE201\\n]*(?:\\uE201)?/g, '')
      .replace(/[\\uE000-\\uF8FF]*cite[\\uE000-\\uF8FF]*turn\\d+(?:search|view|fetch|image|news|source)\\d+(?:[\\uE000-\\uF8FF]*turn\\d+(?:search|view|fetch|image|news|source)\\d+)*[\\uE000-\\uF8FF]*/g, '')
      .replace(/[ \\t]+([.,;:!?])/g, '$1');
  }
  function cleanTree(root) {
    if (!root) {
      return;
    }
    if (root.nodeType === Node.TEXT_NODE) {
      var cleaned = clean(root.nodeValue);
      if (cleaned !== root.nodeValue) {
        root.nodeValue = cleaned;
      }
      return;
    }
    if (
      root.nodeType !== Node.ELEMENT_NODE &&
      root.nodeType !== Node.DOCUMENT_FRAGMENT_NODE
    ) {
      return;
    }
    var walker = document.createTreeWalker(root, NodeFilter.SHOW_TEXT);
    var node;
    while ((node = walker.nextNode())) {
      cleanTree(node);
    }
  }
  function observe(root) {
    if (!root || root.dataset.tempestCitationSanitizer === 'true') {
      return;
    }
    root.dataset.tempestCitationSanitizer = 'true';
    cleanTree(root);
    var observer = new MutationObserver(function(mutations) {
      mutations.forEach(function(mutation) {
        if (mutation.type === 'characterData') {
          cleanTree(mutation.target);
        }
        mutation.addedNodes.forEach(cleanTree);
      });
    });
    observer.observe(root, {
      childList: true,
      characterData: true,
      subtree: true
    });
    if (root.shadowRoot) {
      cleanTree(root.shadowRoot);
      observer.observe(root.shadowRoot, {
        childList: true,
        characterData: true,
        subtree: true
      });
    }
  }
  function start() {
    observe(document.getElementById(rootId));
  }
  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', start, { once: true });
  } else {
    start();
  }
  document.addEventListener('shiny:connected', start);
})('%s');
",
    id
  )))
}

tempest_shinychat_sanitize_text <- function(text) {
  if (is.null(text)) {
    return("")
  }
  text <- as.character(text)
  has_text <- !is.na(text) & nzchar(text)
  if (!any(has_text)) {
    return(text)
  }
  out <- text
  text <- text[has_text]
  pua_open <- intToUtf8(0xE200)
  pua_sep <- intToUtf8(0xE202)
  pua_close <- intToUtf8(0xE201)
  pua_range <- paste0("[", intToUtf8(0xE000), "-", intToUtf8(0xF8FF), "]*")
  text <- gsub(
    paste0(
      pua_open,
      "cite",
      pua_sep,
      "[^",
      pua_close,
      "\n]*(",
      pua_close,
      ")?"
    ),
    "",
    text,
    perl = TRUE
  )
  text <- gsub(
    paste0(
      pua_range,
      "cite",
      pua_range,
      "turn[0-9]+(search|view|fetch|image|news|source)[0-9]+",
      "(",
      pua_range,
      "turn[0-9]+",
      "(search|view|fetch|image|news|source)[0-9]+)*",
      pua_range
    ),
    "",
    text,
    perl = TRUE
  )
  text <- gsub("[ \t]+([.,;:!?])", "\\1", text, perl = TRUE)
  text <- gsub("([^\n])[ \t]{2,}([^\n])", "\\1 \\2", text, perl = TRUE)
  text <- gsub("[ \t]+\n", "\n", text, perl = TRUE)
  out[has_text] <- gsub("\n{3,}", "\n\n", text, perl = TRUE)
  out
}

tempest_shinychat_restore_messages <- function(
  transcript,
  topic = "Untitled topic",
  report_available = FALSE
) {
  topic <- as.character(topic %||% "Untitled topic")[[1L]]
  messages <- list(list(
    role = "assistant",
    content = paste0("Resumed Co-STORM session for: **", topic, "**")
  ))
  for (turn in transcript %||% list()) {
    text <- as.character(turn$text %||% "")[[1L]]
    if (is.na(text) || !nzchar(text)) {
      next
    }
    role <- tolower(as.character(turn$role %||% "")[[1L]])
    speaker <- as.character(
      turn$speaker %||%
        if (identical(role, "user")) "User" else "Moderator"
    )[[1L]]
    messages[[length(messages) + 1L]] <- list(
      role = if (identical(role, "user")) "user" else "assistant",
      content = paste0("**", speaker, ":**\n\n", text)
    )
  }
  if (isTRUE(report_available)) {
    messages[[length(messages) + 1L]] <- list(
      role = "assistant",
      content = "Restored report artifact. See the **Report** tab."
    )
  }
  messages
}

tempest_shinychat_escape_html <- function(text) {
  text <- gsub("&", "&amp;", text, fixed = TRUE)
  text <- gsub("<", "&lt;", text, fixed = TRUE)
  text <- gsub(">", "&gt;", text, fixed = TRUE)
  text <- gsub('"', "&quot;", text, fixed = TRUE)
  gsub("'", "&#39;", text, fixed = TRUE)
}

tempest_shinychat_suggestion_cards <- function(
  questions,
  lead = "**Research next:**"
) {
  titles <- names(questions)
  questions <- trimws(tempest_shinychat_sanitize_text(questions))
  keep <- !is.na(questions) & nzchar(questions)
  questions <- questions[keep]
  if (length(questions) == 0L) {
    return(NULL)
  }
  if (is.null(titles)) {
    titles <- rep("", length(keep))
  }
  titles <- trimws(titles[keep])
  fallback <- rep_len(
    c(
      "Evidence gap",
      "Key uncertainty",
      "Another perspective",
      "How to verify"
    ),
    length(questions)
  )
  missing <- is.na(titles) | !nzchar(titles)
  titles[missing] <- fallback[missing]
  items <- paste0(
    "- <span class=\"suggestion submit\" title=\"",
    tempest_shinychat_escape_html(titles),
    "\">",
    tempest_shinychat_escape_html(questions),
    "</span>"
  )
  structure(
    paste0(lead, "\n\n", paste(items, collapse = "\n")),
    class = c("tempest_shinychat_suggestions", "character")
  )
}

tempest_shinychat_completion_client <- function(client, on_completion) {
  if (!inherits(client, "TempestDeputyChatAdapter")) {
    return(client)
  }
  if (!is.function(on_completion)) {
    tempest_shinychat_error("The completion callback must be a function.")
  }

  stream_async <- client$stream_async
  proxy <- client
  proxy$stream_async <- function(...) {
    source <- stream_async(...)
    completion_id <- tempest_agent_completion_id(source)
    completed <- FALSE
    stream <- coro::async_generator(function() {
      repeat {
        value <- source()
        if (promises::is.promising(value)) {
          value <- coro::await(value)
        }
        if (coro::is_exhausted(value)) {
          if (!completed) {
            completed <<- TRUE
            on_completion(completion_id)
          }
          break
        }
        coro::yield(value)
      }
      coro::exhausted()
    })()
    tempest_agent_completion_tag(stream, completion_id)
  }
  proxy$clone <- function(...) proxy
  class(proxy) <- unique(c("TempestShinyChatCompletionClient", class(client)))
  proxy
}

TempestShinyChatAdapter <- R6::R6Class(
  "TempestShinyChatAdapter",
  public = list(
    initialize = function(
      id,
      initial_client,
      session,
      on_turn,
      workspace = function() NULL,
      render_message = function(text, role, workspace) text,
      on_dispose = NULL,
      backend = tempest_shinychat_backend()
    ) {
      tempest_shinychat_validate_backend(backend)
      if (!rlang::is_string(id) || !nzchar(id)) {
        tempest_shinychat_error("{.arg id} must be a non-empty string.")
      }
      callbacks <- list(
        on_turn = on_turn,
        workspace = workspace,
        render_message = render_message
      )
      invalid <- names(callbacks)[!vapply(callbacks, is.function, logical(1))]
      if (length(invalid) > 0L) {
        tempest_shinychat_error(c(
          "The shinychat adapter callbacks must be functions.",
          "x" = "Invalid callback{?s}: {.field {invalid}}."
        ))
      }
      if (!is.null(on_dispose) && !is.function(on_dispose)) {
        tempest_shinychat_error(
          "{.arg on_dispose} must be a function or `NULL`."
        )
      }

      private$session <- session
      private$initial_client <- initial_client
      private$on_turn <- on_turn
      private$workspace <- workspace
      private$render_message <- render_message
      private$on_dispose <- on_dispose
      private$version <- as.character(backend$version %||% "unknown")[[1L]]
      private$chat <- backend$chat_server(
        id = id,
        client = initial_client,
        history = FALSE,
        session = session
      )
      tempest_shinychat_validate_handle(private$chat, private$version)
      private$install_observers()
      session$onSessionEnded(function() self$dispose())
      invisible(self)
    },

    bind = function(
      client,
      messages = list(),
      client_history = c("clear", "keep"),
      on_turn = NULL
    ) {
      private$assert_active()
      client_history <- match.arg(client_history)
      if (is.null(on_turn)) {
        on_turn <- private$on_turn
      }
      if (!is.function(on_turn)) {
        tempest_shinychat_error("The turn callback must be a function.")
      }
      generation <- private$next_generation()
      transition <- list(
        client = client,
        messages = private$validate_messages(messages),
        client_history = client_history,
        generation = generation,
        on_turn = on_turn
      )
      status <- shiny::isolate(private$chat$status())
      if (identical(status, "streaming")) {
        private$pending_bind <- transition
        return(invisible(FALSE))
      }
      private$apply_bind(transition)
      invisible(TRUE)
    },

    reset = function() {
      self$bind(
        private$initial_client,
        messages = list(),
        client_history = "clear"
      )
    },

    append = function(text, role = c("assistant", "user")) {
      private$assert_active()
      role <- match.arg(role)
      rendered <- private$render_message(
        tempest_shinychat_sanitize_text(text),
        role,
        private$workspace()
      )
      private$in_domain(function() {
        private$chat$append(rendered, role = role)
      })
      invisible(rendered)
    },

    append_suggestions = function(cards) {
      private$assert_active()
      if (!inherits(cards, "tempest_shinychat_suggestions")) {
        tempest_shinychat_error(
          "Suggestion cards must come from tempest_shinychat_suggestion_cards()."
        )
      }
      private$in_domain(function() {
        private$chat$append(as.character(cards), role = "assistant")
      })
      invisible(cards)
    },

    register_commands = function(commands) {
      private$assert_active()
      if (!is.list(commands) || is.null(names(commands))) {
        tempest_shinychat_error(
          "{.arg commands} must be a named list of command definitions."
        )
      }
      private$in_domain(function() {
        for (name in names(commands)) {
          command <- commands[[name]]
          description <- command$description %||% ""
          handler <- command$handler %||% NULL
          cancel <- private$chat$slash_command(name, description, handler)
          if (is.function(cancel)) {
            private$command_cancels <- c(private$command_cancels, list(cancel))
          }
        }
      })
      invisible(commands)
    },

    status = function() {
      if (isTRUE(private$disposed)) "disposed" else private$chat$status()
    },

    invalidate = function() {
      private$assert_active()
      private$next_generation()
      private$pending_bind <- NULL
      invisible(private$generation)
    },

    dispose = function() {
      if (isTRUE(private$disposed)) {
        return(invisible(FALSE))
      }
      private$disposed <- TRUE
      private$generation <- private$generation + 1L
      private$input_generation <- NULL
      private$pending_bind <- NULL
      observers <- list(
        private$input_observer,
        private$turn_observer,
        private$status_observer
      )
      for (observer in observers) {
        if (!is.null(observer)) {
          observer$destroy()
        }
      }
      for (cancel in private$command_cancels) {
        tryCatch(cancel(), error = function(error) NULL)
      }
      private$command_cancels <- list()
      if (is.function(private$on_dispose)) {
        private$on_dispose()
      }
      invisible(TRUE)
    }
  ),
  private = list(
    chat = NULL,
    initial_client = NULL,
    session = NULL,
    on_turn = NULL,
    workspace = NULL,
    render_message = NULL,
    on_dispose = NULL,
    version = "unknown",
    generation = 0L,
    input_generation = NULL,
    pending_bind = NULL,
    disposed = FALSE,
    input_observer = NULL,
    turn_observer = NULL,
    status_observer = NULL,
    command_cancels = list(),
    completed_streams = list(),

    assert_active = function() {
      if (isTRUE(private$disposed)) {
        tempest_shinychat_error("The shinychat adapter has been disposed.")
      }
      invisible(NULL)
    },

    in_domain = function(fn) {
      shiny::withReactiveDomain(
        private$session,
        shiny::isolate(fn())
      )
    },

    next_generation = function() {
      private$generation <- private$generation + 1L
      private$input_generation <- NULL
      invisible(private$generation)
    },

    validate_messages = function(messages) {
      if (!is.list(messages)) {
        tempest_shinychat_error("{.arg messages} must be a list.")
      }
      for (message in messages) {
        valid <- is.list(message) &&
          identical(
            sort(intersect(names(message), c("content", "role"))),
            c("content", "role")
          ) &&
          rlang::is_string(message$content) &&
          message$role %in% c("assistant", "user")
        if (!isTRUE(valid)) {
          tempest_shinychat_error(
            "Each restored message must contain string `role` and `content` fields."
          )
        }
      }
      messages
    },

    apply_bind = function(transition) {
      force(transition)
      private$pending_bind <- NULL
      client <- tempest_shinychat_completion_client(
        transition$client,
        function(completion_id) {
          private$completion_ready(
            completion_id,
            transition$generation,
            transition$on_turn
          )
        }
      )
      private$in_domain(function() {
        private$chat$set_client(client, sync = FALSE)
        private$chat$clear(
          messages = NULL,
          greeting = FALSE,
          client_history = transition$client_history
        )
        for (message in transition$messages) {
          rendered <- private$render_message(
            tempest_shinychat_sanitize_text(message$content),
            message$role,
            private$workspace()
          )
          private$chat$append(rendered, role = message$role)
        }
      })
      invisible(NULL)
    },

    completion_ready = function(completion_id, generation, on_turn) {
      entry <- list(
        completion_id = completion_id,
        generation = generation,
        on_turn = on_turn
      )
      if (isTRUE(private$disposed)) {
        tryCatch(
          on_turn(
            completion_id = completion_id,
            is_current = function() FALSE
          ),
          error = function(error) NULL
        )
        return(invisible(NULL))
      }
      private$completed_streams[[length(private$completed_streams) + 1L]] <-
        entry
      invisible(NULL)
    },

    take_completed_stream = function() {
      if (length(private$completed_streams) == 0L) {
        return(NULL)
      }
      entry <- private$completed_streams[[1L]]
      private$completed_streams <- private$completed_streams[-1L]
      entry
    },

    install_observers = function() {
      private$input_observer <- shiny::observeEvent(
        private$chat$last_input(),
        {
          private$input_generation <- private$generation
        },
        ignoreInit = TRUE,
        domain = private$session
      )
      private$turn_observer <- shiny::observeEvent(
        private$chat$last_turn(),
        {
          entry <- private$take_completed_stream()
          if (is.null(entry)) {
            return()
          }
          input_generation <- private$input_generation
          private$input_generation <- NULL
          is_current <- function() {
            !isTRUE(private$disposed) &&
              identical(input_generation, entry$generation) &&
              identical(entry$generation, private$generation)
          }
          private$in_domain(function() {
            entry$on_turn(
              completion_id = entry$completion_id,
              is_current = is_current
            )
          })
        },
        ignoreInit = TRUE,
        domain = private$session
      )
      private$status_observer <- shiny::observe(
        {
          status <- private$chat$status()
          if (
            !isTRUE(private$disposed) &&
              !identical(status, "streaming") &&
              !is.null(private$pending_bind)
          ) {
            private$apply_bind(private$pending_bind)
          }
        },
        domain = private$session
      )
      invisible(NULL)
    }
  )
)

tempest_shinychat_adapter <- function(...) {
  TempestShinyChatAdapter$new(...)
}

# shinychat does not currently expose programmatic cancellation or disposal for
# an in-flight stream, nor a transformation hook for streamed text. The adapter
# invalidates stale callbacks and keeps completed-message transformations local,
# but those two operations remain upstream limitations.
