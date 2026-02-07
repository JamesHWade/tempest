# Tools for ellmer tool calling

#' Detect provider from model name
#'
#' Parses model names like "openai/gpt-4o-mini" or "anthropic/claude-sonnet"
#' to extract the provider.
#'
#' @param model Model name string
#' @return Provider name (e.g., "openai", "anthropic", "google") or NULL if unknown
#' @keywords internal
tempest_detect_provider <- function(model) {
  if (is.null(model) || !is.character(model) || length(model) == 0) {
    return(NULL)
  }
  # Parse "provider/model-name" format
  if (grepl("/", model)) {
    provider <- sub("/.*$", "", model)
    # Normalize provider names
    provider <- tolower(provider)
    # Map common aliases
    provider <- switch(provider,
      "claude" = "anthropic",
      "gemini" = "google",
      provider
    )
    return(provider)
  }
  NULL
}

#' Get provider-native web tools
#'
#' Returns the appropriate web search/fetch tools for a given provider.
#' These are ellmer's built-in tools that use provider-native APIs.
#'
#' @param provider Provider name ("openai", "anthropic", "google")
#' @return List of ellmer tools, or NULL if provider doesn
#' @keywords internal
tempest_get_native_web_tools <- function(provider) {
  tempest_require("ellmer", "Provider-native web tools require ellmer.")

  tools <- switch(provider,
    "openai" = list(
      ellmer::openai_tool_web_search()
    ),
    "anthropic" = list(
      ellmer::claude_tool_web_search()
    ),
    "google" = list(
      ellmer::google_tool_web_search(),
      ellmer::google_tool_web_fetch()
    ),
    NULL
  )

  tools
}

#' Check if provider has native web search
#'
#' @param provider Provider name
#' @return TRUE if provider supports native web search
#' @keywords internal
tempest_provider_has_native_search <- function(provider) {
  provider %in% c("openai", "anthropic", "google")
}

#' @keywords internal
tempest_tools_retrieval <- function(retriever) {
  tempest_require("ellmer", "Tool calling for retrieval.")
  stopifnot(inherits(retriever, "TempestRetriever"))

  web_search <- function(query, k = retriever$config$max_search_results) {
    retriever$search(query = query, k = k)
  }

  fetch_url <- function(url) {
    src <- retriever$fetch(url)
    # return a compact payload to the model
    list(
      source_id = src$id,
      url = src$url,
      title = src$title,
      fetched_at = src$fetched_at,
      kind = src$meta$kind %||% NA_character_,
      error = src$meta$error %||% NA_character_,
      excerpt = if (!is.na(src$content_text)) substr(src$content_text, 1, 1200) else NA_character_
    )
  }

  get_source <- function(source_id) {
    src <- retriever$store$get_source(source_id)
    if (is.null(src)) return(NULL)
    list(
      source_id = src$id,
      url = src$url,
      title = src$title,
      fetched_at = src$fetched_at,
      kind = src$meta$kind %||% NA_character_,
      error = src$meta$error %||% NA_character_,
      excerpt = if (!is.na(src$content_text)) substr(src$content_text, 1, 1200) else NA_character_
    )
  }

  list_sources <- function() {
    s <- retriever$store$list_sources()
    purrr::map(s, ~ list(
      source_id = .x$id,
      url = .x$url,
      title = .x$title,
      fetched_at = .x$fetched_at,
      kind = .x$meta$kind %||% NA_character_,
      error = .x$meta$error %||% NA_character_
    ))
  }

  list_facts <- function() {
    f <- retriever$store$list_facts()
    purrr::map(f, ~ list(
      fact_id = .x$id,
      claim = .x$claim,
      source_ids = .x$source_ids,
      confidence = .x$confidence,
      created_at = .x$created_at
    ))
  }

  add_fact <- function(claim, source_ids, confidence = "medium", note = "") {
    if (is.null(source_ids) || length(source_ids) == 0) tempest_abort("add_fact requires at least one source_id.")
    fact <- tempest_fact(claim = claim, source_ids = source_ids, confidence = confidence, note = note)
    retriever$store$add_fact(fact)
    list(fact_id = fact$id, claim = fact$claim, source_ids = fact$source_ids, confidence = fact$confidence)
  }

  tools <- list(
    ellmer::tool(
      web_search,
      name = "web_search",
      description = paste(
        "Search the web (or Wikipedia) for relevant sources. Use this to discover sources to cite.",
        "Return values include source_id, title, url, and snippet."
      ),
      arguments = list(
        query = ellmer::type_string("Search query."),
        k = ellmer::type_integer("Maximum number of results to return.", required = FALSE)
      )
    ),
    ellmer::tool(
      fetch_url,
      name = "fetch_url",
      description = paste(
        "Fetch a URL and extract readable plain text. Use this after web_search to read a source.",
        "Returns source_id, title, and a text excerpt."
      ),
      arguments = list(
        url = ellmer::type_string("The URL to fetch.")
      )
    ),
    ellmer::tool(
      get_source,
      name = "get_source",
      description = "Get a previously fetched source by source_id. Returns an excerpt and metadata.",
      arguments = list(
        source_id = ellmer::type_string("Source id, e.g. S123abc...")
      )
    ),
    ellmer::tool(
      list_sources,
      name = "list_sources",
      description = "List all sources currently stored in memory for this session.",
      arguments = list()
    ),
    ellmer::tool(
      list_facts,
      name = "list_facts",
      description = "List all fact notes currently stored in memory for this session.",
      arguments = list()
    ),
    ellmer::tool(
      add_fact,
      name = "add_fact",
      description = paste(
        "Add an atomic fact note to the session memory, with supporting source_ids.",
        "Use this to store key verified claims for later writing."
      ),
      arguments = list(
        claim = ellmer::type_string("Atomic factual claim."),
        source_ids = ellmer::type_array(ellmer::type_string("One or more source ids that support the claim.")),
        confidence = ellmer::type_enum(c("low", "medium", "high"), "Confidence level.", required = FALSE),
        note = ellmer::type_string("Optional note on nuances/conditions/limitations.", required = FALSE)
      )
    )
  )

  tools
}

#' Get source/fact management tools (without web_search/fetch_url)
#'
#' Returns tools for managing sources and facts, but not the web search tools.
#' Used when provider-native web search is enabled.
#'
#' @param retriever A TempestRetriever object
#' @return List of ellmer tools for source/fact management
#' @keywords internal
tempest_tools_source_management <- function(retriever) {
  tempest_require("ellmer", "Tool calling for retrieval.")
  stopifnot(inherits(retriever, "TempestRetriever"))

  get_source <- function(source_id) {
    src <- retriever$store$get_source(source_id)
    if (is.null(src)) return(NULL)
    list(
      source_id = src$id,
      url = src$url,
      title = src$title,
      fetched_at = src$fetched_at,
      kind = src$meta$kind %||% NA_character_,
      error = src$meta$error %||% NA_character_,
      excerpt = if (!is.na(src$content_text)) substr(src$content_text, 1, 1200) else NA_character_
    )
  }

  list_sources <- function() {
    s <- retriever$store$list_sources()
    purrr::map(s, ~ list(
      source_id = .x$id,
      url = .x$url,
      title = .x$title,
      fetched_at = .x$fetched_at,
      kind = .x$meta$kind %||% NA_character_,
      error = .x$meta$error %||% NA_character_
    ))
  }

  list_facts <- function() {
    f <- retriever$store$list_facts()
    purrr::map(f, ~ list(
      fact_id = .x$id,
      claim = .x$claim,
      source_ids = .x$source_ids,
      confidence = .x$confidence,
      created_at = .x$created_at
    ))
  }

  add_fact <- function(claim, source_ids, confidence = "medium", note = "") {
    if (is.null(source_ids) || length(source_ids) == 0) tempest_abort("add_fact requires at least one source_id.")
    fact <- tempest_fact(claim = claim, source_ids = source_ids, confidence = confidence, note = note)
    retriever$store$add_fact(fact)
    list(fact_id = fact$id, claim = fact$claim, source_ids = fact$source_ids, confidence = fact$confidence)
  }

  list(
    ellmer::tool(
      get_source,
      name = "get_source",
      description = "Get a previously fetched source by source_id. Returns an excerpt and metadata.",
      arguments = list(
        source_id = ellmer::type_string("Source id, e.g. S123abc...")
      )
    ),
    ellmer::tool(
      list_sources,
      name = "list_sources",
      description = "List all sources currently stored in memory for this session.",
      arguments = list()
    ),
    ellmer::tool(
      list_facts,
      name = "list_facts",
      description = "List all fact notes currently stored in memory for this session.",
      arguments = list()
    ),
    ellmer::tool(
      add_fact,
      name = "add_fact",
      description = paste(
        "Add an atomic fact note to the session memory, with supporting source_ids.",
        "Use this to store key verified claims for later writing."
      ),
      arguments = list(
        claim = ellmer::type_string("Atomic factual claim."),
        source_ids = ellmer::type_array(ellmer::type_string("One or more source ids that support the claim.")),
        confidence = ellmer::type_enum(c("low", "medium", "high"), "Confidence level.", required = FALSE),
        note = ellmer::type_string("Optional note on nuances/conditions/limitations.", required = FALSE)
      )
    )
  )
}

#' Register default tools on a chat
#'
#' Registers web search and source/fact management tools on a chat object.
#' When search_provider is "native" and the provider supports it, uses the
#' provider's built-in web search. Otherwise uses custom tempest tools.
#'
#' @param chat An ellmer chat object
#' @param retriever A TempestRetriever object
#' @param model Model name (used to detect provider for native tools)
#' @param search_provider Search provider setting from config
#' @return The chat object (invisibly)
#' @keywords internal
tempest_register_default_tools <- function(chat, retriever, model = NULL, search_provider = "native") {
  tempest_require("ellmer", "Tool calling for retrieval.")


  # Determine if we should use native provider tools
  use_native <- FALSE
  if (identical(search_provider, "native") && !is.null(model)) {
    provider <- tempest_detect_provider(model)
    if (!is.null(provider) && tempest_provider_has_native_search(provider)) {
      native_tools <- tempest_get_native_web_tools(provider)
      if (!is.null(native_tools)) {
        chat$register_tools(native_tools)
        use_native <- TRUE
      }
    }
  }

  # If not using native tools, register custom web search/fetch tools
  if (!use_native) {
    tools <- tempest_tools_retrieval(retriever)
    chat$register_tools(tools)
  } else {
    # Still register source/fact management tools even with native search
    mgmt_tools <- tempest_tools_source_management(retriever)
    chat$register_tools(mgmt_tools)
  }

  # Register ragnar retrieve tool if store is available
  if (!is.null(retriever$ragnar_store) && tempest_has("ragnar")) {
    ragnar::ragnar_register_tool_retrieve(
      chat,
      retriever$ragnar_store,
      store_description = "the STORM knowledge base containing fetched web sources"
    )
  }

  invisible(chat)
}

# Expert Subagent Pattern (inspired by btw)

#' Expert Session Manager
#'
#' Manages expert chat sessions with session IDs for conversation continuity.
#' Each expert persona gets their own chat session that can be resumed using
#' the session ID returned from previous interactions.
#'
#' @field sessions Environment storing active chat sessions keyed by session ID.
#' @field config A `TempestConfig` object for creating chats.
#' @field retriever A `TempestRetriever` for registering tools.
#' @field extractor Chat object for fact extraction (optional).
#' @field store A `SourceStore` for storing extracted facts (optional).
#'
#' @keywords internal
ExpertSessionManager <- R6::R6Class(
  "ExpertSessionManager",
  public = list(
    sessions = NULL,
    config = NULL,
    retriever = NULL,
    extractor = NULL,
    store = NULL,

    #' @description
    #' Create a new ExpertSessionManager.
    #' @param config A `TempestConfig` object.
    #' @param retriever A `TempestRetriever` object.
    #' @param extractor Optional chat object for fact extraction.
    #' @param store Optional `SourceStore` for storing extracted facts.
    initialize = function(config, retriever, extractor = NULL, store = NULL) {
      self$sessions <- new.env(parent = emptyenv())
      self$config <- config
      self$retriever <- retriever
      self$extractor <- extractor
      self$store <- store
      invisible(self)
    },

    #' @description
    #' Extract facts from an expert response.
    #' @param response Character string response from expert.
    #' @return Invisibly returns NULL.
    extract_facts = function(response) {
      if (!is.null(self$extractor) && !is.null(self$store)) {
        tempest_extract_facts_from_answer(self$extractor, response, self$store)
      }
      invisible(NULL)
    },

    #' @description
    #' Generate a human-readable session ID from a persona name.
    #' @param persona_name Name of the persona.
    #' @return Character string session ID like "dr-sarah-chen-abc123".
    generate_session_id = function(persona_name) {
      base <- tolower(gsub("[^a-zA-Z0-9]+", "-", persona_name))
      base <- gsub("^-|-$", "", base)
      suffix <- substr(digest::digest(runif(1), algo = "xxhash64"), 1, 7)
      paste0(base, "-", suffix)
    },

    #' @description
    #' Get an existing session or create a new one.
    #' @param persona A persona object from `tempest_generate_personas()`.
    #' @param session_id Optional session ID to resume.
    #' @return List with `chat`, `session_id`, and `is_new` fields.
    get_or_create = function(persona, session_id = NULL) {
      # If session_id provided and exists, return it
      if (!is.null(session_id) && exists(session_id, envir = self$sessions)) {
        return(list(
          chat = get(session_id, envir = self$sessions),
          session_id = session_id,
          is_new = FALSE
        ))
      }

      # Create new session
      sp <- tempest_render_expert_prompt(persona = persona, expert_id = persona$id %||% 1L)
      chat <- self$config$make_chat("expert", system_prompt = sp, echo = "none")
      tempest_register_default_tools(
        chat,
        self$retriever,
        model = self$config$models[["expert"]],
        search_provider = self$config$search_provider
      )

      new_id <- session_id %||% self$generate_session_id(persona$name %||% "expert")
      assign(new_id, chat, envir = self$sessions)

      list(
        chat = chat,
        session_id = new_id,
        is_new = TRUE
      )
    },

    #' @description
    #' List all active session IDs.
    #' @return Character vector of session IDs.
    list_sessions = function() {
      ls(envir = self$sessions)
    }
  )
)

#' Create an Expert Subagent Tool
#'
#' Creates an ellmer tool for a specific expert persona that can be called
#' by the moderator to delegate questions.
#'
#' @param persona A persona object from `tempest_generate_personas()`.
#' @param session_manager An `ExpertSessionManager` instance.
#' @param topic The research topic (for context).
#' @return An ellmer tool.
#' @keywords internal
tempest_create_expert_tool <- function(persona, session_manager, topic) {
  tempest_require("ellmer", "Expert tools require ellmer.")

  # Sanitize name for tool name (e.g., "Dr. Sarah Chen" -> "ask_dr_sarah_chen")
  safe_name <- tolower(gsub("[^a-zA-Z0-9]+", "_", persona$name %||% "expert"))
  safe_name <- gsub("^_|_$", "", safe_name)
  tool_name <- paste0("ask_", safe_name)

  # Build description from persona
  desc <- paste0(
    "Ask ", persona$name %||% "Expert", ", ", persona$title %||% "Research Specialist", ". ",
    persona$perspective %||% "General research perspective."
  )

  # Capture persona in closure
  p <- persona
  topic_str <- topic
  mgr <- session_manager

  ask_expert <- function(question, session_id = NULL) {
    result <- mgr$get_or_create(p, session_id)
    chat <- result$chat
    sid <- result$session_id

    # Build prompt with context
    prompt <- paste0(
      "Topic: ", topic_str, "\n\n",
      "Question: ", question, "\n\n",
      "Instructions:\n",
      "- Use web_search + fetch_url to find and cite sources.\n",
      "- Only state factual claims supported by sources you fetched.\n",
      "- For each factual sentence, add citations like [Sxxxxxxxxxxxx].\n",
      "- If evidence is weak or unclear, say so.\n\n",
      "Respond now:"
    )

    response <- chat$chat(prompt, echo = "none")

    # Extract plain text from the response using ellmer's contents_markdown
    # This properly handles ellmer_output objects and tool call results
    last_turn <- chat$last_turn()
    response_text <- if (is.null(last_turn) || length(last_turn@contents) == 0) {
      "(Expert completed but returned no message.)"
    } else {
      ellmer::contents_markdown(last_turn)
    }

    # Extract facts from expert response
    mgr$extract_facts(response_text)

    list(
      expert = p$name %||% "Expert",
      response = response_text,
      session_id = sid
    )
  }

  ellmer::tool(
    ask_expert,
    name = tool_name,
    description = desc,
    arguments = list(
      question = ellmer::type_string("The question or topic to research. Be specific about what you need."),
      session_id = ellmer::type_string("Optional: Resume a previous conversation with this expert. Use the session_id from a previous response.", required = FALSE)
    )
  )
}

#' Create Expert Tools for All Personas
#'
#' Creates ellmer tools for each expert persona and returns them as a list.
#'
#' @param personas List of persona objects.
#' @param session_manager An `ExpertSessionManager` instance.
#' @param topic The research topic.
#' @return A list of ellmer tools.
#' @keywords internal
tempest_create_expert_tools <- function(personas, session_manager, topic) {
  purrr::map(personas, ~ tempest_create_expert_tool(.x, session_manager, topic))
}

#' Register Expert Tools on a Chat
#'
#' Registers expert subagent tools on a moderator chat, allowing it to
#' delegate questions to specialized experts.
#'
#' @param chat An ellmer chat object (the moderator).
#' @param personas List of persona objects.
#' @param session_manager An `ExpertSessionManager` instance.
#' @param topic The research topic.
#' @return The chat object (invisibly).
#' @keywords internal
tempest_register_expert_tools <- function(chat, personas, session_manager, topic) {
  tools <- tempest_create_expert_tools(personas, session_manager, topic)
  chat$register_tools(tools)
  invisible(chat)
}

#' Register a single expert tool on a chat
#'
#' @param chat An ellmer chat object.
#' @param persona A persona object.
#' @param session_manager An `ExpertSessionManager` instance.
#' @param topic The research topic.
#' @return The chat object (invisibly).
#' @keywords internal
tempest_register_single_expert_tool <- function(chat, persona, session_manager, topic) {
  tool <- tempest_create_expert_tool(persona, session_manager, topic)
  chat$register_tools(list(tool))
  invisible(chat)
}

#' Generate a single expert persona for a specific area
#'
#' @param topic The research topic.
#' @param area The area of expertise needed.
#' @param existing_personas List of existing personas to avoid duplication.
#' @param config A `TempestConfig` object.
#' @return A persona list.
#' @keywords internal
tempest_generate_single_persona <- function(topic, area, existing_personas, config) {
  tempest_require("ellmer", "Generating expert personas requires ellmer.")

  chat <- config$make_chat(
    "coordinator",
    system_prompt = tempest_prompt("expert_generator_system"),
    echo = "none"
  )

  existing_desc <- if (length(existing_personas) > 0) {
    paste(purrr::map_chr(existing_personas, function(p) {
      paste0("- ", p$name %||% "Expert", " (", p$title %||% "", "): ", p$perspective %||% "")
    }), collapse = "\n")
  } else {
    "(none)"
  }

  type <- tempest_type_personas()
  prompt <- paste0(
    "Topic: ", topic, "\n\n",
    "Needed expertise: ", area, "\n\n",
    "Existing experts (do not duplicate):\n", existing_desc, "\n\n",
    "Generate exactly 1 expert persona to fill this knowledge gap.\n",
    "Return structured data."
  )

  result <- tryCatch(
    chat$chat_structured(prompt, type = type, echo = "none", convert = FALSE),
    error = function(e) {
      tempest_warn("Failed to generate expert persona for {.val {area}}: {conditionMessage(e)}")
      list(personas = list())
    }
  )
  personas <- result$personas %||% list()
  if (length(personas) == 0) {
    tempest_warn("Using generic fallback persona for {.val {area}}.")
    return(list(
      name = paste("Expert in", area),
      title = area,
      affiliation = "Independent",
      background = paste("Expert in", area),
      focus_areas = list(area),
      perspective = paste("Specializes in", area),
      initial_questions = list(paste("What are the key aspects of", area, "related to", topic, "?"))
    ))
  }
  personas[[1]]
}
