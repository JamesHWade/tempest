# Co-STORM (interactive multi-agent)

#' @keywords internal
costorm_type_decision <- function(persona_names = NULL) {
  tempest_require("ellmer")
  example <- if (!is.null(persona_names) && length(persona_names) > 0) {
    paste0("moderator, ", paste(persona_names, collapse = ", "))
  } else {
    "moderator, expert_1, expert_2, expert_3"
  }
  ellmer::type_object(
    next_agent = ellmer::type_string(paste0("Agent name to respond next (e.g., ", example, ").")),
    instruction = ellmer::type_string("Instruction/prompt for the chosen agent."),
    rationale = ellmer::type_string("Short reason for choosing this agent.", required = FALSE)
  )
}

#' @keywords internal
costorm_type_mindmap <- function() {
  tempest_require("ellmer")
  node <- ellmer::type_object(
    id = ellmer::type_string("Stable node id (short). Use 'root' for the root."),
    label = ellmer::type_string("Node label"),
    parent = ellmer::type_string("Parent node id (or null for root).", required = FALSE),
    notes = ellmer::type_string("Optional notes for this node.", required = FALSE),
    source_ids = ellmer::type_array(ellmer::type_string("Supporting source ids"), required = FALSE)
  )
  edge <- ellmer::type_object(
    from = ellmer::type_string("From node id"),
    to = ellmer::type_string("To node id"),
    relation = ellmer::type_string("Relation label", required = FALSE)
  )
  ellmer::type_object(
    nodes = ellmer::type_array(node),
    edges = ellmer::type_array(edge)
  )
}

#' @keywords internal
costorm_mindmap_init <- function(topic) {
  list(
    nodes = list(list(id = "root", label = topic, parent = NULL, notes = "", source_ids = character())),
    edges = list()
  )
}

#' @keywords internal
costorm_mindmap_to_markdown <- function(m) {
  nodes <- m$nodes %||% list()
  edges <- m$edges %||% list()
  if (length(nodes) == 0) return("(empty mind map)")

  # build parent->children mapping
  by_parent <- list()
  for (n in nodes) {
    p <- n$parent %||% "root"
    if (is.null(n$parent) && n$id == "root") next
    by_parent[[p]] <- c(by_parent[[p]] %||% character(), n$id)
  }
  node_by_id <- setNames(nodes, purrr::map_chr(nodes, "id"))

  render_node <- function(id, depth = 0) {
    n <- node_by_id[[id]]
    if (is.null(n)) return(character())
    indent <- paste(rep("  ", depth), collapse = "")
    line <- paste0(indent, "- **", n$label, "**")
    if (!is.null(n$notes) && nzchar(n$notes)) line <- paste0(line, ": ", n$notes)
    if (!is.null(n$source_ids) && length(n$source_ids) > 0) {
      line <- paste0(line, " ", paste0("[", paste(n$source_ids, collapse = ", "), "]"))
    }
    kids <- by_parent[[id]] %||% character()
    c(line, unlist(purrr::map(kids, ~ render_node(.x, depth + 1))))
  }

  # root
  out <- c(paste0("**Topic:** ", node_by_id[["root"]]$label), "")
  kids <- by_parent[["root"]] %||% character()
  out <- c(out, unlist(purrr::map(kids, ~ render_node(.x, 0))))
  paste(out, collapse = "\n")
}

#' CoStormSession
#'
#' Maintains state for a Co-STORM session: multi-agent dialog, mind map, sources, and report artifacts.
#'
#' @field topic The research topic.
#' @field title The report title.
#' @field config A `StormConfig` object.
#' @field store A `SourceStore` object.
#' @field retriever A `StormRetriever` object.
#' @field personas List of expert personas.
#' @field expert_session_manager Manages expert chat sessions.
#' @field chats List of chat objects for each role.
#' @field transcript List of dialog turns.
#' @field mindmap The mind map data structure.
#' @field artifacts Environment of report artifacts.
#'
#' @export
CoStormSession <- R6::R6Class(
  "CoStormSession",
  public = list(
    topic = NULL,
    title = NULL,
    config = NULL,
    store = NULL,
    retriever = NULL,
    personas = NULL,
    expert_session_manager = NULL,
    chats = NULL,
    transcript = NULL,
    mindmap = NULL,
    artifacts = NULL,

    #' @description
    #' Create a new CoStormSession.
    #' @param topic The research topic.
    #' @param config A `StormConfig` object.
    #' @param n_experts Number of expert agents.
    #' @param personas Optional list of pre-generated personas. If NULL,
    #'   personas are generated automatically using `storm_generate_personas()`.
    initialize = function(topic, config = storm_config(), n_experts = 3, personas = NULL) {
      tempest_require("ellmer", "CoStormSession requires ellmer.")
      self$topic <- tempest_trim(topic)
      if (is.na(self$topic) || self$topic == "") tempest_abort("topic must be a non-empty string.")
      self$title <- self$topic
      self$config <- config
      self$store <- SourceStore$new()
      self$retriever <- storm_retriever(config = config, store = self$store)
      self$transcript <- list()
      self$mindmap <- costorm_mindmap_init(self$topic)
      self$artifacts <- new.env(parent = emptyenv())

      # Generate or use provided personas
      if (is.null(personas)) {
        self$personas <- storm_generate_personas(
          topic = self$topic,
          n = n_experts,
          config = config,
          verbose = FALSE
        )
      } else {
        self$personas <- personas
      }

      # Create chats first (need extractor for session manager)
      self$chats <- list(
        moderator = config$make_chat("coordinator", system_prompt = storm_prompt("moderator_system")),
        mindmap = config$make_chat("mindmap", system_prompt = storm_prompt("mindmap_system")),
        reporter = config$make_chat("writer", system_prompt = storm_prompt("reporter_system")),
        extractor = config$make_chat("judge", system_prompt = storm_prompt("fact_extractor_system"))
      )

      # Create expert session manager for subagent pattern (with extractor for fact extraction)
      self$expert_session_manager <- ExpertSessionManager$new(
        config,
        self$retriever,
        extractor = self$chats$extractor,
        store = self$store
      )

      # Register tools on moderator: default tools + expert subagent tools
      storm_register_default_tools(
        self$chats$moderator,
        self$retriever,
        model = self$config$models[["coordinator"]],
        search_provider = self$config$search_provider
      )
      storm_register_expert_tools(
        self$chats$moderator,
        self$personas,
        self$expert_session_manager,
        self$topic
      )

      # Register tools on other chats that may need them
      storm_register_default_tools(
        self$chats$mindmap,
        self$retriever,
        model = self$config$models[["mindmap"]],
        search_provider = self$config$search_provider
      )
      storm_register_default_tools(
        self$chats$reporter,
        self$retriever,
        model = self$config$models[["writer"]],
        search_provider = self$config$search_provider
      )

      invisible(self)
    },

    #' @description
    #' Add a turn to the transcript.
    #' @param speaker Speaker name.
    #' @param role Role: "user" or "assistant".
    #' @param text The text content.
    add_turn = function(speaker, role = c("user", "assistant"), text) {
      role <- match.arg(role)
      self$transcript <- c(self$transcript, list(list(
        speaker = speaker,
        role = role,
        text = text,
        at = tempest_now_utc()
      )))
      invisible(TRUE)
    },

    #' @description
    #' Get the transcript as markdown.
    #' @param max_turns Maximum turns to include.
    #' @return Markdown string.
    transcript_markdown = function(max_turns = 50) {
      t <- self$transcript
      if (length(t) == 0) return("(no dialog yet)")
      t <- t[seq_len(min(length(t), max_turns))]
      lines <- purrr::map_chr(t, function(x) {
        who <- x$speaker %||% x$role
        paste0("- **", who, "**: ", x$text)
      })
      paste(lines, collapse = "\n")
    },

    #' @description
    #' Get persona names for agent routing.
    #' @return Character vector of persona names.
    get_persona_names = function() {
      purrr::map_chr(self$personas, ~ .x$name %||% paste0("Expert ", .x$id %||% 1))
    },

    #' @description
    #' Build persona descriptions for moderator context.
    #' @return A formatted string describing all personas.
    get_persona_descriptions = function() {
      descs <- purrr::imap_chr(self$personas, function(p, i) {
        paste0(
          "- **", p$name %||% paste0("Expert ", i), "** (",
          p$title %||% "Research Specialist", "): ",
          p$perspective %||% "General research perspective"
        )
      })
      paste(descs, collapse = "\n")
    },

    #' @description
    #' Decide which agent should respond next.
    #' @param user_input The user's input.
    #' @return A decision object with next_agent and instruction.
    decide_next = function(user_input) {
      # Moderator chooses which agent to respond next and with what instruction.
      m <- self$chats$moderator
      persona_names <- self$get_persona_names()
      type <- costorm_type_decision(persona_names)
      prompt <- paste0(
        "You are routing questions to the appropriate expert.\n\n",
        "Topic: ", self$topic, "\n\n",
        "Expert panel (choose one to respond):\n", self$get_persona_descriptions(), "\n\n",
        "Current mind map:\n", costorm_mindmap_to_markdown(self$mindmap), "\n\n",
        "Recent dialog:\n", self$transcript_markdown(max_turns = 20), "\n\n",
        "New user message:\n", user_input, "\n\n",
        "IMPORTANT: Route to an expert for research questions. Only choose 'moderator' for meta-questions about the session itself.\n",
        "Choose the expert whose expertise best matches the question.\n\n",
        "Valid agent names: moderator, ", paste(persona_names, collapse = ", "), ".\n",
        "Return structured data with next_agent set to the chosen expert's full name."
      )
      m$chat_structured(prompt, type = type, echo = "none", convert = FALSE)
    },

    #' @description
    #' Update the mind map based on new exchange.
    #' @param last_exchange The latest exchange text.
    update_mindmap = function(last_exchange) {
      mm <- self$chats$mindmap
      type <- costorm_type_mindmap()
      prompt <- paste0(
        "Update the research mind map based on the latest exchange.\n\n",
        "Topic: ", self$topic, "\n\n",
        "Current mind map:\n", costorm_mindmap_to_markdown(self$mindmap), "\n\n",
        "Latest exchange:\n", last_exchange, "\n\n",
        "Rules:\n",
        "- Keep node ids stable where possible.\n",
        "- Add nodes for new subtopics, hypotheses, and open questions.\n",
        "- Add source_ids to nodes when the exchange included citations like [Sxxxxxxxxxxxx].\n",
        "- Do not fabricate sources.\n\n",
        "Return an updated mind map as structured data."
      )
      new_mm <- mm$chat_structured(prompt, type = type, echo = "none", convert = FALSE)
      if (!is.null(new_mm$nodes) && length(new_mm$nodes) > 0) {
        self$mindmap <- new_mm
        self$artifacts[["mindmap_md"]] <- costorm_mindmap_to_markdown(new_mm)
      }
      invisible(TRUE)
    },

    #' @description
    #' Get the mind map as markdown.
    #' @return Markdown string.
    mindmap_markdown = function() {
      costorm_mindmap_to_markdown(self$mindmap)
    },

    #' @description
    #' Extract facts from text into the store.
    #' @param text Text containing factual claims.
    extract_facts = function(text) {
      storm_extract_facts_from_answer(self$chats$extractor, text, self$store)
      invisible(TRUE)
    },

    #' @description
    #' Find expert index by persona name.
    #' @param name The agent name to look up.
    #' @return Index of the expert, or NULL if not found.
    find_expert_index = function(name) {
      persona_names <- self$get_persona_names()
      # Exact match
      idx <- match(name, persona_names)
      if (!is.na(idx)) return(idx)
      # Case-insensitive match
      idx <- match(tolower(name), tolower(persona_names))
      if (!is.na(idx)) return(idx)
      # Partial match (first name only)
      first_names <- purrr::map_chr(persona_names, ~ strsplit(.x, " ")[[1]][1])
      idx <- match(tolower(name), tolower(first_names))
      if (!is.na(idx)) return(idx)
      # Legacy expert_N format
      if (grepl("^expert_\\d+$", name, ignore.case = TRUE)) {
        return(as.integer(sub("^expert_", "", name, ignore.case = TRUE)))
      }
      NULL
    },

    #' @description
    #' Process one step of the conversation.
    #' @param user_input User's input message.
    #' @return A list with speaker, answer, tool_calls, and mindmap_md.
    step = function(user_input) {
      user_input <- tempest_trim(user_input)
      if (is.na(user_input) || user_input == "") return(invisible(NULL))

      self$add_turn("user", "user", user_input)

      # Build context for the moderator
      prompt <- paste0(
        "Topic: ", self$topic, "\n\n",
        "Mind map:\n", costorm_mindmap_to_markdown(self$mindmap), "\n\n",
        "Recent dialog:\n", self$transcript_markdown(max_turns = 30), "\n\n",
        "User question:\n", user_input, "\n\n",
        "You have tools to ask experts (ask_*) for research questions.\n",
        "Use the expert tools to delegate research questions to the appropriate expert.\n",
        "Synthesize their responses into a coherent answer for the user.\n",
        "Use citations like [Sxxxxxxxxxxxx] for factual claims."
      )

      # The moderator will use expert tools as needed
      ans <- self$chats$moderator$chat(prompt, echo = "none")
      self$add_turn("Moderator", "assistant", ans)

      # Extract facts (best-effort)
      storm_extract_facts_from_answer(self$chats$extractor, ans, self$store)

      # Update mind map
      self$update_mindmap(last_exchange = paste0("User: ", user_input, "\n\nModerator: ", ans))

      list(
        speaker = "Moderator",
        answer = ans,
        mindmap_md = self$artifacts[["mindmap_md"]] %||% costorm_mindmap_to_markdown(self$mindmap)
      )
    },

    #' @description
    #' Run a warmup phase where each expert researches their initial questions.
    #' This primes the knowledge base with foundational research before interactive Q&A.
    #' @param verbose If TRUE, prints progress messages.
    #' @return A list with results from each expert's warmup.
    warmup = function(verbose = TRUE) {
      if (length(self$personas) == 0) {
        if (verbose) tempest_inform("No personas available for warmup.")
        return(invisible(list()))
      }

      results <- list()

      for (i in seq_along(self$personas)) {
        persona <- self$personas[[i]]
        persona_name <- persona$name %||% paste("Expert", i)
        initial_qs <- persona$initial_questions %||% character()

        if (length(initial_qs) == 0) {
          if (verbose) tempest_inform("Skipping {persona_name}: no initial questions")
          next
        }

        if (verbose) tempest_inform("Warmup: {persona_name} ({length(initial_qs)} questions)")

        # Get or create expert session
        session_result <- self$expert_session_manager$get_or_create(persona)
        chat <- session_result$chat
        session_id <- session_result$session_id

        expert_results <- list()
        for (q in initial_qs) {
          if (verbose) tempest_inform("  Q: {q}")

          prompt <- paste0(
            "Topic: ", self$topic, "\n\n",
            "Question: ", q, "\n\n",
            "Instructions:\n",
            "- Use web_search + fetch_url to find and cite sources.\n",
            "- Only state factual claims supported by sources you fetched.\n",
            "- For each factual sentence, add citations like [Sxxxxxxxxxxxx].\n",
            "- If evidence is weak or unclear, say so.\n\n",
            "Respond now:"
          )

          response <- chat$chat(prompt, echo = "none")

          # Extract facts from expert response
          self$expert_session_manager$extract_facts(response)

          # Add to transcript
          self$add_turn(persona_name, "assistant", response)

          expert_results <- c(expert_results, list(list(
            question = q,
            response = response
          )))

          # Update mind map after each response
          self$update_mindmap(last_exchange = paste0(
            "Initial research by ", persona_name, ":\n",
            "Q: ", q, "\n\n",
            "A: ", response
          ))
        }

        results[[persona_name]] <- list(
          session_id = session_id,
          questions_answered = length(expert_results),
          results = expert_results
        )
      }

      if (verbose) {
        total_facts <- length(self$store$list_facts())
        total_sources <- length(self$store$list_sources())
        tempest_inform("Warmup complete: {total_facts} facts, {total_sources} sources")
      }

      invisible(results)
    },

    #' @description
    #' Generate a report from the session.
    #' @param style Report style: "technical" or "executive".
    #' @param include_references Include references section.
    #' @return Markdown report string.
    report = function(style = c("technical", "executive"), include_references = TRUE) {
      style <- match.arg(style)
      rep <- self$chats$reporter
      prompt <- paste0(
        "Write a comprehensive report based on the session.\n\n",
        "Topic: ", self$topic, "\n\n",
        "Mind map:\n", costorm_mindmap_to_markdown(self$mindmap), "\n\n",
        "Verified facts:\n", storm_summarize_facts_for_prompt(self$store, max_items = 120), "\n\n",
        "Conversation (summary):\n", self$transcript_markdown(max_turns = 80), "\n\n",
        "Style: ", style, "\n\n",
        "Rules:\n",
        "- Use only verified facts (with citations).\n",
        "- Preserve citations like [Sxxxxxxxxxxxx].\n",
        "- Do not invent facts.\n\n",
        "Write the report body in Markdown (no title)."
      )
      body <- rep$chat(prompt, echo = "none")
      self$artifacts[["report"]] <- body
      md <- if (include_references) storm_report_md(self$title %||% self$topic, body, self$store) else body
      self$artifacts[["report_md"]] <- md
      md
    }
  )
)

#' Create a Co-STORM session
#'
#' @param topic Topic string.
#' @param config A `StormConfig`.
#' @param n_experts Number of expert agents.
#' @param personas Optional list of pre-generated personas. If NULL,
#'   personas are generated automatically.
#' @export
costorm_session <- function(topic, config = storm_config(), n_experts = 3, personas = NULL) {
  CoStormSession$new(topic = topic, config = config, n_experts = n_experts, personas = personas)
}
