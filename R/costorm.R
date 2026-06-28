# Co-STORM (interactive multi-agent)

#' @keywords internal
tempest_type_mindmap <- function() {
  tempest_require("ellmer")
  node <- ellmer::type_object(
    id = ellmer::type_string(
      "Stable node id (short). Use 'root' for the root."
    ),
    label = ellmer::type_string("Node label"),
    parent = ellmer::type_string(
      "Parent node id (or null for root).",
      required = FALSE
    ),
    notes = ellmer::type_string(
      "Optional notes for this node.",
      required = FALSE
    ),
    source_ids = ellmer::type_array(
      ellmer::type_string("Supporting source ids"),
      required = FALSE
    )
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
tempest_mindmap_init <- function(topic) {
  list(
    nodes = list(list(
      id = "root",
      label = topic,
      parent = NULL,
      notes = "",
      source_ids = character()
    )),
    edges = list()
  )
}

#' @keywords internal
tempest_mindmap_to_markdown <- function(m) {
  nodes <- m$nodes %||% list()
  edges <- m$edges %||% list()
  if (length(nodes) == 0) {
    return("(empty mind map)")
  }

  # build parent->children mapping
  by_parent <- list()
  for (n in nodes) {
    p <- n$parent %||% "root"
    if (is.null(n$parent) && n$id == "root") {
      next
    }
    by_parent[[p]] <- c(by_parent[[p]] %||% character(), n$id)
  }
  node_by_id <- setNames(nodes, purrr::map_chr(nodes, "id"))

  render_node <- function(id, depth = 0) {
    n <- node_by_id[[id]]
    if (is.null(n)) {
      return(character())
    }
    indent <- paste(rep("  ", depth), collapse = "")
    line <- paste0(indent, "- **", n$label, "**")
    if (!is.null(n$notes) && nzchar(n$notes)) {
      line <- paste0(line, ": ", n$notes)
    }
    if (!is.null(n$source_ids) && length(n$source_ids) > 0) {
      line <- paste0(
        line,
        " ",
        paste0("[", paste(n$source_ids, collapse = ", "), "]")
      )
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

#' Create a Co-STORM expert definition
#'
#' Host applications can pass these definitions to [tempest_session()] via the
#' `personas` argument to control the expert panel without patching
#' `TempestSession` internals.
#'
#' @param name Expert display name.
#' @param title Short title or area of expertise.
#' @param perspective Description of the expert's research perspective.
#' @param initial_questions Character vector of warmup questions for this
#'   expert.
#' @param id Optional numeric expert id. Missing ids are assigned by
#'   [tempest_session()].
#' @param affiliation Optional affiliation.
#' @param background Optional background text.
#' @param focus_areas Optional character vector of focus areas.
#' @return A list suitable for the `personas` argument of [tempest_session()].
#' @examples
#' expert <- tempest_expert(
#'   name = "Dr. Rivera",
#'   title = "Battery policy analyst",
#'   perspective = "Policy and market incentives",
#'   initial_questions = "Which policies affect battery recycling?"
#' )
#' @export
tempest_expert <- function(
  name,
  title = "Research Specialist",
  perspective = "General research perspective",
  initial_questions = character(),
  id = NULL,
  affiliation = NA_character_,
  background = NA_character_,
  focus_areas = character()
) {
  for (field in c("name", "title", "perspective")) {
    value <- get(field, inherits = FALSE)
    if (!rlang::is_string(value) || !nzchar(tempest_trim(value))) {
      tempest_abort("{.arg {field}} must be a single non-empty string.")
    }
  }
  expert <- list(
    name = tempest_trim(name),
    title = tempest_trim(title),
    affiliation = affiliation,
    background = background,
    focus_areas = as.character(focus_areas %||% character()),
    perspective = tempest_trim(perspective),
    initial_questions = tempest_trim(as.character(
      initial_questions %||% character()
    ))
  )
  if (!is.null(id)) {
    expert$id <- tempest_as_expert_id(id)
  }
  expert
}

#' @keywords internal
tempest_as_expert_id <- function(id, arg = "id") {
  coerced <- suppressWarnings(as.integer(id))
  if (length(coerced) != 1L || is.na(coerced)) {
    tempest_abort(
      "{.arg {arg}} must be a single value coercible to an integer."
    )
  }
  coerced
}

#' @keywords internal
tempest_validate_personas <- function(personas) {
  if (is.null(personas)) {
    return(NULL)
  }
  if (!is.list(personas)) {
    tempest_abort("{.arg personas} must be a list of expert definitions.")
  }
  defaults <- list(
    title = "Research Specialist",
    perspective = "General research perspective"
  )
  validated <- purrr::imap(personas, function(persona, idx) {
    if (!is.list(persona)) {
      tempest_abort("Each entry in {.arg personas} must be a list.")
    }
    if (
      !rlang::is_string(persona$name) || !nzchar(tempest_trim(persona$name))
    ) {
      tempest_abort("Each expert must include a non-empty {.field name}.")
    }
    persona$name <- tempest_trim(persona$name)
    for (field in c("title", "perspective")) {
      value <- persona[[field]] %||% defaults[[field]]
      if (!rlang::is_string(value) || !nzchar(tempest_trim(value))) {
        value <- defaults[[field]]
      }
      persona[[field]] <- tempest_trim(value)
    }
    persona$id <- tempest_as_expert_id(persona$id %||% idx)
    persona$initial_questions <- tempest_trim(as.character(
      persona$initial_questions %||% character()
    ))
    persona
  })
  ids <- vapply(validated, function(persona) persona$id, integer(1))
  if (anyDuplicated(ids) > 0) {
    dupes <- unique(ids[duplicated(ids)])
    tempest_abort(c(
      "Expert ids must be unique.",
      x = "Duplicated id{?s}: {dupes}."
    ))
  }
  validated
}

#' TempestSession
#'
#' Maintains state for a Co-STORM session: multi-agent dialog, mind map, sources, and report artifacts.
#'
#' @field topic The research topic.
#' @field title The report title.
#' @field config A `TempestConfig` object.
#' @field session_id Stable identifier shared by progress events for the
#'   session.
#' @field progress Optional progress callback.
#' @field store A `SourceStore` object.
#' @field retriever A `TempestRetriever` object.
#' @field personas List of expert personas.
#' @field expert_session_manager Manages expert chat sessions.
#' @field chats List of chat objects for each role.
#' @field transcript List of dialog turns.
#' @field mindmap The mind map data structure.
#' @field artifacts Environment of report artifacts.
#' @field discourse_manager A `DiscourseManager` object (NULL when disabled).
#'
#' @export
TempestSession <- R6::R6Class(
  "TempestSession",
  public = list(
    topic = NULL,
    title = NULL,
    config = NULL,
    session_id = NULL,
    progress = NULL,
    store = NULL,
    retriever = NULL,
    personas = NULL,
    expert_session_manager = NULL,
    chats = NULL,
    transcript = NULL,
    mindmap = NULL,
    artifacts = NULL,
    discourse_manager = NULL,

    #' @description
    #' Create a new TempestSession.
    #' @param topic The research topic.
    #' @param config A `TempestConfig` object.
    #' @param n_experts Number of expert agents.
    #' @param personas Optional list of pre-generated personas. If NULL,
    #'   personas are generated automatically using `tempest_generate_personas()`.
    #' @param retriever Optional `TempestRetriever` or compatible retriever
    #'   object with a `SourceStore` at `$store`.
    #' @param progress Optional function called with `tempest_progress_event`
    #'   objects as the session makes progress.
    initialize = function(
      topic,
      config = tempest_config(),
      n_experts = 3,
      personas = NULL,
      retriever = NULL,
      progress = NULL
    ) {
      tempest_require("ellmer", "TempestSession requires ellmer.")
      self$topic <- tempest_trim(topic)
      if (is.na(self$topic) || self$topic == "") {
        tempest_abort("topic must be a non-empty string.")
      }
      self$title <- self$topic
      self$config <- config
      self$session_id <- tempest_uuid("session")
      self$progress <- tempest_progress_callback(progress)
      if (is.null(retriever)) {
        self$store <- SourceStore$new()
        self$retriever <- tempest_retriever(config = config, store = self$store)
      } else {
        store <- if (is.list(retriever) || is.environment(retriever)) {
          retriever$store
        } else {
          NULL
        }
        if (is.null(store) || !inherits(store, "SourceStore")) {
          tempest_abort(
            "{.arg retriever} must expose a SourceStore at {.code retriever$store}."
          )
        }
        self$retriever <- retriever
        self$store <- store
      }
      self$transcript <- list()
      self$mindmap <- tempest_mindmap_init(self$topic)
      self$artifacts <- new.env(parent = emptyenv())

      # Generate or use provided personas
      if (is.null(personas)) {
        self$personas <- tempest_generate_personas(
          topic = self$topic,
          n = n_experts,
          config = config,
          verbose = FALSE
        )
      } else {
        self$personas <- tempest_validate_personas(personas)
      }

      # Create chats first (need extractor for session manager)
      self$chats <- list(
        moderator = tempest_make_chat(
          config,
          "coordinator",
          system_prompt = tempest_prompt("moderator_system")
        ),
        mindmap = tempest_make_chat(
          config,
          "mindmap",
          system_prompt = tempest_prompt("mindmap_system")
        ),
        reporter = tempest_make_chat(
          config,
          "writer",
          system_prompt = tempest_prompt("reporter_system")
        ),
        extractor = tempest_make_chat(
          config,
          "judge",
          system_prompt = tempest_prompt("fact_extractor_system")
        )
      )

      # Create expert session manager for subagent pattern (with extractor for fact extraction)
      self$expert_session_manager <- ExpertSessionManager$new(
        config,
        self$retriever,
        extractor = self$chats$extractor,
        store = self$store,
        progress = self$progress,
        run_id = self$session_id
      )

      # Register tools on moderator: default tools + expert subagent tools
      tempest_register_default_tools(
        self$chats$moderator,
        self$retriever,
        model = self$config@models[["coordinator"]],
        search_provider = self$config@search_provider
      )
      tempest_register_expert_tools(
        self$chats$moderator,
        self$personas,
        self$expert_session_manager,
        self$topic
      )

      # Register tools on other chats that may need them
      tempest_register_default_tools(
        self$chats$mindmap,
        self$retriever,
        model = self$config@models[["mindmap"]],
        search_provider = self$config@search_provider
      )
      tempest_register_default_tools(
        self$chats$reporter,
        self$retriever,
        model = self$config@models[["writer"]],
        search_provider = self$config@search_provider
      )

      # Initialize discourse manager if enabled
      if (isTRUE(config@enable_discourse_manager)) {
        self$discourse_manager <- DiscourseManager$new(config)
      }

      self$emit_progress(
        "workflow",
        "started",
        stage = "session",
        step = "created",
        payload = list(persona_count = length(self$personas))
      )

      invisible(self)
    },

    #' @description
    #' Emit a Co-STORM progress event.
    #' @param event_type Progress event type.
    #' @param status Progress event status.
    #' @param stage Optional workflow stage.
    #' @param step Optional workflow step.
    #' @param message Optional progress message.
    #' @param payload Optional progress metadata.
    #' @param parent_event_id Optional parent event id.
    #' @param correlation_id Optional correlation id.
    emit_progress = function(
      event_type,
      status,
      stage = NA_character_,
      step = NA_character_,
      message = NA_character_,
      payload = list(),
      parent_event_id = NA_character_,
      correlation_id = NA_character_
    ) {
      tempest_emit_progress(
        self$progress,
        run_id = self$session_id,
        workflow = "costorm",
        event_type = event_type,
        status = status,
        stage = stage,
        step = step,
        message = message,
        payload = payload,
        parent_event_id = parent_event_id,
        correlation_id = correlation_id
      )
    },

    #' @description
    #' Add a turn to the transcript.
    #' @param speaker Speaker name.
    #' @param role Role: "user" or "assistant".
    #' @param text The text content.
    add_turn = function(speaker, role = c("user", "assistant"), text) {
      role <- match.arg(role)
      self$transcript <- c(
        self$transcript,
        list(list(
          speaker = speaker,
          role = role,
          text = text,
          at = tempest_now_utc()
        ))
      )
      invisible(TRUE)
    },

    #' @description
    #' Get the transcript as markdown.
    #' @param max_turns Maximum turns to include.
    #' @return Markdown string.
    transcript_markdown = function(max_turns = 50) {
      t <- self$transcript
      if (length(t) == 0) {
        return("(no dialog yet)")
      }
      # Turns are appended oldest-first; callers want the most recent ones.
      t <- utils::tail(t, max_turns)
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
      purrr::map_chr(
        self$personas,
        ~ .x$name %||% paste0("Expert ", .x$id %||% 1)
      )
    },

    #' @description
    #' Build persona descriptions for moderator context.
    #' @return A formatted string describing all personas.
    get_persona_descriptions = function() {
      descs <- purrr::imap_chr(self$personas, function(p, i) {
        paste0(
          "- **",
          p$name %||% paste0("Expert ", i),
          "** (",
          p$title %||% "Research Specialist",
          "): ",
          p$perspective %||% "General research perspective"
        )
      })
      paste(descs, collapse = "\n")
    },

    #' @description
    #' Update the mind map based on new exchange.
    #' @param last_exchange The latest exchange text.
    update_mindmap = function(last_exchange) {
      event <- self$emit_progress(
        "step",
        "started",
        stage = "mindmap",
        step = "update"
      )
      mm <- self$chats$mindmap
      type <- tempest_type_mindmap()
      prompt <- paste0(
        "Update the research mind map based on the latest exchange.\n\n",
        "Topic: ",
        self$topic,
        "\n\n",
        "Current mind map:\n",
        tempest_mindmap_to_markdown(self$mindmap),
        "\n\n",
        "Latest exchange:\n",
        last_exchange,
        "\n\n",
        "Rules:\n",
        "- Keep node ids stable where possible.\n",
        "- Add nodes for new subtopics, hypotheses, and open questions.\n",
        "- Add source_ids to nodes when the exchange included citations like [Sxxxxxxxxxxxx].\n",
        "- Do not fabricate sources.\n\n",
        "Return an updated mind map as structured data."
      )
      tryCatch(
        {
          new_mm <- mm$chat_structured(
            prompt,
            type = type,
            echo = "none",
            convert = FALSE
          )
          if (!is.null(new_mm$nodes) && length(new_mm$nodes) > 0) {
            self$mindmap <- new_mm
            self$artifacts[["mindmap_md"]] <- tempest_mindmap_to_markdown(
              new_mm
            )
            # Check for oversized nodes and split if needed
            self$check_and_expand_nodes()
          }
          self$emit_progress(
            "step",
            "succeeded",
            stage = "mindmap",
            step = "update",
            parent_event_id = event@event_id,
            correlation_id = event@correlation_id,
            payload = list(node_count = length(self$mindmap$nodes %||% list()))
          )
        },
        error = function(e) {
          self$emit_progress(
            "step",
            "failed",
            stage = "mindmap",
            step = "update",
            parent_event_id = event@event_id,
            correlation_id = event@correlation_id,
            payload = tempest_progress_error_payload(e)
          )
          stop(e)
        }
      )
      invisible(TRUE)
    },

    #' @description
    #' Get the mind map as markdown.
    #' @return Markdown string.
    mindmap_markdown = function() {
      tempest_mindmap_to_markdown(self$mindmap)
    },

    #' @description
    #' Extract facts from text into the store.
    #' @param text Text containing factual claims.
    #' @param turn Optional ellmer turn to inspect for provider-native sources.
    #' @param source_ids Optional source ids already harvested for the turn.
    extract_facts = function(text, turn = NULL, source_ids = NULL) {
      event <- self$emit_progress(
        "step",
        "started",
        stage = "evidence",
        step = "fact_extraction"
      )
      tryCatch(
        {
          # Only re-harvest when the caller did not already do so; callers that
          # pass source_ids have harvested the turn into the store already.
          harvested <- if (is.null(source_ids)) {
            tempest_harvest_native_sources_from_turn(turn, self$store)
          } else {
            character()
          }
          tempest_extract_facts_from_answer(
            self$chats$extractor,
            text,
            self$store,
            source_ids = unique(c(source_ids, harvested))
          )
          self$emit_progress(
            "step",
            "succeeded",
            stage = "evidence",
            step = "fact_extraction",
            parent_event_id = event@event_id,
            correlation_id = event@correlation_id,
            payload = list(claim_count = length(self$store$list_claims()))
          )
        },
        error = function(e) {
          self$emit_progress(
            "step",
            "failed",
            stage = "evidence",
            step = "fact_extraction",
            parent_event_id = event@event_id,
            correlation_id = event@correlation_id,
            payload = tempest_progress_error_payload(e)
          )
          stop(e)
        }
      )
      invisible(TRUE)
    },

    #' @description
    #' Experimental helper for harvesting source metadata from provider-native
    #' web tool responses.
    #' @param chat Optional chat whose last turn should be inspected.
    #' @param turn Optional explicit ellmer turn.
    #' @return Character vector of source ids added or updated.
    harvest_native_sources = function(chat = NULL, turn = NULL) {
      ids <- character()
      if (!is.null(turn)) {
        ids <- c(
          ids,
          tempest_harvest_native_sources_from_turn(
            turn,
            self$store
          )
        )
      }
      if (!is.null(chat)) {
        ids <- c(
          ids,
          tempest_harvest_native_sources_from_chat(
            chat,
            self$store
          )
        )
      }
      unique(ids[!is.na(ids) & nzchar(ids)])
    },

    #' @description
    #' Suggest follow-up questions for the user based on the conversation so far.
    #' @param n Maximum number of questions to return.
    #' @return A character vector of questions (possibly empty).
    suggest_questions = function(n = 4) {
      event <- self$emit_progress(
        "step",
        "started",
        stage = "suggestions",
        step = "question_generation"
      )
      # Pass NULL (not transcript_markdown's "(no dialog yet)" placeholder) so an
      # empty session gets newcomer-style questions rather than follow-ups.
      context <- if (length(self$transcript) > 0) {
        self$transcript_markdown(max_turns = 12)
      } else {
        NULL
      }
      tryCatch(
        {
          questions <- tempest_suggest_questions(
            topic = self$topic,
            context = context,
            n = n,
            config = self$config
          )
          self$emit_progress(
            "step",
            "succeeded",
            stage = "suggestions",
            step = "question_generation",
            parent_event_id = event@event_id,
            correlation_id = event@correlation_id,
            payload = list(question_count = length(questions))
          )
          questions
        },
        error = function(e) {
          self$emit_progress(
            "step",
            "failed",
            stage = "suggestions",
            step = "question_generation",
            parent_event_id = event@event_id,
            correlation_id = event@correlation_id,
            payload = tempest_progress_error_payload(e)
          )
          stop(e)
        }
      )
    },

    #' @description
    #' Find expert index by persona name.
    #' @param name The agent name to look up.
    #' @return Index of the expert, or NULL if not found.
    find_expert_index = function(name) {
      persona_names <- self$get_persona_names()
      # Exact match
      idx <- match(name, persona_names)
      if (!is.na(idx)) {
        return(idx)
      }
      # Case-insensitive match
      idx <- match(tolower(name), tolower(persona_names))
      if (!is.na(idx)) {
        return(idx)
      }
      # Partial match (first name only)
      first_names <- purrr::map_chr(persona_names, ~ strsplit(.x, " ")[[1]][1])
      idx <- match(tolower(name), tolower(first_names))
      if (!is.na(idx)) {
        return(idx)
      }
      # Legacy expert_N format
      if (grepl("^expert_\\d+$", name, ignore.case = TRUE)) {
        legacy_idx <- as.integer(sub("^expert_", "", name, ignore.case = TRUE))
        if (
          !is.na(legacy_idx) &&
            legacy_idx >= 1L &&
            legacy_idx <= length(self$personas)
        ) {
          return(legacy_idx)
        }
      }
      NULL
    },

    #' @description
    #' Process one step of the conversation.
    #' @param user_input User's input message.
    #' @param auto If TRUE and discourse manager is enabled, let the discourse manager decide.
    #' @return A list with speaker, answer, and mindmap_md.
    step = function(user_input = NULL, auto = FALSE) {
      user_input <- user_input %||% ""
      user_input <- tempest_trim(user_input)
      if (is.na(user_input) || user_input == "") {
        if (!isTRUE(auto)) return(invisible(NULL))
      }

      turn_id <- tempest_uuid("turn")
      turn_event <- self$emit_progress(
        "stage",
        "started",
        stage = "dialogue",
        step = "turn",
        correlation_id = turn_id,
        payload = list(auto = isTRUE(auto))
      )
      tryCatch(
        {
          # Auto mode: let discourse manager decide next turn
          if (isTRUE(auto) && !is.null(self$discourse_manager)) {
            decision <- self$discourse_manager$decide_next_turn(
              topic = self$topic,
              transcript_md = self$transcript_markdown(max_turns = 20),
              mindmap_md = tempest_mindmap_to_markdown(self$mindmap),
              persona_descriptions = self$get_persona_descriptions(),
              unseen_sources = if (
                isTRUE(self$config@enable_unseen_surfacing)
              ) {
                self$find_undiscussed_sources()
              } else {
                character()
              }
            )
            result <- self$execute_turn_decision(decision)
            self$emit_progress(
              "stage",
              "succeeded",
              stage = "dialogue",
              step = "turn",
              parent_event_id = turn_event@event_id,
              correlation_id = turn_id
            )
            return(result)
          }

          self$add_turn("user", "user", user_input)
          self$emit_progress(
            "step",
            "succeeded",
            stage = "dialogue",
            step = "user_turn",
            parent_event_id = turn_event@event_id,
            correlation_id = turn_id
          )

          # Build context for the moderator
          prompt <- paste0(
            "Topic: ",
            self$topic,
            "\n\n",
            "Mind map:\n",
            tempest_mindmap_to_markdown(self$mindmap),
            "\n\n",
            "Recent dialog:\n",
            self$transcript_markdown(max_turns = 30),
            "\n\n",
            "User question:\n",
            user_input,
            "\n\n",
            "You have tools to ask experts (ask_*) for research questions.\n",
            "Use the expert tools to delegate research questions to the appropriate expert.\n",
            "Synthesize their responses into a coherent answer for the user.\n",
            "Use citations like [Sxxxxxxxxxxxx] for factual claims."
          )

          # The moderator will use expert tools as needed
          moderator_event <- self$emit_progress(
            "step",
            "started",
            stage = "dialogue",
            step = "moderator_response",
            parent_event_id = turn_event@event_id,
            correlation_id = turn_id
          )
          ans <- self$chats$moderator$chat(prompt, echo = "none")
          turn <- tryCatch(
            self$chats$moderator$last_turn(),
            error = function(e) NULL
          )
          source_ids <- self$harvest_native_sources(turn = turn)
          self$add_turn("Moderator", "assistant", ans)
          self$emit_progress(
            "step",
            "succeeded",
            stage = "dialogue",
            step = "moderator_response",
            parent_event_id = moderator_event@event_id,
            correlation_id = turn_id
          )

          # Extract facts (best-effort)
          self$extract_facts(ans, turn = turn, source_ids = source_ids)

          # Update mind map
          self$update_mindmap(
            last_exchange = paste0("User: ", user_input, "\n\nModerator: ", ans)
          )

          result <- list(
            speaker = "Moderator",
            answer = ans,
            mindmap_md = self$artifacts[["mindmap_md"]] %||%
              tempest_mindmap_to_markdown(self$mindmap)
          )
          self$emit_progress(
            "stage",
            "succeeded",
            stage = "dialogue",
            step = "turn",
            parent_event_id = turn_event@event_id,
            correlation_id = turn_id
          )
          result
        },
        error = function(e) {
          self$emit_progress(
            "stage",
            "failed",
            stage = "dialogue",
            step = "turn",
            parent_event_id = turn_event@event_id,
            correlation_id = turn_id,
            payload = tempest_progress_error_payload(e)
          )
          stop(e)
        }
      )
    },

    #' @description
    #' Run a warmup phase where each expert researches their initial questions.
    #' This primes the knowledge base with foundational research before interactive Q&A.
    #' @param verbose If TRUE, prints progress messages.
    #' @return A list with results from each expert's warmup.
    warmup = function(verbose = TRUE) {
      warmup_event <- self$emit_progress(
        "stage",
        "started",
        stage = "warmup",
        step = "expert_fanout",
        payload = list(expert_count = length(self$personas))
      )
      tryCatch(
        {
          if (length(self$personas) == 0) {
            if (verbose) {
              tempest_inform("No personas available for warmup.")
            }
            self$emit_progress(
              "stage",
              "skipped",
              stage = "warmup",
              step = "expert_fanout",
              parent_event_id = warmup_event@event_id,
              correlation_id = warmup_event@correlation_id,
              payload = list(reason = "no_personas")
            )
            return(invisible(list()))
          }

          results <- list()

          for (i in seq_along(self$personas)) {
            persona <- self$personas[[i]]
            persona_name <- persona$name %||% paste("Expert", i)
            initial_qs <- persona$initial_questions %||% character()

            if (length(initial_qs) == 0) {
              if (verbose) {
                tempest_inform("Skipping {persona_name}: no initial questions")
              }
              self$emit_progress(
                "expert",
                "skipped",
                stage = "warmup",
                step = "expert_fanout",
                parent_event_id = warmup_event@event_id,
                correlation_id = warmup_event@correlation_id,
                payload = list(
                  expert_id = as.character(persona$id %||% i),
                  expert_name = persona_name,
                  reason = "no_initial_questions"
                )
              )
              next
            }

            if (verbose) {
              tempest_inform(
                "Warmup: {persona_name} ({length(initial_qs)} questions)"
              )
            }

            # Get or create expert session
            session_result <- self$expert_session_manager$get_or_create(persona)
            chat <- session_result$chat
            session_id <- session_result$session_id
            expert_event <- self$emit_progress(
              "expert",
              "started",
              stage = "warmup",
              step = "expert_fanout",
              parent_event_id = warmup_event@event_id,
              correlation_id = warmup_event@correlation_id,
              payload = list(
                expert_id = as.character(persona$id %||% i),
                expert_name = persona_name,
                session_id = session_id,
                question_count = length(initial_qs)
              )
            )

            expert_results <- list()
            tryCatch(
              {
                for (q_i in seq_along(initial_qs)) {
                  q <- initial_qs[[q_i]]
                  if (verbose) {
                    tempest_inform("  Q: {q}")
                  }
                  question_event <- self$emit_progress(
                    "tool",
                    "started",
                    stage = "warmup",
                    step = "expert_question",
                    parent_event_id = expert_event@event_id,
                    correlation_id = expert_event@correlation_id,
                    payload = list(
                      expert_id = as.character(persona$id %||% i),
                      expert_name = persona_name,
                      question_index = q_i
                    )
                  )

                  prompt <- paste0(
                    "Topic: ",
                    self$topic,
                    "\n\n",
                    "Question: ",
                    q,
                    "\n\n",
                    "Instructions:\n",
                    "- Use the available web/source tools to find and cite sources.\n",
                    "- If web_search and fetch_url are available, search first and then fetch sources.\n",
                    "- Only state factual claims supported by sources you inspected.\n",
                    "- For each factual sentence, add source IDs like [Sxxxxxxxxxxxx] when available.\n",
                    "- If evidence is weak or unclear, say so.\n\n",
                    "Respond now:"
                  )

                  response <- tryCatch(
                    {
                      response <- chat$chat(prompt, echo = "none")
                      turn <- tryCatch(
                        chat$last_turn(),
                        error = function(e) NULL
                      )
                      source_ids <- self$harvest_native_sources(turn = turn)

                      self$expert_session_manager$extract_facts(
                        response,
                        turn = turn,
                        source_ids = source_ids
                      )
                      self$add_turn(persona_name, "assistant", response)
                      self$update_mindmap(
                        last_exchange = paste0(
                          "Initial research by ",
                          persona_name,
                          ":\n",
                          "Q: ",
                          q,
                          "\n\n",
                          "A: ",
                          response
                        )
                      )

                      self$emit_progress(
                        "tool",
                        "succeeded",
                        stage = "warmup",
                        step = "expert_question",
                        parent_event_id = question_event@event_id,
                        correlation_id = question_event@correlation_id,
                        payload = list(
                          expert_id = as.character(persona$id %||% i),
                          expert_name = persona_name,
                          question_index = q_i
                        )
                      )
                      response
                    },
                    error = function(e) {
                      self$emit_progress(
                        "tool",
                        "failed",
                        stage = "warmup",
                        step = "expert_question",
                        parent_event_id = question_event@event_id,
                        correlation_id = question_event@correlation_id,
                        payload = c(
                          list(
                            expert_id = as.character(persona$id %||% i),
                            expert_name = persona_name,
                            question_index = q_i
                          ),
                          tempest_progress_error_payload(e)
                        )
                      )
                      stop(e)
                    }
                  )

                  expert_results <- c(
                    expert_results,
                    list(list(
                      question = q,
                      response = response
                    ))
                  )
                }
              },
              error = function(e) {
                self$emit_progress(
                  "expert",
                  "failed",
                  stage = "warmup",
                  step = "expert_fanout",
                  parent_event_id = expert_event@event_id,
                  correlation_id = expert_event@correlation_id,
                  payload = c(
                    list(
                      expert_id = as.character(persona$id %||% i),
                      expert_name = persona_name,
                      questions_answered = length(expert_results)
                    ),
                    tempest_progress_error_payload(e)
                  )
                )
                stop(e)
              }
            )

            self$emit_progress(
              "expert",
              "succeeded",
              stage = "warmup",
              step = "expert_fanout",
              parent_event_id = expert_event@event_id,
              correlation_id = expert_event@correlation_id,
              payload = list(
                expert_id = as.character(persona$id %||% i),
                expert_name = persona_name,
                questions_answered = length(expert_results)
              )
            )
            results[[persona_name]] <- list(
              session_id = session_id,
              questions_answered = length(expert_results),
              results = expert_results
            )
          }

          if (verbose) {
            total_facts <- length(self$store$list_claims())
            total_sources <- length(self$store$list_sources())
            tempest_inform(
              "Warmup complete: {total_facts} facts, {total_sources} sources"
            )
          }

          self$emit_progress(
            "stage",
            "succeeded",
            stage = "warmup",
            step = "expert_fanout",
            parent_event_id = warmup_event@event_id,
            correlation_id = warmup_event@correlation_id,
            payload = list(
              expert_count = length(results),
              claim_count = length(self$store$list_claims()),
              source_count = length(self$store$list_sources())
            )
          )
          invisible(results)
        },
        error = function(e) {
          self$emit_progress(
            "stage",
            "failed",
            stage = "warmup",
            step = "expert_fanout",
            parent_event_id = warmup_event@event_id,
            correlation_id = warmup_event@correlation_id,
            payload = tempest_progress_error_payload(e)
          )
          stop(e)
        }
      )
    },

    #' @description
    #' Generate a report from the session.
    #' @param style Report style: "technical" or "executive".
    #' @param include_references Include references section.
    #' @param reorganize Whether to reorganize mind map before generating.
    #' @return Markdown report string.
    report = function(
      style = c("technical", "executive"),
      include_references = TRUE,
      reorganize = TRUE
    ) {
      style <- match.arg(style)
      report_event <- self$emit_progress(
        "stage",
        "started",
        stage = "report",
        step = "generate",
        payload = list(style = style, include_references = include_references)
      )
      tryCatch(
        {
          # Reorganize mind map before report generation
          if (isTRUE(reorganize)) {
            self$reorganize_mindmap()
          }
          rep <- self$chats$reporter
          prompt <- paste0(
            "Write a comprehensive report based on the session.\n\n",
            "Topic: ",
            self$topic,
            "\n\n",
            "Mind map:\n",
            tempest_mindmap_to_markdown(self$mindmap),
            "\n\n",
            "Verified facts:\n",
            tempest_summarize_facts_for_prompt(self$store, max_items = 120),
            "\n\n",
            "Conversation (summary):\n",
            self$transcript_markdown(max_turns = 80),
            "\n\n",
            "Style: ",
            style,
            "\n\n",
            "Rules:\n",
            "- Use only verified facts (with citations).\n",
            "- Preserve citations like [Sxxxxxxxxxxxx].\n",
            "- Do not invent facts.\n\n",
            "Write the report body in Markdown (no title)."
          )
          body <- rep$chat(prompt, echo = "none")
          self$artifacts[["report"]] <- body
          md <- if (include_references) {
            tempest_report_md(self$title %||% self$topic, body, self$store)
          } else {
            body
          }
          self$artifacts[["report_md"]] <- md
          tempest_artifact_write(
            self$config@artifact_store,
            "report_md",
            md,
            metadata = list(topic = self$topic, style = style)
          )
          self$emit_progress(
            "artifact",
            "available",
            stage = "report",
            step = "report_md",
            parent_event_id = report_event@event_id,
            correlation_id = report_event@correlation_id,
            payload = list(artifact = "report_md")
          )
          self$emit_progress(
            "stage",
            "succeeded",
            stage = "report",
            step = "generate",
            parent_event_id = report_event@event_id,
            correlation_id = report_event@correlation_id
          )
          md
        },
        error = function(e) {
          self$emit_progress(
            "stage",
            "failed",
            stage = "report",
            step = "generate",
            parent_event_id = report_event@event_id,
            correlation_id = report_event@correlation_id,
            payload = tempest_progress_error_payload(e)
          )
          stop(e)
        }
      )
    },

    #' @description
    #' Add a new expert to the panel dynamically.
    #' @param area The area of expertise needed.
    #' @param name Optional name for the new expert.
    #' @return The new persona (invisibly).
    add_expert = function(area, name = NULL) {
      active <- self$get_active_personas()
      if (length(active) >= self$config@max_active_experts) {
        tempest_warn(
          "Maximum active experts ({self$config@max_active_experts}) reached."
        )
        return(invisible(NULL))
      }
      new_persona <- tempest_generate_single_persona(
        self$topic,
        area,
        self$personas,
        self$config
      )
      if (!is.null(name)) {
        new_persona$name <- name
      }
      new_persona$id <- length(self$personas) + 1L
      self$personas <- c(self$personas, list(new_persona))

      # Register new expert tool on moderator
      tempest_register_single_expert_tool(
        self$chats$moderator,
        new_persona,
        self$expert_session_manager,
        self$topic
      )
      invisible(new_persona)
    },

    #' @description
    #' Retire an expert from the panel.
    #' @param name The name of the expert to retire.
    #' @return Logical indicating success.
    retire_expert = function(name) {
      idx <- self$find_expert_index(name)
      if (is.null(idx)) {
        return(FALSE)
      }
      self$personas[[idx]]$retired <- TRUE
      TRUE
    },

    #' @description
    #' Get active (non-retired) personas.
    #' @return List of active persona objects.
    get_active_personas = function() {
      purrr::keep(self$personas, ~ !isTRUE(.x$retired))
    },

    #' @description
    #' Check and expand oversized mind map nodes.
    check_and_expand_nodes = function() {
      trigger <- self$config@node_expansion_trigger_count
      if (is.null(trigger)) {
        return(invisible(NULL))
      }

      oversized <- tempest_mindmap_oversized_nodes(self$mindmap, trigger)
      for (node_id in oversized) {
        self$mindmap <- tempest_mindmap_expand_node(
          self$chats$mindmap,
          self$mindmap,
          node_id
        )
      }
      if (length(oversized) > 0) {
        self$artifacts[["mindmap_md"]] <- tempest_mindmap_to_markdown(
          self$mindmap
        )
      }
      invisible(length(oversized))
    },

    #' @description
    #' Get source IDs that have been discussed in the transcript.
    #' @return Character vector of discussed source IDs.
    get_discussed_source_ids = function() {
      all_text <- paste(purrr::map_chr(self$transcript, "text"), collapse = " ")
      tempest_extract_citation_ids(all_text)
    },

    #' @description
    #' Find sources that haven't been discussed yet.
    #' @return Character vector of undiscussed source IDs.
    find_undiscussed_sources = function() {
      all_source_ids <- purrr::map_chr(self$store$list_sources(), "id")
      discussed <- self$get_discussed_source_ids()
      setdiff(all_source_ids, discussed)
    },

    #' @description
    #' Generate questions about undiscussed sources.
    #' @param max_questions Maximum questions to generate.
    #' @return Character vector of questions, or NULL if none.
    surface_unseen_information = function(max_questions = 3) {
      unseen_ids <- self$find_undiscussed_sources()
      if (length(unseen_ids) == 0) {
        return(NULL)
      }

      # Get snippets from unseen sources
      unseen_info <- purrr::map_chr(head(unseen_ids, 5), function(id) {
        src <- self$store$get_source(id)
        if (is.null(src)) {
          return("")
        }
        paste0(
          "[",
          id,
          "] ",
          src$title %||% "",
          ": ",
          substr(src$snippet %||% "", 1, 200)
        )
      })
      unseen_info <- unseen_info[nzchar(unseen_info)]
      if (length(unseen_info) == 0) {
        return(NULL)
      }

      prompt <- paste0(
        "Topic: ",
        self$topic,
        "\n\n",
        "Current mind map:\n",
        tempest_mindmap_to_markdown(self$mindmap),
        "\n\n",
        "These sources have NOT been discussed yet:\n",
        paste(unseen_info, collapse = "\n"),
        "\n\n",
        "Generate ",
        max_questions,
        " targeted questions that would surface important information from these undiscussed sources.\n",
        "Return each question on a new line."
      )

      response <- self$chats$moderator$chat(prompt, echo = "none")
      questions <- strsplit(tempest_trim(response), "\n")[[1]]
      questions <- tempest_trim(questions)
      questions <- questions[nzchar(questions)]
      head(questions, max_questions)
    },

    #' @description
    #' Reorganize the mind map for clarity.
    reorganize_mindmap = function() {
      mm_chat <- self$chats$mindmap
      type <- tempest_type_mindmap()
      prompt <- paste0(
        "Reorganize this mind map for clarity and coherence.\n\n",
        "Topic: ",
        self$topic,
        "\n\n",
        "Current mind map:\n",
        tempest_mindmap_to_markdown(self$mindmap),
        "\n\n",
        "Rules:\n",
        "- Merge duplicate or overlapping nodes.\n",
        "- Improve hierarchy and logical grouping.\n",
        "- Preserve ALL citations and source_ids exactly.\n",
        "- Do not fabricate sources.\n\n",
        "Return the reorganized mind map as structured data."
      )
      new_mm <- tryCatch(
        mm_chat$chat_structured(
          prompt,
          type = type,
          echo = "none",
          convert = FALSE
        ),
        error = function(e) {
          tempest_warn("Mind map reorganization failed: {conditionMessage(e)}")
          NULL
        }
      )
      if (
        !is.null(new_mm) && !is.null(new_mm$nodes) && length(new_mm$nodes) > 0
      ) {
        self$mindmap <- new_mm
        self$artifacts[["mindmap_md"]] <- tempest_mindmap_to_markdown(new_mm)
      }
      invisible(TRUE)
    },

    #' @description
    #' Execute a discourse manager turn decision.
    #' @param decision A turn decision from the discourse manager.
    #' @return A list with speaker, answer, and mindmap_md.
    execute_turn_decision = function(decision) {
      action <- decision$action %||% "moderator_probes"
      instruction <- decision$instruction %||% ""
      agent_name <- decision$agent_name %||% ""

      if (action == "add_expert") {
        new_persona <- self$add_expert(area = instruction)
        if (is.null(new_persona)) {
          msg <- paste0(
            "Could not add expert: maximum active experts (",
            self$config@max_active_experts,
            ") reached."
          )
          self$add_turn("System", "assistant", msg)
          return(list(
            speaker = "System",
            answer = msg,
            mindmap_md = self$mindmap_markdown()
          ))
        }
        self$add_turn(
          "System",
          "assistant",
          paste0(
            "Added new expert: ",
            new_persona$name,
            " (",
            new_persona$title,
            ")"
          )
        )
        return(list(
          speaker = "System",
          answer = paste0("Added expert: ", new_persona$name),
          mindmap_md = self$mindmap_markdown()
        ))
      }

      if (action == "retire_expert") {
        success <- self$retire_expert(agent_name)
        if (success) {
          self$add_turn(
            "System",
            "assistant",
            paste0("Retired expert: ", agent_name)
          )
          return(list(
            speaker = "System",
            answer = paste0("Retired expert: ", agent_name),
            mindmap_md = self$mindmap_markdown()
          ))
        } else {
          tempest_warn(
            "Discourse manager tried to retire unknown expert: {.val {agent_name}}"
          )
          self$add_turn(
            "System",
            "assistant",
            paste0("Expert not found: ", agent_name)
          )
          return(list(
            speaker = "System",
            answer = paste0("Expert not found: ", agent_name),
            mindmap_md = self$mindmap_markdown()
          ))
        }
      }

      if (action == "surface_unseen") {
        questions <- self$surface_unseen_information(max_questions = 3)
        if (is.null(questions) || length(questions) == 0) {
          return(list(
            speaker = "System",
            answer = "No undiscussed sources to surface.",
            mindmap_md = self$mindmap_markdown()
          ))
        }
        # Ask the first question as if the user asked it
        return(self$step(questions[[1]]))
      }

      if (action == "end_round") {
        self$add_turn(
          "System",
          "assistant",
          "Round ended by discourse manager."
        )
        return(list(
          speaker = "System",
          answer = "Round complete.",
          mindmap_md = self$mindmap_markdown()
        ))
      }

      # expert_speaks or moderator_probes: route through normal step
      self$step(instruction)
    }
  )
)

#' Create a Co-STORM session
#'
#' @param topic Topic string.
#' @param config A `TempestConfig`.
#' @param n_experts Number of expert agents.
#' @param personas Optional list of pre-generated personas. If NULL,
#'   personas are generated automatically.
#' @param retriever Optional `TempestRetriever` or compatible retriever object
#'   with a `SourceStore` at `$store`.
#' @param progress Optional function called with `tempest_progress_event`
#'   objects as the session makes progress.
#' @examples
#' \dontrun{
#' session <- tempest_session("History of jazz", config = tempest_config())
#' session$step("What styles emerged in the 1950s?")
#' }
#' @export
tempest_session <- function(
  topic,
  config = tempest_config(),
  n_experts = 3,
  personas = NULL,
  retriever = NULL,
  progress = NULL
) {
  TempestSession$new(
    topic = topic,
    config = config,
    n_experts = n_experts,
    personas = personas,
    retriever = retriever,
    progress = progress
  )
}
