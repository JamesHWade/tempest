# Co-STORM session class and constructors

TempestSession <- R6::R6Class(
  "TempestSession",
  public = list(
    initialize = function(
      topic,
      config = tempest_config(),
      n_experts = 3,
      experts = NULL,
      retriever = NULL,
      progress = NULL,
      session_id = NULL,
      program_set = NULL,

      .restore_manifest = NULL,
      .restore_token = NULL,
      .admit_knowledge = NULL,
      .artifact_selection = NULL
    ) {
      restoring <- identical(.restore_token, tempest_costorm_restore_token)
      if (
        (!is.null(.restore_manifest) || !is.null(.restore_token)) &&
          !restoring
      ) {
        tempest_costorm_session_abort(
          paste0(
            "Research manifests can be supplied only through Tempest's ",
            "internal session-restoration seam."
          )
        )
      }
      if (restoring && is.null(.restore_manifest)) {
        tempest_costorm_session_abort(
          "Internal session restoration requires a research manifest."
        )
      }
      manifest <- if (restoring) .restore_manifest else NULL
      if (!is.character(topic) || length(topic) != 1L || is.na(topic)) {
        tempest_config_abort("{.arg topic} must be a single non-empty string.")
      }
      private$topic_value <- tempest_trim(topic)
      if (!nzchar(private$topic_value)) {
        tempest_config_abort("{.arg topic} must be a single non-empty string.")
      }
      if (!S7::S7_inherits(config, TempestConfig)) {
        tempest_config_abort(
          "{.arg config} must be created by {.fn tempest_config}."
        )
      }
      program_set <- program_set %||% tempest_program_set()

      program_references <- tempest_program_set_manifest_programs(program_set)
      if (is.null(experts)) {
        n_experts <- tempest_config_count(n_experts, "n_experts")
        if (n_experts > config@max_active_experts) {
          tempest_config_abort(
            c(
              "Expert request exceeds the configured budget.",
              x = "Requested {n_experts}; maximum is {config@max_active_experts}."
            )
          )
        }
      }
      if (!is.null(experts)) {
        experts <- tempest_validate_experts(experts)
        if (length(experts) > config@max_active_experts) {
          tempest_config_abort(
            "{.arg experts} exceeds {.arg max_active_experts}."
          )
        }
      }
      private$config_value <- config
      private$title_value <- private$topic_value
      if (
        !is.null(manifest) &&
          !S7::S7_inherits(
            manifest,
            TempestResearchManifest
          )
      ) {
        tempest_costorm_session_abort(
          "{.arg manifest} must be created by {.fn tempest_research_manifest}."
        )
      }
      if (is.null(session_id) && !is.null(manifest)) {
        session_id <- manifest@research_run_id
      } else if (is.null(session_id)) {
        session_id <- tempest_uuid("session")
      } else if (
        !rlang::is_string(session_id) || !nzchar(tempest_trim(session_id))
      ) {
        tempest_costorm_session_abort(
          "{.arg session_id} must be a single non-empty string or {.code NULL}."
        )
      } else {
        session_id <- tempest_trim(session_id)
      }
      private$progress_value <- tempest_progress_callback(progress)
      if (is.null(retriever)) {
        private$workspace_value <- tempest_research_workspace()
        private$retriever_value <- tempest_retriever(
          config = config,
          workspace = private$workspace_value
        )
      } else {
        retriever_config_digest <- tempest_retriever_config_digest(retriever)
        if (
          !is.null(retriever_config_digest) &&
            !identical(
              retriever_config_digest,
              tempest_research_config_digest(config)
            )
        ) {
          tempest_costorm_session_abort(c(
            "{.arg retriever} does not match the supplied {.arg config}.",
            x = paste0(
              "A TempestRetriever must be created from the same ",
              "behavior-relevant configuration."
            )
          ))
        }
        private$retriever_value <- retriever
        private$workspace_value <- tempest_costorm_retriever_workspace(
          retriever
        )
      }

      tempest_retriever_bind_workspace_budget(
        private$retriever_value,
        private$workspace_value,
        config
      )

      tempest_research_workspace_verification_owner_preflight(
        private$workspace_value,
        restoring = restoring
      )
      if (!is.null(.admit_knowledge)) {
        .admit_knowledge(private$workspace_value)
      }
      private$manifest_value <- if (is.null(manifest)) {
        tempest_research_manifest(
          research_run_id = session_id,
          mode = "costorm",
          config = config,
          programs = program_references,
          artifact_selection = .artifact_selection %||%
            private$workspace_value$artifact_selection,

          runtime = list(),
          traces = list(),
          deliverables = list(),
          status = "running"
        )
      } else {
        tempest_costorm_manifest_validate(
          manifest,
          session_id,
          config,
          private$workspace_value
        )
      }
      private$deputy_traces_value <- tempest_costorm_manifest_deputy_traces(
        private$manifest_value
      )
      private$pending_deputy_runs_value <- list()
      private$agent_completion_owner_value <- new.env(parent = emptyenv())
      private$agent_completion_registry_value <-
        tempest_agent_completion_registry(
          private$agent_completion_owner_value
        )
      private$programs_value <- tempest_bind_program_set(
        program_set,
        private$manifest_value
      )
      private$program_set_value <- program_set
      private$session_id_value <- private$manifest_value@research_run_id
      private$transcript_value <- list()
      private$mindmap_value <- tempest_mindmap_init(self$topic)
      private$events_value <- list()
      private$report_md_value <- NULL
      private$suggestions_value <- character()
      private$stage_records_value <- list()
      private$async_work_value <- new.env(hash = TRUE, parent = emptyenv())
      private$otel_completion_owner_value <- tempest_otel_owner()

      # Generate or use selected expert profiles.
      if (is.null(experts)) {
        private$experts_value <- tempest_generate_experts_with_program(
          topic = self$topic,
          n = n_experts,
          config = config,
          verbose = FALSE,
          module = private$programs_value$personas,
          record_stage = function(record, output = NULL) {
            tempest_session_record_stage(
              self,
              record,
              output,
              commit = if (is.null(output)) {
                NULL
              } else {
                function() {
                  private$experts_value <- tempest_validate_experts(output)
                }
              }
            )
          }
        )
      } else {
        private$experts_value <- experts
      }
      if (length(self$experts) > config@max_active_experts) {
        tempest_config_abort(
          "{.arg experts} exceeds {.arg max_active_experts}."
        )
      }

      private$chats_value <- list(
        moderator = tempest_make_chat(
          config,
          "coordinator",
          system_prompt = tempest_moderator_system_prompt(
            self$topic,
            self$experts
          )
        ),
        extractor = tempest_make_chat(
          config,
          "judge",
          system_prompt = tempest_prompt("fact_extractor_system")
        ),
        next_question = tempest_make_chat(
          config,
          "coordinator",
          system_prompt = tempest_prompt("question_suggester_system")
        )
      )

      private$expert_manager_value <- TempestDeputyExpertManager$new(
        experts = self$experts,
        config = config,
        retriever = private$retriever_value,
        extractor = private$chats_value$extractor,
        workspace = private$workspace_value,
        progress = function(event) private$record_progress_event(event),
        run_id = self$session_id,
        extract_claims_program = private$programs_value$extract_claims,
        stage_recorder = function(record, output = NULL) {
          tempest_session_record_stage(self, record, output)
        },
        manifest = private$manifest_value,
        completion_registry = private$agent_completion_registry_value,
        completion_owner = private$agent_completion_owner_value,
        on_start = function(pending_run) {
          tempest_session_start_deputy_run(self, pending_run)
        },
        on_completion = function(completion) {
          tempest_session_settle_agent_completion(self, completion)
        },
        on_terminal = function(terminal) {
          tempest_session_settle_agent_terminal(self, terminal)
        }
      )

      tempest_research_attach_tools(
        private$chats_value$moderator,
        retriever = private$retriever_value,
        role = "coordinator",
        model = tempest_research_model(private$config_value, "coordinator"),
        search_provider = private$config_value@search_provider,
        max_search_results = private$config_value@max_search_results
      )
      private$chats_value$moderator$register_tool(
        tempest_create_deputy_expert_delegation_tool(
          private$expert_manager_value,
          self$topic,
          self$experts
        )
      )
      private$chats_value$moderator <- tryCatch(
        tempest_deputy_chat_adapter(
          private$chats_value$moderator,
          manifest = private$manifest_value,
          deputy_session_id = tempest_costorm_deputy_session_id(
            self$session_id,
            "moderator"
          ),
          agent_name = "Tempest moderator",
          stage = "dialogue",
          role = "moderator",
          completion_registry = private$agent_completion_registry_value,
          on_start = function(pending_run) {
            tempest_session_start_deputy_run(self, pending_run)
          },
          on_completion = function(completion) {
            tempest_session_settle_agent_completion(self, completion)
          },
          on_terminal = function(terminal) {
            tempest_session_settle_agent_terminal(self, terminal)
          }
        ),
        error = function(error) {
          parent <- if (tempest_condition_chain_sensitive(error)) {
            tempest_condition_safe_summary(error)
          } else {
            error
          }
          tempest_costorm_session_abort(
            "The moderator Deputy execution session could not be created.",
            parent = parent
          )
        }
      )
      private$chats_value$moderator <- tempest_otel_wrap_completion_client(
        private$chats_value$moderator,
        private$otel_completion_owner_value
      )

      private$emit_progress(
        "workflow",
        "started",
        stage = "session",
        step = "created",
        payload = list(expert_count = length(self$experts))
      )

      private$verification_owner_token_value <-
        tempest_research_workspace_bind_verification_owner(
          private$workspace_value
        )

      invisible(self)
    },

    suggest_questions = function(n = 4) {
      tempest_session_assert_mutable(self, "generate suggestions")
      tempest_costorm_await(tempest_session_suggest_questions_async(self, n))
    },

    step = function(user_input = NULL) {
      tempest_session_assert_mutable(self, "process a dialogue turn")
      tempest_session_async_work_assert_startable(self, "dialogue")
      user_input <- tempest_trim(user_input %||% "")
      if (is.na(user_input) || !nzchar(user_input)) {
        return(invisible(NULL))
      }
      response <- private$chats_value$moderator$chat(
        user_input,
        echo = "none"
      )
      completion_id <- tempest_agent_completion_id(response)
      turn <- tempest_costorm_await(tempest_session_process_turn_async(
        self,
        completion_id,
        suggest = FALSE
      ))
      list(
        speaker = "Moderator",
        answer = as.character(response),
        mindmap_md = private$mindmap_markdown(),
        deputy_run_id = turn@deputy_run_id,
        deputy_session_id = turn@deputy_session_id
      )
    },

    warmup = function(verbose = TRUE) {
      tempest_session_assert_mutable(self, "warm up experts")
      verbose <- tempest_product_flag(verbose, "verbose")
      result <- tempest_costorm_await(tempest_session_warmup_async(self))
      if (verbose) {
        tempest_inform(
          "Warmup complete: {result@claim_count} claims, {result@source_count} sources"
        )
      }
      invisible(result)
    },

    publish = function(
      style = c("technical", "executive"),
      include_references = TRUE
    ) {
      tempest_costorm_await(tempest_session_report_async(
        self,
        style = match.arg(style),
        include_references = include_references
      ))
    },

    add_expert = function(area, name = NULL) {
      tempest_session_assert_mutable(self, "add an expert")
      active <- private$get_active_experts()
      if (length(active) >= private$config_value@max_active_experts) {
        tempest_warn(
          "Maximum active experts ({private$config_value@max_active_experts}) reached."
        )
        return(invisible(NULL))
      }
      committed_expert <- NULL
      new_expert <- tempest_generate_single_expert(
        self$topic,
        area,
        self$experts,
        private$config_value,
        module = private$programs_value$personas,
        record_stage = function(record, output = NULL) {
          if (!is.null(output)) {
            output <- tempest_validate_experts(output)
            if (!is.null(name)) {
              output[[1L]] <- tempest_expert_update(
                output[[1L]],
                name = name
              )
              record <- S7::set_props(
                record,
                output_reference = tempest_stage_output_reference(
                  "state_field",
                  "experts",
                  content_digest = tempest_stage_state_output_digest(
                    "personas",
                    output
                  )
                )
              )
            }
            committed_expert <<- output[[1L]]
          }
          tempest_session_record_stage(self, record, output)
        }
      )
      if (!is.null(committed_expert)) {
        new_expert <- committed_expert
      }
      private$experts_value <- tempest_validate_experts(c(
        private$experts_value,
        list(new_expert)
      ))
      private$expert_manager_value$add_expert(new_expert)
      invisible(new_expert)
    },

    retire_expert = function(expert_id) {
      tempest_session_assert_mutable(self, "retire an expert")
      idx <- private$find_expert(expert_id)
      if (is.null(idx)) {
        return(FALSE)
      }
      private$expert_manager_value$retire_expert(expert_id)
      TRUE
    }
  ),
  active = list(
    session_id = function(value) {
      if (!missing(value)) {
        tempest_costorm_session_abort(
          "{.field session_id} is fixed when the session is created."
        )
      }
      private$session_id_value
    },
    topic = function(value) {
      if (!missing(value)) {
        tempest_costorm_session_abort(
          "{.field topic} is fixed when the session is created."
        )
      }
      private$topic_value
    },
    status = function(value) {
      if (!missing(value)) {
        tempest_costorm_session_abort(
          "{.field status} is a read-only product projection."
        )
      }
      private$manifest_value@status
    },
    experts = function(value) {
      if (!missing(value)) {
        tempest_costorm_session_abort(
          "{.field experts} is read-only; use the expert roster methods."
        )
      }
      rlang::duplicate(private$experts_value, shallow = FALSE)
    },
    transcript = function(value) {
      if (!missing(value)) {
        tempest_costorm_session_abort(
          "{.field transcript} is read-only; use session turn methods."
        )
      }
      rlang::duplicate(private$transcript_value, shallow = FALSE)
    },
    mindmap = function(value) {
      if (!missing(value)) {
        tempest_costorm_session_abort(
          "{.field mindmap} is a read-only product projection."
        )
      }
      rlang::duplicate(private$mindmap_value, shallow = FALSE)
    }
  ),
  private = list(
    request_completion_async = function(
      prompt,
      on_chunk = function(chunk) invisible(chunk)
    ) {
      tempest_require(
        "promises",
        "Async completion requests require promises."
      )
      if (!is.function(on_chunk)) {
        tempest_agent_completion_binding_abort()
      }
      tempest_session_assert_mutable(self, "request a moderator completion")
      tempest_session_async_work_assert_startable(self, "dialogue")
      stream <- private$chats_value$moderator$stream_async(prompt)
      completion_id <- tempest_agent_completion_id(stream)
      coro::async(function() {
        repeat {
          content <- stream()
          if (promises::is.promising(content)) {
            content <- coro::await(content)
          }
          if (coro::is_exhausted(content)) {
            break
          }
          tryCatch(
            on_chunk(content),
            error = function(error) invisible(NULL)
          )
        }
        completion_id
      })()
    },
    record_progress_event = function(event) {
      if (!S7::S7_inherits(event, tempest_progress_event)) {
        tempest_abort(
          "{.arg event} must be a tempest_progress_event object."
        )
      }
      event_data <- tempest_progress_event_data(event)
      event_data$sequence <- length(private$events_value) + 1L
      private$events_value[[event_data$sequence]] <- event_data
      if (!is.null(private$progress_value)) {
        tryCatch(
          private$progress_value(event),
          error = function(error) {
            tempest_rethrow_operation(
              error,
              class = "tempest_progress_callback_error"
            )
          }
        )
      }
      invisible(event)
    },
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
      event <- tempest_emit_progress(
        NULL,
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
      private$record_progress_event(event)
    },
    add_turn = function(speaker, role = c("user", "assistant"), text) {
      role <- match.arg(role)
      if (identical(role, "assistant")) {
        tempest_costorm_session_abort(
          "Assistant turns can be committed only from an owned completion."
        )
      }
      tempest_session_assert_mutable(self, "add a transcript turn")
      tempest_session_append_transcript(self, speaker, role, text)
    },
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
    get_expert_names = function() {
      purrr::map_chr(
        self$experts,
        \(expert) expert@name
      )
    },
    get_expert_descriptions = function() {
      descs <- purrr::map_chr(self$experts, function(expert) {
        paste0(
          "- **",
          expert@name,
          "** [",
          expert@expert_id,
          "] (",
          expert@title,
          "): ",
          expert@description
        )
      })
      paste(descs, collapse = "\n")
    },
    update_mindmap = function(last_exchange) {
      tempest_session_assert_mutable(self, "update the mind map")
      tempest_agent_completion_text(last_exchange)
      event <- private$emit_progress(
        "step",
        "started",
        stage = "mindmap",
        step = "update"
      )
      tempest_session_commit_mindmap(
        self,
        tempest_costorm_mindmap_projection(self)
      )
      private$emit_progress(
        "step",
        "succeeded",
        stage = "mindmap",
        step = "update",
        parent_event_id = event@event_id,
        correlation_id = event@correlation_id,
        payload = list(node_count = length(self$mindmap$nodes %||% list()))
      )
      invisible(TRUE)
    },
    mindmap_markdown = function() {
      tempest_mindmap_to_markdown(self$mindmap)
    },
    find_expert = function(expert_id) {
      expert_ids <- purrr::map_chr(self$experts, \(expert) expert@expert_id)
      index <- match(expert_id, expert_ids)
      if (is.na(index)) NULL else index
    },
    get_active_experts = function() {
      private$expert_manager_value$list_experts(active_only = TRUE)
    },
    check_and_expand_nodes = function() {
      tempest_session_assert_mutable(self, "project the mind map")
      tempest_session_commit_mindmap(
        self,
        tempest_costorm_mindmap_projection(self)
      )
      invisible(0L)
    },
    get_discussed_source_ids = function() {
      all_text <- paste(purrr::map_chr(self$transcript, "text"), collapse = " ")
      tempest_extract_citation_ids(all_text)
    },
    find_undiscussed_sources = function() {
      all_source_ids <- purrr::map_chr(
        private$workspace_value$list_retrieved_sources(),
        "id"
      )
      discussed <- private$get_discussed_source_ids()
      setdiff(all_source_ids, discussed)
    },
    surface_unseen_information = function(max_questions = 3) {
      tempest_session_assert_mutable(self, "surface unseen information")
      max_questions <- tempest_config_count(max_questions, "max_questions")
      unseen_ids <- private$find_undiscussed_sources()
      if (length(unseen_ids) == 0) {
        return(NULL)
      }

      # Get snippets from unseen sources
      unseen_info <- purrr::map_chr(head(unseen_ids, 5), function(id) {
        src <- private$workspace_value$get_retrieved_source(id)
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

      correlation_id <- tempest_uuid("unseen")
      response <- private$chats_value$moderator$chat(
        prompt,
        echo = "none",
        run_context = list(
          correlation_id = correlation_id,
          role = "moderator",
          stage = "dialogue"
        )
      )
      completion_id <- tempest_agent_completion_id(response)
      result <- tempest_costorm_await(tempest_session_process_turn_async(
        self,
        completion_id,
        suggest = FALSE
      ))
      questions <- strsplit(tempest_trim(response), "\n")[[1]]
      questions <- tempest_trim(questions)
      questions <- questions[nzchar(questions)]
      trace <- Filter(
        \(candidate) {
          identical(
            candidate$deputy_run_id,
            result@deputy_run_id
          )
        },
        private$deputy_traces_value
      )
      if (length(trace) != 1L) {
        tempest_costorm_session_abort(
          "The unseen-information turn has no exact Deputy trace."
        )
      }
      list(
        questions = unname(head(questions, max_questions)),
        correlation_id = trace[[1L]]$correlation_id,
        deputy_run_id = result@deputy_run_id,
        deputy_session_id = result@deputy_session_id
      )
    },
    reorganize_mindmap = function() {
      tempest_session_assert_mutable(self, "reorganize the mind map")
      tempest_session_commit_mindmap(
        self,
        tempest_costorm_mindmap_projection(self)
      )
      invisible(TRUE)
    },
    title_value = NULL,
    progress_value = NULL,
    experts_value = list(),
    transcript_value = list(),
    mindmap_value = NULL,
    events_value = list(),
    topic_value = NULL,
    config_value = NULL,
    session_id_value = NULL,
    retriever_value = NULL,
    manifest_value = NULL,
    programs_value = NULL,
    program_set_value = NULL,
    workspace_value = NULL,
    chats_value = NULL,
    expert_manager_value = NULL,
    suggestions_value = character(),
    async_work_value = NULL,
    report_md_value = NULL,
    stage_records_value = list(),
    deputy_traces_value = list(),
    pending_deputy_runs_value = list(),
    agent_completion_owner_value = NULL,
    agent_completion_registry_value = NULL,
    otel_completion_owner_value = NULL,
    verification_owner_token_value = NULL
  )
)

#' Create a Co-STORM session
#'
#' @param topic Topic string.
#' @param config A `TempestConfig`.
#' @param n_experts Number of expert agents.
#' @param experts Optional list of validated expert profiles. If `NULL`,
#'   experts are generated automatically.
#' @param retriever Optional `TempestRetriever` or compatible retriever object
#'   with a `ResearchWorkspace` at `$workspace` and `search(query, k)` and
#'   `fetch(url)` methods. `search()` returns a data frame with `title` and
#'   HTTP/HTTPS `url` columns; `snippet` is optional and Tempest derives or
#'   verifies `source_id`. Create the workspace with
#'   [tempest_research_workspace()] and source IDs with [tempest_source_id()].
#' @param progress Optional function called with `tempest_progress_event`
#'   objects as the session makes progress.
#' @param session_id Optional stable session identifier. If `NULL`, a new
#'   identifier is generated.
#' @param knowledge Optional accepted organizational knowledge from
#'   [tempest_artifact_knowledge()]. It supplies accepted
#'   evidence records without selecting or authorizing executable programs.
#' @return A `TempestSession` R6 object for the active Co-STORM research
#'   session.
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
  experts = NULL,
  retriever = NULL,
  progress = NULL,
  session_id = NULL,
  knowledge = NULL
) {
  tempest_session_with_knowledge(
    topic = topic,
    config = config,
    n_experts = n_experts,
    experts = experts,
    retriever = retriever,
    progress = progress,
    session_id = session_id,
    knowledge = knowledge
  )
}

tempest_session_with_knowledge <- function(
  topic,
  config = tempest_config(),
  n_experts = 3,
  experts = NULL,
  retriever = NULL,
  progress = NULL,
  session_id = NULL,
  knowledge = NULL,
  program_set = NULL
) {
  supplied_knowledge <- knowledge
  knowledge <- tempest_knowledge_argument(knowledge, admit = FALSE)
  session <- tempest_session_new(
    topic = topic,
    config = config,
    n_experts = n_experts,
    experts = experts,
    retriever = retriever,
    progress = progress,
    session_id = session_id,
    program_set = program_set %||% knowledge$program_set,

    .artifact_selection = knowledge$artifact_selection,
    .admit_knowledge = function(workspace) {
      tempest_knowledge_workspace_preflight(
        workspace,
        knowledge$artifact_selection
      )
      tempest_knowledge_argument(supplied_knowledge)
    }
  )
  tempest_knowledge_insert_records(
    tempest_session_workspace(session),
    knowledge$records,
    knowledge$artifact_selection
  )
  session
}

tempest_session_new <- function(
  topic,
  config = tempest_config(),
  n_experts = 3,
  experts = NULL,
  retriever = NULL,
  progress = NULL,
  session_id = NULL,
  program_set = NULL,

  .admit_knowledge = NULL,
  .artifact_selection = NULL
) {
  TempestSession$new(
    topic = topic,
    config = config,
    n_experts = n_experts,
    experts = experts,
    retriever = retriever,
    progress = progress,
    session_id = session_id,
    program_set = program_set,

    .admit_knowledge = .admit_knowledge,
    .artifact_selection = .artifact_selection
  )
}

tempest_session_restore_new <- function(
  topic,
  config = tempest_config(),
  n_experts = 3,
  experts = NULL,
  retriever = NULL,
  progress = NULL,
  session_id = NULL,
  program_set = NULL,

  manifest,
  .admit_knowledge = NULL
) {
  TempestSession$new(
    topic = topic,
    config = config,
    n_experts = n_experts,
    experts = experts,
    retriever = retriever,
    progress = progress,
    session_id = session_id,
    program_set = program_set,

    .restore_manifest = manifest,
    .restore_token = tempest_costorm_restore_token,
    .admit_knowledge = .admit_knowledge
  )
}
