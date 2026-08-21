# Product-owned Deputy expert sessions and delegation.

tempest_deputy_expert_abort <- function(message, ..., parent = NULL) {
  tempest_abort(
    message,
    ...,
    class = c(
      "tempest_deputy_expert_error",
      "tempest_session_error",
      "tempest_error"
    ),
    parent = parent,
    .envir = rlang::caller_env()
  )
}

tempest_deputy_expert_session_id <- function(run_id, expert_id) {
  paste0(
    "expert-session_",
    substr(
      digest::digest(
        paste(run_id, expert_id, tempest_uuid("expert"), sep = "|"),
        algo = "sha256",
        serialize = FALSE
      ),
      1L,
      16L
    )
  )
}

tempest_deputy_expert_prompt <- function(expert) {
  tempest_render_expert_prompt(expert, expert_id = expert@expert_id)
}

tempest_deputy_expert_exact_scalar <- function(value, field) {
  if (
    !is.character(value) ||
      is.object(value) ||
      !is.null(attributes(value)) ||
      length(value) != 1L ||
      is.na(value) ||
      !nzchar(value) ||
      !identical(value, tempest_trim(value))
  ) {
    tempest_deputy_expert_abort(
      "Expert session field {.field {field}} must be one exact scalar string."
    )
  }
  value
}

TempestDeputyExpertManager <- R6::R6Class(
  "TempestDeputyExpertManager",
  public = list(
    initialize = function(
      experts,
      config,
      retriever,
      extractor,
      extract_claims_program,
      workspace,
      progress,
      run_id,
      stage_recorder,
      manifest,
      completion_registry,
      completion_owner,
      on_start,
      on_completion,
      on_terminal
    ) {
      experts <- tryCatch(
        tempest_validate_experts(experts),
        error = function(error) {
          tempest_deputy_expert_abort(
            "{.arg experts} must contain validated scientific expert profiles.",
            parent = error
          )
        }
      )
      if (!S7::S7_inherits(config, TempestConfig)) {
        tempest_deputy_expert_abort(
          "{.arg config} must be created by {.fn tempest_config}."
        )
      }
      if (!inherits(retriever, "TempestRetriever")) {
        tempest_deputy_expert_abort(
          "{.arg retriever} must be a TempestRetriever."
        )
      }
      if (!inherits(workspace, "ResearchWorkspace")) {
        tempest_deputy_expert_abort(
          "{.arg workspace} must be a ResearchWorkspace."
        )
      }
      extract_claims_program <- tempest_dsprrr_execution_require(
        extract_claims_program,
        "fact extraction"
      )
      if (!S7::S7_inherits(manifest, TempestResearchManifest)) {
        tempest_deputy_expert_abort(
          "{.arg manifest} must be a TempestResearchManifest."
        )
      }
      callbacks <- list(
        progress = progress,
        stage_recorder = stage_recorder,
        on_start = on_start,
        on_completion = on_completion,
        on_terminal = on_terminal
      )
      invalid <- names(callbacks)[!vapply(callbacks, is.function, logical(1))]
      if (length(invalid) > 0L) {
        tempest_deputy_expert_abort(
          "Deputy expert callbacks must all be functions."
        )
      }
      tempest_agent_completion_assert_owner(
        completion_registry,
        completion_owner
      )

      private$config <- config
      private$retriever <- retriever
      private$extractor <- extractor
      private$extract_claims_program <- extract_claims_program
      private$workspace <- workspace
      private$progress <- progress
      private$run_id <- tempest_product_scalar(run_id, "run_id")
      private$stage_recorder <- stage_recorder
      private$manifest <- manifest
      private$completion_registry <- completion_registry
      private$completion_owner <- completion_owner
      private$on_start <- on_start
      private$on_completion <- on_completion
      private$on_terminal <- on_terminal
      private$experts <- new.env(hash = TRUE, parent = emptyenv())
      private$retired_experts <- new.env(hash = TRUE, parent = emptyenv())
      private$sessions <- new.env(hash = TRUE, parent = emptyenv())
      private$bindings <- new.env(hash = TRUE, parent = emptyenv())
      for (expert in experts) {
        assign(expert@expert_id, expert, private$experts)
      }
      invisible(self)
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
      tempest_emit_progress(
        private$progress,
        run_id = private$run_id,
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

    profile = function(expert_id, active_only = TRUE) {
      expert_id <- tempest_research_expert_id(expert_id, "expert_id")
      active_only <- tempest_product_flag(active_only, "active_only")
      if (!exists(expert_id, private$experts, inherits = FALSE)) {
        tempest_deputy_expert_abort(
          "Expert {.val {expert_id}} is not in the live roster."
        )
      }
      expert <- get(expert_id, private$experts, inherits = FALSE)
      if (
        active_only &&
          exists(expert_id, private$retired_experts, inherits = FALSE)
      ) {
        tempest_deputy_expert_abort(
          "Expert {.val {expert_id}} is retired and cannot execute."
        )
      }
      expert
    },

    list_experts = function(active_only = TRUE) {
      active_only <- tempest_product_flag(active_only, "active_only")
      ids <- sort(ls(private$experts, all.names = TRUE), method = "radix")
      experts <- lapply(ids, get, envir = private$experts, inherits = FALSE)
      if (active_only) {
        experts <- Filter(
          \(expert) {
            !exists(
              expert@expert_id,
              private$retired_experts,
              inherits = FALSE
            )
          },
          experts
        )
      }
      unname(experts)
    },

    add_expert = function(expert, replace = FALSE) {
      expert <- tempest_validate_experts(list(expert))[[1L]]
      replace <- tempest_product_flag(replace, "replace")
      expert_id <- expert@expert_id
      already_exists <- exists(expert_id, private$experts, inherits = FALSE)
      if (already_exists && !replace) {
        tempest_deputy_expert_abort(
          "Expert {.val {expert_id}} is already in the live roster."
        )
      }
      if (already_exists) {
        private$retire_expert_sessions(expert_id)
      }
      assign(expert_id, expert, private$experts)
      if (exists(expert_id, private$retired_experts, inherits = FALSE)) {
        rm(list = expert_id, envir = private$retired_experts)
      }
      invisible(expert_id)
    },

    retire_expert = function(expert_id) {
      expert_id <- tempest_research_expert_id(expert_id, "expert_id")
      if (!exists(expert_id, private$experts, inherits = FALSE)) {
        return(FALSE)
      }
      assign(expert_id, TRUE, private$retired_experts)
      private$retire_expert_sessions(expert_id)
      TRUE
    },

    list_retired_expert_ids = function() {
      sort(ls(private$retired_experts, all.names = TRUE), method = "radix")
    },

    get_or_create = function(expert_id, session_id = NULL) {
      expert <- self$profile(expert_id)
      if (is.null(session_id)) {
        session_id <- private$session_for_expert(expert@expert_id)
        if (is.null(session_id)) {
          return(private$create(expert))
        }
      }
      session_id <- tempest_product_scalar(session_id, "session_id")
      if (!exists(session_id, private$sessions, inherits = FALSE)) {
        tempest_deputy_expert_abort(
          "Expert session {.val {session_id}} is not active."
        )
      }
      binding <- self$session_profile(session_id)
      if (
        !identical(binding$expert_id, expert@expert_id) ||
          !identical(
            binding$expert_fingerprint,
            tempest_expert_profile_fingerprint(expert)
          )
      ) {
        tempest_deputy_expert_abort(
          "Expert session {.val {session_id}} does not match its live profile."
        )
      }
      private$result(session_id, is_new = FALSE)
    },

    request_completion = function(
      expert_id,
      prompt,
      stage,
      correlation_id = tempest_uuid("expert-turn")
    ) {
      stage <- tempest_research_manifest_choice(
        stage,
        "stage",
        c("dialogue", "warmup")
      )
      session <- self$get_or_create(expert_id)
      response <- session$chat$chat(
        tempest_agent_completion_text(prompt),
        echo = "none",
        run_context = list(
          correlation_id = correlation_id,
          role = "expert",
          stage = stage
        )
      )
      tempest_agent_completion_id(response)
    },

    request_completion_async = function(
      expert_id,
      prompt,
      stage,
      correlation_id = tempest_uuid("expert-turn")
    ) {
      tempest_require("promises", "Async expert execution requires promises.")
      stage <- tempest_research_manifest_choice(
        stage,
        "stage",
        c("dialogue", "warmup")
      )
      session <- self$get_or_create(expert_id)
      promises::then(
        session$chat$chat_async(
          tempest_agent_completion_text(prompt),
          echo = "none",
          run_context = list(
            correlation_id = correlation_id,
            role = "expert",
            stage = stage
          )
        ),
        onFulfilled = tempest_agent_completion_id
      )
    },

    commit_completion = function(
      completion_id,
      expert_id,
      stage,
      is_current = function() TRUE
    ) {
      if (!is.function(is_current)) {
        tempest_deputy_expert_abort("{.arg is_current} must be a function.")
      }
      expert <- self$profile(expert_id)
      stage <- tempest_research_manifest_choice(
        stage,
        "stage",
        c("dialogue", "warmup")
      )
      expert_session <- self$get_or_create(expert@expert_id)
      claim <- tempest_agent_completion_claim(
        private$completion_registry,
        completion_id,
        private$completion_owner
      )
      prepared <- tryCatch(
        {
          trace <- tempest_costorm_deputy_trace(claim$deputy_execution)
          valid <- identical(trace$status, "complete") &&
            identical(trace$completion_disposition, "issued") &&
            identical(trace$role, "expert") &&
            identical(trace$stage, stage) &&
            identical(trace$expert_id, expert@expert_id)
          if (!valid) {
            tempest_agent_completion_binding_abort()
          }
          list(
            response = tempest_agent_completion_text(claim$response),
            provider_turn = tempest_agent_completion_provider_turn(
              claim$provider_turn
            ),
            trace = trace
          )
        },
        error = function(error) {
          tempest_agent_completion_release(
            private$completion_registry,
            claim,
            private$completion_owner
          )
          stop(error)
        }
      )
      if (!tempest_async_is_current(is_current)) {
        tempest_agent_completion_cancel(
          private$completion_registry,
          completion_id,
          private$completion_owner
        )
        return(list(cancelled = TRUE))
      }

      prior_sources <- private$source_ids()
      prior_claims <- private$claim_ids()
      tempest_agent_completion_consume(
        private$completion_registry,
        claim,
        private$completion_owner
      )
      source_ids <- character()
      extracted <- FALSE
      evidence_error <- NULL
      tryCatch(
        {
          source_ids <- tempest_harvest_native_sources_from_turn(
            prepared$provider_turn,
            private$workspace
          )
          source_ids <- tempest_answer_source_ids(
            private$workspace,
            prepared$response,
            source_ids
          )
          extracted <- private$extract_facts(
            response = prepared$response,
            turn = prepared$provider_turn,
            source_ids = source_ids,
            expert_id = expert@expert_id,
            correlation_id = prepared$trace$correlation_id,
            deputy_execution = prepared$trace
          )
        },
        error = function(error) {
          evidence_error <<- tempest_progress_error_payload(error)
          invisible(NULL)
        }
      )
      current_sources <- private$source_ids()
      claim_ids <- setdiff(private$claim_ids(), prior_claims)
      added_source_ids <- setdiff(current_sources, prior_sources)
      list(
        cancelled = FALSE,
        completion_id = completion_id,
        response = prepared$response,
        provider_turn = prepared$provider_turn,
        deputy_execution = prepared$trace,
        session_id = expert_session$session_id,
        source_ids = unique(c(
          source_ids,
          intersect(
            tempest_extract_citation_ids(prepared$response),
            current_sources
          ),
          setdiff(current_sources, prior_sources)
        )),
        claim_ids = claim_ids,
        sources_added = as.integer(length(added_source_ids)),
        claims_added = as.integer(length(claim_ids)),
        evidence_committed = isTRUE(extracted),
        evidence_error = evidence_error
      )
    },

    cancel_completion = function(completion_id) {
      tempest_agent_completion_cancel(
        private$completion_registry,
        completion_id,
        private$completion_owner
      )
    },

    session_profile = function(session_id) {
      session_id <- tempest_product_scalar(session_id, "session_id")
      if (!exists(session_id, private$bindings, inherits = FALSE)) {
        tempest_deputy_expert_abort(
          "Expert session {.val {session_id}} is not active."
        )
      }
      rlang::duplicate(
        get(session_id, private$bindings, inherits = FALSE),
        shallow = FALSE
      )
    },

    list_sessions = function() {
      sort(ls(private$sessions, all.names = TRUE), method = "radix")
    },

    snapshot = function() {
      lapply(self$list_sessions(), self$session_profile)
    },

    restore_session = function(binding) {
      expected <- c(
        "session_id",
        "expert_id",
        "expert_version",
        "expert_fingerprint",
        "created_at"
      )
      if (
        !is.list(binding) ||
          is.data.frame(binding) ||
          is.object(binding) ||
          !identical(names(binding), expected)
      ) {
        tempest_deputy_expert_abort(
          "Expert session binding does not match the current schema."
        )
      }
      scalar_fields <- c(
        "session_id",
        "expert_id",
        "expert_version",
        "expert_fingerprint",
        "created_at"
      )
      for (field in scalar_fields) {
        binding[[field]] <- tempest_deputy_expert_exact_scalar(
          binding[[field]],
          field
        )
      }
      expert <- self$profile(binding$expert_id)
      if (
        !identical(binding$expert_version, expert@version) ||
          !identical(
            binding$expert_fingerprint,
            tempest_expert_profile_fingerprint(expert)
          )
      ) {
        tempest_deputy_expert_abort(
          "Expert session binding does not match its live scientific profile."
        )
      }
      created <- private$create(expert, session_id = binding$session_id)
      committed <- FALSE
      on.exit(
        {
          if (!committed) {
            self$retire_session(binding$session_id)
          }
        },
        add = TRUE
      )
      restored <- self$session_profile(created$session_id)
      restored$created_at <- binding$created_at
      assign(created$session_id, restored, private$bindings)
      if (!identical(self$session_profile(created$session_id), binding)) {
        tempest_deputy_expert_abort(
          "Expert session restore did not preserve its exact binding."
        )
      }
      committed <- TRUE
      private$result(created$session_id, is_new = TRUE)
    },

    retire_session = function(session_id) {
      if (is.null(session_id)) {
        return(list(retired = FALSE, cancellation_supported = FALSE))
      }
      session_id <- tempest_product_scalar(session_id, "session_id")
      if (!exists(session_id, private$sessions, inherits = FALSE)) {
        return(list(retired = FALSE, cancellation_supported = FALSE))
      }
      chat <- get(session_id, private$sessions, inherits = FALSE)
      cancel <- chat$cancel %||% chat$stop %||% NULL
      supported <- is.function(cancel)
      if (supported) {
        try(cancel(), silent = TRUE)
      }
      rm(list = session_id, envir = private$sessions)
      rm(list = session_id, envir = private$bindings)
      list(retired = TRUE, cancellation_supported = supported)
    }
  ),
  private = list(
    config = NULL,
    retriever = NULL,
    extractor = NULL,
    extract_claims_program = NULL,
    workspace = NULL,
    progress = NULL,
    run_id = NULL,
    stage_recorder = NULL,
    manifest = NULL,
    completion_registry = NULL,
    completion_owner = NULL,
    on_start = NULL,
    on_completion = NULL,
    on_terminal = NULL,
    experts = NULL,
    retired_experts = NULL,
    sessions = NULL,
    bindings = NULL,

    source_ids = function() {
      vapply(
        private$workspace$list_retrieved_sources(),
        \(source) source$id,
        character(1)
      )
    },

    claim_ids = function() {
      vapply(
        private$workspace$list_proposed_claims(),
        \(claim) claim@claim_id,
        character(1)
      )
    },

    session_for_expert = function(expert_id) {
      for (session_id in sort(ls(private$bindings, all.names = TRUE))) {
        binding <- get(session_id, private$bindings, inherits = FALSE)
        if (identical(binding$expert_id, expert_id)) {
          return(session_id)
        }
      }
      NULL
    },

    result = function(session_id, is_new) {
      list(
        chat = get(session_id, private$sessions, inherits = FALSE),
        session_id = session_id,
        is_new = is_new,
        profile = self$session_profile(session_id)
      )
    },

    create = function(expert, session_id = NULL) {
      session_id <- session_id %||%
        tempest_deputy_expert_session_id(private$run_id, expert@expert_id)
      if (
        exists(session_id, private$sessions, inherits = FALSE) ||
          exists(session_id, private$bindings, inherits = FALSE)
      ) {
        tempest_deputy_expert_abort(
          "Expert session {.val {session_id}} is already active."
        )
      }
      model <- tempest_research_model(private$config, "expert")
      chat <- tempest_make_chat(
        private$config,
        "expert",
        system_prompt = tempest_deputy_expert_prompt(expert),
        echo = "none"
      )
      tempest_research_attach_tools(
        chat,
        retriever = private$retriever,
        role = "expert",
        model = model,
        search_provider = private$config@search_provider,
        claim_provenance = list(
          session_id = private$run_id,
          expert_id = expert@expert_id
        )
      )
      chat <- tempest_deputy_chat_adapter(
        chat,
        manifest = private$manifest,
        deputy_session_id = session_id,
        agent_name = expert@name,
        stage = "dialogue",
        role = "expert",
        expert_id = expert@expert_id,
        completion_registry = private$completion_registry,
        on_start = private$on_start,
        on_completion = private$on_completion,
        on_terminal = private$on_terminal
      )
      binding <- list(
        session_id = session_id,
        expert_id = expert@expert_id,
        expert_version = expert@version,
        expert_fingerprint = tempest_expert_profile_fingerprint(expert),
        created_at = tempest_now_utc()
      )
      assign(session_id, chat, private$sessions)
      assign(session_id, binding, private$bindings)
      private$result(session_id, is_new = TRUE)
    },

    extract_facts = function(
      response,
      turn,
      source_ids,
      expert_id,
      correlation_id,
      deputy_execution
    ) {
      event <- self$emit_progress(
        "step",
        "started",
        stage = "evidence",
        step = "fact_extraction",
        correlation_id = correlation_id
      )
      if (length(source_ids) == 0L) {
        self$emit_progress(
          "step",
          "skipped",
          stage = "evidence",
          step = "fact_extraction",
          parent_event_id = event@event_id,
          correlation_id = correlation_id,
          payload = list(reason = "no_cited_sources")
        )
        return(FALSE)
      }
      tryCatch(
        {
          tempest_extract_facts_from_answer(
            private$extractor,
            response,
            private$workspace,
            module = private$extract_claims_program,
            source_ids = source_ids,
            session_id = private$run_id,
            expert_id = expert_id,
            retrieval_step_id = correlation_id,
            deputy_run_id = deputy_execution$deputy_run_id,
            deputy_session_id = deputy_execution$deputy_session_id,
            parent_run_id = deputy_execution$parent_run_id %||% NA_character_,
            delegation_id = deputy_execution$delegation_id %||% NA_character_,
            tool_call_id = deputy_execution$tool_call_id %||% NA_character_,
            record_stage = private$stage_recorder
          )
          self$emit_progress(
            "step",
            "succeeded",
            stage = "evidence",
            step = "fact_extraction",
            parent_event_id = event@event_id,
            correlation_id = correlation_id,
            payload = list(claim_count = length(private$claim_ids()))
          )
          TRUE
        },
        error = function(error) {
          self$emit_progress(
            "step",
            "failed",
            stage = "evidence",
            step = "fact_extraction",
            parent_event_id = event@event_id,
            correlation_id = correlation_id,
            payload = tempest_progress_error_payload(error)
          )
          tempest_rethrow_operation(
            error,
            class = "tempest_deputy_expert_error"
          )
        }
      )
    },

    retire_expert_sessions = function(expert_id) {
      for (session_id in self$list_sessions()) {
        binding <- self$session_profile(session_id)
        if (identical(binding$expert_id, expert_id)) {
          self$retire_session(session_id)
        }
      }
      invisible(NULL)
    }
  )
)

tempest_create_deputy_expert_delegation_tool <- function(
  manager,
  topic,
  experts = NULL
) {
  tempest_require("ellmer", "Expert delegation requires ellmer.")
  if (!inherits(manager, "TempestDeputyExpertManager")) {
    tempest_deputy_expert_abort(
      "{.arg manager} must be a Tempest Deputy expert manager."
    )
  }
  topic <- tempest_product_scalar(topic, "topic")
  roster <- manager$list_experts()
  if (!is.null(experts)) {
    requested <- tempest_validate_experts(experts)
    if (
      !setequal(
        vapply(requested, \(expert) expert@expert_id, character(1)),
        vapply(roster, \(expert) expert@expert_id, character(1))
      )
    ) {
      tempest_deputy_expert_abort(
        "{.arg experts} must match the manager's exact live roster."
      )
    }
  }
  roster_text <- paste(
    vapply(
      roster,
      function(expert) {
        paste0(expert@expert_id, " (", expert@name, ", ", expert@title, ")")
      },
      character(1)
    ),
    collapse = "; "
  )

  delegate_to_expert <- function(expert_id, question) {
    expert <- manager$profile(expert_id)
    correlation_id <- tempest_uuid("tool")
    session <- manager$get_or_create(expert@expert_id)
    event <- manager$emit_progress(
      "tool",
      "started",
      stage = "dialogue",
      step = "delegate_to_expert",
      correlation_id = correlation_id,
      payload = list(
        expert_id = expert@expert_id,
        expert_name = expert@name,
        session_id = session$session_id
      )
    )
    prompt <- paste0(
      "Topic: ",
      topic,
      "\n\nQuestion: ",
      question,
      "\n\nUse the fixed scientific tools attached to this session. ",
      "Inspect bounded evidence, cite exact source IDs, state remaining gaps, ",
      "and respond in no more than 250 words."
    )
    tryCatch(
      {
        completion_id <- manager$request_completion(
          expert@expert_id,
          prompt,
          stage = "dialogue",
          correlation_id = correlation_id
        )
        result <- manager$commit_completion(
          completion_id,
          expert@expert_id,
          stage = "dialogue"
        )
        trace <- result$deputy_execution
        manager$emit_progress(
          "tool",
          "succeeded",
          stage = "dialogue",
          step = "delegate_to_expert",
          parent_event_id = event@event_id,
          correlation_id = correlation_id,
          payload = list(
            expert_id = expert@expert_id,
            expert_name = expert@name,
            session_id = result$session_id,
            deputy_run_id = trace$deputy_run_id,
            deputy_session_id = trace$deputy_session_id
          )
        )
        list(
          expert_id = expert@expert_id,
          expert = expert@name,
          response = result$response,
          session_id = result$session_id,
          deputy_run_id = trace$deputy_run_id,
          deputy_session_id = trace$deputy_session_id,
          source_ids = result$source_ids,
          claim_ids = result$claim_ids,
          evidence_status = if (!is.null(result$evidence_error)) {
            "failed"
          } else if (isTRUE(result$evidence_committed)) {
            "committed"
          } else {
            "skipped"
          }
        )
      },
      error = function(error) {
        manager$emit_progress(
          "tool",
          "failed",
          stage = "dialogue",
          step = "delegate_to_expert",
          parent_event_id = event@event_id,
          correlation_id = correlation_id,
          payload = c(
            list(
              expert_id = expert@expert_id,
              expert_name = expert@name,
              session_id = session$session_id
            ),
            tempest_progress_error_payload(error)
          )
        )
        tempest_rethrow_operation(
          error,
          class = "tempest_deputy_expert_error"
        )
      }
    )
  }

  ellmer::tool(
    delegate_to_expert,
    name = "delegate_to_expert",
    description = paste(
      "Delegate one narrow evidence question to an active scientific expert.",
      "Use the exact expert_id.",
      "Active experts:",
      roster_text
    ),
    arguments = list(
      expert_id = ellmer::type_string("Exact active expert identifier."),
      question = ellmer::type_string("One narrow evidence question.")
    )
  )
}
