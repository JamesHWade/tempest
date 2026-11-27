# STORM pipeline orchestration helpers

tempest_storm_run_context <- function(
  topic,
  config = tempest_config(),
  retriever = NULL,
  knowledge_records = list(),
  knowledge_selection = list(),
  n_experts = 3,
  experts = NULL,
  research_strategy = c("key_questions", "conversation"),
  max_rounds = 3,
  max_questions_per_perspective = 3,
  parallel_writing = FALSE,
  program_set = NULL,
  steps = c("perspectives", "research", "outline", "write", "polish"),
  output_dir = NULL,
  resume = FALSE,
  run_id = NULL,
  progress = NULL,
  verbose = TRUE,
  .requested_steps = NULL,
  .admit_knowledge = NULL
) {
  if (!is.character(topic) || length(topic) != 1L || is.na(topic)) {
    tempest_config_abort("{.arg topic} must be a single non-empty string.")
  }
  topic <- tempest_trim(topic)
  if (!nzchar(topic)) {
    tempest_config_abort("{.arg topic} must be a single non-empty string.")
  }
  if (!S7::S7_inherits(config, TempestConfig)) {
    tempest_config_abort(
      "{.arg config} must be created by {.fn tempest_config}."
    )
  }
  research_strategy <- match.arg(research_strategy)
  if (is.null(experts)) {
    n_experts <- tempest_config_count(n_experts, "n_experts")
  } else {
    experts <- tempest_validate_experts(experts)
    if (length(experts) == 0L) {
      tempest_config_abort(
        "{.arg experts} must contain at least one active expert profile."
      )
    }
    n_experts <- length(experts)
  }
  if (n_experts > config@max_active_experts) {
    tempest_config_abort(
      c(
        "Expert request exceeds the configured budget.",
        x = "Requested {n_experts}; maximum is {config@max_active_experts}."
      )
    )
  }
  max_rounds <- tempest_config_count(max_rounds, "max_rounds")
  max_questions_per_perspective <- tempest_config_count(
    max_questions_per_perspective,
    "max_questions_per_perspective"
  )
  parallel_writing <- tempest_config_flag(parallel_writing, "parallel_writing")
  resume <- tempest_config_flag(resume, "resume")
  verbose <- tempest_config_flag(verbose, "verbose")
  steps <- tempest_storm_requested_steps(steps)
  requested_steps <- if (is.null(.requested_steps)) {
    steps
  } else {
    tempest_storm_requested_steps(.requested_steps)
  }
  if (!all(steps %in% requested_steps)) {
    tempest_storm_state_abort(
      "Execution {.arg steps} must be contained in {.arg .requested_steps}."
    )
  }

  program_set <- program_set %||% tempest_program_set()
  run_dir <- tempest_storm_prepare_run_dir(output_dir, topic, run_id = run_id)
  supplied_run_id <- if (
    rlang::is_string(run_id) &&
      nzchar(tempest_trim(run_id))
  ) {
    tempest_trim(run_id)
  } else {
    NULL
  }
  loaded_run <- NULL
  if (
    isTRUE(resume) &&
      !is.null(run_dir) &&
      file.exists(tempest_storm_artifact_paths(run_dir)$run_config)
  ) {
    loaded_run <- tempest_storm_load_artifacts(
      run_dir,
      config = config,
      program_set = program_set,
      run_id = supplied_run_id
    )
    tempest_storm_validate_resume_request(
      loaded_run,
      topic,
      requested_steps,
      steps,
      knowledge_selection
    )
  }

  retriever <- retriever %||%
    tempest_retriever(
      config = config,
      workspace = tempest_research_workspace()
    )
  retriever_config_digest <- tempest_retriever_config_digest(retriever)
  if (
    !is.null(retriever_config_digest) &&
      !identical(
        retriever_config_digest,
        tempest_research_config_digest(config)
      )
  ) {
    tempest_config_abort(c(
      "{.arg retriever} does not match the supplied {.arg config}.",
      x = paste0(
        "A TempestRetriever must be created from the same ",
        "behavior-relevant configuration."
      )
    ))
  }
  workspace <- tempest_storm_retriever_workspace(retriever)
  if (!inherits(workspace, "ResearchWorkspace")) {
    tempest_config_abort(
      paste0(
        "{.arg retriever} must expose a ResearchWorkspace at ",
        "{.code retriever$workspace}."
      )
    )
  }
  tempest_retriever_bind_workspace_budget(retriever, workspace, config)
  progress <- tempest_progress_callback(progress)
  if (is.null(loaded_run)) {
    tempest_knowledge_workspace_preflight(workspace, knowledge_selection)
  } else {
    tempest_storm_preflight_workspace(
      workspace,
      loaded_run$workspace,
      loaded_run$research_manifest@status
    )
  }
  if (!is.null(.admit_knowledge)) {
    .admit_knowledge()
  }
  if (is.null(loaded_run)) {
    tempest_knowledge_insert_records(
      workspace,
      knowledge_records,
      knowledge_selection
    )
  }

  progress_run_id <- if (!is.null(supplied_run_id)) {
    supplied_run_id
  } else if (!is.null(run_dir)) {
    basename(run_dir)
  } else {
    tempest_uuid("run")
  }
  state <- tempest_storm_state(
    topic = topic,
    experts = experts %||% list(),
    requested_steps = requested_steps
  )
  program_references <- tempest_program_set_manifest_programs(program_set)
  research_manifest <- tempest_research_manifest(
    research_run_id = progress_run_id,
    mode = "storm",
    config = config,
    programs = program_references,
    artifact_selection = workspace$artifact_selection,
    runtime = list(
      deputy_session_ids = character(),
      deputy_run_ids = character()
    ),
    traces = list(),
    deliverables = list(),
    status = "running"
  )
  programs <- tempest_bind_program_set(program_set, research_manifest)
  completed_stages <- state$completed_stages

  if (!is.null(loaded_run)) {
    loaded_run$workspace <- tempest_storm_attach_workspace(
      workspace,
      loaded_run$workspace,
      loaded_run$research_manifest@status
    )
    workspace <- loaded_run$workspace
    if (!identical(tempest_storm_retriever_workspace(retriever), workspace)) {
      tempest_abort(
        "Restored STORM state must remain bound to the original retriever workspace.",
        class = tempest_persistence_error_class("tempest_run_resume_error")
      )
    }
    state <- loaded_run$state
    completed_stages <- state$completed_stages
    research_manifest <- loaded_run$research_manifest
    programs <- tempest_bind_program_set(program_set, research_manifest)
    progress_run_id <- research_manifest@research_run_id
    if (verbose && length(completed_stages) > 0) {
      tempest_inform(
        "Loaded persisted STORM stages from {.path {run_dir}}: {paste(completed_stages, collapse = ', ')}"
      )
    }
  }

  completion_owner <- new.env(parent = emptyenv())
  ctx <- new.env(parent = emptyenv())
  ctx$topic <- topic
  ctx$config <- config
  ctx$retriever <- retriever
  ctx$workspace <- workspace
  ctx$store <- workspace
  ctx$n_experts <- n_experts
  ctx$research_strategy <- research_strategy
  ctx$max_rounds <- max_rounds
  ctx$max_questions_per_perspective <- max_questions_per_perspective
  ctx$parallel_writing <- parallel_writing
  ctx$program_set <- program_set
  ctx$steps <- steps
  ctx$requested_steps <- requested_steps
  ctx$run_dir <- run_dir
  ctx$resume <- resume
  ctx$verbose <- verbose
  ctx$progress <- progress
  ctx$progress_run_id <- progress_run_id
  ctx$state <- state
  ctx$completed_stages <- completed_stages
  ctx$research_manifest <- research_manifest
  ctx$programs <- programs
  ctx$completion_owner <- completion_owner
  ctx$completion_registry <- tempest_agent_completion_registry(completion_owner)
  ctx$current_progress_stage <- NA_character_
  ctx$verification_ready <- FALSE
  ctx$research_review_ready <- FALSE
  ctx$title <- state$title
  ctx$perspectives <- state$perspectives
  ctx$experts <- state$experts
  ctx$outline <- state$outline
  ctx$draft_md <- state$draft_md
  ctx$report_md <- state$report_md
  ctx$coordinator <- NULL
  ctx$writer <- NULL
  ctx$extractor <- NULL
  ctx
}

tempest_storm_emit_progress <- function(
  ctx,
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
    ctx$progress,
    run_id = ctx$progress_run_id,
    workflow = "storm",
    event_type = event_type,
    status = status,
    stage = stage,
    step = step,
    message = message,
    payload = payload,
    parent_event_id = parent_event_id,
    correlation_id = correlation_id
  )
}

tempest_storm_emit_progress_best_effort <- function(ctx, ...) {
  tryCatch(
    tempest_storm_emit_progress(ctx, ...),
    error = function(error) invisible(NULL)
  )
}

tempest_storm_emit_stage_started <- function(
  ctx,
  stage,
  message = NA_character_,
  payload = list()
) {
  ctx$current_progress_stage <- stage
  tempest_storm_emit_progress(
    ctx,
    "stage",
    "started",
    stage = stage,
    message = message,
    payload = payload
  )
}

tempest_storm_emit_stage_succeeded <- function(
  ctx,
  stage,
  message = NA_character_,
  payload = list()
) {
  tempest_storm_emit_progress(
    ctx,
    "stage",
    "succeeded",
    stage = stage,
    message = message,
    payload = payload
  )
  if (identical(ctx$current_progress_stage, stage)) {
    ctx$current_progress_stage <- NA_character_
  }
}

tempest_storm_emit_stage_skipped <- function(
  ctx,
  stage,
  message = NA_character_,
  payload = list()
) {
  tempest_storm_emit_progress(
    ctx,
    "stage",
    "skipped",
    stage = stage,
    message = message,
    payload = payload
  )
}

tempest_storm_state_with_references <- function(ctx, candidate_state) {
  cited_md <- candidate_state$report_md %||%
    candidate_state$draft_md %||%
    ""
  cited_ids <- tempest_extract_citation_ids(cited_md)
  candidate_state$references <- Filter(
    Negate(is.null),
    lapply(cited_ids, function(id) ctx$workspace$get_retrieved_source(id))
  )
  tempest_storm_state_validate(candidate_state)
}

tempest_storm_write_run_bundle <- function(ctx) {
  ctx$state <- tempest_storm_state_with_references(ctx, ctx$state)
  if (is.null(ctx$run_dir)) {
    return(invisible(NULL))
  }
  bound_manifest <- tempest_storm_save_artifacts(
    ctx$run_dir,
    ctx$workspace,
    ctx$state,
    ctx$research_manifest,
    program_set = ctx$program_set,
    config = ctx$config,
    steps = ctx$requested_steps
  )
  if (!is.null(bound_manifest)) {
    ctx$research_manifest <- bound_manifest
  }
  invisible(bound_manifest)
}

tempest_storm_emit_persistence_started <- function(ctx, stage) {
  step <- paste0(stage, "_artifacts")
  if (!is.null(ctx$run_dir)) {
    tempest_storm_emit_progress(
      ctx,
      "step",
      "started",
      stage = "persistence",
      step = step,
      payload = list(source_stage = stage)
    )
  }
  !is.null(ctx$run_dir)
}

tempest_storm_emit_persistence_failed <- function(ctx, stage, error) {
  if (!is.null(ctx$run_dir)) {
    tempest_storm_emit_progress(
      ctx,
      "step",
      "failed",
      stage = "persistence",
      step = paste0(stage, "_artifacts"),
      payload = c(
        list(source_stage = stage),
        tempest_progress_error_payload(error)
      )
    )
  }
  invisible(NULL)
}

tempest_storm_emit_persistence_succeeded <- function(ctx, stage) {
  if (!is.null(ctx$run_dir)) {
    tempest_storm_emit_progress(
      ctx,
      "step",
      "succeeded",
      stage = "persistence",
      step = paste0(stage, "_artifacts"),
      payload = list(source_stage = stage, output_dir = ctx$run_dir)
    )
  }
  invisible(NULL)
}

tempest_storm_seal_succeeded_workspace <- function(ctx) {
  if (!identical(ctx$research_manifest@status, "succeeded")) {
    return(invisible(ctx$workspace))
  }
  workspace_state <- tempest_research_workspace_mutation_state(ctx$workspace)
  if (identical(workspace_state, "open")) {
    tempest_research_workspace_seal(ctx$workspace)
  } else if (!identical(workspace_state, "sealed")) {
    tempest_stage_governance_abort(
      "A succeeded STORM workspace must be sealed before publication."
    )
  }
  invisible(ctx$workspace)
}

tempest_storm_save_run_artifacts <- function(
  ctx,
  stage,
  persistence_started = FALSE,
  after_write = NULL
) {
  if (!isTRUE(persistence_started)) {
    tempest_storm_emit_persistence_started(ctx, stage)
  }
  tryCatch(
    {
      tempest_storm_write_run_bundle(ctx)
      if (!is.null(after_write)) {
        after_write()
      }
    },
    error = function(e) {
      tempest_storm_emit_persistence_failed(ctx, stage, e)
      tempest_rethrow_operation(e, class = "tempest_run_error")
    }
  )
  tempest_storm_emit_persistence_succeeded(ctx, stage)
}

tempest_storm_persist_terminal_state <- function(ctx) {
  tryCatch(
    tempest_storm_write_run_bundle(ctx),
    error = function(error) invisible(NULL)
  )
  invisible(NULL)
}

tempest_storm_stage_recorder <- function(ctx, commit_output = NULL) {
  if (!is.null(commit_output) && !is.function(commit_output)) {
    tempest_stage_evaluator_abort(
      "{.arg commit_output} must be `NULL` or a function."
    )
  }
  force(commit_output)
  function(record, output = NULL) {
    next_state <- rlang::duplicate(ctx$state, shallow = TRUE)
    next_state$stage_records <- tempest_stage_records_upsert(
      next_state$stage_records,
      record
    )
    if (!is.null(output) && is.function(commit_output)) {
      next_state <- commit_output(next_state, output)
    }
    next_state <- tempest_storm_state_validate(next_state)
    ctx$state <- next_state
    invisible(record)
  }
}

tempest_storm_record_stages <- function(ctx, records, outputs = NULL) {
  next_state <- rlang::duplicate(ctx$state, shallow = TRUE)
  next_state$stage_records <- tempest_stage_records_upsert_many(
    next_state$stage_records,
    records
  )
  next_state <- tempest_storm_state_validate(next_state)
  ctx$state <- next_state
  invisible(records)
}

tempest_storm_process_expert_answer <- function(
  ctx,
  expert_chat,
  prompt,
  extractor,
  expert_id,
  perspective_id
) {
  answer <- expert_chat$chat(
    prompt,
    echo = if (ctx$verbose) "output" else "none"
  )
  completion_id <- tempest_agent_completion_id(answer)
  completion <- tempest_agent_completion_claim(
    ctx$completion_registry,
    completion_id,
    ctx$completion_owner
  )
  settled <- FALSE
  product_mutated <- FALSE
  on.exit(
    {
      if (!isTRUE(settled)) {
        if (isTRUE(product_mutated)) {
          try(
            tempest_agent_completion_consume(
              ctx$completion_registry,
              completion,
              ctx$completion_owner
            ),
            silent = TRUE
          )
        } else {
          try(
            tempest_agent_completion_release(
              ctx$completion_registry,
              completion,
              ctx$completion_owner
            ),
            silent = TRUE
          )
        }
      }
    },
    add = TRUE
  )
  if (!identical(completion$prompt, prompt)) {
    tempest_stage_governance_abort(
      "A claimed STORM completion does not match its exact prompt."
    )
  }
  trace <- tempest_storm_deputy_trace(
    completion$deputy_execution,
    expert_id
  )
  if (!identical(trace$status, "complete")) {
    tempest_stage_governance_abort(
      "A STORM expert answer requires a completed Deputy run."
    )
  }
  answer <- tempest_storm_completion_answer(completion)
  ctx$research_manifest <- tempest_storm_manifest_add_deputy_trace(
    ctx$research_manifest,
    trace
  )
  product_mutated <- TRUE
  harvest <- list(
    answer_text = answer$answer_text,
    source_ids = tempest_harvest_native_sources_from_turn(
      answer$provider_turn,
      ctx$store
    )
  )
  harvest$source_ids <- tempest_answer_source_ids(
    ctx$store,
    harvest$answer_text,
    harvest$source_ids
  )
  tempest_storm_capture_sources(ctx$retriever, ctx$store, harvest$source_ids)
  tempest_extract_facts_from_answer(
    extractor,
    harvest$answer_text,
    ctx$store,
    module = ctx$programs$extract_claims,
    source_ids = harvest$source_ids,
    session_id = ctx$progress_run_id,
    expert_id = expert_id,
    retrieval_step_id = trace$correlation_id,
    perspective_id = perspective_id,
    deputy_run_id = trace$deputy_run_id,
    deputy_session_id = trace$deputy_session_id,
    record_stage = tempest_storm_stage_recorder(ctx)
  )
  tempest_agent_completion_consume(
    ctx$completion_registry,
    completion,
    ctx$completion_owner
  )
  settled <- TRUE
  harvest
}

tempest_storm_ensure_verification_complete <- function(ctx) {
  claims <- ctx$store$list_proposed_claims()
  if (isTRUE(ctx$verification_ready)) {
    return(claims)
  }
  if (length(claims) == 0L) {
    ctx$verification_ready <- TRUE
    return(list())
  }
  audit <- ctx$store$citation_audit
  claim_ids <- vapply(claims, \(claim) claim@claim_id, character(1))
  audit_complete <- !is.null(audit) &&
    setequal(audit$claim_id, claim_ids) &&
    all(vapply(
      claims,
      \(claim) !identical(claim@verification_status, "unverified"),
      logical(1)
    ))
  if (!audit_complete) {
    parent_progress_stage <- ctx$current_progress_stage
    tempest_storm_emit_stage_started(
      ctx,
      "verification",
      message = "Verifying evidence for grounded generation."
    )
    tempest_run_verification(
      ctx$store,
      ctx$config,
      program = ctx$programs$verify_claim_support,
      record_stage = tempest_storm_stage_recorder(ctx),
      record_stages = function(records, outputs = NULL) {
        tempest_storm_record_stages(ctx, records, outputs = outputs)
      }
    )
    tempest_storm_emit_stage_succeeded(
      ctx,
      "verification",
      message = "Finished evidence verification.",
      payload = list(claim_count = length(claims))
    )
    ctx$current_progress_stage <- parent_progress_stage
  }
  if (!ctx$research_review_ready && ctx$config@research_review) {
    tempest_storm_emit_progress(
      ctx,
      "step",
      "started",
      stage = "research",
      step = "scientific_review",
      message = "Reviewing scientific claims and evidence gaps."
    )
    tempest_research_review(ctx$store, ctx$retriever, ctx$config)
    ctx$research_review_ready <- TRUE
    tempest_storm_emit_progress(
      ctx,
      "step",
      "succeeded",
      stage = "research",
      step = "scientific_review",
      message = "Scientific review completed; findings remain advisory."
    )
  }
  ctx$verification_ready <- TRUE
  ctx$store$list_proposed_claims()
}

tempest_storm_ensure_grounded_evidence <- function(ctx) {
  claims <- tempest_storm_ensure_verification_complete(ctx)
  if (length(claims) == 0L) {
    tempest_stage_governance_abort(
      "Grounded STORM stages require at least one proposed evidence claim."
    )
  }
  verified <- tempest_supported_claims(
    ctx$store,
    min_support_score = ctx$config@min_support_score
  )
  if (length(verified) == 0L) {
    tempest_stage_governance_abort(
      paste0(
        "Grounded STORM stages require at least one supported claim at ",
        "the configured support threshold."
      )
    )
  }
  verified
}

tempest_storm_stage_perspectives <- function(ctx) {
  if (
    "perspectives" %in%
      ctx$steps &&
      !tempest_storm_stage_complete(ctx$completed_stages, "perspectives")
  ) {
    tempest_storm_emit_stage_started(
      ctx,
      "perspectives",
      message = "Discovering perspectives and experts.",
      payload = list(n_experts = ctx$n_experts)
    )
    if (ctx$verbose) {
      tempest_inform("Discovering perspectives for: {.val {ctx$topic}}")
    }
    seed <- tempest_retriever_search(
      ctx$retriever,
      ctx$topic,
      k = min(5, ctx$config@max_search_results)
    )
    seed_txt <- paste0(
      "Seed sources:\n",
      paste0(
        "- ",
        seed$title,
        " (",
        seed$source_id,
        ") ",
        seed$url,
        collapse = "\n"
      )
    )

    toc_lines <- character()
    seed_urls <- head(seed$url, 3)
    for (u in seed_urls) {
      wiki_title <- sub("^https://en.wikipedia.org/wiki/", "", u)
      if (grepl("^https://en.wikipedia.org/", u)) {
        toc <- tempest_wiki_page_sections(gsub("_", " ", wiki_title))
      } else {
        toc <- tempest_extract_toc_from_url(u)
      }
      if (length(toc) > 0) {
        toc_lines <- c(toc_lines, paste0("\nToC from ", u, ":"), toc)
      }
    }
    if (length(toc_lines) > 0) {
      seed_txt <- paste0(seed_txt, "\n\n", paste(toc_lines, collapse = "\n"))
    }

    plan <- tempest_generate_perspectives(
      ctx$coordinator,
      ctx$topic,
      seed_txt,
      ctx$n_experts,
      module = ctx$programs$perspectives,
      record_stage = tempest_storm_stage_recorder(
        ctx,
        function(next_state, output) {
          next_state$title <- output$title
          next_state$perspectives <- output$perspectives
          next_state
        }
      )
    )
    ctx$title <- plan$title
    ctx$perspectives <- plan$perspectives

    if (length(ctx$experts) == 0L) {
      if (ctx$verbose) {
        tempest_inform("Generating {ctx$n_experts} expert profiles")
      }
      ctx$experts <- tempest_generate_experts_with_program(
        topic = ctx$topic,
        n = ctx$n_experts,
        config = ctx$config,
        verbose = ctx$verbose,
        module = ctx$programs$personas,
        record_stage = tempest_storm_stage_recorder(
          ctx,
          function(next_state, output) {
            next_state$experts <- output
            next_state
          }
        )
      )
    }
    if (length(ctx$experts) != length(ctx$perspectives)) {
      tempest_stage_governance_abort(
        "Every research perspective requires one explicit expert profile."
      )
    }
    ctx$completed_stages <- tempest_storm_mark_stage_complete(
      ctx$completed_stages,
      "perspectives"
    )
    ctx$state$completed_stages <- ctx$completed_stages
    ctx$state <- tempest_storm_state_validate(ctx$state)
    tempest_storm_save_run_artifacts(ctx, "perspectives")
    tempest_storm_emit_stage_succeeded(
      ctx,
      "perspectives",
      message = "Finished perspectives and expert selection.",
      payload = list(
        perspective_count = length(ctx$perspectives),
        expert_count = length(ctx$experts)
      )
    )
    return(invisible(NULL))
  }

  if (
    ctx$verbose &&
      "perspectives" %in% ctx$steps &&
      tempest_storm_stage_complete(ctx$completed_stages, "perspectives")
  ) {
    tempest_inform("Using persisted perspectives from {.path {ctx$run_dir}}")
  }
  ctx$title <- ctx$state$title
  ctx$perspectives <- ctx$state$perspectives
  ctx$experts <- tempest_validate_experts(ctx$state$experts)
  if ("perspectives" %in% ctx$steps) {
    tempest_storm_emit_stage_skipped(
      ctx,
      "perspectives",
      message = "Using persisted perspectives.",
      payload = list(reason = "already_complete")
    )
  }
  invisible(NULL)
}

tempest_storm_build_expert_chats <- function(ctx) {
  expert_chats <- vector("list", length(ctx$perspectives))
  for (i in seq_along(ctx$perspectives)) {
    expert_profile <- ctx$experts[[i]]
    expert_record <- tempest_expert_runtime_record(expert_profile)
    expert_id <- expert_record$expert_id
    model <- tempest_research_model(ctx$config, "expert")
    sp <- tempest_render_expert_prompt(
      persona = expert_profile,
      expert_id = expert_id
    )
    expert_chat <- tempest_make_chat(
      ctx$config,
      "expert",
      system_prompt = sp,
      echo = if (ctx$verbose) "output" else "none"
    )
    tempest_research_attach_tools(
      expert_chat,
      ctx$retriever,
      role = "expert",
      model = model,
      search_provider = ctx$config@search_provider,
      max_search_results = ctx$config@max_search_results,
      claim_provenance = list(
        session_id = ctx$progress_run_id,
        expert_id = expert_id
      )
    )
    expert_chats[[i]] <- local({
      current_chat <- expert_chat
      current_expert_id <- expert_id
      current_name <- expert_record$name
      terminal_traces <- new.env(parent = emptyenv())
      tempest_deputy_chat_adapter(
        current_chat,
        manifest = ctx$research_manifest,
        deputy_session_id = tempest_storm_deputy_session_id(
          ctx$progress_run_id,
          current_expert_id
        ),
        agent_name = current_name,
        stage = "research",
        role = "expert",
        expert_id = current_expert_id,
        completion_registry = ctx$completion_registry,
        on_run = function(trace) {
          trace <- tempest_storm_deputy_trace(trace, current_expert_id)
          assign(trace$deputy_run_id, trace, envir = terminal_traces)
          invisible(trace)
        },
        on_completion = function(completion) {
          trace <- tempest_storm_deputy_trace(
            completion$deputy_execution,
            current_expert_id
          )
          recorded <- get0(
            trace$deputy_run_id,
            envir = terminal_traces,
            inherits = FALSE
          )
          if (!identical(recorded, trace)) {
            tempest_stage_governance_abort(
              paste0(
                "A STORM completion must match its exact terminal ",
                "Deputy trace."
              )
            )
          }
          tempest_agent_completion_issue(
            ctx$completion_registry,
            completion_id = completion$completion_id,
            prompt = completion$prompt,
            response = completion$response,
            provider_turn = completion$provider_turn,
            deputy_execution = trace
          )
          rm(
            list = trace$deputy_run_id,
            envir = terminal_traces,
            inherits = FALSE
          )
          invisible(completion$completion_id)
        }
      )
    })
  }
  expert_chats
}

tempest_storm_stage_research <- function(ctx) {
  if (
    "research" %in%
      ctx$steps &&
      !tempest_storm_stage_complete(ctx$completed_stages, "research")
  ) {
    tempest_storm_emit_stage_started(
      ctx,
      "research",
      message = "Running STORM research loop.",
      payload = list(
        perspective_count = length(ctx$perspectives),
        research_strategy = ctx$research_strategy,
        parallel = FALSE
      )
    )
    if (ctx$verbose) {
      tempest_inform("Research loop: {length(ctx$perspectives)} perspectives")
    }
    if (length(ctx$perspectives) == 0L) {
      tempest_stage_governance_abort(
        "STORM research requires evaluated perspectives."
      )
    }
    if (length(ctx$experts) != length(ctx$perspectives)) {
      tempest_stage_governance_abort(
        "Every research perspective requires one explicit expert profile."
      )
    }

    expert_chats <- tempest_storm_build_expert_chats(ctx)
    record_stage <- tempest_storm_stage_recorder(ctx)

    for (i in seq_along(ctx$perspectives)) {
      p <- ctx$perspectives[[i]]
      p_name <- p$name
      p_desc <- p$description
      qs <- p$key_questions

      expert <- expert_chats[[i]]
      expert_profile <- ctx$experts[[i]]
      expert_record <- tempest_expert_runtime_record(expert_profile)
      expert_name <- expert_record$name
      expert_id <- expert_record$expert_id
      perspective_id <- as.character(p$id %||% i)

      if (ctx$verbose) {
        tempest_inform("Perspective: {.val {p_name}}")
        if (!is.null(expert_profile)) {
          tempest_inform("  Expert: {expert_name} ({expert_record$title})")
        }
      }

      ask_perspective_question <- function(q) {
        decomposed <- tempest_decompose_query(
          ctx$writer,
          q,
          ctx$topic,
          module = ctx$programs$query_decomposition,
          max_queries = ctx$config@max_search_queries_per_turn,
          record_stage = record_stage
        )
        search_instructions <- paste0(
          "Suggested search queries:\n",
          paste0("- ", decomposed$queries, collapse = "\n"),
          "\n\n"
        )
        prompt <- paste0(
          "Perspective: ",
          p_name,
          "\n",
          "Description: ",
          p_desc,
          "\n\n",
          "Question: ",
          q,
          "\n\n",
          search_instructions,
          "Instructions:\n",
          "- Use web_search + fetch_url as needed.\n",
          "- Only state factual claims that are supported by sources you fetched.\n",
          "- For each factual sentence, add one or more citations like [Sxxxxxxxxxxxx].\n",
          "- If evidence is weak or unclear, say so and do not overclaim.\n\n",
          "Answer:"
        )
        tempest_storm_process_expert_answer(
          ctx,
          expert,
          prompt,
          ctx$extractor,
          expert_id,
          perspective_id
        )
      }

      if (identical(ctx$research_strategy, "key_questions")) {
        qs_limited <- head(qs, ctx$max_questions_per_perspective)
        if (ctx$verbose && length(qs) > length(qs_limited)) {
          tempest_inform(
            "  Limiting to {length(qs_limited)} of {length(qs)} questions"
          )
        }
        for (q in qs_limited) {
          ask_perspective_question(q)
        }
      } else {
        answered <- character()
        for (round in seq_len(ctx$max_rounds)) {
          answered_md <- if (length(answered) == 0) {
            "(none yet)"
          } else {
            paste0("- ", answered, collapse = "\n")
          }
          facts_md <- tempest_summarize_facts_for_prompt(
            ctx$store,
            max_items = 60
          )
          nxt <- tempest_generate_next_question(
            ctx$writer,
            ctx$topic,
            p,
            answered_md = answered_md,
            facts_md = facts_md,
            module = ctx$programs$next_question,
            record_stage = record_stage
          )
          q <- nxt$question
          done <- isTRUE(nxt$done)
          harvest <- ask_perspective_question(q)
          answered <- c(
            answered,
            paste0("Q: ", q, "\nA: ", harvest$answer_text)
          )
          if (done) break
        }
      }
    }

    ctx$completed_stages <- tempest_storm_mark_stage_complete(
      ctx$completed_stages,
      "research"
    )
    ctx$state$completed_stages <- ctx$completed_stages
    ctx$state <- tempest_storm_state_validate(ctx$state)
    tempest_storm_save_run_artifacts(ctx, "research")
    tempest_storm_emit_stage_succeeded(
      ctx,
      "research",
      message = "Finished STORM research loop.",
      payload = list(
        source_count = length(ctx$store$list_retrieved_sources()),
        claim_count = length(ctx$store$list_proposed_claims())
      )
    )
    return(invisible(NULL))
  }

  if (
    "research" %in%
      ctx$steps &&
      tempest_storm_stage_complete(ctx$completed_stages, "research")
  ) {
    if (ctx$verbose) {
      tempest_inform(
        "Using persisted research artifacts from {.path {ctx$run_dir}}"
      )
    }
    tempest_storm_emit_stage_skipped(
      ctx,
      "research",
      message = "Using persisted research artifacts.",
      payload = list(reason = "already_complete")
    )
  }
  invisible(NULL)
}

tempest_storm_stage_outline <- function(ctx) {
  if (
    "outline" %in%
      ctx$steps &&
      !tempest_storm_stage_complete(ctx$completed_stages, "outline")
  ) {
    tempest_storm_emit_stage_started(
      ctx,
      "outline",
      message = "Generating STORM outline."
    )
    if (ctx$verbose) {
      tempest_inform("Generating outline (two-step)")
    }

    if (tempest_storm_has_durable_draft_outline(ctx$state)) {
      draft_outline <- ctx$state$draft_outline
      if (ctx$verbose) {
        tempest_inform(
          "Using persisted draft outline from {.path {ctx$run_dir}}"
        )
      }
    } else {
      draft_outline <- tempest_draft_outline(
        ctx$writer,
        ctx$topic,
        ctx$title,
        module = ctx$programs$draft_outline,
        record_stage = tempest_storm_stage_recorder(
          ctx,
          function(next_state, output) {
            next_state$draft_outline <- output
            next_state
          }
        )
      )
    }

    verified_evidence <- tempest_storm_ensure_grounded_evidence(ctx)
    facts_txt <- tempest_summarize_facts_for_prompt(
      ctx$store,
      max_items = 80,
      verified_only = TRUE,
      min_support_score = ctx$config@min_support_score
    )
    ctx$outline <- tempest_refine_outline(
      ctx$writer,
      ctx$topic,
      ctx$title,
      draft_outline,
      facts_txt,
      module = ctx$programs$refined_outline,
      workspace = ctx$store,
      evidence = verified_evidence,
      verified_evidence = verified_evidence,
      verified_facts = facts_txt,
      min_support_score = ctx$config@min_support_score,
      record_stage = tempest_storm_stage_recorder(
        ctx,
        function(next_state, output) {
          next_state$outline <- output
          next_state
        }
      )
    )
    ctx$completed_stages <- tempest_storm_mark_stage_complete(
      ctx$completed_stages,
      "outline"
    )
    ctx$state$completed_stages <- ctx$completed_stages
    ctx$state <- tempest_storm_state_validate(ctx$state)
    tempest_storm_save_run_artifacts(ctx, "outline")
    tempest_storm_emit_stage_succeeded(
      ctx,
      "outline",
      message = "Finished STORM outline.",
      payload = list(section_count = length(ctx$outline$sections %||% list()))
    )
    return(invisible(NULL))
  }

  if (
    ctx$verbose &&
      "outline" %in% ctx$steps &&
      tempest_storm_stage_complete(ctx$completed_stages, "outline")
  ) {
    tempest_inform("Using persisted outline from {.path {ctx$run_dir}}")
  }
  ctx$outline <- ctx$state$outline
  if ("outline" %in% ctx$steps) {
    tempest_storm_emit_stage_skipped(
      ctx,
      "outline",
      message = "Using persisted outline.",
      payload = list(reason = "already_complete")
    )
  }
  invisible(NULL)
}

tempest_storm_stage_write <- function(ctx) {
  if (
    "write" %in%
      ctx$steps &&
      !tempest_storm_stage_complete(ctx$completed_stages, "write")
  ) {
    tempest_storm_emit_stage_started(
      ctx,
      "write",
      message = "Writing STORM draft."
    )
    if (ctx$verbose) {
      tempest_inform("Writing draft")
    }
    if (is.null(ctx$outline) || is.null(ctx$outline$sections)) {
      tempest_abort(
        "No outline available; run steps including 'outline'.",
        class = c("tempest_run_error", "tempest_error")
      )
    }
    verified_evidence <- tempest_storm_ensure_grounded_evidence(ctx)

    section_jobs <- tempest_section_jobs(
      ctx$outline,
      ctx$retriever,
      ctx$store,
      retrieve_top_k = ctx$config@retrieve_top_k,
      min_support_score = ctx$config@min_support_score
    )
    section_results <- tempest_write_section_jobs(
      section_jobs,
      ctx$writer,
      config = ctx$config,
      programs = ctx$programs,
      parallel = ctx$parallel_writing,
      verbose = ctx$verbose,
      record_stage = tempest_storm_stage_recorder(ctx)
    )
    parts <- purrr::map_chr(section_results, "markdown")
    draft_md <- paste(parts, collapse = "\n\n")

    if (ctx$verbose) {
      tempest_inform("Generating at-a-glance brief")
    }
    lead_facts <- tempest_summarize_facts_for_prompt(
      ctx$store,
      max_items = 40,
      verified_only = TRUE,
      min_support_score = ctx$config@min_support_score
    )
    article_body <- draft_md
    lead_section <- tempest_write_lead_section(
      ctx$writer,
      ctx$topic,
      ctx$title,
      draft_md,
      facts_txt = lead_facts,
      module = ctx$programs$lead_section,
      workspace = ctx$store,
      evidence = verified_evidence,
      verified_evidence = verified_evidence,
      verified_facts = lead_facts,
      min_support_score = ctx$config@min_support_score,
      verbose = ctx$verbose,
      record_stage = tempest_storm_stage_recorder(
        ctx,
        function(next_state, output) {
          next_state$lead_section <- output
          next_state$draft_md <- paste0(output, "\n\n", article_body)
          next_state
        }
      )
    )
    ctx$draft_md <- paste0(lead_section, "\n\n", draft_md)
    ctx$completed_stages <- tempest_storm_mark_stage_complete(
      ctx$completed_stages,
      "write"
    )
    ctx$state$completed_stages <- ctx$completed_stages
    ctx$state <- tempest_storm_state_validate(ctx$state)
    tempest_storm_save_run_artifacts(ctx, "write")
    tempest_storm_emit_stage_succeeded(
      ctx,
      "write",
      message = "Finished STORM draft.",
      payload = list(section_count = length(section_results))
    )
    return(invisible(NULL))
  }

  if (
    ctx$verbose &&
      "write" %in% ctx$steps &&
      tempest_storm_stage_complete(ctx$completed_stages, "write")
  ) {
    tempest_inform("Using persisted draft article from {.path {ctx$run_dir}}")
  }
  ctx$draft_md <- ctx$state$draft_md
  if ("write" %in% ctx$steps) {
    tempest_storm_emit_stage_skipped(
      ctx,
      "write",
      message = "Using persisted draft article.",
      payload = list(reason = "already_complete")
    )
  }
  invisible(NULL)
}

tempest_storm_stage_polish <- function(ctx) {
  if (
    "polish" %in%
      ctx$steps &&
      !tempest_storm_stage_complete(ctx$completed_stages, "polish")
  ) {
    tempest_storm_emit_stage_started(
      ctx,
      "polish",
      message = "Polishing STORM report."
    )
    if (ctx$verbose) {
      tempest_inform("Polishing and consistency pass")
    }
    if (ctx$config@citation_policy %in% c("claim_verified", "strict")) {
      tempest_storm_ensure_verification_complete(ctx)
    }
    candidate_report_md <- tempest_storm_polish_report(
      title = ctx$title,
      draft_md = ctx$draft_md,
      workspace = ctx$workspace,
      config = ctx$config
    )
    candidate_report_md <- tempest_storm_report_with_execution_review(
      candidate_report_md,
      ctx$state$stage_records,
      title = ctx$title
    )
    candidate_completed_stages <- tempest_storm_mark_stage_complete(
      ctx$completed_stages,
      "polish"
    )
    candidate_state <- rlang::duplicate(ctx$state, shallow = TRUE)
    candidate_state$report_md <- candidate_report_md
    candidate_state$completed_stages <- candidate_completed_stages
    candidate_state <- tempest_storm_state_with_references(ctx, candidate_state)
    publication_persistence_started <- tempest_storm_emit_persistence_started(
      ctx,
      "polish"
    )
    candidate_manifest <- tryCatch(
      tempest_product_authority_finalize_manifest(
        manifest = ctx$research_manifest,
        stage_records = candidate_state$stage_records,
        workspace = ctx$workspace,
        report_md = candidate_report_md,
        config = ctx$config,
        experts = candidate_state$experts,
        product_state = candidate_state,
        status = "succeeded",
        require_publishable = TRUE
      ),
      error = function(error) {
        tempest_storm_emit_persistence_failed(ctx, "polish", error)
        rlang::cnd_signal(error)
      }
    )
    ctx$report_md <- candidate_report_md
    ctx$completed_stages <- candidate_completed_stages
    ctx$state <- candidate_state
    ctx$research_manifest <- candidate_manifest
    tempest_storm_save_run_artifacts(
      ctx,
      "polish",
      persistence_started = publication_persistence_started,
      after_write = function() tempest_storm_seal_succeeded_workspace(ctx)
    )
    tempest_storm_emit_stage_succeeded(
      ctx,
      "polish",
      message = "Finished polished STORM report.",
      payload = list(has_report = !is.null(ctx$report_md))
    )
    return(invisible(NULL))
  }

  if (
    ctx$verbose &&
      "polish" %in% ctx$steps &&
      tempest_storm_stage_complete(ctx$completed_stages, "polish")
  ) {
    tempest_inform("Using persisted polished report from {.path {ctx$run_dir}}")
  }
  ctx$report_md <- ctx$state$report_md
  if ("polish" %in% ctx$steps) {
    tempest_storm_emit_stage_skipped(
      ctx,
      "polish",
      message = "Using persisted polished report.",
      payload = list(reason = "already_complete")
    )
  }
  invisible(NULL)
}

tempest_storm_finalize_result <- function(ctx) {
  if (!is.null(ctx$report_md)) {
    tempest_storm_emit_progress(
      ctx,
      "artifact",
      "available",
      stage = "polish",
      step = "report_md",
      payload = list(artifact = "report_md", persisted = !is.null(ctx$run_dir))
    )
  }
  product_complete <- tempest_storm_state_is_complete(ctx$state)
  if (
    is.null(ctx$run_dir) &&
      identical(ctx$research_manifest@status, "running") &&
      !isTRUE(product_complete)
  ) {
    ctx$research_manifest <- tempest_product_authority_finalize_manifest(
      manifest = ctx$research_manifest,
      stage_records = ctx$state$stage_records,
      workspace = ctx$workspace,
      report_md = NULL,
      config = ctx$config,
      experts = ctx$state$experts,
      product_state = ctx$state,
      status = "running",
      require_publishable = FALSE
    )
  }
  if (
    isTRUE(product_complete) &&
      !identical(ctx$research_manifest@status, "succeeded")
  ) {
    tempest_stage_governance_abort(
      "A complete STORM product requires atomic publication authority."
    )
  }
  tempest_storm_emit_progress(
    ctx,
    "workflow",
    "succeeded",
    message = "Finished STORM workflow.",
    payload = list(completed_stages = ctx$completed_stages)
  )

  tempest_product_result(
    title = ctx$title,
    topic = ctx$topic,
    perspectives = ctx$perspectives,
    experts = ctx$experts,
    outline = ctx$outline,
    draft_md = ctx$draft_md,
    report_md = ctx$report_md,
    manifest = ctx$research_manifest,
    state = ctx$state,
    workspace = ctx$workspace,
    retriever = ctx$retriever,
    output_dir = ctx$run_dir,
    config = ctx$config
  )
}
