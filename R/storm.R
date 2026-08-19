# STORM pipeline (scripted)

#' @keywords internal
tempest_as_character_vector <- function(x) {
  if (is.null(x)) {
    return(character())
  }
  if (is.data.frame(x)) {
    x <- unlist(x, recursive = TRUE, use.names = FALSE)
  }
  if (is.list(x)) {
    x <- unlist(x, recursive = TRUE, use.names = FALSE)
  }
  x <- as.character(x)
  x <- tempest_trim(x)
  unique(x[nzchar(x) & !is.na(x)])
}

#' @keywords internal
tempest_storm_report_with_execution_review <- function(
  report_md,
  stage_records,
  title = NULL
) {
  if (!rlang::is_string(report_md) || !nzchar(tempest_trim(report_md))) {
    tempest_abort(
      "STORM execution review requires a non-empty Markdown report.",
      class = "tempest_stage_commit_error"
    )
  }
  review <- tempest_stage_records_execution_review(stage_records)
  tempest_markdown_append_execution_review(
    report_md,
    review,
    trusted_title = title
  )
}

#' @keywords internal
tempest_run_verification <- function(
  store,
  config,
  verifier = NULL,
  program,
  record_stage = function(record, output = NULL) invisible(record),
  record_stages = function(records, outputs = NULL) invisible(records)
) {
  verifier <- verifier %||% tempest_make_chat(config, "judge")
  tempest_verify_claims_internal(
    workspace = store,
    verifier = verifier,
    policy = "claim_verified",
    verifier_model = config@models[["judge"]] %||% NA_character_,
    program = program,
    min_support_score = config@min_support_score,
    record_stage = record_stage,
    record_stages = record_stages
  )
}

#' @keywords internal
tempest_storm_retriever_workspace <- function(retriever) {
  workspace <- retriever$workspace %||% NULL
  if (!inherits(workspace, "ResearchWorkspace")) {
    tempest_config_abort(
      "{.arg retriever} must expose a ResearchWorkspace at {.code retriever$workspace}."
    )
  }
  workspace
}

tempest_program_set_requires_knowledge_view <- function(program_set) {
  entries <- tempest_program_set_entries(program_set)
  any(vapply(
    entries,
    \(entry) !is.null(entry$governed_procedure_ref),
    logical(1)
  ))
}

tempest_programs_bind_knowledge_view <- function(programs, knowledge_view) {
  lapply(programs, function(program) {
    if (!inherits(program, "tempest_dsprrr_execution")) {
      tempest_ecosystem_contract_abort(
        "Every structured program must be a ProgramSet-bound execution."
      )
    }
    program$knowledge_view <- knowledge_view
    program
  })
}

tempest_programs_have_knowledge_view <- function(programs) {
  any(vapply(
    programs,
    \(program) !is.null(program$knowledge_view %||% NULL),
    logical(1)
  ))
}

tempest_stage_context_knowledge_view <- function(
  context = list(),
  module,
  knowledge_view = module$knowledge_view %||% NULL
) {
  if (!is.list(context) || is.data.frame(context)) {
    tempest_ecosystem_contract_abort("Stage context must be a list.")
  }
  if (!is.null(knowledge_view)) {
    context$knowledge_view <- knowledge_view
  }
  context
}

#' Run the STORM pipeline
#'
#' This is a scripted workflow that:
#' 1) discovers perspectives and research questions,
#' 2) runs a multi-perspective research loop (search/fetch + expert synthesis),
#' 3) creates an outline,
#' 4) writes a cited report in Markdown.
#'
#' @param topic Research topic or question.
#' @param config A `TempestConfig`.
#' @param retriever Optional `TempestRetriever`. If `NULL`, created from `config`.
#' @param knowledge_view Optional immutable Graft view. It is required when
#'   `program_set` contains any governed procedure and is never persisted.
#' @param n_experts Number of expert profiles to generate when `experts` is
#'   `NULL` (default 3).
#' @param experts Optional list of active profiles created by
#'   [tempest_expert()]. When supplied, STORM uses this selected team and does
#'   not generate experts.
#' @param research_strategy Either "key_questions" (default, faster) or "conversation"
#'   (more thorough but slower). Key questions uses predefined questions; conversation
#'   dynamically generates follow-up questions.
#' @param max_rounds Maximum rounds per perspective for "conversation" strategy (default 3).
#' @param max_questions_per_perspective Maximum questions per perspective for "key_questions"
#'   strategy (default 3).
#' @param parallel_research If `TRUE`, run research perspectives in parallel using
#'   the mirai package. Requires mirai to be installed. Default `FALSE`.
#' @param parallel_writing If `TRUE`, write report sections in parallel using
#'   the mirai package. Failed parallel sections are retried sequentially.
#' @param program_set A [TempestProgramSet] containing the exact dsprrr programs
#'   used by STORM. If `NULL`, [tempest_program_set()] creates the default set.
#' @param steps Character vector controlling which steps to run. Defaults to all.
#' @param output_dir Optional directory for persisted STORM run artifacts. When
#'   supplied, artifacts are written under a topic-specific subdirectory.
#' @param resume If `TRUE` and `output_dir` contains a previous run, load saved
#'   artifacts and skip stages recorded as complete.
#' @param run_id Optional run directory name. Defaults to a slug of `topic`.
#' @param remove_duplicate Must be `FALSE`. Duplicate removal is unavailable on
#'   the authoritative deterministic STORM report path.
#' @param progress Optional function called with a `tempest_progress_event`
#'   object as STORM workflow stages start, finish, fail, persist artifacts, or
#'   make final artifacts available.
#' @param verbose If `TRUE`, prints progress messages.
#' @return A list with product fields `title`, `perspectives`, `experts`,
#'   `outline`, `draft_md`, `report_md`, `manifest`, `state`, `workspace`,
#'   `retriever`, and `output_dir`.
#' @examples
#' \dontrun{
#' cfg <- tempest_config()
#' result <- tempest_run("History of jazz", config = cfg)
#' cat(result$report_md)
#' }
#' @export
tempest_run <- function(
  topic,
  config = tempest_config(),
  retriever = NULL,
  knowledge_view = NULL,
  n_experts = 3,
  experts = NULL,
  research_strategy = c("key_questions", "conversation"),
  max_rounds = 3,
  max_questions_per_perspective = 3,
  parallel_research = FALSE,
  parallel_writing = FALSE,
  program_set = NULL,
  steps = c("perspectives", "research", "outline", "write", "polish"),
  output_dir = NULL,
  resume = FALSE,
  run_id = NULL,
  remove_duplicate = FALSE,
  progress = NULL,
  verbose = TRUE
) {
  tempest_run_internal(
    topic = topic,
    config = config,
    retriever = retriever,
    knowledge_view = knowledge_view,
    n_experts = n_experts,
    experts = experts,
    research_strategy = research_strategy,
    max_rounds = max_rounds,
    max_questions_per_perspective = max_questions_per_perspective,
    parallel_research = parallel_research,
    parallel_writing = parallel_writing,
    program_set = program_set,
    steps = steps,
    output_dir = output_dir,
    resume = resume,
    run_id = run_id,
    remove_duplicate = remove_duplicate,
    progress = progress,
    verbose = verbose
  )
}

tempest_run_internal <- function(
  topic,
  config = tempest_config(),
  retriever = NULL,
  knowledge_view = NULL,
  n_experts = 3,
  experts = NULL,
  research_strategy = c("key_questions", "conversation"),
  max_rounds = 3,
  max_questions_per_perspective = 3,
  parallel_research = FALSE,
  parallel_writing = FALSE,
  program_set = NULL,
  steps = c("perspectives", "research", "outline", "write", "polish"),
  output_dir = NULL,
  resume = FALSE,
  run_id = NULL,
  remove_duplicate = FALSE,
  progress = NULL,
  verbose = TRUE,
  .requested_steps = NULL
) {
  tempest_require("ellmer", "tempest_run() requires ellmer.")
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
  parallel_research <- tempest_config_flag(
    parallel_research,
    "parallel_research"
  )
  if (isTRUE(parallel_research)) {
    tempest_config_abort(
      paste0(
        "{.arg parallel_research} is unavailable until Deputy owns ",
        "parallel STORM execution."
      )
    )
  }
  parallel_writing <- tempest_config_flag(parallel_writing, "parallel_writing")
  resume <- tempest_config_flag(resume, "resume")
  remove_duplicate <- tempest_config_flag(remove_duplicate, "remove_duplicate")
  if (isTRUE(remove_duplicate)) {
    tempest_config_abort(
      paste0(
        "{.arg remove_duplicate} is unavailable on the authoritative STORM ",
        "report path."
      )
    )
  }
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
  knowledge <- tempest_product_knowledge_view(
    program_set,
    knowledge_view
  )
  retriever <- retriever %||%
    tempest_retriever(
      config = config,
      workspace = tempest_research_workspace(
        graft_snapshot = knowledge$snapshot
      )
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
  workspace <- tempest_product_workspace_validate(
    workspace,
    knowledge,
    arg = "retriever"
  )
  if (!inherits(workspace, "ResearchWorkspace")) {
    tempest_config_abort(
      paste0(
        "{.arg retriever} must expose a ResearchWorkspace at ",
        "{.code retriever$workspace}."
      )
    )
  }
  store <- workspace
  run_dir <- tempest_storm_prepare_run_dir(output_dir, topic, run_id = run_id)
  progress <- tempest_progress_callback(progress)
  supplied_run_id <- if (
    rlang::is_string(run_id) &&
      nzchar(tempest_trim(run_id))
  ) {
    tempest_trim(run_id)
  } else {
    NULL
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
    knowledge_snapshot = tempest_storm_snapshot_reference(workspace),
    runtime = list(
      deputy_session_ids = character(),
      deputy_run_ids = character()
    ),
    traces = list(),
    deliverables = list(),
    status = "running"
  )
  programs <- tempest_bind_program_set(
    program_set,
    research_manifest
  )
  programs <- tempest_programs_bind_knowledge_view(
    programs,
    knowledge$view
  )
  completion_owner <- new.env(parent = emptyenv())
  completion_registry <- tempest_agent_completion_registry(completion_owner)
  current_progress_stage <- NA_character_
  emit_progress <- function(
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
      progress,
      run_id = progress_run_id,
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
  emit_progress_best_effort <- function(...) {
    tryCatch(
      emit_progress(...),
      error = function(error) invisible(NULL)
    )
  }
  emit_stage_started <- function(
    stage,
    message = NA_character_,
    payload = list()
  ) {
    current_progress_stage <<- stage
    emit_progress(
      "stage",
      "started",
      stage = stage,
      message = message,
      payload = payload
    )
  }
  emit_stage_succeeded <- function(
    stage,
    message = NA_character_,
    payload = list()
  ) {
    emit_progress(
      "stage",
      "succeeded",
      stage = stage,
      message = message,
      payload = payload
    )
    if (identical(current_progress_stage, stage)) {
      current_progress_stage <<- NA_character_
    }
  }
  emit_stage_skipped <- function(
    stage,
    message = NA_character_,
    payload = list()
  ) {
    emit_progress(
      "stage",
      "skipped",
      stage = stage,
      message = message,
      payload = payload
    )
  }
  state_with_references <- function(candidate_state) {
    cited_md <- candidate_state$report_md %||%
      candidate_state$draft_md %||%
      ""
    cited_ids <- tempest_extract_citation_ids(cited_md)
    candidate_state$references <- Filter(
      Negate(is.null),
      lapply(cited_ids, function(id) workspace$get_retrieved_source(id))
    )
    tempest_storm_state_validate(candidate_state)
  }
  refresh_state_references <- function() {
    state <<- state_with_references(state)
    invisible(state)
  }
  write_run_bundle <- function() {
    refresh_state_references()
    if (is.null(run_dir)) {
      return(invisible(NULL))
    }
    bound_manifest <- tempest_storm_save_artifacts(
      run_dir,
      workspace,
      state,
      research_manifest,
      program_set = program_set,
      config = config,
      steps = requested_steps,
      research_strategy = research_strategy,
      parallel_writing = parallel_writing,
      remove_duplicate = remove_duplicate
    )
    if (!is.null(bound_manifest)) {
      research_manifest <<- bound_manifest
    }
    invisible(bound_manifest)
  }
  emit_persistence_started <- function(stage) {
    step <- paste0(stage, "_artifacts")
    if (!is.null(run_dir)) {
      emit_progress(
        "step",
        "started",
        stage = "persistence",
        step = step,
        payload = list(source_stage = stage)
      )
    }
    !is.null(run_dir)
  }
  emit_persistence_failed <- function(stage, error) {
    if (!is.null(run_dir)) {
      emit_progress(
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
  emit_persistence_succeeded <- function(stage) {
    if (!is.null(run_dir)) {
      emit_progress(
        "step",
        "succeeded",
        stage = "persistence",
        step = paste0(stage, "_artifacts"),
        payload = list(source_stage = stage, output_dir = run_dir)
      )
    }
    invisible(NULL)
  }
  seal_succeeded_workspace <- function() {
    if (!identical(research_manifest@status, "succeeded")) {
      return(invisible(workspace))
    }
    workspace_state <- tempest_research_workspace_mutation_state(workspace)
    if (identical(workspace_state, "open")) {
      tempest_research_workspace_seal(workspace)
    } else if (!identical(workspace_state, "sealed")) {
      tempest_stage_governance_abort(
        "A succeeded STORM workspace must be sealed before publication."
      )
    }
    invisible(workspace)
  }
  save_run_artifacts <- function(
    stage,
    persistence_started = FALSE,
    after_write = NULL
  ) {
    if (!isTRUE(persistence_started)) {
      emit_persistence_started(stage)
    }
    tryCatch(
      {
        write_run_bundle()
        if (!is.null(after_write)) {
          after_write()
        }
      },
      error = function(e) {
        emit_persistence_failed(stage, e)
        tempest_rethrow_operation(e, class = "tempest_run_error")
      }
    )
    emit_persistence_succeeded(stage)
  }
  persist_terminal_state <- function() {
    tryCatch(
      write_run_bundle(),
      error = function(error) invisible(NULL)
    )
    invisible(NULL)
  }

  completed_stages <- state$completed_stages
  run_manifest <- if (is.null(run_dir)) {
    NULL
  } else {
    tempest_storm_artifact_paths(run_dir)$run_config
  }
  if (
    !is.null(run_manifest) &&
      isTRUE(resume) &&
      file.exists(run_manifest)
  ) {
    loaded_run <- tempest_storm_load_artifacts(
      run_dir,
      workspace = workspace,
      config = config,
      program_set = program_set,
      run_id = supplied_run_id
    )
    workspace <- loaded_run$workspace
    workspace <- tempest_product_workspace_validate(
      workspace,
      knowledge,
      arg = "restored workspace"
    )
    store <- workspace
    if (!identical(tempest_storm_retriever_workspace(retriever), workspace)) {
      tempest_abort(
        "Restored STORM state must remain bound to the original retriever workspace.",
        class = tempest_persistence_error_class(
          "tempest_run_resume_error"
        )
      )
    }
    state <- loaded_run$state
    if (!identical(state$topic, topic)) {
      tempest_abort(
        paste0(
          "Cannot resume a STORM research run for a different topic. ",
          "The requested {.arg topic} must match the persisted state topic."
        ),
        class = tempest_persistence_error_class(
          "tempest_run_resume_error"
        )
      )
    }
    if (!identical(state$requested_steps, requested_steps)) {
      tempest_abort(
        paste0(
          "Cannot resume a STORM research run with different requested ",
          "steps. The requested {.arg steps} must match persisted state."
        ),
        class = tempest_persistence_error_class(
          "tempest_run_resume_error"
        )
      )
    }
    completed_stages <- state$completed_stages
    research_manifest <- loaded_run$research_manifest
    programs <- tempest_bind_program_set(
      program_set,
      research_manifest
    )
    programs <- tempest_programs_bind_knowledge_view(
      programs,
      knowledge$view
    )
    terminal_status <- research_manifest@status
    pending_steps <- setdiff(steps, completed_stages)
    if (terminal_status %in% c("failed", "cancelled")) {
      tempest_abort(
        paste0(
          "Cannot resume a {.val {terminal_status}} STORM research run. ",
          "Start a new run with a new {.arg run_id}."
        ),
        class = tempest_persistence_error_class(
          "tempest_run_resume_error"
        )
      )
    }
    if (identical(terminal_status, "succeeded") && length(pending_steps) > 0L) {
      tempest_abort(
        paste0(
          "Cannot execute additional stages for a succeeded STORM research ",
          "run. Start a new run with a new {.arg run_id}."
        ),
        class = tempest_persistence_error_class(
          "tempest_run_resume_error"
        )
      )
    }
    progress_run_id <- research_manifest@research_run_id
    if (verbose && length(completed_stages) > 0) {
      tempest_inform(
        "Loaded persisted STORM stages from {.path {run_dir}}: {paste(completed_stages, collapse = ', ')}"
      )
    }
  }

  if (identical(research_manifest@status, "succeeded")) {
    if (
      !tempest_storm_state_is_complete(state) ||
        !identical(
          tempest_research_workspace_mutation_state(workspace),
          "sealed"
        )
    ) {
      tempest_abort(
        "A succeeded STORM resume requires complete state and a sealed workspace.",
        class = tempest_persistence_error_class(
          "tempest_run_resume_error"
        )
      )
    }
    emit_progress(
      "workflow",
      "started",
      message = "Resuming completed STORM workflow.",
      payload = list(steps = steps, resume = TRUE)
    )
    emit_progress(
      "workflow",
      "succeeded",
      message = "Loaded completed STORM workflow.",
      payload = list(completed_stages = completed_stages)
    )
    return(list(
      title = state$title,
      perspectives = state$perspectives,
      experts = state$experts,
      outline = state$outline,
      draft_md = state$draft_md,
      report_md = state$report_md,
      manifest = research_manifest,
      state = state,
      workspace = workspace,
      retriever = retriever,
      output_dir = run_dir
    ))
  }

  stage_recorder <- function(commit_output = NULL) {
    if (!is.null(commit_output) && !is.function(commit_output)) {
      tempest_stage_evaluator_abort(
        "{.arg commit_output} must be `NULL` or a function."
      )
    }
    force(commit_output)
    function(record, output = NULL) {
      next_state <- rlang::duplicate(state, shallow = TRUE)
      next_state$stage_records <- tempest_stage_records_upsert(
        next_state$stage_records,
        record
      )
      if (!is.null(output) && is.function(commit_output)) {
        next_state <- commit_output(next_state, output)
      }
      next_state <- tempest_storm_state_validate(next_state)
      state <<- next_state
      invisible(record)
    }
  }
  record_stage <- stage_recorder()
  record_stages <- function(records, outputs = NULL) {
    next_state <- rlang::duplicate(state, shallow = TRUE)
    next_state$stage_records <- tempest_stage_records_upsert_many(
      next_state$stage_records,
      records
    )
    next_state <- tempest_storm_state_validate(next_state)
    state <<- next_state
    invisible(records)
  }
  process_expert_answer <- function(
    expert_chat,
    prompt,
    extractor,
    expert_id,
    perspective_id
  ) {
    answer <- expert_chat$chat(
      prompt,
      echo = if (verbose) "output" else "none"
    )
    completion_id <- tempest_agent_completion_id(answer)
    completion <- tempest_agent_completion_claim(
      completion_registry,
      completion_id,
      completion_owner
    )
    settled <- FALSE
    product_mutated <- FALSE
    on.exit(
      {
        if (!isTRUE(settled)) {
          if (isTRUE(product_mutated)) {
            try(
              tempest_agent_completion_consume(
                completion_registry,
                completion,
                completion_owner
              ),
              silent = TRUE
            )
          } else {
            try(
              tempest_agent_completion_release(
                completion_registry,
                completion,
                completion_owner
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
    research_manifest <<- tempest_storm_manifest_add_deputy_trace(
      research_manifest,
      trace
    )
    product_mutated <- TRUE
    harvest <- list(
      answer_text = answer$answer_text,
      source_ids = tempest_harvest_native_sources_from_turn(
        answer$provider_turn,
        store
      )
    )
    tempest_extract_facts_from_answer(
      extractor,
      harvest$answer_text,
      store,
      module = programs$extract_claims,
      source_ids = harvest$source_ids,
      session_id = progress_run_id,
      expert_id = expert_id,
      retrieval_step_id = trace$correlation_id,
      perspective_id = perspective_id,
      deputy_run_id = trace$deputy_run_id,
      deputy_session_id = trace$deputy_session_id,
      record_stage = record_stage
    )
    tempest_agent_completion_consume(
      completion_registry,
      completion,
      completion_owner
    )
    settled <- TRUE
    harvest
  }
  verification_ready <- FALSE
  ensure_verification_complete <- function() {
    claims <- store$list_proposed_claims()
    if (isTRUE(verification_ready)) {
      return(claims)
    }
    if (length(claims) == 0L) {
      verification_ready <<- TRUE
      return(list())
    }
    audit <- store$citation_audit
    claim_ids <- vapply(claims, \(claim) claim@claim_id, character(1))
    audit_complete <- !is.null(audit) &&
      setequal(audit$claim_id, claim_ids) &&
      all(vapply(
        claims,
        \(claim) !identical(claim@verification_status, "unverified"),
        logical(1)
      ))
    if (!audit_complete) {
      parent_progress_stage <- current_progress_stage
      emit_stage_started(
        "verification",
        message = "Verifying evidence for grounded generation."
      )
      tempest_run_verification(
        store,
        config,
        program = programs$verify_claim_support,
        record_stage = record_stage,
        record_stages = record_stages
      )
      emit_stage_succeeded(
        "verification",
        message = "Finished evidence verification.",
        payload = list(claim_count = length(claims))
      )
      current_progress_stage <<- parent_progress_stage
    }
    verification_ready <<- TRUE
    store$list_proposed_claims()
  }
  ensure_grounded_evidence <- function() {
    claims <- ensure_verification_complete()
    if (length(claims) == 0L) {
      tempest_stage_governance_abort(
        "Grounded STORM stages require at least one proposed evidence claim."
      )
    }
    verified <- tempest_supported_claims(
      store,
      min_support_score = config@min_support_score
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

  emit_progress(
    "workflow",
    "started",
    message = "Starting STORM workflow.",
    payload = list(steps = steps, resume = isTRUE(resume))
  )

  withCallingHandlers(
    {
      coordinator <- tempest_make_chat(
        config,
        "coordinator",
        echo = if (verbose) "output" else "none"
      )
      writer <- tempest_make_chat(
        config,
        "writer",
        echo = if (verbose) "output" else "none"
      )
      extractor <- tempest_make_chat(
        config,
        "judge",
        system_prompt = tempest_prompt("fact_extractor_system"),
        echo = "none"
      )
      tempest_research_attach_tools(
        writer,
        retriever,
        role = "writer",
        model = tempest_research_model(config, "writer"),
        search_provider = config@search_provider
      )

      title <- state$title
      perspectives <- state$perspectives
      experts <- state$experts
      expert_chats <- list() # Created per-perspective
      outline <- state$outline
      draft_md <- state$draft_md
      report_md <- state$report_md

      if (
        "perspectives" %in%
          steps &&
          !tempest_storm_stage_complete(completed_stages, "perspectives")
      ) {
        emit_stage_started(
          "perspectives",
          message = "Discovering perspectives and experts.",
          payload = list(n_experts = n_experts)
        )
        if (verbose) {
          tempest_inform("Discovering perspectives for: {.val {topic}}")
        }
        seed <- retriever$search(topic, k = min(5, config@max_search_results))
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

        # Enrich perspective discovery with ToC headings from top seed URLs
        toc_lines <- character()
        seed_urls <- head(seed$url, 3)
        for (u in seed_urls) {
          # Try Wikipedia sections first, then general ToC
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
          seed_txt <- paste0(
            seed_txt,
            "\n\n",
            paste(toc_lines, collapse = "\n")
          )
        }

        plan <- tempest_generate_perspectives(
          coordinator,
          topic,
          seed_txt,
          n_experts,
          module = programs$perspectives,
          record_stage = stage_recorder(function(next_state, output) {
            next_state$title <- output$title
            next_state$perspectives <- output$perspectives
            next_state
          })
        )
        title <- plan$title
        perspectives <- plan$perspectives

        if (length(experts) == 0L) {
          if (verbose) {
            tempest_inform("Generating {n_experts} expert profiles")
          }
          experts <- tempest_generate_experts_with_program(
            topic = topic,
            n = n_experts,
            config = config,
            verbose = verbose,
            module = programs$personas,
            record_stage = stage_recorder(function(next_state, output) {
              next_state$experts <- output
              next_state
            })
          )
        }
        if (length(experts) != length(perspectives)) {
          tempest_stage_governance_abort(
            "Every research perspective requires one explicit expert profile."
          )
        }
        completed_stages <- tempest_storm_mark_stage_complete(
          completed_stages,
          "perspectives"
        )
        state$completed_stages <- completed_stages
        state <- tempest_storm_state_validate(state)
        save_run_artifacts("perspectives")
        emit_stage_succeeded(
          "perspectives",
          message = "Finished perspectives and expert selection.",
          payload = list(
            perspective_count = length(perspectives),
            expert_count = length(experts)
          )
        )
      } else {
        if (
          verbose &&
            "perspectives" %in% steps &&
            tempest_storm_stage_complete(completed_stages, "perspectives")
        ) {
          tempest_inform("Using persisted perspectives from {.path {run_dir}}")
        }
        title <- state$title
        perspectives <- state$perspectives
        experts <- state$experts
        experts <- tempest_validate_experts(experts)
        if ("perspectives" %in% steps) {
          emit_stage_skipped(
            "perspectives",
            message = "Using persisted perspectives.",
            payload = list(reason = "already_complete")
          )
        }
      }

      if (
        "research" %in%
          steps &&
          !tempest_storm_stage_complete(completed_stages, "research")
      ) {
        emit_stage_started(
          "research",
          message = "Running STORM research loop.",
          payload = list(
            perspective_count = length(perspectives),
            research_strategy = research_strategy,
            parallel = isTRUE(parallel_research)
          )
        )
        if (verbose) {
          tempest_inform("Research loop: {length(perspectives)} perspectives")
        }

        if (length(perspectives) == 0L) {
          tempest_stage_governance_abort(
            "STORM research requires evaluated perspectives."
          )
        }
        if (length(experts) != length(perspectives)) {
          tempest_stage_governance_abort(
            "Every research perspective requires one explicit expert profile."
          )
        }

        # Create one Deputy-backed chat per selected expert/perspective pair.
        for (i in seq_along(perspectives)) {
          expert_profile <- experts[[i]]
          expert_record <- tempest_expert_runtime_record(expert_profile)
          expert_id <- expert_record$expert_id
          model_role <- expert_record$model_role
          if (is.na(model_role)) {
            model_role <- "expert"
          }
          model <- tempest_research_model(config, model_role)
          sp <- tempest_render_expert_prompt(
            persona = expert_profile,
            expert_id = expert_id
          )
          expert_chat <- tempest_make_chat(
            config,
            model_role,
            system_prompt = sp,
            echo = if (verbose) "output" else "none"
          )
          tempest_research_attach_tools(
            expert_chat,
            retriever,
            role = "expert",
            model = model,
            search_provider = config@search_provider,
            claim_provenance = list(
              session_id = progress_run_id,
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
              manifest = research_manifest,
              deputy_session_id = tempest_storm_deputy_session_id(
                progress_run_id,
                current_expert_id
              ),
              agent_name = current_name,
              stage = "research",
              role = "expert",
              expert_id = current_expert_id,
              completion_registry = completion_registry,
              on_run = function(trace) {
                trace <- tempest_storm_deputy_trace(trace, current_expert_id)
                assign(
                  trace$deputy_run_id,
                  trace,
                  envir = terminal_traces
                )
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
                  completion_registry,
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

        for (i in seq_along(perspectives)) {
          p <- perspectives[[i]]
          p_name <- p$name
          p_desc <- p$description
          qs <- p$key_questions

          expert <- expert_chats[[i]]
          expert_profile <- experts[[i]]
          expert_record <- tempest_expert_runtime_record(expert_profile)
          expert_name <- expert_record$name
          expert_id <- expert_record$expert_id
          perspective_id <- as.character(p$id %||% i)

          if (verbose) {
            tempest_inform("Perspective: {.val {p_name}}")
            if (!is.null(expert_profile)) {
              tempest_inform(
                "  Expert: {expert_name} ({expert_record$title})"
              )
            }
          }

          if (identical(research_strategy, "key_questions")) {
            # Ask expert to answer each planned key question (limited)
            qs_limited <- head(qs, max_questions_per_perspective)
            if (verbose && length(qs) > length(qs_limited)) {
              tempest_inform(
                "  Limiting to {length(qs_limited)} of {length(qs)} questions"
              )
            }
            for (q in qs_limited) {
              # Decompose query into targeted search queries
              decomposed <- tempest_decompose_query(
                writer,
                q,
                topic,
                module = programs$query_decomposition,
                max_queries = config@max_search_queries_per_turn,
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
              process_expert_answer(
                expert,
                prompt,
                extractor,
                expert_id,
                perspective_id
              )
            }
          } else {
            # Conversation-style interviewing: writer proposes the next best question,
            # expert answers with citations; repeat until done or max_rounds.
            answered <- character()
            for (round in seq_len(max_rounds)) {
              answered_md <- if (length(answered) == 0) {
                "(none yet)"
              } else {
                paste0("- ", answered, collapse = "\n")
              }
              facts_md <- tempest_summarize_facts_for_prompt(
                store,
                max_items = 60
              )

              nxt <- tempest_generate_next_question(
                writer,
                topic,
                p,
                answered_md = answered_md,
                facts_md = facts_md,
                module = programs$next_question,
                record_stage = record_stage
              )
              q <- nxt$question
              done <- isTRUE(nxt$done)

              # Decompose query into targeted search queries
              decomposed <- tempest_decompose_query(
                writer,
                q,
                topic,
                module = programs$query_decomposition,
                max_queries = config@max_search_queries_per_turn,
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

              harvest <- process_expert_answer(
                expert,
                prompt,
                extractor,
                expert_id,
                perspective_id
              )
              answered <- c(
                answered,
                paste0("Q: ", q, "\nA: ", harvest$answer_text)
              )

              if (done) break
            }
          }
        }

        completed_stages <- tempest_storm_mark_stage_complete(
          completed_stages,
          "research"
        )
        state$completed_stages <- completed_stages
        state <- tempest_storm_state_validate(state)
        save_run_artifacts("research")
        emit_stage_succeeded(
          "research",
          message = "Finished STORM research loop.",
          payload = list(
            source_count = length(store$list_retrieved_sources()),
            claim_count = length(store$list_proposed_claims())
          )
        )
      } else {
        if (
          verbose &&
            "research" %in% steps &&
            tempest_storm_stage_complete(completed_stages, "research")
        ) {
          tempest_inform(
            "Using persisted research artifacts from {.path {run_dir}}"
          )
        }
        if (
          "research" %in%
            steps &&
            tempest_storm_stage_complete(completed_stages, "research")
        ) {
          emit_stage_skipped(
            "research",
            message = "Using persisted research artifacts.",
            payload = list(reason = "already_complete")
          )
        }
      }

      if (
        "outline" %in%
          steps &&
          !tempest_storm_stage_complete(completed_stages, "outline")
      ) {
        emit_stage_started(
          "outline",
          message = "Generating STORM outline."
        )
        if (verbose) {
          tempest_inform("Generating outline (two-step)")
        }

        # Step 1: Draft outline from LLM knowledge alone
        draft_outline <- tempest_draft_outline(
          writer,
          topic,
          title,
          module = programs$draft_outline,
          record_stage = stage_recorder(function(next_state, output) {
            next_state$draft_outline <- output
            next_state
          })
        )

        # Step 2: Refined outline incorporating facts
        verified_evidence <- ensure_grounded_evidence()
        facts_txt <- tempest_summarize_facts_for_prompt(
          store,
          max_items = 80,
          verified_only = TRUE,
          min_support_score = config@min_support_score
        )
        outline <- tempest_refine_outline(
          writer,
          topic,
          title,
          draft_outline,
          facts_txt,
          module = programs$refined_outline,
          workspace = store,
          evidence = verified_evidence,
          verified_evidence = verified_evidence,
          verified_facts = facts_txt,
          min_support_score = config@min_support_score,
          record_stage = stage_recorder(function(next_state, output) {
            next_state$outline <- output
            next_state
          })
        )
        completed_stages <- tempest_storm_mark_stage_complete(
          completed_stages,
          "outline"
        )
        state$completed_stages <- completed_stages
        state <- tempest_storm_state_validate(state)
        save_run_artifacts("outline")
        emit_stage_succeeded(
          "outline",
          message = "Finished STORM outline.",
          payload = list(section_count = length(outline$sections %||% list()))
        )
      } else {
        if (
          verbose &&
            "outline" %in% steps &&
            tempest_storm_stage_complete(completed_stages, "outline")
        ) {
          tempest_inform("Using persisted outline from {.path {run_dir}}")
        }
        outline <- state$outline
        if ("outline" %in% steps) {
          emit_stage_skipped(
            "outline",
            message = "Using persisted outline.",
            payload = list(reason = "already_complete")
          )
        }
      }

      if (
        "write" %in%
          steps &&
          !tempest_storm_stage_complete(completed_stages, "write")
      ) {
        emit_stage_started(
          "write",
          message = "Writing STORM draft."
        )
        if (verbose) {
          tempest_inform("Writing draft")
        }
        if (is.null(outline) || is.null(outline$sections)) {
          tempest_abort(
            "No outline available; run steps including 'outline'.",
            class = c("tempest_run_error", "tempest_error")
          )
        }
        verified_evidence <- ensure_grounded_evidence()

        section_jobs <- tempest_section_jobs(
          outline,
          retriever,
          store,
          retrieve_top_k = config@retrieve_top_k,
          min_support_score = config@min_support_score
        )
        section_results <- tempest_write_section_jobs(
          section_jobs,
          writer,
          config = config,
          programs = programs,
          parallel = parallel_writing,
          verbose = verbose,
          record_stage = record_stage
        )
        parts <- purrr::map_chr(section_results, "markdown")

        draft_md <- paste(parts, collapse = "\n\n")

        # Generate Wikipedia-style lead section
        if (verbose) {
          tempest_inform("Generating lead section")
        }
        lead_facts <- tempest_summarize_facts_for_prompt(
          store,
          max_items = 40,
          verified_only = TRUE,
          min_support_score = config@min_support_score
        )
        article_body <- draft_md
        lead_section <- tempest_write_lead_section(
          writer,
          topic,
          title,
          draft_md,
          facts_txt = lead_facts,
          module = programs$lead_section,
          workspace = store,
          evidence = verified_evidence,
          verified_evidence = verified_evidence,
          verified_facts = lead_facts,
          min_support_score = config@min_support_score,
          verbose = verbose,
          record_stage = stage_recorder(function(next_state, output) {
            next_state$lead_section <- output
            next_state$draft_md <- paste0(output, "\n\n", article_body)
            next_state
          })
        )
        draft_md <- paste0(lead_section, "\n\n", draft_md)
        completed_stages <- tempest_storm_mark_stage_complete(
          completed_stages,
          "write"
        )
        state$completed_stages <- completed_stages
        state <- tempest_storm_state_validate(state)
        save_run_artifacts("write")
        emit_stage_succeeded(
          "write",
          message = "Finished STORM draft.",
          payload = list(section_count = length(section_results))
        )
      } else {
        if (
          verbose &&
            "write" %in% steps &&
            tempest_storm_stage_complete(completed_stages, "write")
        ) {
          tempest_inform("Using persisted draft article from {.path {run_dir}}")
        }
        draft_md <- state$draft_md
        if ("write" %in% steps) {
          emit_stage_skipped(
            "write",
            message = "Using persisted draft article.",
            payload = list(reason = "already_complete")
          )
        }
      }

      if (
        "polish" %in%
          steps &&
          !tempest_storm_stage_complete(completed_stages, "polish")
      ) {
        emit_stage_started(
          "polish",
          message = "Polishing STORM report."
        )
        if (verbose) {
          tempest_inform("Polishing and consistency pass")
        }
        if (config@citation_policy %in% c("claim_verified", "strict")) {
          ensure_verification_complete()
        }
        candidate_report_md <- tempest_storm_polish_report(
          title = title,
          draft_md = draft_md,
          workspace = workspace,
          config = config
        )
        candidate_report_md <- tempest_storm_report_with_execution_review(
          candidate_report_md,
          state$stage_records,
          title = title
        )
        candidate_completed_stages <- tempest_storm_mark_stage_complete(
          completed_stages,
          "polish"
        )
        candidate_state <- rlang::duplicate(state, shallow = TRUE)
        candidate_state$report_md <- candidate_report_md
        candidate_state$completed_stages <- candidate_completed_stages
        candidate_state <- state_with_references(candidate_state)
        publication_persistence_started <- emit_persistence_started("polish")
        candidate_manifest <- tryCatch(
          tempest_product_authority_finalize_manifest(
            manifest = research_manifest,
            stage_records = candidate_state$stage_records,
            workspace = workspace,
            report_md = candidate_report_md,
            config = config,
            experts = candidate_state$experts,
            product_state = candidate_state,
            status = "succeeded",
            require_publishable = TRUE
          ),
          error = function(error) {
            emit_persistence_failed("polish", error)
            rlang::cnd_signal(error)
          }
        )
        report_md <- candidate_report_md
        completed_stages <- candidate_completed_stages
        state <- candidate_state
        research_manifest <- candidate_manifest
        save_run_artifacts(
          "polish",
          persistence_started = publication_persistence_started,
          after_write = seal_succeeded_workspace
        )
        emit_stage_succeeded(
          "polish",
          message = "Finished polished STORM report.",
          payload = list(has_report = !is.null(report_md))
        )
      } else {
        if (
          verbose &&
            "polish" %in% steps &&
            tempest_storm_stage_complete(completed_stages, "polish")
        ) {
          tempest_inform(
            "Using persisted polished report from {.path {run_dir}}"
          )
        }
        report_md <- state$report_md
        if ("polish" %in% steps) {
          emit_stage_skipped(
            "polish",
            message = "Using persisted polished report.",
            payload = list(reason = "already_complete")
          )
        }
      }

      if (!is.null(report_md)) {
        emit_progress(
          "artifact",
          "available",
          stage = "polish",
          step = "report_md",
          payload = list(artifact = "report_md", persisted = !is.null(run_dir))
        )
      }
      product_complete <- tempest_storm_state_is_complete(state)
      if (
        is.null(run_dir) &&
          identical(research_manifest@status, "running") &&
          !isTRUE(product_complete)
      ) {
        research_manifest <- tempest_product_authority_finalize_manifest(
          manifest = research_manifest,
          stage_records = state$stage_records,
          workspace = workspace,
          report_md = NULL,
          config = config,
          experts = state$experts,
          product_state = state,
          status = "running",
          require_publishable = FALSE
        )
      }
      if (
        isTRUE(product_complete) &&
          !identical(research_manifest@status, "succeeded")
      ) {
        tempest_stage_governance_abort(
          "A complete STORM product requires atomic publication authority."
        )
      }
      emit_progress(
        "workflow",
        "succeeded",
        message = "Finished STORM workflow.",
        payload = list(completed_stages = completed_stages)
      )

      list(
        title = title,
        perspectives = perspectives,
        experts = experts,
        outline = outline,
        draft_md = draft_md,
        report_md = report_md,
        manifest = research_manifest,
        state = state,
        workspace = workspace,
        retriever = retriever,
        output_dir = run_dir
      )
    },
    error = function(e) {
      if (identical(research_manifest@status, "running")) {
        persist_terminal_state()
      }
      stage <- current_progress_stage
      if (!is.na(stage)) {
        emit_progress_best_effort(
          "stage",
          "failed",
          stage = stage,
          payload = tempest_progress_error_payload(e)
        )
        current_progress_stage <<- NA_character_
      }
      emit_progress_best_effort(
        "workflow",
        "failed",
        message = "STORM workflow failed.",
        payload = c(list(stage = stage), tempest_progress_error_payload(e))
      )
      tempest_rethrow_operation(e, class = "tempest_run_error")
    },
    interrupt = function(e) {
      if (identical(research_manifest@status, "running")) {
        persist_terminal_state()
      }
      stage <- current_progress_stage
      emit_progress_best_effort(
        "cancellation",
        "cancelled",
        stage = stage,
        message = "STORM workflow cancelled.",
        payload = list(stage = stage)
      )
      tempest_abort(
        "STORM workflow was cancelled.",
        class = c(
          "tempest_run_cancelled",
          "tempest_run_error",
          "tempest_error",
          "interrupt"
        )
      )
    }
  )
}

#' Run STORM asynchronously (Shiny-friendly)
#'
#' This runs [tempest_run()] in a Mirai worker and returns a promise immediately.
#' Use [tempest_run_cancel()] to stop a run that is no longer needed.
#'
#' @param ... Arguments passed to [tempest_run()]. See [tempest_run()] for details
#'   on available parameters including `topic`, `config`, `retriever`,
#'   `n_experts`, `research_strategy`, `max_rounds`, `steps`, and `verbose`.
#' @param knowledge_view A live pinned Graft view cannot cross the asynchronous
#'   worker boundary. Governed runs must use [tempest_run()] in the process that
#'   owns the view.
#' @return A `tempest_async_run` promise that resolves with the
#'   [tempest_run()] result.
#' @seealso [tempest_run()] for the synchronous version.
#' @examples
#' \dontrun{
#' tempest_run_async("History of jazz", config = tempest_config()) |>
#'   promises::then(function(result) cat(result$report_md))
#' }
#' @export
tempest_run_async <- function(..., knowledge_view = NULL) {
  args <- list(...)
  program_set <- args$program_set %||% NULL
  if (
    !is.null(knowledge_view) ||
      (!is.null(program_set) &&
        tempest_program_set_requires_knowledge_view(program_set))
  ) {
    tempest_governed_procedure_abort(
      paste0(
        "{.fn tempest_run_async} never serializes a live pinned ",
        "{.arg knowledge_view}. Run the governed workflow with ",
        "{.fn tempest_run} in the process that owns the view."
      )
    )
  }
  tempest_require("promises", "tempest_run_async() uses promises.")
  tempest_require("mirai", "tempest_run_async() uses a Mirai worker.")
  runner <- getOption("tempest.async_runner", tempest_run)
  if (!is.function(runner)) {
    tempest_abort(
      "The configured async runner must be a function.",
      class = c("tempest_async_error", "tempest_error")
    )
  }
  job <- mirai::mirai(
    {
      tryCatch(
        list(ok = TRUE, value = do.call(runner, args)),
        error = function(error) list(ok = FALSE, condition = error)
      )
    },
    runner = runner,
    args = args
  )
  promise <- promises::then(
    promises::as.promise(job),
    function(result) {
      if (inherits(result, "errorValue")) {
        tempest_abort(
          "Asynchronous STORM run was cancelled.",
          class = c(
            "tempest_async_cancelled",
            "tempest_async_error",
            "tempest_error"
          )
        )
      }
      if (!is.list(result) || !isTRUE(result$ok)) {
        condition <- result$condition %||%
          simpleError(
            "Asynchronous STORM worker returned an invalid result."
          )
        tempest_rethrow_operation(condition, class = "tempest_async_error")
      }
      result$value
    },
    onRejected = function(error) {
      if (grepl("Operation canceled", conditionMessage(error), fixed = TRUE)) {
        tempest_abort(
          "Asynchronous STORM run was cancelled.",
          class = c(
            "tempest_async_cancelled",
            "tempest_async_error",
            "tempest_error"
          )
        )
      }
      tempest_rethrow_operation(error, class = "tempest_async_error")
    }
  )
  attr(promise, "tempest_mirai") <- job
  class(promise) <- c("tempest_async_run", class(promise))
  promise
}

#' Cancel an asynchronous STORM run
#'
#' Stops the Mirai worker owned by a promise returned from
#' [tempest_run_async()]. Cancellation is idempotent after a run has settled.
#'
#' @param run A `tempest_async_run` returned by [tempest_run_async()].
#' @return Invisibly returns `TRUE` when cancellation was requested and
#'   `FALSE` when the run had already settled.
#' @export
tempest_run_cancel <- function(run) {
  if (!inherits(run, "tempest_async_run")) {
    tempest_abort(
      "{.arg run} must be returned by {.fn tempest_run_async}.",
      class = c("tempest_async_error", "tempest_error")
    )
  }
  job <- attr(run, "tempest_mirai", exact = TRUE)
  if (is.null(job) || !mirai::unresolved(job)) {
    return(invisible(FALSE))
  }
  mirai::stop_mirai(job)
  invisible(TRUE)
}
