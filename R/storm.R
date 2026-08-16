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
tempest_run_verification <- function(
  store,
  config,
  verifier = NULL,
  modules = NULL
) {
  if (!config@citation_policy %in% c("claim_verified", "strict")) {
    return(invisible(NULL))
  }
  verifier <- verifier %||% tempest_make_chat(config, "judge")
  # Verification runs after the expensive polish step; never let it abort the
  # run -- degrade to an unverified report instead.
  tryCatch(
    tempest_verify_claims(
      store,
      verifier = verifier,
      policy = config@citation_policy,
      verifier_model = config@models[["judge"]] %||% NA_character_,
      modules = modules,
      min_support_score = config@min_support_score
    ),
    error = function(e) {
      tempest_warn(
        "Citation verification failed; report left unverified: {conditionMessage(e)}"
      )
      NULL
    }
  )
  invisible(NULL)
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
#' @param dsprrr_modules Optional named list of dsprrr modules, typically from
#'   [tempest_optimize_dsprrr_modules()]. If `NULL`, fresh modules are created.
#' @param steps Character vector controlling which steps to run. Defaults to all.
#' @param output_dir Optional directory for persisted STORM run artifacts. When
#'   supplied, artifacts are written under a topic-specific subdirectory.
#' @param resume If `TRUE` and `output_dir` contains a previous run, load saved
#'   artifacts and skip stages recorded as complete.
#' @param run_id Optional run directory name. Defaults to a slug of `topic`.
#' @param remove_duplicate If `TRUE`, ask the polish step to remove duplicate
#'   or highly repetitive content while preserving unique cited claims.
#' @param progress Optional function called with a `tempest_progress_event`
#'   object as STORM workflow stages start, finish, fail, persist artifacts, or
#'   make final artifacts available.
#' @param verbose If `TRUE`, prints progress messages.
#' @param .state Internal adapter-only fixed STORM product state. This is not a
#'   public continuation API.
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
  n_experts = 3,
  experts = NULL,
  research_strategy = c("key_questions", "conversation"),
  max_rounds = 3,
  max_questions_per_perspective = 3,
  parallel_research = FALSE,
  parallel_writing = FALSE,
  dsprrr_modules = NULL,
  steps = c("perspectives", "research", "outline", "write", "polish"),
  output_dir = NULL,
  resume = FALSE,
  run_id = NULL,
  remove_duplicate = FALSE,
  progress = NULL,
  verbose = TRUE,
  .state = NULL
) {
  tempest_run_internal(
    topic = topic,
    config = config,
    retriever = retriever,
    n_experts = n_experts,
    experts = experts,
    research_strategy = research_strategy,
    max_rounds = max_rounds,
    max_questions_per_perspective = max_questions_per_perspective,
    parallel_research = parallel_research,
    parallel_writing = parallel_writing,
    dsprrr_modules = dsprrr_modules,
    steps = steps,
    output_dir = output_dir,
    resume = resume,
    run_id = run_id,
    remove_duplicate = remove_duplicate,
    progress = progress,
    verbose = verbose,
    .state = .state
  )
}

tempest_run_internal <- function(
  topic,
  config = tempest_config(),
  retriever = NULL,
  n_experts = 3,
  experts = NULL,
  research_strategy = c("key_questions", "conversation"),
  max_rounds = 3,
  max_questions_per_perspective = 3,
  parallel_research = FALSE,
  parallel_writing = FALSE,
  dsprrr_modules = NULL,
  steps = c("perspectives", "research", "outline", "write", "polish"),
  output_dir = NULL,
  resume = FALSE,
  run_id = NULL,
  remove_duplicate = FALSE,
  progress = NULL,
  verbose = TRUE,
  .state = NULL,
  runtime = tempest_runtime(),
  runtime_factory = function() tempest_runtime(),
  connection_permissions = list(),
  artifact_catalog = NULL
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
  if (!inherits(runtime, "TempestRuntime")) {
    tempest_runtime_abort(
      "{.arg runtime} must be created by {.fn tempest_runtime}."
    )
  }
  if (!is.function(runtime_factory)) {
    tempest_runtime_abort("{.arg runtime_factory} must be a function.")
  }
  connection_permissions <- tempest_run_connection_permissions(
    connection_permissions,
    runtime
  )
  if (
    !is.null(artifact_catalog) &&
      !inherits(artifact_catalog, "TempestArtifactCatalog")
  ) {
    tempest_config_abort(
      "{.arg artifact_catalog} must be a TempestArtifactCatalog or NULL."
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
  parallel_writing <- tempest_config_flag(parallel_writing, "parallel_writing")
  resume <- tempest_config_flag(resume, "resume")
  remove_duplicate <- tempest_config_flag(remove_duplicate, "remove_duplicate")
  verbose <- tempest_config_flag(verbose, "verbose")
  allowed_steps <- c("perspectives", "research", "outline", "write", "polish")
  if (
    !is.character(steps) ||
      length(steps) == 0L ||
      anyNA(steps) ||
      length(setdiff(steps, allowed_steps)) > 0L
  ) {
    tempest_config_abort(
      "{.arg steps} must contain only: {.val {allowed_steps}}."
    )
  }
  steps <- unique(steps)

  retriever_supplied <- !is.null(retriever)
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
  store <- workspace
  run_dir <- tempest_prepare_run_dir(output_dir, topic, run_id = run_id)
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
  state <- if (is.null(.state)) {
    tempest_storm_state(
      topic = topic,
      experts = experts %||% list()
    )
  } else {
    tempest_storm_state_validate(.state)
  }
  if (!identical(state$topic, topic)) {
    tempest_storm_state_abort(
      "{.field topic} in {.arg .state} must match {.arg topic}."
    )
  }
  if (length(experts %||% list()) > 0L && length(state$experts) > 0L) {
    state_experts <- tempest_expert_records(state$experts)
    supplied_experts <- tempest_expert_records(experts)
    if (!identical(state_experts, supplied_experts)) {
      tempest_storm_state_abort(
        "{.field experts} in {.arg .state} must match {.arg experts}."
      )
    }
  } else if (length(state$experts) == 0L && length(experts %||% list()) > 0L) {
    state$experts <- experts
    state <- tempest_storm_state_validate(state)
  }
  dsprrr_modules <- dsprrr_modules %||% tempest_make_dsprrr_modules(config)
  program_references <- tempest_program_references(dsprrr_modules)
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
  dsprrr_modules <- tempest_bind_dsprrr_trace_context(
    dsprrr_modules,
    research_manifest
  )
  artifact_catalog <- artifact_catalog %||%
    tempest_artifact_catalog(store = config@artifact_store)
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
  refresh_state_references <- function() {
    cited_md <- state$report_md %||% state$draft_md %||% ""
    cited_ids <- tempest_extract_citation_ids(cited_md)
    state$references <<- Filter(
      Negate(is.null),
      lapply(cited_ids, function(id) workspace$get_retrieved_source(id))
    )
    state <<- tempest_storm_state_validate(state)
    invisible(state)
  }
  write_run_bundle <- function() {
    refresh_state_references()
    if (is.null(run_dir)) {
      return(invisible(NULL))
    }
    tempest_save_run_artifacts(
      run_dir,
      workspace,
      state,
      research_manifest,
      config = config,
      steps = steps,
      research_strategy = research_strategy,
      parallel_writing = parallel_writing,
      remove_duplicate = remove_duplicate
    )
  }
  save_run_artifacts <- function(stage) {
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
    tryCatch(
      write_run_bundle(),
      error = function(e) {
        if (!is.null(run_dir)) {
          emit_progress(
            "step",
            "failed",
            stage = "persistence",
            step = step,
            payload = c(
              list(source_stage = stage),
              tempest_progress_error_payload(e)
            )
          )
        }
        stop(e)
      }
    )
    if (!is.null(run_dir)) {
      emit_progress(
        "step",
        "succeeded",
        stage = "persistence",
        step = step,
        payload = list(source_stage = stage, output_dir = run_dir)
      )
    }
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
    tempest_run_artifact_paths(run_dir)$run_config
  }
  if (
    !is.null(run_manifest) &&
      isTRUE(resume) &&
      file.exists(run_manifest)
  ) {
    loaded_run <- tempest_load_run_artifacts(
      run_dir,
      workspace = if (retriever_supplied) workspace else NULL,
      config = config,
      run_id = supplied_run_id
    )
    workspace <- loaded_run$workspace
    store <- workspace
    if (!identical(tempest_storm_retriever_workspace(retriever), workspace)) {
      retriever <- tempest_retriever(config = config, workspace = workspace)
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
    completed_stages <- state$completed_stages
    research_manifest <- loaded_run$research_manifest
    dsprrr_modules <- tempest_bind_dsprrr_trace_context(
      dsprrr_modules,
      research_manifest
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
      polisher <- tempest_make_chat(
        config,
        "writer",
        system_prompt = tempest_prompt("polisher_system"),
        echo = if (verbose) "output" else "none"
      )
      extractor <- tempest_make_chat(
        config,
        "judge",
        system_prompt = tempest_prompt("fact_extractor_system"),
        echo = "none"
      )
      writer_resolution <- runtime$resolve_role(
        "writer",
        required_capability_ids = "tempest.evidence.read",
        optional_capability_ids = "tempest.retrieval.semantic",
        context = list(
          retriever = retriever,
          model = tempest_runtime_model(config, "writer"),
          search_provider = config@search_provider,
          run_id = progress_run_id
        )
      )
      runtime$attach(
        writer,
        writer_resolution,
        context = list(
          retriever = retriever,
          run_id = progress_run_id
        )
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
          !tempest_stage_complete(completed_stages, "perspectives")
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
          module = dsprrr_modules$perspectives
        )
        title <- plan$title %||% topic
        perspectives <- plan$perspectives %||% list()
        state$title <- title
        state$perspectives <- perspectives

        if (length(experts) == 0L) {
          if (verbose) {
            tempest_inform("Generating {n_experts} expert profiles")
          }
          experts <- tempest_generate_experts(
            topic = topic,
            n = n_experts,
            config = config,
            verbose = verbose,
            module = dsprrr_modules$personas
          )
        }
        state$experts <- experts
        completed_stages <- tempest_mark_stage_complete(
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
            tempest_stage_complete(completed_stages, "perspectives")
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
          !tempest_stage_complete(completed_stages, "research")
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

        if (length(perspectives) == 0) {
          # Fallback: single perspective
          perspectives <- list(list(
            name = "Overview",
            description = "General overview",
            key_questions = c(topic)
          ))
          state$perspectives <- perspectives
        }

        if (
          isTRUE(parallel_research) &&
            tempest_has("mirai") &&
            identical(research_strategy, "key_questions")
        ) {
          if (verbose) {
            tempest_inform(
              "Running research in parallel ({length(perspectives)} perspectives)"
            )
          }
          tempest_research_parallel(
            perspectives,
            experts,
            config,
            runtime,
            runtime_factory,
            connection_permissions,
            retriever,
            store,
            topic,
            research_strategy,
            max_rounds,
            max_questions_per_perspective,
            dsprrr_modules,
            verbose,
            run_id = progress_run_id
          )
        } else {
          # Sequential research loop
          # Create one chat per selected expert/perspective pair.
          for (i in seq_along(perspectives)) {
            expert_profile <- if (i <= length(experts)) {
              experts[[i]]
            } else {
              tempest_fallback_expert_profile(i)
            }
            expert_record <- tempest_expert_runtime_record(expert_profile)
            expert_id <- expert_record$expert_id
            model_role <- expert_record$model_role
            if (is.na(model_role)) {
              model_role <- "expert"
            }
            model <- tempest_runtime_model(config, model_role)
            sp <- tempest_render_expert_prompt(
              persona = expert_profile,
              expert_id = expert_id
            )
            expert_chats[[i]] <- tempest_make_chat(
              config,
              model_role,
              system_prompt = sp,
              echo = if (verbose) "output" else "none"
            )
            capability_resolution <- runtime$resolve_expert(
              expert_profile,
              allowed_connection_ref_ids = tempest_storm_allowed_connection_ref_ids(
                connection_permissions,
                expert_id,
                model_role
              ),
              context = list(
                retriever = retriever,
                model = model,
                search_provider = config@search_provider,
                claim_provenance = list(
                  session_id = progress_run_id,
                  expert_id = expert_id
                )
              )
            )
            runtime$attach(
              expert_chats[[i]],
              capability_resolution,
              context = list(
                run_id = progress_run_id,
                expert_id = expert_id
              )
            )
          }

          for (i in seq_along(perspectives)) {
            p <- perspectives[[i]]
            p_name <- p$name %||% "Perspective"
            p_desc <- p$description %||% ""
            qs <- p$key_questions %||% c(topic)

            expert <- expert_chats[[i]]
            expert_profile <- if (i <= length(experts)) {
              experts[[i]]
            } else {
              tempest_fallback_expert_profile(i)
            }
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
                decomposed <- tryCatch(
                  tempest_decompose_query(
                    writer,
                    q,
                    topic,
                    module = dsprrr_modules$query_decomposition,
                    max_queries = config@max_search_queries_per_turn
                  ),
                  error = function(e) {
                    tempest_warn(
                      "Query decomposition failed, using original query: {conditionMessage(e)}"
                    )
                    list(queries = list(q))
                  }
                )
                search_instructions <- paste0(
                  "Suggested search queries:\n",
                  paste0("- ", decomposed$queries %||% q, collapse = "\n"),
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
                ans <- tryCatch(
                  expert$chat(prompt, echo = if (verbose) "output" else "none"),
                  error = function(e) {
                    tempest_warn(
                      "Expert answer failed for {.val {p_name}}: {conditionMessage(e)}"
                    )
                    NULL
                  }
                )
                if (!is.null(ans)) {
                  harvest <- tempest_turn_answer_and_sources(expert, ans, store)
                  tryCatch(
                    tempest_extract_facts_from_answer(
                      extractor,
                      harvest$answer_text,
                      store,
                      module = dsprrr_modules$extract_claims,
                      source_ids = harvest$source_ids,
                      session_id = progress_run_id,
                      expert_id = expert_id,
                      perspective_id = perspective_id
                    ),
                    error = function(e) {
                      tempest_warn(
                        "Fact extraction failed: {conditionMessage(e)}"
                      )
                    }
                  )
                }
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
                  module = dsprrr_modules$next_question
                )
                q <- tempest_trim(nxt$question %||% "")
                done <- isTRUE(nxt$done)

                if (is.na(q) || q == "") {
                  break
                }

                # Decompose query into targeted search queries
                decomposed <- tryCatch(
                  tempest_decompose_query(
                    writer,
                    q,
                    topic,
                    module = dsprrr_modules$query_decomposition,
                    max_queries = config@max_search_queries_per_turn
                  ),
                  error = function(e) {
                    tempest_warn(
                      "Query decomposition failed, using original query: {conditionMessage(e)}"
                    )
                    list(queries = list(q))
                  }
                )
                search_instructions <- paste0(
                  "Suggested search queries:\n",
                  paste0("- ", decomposed$queries %||% q, collapse = "\n"),
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

                ans <- tryCatch(
                  expert$chat(prompt, echo = if (verbose) "output" else "none"),
                  error = function(e) {
                    tempest_warn(
                      "Expert answer failed for {.val {p_name}}: {conditionMessage(e)}"
                    )
                    NULL
                  }
                )
                if (is.null(ans)) {
                  if (done) {
                    break
                  }
                  next
                }
                harvest <- tempest_turn_answer_and_sources(expert, ans, store)
                answered <- c(
                  answered,
                  paste0("Q: ", q, "\nA: ", harvest$answer_text)
                )
                tryCatch(
                  tempest_extract_facts_from_answer(
                    extractor,
                    harvest$answer_text,
                    store,
                    module = dsprrr_modules$extract_claims,
                    source_ids = harvest$source_ids,
                    session_id = progress_run_id,
                    expert_id = expert_id,
                    perspective_id = perspective_id
                  ),
                  error = function(e) {
                    tempest_warn(
                      "Fact extraction failed: {conditionMessage(e)}"
                    )
                  }
                )

                if (done) break
              }
            }
          }
        }

        completed_stages <- tempest_mark_stage_complete(
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
            tempest_stage_complete(completed_stages, "research")
        ) {
          tempest_inform(
            "Using persisted research artifacts from {.path {run_dir}}"
          )
        }
        if (
          "research" %in%
            steps &&
            tempest_stage_complete(completed_stages, "research")
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
          !tempest_stage_complete(completed_stages, "outline")
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
          module = dsprrr_modules$draft_outline
        )
        state$draft_outline <- draft_outline

        # Step 2: Refined outline incorporating facts
        facts_txt <- tempest_summarize_facts_for_prompt(store, max_items = 80)
        outline <- tempest_refine_outline(
          writer,
          topic,
          title,
          draft_outline,
          facts_txt,
          module = dsprrr_modules$refined_outline
        )
        state$outline <- outline
        completed_stages <- tempest_mark_stage_complete(
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
            tempest_stage_complete(completed_stages, "outline")
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
        "write" %in% steps && !tempest_stage_complete(completed_stages, "write")
      ) {
        emit_stage_started(
          "write",
          message = "Writing STORM draft."
        )
        if (verbose) {
          tempest_inform("Writing draft")
        }
        if (is.null(outline) || is.null(outline$sections)) {
          tempest_abort("No outline available; run steps including 'outline'.")
        }

        section_jobs <- tempest_section_jobs(
          outline,
          retriever,
          store,
          retrieve_top_k = config@retrieve_top_k
        )
        section_results <- tempest_write_section_jobs(
          section_jobs,
          writer,
          config = config,
          dsprrr_modules = dsprrr_modules,
          parallel = parallel_writing,
          verbose = verbose
        )
        parts <- purrr::map_chr(section_results, "markdown")

        for (section_result in section_results) {
          # Extract any newly-cited facts from the section itself
          tempest_extract_facts_from_answer(
            extractor,
            section_result$section_text,
            store,
            module = dsprrr_modules$extract_claims,
            session_id = progress_run_id,
            section_id = section_result$title %||% NA_character_
          )
        }

        draft_md <- paste(parts, collapse = "\n\n")

        # Generate Wikipedia-style lead section
        if (verbose) {
          tempest_inform("Generating lead section")
        }
        lead_facts <- tempest_summarize_facts_for_prompt(store, max_items = 40)
        lead_section <- tempest_write_lead_section(
          writer,
          topic,
          title,
          draft_md,
          facts_txt = lead_facts,
          module = dsprrr_modules$lead_section,
          verbose = verbose
        )
        draft_md <- paste0(lead_section, "\n\n", draft_md)
        state$lead_section <- lead_section
        state$draft_md <- draft_md
        completed_stages <- tempest_mark_stage_complete(
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
            tempest_stage_complete(completed_stages, "write")
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
          !tempest_stage_complete(completed_stages, "polish")
      ) {
        emit_stage_started(
          "polish",
          message = "Polishing STORM report."
        )
        if (verbose) {
          tempest_inform("Polishing and consistency pass")
        }
        report_plan <- tempest_storm_report_plan(
          title = title,
          draft_md = draft_md,
          workspace = workspace,
          config = config,
          remove_duplicate = remove_duplicate,
          catalog = artifact_catalog,
          run_id = progress_run_id,
          generate_text = function(prompt) {
            polisher$chat(
              prompt,
              echo = if (verbose) "output" else "none"
            )
          }
        )
        polished <- tempest_deliverable_generate(report_plan)
        if (config@citation_policy %in% c("claim_verified", "strict")) {
          emit_progress(
            "stage",
            "started",
            stage = "verification",
            message = "Verifying cited claims."
          )
          tryCatch(
            tempest_run_verification(store, config, modules = dsprrr_modules),
            error = function(e) {
              emit_progress(
                "stage",
                "failed",
                stage = "verification",
                message = "Citation verification failed.",
                payload = tempest_progress_error_payload(e)
              )
              stop(e)
            }
          )
          emit_progress(
            "stage",
            "succeeded",
            stage = "verification",
            message = "Finished citation verification.",
            payload = list(
              claim_count = length(store$list_proposed_claims())
            )
          )
        } else {
          emit_progress(
            "stage",
            "skipped",
            stage = "verification",
            message = "Citation verification skipped.",
            payload = list(citation_policy = config@citation_policy)
          )
        }
        deliverable_result <- tempest_deliverable_finalize(
          report_plan,
          polished
        )
        report_artifact <- tempest_deliverable_primary_artifact(
          deliverable_result
        )
        report_md <- report_artifact@content
        state$report_md <- report_md
        completed_stages <- tempest_mark_stage_complete(
          completed_stages,
          "polish"
        )
        state$completed_stages <- completed_stages
        state <- tempest_storm_state_validate(state)
        save_run_artifacts("polish")
        emit_stage_succeeded(
          "polish",
          message = "Finished polished STORM report.",
          payload = list(has_report = !is.null(report_md))
        )
      } else {
        if (
          verbose &&
            "polish" %in% steps &&
            tempest_stage_complete(completed_stages, "polish")
        ) {
          tempest_inform(
            "Using persisted polished report from {.path {run_dir}}"
          )
        }
        report_md <- state$report_md
        if (
          !is.null(report_md) &&
            !artifact_catalog$has("report_md")
        ) {
          tempest_storm_restore_report_artifact(
            report_md = report_md,
            title = title,
            config = config,
            remove_duplicate = remove_duplicate,
            catalog = artifact_catalog,
            run_id = progress_run_id
          )
        }
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
        identical(research_manifest@status, "running") &&
          isTRUE(product_complete)
      ) {
        running_manifest <- research_manifest
        research_manifest <- tempest_research_manifest_update(
          research_manifest,
          status = "succeeded"
        )
        tryCatch(
          write_run_bundle(),
          error = function(error) {
            research_manifest <<- running_manifest
            stop(error)
          }
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
      if (inherits(e, "tempest_progress_callback_error")) {
        return()
      }
      if (identical(research_manifest@status, "running")) {
        research_manifest <<- tempest_research_manifest_update(
          research_manifest,
          status = "failed"
        )
        persist_terminal_state()
      }
      stage <- current_progress_stage
      if (!is.na(stage)) {
        emit_progress(
          "stage",
          "failed",
          stage = stage,
          payload = tempest_progress_error_payload(e)
        )
        current_progress_stage <<- NA_character_
      }
      emit_progress(
        "workflow",
        "failed",
        message = "STORM workflow failed.",
        payload = c(list(stage = stage), tempest_progress_error_payload(e))
      )
    },
    interrupt = function(e) {
      if (inherits(e, "tempest_progress_callback_error")) {
        return()
      }
      if (identical(research_manifest@status, "running")) {
        research_manifest <<- tempest_research_manifest_update(
          research_manifest,
          status = "cancelled"
        )
        persist_terminal_state()
      }
      stage <- current_progress_stage
      emit_progress(
        "cancellation",
        "cancelled",
        stage = stage,
        message = "STORM workflow cancelled.",
        payload = list(stage = stage)
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
#' @return A `tempest_async_run` promise that resolves with the
#'   [tempest_run()] result.
#' @seealso [tempest_run()] for the synchronous version.
#' @examples
#' \dontrun{
#' tempest_run_async("History of jazz", config = tempest_config()) |>
#'   promises::then(function(result) cat(result$report_md))
#' }
#' @export
tempest_run_async <- function(...) {
  tempest_require("promises", "tempest_run_async() uses promises.")
  tempest_require("mirai", "tempest_run_async() uses a Mirai worker.")
  args <- list(...)
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
        stop(condition)
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
      stop(error)
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
