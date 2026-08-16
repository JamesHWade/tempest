test_that("tempest_run rejects invalid runtime budgets before provider work", {
  skip_if_not_installed("ellmer")
  cfg <- tempest_config(max_active_experts = 2L)
  invalid <- list(
    list(topic = character()),
    list(topic = "Topic", n_experts = 0),
    list(topic = "Topic", n_experts = 3),
    list(topic = "Topic", max_rounds = NA_real_),
    list(topic = "Topic", max_questions_per_perspective = Inf),
    list(topic = "Topic", parallel_research = NA),
    list(topic = "Topic", steps = "unknown")
  )

  for (args in invalid) {
    expect_error(
      do.call(tempest_run, c(args, list(config = cfg))),
      class = "tempest_config_error"
    )
  }
})

test_that("tempest_run rejects a mismatched TempestRetriever before execution", {
  skip_if_not_installed("ellmer")
  chat_calls <- 0L
  run_config <- tempest_config(
    max_search_results = 2L,
    chat_fn = function(role, model, system_prompt, echo) {
      chat_calls <<- chat_calls + 1L
      fake_chat()
    }
  )
  retriever_config <- tempest_config(
    max_search_results = 3L,
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  retriever <- tempest_retriever(config = retriever_config)

  expect_error(
    tempest_run(
      "Retriever config identity",
      config = run_config,
      retriever = retriever,
      experts = list(test_expert(
        expert_id = "expert.retriever-config",
        name = "Retriever Config Expert"
      )),
      program_set = tempest_program_set(),
      steps = "perspectives",
      verbose = FALSE
    ),
    class = "tempest_config_error",
    regexp = "same behavior-relevant configuration"
  )
  expect_equal(chat_calls, 0L)
})

test_that("generated experts normalize to stable S7 profiles", {
  provider_result <- list(
    personas = list(
      list(
        name = "Dr. Flow",
        title = "Workflow analyst",
        affiliation = "Systems Lab",
        background = "Studies orchestration systems.",
        focus_areas = c("Progress", "Recovery"),
        perspective = "Reliable workflow execution",
        initial_questions = c("How is progress recorded?")
      )
    )
  )

  first <- tempest:::tempest_normalize_experts(provider_result)
  second <- tempest:::tempest_normalize_experts(provider_result)

  expect_length(first, 1L)
  expect_s7_class(first[[1]], TempestExpertProfile)
  expect_equal(first[[1]]@version, "1")
  expect_equal(first[[1]]@name, "Dr. Flow")
  expect_equal(first[[1]]@description, "Reliable workflow execution")
  expect_match(first[[1]]@instructions, "Reliable workflow execution")
  expect_equal(first[[1]]@focus_areas, c("Progress", "Recovery"))
  expect_equal(
    first[[1]]@initial_questions,
    "How is progress recorded?"
  )
  expect_identical(first[[1]]@expert_id, second[[1]]@expert_id)
  expect_match(first[[1]]@expert_id, "^expert\\.generated-")
})

test_that("tempest_generate_experts returns validated profiles", {
  skip_if_not_installed("ellmer")
  provider_result <- list(
    personas = list(list(
      name = "Dr. Scope",
      title = "Scope analyst",
      affiliation = "",
      background = "",
      focus_areas = "Boundaries",
      perspective = "Integration boundaries",
      initial_questions = "What belongs in the package?"
    ))
  )
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) {
      fake_chat(structured = list(provider_result))
    }
  )

  experts <- tempest_generate_experts(
    "Reusable workflows",
    n = 1,
    config = cfg
  )

  expect_length(experts, 1L)
  expect_s7_class(experts[[1]], TempestExpertProfile)
  expect_equal(experts[[1]]@name, "Dr. Scope")
})

test_that("tempest_run uses the selected expert team", {
  skip_if_not_installed("ellmer")
  expert <- tempest_expert(
    expert_id = "expert.selected",
    name = "Selected Expert",
    title = "Domain specialist",
    description = "Host-selected expertise",
    instructions = "Use the host-selected perspective."
  )
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  workspace <- tempest_research_workspace()
  retriever <- tempest_retriever(config = cfg, workspace = workspace)
  local_mocked_bindings(
    tempest_wiki_search = function(query, limit = 8L) {
      tibble::tibble(
        title = "Seed",
        url = "https://example.org/seed",
        snippet = "Seed"
      )
    },
    tempest_extract_toc_from_url = function(url) character(),
    tempest_generate_perspectives = function(
      chat,
      topic,
      seed_context,
      n_experts,
      module = NULL
    ) {
      list(
        title = topic,
        perspectives = list(list(
          name = "Selected perspective",
          description = "Uses the selected expert.",
          key_questions = "What matters?"
        ))
      )
    },
    tempest_generate_experts = function(...) {
      stop("selected experts must not trigger generation")
    }
  )

  result <- tempest_run(
    "Selected team",
    config = cfg,
    retriever = retriever,
    experts = list(expert),
    program_set = tempest_program_set(),
    steps = "perspectives",
    verbose = FALSE
  )

  expect_identical(result$experts[[1]], expert)
  expect_identical(result$workspace, workspace)
  expect_equal("store" %in% names(result), FALSE)
  expect_identical(result$manifest@mode, "storm")
  expect_equal("artifacts" %in% names(result$workspace), FALSE)
  expect_null(result$personas)
})

test_that("tempest_run continues from fixed product state", {
  skip_if_not_installed("ellmer")
  fixture <- storm_progress_fixture()

  first <- tempest_run(
    "State continuation",
    config = fixture$config,
    retriever = fixture$retriever,
    n_experts = 1,
    max_questions_per_perspective = 1,
    program_set = tempest_program_set(),
    steps = c("perspectives", "research", "outline"),
    verbose = FALSE
  )
  continued <- tempest_run(
    "State continuation",
    config = fixture$config,
    retriever = fixture$retriever,
    experts = first$experts,
    program_set = tempest_program_set(),
    steps = "write",
    verbose = FALSE,
    .state = first$state
  )

  expect_identical(continued$state$outline, first$state$outline)
  expect_match(continued$state$draft_md, "Section body cites events")
  expect_identical(
    continued$state$completed_stages,
    c("perspectives", "research", "outline", "write")
  )
  expect_identical(continued$workspace, first$workspace)
  expect_equal("artifacts" %in% names(continued$state), FALSE)
})

test_that("tempest_run resume starts fresh when no manifest exists", {
  loader_called <- FALSE
  local_mocked_bindings(
    tempest_load_run_artifacts = function(...) {
      loader_called <<- TRUE
      stop("unexpected loader call")
    },
    tempest_make_chat = function(...) {
      stop("continued with a fresh run")
    }
  )

  expect_error(
    tempest_run(
      "New resumable run",
      output_dir = withr::local_tempdir(),
      resume = TRUE,
      steps = "perspectives",
      verbose = FALSE
    ),
    "continued with a fresh run"
  )
  expect_identical(loader_called, FALSE)
})

test_that("tempest_run rejects a resumed checkpoint for another topic", {
  skip_if_not_installed("ellmer")
  chat_calls <- 0L
  config <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) {
      chat_calls <<- chat_calls + 1L
      fake_chat()
    }
  )
  output_root <- withr::local_tempdir()
  run_id <- "topic-identity"
  run_dir <- tempest:::tempest_prepare_run_dir(
    output_root,
    "Persisted topic",
    run_id = run_id
  )
  program_set <- tempest_program_set()
  tempest:::tempest_save_run_artifacts(
    run_dir,
    tempest_research_workspace(),
    tempest:::tempest_storm_state("Persisted topic"),
    tempest_research_manifest(
      run_id,
      config = config,
      programs = tempest:::tempest_program_set_manifest_programs(program_set)
    ),
    program_set = program_set,
    config = config,
    steps = "write",
    research_strategy = "key_questions"
  )

  expect_error(
    tempest_run(
      "Different requested topic",
      config = config,
      program_set = program_set,
      steps = "write",
      output_dir = output_root,
      resume = TRUE,
      run_id = run_id,
      verbose = FALSE
    ),
    class = "tempest_run_resume_error",
    regexp = "different topic"
  )
  expect_equal(chat_calls, 0L)
})

test_that("tempest_run preserves absorbing terminal manifest identities", {
  skip_if_not_installed("ellmer")
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  output_root <- withr::local_tempdir()

  for (terminal_status in c("failed", "cancelled")) {
    run_id <- paste0("terminal-", terminal_status)
    run_dir <- tempest:::tempest_prepare_run_dir(
      output_root,
      "Terminal resume",
      run_id = run_id
    )
    workspace <- tempest_research_workspace()
    program_set <- tempest_program_set()
    manifest <- tempest_research_manifest(
      run_id,
      config = cfg,
      programs = tempest:::tempest_program_set_manifest_programs(program_set),
      status = terminal_status
    )
    tempest:::tempest_save_run_artifacts(
      run_dir,
      workspace,
      tempest:::tempest_storm_state("Terminal resume"),
      manifest,
      program_set = program_set,
      config = cfg,
      steps = "write",
      research_strategy = "key_questions"
    )

    expect_error(
      tempest_run(
        "Terminal resume",
        config = cfg,
        retriever = tempest_retriever(config = cfg, workspace = workspace),
        steps = "write",
        program_set = program_set,
        output_dir = output_root,
        resume = TRUE,
        run_id = run_id,
        verbose = FALSE
      ),
      class = "tempest_run_resume_error"
    )
    persisted <- tempest:::tempest_read_json_strict(file.path(
      run_dir,
      "run_config.json"
    ))

    expect_identical(persisted$research_manifest$status, terminal_status)
    expect_identical(
      persisted$research_manifest$research_run_id,
      run_id
    )
  }

  run_dir <- tempest:::tempest_prepare_run_dir(
    output_root,
    "Completed resume",
    run_id = "terminal-succeeded"
  )
  state <- tempest:::tempest_storm_state(
    "Completed resume",
    draft_md = "# Completed resume",
    report_md = "# Completed resume",
    completed_stages = "polish"
  )
  program_set <- tempest_program_set()
  manifest <- tempest_research_manifest(
    "terminal-succeeded",
    config = cfg,
    programs = tempest:::tempest_program_set_manifest_programs(program_set),
    status = "succeeded"
  )
  tempest:::tempest_save_run_artifacts(
    run_dir,
    tempest_research_workspace(),
    state,
    manifest,
    program_set = program_set,
    config = cfg,
    steps = "polish",
    research_strategy = "key_questions"
  )

  loaded <- tempest_run(
    "Completed resume",
    config = cfg,
    steps = "polish",
    program_set = program_set,
    output_dir = output_root,
    resume = TRUE,
    run_id = "terminal-succeeded",
    verbose = FALSE
  )
  expect_identical(loaded$manifest@status, "succeeded")
  expect_identical(loaded$manifest@research_run_id, "terminal-succeeded")
  expect_identical(loaded$state, state)

  expect_error(
    tempest_run(
      "Completed resume",
      config = cfg,
      steps = "write",
      program_set = program_set,
      output_dir = output_root,
      resume = TRUE,
      run_id = "terminal-succeeded",
      verbose = FALSE
    ),
    class = "tempest_run_resume_error"
  )
  persisted <- tempest:::tempest_read_json_strict(file.path(
    run_dir,
    "run_config.json"
  ))
  expect_identical(persisted$research_manifest$status, "succeeded")
})

test_that("tempest_run emits ordered STORM progress events", {
  skip_if_not_installed("ellmer")
  local_mocked_bindings(
    tempest_wiki_search = function(query, limit = 8L) {
      tibble::tibble(
        title = character(),
        url = character(),
        snippet = character()
      )
    },
    tempest_extract_toc_from_url = function(url) character(),
    tempest_wiki_page_sections = function(title) character()
  )

  source <- fake_source(
    url = "https://example.org/progress",
    title = "Progress source",
    content_text = "Progress uses staged events and persisted artifacts."
  )
  source_id <- source$id
  store <- tempest_research_workspace()
  store$upsert_retrieved_resource(source)
  collector <- tempest_progress_collector(include_payload = TRUE)
  outline <- list(
    title = "Progress report",
    sections = list(list(
      title = "Workflow evidence",
      summary = "How progress events flow through STORM.",
      subsections = list(list(
        title = "Signals",
        bullets = c("Stage events", "Artifact events"),
        needed = c("Cited facts")
      ))
    ))
  )
  claim_result <- function(claim) {
    list(
      facts = list(list(
        claim = claim,
        sources = list(list(source_id = source_id)),
        confidence = "high"
      ))
    )
  }
  cfg <- tempest_config(
    citation_policy = "claim_verified",
    chat_fn = function(role, model, system_prompt, echo) {
      if (
        identical(role, "writer") &&
          identical(system_prompt, tempest_prompt("polisher_system"))
      ) {
        return(fake_chat(
          text = list(paste0(
            "Polished report cites progress evidence [",
            source_id,
            "]."
          ))
        ))
      }
      if (identical(role, "writer")) {
        return(fake_chat(
          structured = list(
            list(queries = c("progress events")),
            outline,
            outline,
            list(
              section_text = paste0(
                "Section body cites events [",
                source_id,
                "]."
              )
            ),
            list(
              lead_section = paste0(
                "Lead body cites events [",
                source_id,
                "]."
              )
            )
          ),
          text = list(
            paste0("Section body cites events [", source_id, "]."),
            paste0("Lead body cites events [", source_id, "].")
          )
        ))
      }
      if (
        identical(role, "judge") &&
          identical(system_prompt, tempest_prompt("fact_extractor_system"))
      ) {
        return(fake_chat(
          structured = list(
            claim_result("STORM progress emits stage events."),
            claim_result("STORM progress persists artifacts.")
          )
        ))
      }
      if (identical(role, "judge")) {
        return(fake_chat(
          structured = list(
            list(status = "supported", score = 0.9, rationale = "ok"),
            list(status = "supported", score = 0.9, rationale = "ok")
          )
        ))
      }
      if (identical(role, "expert")) {
        return(fake_chat(
          text = list(paste0(
            "Expert answer cites progress evidence [",
            source_id,
            "]."
          ))
        ))
      }
      if (
        identical(role, "coordinator") &&
          identical(system_prompt, tempest_prompt("persona_generator_system"))
      ) {
        return(fake_chat(
          structured = list(list(
            personas = list(list(
              name = "Dr. Flow",
              title = "Workflow analyst",
              affiliation = "",
              background = "",
              focus_areas = c("Progress"),
              perspective = "Workflow progress",
              initial_questions = c("How should progress be reported?")
            ))
          ))
        ))
      }
      fake_chat(
        structured = list(
          list(
            title = "Progress report",
            perspectives = list(list(
              name = "Workflow",
              description = "Workflow progress perspective.",
              key_questions = c("How should progress be reported?")
            ))
          )
        )
      )
    }
  )
  retriever <- tempest_retriever(config = cfg, workspace = store)

  result <- tempest_run(
    "Progress events",
    config = cfg,
    retriever = retriever,
    n_experts = 1,
    max_questions_per_perspective = 1,
    program_set = tempest_program_set(),
    output_dir = withr::local_tempdir(),
    run_id = "progress-run",
    progress = collector$record,
    verbose = FALSE
  )

  event_data <- collector$data()
  labels <- vapply(
    event_data,
    function(event) {
      paste(
        event$event_type,
        event$stage,
        event$step,
        event$status,
        sep = ":"
      )
    },
    character(1)
  )

  expect_contains(
    labels,
    c(
      "workflow:NA:NA:started",
      "stage:perspectives:NA:started",
      "stage:perspectives:NA:succeeded",
      "stage:research:NA:started",
      "stage:research:NA:succeeded",
      "stage:outline:NA:started",
      "stage:outline:NA:succeeded",
      "stage:write:NA:started",
      "stage:write:NA:succeeded",
      "stage:polish:NA:started",
      "stage:verification:NA:started",
      "stage:verification:NA:succeeded",
      "stage:polish:NA:succeeded",
      "step:persistence:perspectives_artifacts:succeeded",
      "step:persistence:research_artifacts:succeeded",
      "step:persistence:outline_artifacts:succeeded",
      "step:persistence:write_artifacts:succeeded",
      "step:persistence:polish_artifacts:succeeded",
      "artifact:polish:report_md:available",
      "workflow:NA:NA:succeeded"
    )
  )
  expect_lt(
    match("stage:perspectives:NA:started", labels),
    match("stage:research:NA:started", labels)
  )
  expect_lt(
    match("stage:write:NA:succeeded", labels),
    match("stage:polish:NA:started", labels)
  )
  expect_lt(
    match("stage:verification:NA:succeeded", labels),
    match("stage:polish:NA:succeeded", labels)
  )
  expect_match(result$report_md, "Polished report")
  expect_equal(
    intersect(names(result), c("store", "artifact_catalog", "workflow_run")),
    character()
  )
  expect_equal(
    collector$data(event_type = "artifact")[[1]]$payload$artifact,
    "report_md"
  )
  expect_equal(
    unique(vapply(event_data, `[[`, character(1), "run_id")),
    "progress-run"
  )
  expect_equal(
    unique(vapply(event_data, `[[`, character(1), "workflow")),
    "storm"
  )
})

test_that("STORM research harvests OpenAI native annotations", {
  skip_if_not_installed("ellmer")

  url <- "https://example.org/storm-native-source"
  source_id <- tempest:::tempest_source_id(url)
  turn <- native_openai_json_turn(
    claim_text = "STORM native-backed claim.",
    url = url,
    title = "STORM Native Source"
  )
  expert_chat <- list(
    chat = function(prompt, ...) "STORM native-backed claim.",
    last_turn = function() turn,
    register_tools = function(...) invisible(NULL)
  )
  writer_chat <- fake_chat(
    structured = list(list(queries = "storm native evidence"))
  )
  extractor <- fake_chat(
    structured = list(list(
      facts = list(list(
        claim = "STORM native-backed claim",
        sources = list(list(source_id = source_id)),
        confidence = "high",
        support_score = 0.87
      ))
    ))
  )
  cfg <- tempest_config(chat_fn = function(role, model, system_prompt, echo) {
    if (identical(role, "expert")) {
      return(expert_chat)
    }
    if (
      identical(role, "judge") &&
        identical(system_prompt, tempest_prompt("fact_extractor_system"))
    ) {
      return(extractor)
    }
    if (identical(role, "writer")) {
      return(writer_chat)
    }
    fake_chat()
  })
  perspectives <- list(list(
    name = "Native evidence",
    description = "Native evidence perspective.",
    key_questions = c("What does native evidence say?")
  ))
  experts <- list(tempest_expert(
    expert_id = "expert.native",
    name = "Dr. Native",
    title = "Researcher",
    description = "Native evidence",
    instructions = "Research native evidence."
  ))
  result <- tempest:::tempest_research_one_perspective(
    1,
    perspectives = perspectives,
    experts = experts,
    config = cfg,
    topic = "Native evidence",
    research_strategy = "key_questions",
    max_questions_per_perspective = 1,
    programs = test_program_executions(
      cfg,
      run_id = "research-one-perspective"
    )
  )

  expect_equal(result$retrieved_resources[[1]]$id, source_id)
  expect_equal(
    result$retrieved_resources[[1]]$title,
    "STORM Native Source"
  )
  result_workspace <- tempest_research_workspace()
  result_workspace$upsert_retrieved_resource(
    result$retrieved_resources[[1]]
  )
  sources <- tempest_sources(result_workspace)
  expect_match(sources$snippet[[1]], "STORM native-backed claim")
  expect_match(sources$context_text[[1]], "STORM native-backed claim")
  expect_length(result$proposed_claims, 1L)
  expect_equal(result$proposed_claims[[1]]@source_ids, source_id)
  expect_equal(result$proposed_claims[[1]]@support_score, 0.87)
})

test_that("STORM research does not downgrade dsprrr contract failures", {
  skip_if_not_installed("ellmer")
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  expert <- tempest_expert(
    expert_id = "expert.contract",
    name = "Contract Expert",
    title = "Researcher",
    description = "Contract testing",
    instructions = "Research the supplied question."
  )
  local_mocked_bindings(
    tempest_decompose_query = function(...) {
      rlang::abort(
        "trace metadata mismatch",
        class = "dsprrr_trace_contract_error"
      )
    }
  )

  expect_error(
    tempest:::tempest_research_one_perspective(
      1,
      perspectives = list(list(
        name = "Contract",
        description = "Contract boundary",
        key_questions = "What changed?"
      )),
      experts = list(expert),
      config = cfg,
      topic = "Contract boundary",
      research_strategy = "key_questions",
      max_questions_per_perspective = 1,
      programs = test_program_executions(cfg, "storm-contract")
    ),
    class = "dsprrr_trace_contract_error"
  )
})

test_that("tempest_run emits a failed verification stage event", {
  skip_if_not_installed("ellmer")
  local_mocked_bindings(
    tempest_wiki_search = function(query, limit = 8L) {
      tibble::tibble(
        title = character(),
        url = character(),
        snippet = character()
      )
    },
    tempest_extract_toc_from_url = function(url) character(),
    tempest_wiki_page_sections = function(title) character(),
    tempest_run_verification = function(...) {
      stop("verification module unavailable")
    }
  )
  fixture <- storm_progress_fixture()
  collector <- tempest_progress_collector()

  expect_error(
    tempest_run(
      "Progress events",
      config = fixture$config,
      retriever = fixture$retriever,
      n_experts = 1,
      max_questions_per_perspective = 1,
      program_set = tempest_program_set(),
      output_dir = withr::local_tempdir(),
      run_id = "progress-verify-fail",
      progress = collector$record,
      verbose = FALSE
    ),
    "verification module unavailable"
  )

  labels <- vapply(
    collector$data(),
    function(event) {
      paste(event$event_type, event$stage, event$status, sep = ":")
    },
    character(1)
  )
  expect_contains(
    labels,
    c(
      "stage:verification:started",
      "stage:verification:failed",
      "workflow:NA:failed"
    )
  )
})

test_that("tempest_run emits terminal progress events on failure", {
  skip_if_not_installed("ellmer")
  collector <- tempest_progress_collector()
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  output_root <- withr::local_tempdir()

  expect_error(
    tempest_run(
      "Progress failure",
      config = cfg,
      retriever = tempest_retriever(
        config = cfg,
        workspace = tempest_research_workspace()
      ),
      steps = "write",
      program_set = tempest_program_set(),
      output_dir = output_root,
      run_id = "failed-storm-run",
      progress = collector$record,
      verbose = FALSE
    ),
    "No outline available"
  )

  labels <- vapply(
    collector$data(),
    function(event) {
      paste(event$event_type, event$stage, event$status, sep = ":")
    },
    character(1)
  )
  expect_contains(
    labels,
    c(
      "workflow:NA:started",
      "stage:write:started",
      "stage:write:failed",
      "workflow:NA:failed"
    )
  )
  persisted <- tempest:::tempest_read_json_strict(file.path(
    output_root,
    "failed-storm-run",
    "run_config.json"
  ))
  expect_identical(persisted$research_manifest$status, "failed")
  expect_equal("write" %in% unlist(persisted$completed_stages), FALSE)
})

test_that("final STORM persistence failure is recorded without masking it", {
  skip_if_not_installed("ellmer")
  fixture <- storm_progress_fixture()
  output_root <- withr::local_tempdir()
  original_save <- tempest:::tempest_save_run_artifacts
  local_mocked_bindings(
    tempest_save_run_artifacts = function(
      run_dir,
      workspace,
      state,
      research_manifest,
      ...
    ) {
      if (identical(research_manifest@status, "succeeded")) {
        rlang::abort(
          "Final persistence failed.",
          class = "test_terminal_persistence_error"
        )
      }
      original_save(
        run_dir,
        workspace,
        state,
        research_manifest,
        ...
      )
    }
  )

  condition <- tryCatch(
    tempest_run(
      "Terminal persistence",
      config = fixture$config,
      retriever = fixture$retriever,
      n_experts = 1,
      max_questions_per_perspective = 1,
      program_set = tempest_program_set(),
      output_dir = output_root,
      run_id = "terminal-persistence",
      verbose = FALSE
    ),
    error = \(error) error
  )
  persisted <- tempest:::tempest_read_json_strict(file.path(
    output_root,
    "terminal-persistence",
    "run_config.json"
  ))

  expect_s3_class(condition, "test_terminal_persistence_error")
  expect_identical(conditionMessage(condition), "Final persistence failed.")
  expect_identical(persisted$research_manifest$status, "failed")
  expect_contains(unlist(persisted$completed_stages), "polish")
})
