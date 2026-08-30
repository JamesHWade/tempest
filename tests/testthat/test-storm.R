test_that("tempest_run rejects invalid runtime budgets before provider work", {
  skip_if_not_installed("ellmer")
  cfg <- tempest_config(max_active_experts = 2L)
  invalid <- list(
    list(topic = character()),
    list(topic = "Topic", n_experts = 0),
    list(topic = "Topic", n_experts = 3),
    list(topic = "Topic", max_rounds = NA_real_),
    list(topic = "Topic", max_questions_per_perspective = Inf),
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
        name = "Retriever Config Expert"
      )),
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
  expect_match(first[[1]]@expert_id, "^expert::[a-f0-9]{64}$")
  expect_match(first[[1]]@version, "^sha256-[a-f0-9]{64}$")
  expect_equal(first[[1]]@name, "Dr. Flow")
  expect_match(
    first[[1]]@description,
    "Affiliation: Systems Lab",
    fixed = TRUE
  )
  expect_match(
    first[[1]]@description,
    "Background: Studies orchestration systems.",
    fixed = TRUE
  )
  expect_match(
    first[[1]]@description,
    "Perspective: Reliable workflow execution",
    fixed = TRUE
  )
  expect_match(first[[1]]@instructions, "Reliable workflow execution")
  expect_equal(first[[1]]@focus_areas, c("Progress", "Recovery"))
  expect_equal(
    first[[1]]@initial_questions,
    "How is progress recorded?"
  )
  expect_identical(first[[1]]@expert_id, second[[1]]@expert_id)
  expect_identical(first[[1]]@version, second[[1]]@version)
})

test_that("tempest_generate_experts returns validated profiles", {
  skip_if_not_installed("ellmer")
  provider_result <- list(
    personas = list(list(
      name = "Dr. Scope",
      title = "Scope analyst",
      affiliation = "Independent",
      background = "Studies reusable workflow boundaries.",
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
      module = NULL,
      record_stage = function(record, output = NULL) invisible(record)
    ) {
      output <- list(
        title = topic,
        perspectives = list(list(
          name = "Selected perspective",
          description = "Uses the selected expert.",
          key_questions = "What matters?"
        ))
      )
      record <- tempest:::tempest_stage_record_succeed(
        tempest:::tempest_stage_record_start(
          "perspectives",
          module$program_artifact_id,
          trace_references = tempest:::tempest_stage_execution_trace_references(
            module,
            list()
          )
        ),
        tempest:::tempest_stage_output_reference(
          "state_field",
          c("title", "perspectives"),
          content_digest = tempest:::tempest_stage_state_output_digest(
            "perspectives",
            output
          )
        ),
        support_status = "unknown"
      )
      record_stage(record, output)
      output
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
    steps = "perspectives",
    verbose = FALSE
  )

  expect_identical(result@experts[[1]], expert)
  expect_identical(result@workspace, workspace)
  expect_equal(
    "store" %in% names(tempest:::TempestResult@properties),
    FALSE
  )
  expect_identical(result@manifest@mode, "storm")
  expect_equal("artifacts" %in% names(result@workspace), FALSE)
  expect_equal(
    "personas" %in% names(tempest:::TempestResult@properties),
    FALSE
  )
  expect_length(
    Filter(
      \(record) identical(record@stage, "personas"),
      result@state$stage_records
    ),
    0L
  )
})

test_that("tempest_run rejects state-only continuation", {
  expect_identical(".state" %in% names(formals(tempest_run)), FALSE)
  expect_identical(
    ".state" %in% names(formals(tempest:::tempest_run_internal)),
    FALSE
  )
  condition <- tryCatch(
    tempest_run(
      "State continuation",
      .state = tempest:::tempest_storm_state("State continuation")
    ),
    error = identity
  )

  expect_s3_class(condition, "simpleError")
  expect_match(conditionMessage(condition), "unused argument", fixed = TRUE)
})

test_that("public partial STORM resumes cannot expand their request", {
  skip_if_not_installed("ellmer")
  fixture <- storm_progress_fixture()
  output_root <- withr::local_tempdir()
  run_id <- "public-partial-request"
  program_set <- tempest_program_set()

  first <- tempest_run(
    "Public partial request",
    config = fixture$config,
    retriever = fixture$retriever,
    n_experts = 1,
    max_questions_per_perspective = 1,
    steps = "perspectives",
    output_dir = output_root,
    run_id = run_id,
    verbose = FALSE
  )
  expect_identical(first@state$requested_steps, "perspectives")
  first_personas <- Filter(
    \(record) {
      identical(record@stage, "personas") &&
        identical(record@status, "succeeded")
    },
    first@state$stage_records
  )
  expect_length(first_personas, 1L)
  expect_identical(
    first_personas[[1]]@output_reference$content_digest,
    tempest:::tempest_stage_state_output_digest("personas", first@experts)
  )

  resumed <- tempest_run(
    "Public partial request",
    config = fixture$config,
    steps = "perspectives",
    output_dir = output_root,
    resume = TRUE,
    run_id = run_id,
    verbose = FALSE
  )
  expect_identical(resumed@state$requested_steps, "perspectives")
  expect_identical(resumed@state$completed_stages, "perspectives")
  expect_identical(
    tempest:::tempest_stage_records_data(resumed@state$stage_records),
    tempest:::tempest_stage_records_data(first@state$stage_records)
  )

  expect_error(
    tempest_run(
      "Public partial request",
      config = fixture$config,
      steps = c("perspectives", "research"),
      output_dir = output_root,
      resume = TRUE,
      run_id = run_id,
      verbose = FALSE
    ),
    class = "tempest_run_resume_error"
  )
})

test_that("tempest_run resume starts fresh when no manifest exists", {
  provider_started <- FALSE
  loaded_before_provider <- FALSE
  original_loader <- tempest:::tempest_storm_load_artifacts
  local_mocked_bindings(
    tempest_storm_load_artifacts = function(...) {
      if (!provider_started) {
        loaded_before_provider <<- TRUE
      }
      original_loader(...)
    },
    tempest_make_chat = function(...) {
      provider_started <<- TRUE
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
    class = "tempest_run_error"
  )
  expect_identical(provider_started, TRUE)
  expect_identical(loaded_before_provider, FALSE)
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
  run_dir <- tempest:::tempest_storm_prepare_run_dir(
    output_root,
    "Persisted topic",
    run_id = run_id
  )
  program_set <- tempest_program_set()
  tempest:::tempest_storm_save_artifacts(
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
    steps = "write"
  )

  expect_error(
    tempest_run(
      "Different requested topic",
      config = config,
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
  chat_calls <- 0L
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) {
      chat_calls <<- chat_calls + 1L
      fake_chat()
    }
  )
  output_root <- withr::local_tempdir()

  for (terminal_status in c("failed", "cancelled")) {
    run_id <- paste0("terminal-", terminal_status)
    run_dir <- tempest:::tempest_storm_prepare_run_dir(
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
    tempest:::tempest_storm_save_artifacts(
      run_dir,
      workspace,
      tempest:::tempest_storm_state("Terminal resume"),
      manifest,
      program_set = program_set,
      config = cfg,
      steps = "write"
    )

    expect_error(
      tempest_run(
        "Terminal resume",
        config = cfg,
        retriever = tempest_retriever(config = cfg, workspace = workspace),
        steps = "write",
        output_dir = output_root,
        resume = TRUE,
        run_id = run_id,
        verbose = FALSE
      ),
      class = "tempest_run_resume_error"
    )
    persisted <- tempest:::tempest_product_read_json(file.path(
      run_dir,
      "run_config.json"
    ))

    expect_identical(persisted$research_manifest$status, terminal_status)
    expect_identical(
      persisted$research_manifest$research_run_id,
      run_id
    )
  }

  run_dir <- tempest:::tempest_storm_prepare_run_dir(
    output_root,
    "Completed resume",
    run_id = "terminal-succeeded"
  )
  program_set <- tempest_program_set()
  completed <- test_persistence_complete_storm_product(
    "Completed resume",
    "terminal-succeeded",
    cfg,
    program_set
  )
  tempest:::tempest_storm_save_artifacts(
    run_dir,
    completed$workspace,
    completed$state,
    completed$manifest,
    program_set = program_set,
    config = cfg,
    steps = c("perspectives", "research", "outline", "write", "polish")
  )
  expected_state <- tempest:::tempest_storm_load_artifacts(
    run_dir,
    config = cfg,
    program_set = program_set,
    run_id = "terminal-succeeded"
  )$state

  original_retriever <- tempest_retriever
  sealed_retriever_calls <- 0L
  local_mocked_bindings(
    tempest_retriever = function(
      config = tempest_config(),
      workspace = tempest_research_workspace()
    ) {
      if (
        inherits(workspace, "ResearchWorkspace") &&
          identical(
            tempest:::tempest_research_workspace_mutation_state(workspace),
            "sealed"
          )
      ) {
        sealed_retriever_calls <<- sealed_retriever_calls + 1L
      }
      original_retriever(config = config, workspace = workspace)
    }
  )
  chat_calls <- 0L

  loaded <- tempest_run(
    "Completed resume",
    config = cfg,
    steps = c("perspectives", "research", "outline", "write", "polish"),
    output_dir = output_root,
    resume = TRUE,
    run_id = "terminal-succeeded",
    verbose = FALSE
  )
  expect_identical(loaded@manifest@status, "succeeded")
  expect_identical(loaded@manifest@research_run_id, "terminal-succeeded")
  expect_identical(loaded@state, expected_state)
  expect_identical(chat_calls, 0L)
  expect_identical(sealed_retriever_calls, 0L)
  expect_identical(loaded@retriever$workspace, loaded@workspace)
  expect_identical(
    tempest:::tempest_research_workspace_mutation_state(loaded@workspace),
    "sealed"
  )
  expect_error(
    loaded@workspace$set_max_sources(cfg@max_sources),
    class = "tempest_research_workspace_error"
  )

  expect_error(
    tempest_run(
      "Completed resume",
      config = cfg,
      steps = "write",
      output_dir = output_root,
      resume = TRUE,
      run_id = "terminal-succeeded",
      verbose = FALSE
    ),
    class = "tempest_run_resume_error"
  )
  persisted <- tempest:::tempest_product_read_json(file.path(
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
    tempest_wiki_page_sections = function(title) character(),
    tempest_storm_semantic_filter_facts = function(
      retriever,
      query,
      store,
      max_items = 30,
      min_support_score = 0.7
    ) {
      facts <- tempest:::tempest_supported_claims(
        store,
        min_support_score = min_support_score
      )
      utils::head(facts, max_items)
    }
  )

  source <- fake_source(
    url = "https://example.org/progress",
    title = "Progress source",
    content_text = "Progress uses staged events and persisted artifacts."
  )
  source_id <- source@resource_id
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
        sources = list(list(
          source_id = source_id,
          quote = "Progress uses staged events and persisted artifacts."
        )),
        confidence = "high"
      ))
    )
  }
  cfg <- tempest_config(
    citation_policy = "claim_verified",
    chat_fn = function(role, model, system_prompt, echo) {
      if (identical(role, "writer")) {
        return(fake_chat(
          structured = list(
            list(queries = c("progress events")),
            outline,
            outline,
            list(
              section_text = paste0(
                "STORM progress emits stage events [",
                source_id,
                "]."
              )
            ),
            list(
              lead_section = paste0(
                "STORM progress emits stage events [",
                source_id,
                "]."
              )
            )
          ),
          text = list(
            paste0(
              "STORM progress emits stage events [",
              source_id,
              "]."
            ),
            paste0(
              "STORM progress emits stage events [",
              source_id,
              "]."
            )
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
              affiliation = "Independent",
              background = "Studies workflow progress reporting.",
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
  expect_match(result@report_md, "STORM progress emits stage events")
  expect_equal(
    intersect(
      names(tempest:::TempestResult@properties),
      c("store", "artifact_catalog", "workflow_run")
    ),
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
  skip_if_not_installed("deputy")
  skip_if_not_installed("ellmer")

  url <- "https://example.org/storm-native-source"
  source_id <- tempest:::tempest_source_id(url)
  turn <- native_openai_json_turn(
    claim_text = "STORM native-backed claim.",
    url = url,
    title = "STORM Native Source"
  )
  expert_chat <- fake_chat(
    text = list("STORM native-backed claim."),
    provider_turns = list(turn)
  )
  writer_chat <- fake_chat(
    structured = list(list(queries = "storm native evidence"))
  )
  extractor <- fake_chat(
    structured = list(list(
      facts = list(list(
        claim = "STORM native-backed claim",
        sources = list(list(
          source_id = source_id,
          quote = "STORM native-backed claim."
        )),
        confidence = "high"
      ))
    ))
  )
  config <- tempest_config(
    cache_enabled = FALSE,
    chat_fn = function(role, model, system_prompt, echo) {
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
    }
  )
  workspace <- tempest_research_workspace()
  retriever <- tempest_retriever(config = config, workspace = workspace)
  expert <- tempest_expert(
    name = "Dr. Native",
    title = "Researcher",
    description = "Native evidence",
    instructions = "Research native evidence."
  )
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
    tempest_generate_perspectives = function(
      chat,
      topic,
      seed_context,
      n_experts,
      module = NULL,
      record_stage = function(record, output = NULL) invisible(record)
    ) {
      output <- list(
        title = "Native evidence",
        perspectives = list(list(
          id = "perspective.native",
          name = "Native evidence",
          description = "Native evidence perspective.",
          key_questions = "What does native evidence say?"
        ))
      )
      record <- tempest:::tempest_stage_record_succeed(
        tempest:::tempest_stage_record_start(
          "perspectives",
          module$program_artifact_id,
          trace_references = tempest:::tempest_stage_execution_trace_references(
            module,
            list()
          )
        ),
        tempest:::tempest_stage_output_reference(
          "state_field",
          c("title", "perspectives"),
          content_digest = tempest:::tempest_stage_state_output_digest(
            "perspectives",
            output
          )
        ),
        support_status = "unknown"
      )
      record_stage(record, output)
      output
    }
  )

  result <- tempest_run(
    "Native evidence",
    config = config,
    retriever = retriever,
    experts = list(expert),
    max_questions_per_perspective = 1,
    steps = c("perspectives", "research"),
    verbose = FALSE
  )

  source <- result@workspace$get_retrieved_source(source_id)
  expect_identical(source$title, "STORM Native Source")
  sources <- tempest:::tempest_workspace_sources(result@workspace)
  expect_match(sources$snippet[[1L]], "STORM native-backed claim")
  expect_match(sources$context_text[[1L]], "STORM native-backed claim")
  claims <- result@workspace$list_proposed_claims()
  expect_length(claims, 1L)
  expect_identical(claims[[1L]]@source_ids, source_id)
  expect_identical(claims[[1L]]@support_score, NA_real_)
  expect_identical(claims[[1L]]@verification_status, "unverified")
})

test_that("STORM research does not downgrade dsprrr contract failures", {
  skip_if_not_installed("ellmer")
  expert_chat <- fake_chat(text = list("Provider work must not begin."))
  config <- tempest_config(
    cache_enabled = FALSE,
    chat_fn = function(role, model, system_prompt, echo) {
      if (identical(role, "expert")) {
        return(expert_chat)
      }
      fake_chat()
    }
  )
  expert <- tempest_expert(
    name = "Contract Expert",
    title = "Researcher",
    description = "Contract testing",
    instructions = "Research the supplied question."
  )
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
    tempest_generate_perspectives = function(...) {
      list(
        title = "Contract boundary",
        perspectives = list(list(
          id = "perspective.contract",
          name = "Contract",
          description = "Contract boundary",
          key_questions = "What changed?"
        ))
      )
    },
    tempest_decompose_query = function(...) {
      rlang::abort(
        "trace metadata mismatch",
        class = "dsprrr_trace_contract_error"
      )
    }
  )

  expect_error(
    tempest_run(
      "Contract boundary",
      config = config,
      retriever = tempest_retriever(
        config = config,
        workspace = tempest_research_workspace()
      ),
      experts = list(expert),
      max_questions_per_perspective = 1,
      steps = c("perspectives", "research"),
      verbose = FALSE
    ),
    class = "tempest_run_error"
  )
  expect_length(expert_chat$.calls(), 0L)
})
test_that("tempest_run emits a failed verification stage event", {
  skip_if_not_installed("ellmer")
  verification_called <- FALSE
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
      verification_called <<- TRUE
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
      output_dir = withr::local_tempdir(),
      run_id = "progress-verify-fail",
      progress = collector$record,
      verbose = FALSE
    ),
    class = "tempest_run_error"
  )
  expect_identical(verification_called, TRUE)

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
  persisted <- tempest:::tempest_product_read_json(file.path(
    output_root,
    "failed-storm-run",
    "run_config.json"
  ))
  expect_identical(persisted$research_manifest$status, "running")
  expect_null(persisted$research_manifest$deliverables$report_md)
  expect_equal("write" %in% unlist(persisted$completed_stages), FALSE)
})

test_that("tempest_run propagates progress callback failures", {
  skip_if_not_installed("ellmer")
  fixture <- storm_progress_fixture()
  calls <- 0L
  callback <- function(event) {
    calls <<- calls + 1L
    if (calls >= 2L) {
      stop("progress consumer failed")
    }
    invisible(event)
  }

  expect_error(
    tempest_run(
      "Progress callback failure",
      config = fixture$config,
      retriever = fixture$retriever,
      n_experts = 1,
      steps = "perspectives",
      progress = callback,
      verbose = FALSE
    ),
    class = "tempest_progress_callback_error"
  )
  expect_gte(calls, 2L)
})

test_that("final STORM persistence failure is recorded without masking it", {
  skip_if_not_installed("ellmer")
  fixture <- storm_progress_fixture()
  output_root <- withr::local_tempdir()
  original_save <- tempest:::tempest_storm_save_artifacts
  local_mocked_bindings(
    tempest_storm_save_artifacts = function(
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
      output_dir = output_root,
      run_id = "terminal-persistence",
      verbose = FALSE
    ),
    error = \(error) error
  )
  persisted <- tempest:::tempest_product_read_json(file.path(
    output_root,
    "terminal-persistence",
    "run_config.json"
  ))

  expect_s3_class(condition, "tempest_run_error")
  expect_identical(conditionMessage(condition), "The operation failed.")
  expect_identical(persisted$research_manifest$status, "running")
  expect_equal("polish" %in% unlist(persisted$completed_stages), FALSE)
  expect_null(persisted$research_manifest$deliverables$report_md)
  expect_identical(
    tempest:::tempest_research_workspace_mutation_state(fixture$store),
    "open"
  )
})

test_that("STORM publishes report authority atomically and restores it", {
  skip_if_not_installed("deputy")
  skip_if_not_installed("ellmer")
  skip_if_not_installed("graft")
  fixture <- storm_progress_fixture()
  output_root <- withr::local_tempdir()
  program_set <- tempest_program_set()
  observations <- list()
  mutation_conditions <- list()
  original_save <- tempest:::tempest_storm_save_artifacts
  local_mocked_bindings(
    tempest_storm_save_artifacts = function(
      run_dir,
      workspace,
      state,
      research_manifest,
      ...
    ) {
      observations[[length(observations) + 1L]] <<- list(
        has_report = !is.null(state$report_md),
        status = research_manifest@status
      )
      original_save(
        run_dir,
        workspace,
        state,
        research_manifest,
        ...
      )
    }
  )
  progress <- function(event) {
    is_post_authority <-
      identical(event$status, "succeeded") &&
      ((identical(event$event_type, "step") &&
        identical(event$stage, "persistence") &&
        identical(event$step, "polish_artifacts")) ||
        (identical(event$event_type, "stage") &&
          identical(event$stage, "polish")))
    if (is_post_authority) {
      mutation_attempt <- length(mutation_conditions) + 1L
      condition <- tryCatch(
        {
          fixture$store$record_accepted_graft_reference(list(
            record_id = paste0("callback-mutation-", mutation_attempt)
          ))
          NULL
        },
        error = \(error) error
      )
      mutation_conditions <<- c(mutation_conditions, list(condition))
    }
    invisible(event)
  }

  result <- tempest_run(
    "Atomic STORM publication",
    config = fixture$config,
    retriever = fixture$retriever,
    n_experts = 1,
    max_questions_per_perspective = 1,
    output_dir = output_root,
    run_id = "atomic-storm-publication",
    progress = progress,
    verbose = FALSE
  )
  persisted <- tempest:::tempest_product_read_json(file.path(
    result@output_dir,
    "run_config.json"
  ))$research_manifest
  restored <- tempest:::tempest_storm_load_artifacts(
    result@output_dir,
    config = fixture$config,
    program_set = program_set,
    run_id = "atomic-storm-publication"
  )
  persisted_workspace <- tempest:::tempest_product_read_json(file.path(
    result@output_dir,
    "workspace.json"
  ))
  live_workspace <- tempest:::tempest_research_workspace_snapshot(
    result@workspace
  )
  restored_workspace <- tempest:::tempest_research_workspace_snapshot(
    restored$workspace
  )
  report_observations <- Filter(
    \(observation) isTRUE(observation$has_report),
    observations
  )
  running_observations <- Filter(
    \(observation) identical(observation$status, "running"),
    observations
  )

  expect_length(report_observations, 1L)
  expect_identical(report_observations[[1L]]$status, "succeeded")
  expect_all_true(vapply(
    running_observations,
    \(observation) !isTRUE(observation$has_report),
    logical(1)
  ))
  expect_identical(
    tempest_research_manifest_record(result@manifest),
    persisted
  )
  expect_identical(
    tempest_research_manifest_record(restored$research_manifest),
    persisted
  )
  expect_identical(restored$state$report_md, result@report_md)
  expect_length(mutation_conditions, 2L)
  expect_all_true(vapply(
    mutation_conditions,
    inherits,
    logical(1),
    what = "tempest_research_workspace_error"
  ))
  expect_identical(live_workspace, persisted_workspace)
  expect_identical(restored_workspace, persisted_workspace)
  expect_identical(
    tempest:::tempest_research_workspace_mutation_state(result@workspace),
    "sealed"
  )
  expect_identical(
    tempest:::tempest_research_workspace_mutation_state(restored$workspace),
    "sealed"
  )
})

test_that("STORM finalization failure preserves a running report-free bundle", {
  skip_if_not_installed("deputy")
  skip_if_not_installed("ellmer")
  skip_if_not_installed("graft")
  mode <- "error"
  original_finalize <- tempest:::tempest_product_authority_finalize_manifest
  local_mocked_bindings(
    tempest_product_authority_finalize_manifest = function(
      manifest,
      stage_records,
      workspace,
      report_md = NULL,
      config = NULL,
      experts = list(),
      expert_sessions = list(),
      product_state = NULL,
      status = manifest@status,
      require_publishable = FALSE
    ) {
      if (identical(status, "succeeded")) {
        if (identical(mode, "interrupt")) {
          rlang::interrupt()
        }
        rlang::abort(
          "Publication finalization failed.",
          class = "test_publication_finalization_error"
        )
      }
      original_finalize(
        manifest = manifest,
        stage_records = stage_records,
        workspace = workspace,
        report_md = report_md,
        config = config,
        experts = experts,
        expert_sessions = expert_sessions,
        product_state = product_state,
        status = status,
        require_publishable = require_publishable
      )
    }
  )

  for (failure_mode in c("error", "interrupt")) {
    mode <- failure_mode
    fixture <- storm_progress_fixture()
    output_root <- withr::local_tempdir()
    run_id <- paste0("storm-finalize-", failure_mode)
    condition <- tryCatch(
      tempest_run(
        "Recoverable STORM publication",
        config = fixture$config,
        retriever = fixture$retriever,
        n_experts = 1,
        max_questions_per_perspective = 1,
        output_dir = output_root,
        run_id = run_id,
        verbose = FALSE
      ),
      error = \(error) error
    )
    persisted <- tempest:::tempest_product_read_json(file.path(
      output_root,
      run_id,
      "run_config.json"
    ))

    expect_s3_class(condition, "tempest_run_error")
    expect_identical(persisted$research_manifest$status, "running")
    expect_null(persisted$research_manifest$deliverables$report_md)
    expect_equal("report.md" %in% unlist(persisted$files), FALSE)
    expect_equal("polish" %in% unlist(persisted$completed_stages), FALSE)
    expect_identical(
      tempest:::tempest_research_workspace_mutation_state(fixture$store),
      "open"
    )
  }
})

test_that("STORM preserves a terminal Deputy attempt when extraction fails", {
  skip_if_not_installed("deputy")
  skip_if_not_installed("ellmer")
  skip_if_not_installed("graft")
  fixture <- storm_progress_fixture()
  output_root <- withr::local_tempdir()
  program_set <- tempest_program_set()
  local_mocked_bindings(
    tempest_extract_facts_from_answer = function(...) {
      tempest:::tempest_stage_governance_abort(
        "Injected extraction failure after the terminal Deputy completion."
      )
    }
  )

  condition <- tryCatch(
    tempest_run(
      "Recoverable post-completion extraction failure",
      config = fixture$config,
      retriever = fixture$retriever,
      experts = list(tempest_expert(
        name = "Dr. Recovery",
        title = "Recovery researcher",
        description = "Exercises post-completion recovery.",
        instructions = "Use the supplied evidence."
      )),
      max_questions_per_perspective = 1,
      output_dir = output_root,
      run_id = "storm-post-completion-extraction-failure",
      verbose = FALSE
    ),
    error = \(error) error
  )
  restored <- tempest:::tempest_storm_load_artifacts(
    file.path(output_root, "storm-post-completion-extraction-failure"),
    config = fixture$config,
    program_set = program_set,
    run_id = "storm-post-completion-extraction-failure"
  )
  deputy_traces <- Filter(
    \(trace) identical(trace$trace_type, "deputy_run"),
    restored$research_manifest@traces
  )
  extraction_records <- Filter(
    \(record) identical(record@stage, "extract_claims"),
    restored$state$stage_records
  )

  expect_s3_class(condition, "tempest_stage_governance_error")
  expect_identical(
    conditionMessage(condition),
    "Injected extraction failure after the terminal Deputy completion."
  )
  expect_identical(restored$research_manifest@status, "running")
  expect_null(restored$research_manifest@deliverables$report_md)
  expect_identical(restored$state$completed_stages, "perspectives")
  expect_null(restored$state$report_md)
  expect_length(deputy_traces, 1L)
  expect_length(extraction_records, 0L)
  expect_identical(
    tempest:::tempest_research_workspace_mutation_state(restored$workspace),
    "open"
  )
  expect_no_error(restored$workspace$upsert_retrieved_resource(fake_source(
    url = "https://example.org/recoverable-storm",
    title = "Recoverable STORM workspace",
    content_text = "Running research remains open for recovery."
  )))
})

test_that("default STORM verification authorizes extracted claims", {
  skip_if_not_installed("deputy")
  skip_if_not_installed("ellmer")
  skip_if_not_installed("graft")
  fixture <- storm_progress_fixture()
  config <- tempest_config(
    cache_enabled = FALSE,
    chat_fn = fixture$config@chat_fn
  )
  retriever <- tempest_retriever(config = config, workspace = fixture$store)

  expect_identical(config@citation_policy, "source_attributed")

  result <- tempest_run(
    "Default STORM verification",
    config = config,
    retriever = retriever,
    n_experts = 1,
    max_questions_per_perspective = 1,
    verbose = FALSE
  )

  expect_identical(result@manifest@status, "succeeded")
  expect_gt(length(result@workspace$list_claim_supports()), 0L)
  expect_all_true(vapply(
    result@workspace$list_proposed_claims(),
    \(claim) identical(claim@verification_status, "supported"),
    logical(1)
  ))
})

test_that("STORM appends a safe execution review after polish", {
  artifact_id <- paste0("sha256:", strrep("a", 64L))
  running <- tempest:::tempest_stage_record_start(
    "query_decomposition",
    artifact_id,
    attempt_id = "attempt-review",
    started_at = "2026-08-16T01:00:00Z"
  )
  record <- tempest:::tempest_stage_record_succeed(
    running,
    tempest:::tempest_stage_content_reference(list(queries = "topic")),
    support_status = "unknown",
    fallback_taken = TRUE,
    primary_error = simpleError("provider unavailable"),
    completed_at = "2026-08-16T01:00:01Z"
  )

  reviewed <- tempest:::tempest_storm_report_with_execution_review(
    "# Reviewed report\n\nPolished body.",
    list(record),
    title = "Reviewed report"
  )

  expect_match(reviewed, "## Execution review", fixed = TRUE)
  expect_match(reviewed, "attempt-review", fixed = TRUE)
  expect_match(
    reviewed,
    "tempest::fallback/query-decomposition/original-question@1",
    fixed = TRUE
  )
})

test_that("tempest_run binds each expert answer to one Deputy execution", {
  skip_if_not_installed("deputy")
  skip_if_not_installed("ellmer")
  fixture <- storm_progress_fixture()
  expert_chat <- fake_chat(
    text = list(paste0(
      "Expert answer cites progress evidence [",
      fixture$source_id,
      "]."
    ))
  )
  direct_chat_calls <- 0L
  original_chat <- expert_chat$chat
  expert_chat$chat <- function(...) {
    direct_chat_calls <<- direct_chat_calls + 1L
    original_chat(...)
  }
  original_chat_fn <- fixture$config@chat_fn
  fixture$config@chat_fn <- function(role, model, system_prompt, echo) {
    if (identical(role, "expert")) {
      return(expert_chat)
    }
    original_chat_fn(role, model, system_prompt, echo)
  }

  result <- tempest_run(
    "T7 STORM Deputy route",
    config = fixture$config,
    retriever = fixture$retriever,
    experts = list(tempest_expert(
      name = "T7 Route Expert",
      title = "Researcher",
      description = "Exercises the T7 Deputy route.",
      instructions = "Use the supplied evidence."
    )),
    max_questions_per_perspective = 1,
    steps = c("perspectives", "research"),
    verbose = FALSE
  )

  expect_identical(direct_chat_calls, 0L)
  expect_identical(
    vapply(expert_chat$.calls(), `[[`, character(1), "transport"),
    "stream_async"
  )

  deputy_traces <- Filter(
    \(trace) identical(trace$trace_type, "deputy_run"),
    result@manifest@traces
  )
  extraction_records <- Filter(
    function(record) {
      identical(record@stage, "extract_claims") &&
        identical(record@status, "succeeded")
    },
    result@state$stage_records
  )
  expect_length(deputy_traces, 1L)
  expect_length(extraction_records, 1L)

  deputy_fields <- c(
    "deputy_run_id",
    "deputy_session_id",
    "parent_run_id",
    "delegation_id",
    "tool_call_id"
  )
  transform_records <- Filter(
    \(record) !identical(record@stage, "extract_claims"),
    result@state$stage_records
  )
  expect_identical(
    lapply(
      transform_records,
      \(record) intersect(names(record@trace_references), deputy_fields)
    ),
    rep(list(character()), length(transform_records))
  )

  if (length(deputy_traces) == 1L && length(extraction_records) == 1L) {
    trace <- deputy_traces[[1L]]
    extraction <- extraction_records[[1L]]
    expert <- result@state$experts[[1L]]

    expect_identical(trace$status, "complete")
    expect_identical(trace$stage, "research")
    expect_identical(trace$role, "expert")
    expect_identical(trace$trace_id, trace$deputy_run_id)
    expect_identical(trace$expert_id, expert@expert_id)
    expect_identical(
      unlist(result@manifest@runtime$deputy_run_ids, use.names = FALSE),
      trace$deputy_run_id
    )
    expect_identical(
      unlist(result@manifest@runtime$deputy_session_ids, use.names = FALSE),
      trace$deputy_session_id
    )
    expect_identical(
      extraction@program_artifact_id,
      result@manifest@programs$extract_claims$program_artifact_id
    )
    expect_identical(
      extraction@trace_references$deputy_run_id,
      trace$deputy_run_id
    )
    expect_identical(
      extraction@trace_references$deputy_session_id,
      trace$deputy_session_id
    )
    expect_identical(
      extraction@trace_references$correlation_id,
      trace$correlation_id
    )
    expect_identical(
      extraction@trace_references$expert_id,
      trace$expert_id
    )
    expect_identical(
      intersect(names(extraction@trace_references), deputy_fields[-(1:2)]),
      character()
    )

    claims <- fixture$store$list_proposed_claims()
    expect_length(claims, 1L)
    if (length(claims) == 1L) {
      expect_identical(
        claims[[1L]]@session_id,
        result@manifest@research_run_id
      )
      expect_identical(claims[[1L]]@expert_id, trace$expert_id)
      expect_identical(
        claims[[1L]]@retrieval_step_id,
        trace$correlation_id
      )
    }
  }
})

test_that("retired STORM compatibility arguments are unknown", {
  skip_if_not_installed("ellmer")
  provider_calls <- 0L
  config <- tempest_config(
    cache_enabled = FALSE,
    chat_fn = function(role, model, system_prompt, echo) {
      provider_calls <<- provider_calls + 1L
      rlang::abort("Provider work began.", class = "test_provider_called")
    }
  )
  retriever <- tempest_retriever(
    config = config,
    workspace = tempest_research_workspace()
  )
  expert <- tempest_expert(
    name = "Parallel Boundary Expert",
    title = "Researcher",
    description = "Tests the unsupported parallel boundary.",
    instructions = "Do not execute provider work."
  )

  base_args <- list(
    topic = "Compatibility boundary",
    config = config,
    retriever = retriever,
    experts = list(expert),
    max_questions_per_perspective = 1,
    steps = c("perspectives", "research"),
    verbose = FALSE
  )
  for (argument in c("parallel_research", "remove_duplicate")) {
    condition <- tryCatch(
      do.call(
        tempest_run,
        c(base_args, stats::setNames(list(TRUE), argument))
      ),
      error = \(error) error
    )
    expect_s3_class(condition, "simpleError")
    expect_match(
      conditionMessage(condition),
      paste0("unused argument (", argument, " = TRUE)"),
      fixed = TRUE,
      info = argument
    )
  }
  expect_identical(provider_calls, 0L)
})
