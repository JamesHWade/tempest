test_that("Co-STORM sessions own a manifest and research workspace", {
  skip_if_not_installed("ellmer")
  source <- fake_source("https://example.org/session-workspace")
  extracted <- list(
    facts = list(list(
      claim = "The session owns ProgramSet-bound extraction.",
      sources = list(list(
        source_id = source@resource_id,
        quote = source@content
      )),
      confidence = "high"
    ))
  )
  moderator <- fake_chat(
    text = list(paste0(
      "ProgramSet-bound extraction [",
      source@resource_id,
      "]."
    ))
  )
  extractor <- fake_chat(structured = list(extracted))
  config <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) {
      if (identical(system_prompt, tempest_prompt("fact_extractor_system"))) {
        return(extractor)
      }
      if (identical(role, "coordinator")) {
        return(moderator)
      }
      fake_chat()
    }
  )
  program_set <- tempest_program_set()
  expert <- test_expert(
    expert_id = "expert.research-session",
    name = "Research Session Expert"
  )

  session <- tempest_session(
    "Research session",
    config = config,
    experts = list(expert),
    session_id = "research-session-1"
  )

  expect_r6_class(
    tempest:::tempest_session_workspace(session),
    "ResearchWorkspace"
  )
  expect_identical(
    tempest:::tempest_session_retriever(session)$workspace,
    tempest:::tempest_session_workspace(session)
  )
  expect_identical(
    session$session_id,
    tempest:::tempest_session_manifest(session)@research_run_id
  )
  expect_identical(
    tempest:::tempest_session_manifest(session)@research_run_id,
    "research-session-1"
  )
  expect_identical(tempest:::tempest_session_manifest(session)@mode, "costorm")
  expect_identical(
    tempest:::tempest_session_manifest(session)@status,
    "running"
  )
  expect_identical(
    tempest:::tempest_session_manifest(session)@config_digest,
    tempest_research_config_digest(config)
  )
  expect_identical(
    tempest:::tempest_session_manifest(session)@programs,
    tempest:::tempest_program_set_manifest_programs(program_set)
  )
  stages <- tempest:::tempest_program_set_stages()
  programs <- tempest:::tempest_session_programs(session)
  expect_identical("programs" %in% names(session), FALSE)
  expect_named(programs, stages)
  expect_identical(
    unname(vapply(
      programs,
      inherits,
      logical(1),
      what = "tempest_dsprrr_execution"
    )),
    rep(TRUE, length(stages))
  )
  expect_r6_class(
    tempest:::tempest_session_expert_manager(session),
    "TempestDeputyExpertManager"
  )
  expect_identical(
    programs$extract_claims$trace_context$research_run_id,
    session$session_id
  )
  expect_identical(
    programs$extract_claims$trace_context$stage,
    "extract_claims"
  )
  expect_disjoint(
    names(programs$extract_claims$trace_context),
    "program_artifact_id"
  )
  expect_identical(
    tempest:::tempest_session_manifest(session)@knowledge_snapshot,
    list()
  )
  expect_identical(tempest:::tempest_session_manifest(session)@runtime, list())
  expect_identical(tempest:::tempest_session_manifest(session)@traces, list())
  expect_identical(
    tempest:::tempest_session_manifest(session)@deliverables,
    list()
  )

  original_retriever <- tempest:::tempest_session_retriever(session)
  replacement <- tempest_research_workspace()
  expect_error(
    session$topic <- "Replacement topic",
    class = "tempest_session_error",
    regexp = "fixed when the session is created"
  )
  expect_error(
    session$session_id <- "replacement-session",
    class = "tempest_session_error",
    regexp = "fixed when the session is created"
  )
  expect_setequal(
    intersect(
      names(session),
      c("config", "retriever", "workspace", "manifest", "events", "title")
    ),
    character()
  )
  expect_error(session$workspace <- replacement)
  mutated_config <- tempest:::tempest_session_config(session)
  mutated_config@max_sources <- config@max_sources + 1L
  expect_identical(tempest:::tempest_session_config(session), config)
  expect_error(
    tempest:::tempest_session_retriever(session)$workspace <- replacement,
    class = "tempest_retriever_identity_error"
  )
  mutated_manifest <- tempest:::tempest_session_manifest(session)
  mutated_manifest@status <- "succeeded"
  expect_identical(
    tempest:::tempest_session_manifest(session)@status,
    "running"
  )
  expect_identical(session$topic, "Research session")
  expect_identical(tempest:::tempest_session_config(session), config)
  expect_identical(session$session_id, "research-session-1")
  expect_identical(
    tempest:::tempest_session_retriever(session),
    original_retriever
  )
  expect_identical(
    tempest:::tempest_session_retriever(session)$workspace,
    tempest:::tempest_session_workspace(session)
  )
  expect_identical(
    tempest:::tempest_session_manifest(session)@status,
    "running"
  )

  expect_no_error(tempest:::tempest_session_workspace(
    session
  )$upsert_retrieved_resource(source))
  expect_identical(
    tempest:::tempest_session_workspace(session)$get_retrieved_source(
      source@resource_id
    )$id,
    source@resource_id
  )
  completion <- await_tempest_promise(
    session$.__enclos_env__$private$request_completion_async(
      "What does the bound source establish?"
    )
  )
  expect_null(completion$error)
  processed <- withCallingHandlers(
    await_tempest_promise(tempest_session_process_turn_async(
      session,
      completion$value,
      suggest = FALSE
    )),
    dsprrr_cache_security_warning = function(condition) {
      invokeRestart("muffleWarning")
    }
  )
  expect_null(processed$error)
  expect_equal(
    tempest:::tempest_session_workspace(session)$list_proposed_claims()[[
      1
    ]]@claim_text,
    "The session owns ProgramSet-bound extraction."
  )
  extraction_records <- tempest:::tempest_session_stage_records(session)
  expect_length(extraction_records, 1L)
  expect_identical(extraction_records[[1]]@stage, "extract_claims")
  expect_identical(extraction_records[[1]]@status, "succeeded")
  expect_identical(
    unlist(extraction_records[[1]]@output_reference$ids),
    tempest:::tempest_session_workspace(session)$list_proposed_claims()[[
      1
    ]]@claim_id
  )

  session$.__enclos_env__$private$add_turn(
    "User",
    "user",
    "What evidence is available?"
  )
  expect_match(
    session$.__enclos_env__$private$transcript_markdown(),
    "What evidence is available?",
    fixed = TRUE
  )
  expect_identical(
    tempest:::tempest_session_manifest(session)@status,
    "running"
  )
})

test_that("Co-STORM keeps its stage ledger private and mutation isolated", {
  skip_if_not_installed("ellmer")
  config <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  session <- tempest_session(
    "Private stage ledger",
    config = config,
    experts = list(test_expert(
      expert_id = "expert.private-ledger",
      name = "Private Ledger Expert"
    )),
    session_id = "private-stage-ledger"
  )
  program <- tempest:::tempest_session_programs(session)$extract_claims
  record <- tempest:::tempest_stage_record_start(
    "extract_claims",
    program$program_artifact_id,
    trace_references = list(
      research_run_id = session$session_id,
      role = "extractor"
    ),
    attempt_id = "stage-attempt-private-ledger"
  )
  tempest:::tempest_session_record_stage(session, record)

  expect_identical("stage_records" %in% names(session), FALSE)
  expect_null(session$stage_records)
  record@trace_references$role <- "mutated-input"
  first_read <- tempest:::tempest_session_stage_records(session)
  expect_identical(first_read[[1]]@trace_references$role, "extractor")

  first_read[[1]]@trace_references$role <- "mutated-output"
  second_read <- tempest:::tempest_session_stage_records(session)
  expect_identical(second_read[[1]]@trace_references$role, "extractor")
})

test_that("Co-STORM stage ledger batches commit atomically", {
  skip_if_not_installed("ellmer")
  config <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  session <- tempest_session(
    "Atomic stage ledger",
    config = config,
    experts = list(test_expert(
      expert_id = "expert.atomic-ledger",
      name = "Atomic Ledger Expert"
    )),
    session_id = "atomic-stage-ledger"
  )
  programs <- tempest:::tempest_session_programs(session)
  first <- tempest:::tempest_stage_record_start(
    "extract_claims",
    programs$extract_claims$program_artifact_id,
    attempt_id = "stage-attempt-atomic-ledger"
  )
  conflicting <- tempest:::tempest_stage_record_start(
    "verify_claim_support",
    programs$verify_claim_support$program_artifact_id,
    trace_references = list(
      min_support_score = "0.7",
      verified_at = "2026-08-16T12:03:00Z"
    ),
    attempt_id = "stage-attempt-atomic-ledger"
  )

  expect_error(
    tempest:::tempest_session_record_stages(
      session,
      list(first, conflicting)
    ),
    class = "tempest_stage_lifecycle_error"
  )
  expect_length(tempest:::tempest_session_stage_records(session), 0L)

  tempest:::tempest_session_record_stages(session, list(first))
  expect_identical(
    tempest:::tempest_session_stage_records(session),
    list(first)
  )
})

test_that("Co-STORM mind maps ignore raw chat output", {
  skip_if_not_installed("ellmer")
  invalid_map <- list(
    nodes = list(
      list(id = "root", label = "Mind-map integrity", source_ids = character()),
      list(
        id = "finding",
        label = "Unbound finding",
        parent = "root",
        source_ids = "Sffffffffffff"
      )
    ),
    edges = list(list(from = "root", to = "finding", relation = "contains"))
  )
  mindmap_chat <- fake_chat(structured = list(invalid_map))
  config <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) {
      if (identical(role, "mindmap")) mindmap_chat else fake_chat()
    }
  )
  session <- tempest_session(
    "Mind-map integrity",
    config = config,
    experts = list(test_expert(
      expert_id = "expert.mindmap-integrity",
      name = "Mind-map Integrity Expert"
    )),
    session_id = "mindmap-integrity"
  )
  original <- session$mindmap

  expect_no_error(
    session$.__enclos_env__$private$update_mindmap(
      "An unsupported finding appeared."
    )
  )
  expect_identical(session$mindmap, original)
  expect_length(mindmap_chat$.calls(), 0L)
})

test_that("automatic Co-STORM personas record the exact product attempt", {
  skip_if_not_installed("ellmer")
  generated <- list(
    personas = list(list(
      name = "Dr. Recorded",
      title = "Research systems analyst",
      affiliation = "Independent",
      background = "Studies durable research execution.",
      focus_areas = list("execution records"),
      perspective = "Product-owned provenance",
      initial_questions = list("Which attempt produced this persona?")
    ))
  )
  config <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) {
      fake_chat(structured = list(generated))
    }
  )
  session <- tempest_session(
    "Recorded personas",
    config = config,
    n_experts = 1L,
    session_id = "recorded-personas"
  )
  records <- tempest:::tempest_session_stage_records(session)

  expect_length(session$experts, 1L)
  expect_length(records, 1L)
  expect_identical(records[[1]]@stage, "personas")
  expect_identical(records[[1]]@status, "succeeded")
  expect_identical(
    records[[1]]@program_artifact_id,
    tempest:::tempest_session_manifest(
      session
    )@programs$personas$program_artifact_id
  )
  expect_identical(
    unlist(records[[1]]@output_reference$ids),
    "experts"
  )
})

test_that("Co-STORM snapshots reject mutated live ProgramSets", {
  skip_if_not_installed("ellmer")
  config <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  session <- tempest_session(
    "Mutated ProgramSet",
    config = config,
    experts = list(test_expert(
      expert_id = "expert.program-mutation",
      name = "Program Mutation Expert"
    ))
  )
  expected_id <- tempest:::tempest_session_manifest(
    session
  )@programs$extract_claims$program_artifact_id
  program_set <- tempest:::tempest_session_program_set(session)
  module <- program_set@programs$extract_claims

  module$signature@instructions <- paste(
    module$signature@instructions,
    "Mutated after session construction."
  )

  expect_identical(
    identical(dsprrr::program_artifact_id(module), expected_id),
    FALSE
  )
  expect_error(
    tempest_session_snapshot(session),
    class = "tempest_session_snapshot_error",
    regexp = "inconsistent Co-STORM ProgramSet"
  )
})

test_that("Co-STORM rejects scalar-only pinned workspaces", {
  skip_if_not_installed("ellmer")
  config <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  workspace <- tempest_research_workspace(
    base_snapshot_id = "snapshot-accepted-1"
  )
  retriever <- tempest_retriever(config = config, workspace = workspace)

  expect_error(
    tempest_session(
      "Pinned research session",
      config = config,
      experts = list(test_expert(
        expert_id = "expert.pinned-session",
        name = "Pinned Session Expert"
      )),
      retriever = retriever,
      session_id = "research-session-pinned"
    ),
    class = "tempest_ecosystem_contract_error"
  )
})

test_that("Co-STORM rejects a mismatched TempestRetriever before execution", {
  skip_if_not_installed("ellmer")
  chat_calls <- 0L
  session_config <- tempest_config(
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
    tempest_session(
      "Retriever config identity",
      config = session_config,
      experts = list(test_expert(
        expert_id = "expert.session-retriever-config",
        name = "Session Retriever Config Expert"
      )),
      retriever = retriever
    ),
    class = "tempest_session_error",
    regexp = "same behavior-relevant configuration"
  )
  expect_equal(chat_calls, 0L)
})

test_that("Co-STORM sessions reject retriever lookalikes", {
  skip_if_not_installed("ellmer")
  config <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  retriever <- new.env(parent = emptyenv())
  retriever$workspace <- tempest_research_workspace()

  expect_error(
    tempest_session(
      "Divergent workspace aliases",
      config = config,
      experts = list(test_expert(
        expert_id = "expert.divergent-session",
        name = "Divergent Session Expert"
      )),
      retriever = retriever
    ),
    class = "tempest_deputy_expert_error",
    regexp = "must be a TempestRetriever"
  )
})

test_that("Co-STORM restoration preserves manifest identity", {
  skip_if_not_installed("ellmer")
  config <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) {
      if (identical(role, "writer")) {
        return(fake_chat(text = list("Report body.")))
      }
      fake_chat()
    }
  )
  program_set <- tempest_program_set()
  knowledge <- test_knowledge_view()
  workspace <- tempest_research_workspace(graft_snapshot = knowledge$snapshot)
  snapshot_reference <- tempest:::tempest_snapshot_reference(
    knowledge$snapshot
  )
  retriever <- tempest_retriever(config = config, workspace = workspace)
  manifest <- tempest_research_manifest(
    research_run_id = "restored-costorm-session",
    mode = "costorm",
    config = config,
    programs = tempest:::tempest_program_set_manifest_programs(program_set),
    knowledge_snapshot = snapshot_reference,
    runtime = list(),
    traces = list(),
    deliverables = list(),
    status = "running"
  )

  expect_identical("manifest" %in% names(formals(tempest_session)), FALSE)
  session <- tempest_session_restore_new(
    "Restored research session",
    config = config,
    experts = list(test_expert(
      expert_id = "expert.restored-session",
      name = "Restored Session Expert"
    )),
    retriever = retriever,
    program_set = program_set,
    manifest = manifest
  )
  expect_identical(tempest:::tempest_session_manifest(session), manifest)
  expect_identical(session$session_id, manifest@research_run_id)
  expect_null(tempest:::tempest_session_report_value(session))
  expect_identical(
    tempest:::tempest_session_manifest(session)@status,
    "running"
  )
  expect_false("manifest" %in% names(session))
  expect_error(
    session$manifest <- tempest_research_manifest_update(
      manifest,
      status = "succeeded"
    )
  )
  expect_identical(tempest:::tempest_session_manifest(session), manifest)
})

test_that("Co-STORM restoration rejects mismatched manifests", {
  skip_if_not_installed("ellmer")
  config <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  program_set <- tempest_program_set()
  knowledge <- test_knowledge_view()
  workspace <- tempest_research_workspace(graft_snapshot = knowledge$snapshot)
  snapshot_reference <- tempest:::tempest_snapshot_reference(
    knowledge$snapshot
  )
  retriever <- tempest_retriever(config = config, workspace = workspace)
  expert <- test_expert(
    expert_id = "expert.invalid-manifest",
    name = "Invalid Manifest Expert"
  )
  create_session <- function(manifest, session_id = NULL, config_ = config) {
    tempest_session_restore_new(
      "Manifest validation",
      config = config_,
      experts = list(expert),
      retriever = retriever,
      session_id = session_id,
      program_set = program_set,
      manifest = manifest
    )
  }
  manifest <- function(
    mode = "costorm",
    status = "running",
    snapshot_id = snapshot_reference$snapshot_id,
    config_ = config
  ) {
    knowledge_snapshot <- snapshot_reference
    knowledge_snapshot$snapshot_id <- snapshot_id
    tempest_research_manifest(
      research_run_id = "manifest-session",
      mode = mode,
      config = config_,
      programs = tempest:::tempest_program_set_manifest_programs(program_set),
      knowledge_snapshot = knowledge_snapshot,
      runtime = list(),
      status = status
    )
  }

  expect_error(
    tempest:::TempestSession$new(
      "Manifest validation",
      config = config,
      experts = list(expert),
      retriever = retriever,
      program_set = program_set,
      .restore_manifest = manifest()
    ),
    class = "tempest_session_error",
    regexp = "internal session-restoration seam"
  )
  expect_error(
    create_session(list()),
    class = "tempest_session_error",
    regexp = "tempest_research_manifest"
  )
  expect_error(
    create_session(manifest(), session_id = "replacement-session"),
    class = "tempest_session_error",
    regexp = "identity cannot be replaced"
  )
  expect_error(
    create_session(manifest(mode = "storm")),
    class = "tempest_session_error",
    regexp = "costorm"
  )
  expect_error(
    create_session(manifest(status = "succeeded")),
    class = "tempest_session_error",
    regexp = "canonical durable report binding"
  )
  expect_error(
    create_session(manifest(snapshot_id = "snapshot:mismatched")),
    class = "tempest_session_error",
    regexp = "base snapshot"
  )

  changed_config <- tempest_config(
    max_sources = config@max_sources + 1L,
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  expect_error(
    create_session(manifest(), config_ = changed_config),
    class = "tempest_session_error",
    regexp = "does not match the supplied.*config"
  )
})
