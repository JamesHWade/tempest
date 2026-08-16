test_that("Co-STORM sessions own a manifest and research workspace", {
  skip_if_not_installed("ellmer")
  source <- fake_source("https://example.org/session-workspace")
  extracted <- list(
    facts = list(list(
      claim = "The session owns ProgramSet-bound extraction.",
      sources = list(list(source_id = source$id)),
      confidence = "high"
    ))
  )
  config <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) {
      fake_chat(structured = list(extracted))
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
    session_id = "research-session-1",
    program_set = program_set
  )

  expect_r6_class(session$workspace, "ResearchWorkspace")
  expect_identical(session$retriever$workspace, session$workspace)
  expect_identical(session$session_id, session$manifest@research_run_id)
  expect_identical(session$manifest@research_run_id, "research-session-1")
  expect_identical(session$manifest@mode, "costorm")
  expect_identical(session$manifest@status, "running")
  expect_identical(
    session$manifest@config_digest,
    tempest_research_config_digest(config)
  )
  expect_identical(
    session$manifest@programs,
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
  expect_identical(
    session$expert_session_manager$extract_claims_program,
    programs$extract_claims
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
  expect_identical(session$manifest@knowledge_snapshot, list())
  expect_identical(session$manifest@runtime, list())
  expect_identical(session$manifest@traces, list())
  expect_identical(session$manifest@deliverables, list())

  original_retriever <- session$retriever
  replacement <- tempest_research_workspace()
  expect_error(
    session$topic <- "Replacement topic",
    class = "tempest_session_error",
    regexp = "fixed when the session is created"
  )
  expect_error(
    session$config <- tempest_config(),
    class = "tempest_session_error",
    regexp = "fixed when the session is created"
  )
  expect_error(
    session$session_id <- "replacement-session",
    class = "tempest_session_error",
    regexp = "fixed when the session is created"
  )
  expect_error(
    session$retriever <- tempest_retriever(config = config),
    class = "tempest_session_error",
    regexp = "fixed when the session is created"
  )
  expect_error(
    session$workspace <- replacement,
    class = "tempest_session_error",
    regexp = "fixed when the session is created"
  )
  expect_error(
    session$config@max_sources <- config@max_sources + 1L,
    class = "tempest_session_error"
  )
  expect_error(
    session$retriever$workspace <- replacement,
    class = "tempest_retriever_identity_error"
  )
  expect_error(
    session$manifest@status <- "succeeded",
    class = "tempest_session_error"
  )
  expect_identical(session$topic, "Research session")
  expect_identical(session$config, config)
  expect_identical(session$session_id, "research-session-1")
  expect_identical(session$retriever, original_retriever)
  expect_identical(session$retriever$workspace, session$workspace)
  expect_identical(session$manifest@status, "running")

  expect_no_error(session$workspace$upsert_retrieved_resource(source))
  expect_identical(
    session$workspace$get_retrieved_source(source$id)$id,
    source$id
  )
  expect_no_error(withCallingHandlers(
    session$extract_facts(
      paste0("ProgramSet-bound extraction [", source$id, "]."),
      source_ids = source$id
    ),
    dsprrr_cache_security_warning = function(condition) {
      invokeRestart("muffleWarning")
    }
  ))
  expect_equal(
    session$workspace$list_proposed_claims()[[1]]@claim_text,
    "The session owns ProgramSet-bound extraction."
  )

  session$add_turn("User", "user", "What evidence is available?")
  expect_match(
    session$transcript_markdown(),
    "What evidence is available?",
    fixed = TRUE
  )
  expect_identical(session$manifest@status, "running")
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
  expected_id <- session$manifest@programs$extract_claims$program_artifact_id
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
    class = "tempest_expert_session_error",
    regexp = "must be a <TempestRetriever>"
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
  workspace <- tempest_research_workspace(base_snapshot_id = "snapshot-1")
  retriever <- tempest_retriever(config = config, workspace = workspace)
  manifest <- tempest_research_manifest(
    research_run_id = "restored-costorm-session",
    mode = "costorm",
    config = config,
    programs = tempest:::tempest_program_set_manifest_programs(program_set),
    knowledge_snapshot = list(snapshot_id = "snapshot-1"),
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
  report <- session$report(
    include_references = FALSE,
    reorganize = FALSE
  )

  expect_identical(session$manifest, manifest)
  expect_identical(session$session_id, manifest@research_run_id)
  expect_identical(report, "Report body.")
  expect_identical(session$manifest@status, "running")
  expect_error(
    session$manifest <- tempest_research_manifest_update(
      manifest,
      status = "succeeded"
    ),
    class = "tempest_session_error",
    regexp = "immutable"
  )
  expect_identical(session$manifest, manifest)
})

test_that("Co-STORM restoration rejects mismatched manifests", {
  skip_if_not_installed("ellmer")
  config <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  program_set <- tempest_program_set()
  workspace <- tempest_research_workspace(base_snapshot_id = "snapshot-1")
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
    snapshot_id = "snapshot-1",
    config_ = config
  ) {
    tempest_research_manifest(
      research_run_id = "manifest-session",
      mode = mode,
      config = config_,
      programs = tempest:::tempest_program_set_manifest_programs(program_set),
      knowledge_snapshot = list(snapshot_id = snapshot_id),
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
    regexp = "terminal manifests cannot be resumed"
  )
  expect_error(
    create_session(manifest(snapshot_id = "snapshot-2")),
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
