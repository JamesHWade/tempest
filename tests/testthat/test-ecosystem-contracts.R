test_that("dsprrr identity enters the manifest and execution metadata", {
  forward <- function(text, ...) list(answer = text)
  program <- dsprrr::module_fn("text -> answer", forward)
  program_artifact_id <- dsprrr::program_artifact_id(
    program,
    registry = list(forward = forward)
  )
  program_set <- test_program_set_from_program(
    program,
    registry = list(forward = forward)
  )
  manifest <- tempest_research_manifest(
    research_run_id = "research-contract",
    mode = "storm",
    config = tempest_config(),
    programs = tempest:::tempest_program_set_manifest_programs(program_set),
    knowledge_snapshot = list(snapshot_id = "snapshot-contract")
  )

  programs <- tempest:::tempest_bind_program_set(program_set, manifest)
  execution <- tempest:::tempest_run_dsprrr_module_structured(
    programs$extract_claims,
    chat = NULL,
    inputs = list(text = "supported"),
    step = "extract_claims"
  )
  expected_context <- list(
    knowledge_snapshot_id = "snapshot-contract",
    mode = "storm",
    product = "tempest",
    research_run_id = "research-contract",
    role = "program",
    stage = "extract_claims"
  )

  expect_identical(execution$output$answer, "supported")
  expect_identical(
    manifest@programs$extract_claims$program_artifact_id,
    program_artifact_id
  )
  expect_identical(
    execution$metadata$program_artifact_id,
    program_artifact_id
  )
  expect_identical(execution$metadata$trace_context, expected_context)
  expect_identical(
    dsprrr::program_artifact_id(program),
    program_artifact_id
  )
  expect_null(attr(program, "tempest_program_artifact_id", exact = TRUE))
  expect_null(attr(program, "tempest_trace_context", exact = TRUE))
})

test_that("dsprrr execution fails closed on a bound identity mismatch", {
  forward <- function(text, ...) list(answer = text)
  program <- dsprrr::module_fn("text -> answer", forward)
  dsprrr::program_artifact_id(program, registry = list(forward = forward))
  program_set <- test_program_set_from_program(
    program,
    registry = list(forward = forward)
  )
  manifest <- tempest_research_manifest(
    research_run_id = "research-tampered-program",
    mode = "storm",
    config = tempest_config(),
    programs = tempest:::tempest_program_set_manifest_programs(program_set)
  )
  programs <- tempest:::tempest_bind_program_set(program_set, manifest)
  programs$extract_claims$program_artifact_id <- paste0(
    "sha256:",
    strrep("0", 64L)
  )

  expect_error(
    tempest:::tempest_run_dsprrr_module(
      programs$extract_claims,
      chat = NULL,
      inputs = list(text = "supported"),
      step = "extract_claims"
    ),
    class = "tempest_program_set_verification_error"
  )
})

test_that("dsprrr execution rejects tampered structured metadata", {
  forward <- function(text, ...) list(answer = text)
  program_set <- test_program_set_from_program(
    dsprrr::module_fn("text -> answer", forward),
    registry = list(forward = forward)
  )
  manifest <- tempest_research_manifest(
    research_run_id = "research-tampered-metadata",
    mode = "storm",
    config = tempest_config(),
    programs = tempest:::tempest_program_set_manifest_programs(program_set)
  )
  program <- tempest:::tempest_bind_program_set(
    program_set,
    manifest
  )$extract_claims
  local_mocked_bindings(
    tempest_dsprrr_run = function(...) {
      structure(
        list(
          output = list(answer = "supported"),
          chat = NULL,
          metadata = list(
            program_artifact_id = paste0("sha256:", strrep("0", 64L)),
            trace_context = program$trace_context
          )
        ),
        class = "dsprrr_result"
      )
    }
  )

  expect_error(
    tempest:::tempest_run_dsprrr_module(
      program,
      chat = NULL,
      inputs = list(text = "supported"),
      step = "extract_claims"
    ),
    class = "tempest_ecosystem_contract_error",
    regexp = "bound program artifact"
  )
})

test_that("dsprrr bindings are isolated per invocation", {
  forward <- function(text, ...) list(answer = text)
  program <- dsprrr::module_fn("text -> answer", forward)
  program_artifact_id <- dsprrr::program_artifact_id(
    program,
    registry = list(forward = forward)
  )
  program_set <- test_program_set_from_program(
    program,
    registry = list(forward = forward)
  )
  program_reference <- tempest:::tempest_program_set_manifest_programs(
    program_set
  )
  manifest_a <- tempest_research_manifest(
    research_run_id = "research-binding-a",
    mode = "storm",
    config = tempest_config(),
    programs = program_reference,
    knowledge_snapshot = list(snapshot_id = "snapshot-a")
  )
  manifest_b <- tempest_research_manifest(
    research_run_id = "research-binding-b",
    mode = "storm",
    config = tempest_config(),
    programs = program_reference,
    knowledge_snapshot = list(snapshot_id = "snapshot-b")
  )

  binding_a <- tempest:::tempest_bind_program_set(program_set, manifest_a)
  binding_b <- tempest:::tempest_bind_program_set(program_set, manifest_b)
  result_a <- tempest:::tempest_run_dsprrr_module_structured(
    binding_a$extract_claims,
    chat = NULL,
    inputs = list(text = "a"),
    step = "extract_claims"
  )
  result_b <- tempest:::tempest_run_dsprrr_module_structured(
    binding_b$extract_claims,
    chat = NULL,
    inputs = list(text = "b"),
    step = "extract_claims"
  )

  expect_identical(
    binding_a$extract_claims$trace_context$research_run_id,
    "research-binding-a"
  )
  expect_identical(
    binding_b$extract_claims$trace_context$research_run_id,
    "research-binding-b"
  )
  expect_identical(
    result_a$metadata$trace_context,
    binding_a$extract_claims$trace_context
  )
  expect_identical(
    result_b$metadata$trace_context,
    binding_b$extract_claims$trace_context
  )
  expect_identical(
    result_a$metadata$program_artifact_id,
    program_artifact_id
  )
  expect_identical(
    result_b$metadata$program_artifact_id,
    program_artifact_id
  )
  expect_null(attr(program, "tempest_program_artifact_id", exact = TRUE))
  expect_null(attr(program, "tempest_trace_context", exact = TRUE))
})

test_that("dsprrr trace-context contract errors do not trigger fallback", {
  forward <- function(text, ...) list(answer = text)
  program <- dsprrr::module_fn("text -> answer", forward)
  program_set <- test_program_set_from_program(
    program,
    registry = list(forward = forward)
  )
  manifest <- tempest_research_manifest(
    research_run_id = "research-invalid-context",
    mode = "storm",
    config = tempest_config(),
    programs = tempest:::tempest_program_set_manifest_programs(program_set)
  )
  execution <- tempest:::tempest_bind_program_set(
    program_set,
    manifest
  )$extract_claims
  execution$trace_context$program_artifact_id <- execution$program_artifact_id

  expect_error(
    tempest:::tempest_run_dsprrr_module(
      execution,
      chat = NULL,
      inputs = list(text = "invalid"),
      step = "extract_claims"
    ),
    class = "dsprrr_trace_context_error"
  )
})

test_that("Deputy context is canonical and contains no runtime objects", {
  program_reference <- test_program_reference("extract_claims")
  program_artifact_id <- program_reference$program_artifact_id
  manifest <- tempest_research_manifest(
    research_run_id = "research-context",
    mode = "costorm",
    config = tempest_config(),
    programs = list(
      extract_claims = program_reference
    ),
    knowledge_snapshot = list(snapshot_id = "sha256:snapshot")
  )

  context <- tempest:::tempest_deputy_run_context(
    manifest,
    stage = "expert_research",
    role = "expert",
    program_artifact_id = program_artifact_id
  )

  expect_identical(
    context,
    list(
      knowledge_snapshot_id = "sha256:snapshot",
      mode = "costorm",
      product = "tempest",
      program_artifact_id = program_artifact_id,
      research_run_id = "research-context",
      role = "expert",
      stage = "expert_research"
    )
  )
  expect_no_error(jsonlite::toJSON(context, auto_unbox = TRUE))
  expect_length(Filter(is.object, context), 0L)
})

test_that("Deputy context binds program identity to the manifest", {
  program_artifact_id <- paste0("sha256:", strrep("a", 64L))
  unrecorded_program_artifact_id <- paste0("sha256:", strrep("b", 64L))
  manifest <- tempest_research_manifest(
    research_run_id = "research-context-program",
    mode = "costorm",
    config = tempest_config(),
    programs = list(
      evidence_review = test_program_reference(
        "evidence_review",
        program_artifact_id
      ),
      evidence_summary = test_program_reference(
        "evidence_summary",
        program_artifact_id
      )
    )
  )

  context <- tempest:::tempest_deputy_run_context(
    manifest,
    stage = "evidence_review",
    role = "moderator",
    program_artifact_id = program_artifact_id
  )
  expect_identical(
    context$program_artifact_id,
    program_artifact_id
  )

  mismatch <- expect_error(
    tempest:::tempest_deputy_run_context(
      manifest,
      stage = "evidence_review",
      role = "moderator",
      program_artifact_id = unrecorded_program_artifact_id
    ),
    class = "tempest_ecosystem_contract_error"
  )
  expect_match(conditionMessage(mismatch), "not recorded")

  empty_manifest <- tempest_research_manifest(
    research_run_id = "research-context-empty-programs",
    mode = "costorm",
    config = tempest_config()
  )
  expect_error(
    tempest:::tempest_deputy_run_context(
      empty_manifest,
      stage = "evidence_review",
      role = "moderator",
      program_artifact_id = unrecorded_program_artifact_id
    ),
    class = "tempest_ecosystem_contract_error"
  )
})

test_that("Graft snapshots retain their immutable restoration boundary", {
  skip_if_not_installed("graft")

  store_path <- file.path(withr::local_tempdir(), "contract.duckdb")
  schema <- graft::graft_schema(system.file(
    "extdata",
    "team-directory.data-dict.json",
    package = "graft",
    mustWork = TRUE
  ))
  store <- graft::graft_open(schema, store_path, okf = "disabled")
  withr::defer(graft::graft_close(store))
  empty_snapshot <- graft::graft_snapshot(store)
  empty_reference <- tempest:::tempest_snapshot_reference(empty_snapshot)
  empty_workspace <- tempest_research_workspace(
    graft_snapshot = empty_snapshot
  )
  graft::graft_ingest(
    store,
    list(organization = data.frame(id = "org:tempest", name = "Tempest")),
    graft::graft_provenance(
      "tempest-contract",
      idempotency_key = "tempest-contract-1"
    )
  )
  snapshot <- graft::graft_snapshot(store)
  reference <- tempest:::tempest_snapshot_reference(snapshot)
  snapshot_fields <- c(
    "schema_version",
    "snapshot_id",
    "store_id",
    "store_format_version",
    "schema_build_digest",
    "commit_order",
    "batch_id",
    "committed_at",
    "history_complete"
  )
  config <- tempest_config()
  empty_manifest <- tempest_research_manifest(
    research_run_id = "research-empty-graft-contract",
    mode = "storm",
    config = config,
    knowledge_snapshot = empty_reference
  )
  restored_empty_manifest <- tempest_research_manifest_from_record(
    jsonlite::fromJSON(
      tempest_research_manifest_canonical_json(
        tempest_research_manifest_record(empty_manifest)
      ),
      simplifyVector = FALSE
    )
  )
  program_set <- tempest_program_set()
  manifest <- tempest_research_manifest(
    research_run_id = "research-graft-contract",
    mode = "storm",
    config = config,
    programs = tempest:::tempest_program_set_manifest_programs(program_set),
    knowledge_snapshot = reference
  )
  manifest_json <- tempest_research_manifest_canonical_json(
    tempest_research_manifest_record(manifest)
  )
  restored_manifest <- tempest_research_manifest_from_record(
    jsonlite::fromJSON(manifest_json, simplifyVector = FALSE)
  )
  workspace <- tempest_research_workspace(graft_snapshot = snapshot)
  run_dir <- file.path(dirname(store_path), "storm-bundle")
  tempest:::tempest_save_run_artifacts(
    run_dir,
    workspace,
    tempest:::tempest_storm_state("Graft contract"),
    manifest,
    program_set = program_set,
    config = config,
    steps = "research",
    research_strategy = "key_questions"
  )
  serialized_snapshot <- serialize(snapshot, NULL, version = 3L)
  snapshot_id_raw <- charToRaw(reference$snapshot_id)
  match_starts <- Filter(
    \(start) {
      end <- start + length(snapshot_id_raw) - 1L
      identical(serialized_snapshot[start:end], snapshot_id_raw)
    },
    seq_len(length(serialized_snapshot) - length(snapshot_id_raw) + 1L)
  )
  expect_length(match_starts, 1L)
  replacement_raw <- charToRaw(paste0("sha256:", strrep("0", 64L)))
  tampered_serialized <- serialized_snapshot
  start <- match_starts[[1L]]
  end <- start + length(replacement_raw) - 1L
  tampered_serialized[start:end] <- replacement_raw
  tampered_snapshot <- unserialize(tampered_serialized)

  rm(snapshot, workspace)
  graft::graft_close(store)
  store <- graft::graft_open(schema, store_path, okf = "disabled")
  graft::graft_ingest(
    store,
    list(organization = data.frame(id = "org:tempest", name = "Tempest 2")),
    graft::graft_provenance(
      "tempest-contract",
      idempotency_key = "tempest-contract-2"
    )
  )
  restored_run <- tempest:::tempest_load_run_artifacts(
    run_dir,
    config = config,
    program_set = program_set,
    run_id = "research-graft-contract"
  )
  restored_snapshot <- restored_run$workspace$graft_snapshot
  pinned <- graft::graft_at(store, restored_snapshot)

  expect_setequal(names(reference), snapshot_fields)
  expect_setequal(names(empty_reference), snapshot_fields)
  expect_null(empty_reference$batch_id)
  expect_null(empty_reference$committed_at)
  expect_identical(empty_workspace$graft_snapshot, empty_snapshot)
  expect_identical(
    empty_workspace$list_accepted_graft_references(),
    list(empty_reference)
  )
  expect_identical(
    restored_empty_manifest@knowledge_snapshot,
    empty_reference
  )
  expect_identical(restored_manifest@knowledge_snapshot, reference)
  expect_identical(
    tempest_research_manifest_record(restored_manifest)$knowledge_snapshot,
    reference
  )
  expect_identical(
    restored_run$workspace$list_accepted_graft_references(),
    list(reference)
  )
  expect_identical(
    restored_run$research_manifest@knowledge_snapshot,
    reference
  )
  expect_identical(
    graft::graft_get(pinned, "org:tempest")$record$name,
    "Tempest"
  )
  expect_identical(
    graft::graft_get(store, "org:tempest")$record$name,
    "Tempest 2"
  )
  expect_error(
    tempest:::tempest_snapshot_reference(pinned),
    class = "tempest_ecosystem_contract_error"
  )
  expect_error(
    tempest:::tempest_snapshot_reference(tampered_snapshot),
    class = "tempest_ecosystem_contract_error"
  )
})

test_that("Tempest context survives Deputy delegation and hooks", {
  skip_if_not_installed("deputy")

  program_reference <- test_program_reference("review")
  program_artifact_id <- program_reference$program_artifact_id
  manifest <- tempest_research_manifest(
    research_run_id = "research-deputy-contract",
    mode = "costorm",
    config = tempest_config(),
    programs = list(
      review = program_reference
    ),
    knowledge_snapshot = list(snapshot_id = "sha256:deputy-snapshot")
  )
  context <- tempest:::tempest_deputy_run_context(
    manifest,
    stage = "evidence_review",
    role = "moderator",
    program_artifact_id = program_artifact_id
  )
  child_chat <- tempest_contract_child_chat()
  parent_chat <- tempest_contract_parent_chat(child_chat)
  inspect_evidence <- ellmer::tool(
    fun = function(claim) paste("reviewed", claim),
    name = "inspect_evidence",
    description = "Inspect deterministic evidence.",
    arguments = list(claim = ellmer::type_string("Claim identifier")),
    annotations = ellmer::tool_annotations(read_only_hint = TRUE)
  )
  definition <- deputy::agent_definition(
    name = "evidence_reviewer",
    description = "Reviews evidence",
    prompt = "Review the supplied claim.",
    tools = list(inspect_evidence)
  )
  root <- withr::local_tempdir(pattern = "tempest-deputy-")
  lead <- deputy::LeadAgent$new(
    chat = parent_chat,
    sub_agents = list(definition),
    permissions = deputy::permissions_standard(root),
    working_dir = root,
    run_context = context,
    agent_id = "agent-tempest-lead",
    agent_name = "moderator"
  )
  lead$configure_sdk_compat(list(
    persist_session = FALSE,
    session_store_dir = root,
    session_id = "tempest-session-contract"
  ))
  seen <- new.env(parent = emptyenv())
  lead$add_hook(deputy::HookMatcher$new(
    event = "SessionStart",
    timeout = 0,
    callback = function(context) {
      seen$start <- context
      deputy::HookResultSessionStart()
    }
  ))
  lead$add_hook(deputy::HookMatcher$new(
    event = "SessionEnd",
    timeout = 0,
    callback = function(reason, context) {
      seen$end <- context
      deputy::HookResultSessionEnd()
    }
  ))
  lead$add_hook(deputy::HookMatcher$new(
    event = "SubagentStart",
    timeout = 0,
    callback = function(agent_name, task, context) {
      seen$subagent_start <- context
      NULL
    }
  ))
  lead$add_hook(deputy::HookMatcher$new(
    event = "SubagentStop",
    timeout = 0,
    callback = function(agent_name, task, result, context) {
      seen$subagent_stop <- context
      deputy::HookResultSubagentStop()
    }
  ))

  parent_result <- lead$run_sync("Delegate evidence review")
  parent_start <- parent_result$tool_calls()[[1L]]
  parent_end <- parent_result$tool_results()[[1L]]
  delegated <- lead$list_subagents()
  child_result <- lead$get_subagent_results(
    delegation_id = delegated$delegation_id
  )[[1L]]
  child_start <- child_result$tool_calls()[[1L]]
  child_end <- child_result$tool_results()[[1L]]
  child_context <- context
  child_context$role <- "evidence_reviewer"
  child_context <- child_context[order(names(child_context))]
  expert_context <- tempest:::tempest_deputy_run_context(
    manifest,
    stage = "evidence_review",
    role = "expert",
    program_artifact_id = program_artifact_id,
    expert_id = "expert.evidence-reviewer"
  )
  correlated_manifest <- tempest_research_manifest_update(
    manifest,
    runtime = list(
      deputy_run_ids = c(parent_result$run_id, child_result$run_id),
      deputy_session_ids = c(
        parent_result$session_id,
        child_result$session_id
      )
    ),
    traces = list(
      list(
        trace_id = parent_result$run_id,
        trace_type = "deputy_run",
        stage = "evidence_review",
        role = "moderator",
        status = parent_result$stop_reason,
        deputy_run_id = parent_result$run_id,
        deputy_session_id = parent_result$session_id,
        agent_id = parent_result$agent_id
      ),
      list(
        trace_id = child_result$run_id,
        trace_type = "deputy_delegation",
        stage = "evidence_review",
        role = "expert",
        status = child_result$stop_reason,
        expert_id = "expert.evidence-reviewer",
        deputy_run_id = child_result$run_id,
        deputy_session_id = child_result$session_id,
        agent_id = child_result$agent_id,
        parent_agent_id = child_result$parent_agent_id,
        parent_run_id = child_result$parent_run_id,
        delegation_id = child_result$delegation_id,
        tool_call_id = parent_start$tool_call_id
      )
    )
  )
  restored_manifest <- tempest_research_manifest_from_record(
    jsonlite::fromJSON(
      tempest_research_manifest_canonical_json(
        tempest_research_manifest_record(correlated_manifest)
      ),
      simplifyVector = FALSE
    )
  )

  expect_identical(parent_result$run_context, context)
  expect_identical(seen$start$run_context, context)
  expect_identical(seen$end$run_context, context)
  expect_identical(child_result$run_context, child_context)
  expect_identical(expert_context$expert_id, "expert.evidence-reviewer")
  expect_identical(parent_start$tool_name, "delegate_to_agent")
  expect_identical(parent_end$tool_name, "delegate_to_agent")
  expect_identical(parent_start$tool_call_id, "parent-delegate-call")
  expect_identical(parent_end$tool_call_id, parent_start$tool_call_id)
  expect_identical(parent_end$delegation_id, parent_start$delegation_id)
  expect_identical(child_start$tool_name, "inspect_evidence")
  expect_identical(child_end$tool_name, "inspect_evidence")
  expect_identical(child_start$tool_call_id, "child-tool-call")
  expect_identical(child_end$tool_call_id, child_start$tool_call_id)
  expect_identical(child_start$agent_id, child_result$agent_id)
  expect_identical(child_end$agent_id, child_result$agent_id)
  expect_identical(child_start$run_id, child_result$run_id)
  expect_identical(child_end$run_id, child_result$run_id)
  expect_identical(child_start$parent_agent_id, parent_result$agent_id)
  expect_identical(child_end$parent_agent_id, parent_result$agent_id)
  expect_identical(child_start$parent_run_id, parent_result$run_id)
  expect_identical(child_end$parent_run_id, parent_result$run_id)
  expect_identical(child_start$delegation_id, parent_start$delegation_id)
  expect_identical(child_end$delegation_id, parent_start$delegation_id)
  expect_identical(child_start$run_context, child_context)
  expect_identical(child_end$run_context, child_context)
  expect_identical(delegated$parent_run_id, parent_result$run_id)
  expect_identical(child_result$parent_run_id, parent_result$run_id)
  expect_identical(delegated$session_id, child_result$session_id)
  expect_identical(delegated$tool_call_id, parent_start$tool_call_id)
  expect_identical(delegated$delegation_id, parent_start$delegation_id)
  expect_identical(seen$subagent_start$run_context, context)
  expect_identical(seen$subagent_stop$run_context, context)
  expect_identical(seen$subagent_start$child_run_context, child_context)
  expect_identical(seen$subagent_stop$child_run_context, child_context)
  expect_identical(seen$subagent_start$agent_id, parent_result$agent_id)
  expect_identical(seen$subagent_stop$agent_id, parent_result$agent_id)
  expect_identical(seen$subagent_start$run_id, parent_result$run_id)
  expect_identical(seen$subagent_stop$run_id, parent_result$run_id)
  expect_identical(seen$subagent_start$child_agent_id, child_result$agent_id)
  expect_identical(seen$subagent_stop$child_agent_id, child_result$agent_id)
  expect_identical(seen$subagent_stop$child_run_id, child_result$run_id)
  expect_identical(
    seen$subagent_start$delegation_id,
    child_result$delegation_id
  )
  expect_identical(
    seen$subagent_stop$delegation_id,
    child_result$delegation_id
  )
  expect_identical(
    seen$subagent_start$parent_agent_id,
    parent_result$agent_id
  )
  expect_identical(
    seen$subagent_stop$parent_agent_id,
    parent_result$agent_id
  )
  expect_identical(
    seen$subagent_start$parent_run_id,
    parent_result$run_id
  )
  expect_identical(
    seen$subagent_stop$parent_run_id,
    parent_result$run_id
  )
  expect_identical(
    seen$subagent_start$tool_call_id,
    parent_start$tool_call_id
  )
  expect_identical(
    seen$subagent_stop$tool_call_id,
    parent_start$tool_call_id
  )
  expect_identical(parent_result$session_id, "tempest-session-contract")
  expect_setequal(
    unlist(restored_manifest@runtime$deputy_run_ids, use.names = FALSE),
    c(parent_result$run_id, child_result$run_id)
  )
  expect_setequal(
    unlist(restored_manifest@runtime$deputy_session_ids, use.names = FALSE),
    c(parent_result$session_id, child_result$session_id)
  )
  expect_identical(
    restored_manifest@traces[[2L]]$expert_id,
    "expert.evidence-reviewer"
  )
  expect_identical(
    restored_manifest@traces[[2L]]$delegation_id,
    child_result$delegation_id
  )
  expect_no_error(jsonlite::toJSON(
    parent_result$run_context,
    auto_unbox = TRUE
  ))
  expect_no_error(jsonlite::toJSON(child_result$run_context, auto_unbox = TRUE))
  contains_runtime_object <- function(value) {
    if (is.function(value) || is.environment(value)) {
      return(TRUE)
    }
    if (!is.list(value)) {
      return(FALSE)
    }
    any(vapply(value, contains_runtime_object, logical(1)))
  }
  expect_identical(contains_runtime_object(context), FALSE)
  expect_identical(
    contains_runtime_object(tempest_research_manifest_record(
      restored_manifest
    )),
    FALSE
  )
})

test_that("research manifests reject the removed program_id alias", {
  expect_error(
    tempest_research_manifest(
      research_run_id = "research-no-alias",
      mode = "storm",
      config = tempest_config(),
      programs = list(stage = list(program_id = "sha256:legacy"))
    ),
    class = "tempest_research_manifest_error"
  )
})

test_that("research manifests require governed procedure vocabulary", {
  legacy_reference <- test_program_reference("extract_claims")
  names(legacy_reference)[
    names(legacy_reference) == "governed_procedure_ref"
  ] <-
    "procedure_revision_id"
  legacy_reference$procedure_revision_id <- "procedure:legacy"
  expect_error(
    tempest_research_manifest(
      research_run_id = "research-no-procedure-alias",
      mode = "storm",
      config = tempest_config(),
      programs = list(
        extract_claims = legacy_reference
      )
    ),
    class = "tempest_research_manifest_error"
  )
  program_id <- test_program_reference("extract_claims")$program_artifact_id
  procedure_ref <- test_governed_procedure_ref(
    "extract_claims",
    program_id
  )
  manifest <- tempest_research_manifest(
    research_run_id = "research-governed-procedure",
    mode = "storm",
    config = tempest_config(),
    programs = list(
      extract_claims = test_program_reference(
        "extract_claims",
        governed_procedure_ref = tempest:::tempest_governed_procedure_record(
          procedure_ref
        )
      )
    )
  )
  expect_identical(
    manifest@programs$extract_claims$governed_procedure_ref,
    tempest:::tempest_governed_procedure_record(procedure_ref)
  )
})

test_that("the 0.2 surface has no T1 compatibility vocabulary", {
  workspace <- tempest_research_workspace()
  retriever <- tempest_retriever(workspace = workspace)
  forbidden_workspace <- c(
    "resources",
    "sources",
    "claims",
    "upsert_source",
    "get_source",
    "list_sources",
    "upsert_resource",
    "get_resource",
    "list_resources",
    "add_claim",
    "get_claim",
    "list_claims",
    "claims_for_source",
    "link_evidence",
    "get_evidence_for_claim",
    "verify_claim"
  )
  forbidden_run_options <- c(
    "runtime",
    "runtime_factory",
    "connection_permissions",
    "artifact_catalog",
    "workflow_run"
  )
  translators <- c(
    "tempest_source_store_snapshot",
    "tempest_source_store_restore",
    "tempest_session_snapshot_translate_v4",
    "tempest_session_snapshot_translate_v5",
    "tempest_session_bundle_translate_v5",
    "tempest_storm_state_translate_v1",
    "tempest_run_bundle_translate_v4",
    "tempest_restore_sources",
    "tempest_restore_claims"
  )

  expect_equal(intersect(names(workspace), forbidden_workspace), character())
  expect_equal(
    intersect(names(retriever), c("store", "list_sources")),
    character()
  )
  workspace_apis <- list(
    tempest_retriever,
    tempest_session,
    tempest_report_md,
    tempest_verify_claims
  )
  expect_equal(
    vapply(workspace_apis, \(fun) "store" %in% names(formals(fun)), logical(1)),
    rep(FALSE, length(workspace_apis))
  )
  expect_equal(
    intersect(names(formals(tempest_run)), forbidden_run_options),
    character()
  )
  product_entry_points <- list(
    tempest_session_restore,
    tempest_session_resume,
    tempest_shiny_server
  )
  expect_equal(
    vapply(
      product_entry_points,
      \(fun) {
        length(intersect(
          names(formals(fun)),
          c(
            "runtime",
            "connection_permissions",
            "artifact_catalog"
          )
        )) >
          0L
      },
      logical(1)
    ),
    rep(FALSE, length(product_entry_points))
  )
  expect_equal(
    intersect(
      getNamespaceExports("tempest"),
      c(
        "SourceStore",
        "TempestSession",
        "tempest_resources",
        "tempest_expert_session_manager",
        "tempest_run_restore",
        "tempest_run_resume"
      )
    ),
    character()
  )
  expect_equal(
    translators[vapply(
      translators,
      exists,
      logical(1),
      envir = asNamespace("tempest"),
      inherits = FALSE
    )],
    character()
  )
  # Frozen generic-kernel state is removed by the section 10 deletion train.
  expect_contains(
    names(formals(tempest:::tempest_run_restore)),
    "source_store"
  )

  session <- tempest_session(
    "Hard-cut surface",
    config = tempest_config(
      chat_fn = function(role, model, system_prompt, echo) fake_chat()
    ),
    experts = list(test_expert(
      expert_id = "expert.hard-cut",
      name = "Hard Cut Expert"
    ))
  )
  expect_equal(
    intersect(names(session), c("artifact_catalog", "workflow_run")),
    character()
  )
})

test_that("legacy product bundles fail closed", {
  bundle_dir <- withr::local_tempdir()
  expect_error(
    tempest:::tempest_research_workspace_restore(list(schema_version = 3L)),
    class = "tempest_unsupported_format_error"
  )
  expect_error(
    tempest_session_restore(list(schema_version = 5L)),
    class = "tempest_unsupported_format_error"
  )
  expect_error(
    tempest:::tempest_session_bundle_validate_manifest(
      bundle_dir,
      list(schema_version = 5L)
    ),
    class = "tempest_unsupported_format_error"
  )
  expect_error(
    tempest:::tempest_run_bundle_validate_manifest(
      bundle_dir,
      list(schema_version = 4L)
    ),
    class = "tempest_unsupported_format_error"
  )
})
