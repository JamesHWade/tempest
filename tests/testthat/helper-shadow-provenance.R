test_shadow_deputy_execution <- function(
  manifest,
  correlation_id,
  expert_id,
  .local_envir = parent.frame()
) {
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
  context <- tempest:::tempest_deputy_run_context(
    manifest,
    stage = "dialogue",
    role = "moderator"
  )
  lead_agent_id <- tempest:::tempest_deputy_adapter_agent_id(context)
  context$correlation_id <- correlation_id
  context <- tempest:::tempest_research_manifest_canonical_value(
    context,
    "shadow_deputy_context"
  )
  root <- withr::local_tempdir(
    pattern = "tempest-shadow-deputy-",
    .local_envir = .local_envir
  )
  lead <- deputy::LeadAgent$new(
    chat = parent_chat,
    sub_agents = list(definition),
    permissions = deputy::permissions_standard(root),
    working_dir = root,
    run_context = context,
    agent_id = lead_agent_id,
    agent_name = "moderator"
  )
  lead$configure_sdk_compat(list(
    persist_session = FALSE,
    session_store_dir = root,
    session_id = "shadow-parent-session"
  ))
  parent <- lead$run_sync("Delegate evidence review")
  delegation <- lead$list_subagents()
  child <- lead$get_subagent_results(
    delegation_id = delegation$delegation_id
  )[[1L]]
  parent_call <- parent$tool_calls()[[1L]]
  traces <- list(
    list(
      trace_id = parent$run_id,
      trace_type = "deputy_run",
      stage = "dialogue",
      role = "moderator",
      status = parent$stop_reason,
      completion_disposition = if (
        identical(
          parent$stop_reason,
          "complete"
        )
      ) {
        "issued"
      } else {
        "terminal"
      },
      correlation_id = correlation_id,
      deputy_run_id = parent$run_id,
      deputy_session_id = parent$session_id,
      agent_id = parent$agent_id
    ),
    list(
      trace_id = child$run_id,
      trace_type = "deputy_delegation",
      stage = "dialogue",
      role = "expert",
      status = child$stop_reason,
      completion_disposition = if (
        identical(
          child$stop_reason,
          "complete"
        )
      ) {
        "issued"
      } else {
        "terminal"
      },
      correlation_id = correlation_id,
      expert_id = expert_id,
      deputy_run_id = child$run_id,
      deputy_session_id = child$session_id,
      agent_id = child$agent_id,
      parent_agent_id = child$parent_agent_id,
      parent_run_id = child$parent_run_id,
      delegation_id = child$delegation_id,
      tool_call_id = parent_call$tool_call_id
    )
  )
  list(
    parent = parent,
    child = child,
    parent_call = parent_call,
    traces = tempest:::tempest_research_manifest_traces(traces)
  )
}

test_shadow_provenance_fixture <- function(
  verifier_status = "supported",
  verifier_score = 0.95,
  .local_envir = parent.frame()
) {
  run_id <- "research-shadow-provenance"
  correlation_id <- "correlation-shadow-provenance"
  expert_id <- "expert.evidence-reviewer"
  knowledge <- test_knowledge_view(.local_envir = .local_envir)
  workspace <- tempest_research_workspace(graft_snapshot = knowledge$snapshot)
  program_set <- tempest_program_set()
  program_references <- tempest:::tempest_program_set_manifest_programs(
    program_set
  )
  manifest <- tempest_research_manifest(
    research_run_id = run_id,
    mode = "costorm",
    config = tempest_config(min_support_score = 0.7),
    programs = program_references,
    knowledge_snapshot = tempest:::tempest_snapshot_reference(
      knowledge$snapshot
    )
  )
  deputy <- test_shadow_deputy_execution(
    manifest,
    correlation_id = correlation_id,
    expert_id = expert_id,
    .local_envir = .local_envir
  )
  manifest <- tempest_research_manifest_update(
    manifest,
    runtime = list(
      deputy_run_ids = c(deputy$parent$run_id, deputy$child$run_id),
      deputy_session_ids = c(
        deputy$parent$session_id,
        deputy$child$session_id
      )
    ),
    traces = deputy$traces
  )
  programs <- tempest:::tempest_bind_program_set(program_set, manifest)
  programs <- tempest:::tempest_programs_bind_knowledge_view(
    programs,
    knowledge$view
  )

  source <- fake_source(
    url = "https://example.org/shadow-provenance",
    title = "Shadow provenance evidence",
    content_text = "Photosynthesis converts light to chemical energy."
  )
  workspace$upsert_retrieved_resource(source)
  stage_records <- list()
  record_stage <- function(record, output = NULL) {
    stage_records <<- tempest:::tempest_stage_records_upsert(
      stage_records,
      record
    )
    invisible(record)
  }
  record_stages <- function(records, outputs = NULL) {
    stage_records <<- tempest:::tempest_stage_records_upsert_many(
      stage_records,
      records
    )
    invisible(records)
  }
  extraction_output <- list(
    facts = list(list(
      claim = "Photosynthesis converts light to chemical energy.",
      sources = list(list(
        source_id = source$id,
        url = source$url,
        quote = source$content_text
      )),
      confidence = "high"
    ))
  )
  withCallingHandlers(
    tempest:::tempest_extract_facts_from_answer(
      chat = fake_chat(structured = list(extraction_output)),
      answer_text = extraction_output$facts[[1L]]$claim,
      store = workspace,
      module = programs$extract_claims,
      source_ids = source$id,
      session_id = run_id,
      expert_id = expert_id,
      retrieval_step_id = correlation_id,
      deputy_run_id = deputy$child$run_id,
      deputy_session_id = deputy$child$session_id,
      parent_run_id = deputy$child$parent_run_id,
      delegation_id = deputy$child$delegation_id,
      tool_call_id = deputy$parent_call$tool_call_id,
      knowledge_view = knowledge$view,
      record_stage = record_stage
    ),
    dsprrr_cache_security_warning = function(condition) {
      invokeRestart("muffleWarning")
    }
  )
  verdict <- list(
    status = verifier_status,
    rationale = "The captured source was assessed exactly."
  )
  if (!is.na(verifier_score)) {
    verdict$score <- verifier_score
  }
  verifier <- fake_chat(structured = list(verdict))
  withCallingHandlers(
    tempest:::tempest_verify_claims_internal(
      workspace = workspace,
      verifier = verifier,
      policy = "claim_verified",
      verifier_model = "test::shadow-verifier",
      program = programs$verify_claim_support,
      min_support_score = 0.7,
      record_stage = record_stage,
      record_stages = record_stages
    ),
    dsprrr_cache_security_warning = function(condition) {
      invokeRestart("muffleWarning")
    }
  )
  stage_traces <- tempest:::tempest_persistence_stage_manifest_traces(
    stage_records
  )
  manifest <- tempest_research_manifest_update(
    manifest,
    status = "succeeded",
    traces = c(deputy$traces, stage_traces)
  )
  list(
    run_id = run_id,
    correlation_id = correlation_id,
    expert_id = expert_id,
    knowledge = knowledge,
    workspace = workspace,
    program_set = program_set,
    programs = programs,
    manifest = manifest,
    stage_records = stage_records,
    deputy = deputy,
    source = source
  )
}

test_shadow_rebind_extraction_session <- function(fixture, session_id) {
  records <- fixture$stage_records
  stages <- vapply(records, \(record) record@stage, character(1))
  index <- which(stages == "extract_claims")
  stopifnot(length(index) == 1L)
  data <- tempest:::tempest_stage_record_data(records[[index]])
  data$trace_references$deputy_session_id <- session_id
  rebound <- tempest:::tempest_stage_record_from_data(data)
  claims <- lapply(
    unlist(rebound@output_reference$ids, use.names = FALSE),
    fixture$workspace$get_proposed_claim
  )
  span_ids <- unname(unlist(
    lapply(claims, \(claim) claim@evidence_span_ids),
    use.names = FALSE
  ))
  spans <- lapply(span_ids, fixture$workspace$get_evidence_span)
  data$output_reference$content_digest <-
    tempest:::tempest_stage_claims_output_digest(claims, rebound, spans)
  records[[index]] <- tempest:::tempest_stage_record_from_data(data)
  tempest:::tempest_stage_records_validate(records, allow_running = FALSE)
}
