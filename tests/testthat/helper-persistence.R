test_persistence_storm_stage_records <- function(
  state,
  workspace,
  manifest,
  min_support_score = 0.7,
  deputy_trace = NULL,
  personas_generated = FALSE
) {
  records <- list()
  sequence <- 0L
  start <- function(stage, trace = list()) {
    sequence <<- sequence + 1L
    reference <- manifest@programs[[stage]]
    governed_reference <- reference$governed_procedure_ref %||% NULL
    if (!is.null(governed_reference)) {
      trace$governed_procedure <-
        tempest:::tempest_governed_procedure_trace_binding(
          governed_reference
        )
    }
    snapshot_id <- manifest@knowledge_snapshot$snapshot_id %||% NULL
    if (!is.null(snapshot_id)) {
      trace$knowledge_snapshot_id <- snapshot_id
    }
    tempest:::tempest_stage_record_start(
      stage,
      reference$program_artifact_id,
      governed_reference$revision_id %||% NULL,
      trace_references = c(
        list(
          research_run_id = manifest@research_run_id,
          mode = manifest@mode,
          role = "program"
        ),
        trace
      ),
      attempt_id = sprintf("attempt-persistence-%02d-%s", sequence, stage),
      started_at = sprintf("2026-08-16T00:%02d:00Z", sequence)
    )
  }
  succeed <- function(running, reference, support_status = "unknown") {
    record <- tempest:::tempest_stage_record_succeed(
      running,
      reference,
      support_status = support_status,
      completed_at = sprintf("2026-08-16T00:%02d:30Z", sequence)
    )
    records <<- c(records, list(record))
    invisible(record)
  }
  state_reference <- function(running, output, ids) {
    tempest:::tempest_stage_output_reference(
      "state_field",
      ids,
      content_digest = tempest:::tempest_stage_state_output_digest(
        running@stage,
        output
      )
    )
  }
  content_reference <- function(output) {
    digest <- tempest:::tempest_stage_content_digest_id(output)
    tempest:::tempest_stage_output_reference(
      "content_digest",
      digest,
      content_digest = digest
    )
  }

  if ("perspectives" %in% state$completed_stages) {
    running <- start("perspectives")
    output <- list(title = state$title, perspectives = state$perspectives)
    succeed(
      running,
      state_reference(
        running,
        output,
        c("title", "perspectives")
      )
    )
    if (isTRUE(personas_generated)) {
      running <- start("personas")
      succeed(running, state_reference(running, state$experts, "experts"))
    }
  }
  if ("research" %in% state$completed_stages) {
    running <- start("query_decomposition")
    succeed(running, content_reference(list(queries = state$topic)))
    claims <- workspace$list_proposed_claims()
    claim_ids <- vapply(claims, \(claim) claim@claim_id, character(1))
    span_ids <- unname(unlist(
      lapply(claims, \(claim) claim@evidence_span_ids),
      use.names = FALSE
    ))
    spans <- lapply(span_ids, workspace$get_evidence_span)
    trace <- list()
    for (field in c("expert_id", "retrieval_step_id")) {
      values <- unique(vapply(
        claims,
        function(claim) {
          value <- S7::prop(claim, field)
          if (is.na(value)) "" else value
        },
        character(1)
      ))
      values <- values[nzchar(values)]
      if (length(values) == 1L) {
        trace[[
          if (identical(field, "retrieval_step_id")) {
            "correlation_id"
          } else {
            field
          }
        ]] <- values[[1]]
      }
    }
    if (length(claim_ids) > 0L) {
      stopifnot(!is.null(deputy_trace))
      trace$deputy_run_id <- deputy_trace$deputy_run_id
      trace$deputy_session_id <- deputy_trace$deputy_session_id
      trace$expert_id <- if (identical(deputy_trace$role, "moderator")) {
        "moderator"
      } else {
        deputy_trace$expert_id
      }
      trace$correlation_id <- deputy_trace$correlation_id
    }
    running <- start("extract_claims", trace)
    succeed(
      running,
      tempest:::tempest_stage_output_reference(
        "workspace_claims",
        claim_ids,
        content_digest = tempest:::tempest_stage_claims_output_digest(
          claims,
          running,
          spans
        )
      )
    )
  }
  claim_supports <- workspace$list_claim_supports()
  verified_ids <- sort(unique(vapply(
    claim_supports,
    \(support) support@claim_id,
    character(1)
  )))
  if (length(claim_supports) > 0L) {
    for (support in claim_supports) {
      claim <- workspace$get_proposed_claim(support@claim_id)
      verification_trace <- list(
        min_support_score = tempest:::tempest_stage_support_threshold_string(
          min_support_score
        ),
        verified_at = claim@verified_at
      )
      if (!is.na(claim@verifier_model)) {
        verification_trace$verifier_model <- claim@verifier_model
      }
      running <- start(
        "verify_claim_support",
        verification_trace
      )
      evidence_span <- workspace$get_evidence_span(
        support@evidence_span_id
      )
      succeed(
        running,
        tempest:::tempest_stage_output_reference(
          "claim_supports",
          support@claim_support_id,
          content_digest = tempest:::tempest_stage_verification_output_digest(
            support,
            running,
            claim,
            evidence_span,
            workspace
          )
        ),
        support_status = "verified"
      )
    }
  }
  if ("outline" %in% state$completed_stages) {
    running <- start("draft_outline")
    succeed(
      running,
      state_reference(
        running,
        state$draft_outline,
        "draft_outline"
      )
    )
    running <- start("refined_outline")
    succeed(running, state_reference(running, state$outline, "outline"))
  }
  if ("write" %in% state$completed_stages) {
    evidence_trace <- list(
      evidence_claim_ids = as.list(verified_ids),
      verified_evidence_claim_ids = as.list(verified_ids)
    )
    running <- start("lead_section", evidence_trace)
    succeed(
      running,
      content_reference(state$lead_section),
      support_status = "verified"
    )
    sections <- tempest:::tempest_storm_draft_section_texts(state)
    for (section in sections) {
      running <- start("section_writing", evidence_trace)
      succeed(
        running,
        content_reference(section),
        support_status = "verified"
      )
    }
  }
  records
}

test_persistence_storm_deputy_trace <- function(state, workspace, manifest) {
  claims <- workspace$list_proposed_claims()
  if (
    !"research" %in% state$completed_stages ||
      length(claims) == 0L
  ) {
    return(list(state = state, trace = NULL))
  }
  claim_expert_ids <- unique(vapply(
    claims,
    function(claim) {
      value <- claim@expert_id
      if (is.na(value)) "" else value
    },
    character(1)
  ))
  claim_expert_ids <- claim_expert_ids[nzchar(claim_expert_ids)]
  stopifnot(length(claim_expert_ids) <= 1L)
  expert_ids <- vapply(
    state$experts,
    \(expert) expert@expert_id,
    character(1)
  )
  if (length(expert_ids) == 0L) {
    expert <- tempest_expert(
      name = "Persistence Fixture Expert",
      title = "Research persistence analyst",
      description = "Binds durable fixture evidence to one execution.",
      instructions = "Preserve exact execution identity."
    )
    state$experts <- list(expert)
    expert_ids <- expert@expert_id
  }
  expert_id <- if (length(claim_expert_ids) == 1L) {
    claim_expert_ids[[1L]]
  } else {
    expert_ids[[1L]]
  }
  stopifnot(expert_id %in% expert_ids)
  correlations <- unique(vapply(
    claims,
    function(claim) {
      value <- claim@retrieval_step_id
      if (is.na(value)) "" else value
    },
    character(1)
  ))
  correlations <- correlations[nzchar(correlations)]
  stopifnot(length(correlations) <= 1L)
  correlation_id <- if (length(correlations) == 1L) {
    correlations[[1L]]
  } else {
    paste0("correlation.persistence-", manifest@research_run_id)
  }
  deputy_context <- tempest:::tempest_deputy_run_context(
    manifest,
    stage = "research",
    role = "expert",
    expert_id = expert_id
  )
  deputy_run_id <- paste0("deputy.persistence-", manifest@research_run_id)
  trace <- tempest:::tempest_research_manifest_traces(list(list(
    agent_id = tempest:::tempest_deputy_adapter_agent_id(deputy_context),
    correlation_id = correlation_id,
    deputy_run_id = deputy_run_id,
    deputy_session_id = tempest:::tempest_storm_deputy_session_id(
      manifest@research_run_id,
      expert_id
    ),
    expert_id = expert_id,
    role = "expert",
    stage = "research",
    status = "complete",
    completion_disposition = "issued",
    trace_id = deputy_run_id,
    trace_type = "deputy_run"
  )))[[1L]]
  list(state = state, trace = trace)
}

test_persistence_add_costorm_evidence <- function(
  session,
  key,
  claim_text = "Durable session evidence supports this report."
) {
  programs <- tempest:::tempest_program_set_manifest_programs(
    tempest:::tempest_session_program_set(session)
  )
  fixture <- test_add_verifiable_claim(
    tempest:::tempest_session_workspace(session),
    key = key,
    claim_text = claim_text,
    quote = claim_text,
    extracted_by = programs$extract_claims$program_artifact_id
  )
  tempest:::tempest_session_workspace(session)$verify_proposed_claims_batch(
    list(test_claim_support(fixture$claim, fixture$span)),
    verified_at = "2026-08-16T00:00:00Z",
    verifier = programs$verify_claim_support$program_artifact_id,
    .verification_owner_token = tempest:::tempest_session_verification_owner_token(
      session
    )
  )
  fixture
}

test_persistence_commit_costorm_report <- function(session, report_md) {
  correlation_id <- paste0(
    "correlation.persistence-",
    session$session_id
  )
  deputy_context <- tempest:::tempest_deputy_run_context(
    tempest:::tempest_session_manifest(session),
    stage = "dialogue",
    role = "moderator"
  )
  deputy_run_id <- paste0("deputy.persistence-", session$session_id)
  deputy_trace <- tempest:::tempest_research_manifest_traces(list(list(
    agent_id = tempest:::tempest_deputy_adapter_agent_id(deputy_context),
    correlation_id = correlation_id,
    deputy_run_id = deputy_run_id,
    deputy_session_id = tempest:::tempest_costorm_deputy_session_id(
      session$session_id,
      "moderator"
    ),
    role = "moderator",
    stage = "dialogue",
    status = "complete",
    completion_disposition = "issued",
    trace_id = deputy_run_id,
    trace_type = "deputy_run"
  )))[[1L]]
  state <- tempest:::tempest_storm_state(
    session$topic,
    completed_stages = "research"
  )
  records <- test_persistence_storm_stage_records(
    state,
    tempest:::tempest_session_workspace(session),
    tempest:::tempest_session_manifest(session),
    min_support_score = tempest:::tempest_session_config(
      session
    )@min_support_score,
    deputy_trace = deputy_trace
  )
  tempest:::tempest_session_record_deputy_trace(session, deputy_trace)
  tempest:::tempest_session_set_stage_records(session, records)
  test_persistence_commit_existing_costorm_report(session, report_md)
}

test_persistence_commit_existing_costorm_report <- function(
  session,
  report_md
) {
  records <- tempest:::tempest_session_stage_records(session)
  report_md <- tempest:::tempest_product_report_for_stage_records(
    report_md,
    records,
    trusted_title = tempest:::tempest_session_title(session)
  )
  manifest <- tempest:::tempest_product_authority_finalize_manifest(
    manifest = tempest:::tempest_session_manifest(session),
    stage_records = records,
    workspace = tempest:::tempest_session_workspace(session),
    deputy_traces = tempest:::tempest_session_deputy_traces(session),
    report_md = report_md,
    config = tempest:::tempest_session_config(session),
    experts = session$experts,
    expert_sessions = tempest:::tempest_expert_sessions_snapshot(session),
    product_state = list(title = tempest:::tempest_session_title(session)),
    status = "succeeded",
    require_publishable = TRUE
  )
  tempest:::tempest_session_commit_terminal_report(
    session,
    manifest,
    report_md
  )
  report_md
}

test_persistence_bind_storm_records <- function(
  state,
  workspace,
  manifest,
  min_support_score = 0.7,
  personas_generated = FALSE
) {
  deputy <- test_persistence_storm_deputy_trace(
    state,
    workspace,
    manifest
  )
  state <- deputy$state
  state$stage_records <- test_persistence_storm_stage_records(
    state,
    workspace,
    manifest,
    min_support_score = min_support_score,
    deputy_trace = deputy$trace,
    personas_generated = personas_generated
  )
  if (!is.null(state$report_md)) {
    state$report_md <- tempest:::tempest_product_report_for_stage_records(
      state$report_md,
      state$stage_records
    )
  }
  state <- tempest:::tempest_storm_state_validate(state)
  expert_ids <- vapply(
    state$experts,
    \(expert) expert@expert_id,
    character(1)
  )
  manifest <- tempest:::tempest_product_authority_bind_stage_records(
    manifest,
    state$stage_records,
    deputy_traces = if (is.null(deputy$trace)) list() else list(deputy$trace),
    expert_ids = expert_ids
  )
  list(state = state, manifest = manifest)
}

test_persistence_complete_storm_product <- function(
  topic,
  run_id,
  config,
  program_set,
  manifest_status = "succeeded",
  extra_sources = list()
) {
  programs <- tempest:::tempest_program_set_manifest_programs(program_set)
  workspace <- tempest_research_workspace()
  expert <- tempest_expert(
    name = "Durable Product Expert",
    title = "Research integrity analyst",
    description = "Checks complete durable research products.",
    instructions = "Require exact evidence and execution history."
  )
  correlation_id <- paste0("correlation.persistence-", run_id)
  source <- fake_source(
    url = paste0("https://example.com/", run_id),
    title = paste(topic, "source"),
    content_text = "Durable evidence supports the completed research product."
  )
  workspace$upsert_retrieved_resource(source)
  for (extra_source in extra_sources) {
    workspace$upsert_retrieved_resource(extra_source)
  }
  span_id <- workspace$add_evidence_span(tempest_evidence_span(
    evidence_span_id = paste0("span.", run_id),
    source_id = source@resource_id,
    quote = "Durable evidence supports the completed research product.",
    extracted_by = programs$extract_claims$program_artifact_id
  ))
  claim_id <- workspace$add_proposed_claim(tempest_claim(
    claim_id = paste0("claim.", run_id),
    claim_text = "Durable evidence supports the completed research product.",
    source_ids = source@resource_id,
    evidence_span_ids = span_id,
    supporting_quotes = list(
      "Durable evidence supports the completed research product."
    ),
    retrieval_step_id = correlation_id,
    expert_id = expert@expert_id,
    session_id = run_id
  ))
  support_score <- max(config@min_support_score, 0.9)
  workspace$verify_proposed_claims_batch(
    list(tempest_claim_support(
      claim_id = claim_id,
      evidence_span_id = span_id,
      source_id = source@resource_id,
      verification_status = "supported",
      support_score = support_score,
      rationale = "The exact durable excerpt supports the claim."
    )),
    verified_at = "2026-08-16T00:00:00Z",
    min_support_score = config@min_support_score,
    verifier = programs$verify_claim_support$program_artifact_id
  )
  outline <- list(
    title = topic,
    sections = list(list(
      title = "Findings",
      summary = "Durable findings",
      subsections = list()
    ))
  )
  report_md <- tempest:::tempest_report_md_render(
    title = topic,
    body = paste0(
      "Durable evidence supports the completed research product. [",
      source@resource_id,
      "]"
    ),
    workspace = workspace,
    citation_policy = config@citation_policy,
    on_unsupported_claim = config@on_unsupported_claim,
    min_support_score = config@min_support_score
  )
  state <- tempest:::tempest_storm_state(
    topic,
    perspectives = list(list(
      name = "Evidence",
      description = "Durable evidence",
      key_questions = "What supports the completed product?"
    )),
    experts = list(expert),
    draft_outline = outline,
    outline = outline,
    lead_section = "Durable evidence supports the completed research product.",
    draft_md = paste0(
      "Durable evidence supports the completed research product.\n\n",
      tempest:::tempest_section_markdown_heading("Findings"),
      "\n\n",
      "Durable evidence supports the completed research product."
    ),
    report_md = report_md,
    completed_stages = c(
      "perspectives",
      "research",
      "outline",
      "write",
      "polish"
    )
  )
  manifest <- tempest_research_manifest(
    run_id,
    mode = "storm",
    config = config,
    programs = programs,
    status = manifest_status
  )
  bound <- test_persistence_bind_storm_records(
    state,
    workspace,
    manifest,
    min_support_score = config@min_support_score
  )
  state <- bound$state
  manifest <- bound$manifest
  list(
    workspace = workspace,
    state = state,
    manifest = manifest,
    source = source,
    claim_id = claim_id,
    span_id = span_id
  )
}
