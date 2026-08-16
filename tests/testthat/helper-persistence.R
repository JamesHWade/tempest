test_persistence_storm_stage_records <- function(state, workspace, manifest) {
  records <- list()
  sequence <- 0L
  start <- function(stage, trace = list()) {
    sequence <<- sequence + 1L
    reference <- manifest@programs[[stage]]
    tempest:::tempest_stage_record_start(
      stage,
      reference$program_artifact_id,
      reference$governed_procedure_revision_id,
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
    running <- start("personas")
    succeed(running, state_reference(running, state$experts, "experts"))
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
  verified_ids <- if (is.null(workspace$citation_audit)) {
    character()
  } else {
    workspace$citation_audit$claim_id
  }
  if ("write" %in% state$completed_stages) {
    for (claim_id in verified_ids) {
      running <- start("verify_claim_support")
      row <- workspace$citation_audit[
        workspace$citation_audit$claim_id == claim_id,
        ,
        drop = FALSE
      ]
      succeed(
        running,
        tempest:::tempest_stage_output_reference(
          "citation_audit",
          claim_id,
          content_digest = tempest:::tempest_stage_verification_output_digest(
            row,
            running,
            workspace$get_proposed_claim(claim_id),
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

test_persistence_bind_storm_records <- function(state, workspace, manifest) {
  state$stage_records <- test_persistence_storm_stage_records(
    state,
    workspace,
    manifest
  )
  if (!is.null(state$report_md)) {
    state$report_md <- tempest:::tempest_persistence_report_for_records(
      state$report_md,
      state$stage_records
    )
  }
  tempest:::tempest_storm_state_validate(state)
}

test_persistence_complete_storm_product <- function(
  topic,
  run_id,
  config,
  program_set,
  manifest_status = "succeeded"
) {
  programs <- tempest:::tempest_program_set_manifest_programs(program_set)
  workspace <- tempest_research_workspace()
  source <- tempest:::tempest_source(
    paste0("https://example.com/", run_id),
    title = paste(topic, "source"),
    content_text = "Durable evidence supports the completed research product."
  )
  workspace$upsert_retrieved_resource(source)
  span_id <- workspace$add_evidence_span(tempest_evidence_span(
    evidence_span_id = paste0("span.", run_id),
    source_id = source$id,
    quote = "Durable evidence supports the completed research product.",
    extracted_by = programs$extract_claims$program_artifact_id
  ))
  claim_id <- workspace$add_proposed_claim(tempest_claim(
    claim_id = paste0("claim.", run_id),
    claim_text = "Durable evidence supports the completed research product.",
    source_ids = source$id,
    evidence_span_ids = span_id,
    supporting_quotes = list(
      "Durable evidence supports the completed research product."
    ),
    verification_status = "supported",
    support_score = 0.9
  ))
  workspace$set_citation_audit(tibble::tibble(
    claim_id = claim_id,
    claim_text = "Durable evidence supports the completed research product.",
    verification_status = "supported",
    support_score = 0.9,
    rationale = "The exact durable excerpt supports the claim."
  ))
  outline <- list(
    title = topic,
    sections = list(list(
      title = "Findings",
      summary = "Durable findings",
      subsections = list()
    ))
  )
  report_md <- tempest_report_md(
    title = topic,
    body = paste0(
      "Durable evidence supports the completed research product. [",
      source$id,
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
    experts = list(tempest_expert(
      expert_id = paste0("expert.", run_id),
      name = "Durable Product Expert",
      title = "Research integrity analyst",
      description = "Checks complete durable research products.",
      instructions = "Require exact evidence and execution history."
    )),
    draft_outline = outline,
    outline = outline,
    lead_section = "Durable evidence supports the completed research product.",
    draft_md = paste0(
      "Durable evidence supports the completed research product.\n\n",
      "## Findings\n\n",
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
  state <- test_persistence_bind_storm_records(state, workspace, manifest)
  list(
    workspace = workspace,
    state = state,
    manifest = manifest,
    source = source,
    claim_id = claim_id,
    span_id = span_id
  )
}
