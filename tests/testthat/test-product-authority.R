test_that("product authority separates execution identity from publication", {
  fixture <- test_promotion_fixture()

  authority <- tempest:::tempest_product_authority_validate(
    fixture$manifest,
    fixture$stage_records,
    fixture$workspace
  )

  expect_identical(authority$binding_scope, "execution_identity")
  expect_identical(authority$mode, "storm")
  expect_identical(authority$completed, TRUE)
  expect_identical(authority$report_reference_bound, TRUE)
  expect_identical(authority$publishable, FALSE)
  expect_error(
    tempest:::tempest_product_authority_validate(
      fixture$manifest,
      fixture$stage_records,
      fixture$workspace,
      require_publishable = TRUE
    ),
    class = "tempest_product_authority_error"
  )

  report_md <- tempest:::tempest_persistence_report_for_records(
    paste0(
      "# Promotion evidence\n\n",
      "The intervention improved the measured outcome [",
      fixture$resource@resource_id,
      "].\n"
    ),
    fixture$stage_records
  )
  publication_manifest <-
    tempest:::tempest_persistence_manifest_bind_report(
      fixture$manifest,
      report_md
    )
  publication <- tempest:::tempest_product_authority_validate(
    publication_manifest,
    fixture$stage_records,
    fixture$workspace,
    report_md = report_md,
    report_reference = tempest:::tempest_persistence_report_reference(
      report_md
    ),
    config = tempest_config(),
    product_state = list(title = "Promotion evidence"),
    require_publishable = TRUE
  )
  expect_identical(publication$publishable, TRUE)

  running_data <- tempest_research_manifest_record(fixture$manifest)
  running_data$status <- "running"
  running_data$deliverables <- list()
  running <- tempest:::tempest_research_manifest_from_record(running_data)
  partial <- tempest:::tempest_product_authority_validate(
    running,
    fixture$stage_records,
    fixture$workspace
  )
  expect_identical(partial$publishable, FALSE)

  running_with_report_data <- tempest_research_manifest_record(
    fixture$manifest
  )
  running_with_report_data$status <- "running"
  running_with_bound_report <-
    tempest:::tempest_research_manifest_from_record(
      running_with_report_data
    )
  expect_error(
    tempest:::tempest_product_authority_validate(
      running_with_bound_report,
      fixture$stage_records,
      fixture$workspace
    ),
    class = "tempest_product_authority_error"
  )
  expect_error(
    tempest:::tempest_product_authority_validate(
      running,
      fixture$stage_records,
      fixture$workspace,
      report_md = "# Forged partial report",
      report_reference = tempest:::tempest_persistence_report_reference(
        "# Forged partial report"
      )
    ),
    class = "tempest_product_authority_error"
  )
  for (status in c("failed", "cancelled")) {
    terminal_data <- tempest_research_manifest_record(fixture$manifest)
    terminal_data$status <- status
    terminal <- tempest:::tempest_research_manifest_from_record(terminal_data)
    expect_error(
      tempest:::tempest_product_authority_validate(
        terminal,
        fixture$stage_records,
        fixture$workspace
      ),
      class = "tempest_product_authority_error",
      info = status
    )
  }
})

test_that("publication authority is non-vacuous", {
  config <- tempest_config()
  report_md <- "# Empty publication"
  manifest <- tempest_research_manifest(
    research_run_id = "authority-empty-publication",
    mode = "storm",
    config = config,
    programs = tempest:::tempest_program_set_manifest_programs(
      tempest_program_set()
    ),
    traces = list(),
    status = "succeeded"
  )
  manifest <- tempest:::tempest_persistence_manifest_bind_stage_records(
    manifest,
    list()
  )
  manifest <- tempest:::tempest_persistence_manifest_bind_report(
    manifest,
    report_md
  )

  expect_error(
    tempest:::tempest_product_authority_validate(
      manifest,
      list(),
      tempest_research_workspace(),
      report_md = report_md,
      report_reference = tempest:::tempest_persistence_report_reference(
        report_md
      ),
      config = config,
      product_state = list(title = "Empty publication"),
      require_publishable = TRUE
    ),
    class = "tempest_product_authority_error"
  )

  cases <- list(
    list(status = "unverifiable", score = NA_real_, suffix = "unverifiable"),
    list(
      status = "partially_supported",
      score = 0.9,
      suffix = "partially-supported"
    )
  )
  for (case in cases) {
    fixture <- test_promotion_fixture()
    workspace <- tempest_research_workspace()
    workspace$upsert_retrieved_resource(fixture$resource)
    claim <- tempest:::tempest_claim(
      claim_text = fixture$claim@claim_text,
      source_ids = fixture$claim@source_ids,
      evidence_span_ids = fixture$claim@evidence_span_ids,
      supporting_quotes = fixture$claim@supporting_quotes,
      claim_type = fixture$claim@claim_type,
      confidence = fixture$claim@confidence,
      claim_id = fixture$claim@claim_id,
      created_at = fixture$claim@created_at
    )
    workspace$add_extracted_claim_batch(list(claim), list(fixture$span))
    support <- tempest_claim_support(
      claim@claim_id,
      fixture$span@evidence_span_id,
      fixture$resource@resource_id,
      case$status,
      case$score,
      "The source cannot verify this claim."
    )
    verified_at <- "2026-08-16T12:03:00Z"
    verifier <- "verifier-promotion-1"
    workspace$verify_proposed_claims_batch(
      list(support),
      verified_at = verified_at,
      min_support_score = 0.7,
      verifier = verifier
    )
    claim <- workspace$get_proposed_claim(claim@claim_id)
    programs <- fixture$programs
    run_id <- paste0("authority-", case$suffix, "-publication")
    manifest <- tempest_research_manifest(
      research_run_id = run_id,
      mode = "storm",
      config = config,
      programs = tempest:::tempest_program_set_manifest_programs(
        test_program_set()
      ),
      traces = list(),
      status = "succeeded"
    )
    deputy_expert_id <- paste0("expert.authority-", case$suffix)
    deputy_context <- tempest:::tempest_deputy_run_context(
      manifest,
      stage = "research",
      role = "expert",
      expert_id = deputy_expert_id
    )
    deputy_trace <- tempest:::tempest_research_manifest_traces(list(list(
      agent_id = tempest:::tempest_deputy_adapter_agent_id(deputy_context),
      correlation_id = paste0("authority-", case$suffix, "-correlation"),
      deputy_run_id = paste0("authority-", case$suffix, "-run"),
      deputy_session_id = tempest:::tempest_storm_deputy_session_id(
        run_id,
        deputy_expert_id
      ),
      expert_id = deputy_expert_id,
      role = "expert",
      stage = "research",
      status = "complete",
      completion_disposition = "issued",
      trace_id = paste0("authority-", case$suffix, "-run"),
      trace_type = "deputy_run"
    )))[[1L]]
    base_trace <- list(
      research_run_id = run_id,
      mode = "storm",
      role = "program"
    )
    trace <- c(
      base_trace,
      deputy_trace[c(
        "deputy_run_id",
        "deputy_session_id",
        "expert_id",
        "correlation_id"
      )]
    )
    extraction <- tempest:::tempest_stage_record_start(
      "extract_claims",
      programs$extract_claims$program_artifact_id,
      programs$extract_claims$governed_procedure_ref$revision_id,
      trace_references = trace,
      attempt_id = paste0("attempt-authority-", case$suffix, "-extraction"),
      started_at = "2026-08-16T12:03:00Z"
    )
    extraction <- tempest:::tempest_stage_record_succeed(
      extraction,
      tempest:::tempest_stage_output_reference(
        "workspace_claims",
        claim@claim_id,
        content_digest = tempest:::tempest_stage_claims_output_digest(
          list(claim),
          extraction,
          list(fixture$span)
        )
      ),
      support_status = "unknown",
      completed_at = "2026-08-16T12:04:00Z"
    )
    verification <- tempest:::tempest_stage_record_start(
      "verify_claim_support",
      programs$verify_claim_support$program_artifact_id,
      programs$verify_claim_support$governed_procedure_ref$revision_id,
      trace_references = c(
        base_trace,
        list(
          min_support_score = tempest:::tempest_stage_support_threshold_string(
            0.7
          ),
          verified_at = verified_at,
          verifier_model = verifier
        )
      ),
      attempt_id = paste0("attempt-authority-", case$suffix, "-verification"),
      started_at = "2026-08-16T12:05:00Z"
    )
    verification <- tempest:::tempest_stage_record_succeed(
      verification,
      tempest:::tempest_stage_output_reference(
        "claim_supports",
        support@claim_support_id,
        content_digest = tempest:::tempest_stage_verification_output_digest(
          support,
          verification,
          claim,
          fixture$span,
          workspace
        )
      ),
      support_status = tempest:::tempest_stage_verification_support_status(
        case$status,
        case$score,
        0.7
      ),
      completed_at = "2026-08-16T12:06:00Z"
    )
    stages <- list(extraction, verification)
    manifest <- tempest:::tempest_persistence_manifest_bind_stage_records(
      manifest,
      stages,
      deputy_traces = list(deputy_trace)
    )
    manifest <- tempest:::tempest_persistence_manifest_bind_report(
      manifest,
      paste("#", case$suffix, "publication")
    )

    expect_error(
      tempest:::tempest_product_authority_validate(
        manifest,
        stages,
        workspace,
        config = config
      ),
      class = "tempest_product_authority_error"
    )
  }
})

test_that("accepted context conditionally requires a real Graft snapshot", {
  config <- tempest_config()
  manifest <- tempest_research_manifest(
    research_run_id = "authority-accepted-context",
    mode = "storm",
    config = config,
    programs = tempest:::tempest_program_set_manifest_programs(
      tempest_program_set()
    ),
    runtime = list(deputy_run_ids = list(), deputy_session_ids = list()),
    traces = list(),
    status = "running"
  )
  exploratory <- tempest_research_workspace()
  expect_no_error(tempest:::tempest_product_authority_validate(
    manifest,
    list(),
    exploratory,
    config = config
  ))

  accepted_without_snapshot <- tempest_research_workspace(
    accepted_graft_references = list(list(record_id = "accepted-record-1"))
  )
  expect_error(
    tempest:::tempest_product_authority_validate(
      manifest,
      list(),
      accepted_without_snapshot,
      config = config
    ),
    class = "tempest_product_authority_error"
  )
})

test_that("Deputy traces reject cross-mode execution identity splices", {
  config <- tempest_config()
  manifest <- tempest_research_manifest(
    research_run_id = "authority-costorm-direct",
    mode = "costorm",
    config = config,
    programs = tempest:::tempest_program_set_manifest_programs(
      tempest_program_set()
    ),
    runtime = list(deputy_run_ids = list(), deputy_session_ids = list()),
    traces = list(),
    status = "running"
  )
  context <- tempest:::tempest_deputy_run_context(
    manifest,
    stage = "dialogue",
    role = "moderator"
  )
  trace <- tempest:::tempest_research_manifest_traces(list(list(
    agent_id = tempest:::tempest_deputy_adapter_agent_id(context),
    correlation_id = "authority-costorm-correlation",
    deputy_run_id = "authority-costorm-run",
    deputy_session_id = tempest:::tempest_costorm_deputy_session_id(
      manifest@research_run_id,
      "moderator"
    ),
    role = "moderator",
    stage = "dialogue",
    status = "complete",
    completion_disposition = "issued",
    trace_id = "authority-costorm-run",
    trace_type = "deputy_run"
  )))[[1L]]
  manifest <- tempest_research_manifest_update(
    manifest,
    runtime = list(
      deputy_run_ids = trace$deputy_run_id,
      deputy_session_ids = trace$deputy_session_id
    ),
    traces = list(trace)
  )
  expect_no_error(tempest:::tempest_product_authority_validate(
    manifest,
    list(),
    tempest_research_workspace(),
    config = config
  ))

  cross_mode <- trace
  cross_mode$expert_id <- "expert.cross-mode"
  cross_mode$role <- "expert"
  cross_mode$stage <- "research"
  cross_mode <- tempest:::tempest_research_manifest_traces(
    list(cross_mode)
  )[[1L]]
  expect_error(
    tempest:::tempest_persistence_deputy_manifest_traces(
      list(cross_mode),
      manifest
    ),
    class = "tempest_stage_record_error"
  )

  partial_delegation <- trace
  partial_delegation$parent_run_id <- "authority-parent-run"
  partial_delegation$trace_type <- "deputy_delegation"
  partial_delegation <- tempest:::tempest_research_manifest_traces(
    list(partial_delegation)
  )[[1L]]
  expect_error(
    tempest:::tempest_persistence_deputy_manifest_traces(
      list(partial_delegation),
      manifest
    ),
    class = "tempest_stage_record_error"
  )
})

test_that("STORM Deputy extraction rejects terminal tuple tampering", {
  fixture <- test_promotion_fixture()
  expert <- test_expert(expert_id = "expert.authority-storm")
  run_id <- "authority-storm-direct"
  base <- tempest_research_manifest(
    research_run_id = run_id,
    mode = "storm",
    config = tempest_config(),
    programs = fixture$manifest@programs,
    runtime = list(deputy_run_ids = list(), deputy_session_ids = list()),
    traces = list(),
    status = "succeeded"
  )
  context <- tempest:::tempest_deputy_run_context(
    base,
    stage = "research",
    role = "expert",
    expert_id = expert@expert_id
  )
  trace <- tempest:::tempest_research_manifest_traces(list(list(
    agent_id = tempest:::tempest_deputy_adapter_agent_id(context),
    correlation_id = "authority-storm-correlation",
    deputy_run_id = "authority-storm-run",
    deputy_session_id = tempest:::tempest_storm_deputy_session_id(
      run_id,
      expert@expert_id
    ),
    expert_id = expert@expert_id,
    role = "expert",
    stage = "research",
    status = "complete",
    completion_disposition = "issued",
    trace_id = "authority-storm-run",
    trace_type = "deputy_run"
  )))[[1L]]

  stage_records <- fixture$stage_records
  extraction <- tempest:::tempest_stage_record_data(stage_records[[1L]])
  extraction$trace_references$research_run_id <- run_id
  extraction$trace_references$deputy_run_id <- trace$deputy_run_id
  extraction$trace_references$deputy_session_id <- trace$deputy_session_id
  extraction$trace_references$expert_id <- trace$expert_id
  extraction$trace_references$correlation_id <- trace$correlation_id
  extraction$trace_references <- tempest:::tempest_stage_trace_references(
    extraction$trace_references,
    extraction$attempt_id
  )
  extraction <- tempest:::tempest_stage_record_from_data(extraction)
  claims <- lapply(
    unlist(extraction@output_reference$ids, use.names = FALSE),
    fixture$workspace$get_proposed_claim
  )
  spans <- lapply(
    unlist(lapply(claims, \(claim) claim@evidence_span_ids), use.names = FALSE),
    fixture$workspace$get_evidence_span
  )
  extraction@output_reference$content_digest <-
    tempest:::tempest_stage_claims_output_digest(claims, extraction, spans)
  verification <- tempest:::tempest_stage_record_data(stage_records[[2L]])
  verification$trace_references$research_run_id <- run_id
  verification$trace_references <- tempest:::tempest_stage_trace_references(
    verification$trace_references,
    verification$attempt_id
  )
  verification <- tempest:::tempest_stage_record_from_data(verification)
  support <- fixture$workspace$get_claim_support(
    verification@output_reference$ids[[1L]]
  )
  claim <- fixture$workspace$get_proposed_claim(support@claim_id)
  span <- fixture$workspace$get_evidence_span(support@evidence_span_id)
  verification@output_reference$content_digest <-
    tempest:::tempest_stage_verification_output_digest(
      support,
      verification,
      claim,
      span,
      fixture$workspace
    )
  stage_records <- list(extraction, verification)

  bind <- function(candidate_trace, records = stage_records) {
    manifest <- tempest_research_manifest_update(
      base,
      runtime = list(
        deputy_run_ids = candidate_trace$deputy_run_id,
        deputy_session_ids = candidate_trace$deputy_session_id
      ),
      traces = list(candidate_trace)
    )
    tempest:::tempest_persistence_manifest_bind_stage_records(
      manifest,
      records,
      deputy_traces = list(candidate_trace),
      expert_ids = expert@expert_id
    )
  }
  expect_no_error(bind(trace))

  unbound_data <- tempest:::tempest_stage_record_data(extraction)
  unbound_data$trace_references[c(
    "deputy_run_id",
    "deputy_session_id",
    "expert_id",
    "correlation_id"
  )] <- NULL
  unbound_data$trace_references <- tempest:::tempest_stage_trace_references(
    unbound_data$trace_references,
    unbound_data$attempt_id
  )
  unbound_extraction <- tempest:::tempest_stage_record_from_data(unbound_data)
  unbound_extraction@output_reference$content_digest <-
    tempest:::tempest_stage_claims_output_digest(
      claims,
      unbound_extraction,
      spans
    )
  expect_error(
    bind(trace, list(unbound_extraction, verification)),
    class = "tempest_stage_record_error"
  )

  changes <- list(
    function(value) {
      value$deputy_session_id <- "authority-spliced-session"
      value
    },
    function(value) {
      value$agent_id <- "authority-spliced-agent"
      value
    },
    function(value) {
      value$expert_id <- "expert.spliced"
      value
    },
    function(value) {
      value$correlation_id <- "authority-spliced-correlation"
      value
    },
    function(value) {
      value$status <- "error"
      value$completion_disposition <- "terminal"
      value
    },
    function(value) {
      value$completion_disposition <- "discarded"
      value
    },
    function(value) {
      value$stage <- "dialogue"
      value
    }
  )
  for (change in changes) {
    tampered <- tempest:::tempest_research_manifest_traces(
      list(change(trace))
    )[[1L]]
    expect_error(bind(tampered), class = "tempest_stage_record_error")
  }
})

test_that("STORM finalization preserves discarded Deputy attempt history", {
  fixture <- test_promotion_fixture()
  expert <- test_expert(expert_id = "expert.authority-recovery")
  run_id <- "authority-storm-recovery"
  running <- tempest_research_manifest(
    research_run_id = run_id,
    mode = "storm",
    config = tempest_config(),
    programs = fixture$manifest@programs,
    traces = list(),
    status = "running"
  )
  context <- tempest:::tempest_deputy_run_context(
    running,
    stage = "research",
    role = "expert",
    expert_id = expert@expert_id
  )
  deputy_trace <- tempest:::tempest_research_manifest_traces(list(list(
    agent_id = tempest:::tempest_deputy_adapter_agent_id(context),
    correlation_id = "authority-recovery-correlation",
    deputy_run_id = "authority-recovery-run",
    deputy_session_id = tempest:::tempest_storm_deputy_session_id(
      run_id,
      expert@expert_id
    ),
    expert_id = expert@expert_id,
    role = "expert",
    stage = "research",
    status = "complete",
    completion_disposition = "discarded",
    trace_id = "authority-recovery-run",
    trace_type = "deputy_run"
  )))[[1L]]
  running <- tempest_research_manifest_update(
    running,
    runtime = list(
      deputy_run_ids = deputy_trace$deputy_run_id,
      deputy_session_ids = deputy_trace$deputy_session_id
    ),
    traces = list(deputy_trace)
  )
  prior_stage <- list(list(
    trace_id = "authority-prior-stage",
    stage = "draft_perspectives",
    status = "succeeded",
    trace_type = "stage_attempt"
  ))

  validate <- function(manifest) {
    tempest:::tempest_persistence_manifest_validate_trace_ids(
      prior_stage,
      list(deputy_trace),
      manifest
    )
  }
  expect_no_error(validate(running))
  succeeded <- tempest_research_manifest_update(running, status = "succeeded")
  expect_no_error(validate(succeeded))
  expect_identical(succeeded@traces, running@traces)

  bound_stage <- list(
    trace_id = "authority-bound-stage-one",
    deputy_run_id = deputy_trace$deputy_run_id,
    deputy_session_id = deputy_trace$deputy_session_id,
    expert_id = deputy_trace$expert_id,
    correlation_id = deputy_trace$correlation_id,
    stage = "extract_claims",
    status = "succeeded",
    trace_type = "stage_attempt"
  )
  duplicate_binding <- bound_stage
  duplicate_binding$trace_id <- "authority-bound-stage-two"
  expect_error(
    tempest:::tempest_persistence_manifest_validate_trace_ids(
      list(bound_stage),
      list(deputy_trace),
      succeeded
    ),
    class = "tempest_stage_record_error"
  )
  expect_error(
    tempest:::tempest_persistence_manifest_validate_trace_ids(
      list(bound_stage, duplicate_binding),
      list(deputy_trace),
      succeeded
    ),
    class = "tempest_stage_record_error"
  )
})
