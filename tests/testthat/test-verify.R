test_that("verification produces one explicit support per claim-span pair", {
  workspace <- tempest_research_workspace()
  first <- test_add_verifiable_claim(workspace, "1")
  second <- test_add_verifiable_claim(workspace, "2")
  judge <- fake_chat(
    structured = list(
      list(status = "supported", score = 0.92, rationale = "Direct support."),
      list(status = "unsupported", score = 0.1, rationale = "No support.")
    )
  )

  audit <- tempest_verify_claims(workspace, verifier = judge)

  expect_s3_class(audit, "tbl_df")
  expect_named(
    audit,
    c(
      "claim_support_id",
      "claim_id",
      "evidence_span_id",
      "source_id",
      "verification_status",
      "support_score",
      "rationale"
    )
  )
  expect_identical(
    audit$claim_id,
    c(first$claim@claim_id, second$claim@claim_id)
  )
  expect_identical(
    audit$evidence_span_id,
    c(first$span@evidence_span_id, second$span@evidence_span_id)
  )
  expect_identical(
    audit$verification_status,
    c("supported", "unsupported")
  )
  expect_length(workspace$list_claim_supports(), 2L)
  expect_length(judge$.calls(), 2L)
})

test_that("verification assesses every exact span and derives claim summary", {
  workspace <- tempest_research_workspace()
  source <- tempest_resource(
    resource_kind = "web.page",
    locator = "https://example.org/multi-span",
    title = "Two exact spans",
    media_type = "text/plain",
    content = "First exact span. Second exact span.",
    resource_id = "source.multi-span",
    retrieved_at = "2026-08-16T12:00:00Z"
  )
  workspace$upsert_retrieved_resource(source)
  spans <- list(
    tempest_evidence_span(
      source_id = source@resource_id,
      quote = "First exact span.",
      evidence_span_id = "span.multi.1"
    ),
    tempest_evidence_span(
      source_id = source@resource_id,
      quote = "Second exact span.",
      evidence_span_id = "span.multi.2"
    )
  )
  claim <- tempest_claim(
    "A claim with two spans",
    source_ids = source@resource_id,
    evidence_span_ids = vapply(spans, \(span) span@evidence_span_id, ""),
    supporting_quotes = lapply(spans, \(span) span@quote),
    claim_id = "claim.multi"
  )
  workspace$add_extracted_claim_batch(list(claim), spans)
  judge <- fake_chat(
    structured = list(
      list(status = "supported", score = 0.9, rationale = "Supports."),
      list(
        status = "partially_supported",
        score = 0.6,
        rationale = "Supports only part."
      )
    )
  )

  audit <- tempest_verify_claims(workspace, verifier = judge)

  expect_identical(nrow(audit), 2L)
  expect_identical(
    workspace$get_proposed_claim(claim@claim_id)@verification_status,
    "partially_supported"
  )
  expect_identical(
    workspace$get_proposed_claim(claim@claim_id)@support_score,
    0.6
  )
})

test_that("verification normalizes supported scores below the exact threshold", {
  workspace <- tempest_research_workspace()
  fixture <- test_add_verifiable_claim(workspace)
  judge <- fake_chat(
    structured = list(list(
      status = "supported",
      score = 0.6,
      rationale = "Weak match."
    ))
  )

  audit <- tempest_verify_claims(
    workspace,
    verifier = judge,
    min_support_score = 0.7
  )

  expect_identical(audit$verification_status, "unsupported")
  expect_identical(audit$support_score, 0.6)
  expect_identical(
    workspace$get_proposed_claim(fixture$claim@claim_id)@verification_status,
    "unsupported"
  )
})

test_that("unverifiable pair support carries no numeric score", {
  workspace <- tempest_research_workspace()
  test_add_verifiable_claim(workspace)
  judge <- fake_chat(
    structured = list(list(
      status = "unverifiable",
      rationale = "The span is not interpretable."
    ))
  )

  audit <- tempest_verify_claims(workspace, verifier = judge)

  expect_identical(audit$verification_status, "unverifiable")
  expect_identical(audit$support_score, NA_real_)
})

test_that("non-verifying policies do not mutate authoritative support state", {
  verified <- test_verified_workspace()
  before <- tempest_claim_supports(verified$workspace)

  audit <- tempest_verify_claims(
    verified$workspace,
    verifier = fake_chat(),
    policy = "source_attributed"
  )

  expect_identical(nrow(audit), 0L)
  expect_identical(tempest_claim_supports(verified$workspace), before)
})

test_that("malformed pair output cannot partially commit verification", {
  workspace <- tempest_research_workspace()
  test_add_verifiable_claim(workspace, "1")
  test_add_verifiable_claim(workspace, "2")
  judge <- fake_chat(
    structured = list(
      list(status = "supported", score = 0.9, rationale = "Direct support."),
      list(status = "supported", score = 1.5, rationale = "Invalid score.")
    )
  )

  expect_error(
    tempest_verify_claims(workspace, verifier = judge),
    class = "tempest_stage_output_validation_error"
  )
  expect_length(workspace$list_claim_supports(), 0L)
  expect_identical(
    vapply(
      workspace$list_proposed_claims(),
      \(claim) claim@verification_status,
      ""
    ),
    rep("unverified", 2L)
  )
  expect_null(workspace$citation_audit)
})

test_that("verification validates the complete evidence set before providers", {
  workspace <- tempest_research_workspace()
  source <- tempest_resource(
    resource_kind = "web.page",
    locator = "https://example.org/no-span",
    title = "No span",
    media_type = "text/plain",
    content = "Captured content.",
    resource_id = "source.no-span"
  )
  workspace$upsert_retrieved_resource(source)
  workspace$add_proposed_claim(tempest_claim(
    "Claim without a span",
    source_ids = source@resource_id,
    claim_id = "claim.no-span"
  ))
  judge <- fake_chat(
    structured = list(list(
      status = "supported",
      score = 0.9,
      rationale = "Must not run."
    ))
  )

  expect_error(
    tempest_verify_claims(workspace, verifier = judge),
    class = "tempest_stage_governance_error"
  )
  expect_length(judge$.calls(), 0L)
  expect_length(workspace$list_claim_supports(), 0L)
})

test_that("verification rejects unsafe metadata before provider use", {
  workspace <- tempest_research_workspace()
  test_add_verifiable_claim(workspace)
  judge <- fake_chat()

  expect_error(
    tempest_verify_claims(
      workspace,
      verifier = judge,
      verifier_model = "sk-live-secret"
    ),
    class = "tempest_stage_governance_error"
  )
  expect_length(judge$.calls(), 0L)
})

test_that("verification output digest binds the exact span and source content", {
  workspace <- tempest_research_workspace()
  fixture <- test_add_verifiable_claim(workspace)
  support <- test_claim_support(fixture$claim, fixture$span)
  record <- tempest:::tempest_stage_record_start(
    "verify_claim_support",
    paste0("sha256:", strrep("a", 64L)),
    trace_references = list(
      min_support_score = "0.7",
      verified_at = "2026-08-16T12:03:00Z"
    ),
    attempt_id = "attempt-source-digest"
  )
  before <- tempest:::tempest_stage_verification_output_digest(
    support,
    record,
    fixture$claim,
    fixture$span,
    workspace
  )
  changed <- tempest_resource(
    resource_kind = fixture$source@resource_kind,
    locator = fixture$source@locator,
    title = fixture$source@title,
    media_type = fixture$source@media_type,
    resource_id = fixture$source@resource_id,
    content = "Evidence for claim 1 with changed captured content.",
    retrieved_at = fixture$source@retrieved_at
  )
  workspace$upsert_retrieved_resource(changed)
  after <- tempest:::tempest_stage_verification_output_digest(
    support,
    record,
    workspace$get_proposed_claim(fixture$claim@claim_id),
    fixture$span,
    workspace
  )

  expect_identical(identical(before, after), FALSE)
})

test_that("pair verification context carries the exact transient view", {
  workspace <- tempest_research_workspace()
  fixture <- test_add_verifiable_claim(workspace)
  view <- new.env(parent = emptyenv())
  module <- structure(
    list(knowledge_view = view),
    class = c("tempest_dsprrr_execution", "list")
  )
  seen <- NULL
  local_mocked_bindings(
    tempest_execute_stage = function(
      module,
      verifier,
      inputs,
      context,
      ...
    ) {
      seen <<- context
      list(output = test_claim_support(fixture$claim, fixture$span))
    }
  )

  tempest:::tempest_verify_one_claim_span(
    fixture$claim,
    fixture$span,
    workspace,
    verifier = fake_chat(),
    module = module,
    verified_at = "2026-08-16T12:03:00Z",
    verifier_model = "test-verifier"
  )

  expect_identical(seen$knowledge_view, view)
  expect_identical(seen$claim, fixture$claim)
  expect_identical(seen$evidence_span, fixture$span)
  expect_identical(seen$verified_at, "2026-08-16T12:03:00Z")
  expect_identical(seen$verifier_model, "test-verifier")
})

test_that("session verification commits pair records and support state atomically", {
  skip_if_not_installed("ellmer")
  config <- tempest_config(
    citation_policy = "claim_verified",
    min_support_score = 0.9,
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  session <- tempest_session(
    "Session verification",
    config = config,
    experts = list(test_expert(
      expert_id = "expert.session-verifier",
      name = "Session Verifier"
    )),
    session_id = "session-verification"
  )
  fixtures <- list(
    test_add_verifiable_claim(session$workspace, "1"),
    test_add_verifiable_claim(session$workspace, "2")
  )
  bypass_supports <- lapply(fixtures, function(fixture) {
    test_claim_support(fixture$claim, fixture$span)
  })
  claims_before_bypass <- session$workspace$list_proposed_claims()
  workspace_before_public_bypass <-
    tempest:::tempest_research_workspace_snapshot(session$workspace)
  public_bypass_judge <- fake_chat(
    structured = list(
      list(status = "supported", score = 0.95, rationale = "Exact support."),
      list(status = "supported", score = 0.95, rationale = "Exact support.")
    )
  )
  expect_error(
    tempest_verify_claims(
      session$workspace,
      verifier = public_bypass_judge,
      min_support_score = 0.9
    ),
    class = "tempest_research_workspace_integrity_error"
  )
  expect_length(public_bypass_judge$.calls(), 0L)
  expect_identical(
    tempest:::tempest_research_workspace_snapshot(session$workspace),
    workspace_before_public_bypass
  )
  expect_error(
    session$workspace$verify_proposed_claims_batch(
      bypass_supports,
      verified_at = "2026-08-16T12:03:00Z"
    ),
    class = "tempest_research_workspace_integrity_error"
  )
  expect_identical(
    session$workspace$list_proposed_claims(),
    claims_before_bypass
  )
  expect_length(session$workspace$list_claim_supports(), 0L)
  expect_length(tempest:::tempest_session_stage_records(session), 0L)
  judge <- fake_chat(
    structured = list(
      list(status = "supported", score = 0.95, rationale = "Exact support."),
      list(status = "unsupported", score = 0.2, rationale = "No support.")
    )
  )

  audit <- tempest_verify_claims(session, verifier = judge)
  records <- tempest:::tempest_session_stage_records(session)

  expect_identical(nrow(audit), 2L)
  expect_length(records, 2L)
  expect_identical(
    vapply(records, \(record) record@stage, ""),
    rep("verify_claim_support", 2L)
  )
  expect_identical(
    vapply(records, \(record) record@output_reference$kind, ""),
    rep("claim_supports", 2L)
  )
  expect_setequal(
    vapply(records, \(record) record@output_reference$ids[[1]], ""),
    audit$claim_support_id
  )
  expect_identical(
    vapply(
      records,
      \(record) record@trace_references$min_support_score,
      ""
    ),
    rep("0.9", 2L)
  )
  expect_identical(
    vapply(records, \(record) record@trace_references$verified_at, ""),
    rep(records[[1]]@trace_references$verified_at, 2L)
  )
  expect_identical(
    vapply(records, \(record) record@trace_references$verifier_model, ""),
    rep(config@models[["judge"]], 2L)
  )

  calls_before <- length(judge$.calls())
  expect_error(
    tempest_verify_claims(session, verifier = judge),
    class = "tempest_stage_governance_error"
  )
  expect_identical(length(judge$.calls()), calls_before)
  expect_length(tempest:::tempest_session_stage_records(session), 2L)
  workspace_before_mutation <-
    tempest:::tempest_research_workspace_snapshot(session$workspace)
  records_before_mutation <- tempest:::tempest_stage_records_data(
    tempest:::tempest_session_stage_records(session)
  )
  expect_error(
    session$workspace$upsert_retrieved_resource(tempest_resource(
      resource_kind = "web.page",
      locator = "https://example.org/after-verification",
      title = "Late evidence",
      media_type = "text/plain",
      content = "Late evidence.",
      resource_id = "source.after.verification"
    )),
    class = "tempest_research_workspace_integrity_error"
  )
  expect_identical(
    tempest:::tempest_research_workspace_snapshot(session$workspace),
    workspace_before_mutation
  )
  expect_identical(
    tempest:::tempest_stage_records_data(
      tempest:::tempest_session_stage_records(session)
    ),
    records_before_mutation
  )
  snapshot <- tempest_session_snapshot(session)
  restored <- tempest_session_restore(snapshot, config = config)
  expect_identical(
    identical(
      tempest:::tempest_session_verification_owner_token(restored),
      tempest:::tempest_session_verification_owner_token(session)
    ),
    FALSE
  )
  expect_false("verification_owner_token" %in% names(snapshot$workspace))
  expect_length(tempest:::tempest_session_stage_records(restored), 2L)
  expect_identical(
    tempest_claim_supports(restored$workspace),
    tempest_claim_supports(session$workspace)
  )
  restored_before_bypass <-
    tempest:::tempest_research_workspace_snapshot(restored$workspace)
  expect_error(
    restored$workspace$verify_proposed_claims_batch(
      restored$workspace$list_claim_supports(),
      verified_at = records[[1]]@trace_references$verified_at,
      verifier = records[[1]]@trace_references$verifier_model
    ),
    class = "tempest_research_workspace_integrity_error"
  )
  expect_identical(
    tempest:::tempest_research_workspace_snapshot(restored$workspace),
    restored_before_bypass
  )
})

test_that("session verification rejects policy and ProgramSet drift pre-provider", {
  skip_if_not_installed("ellmer")
  calls <- 0L
  config <- tempest_config(
    citation_policy = "strict",
    min_support_score = 0.9,
    chat_fn = function(role, model, system_prompt, echo) {
      calls <<- calls + 1L
      fake_chat()
    }
  )
  session <- tempest_session(
    "Session verification governance",
    config = config,
    experts = list(test_expert(
      expert_id = "expert.session-governance",
      name = "Session Governance"
    )),
    session_id = "session-verification-governance"
  )
  calls <- 0L

  expect_error(
    tempest_verify_claims(
      session,
      verifier = fake_chat(),
      policy = "claim_verified"
    ),
    class = "tempest_stage_governance_error"
  )
  expect_error(
    tempest_verify_claims(
      session,
      verifier = fake_chat(),
      min_support_score = 0.7
    ),
    class = "tempest_stage_governance_error"
  )
  expect_error(
    tempest_verify_claims(
      session,
      verifier = fake_chat(),
      program_set = tempest_program_set()
    ),
    class = "tempest_stage_governance_error"
  )
  expect_identical(calls, 0L)
  expect_length(tempest:::tempest_session_stage_records(session), 0L)
})

test_that("TempestSession adopts only fresh unowned workspaces", {
  skip_if_not_installed("ellmer")
  config <- tempest_config(
    citation_policy = "claim_verified",
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  verified <- test_verified_workspace()$workspace
  expect_error(
    tempest_session(
      "Preverified workspace",
      config = config,
      experts = list(test_expert(expert_id = "expert.preverified")),
      retriever = tempest_retriever(config = config, workspace = verified)
    ),
    class = "tempest_research_workspace_integrity_error"
  )

  failed_workspace <- tempest_research_workspace()
  limited_config <- tempest_config(
    citation_policy = "claim_verified",
    max_active_experts = 1L,
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  failed_retriever <- tempest_retriever(
    config = limited_config,
    workspace = failed_workspace
  )
  expect_error(
    tempest_session(
      "Failed adoption",
      config = limited_config,
      experts = list(
        test_expert(expert_id = "expert.failed-owner-1"),
        test_expert(expert_id = "expert.failed-owner-2")
      ),
      retriever = failed_retriever
    ),
    class = "tempest_config_error"
  )
  expect_r6_class(
    tempest_session(
      "Recovered adoption",
      config = limited_config,
      experts = list(test_expert(expert_id = "expert.recovered-owner")),
      retriever = failed_retriever
    ),
    "TempestSession"
  )

  workspace <- tempest_research_workspace()
  retriever <- tempest_retriever(config = config, workspace = workspace)
  first <- tempest_session(
    "First owner",
    config = config,
    experts = list(test_expert(expert_id = "expert.first-owner")),
    retriever = retriever
  )
  expect_r6_class(first, "TempestSession")
  expect_error(
    tempest_session(
      "Second owner",
      config = config,
      experts = list(test_expert(expert_id = "expert.second-owner")),
      retriever = retriever
    ),
    class = "tempest_research_workspace_integrity_error"
  )
})

test_that("failed session verification permits evidence mutation and retry", {
  skip_if_not_installed("ellmer")
  config <- tempest_config(
    citation_policy = "claim_verified",
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  session <- tempest_session(
    "Retry verification",
    config = config,
    experts = list(test_expert(expert_id = "expert.retry-verification"))
  )
  test_add_verifiable_claim(session$workspace, "retry-1")
  failed_judge <- fake_chat()
  expect_error(
    tempest_verify_claims(session, verifier = failed_judge),
    class = "tempest_stage_execution_error"
  )
  expect_length(session$workspace$list_claim_supports(), 0L)
  expect_identical(
    tempest:::tempest_session_stage_records(session)[[1]]@status,
    "failed"
  )

  expect_no_error(test_add_verifiable_claim(session$workspace, "retry-2"))
  judge <- fake_chat(
    structured = list(
      list(status = "supported", score = 0.9, rationale = "Exact support."),
      list(status = "supported", score = 0.9, rationale = "Exact support.")
    )
  )
  expect_no_error(tempest_verify_claims(session, verifier = judge))
  expect_length(session$workspace$list_claim_supports(), 2L)
  snapshot <- tempest_session_snapshot(session)
  expect_r6_class(
    tempest_session_restore(snapshot, config = config),
    "TempestSession"
  )
})
