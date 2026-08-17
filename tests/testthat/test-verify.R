test_that("verify_claims labels each claim and returns an audit tibble", {
  store <- tempest_research_workspace()
  store$upsert_retrieved_resource(fake_source("https://example.org/1"))
  s1 <- store$list_retrieved_sources()[[1]]$id
  store$add_proposed_claim(tempest_claim(
    claim_text = "supported claim",
    source_ids = s1
  ))
  store$add_proposed_claim(tempest_claim(
    claim_text = "weak claim",
    source_ids = s1
  ))

  judge <- fake_chat(
    structured = list(
      list(status = "supported", score = 0.92, rationale = "stated verbatim"),
      list(status = "unsupported", score = 0.10, rationale = "not in source")
    )
  )

  audit <- tempest_verify_claims(store, verifier = judge)
  expect_s3_class(audit, "tbl_df")
  expect_equal(nrow(audit), 2)
  expect_setequal(audit$verification_status, c("supported", "unsupported"))

  statuses <- vapply(
    store$list_proposed_claims(),
    function(c) c@verification_status,
    character(1)
  )
  expect_contains(statuses, c("supported", "unsupported"))
})

test_that("verify_claims enforces min_support_score", {
  store <- fake_store_with_sources(1)
  s1 <- store$list_retrieved_sources()[[1]]$id
  store$add_proposed_claim(tempest_claim(
    claim_text = "weakly scored claim",
    source_ids = s1
  ))

  judge <- fake_chat(
    structured = list(
      list(status = "supported", score = 0.60, rationale = "weak match")
    )
  )

  audit <- tempest_verify_claims(
    store,
    verifier = judge,
    min_support_score = 0.7
  )
  expect_equal(audit$verification_status, "unsupported")
  expect_equal(
    store$list_proposed_claims()[[1]]@verification_status,
    "unsupported"
  )
  expect_equal(store$list_proposed_claims()[[1]]@support_score, 0.60)
})

test_that("verify_claims skips when policy is none/source_attributed", {
  store <- tempest_research_workspace()
  store$upsert_retrieved_resource(fake_source("https://example.org/1"))
  claim <- tempest_claim(
    claim_text = "c",
    source_ids = store$list_retrieved_sources()[[1]]$id
  )
  store$add_proposed_claim(claim)
  store$set_citation_audit(tibble::tibble(
    claim_id = claim@claim_id,
    claim_text = claim@claim_text,
    verification_status = claim@verification_status,
    support_score = claim@support_score,
    rationale = NA_character_
  ))
  audit <- tempest_verify_claims(
    store,
    verifier = fake_chat(),
    policy = "source_attributed"
  )
  expect_equal(nrow(audit), 0)
  expect_identical(store$citation_audit, audit)
})

test_that("verify_claims rejects invalid output without a partial commit", {
  store <- fake_store_with_sources(1)
  s1 <- store$list_retrieved_sources()[[1]]$id
  store$add_proposed_claim(tempest_claim(claim_text = "a", source_ids = s1))
  store$add_proposed_claim(tempest_claim(claim_text = "b", source_ids = s1))
  judge <- fake_chat(
    structured = list(
      list(status = "supported", score = 1.5, rationale = "over range"),
      list(
        status = "weird_status",
        score = "0.7",
        rationale = "string score, bad status"
      )
    )
  )

  expect_error(
    tempest_verify_claims(store, verifier = judge),
    class = "tempest_stage_output_validation_error"
  )
  claims <- store$list_proposed_claims()
  expect_equal(
    vapply(claims, \(claim) claim@verification_status, character(1)),
    rep("unverified", 2L)
  )
  expect_equal(
    vapply(claims, \(claim) claim@support_score, numeric(1)),
    rep(NA_real_, 2L)
  )
  expect_null(store$citation_audit)
})

test_that("verification rejects credential-bearing provider metadata", {
  store <- fake_store_with_sources(1)
  source_id <- store$list_retrieved_sources()[[1]]$id
  store$add_proposed_claim(tempest_claim(
    claim_text = "A source-backed claim",
    source_ids = source_id
  ))
  judge <- fake_chat(
    structured = list(list(
      status = "supported",
      score = 0.9,
      rationale = "Authorization: Bearer sk-live-secret"
    ))
  )

  expect_error(
    tempest_verify_claims(store, verifier = judge),
    class = "tempest_stage_output_validation_error"
  )
  expect_identical(
    store$list_proposed_claims()[[1]]@verification_status,
    "unverified"
  )
  expect_null(store$citation_audit)
})

test_that("verification rejects unsafe verifier identity before provider use", {
  store <- fake_store_with_sources(1)
  source_id <- store$list_retrieved_sources()[[1]]$id
  store$add_proposed_claim(tempest_claim(
    claim_text = "A source-backed claim",
    source_ids = source_id
  ))
  provider_calls <- 0L
  local_mocked_bindings(
    tempest_verify_one_claim = function(...) {
      provider_calls <<- provider_calls + 1L
      stop("provider should not run")
    }
  )

  expect_error(
    tempest_verify_claims(
      store,
      verifier = fake_chat(),
      verifier_model = "sk-live-secret"
    ),
    class = "tempest_stage_governance_error"
  )
  expect_identical(provider_calls, 0L)
})

test_that("verification requires captured source evidence before provider use", {
  store <- tempest_research_workspace()
  store$upsert_retrieved_resource(fake_source(
    "https://example.org/empty-verification-source",
    content_text = NA_character_
  ))
  source_id <- store$list_retrieved_sources()[[1]]$id
  store$add_proposed_claim(tempest_claim(
    claim_text = "An uncaptured claim",
    source_ids = source_id
  ))
  judge <- fake_chat(
    structured = list(list(
      status = "supported",
      score = 0.9,
      rationale = "Should not run"
    ))
  )

  expect_error(
    tempest_verify_claims(store, verifier = judge),
    class = "tempest_stage_governance_error"
  )
  expect_length(judge$.calls(), 0L)
  expect_identical(
    store$list_proposed_claims()[[1]]@verification_status,
    "unverified"
  )
  expect_null(store$citation_audit)
})

test_that("verification output digest binds exact captured source evidence", {
  store <- fake_store_with_sources(1)
  source <- store$list_retrieved_sources()[[1]]
  claim <- tempest_claim("Bound claim", source_ids = source$id)
  store$add_proposed_claim(claim)
  record <- tempest:::tempest_stage_record_start(
    "verify_claim_support",
    paste0("sha256:", strrep("a", 64L)),
    attempt_id = "attempt-source-digest"
  )
  audit <- list(
    claim_id = claim@claim_id,
    claim_text = claim@claim_text,
    verification_status = "supported",
    support_score = 0.9,
    rationale = "Exact support"
  )
  before <- tempest:::tempest_stage_verification_output_digest(
    audit,
    record,
    claim,
    store
  )
  store$upsert_retrieved_resource(fake_source(
    source$url,
    title = source$title,
    content_text = "Irrelevant replacement content."
  ))
  after <- tempest:::tempest_stage_verification_output_digest(
    audit,
    record,
    claim,
    store
  )

  expect_identical(identical(before, after), FALSE)
})

test_that("verify_claims does not downgrade dsprrr contract failures", {
  store <- fake_store_with_sources(1)
  source_id <- store$list_retrieved_sources()[[1]]$id
  store$add_proposed_claim(tempest_claim(
    claim_text = "contract-bound claim",
    source_ids = source_id
  ))
  local_mocked_bindings(
    tempest_verify_one_claim = function(...) {
      rlang::abort(
        "program identity mismatch",
        class = "dsprrr_program_artifact_integrity_error"
      )
    }
  )

  expect_error(
    tempest_verify_claims(store, verifier = fake_chat()),
    class = "dsprrr_program_artifact_integrity_error"
  )
})

test_that("verify_claims commits session audits and records as one batch", {
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
  session$workspace$upsert_retrieved_resource(
    fake_source("https://example.org/session-verification")
  )
  source_id <- session$workspace$list_retrieved_sources()[[1]]$id
  claims <- lapply(c("Claim one", "Claim two"), function(text) {
    claim <- tempest_claim(text, source_ids = source_id)
    session$workspace$add_proposed_claim(claim)
    claim
  })
  claims <- session$workspace$list_proposed_claims()
  judge <- fake_chat(
    structured = list(
      list(status = "supported", score = 0.95, rationale = "Exact support."),
      list(status = "unsupported", score = 0.2, rationale = "No support.")
    )
  )

  audit <- tempest_verify_claims(session, verifier = judge)
  records <- tempest:::tempest_session_stage_records(session)

  expect_identical(audit$claim_id, vapply(claims, \(claim) claim@claim_id, ""))
  expect_identical(
    audit$verification_status,
    c("supported", "unsupported")
  )
  expect_identical(
    vapply(
      session$workspace$list_proposed_claims(),
      \(claim) claim@verifier_model,
      ""
    ),
    rep(config@models[["judge"]], 2L)
  )
  expect_length(records, 2L)
  expect_identical(
    vapply(records, \(record) record@stage, ""),
    rep("verify_claim_support", 2L)
  )
  expect_identical(
    vapply(records, \(record) record@status, ""),
    rep("succeeded", 2L)
  )
  expect_identical(
    vapply(records, \(record) record@program_artifact_id, ""),
    rep(
      tempest:::tempest_session_programs(
        session
      )$verify_claim_support$program_artifact_id,
      2L
    )
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
