test_that("Co-STORM report prompts include only threshold-verified evidence", {
  skip_if_not_installed("ellmer")
  config <- tempest_config(
    citation_policy = "claim_verified",
    min_support_score = 0.95,
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  session <- tempest_session(
    "Verified report evidence",
    config = config,
    experts = list(test_expert(
      expert_id = "expert.verified-report",
      name = "Verified Report Expert"
    ))
  )
  below_threshold <- test_add_verifiable_claim(
    session$workspace,
    key = "below",
    claim_text = "Below-threshold evidence must not enter the report prompt.",
    quote = "Below-threshold report evidence."
  )
  verified <- test_add_verifiable_claim(
    session$workspace,
    key = "verified",
    claim_text = "Threshold-verified evidence enters the report prompt.",
    quote = "Threshold-verified report evidence."
  )
  tempest_verify_claims(
    session,
    verifier = fake_chat(
      structured = list(
        list(status = "supported", score = 0.9, rationale = "Below threshold."),
        list(status = "supported", score = 0.98, rationale = "Verified.")
      )
    )
  )

  prompt <- tempest:::tempest_costorm_report_prompt(session, "technical")

  expect_match(
    prompt,
    "Threshold-verified evidence enters the report prompt.",
    fixed = TRUE
  )
  expect_no_match(
    prompt,
    "Below-threshold evidence must not enter the report prompt.",
    fixed = TRUE
  )
})

test_that("Co-STORM report preflight rejects invalid live state before chat", {
  skip_if_not_installed("ellmer")
  chat <- fake_chat()
  config <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) chat
  )
  session <- tempest_session(
    "Report preflight",
    config = config,
    experts = list(test_expert(
      expert_id = "expert.report-preflight",
      name = "Report Preflight Expert"
    ))
  )

  expect_error(
    session$report(include_references = NA),
    class = "tempest_product_validation_error"
  )
  expect_length(chat$.calls(), 0L)
})
