test_that("promotion and Graft preserve advisory claim-review proof", {
  skip_if_not_installed("graft")

  run_id <- "research-review-artifact"
  config <- tempest_config(
    chat_fn = function(...) fake_chat(),
    research_review = TRUE
  )
  extra_source <- fake_source(
    url = "https://example.org/independent-review",
    title = "Independent review source",
    content_text = "The independent report identifies a missing comparison."
  )
  completed <- test_persistence_complete_storm_product(
    topic = "Promotion evidence",
    run_id = run_id,
    config = config,
    program_set = test_promotion_program_set(),
    extra_sources = list(extra_source)
  )
  claim <- completed$workspace$get_proposed_claim(completed$claim_id)
  quote <- "identifies a missing comparison"
  evidence <- list(list(source_id = extra_source@resource_id, quote = quote))
  review <- tempest_claim_review(list(
    schema_version = 1L,
    claim_id = claim@claim_id,
    claim_text = claim@claim_text,
    dossier = lapply(
      c(
        lapply(claim@source_ids, completed$workspace$get_retrieved_resource),
        list(extra_source)
      ),
      function(source) {
        list(
          source_id = source@resource_id,
          locator = source@locator,
          content_hash = source@content_hash,
          text = source@content
        )
      }
    ),
    reviewer = list(
      status = "potential_issue",
      finding = "The claim may omit a comparison.",
      rationale = "The independent source describes the missing comparison.",
      evidence = evidence,
      query = "Find the comparison discussed by the independent report."
    ),
    adjudication = list(
      verdict = "confirmed",
      rationale = "The source confirms that the comparison is missing.",
      evidence = evidence
    ),
    repair = list(
      status = "not_requested",
      query = "",
      source_ids = character()
    ),
    programs = list(
      reviewer = paste0("sha256:", strrep("a", 64L)),
      adjudicator = paste0("sha256:", strrep("b", 64L))
    ),
    models = list(
      reviewer = "gpt-4.1",
      adjudicator = "gpt-4.1"
    )
  ))
  completed$workspace$replace_claim_reviews(list(review))
  report_base <- tempest:::tempest_product_report_without_execution_review(
    completed$state$report_md,
    completed$state$stage_records,
    trusted_title = completed$state$title
  )
  report_base <- tempest:::tempest_report_append_claim_reviews(
    report_base,
    completed$workspace
  )
  completed$state$report_md <- tempest:::tempest_product_report_for_stage_records(
    report_base,
    completed$state$stage_records,
    trusted_title = completed$state$title
  )
  completed$state <- tempest:::tempest_storm_state_validate(completed$state)
  manifest <- tempest:::tempest_product_authority_finalize_manifest(
    manifest = completed$manifest,
    stage_records = completed$state$stage_records,
    workspace = completed$workspace,
    report_md = completed$state$report_md,
    config = config,
    experts = completed$state$experts,
    product_state = completed$state,
    status = "succeeded",
    require_publishable = TRUE
  )
  retriever <- tempest_retriever(
    config = config,
    workspace = completed$workspace
  )
  tempest:::tempest_research_workspace_seal(completed$workspace)
  research <- tempest:::tempest_product_result(
    title = completed$state$title,
    topic = completed$state$title,
    perspectives = completed$state$perspectives,
    experts = completed$state$experts,
    outline = completed$state$outline,
    draft_md = completed$state$draft_md,
    report_md = completed$state$report_md,
    manifest = manifest,
    state = completed$state,
    workspace = completed$workspace,
    retriever = retriever,
    output_dir = NULL
  )

  bundle <- tempest_promotion_bundle(research)
  proof_data <- tempest:::tempest_promotion_bundle_data(bundle)$proof
  proof_reviews <- lapply(
    proof_data$claim_reviews,
    tempest:::tempest_claim_review_from_data
  )
  expect_identical(
    lapply(proof_reviews, tempest_claim_review_record),
    list(tempest_claim_review_record(review))
  )
  review_resource <- Filter(
    function(row) identical(row$resource_id, extra_source@resource_id),
    proof_data$resources
  )
  expect_length(review_resource, 1L)
  expect_identical(review_resource[[1L]]$content, extra_source@content)

  baseline <- tempest_promotion_bundle(
    test_promotion_storm_fixture(run_id = run_id)$research
  )
  expect_identical(
    bundle@records$ClaimSupport,
    baseline@records$ClaimSupport
  )

  omitted <- tempest:::tempest_promotion_bundle_data(bundle)
  omitted$proof$claim_reviews <- list()
  expect_error(
    tempest:::tempest_promotion_bundle_from_data(omitted),
    class = "tempest_promotion_error"
  )
  changed <- tempest:::tempest_promotion_bundle_data(bundle)
  changed_review <- tempest:::tempest_claim_review_from_data(
    changed$proof$claim_reviews[[1L]]
  )
  changed_record <- tempest_claim_review_record(changed_review)
  changed_record$dossier[[2L]]$text <- "Forged dossier text."
  changed_record$reviewer$evidence[[1L]]$quote <- "Forged dossier text."
  changed_record$adjudication$evidence[[1L]]$quote <- "Forged dossier text."
  changed_record <- tempest_claim_review(
    changed_record[setdiff(names(changed_record), "review_id")]
  )
  changed$proof$claim_reviews[[1L]] <-
    tempest:::tempest_promotion_claim_review_proof_row(changed_record)
  expect_error(
    tempest:::tempest_promotion_bundle_from_data(changed),
    class = "tempest_promotion_error"
  )

  store <- test_graft_store(tempfile(), create = TRUE)
  selection <- tempest_publish_artifact_research(research, store)
  retained <- tempest_read_artifact_research(store, selection)
  expect_identical(retained$report_md, research@report_md)
  expect_identical(
    retained$claim_reviews,
    list(tempest_claim_review_record(review))
  )
  expect_match(retained$report_md, "## Scientific review", fixed = TRUE)
  expect_match(
    retained$report_md,
    "Source: <https://example.org/independent-review>",
    fixed = TRUE
  )
  decision <- test_graft_decide(
    store,
    "review-artifact",
    "review-artifact-accept",
    expected = NULL,
    selection = selection,
    action = "accept",
    actor = "host",
    reason = "Reviewed the retained evidence.",
    purpose = "briefing"
  )
  knowledge <- tempest_reuse_artifact_research(
    store,
    "review-artifact",
    decision$id,
    "briefing",
    eligible = function(event) TRUE
  )
  expect_length(knowledge@records, length(retained$records))
  expect_length(
    knowledge@records,
    length(bundle@records$Source) +
      length(bundle@records$Claim) +
      length(bundle@records$EvidenceSpan) +
      length(bundle@records$ClaimSupport)
  )
})

test_that("selected promotion proof excludes unrelated advisory reviews", {
  completed <- test_persistence_complete_storm_product(
    topic = "Selective promotion",
    run_id = "selective-review-proof",
    config = tempest_config(chat_fn = function(...) fake_chat()),
    program_set = test_promotion_program_set()
  )
  selected <- completed$workspace$get_proposed_claim(completed$claim_id)
  source <- completed$workspace$get_retrieved_resource(selected@source_ids[[1]])
  workspace <- tempest_research_workspace()
  workspace$upsert_retrieved_resource(source)
  for (span in completed$workspace$list_evidence_spans()) {
    workspace$add_evidence_span(span)
  }
  workspace$add_proposed_claim(selected)
  unrelated <- tempest_claim(
    claim_id = "unrelated-unsupported",
    claim_text = "The result applies to every population.",
    source_ids = source@resource_id,
    verification_status = "unsupported"
  )
  workspace$add_proposed_claim(unrelated)
  review <- tempest_claim_review(list(
    schema_version = 1L,
    claim_id = unrelated@claim_id,
    claim_text = unrelated@claim_text,
    dossier = list(list(
      source_id = source@resource_id,
      locator = source@locator,
      content_hash = source@content_hash,
      text = source@content
    )),
    reviewer = list(
      status = "insufficient_evidence",
      finding = "",
      rationale = "The captured source does not establish universal scope.",
      evidence = list(),
      query = ""
    ),
    adjudication = list(
      verdict = "unresolved",
      rationale = "The population scope remains unresolved.",
      evidence = list()
    ),
    repair = list(
      status = "not_requested",
      query = "",
      source_ids = character()
    ),
    programs = list(
      reviewer = paste0("sha256:", strrep("a", 64)),
      adjudicator = paste0("sha256:", strrep("b", 64))
    ),
    models = list(reviewer = "fixture", adjudicator = "fixture")
  ))
  workspace$replace_claim_reviews(list(review))
  proof <- tempest_promotion_proof_data(
    workspace,
    completed$state$stage_records,
    completed$workspace$list_claim_supports()
  )
  expect_length(proof$claim_reviews, 0L)
  expect_identical(
    vapply(proof$claims, `[[`, character(1), "claim_id"),
    selected@claim_id
  )
  expect_identical(
    tempest_claim_review_record(workspace$list_claim_reviews()[[1]])$claim_id,
    unrelated@claim_id
  )
})
