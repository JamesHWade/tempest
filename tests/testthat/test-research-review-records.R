test_that("claim-review records are canonical, content-bound values", {
  source_text <- "The mechanism absorbs photons and releases oxygen."
  source <- test_typed_web_resource(content = source_text)
  workspace <- tempest_research_workspace()
  workspace$upsert_retrieved_resource(source)
  workspace$add_proposed_claim(tempest:::tempest_claim(
    claim_id = "claim-review-1",
    claim_text = "The mechanism absorbs photons.",
    source_ids = source@resource_id
  ))
  record <- list(
    schema_version = 1L,
    claim_id = "claim-review-1",
    claim_text = "The mechanism absorbs photons.",
    dossier = list(list(
      source_id = source@resource_id,
      locator = source@locator,
      content_hash = source@content_hash,
      text = "The mechanism absorbs photons and releases oxygen."
    )),
    reviewer = list(
      status = "potential_issue",
      finding = "The mechanism description may overstate the result.",
      rationale = "The source describes a process, not a measured outcome.",
      evidence = list(list(
        source_id = source@resource_id,
        quote = "absorbs photons"
      )),
      query = "Does the source support this claim?"
    ),
    adjudication = list(
      verdict = "confirmed",
      rationale = "The cited wording supports the reviewed issue.",
      evidence = list(list(
        source_id = source@resource_id,
        quote = "releases oxygen"
      ))
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
    models = list(reviewer = "gpt-4.1", adjudicator = "gpt-4.1")
  )

  review <- tempest:::tempest_claim_review(record)
  expect_identical(
    S7::prop(review, "record"),
    tempest:::tempest_claim_review_record(review)
  )
  projection <- tempest:::tempest_claim_review_record(review)
  expect_identical(
    names(projection),
    tempest:::tempest_claim_review_record_fields()
  )
  expect_match(projection$review_id, "^sha256:[a-f0-9]{64}$")
  projection$dossier[[1]]$text <- "Changed copy"
  expect_identical(
    tempest:::tempest_claim_review_record(review)$dossier[[1]]$text,
    source_text
  )

  reordered <- record[c(
    "models",
    "claim_text",
    "programs",
    "repair",
    "adjudication",
    "dossier",
    "claim_id",
    "reviewer",
    "schema_version"
  )]
  reordered$reviewer <- reordered$reviewer[
    c("query", "evidence", "rationale", "finding", "status")
  ]
  canonicalized <- tempest:::tempest_claim_review(reordered)
  expect_identical(
    tempest:::tempest_claim_review_record(canonicalized),
    tempest:::tempest_claim_review_record(review)
  )

  other_claim <- tempest:::tempest_claim(
    claim_id = "claim-review-0",
    claim_text = "Another claim from the same source.",
    source_ids = source@resource_id
  )
  workspace$add_proposed_claim(other_claim)
  other_record <- record
  other_record$claim_id <- other_claim@claim_id
  other_record$claim_text <- other_claim@claim_text
  other_review <- tempest:::tempest_claim_review(other_record)
  workspace$replace_claim_reviews(list(review, other_review))
  expect_identical(
    vapply(
      workspace$list_claim_reviews(),
      function(value) {
        tempest:::tempest_claim_review_record(value)$claim_id
      },
      character(1)
    ),
    c("claim-review-0", "claim-review-1")
  )
  duplicate_claim_record <- record
  duplicate_claim_record$adjudication$rationale <-
    "A different review of the same claim."
  expect_error(
    workspace$replace_claim_reviews(list(
      review,
      tempest:::tempest_claim_review(duplicate_claim_record)
    )),
    class = "tempest_research_workspace_integrity_error",
    regexp = "one record per claim"
  )

  forged_text <- record
  forged_text$dossier[[1]]$text <-
    "Edited passage: absorbs photons and releases oxygen."
  forged_text$reviewer$evidence[[1]]$quote <- "absorbs photons"
  expect_error(
    workspace$replace_claim_reviews(list(
      tempest:::tempest_claim_review(forged_text)
    )),
    class = "tempest_research_workspace_integrity_error",
    regexp = "exact substring of source"
  )
  unknown_source <- record
  unknown_source_id <- "S000000000000"
  unknown_source$dossier[[2]] <- unknown_source$dossier[[1]]
  unknown_source$dossier[[2]]$source_id <- unknown_source_id
  unknown_source$reviewer$evidence[[1]]$source_id <- unknown_source_id
  unknown_source$adjudication$evidence[[1]]$source_id <- unknown_source_id
  expect_error(
    workspace$replace_claim_reviews(list(
      tempest:::tempest_claim_review(unknown_source)
    )),
    class = "tempest_research_workspace_integrity_error",
    regexp = "unknown source id"
  )

  invalid_quote <- record
  invalid_quote$reviewer$evidence[[1]]$quote <- "water is produced"
  expect_error(
    tempest:::tempest_claim_review(invalid_quote),
    class = "tempest_claim_review_error",
    regexp = "exact substring"
  )
  duplicated_source <- record
  duplicated_source$dossier[[2]] <- duplicated_source$dossier[[1]]
  expect_error(
    tempest:::tempest_claim_review(duplicated_source),
    class = "tempest_claim_review_error",
    regexp = "duplicate source_id"
  )
  forged_id <- projection
  forged_id$claim_text <- "Changed claim text."
  expect_error(
    tempest:::tempest_claim_review(forged_id),
    class = "tempest_claim_review_error",
    regexp = "review_id"
  )
  invalid_verdict <- record
  invalid_verdict$adjudication$evidence <- list()
  expect_error(
    tempest:::tempest_claim_review(invalid_verdict),
    class = "tempest_claim_review_error",
    regexp = "adjudicator with cited evidence"
  )
  no_issue_mismatch <- record
  no_issue_mismatch$adjudication$verdict <- "no_issue"
  expect_error(
    tempest:::tempest_claim_review(no_issue_mismatch),
    class = "tempest_claim_review_error",
    regexp = "requires a no_issue reviewer"
  )
  too_many_repair_ids <- record
  too_many_repair_ids$repair <- list(
    status = "completed",
    query = record$reviewer$query,
    source_ids = c(
      "S000000000001",
      "S000000000002",
      "S000000000003"
    )
  )
  expect_error(
    tempest:::tempest_claim_review(too_many_repair_ids),
    class = "tempest_claim_review_error",
    regexp = "at most 2"
  )
  repair_source_outside_dossier <- record
  repair_source_outside_dossier$repair <- list(
    status = "completed",
    query = record$reviewer$query,
    source_ids = "S000000000099"
  )
  expect_error(
    tempest:::tempest_claim_review(repair_source_outside_dossier),
    class = "tempest_claim_review_error",
    regexp = "sources in the dossier"
  )
  repair_query_mismatch <- record
  repair_query_mismatch$repair <- list(
    status = "no_results",
    query = "A different repair query",
    source_ids = character()
  )
  expect_error(
    tempest:::tempest_claim_review(repair_query_mismatch),
    class = "tempest_claim_review_error",
    regexp = "match reviewer query"
  )
})

test_that("staged repair resources preserve authoritative verified evidence", {
  make_review <- function(fixture, supplemental) {
    source <- fixture$source
    tempest:::tempest_claim_review(list(
      schema_version = 1L,
      claim_id = fixture$claim@claim_id,
      claim_text = fixture$claim@claim_text,
      dossier = list(
        list(
          source_id = source@resource_id,
          locator = source@locator,
          content_hash = source@content_hash,
          text = source@content
        ),
        list(
          source_id = supplemental@resource_id,
          locator = supplemental@locator,
          content_hash = supplemental@content_hash,
          text = supplemental@content
        )
      ),
      reviewer = list(
        status = "no_issue",
        finding = "",
        rationale = "Both captured sources support the claim.",
        evidence = list(
          list(
            source_id = source@resource_id,
            quote = fixture$span@quote
          ),
          list(
            source_id = supplemental@resource_id,
            quote = "independent corroboration"
          )
        ),
        query = "Independent corroboration for the claim"
      ),
      adjudication = list(
        verdict = "no_issue",
        rationale = "The independent source corroborates the claim.",
        evidence = list(list(
          source_id = supplemental@resource_id,
          quote = "independent corroboration"
        ))
      ),
      repair = list(
        status = "completed",
        query = "Independent corroboration for the claim",
        source_ids = supplemental@resource_id
      ),
      programs = list(
        reviewer = paste0("sha256:", strrep("e", 64L)),
        adjudicator = paste0("sha256:", strrep("f", 64L))
      ),
      models = list(reviewer = "gpt-4.1", adjudicator = "gpt-4.1")
    ))
  }

  workspace <- tempest_research_workspace(max_sources = 2L)
  fixture <- test_add_verifiable_claim(workspace)
  support <- test_claim_support(fixture$claim, fixture$span)
  workspace$verify_proposed_claims_batch(
    list(support),
    verified_at = "2026-08-16T12:03:00Z",
    verifier = "judge.fixture"
  )
  supports_before <- workspace$list_claim_supports()
  audit_before <- workspace$citation_audit
  claim_before <- workspace$get_proposed_claim(fixture$claim@claim_id)
  supplemental <- test_typed_web_resource(
    url = "https://example.org/review-repair",
    content = "A second study provides independent corroboration."
  )
  review <- make_review(fixture, supplemental)

  workspace$replace_claim_reviews(
    list(review),
    .resources = list(supplemental)
  )
  expect_identical(
    workspace$get_retrieved_resource(supplemental@resource_id),
    supplemental
  )
  expect_identical(workspace$list_claim_supports(), supports_before)
  expect_identical(workspace$citation_audit, audit_before)
  expect_identical(
    workspace$get_proposed_claim(fixture$claim@claim_id),
    claim_before
  )
  expect_no_error(workspace$validate_integrity())

  conflicting <- tempest_resource(
    resource_kind = supplemental@resource_kind,
    locator = supplemental@locator,
    title = supplemental@title,
    media_type = supplemental@media_type,
    resource_id = supplemental@resource_id,
    content = "Different content under the same resource id.",
    retrieved_at = supplemental@retrieved_at
  )
  expect_error(
    workspace$replace_claim_reviews(
      list(review),
      .resources = list(conflicting)
    ),
    class = "tempest_research_workspace_integrity_error",
    regexp = "conflicts with existing source"
  )
  expect_identical(workspace$list_claim_supports(), supports_before)
  expect_identical(workspace$citation_audit, audit_before)
  expect_identical(workspace$list_claim_reviews(), list(review))

  expect_error(
    workspace$replace_claim_reviews(
      list(review),
      .resources = list(test_subclassed_resource(supplemental))
    ),
    class = "tempest_research_workspace_integrity_error",
    regexp = "exact TempestResource"
  )
  expect_identical(workspace$list_claim_supports(), supports_before)
  expect_identical(workspace$citation_audit, audit_before)

  updated_support <- test_claim_support(
    fixture$claim,
    fixture$span,
    status = "unsupported",
    score = 0.1,
    rationale = "A later pass found the cited span does not support the claim."
  )
  workspace$verify_proposed_claims_batch(
    list(updated_support),
    verified_at = "2026-08-16T12:04:00Z",
    verifier = "judge.fixture"
  )
  expect_identical(
    workspace$get_proposed_claim(fixture$claim@claim_id)@verification_status,
    "unsupported"
  )
  expect_length(workspace$list_claim_reviews(), 1L)
  expect_no_error(workspace$validate_integrity())

  limited_workspace <- tempest_research_workspace(max_sources = 1L)
  limited_fixture <- test_add_verifiable_claim(limited_workspace)
  limited_support <- test_claim_support(
    limited_fixture$claim,
    limited_fixture$span
  )
  limited_workspace$verify_proposed_claims_batch(
    list(limited_support),
    verified_at = "2026-08-16T12:03:00Z",
    verifier = "judge.fixture"
  )
  excess_source <- test_typed_web_resource(
    url = "https://example.org/review-repair-excess",
    content = "A second study provides independent corroboration."
  )
  limited_supports_before <- limited_workspace$list_claim_supports()
  expect_error(
    limited_workspace$replace_claim_reviews(
      list(make_review(limited_fixture, excess_source)),
      .resources = list(excess_source)
    ),
    class = "tempest_research_workspace_integrity_error",
    regexp = "resource limit"
  )
  expect_length(limited_workspace$list_retrieved_resources(), 1L)
  expect_length(limited_workspace$list_claim_reviews(), 0L)
  expect_identical(
    limited_workspace$list_claim_supports(),
    limited_supports_before
  )
})

test_that("claim-review dossiers cover linked and disputed source evidence", {
  source_a <- test_typed_web_resource(
    url = "https://example.org/dispute-a",
    content = "The first source describes a treatment effect."
  )
  source_b <- test_typed_web_resource(
    url = "https://example.org/dispute-b",
    content = "The second source reports a different effect."
  )
  workspace <- tempest_research_workspace()
  workspace$upsert_retrieved_resource(source_a)
  workspace$upsert_retrieved_resource(source_b)
  span_a <- tempest_evidence_span(
    evidence_span_id = "span-dispute-a",
    source_id = source_a@resource_id,
    quote = "describes a treatment effect"
  )
  workspace$add_evidence_span(span_a)
  claim_a <- tempest:::tempest_claim(
    claim_id = "claim-dispute-a",
    claim_text = "The treatment has a measurable effect.",
    source_ids = source_a@resource_id,
    evidence_span_ids = span_a@evidence_span_id,
    supporting_quotes = list(span_a@quote)
  )
  claim_b <- tempest:::tempest_claim(
    claim_id = "claim-dispute-b",
    claim_text = "The treatment has no measurable effect.",
    source_ids = source_b@resource_id
  )
  workspace$add_proposed_claim(claim_a)
  workspace$add_proposed_claim(claim_b)
  workspace$add_dispute(tempest:::tempest_dispute(
    dispute_id = "dispute-review-a",
    topic = "Conflicting treatment effects",
    claim_ids = c(claim_a@claim_id, claim_b@claim_id)
  ))

  record <- list(
    schema_version = 1L,
    claim_id = claim_a@claim_id,
    claim_text = claim_a@claim_text,
    dossier = list(list(
      source_id = source_a@resource_id,
      locator = source_a@locator,
      content_hash = source_a@content_hash,
      text = "The first source"
    )),
    reviewer = list(
      status = "insufficient_evidence",
      finding = "",
      rationale = "The conflicting source has not been included.",
      evidence = list(),
      query = ""
    ),
    adjudication = list(
      verdict = "unresolved",
      rationale = "The disputed claim's source context is required.",
      evidence = list()
    ),
    repair = list(
      status = "not_requested",
      query = "",
      source_ids = character()
    ),
    programs = list(
      reviewer = paste0("sha256:", strrep("1", 64L)),
      adjudicator = paste0("sha256:", strrep("2", 64L))
    ),
    models = list(reviewer = "gpt-4.1", adjudicator = "gpt-4.1")
  )
  expect_error(
    workspace$replace_claim_reviews(list(tempest:::tempest_claim_review(
      record
    ))),
    class = "tempest_research_workspace_integrity_error",
    regexp = "omits required source id"
  )

  record$dossier[[2]] <- list(
    source_id = source_b@resource_id,
    locator = source_b@locator,
    content_hash = source_b@content_hash,
    text = source_b@content
  )
  expect_error(
    workspace$replace_claim_reviews(list(tempest:::tempest_claim_review(
      record
    ))),
    class = "tempest_research_workspace_integrity_error",
    regexp = "omits linked evidence quote"
  )

  record$dossier[[1]]$text <- source_a@content
  workspace$replace_claim_reviews(list(tempest:::tempest_claim_review(record)))
  expect_length(workspace$list_claim_reviews(), 1L)
  expect_no_error(workspace$validate_integrity())
})

test_that("claim reviews persist with source and claim integrity", {
  source <- test_typed_web_resource(
    url = "https://example.org/review-source",
    content = "Evidence text states that cells absorb photons."
  )
  workspace <- tempest_research_workspace()
  workspace$upsert_retrieved_resource(source)
  workspace$add_proposed_claim(tempest:::tempest_claim(
    claim_id = "claim-review-persist",
    claim_text = "Cells absorb photons.",
    source_ids = source@resource_id
  ))
  review <- tempest:::tempest_claim_review(list(
    schema_version = 1L,
    claim_id = "claim-review-persist",
    claim_text = "Cells absorb photons.",
    dossier = list(list(
      source_id = source@resource_id,
      locator = source@locator,
      content_hash = source@content_hash,
      text = "Evidence text states that cells absorb photons."
    )),
    reviewer = list(
      status = "no_issue",
      finding = "",
      rationale = "The source directly supports the claim.",
      evidence = list(list(
        source_id = source@resource_id,
        quote = "cells absorb photons"
      )),
      query = ""
    ),
    adjudication = list(
      verdict = "no_issue",
      rationale = "The source directly supports the claim.",
      evidence = list(list(
        source_id = source@resource_id,
        quote = "cells absorb photons"
      ))
    ),
    repair = list(
      status = "not_requested",
      query = "",
      source_ids = character()
    ),
    programs = list(
      reviewer = paste0("sha256:", strrep("c", 64L)),
      adjudicator = paste0("sha256:", strrep("d", 64L))
    ),
    models = list(reviewer = "gpt-4.1", adjudicator = "gpt-4.1")
  ))

  workspace$replace_claim_reviews(list(review))
  listed <- workspace$list_claim_reviews()
  expect_length(listed, 1L)
  expect_identical(
    tempest:::tempest_claim_review_record(listed[[1]]),
    tempest:::tempest_claim_review_record(review)
  )

  snapshot <- tempest:::tempest_research_workspace_snapshot(workspace)
  expect_identical(snapshot$schema_version, 8L)
  expect_type(
    snapshot$claim_reviews[[1]]$repair$source_ids,
    "list"
  )
  restored <- tempest:::tempest_research_workspace_restore(snapshot)
  expect_identical(
    tempest:::tempest_claim_review_record(
      restored$list_claim_reviews()[[1]]
    ),
    tempest:::tempest_claim_review_record(review)
  )
  json_snapshot <- jsonlite::fromJSON(
    as.character(jsonlite::toJSON(
      snapshot,
      auto_unbox = TRUE,
      null = "null",
      na = "null"
    )),
    simplifyVector = FALSE
  )
  restored_json <- tempest:::tempest_research_workspace_restore(json_snapshot)
  expect_identical(
    tempest:::tempest_claim_review_record(
      restored_json$list_claim_reviews()[[1]]
    ),
    tempest:::tempest_claim_review_record(review)
  )

  reordered_snapshot <- snapshot
  reordered_snapshot$claim_reviews[[1]] <-
    reordered_snapshot$claim_reviews[[1]][
      rev(names(reordered_snapshot$claim_reviews[[1]]))
    ]
  reordered_snapshot$claim_reviews[[1]]$reviewer <-
    reordered_snapshot$claim_reviews[[1]]$reviewer[
      rev(names(reordered_snapshot$claim_reviews[[1]]$reviewer))
    ]
  restored_reordered <- tempest:::tempest_research_workspace_restore(
    reordered_snapshot
  )
  expect_identical(
    tempest:::tempest_claim_review_record(
      restored_reordered$list_claim_reviews()[[1]]
    ),
    tempest:::tempest_claim_review_record(review)
  )

  token <- tempest:::tempest_research_workspace_bind_verification_owner(
    workspace
  )
  expect_error(
    workspace$replace_claim_reviews(list(review)),
    class = "tempest_research_workspace_integrity_error",
    regexp = "exact workspace owner token"
  )
  workspace$replace_claim_reviews(
    list(review),
    .verification_owner_token = token
  )

  stale_source <- tempest_resource(
    resource_kind = "web",
    locator = source@locator,
    title = source@title,
    media_type = source@media_type,
    resource_id = source@resource_id,
    content = "Different evidence text.",
    retrieved_at = source@retrieved_at
  )
  workspace$upsert_retrieved_resource(stale_source)
  expect_error(
    workspace$validate_integrity(),
    class = "tempest_research_workspace_integrity_error",
    regexp = "stale source locator or content hash"
  )
})
