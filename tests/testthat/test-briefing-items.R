test_that("grounded writing renders typed briefing items", {
  workspace <- fake_store_with_sources(2)
  source_ids <- vapply(
    workspace$list_retrieved_sources(),
    `[[`,
    character(1),
    "id"
  )
  claims <- list(
    tempest_claim(
      "Pilot output increased by 18 percent",
      source_ids = source_ids[[1]],
      verification_status = "supported",
      support_score = 0.94
    ),
    tempest_claim(
      "The permit schedule remains unchanged since the prior review",
      source_ids = source_ids[[2]],
      verification_status = "supported",
      support_score = 0.91
    )
  )
  lapply(claims, workspace$add_proposed_claim)
  claims <- fake_verify_claim_supports(workspace, claims)
  output <- list(
    items = list(
      list(
        kind = "observation",
        text = claims[[1]]@claim_text,
        claim_ids = list(claims[[1]]@claim_id),
        confidence = "high"
      ),
      list(
        kind = "assessment",
        text = "The scale-up case is stronger, but permitting still gates timing.",
        claim_ids = as.list(vapply(claims, \(x) x@claim_id, character(1))),
        confidence = "medium"
      ),
      list(
        kind = "review_action",
        text = "Review whether the pending permit changes the launch date.",
        claim_ids = list(claims[[2]]@claim_id),
        confidence = "low"
      ),
      list(
        kind = "no_change",
        text = claims[[2]]@claim_text,
        claim_ids = list(claims[[2]]@claim_id),
        confidence = "high"
      )
    )
  )

  evaluated <- tempest:::tempest_stage_evaluate(
    test_program_executions()$section_writing,
    output,
    context = list(
      workspace = workspace,
      evidence = claims,
      min_support_score = 0.7
    )
  )

  expect_identical(evaluated$support_status, "verified")
  expect_match(evaluated$output, "### Verified observations", fixed = TRUE)
  expect_match(evaluated$output, "### Why it matters", fixed = TRUE)
  expect_match(evaluated$output, "### Review today", fixed = TRUE)
  expect_match(evaluated$output, "### No material change", fixed = TRUE)
  expect_match(evaluated$output, claims[[1]]@claim_text, fixed = TRUE)
  expect_match(evaluated$output, "tempest-briefing-item", fixed = TRUE)

  items <- tempest:::tempest_briefing_items_from_markdown(
    evaluated$output,
    workspace,
    min_support_score = 0.7
  )
  expect_identical(
    vapply(items, \(item) item@kind, character(1)),
    c("observation", "assessment", "review_action", "no_change")
  )
  expect_identical(
    items[[2]]@claim_ids,
    sort(vapply(
      claims,
      \(claim) claim@claim_id,
      character(1)
    ))
  )
  expect_identical(items[[1]]@confidence, NA_character_)
  expect_identical(items[[3]]@confidence, NA_character_)
})

test_that("briefing items fail closed on ungoverned synthesis", {
  workspace <- fake_store_with_sources(1)
  source_id <- workspace$list_retrieved_sources()[[1]]$id
  claim <- tempest_claim(
    "A verified operational change occurred",
    source_ids = source_id,
    verification_status = "supported",
    support_score = 0.9
  )
  workspace$add_proposed_claim(claim)
  claim <- fake_verify_claim_supports(workspace, list(claim))[[1]]
  evaluate <- function(items) {
    tempest:::tempest_stage_evaluate(
      test_program_executions()$section_writing,
      list(items = items),
      context = list(
        workspace = workspace,
        evidence = list(claim),
        min_support_score = 0.7
      )
    )
  }
  observation <- list(
    kind = "observation",
    text = claim@claim_text,
    claim_ids = list(claim@claim_id)
  )

  expect_error(
    evaluate(list(
      observation,
      list(
        kind = "assessment",
        text = "This matters.",
        claim_ids = list(),
        confidence = "medium"
      )
    )),
    class = "tempest_stage_output_validation_error"
  )
  expect_error(
    evaluate(list(
      observation,
      list(
        kind = "no_change",
        text = claim@claim_text,
        claim_ids = list(claim@claim_id),
        confidence = "high"
      )
    )),
    class = "tempest_stage_output_validation_error"
  )
  misclassified_no_change <- tempest:::TempestBriefingItem(
    kind = "no_change",
    text = claim@claim_text,
    claim_ids = claim@claim_id,
    confidence = "high"
  )
  expect_error(
    tempest:::tempest_briefing_items_from_markdown(
      tempest:::tempest_briefing_item_markdown(
        misclassified_no_change,
        workspace
      ),
      workspace,
      min_support_score = 0.7
    ),
    class = "tempest_product_report_error"
  )
  expect_error(
    evaluate(list(utils::modifyList(
      observation,
      list(text = "A convenient paraphrase")
    ))),
    class = "tempest_stage_output_validation_error"
  )
  expect_error(
    evaluate(list(
      observation,
      list(
        kind = "no_change",
        text = "No defensible no-change signal is available.",
        claim_ids = list(claim@claim_id),
        confidence = "high"
      )
    )),
    class = "tempest_stage_output_validation_error"
  )
  expect_error(
    evaluate(list(
      observation,
      list(
        kind = "assessment",
        text = paste0("This matters [", source_id, "]."),
        claim_ids = list(claim@claim_id),
        confidence = "medium"
      )
    )),
    class = "tempest_stage_output_validation_error"
  )
  expect_error(
    evaluate(list(
      observation,
      list(
        kind = "review_action",
        text = "Review an unrelated claim.",
        claim_ids = list("claim.unknown"),
        confidence = "low"
      )
    )),
    class = "tempest_stage_governance_error"
  )

  repeated_assessments <- c(
    list(observation),
    rep(
      list(list(
        kind = "assessment",
        text = "This result changes the decision context.",
        claim_ids = list(claim@claim_id),
        confidence = "medium"
      )),
      2L
    )
  )
  expect_error(
    tempest:::tempest_stage_evaluate(
      test_program_executions()$lead_section,
      list(items = repeated_assessments),
      context = list(
        workspace = workspace,
        evidence = list(claim),
        min_support_score = 0.7
      )
    ),
    class = "tempest_stage_output_validation_error"
  )
})

test_that("no-change claims use a conservative deterministic gate", {
  expect_equal(
    tempest:::tempest_briefing_claim_affirms_no_change(
      "The permit schedule remains unchanged since the prior review"
    ),
    TRUE
  )
  expect_equal(
    tempest:::tempest_briefing_claim_affirms_no_change(
      "No material change was reported in permit timing"
    ),
    TRUE
  )
  expect_equal(
    tempest:::tempest_briefing_claim_affirms_no_change(
      "The project start date remains unchanged"
    ),
    TRUE
  )
  expect_equal(
    tempest:::tempest_briefing_claim_affirms_no_change(
      "Pilot output increased by 18 percent"
    ),
    FALSE
  )
  expect_equal(
    tempest:::tempest_briefing_claim_affirms_no_change(
      paste(
        "The permit schedule remains unchanged,",
        "but pilot output increased by 18 percent"
      )
    ),
    FALSE
  )
  expect_equal(
    tempest:::tempest_briefing_claim_affirms_no_change(
      "Pilot output remains unchanged at an 18 percent increase"
    ),
    FALSE
  )
})

test_that("canonical reports preserve assessment provenance", {
  workspace <- fake_store_with_sources(1)
  source_id <- workspace$list_retrieved_sources()[[1]]$id
  claim <- tempest_claim(
    "A threshold-passing result was reported",
    source_ids = source_id,
    verification_status = "supported",
    support_score = 0.9
  )
  workspace$add_proposed_claim(claim)
  claim <- fake_verify_claim_supports(workspace, list(claim))[[1]]
  evaluated <- tempest:::tempest_stage_evaluate(
    test_program_executions()$section_writing,
    list(
      items = list(
        list(
          kind = "observation",
          text = claim@claim_text,
          claim_ids = list(claim@claim_id)
        ),
        list(
          kind = "assessment",
          text = "The result warrants a closer operating review.",
          claim_ids = list(claim@claim_id),
          confidence = "medium"
        )
      )
    ),
    context = list(
      workspace = workspace,
      evidence = list(claim),
      min_support_score = 0.7
    )
  )

  report <- tempest:::tempest_report_md_render(
    title = "Daily decision brief",
    body = evaluated$output,
    workspace = workspace,
    citation_policy = "strict",
    min_support_score = 0.7
  )

  expect_match(
    report,
    "The result warrants a closer operating review.",
    fixed = TRUE
  )
  expect_match(report, paste0("[^", source_id, "]"), fixed = TRUE)
  expect_invisible(tempest:::tempest_final_report_validate(
    report,
    workspace,
    title = "Daily decision brief",
    citation_policy = "strict",
    on_unsupported_claim = "flag",
    min_support_score = 0.7
  ))

  item_text <- evaluated$output
  expect_error(
    tempest:::tempest_report_md_render(
      title = "Repetitive brief",
      body = paste(rep(item_text, 3L), collapse = "\n\n"),
      workspace = workspace,
      citation_policy = "claim_verified",
      min_support_score = 0.7
    ),
    class = "tempest_product_report_error"
  )

  expect_error(
    tempest:::tempest_report_md_render(
      title = "Misplaced repetition",
      body = paste(
        "## First detail",
        item_text,
        "## Second detail",
        item_text,
        sep = "\n\n"
      ),
      workspace = workspace,
      citation_policy = "claim_verified",
      min_support_score = 0.7
    ),
    class = "tempest_product_report_error"
  )

  expect_no_error(tempest:::tempest_report_md_render(
    title = "Intentional repetition",
    body = paste(
      "## At a glance",
      item_text,
      "## Detail",
      item_text,
      sep = "\n\n"
    ),
    workspace = workspace,
    citation_policy = "claim_verified",
    min_support_score = 0.7
  ))

  expect_error(
    tempest:::tempest_report_md_render(
      title = "Daily decision brief",
      body = "- **Assessment:** An unbound conclusion.",
      workspace = workspace,
      citation_policy = "claim_verified",
      min_support_score = 0.7
    ),
    class = "tempest_product_report_error"
  )
})
