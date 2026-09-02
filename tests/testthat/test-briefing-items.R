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
  fake_accepted_claim(workspace, claims[[2]]@claim_text)
  lapply(claims, workspace$add_proposed_claim)
  claims <- fake_verify_claim_supports(workspace, claims)
  assessment_text <- tempest:::tempest_briefing_item_synthesis_text(
    "assessment",
    claims
  )
  review_text <- tempest:::tempest_briefing_item_synthesis_text(
    "review_action",
    claims[2]
  )
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
        text = assessment_text,
        claim_ids = as.list(vapply(claims, \(x) x@claim_id, character(1))),
        confidence = "medium"
      ),
      list(
        kind = "review_action",
        text = review_text,
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
  expect_match(evaluated$output, "### What changed", fixed = TRUE)
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
        kind = "assessment",
        text = "Revenue doubled in the latest quarter.",
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
        text = "Review an unrelated revenue claim.",
        claim_ids = list(claim@claim_id)
      )
    )),
    class = "tempest_stage_output_validation_error"
  )
  forged_assessment <- tempest:::TempestBriefingItem(
    kind = "assessment",
    text = "Revenue doubled in the latest quarter.",
    claim_ids = claim@claim_id,
    confidence = "medium"
  )
  expect_error(
    tempest:::tempest_briefing_items_from_markdown(
      tempest:::tempest_briefing_item_markdown(
        forged_assessment,
        workspace
      ),
      workspace,
      min_support_score = 0.7
    ),
    class = "tempest_product_report_error"
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
        text = tempest:::tempest_briefing_item_synthesis_text(
          "assessment",
          list(claim)
        ),
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

test_that("claim dispositions compare verified text with pinned Claims", {
  workspace <- fake_store_with_sources(1)
  fake_accepted_claim(workspace, "The permit schedule remains unchanged.")

  expect_identical(
    tempest:::tempest_briefing_claim_disposition(
      "The permit schedule remains unchanged.",
      tempest:::tempest_workspace_accepted_claim_keys(workspace)
    ),
    "duplicate"
  )
  expect_identical(
    tempest:::tempest_briefing_claim_disposition(
      "  the permit  schedule remains UNCHANGED",
      tempest:::tempest_workspace_accepted_claim_keys(workspace)
    ),
    "duplicate"
  )
  expect_identical(
    tempest:::tempest_briefing_claim_disposition(
      "The permit schedule moved by two weeks.",
      tempest:::tempest_workspace_accepted_claim_keys(workspace)
    ),
    "new"
  )
  expect_identical(
    tempest:::tempest_briefing_claim_disposition("Anything", character()),
    "new"
  )
})

test_that("accepted claim text is recovered from legacy record content", {
  workspace <- fake_store_with_sources(1)
  resource <- tempest:::tempest_resource(
    resource_kind = "graft.record",
    locator = "graft/Claim/legacy",
    title = "Claim legacy",
    media_type = "text/plain",
    content = "claim_type: finding\nstatement_text: Output held steady.\n",
    metadata = list(
      graft_record_id = "legacy",
      graft_record_class = "Claim",
      graft_revision_id = "legacy"
    )
  )
  workspace$upsert_retrieved_resource(resource)

  expect_identical(
    tempest:::tempest_workspace_accepted_claim_keys(workspace),
    tempest:::tempest_claim_text_key("Output held steady.")
  )
})

test_that("no-change items require a claim already accepted in the snapshot", {
  workspace <- fake_store_with_sources(1)
  source_id <- workspace$list_retrieved_sources()[[1]]$id
  claim <- tempest_claim(
    "Headcount remained unchanged this quarter.",
    source_ids = source_id,
    verification_status = "supported",
    support_score = 0.9
  )
  workspace$add_proposed_claim(claim)
  claim <- fake_verify_claim_supports(workspace, list(claim))[[1]]
  value <- list(
    kind = "no_change",
    text = claim@claim_text,
    claim_ids = list(claim@claim_id),
    confidence = "high"
  )
  accepted <- fake_store_with_sources(1)
  fake_accepted_claim(accepted, claim@claim_text)
  accepted_claim <- tempest_claim(
    claim@claim_text,
    source_ids = accepted$list_retrieved_sources()[[1]]$id,
    verification_status = "supported",
    support_score = 0.9
  )
  accepted$add_proposed_claim(accepted_claim)
  accepted_claim <- fake_verify_claim_supports(
    accepted,
    list(accepted_claim)
  )[[1]]
  context <- list(
    workspace = workspace,
    evidence = list(claim),
    min_support_score = 0.7
  )

  expect_error(
    tempest:::tempest_stage_evaluate(
      test_program_executions()$section_writing,
      list(items = list(value)),
      context = context
    ),
    class = "tempest_stage_output_validation_error"
  )
  item <- tempest:::TempestBriefingItem(
    kind = value$kind,
    text = value$text,
    claim_ids = claim@claim_id,
    confidence = value$confidence
  )
  expect_error(
    tempest:::tempest_briefing_items_from_markdown(
      tempest:::tempest_briefing_item_markdown(item, workspace),
      workspace,
      min_support_score = 0.7
    ),
    class = "tempest_product_report_error"
  )

  observation <- list(
    kind = "observation",
    text = accepted_claim@claim_text,
    claim_ids = list(accepted_claim@claim_id)
  )
  expect_error(
    tempest:::tempest_stage_evaluate(
      test_program_executions()$section_writing,
      list(items = list(observation)),
      context = list(
        workspace = accepted,
        evidence = list(accepted_claim),
        min_support_score = 0.7
      )
    ),
    class = "tempest_stage_output_validation_error"
  )
  accepted_item <- tempest:::TempestBriefingItem(
    kind = "no_change",
    text = accepted_claim@claim_text,
    claim_ids = accepted_claim@claim_id,
    confidence = "high"
  )
  reloaded <- tempest:::tempest_briefing_items_from_markdown(
    tempest:::tempest_briefing_item_markdown(accepted_item, accepted),
    accepted,
    min_support_score = 0.7
  )
  expect_identical(reloaded[[1L]]@kind, "no_change")
})

test_that("observations render under What changed and no-change separately", {
  workspace <- fake_store_with_sources(1)
  source_id <- workspace$list_retrieved_sources()[[1]]$id
  claims <- list(
    tempest_claim(
      "A new recycling line reached commercial yield.",
      source_ids = source_id,
      verification_status = "supported",
      support_score = 0.9
    ),
    tempest_claim(
      "The permit schedule remains unchanged.",
      source_ids = source_id,
      verification_status = "supported",
      support_score = 0.9
    )
  )
  fake_accepted_claim(workspace, claims[[2L]]@claim_text)
  for (claim in claims) {
    workspace$add_proposed_claim(claim)
  }
  claims <- fake_verify_claim_supports(workspace, claims)
  evaluated <- tempest:::tempest_stage_evaluate(
    test_program_executions()$section_writing,
    list(
      items = list(
        list(
          kind = "observation",
          text = claims[[1L]]@claim_text,
          claim_ids = list(claims[[1L]]@claim_id)
        ),
        list(
          kind = "no_change",
          text = claims[[2L]]@claim_text,
          claim_ids = list(claims[[2L]]@claim_id),
          confidence = "high"
        )
      )
    ),
    context = list(
      workspace = workspace,
      evidence = claims,
      min_support_score = 0.7
    )
  )
  lines <- strsplit(evaluated$output, "\n", fixed = TRUE)[[1L]]

  expect_identical(
    lines[startsWith(lines, "### ")],
    c("### What changed", "### No material change")
  )
  expect_match(
    evaluated$output,
    "**No material change:** The permit",
    fixed = TRUE
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
  assessment_text <- tempest:::tempest_briefing_item_synthesis_text(
    "assessment",
    list(claim)
  )
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
          text = assessment_text,
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
    assessment_text,
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

test_that("a briefing may consist of no-change findings alone", {
  workspace <- fake_store_with_sources(1)
  source_id <- workspace$list_retrieved_sources()[[1]]$id
  claim <- tempest_claim(
    "The permit schedule remains unchanged.",
    source_ids = source_id,
    verification_status = "supported",
    support_score = 0.9
  )
  fake_accepted_claim(workspace, claim@claim_text)
  workspace$add_proposed_claim(claim)
  claim <- fake_verify_claim_supports(workspace, list(claim))[[1]]

  evaluated <- tempest:::tempest_stage_evaluate(
    test_program_executions()$section_writing,
    list(
      items = list(
        list(
          kind = "no_change",
          text = claim@claim_text,
          claim_ids = list(claim@claim_id),
          confidence = "high"
        )
      )
    ),
    context = list(
      workspace = workspace,
      evidence = list(claim),
      min_support_score = 0.7
    )
  )

  expect_match(evaluated$output, "### No material change", fixed = TRUE)
  expect_no_match(evaluated$output, "### What changed", fixed = TRUE)
})

test_that("section facts carry each claim's disposition for the writer", {
  workspace <- fake_store_with_sources(1)
  source_id <- workspace$list_retrieved_sources()[[1]]$id
  fake_accepted_claim(workspace, "The permit schedule remains unchanged.")
  claims <- list(
    tempest_claim("A new line reached yield.", source_ids = source_id),
    tempest_claim(
      "The permit schedule remains unchanged.",
      source_ids = source_id
    )
  )

  text <- tempest:::tempest_section_evidence_text(
    claims,
    accepted = tempest:::tempest_workspace_accepted_claim_keys(workspace)
  )
  lines <- strsplit(text, "\n", fixed = TRUE)[[1L]]

  expect_match(lines[[1L]], "\\(status: new\\)$")
  expect_match(lines[[2L]], "\\(status: already accepted\\)$")
  expect_match(
    lines[[1L]],
    paste0("(claim_id: ", claims[[1L]]@claim_id, ")"),
    fixed = TRUE
  )
})

test_that("retracted or superseded accepted claims are not no-change anchors", {
  workspace <- fake_store_with_sources(1)
  fake_accepted_claim(workspace, "Output held steady.", status = "superseded")
  fake_accepted_claim(workspace, "Yield reached target.", status = "retracted")
  fake_accepted_claim(workspace, "Permits are unchanged.")
  legacy <- tempest:::tempest_resource(
    resource_kind = "graft.record",
    locator = "graft/Claim/legacy",
    title = "Claim legacy",
    media_type = "text/plain",
    content = "statement_text: Old finding.\nstatus: superseded\n",
    metadata = list(
      graft_record_id = "legacy",
      graft_record_class = "Claim",
      graft_revision_id = "legacy"
    )
  )
  workspace$upsert_retrieved_resource(legacy)

  expect_identical(
    tempest:::tempest_workspace_accepted_claim_keys(workspace),
    tempest:::tempest_claim_text_key("Permits are unchanged.")
  )
})
