test_that("module factory includes extract_claims and verify_claim_support", {
  skip_if_not_installed("dsprrr")
  cfg <- tempest_config()
  modules <- tempest_make_dsprrr_modules(cfg)
  expect_contains(names(modules), c("extract_claims", "verify_claim_support"))
  expect_no_match(names(modules), "^fact_extraction$")
})

test_that("extract_claims module accepts source context inputs", {
  skip_if_not_installed("dsprrr")
  cfg <- tempest_config()
  modules <- tempest_make_dsprrr_modules(cfg)
  inputs <- S7::prop(modules$extract_claims$signature, "inputs")
  input_names <- vapply(inputs, `[[`, character(1), "name")
  expect_equal(
    input_names,
    c("answer_text", "source_context", "source_ids", "citation_mode")
  )
  instructions <- S7::prop(modules$extract_claims$signature, "instructions")
  expect_match(instructions, "verbatim contiguous substring", fixed = TRUE)
  expect_match(
    tempest:::tempest_prompt("fact_extractor_system"),
    "provider-native citation",
    fixed = TRUE
  )
})

test_that("outline module inputs do not collide with output fields", {
  modules <- tempest_make_dsprrr_modules(tempest_config())
  draft_inputs <- S7::prop(modules$draft_outline$signature, "inputs")
  refined_inputs <- S7::prop(modules$refined_outline$signature, "inputs")

  expect_equal(
    vapply(draft_inputs, `[[`, character(1), "name"),
    c("topic", "report_title")
  )
  expect_equal(
    vapply(refined_inputs, `[[`, character(1), "name"),
    c("topic", "report_title", "draft_outline", "facts")
  )
})

test_that("grounded writing programs separate observation and synthesis", {
  modules <- tempest_make_dsprrr_modules(tempest_config())
  section_instructions <- S7::prop(
    modules$section_writing$signature,
    "instructions"
  )
  lead_instructions <- S7::prop(
    modules$lead_section$signature,
    "instructions"
  )

  expect_match(
    section_instructions,
    "observations by copying the exact text of facts whose status is new",
    fixed = TRUE
  )
  expect_match(
    lead_instructions,
    "observations by copying the exact text of facts whose status is new",
    fixed = TRUE
  )
  expect_match(
    section_instructions,
    "Every non-observation item must bind exactly the claim_ids copied into its text",
    fixed = TRUE
  )
  expect_match(
    lead_instructions,
    "Every non-observation item must bind exactly the verified claim_ids copied into its text",
    fixed = TRUE
  )
  expect_match(
    section_instructions,
    "Omit no_change when no fact is already accepted",
    fixed = TRUE
  )
  expect_match(
    section_instructions,
    "When no fact is new, return exactly one no_change item",
    fixed = TRUE
  )
  expect_match(
    section_instructions,
    "at least one observation is required whenever any fact is new",
    fixed = TRUE
  )
  expect_match(
    lead_instructions,
    "at least one observation is required whenever any fact is new",
    fixed = TRUE
  )
  expect_match(
    lead_instructions,
    "When no fact is new, return exactly one no_change item",
    fixed = TRUE
  )
  expect_match(
    lead_instructions,
    "no_change item must copy the exact text of one fact whose status is already accepted",
    fixed = TRUE
  )
})

test_that("draft outline execution uses the disjoint report title input", {
  observed_inputs <- NULL
  local_mocked_bindings(
    tempest_run_dsprrr_module_structured = function(
      module,
      chat,
      inputs,
      step
    ) {
      observed_inputs <<- inputs
      structure(
        list(
          output = list(
            title = "Report",
            sections = list(list(
              title = "Section",
              summary = "Summary",
              subsections = list(list(
                title = "Subsection",
                bullets = "Finding"
              ))
            ))
          )
        ),
        class = "dsprrr_result"
      )
    }
  )

  tempest:::tempest_draft_outline(
    writer = NULL,
    topic = "Topic",
    title = "Report",
    module = test_program_executions()$draft_outline
  )

  expect_identical(
    observed_inputs,
    list(topic = "Topic", report_title = "Report")
  )
})

test_that("refined outline execution uses the disjoint report title input", {
  observed_inputs <- NULL
  verified <- test_verified_workspace()
  local_mocked_bindings(
    tempest_run_dsprrr_module_structured = function(
      module,
      chat,
      inputs,
      step
    ) {
      observed_inputs <<- inputs
      structure(
        list(
          output = list(
            title = "Report",
            sections = list(list(
              title = "Section",
              summary = "Summary",
              subsections = list(list(
                title = "Subsection",
                bullets = "Finding"
              ))
            ))
          )
        ),
        class = "dsprrr_result"
      )
    }
  )

  tempest:::tempest_refine_outline(
    writer = NULL,
    topic = "Topic",
    title = "Report",
    draft_outline = list(
      title = "Draft",
      sections = list(list(
        title = "Draft section",
        summary = "Draft summary",
        subsections = list(list(
          title = "Draft subsection",
          bullets = "Draft finding"
        ))
      ))
    ),
    facts_txt = "Facts",
    module = test_program_executions()$refined_outline,
    workspace = verified$workspace,
    verified_evidence = verified$workspace$list_proposed_claims()
  )

  expect_identical(
    observed_inputs,
    list(
      topic = "Topic",
      report_title = "Report",
      draft_outline = "- Draft section: Draft summary",
      facts = "Facts"
    )
  )
})
