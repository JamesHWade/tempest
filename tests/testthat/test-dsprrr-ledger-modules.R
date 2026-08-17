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
    workspace = test_research_workspace()
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
