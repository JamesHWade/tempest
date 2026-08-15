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
    tempest_run_dsprrr_module = function(module, chat, inputs, step) {
      observed_inputs <<- inputs
      list(title = "Report", sections = list())
    }
  )

  tempest:::tempest_draft_outline(
    writer = NULL,
    topic = "Topic",
    title = "Report",
    module = "module"
  )

  expect_identical(
    observed_inputs,
    list(topic = "Topic", report_title = "Report")
  )
})

test_that("refined outline execution uses the disjoint report title input", {
  observed_inputs <- NULL
  local_mocked_bindings(
    tempest_run_dsprrr_module = function(module, chat, inputs, step) {
      observed_inputs <<- inputs
      list(title = "Report", sections = list())
    }
  )

  tempest:::tempest_refine_outline(
    writer = NULL,
    topic = "Topic",
    title = "Report",
    draft_outline = list(title = "Draft", sections = list()),
    facts_txt = "Facts",
    module = "module"
  )

  expect_identical(
    observed_inputs,
    list(
      topic = "Topic",
      report_title = "Report",
      draft_outline = "(no sections)",
      facts = "Facts"
    )
  )
})
